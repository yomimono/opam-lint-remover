open OpamParserTypes
open Cmdliner

let version = "0.0.1"
let repo = "/home/user/opam-repository"

let acted = ref false

let field_of_errno = function
  | "25" -> "authors"
  | "35" -> "homepage"
  | "36" -> "bug-reports"

let has_variable name = function
  | Variable (_, n, _) when n = name -> true
  | _ -> false

let file_has file name =
  let l = (OpamParser.file file).file_contents in
  List.exists (has_variable name) l

module Repo_search = struct
  let (/) x y = Fpath.add_seg x y
  let base n = Fpath.of_string n |> Rresult.R.get_ok

  let path package version repo =
    let repo = base repo in
    let this_one = package ^ "." ^ version in
    repo / "packages" / package / this_one / "opam"

  let discard_name s =
    Astring.String.cuts ~sep:"." s |> function
    | name :: version -> List.map int_of_string version
    | _ -> failwith "incomprehensible package version"

  let version_list_of_string s =
    Astring.String.cuts ~sep:"." s |> List.map int_of_string

  let string_of_version_list i =
    Astring.String.concat ~sep:"." @@ List.map string_of_int i

  let newer_than x y =
    let rec check x y =
      match (x, y) with
      | [], [] -> false
      | _ , [] -> false
      | [], _ -> true
      | x::_, y::_ when x > y -> false
      | x::_, y::_ when x < y -> true
      | x::xs , y::ys -> check xs ys
    in
    check x y

  let find_newer package version repo =
    let repo = base repo in
    let packages_dir = repo / "packages" / package in
    let versions = Bos.OS.Dir.contents ~rel:true packages_dir |> Rresult.R.get_ok |> List.sort compare in
    let available_versions = List.map (fun a -> Fpath.to_string a|> discard_name) versions in
    List.filter (newer_than (version_list_of_string version)) available_versions |> List.map string_of_version_list

  let get_errors package version repo =
    let opam_file = path package version repo in
    let lint = Bos.Cmd.(Bos.OS.Cmd.must_exist @@ v "opam" % "lint" % "-s" % p opam_file) |> Rresult.R.get_ok in
    let errs = Bos.OS.Cmd.run_out lint |> Bos.OS.Cmd.out_string |> Rresult.R.get_ok in
    (* errs should be a whitespace-separated list of problem codes *)
    Astring.String.fields ~empty:false (fst errs)

  let fixes_err err package version repo =
    match get_errors package version repo with
    | [] -> true
    | l -> not @@ List.mem err l

end

module Fix = struct
  let find_value opam value =
    let open OpamParserTypes in
    let check_node value = function
    | Variable ((_path, _number, _), name, _value) when name = value -> true
    | _ -> false
    in
    List.filter (check_node value) opam.file_contents

  let add_value opam value =
    let first_variable l =
      let var = function
      | Variable (pos, name, value) -> true
      | _ -> false
      in
      List.find var l
    in
    let translate = function
    | Variable (_, name, value) ->
      let (Variable ((file, line, column), _, _)) = first_variable opam.file_contents in
      Variable ((file, line, column), name, value)
    | _ -> invalid_arg "non-variable value given"
    in
    { opam with file_contents = (translate value) :: opam.file_contents }

  let backport err repo package fixed flawed =
    (* for now, all errs are err 25, missing field 'authors' *)
    let fixed = OpamParser.file @@ (Repo_search.path package fixed repo |> Fpath.to_string) in
    let value = find_value fixed (field_of_errno err) in
    let opam = add_value flawed (List.hd value) in
    acted := true;
    opam

  let write file =
    (* overwrite previous contents *)
    let fd = Unix.(openfile file.file_name [O_WRONLY; O_TRUNC] 0o755) in
    let ch = Unix.out_channel_of_descr fd in
    set_binary_mode_out ch false;
    let fmt = Format.formatter_of_out_channel ch in
    Format.(fprintf fmt "%s\n" @@ OpamPrinter.opamfile file);
    flush ch;
    Unix.close fd 

end

let fix_problems package version repo =
  match Repo_search.get_errors package version repo with
  | [] -> `Ok
  | errs ->
    (* are there newer opam files we can consult? *)
    match Repo_search.find_newer package version repo with
    | [] ->
      Format.eprintf "%s version %s had errors %s, but there are no newer opam files to consult" package version (String.concat " " errs);
      `Error
    | newer_versions ->
      let closest err = List.find (fun version -> Repo_search.fixes_err err package version repo) newer_versions in
      let opam_file = OpamParser.file @@ (Repo_search.path package version repo |> Fpath.to_string) in
      let updated = 
        List.fold_left (fun opam err -> Fix.backport err repo package (closest err) opam) opam_file errs
      in
      Fix.write updated;
      match Repo_search.get_errors package version repo with
      | [] -> Format.printf "Successfully fixed all errors (%s) in %s version %s\n" (String.concat " " errs) package version; `Ok
      | l -> Format.printf "Errors remained in %s version %s after attempting to fix: %s\n" package version (String.concat " " l); `Ok

let package =
  let doc = "The package name for which to search for more modern OPAM files." in
  Arg.(value & pos 0 string "mirage-net-xen" & (info ~doc []))

let version =
  let doc = "The version whose opam file you'd like to add upper bounds to." in
  Arg.(value & pos 1 string "0.0.1" & info [] ~doc)

let repo =
  let doc = "The location of the repository in which to look for information." in
  Arg.(value & opt dir "opam-repository" & info ["repo"; "r"] ~doc)

let info =
  let doc = "camelus got you down? we got your back." in
  let man = [ `S "BUGS"; `P "Please report bugs at https://github.com/yomimono/opam-lint-remover/issues"; ] in
  Term.info "opam-lint-remover" ~version:"0.0.1" ~doc ~man

let () =
  let find_t = Term.(const fix_problems $ package $ version $ repo) in
  match Term.eval (find_t, info) with 
  | `Ok _ -> exit 0
  | _ -> exit 1
