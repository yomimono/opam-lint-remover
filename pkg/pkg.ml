#!/usr/bin/env ocaml
#use "topfind"
#require "topkg"
open Topkg

let () =
  Pkg.describe "opam-lint-remover" @@ fun c ->
  Ok [ 
       Pkg.bin "src/main" ~dst:"delint"
  ]
