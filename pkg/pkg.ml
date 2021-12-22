#!/usr/bin/env ocaml
#use "topfind"
#require "topkg"
open Topkg

let () =
  Pkg.describe "qoic" @@ fun c ->
  Ok [ Pkg.mllib "src/qoic.mllib";
       Pkg.doc "doc/index.mld" ~dst:"odoc-pages/index.mld";
       Pkg.test "test/trip";
       Pkg.test "test/example"; ]
