open B0_kit.V000
open Result.Syntax

(* OCaml library names *)

let qoic = B0_ocaml.libname "qoic"
let unix = B0_ocaml.libname "unix"
let b0_std = B0_ocaml.libname "b0.std"

(* Libraries *)

let qoic_lib =
  let srcs = Fpath.[ `Dir (v "src") ] in
  let requires = [] in
  B0_ocaml.lib qoic ~doc:"Qoic library" ~srcs ~requires

(* Tests *)

let test_src f = `File (Fpath.v (Fmt.str "test/%s" f))
let bigfile_srcs = [test_src "bigfile.mli"; test_src "bigfile.ml" ]

let test =
  let srcs = test_src "trip.ml" :: bigfile_srcs  in
  let requires = [ b0_std; qoic; unix ] in
  let meta =
    B0_meta.empty
    |> B0_meta.(tag test)
    |> B0_meta.add B0_unit.Action.cwd `Scope_dir
  in
  let doc = "Round trip QOI images" in
  B0_ocaml.exe "trip" ~doc ~meta ~srcs ~requires

let example =
  let srcs = test_src "example.ml" :: bigfile_srcs  in
  let requires = [ qoic; unix ] in
  let meta = B0_meta.empty |> B0_meta.tag B0_meta.test in
  let doc = "Sample code" in
  B0_ocaml.exe "example" ~doc ~meta ~srcs ~requires

(* Packs *)

let default =
  let meta =
    B0_meta.empty
    |> B0_meta.(add authors) ["The qoic programmers"]
    |> B0_meta.(add maintainers)
       ["Daniel Bünzli <daniel.buenzl i@erratique.ch>"]
    |> B0_meta.(add homepage) "https://erratique.ch/software/qoic"
    |> B0_meta.(add online_doc) "https://erratique.ch/software/qoic/doc"
    |> B0_meta.(add licenses) ["ISC"]
    |> B0_meta.(add repo) "git+https://erratique.ch/repos/qoic.git"
    |> B0_meta.(add issues) "https://github.com/dbuenzli/qoic/issues"
    |> B0_meta.(add description_tags) ["codec"; "qoi"; "image"; "org:erratique"]
    |> B0_meta.tag B0_opam.tag
    |> B0_meta.add B0_opam.build
      {|[["ocaml" "pkg/pkg.ml" "build" "--dev-pkg" "%{dev}%"]]|}
    |> B0_meta.add B0_opam.depends
      [ "ocaml", {|>= "4.12.0"|};
        "ocamlfind", {|build|};
        "ocamlbuild", {|build|}; ]
  in
  B0_pack.make "default" ~doc:"qoic package" ~meta ~locked:true @@
  B0_unit.list ()

(* Actions *)

let spec_images =
  let doc = "Download specification test images" in
  B0_unit.of_action "download-spec-images" ~doc @@
  fun env _ ~args ->
  let src = "https://qoiformat.org/qoi_test_images.zip" in
  let dst = B0_env.in_scope_dir env (Fpath.v "images") in
  let* curl = B0_env.get_cmd env (Cmd.arg "curl") in
  let* unzip = B0_env.get_cmd env (Cmd.arg "unzip") in
  let* tmp_zip = Os.Path.tmp ~name:"tmp-%s.zip" () in
  let* () =
    let outf = Os.Cmd.out_file ~force:true ~make_path:false tmp_zip in
    Os.Cmd.run ~stdout:outf Cmd.(curl % "-L" % src)
  in
  Os.Cmd.run @@
  Cmd.(unzip % "-j" % "-o" %% path tmp_zip % "*.qoi" % "-d" %% path dst)
