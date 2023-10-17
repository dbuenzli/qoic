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
    B0_meta.(empty |> tag test |>
             add B0_unit.Action.exec_cwd B0_unit.Action.scope_cwd)
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
    let open B0_meta in
    empty
    |> add authors ["The qoic programmers"]
    |> add maintainers ["Daniel Bünzli <daniel.buenzl i@erratique.ch>"]
    |> add homepage "https://erratique.ch/software/qoic"
    |> add online_doc "https://erratique.ch/software/qoic/doc"
    |> add licenses ["ISC"]
    |> add repo "git+https://erratique.ch/repos/qoic.git"
    |> add issues "https://github.com/dbuenzli/qoic/issues"
    |> add description_tags ["codec"; "qoi"; "image"; "org:erratique"; ]
    |> add B0_opam.Meta.build
      {|[["ocaml" "pkg/pkg.ml" "build" "--dev-pkg" "%{dev}%"]]|}
    |> tag B0_opam.tag
    |> add B0_opam.Meta.depends
      [ "ocaml", {|>= "4.12.0"|};
        "ocamlfind", {|build|};
        "ocamlbuild", {|build|}; ]
  in
  B0_pack.v "default" ~doc:"qoic package" ~meta ~locked:true @@
  B0_unit.list ()

(* Cmdlets *)

let spec_images =
  let doc = "Download specification test images" in
  B0_cmdlet.v "download-spec-images" ~doc @@
  fun env _args -> B0_cmdlet.exit_of_result @@
  let src = "https://qoiformat.org/qoi_test_images.zip" in
  let dst = B0_cmdlet.in_scope_dir env (Fpath.v ("images")) in
  let* curl = Os.Cmd.get (Cmd.atom "curl") in
  let* unzip = Os.Cmd.get (Cmd.atom "unzip") in
  let* tmp_zip = Os.Path.tmp ~name:"tmp-%s.zip" () in
  let* () =
    let outf = Os.Cmd.out_file ~force:true ~make_path:false tmp_zip in
    Os.Cmd.run ~stdout:outf Cmd.(curl % "-L" % src)
  in
  Os.Cmd.run @@
  Cmd.(unzip % "-j" % "-o" %% path tmp_zip % "*.qoi" % "-d" %% path dst)
