(*---------------------------------------------------------------------------
   Copyright (c) 2021 The qoic programmers. All rights reserved.
   Distributed under the ISC license, see terms at the end of the file.
  ---------------------------------------------------------------------------*)

open B00_std
open Result.Syntax

let green = Fmt.tty_string [`Fg `Green]
let red = Fmt.tty_string [`Fg `Red]
let log = Format.printf
let log_trip_ok () = log " %a@." green "OK"
let log_trip_error e = log "@\n%a: %s@\n@." red "Error" e
let log_final_result span n fail =
  let pp_span = Time.Span.pp in
  if fail <> 0
  then log "[%a] %d out of %d files did not round trip.@." red "FAIL" fail n
  else log "[ %a ] All %d file(s) round trip in %a.@." green "OK" n pp_span span

let hexdump ~dst bytes =
  let bytes_dump = Fpath.(dst + ".bytes") in
  let* xxd = Os.Cmd.get Cmd.(atom "xxd") in
  let* () = Bigfile.write (Fpath.to_string bytes_dump) bytes in
  let* () = Os.Cmd.run Cmd.(xxd %% path bytes_dump %% path dst) in
  Ok dst

let diff file spec_bytes qoic_bytes =
  Result.retract @@ Result.join @@ Os.Dir.with_tmp @@ fun dir ->
  let* diff =
    let color = match Fmt.tty_cap () with
    | `None -> "--color=never"
    | `Ansi -> "--color=always"
    in
    Os.Cmd.get Cmd.(atom "git" % "diff" % "--no-index" % "--patience" % color)
  in
  let base = Fpath.(dir / basename file) in
  let* spec_hex = hexdump ~dst:Fpath.(base + ".spec") spec_bytes in
  let* qoic_hex = hexdump ~dst:Fpath.(base + ".qoic") qoic_bytes in
  let diff = Cmd.(diff % Fpath.basename spec_hex % Fpath.basename qoic_hex) in
  Result.map snd (Os.Cmd.run_status_out ~trim:false ~cwd:dir diff)

let trip file =
  log "Trip %a@?" Fpath.pp_unquoted file;
  let* bytes = Bigfile.read (Fpath.to_string file) in
  let* meta, pixels = Qoic.decode' bytes in
  log " %a@?" Qoic.Meta.pp meta;
  let bytes' = Qoic.encode meta pixels in
  if bytes <> bytes' then Error ("\n" ^ diff file bytes bytes') else Ok ()

let default_files () =
  let default_dir = Fpath.v "images" in
  let* fs = Os.Dir.fold_files ~recurse:true Os.Dir.path_list default_dir [] in
  Ok (List.rev (List.filter (Fpath.has_ext ".qoi") fs))

let trips files =
  Log.if_error ~use:1 @@
  let* files = match files with [] -> default_files () | files -> Ok files in
  let rec loop t n fail = function
  | [] -> log_final_result (Time.count t) n fail; Ok fail
  | f :: fs ->
      match trip f with
      | Error e -> log_trip_error e; loop t (n + 1) (fail + 1) fs
      | Ok () -> log_trip_ok (); loop t (n + 1) fail fs
  in
  loop (Time.counter ()) 0 0 files

let main () =
  let usage = "Usage: trip [FILE.qoi]…" in
  let rev_files = ref [] in
  let pos s = rev_files := Fpath.v s :: !rev_files in
  Arg.parse [] pos usage;
  Fmt.set_tty_cap ();
  trips (List.rev !rev_files)

let () = if !Sys.interactive then () else exit (main ())

(*---------------------------------------------------------------------------
   Copyright (c) 2021 The qoic programmers

   Permission to use, copy, modify, and/or distribute this software for any
   purpose with or without fee is hereby granted, provided that the above
   copyright notice and this permission notice appear in all copies.

   THE SOFTWARE IS PROVIDED "AS IS" AND THE AUTHOR DISCLAIMS ALL WARRANTIES
   WITH REGARD TO THIS SOFTWARE INCLUDING ALL IMPLIED WARRANTIES OF
   MERCHANTABILITY AND FITNESS. IN NO EVENT SHALL THE AUTHOR BE LIABLE FOR
   ANY SPECIAL, DIRECT, INDIRECT, OR CONSEQUENTIAL DAMAGES OR ANY DAMAGES
   WHATSOEVER RESULTING FROM LOSS OF USE, DATA OR PROFITS, WHETHER IN AN
   ACTION OF CONTRACT, NEGLIGENCE OR OTHER TORTIOUS ACTION, ARISING OUT OF
   OR IN CONNECTION WITH THE USE OR PERFORMANCE OF THIS SOFTWARE.
  ---------------------------------------------------------------------------*)
