(*---------------------------------------------------------------------------
   Copyright (c) 2021 The qoic programmers. All rights reserved.
   Distributed under the ISC license, see terms at the end of the file.
  ---------------------------------------------------------------------------*)

type fpath = string

type bigbytes =
  (int, Bigarray.int8_unsigned_elt, Bigarray.c_layout) Bigarray.Array1.t

let map_bytes ?(len = -1) ~write fd =
  let t = Bigarray.int8_unsigned and l = Bigarray.C_layout in
  Bigarray.array1_of_genarray (Unix.map_file fd t l write [|len|])

let error f e = Error (Printf.sprintf "%s: %s" f (Unix.error_message e))

let read file =
  try
    let fd = Unix.openfile file Unix.[O_RDONLY] 0x600 in
    let finally () = try Unix.close fd with Unix.Unix_error _ -> () in
    Fun.protect ~finally @@ fun () -> Ok (map_bytes ~write:false fd)
  with
  | Unix.Unix_error (e, _, _) -> error file e

let write file bytes =
  try
    let fd = Unix.openfile file Unix.[O_CREAT; O_RDWR; O_TRUNC] 0o644 in
    let finally () = try Unix.close fd with Unix.Unix_error _ -> () in
    Fun.protect ~finally @@ fun () ->
    let dst = map_bytes ~len:(Bigarray.Array1.dim bytes) ~write:true fd in
    Ok (Bigarray.Array1.blit bytes dst)
  with
  | Unix.Unix_error (e, _, _) -> error file e

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
