(*---------------------------------------------------------------------------
   Copyright (c) 2021 The qoic programmers. All rights reserved.
   Distributed under the ISC license, see terms at the end of the file.
  ---------------------------------------------------------------------------*)

(** QOI image codec.

    Encodes and decodes {{:https://qoiformat.org/}QOI images}.

    See the {{!limitations}limitations} and an {{!example}example}. *)

(** {1:meta Image metadata} *)

type color_space = [ `Srgb | `Linear ]
(** The type for color spaces. *)

type channels = [ `Rgb | `Rgba ]
(** The type for image channels. *)

val channel_count : channels -> int
(** [channel_count c] is the number of channels in [c]. *)

(** Image metadata. *)
module Meta : sig
  type t
  (** The type for image metadata. *)

  val v : color_space -> channels -> w:int -> h:int -> t
  (** [v] is image metadata with the given parameters. See accessors for
      semantics.

      Raises [Invalid_argument] if [w] or [h] are not in the range of
      unsigned 32-bit integers. *)

  val w : t -> int
  (** [w m] is the image width in pixels. *)

  val h : t -> int
  (** [h m] is the image height in pixels. *)

  val channels : t -> channels
  (** [channels m] are the image's channels. *)

  val color_space : t -> color_space
  (** [color_space m] is the image's colorspace. *)

  val pixels_byte_length : t -> int option
  (** [pixels_byte_length m] is the number of bytes needed to store
      the pixels of an image described by [m]. This is [None] if [w m *
      h m * channel_count (channels m)] overflows. *)

  val encodable : t -> bool
  (** [encodable m] is [true] iff [m] can be encoded by Qoic. This just
      checks that the allocated encoding buffer length does not overflow. *)

  val pp : Format.formatter -> t -> unit
  (** [pp ppf m] formats an unspecified representation of [m] on [ppf]. *)
end

(** {1:bytes_pixels Bytes and pixel data} *)

type uint32 = int32
(** The type for unsigned 32-bit integers. *)

(** Bigarrays of bytes. *)
module Bigbytes : sig
  type t =
    (int, Bigarray.int8_unsigned_elt, Bigarray.c_layout) Bigarray.Array1.t
    (** The type for bigarrays of bytes. *)

  val create : len:int -> init:int -> t
  (** [create ~len ~init] is a bigarray of bytes of length [len] filled
      with byte [init]. *)

  val length : t -> int
  (** [length b] is the length of [b]. *)

  val get : t -> int -> int
  (** [get b i] is the byte at index [i] of [b]. *)

  val set : t -> int -> int -> unit
  (** [set b i v] sets the byte at index [i] of [b] to [v]. *)

  val get_uint32_be : t -> int -> uint32
  (** [get b i] is the big endian unsigned 32-bit integer at byte index [i]
      of [b]. *)

  val set_uint32_be : t -> int -> uint32 -> unit
  (** [set b i v] sets the big endian unsigned 32-bit integer at byte index [i]
      of [b] to [v]. *)
end

type pixels = Bigbytes.t
(** The type for pixel data.

    Pixels are stored in a linear buffer, line-by-line, from left to
    right and top to bottom in RGB or RGBA order with one byte per
    component. *)

(** {1:encoding Encoding} *)

val encode : Meta.t -> pixels -> Bigbytes.t
(** [encode m p] is the encoding of pixels [p] described by [m].

    Raises [Invalid_argument] if [Meta.pixels_byte_length m] does not
    match [p]'s length or if [Meta.encodable m] is [false]. *)

(** {1:decoding Decoding} *)

(** Decoding errors. *)
module Error : sig
  type kind =
  | Image_too_large of uint32 * uint32
  | Invalid_channels of int
  | Invalid_color_space of int
  | Invalid_end_marker
  | Invalid_image
  | Not_a_qoi_file
  | Truncated_chunk_stream (** *)
  (** The type for kinds of decoding errors. *)

  val pp_kind : Format.formatter -> kind -> unit
  (** [pp_kind ppf k] formats [k] in english on [ppf]. *)

  type t = kind * int
  (** The type for errors. The kind of error and the byte offset where it
      occured. *)

  val pp : Format.formatter -> t -> unit
  (** [pp ppf e] formats [e] in english on [ppf]. *)

  val to_string : t -> string
  (** [to_string e] is an english error message for [e]. *)
end

val decode :
  ?channels:channels -> Bigbytes.t -> (Meta.t * pixels, Error.t) result
(** [decode ~channels b] decodes a QOI image from [b].

    If [channels] is [Some c], forces the channels of the resulting
    metadata and pixels to be [c]. An [`Rgb] image forced to
    [`Rgba] gets a constant [0xFF] alpha channel and an [`Rgba] image forced
    to [`Rgb] drops the alpha channel. *)

val decode' :
  ?channels:channels -> Bigbytes.t -> (Meta.t * pixels, string) result
(** [decode' b] is [Result.map_error Error.to_string (decode b)]. *)

val decode_meta : Bigbytes.t -> (Meta.t, Error.t) result
(** [decode_meta b] decodes QOI image metadata from [b]. *)

(** {1:limitations Limitations}

    {ul
    {- [int]s are used for the image size and indexing pixel components in
       bigarrays of bytes. On 32-bit platforms, this limits images
       to around 536 millions pixels (23k x 23k) for RGBA.}
    {- The codec is not streaming. It encodes and decodes to memory.}}

    {1:example Example}

    The [Stdlib] (at least until 4.14) does not provide a simple interface to
    interface bigarrays with files. Assume we have a module
    with the following interface (see below for an
    implementation):
{[
module Bigfile : sig
  type fpath = string
  type bigbytes =
    (int, Bigarray.int8_unsigned_elt, Bigarray.c_layout) Bigarray.Array1.t

  val read : fpath -> (bigbytes, string) result
  val write : fpath -> bigbytes -> (unit, string) result
end
]}
    The following function recodes a QOI file with {!Qoic}:
{[
let recode ?channels file ~dst =
  let ( let* ) = Result.bind in
  let* bytes = Bigfile.read file in
  let* meta, pixels = Qoic.decode' ?channels bytes in
  Bigfile.write dst (Qoic.encode meta pixels)
]}

   The [Bigfile] module can be implemented by:
{[
module Bigfile = struct
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
end
]}
*)

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
