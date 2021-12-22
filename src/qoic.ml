(*---------------------------------------------------------------------------
   Copyright (c) 2021 The qoic programmers. All rights reserved.
   Distributed under the ISC license, see terms at the end of the file.
  ---------------------------------------------------------------------------*)

let strf = Printf.sprintf

(* Programming errors. *)

let err_img_too_large = "Image too large to be encoded"
let err_img_size w h = strf "Invalid image size: %dx%d" w h
let err_byte_size isize bsize =
  let isize = match isize with None -> "overflow" | Some s -> string_of_int s in
  strf "Image byte size (%s) does not match buffer size (%d)" isize bsize

(* Integer fiddling *)

type uint32 = int32
let int_is_uint32 u = 0 <= u && Int64.(compare (of_int u) 4294967295L) <= 0

let shift8 = Sys.word_size - 9
let[@inline] int8_of_uint8_bits v = (v lsl shift8) asr shift8

(* Bigarrays of bytes *)

module Bigbytes = struct
  type t = (int,Bigarray.int8_unsigned_elt,Bigarray.c_layout) Bigarray.Array1.t

  let _create len = Bigarray.(Array1.create int8_unsigned c_layout len)
  let create ~len ~init:v = let a = _create len in Bigarray.Array1.fill a v; a
  let length b = Bigarray.Array1.dim b

  let[@inline] get b i = (Bigarray.Array1.get : t -> int -> int) b i
  let[@inline] set b i v = (Bigarray.Array1.set : t -> int -> int -> unit) b i v

  external swap_32 : int32 -> int32 = "%bswap_int32"
  external get_uint32_ne : t -> int -> uint32 = "%caml_bigstring_get32"
  external set_uint32_ne : t -> int -> uint32 -> unit = "%caml_bigstring_set32"

  let[@inline] get_uint32_be b i =
    if Sys.big_endian
    then get_uint32_ne b i
    else swap_32 (get_uint32_ne b i)

  let[@inline] set_uint32_be b i v =
    if Sys.big_endian
    then set_uint32_ne b i v
    else set_uint32_ne b i (swap_32 v)

  let resize b ~len = Bigarray.Array1.sub b 0 len
end

type pixels = Bigbytes.t

(* QOI constants *)

let qoi_magic = 0x716f6966l (* "qoif" *)
let qoi_header_length = 14
let qoi_end_marker_length = 8
let qoi_op_mask  = 0xc0
let qoi_op_index = 0x00
let qoi_op_diff  = 0x40
let qoi_op_luma  = 0x80
let qoi_op_run   = 0xc0
let qoi_op_rgb   = 0xfe
let qoi_op_rgba  = 0xff

(* Image metadata *)

type color_space = [ `Srgb | `Linear ]
type channels = [ `Rgb | `Rgba ]
let channel_count = function `Rgb -> 3 | `Rgba -> 4

module Meta = struct
  type t =
    { w : int; h : int;
      channels : channels;
      color_space : color_space; }

  let v color_space channels ~w ~h =
    if not (int_is_uint32 w) || not (int_is_uint32 h)
    then invalid_arg (err_img_size w h)
    else { w; h; channels; color_space }

  let w m = m.w
  let h m = m.h
  let color_space m = m.color_space
  let channels m = m.channels
  let pixels_byte_length m =
    let count = m.w * m.h * channel_count m.channels in
    if count < m.w || count < m.h then None (* overflow *) else Some count

  let encoding_buffer_byte_length m =
    let pixel_count = m.w * m.h in
    let count = pixel_count * channel_count m.channels in
    if count < pixel_count || count < pixel_count then None (* overflow *) else
    let l = count + pixel_count + qoi_header_length + qoi_end_marker_length in
    if l < count then None else Some l

  let encodable m = Option.is_some (encoding_buffer_byte_length m)

  let samples_descr m = match m.color_space, m.channels with
  | `Srgb, `Rgb -> "sRGB"
  | `Srgb, `Rgba -> "sRGBA"
  | `Linear, `Rgb -> "linear RGB"
  | `Linear, `Rgba -> "linear RGBA"

  let pp ppf m = Format.fprintf ppf "%dx%d %s" m.w m.h (samples_descr m)
end

(* Pixel index *)

module Index = struct
  let create () = Array.make (64 * 4) 0
  let[@inline] get a i = Array.get a i
  let[@inline] set a i v = Array.set a i v

  let[@inline] color_idx r g b a = (r * 3 + g * 5 + b * 7 + a * 11) mod 64
  let[@inline] color_idx_pos idx = idx * 4
  let[@inline] set index ~color_idx r g b a =
    let k = color_idx_pos color_idx in
    set index k r; set index (k + 1) g;
    set index (k + 2) b; set index (k + 3) a

  let[@inline] get_r index ~color_idx_pos:k = get index k
  let[@inline] get_g index ~color_idx_pos:k = get index (k + 1)
  let[@inline] get_b index ~color_idx_pos:k = get index (k + 2)
  let[@inline] get_a index ~color_idx_pos:k = get index (k + 3)
end

(* Encoding *)

let check_length_match m p =
  let plen = Bigbytes.length p in
  match Meta.pixels_byte_length m with
  | None -> invalid_arg (err_byte_size None plen)
  | Some mlen as l -> if mlen <> plen then invalid_arg (err_byte_size l plen)

let encode_header dst m =
  Bigbytes.set_uint32_be dst 0 qoi_magic;
  Bigbytes.set_uint32_be dst 4 (Int32.of_int (Meta.w m));
  Bigbytes.set_uint32_be dst 8 (Int32.of_int (Meta.h m));
  Bigbytes.set dst 12 (match Meta.channels m with `Rgb -> 3 | `Rgba -> 4);
  Bigbytes.set dst 13 (match Meta.color_space m with `Srgb -> 0 | `Linear -> 1)

let encode_padding_and_finish dst i =
  Bigbytes.set_uint32_be dst i 0x00l;
  Bigbytes.set_uint32_be dst (i + 4) 0x01l;
  Bigbytes.resize dst ~len:(i + 8)

let encode_pixels src ~channels dst =
  let p_max = Bigbytes.length src - 1 in
  let index = Index.create () in
  let rec loop run i p r g b a =
    if p > p_max then encode_padding_and_finish dst i else
    let nr = Bigbytes.get src (p    ) in
    let ng = Bigbytes.get src (p + 1) in
    let nb = Bigbytes.get src (p + 2) in
    let na = if channels = 4 then Bigbytes.get src (p + 3) else a in
    let p = p + channels in
    if r = nr && g = ng && b = nb && a = na then
      let run = run + 1 in
      if not (run = 62 || p > p_max) then loop run i p r g b a else
      let op = qoi_op_run lor (run - 1) in
      Bigbytes.set dst i op;
      (loop[@tailcall]) 0 (i + 1) p r g b a
    else
    let i =
      if run = 0 then i else
      let op = qoi_op_run lor (run - 1) in
      Bigbytes.set dst i op; i + 1
    in
    let color_idx = Index.color_idx nr ng nb na in
    let color_idx_pos = Index.color_idx_pos color_idx in
    let ir = Index.get_r index ~color_idx_pos in
    let ig = Index.get_g index ~color_idx_pos in
    let ib = Index.get_b index ~color_idx_pos in
    let ia = Index.get_a index ~color_idx_pos in
    if ir = nr && ig = ng && ib = nb && ia = na then
      let op = qoi_op_index lor color_idx in
      Bigbytes.set dst i op;
      (loop[@tailcall]) 0 (i + 1) p nr ng nb na
    else
    let () = Index.set index ~color_idx nr ng nb na in
    if a <> na then
      (Bigbytes.set dst i qoi_op_rgba;
       Bigbytes.set dst (i + 1) nr;
       Bigbytes.set dst (i + 2) ng;
       Bigbytes.set dst (i + 3) nb;
       Bigbytes.set dst (i + 4) na;
       (loop[@tailcall]) 0 (i + 5) p nr ng nb na)
    else
    let dr = int8_of_uint8_bits ((nr - r) land 0xFF) in
    let dg = int8_of_uint8_bits ((ng - g) land 0xFF) in
    let db = int8_of_uint8_bits ((nb - b) land 0xFF) in
    if dr > -3 && dr < 2 && dg > -3 && dg < 2 && db > -3 && db < 2 then
      let d = ((dr + 2) lsl 4) lor ((dg + 2) lsl 2) lor (db + 2) in
      Bigbytes.set dst i (qoi_op_diff lor d);
      (loop[@tailcall]) 0 (i + 1) p nr ng nb na
    else
    let dr_dg = int8_of_uint8_bits ((dr - dg) land 0xFF)in
    let db_dg = int8_of_uint8_bits ((db - dg) land 0xFF)in
    if dr_dg > -9 && dr_dg < 8 && dg > -33 && dg < 32 && db_dg > -9 && db_dg < 8
    then
      (Bigbytes.set dst i (qoi_op_luma lor (dg + 32));
       Bigbytes.set dst (i + 1) (((dr_dg + 8) lsl 4) lor (db_dg + 8));
       loop 0 (i + 2) p nr ng nb na)
    else
    (Bigbytes.set dst i qoi_op_rgb;
     Bigbytes.set dst (i + 1) nr;
     Bigbytes.set dst (i + 2) ng;
     Bigbytes.set dst (i + 3) nb;
     (loop[@tailcall]) 0 (i + 4) p nr ng nb na)
  in
  loop 0 qoi_header_length 0 0 0 0 0xFF

let encode m p =
  check_length_match m p;
  let dst = match Meta.encoding_buffer_byte_length m with
  | None -> invalid_arg err_img_too_large
  | Some len -> Bigbytes._create len
  in
  let channels = channel_count (Meta.channels m) in
  encode_header dst m;
  encode_pixels p channels dst

(* Decoding *)

module Error = struct
  type t =
  | Image_too_large of uint32 * uint32
  | Invalid_channels of int
  | Invalid_color_space of int
  | Invalid_end_marker
  | Invalid_image
  | Not_a_qoi_file
  | Truncated_chunk_stream

  let to_string = function
  | Image_too_large (w, h) -> strf "Image too large (%lux%lu)" w h
  | Invalid_channels i -> strf "Invalid channels (%d)" i
  | Invalid_color_space i -> strf "Invalid color space (%d)" i
  | Invalid_end_marker -> "Invalid end marker"
  | Invalid_image -> "Invalid image"
  | Not_a_qoi_file -> "Not a QOI file"
  | Truncated_chunk_stream -> "Truncated chunk stream"
end

exception Err of Error.t
let err e = raise (Err e)

let pixel_buffer m = match Meta.pixels_byte_length m with
| Some len -> Bigbytes._create len
| None -> err Int32.(Image_too_large (of_int (Meta.w m), of_int (Meta.h m)))

let decode_header ?channels b =
  if Bigbytes.length b < qoi_header_length then err Not_a_qoi_file else
  let magic = Bigbytes.get_uint32_be b 0 in
  if magic <> qoi_magic then err Not_a_qoi_file else
  let w = Bigbytes.get_uint32_be b 4 in
  let h = Bigbytes.get_uint32_be b 8 in
  let decoded_channels = match Bigbytes.get b 12 with
  | 3 -> `Rgb | 4 -> `Rgba | i -> err (Invalid_channels i)
  in
  let channels = Option.value ~default:decoded_channels channels in
  let color_space = match Bigbytes.get b 13 with
  | 0 -> `Srgb | 1 -> `Linear | i -> err (Invalid_color_space i)
  in
  let wi = Int32.unsigned_to_int w and hi = Int32.unsigned_to_int h in
  match wi, hi with
  | None, _ | _, None -> err (Image_too_large (h, w))
  | Some w, Some h -> Meta.v color_space channels ~w ~h

let decode_end_marker b =
  let first = Bigbytes.length b - qoi_end_marker_length in
  if first < qoi_header_length then err Invalid_end_marker else
  let m0 = Bigbytes.get_uint32_be b (first    ) in
  let m1 = Bigbytes.get_uint32_be b (first + 4) in
  if m0 <> 0l || m1 <> 1l then err Invalid_end_marker else ()

let[@inline] set_pixel channels dst p r g b a =
  Bigbytes.set dst (p    ) r;
  Bigbytes.set dst (p + 1) g;
  Bigbytes.set dst (p + 2) b;
  if channels = 4 then Bigbytes.set dst (p + 3) a;
  p + channels

let decode_pixels src ~channels dst =
  let i_max = Bigbytes.length src - qoi_end_marker_length - 1 in
  let p_max = Bigbytes.length dst - 1 in
  let index = Index.create () in
  let rec loop run i p r g b a =
    if p > p_max
    then (if run <> 0 || i <> i_max + 1 then err Invalid_image else ())
    else
    if run > 0 then
      let p = set_pixel channels dst p r g b a in
      (loop[@tailcall]) (run - 1) i p r g b a
    else
    if i > i_max then err Truncated_chunk_stream else
    let b1 = Bigbytes.get src i in
    if b1 = qoi_op_rgb then
      if i + 3 > i_max then err Truncated_chunk_stream else
      let r = Bigbytes.get src (i + 1) in
      let g = Bigbytes.get src (i + 2) in
      let b = Bigbytes.get src (i + 3) in
      let p = set_pixel channels dst p r g b a in
      Index.set index ~color_idx:(Index.color_idx r g b a) r g b a;
      (loop[@tailcall]) run (i + 4) p r g b a
    else
    if b1 = qoi_op_rgba then
      if i + 4 > i_max then err Truncated_chunk_stream else
      let r = Bigbytes.get src (i + 1) in
      let g = Bigbytes.get src (i + 2) in
      let b = Bigbytes.get src (i + 3) in
      let a = Bigbytes.get src (i + 4) in
      let p = set_pixel channels dst p r g b a in
      Index.set index ~color_idx:(Index.color_idx r g b a) r g b a;
      (loop[@tailcall]) run (i + 5) p r g b a
    else
    let op = b1 land qoi_op_mask in
    if op = qoi_op_index then
      let color_idx_pos = Index.color_idx_pos b1 in
      let r = Index.get_r index ~color_idx_pos in
      let g = Index.get_g index ~color_idx_pos in
      let b = Index.get_b index ~color_idx_pos in
      let a = Index.get_a index ~color_idx_pos in
      let p = set_pixel channels dst p r g b a in
      (loop[@tailcall]) run (i + 1) p r g b a
    else
    if op = qoi_op_diff then
      let r = (r + ((b1 lsr 4) land 0x03) - 2) land 0xFF in
      let g = (g + ((b1 lsr 2) land 0x03) - 2) land 0xFF in
      let b = (b + ((b1      ) land 0x03) - 2) land 0xFF in
      let p = set_pixel channels dst p r g b a in
      Index.set index ~color_idx:(Index.color_idx r g b a) r g b a;
      (loop[@tailcall]) run (i + 1) p r g b a
    else
    if op = qoi_op_luma then
      if i + 1 > i_max then err Truncated_chunk_stream else
      let b2 = Bigbytes.get src (i + 1) in
      let dg = (b1 land 0x3f) - 32 in
      let r = (r + dg - 8 + ((b2 lsr 4) land 0x0f)) land 0xFF in
      let g = (g + dg) land 0xFF in
      let b = (b + dg - 8 + (b2 land 0x0f)) land 0xFF in
      let p = set_pixel channels dst p r g b a in
      Index.set index ~color_idx:(Index.color_idx r g b a) r g b a;
      (loop[@tailcall]) run (i + 2) p r g b a
    else
    if op = qoi_op_run then
      let run = b1 land 0x3f in
      let p = set_pixel channels dst p r g b a in
      (loop[@tailcall]) run (i + 1) p r g b a
    else
    assert false
  in
  loop 0 qoi_header_length 0 0 0 0 0xFF

let decode ?channels src =
  try
    let m = decode_header ?channels src in
    let () = decode_end_marker src in
    let dst = pixel_buffer m in
    let channels = channel_count (Meta.channels m) in
    decode_pixels src ~channels dst;
    Ok (m, dst)
  with
  | Err e -> Error e

let decode' ?channels b = Result.map_error Error.to_string (decode ?channels b)
let decode_meta b = try Ok (decode_header b) with Err e -> Error e

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
