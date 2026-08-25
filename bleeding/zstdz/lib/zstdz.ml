(*---------------------------------------------------------------------------
   Copyright (c) 2025 Anil Madhavapeddy <anil@recoil.org>. All rights reserved.
   SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

type cctx
type dctx

exception Error of int * string

external create_cctx : unit -> cctx @@ portable = "zstdz_create_cctx"
external create_dctx : unit -> dctx @@ portable = "zstdz_create_dctx"
external compress_bound : int -> int @@ portable = "zstdz_compress_bound"
  [@@noalloc]
external error_name : int -> string @ local @@ portable = "zstdz_error_name"

(* The two working stubs return the byte count written, or the negated
   libzstd error code. Neither may carry [@@noalloc]: both release the
   runtime lock, and reacquiring it can run a pending signal handler,
   which needs a frame descriptor the compiler only emits for an
   allocating call. *)

external compress_raw :
  cctx ->
  Base_bigstring.t ->
  int ->
  int ->
  Base_bigstring.t ->
  int ->
  int ->
  int ->
  bool ->
  int
  @@ portable = "zstdz_compress_bytecode" "zstdz_compress_native"

external decompress_raw :
  dctx ->
  Base_bigstring.t ->
  int ->
  int ->
  Base_bigstring.t ->
  int ->
  int ->
  int
  @@ portable = "zstdz_decompress_bytecode" "zstdz_decompress_native"

(* An unboxed result needs both halves of the external: the native stub
   returns a raw int64_t and the bytecode stub returns a boxed Int64.
   The native stub only reads the frame header, so it is [@@noalloc]. *)
external content_size_raw : Base_bigstring.t -> int -> int -> int64#
  @@ portable = "zstdz_content_size_bytecode" "zstdz_content_size_native"
  [@@noalloc]

type frame_info = {
  content_size : int;
  window_size : int;
  dict_id : int;
  has_checksum : bool;
}

external frame_info_raw :
  Base_bigstring.t -> int -> int -> frame_info @ local @@ portable
  = "zstdz_frame_info"

(* The stubs trust their offsets, so every range reaches them checked.
   The subtraction cannot overflow: both operands are bigstring lengths
   or already-checked offsets. The message is built only on the failing
   branch, which is what keeps the guard allocation free. *)
let[@zero_alloc] check fn buf off len =
  let buf_len = Base_bigstring.length buf in
  if off < 0 || len < 0 || len > buf_len - off then
    invalid_arg
      (Printf.sprintf "Zstdz.%s: %d bytes at offset %d outside a buffer of %d"
         fn len off buf_len)

(* error_name is stack allocated, so the exception payload is a copy.
   Bytes.of_string already produces a fresh heap buffer, so the unsafe
   cast back to string hands out nothing that is aliased. *)
let globalize (s @ local) = Bytes.unsafe_to_string (Bytes.of_string s)
let fail r = raise (Error (-r, globalize (error_name (-r))))

let compress ?(level = 3) ?(checksum = false) cctx ~src ~src_off ~src_len ~dst
    ~dst_off ~dst_len =
  check "compress" src src_off src_len;
  check "compress" dst dst_off dst_len;
  let r =
    compress_raw cctx src src_off src_len dst dst_off dst_len level checksum
  in
  if r < 0 then fail r else r

let decompress dctx ~src ~src_off ~src_len ~dst ~dst_off ~dst_len =
  check "decompress" src src_off src_len;
  check "decompress" dst dst_off dst_len;
  let r = decompress_raw dctx src src_off src_len dst dst_off dst_len in
  if r < 0 then fail r else r

let[@zero_alloc] content_size buf ~off ~len =
  check "content_size" buf off len;
  content_size_raw buf off len

let frame_info buf ~off ~len =
  check "frame_info" buf off len;
  exclave_ frame_info_raw buf off len
