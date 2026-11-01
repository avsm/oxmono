(*---------------------------------------------------------------------------
   Copyright (c) 2025 Anil Madhavapeddy <anil@recoil.org>. All rights reserved.
   SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

exception Error of int * string

external max_overhead_raw : unit -> int @@ portable
  = "bloscz_max_overhead"
  [@@noalloc]

external list_compressors : unit -> string @@ portable
  = "bloscz_list_compressors"

(* The two working stubs return the byte count written, or a negative
   blosc code. Neither may carry [@@noalloc]: both release the runtime
   lock, and reacquiring it can run a pending signal handler, which needs
   a frame descriptor the compiler only emits for an allocating call. *)

external compress_raw :
  string ->
  Base_bigstring.t ->
  int ->
  int ->
  Base_bigstring.t ->
  int ->
  int ->
  int ->
  int ->
  int ->
  int ->
  int
  @@ portable = "bloscz_compress_bytecode" "bloscz_compress_native"

external decompress_raw :
  Base_bigstring.t ->
  int ->
  int ->
  Base_bigstring.t ->
  int ->
  int ->
  int
  @@ portable = "bloscz_decompress_bytecode" "bloscz_decompress_native"

external buffer_sizes_raw :
  Base_bigstring.t -> int -> nbytes:int * cbytes:int * blocksize:int @@ portable
  = "bloscz_buffer_sizes"

external validate_raw : Base_bigstring.t -> int -> int -> int @@ portable
  = "bloscz_validate"
  [@@noalloc]

let max_overhead = max_overhead_raw ()

(* blosc_list_compressors is comma separated with no spaces. An empty
   list would split to one empty name, which is not a compressor. *)
let compressors () =
  match list_compressors () with
  | "" -> []
  | s -> String.split_on_char ',' s

(* The stubs trust their offsets, so every range reaches them checked.
   The subtraction cannot overflow: both operands are bigstring lengths
   or already-checked offsets. The message is built only on the failing
   branch, which is what keeps the guard allocation free. *)
let[@zero_alloc] check fn buf off len =
  let buf_len = Base_bigstring.length buf in
  if off < 0 || len < 0 || len > buf_len - off then
    invalid_arg
      (Printf.sprintf "Bloscz.%s: %d bytes at offset %d outside a buffer of %d"
         fn len off buf_len)

(* Blosc1 has no error name table, so these are this binding's own
   wording for the codes 1.21 returns. Compressing into too small a
   destination is the one failure blosc reports as a zero-length result
   rather than a code, and it reaches here as 0. *)
let what code =
  match code with
  | 0 -> "the destination is too small for the frame"
  | -1 -> "an unspecified failure, such as a corrupt or truncated frame"
  | -5 -> "the compressor is not supported by this build"
  | -10 -> "a parameter is outside its range"
  | _ -> "an error blosc does not name"

let fail fn code = raise (Error (code, Printf.sprintf "%s: %s" fn (what code)))

(* Guarded rather than passed on: blosc divides by [typesize], so a zero
   raises SIGFPE inside the C library, and it prints a diagnostic to
   stderr for a [level] or a [cname] it dislikes before returning a
   code. The longest name blosc knows is "blosclz", and the stub copies
   the name into a 32 byte stack buffer before releasing the runtime
   lock, so 31 is the limit here. *)
let compress ?(level = 5) ?(shuffle = `No) ?(blocksize = 0) cname ~typesize
    ~src ~src_off ~src_len ~dst ~dst_off ~dst_len =
  check "compress" src src_off src_len;
  check "compress" dst dst_off dst_len;
  if typesize < 1 then
    invalid_arg (Printf.sprintf "Bloscz.compress: typesize %d is below 1"
                   typesize);
  if level < 0 || level > 9 then
    invalid_arg
      (Printf.sprintf "Bloscz.compress: level %d is outside [0, 9]" level);
  if blocksize < 0 then
    invalid_arg
      (Printf.sprintf "Bloscz.compress: blocksize %d is negative" blocksize);
  let n = String.length cname in
  if n = 0 || n > 31 then
    invalid_arg
      (Printf.sprintf "Bloscz.compress: compressor name of %d bytes" n);
  let doshuffle = match shuffle with `No -> 0 | `Byte -> 1 | `Bit -> 2 in
  let r =
    compress_raw cname src src_off src_len dst dst_off dst_len level doshuffle
      typesize blocksize
  in
  if r <= 0 then fail "compress" r else r

let decompress ~src ~src_off ~src_len ~dst ~dst_off ~dst_len =
  check "decompress" src src_off src_len;
  check "decompress" dst dst_off dst_len;
  let r = decompress_raw src src_off src_len dst dst_off dst_len in
  if r < 0 then fail "decompress" r else r

(* blosc_cbuffer_sizes reads the header without being told how much it
   may read, so the header must be known to be there before the call. *)
let buffer_sizes buf ~off ~len =
  check "buffer_sizes" buf off len;
  if len < max_overhead then
    invalid_arg
      (Printf.sprintf "Bloscz.buffer_sizes: %d bytes cannot hold a %d byte \
                       header"
         len max_overhead);
  buffer_sizes_raw buf off

let validate buf ~off ~len =
  check "validate" buf off len;
  match validate_raw buf off len with n when n < 0 -> None | n -> Some n
