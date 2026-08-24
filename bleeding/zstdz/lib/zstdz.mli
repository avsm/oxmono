(*---------------------------------------------------------------------------
   Copyright (c) 2025 Anil Madhavapeddy <anil@recoil.org>. All rights reserved.
   SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

(** Zstandard compression.

    Bindings to the system libzstd. Compression and decompression are one
    shot: the caller sizes the destination and the codec writes into it, so a
    pipeline that already owns its buffers never pays for an intermediate
    copy. Data lives in [Base_bigstring.t], which is off heap and readable
    from the stubs without copying.

    Sizes are OCaml [int]. A zstd size fits in 63 bits with room to spare.

    Everything here is [portable]. The stubs close over no state, and a
    context is passed in rather than held at module level.

    {2 Quick Start}

    {[
      let cctx = Zstdz.create_cctx ()
      let dctx = Zstdz.create_dctx ()

      let round_trip (src : Base_bigstring.t) =
        let src_len = Base_bigstring.length src in
        let dst = Base_bigstring.create (Zstdz.compress_bound src_len) in
        let n =
          Zstdz.compress ~level:3 cctx ~src ~src_off:0 ~src_len ~dst
            ~dst_off:0 ~dst_len:(Base_bigstring.length dst)
        in
        let out = Base_bigstring.create src_len in
        Zstdz.decompress dctx ~src:dst ~src_off:0 ~src_len:n ~dst:out
          ~dst_off:0 ~dst_len:src_len
    ]}

    {2 Contexts} *)

@@ portable

type cctx
(** A compression context. It holds the working memory libzstd reuses between
    calls, and is freed when the value is collected.

    A context is not thread safe and carries no lock. Use one per domain. It
    is safe to share one across Eio fibers of a single domain, because a fiber
    cannot switch inside a C call. *)

type dctx
(** A decompression context, with the same sharing rules as {!cctx}. *)

val create_cctx : unit -> cctx
(** [create_cctx ()] is a fresh compression context.

    @raise Out_of_memory if libzstd cannot allocate it. *)

val create_dctx : unit -> dctx
(** [create_dctx ()] is a fresh decompression context.

    @raise Out_of_memory if libzstd cannot allocate it. *)

(** {2 Errors} *)

exception Error of int * string
(** [Error (code, name)] is raised by {!compress} and {!decompress}. [code] is
    a positive libzstd error code and [name] is {!error_name} [code] copied to
    the heap. *)

val error_name : int -> string @ local
(** [error_name code] is libzstd's description of error [code]. The result is
    allocated in the caller's stack region, so naming an error on a hot path
    does not touch the minor heap, and the string must be consumed or copied
    before the enclosing region ends.

    A [code] that names no error is ["No error detected"], and one outside the
    range libzstd knows is ["Unspecified error code"]. *)

(** {2 Compression} *)

val compress_bound : int -> int
(** [compress_bound n] is the largest frame libzstd can produce from [n] bytes
    of input. A destination of this size makes {!compress} incapable of
    failing for want of room. *)

val compress :
  ?level:int ->
  ?checksum:bool ->
  cctx ->
  src:Base_bigstring.t ->
  src_off:int ->
  src_len:int ->
  dst:Base_bigstring.t ->
  dst_off:int ->
  dst_len:int ->
  int
(** [compress cctx ~src ~src_off ~src_len ~dst ~dst_off ~dst_len] compresses
    [src_len] bytes from [src] at [src_off] into [dst] at [dst_off], writing
    at most [dst_len] bytes, and is the number of bytes written. The result is
    one complete frame.

    [level] defaults to 3, which is libzstd's own default. It is passed on
    unchanged: libzstd clamps it to the range it supports, currently up to 22,
    reads 0 as the default, and accepts negative levels as the fast modes.
    [checksum] defaults to [false] and, when [true], adds the 4 byte frame
    checksum that {!decompress} then verifies.

    Both parameters are applied to [cctx] on every call, so a context carries
    nothing between calls.

    The runtime lock is released for the duration of the compression when
    [src_len + dst_len] exceeds 64 KiB, so a large frame does not stop other
    domains. The bigstrings must not be resized or freed by another domain
    while the call runs.

    @raise Invalid_argument if a range falls outside its bigstring.
    @raise Error if libzstd fails, in particular when [dst_len] is too small. *)

(** {2 Decompression} *)

val decompress :
  dctx ->
  src:Base_bigstring.t ->
  src_off:int ->
  src_len:int ->
  dst:Base_bigstring.t ->
  dst_off:int ->
  dst_len:int ->
  int
(** [decompress dctx ~src ~src_off ~src_len ~dst ~dst_off ~dst_len]
    decompresses the frame in [src] into [dst], and is the number of bytes
    written. [src] must hold exactly one complete frame.

    The runtime lock is released under the same rule as {!compress}.

    @raise Invalid_argument if a range falls outside its bigstring.
    @raise Error if the frame is corrupt or truncated, if its checksum fails,
      or if [dst_len] is smaller than the decompressed size. *)

(** {2 Frame inspection} *)

val content_size : Base_bigstring.t -> off:int -> len:int -> int64#
(** [content_size buf ~off ~len] is the decompressed size recorded in the
    header of the frame at [off], as an unboxed [int64#]. [-1L] means the
    header does not record a size, which is what a streaming encoder produces.
    [-2L] means the bytes are not the start of a readable frame, either
    because they are not a zstd frame or because [len] stops short of the end
    of the header.

    @raise Invalid_argument if the range falls outside [buf]. *)

type frame_info = {
  content_size : int;  (** Decompressed size, or [-1] unknown, [-2] error. *)
  window_size : int;  (** Buffer a decoder must keep to read the frame. *)
  dict_id : int;  (** Dictionary the frame needs, or [0] for none. *)
  has_checksum : bool;  (** Whether a frame checksum follows the content. *)
}
(** Frame header fields. Every field is an immediate, which is what lets
    {!frame_info} build the record on the stack without boxing anything.
    libzstd types [content_size] and [window_size] as unsigned 64 bit and
    [dict_id] as unsigned 32 bit, all of which fit an OCaml [int] for any
    frame a 64 bit machine can decode. *)

val frame_info : Base_bigstring.t -> off:int -> len:int -> frame_info @ local
(** [frame_info buf ~off ~len] reads the header of the frame at [off]. The
    record is allocated in the caller's stack region, so probing a frame does
    not touch the minor heap, and the record must be consumed or copied field
    by field before the enclosing region ends.

    A header that cannot be read gives [content_size = -2] with the remaining
    fields zero, under the same conditions as {!content_size}. This is
    reported rather than raised so that the function allocates nothing at all
    on the OCaml heap.

    @raise Invalid_argument if the range falls outside [buf]. *)
