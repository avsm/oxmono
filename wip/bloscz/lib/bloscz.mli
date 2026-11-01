(*---------------------------------------------------------------------------
   Copyright (c) 2025 Anil Madhavapeddy <anil@recoil.org>. All rights reserved.
   SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

(** Blosc1 compression.

    Bindings to the system C-Blosc1, 1.21 or later. Only the context
    interface is bound. It takes no global lock, holds no state between
    calls and needs no initialisation, so a call is a pure function of
    its arguments and safe from any domain.

    Blosc is a container. It splits the input into blocks, optionally
    shuffles the bytes or bits of each block so that like-valued bytes of
    an element sit together, and hands each block to an inner compressor
    named by {!compressors}. A frame carries a 16 byte header recording
    the decompressed size, so a reader does not have to be told it.

    Compression and decompression are one shot: the caller sizes the
    destination and the codec writes into it. Buffers are
    [Base_bigstring.t], which is off heap and read by the stubs without
    copying. Everything here is [portable].

    {2 Quick Start}

    {[
      let round_trip (src : Base_bigstring.t) =
        let src_len = Base_bigstring.length src in
        let dst_len = src_len + Bloscz.max_overhead in
        let dst = Base_bigstring.create dst_len in
        let n =
          Bloscz.compress "zstd" ~shuffle:`Byte ~typesize:4 ~src ~src_off:0
            ~src_len ~dst ~dst_off:0 ~dst_len
        in
        let ~nbytes, ~cbytes:_, ~blocksize:_ =
          Bloscz.buffer_sizes dst ~off:0 ~len:n
        in
        let out = Base_bigstring.create nbytes in
        Bloscz.decompress ~src:dst ~src_off:0 ~src_len:n ~dst:out ~dst_off:0
          ~dst_len:nbytes
    ]}

    {2 Errors} *)

@@ portable

exception Error of int * string
(** [Error (code, what)] is raised by {!compress} and {!decompress}.
    [code] is the value the C function returned and [what] is this
    binding's own wording for it, since Blosc1 publishes no error name
    table. Match on [code] rather than on [what].

    [0] comes only from {!compress} and means the frame did not fit
    [dst_len]. [-5] is a compressor this build of the library does not
    have, [-10] a parameter outside its range and [-1] an unspecified
    failure, which is what a corrupt frame, a truncated frame and a
    {!decompress} destination that is too small all give. Any other code
    is described only in general terms and still reaches the caller in
    the payload. *)

(** {2 Library facts} *)

val max_overhead : int
(** [max_overhead] is the largest number of bytes blosc adds to an
    incompressible input, currently 16, the size of the frame header. A
    destination of [n + max_overhead] bytes makes {!compress} incapable
    of failing for want of room. *)

val compressors : unit -> string list
(** [compressors ()] are the inner compressors this build of the C
    library supports, in its own order. A name outside this list makes
    {!compress} raise [Error (-5, _)]. The full set is ["blosclz"],
    ["lz4"], ["lz4hc"], ["snappy"], ["zlib"] and ["zstd"], and a
    distribution may build without some of them, so a caller that needs a
    particular compressor must check for it here rather than assume it. *)

(** {2 Compression} *)

val compress :
  ?level:int ->
  ?shuffle:[ `No | `Byte | `Bit ] ->
  ?blocksize:int ->
  string ->
  typesize:int ->
  src:Base_bigstring.t ->
  src_off:int ->
  src_len:int ->
  dst:Base_bigstring.t ->
  dst_off:int ->
  dst_len:int ->
  int
(** [compress cname ~typesize ~src ~src_off ~src_len ~dst ~dst_off
    ~dst_len] compresses [src_len] bytes from [src] at [src_off] into
    [dst] at [dst_off], writing at most [dst_len] bytes, and is the
    number of bytes written. The result is one complete frame. An empty
    input still produces the 16 byte header.

    [cname] names the inner compressor and must be one of
    {!compressors}. [typesize] is the size in bytes of one element of the
    input, which is what the shuffle filter permutes around. It must be
    at least 1 even under [`No]. A size above 255 is accepted and treated
    as an unstructured byte stream.

    [level] defaults to 5 and must be in [0, 9], where 0 stores the
    blocks uncompressed. [shuffle] defaults to [`No]. [`Byte] gathers
    the like-numbered bytes of each element together, [`Bit] does the
    same one bit at a time, and both are recorded in the frame so that
    {!decompress} undoes them without being told. [blocksize] is the
    number of bytes blosc compresses at a time and defaults to 0, which
    lets it choose from [typesize] and [level].

    The runtime lock is released for the duration of the compression
    when [src_len + dst_len] exceeds 64 KiB, so a large frame does not
    stop other domains. The bigstrings must not be resized or freed by
    another domain while the call runs.

    @raise Invalid_argument if a range falls outside its bigstring, if
      [typesize] is below 1, if [level] is outside [0, 9], if
      [blocksize] is negative, or if [cname] is empty or longer than 31
      bytes, which is longer than any name blosc knows.
    @raise Error if blosc fails, with [code] 0 when [dst_len] leaves no
      room for the frame. *)

(** {2 Decompression} *)

val decompress :
  src:Base_bigstring.t ->
  src_off:int ->
  src_len:int ->
  dst:Base_bigstring.t ->
  dst_off:int ->
  dst_len:int ->
  int
(** [decompress ~src ~src_off ~src_len ~dst ~dst_off ~dst_len]
    decompresses the frame in [src] into [dst], writing at most
    [dst_len] bytes, and is the number of bytes written. Decompression
    never writes beyond [dst_len].

    The result is 0 both for a frame whose payload is empty and for a
    failure blosc declines to code. Size the destination from
    {!buffer_sizes} or {!validate} and compare the result against it to
    tell the two apart.

    The runtime lock is released under the same rule as {!compress}.

    @raise Invalid_argument if a range falls outside its bigstring.
    @raise Error if the frame is corrupt or truncated, or if [dst_len]
      is smaller than the decompressed size. *)

(** {2 Frame inspection} *)

val buffer_sizes :
  Base_bigstring.t ->
  off:int ->
  len:int ->
  nbytes:int * cbytes:int * blocksize:int
(** [buffer_sizes buf ~off ~len] reads the header of the frame at [off]:
    [nbytes] is the decompressed size, [cbytes] the size of the whole
    frame and [blocksize] the size of one compressed block. A header
    whose format this build does not know gives three zeroes.

    [len] must be at least 16, the header length, because the C function
    reads the header without being told how far it may read. Nothing else
    about the frame is checked, so a header that survives this can still
    be a lie about the bytes that follow. Use {!validate} before
    decompressing anything a stranger wrote.

    @raise Invalid_argument if the range falls outside [buf] or if [len]
      is below 16. *)

val validate : Base_bigstring.t -> off:int -> len:int -> int option
(** [validate buf ~off ~len] is [Some nbytes] when the [len] bytes at
    [off] are one complete blosc frame that is safe to hand to
    {!decompress}, and [None] otherwise. [nbytes] is the decompressed
    size.

    [len] must be the length of the frame exactly. A range that runs
    past the end of the frame is rejected along with one that stops
    short of it, so a caller holding a larger buffer must take
    [cbytes] from {!buffer_sizes} first.

    A frame that validates can still fail to decompress, because the
    check is of the header and the block offsets rather than of the
    compressed bytes themselves. What it does guarantee is that
    decompressing stays inside the buffer.

    @raise Invalid_argument if the range falls outside [buf]. *)
