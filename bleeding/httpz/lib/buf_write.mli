(** This module provides unchecked writers for HTTP wire buffers.

    Each writer stores bytes beginning at [off] and returns the first offset
    after the new data. The caller must provide sufficient buffer space and
    values within each function's documented range; no bounds check is
    performed. The one range check is the sign of the integer given to {!int}
    and {!hex}, where a negative value would otherwise write a wrong spelling
    into a header rather than fail. Offsets are [int16#] to match {!Buf_read}
    and to keep the arithmetic unboxed. *)

val char : bytes -> off:int16# -> char -> int16# @@ portable
(** [char buf ~off byte] is [off + 1] after writing [byte]. *)

val blit :
  bytes -> off:int16# -> src:local_ bytes -> src_off:int16# -> len:int -> int16#
  @@ portable
(** [blit buf ~off ~src ~src_off ~len] is [off + len] after copying [len] bytes
    from [src] beginning at [src_off]. *)

val string : bytes -> off:int16# -> local_ string -> int16# @@ portable
(** [string buf ~off value] is [off + String.length value] after writing
    [value]. *)

val crlf : bytes -> off:int16# -> int16# @@ portable
(** [crlf buf ~off] is [off + 2] after writing ["\r\n"]. *)

val[@zero_alloc opt] int : bytes -> off:int16# -> int -> int16# @@ portable
(** [int buf ~off value] is the next offset after writing non-negative [value]
    in decimal. It raises [Invalid_argument] on a negative [value]. *)

val[@zero_alloc opt] int64 :
  bytes -> off:int16# -> int64# -> int16# @@ portable
(** [int64 buf ~off value] is the next offset after writing non-negative [value]
    in decimal. It raises [Invalid_argument] on a negative [value]. *)

val hex : bytes -> off:int16# -> int -> int16# @@ portable
(** [hex buf ~off value] is the next offset after writing non-negative [value]
    in lowercase hexadecimal. It raises [Invalid_argument] on a negative
    [value]. *)

val digit_pairs : string @@ portable
(** [digit_pairs] is the 200-byte table of two-digit decimal spellings, so that
    the bytes at [2 * n] and [2 * n + 1] are the tens and units of [n] for [n]
    from [0] through [99]. *)

val i16 : int -> int16# @@ portable
(** [i16 value] is [value] as an [int16#]. *)

val to_int : int16# -> int @@ portable
(** [to_int value] is [value] as an [int]. *)
