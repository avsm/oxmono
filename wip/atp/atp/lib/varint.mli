(*---------------------------------------------------------------------------
   Copyright (c) 2025 Anil Madhavapeddy. All rights reserved.
   SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

(** Unsigned variable-length integer encoding (LEB128).

    Variable-length integers are used in CAR files and other IPLD formats to
    efficiently encode lengths and sizes. *)

(** {1 Errors} *)

type error = [ `Varint_overflow | `Varint_unterminated | `Varint_negative ]
(** Varint encoding/decoding errors. *)

val pp_error : error Fmt.t
(** Pretty-print an error. *)

type Eio.Exn.err += E of error  (** Eio exception wrapper for varint errors. *)

(** {1 Encoding} *)

val encode : int -> string
(** [encode n] encodes non-negative integer [n] as a varint string.

    @raise Eio.Io with [`Varint_negative] if [n < 0]. *)

val encode_to_buffer : Buffer.t -> int -> unit
(** [encode_to_buffer buf n] appends the varint encoding of [n] to [buf].

    @raise Eio.Io with [`Varint_negative] if [n < 0]. *)

val encoded_length : int -> int
(** [encoded_length n] returns the number of bytes needed to encode [n].

    Returns 1 for values 0-127, 2 for 128-16383, etc. *)

(** {1 Decoding} *)

val decode_string : string -> int -> int * int
(** [decode_string s off] decodes a varint from string [s] starting at offset
    [off]. Returns [(value, bytes_consumed)].

    @raise Eio.Io with [`Varint_overflow] if the value exceeds OCaml int.
    @raise Eio.Io with [`Varint_unterminated] if the string ends mid-varint. *)

val decode_bytes : bytes -> int -> int * int
(** [decode_bytes b off] decodes a varint from bytes [b] starting at offset
    [off]. Returns [(value, bytes_consumed)].

    @raise Eio.Io with [`Varint_overflow] if the value exceeds OCaml int.
    @raise Eio.Io with [`Varint_unterminated] if the bytes end mid-varint. *)
