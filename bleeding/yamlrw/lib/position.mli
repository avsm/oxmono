(*---------------------------------------------------------------------------
  Copyright (c) 2025 Anil Madhavapeddy <anil@recoil.org>. All rights reserved.
  SPDX-License-Identifier: ISC
 ---------------------------------------------------------------------------*)

(** Position tracking for source locations *)

type t = {
  index : int;  (** Byte offset from start *)
  line : int;  (** 1-indexed line number *)
  column : int;  (** 1-indexed column number *)
}

val initial : t
(** Initial position (index=0, line=1, column=1) *)

val advance_byte : t -> t
(** Advance by one byte (increments index and column) *)

val advance_line : t -> t
(** Advance to next line (increments index and line, resets column to 1) *)

val advance_char : char -> t -> t
(** Advance by one character, handling newlines appropriately *)

val advance_utf8 : Uchar.t -> t -> t
(** Advance by one Unicode character, handling newlines and multi-byte
    characters *)

val advance_bytes : int -> t -> t
(** Advance by n bytes *)

val pp : Format.formatter -> t -> unit
(** Pretty-print a position *)

val to_string : t -> string
(** Convert position to string *)

val compare : t -> t -> int
(** Compare two positions by index *)

val equal : t -> t -> bool
(** Test equality of two positions *)
