(*---------------------------------------------------------------------------
  Copyright (c) 2025 Anil Madhavapeddy <anil@recoil.org>. All rights reserved.
  SPDX-License-Identifier: ISC
 ---------------------------------------------------------------------------*)

(** Character input source with lookahead, based on Bytes.Reader.t

    This module wraps a bytesrw [Bytes.Reader.t] to provide
    character-by-character access with lookahead for the YAML scanner. *)

(** {2 Re-exported Character Classification} *)

include module type of Char_class

(** {2 Input Type} *)

type t

(** {2 Constructors} *)

val of_reader : ?initial_position:Position.t -> Bytesrw.Bytes.Reader.t -> t
(** Create input from a Bytes.Reader.t *)

val of_string : string -> t
(** Create input from a string *)

(** {2 Position and State} *)

val position : t -> Position.t
(** Get current position *)

val line : t -> int
[@@zero_alloc]
(** [line t] is the current one-based line number. *)

val column : t -> int
[@@zero_alloc]
(** [column t] is the current one-based byte column. *)

val is_eof : t -> bool
(** Check if at end of input *)

val mark : t -> Position.t
(** Mark current position for span creation *)

(** {2 Lookahead} *)

val peek : t -> char option
(** Peek at current character without advancing *)

val peek_or_null : t -> char or_null
(** [peek_or_null t] is the current byte, or [Null] at end of input. Buffered
    reads do not allocate. *)

val peek_nth_or_null : t -> int -> char or_null
(** [peek_nth_or_null t n] is the byte [n] positions ahead, or [Null] at end of
    input. Looking within the current slice does not read ahead. Raises
    [Invalid_argument] if [n] is negative or at least [Sys.max_string_length]. *)

val peek_back_or_null : t -> char or_null
[@@zero_alloc]
(** [peek_back_or_null t] is the last consumed byte, or [Null] before the first
    byte is consumed. It remains available across slice boundaries. *)

val next_or_null : t -> char or_null
(** [next_or_null t] consumes the current byte, or returns [Null] at end of
    input. Buffered reads do not allocate. *)

val peek_exn : t -> char
(** Peek at current character, raising on EOF *)

val peek_nth : t -> int -> char option
(** [peek_nth t n] is the byte [n] positions ahead, or [None] at end of input.
    Raises [Invalid_argument] if [n] is negative or at least
    [Sys.max_string_length]. *)

val peek_string : t -> int -> string
(** Peek at up to n characters as a string *)

val peek_back : t -> char option
(** Get the character before the current position *)

(** {2 Consumption} *)

val next : t -> char option
(** Consume and return next character *)

val next_exn : t -> char
(** Consume and return next character, raising on EOF *)

val skip : t -> int -> unit
(** Skip n characters *)

val skip_while : t -> (char -> bool) -> unit
(** Skip characters while predicate holds *)

val consume_break : t -> unit
(** Consume line break, handling \r\n as single break *)

(** {2 Predicates} *)

val next_is : (char -> bool) -> t -> bool
(** Check if next char satisfies predicate *)

val next_is_break : t -> bool
val next_is_blank : t -> bool
val next_is_whitespace : t -> bool
val next_is_digit : t -> bool
val next_is_hex : t -> bool
val next_is_alpha : t -> bool
val next_is_indicator : t -> bool

val at_document_boundary : t -> bool
(** Check if at document boundary (--- or ...) *)

(** {2 Utilities} *)

val remaining : t -> string
(** [remaining t] consumes and returns all bytes from the current position. It
    advances the cursor to end of input. *)

val source : t -> string
(** Get a sample of the source for encoding detection *)

val byte_pos : t -> int
(** Get the byte position in the underlying stream *)
