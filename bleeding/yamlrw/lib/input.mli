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

val is_eof : t -> bool
(** Check if at end of input *)

val mark : t -> Position.t
(** Mark current position for span creation *)

(** {2 Lookahead} *)

val peek : t -> char option
(** Peek at current character without advancing *)

val peek_exn : t -> char
(** Peek at current character, raising on EOF *)

val peek_nth : t -> int -> char option
(** Peek at nth character (0-indexed from current position) *)

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
(** Get remaining content from current position *)

val source : t -> string
(** Get a sample of the source for encoding detection *)

val byte_pos : t -> int
(** Get the byte position in the underlying stream *)
