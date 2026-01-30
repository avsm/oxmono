(*---------------------------------------------------------------------------
   Copyright (c) 2025 Anil Madhavapeddy <anil@recoil.org>. All rights reserved.
   SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

(** TOML parsing and encoding error types.

    This module defines structured error types for TOML parsing and encoding,
    with location tracking and pretty-printing support. *)

(** {1 Location} *)

(** Location in the input *)
type location = {
  line : int;
  column : int;
  file : string option;
}

val pp_location : Format.formatter -> location -> unit
val loc : ?file:string -> line:int -> column:int -> unit -> location

(** {1 Error Categories} *)

(** Lexer errors - low-level tokenization issues *)
type lexer_error =
  | Invalid_utf8
  | Incomplete_utf8
  | Invalid_escape of char
  | Incomplete_escape of string
  | Invalid_unicode_escape of string
  | Invalid_unicode_codepoint of int
  | Surrogate_codepoint of int
  | Bare_carriage_return
  | Control_character of int
  | Unterminated_string
  | Unterminated_comment
  | Too_many_quotes
  | Newline_in_string
  | Unexpected_character of char
  | Unexpected_eof

val pp_lexer_error : Format.formatter -> lexer_error -> unit

(** Number parsing errors *)
type number_error =
  | Leading_zero
  | Leading_underscore
  | Trailing_underscore
  | Double_underscore
  | Underscore_not_between_digits
  | Underscore_after_exponent
  | Missing_digit
  | Missing_digit_after_sign
  | Missing_digit_after_decimal
  | Missing_digit_after_exponent
  | Invalid_hex_digit
  | Invalid_octal_digit
  | Invalid_binary_digit

val pp_number_error : Format.formatter -> number_error -> unit

(** DateTime parsing errors *)
type datetime_error =
  | Invalid_month of int
  | Invalid_day of int * int
  | Invalid_hour of int
  | Invalid_minute of int
  | Invalid_second of int
  | Invalid_timezone_offset_hour of int
  | Invalid_timezone_offset_minute of int
  | Invalid_format of string

val pp_datetime_error : Format.formatter -> datetime_error -> unit

(** Semantic/table structure errors *)
type semantic_error =
  | Duplicate_key of string
  | Table_already_defined of string
  | Cannot_redefine_table_as_value of string
  | Cannot_redefine_array_as_value of string
  | Cannot_use_value_as_table of string
  | Cannot_extend_inline_table of string
  | Cannot_extend_closed_table of string
  | Cannot_extend_array_of_tables of string
  | Cannot_convert_table_to_array of string
  | Cannot_convert_array_to_table of string
  | Table_has_content of string
  | Conflicting_keys
  | Empty_key
  | Multiline_key

val pp_semantic_error : Format.formatter -> semantic_error -> unit

(** Syntax errors *)
type syntax_error =
  | Expected of string
  | Invalid_table_header
  | Invalid_array_of_tables_header
  | Unexpected_token of string
  | Unexpected_bare_key of string

val pp_syntax_error : Format.formatter -> syntax_error -> unit

(** Encoding errors *)
type encode_error =
  | Cannot_encode_inline_table
  | Not_a_table

val pp_encode_error : Format.formatter -> encode_error -> unit

(** {1 Combined Error Type} *)

(** All error kinds *)
type kind =
  | Lexer of lexer_error
  | Number of number_error
  | Datetime of datetime_error
  | Semantic of semantic_error
  | Syntax of syntax_error
  | Encode of encode_error

val pp_kind : Format.formatter -> kind -> unit

(** Full error with location *)
type t = {
  kind : kind;
  location : location option;
}

val make : ?location:location -> kind -> t
val pp : Format.formatter -> t -> unit
val to_string : t -> string

(** {1 Exception} *)

exception Error of t

(** {1 Raising Errors} *)

val raise_error : ?location:location -> kind -> 'a
val raise_lexer : ?location:location -> lexer_error -> 'a
val raise_number : ?location:location -> number_error -> 'a
val raise_datetime : ?location:location -> datetime_error -> 'a
val raise_semantic : ?location:location -> semantic_error -> 'a
val raise_syntax : ?location:location -> syntax_error -> 'a
val raise_encode : ?location:location -> encode_error -> 'a
