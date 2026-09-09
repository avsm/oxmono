(*---------------------------------------------------------------------------
  Copyright (c) 2025 Anil Madhavapeddy <anil@recoil.org>. All rights reserved.
  SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

(** TOML error helpers. Extends {!Loc.module-Error} with typed TOML error kinds
    and raising helpers used by the lexer, parser and encoder. *)

(** {1:payloads Typed error payloads} *)

(** Lexer errors - low-level tokenization issues. *)
type lexer_error =
  | Invalid_utf8
  | Incomplete_utf8
  | Invalid_escape of char
  | Incomplete_escape of string  (** e.g., "\\x", "\\u", "\\U" *)
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
(** [pp_lexer_error ppf e] formats a {!lexer_error}. *)

(** Number parsing errors. *)
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
(** [pp_number_error ppf e] formats a {!number_error}. *)

(** Datetime parsing errors. *)
type datetime_error =
  | Month of int
  | Day of int * int  (** day, month *)
  | Hour of int
  | Minute of int
  | Second of int
  | Timezone_offset_hour of int
  | Timezone_offset_minute of int
  | Format of string  (** expected format description *)

val pp_datetime_error : Format.formatter -> datetime_error -> unit
(** [pp_datetime_error ppf e] formats the invalid TOML date/time component or
    shape. *)

(** Semantic / table structure errors. *)
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
(** [pp_semantic_error ppf e] formats a {!semantic_error}. *)

(** Syntax errors. *)
type syntax_error =
  | Expected of string
  | Invalid_table_header
  | Invalid_array_of_tables_header
  | Unexpected_token of string
  | Unexpected_bare_key of string

val pp_syntax_error : Format.formatter -> syntax_error -> unit
(** [pp_syntax_error ppf e] formats a {!syntax_error}. *)

(** Encoding errors. *)
type encode_error = Cannot_encode_inline_table | Not_a_table

val pp_encode_error : Format.formatter -> encode_error -> unit
(** [pp_encode_error ppf e] formats an {!encode_error}. *)

(** {1:kinds Error kinds}

    TOML error kinds extend the shared extensible {!Loc.Error.type-kind}. Each
    constructor carries one of the typed payloads above. *)

type kind = Loc.Error.kind = ..
(** Alias re-opens the extensible variant under this module's name. *)

type Loc.Error.kind +=
  | Lexer of lexer_error
  | Number of number_error
  | Datetime of datetime_error
  | Semantic of semantic_error
  | Syntax of syntax_error
  | Encode of encode_error
  | Sort_mismatch of { exp : Sort.t; fnd : Sort.t }
  | Kinded_sort_mismatch of { exp : string; fnd : Sort.t }

val string_of_kind : kind -> string
(** [string_of_kind k] renders [k] via the printers registered with
    {!Loc.Error.register_kind_printer}. *)

(** {1:errors Errors}

    A full error is a {!Loc.Context.t} (path + sort labels), a {!Loc.Meta.t}
    (source location + whitespace) and a {!type-kind}. *)

type t = Loc.Error.t = { ctx : Loc.Context.t; meta : Loc.Meta.t; kind : kind }

val v : ctx:Loc.Context.t -> meta:Loc.Meta.t -> kind -> t
(** [v ~ctx ~meta k] is a fresh error. *)

val msg : ctx:Loc.Context.t -> meta:Loc.Meta.t -> string -> t
(** [msg ~ctx ~meta s] is an error with kind [Loc.Error.Msg s]. *)

val raise : ctx:Loc.Context.t -> meta:Loc.Meta.t -> kind -> 'a
(** [raise ~ctx ~meta k] raises [Loc.Error.Error (v ~ctx ~meta k)]. *)

val fail : Loc.Meta.t -> string -> 'a
(** [fail meta s] raises with empty context and string [s]. *)

val failf : Loc.Meta.t -> ('a, Format.formatter, unit, 'b) format4 -> 'a
(** [failf meta fmt] is {!fail} with a formatted message. *)

val expected : Loc.Meta.t -> string -> fnd:string -> 'a
(** [expected meta exp ~fnd] raises ["Expected exp but found fnd"]. *)

val push_array : string Loc.node -> int Loc.node -> t -> 'a
(** [push_array sort n e] re-raises [e] after pushing an array index onto its
    context. *)

val push_object : string Loc.node -> string Loc.node -> t -> 'a
(** [push_object sort n e] re-raises [e] after pushing an object member onto its
    context. *)

val adjust_context :
  first_byte:Loc.byte_pos ->
  first_line_num:Loc.line_num ->
  first_line_byte:Loc.byte_pos ->
  t ->
  'a
(** [adjust_context] re-raises [e] with its innermost context's first position
    updated. *)

val pp : Format.formatter -> t -> unit
(** [pp] formats an error with its location, message and context, in that order;
    see {!Loc.Error.pp}. *)

val to_string : t -> string
(** [to_string e] is {!pp} as a string. *)

val pp_label : Format.formatter -> unit -> unit
(** [pp_label ppf ()] prints ["Error:"] (red/bold on ANSI formatters). *)

(** {1:helpers Raising helpers}

    One helper per payload category. All take a {!Loc.Meta.t}: the parser builds
    one from its current position. *)

val raise_lexer : meta:Loc.Meta.t -> lexer_error -> 'a
(** [raise_lexer ~meta e] raises a {!extension-Lexer} carrying [e]. *)

val raise_number : meta:Loc.Meta.t -> number_error -> 'a
(** [raise_number ~meta e] raises a {!extension-Number} carrying [e]. *)

val raise_datetime : meta:Loc.Meta.t -> datetime_error -> 'a
(** [raise_datetime ~meta e] raises a {!extension-Datetime} carrying [e]. *)

val raise_semantic : meta:Loc.Meta.t -> semantic_error -> 'a
(** [raise_semantic ~meta e] raises a {!extension-Semantic} carrying [e]. *)

val raise_syntax : meta:Loc.Meta.t -> syntax_error -> 'a
(** [raise_syntax ~meta e] raises a {!extension-Syntax} carrying [e]. *)

val raise_encode : meta:Loc.Meta.t -> encode_error -> 'a
(** [raise_encode ~meta e] raises an {!extension-Encode} carrying [e]. *)

(** {1:shape Shape-error helpers}

    Generic helpers for the recurring shape categories a decoder hits — typed
    sort mismatches, object member issues, index bounds, numeric ranges. *)

val sort : Loc.Meta.t -> exp:Sort.t -> fnd:Sort.t -> 'a
(** [sort meta ~exp ~fnd] raises {!Sort_mismatch}. *)

val kinded_sort : Loc.Meta.t -> exp:string -> fnd:Sort.t -> 'a
(** [kinded_sort meta ~exp ~fnd] raises {!Kinded_sort_mismatch}. *)

val missing_mems :
  Loc.Meta.t -> kinded_sort:string -> exp:string list -> fnd:string list -> 'a
(** [missing_mems meta ~kinded_sort ~exp ~fnd] raises listing [exp] members that
    were missing from a table of [kinded_sort]. *)

val unexpected_mems :
  Loc.Meta.t ->
  kinded_sort:string ->
  exp:string list ->
  fnd:(string * Loc.Meta.t) list ->
  'a
(** [unexpected_mems meta ~kinded_sort ~exp ~fnd] raises listing the unexpected
    members in [fnd] for a table of [kinded_sort]. *)

val unexpected_case_tag :
  Loc.Meta.t ->
  kinded_sort:string ->
  mem_name:string ->
  exp:string list ->
  fnd:string ->
  'a
(** [unexpected_case_tag meta ~kinded_sort ~mem_name ~exp ~fnd] raises when the
    tag member [mem_name] carries [fnd] outside the allowed [exp] values. *)

val index_out_of_range : Loc.Meta.t -> n:int -> len:int -> 'a
(** [index_out_of_range meta ~n ~len] raises ["Index n out of range [0;len-1]"].
*)

val number_range : Loc.Meta.t -> kind:string -> float -> 'a
(** [number_range meta ~kind n] raises ["Number n not in kind range"]. *)

val parse_string_number : Loc.Meta.t -> kind:string -> string -> 'a
(** [parse_string_number meta ~kind s] raises
    ["String s does not parse to a kind value"]. *)

val integer_range : Loc.Meta.t -> kind:string -> int -> 'a
(** [integer_range meta ~kind n] raises ["Integer n not in kind range"]. *)

val no_decoder : Loc.Meta.t -> kind:string -> 'a
(** [no_decoder meta ~kind] raises ["No decoder for kind"]. *)

val no_encoder : Loc.Meta.t -> kind:string -> 'a
(** [no_encoder meta ~kind] raises ["No encoder for kind"]. *)

val decode_todo : Loc.Meta.t -> kind_opt:string -> 'a
(** [decode_todo meta ~kind_opt] raises ["TODO: decode kind_opt"]. *)

val encode_todo : Loc.Meta.t -> kind_opt:string -> 'a
(** [encode_todo meta ~kind_opt] raises ["TODO: encode kind_opt"]. *)
