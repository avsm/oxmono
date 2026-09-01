(** This module represents parsed HTTP fields.

    A field contains borrowed spans into the parse buffer. Known names use a {!Name.t}
    variant. For {!Name.Other}, inspect {!name_span} to recover or compare the original
    field name. Field values exclude surrounding optional whitespace. *)

(** This module represents HTTP field names recognized by Httpz. *)
module Name = Header_name

module Syntax : sig
  (** Owned-string helpers for the common RFC 9110 field grammar. *)

  val[@zero_alloc] is_token : local_ string -> bool @@ portable
  (** [is_token s] is whether [s] is a non-empty HTTP token. *)

  val[@zero_alloc] is_token_sub : local_ string -> pos:int -> len:int -> bool @@ portable
  (** [is_token_sub s ~pos ~len] is whether the selected substring is a
      non-empty HTTP token. Invalid slice bounds return [false]. The substring
      is not allocated. *)

  val[@zero_alloc] is_field_value : local_ string -> bool @@ portable
  (** [is_field_value s] is whether every byte of [s] is permitted in an
      unfolded HTTP field value. *)

  val quote_string : local_ string -> string @@ portable
  (** [quote_string s] surrounds [s] with quotes and escapes quote and
      backslash bytes.

      @raise Stdlib.Invalid_argument if [s] contains a byte forbidden from an
      HTTP quoted pair. *)

  val unquote_string : local_ string -> string option @@ portable
  (** [unquote_string s] validates and decodes one complete HTTP quoted string,
      or returns [None]. Quoted pairs are unescaped. *)

  val[@zero_alloc] is_quoted_string_sub :
    local_ string -> pos:int -> len:int -> bool @@ portable
  (** [is_quoted_string_sub s ~pos ~len] validates one complete HTTP quoted
      string in the selected substring. Invalid slice bounds return [false].
      The substring is not allocated. *)

  val[@zero_alloc] qvalue_sub : local_ string -> pos:int -> len:int -> int @@ portable
  (** [qvalue_sub s ~pos ~len] parses an RFC 9110 quality value from the
      selected substring and returns thousandths from [0] through [1000]. It
      returns [-1] for malformed input or invalid slice bounds and allocates no
      substring. *)
end

(** A [t] is a parsed HTTP field. *)
type t =
  { name : Name.t (** [name] is the recognized name, or {!Name.Other}. *)
  ; name_span : Span.t
  (** [name_span] is the original field-name span. It is primarily useful for
      {!Name.Other}. *)
  ; value : Span.t
  (** [value] is the field value without surrounding optional whitespace. *)
  }

(** [find headers name] is the first field whose recognized name is [name]. It does not
    match {!Name.Other}; use {!find_string} for extension names. *)
val find : t list @ local -> Name.t -> t option @ local @@ portable

(** [find_string buf headers name] is the first field named [name], matching both known
    and extension names without regard to ASCII case. *)
val find_string : local_ bytes -> t list @ local -> string -> t option @ local @@ portable

(** [to_string_pair buf header] is [header]'s name and value copied into strings. *)
val to_string_pair : bytes -> t -> string * string @@ portable

(** [to_string_pairs buf headers] is every field copied into a list of name-value pairs in
    input list order. *)
val to_string_pairs : bytes -> t list -> (string * string) list @@ portable

(** [to_string_pairs_local buf headers] is {!to_string_pairs} over a local [headers] list.
    The pairs it returns are global. *)
val to_string_pairs_local : bytes -> t list @ local -> (string * string) list @@ portable

(** [pp formatter header] is the formatter operation that prints the parsed variants and
    span positions. *)
val pp : Stdlib.Format.formatter -> t -> unit @@ portable

(** [pp_with_buf buf formatter header] is the formatter operation that prints [header] as
    a name and value. *)
val pp_with_buf : bytes -> Stdlib.Format.formatter -> t -> unit @@ portable
