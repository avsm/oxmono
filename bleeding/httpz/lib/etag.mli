(** This module provides HTTP entity tags.

    An entity tag is an opaque validator enclosed in double quotes. A weak tag begins with
    [W/] and can establish semantic equivalence; a strong tag can also establish
    byte-for-byte equivalence. Their syntax and comparison rules are defined by
    {{:https://www.rfc-editor.org/rfc/rfc9110.html#section-8.8.3} RFC 9110, Section 8.8.3}.

    Parsed tags borrow their opaque bytes from the input buffer. *)

(** A [t] is a parsed entity tag. *)
type t =
  #{ weak : bool (** [weak] is [true] when the tag has the case-sensitive [W/] prefix. *)
   ; off : int16#
   (** [off] is the first byte of the opaque value, after the opening quote. *)
   ; len : int16# (** [len] is the opaque value length, excluding quotes. *)
   }

(** A [status] is the result of parsing a single entity tag. *)
type status =
  | Valid (** [Valid] means one entity tag was parsed. *)
  | Invalid (** [Invalid] means no valid entity tag was parsed. *)

val[@zero_alloc] valid_tag_char : char# -> bool @@ portable
(** [valid_tag_char byte] tests the RFC 9110 [etagc] grammar for one opaque
    entity-tag byte. Quotes, controls and space are excluded. *)

(** [parse buf span] is [(Valid, tag)] for one quoted entity tag with an optional
    case-sensitive [W/] prefix. It returns {!Invalid} and {!empty} when the required
    delimiters or valid [etagc] bytes are absent. *)
val parse : local_ bytes -> Span.t -> #(status * t) @@ portable
[@@zero_alloc opt]

(** [empty] is the zero-length strong tag used as an invalid-result placeholder. *)
val empty : t @@ portable

(** [to_string buf tag] is a copy of [tag]'s opaque value without its quotes or weak
    prefix. *)
val to_string : local_ bytes -> t -> string @@ portable

(** A [match_condition] is a parsed If-Match or If-None-Match condition. *)
type match_condition =
  | Any (** [Any] means the wildcard [*]. *)
  | Tags (** [Tags] means one or more tags were stored in the caller's array. *)
  | Empty (** [Empty] means no valid tag was stored. *)

(** [max_tags] is the maximum number of tags stored from one field, 16. *)
val max_tags : int16# @@ portable

(** [parse_match_header buf span tags] is [(condition, count)] for [*] or a
    comma-separated tag list. It stores at most [min max_tags (Array.length tags)] valid
    tags. A malformed member or input beyond that capacity rejects the complete field as
    [(Empty, 0)]. *)
val parse_match_header
  :  local_ bytes
  -> Span.t
  -> local_ t array
  -> #(match_condition * int16#)
  @@ portable

(** [strong_match buf left right] is [true] when both tags are strong and their opaque
    values are identical. *)
val strong_match : local_ bytes -> t -> t -> bool @@ portable

(** [weak_match buf left right] is [true] when the tags' opaque values are identical,
    regardless of their weak flags. *)
val weak_match : local_ bytes -> t -> t -> bool @@ portable

(** [matches_any_weak buf tag tags ~count] is [true] when [tag] weakly matches one of the
    first [count] entries. [count] must not exceed the array length. *)
val matches_any_weak : local_ bytes -> t -> t array -> count:int16# -> bool @@ portable

(** [matches_any_strong buf tag tags ~count] is [true] when [tag] strongly matches one of
    the first [count] entries. [count] must not exceed the array length. *)
val matches_any_strong : local_ bytes -> t -> t array -> count:int16# -> bool @@ portable

(** [write_etag dst ~off tag src] is the next offset after writing an ETag field using
    [tag]'s opaque bytes from [src]. The opaque bytes and destination capacity are not
    validated. *)
val write_etag : bytes -> off:int16# -> t -> local_ bytes -> int16# @@ portable

(** [write_etag_string dst ~off ~weak value] is the next offset after writing [value] as
    an ETag field. [value] must be a valid opaque tag without quotes; it is not escaped.
    A byte outside [etagc] raises [Invalid_argument]. *)
val write_etag_string : bytes -> off:int16# -> weak:bool -> string -> int16# @@ portable

(** [pp buf formatter tag] is the formatter operation that prints [tag], including its
    quotes and optional weak prefix. *)
val pp : local_ bytes -> Stdlib.Format.formatter -> t -> unit @@ portable
