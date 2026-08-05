(** ETag parsing and comparison per RFC 7232.

    Entity tags are opaque validators used for conditional requests.
    They may be "strong" or "weak" - weak tags are prefixed with W/.

    {2 Usage}

    {[
      (* Parse ETag header *)
      let #(status, etag) = Etag.parse buf header_value_span in
      if status = Etag.Valid then
        (* use etag *)

      (* Parse If-None-Match (may contain multiple tags or "*") *)
      let cond = Etag.parse_if_none_match buf header_value_span in

      (* Compare for cache validation (weak comparison) *)
      let matches = Etag.weak_match buf etag current_etag in

      (* Compare for range requests (strong comparison) *)
      let matches = Etag.strong_match buf etag current_etag in
    ]}

    @see <https://datatracker.ietf.org/doc/html/rfc7232#section-2.3> RFC 7232 Section 2.3 *)

(** Entity tag. Unboxed record pointing into the parse buffer.
    The [off] and [len] fields reference the opaque-tag content without quotes. *)
type t =
  #{ weak : bool   (** [true] if prefixed with W/ *)
   ; off : int16#  (** Offset of tag content (after opening quote) *)
   ; len : int16#  (** Length of tag content (excluding quotes) *)
   }

(** Parse status. *)
type status =
  | Valid    (** Successfully parsed *)
  | Invalid  (** Invalid ETag syntax *)

(** Parse a single ETag value from a span.
    Accepts formats: ["xyzzy"], [W/"xyzzy"], [""]
    Returns status and tag (tag is only valid if status = Valid). *)
val parse : local_ bytes -> Span.t -> #(status * t)

(** Empty/invalid ETag constant. *)
val empty : t

(** Parse ETag to string (allocates). Useful for storage/comparison. *)
val to_string : local_ bytes -> t -> string

(** {2 If-Match / If-None-Match Parsing} *)

(** Result of parsing If-Match or If-None-Match header. *)
type match_condition =
  | Any           (** "*" - matches any entity *)
  | Tags          (** List of entity tags - use [get_tags] to retrieve *)
  | Empty         (** No valid tags found *)

(** Maximum number of ETags that can be parsed from a header. *)
val max_tags : int16#

(** Parse If-Match or If-None-Match header value.
    Handles "*" and comma-separated lists of entity tags.
    Tags are stored in the provided array (up to [max_tags]).
    Returns (condition, count) where count is number of tags if Tags. *)
val parse_match_header
  :  local_ bytes
  -> Span.t
  -> t array
  -> #(match_condition * int16#)

(** {2 Comparison Functions} *)

(** Strong comparison per RFC 7232 Section 2.3.2.
    Two entity-tags are equivalent if both are not weak and their
    opaque-tags match character-by-character. *)
val strong_match : local_ bytes -> t -> t -> bool

(** Weak comparison per RFC 7232 Section 2.3.2.
    Two entity-tags are equivalent if their opaque-tags match
    character-by-character, regardless of either or both being weak. *)
val weak_match : local_ bytes -> t -> t -> bool

(** Check if an etag matches any in array (weak comparison).
    [count] is the number of valid tags in the array. *)
val matches_any_weak : local_ bytes -> t -> t array -> count:int16# -> bool

(** Check if an etag matches any in array (strong comparison).
    [count] is the number of valid tags in the array. *)
val matches_any_strong : local_ bytes -> t -> t array -> count:int16# -> bool

(** {2 Response Writing} *)

(** Write ETag header: [ETag: "tag"\r\n] or [ETag: W/"tag"\r\n].
    Returns new offset. *)
val write_etag : bytes -> off:int16# -> t -> local_ bytes -> int16#

(** Write ETag header from string value.
    Returns new offset. *)
val write_etag_string : bytes -> off:int16# -> weak:bool -> string -> int16#

(** Pretty-print etag. *)
val pp : local_ bytes -> Stdlib.Format.formatter -> t -> unit
