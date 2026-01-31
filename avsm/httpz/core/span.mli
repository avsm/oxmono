(** Unboxed spans into the parse buffer.

    A span is a lightweight reference to a region of bytes in the parse buffer.
    Spans use [int16#] internally for offset and length since the maximum buffer
    size is 32KB, enabling fully unboxed representation with no heap allocation.

    {2 Usage}

    Spans are returned by parsing functions and allow zero-copy access to
    parsed values:

    {[
      let #(status, req, headers) = Httpz.parse buf ~len ~limits in
      (* req.#target is a span - compare without allocating *)
      if Span.equal buf req.#target "/api/users" then
        handle_users ()
      else if Span.starts_with buf req.#target '/' then
        handle_path ()
    ]}

    {2 String Conversion}

    Use {!to_string} when you need an actual string (allocates):
    {[
      let path = Span.to_string buf req.#target in
      Printf.printf "Path: %s\n" path
    ]} *)

(** {1 Types} *)

type t =
  #{ off : int16#  (** Byte offset into buffer *)
   ; len : int16#  (** Length in bytes *)
   }
(** Unboxed span record. Zero allocation. *)

(** {1 Construction} *)

val make : off:int16# -> len:int16# -> t
(** [make ~off ~len] creates a span at the given offset and length. *)

(** {1 Accessors} *)

val off : t -> int
(** [off span] returns the byte offset as a regular [int].
    Use for array indexing. *)

val len : t -> int
(** [len span] returns the length as a regular [int].
    Use for comparisons with [String.length] etc. *)

val off16 : t -> int16#
(** [off16 span] returns the byte offset as [int16#].
    Use for unboxed arithmetic. *)

val len16 : t -> int16#
(** [len16 span] returns the length as [int16#].
    Use for unboxed arithmetic. *)

val is_empty : t -> bool
(** [is_empty span] returns [true] if [span.len = 0]. *)

(** {1 Comparison} *)

val equal : local_ bytes -> t -> string -> bool
(** [equal buf span s] returns [true] if the bytes at [span] in [buf]
    are exactly equal to string [s]. Case-sensitive.

    {[
      if Span.equal buf target "/index.html" then serve_index ()
    ]} *)

val equal_caseless : local_ bytes -> t -> string -> bool
(** [equal_caseless buf span s] returns [true] if the bytes at [span]
    in [buf] equal string [s], ignoring ASCII case.

    {[
      (* Match header name case-insensitively *)
      if Span.equal_caseless buf hdr.name_span "x-request-id" then ...
    ]} *)

(** {1 Character Operations} *)

val starts_with : local_ bytes -> t -> char -> bool
(** [starts_with buf span c] returns [true] if [span] starts with character [c]. *)

val find_char : local_ bytes -> t -> char -> int
(** [find_char buf span c] returns the index of the first occurrence of [c]
    in [span], or [-1] if not found. The index is relative to [span.off]. *)

val unsafe_get : local_ bytes -> t -> int -> char
(** [unsafe_get buf span i] returns the character at position [i] within [span].
    No bounds checking - caller must ensure [0 <= i < span.len]. *)

(** {1 Subspan Operations} *)

val sub : t -> pos:int -> len:int -> t
(** [sub span ~pos ~len] creates a subspan starting at relative offset [pos]
    with the given length. No bounds checking. *)

val skip_char : local_ bytes -> t -> char -> t
(** [skip_char buf span c] returns a new span with the leading character
    removed if it equals [c], otherwise returns [span] unchanged. *)

val split_on_char : local_ bytes -> t -> char -> #(t * t)
(** [split_on_char buf span c] splits [span] at the first occurrence of [c].

    Returns [#(before, after)] where [after] excludes the separator character.
    If [c] is not found, returns [#(span, empty_span)].

    {[
      let #(path, query) = Span.split_on_char buf target '?' in
      (* path = everything before '?', query = everything after *)
    ]} *)

(** {1 Integer Parsing} *)

val parse_int64 : local_ bytes -> t -> int64#
(** [parse_int64 buf span] parses a decimal integer from [span].
    Returns [-1L] on error (empty span or non-digit character).

    {b Warning:} Does NOT check for overflow. Use {!parse_int64_limited}
    for untrusted input. *)

val parse_int64_limited : local_ bytes -> t -> max_value:int64# -> #(int64# * bool)
(** [parse_int64_limited buf span ~max_value] parses a decimal integer
    with overflow protection.

    Returns [#(value, overflow)] where:
    - [value]: The parsed value, or [-1L] if empty/invalid
    - [overflow]: [true] if the value exceeds [max_value] or parsing failed

    {[
      let #(len, overflow) = Span.parse_int64_limited buf cl_span ~max_value:max_cl in
      if overflow then reject_request ()
    ]} *)

(** {1 String Conversion} *)

val to_string : local_ bytes -> t -> string
(** [to_string buf span] copies the span contents to a new string.
    This allocates - use span comparisons when possible. *)

val to_bytes : local_ bytes -> t -> bytes
(** [to_bytes buf span] copies the span contents to a new bytes value.
    This allocates. *)

(** {1 Utility} *)

val of_int : int -> int16#
(** [of_int n] converts a regular [int] to [int16#]. *)

val to_int : int16# -> int
(** [to_int n] converts an [int16#] to a regular [int]. *)

(** {1 Pretty Printing} *)

val pp : Stdlib.Format.formatter -> t -> unit
(** [pp fmt span] prints the span structure (offset and length). *)

val pp_with_buf : local_ bytes -> Stdlib.Format.formatter -> t -> unit
(** [pp_with_buf buf fmt span] prints the actual content of [span] from [buf]. *)
