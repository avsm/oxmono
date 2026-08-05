(** Unboxed spans into the parse buffer.

    A span is a lightweight reference to a region of bytes in the parse buffer.
    Spans use [int16#] internally for offset and length since the maximum buffer
    size is 32KB, enabling fully unboxed representation with no heap allocation.

    {2 Usage}

    Spans are returned by parsing functions and allow zero-copy access to
    parsed values:

    {[
      let #(status, req, headers) = Httpz.parse buf ~len ~limits in
      if Span.equal buf req.#target "/api/users" then
        handle_users ()
    ]}

    {2 String Conversion}

    Use {!to_string} when you need an actual string (allocates):
    {[
      let path = Span.to_string buf req.#target in
      Printf.printf "Path: %s\n" path
    ]} *)

(** {1 Types} *)

type t =
  #{ off : int16#
   ; len : int16#
   }
(** Unboxed span record. Zero allocation. *)

(** {1 Construction} *)

val make : off:int16# -> len:int16# -> t
(** [make ~off ~len] creates a span at the given offset and length. *)

(** {1 Accessors} *)

val off : t -> int
(** [off span] returns the byte offset as a regular [int]. *)

val len : t -> int
(** [len span] returns the length as a regular [int]. *)

val is_empty : t -> bool
(** [is_empty span] returns [true] if [span.len = 0]. *)

(** {1 Comparison} *)

val equal : local_ bytes -> t -> string -> bool
(** [equal buf span s] returns [true] if the bytes at [span] in [buf]
    are exactly equal to string [s]. Case-sensitive. *)

val equal_caseless : local_ bytes -> t -> string -> bool
(** [equal_caseless buf span s] returns [true] if the bytes at [span]
    in [buf] equal string [s], ignoring ASCII case. *)

(** {1 Operations} *)

val split_on_char : local_ bytes -> t -> char# -> #(t * t)
(** [split_on_char buf span c] splits [span] at the first occurrence of [c].

    Returns [#(before, after)] where [after] excludes the separator.
    If [c] is not found, returns [#(span, empty_span)]. *)

(** {1 Integer Parsing} *)

val parse_int64 : local_ bytes -> t -> #(int64# * bool)
(** [parse_int64 buf span] parses a decimal integer with overflow protection.

    Returns [#(value, overflow)] where:
    - [value]: The parsed value, or [-1L] if empty/invalid
    - [overflow]: [true] if the value overflows int64 *)

(** {1 String Conversion} *)

val to_string : local_ bytes -> t -> string
(** [to_string buf span] copies the span contents to a new string.
    This allocates - use span comparisons when possible. *)
