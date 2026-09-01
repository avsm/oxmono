(** This module represents borrowed regions of a parse buffer.

    A span records an offset and length; it does not own or copy bytes. The caller must
    retain the associated buffer and must not modify the spanned region while using the
    span. Comparison and parsing functions avoid allocating strings; {!to_string} makes an
    independent copy.

    Offset and length are [int16#], which suffices for the 32 KiB maximum buffer and keeps
    a span free of any heap allocation. *)

(** A [t] is a byte range in a caller-owned buffer. *)
type t =
  #{ off : int16# (** [off] is the zero-based offset of the first byte. *)
   ; len : int16# (** [len] is the number of bytes. *)
   }

(** [make ~off ~len] is the span beginning at [off] with length [len]. The arguments are
    not bounds-checked against any buffer. *)
val make : off:int16# -> len:int16# -> t @@ portable

(** [off span] is [span]'s byte offset. *)
val off : t -> int @@ portable

(** [len span] is [span]'s byte length. *)
val len : t -> int @@ portable

(** [is_empty span] is [true] exactly when [span] has length zero. *)
val is_empty : t -> bool @@ portable

(** [equal buf span value] is [true] when [span] and [value] contain the same bytes. *)
val equal : local_ bytes -> t -> string -> bool @@ portable

(** [equal_caseless buf span value] is [true] when [span] equals the lowercase ASCII
    string [value] after folding uppercase ASCII bytes in [span]. This is suitable for the
    case-insensitive protocol tokens passed by Httpz. *)
val equal_caseless : local_ bytes -> t -> string -> bool @@ portable

(** [split_on_char buf span separator] is the pair of spans before and after the first
    [separator]. The separator is excluded. If it is absent, the result is [span] and an
    empty span at the end of [span]. *)
val split_on_char : local_ bytes -> t -> char# -> #(t * t) @@ portable

(** [parse_content_length buf span] is the parsed Content-Length field value described by
    {{:https://www.rfc-editor.org/rfc/rfc9110.html#section-8.6} RFC 9110, Section 8.6}. It
    accepts a comma-separated repetition only when every member is the same decimal value.
    The result is [(value, overflow, conflicting)]; malformed input gives [-1L] with both
    flags clear. *)
val parse_content_length : local_ bytes -> t -> #(int64# * bool * bool) @@ portable

(** [token_list_last_is buf span token] is the number of non-empty members in the
    comma-separated list and whether its final member equals [token], ignoring ASCII case
    and optional whitespace. *)
val token_list_last_is : local_ bytes -> t -> string -> #(int * bool) @@ portable

(** [token_list_all_are buf span token] is the number of non-empty members in the
    comma-separated list and whether every member equals [token], ignoring ASCII case
    and optional whitespace. *)
val token_list_all_are : local_ bytes -> t -> string -> #(int * bool) @@ portable

(** [token_list_contains buf span token] is [true] when a member of the comma-separated
    list equals [token], ignoring ASCII case and optional whitespace. *)
val token_list_contains : local_ bytes -> t -> string -> bool @@ portable

val[@zero_alloc] token_list_valid : local_ bytes -> t -> bool @@ portable
(** [token_list_valid buf span] is [true] when every non-empty member of the
    comma-separated list is an HTTP token. Empty members are ignored and
    surrounding optional whitespace is allowed. No member strings are
    allocated. *)

(** [parse_transfer_encoding buf span] is the number of codings, the number named
    [chunked], whether [chunked] is final, and whether the complete list is syntactically
    valid. Empty list members are ignored. *)
val parse_transfer_encoding : local_ bytes -> t -> #(int * int * bool * bool) @@ portable

(** [to_string buf span] is a new string containing [span]. *)
val to_string : local_ bytes -> t -> string @@ portable

val[@zero_alloc] to_string_local : local_ bytes -> t -> string @ local @@ portable
(** [to_string_local buf t] is {!to_string} built in the caller's region. *)
