(** Zero-allocation HTTP target parsing.

    This module provides utilities for parsing HTTP request targets
    (the path and query string) without heap allocation. All results
    are returned as spans referencing the original buffer.

    Functions that may fail return unboxed tuples [#(success, result)]
    instead of [option] to avoid allocation. *)

(** Parsed target components - unboxed record with span references. *)
type t =
  #{ path : Span.t       (** Path portion, e.g., "/foo/bar" *)
   ; query : Span.t      (** Query portion excluding '?', e.g., "a=1&b=2" *)
   }

(** Parse target span into path and query components.
    Zero allocation - returns unboxed record with span references. *)
val parse : local_ Base_bigstring.t -> Span.t -> t

(** Get the path span. *)
val path : t -> Span.t

(** Get the query span (empty if no query string). *)
val query : t -> Span.t

(** Check if target has a query string. *)
val has_query : t -> bool

(** {1 Path Segment Matching}

    Match path segments against expected patterns without allocation.
    Functions return unboxed tuples to avoid option allocation. *)

(** Match a literal path segment. Returns [#(matched, remaining_path)].
    If [matched] is [false], [remaining_path] is undefined.
    The path should not have a leading slash. *)
val match_segment : local_ Base_bigstring.t -> Span.t -> string -> #(bool * Span.t)

(** Match any path segment (parameter capture). Returns [#(matched, segment, remaining)].
    If [matched] is [false], [segment] and [remaining] are undefined. *)
val match_param : local_ Base_bigstring.t -> Span.t -> #(bool * Span.t * Span.t)

(** Check if path is empty or just trailing slash. *)
val path_is_empty : local_ Base_bigstring.t -> Span.t -> bool

(** Skip leading slash from path. *)
val skip_leading_slash : local_ Base_bigstring.t -> Span.t -> Span.t

(** Count path segments (for validation). *)
val count_segments : local_ Base_bigstring.t -> Span.t -> int

(** {1 Query Parameter Lookup}

    Look up query parameters without full iteration. *)

(** Find query parameter by name. Returns [#(found, value_span)].
    If [found] is [false], [value_span] is undefined. *)
val find_query_param : local_ Base_bigstring.t -> Span.t -> string -> #(bool * Span.t)

(** Fold over all query parameters.
    [f acc key_span value_span] is called for each parameter. *)
val fold_query_params :
  local_ Base_bigstring.t ->
  Span.t ->
  init:'a ->
  f:('a -> Span.t -> Span.t -> 'a) ->
  'a

(** Convert query parameters to string pairs. Allocates. *)
val query_to_string_pairs : local_ Base_bigstring.t -> Span.t -> (string * string) list
