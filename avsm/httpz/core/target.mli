(** Zero-allocation HTTP target parsing.

    This module provides utilities for parsing HTTP request targets
    (the path and query string) without heap allocation. All results
    are returned as spans referencing the original buffer.

    The {!parse} function strips the leading slash from paths, so
    "/foo/bar" becomes "foo/bar" in the returned path span.

    Functions that may fail return unboxed tuples [#(success, result)]
    instead of [option] to avoid allocation. *)

(** Parsed target components - unboxed record with span references. *)
type t =
  #{ path : Span.t       (** Path without leading slash, e.g., "foo/bar" *)
   ; query : Span.t      (** Query portion excluding '?', e.g., "a=1&b=2" *)
   }

(** Parse target span into path and query components.
    The leading slash is stripped from the path in a single pass.
    Zero allocation - returns unboxed record with span references. *)
val parse : local_ bytes -> Span.t -> t

(** Get the path span (without leading slash). *)
val path : t -> Span.t

(** Get the query span (empty if no query string). *)
val query : t -> Span.t

(** Check if target has a query string. *)
val has_query : t -> bool

(** {1 Path Segment Matching}

    Match path segments against expected patterns without allocation.
    Functions return unboxed tuples to avoid option allocation. *)

(** Match a literal path segment. Returns [#(matched, remaining_path)].
    If [matched] is [false], [remaining_path] is undefined. *)
val match_segment : local_ bytes -> Span.t -> string -> #(bool * Span.t)

(** Match any path segment (parameter capture). Returns [#(matched, segment, remaining)].
    If [matched] is [false], [segment] and [remaining] are undefined. *)
val match_param : local_ bytes -> Span.t -> #(bool * Span.t * Span.t)

(** Check if path span is empty (complete match). *)
val is_empty : Span.t -> bool

(** {1 Query Parameter Lookup}

    Look up query parameters without full iteration. *)

(** Find query parameter by name. Returns [#(found, value_span)].
    If [found] is [false], [value_span] is undefined. *)
val find_query_param : local_ bytes -> Span.t -> string -> #(bool * Span.t)

(** Fold over all query parameters.
    [f acc key_span value_span] is called for each parameter. *)
val fold_query_params :
  local_ bytes ->
  Span.t ->
  init:'a ->
  f:('a -> Span.t -> Span.t -> 'a) ->
  'a

(** Convert query parameters to string pairs. Allocates. *)
val query_to_string_pairs : local_ bytes -> Span.t -> (string * string) list
