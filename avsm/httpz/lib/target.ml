(* target.ml - Zero-allocation HTTP target parsing *)

open Base

module I16 = Stdlib_stable.Int16_u

let[@inline] i16 x = I16.of_int x

(** Parsed target components - all spans, no allocation. *)
type t =
  #{ path : Span.t       (** Path portion, e.g., "/foo/bar" *)
   ; query : Span.t      (** Query portion excluding '?', e.g., "a=1&b=2" *)
   }

(** Parse target span into path and query components.
    Zero allocation - returns unboxed record with span references. *)
let[@inline] parse (local_ buf) (target : Span.t) : t =
  let #(path, query) = Span.split_on_char buf target '?' in
  #{ path; query }
;;

(** Get the path span. *)
let[@inline] path (t : t) = t.#path

(** Get the query span (empty if no query string). *)
let[@inline] query (t : t) = t.#query

(** Check if target has a query string. *)
let[@inline] has_query (t : t) = not (Span.is_empty t.#query)

(** {1 Path Segment Matching}

    Match path segments against expected patterns without allocation.
    Functions return unboxed tuples #(success, result_span) to avoid allocation. *)

(** Empty span sentinel for failed matches. *)
let empty_span : Span.t = Span.make ~off:(i16 0) ~len:(i16 0)

(** Match a literal path segment. Returns #(matched, remaining_path).
    If matched is false, remaining_path is undefined.
    The path should not have a leading slash. *)
let[@inline] match_segment (local_ buf) (path : Span.t) (expected : string) : #(bool * Span.t) =
  if Span.is_empty path
  then #(false, empty_span)
  else
    let #(seg, rest) = Span.split_on_char buf path '/' in
    if Span.equal buf seg expected
    then #(true, rest)
    else #(false, empty_span)
;;

(** Match any path segment (parameter capture). Returns #(matched, segment, remaining).
    If matched is false, segment and remaining are undefined. *)
let[@inline] match_param (local_ buf) (path : Span.t) : #(bool * Span.t * Span.t) =
  if Span.is_empty path
  then #(false, empty_span, empty_span)
  else
    let #(seg, rest) = Span.split_on_char buf path '/' in
    #(true, seg, rest)
;;

(** Check if path is empty or just trailing slash. *)
let[@inline] path_is_empty (local_ buf) (path : Span.t) : bool =
  Span.is_empty path || (Span.len path = 1 && Span.starts_with buf path '/')
;;

(** Skip leading slash from path. *)
let[@inline] skip_leading_slash (local_ buf) (path : Span.t) : Span.t =
  Span.skip_char buf path '/'
;;

(** Count path segments (for validation). *)
let count_segments (local_ buf) (path : Span.t) : int =
  let path = skip_leading_slash buf path in
  if Span.is_empty path then 0
  else
    let mutable count = 1 in
    let mutable remaining = path in
    while not (Span.is_empty remaining) do
      let #(_, rest) = Span.split_on_char buf remaining '/' in
      if not (Span.is_empty rest) then count <- count + 1;
      remaining <- rest
    done;
    count
;;

(** {1 Query Parameter Lookup}

    Look up query parameters without full iteration. *)

(** Find query parameter by name. Returns #(found, value_span).
    If found is false, value_span is undefined. *)
let rec find_query_param (local_ buf) (query : Span.t) (name : string) : #(bool * Span.t) =
  if Span.is_empty query
  then #(false, empty_span)
  else
    let #(pair, rest) = Span.split_on_char buf query '&' in
    let #(key, value) = Span.split_on_char buf pair '=' in
    if Span.equal buf key name
    then #(true, value)
    else find_query_param buf rest name
;;

(** Fold over all query parameters.
    [f acc key_span value_span] is called for each parameter. *)
let rec fold_query_params (local_ buf) (query : Span.t) ~init ~f =
  if Span.is_empty query
  then init
  else
    let #(pair, rest) = Span.split_on_char buf query '&' in
    let #(key, value) = Span.split_on_char buf pair '=' in
    let acc = f init key value in
    fold_query_params buf rest ~init:acc ~f
;;

(** Convert query parameters to string pairs. Allocates. *)
let query_to_string_pairs (local_ buf) (query : Span.t) : (string * string) list =
  fold_query_params buf query ~init:[] ~f:(fun acc key value ->
      (Span.to_string buf key, Span.to_string buf value) :: acc)
  |> List.rev
;;
