(* target.ml - Zero-allocation HTTP target parsing *)

open Base

module I16 = Stdlib_stable.Int16_u

let[@inline] i16 x = I16.of_int x

(** Parsed target with path (no leading slash) and query. *)
type t =
  #{ path : Span.t       (** Path without leading slash *)
   ; query : Span.t      (** Query without '?' *)
   }

(** Empty span sentinel. *)
let empty_span : Span.t = Span.make ~off:(i16 0) ~len:(i16 0)

(** Parse target into path (without leading slash) and query.
    Single pass, zero allocation. *)
let[@inline] parse (local_ buf) (target : Span.t) : t =
  let toff = Span.off target in
  let tlen = Span.len target in
  if tlen = 0 then #{ path = empty_span; query = empty_span }
  else
    (* Skip leading slash if present *)
    let path_start, path_max_len =
      if Char.equal (Base_bigstring.unsafe_get buf toff) '/' then
        (toff + 1, tlen - 1)
      else
        (toff, tlen)
    in
    (* Find '?' to split path and query *)
    let mutable i = 0 in
    let mutable found = -1 in
    while found < 0 && i < path_max_len do
      if Char.equal (Base_bigstring.unsafe_get buf (path_start + i)) '?' then
        found <- i
      else
        i <- i + 1
    done;
    if found < 0 then
      (* No query string *)
      #{ path = Span.make ~off:(i16 path_start) ~len:(i16 path_max_len)
       ; query = empty_span
       }
    else
      (* Split at '?' *)
      let path = Span.make ~off:(i16 path_start) ~len:(i16 found) in
      let query_start = path_start + found + 1 in
      let query_len = path_max_len - found - 1 in
      let query = Span.make ~off:(i16 query_start) ~len:(i16 query_len) in
      #{ path; query }
;;

let[@inline] path (t : t) = t.#path
let[@inline] query (t : t) = t.#query
let[@inline] has_query (t : t) = Span.len t.#query > 0

(** {1 Path Segment Matching} *)

(** Match literal segment. Returns #(matched, remaining). *)
let[@inline] match_segment (local_ buf) (path : Span.t) (expected : string) : #(bool * Span.t) =
  let plen = Span.len path in
  if plen = 0 then #(false, empty_span)
  else
    let poff = Span.off path in
    let elen = String.length expected in
    (* Find next '/' *)
    let mutable i = 0 in
    while i < plen && not (Char.equal (Base_bigstring.unsafe_get buf (poff + i)) '/') do
      i <- i + 1
    done;
    (* Check if segment matches *)
    if i <> elen then #(false, empty_span)
    else if Base_bigstring.memcmp_string buf ~pos1:poff expected ~pos2:0 ~len:elen <> 0 then
      #(false, empty_span)
    else
      (* Skip the '/' separator if present *)
      let rest_off = if i < plen then poff + i + 1 else poff + i in
      let rest_len = if i < plen then plen - i - 1 else 0 in
      #(true, Span.make ~off:(i16 rest_off) ~len:(i16 rest_len))
;;

(** Match any segment (capture). Returns #(matched, segment, remaining). *)
let[@inline] match_param (local_ buf) (path : Span.t) : #(bool * Span.t * Span.t) =
  let plen = Span.len path in
  if plen = 0 then #(false, empty_span, empty_span)
  else
    let poff = Span.off path in
    (* Find next '/' *)
    let mutable i = 0 in
    while i < plen && not (Char.equal (Base_bigstring.unsafe_get buf (poff + i)) '/') do
      i <- i + 1
    done;
    let seg = Span.make ~off:(i16 poff) ~len:(i16 i) in
    let rest_off = if i < plen then poff + i + 1 else poff + i in
    let rest_len = if i < plen then plen - i - 1 else 0 in
    let rest = Span.make ~off:(i16 rest_off) ~len:(i16 rest_len) in
    #(true, seg, rest)
;;

(** Check if path is empty (complete match). *)
let[@inline] is_empty (path : Span.t) : bool = Span.len path = 0

(** {1 Query Parameter Lookup} *)

(** Find query parameter by name. Returns #(found, value_span). *)
let rec find_query_param (local_ buf) (query : Span.t) (name : string) : #(bool * Span.t) =
  if Span.is_empty query then #(false, empty_span)
  else
    let #(pair, rest) = Span.split_on_char buf query '&' in
    let #(key, value) = Span.split_on_char buf pair '=' in
    if Span.equal buf key name then #(true, value)
    else find_query_param buf rest name
;;

(** Fold over query parameters. *)
let rec fold_query_params (local_ buf) (query : Span.t) ~init ~f =
  if Span.is_empty query then init
  else
    let #(pair, rest) = Span.split_on_char buf query '&' in
    let #(key, value) = Span.split_on_char buf pair '=' in
    fold_query_params buf rest ~init:(f init key value) ~f
;;

(** Convert query to string pairs (allocates). *)
let query_to_string_pairs (local_ buf) (query : Span.t) : (string * string) list =
  fold_query_params buf query ~init:[] ~f:(fun acc key value ->
      (Span.to_string buf key, Span.to_string buf value) :: acc)
  |> List.rev
;;
