open! Core
open Async

type t =
  | Ragged of int list Int.Map.t option
  | Rectangular
[@@deriving sexp]

(** If [create_verbose] returns [Ragged lengths], [lengths] will be an exhaustive listing
    of every length and every line of each length. *)
val create_verbose : string list list -> t

(** If [create_streaming] returns [Ragged lengths], [lengths] will be None *)
val create_streaming : ?sep:char -> Reader.t -> t Deferred.t

val to_error_string : t -> (unit, string list list) result
