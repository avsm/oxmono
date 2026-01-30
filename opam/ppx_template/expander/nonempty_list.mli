open! Stdppx

type 'a t = ( :: ) of 'a * 'a list

val to_list : 'a t -> 'a list

(** [map_result t ~f] returns [Ok _] if all [f x] are [Ok _], or else one [Error _]. *)
val map_result : 'a t -> f:('a -> ('b, 'c) result) -> ('b t, 'c) result

(** [product ts] is the cartesian product of all the [t]s in [ts], i.e. a list of all [t]s
    of the form [[t0; t1; ...]] where [t0] is from [ts[0]], [t1] is from [ts[1]], etc. *)
val product : 'a t t -> 'a t t

(** All of the functions below behave as their counterparts in [Stdppx.List]. *)

val map : 'a t -> f:('a -> 'b) -> 'b t
val compare : 'a t -> 'a t -> cmp:('a -> 'a -> int) -> int
val sort_uniq : 'a t -> cmp:('a -> 'a -> int) -> 'a t
val concat : 'a t t -> 'a t
