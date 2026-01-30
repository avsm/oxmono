@@ portable

open! Base

(** Represents a non-linearizable scalable counter. *)
type t : value mod contended portable

(** [create value] returns a new non-linearizable counter with given initial [value]. *)
val create : int -> t

(** [zero ~width_of:t] returns a new non-linearizable counter whose internal width is the
    same as that of [t] and whose value is [0]. *)
val zero : width_of:t -> t

(** [get t] is an "estimate" of the accumulated value, which can be off by one or more and
    even be negative, so this must be used with care. *)
val get : t -> int

(** [add_or_resize t delta] either adds the [delta] to [t] and returns [Null] or returns
    [This t'], which shares the value of [t] in a new wider counter. The widening is done
    to avoid contention. *)
val add_or_resize : t -> int -> t or_null
