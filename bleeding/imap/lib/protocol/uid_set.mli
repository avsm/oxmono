(** Finite UID sets, the UID form of the RFC 9051 [sequence-set]. *)

type t
(** Sorted, disjoint, finite intervals. The empty set has no intervals. *)

val empty : t
val is_empty : t -> bool
(** [is_empty s] is [true] iff [s] has no intervals. *)

val singleton : Uid.t -> t
val of_list : Uid.t list -> t
(** [of_list l] is the set of the UIDs of [l]. Order and repeats in [l] do
    not matter. *)

val of_intervals : (Uid.t * Uid.t) list -> t
(** [of_intervals l] is the set covering every interval of [l]. A reversed
    pair is read with its bounds swapped. Overlapping and adjacent intervals
    merge. *)

val of_wire : ?allow_star:bool -> string -> (t, string) result
(** [of_wire s] parses the RFC 9051 [sequence-set] [s] and normalises it as
    {!of_intervals} does. Endpoints are [nz-number] values within the UID
    range, so zero, signs, leading zeros and non-decimal syntax are
    rejected. [allow_star] defaults to [false]. When it is [true], [*] is
    accepted and read as 4294967295, so the result is a superset of the set
    the server resolves. The error names the offending token. *)

val to_wire : t -> string
(** [to_wire s] is the canonical wire form of [s].

    @raise Invalid_argument if [s] is empty, which has no wire form. *)

val intervals : t -> (Uid.t * Uid.t) list
(** [intervals s] is the normalised intervals of [s] in ascending order. *)

val cardinality : t -> int64
(** [cardinality s] is the number of UIDs in [s]. *)

val mem : Uid.t -> t -> bool
val add : Uid.t -> t -> t
val union : t -> t -> t
val inter : t -> t -> t
val diff : t -> t -> t
(** [diff a b] is the UIDs of [a] that are not in [b]. *)

val iter : (Uid.t -> unit) -> t -> unit
(** [iter f s] applies [f] to every UID of [s] in ascending order. *)

val fold : (Uid.t -> 'a -> 'a) -> t -> 'a -> 'a
(** [fold f s acc] folds [f] over every UID of [s] in ascending order. *)

val to_list : t -> Uid.t list
(** [to_list s] is every UID of [s] in ascending order. It materialises the
    whole set, which can hold 4294967295 UIDs, so check {!cardinality}
    first. *)

val equal : t -> t -> bool
val compare : t -> t -> int
val pp : Format.formatter -> t -> unit
(** [pp] prints the wire form, or [(empty)] for the empty set. *)
