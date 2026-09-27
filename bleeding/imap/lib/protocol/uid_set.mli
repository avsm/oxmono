(** Finite UID sets.

    A set is the UID form of the RFC 9051 [sequence-set]. It is held as
    sorted, disjoint, non-adjacent intervals, so two sets with the same
    UIDs are equal and print the same wire form. *)

type t
(** The type for finite sets of UIDs. *)

val empty : t
(** [empty] is the set with no UIDs. *)

val is_empty : t -> bool
(** [is_empty s] is [true] if [s] has no UIDs. *)

val singleton : Uid.t -> t
(** [singleton u] is the set holding [u] alone. *)

val of_list : Uid.t list -> t
(** [of_list l] is the set of the UIDs of [l]. Order and repeats in [l] do
    not matter. *)

val of_intervals : (Uid.t * Uid.t) list -> t
(** [of_intervals l] is the set covering every interval of [l]. A reversed
    pair is read with its bounds swapped. Overlapping and adjacent intervals
    merge. *)

val of_wire : ?allow_star:bool -> string -> (t, string) result
(** [of_wire ~allow_star s] is the set written by the RFC 9051
    [sequence-set] [s], normalised as {!of_intervals} does. Endpoints are
    [nz-number] values within the UID range, so zero, signs, leading zeros
    and non-decimal syntax are errors. [allow_star] defaults to [false].
    When it is [true], [*] reads as 4294967295, so the result is a superset
    of the set the server resolves. The error names the offending token. *)

val to_wire : t -> string
(** [to_wire s] is the canonical wire form of [s], its intervals in
    ascending order joined by commas.

    @raise Invalid_argument if [s] is empty, which has no wire form. *)

val intervals : t -> (Uid.t * Uid.t) list
(** [intervals s] is the normalised intervals of [s] in ascending order. *)

val cardinality : t -> int64
(** [cardinality s] is the number of UIDs in [s]. *)

val mem : Uid.t -> t -> bool
(** [mem u s] is [true] if [u] is in [s]. It takes time linear in the
    number of intervals of [s]. *)

val add : Uid.t -> t -> t
(** [add u s] is [s] with [u] added. *)

val union : t -> t -> t
(** [union a b] is the UIDs in [a] or in [b]. *)

val inter : t -> t -> t
(** [inter a b] is the UIDs in both [a] and [b]. *)

val diff : t -> t -> t
(** [diff a b] is the UIDs of [a] that are not in [b]. *)

val iter : (Uid.t -> unit) -> t -> unit
(** [iter f s] applies [f] to every UID of [s] in ascending order. *)

val fold : (Uid.t -> 'a -> 'a) -> t -> 'a -> 'a
(** [fold f s acc] folds [f] over every UID of [s] in ascending order,
    starting from [acc]. *)

val to_list : t -> Uid.t list
(** [to_list s] is every UID of [s] in ascending order. The list holds one
    element per UID, up to 4294967295, so check {!cardinality} first. *)

val equal : t -> t -> bool
(** [equal a b] is [true] if [a] and [b] hold the same UIDs. *)

val compare : t -> t -> int
(** [compare a b] is a total order on sets, compatible with {!equal}. It
    compares the interval lists lexicographically. *)

val pp : Format.formatter -> t -> unit
(** [pp ppf s] prints the wire form of [s] on [ppf], or [(empty)] when [s]
    is empty. *)
