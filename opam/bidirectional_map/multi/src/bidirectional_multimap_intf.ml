(** Defines a bidirectional map type for many-to-many bindings on two key types.

    Maintains the invariants that a left key [l] is bound to right key [r] if and only if
    [r] is also bound to [l], and that there are no duplicates of a given [l, r] binding.

    Unless otherwise specified, iteration is ordered by left keys, then by right keys.
    This includes lists such as [to_alist t] and callbacks such as [map ~f].

    Performance-wise, uses [Base.Map] and [Base.Set] under the hood. *)

open! Base

module type Bidirectional_multimap = sig @@ portable
  (** An internal type, exposed to express the necessary [with] bounds on [t] below. *)
  module Binding : Comparator.Derived2 [@mode portable]

  (** Type of a bidirectional multi-map with ['l] left keys, ['r] right keys, and ['lc]
      and ['rc] comparator witness types, respectively. *)
  type ('l
       , 'lc
       , 'r
       , 'rc)
       t :
       immutable_data
       with 'l
       with 'r
       with ('l, 'lc) Comparator.t
       with ('r, 'rc) Comparator.t
       with ('l * 'r, ('lc, 'rc) Binding.comparator_witness) Comparator.t

  type 'a workaround_to_make_the_above_typecheck

  (** {1 Deriving} *)

  (** Defines [M(Left)(Right).t] deriving [compare, equal, hash, quickcheck, sexp]. For
      stable serializations, including [bin_io], see [Bidirectional_multimap_stable]. *)
  include
    Bidirectional_map_interfaces.Deriving
    with type ('l, 'lc, 'r, 'rc) t := ('l, 'lc, 'r, 'rc) t
  (** @inline *)

  (** {1 Accessors} *)

  (** Reports whether [t] is empty. *)
  val is_empty : (_, _, _, _) t -> bool

  (** Returns the number of left/right key bindings in [t]. *)
  val length : (_, _, _, _) t -> int

  (** Returns the number of left keys in [t]. *)
  val number_of_left_keys : (_, _, _, _) t -> int

  (** Returns the number of right keys in [t]. *)
  val number_of_right_keys : (_, _, _, _) t -> int

  (** Returns the number of bindings for a given left key in [t]. *)
  val number_of_bindings_for_left_key : ('l, _, _, _) t -> 'l -> int

  (** Returns the number of bindings for a given right key in [t]. *)
  val number_of_bindings_for_right_key : (_, _, 'r, _) t -> 'r -> int

  (** Reports whether the left key [l] is bound in [t]. *)
  val mem_left : ('l, _, _, _) t -> 'l -> bool

  (** Reports whether the right key [r] is bound in [t]. *)
  val mem_right : (_, _, 'r, _) t -> 'r -> bool

  (** Reports whether the keys [l] and [r] are bound to each other in [t]. *)
  val mem_binding : ('l, _, 'r, _) t -> 'l -> 'r -> bool

  (** Finds all bindings for left key [l]. *)
  val find_left : ('l, 'lc, 'r, 'rc) t -> 'l -> ('r, 'rc) Set.t

  (** Finds all bindings for right key [r]. *)
  val find_right : ('l, 'lc, 'r, 'rc) t -> 'r -> ('l, 'lc) Set.t

  (** Finds all bindings for left key [l], if any. *)
  val find_left_if_nonempty : ('l, 'lc, 'r, 'rc) t -> 'l -> ('r, 'rc) Set.t option

  (** Finds all bindings for right key [l], if any. *)
  val find_right_if_nonempty : ('l, 'lc, 'r, 'rc) t -> 'r -> ('l, 'lc) Set.t option

  (** Produces a list of left and right key bindings as pairs. *)
  val to_alist : ('l, _, 'r, _) t -> ('l * 'r) list

  (** Produces the left keys of [t] in sorted order. *)
  val lefts : ('l, _, _, _) t -> 'l list

  (** Produces the right keys of [t] in sorted order. *)
  val rights : (_, _, 'r, _) t -> 'r list

  (** Produces a map from left keys to non-empty sets of right keys. *)
  val left_to_right : ('l, 'lc, 'r, 'rc) t -> ('l, ('r, 'rc) Set.t, 'lc) Map.t

  (** Produces a map from right keys to non-empty sets of left keys. *)
  val right_to_left : ('l, 'lc, 'r, 'rc) t -> ('r, ('l, 'lc) Set.t, 'rc) Map.t

  (** Iterates over bindings in [t]. *)
  val iter : ('l, _, 'r, _) t -> f:('l -> 'r -> unit) -> unit

  (** Folds over bindings in [t]. *)
  val fold : ('l, _, 'r, _) t -> init:'acc -> f:('l -> 'r -> 'acc -> 'acc) -> 'acc

  (** Reports whether all bindings in [t] satisfy [f]. *)
  val for_all : ('l, _, 'r, _) t -> f:('l -> 'r -> bool) -> bool

  (** Reports whether at least one binding in [t] satisfies [f]. *)
  val exists : ('l, _, 'r, _) t -> f:('l -> 'r -> bool) -> bool

  (** Checks [t]'s internal invariants. *)
  val invariant : 'l Invariant.t -> 'r Invariant.t -> ('l, _, 'r, _) t Invariant.t

  (** {1 Constructors} *)

  (** An empty bidirectional multi-map. *)
  val empty
    :  ('l, 'lc) Comparator.Module.t
    -> ('r, 'rc) Comparator.Module.t
    -> ('l, 'lc, 'r, 'rc) t

  (** Constructs a bidirectional multi-map with the given pairing. *)
  val singleton
    :  ('l, 'lc) Comparator.Module.t
    -> ('r, 'rc) Comparator.Module.t
    -> 'l
    -> 'r
    -> ('l, 'lc, 'r, 'rc) t

  (** Constructs a bidirectional multi-map with the given pairings. *)
  val of_alist
    :  ('l, 'lc) Comparator.Module.t
    -> ('r, 'rc) Comparator.Module.t
    -> ('l * 'r) list
    -> ('l, 'lc, 'r, 'rc) t

  (** Binds the given left and right key if they are not already bound to each other. *)
  val add : ('l, 'lc, 'r, 'rc) t -> 'l -> 'r -> ('l, 'lc, 'r, 'rc) t option

  (** Binds the given left and right key to each other. *)
  val set : ('l, 'lc, 'r, 'rc) t -> 'l -> 'r -> ('l, 'lc, 'r, 'rc) t

  (** Removes all bindings for the left key [l]. *)
  val remove_left : ('l, 'lc, 'r, 'rc) t -> 'l -> ('l, 'lc, 'r, 'rc) t

  (** Removes all bindings for the right key [r]. *)
  val remove_right : ('l, 'lc, 'r, 'rc) t -> 'r -> ('l, 'lc, 'r, 'rc) t

  (** If [l] and [r] are bound to each other, removes that binding. *)
  val remove_binding : ('l, 'lc, 'r, 'rc) t -> 'l -> 'r -> ('l, 'lc, 'r, 'rc) t

  (** Produces a subset of [t]s bindings, keeping only the bindings satisfying [f]. *)
  val filter : ('l, 'lc, 'r, 'rc) t -> f:('l -> 'r -> bool) -> ('l, 'lc, 'r, 'rc) t

  (** Produces [filter t ~f, filter t ~f:(Fn.non f)]. *)
  val partition_tf
    :  ('l, 'lc, 'r, 'rc) t
    -> f:('l -> 'r -> bool)
    -> ('l, 'lc, 'r, 'rc) t * ('l, 'lc, 'r, 'rc) t

  (** Combines the bindings of two bidirectional multi-maps. *)
  val merge : ('l, 'lc, 'r, 'rc) t -> ('l, 'lc, 'r, 'rc) t -> ('l, 'lc, 'r, 'rc) t
end
