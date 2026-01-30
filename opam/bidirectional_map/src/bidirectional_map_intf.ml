(** Defines a bidirectional map type for one-to-one bindings on two key types.

    Maintains the invariants that a left key [l] is bound to right key [r] if and only if
    [r] is also bound to [l], and that there are never multiple bindings for a given left
    or right key.

    Unless otherwise specified, iteration is ordered by left keys. This includes lists
    such as [to_alist t] and callbacks such as [map ~f].

    Performance-wise, uses [Base.Map] and [Base.Set] under the hood. *)

open! Base

module type Bidirectional_map = sig @@ portable
  (** An internal type, exposed to express the necessary [with] bounds on [t] below. *)
  module Binding : Comparator.Derived2 [@mode portable]

  (** Type of a bidirectional map with ['l] left keys, ['r] right keys, and ['lc] and
      ['rc] comparator witness types, respectively. *)
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
      stable serializations, including [bin_io], see [Bidirectional_map_stable]. *)
  include
    Bidirectional_map_interfaces.Deriving
    with type ('l, 'lc, 'r, 'rc) t := ('l, 'lc, 'r, 'rc) t
  (** @inline *)

  (** {1 Accessors} *)

  (** Reports whether [t] is empty. *)
  val is_empty : (_, _, _, _) t -> bool

  (** Returns the number of left/right key bindings in [t]. *)
  val length : (_, _, _, _) t -> int

  (** Reports whether the left key [l] is bound in [t]. *)
  val mem_left : ('l, _, _, _) t -> 'l -> bool

  (** Reports whether the right key [r] is bound in [t]. *)
  val mem_right : (_, _, 'r, _) t -> 'r -> bool

  (** Reports whether the keys [l] and [r] are bound to each other in [t]. *)
  val mem_binding : ('l, _, 'r, _) t -> 'l -> 'r -> bool

  (** Finds the binding for left key [l], if any. *)
  val find_left : ('l, 'lc, 'r, 'rc) t -> 'l -> 'r option

  (** Finds the binding for right key [r], if any. *)
  val find_right : ('l, 'lc, 'r, 'rc) t -> 'r -> 'l option

  (** Produces a list of left and right key bindings as pairs. *)
  val to_alist : ('l, _, 'r, _) t -> ('l * 'r) list

  (** Produces the left keys of [t] in sorted order. *)
  val lefts : ('l, _, _, _) t -> 'l list

  (** Produces the right keys of [t] in sorted order. *)
  val rights : (_, _, 'r, _) t -> 'r list

  (** Produces a map from left keys to right keys. *)
  val left_to_right : ('l, 'lc, 'r, _) t -> ('l, 'r, 'lc) Map.t

  (** Produces a map from right keys to left keys. *)
  val right_to_left : ('l, _, 'r, 'rc) t -> ('r, 'l, 'rc) Map.t

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

  (** An empty bidirectional map. *)
  val empty
    :  ('l, 'lc) Comparator.Module.t
    -> ('r, 'rc) Comparator.Module.t
    -> ('l, 'lc, 'r, 'rc) t

  (** Constructs a bidirectional map with the given pairing. *)
  val singleton
    :  ('l, 'lc) Comparator.Module.t
    -> ('r, 'rc) Comparator.Module.t
    -> 'l
    -> 'r
    -> ('l, 'lc, 'r, 'rc) t

  (** Constructs a bidirectional map with the given pairings, if there are no duplicate
      left or right keys. *)
  val of_alist_or_error
    :  ('l, 'lc) Comparator.Module.t
    -> ('r, 'rc) Comparator.Module.t
    -> ('l * 'r) list
    -> ('l, 'lc, 'r, 'rc) t Or_error.t

  (** Binds the given left and right key to each other if neither key is bound. *)
  val add : ('l, 'lc, 'r, 'rc) t -> 'l -> 'r -> ('l, 'lc, 'r, 'rc) t option

  (** Binds the given left and right key to each other if neither key is bound or if they
      are already bound to each other. *)
  val add_or_keep : ('l, 'lc, 'r, 'rc) t -> 'l -> 'r -> ('l, 'lc, 'r, 'rc) t option

  (** Binds the given left and right key to each other, replacing any existing bindings
      for both keys. *)
  val set : ('l, 'lc, 'r, 'rc) t -> 'l -> 'r -> ('l, 'lc, 'r, 'rc) t

  (** Removes any binding for the left key [l]. *)
  val remove_left : ('l, 'lc, 'r, 'rc) t -> 'l -> ('l, 'lc, 'r, 'rc) t

  (** Removes any binding for the right key [r]. *)
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

  (** Combines the bindings of two bidirectional maps, unless they contain conflicting
      bindings for a given left or right key. *)
  val merge
    :  ('l, 'lc, 'r, 'rc) t
    -> ('l, 'lc, 'r, 'rc) t
    -> ('l, 'lc, 'r, 'rc) t Or_error.t
end
