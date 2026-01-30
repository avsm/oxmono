(** Defines module types shared by [Bidirectional_map], [Bidirectional_many_to_one_map],
    [Bidirectional_multimap] to support ppx deriving for their types. *)

open Base

(** Module types used by [Deriving], below. *)

module type With_compare = sig
  type t [@@deriving compare]
end

module type With_comparator_witness = sig
  type t
  type comparator_witness
end

module type With_equal = sig
  type t [@@deriving equal]
end

module type With_hash_fold = sig
  type t [@@deriving hash]
end

module type With_of_sexp = sig
  type t [@@deriving of_sexp]

  include Comparator.S with type t := t
end

module type With_quickcheck_generator = sig
  type t

  val quickcheck_generator : t Base_quickcheck.Generator.t

  include Comparator.S with type t := t
end

module type With_quickcheck_observer = sig
  type t

  val quickcheck_observer : t Base_quickcheck.Observer.t
end

module type With_quickcheck_shrinker = sig
  type t

  val quickcheck_shrinker : t Base_quickcheck.Shrinker.t
end

module type With_sexp_of = sig
  type t [@@deriving sexp_of]
end

(** Defines [M(Left)(Right).t] and derives [compare, equal, hash, sexp]. *)
module type Deriving_shared = sig
  type ('l, 'lc, 'r, 'rc) t

  module M (Left : With_comparator_witness) (Right : With_comparator_witness) : sig
    type nonrec t = (Left.t, Left.comparator_witness, Right.t, Right.comparator_witness) t
  end

  (** Used by [@@deriving sexp_of] *)
  val sexp_of_m__t
    :  (module With_sexp_of with type t = 'l)
    -> (module With_sexp_of with type t = 'r)
    -> ('l, _, 'r, _) t
    -> Sexp.t

  (** Used by [@@deriving of_sexp] *)
  val m__t_of_sexp
    :  (module With_of_sexp with type t = 'l and type comparator_witness = 'lc)
    -> (module With_of_sexp with type t = 'r and type comparator_witness = 'rc)
    -> Sexp.t
    -> ('l, 'lc, 'r, 'rc) t

  (** Used by [@@deriving compare] *)
  val compare_m__t
    :  (module With_compare with type t = 'l)
    -> (module With_compare with type t = 'r)
    -> ('l, 'lc, 'r, 'rc) t
    -> ('l, 'lc, 'r, 'rc) t
    -> int

  (** Used by [@@deriving equal] *)
  val equal_m__t
    :  (module With_equal with type t = 'l)
    -> (module With_equal with type t = 'r)
    -> ('l, 'lc, 'r, 'rc) t
    -> ('l, 'lc, 'r, 'rc) t
    -> bool

  (** Used by [@@deriving hash] *)
  val hash_fold_m__t
    :  (module With_hash_fold with type t = 'l)
    -> (module With_hash_fold with type t = 'r)
    -> Hash.state
    -> ('l, _, 'r, _) t
    -> Hash.state

  (** Used by [@@deriving hash] *)
  val hash_m__t
    :  (module With_hash_fold with type t = 'l)
    -> (module With_hash_fold with type t = 'r)
    -> ('l, _, 'r, _) t
    -> int
end

(** Adds [@@deriving quickcheck] support to [Deriving_shared]. *)
module type Deriving = sig
  type ('l, 'lc, 'r, 'rc) t

  include Deriving_shared with type ('l, 'lc, 'r, 'rc) t := ('l, 'lc, 'r, 'rc) t

  (** Used by [@@deriving quickcheck] *)
  val quickcheck_generator_m__t
    :  (module With_quickcheck_generator with type t = 'l and type comparator_witness = 'lc)
    -> (module With_quickcheck_generator
          with type t = 'r
           and type comparator_witness = 'rc)
    -> ('l, 'lc, 'r, 'rc) t Base_quickcheck.Generator.t

  (** Used by [@@deriving quickcheck] *)
  val quickcheck_observer_m__t
    :  (module With_quickcheck_observer with type t = 'l)
    -> (module With_quickcheck_observer with type t = 'r)
    -> ('l, _, 'r, _) t Base_quickcheck.Observer.t

  (** Used by [@@deriving quickcheck] *)
  val quickcheck_shrinker_m__t
    :  (module With_quickcheck_shrinker with type t = 'l)
    -> (module With_quickcheck_shrinker with type t = 'r)
    -> ('l, _, 'r, _) t Base_quickcheck.Shrinker.t
end
