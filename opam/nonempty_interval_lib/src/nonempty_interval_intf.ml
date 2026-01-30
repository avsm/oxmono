open! Core

(** See {!Nonempty_interval} below. *)

module type S = sig
  type t [@@deriving bin_io, sexp, compare, hash]
  type bound
  type interval

  val of_interval : interval -> t option
  val of_interval_exn : interval -> t
  val to_interval : t -> interval

  (** [create l u] returns [Some] interval with lower bound [l] and upper bound [u],
      unless [l > u], in which case it returns [None]. *)
  val create : bound -> bound -> t option

  (** [create_exn] is the same as {!val:create} except it raises on [None]. *)
  val create_exn : bound -> bound -> t

  (** [singleton] returns an interval with the lower bound equal to the upper bound. *)
  val singleton : bound -> t

  val intersect : t -> t -> t option
  val is_singleton : t -> bool
  val bounds : t -> bound * bound
  val lbound : t -> bound
  val ubound : t -> bound
  val convex_hull : t Nonempty_list.t -> t
  val contains : t -> bound -> bool
  val compare_value : t -> bound -> [ `Below | `Within | `Above ]
  val bound : t -> bound -> bound
  val is_superset : t -> of_:t -> bool
  val is_subset : t -> of_:t -> bool

  (** [map t ~f] returns [Some (create (f l) (f u))] if the resulting interval is
      nonempty, else it returns [None]. *)
  val map : t -> f:(bound -> bound) -> t option

  (** [map_exn] is the same as {!val:map} except it raises on [None]. *)
  val map_exn : t -> f:(bound -> bound) -> t

  val are_disjoint : t list -> bool
  val are_disjoint_as_open_intervals : t list -> bool
  val list_intersect : t list -> t list -> t list
  val half_open_intervals_are_a_partition : t list -> bool
end

module type S_stable = sig
  include S

  module Stable : sig
    module V1 : sig
      type nonrec t = t [@@deriving bin_io, compare, sexp, stable_witness, hash]
    end
  end
end

module type S_stable_with_sexp_grammar = sig
  include S_stable

  module Stable : sig
    include module type of Stable

    module V1 : sig
      type nonrec t = t [@@deriving sexp_grammar]

      include module type of V1 with type t := t
    end
  end
end

module type Bound = sig
  type t [@@deriving bin_io, sexp, hash]

  include Comparable.S with type t := t
end

module type Bound_stable = sig
  type t [@@deriving stable_witness]

  include Bound with type t := t
end

module type Bound_stable_with_sexp_grammar = sig
  type t [@@deriving stable_witness, sexp_grammar]

  include Bound with type t := t
end

module type Nonempty_interval = sig
  (** [Nonempty_interval] is [Interval_lib.Interval] but with empty intervals disallowed.

      This adds a layer of safety if you don't expect your intervals to be empty. And it
      allows for more convenient accessors that return values instead of value options. *)

  module type Bound = Bound
  module type Bound_stable = Bound_stable
  module type Bound_stable_with_sexp_grammar = Bound_stable_with_sexp_grammar
  module type S = S
  module type S_stable = S_stable
  module type S_stable_with_sexp_grammar = S_stable_with_sexp_grammar

  module Make (Bound : Bound) :
    S with type bound = Bound.t and type interval := Interval_lib.Interval.Make(Bound).t

  module Make_stable (Bound : Bound_stable) :
    S_stable
    with type bound = Bound.t
     and type interval := Interval_lib.Interval.Make(Bound).t

  module Make_stable_with_sexp_grammar (Bound : Bound_stable_with_sexp_grammar) :
    S_stable_with_sexp_grammar
    with type bound = Bound.t
     and type interval := Interval_lib.Interval.Make(Bound).t

  module Date :
    S_stable
    with type bound = Date.t
     and type interval := Interval_lib.Interval.Make(Date).t

  module Float :
    S_stable with type bound = float and type interval := Interval_lib.Interval.Float.t

  module Ofday_ns :
    S_stable
    with type bound = Time_ns.Ofday.t
     and type interval := Interval_lib.Interval.Ofday_ns.t

  module Span_ns :
    S_stable
    with type bound = Time_ns.Span.t
     and type interval := Interval_lib.Interval.Make(Time_ns.Span).t
end
