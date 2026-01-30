open! Core

module Nonempty : sig
  include
    Nonempty_interval_lib.Nonempty_interval.S
    with type bound = Int.t
     and type interval := Interval_lib.Interval.Make(Int).t

  val zero_to_one : t
  val neg_one_to_one : t
  val equal : t -> t -> bool
  val gen : t Base_quickcheck.Generator.t
end

include
  Interval_lib.Interval.S
  with type t = Interval_lib.Interval.Make(Int).t
   and type bound = Int.t

val zero_to_one : t
val equal : t -> t -> bool
val gen_nonempty : t Base_quickcheck.Generator.t
