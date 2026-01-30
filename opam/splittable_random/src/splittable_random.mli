@@ portable

(** A splittable pseudo-random number generator (SPRNG) functions like a PRNG in that it
    can be used as a stream of random values; it can also be "split" to produce a second,
    independent stream of random values.

    This module implements a splittable pseudo-random number generator that sacrifices
    cryptographic-quality randomness in favor of performance.

    The primary difference between [Splittable_random] and {!Random} is the [split]
    operation for generating new pseudo-random states. While it is easy to simulate
    [split] using [Random], the result has undesirable statistical properties; the new
    state does not behave independently of the original. It is better to switch to
    [Splittable_random] if you need an operation like [split], as this module has been
    implemented with the statistical properties of splitting in mind. For most other
    purposes, [Random] is likely a better choice, as its implementation passes all Diehard
    tests, while [Splittable_random] fails some Diehard tests. *)

open! Base

type t : value mod portable

(** Create a new [t] seeded from the given random state. This allows nondeterministic
    initialization, for example in the case that the input state was created using
    [Random.make_self_init].

    Constructors like [create] and [of_int] should be called once at the start of a
    randomized computation and the resulting state should be threaded through. Repeatedly
    creating splittable random states from seeds in the middle of computation can defeat
    the SPRNG's splittable properties. *)
val create : Random.State.t -> t

(** Create a new [t] that will return identical results to any other [t] created with that
    integer. *)
val of_int : int -> t

(** [perturb t salt] adds the entropy of [salt] to [t]. *)
val perturb : t -> int -> unit

(** Create a copy of [t] that will return the same random samples as [t]. *)
val copy : t @ shared -> t

(** Like [copy], but puts the result in an arbitrary capsule. *)
val copy_into_capsule : t @ shared -> (t, 'k) Capsule_expert.Data.t

(** [split t] produces a new state that behaves deterministically (i.e. only depending on
    the state of [t]), but pseudo-independently from [t]. This operation mutates [t],
    i.e., [t] will return different values than if this hadn't been called. *)
val split : t -> t

(** Like [split], but puts the result into an arbitrary capsule. *)
val split_into_capsule : t -> (t, 'k) Capsule_expert.Data.t

(** Legacy aliases for the preceding definitions. *)
module State : sig
  type nonrec t = t

  val create : Random.State.t -> t
  val of_int : int -> t
  val perturb : t -> int -> unit
  val copy : t -> t
  val split : t -> t
end
[@@deprecated
  "[since 2023-10] There is no longer any need to use [Splittable_random.State]. Its \
   definitions are now included directly in [Splittable_random]."]

(** Produces a random, fair boolean. *)
val bool : t -> bool

(** Produce a random number uniformly distributed in the given inclusive range. (In the
    case of [float], [hi] may or may not be attainable, depending on rounding.) *)
val int : t -> lo:int -> hi:int -> int

val int32 : t -> lo:int32 -> hi:int32 -> int32
val int63 : t -> lo:Int63.t -> hi:Int63.t -> Int63.t
val int64 : t -> lo:int64 -> hi:int64 -> int64
val nativeint : t -> lo:nativeint -> hi:nativeint -> nativeint
val float : t -> lo:float -> hi:float -> float

(** [unit_float state = float state ~lo:0. ~hi:1.], but slightly more efficient (and right
    endpoint is exclusive). *)
val unit_float : t -> float

module Log_uniform : sig
  (** Produce a random number in the given inclusive range, where the number of bits in
      the representation is chosen uniformly based on the given range, and then the value
      is chosen uniformly within the range restricted to the chosen bit width. Raises if
      [lo < 0 || hi < lo].

      These functions are useful for choosing numbers that are weighted low within a given
      range. *)
  val int : t -> lo:int -> hi:int -> int

  val int32 : t -> lo:int32 -> hi:int32 -> int32
  val int63 : t -> lo:Int63.t -> hi:Int63.t -> Int63.t
  val int64 : t -> lo:int64 -> hi:int64 -> int64
  val nativeint : t -> lo:nativeint -> hi:nativeint -> nativeint
end
