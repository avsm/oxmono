@@ portable

(** See {!Portable_kernel.Subatomic} for documentation. *)

type (!'a : value_or_null) t : mutable_data with 'a

external make : ('a : value_or_null). 'a -> ('a t[@local_opt]) = "%makemutable"

external make_contended
  : ('a : value_or_null).
  'a -> ('a t[@local_opt])
  = "caml_atomic_make_contended"

val get : ('a : value_or_null). 'a t @ local -> 'a
val set : ('a : value_or_null). 'a t @ local -> 'a -> unit

module Shared : sig
  val get : ('a : value_or_null). 'a t @ local shared -> 'a @ shared
  val set : ('a : value_or_null mod contended). 'a t @ local shared -> 'a -> unit
  val exchange : ('a : value_or_null mod contended). 'a t @ local shared -> 'a -> 'a

  val compare_and_set
    : ('a : value_or_null mod contended).
    'a t @ local shared -> 'a -> 'a -> bool

  val compare_exchange
    : ('a : value_or_null mod contended).
    'a t @ local shared -> 'a -> 'a -> 'a

  val fetch_and_add : int t @ local shared -> int -> int
  val add : int t @ local shared -> int -> unit
  val sub : int t @ local shared -> int -> unit
  val logand : int t @ local shared -> int -> unit
  val logor : int t @ local shared -> int -> unit
  val logxor : int t @ local shared -> int -> unit
  val incr : int t @ local shared -> unit
  val decr : int t @ local shared -> unit
end

module Loc : sig
  type (!'a : value_or_null) t : mutable_data with 'a

  external unsafe_of_atomic_loc
    : ('a : value_or_null).
    ('a Stdlib.Atomic.Loc.t[@local_opt]) -> ('a t[@local_opt])
    = "%identity"

  val get : ('a : value_or_null). 'a t @ local -> 'a
  val set : ('a : value_or_null). 'a t @ local -> 'a -> unit

  module Shared : sig
    external unsafe_of_atomic_loc
      : ('a : value_or_null).
      ('a Stdlib.Atomic.Loc.t[@local_opt]) @ shared -> ('a t[@local_opt]) @ shared
      = "%identity"

    val get : ('a : value_or_null). 'a t @ local shared -> 'a @ shared
    val set : ('a : value_or_null mod contended). 'a t @ local shared -> 'a -> unit
    val exchange : ('a : value_or_null mod contended). 'a t @ local shared -> 'a -> 'a

    val compare_and_set
      : ('a : value_or_null mod contended).
      'a t @ local shared -> 'a -> 'a -> bool

    val compare_exchange
      : ('a : value_or_null mod contended).
      'a t @ local shared -> 'a -> 'a -> 'a

    val fetch_and_add : int t @ local shared -> int -> int
    val add : int t @ local shared -> int -> unit
    val sub : int t @ local shared -> int -> unit
    val logand : int t @ local shared -> int -> unit
    val logor : int t @ local shared -> int -> unit
    val logxor : int t @ local shared -> int -> unit
    val incr : int t @ local shared -> unit
    val decr : int t @ local shared -> unit
  end
end
