type ('a : value_or_null) t = { mutable contents : 'a }

external make
  : ('a : value_or_null).
  'a -> ('a t[@local_opt])
  @@ portable
  = "%makemutable"

external make_contended
  : ('a : value_or_null).
  'a -> ('a t[@local_opt])
  @@ portable
  = "caml_atomic_make_contended"

external get : ('a : value_or_null). 'a t @ local -> 'a @@ portable = "%field0"
external set : ('a : value_or_null). 'a t @ local -> 'a -> unit @@ portable = "%setfield0"

module Shared = struct
  external get
    : ('a : value_or_null).
    'a t @ local shared -> 'a @ shared
    @@ portable
    = "%atomic_load"

  external set
    : ('a : value_or_null mod contended).
    'a t @ local shared -> 'a -> unit
    @@ portable
    = "%atomic_set"

  external exchange
    : ('a : value_or_null mod contended).
    'a t @ local shared -> 'a -> 'a
    @@ portable
    = "%atomic_exchange"

  external compare_and_set
    : ('a : value_or_null mod contended).
    'a t @ local shared -> 'a -> 'a -> bool
    @@ portable
    = "%atomic_cas"

  external compare_exchange
    : ('a : value_or_null mod contended).
    'a t @ local shared -> 'a -> 'a -> 'a
    @@ portable
    = "%atomic_compare_exchange"

  external fetch_and_add
    :  int t @ local shared
    -> int
    -> int
    @@ portable
    = "%atomic_fetch_add"

  external add : int t @ local shared -> int -> unit @@ portable = "%atomic_add"
  external sub : int t @ local shared -> int -> unit @@ portable = "%atomic_sub"
  external logand : int t @ local shared -> int -> unit @@ portable = "%atomic_land"
  external logor : int t @ local shared -> int -> unit @@ portable = "%atomic_lor"
  external logxor : int t @ local shared -> int -> unit @@ portable = "%atomic_lxor"

  let incr r = add r 1
  let decr r = sub r 1
end
