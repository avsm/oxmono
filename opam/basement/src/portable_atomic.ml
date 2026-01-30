type ('a : value_or_null) t =
  'a Stdlib_shim.Modes.Portable.t Atomic.t Stdlib_shim.Modes.Contended.t

external make
  : ('a : value_or_null).
  'a @ contended portable -> ('a t[@local_opt])
  @@ portable
  = "%makemutable"

external make_contended
  : ('a : value_or_null).
  'a @ contended portable -> ('a t[@local_opt])
  @@ portable
  = "caml_atomic_make_contended"

external get
  : ('a : value_or_null).
  'a t @ local -> 'a @ contended portable
  @@ portable
  = "%atomic_load"

external set
  : ('a : value_or_null).
  'a t @ local -> 'a @ contended portable -> unit
  @@ portable
  = "%atomic_set"

external exchange
  : ('a : value_or_null).
  'a t @ local -> 'a @ contended portable -> 'a @ contended portable
  @@ portable
  = "%atomic_exchange"

external compare_and_set
  : ('a : value_or_null).
  'a t @ local -> 'a @ contended portable -> 'a @ contended portable -> bool
  @@ portable
  = "%atomic_cas"

external compare_exchange
  : ('a : value_or_null).
  'a t @ local
  -> 'a @ contended portable
  -> 'a @ contended portable
  -> 'a @ contended portable
  @@ portable
  = "%atomic_compare_exchange"

external fetch_and_add : int t @ local -> int -> int @@ portable = "%atomic_fetch_add"
external add : int t @ local -> int -> unit @@ portable = "%atomic_add"
external sub : int t @ local -> int -> unit @@ portable = "%atomic_sub"
external logand : int t @ local -> int -> unit @@ portable = "%atomic_land"
external logor : int t @ local -> int -> unit @@ portable = "%atomic_lor"
external logxor : int t @ local -> int -> unit @@ portable = "%atomic_lxor"

let incr r = add r 1
let decr r = sub r 1

module Expert = struct
  external fenceless_get
    : ('a : value_or_null).
    'a t @ local -> 'a @ contended portable
    @@ portable
    = "%field0"

  external fenceless_set
    : ('a : value_or_null).
    'a t @ local -> 'a @ contended portable -> unit
    @@ portable
    = "%setfield0"
end
