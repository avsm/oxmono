@@ portable

type ('a : value_or_null) t =
  'a Stdlib_shim.Modes.Portable.t Atomic.t Stdlib_shim.Modes.Contended.t

external make
  : ('a : value_or_null).
  'a @ contended portable -> ('a t[@local_opt])
  = "%makemutable"

external make_contended
  : ('a : value_or_null).
  'a @ contended portable -> ('a t[@local_opt])
  = "caml_atomic_make_contended"

external get
  : ('a : value_or_null).
  'a t @ local -> 'a @ contended portable
  = "%atomic_load"

external set
  : ('a : value_or_null).
  'a t @ local -> 'a @ contended portable -> unit
  = "%atomic_set"

external exchange
  : ('a : value_or_null).
  'a t @ local -> 'a @ contended portable -> 'a @ contended portable
  = "%atomic_exchange"

external compare_and_set
  : ('a : value_or_null).
  'a t @ local -> 'a @ contended portable -> 'a @ contended portable -> bool
  = "%atomic_cas"

external compare_exchange
  : ('a : value_or_null).
  'a t @ local
  -> 'a @ contended portable
  -> 'a @ contended portable
  -> 'a @ contended portable
  = "%atomic_compare_exchange"

external fetch_and_add : int t @ local -> int -> int = "%atomic_fetch_add"
external add : int t @ local -> int -> unit = "%atomic_add"
external sub : int t @ local -> int -> unit = "%atomic_sub"
external logand : int t @ local -> int -> unit = "%atomic_land"
external logor : int t @ local -> int -> unit = "%atomic_lor"
external logxor : int t @ local -> int -> unit = "%atomic_lxor"
val incr : int t @ local -> unit
val decr : int t @ local -> unit

module Expert : sig
  external fenceless_get
    : ('a : value_or_null).
    'a t @ local -> 'a @ contended portable
    = "%field0"

  external fenceless_set
    : ('a : value_or_null).
    'a t @ local -> 'a @ contended portable -> unit
    = "%setfield0"
end
