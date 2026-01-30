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

module Loc = struct
  type record : mutable_data
  type repr = record * int
  type (!'a : value_or_null) t : mutable_data with 'a

  external unsafe_of_atomic_loc
    : ('a : value_or_null).
    ('a Stdlib.Atomic.Loc.t[@local_opt]) -> ('a t[@local_opt])
    @@ portable
    = "%identity"

  external to_repr
    : ('a : value_or_null).
    ('a t[@local_opt]) -> (repr[@local_opt])
    @@ portable
    = "%identity"

  external field
    : ('a : value_or_null).
    record @ local -> int -> 'a
    @@ portable
    = "%obj_field"

  let get t =
    let r, n = to_repr t in
    field (Sys.opaque_identity r) n
  ;;

  external set_field
    : ('a : value_or_null).
    record @ local -> int -> 'a -> unit
    @@ portable
    = "%obj_set_field"

  let set t v =
    let r, n = to_repr t in
    set_field (Sys.opaque_identity r) n v
  ;;

  module Shared = struct
    external unsafe_of_atomic_loc
      : ('a : value_or_null).
      ('a Stdlib.Atomic.Loc.t[@local_opt]) @ shared -> ('a t[@local_opt]) @ shared
      @@ portable
      = "%identity"

    external get
      : ('a : value_or_null).
      'a t @ local shared -> 'a @ shared
      @@ portable
      = "%atomic_load_loc"

    external set
      : ('a : value_or_null mod contended).
      'a t @ local shared -> 'a -> unit
      @@ portable
      = "%atomic_set_loc"

    external exchange
      : ('a : value_or_null mod contended).
      'a t @ local shared -> 'a -> 'a
      @@ portable
      = "%atomic_exchange_loc"

    external compare_and_set
      : ('a : value_or_null mod contended).
      'a t @ local shared -> 'a -> 'a -> bool
      @@ portable
      = "%atomic_cas_loc"

    external compare_exchange
      : ('a : value_or_null mod contended).
      'a t @ local shared -> 'a -> 'a -> 'a
      @@ portable
      = "%atomic_compare_exchange_loc"

    external fetch_and_add
      :  int t @ local shared
      -> int
      -> int
      @@ portable
      = "%atomic_fetch_add_loc"

    external add : int t @ local shared -> int -> unit @@ portable = "%atomic_add_loc"
    external sub : int t @ local shared -> int -> unit @@ portable = "%atomic_sub_loc"
    external logand : int t @ local shared -> int -> unit @@ portable = "%atomic_land_loc"
    external logor : int t @ local shared -> int -> unit @@ portable = "%atomic_lor_loc"
    external logxor : int t @ local shared -> int -> unit @@ portable = "%atomic_lxor_loc"

    let incr r = add r 1
    let decr r = sub r 1
  end
end
