@@ portable

open! Core
open! Import

module Arr_impl : sig
  type 'a t

  val unsafe_to_array_inplace__promise_not_a_float : 'a t -> Obj.t array
end

type%template 'a arr_impl = 'a Arr_impl.t
type%template 'a arr_impl = 'a F32.Array.t [@@kind float32]
type%template 'a arr_impl = 'a I64.Array.t [@@kind bits64]

[%%template:
[@@@kind.default
  k
  = ( float32
    , bits64
    , immediate64
    , value & value
    , immediate64 & immediate64
    , value & value & value
    , immediate64 & immediate64 & immediate64
    , value & value & value & value
    , immediate64 & immediate64 & immediate64 & immediate64
    , value )]

type ('a : k) t : mutable_data with 'a

val length : local_ (_ t[@kind k]) -> int [@@zero_alloc]
val capacity : (_ t[@kind k]) -> int [@@zero_alloc]
val create : ?initial_capacity:int -> unit -> (_ t[@kind k])
val unsafe_create_uninitialized : len:int -> ('a t[@kind k])
val init : int -> f:local_ (int -> 'a) -> ('a t[@kind k])
val unsafe_get : ('a t[@kind k]) @ m -> int -> 'a [@@mode m = (local, global)]
val unsafe_set : ('a t[@kind k]) -> int -> 'a -> unit

val unsafe_blit
  :  src:local_ ('a t[@kind k])
  -> src_pos:int
  -> dst:local_ ('a t[@kind k])
  -> dst_pos:int
  -> len:int
  -> unit

val max_index : (_ t[@kind k]) -> int
val grow_capacity_once : (_ t[@kind k]) -> unit
val grow_capacity_to_at_least : (_ t[@kind k]) -> capacity:int -> unit
val unsafe_clear_pointer_at : (_ t[@kind k]) -> int -> unit
val unsafe_set_length : (_ t[@kind k]) -> int -> unit
val copy : ('a t[@kind k]) -> ('a t[@kind k])
val invariant : ('a -> unit) -> ('a t[@kind k]) Invariant.t]

module With_structure_details : sig
  type%template nonrec 'a t = ('a t[@kind k])
  [@@deriving sexp_of]
  [@@kind
    k
    = ( float32
      , bits64
      , value
      , immediate64
      , value & value
      , immediate64 & immediate64
      , value & value & value
      , immediate64 & immediate64 & immediate64
      , value & value & value & value
      , immediate64 & immediate64 & immediate64 & immediate64 )]
end

val unsafe_set_imm : ('a : immediate64). 'a t -> int -> 'a -> unit

module Expert : sig
  val%template unsafe_inner : ('a t[@kind k]) -> ('a arr_impl[@kind k])
  [@@kind k = (value, float32, bits64)]
end

val sort : ?pos:int -> ?len:int -> 'a t -> compare:('a -> 'a -> int) -> unit
