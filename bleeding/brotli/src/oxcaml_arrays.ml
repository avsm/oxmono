(* Layout-polymorphic array primitives for OxCaml unboxed types.
   These allow accessing arrays of int8#, int16#, int32#, float#, etc. *)

(* Safe accessors *)
external[@layout_poly] get : ('a : any mod separable). 'a array -> int -> 'a = "%array_safe_get"
external[@layout_poly] set : ('a : any mod separable). 'a array -> int -> 'a -> unit = "%array_safe_set"

(* Unsafe accessors - use in hot paths after bounds checking *)
external[@layout_poly] unsafe_get : ('a : any mod separable). 'a array -> int -> 'a = "%array_unsafe_get"
external[@layout_poly] unsafe_set : ('a : any mod separable). 'a array -> int -> 'a -> unit = "%array_unsafe_set"

(* Length *)
external[@layout_poly] length : ('a : any mod separable). 'a array -> int = "%array_length"
