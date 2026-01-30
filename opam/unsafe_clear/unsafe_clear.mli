open! Base

module Array : sig
  [%%template:
  [@@@kind.default
    k'
    = ( value_or_null
      , bits64
      , float64
      , immediate
      , immediate64
      , value_or_null & value_or_null
      , immediate64 & immediate64
      , value_or_null & value_or_null & value_or_null
      , immediate64 & immediate64 & immediate64
      , value_or_null & value_or_null & value_or_null & value_or_null
      , immediate64 & immediate64 & immediate64 & immediate64 )]

  [@@@kind k = k' mod non_float]

  (** If the index currently holds a GC-able pointer, replace it with a value that is not
      a pointer. The value is not guaranteed to be an inhabitant of the type, and does no
      bounds checking.

      The overloads of this function for non-pointer types (such as immediate or bits64
      layouts) are (inlinable) no-ops. *)
  val unsafe_clear_if_pointer : ('a : k). 'a Array.t -> int -> unit
  [@@zero_alloc strict]

  (** Calls [unsafe_clear_if_pointer] on the closed range [from,to_], assumed to be
      nonempty (and not bounds checked.). For pointer-free types, this is more efficient
      than calling that loop, as the compiler is not capable of eliminating a loop with an
      empty body. *)
  val unsafe_clear_if_pointer_range : ('a : k). 'a Array.t -> from:int -> to_:int -> unit
  [@@zero_alloc strict]]
end
