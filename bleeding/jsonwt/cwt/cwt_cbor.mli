(** Bounded CBOR for the CWT profile. Maps use integer or text labels. *)
val decode : ?max_size:int -> string -> (Cbort.Cbor.t, string) result
  @@ portable
(** [decode bytes] consumes exactly one item, preserving tags. It bounds bytes,
    nesting and item count, validates UTF-8 and rejects duplicate map labels. *)
val encode : Cbort.Cbor.t -> string @@ portable
(** [encode value] uses definite lengths. Invalid or excessive values raise
    [Invalid_argument]. Integers must fit CBOR's unsigned 64-bit argument. *)
