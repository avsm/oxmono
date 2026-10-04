(** A producer-supplied layer identifier.

    No hash algorithm is imposed. Producers must include every build input and
    dependency in their cache identity. Values are used as directory names, so
    callers must supply a non-empty path component, conventionally a hex digest.
    Conversions do not validate or sanitize the string. *)

type t = private string

val of_string : string -> t @@ portable
(** [of_string s] tags [s] as a layer hash. *)

val to_string : t -> string @@ portable
(** [to_string h] is the underlying string. *)

val equal : t -> t -> bool
(** [equal a b] is byte-equality of the underlying strings. *)

val compare : t -> t -> int
(** [compare a b] orders layer hashes by string comparison. *)

val pp : t Fmt.t
(** [pp] renders the identifier. *)
