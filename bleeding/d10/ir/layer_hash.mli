(** A d10 layer hash.

    Identical format to {!D10.Layer.hash} output: hex string, MD5 of the
    concatenated SHA-512 effective-opam hashes of the package and its transitive
    deps. The d10ir library does not compute layer hashes itself — it consumes
    them as identifiers. *)

type t = private string

val of_string : string -> t @@ portable
(** [of_string s] tags [s] as a layer hash. *)

val to_string : t -> string @@ portable
(** [to_string h] is the underlying hex string. *)

val equal : t -> t -> bool
(** [equal a b] is byte-equality of the underlying hex strings. *)

val compare : t -> t -> int
(** [compare a b] orders layer hashes by string comparison. *)

val pp : t Fmt.t
(** [pp] renders the full hex string. *)
