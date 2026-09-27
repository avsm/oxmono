(** CONDSTORE modification sequences, RFC 7162 [mod-sequence-value]. *)

type t
(** A received MODSEQ, a positive signed 64-bit number. *)

val of_int64 : int64 -> (t, string) result
(** [of_int64 n] is [n] as a MODSEQ, or an error when [n] is not
    positive. *)

val to_int64 : t -> int64
val to_string : t -> string
(** [to_string m] is the decimal wire form of [m]. *)

val equal : t -> t -> bool
val compare : t -> t -> int
(** [compare a b] orders values numerically. *)

val pp : Format.formatter -> t -> unit
(** [pp] prints the decimal form. *)
