(** IMAP message unique identifiers, the RFC 9051 [uniqueid]. *)

type t
(** A UID, a number from 1 to 4294967295. *)

val of_int64 : int64 -> (t, string) result
(** [of_int64 n] is [n] as a UID, or an error when [n] is outside 1 to
    4294967295. *)

val to_int64 : t -> int64
val to_string : t -> string
(** [to_string u] is the decimal wire form of [u]. *)

val succ : t -> t option
(** [succ u] is the UID after [u], or [None] when [u] is 4294967295. *)

val pred : t -> t option
(** [pred u] is the UID before [u], or [None] when [u] is 1. *)

val equal : t -> t -> bool
val compare : t -> t -> int
(** [compare a b] orders UIDs numerically. *)

val pp : Format.formatter -> t -> unit
(** [pp] prints the decimal form. *)
