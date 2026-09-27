(** IMAP message unique identifiers.

    A UID is the RFC 9051 [uniqueid], a number from 1 to 4294967295 that
    names one message of a mailbox for as long as its UIDVALIDITY holds. *)

type t
(** The type for UIDs. *)

val of_int64 : int64 -> (t, string) result
(** [of_int64 n] is [n] as a UID. The error names the valid range when [n]
    is outside 1 to 4294967295. *)

val to_int64 : t -> int64
(** [to_int64 u] is the numeric value of [u]. *)

val to_string : t -> string
(** [to_string u] is the decimal wire form of [u]. *)

val succ : t -> t option
(** [succ u] is the UID after [u], or [None] when [u] is 4294967295. *)

val pred : t -> t option
(** [pred u] is the UID before [u], or [None] when [u] is 1. *)

val equal : t -> t -> bool
(** [equal a b] is [true] if [a] and [b] are the same UID. *)

val compare : t -> t -> int
(** [compare a b] orders [a] and [b] numerically. *)

val pp : Format.formatter -> t -> unit
(** [pp ppf u] prints the decimal form of [u] on [ppf]. *)
