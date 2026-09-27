(** IMAP mailbox UIDVALIDITY values, RFC 9051 section 2.3.1.1. *)

type t
(** A UIDVALIDITY, a number from 1 to 4294967295. *)

val of_int64 : int64 -> (t, string) result
(** [of_int64 n] is [n] as a UIDVALIDITY, or an error when [n] is outside 1
    to 4294967295. *)

val to_int64 : t -> int64
val to_string : t -> string
(** [to_string v] is the decimal wire form of [v]. *)

val equal : t -> t -> bool
val compare : t -> t -> int
(** [compare a b] orders values numerically. *)

val pp : Format.formatter -> t -> unit
(** [pp] prints the decimal form. *)
