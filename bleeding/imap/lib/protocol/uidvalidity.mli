(** IMAP mailbox UIDVALIDITY values.

    A UIDVALIDITY is a number from 1 to 4294967295 that a server announces
    for a mailbox, RFC 9051 §2.3.1.1. A UID is meaningful only together
    with the UIDVALIDITY in force when it was assigned. *)

type t
(** The type for UIDVALIDITY values. *)

val of_int64 : int64 -> (t, string) result
(** [of_int64 n] is [n] as a UIDVALIDITY. The error names the valid range
    when [n] is outside 1 to 4294967295. *)

val to_int64 : t -> int64
(** [to_int64 v] is the numeric value of [v]. *)

val to_string : t -> string
(** [to_string v] is the decimal wire form of [v]. *)

val equal : t -> t -> bool
(** [equal a b] is [true] if [a] and [b] are the same value. *)

val compare : t -> t -> int
(** [compare a b] orders [a] and [b] numerically. *)

val pp : Format.formatter -> t -> unit
(** [pp ppf v] prints the decimal form of [v] on [ppf]. *)
