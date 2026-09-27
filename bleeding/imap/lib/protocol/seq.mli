(** IMAP message sequence numbers, RFC 9051 [seq-number]. *)

type t
(** A sequence number, from 1 to 4294967295. *)

val of_int64 : int64 -> (t, string) result
(** [of_int64 n] is [n] as a sequence number, or an error when [n] is
    outside 1 to 4294967295. *)

val to_int64 : t -> int64
val to_string : t -> string
(** [to_string n] is the decimal wire form of [n]. *)

val equal : t -> t -> bool
val compare : t -> t -> int
(** [compare a b] orders sequence numbers numerically. *)

val pp : Format.formatter -> t -> unit
(** [pp] prints the decimal form. *)
