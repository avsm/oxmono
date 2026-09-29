@@ portable

(** IMAP message sequence numbers.

    A sequence number is the RFC 9051 [seq-number], the position of a
    message in the selected mailbox from 1 to 4294967295. It changes when
    an earlier message is expunged. *)

type t : immediate
(** The type for sequence numbers. A number is an immediate integer, so
    it is never allocated and may be used at any mode. *)

val of_int64 : int64 -> (t, string) result
(** [of_int64 n] is [n] as a sequence number. The error names the valid
    range when [n] is outside 1 to 4294967295. *)

val to_int64 : t -> int64
(** [to_int64 n] is the numeric value of [n]. *)

val of_int : int -> (t, string) result
(** [of_int n] is [n] as a sequence number. The error names the valid
    range when [n] is outside 1 to 4294967295. *)

val to_int : t -> int
(** [to_int n] is the numeric value of [n]. Unlike {!to_int64} it
    allocates nothing. *)

val to_string : t -> string
(** [to_string n] is the decimal wire form of [n]. *)

val equal : t -> t -> bool
(** [equal a b] is [true] if [a] and [b] are the same position. *)

val compare : t -> t -> int
(** [compare a b] orders [a] and [b] numerically. *)

val pp : Format.formatter -> t -> unit
(** [pp ppf n] prints the decimal form of [n] on [ppf]. *)
