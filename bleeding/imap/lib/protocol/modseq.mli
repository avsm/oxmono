@@ portable

(** CONDSTORE modification sequences.

    A MODSEQ is the RFC 7162 [mod-sequence-value] a server reports for a
    message or a mailbox, a number from 1 to the largest signed 64-bit
    integer. The value 0, which RFC 7162 permits only as an UNCHANGEDSINCE
    argument, is not a [t]. *)

type t : immutable_data
(** The type for received MODSEQ values. A value is immutable data, so it
    may be shared between domains. *)

val of_int64 : int64 -> (t, string) result
(** [of_int64 n] is [n] as a MODSEQ, or an error when [n] is not
    positive. *)

val to_int64 : t -> int64
(** [to_int64 m] is the numeric value of [m]. *)

val to_string : t -> string
(** [to_string m] is the decimal wire form of [m]. *)

val equal : t -> t -> bool
(** [equal a b] is [true] if [a] and [b] are the same value. *)

val compare : t -> t -> int
(** [compare a b] orders [a] and [b] numerically. *)

val pp : Format.formatter -> t -> unit
(** [pp ppf m] prints the decimal form of [m] on [ppf]. *)
