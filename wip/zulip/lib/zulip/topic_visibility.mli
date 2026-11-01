@@ portable

(** Personal topic visibility policies.

    Known policies use wire values [0] through [3]. Unknown integer values are
    preserved for compatibility with newer servers. *)

type t =
  | Inherit
  | Muted
  | Unmuted
  | Followed
  | Other of int
      (** The type for topic visibility. [Inherit] uses the channel preference.
          [Muted], [Unmuted], and [Followed] override it. [Other n] preserves
          the unknown wire value [n]. *)

val of_int : int -> t
(** [of_int n] is the policy represented by [n]. Unknown values produce
    [Other n]. *)

val to_int : t -> int
(** [to_int policy] is the wire value of [policy]. *)

val equal : t -> t -> bool
(** [equal a b] is [true] if [a] and [b] have equal wire values. *)

val compare : t -> t -> int
(** [compare a b] orders [a] and [b] by their wire values. *)

val pp : Format.formatter -> t -> unit
(** [pp ppf policy] writes a human-readable representation of [policy] to [ppf].
*)

val jsont : t Jsont.t
(** [jsont] is a codec for integer topic visibility policies. Decoding preserves
    unknown integers and rejects fractions and nonfinite or out-of-range
    numbers. Encoding rejects out-of-range integers. *)
