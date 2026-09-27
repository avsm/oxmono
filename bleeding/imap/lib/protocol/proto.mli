(** Checked IMAP scalar values and finite UID sets. *)

module Uid : sig
  type t
  val of_int64 : int64 -> (t, string) result
  val to_int64 : t -> int64
  val to_string : t -> string
  val equal : t -> t -> bool
  val compare : t -> t -> int
  (** [compare a b] orders UIDs numerically. *)

  val pp : Format.formatter -> t -> unit
end

module Uidvalidity : sig
  type t
  val of_int64 : int64 -> (t, string) result
  val to_int64 : t -> int64
  val equal : t -> t -> bool
  val compare : t -> t -> int
  (** [compare a b] orders values numerically. *)

  val pp : Format.formatter -> t -> unit
end

module Seq : sig
  type t
  val of_int64 : int64 -> (t, string) result
  val to_int64 : t -> int64
end

module Modseq : sig
  type t
  val of_int64 : int64 -> (t, string) result
  val to_int64 : t -> int64
  val equal : t -> t -> bool
  val compare : t -> t -> int
  (** [compare a b] orders values numerically. *)

  val pp : Format.formatter -> t -> unit
end

module Uid_set : sig
  (** Sorted, disjoint, finite intervals. An empty set has no intervals. *)
  type t
  val empty : t
  val is_empty : t -> bool
  (** [is_empty s] is [true] iff [s] has no intervals. *)

  val singleton : Uid.t -> t
  val of_intervals : (Uid.t * Uid.t) list -> t
  (** [of_intervals l] is the set covering every interval of [l]. A reversed
      pair is read with its bounds swapped. Overlapping and adjacent intervals
      merge. *)

  val of_wire : ?allow_star:bool -> string -> (t, string) result
  (** [of_wire s] parses the RFC 9051 [sequence-set] [s] and normalises it as
      {!of_intervals} does. Endpoints are [nz-number] values within the UID
      range, so zero, signs, leading zeros and non-decimal syntax are
      rejected. [allow_star] defaults to [false]. When it is [true], [*] is
      accepted and read as 4294967295, so the result is a superset of the set
      the server resolves. The error names the offending token. *)

  val intervals : t -> (Uid.t * Uid.t) list
  val cardinality : t -> int64
  val union : t -> t -> t
  val mem : Uid.t -> t -> bool
  val to_wire : t -> string
  (** [to_wire s] is the canonical wire form of [s]. It is [""] for the empty
      set, which is not a valid [sequence-set]. *)

  val equal : t -> t -> bool
  val compare : t -> t -> int
  val pp : Format.formatter -> t -> unit
  (** [pp] prints the wire form, or [(empty)] for the empty set. *)
end
