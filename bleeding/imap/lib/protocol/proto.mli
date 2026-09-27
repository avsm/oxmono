(** Checked IMAP scalar values and finite UID sets. *)

module Uid : sig
  type t
  val of_int64 : int64 -> (t, string) result
  val to_int64 : t -> int64
  val to_string : t -> string
end

module Uidvalidity : sig
  type t
  val of_int64 : int64 -> (t, string) result
  val to_int64 : t -> int64
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
end

module Uid_set : sig
  (** Sorted, disjoint, finite intervals. An empty set has no intervals. *)
  type t
  val empty : t
  val singleton : Uid.t -> t
  val of_intervals : (Uid.t * Uid.t) list -> t
  val of_wire : string -> (t, string) result
  (** Parse a finite UID set. Wildcards and zero are rejected. *)
  val intervals : t -> (Uid.t * Uid.t) list
  val cardinality : t -> int64
  val union : t -> t -> t
  val mem : Uid.t -> t -> bool
  val to_wire : t -> string
end
