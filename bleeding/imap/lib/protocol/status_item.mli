(** Mailbox STATUS data items (RFC 9051 section 6.3.11). *)

type t =
  | Messages
  | Unseen
  | Uidnext
  | Uidvalidity
  | Highestmodseq  (** RFC 7162. *)
  | Mailboxid  (** RFC 8474. *)
  | Objectid  (** The draft OBJECTID+ compound identity. *)
  | Size  (** RFC 8438. *)
  | Deleted  (** RFC 9051 and RFC 9208. *)
  | Deleted_storage  (** RFC 9208. *)

val to_wire : t -> string
(** [to_wire i] is the uppercase item name of [i], such as [DELETED-STORAGE]. *)

val equal : t -> t -> bool
(** [equal a b] holds when [a] and [b] are the same item. *)

val pp : Format.formatter -> t -> unit
(** [pp] prints {!to_wire}. *)
