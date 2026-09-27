(** Operations that choose a command strategy from the server's extensions,
    documented in [Imap_eio.Mailbox]. *)

type t

val of_selected : Selected.t -> t

type ('a, 's) outcome = { strategy : 's; result : ('a, Error.t) result }

val search : t -> criteria:Imap.Search.t ->
  (Imap.Uid.t list, [ `Search ]) outcome

val fetch : ?drop_unsupported:bool -> t -> uids:Imap.Uid.t list ->
  items:Imap.Fetch_item.t list -> (Selected.row list, [ `Fetch of int ]) outcome

val store : ?unchangedsince:int64 -> t -> set:Imap.Uid_set.t ->
  operation:[ `Add | `Remove | `Replace ] ->
  flags:Mail_flag.Imap_flag.t list ->
  (Selected.store_receipt, [ `Conditional | `Unconditional ]) outcome

type move_strategy = [
  | `Move
  | `Copy_then_expunge
  | `Copy_then_flag
  | `Copied of Selected.copy_receipt option
  | `Copied_and_flagged of Selected.copy_receipt option ]

val move : t -> set:Imap.Uid_set.t -> mailbox:string ->
  (Selected.copy_receipt option, move_strategy) outcome

type change =
  | Flags of Imap.Uid.t * Mail_flag.Imap_flag.t list
  | Vanished of Imap.Uid_set.t
  | New of Imap.Uid.t

val changes_since : ?uidnext:Imap.Uid.t -> t -> Imap.Modseq.t option ->
  (change list, [ `Qresync | `Condstore | `Full ]) outcome

val list_with_status : Client.t -> ?reference:string -> pattern:string ->
  Imap.Status_item.t list ->
  ((Client.mailbox_entry * Imap.Response.mailbox_status option) list,
   [ `List_status | `List_then_status ]) outcome
(** [list_with_status] runs on the connection and is refused inside
    [Client.with_mailbox]. *)

val wait : ?timeout:float -> t -> clock:_ Eio.Time.clock ->
  poll_seconds:float ->
  (Imap.Response.t list, [ `Idle | `Poll ]) outcome
