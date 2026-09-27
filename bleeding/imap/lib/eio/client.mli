(** A single IMAP connection with commands serialized across fibers,
    documented in [Imap_eio.Client]. *)

type t
type error = Error.t

val pp_error : Format.formatter -> error -> unit
val error_to_string : error -> string

val connect :
  sw:Eio.Switch.t -> ?auth:Auth.t -> Transport.t -> (t, error) result

val of_flow :
  sw:Eio.Switch.t -> ?auth:Auth.t ->
  [> Eio.Flow.two_way_ty | Eio.Resource.close_ty ] Eio.Resource.t ->
  (t, error) result

val capabilities : t -> Imap.Capability.Set.t
val enabled : t -> Imap.Capability.Set.t
val has : t -> Imap.Capability.t -> bool
val is_enabled : t -> Imap.Capability.t -> bool
val is_open : t -> bool
val compress_deflate : t -> (unit, error) result
val enable : t -> Imap.Capability.t list ->
  (Imap.Capability.t list, error) result
val enable_uidonly : t -> (unit, error) result
val enable_objectid_plus : t -> (unit, error) result

val pin_mailbox_objectid : t -> mailbox:string -> account_id:string ->
  mailbox_id:string -> (unit, error) result
(** [pin_mailbox_objectid t ~mailbox ~account_id ~mailbox_id] makes later
    selections and APPENDs of [mailbox] on [t] verify that identity. *)

val list : t -> ?reference:string -> pattern:string ->
  unit -> (Imap.Response.list_result list, error) result
val lsub : t -> ?reference:string -> pattern:string ->
  unit -> (Imap.Response.list_result list, error) result
val namespace : t -> (Imap.Response.namespace, error) result

type discovery = {
  mailboxes :
    (Imap.Response.list_result * Imap.Response.mailbox_status option) list;
  unpaired_status : Imap.Response.mailbox_status list;
}

val list_extended : t -> ?reference:string -> patterns:string list ->
  ?selection:Imap.Command.list_selection list ->
  ?returns:Imap.Command.list_return list ->
  ?status:Imap.Command.status_item list -> unit -> (discovery, error) result
val mailbox_mode : t -> Imap.Mailbox_name.mode
val status : t -> mailbox:string -> items:Imap.Command.status_item list ->
  (Imap.Response.mailbox_status, error) result
val get_jmap_access : t -> (string, error) result
val get_acl : t -> mailbox:string -> (Imap.Response.acl, error) result
val list_rights : t -> mailbox:string -> identifier:string ->
  (Imap.Response.list_rights, error) result
val my_rights : t -> mailbox:string -> (Imap.Response.my_rights, error) result
val set_acl : t -> mailbox:string -> identifier:string ->
  operation:[ `Add | `Remove | `Replace ] -> rights:string ->
  (unit, error) result
val delete_acl : t -> mailbox:string -> identifier:string ->
  (unit, error) result
val get_quota : t -> root:string -> (Imap.Response.quota, error) result
val get_quota_root : t -> mailbox:string ->
  ((Imap.Response.quota_root * Imap.Response.quota list), error) result
val set_quota : t -> root:string -> limits:(string * int64) list ->
  (Imap.Response.quota option, error) result

type metadata_result = {
  responses : Imap.Response.metadata list;
  longentries : int64 option;
}

val get_metadata : t -> mailbox:string -> entries:string list ->
  ?maxsize:int64 -> ?depth:Imap.Command.metadata_depth -> unit ->
  (metadata_result, error) result
val set_metadata : t -> mailbox:string ->
  values:(string * string option) list -> (unit, error) result
val notify_set : t -> ?status:bool -> groups:Imap.Command.notify_group list ->
  unit -> (Imap.Response.mailbox_status list, error) result
val notify_none : t -> (unit, error) result
val create_mailbox : t -> string -> (unit, error) result
val create_mailbox_objectid : t -> string ->
  (Imap.Response.compound_object_id, error) result
val delete_mailbox : t -> string -> (unit, error) result
val rename_mailbox : t -> old_name:string -> new_name:string ->
  (unit, error) result
val rename_mailbox_objectid : t -> old_name:string -> new_name:string ->
  (Imap.Response.compound_object_id, error) result
val subscribe_mailbox : t -> string -> (unit, error) result
val unsubscribe_mailbox : t -> string -> (unit, error) result

val with_mailbox : t -> ?qresync:(Imap.Uidvalidity.t * Imap.Modseq.t) ->
  ?objectid:(string * string) ->
  mode:[ `Read_only | `Read_write ] -> string ->
  (Selected.t -> ('a, error) result) -> ('a, error) result
(** [with_mailbox t ~mode mailbox f] holds an exclusive selection lease on
    [t] for the duration of [f]. *)

val append_flow : t -> mailbox:string -> ?flags:string list ->
  ?internal_date:Imap.Internal_date.t ->
  length:int64 -> _ Eio.Flow.source -> (unit, error) result
(** [append_flow t ~mailbox ~length source] is [Error.Uncertain] for any
    failure after the final CRLF other than a tagged rejection. *)

type append_receipt = {
  uidvalidity : Imap.Uidvalidity.t;
  uid : Imap.Uid.t;
}

val append_flow_receipt : t -> mailbox:string -> ?flags:string list ->
  ?internal_date:Imap.Internal_date.t ->
  length:int64 -> _ Eio.Flow.source -> (append_receipt option, error) result
val append_binary_flow_receipt : t -> mailbox:string -> ?flags:string list ->
  ?internal_date:Imap.Internal_date.t -> length:int64 -> _ Eio.Flow.source ->
  (append_receipt option, error) result
val append_binary_flow : t -> mailbox:string -> ?flags:string list ->
  ?internal_date:Imap.Internal_date.t -> length:int64 -> _ Eio.Flow.source ->
  (unit, error) result
val close : t -> unit

type append_message

val append_message :
  ?flags:string list -> ?internal_date:Imap.Internal_date.t -> length:int64 ->
  _ Eio.Flow.source -> append_message
(** [append_message ~length source] borrows [source] without closing it. *)

type multiappend_receipt = {
  uidvalidity : Imap.Uidvalidity.t;
  uids : Imap.Uid.t list;
}

val append_messages : t -> mailbox:string -> append_message list ->
  (multiappend_receipt option, error) result
(** [append_messages t ~mailbox messages] sends [messages] as one RFC 3502
    atomic APPEND. *)

val noop : t -> (Imap.Response.t list, error) result
val logout : t -> (unit, error) result
