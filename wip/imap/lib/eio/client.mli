@@ portable

(** A single IMAP connection with commands serialized across fibers,
    documented in [Imap_eio.Client]. *)

type t
type error = Error.t

val pp_error : Format.formatter -> error -> unit
val error_to_string : error -> string

val connect :
  sw:Eio.Switch.t -> ?auth:Auth.t -> Transport.t ->
  (t, error) result @@ nonportable

val of_flow :
  sw:Eio.Switch.t -> ?auth:Auth.t ->
  [> Eio.Flow.two_way_ty | Eio.Resource.close_ty ] Eio.Resource.t ->
  (t, error) result @@ nonportable

val capabilities : t -> Imap.Capability.Set.t
val enabled : t -> Imap.Capability.Set.t
val has : t -> Imap.Capability.t -> bool
val is_enabled : t -> Imap.Capability.t -> bool
val is_open : t -> bool
val enable : t -> Imap.Capability.t list ->
  (Imap.Capability.t list, error) result @@ nonportable

type mailbox_entry = {
  name : Imap.Mailbox_name.t;
  info : Imap.Response.list_result;
}

val list : t -> ?reference:string -> pattern:string ->
  unit -> (mailbox_entry list, error) result @@ nonportable
val lsub : t -> ?reference:string -> pattern:string ->
  unit -> (mailbox_entry list, error) result @@ nonportable
val namespace : t -> (Imap.Response.namespace, error) result @@ nonportable

type discovery = {
  mailboxes : (mailbox_entry * Imap.Response.mailbox_status option) list;
  unpaired_status : Imap.Response.mailbox_status list;
}

val list_extended : t -> ?reference:string -> patterns:string list ->
  ?selection:Imap.Mailbox_list.selection list ->
  ?returns:Imap.Mailbox_list.return list ->
  ?status:Imap.Status_item.t list -> unit ->
  (discovery, error) result @@ nonportable
val mailbox_mode : t -> Imap.Mailbox_name.mode
val status : t -> mailbox:string -> items:Imap.Status_item.t list ->
  (Imap.Response.mailbox_status, error) result @@ nonportable
val get_jmap_access : t -> (string, error) result @@ nonportable

type metadata_result = {
  responses : Imap.Response.metadata list;
  longentries : int64 option;
}

val create_mailbox : t -> mailbox:string -> (unit, error) result @@ nonportable
val delete_mailbox : t -> mailbox:string -> (unit, error) result @@ nonportable
val rename_mailbox : t -> old_name:string -> new_name:string ->
  (unit, error) result @@ nonportable
val subscribe_mailbox : t -> mailbox:string ->
  (unit, error) result @@ nonportable
val unsubscribe_mailbox : t -> mailbox:string ->
  (unit, error) result @@ nonportable

val with_mailbox : t -> ?qresync:(Imap.Uidvalidity.t * Imap.Modseq.t) ->
  ?objectid:(string * string) ->
  mode:[ `Read_only | `Read_write ] -> string ->
  (Selected.t -> ('a, error) result) -> ('a, error) result @@ nonportable
(** [with_mailbox t ~mode mailbox f] holds an exclusive selection lease on
    [t] for the duration of [f]. *)

type append_receipt = {
  uidvalidity : Imap.Uidvalidity.t;
  uid : Imap.Uid.t;
}

type append_message

val append_message :
  ?flags:Mail_flag.Imap_flag.t list -> ?internal_date:Imap.Internal_date.t ->
  length:int64 -> _ Eio.Flow.source -> append_message
(** [append_message ~length source] borrows [source] without closing it. *)

val append : t -> mailbox:string -> ?binary:bool -> append_message ->
  (append_receipt option, error) result @@ nonportable
(** [append t ~mailbox message] is [Error.Uncertain] for any failure after
    the final CRLF other than a tagged rejection. *)

val close : t -> unit

type multiappend_receipt = {
  uidvalidity : Imap.Uidvalidity.t;
  uids : Imap.Uid.t list;
}

val noop : t -> (Imap.Response.t list, error) result @@ nonportable
val logout : t -> (unit, error) result @@ nonportable

(** Each submodule's [t] is a witness that its extension is usable on one
    connection. *)

module Acl : sig
  type client := t
  type t
  val require : client -> (t, error) result @@ portable
  val get_acl : t -> mailbox:string -> (Imap.Response.acl, error) result
  val list_rights : t -> mailbox:string -> identifier:string ->
    (Imap.Response.list_rights, error) result
  val my_rights : t -> mailbox:string ->
    (Imap.Response.my_rights, error) result
  val set_acl : t -> mailbox:string -> identifier:string ->
    operation:[ `Add | `Remove | `Replace ] -> rights:string ->
    (unit, error) result
  val delete_acl : t -> mailbox:string -> identifier:string ->
    (unit, error) result
end @@ nonportable

module Quota : sig
  type client := t
  type t
  val require : client -> (t, error) result @@ portable
  val get_quota : t -> root:string -> (Imap.Response.quota, error) result
  val get_quota_root : t -> mailbox:string ->
    ((Imap.Response.quota_root * Imap.Response.quota list), error) result
  val set_quota : t -> root:string -> limits:(string * int64) list ->
    (Imap.Response.quota option, error) result
end @@ nonportable

module Metadata : sig
  type client := t
  type t
  val require : client -> (t, error) result @@ portable
  val get_metadata : t -> mailbox:string -> entries:string list ->
    ?maxsize:int64 -> ?depth:Imap.Metadata.depth -> unit ->
    (metadata_result, error) result
  val set_metadata : t -> mailbox:string ->
    values:(string * string option) list -> (unit, error) result
end @@ nonportable

module Notify : sig
  type client := t
  type t
  val require : client -> (t, error) result @@ portable
  val notify_set : t -> ?status:bool -> groups:Imap.Notify.group list ->
    unit -> (Imap.Response.mailbox_status list, error) result
  val notify_none : t -> (unit, error) result
end @@ nonportable

module Multiappend : sig
  type client := t
  type t
  val require : client -> (t, error) result @@ portable
  val append_many : t -> mailbox:string -> append_message list ->
    (multiappend_receipt option, error) result
  (** [append_many t ~mailbox messages] sends [messages] as one RFC 3502
      atomic APPEND. *)
end @@ nonportable

module Compress : sig
  type client := t
  type t
  val require : client -> (t, error) result @@ portable
  val activate : t -> (unit, error) result
end @@ nonportable

module Objectid_plus : sig
  type client := t
  type t
  val enable : client -> (t, error) result
  val pin_mailbox : t -> mailbox:string -> account_id:string ->
    mailbox_id:string -> (unit, error) result @@ portable
  (** [pin_mailbox t ~mailbox ~account_id ~mailbox_id] makes later
      selections and APPENDs of [mailbox] on the connection verify that
      identity. *)

  val create_mailbox : t -> mailbox:string ->
    (Imap.Response.compound_object_id, error) result
  val rename_mailbox : t -> old_name:string -> new_name:string ->
    (Imap.Response.compound_object_id, error) result
  val status : t -> mailbox:string -> items:Imap.Status_item.t list ->
    (Imap.Response.mailbox_status, error) result
end @@ nonportable

module Uidonly : sig
  type client := t
  type t
  val enable : client -> (t, error) result
end @@ nonportable
