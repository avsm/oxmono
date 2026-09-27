(** RFC 5465 NOTIFY filters and events. *)

type filter =
  | Selected
  | Selected_delayed
  | Inboxes
  | Personal
  | Subscribed
  | Subtree of string list  (** Nonempty wire mailbox names. *)
  | Mailboxes of string list  (** Nonempty wire mailbox names. *)

type event =
  | Message_new
  | Message_expunge
  | Flag_change
  | Annotation_change
  | Mailbox_name
  | Subscription_change
  | Mailbox_metadata_change
  | Server_metadata_change

type group = filter * event list
(** A filter with its events. An empty list means [NONE]. *)

val event_to_wire : event -> string
(** [event_to_wire e] is the RFC 5465 event name of [e], such as
    [MessageNew]. *)

val equal_event : event -> event -> bool
(** [equal_event a b] holds when [a] and [b] are the same event. *)

val is_selected : filter -> bool
(** [is_selected f] holds for [Selected] and [Selected_delayed], the filters
    that apply to the selected mailbox. *)
