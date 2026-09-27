(** NOTIFY filters and events.

    The vocabulary of the RFC 5465 NOTIFY command. {!Command.notify_set}
    checks the rules that tie events to filters. *)

type filter =
  | Selected
  | Selected_delayed
  | Inboxes
  | Personal
  | Subscribed
  | Subtree of string list  (** Nonempty wire mailbox names. *)
  | Mailboxes of string list  (** Nonempty wire mailbox names. *)
(** The type for mailbox filters, which choose the mailboxes an event group
    watches. *)

type event =
  | Message_new
  | Message_expunge
  | Flag_change
  | Annotation_change
  | Mailbox_name
  | Subscription_change
  | Mailbox_metadata_change
  | Server_metadata_change
(** The type for NOTIFY events. *)

type group = filter * event list
(** The type for event groups, a filter with its events. An empty event
    list means [NONE]. *)

val event_to_wire : event -> string
(** [event_to_wire e] is the RFC 5465 event name of [e], such as
    [MessageNew]. *)

val equal_event : event -> event -> bool
(** [equal_event a b] is [true] if [a] and [b] are the same event. *)

val is_selected : filter -> bool
(** [is_selected f] is [true] if [f] is [Selected] or [Selected_delayed],
    the filters that apply to the selected mailbox. *)
