type filter =
  | Selected | Selected_delayed | Inboxes | Personal | Subscribed
  | Subtree of string list | Mailboxes of string list

type event =
  | Message_new | Message_expunge | Flag_change | Annotation_change
  | Mailbox_name | Subscription_change | Mailbox_metadata_change
  | Server_metadata_change

type group = filter * event list

let event_to_wire = function
  | Message_new -> "MessageNew" | Message_expunge -> "MessageExpunge"
  | Flag_change -> "FlagChange" | Annotation_change -> "AnnotationChange"
  | Mailbox_name -> "MailboxName" | Subscription_change -> "SubscriptionChange"
  | Mailbox_metadata_change -> "MailboxMetadataChange"
  | Server_metadata_change -> "ServerMetadataChange"

let equal_event (a : event) b = a = b

let is_selected = function
  | Selected | Selected_delayed -> true
  | Inboxes | Personal | Subscribed | Subtree _ | Mailboxes _ -> false
