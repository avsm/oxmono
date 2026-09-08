(** Personal reminders linked to messages.

    A reminder is a future direct message to the user authenticated by the
    client. Its identifier differs from the identifier assigned after delivery.
*)

module Id = Zulip.Id.Reminder
(** Reminder identifiers. *)

type t = {
  id : Id.t;
  recipients : Zulip.Id.User.t list;
      (** The user to whom Zulip will send the reminder. *)
  content : string;  (** The reminder content in Zulip-flavored Markdown. *)
  rendered_content : string;  (** The reminder content rendered as HTML. *)
  scheduled_delivery_timestamp : int;
      (** The scheduled delivery time as UTC Unix seconds. *)
  failed : bool;
      (** Whether Zulip has tried and failed to deliver the reminder. *)
  target_message_id : Zulip.Id.Message.t;
      (** The message referenced by the reminder. *)
  raw : Jsont.json;
      (** The complete reminder object, including unrecognized fields. *)
}
(** The type for personal reminders. *)

type page = {
  reminders : t list;
  raw : Jsont.json;
      (** The complete response object, including unrecognized fields. *)
}
(** The type for reminder-list responses. *)

type create_result = {
  id : Id.t;
  raw : Jsont.json;
      (** The complete response object, including unrecognized fields. *)
}
(** The type for detailed reminder-creation responses. *)

val jsont : t Jsont.t
(** [jsont] is the JSON codec for reminders. *)

val page_jsont : page Jsont.t
(** [page_jsont] is the JSON codec for reminder-list responses. *)

val list : Client.t -> (page, Error.t) result
(** [list client] is every reminder belonging to the user authenticated by
    [client]. *)

val create_detailed :
  Client.t ->
  message_id:Zulip.Id.Message.t ->
  scheduled_delivery_timestamp:int ->
  ?note:string ->
  unit ->
  (create_result, Error.t) result
(** [create_detailed client ~message_id ~scheduled_delivery_timestamp ~note ()]
    is the detailed response after scheduling a reminder that references
    [message_id]. [scheduled_delivery_timestamp] is expressed as UTC Unix
    seconds. Omitting [note] creates the reminder without an additional note.
    Zulip rejects a message that the authenticated user cannot access. *)

val create :
  Client.t ->
  message_id:Zulip.Id.Message.t ->
  scheduled_delivery_timestamp:int ->
  ?note:string ->
  unit ->
  (Id.t, Error.t) result
(** [create client ~message_id ~scheduled_delivery_timestamp ~note ()] is the
    identifier of a new reminder that references [message_id].
    [scheduled_delivery_timestamp] is expressed as UTC Unix seconds. Omitting
    [note] creates the reminder without an additional note. Zulip rejects a
    message that the authenticated user cannot access. *)

val delete : Client.t -> reminder_id:Id.t -> (unit, Error.t) result
(** [delete client ~reminder_id] deletes the reminder identified by
    [reminder_id] and cancels its delivery. *)
