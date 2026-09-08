(** Messages scheduled for future delivery.

    Scheduled messages belong to the user authenticated by the client. Their
    identifiers differ from the identifiers assigned after delivery. *)

module Id = Zulip.Id.Scheduled_message
(** Scheduled-message identifiers. *)

type destination =
  | Channel of Zulip.Id.Channel.t
  | Direct of Zulip.Id.User.t list
      (** The type for scheduled-message destinations. *)

type t = {
  id : Id.t;
  destination : destination;
  topic : string option;
      (** The channel topic, or [None] for a direct message. *)
  content : string;  (** The content in Zulip-flavored Markdown. *)
  rendered_content : string;  (** The content rendered as HTML. *)
  scheduled_delivery_timestamp : int;
      (** The scheduled delivery time as UTC Unix seconds. *)
  failed : bool;
      (** Whether Zulip has tried and failed to deliver the message. *)
  raw : Jsont.json;
      (** The complete scheduled-message object, including unrecognized fields.
      *)
}
(** The type for scheduled messages. *)

type page = {
  scheduled_messages : t list;
  raw : Jsont.json;
      (** The complete response object, including unrecognized fields. *)
}
(** The type for scheduled-message list responses. *)

type create_result = {
  id : Id.t;
  raw : Jsont.json;
      (** The complete response object, including unrecognized fields. *)
}
(** The type for detailed scheduled-message creation responses. *)

type update = {
  destination : destination option;
      (** The replacement destination, or [None] to retain the existing one. *)
  topic : string option;
      (** The replacement topic, or [None] to retain the existing one. *)
  content : string option;
      (** Replacement Zulip-flavored Markdown, or [None] to retain the existing
          content. *)
  scheduled_delivery_timestamp : int option;
      (** The replacement delivery time as UTC Unix seconds, or [None] to retain
          the existing time. *)
}
(** The type for scheduled-message updates. *)

val jsont : t Jsont.t
(** [jsont] is the JSON codec for scheduled messages. *)

val page_jsont : page Jsont.t
(** [page_jsont] is the JSON codec for scheduled-message list responses. *)

val list : Client.t -> (page, Error.t) result
(** [list client] is every scheduled message belonging to the user authenticated
    by [client]. *)

val create_detailed :
  Client.t ->
  destination:destination ->
  ?topic:string ->
  ?read_by_sender:bool ->
  content:string ->
  scheduled_delivery_timestamp:int ->
  unit ->
  (create_result, Error.t) result
(** [create_detailed client ~destination ~topic ~read_by_sender ~content
     ~scheduled_delivery_timestamp ()] is the detailed response after scheduling
    [content] for delivery to [destination] at [scheduled_delivery_timestamp],
    expressed as UTC Unix seconds. [content] is Zulip-flavored Markdown.
    [read_by_sender] defaults to [true]. A channel destination requires [topic].
    The empty topic is passed to Zulip and is valid unless the organization
    requires topics. A direct destination with a topic returns
    {!Error.t.constructor-Invalid_request}. *)

val create :
  Client.t ->
  destination:destination ->
  ?topic:string ->
  ?read_by_sender:bool ->
  content:string ->
  scheduled_delivery_timestamp:int ->
  unit ->
  (Id.t, Error.t) result
(** [create client ~destination ~topic ~read_by_sender ~content
     ~scheduled_delivery_timestamp ()] is the identifier assigned after
    scheduling [content] for delivery to [destination] at
    [scheduled_delivery_timestamp], expressed as UTC Unix seconds.
    [read_by_sender] defaults to [true]. A channel destination requires [topic].
    The empty topic is passed to Zulip and is valid unless the organization
    requires topics. A direct destination with a topic returns
    {!Error.t.constructor-Invalid_request}. *)

val update :
  Client.t -> scheduled_message_id:Id.t -> update -> (unit, Error.t) result
(** [update client ~scheduled_message_id changes] applies [changes] to the
    scheduled message identified by [scheduled_message_id]. Omitted fields
    retain their existing values. Changing only the destination to a channel may
    retain the existing topic. Zulip requires a topic when changing a direct
    message to a channel message. A direct destination combined with a topic, or
    an update with no changes, returns {!Error.t.constructor-Invalid_request}. A
    failed scheduled message requires a new [scheduled_delivery_timestamp]. *)

val delete : Client.t -> scheduled_message_id:Id.t -> (unit, Error.t) result
(** [delete client ~scheduled_message_id] deletes the scheduled message
    identified by [scheduled_message_id] and cancels its delivery. *)
