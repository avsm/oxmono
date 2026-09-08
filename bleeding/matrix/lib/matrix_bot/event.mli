(** event — what a bot is told about.

    Events are derived from the room's synced events, one per event in room
    order, after the bot's own events and anything older than its cursor have
    been dropped. They come from {!Matrix_ui.Event_cache.val-events} rather than
    from {!Matrix_ui.Room_timeline.val-items}, so a reaction, a redaction and an
    edit arrive as themselves rather than folded into the item they act on. An
    event whose room key has not arrived is not delivered as anything until it
    decrypts, and then once.

    An event type the projection does not know is delivered as {!Custom}, with
    its JSON content intact. A malformed event reaches no handler.

    Every event that happened in a room carries an {!type-envelope}, and the
    rest of its record is what that kind of event adds. *)

type envelope = {
  room : Room.t;  (** The handle to send through, and to read the room from. *)
  sender : Matrix_proto.Id.User_id.t;  (** Who put the event in the room. *)
  event_id : Matrix_proto.Id.Event_id.t;
}
(** The type for where an event came from. *)

type message = {
  envelope : envelope;
  content : Matrix_ui.Presentation.message;
      (** The message itself, with its type, its body and its sanitised HTML. *)
  presentation : Matrix_ui.Presentation.t;
      (** The whole projection, for the timestamp, the relation and the event it
          was read from. *)
  reply_to : Matrix_proto.Id.Event_id.t option;
      (** The event this answers, from its [m.in_reply_to]. *)
}
(** The type for an [m.room.message]. *)

type command = {
  message : message;
  name : string;  (** Without the prefix. *)
  args : string;  (** Everything after the name, trimmed; may be empty. *)
  argv : string list;  (** [args] split on whitespace. *)
}
(** The type for a message that begins with the bot's prefix. *)

type edit = {
  message : message;  (** The new content. *)
  original : Matrix_proto.Id.Event_id.t;  (** The event it replaces. *)
}
(** The type for an [m.replace]. *)

type sticker = {
  envelope : envelope;
  body : string;  (** The description, which stands in for the image. *)
  url : string option;  (** The [mxc://] URL of the image. *)
}
(** The type for an [m.sticker]. *)

type poll = {
  envelope : envelope;
  text : string;  (** The question, as the poll's fallback text. *)
}
(** The type for the start of a poll ([MSC3381]). Votes and the end of it are
    not delivered. *)

type reaction = {
  envelope : envelope;
  key : string;  (** What was reacted with, usually an emoji. *)
  relates_to : Matrix_proto.Id.Event_id.t;  (** The event annotated. *)
}
(** The type for an [m.reaction]. *)

type redaction = {
  envelope : envelope;
  target : Matrix_proto.Id.Event_id.t option;
      (** The event removed, and [None] when the redaction names none. *)
  reason : string option;  (** Why the event was removed, if given. *)
}
(** The type for an [m.room.redaction]. *)

type membership = {
  envelope : envelope;
  user : Matrix_proto.Id.User_id.t;
      (** The subject, who is the sender only when they acted on themselves. *)
  change : Matrix_ui.Presentation.membership_change;
  reason : string option;  (** Why the membership changed, if given. *)
}
(** The type for an [m.room.member] event that changes a membership. *)

type profile = {
  envelope : envelope;
  user : Matrix_proto.Id.User_id.t;
  change : Matrix_ui.Presentation.profile_change;
}
(** The type for an [m.room.member] event that changes a display name or an
    avatar rather than a membership. *)

type room_state = {
  envelope : envelope;
  state : Matrix_ui.Presentation.other_state;
}
(** The type for a state event that is neither a membership nor a profile
    change. *)

type custom = {
  envelope : envelope;
  event_type : string;  (** The Matrix event type. *)
  content : Jsont.json;  (** The event's content, passed through unread. *)
  presentation : Matrix_ui.Presentation.t;
      (** The whole projection, including the timestamp, relation and raw event.
      *)
}
(** The type for an event the projection does not otherwise model. *)

type invitation = {
  room_id : Matrix_proto.Id.Room_id.t;
  inviter : Matrix_proto.Id.User_id.t option;
      (** [None] when the invitation reached the bot without its sender. *)
}
(** The type for an invitation the bot has not acted on. *)

(** The type for what a bot is told about. *)
type t =
  | Message of message
      (** A message from another user that is not a command. Notices are
          included only when the bot was built with [~ignore_notices:false]. *)
  | Command of command
      (** A message beginning with the bot's prefix. Delivered whether or not a
          handler is registered for its name. *)
  | Edit of edit
  | Sticker of sticker
  | Poll of poll
  | Reaction of reaction
  | Redaction of redaction
  | Membership of membership
  | Profile of profile
  | Room_state of room_state
  | Custom of custom
  | Invited of invitation  (** Delivered before any automatic join. *)
  | Joined of Room.t
      (** The room is now handled; delivered before its first event. *)
  | Left of Matrix_proto.Id.Room_id.t
  | Sync of Matrix_ui.Runtime.sync_state
      (** Delivered outside any room, in the order the states happen.
          {!Matrix_ui.Runtime.Not_started} and {!Matrix_ui.Runtime.Syncing} are
          not delivered, and {!Matrix_ui.Runtime.Live} is delivered once rather
          than once per batch. Failures and {!Matrix_ui.Runtime.Offline} are
          delivered so a bot can report a degraded connection. *)

val envelope : t -> envelope option
(** [envelope t] is where the event came from, and [None] for {!Invited},
    {!Joined}, {!Left} and {!Sync}, which are not events in a room the bot
    handles. *)

val room : t -> Room.t option
(** [room t] is the room the event happened in, and the room {!Joined} names. It
    is [None] for {!Invited}, {!Left} and {!Sync}, whose rooms the bot has no
    handle for. *)

val room_id : t -> Matrix_proto.Id.Room_id.t option
(** [room_id t] is {!val-room}'s id, and also the room an {!Invited} or a
    {!Left} names. It is [None] for {!Sync} alone. *)

val sender : t -> Matrix_proto.Id.User_id.t option
(** [sender t] is who caused the event, which is the envelope's sender and the
    inviter of an {!Invited}. For a {!Membership} it is who acted rather than
    who the change is about. It is [None] for {!Joined}, {!Left} and {!Sync}. *)

val pp : Format.formatter -> t -> unit
(** [pp ppf t] prints one line naming the kind, the room and the sender, without
    a trailing newline. *)

(** {1 Answering} *)

val reply : envelope -> ?html:string -> string -> Sent.t
(** [reply e body] queues an [m.notice] in the event's room, in reply to the
    event. [html] is sent as the [formatted_body]. Without it the message is
    plain. *)

val react : envelope -> string -> Sent.t
(** [react e key] annotates the event with [key], usually an emoji. *)
