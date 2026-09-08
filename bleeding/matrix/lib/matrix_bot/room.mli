(** room — a room the bot is in, as the handle handlers send through.

    Every send goes through the runtime's send queue, so it is encrypted when
    the room is, retried on rate limits, and answered by a {!Sent.t}. A message
    a bot sends should be an [m.notice], which a client answering messages is
    required to ignore, so that two bots in one room do not answer each other
    for ever. Administrative calls go straight to the homeserver and return its
    answer. *)

type t
(** The type for room handles. A handler receives one rather than building it.
*)

val id : t -> Matrix_proto.Id.Room_id.t
(** [id t] is the room's identifier. *)

val name : t -> string
(** [name t] is the room's display name, or its identifier when the room list
    has no entry for it. *)

val topic : t -> string option
(** [topic t] is the room's topic, and [None] when it has none. *)

val encrypted : t -> bool
(** [encrypted t] is [true] when the room has an [m.room.encryption] state
    event. *)

val is_dm : t -> bool
(** [is_dm t] is [true] when the room is tagged as a direct message. *)

val members : t -> Matrix_proto.Id.User_id.t list
(** [members t] is the joined members as the sync state knows them. *)

val sync_members : t -> (unit, Matrix_client.Error.t) result
(** [sync_members t] fetches [/rooms/{roomId}/members] when the synchronized
    member state is incomplete, installs the authoritative result in the runtime
    and makes it durable when the runtime has a base-state store. It is a no-op
    when the state is already complete. *)

val ready_to_send : t -> bool
(** [ready_to_send t] is [true] once synchronization has established whether the
    room is encrypted and, for an encrypted room, supplied a complete recipient
    list and installed matching encryption settings. It is [false] rather than
    risking a plaintext send while encryption state is incomplete, or encrypting
    to only part of the room. *)

val await_ready_to_send : ?timeout:float -> t -> bool
(** [await_ready_to_send t] waits until {!ready_to_send} is [true], or until
    [timeout] seconds have elapsed, and says which happened. [timeout] defaults
    to 120 seconds; zero performs one immediate readiness check. It raises
    [Invalid_argument] for a negative or NaN timeout. *)

val info : t -> Matrix_ui.Room_list.room option
(** [info t] is the room list's record, with the preview and the unread counts.
    It is [None] until the sync that carries the room arrives. *)

val timeline : t -> Matrix_ui.Room_timeline.t
(** [timeline t] is the underlying model, for a bot that wants items, reactions
    or the read marker rather than events. *)

(** {1 Sending} *)

val send_text :
  t -> ?html:string -> ?reply_to:Matrix_proto.Id.Event_id.t -> string -> Sent.t
(** [send_text t body] queues an [m.text] message. [html] is sent as the
    [formatted_body], sanitised to the Matrix subset on the way out, and [body]
    is what a client without HTML shows. Without [html] the message is plain.
    [reply_to] names the event the message answers, through an [m.in_reply_to]
    relation. Without it the message stands alone. *)

val send_notice :
  t -> ?html:string -> ?reply_to:Matrix_proto.Id.Event_id.t -> string -> Sent.t
(** [send_notice t body] is {!send_text} with an [m.notice] message type, which
    is what a bot's own output should be. *)

val send_emote : t -> string -> Sent.t
(** [send_emote t body] queues an [m.emote], the message a client renders as the
    sender doing something. *)

val react : t -> Matrix_proto.Id.Event_id.t -> string -> Sent.t
(** [react t target key] annotates [target] with [key], usually an emoji. *)

val redact : t -> ?reason:string -> Matrix_proto.Id.Event_id.t -> Sent.t
(** [redact t event_id] removes the event's content. [reason] is recorded with
    the redaction and shown by clients. Without it none is given. Redacting
    another user's event needs the room's redaction power level. *)

(** {1 Administration} *)

val set_topic : t -> string -> (unit, Matrix_client.Error.t) result
(** [set_topic t topic] sets the room's [m.room.topic]. *)

val set_name : t -> string -> (unit, Matrix_client.Error.t) result
(** [set_name t name] sets the room's [m.room.name]. *)

val invite :
  t -> Matrix_proto.Id.User_id.t -> (unit, Matrix_client.Error.t) result
(** [invite t user_id] invites the user to the room. *)

val kick :
  t ->
  ?reason:string ->
  Matrix_proto.Id.User_id.t ->
  (unit, Matrix_client.Error.t) result
(** [kick t user_id] removes the user from the room, who may be invited back.
    [reason] is recorded with the membership event. Without it none is given. *)

val ban :
  t ->
  ?reason:string ->
  Matrix_proto.Id.User_id.t ->
  (unit, Matrix_client.Error.t) result
(** [ban t user_id] removes the user and stops them rejoining. [reason] is
    recorded with the membership event. Without it none is given. *)

val leave : t -> (unit, Matrix_client.Error.t) result
(** [leave t] leaves the room. The bot stops receiving its events on the sync
    that carries the departure. *)

val power_level : t -> Matrix_proto.Id.User_id.t -> int
(** [power_level t user_id] is the user's level under the room's
    [m.room.power_levels], or the default for users. It is read from the state
    the sync loop holds and makes no request. *)

(** {1 History} *)

val backfill :
  t ->
  ?limit:int ->
  ?pages:int ->
  unit ->
  (Matrix_ui.Room_timeline.pagination, Matrix_client.Error.t) result
(** [backfill t ()] paginates back [pages] times (default 20) of [limit] events
    (default 50). [`Reached_start] is the room's beginning. [`More] is history
    left unfetched after the last page. [`Nothing_to_do] is a page that fetched
    nothing, because there was no token, the gap was gone or another pagination
    was in flight, and it stops the walk with the remaining pages unattempted.
    The first page that fails stops the walk and its error is the result. The
    events arrive as items on {!timeline}, and reach a handler as {!Event.t}
    like anything else. *)

module Internal : sig
  (** How {!Bot} makes one. Not for a handler to call. *)

  val v :
    runtime:Matrix_ui.Runtime.t ->
    client:Matrix_eio.Client.t ->
    sender:Sent.Internal.tracker ->
    clock:Context.clock ->
    encryption:Matrix_eio.Encryption.t option ->
    Matrix_proto.Id.Room_id.t ->
    t
  (** [v ~runtime ~client ~sender ~clock ~encryption room_id] is the handle
      {!Bot.run} makes when it starts handling a room, and hands to every event
      from it. *)
end
