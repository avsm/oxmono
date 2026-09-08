(** messages — sending, redacting and paginating room events.

    The senders build the content of an [m.room.message] for the caller.
    {!send_event} takes content already built, for any event type. Each send
    generates its own transaction identifier, so a retry after a timeout sends a
    second event rather than deduplicating. {!Send_queue} is the layer that
    retries safely. *)

(** {1 Sending} *)

type send_response = { event_id : Matrix_proto.Id.Event_id.t }
(** What every send endpoint answers. *)

val send_response_jsont : send_response Jsont.t
(** [send_response_jsont] is the JSON codec for {!send_response}, which the
    other modules that write events decode their replies with. *)

val send_event :
  Client.t ->
  room_id:Matrix_proto.Id.Room_id.t ->
  event_type:Matrix_proto.Event.Event_type.t ->
  content:Jsont.json ->
  (Matrix_proto.Id.Event_id.t, Error.t) result
(** [send_event t ~room_id ~event_type ~content] is
    [PUT /_matrix/client/v3/rooms/{roomId}/send/{eventType}/{txnId}] (Matrix
    1.0) and is the new event's identifier.

    [content] is sent verbatim. Nothing here encrypts it, so in an encrypted
    room this puts plaintext in the timeline. A user without the power level the
    room requires for [event_type] gets [M_FORBIDDEN], and too many sends in a
    row get [M_LIMIT_EXCEEDED] with a [retry_after_ms]. *)

val send_text :
  Client.t ->
  room_id:Matrix_proto.Id.Room_id.t ->
  body:string ->
  ?format:string ->
  ?formatted_body:string ->
  ?extra_content:Jsont.json ->
  unit ->
  (Matrix_proto.Id.Event_id.t, Error.t) result
(** [send_text t ~room_id ~body ()] sends an [m.text] message.

    [format] is a markup name, in practice ["org.matrix.custom.html"]. It
    defaults to absent and is meaningful only with [formatted_body].
    [formatted_body] is the marked-up body and defaults to absent. [body] stays
    the plain-text fallback and must be given whether or not it is.
    [extra_content] appends vendor fields without overriding generated fields.
*)

val send_emote :
  ?extra_content:Jsont.json ->
  Client.t ->
  room_id:Matrix_proto.Id.Room_id.t ->
  body:string ->
  (Matrix_proto.Id.Event_id.t, Error.t) result
(** [send_emote t ~room_id ~body] sends an [m.emote], the third-person form
    clients render as "* alice waves". *)

val send_notice :
  ?extra_content:Jsont.json ->
  Client.t ->
  room_id:Matrix_proto.Id.Room_id.t ->
  body:string ->
  (Matrix_proto.Id.Event_id.t, Error.t) result
(** [send_notice t ~room_id ~body] sends an [m.notice], which clients render
    more quietly and which a bot must use for anything it sends automatically,
    so that two bots cannot loop on each other. *)

val send_image :
  Client.t ->
  room_id:Matrix_proto.Id.Room_id.t ->
  body:string ->
  url:Media.Mxc.t ->
  ?info:Matrix_proto.Event.Media_info.t ->
  ?extra_content:Jsont.json ->
  unit ->
  (Matrix_proto.Id.Event_id.t, Error.t) result
(** [send_image t ~room_id ~body ~url ()] sends an [m.image]. [url] is normally
    what {!Media.val-upload} returned, and [body] is the filename or caption.

    [info] describes the attachment and defaults to absent. [extra_content]
    appends vendor fields without overriding [msgtype], [body], [url] or [info].
*)

val send_file :
  Client.t ->
  room_id:Matrix_proto.Id.Room_id.t ->
  body:string ->
  url:Media.Mxc.t ->
  ?info:Matrix_proto.Event.Media_info.t ->
  ?extra_content:Jsont.json ->
  unit ->
  (Matrix_proto.Id.Event_id.t, Error.t) result
(** [send_file t ~room_id ~body ~url ()] sends an [m.file], as {!send_image}
    does for an image. [extra_content] appends vendor fields without overriding
    generated attachment fields. *)

(** {1 Redaction} *)

val redact :
  Client.t ->
  room_id:Matrix_proto.Id.Room_id.t ->
  event_id:Matrix_proto.Id.Event_id.t ->
  ?reason:string ->
  unit ->
  (Matrix_proto.Id.Event_id.t, Error.t) result
(** [redact t ~room_id ~event_id ()] is
    [PUT /_matrix/client/v3/rooms/{roomId}/redact/{eventId}/{txnId}] (Matrix
    1.0) and is the redaction event's own identifier.

    The event stays in the timeline with its content stripped. Redacting another
    user's event needs the room's [redact] power level, and without it the reply
    is [M_FORBIDDEN]. [reason] defaults to absent. *)

(** {1 Reading} *)

type messages_response = {
  page : Matrix_proto.Event.Raw_event.t Matrix_proto.Common.Page.t;
      (** The events, newest first when paging backwards. [next_batch] continues
          in the direction the request walked, and [prev_batch] is the token the
          page began at. *)
  state : Matrix_proto.Event.Raw_event.t list;
      (** The state events needed to render the page, when a filter asked for
          them. *)
}
(** One page of a room's timeline. *)

val get_messages :
  Client.t ->
  room_id:Matrix_proto.Id.Room_id.t ->
  ?from:string ->
  ?dir:Matrix_proto.Common.Direction.t ->
  ?limit:int ->
  ?filter:string ->
  unit ->
  (messages_response, Error.t) result
(** [get_messages t ~room_id ()] is
    [GET /_matrix/client/v3/rooms/{roomId}/messages] (Matrix 1.0).

    [from] is a pagination token, either a [prev_batch] from a sync or a
    [next_batch] from an earlier page. It defaults to absent, which starts at
    the end of the room in the direction [dir] walks. [dir] defaults to
    {!Matrix_proto.Common.Direction.Backward}, so the call with neither is the
    latest page. [limit] caps the events in the page and defaults to absent,
    leaving the size to the server. [filter] is a [RoomEventFilter] as JSON
    text, or the identifier of a filter created earlier, and defaults to absent.
*)

val get_event :
  Client.t ->
  room_id:Matrix_proto.Id.Room_id.t ->
  event_id:Matrix_proto.Id.Event_id.t ->
  (Matrix_proto.Event.Raw_event.t, Error.t) result
(** [get_event t ~room_id ~event_id] is
    [GET /_matrix/client/v3/rooms/{roomId}/event/{eventId}] (Matrix 1.0). An
    event the user may not see is [M_NOT_FOUND], the same reply as one that does
    not exist. *)

type context = {
  event : Matrix_proto.Event.Raw_event.t;
  events_before : Matrix_proto.Event.Raw_event.t list;
  events_after : Matrix_proto.Event.Raw_event.t list;
  prev_batch : string option;
      (** Token for paging further back from [events_before], absent when the
          room's beginning was reached. *)
  next_batch : string option;
      (** Token for paging further forward from [events_after], absent when the
          room's end was reached. *)
  state : Matrix_proto.Event.Raw_event.t list;
      (** The room's state at the end of the returned window. *)
}
(** An event with the timeline around it. *)

val get_context :
  Client.t ->
  room_id:Matrix_proto.Id.Room_id.t ->
  event_id:Matrix_proto.Id.Event_id.t ->
  ?limit:int ->
  unit ->
  (context, Error.t) result
(** [get_context t ~room_id ~event_id ()] is
    [GET /_matrix/client/v3/rooms/{roomId}/context/{eventId}] (Matrix 1.0), the
    event plus the events on either side of it, which is how a client jumps to a
    permalink.

    [limit] counts the events before and after together rather than on each
    side. It defaults to absent, and the server's own default is 10. *)
