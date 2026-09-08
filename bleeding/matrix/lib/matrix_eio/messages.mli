(** messages — sending, redacting and paginating room events, raising instead of
    returning.

    Every function raises [Eio.Io] carrying [Error.E e] where
    {!Matrix_client.Messages} returns [Error e]. That module documents what each
    call does, which endpoint it uses and which errors it produces.

    Nothing here encrypts, so in an encrypted room these put plaintext in the
    timeline. Use {!Matrix_eio.Encryption} instead. *)

(** {1 Sending} *)

val send_event :
  Client.t ->
  room_id:Matrix_proto.Id.Room_id.t ->
  event_type:Matrix_proto.Event.Event_type.t ->
  content:Jsont.json ->
  Matrix_proto.Id.Event_id.t
(** [send_event c ~room_id ~event_type ~content] is
    {!Matrix_client.Messages.send_event} with the result unwrapped. *)

val send_text :
  Client.t ->
  room_id:Matrix_proto.Id.Room_id.t ->
  body:string ->
  ?format:string ->
  ?formatted_body:string ->
  ?extra_content:Jsont.json ->
  unit ->
  Matrix_proto.Id.Event_id.t
(** [send_text c ~room_id ~body ()] is {!Matrix_client.Messages.send_text} with
    the result unwrapped. *)

val send_emote :
  ?extra_content:Jsont.json ->
  Client.t ->
  room_id:Matrix_proto.Id.Room_id.t ->
  body:string ->
  Matrix_proto.Id.Event_id.t
(** [send_emote c ~room_id ~body] is {!Matrix_client.Messages.send_emote} with
    the result unwrapped. *)

val send_notice :
  ?extra_content:Jsont.json ->
  Client.t ->
  room_id:Matrix_proto.Id.Room_id.t ->
  body:string ->
  Matrix_proto.Id.Event_id.t
(** [send_notice c ~room_id ~body] is {!Matrix_client.Messages.send_notice} with
    the result unwrapped. *)

val send_image :
  Client.t ->
  room_id:Matrix_proto.Id.Room_id.t ->
  body:string ->
  url:Matrix_client.Media.Mxc.t ->
  ?info:Matrix_proto.Event.Media_info.t ->
  ?extra_content:Jsont.json ->
  unit ->
  Matrix_proto.Id.Event_id.t
(** [send_image c ~room_id ~body ~url ()] is
    {!Matrix_client.Messages.send_image} with the result unwrapped. *)

val send_file :
  Client.t ->
  room_id:Matrix_proto.Id.Room_id.t ->
  body:string ->
  url:Matrix_client.Media.Mxc.t ->
  ?info:Matrix_proto.Event.Media_info.t ->
  ?extra_content:Jsont.json ->
  unit ->
  Matrix_proto.Id.Event_id.t
(** [send_file c ~room_id ~body ~url ()] is {!Matrix_client.Messages.send_file}
    with the result unwrapped. *)

(** {1 Redaction} *)

val redact :
  Client.t ->
  room_id:Matrix_proto.Id.Room_id.t ->
  event_id:Matrix_proto.Id.Event_id.t ->
  ?reason:string ->
  unit ->
  Matrix_proto.Id.Event_id.t
(** [redact c ~room_id ~event_id ()] is {!Matrix_client.Messages.redact} with
    the result unwrapped. *)

(** {1 Reading} *)

type messages_response = Matrix_client.Messages.messages_response = {
  page : Matrix_proto.Event.Raw_event.t Matrix_proto.Common.Page.t;
  state : Matrix_proto.Event.Raw_event.t list;
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
  messages_response
(** [get_messages c ~room_id ()] is {!Matrix_client.Messages.get_messages} with
    the result unwrapped. *)

val get_event :
  Client.t ->
  room_id:Matrix_proto.Id.Room_id.t ->
  event_id:Matrix_proto.Id.Event_id.t ->
  Matrix_proto.Event.Raw_event.t
(** [get_event c ~room_id ~event_id] is {!Matrix_client.Messages.get_event} with
    the result unwrapped. *)

type context = Matrix_client.Messages.context = {
  event : Matrix_proto.Event.Raw_event.t;
  events_before : Matrix_proto.Event.Raw_event.t list;
  events_after : Matrix_proto.Event.Raw_event.t list;
  prev_batch : string option;
  next_batch : string option;
  state : Matrix_proto.Event.Raw_event.t list;
}
(** An event with the timeline around it. *)

val get_context :
  Client.t ->
  room_id:Matrix_proto.Id.Room_id.t ->
  event_id:Matrix_proto.Id.Event_id.t ->
  ?limit:int ->
  unit ->
  context
(** [get_context c ~room_id ~event_id ()] is
    {!Matrix_client.Messages.get_context} with the result unwrapped. *)
