(** state — the current state events of a room, raising instead of returning.

    Every function raises [Eio.Io] carrying [Error.E e] where
    {!Matrix_client.State} returns [Error e]. That module documents what each
    call does, which endpoint it uses and which errors it produces. *)

(** {1 Any state event} *)

val get_state :
  Client.t ->
  room_id:Matrix_proto.Id.Room_id.t ->
  Matrix_proto.Event.Raw_event.t list
(** [get_state c ~room_id] is {!Matrix_client.State.get_state} with the result
    unwrapped. *)

val get_state_event :
  Client.t ->
  room_id:Matrix_proto.Id.Room_id.t ->
  event_type:Matrix_proto.Event.Event_type.t ->
  ?state_key:string ->
  unit ->
  Jsont.json
(** [get_state_event c ~room_id ~event_type ()] is
    {!Matrix_client.State.get_state_event} with the result unwrapped. *)

val set_state :
  Client.t ->
  room_id:Matrix_proto.Id.Room_id.t ->
  event_type:Matrix_proto.Event.Event_type.t ->
  ?state_key:string ->
  content:Jsont.json ->
  unit ->
  Matrix_proto.Id.Event_id.t
(** [set_state c ~room_id ~event_type ~content ()] is
    {!Matrix_client.State.set_state} with the result unwrapped. *)

(** {1 Name, topic and avatar} *)

val get_name : Client.t -> room_id:Matrix_proto.Id.Room_id.t -> string option
(** [get_name c ~room_id] is {!Matrix_client.State.get_name} with the result
    unwrapped. *)

val set_name :
  Client.t ->
  room_id:Matrix_proto.Id.Room_id.t ->
  name:string ->
  Matrix_proto.Id.Event_id.t
(** [set_name c ~room_id ~name] is {!Matrix_client.State.set_name} with the
    result unwrapped. *)

val get_topic : Client.t -> room_id:Matrix_proto.Id.Room_id.t -> string option
(** [get_topic c ~room_id] is {!Matrix_client.State.get_topic} with the result
    unwrapped. *)

val set_topic :
  Client.t ->
  room_id:Matrix_proto.Id.Room_id.t ->
  topic:string ->
  Matrix_proto.Id.Event_id.t
(** [set_topic c ~room_id ~topic] is {!Matrix_client.State.set_topic} with the
    result unwrapped. *)

val get_avatar :
  Client.t ->
  room_id:Matrix_proto.Id.Room_id.t ->
  Matrix_client.Media.Mxc.t option
(** [get_avatar c ~room_id] is {!Matrix_client.State.get_avatar} with the result
    unwrapped. *)

val set_avatar :
  Client.t ->
  room_id:Matrix_proto.Id.Room_id.t ->
  avatar_url:Matrix_client.Media.Mxc.t ->
  Matrix_proto.Id.Event_id.t
(** [set_avatar c ~room_id ~avatar_url] is {!Matrix_client.State.set_avatar}
    with the result unwrapped. *)
