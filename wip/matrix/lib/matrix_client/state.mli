(** state — the current state events of a room.

    State is the set of events in a room keyed by a type and a state key, with
    the most recent event under a key replacing the one before it. The name,
    topic and avatar helpers are the three keys a client almost always wants,
    read and written through the generic pair. *)

(** {1 Any state event} *)

val get_state :
  Client.t ->
  room_id:Matrix_proto.Id.Room_id.t ->
  (Matrix_proto.Event.Raw_event.t list, Error.t) result
(** [get_state t ~room_id] is [GET /_matrix/client/v3/rooms/{roomId}/state]
    (Matrix 1.0), every current state event in the room. A room the user has
    left reports the state as it was when they left, and a room they were never
    in is [M_FORBIDDEN]. *)

val get_state_event :
  Client.t ->
  room_id:Matrix_proto.Id.Room_id.t ->
  event_type:Matrix_proto.Event.Event_type.t ->
  ?state_key:string ->
  unit ->
  (Jsont.json, Error.t) result
(** [get_state_event t ~room_id ~event_type ()] is
    [GET /_matrix/client/v3/rooms/{roomId}/state/{type}/{key}] (Matrix 1.0),
    which answers the {e content} of the event rather than the event. A key that
    was never set is [M_NOT_FOUND].

    [state_key] defaults to [""], the key of a singleton state event. *)

val set_state :
  Client.t ->
  room_id:Matrix_proto.Id.Room_id.t ->
  event_type:Matrix_proto.Event.Event_type.t ->
  ?state_key:string ->
  content:Jsont.json ->
  unit ->
  (Matrix_proto.Id.Event_id.t, Error.t) result
(** [set_state t ~room_id ~event_type ~content ()] is
    [PUT /_matrix/client/v3/rooms/{roomId}/state/{type}/{key}] (Matrix 1.0) and
    is the new event's identifier. [content] replaces what was under the key
    rather than merging with it.

    A user without the power level the room requires for [event_type] gets
    [M_FORBIDDEN], and content the room's rules reject is [M_BAD_JSON].

    [state_key] defaults to [""]. *)

(** {1 Name, topic and avatar}

    Each getter is [None] both when the event was never set and when its content
    does not carry the member the specification defines, so a malformed room
    reads as an unset one rather than failing. *)

val get_name :
  Client.t ->
  room_id:Matrix_proto.Id.Room_id.t ->
  (string option, Error.t) result
(** [get_name t ~room_id] is the room's [m.room.name]. *)

val set_name :
  Client.t ->
  room_id:Matrix_proto.Id.Room_id.t ->
  name:string ->
  (Matrix_proto.Id.Event_id.t, Error.t) result
(** [set_name t ~room_id ~name] sets [m.room.name] and is the new event's
    identifier. *)

val get_topic :
  Client.t ->
  room_id:Matrix_proto.Id.Room_id.t ->
  (string option, Error.t) result
(** [get_topic t ~room_id] is the room's [m.room.topic]. *)

val set_topic :
  Client.t ->
  room_id:Matrix_proto.Id.Room_id.t ->
  topic:string ->
  (Matrix_proto.Id.Event_id.t, Error.t) result
(** [set_topic t ~room_id ~topic] sets [m.room.topic] and is the new event's
    identifier. *)

val get_avatar :
  Client.t ->
  room_id:Matrix_proto.Id.Room_id.t ->
  (Media.Mxc.t option, Error.t) result
(** [get_avatar t ~room_id] is the URI in the room's [m.room.avatar]. *)

val set_avatar :
  Client.t ->
  room_id:Matrix_proto.Id.Room_id.t ->
  avatar_url:Media.Mxc.t ->
  (Matrix_proto.Id.Event_id.t, Error.t) result
(** [set_avatar t ~room_id ~avatar_url] sets [m.room.avatar] and is the new
    event's identifier. *)
