(** Legacy client-server peeking endpoints.

    These are the historical [/initialSync] and [/events] endpoints. They are
    deliberately separate from {!Sync}; neither function silently falls back to
    [/sync], and a server that does not expose them returns its normal error. *)

type membership =
  | Invite
  | Join
  | Leave
  | Ban
  | Knock  (** The membership value returned by [/initialSync]. *)

type visibility =
  | Private
  | Public  (** The room visibility returned by [/initialSync]. *)

type account_data_event = {
  type_ : Matrix_proto.Event.Event_type.t;
  content : Jsont.json;
}
(** The minimal event envelope used by initial-sync account data. *)

type message_page = {
  chunk : Matrix_proto.Event.Raw_event.t list;
  start : string option;
  end_ : string;
}
(** A page of timeline events returned by [/initialSync]. *)

type initial_sync_response = {
  room_id : Matrix_proto.Id.Room_id.t;
  membership : membership option;
      (** The membership, when the server includes it. *)
  visibility : visibility option;
      (** The visibility, when the server includes it. *)
  account_data : account_data_event list;
      (** Private account data; absent on the wire means an empty list. *)
  messages : message_page option;
  state : Matrix_proto.Event.Raw_event.t list;
      (** State events; absent on the wire means an empty list. *)
}

val initial_sync_response_jsont : initial_sync_response Jsont.t

val initial_sync :
  Client.t ->
  room_id:Matrix_proto.Id.Room_id.t ->
  (initial_sync_response, Error.t) result
(** [initial_sync client ~room_id] calls [GET /rooms/{room_id}/initialSync]. *)

type events_response = {
  chunk : Jsont.json list;
      (** Raw event-shaped objects; stream responses may also contain presence
          objects without event identifiers. *)
  start : string option;
  end_ : string option;
}
(** A response from the historical [/events] event stream. *)

val events_response_jsont : events_response Jsont.t

val events :
  Client.t ->
  ?from:string ->
  ?timeout:int ->
  unit ->
  (events_response, Error.t) result
(** [events client ()] calls [GET /events]. [timeout] is a non-negative number
    of milliseconds.

    Raises [Invalid_argument] if [timeout] is negative. *)

val peek_events :
  Client.t ->
  room_id:Matrix_proto.Id.Room_id.t ->
  ?from:string ->
  ?timeout:int ->
  unit ->
  (events_response, Error.t) result
(** [peek_events client ~room_id ()] calls [GET /events?room_id=...]. [timeout]
    is a non-negative number of milliseconds.

    Raises [Invalid_argument] if [timeout] is negative. *)
