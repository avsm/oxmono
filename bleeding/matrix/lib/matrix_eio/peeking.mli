type membership = Matrix_client.Peeking.membership =
  | Invite
  | Join
  | Leave
  | Ban
  | Knock

type visibility = Matrix_client.Peeking.visibility = Private | Public

type account_data_event = Matrix_client.Peeking.account_data_event = {
  type_ : Matrix_proto.Event.Event_type.t;
  content : Jsont.json;
}

type message_page = Matrix_client.Peeking.message_page = {
  chunk : Matrix_proto.Event.Raw_event.t list;
  start : string option;
  end_ : string;
}

type initial_sync_response = Matrix_client.Peeking.initial_sync_response = {
  room_id : Matrix_proto.Id.Room_id.t;
  membership : membership option;
  visibility : visibility option;
  account_data : account_data_event list;
  messages : message_page option;
  state : Matrix_proto.Event.Raw_event.t list;
}

val initial_sync :
  Client.t -> room_id:Matrix_proto.Id.Room_id.t -> initial_sync_response

type events_response = Matrix_client.Peeking.events_response = {
  chunk : Jsont.json list;
  start : string option;
  end_ : string option;
}

val events : Client.t -> ?from:string -> ?timeout:int -> unit -> events_response

val peek_events :
  Client.t ->
  room_id:Matrix_proto.Id.Room_id.t ->
  ?from:string ->
  ?timeout:int ->
  unit ->
  events_response
