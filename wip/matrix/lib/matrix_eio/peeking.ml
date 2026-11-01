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

let initial_sync client ~room_id =
  Error.unwrap ~context:"starting room peek"
    (Matrix_client.Peeking.initial_sync (Client.base client) ~room_id)

type events_response = Matrix_client.Peeking.events_response = {
  chunk : Jsont.json list;
  start : string option;
  end_ : string option;
}

let events client ?from ?timeout () =
  Error.unwrap ~context:"getting peek events"
    (Matrix_client.Peeking.events (Client.base client) ?from ?timeout ())

let peek_events client ~room_id ?from ?timeout () =
  Error.unwrap ~context:"peeking room events"
    (Matrix_client.Peeking.peek_events (Client.base client) ~room_id ?from
       ?timeout ())
