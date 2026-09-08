let get_state_event client ~room_id ~event_type ?state_key () =
  Error.unwrap ~context:"getting state event"
    (Matrix_client.State.get_state_event (Client.base client) ~room_id
       ~event_type ?state_key ())

let set_state client ~room_id ~event_type ?state_key ~content () =
  Error.unwrap ~context:"setting state event"
    (Matrix_client.State.set_state (Client.base client) ~room_id ~event_type
       ?state_key ~content ())

let get_state client ~room_id =
  Error.unwrap ~context:"getting room state"
    (Matrix_client.State.get_state (Client.base client) ~room_id)

let get_name client ~room_id =
  Error.unwrap ~context:"getting room name"
    (Matrix_client.State.get_name (Client.base client) ~room_id)

let set_name client ~room_id ~name =
  Error.unwrap ~context:"setting room name"
    (Matrix_client.State.set_name (Client.base client) ~room_id ~name)

let get_topic client ~room_id =
  Error.unwrap ~context:"getting room topic"
    (Matrix_client.State.get_topic (Client.base client) ~room_id)

let set_topic client ~room_id ~topic =
  Error.unwrap ~context:"setting room topic"
    (Matrix_client.State.set_topic (Client.base client) ~room_id ~topic)

let get_avatar client ~room_id =
  Error.unwrap ~context:"getting room avatar"
    (Matrix_client.State.get_avatar (Client.base client) ~room_id)

let set_avatar client ~room_id ~avatar_url =
  Error.unwrap ~context:"setting room avatar"
    (Matrix_client.State.set_avatar (Client.base client) ~room_id ~avatar_url)
