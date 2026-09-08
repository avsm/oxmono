let get client ~event_type =
  Error.unwrap ~context:"getting account data"
    (Matrix_client.Account_data.get (Client.base client) ~event_type)

let set client ~event_type ~content =
  Error.unwrap ~context:"setting account data"
    (Matrix_client.Account_data.set (Client.base client) ~event_type ~content)

let get_room client ~room_id ~event_type =
  Error.unwrap ~context:"getting room account data"
    (Matrix_client.Account_data.get_room (Client.base client) ~room_id
       ~event_type)

let set_room client ~room_id ~event_type ~content =
  Error.unwrap ~context:"setting room account data"
    (Matrix_client.Account_data.set_room (Client.base client) ~room_id
       ~event_type ~content)

let set_marked_unread client ~room_id ~unread =
  Error.unwrap ~context:"setting room unread marker"
    (Matrix_client.Account_data.set_marked_unread (Client.base client) ~room_id
       ~unread)

let find_dm_rooms client ~user_id =
  Error.unwrap ~context:"finding direct-message rooms"
    (Matrix_client.Account_data.find_dm_rooms (Client.base client) ~user_id)

let get_or_create_dm client ~user_id ?encrypted () =
  Error.unwrap ~context:"getting or creating direct-message room"
    (Matrix_client.Account_data.get_or_create_dm (Client.base client) ~user_id
       ?encrypted ())
