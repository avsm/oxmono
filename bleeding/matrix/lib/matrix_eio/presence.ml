type presence_state = Matrix_client.Presence.presence_state =
  | Online
  | Offline
  | Unavailable

type presence = Matrix_client.Presence.presence = {
  presence : presence_state;
  status_msg : string option;
  last_active_ago : int option;
  currently_active : bool option;
}

let get_presence client ~user_id =
  Error.unwrap ~context:"getting presence"
    (Matrix_client.Presence.get_presence (Client.base client) ~user_id)

let set_presence client ~presence ?status_msg ?immediate () =
  Error.unwrap ~context:"setting presence"
    (Matrix_client.Presence.set_presence (Client.base client) ~presence
       ?status_msg ?immediate ())
