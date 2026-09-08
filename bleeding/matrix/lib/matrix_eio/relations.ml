let send_reaction ?extra_content client ~room_id ~event_id ~key =
  Error.unwrap ~context:"sending reaction"
    (Matrix_client.Relations.send_reaction (Client.base client) ~room_id
       ~event_id ~key ?extra_content)

let edit_message client ~room_id ~event_id ~new_body ?formatted_body ?format ()
    =
  Error.unwrap ~context:"editing message"
    (Matrix_client.Relations.edit_message (Client.base client) ~room_id
       ~event_id ~new_body ?formatted_body ?format ())

let send_reply client ~room_id ~event_id ~body ?formatted_body ?format () =
  Error.unwrap ~context:"sending reply"
    (Matrix_client.Relations.send_reply (Client.base client) ~room_id ~event_id
       ~body ?formatted_body ?format ())

let send_in_thread client ~room_id ~thread_root_id ?reply_to_id ~body () =
  Error.unwrap ~context:"sending thread reply"
    (Matrix_client.Relations.send_in_thread (Client.base client) ~room_id
       ~thread_root_id ?reply_to_id ~body ())

type related_event = Matrix_client.Relations.related_event = {
  event_id : Matrix_proto.Id.Event_id.t;
  origin_server_ts : Matrix_proto.Event.Timestamp.t;
  sender : Matrix_proto.Id.User_id.t;
  key : string option;
}

let get_relations client ~room_id ~event_id ?rel_type ?event_type ?limit ?from
    () =
  Error.unwrap ~context:"getting related events"
    (Matrix_client.Relations.get_relations (Client.base client) ~room_id
       ~event_id ?rel_type ?event_type ?limit ?from ())

let get_raw_relations client ~room_id ~event_id ?rel_type ?event_type ?limit
    ?from () =
  Error.unwrap ~context:"getting raw relations"
    (Matrix_client.Relations.get_raw_relations (Client.base client) ~room_id
       ~event_id ?rel_type ?event_type ?limit ?from ())

let get_edit_revisions client ~room_id ~event_id () =
  Error.unwrap ~context:"getting edit revisions"
    (Matrix_client.Relations.get_edit_revisions (Client.base client) ~room_id
       ~event_id ())

let get_reactions client ~room_id ~event_id =
  Error.unwrap ~context:"getting reactions"
    (Matrix_client.Relations.get_reactions (Client.base client) ~room_id
       ~event_id)

type thread_filter = Matrix_client.Relations.thread_filter =
  | All
  | Participated

let list_threads client ~room_id ?filter ?from ?limit () =
  Error.unwrap ~context:"listing threads"
    (Matrix_client.Relations.list_threads (Client.base client) ~room_id ?filter
       ?from ?limit ())
