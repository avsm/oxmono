module R = Matrix_client.Room_keys

type version_info = R.version_info = {
  version : string;
  algorithm : string;
  auth_data : Jsont.json;
  count : int;
  etag : string;
}

type key_backup_data = R.key_backup_data = {
  first_message_index : int;
  forwarded_count : int;
  is_verified : bool;
  session_data : Matrix_client.Backup.encrypted_session_data;
}

type sessions = R.sessions
type rooms = R.rooms

let equal_version_info = R.equal_version_info

type update_response = R.update_response = { etag : string; count : int }

let create_version client ~algorithm ~auth_data =
  Error.unwrap ~context:"creating room-key backup version"
    (R.create_version (Client.base client) ~algorithm ~auth_data)

let get_current_version client =
  Error.unwrap ~context:"getting current room-key backup version"
    (R.get_current_version (Client.base client))

let get_version client ~version =
  Error.unwrap ~context:"getting room-key backup version"
    (R.get_version (Client.base client) ~version)

let update_version client ~version ~algorithm ~auth_data =
  Error.unwrap ~context:"updating room-key backup version"
    (R.update_version (Client.base client) ~version ~algorithm ~auth_data)

let delete_version client ~version =
  Error.unwrap ~context:"deleting room-key backup version"
    (R.delete_version (Client.base client) ~version)

let put_keys client ~version rooms =
  Error.unwrap ~context:"putting room-key backup keys"
    (R.put_keys (Client.base client) ~version rooms)

let get_keys client ~version =
  Error.unwrap ~context:"getting room-key backup keys"
    (R.get_keys (Client.base client) ~version)

let delete_keys client ~version =
  Error.unwrap ~context:"deleting room-key backup keys"
    (R.delete_keys (Client.base client) ~version)

let put_room_keys client ~version ~room_id sessions =
  Error.unwrap ~context:"putting room keys into backup"
    (R.put_room_keys (Client.base client) ~version ~room_id sessions)

let get_room_keys client ~version ~room_id =
  Error.unwrap ~context:"getting room keys from backup"
    (R.get_room_keys (Client.base client) ~version ~room_id)

let delete_room_keys client ~version ~room_id =
  Error.unwrap ~context:"deleting room keys from backup"
    (R.delete_room_keys (Client.base client) ~version ~room_id)

let put_session_key client ~version ~room_id ~session_id data =
  Error.unwrap ~context:"putting room-key backup session"
    (R.put_session_key (Client.base client) ~version ~room_id ~session_id data)

let get_session_key client ~version ~room_id ~session_id =
  Error.unwrap ~context:"getting room-key backup session"
    (R.get_session_key (Client.base client) ~version ~room_id ~session_id)

let delete_session_key client ~version ~room_id ~session_id =
  Error.unwrap ~context:"deleting room-key backup session"
    (R.delete_session_key (Client.base client) ~version ~room_id ~session_id)
