type room_id_or_alias = Matrix_client.Directory.room_id_or_alias

type alias_info = Matrix_client.Directory.alias_info = {
  room_id : Matrix_proto.Id.Room_id.t;
  servers : string list;
}

let resolve_alias client ~alias =
  Error.unwrap ~context:"resolving room alias"
    (Matrix_client.Directory.resolve_alias (Client.base client) ~alias)

let create_alias client ~alias ~room_id =
  Error.unwrap ~context:"creating room alias"
    (Matrix_client.Directory.create_alias (Client.base client) ~alias ~room_id)

let delete_alias client ~alias =
  Error.unwrap ~context:"deleting room alias"
    (Matrix_client.Directory.delete_alias (Client.base client) ~alias)

let get_visibility client ~room_id =
  Error.unwrap ~context:"getting room visibility"
    (Matrix_client.Directory.get_visibility (Client.base client) ~room_id)

let set_visibility client ~room_id ~visibility =
  Error.unwrap ~context:"setting room visibility"
    (Matrix_client.Directory.set_visibility (Client.base client) ~room_id
       ~visibility)

type space_child = Matrix_client.Directory.space_child = {
  child_id : Matrix_proto.Id.Room_id.t;
  content : Matrix_proto.Event.Space_child_content.t;
}

type room_summary = Matrix_client.Directory.room_summary = {
  room_id : Matrix_proto.Id.Room_id.t;
  name : string option;
  topic : string option;
  avatar_url : Matrix_client.Media.Mxc.t option;
  canonical_alias : Matrix_proto.Id.Room_alias.t option;
  num_joined_members : int;
  room_type : string option;
  room_version : string option;
  join_rule : Matrix_proto.Event.Join_rule.t option;
  guest_can_join : bool;
  world_readable : bool;
  encryption : string option;
  membership : Matrix_proto.Event.Membership.t option;
  children_state : space_child list;
}

let get_summary client ~room_id_or_alias ?via () =
  Error.unwrap ~context:"getting room summary"
    (Matrix_client.Directory.get_summary (Client.base client) ~room_id_or_alias
       ?via ())

type search_filter = Matrix_client.Directory.search_filter = {
  generic_search_term : string option;
  room_types : string list option;
}

type published_rooms = Matrix_client.Directory.published_rooms = {
  page : room_summary Matrix_proto.Common.Page.t;
  total_room_count_estimate : int option;
}

let get_public_rooms client ?limit ?from ?server () =
  Error.unwrap ~context:"listing public rooms"
    (Matrix_client.Directory.get_public_rooms (Client.base client) ?limit ?from
       ?server ())

let search_public_rooms client ?server ?limit ?from ?filter () =
  Error.unwrap ~context:"searching public rooms"
    (Matrix_client.Directory.search_public_rooms (Client.base client) ?server
       ?limit ?from ?filter ())
