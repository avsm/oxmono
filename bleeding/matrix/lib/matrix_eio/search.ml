type key = Matrix_client.Search.key
type order_by = Matrix_client.Search.order_by
type group_by = Matrix_client.Search.group_by

type event_context_request = Matrix_client.Search.event_context_request = {
  before_limit : int option;
  after_limit : int option;
  include_profile : bool option;
}

type criteria = Matrix_client.Search.criteria = {
  search_term : string;
  keys : key list;
  filter : Matrix_client.Sync.Filter.room_event option;
  order_by : order_by option;
  event_context : event_context_request option;
  include_state : bool option;
  group_by : group_by list;
}

let v = Matrix_client.Search.v

type user_profile = Matrix_client.Search.user_profile = {
  displayname : string option;
  avatar_url : string option;
}

type event_context = Matrix_client.Search.event_context = {
  start : string option;
  end_ : string option;
  profile_info : (Matrix_proto.Id.User_id.t * user_profile) list;
  events_before : Matrix_proto.Event.Raw_event.t list;
  events_after : Matrix_proto.Event.Raw_event.t list;
}

type hit = Matrix_client.Search.hit = {
  rank : float option;
  result : Matrix_proto.Event.Raw_event.t option;
  context : event_context option;
}

type group = Matrix_client.Search.group = {
  group_next_batch : string option;
  order : int option;
  results : Matrix_proto.Id.Event_id.t list;
}

type room_events_result = Matrix_client.Search.room_events_result = {
  count : int option;
  highlights : string list;
  next_batch : string option;
  results : hit list;
  state :
    (Matrix_proto.Id.Room_id.t * Matrix_proto.Event.Raw_event.t list) list;
  groups : (group_by * (string * group) list) list;
}

let room_events client ~criteria ?next_batch () =
  Error.unwrap ~context:"searching room events"
    (Matrix_client.Search.room_events (Client.base client) ~criteria ?next_batch
       ())

type user = Matrix_client.Search.user = {
  user_id : Matrix_proto.Id.User_id.t;
  display_name : string option;
  avatar_url : string option;
}

type user_directory_result = Matrix_client.Search.user_directory_result = {
  users : user list;
  limited : bool;
}

let user_directory client ~search_term ?limit () =
  Error.unwrap ~context:"searching user directory"
    (Matrix_client.Search.user_directory (Client.base client) ~search_term
       ?limit ())
