(** search — full-text search over room events and over the user directory,
    raising instead of returning.

    Every function that performs a request raises [Eio.Io] carrying [Error.E e]
    where {!Matrix_client.Search} returns [Error e]. That module documents what
    each call does, which endpoint it uses and which errors it produces. *)

(** {1 Room event search} *)

type key = Matrix_client.Search.key
(** Which part of an event a term is matched against. *)

type order_by = Matrix_client.Search.order_by
(** How hits are ordered. *)

type group_by = Matrix_client.Search.group_by
(** What hits are grouped by. *)

type event_context_request = Matrix_client.Search.event_context_request = {
  before_limit : int option;
  after_limit : int option;
  include_profile : bool option;
}
(** How much of the timeline around each hit to ask for. *)

type criteria = Matrix_client.Search.criteria = {
  search_term : string;
  keys : key list;
  filter : Matrix_client.Sync.Filter.room_event option;
  order_by : order_by option;
  event_context : event_context_request option;
  include_state : bool option;
  group_by : group_by list;
}
(** A room event query. *)

val v :
  ?keys:key list ->
  ?filter:Matrix_client.Sync.Filter.room_event ->
  ?order_by:order_by ->
  ?event_context:event_context_request ->
  ?include_state:bool ->
  ?group_by:group_by list ->
  string ->
  criteria
(** [v term] is {!Matrix_client.Search.v}. It performs no request and raises
    nothing. *)

type user_profile = Matrix_client.Search.user_profile = {
  displayname : string option;
  avatar_url : string option;
}
(** The profile of a sender returned alongside a hit. *)

type event_context = Matrix_client.Search.event_context = {
  start : string option;
  end_ : string option;
  profile_info : (Matrix_proto.Id.User_id.t * user_profile) list;
  events_before : Matrix_proto.Event.Raw_event.t list;
  events_after : Matrix_proto.Event.Raw_event.t list;
}
(** The timeline around a hit. *)

type hit = Matrix_client.Search.hit = {
  rank : float option;
  result : Matrix_proto.Event.Raw_event.t option;
  context : event_context option;
}
(** One matching event. *)

type group = Matrix_client.Search.group = {
  group_next_batch : string option;
  order : int option;
  results : Matrix_proto.Id.Event_id.t list;
}
(** The hits sharing one grouping value. *)

type room_events_result = Matrix_client.Search.room_events_result = {
  count : int option;
  highlights : string list;
  next_batch : string option;
  results : hit list;
  state :
    (Matrix_proto.Id.Room_id.t * Matrix_proto.Event.Raw_event.t list) list;
  groups : (group_by * (string * group) list) list;
}
(** One page of room event hits. *)

val room_events :
  Client.t ->
  criteria:criteria ->
  ?next_batch:string ->
  unit ->
  room_events_result
(** [room_events c ~criteria ()] is {!Matrix_client.Search.room_events} with the
    result unwrapped. *)

(** {1 User directory search} *)

type user = Matrix_client.Search.user = {
  user_id : Matrix_proto.Id.User_id.t;
  display_name : string option;
  avatar_url : string option;
}
(** One user the directory matched. *)

type user_directory_result = Matrix_client.Search.user_directory_result = {
  users : user list;
  limited : bool;
}
(** The users the directory matched, and whether the server truncated them. *)

val user_directory :
  Client.t -> search_term:string -> ?limit:int -> unit -> user_directory_result
(** [user_directory c ~search_term ()] is {!Matrix_client.Search.user_directory}
    with the result unwrapped. *)
