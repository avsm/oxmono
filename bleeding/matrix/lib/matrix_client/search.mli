(** search — full-text search over room events and over the user directory.

    [POST /_matrix/client/v3/search] (Matrix 1.0) searches room events. The
    request and the response are nested under [search_categories] on the wire,
    and [room_events] is the only category the specification defines, so both
    wrappers are hidden here.

    [POST /_matrix/client/v3/user_directory/search] (Matrix 1.0) searches the
    user directory. *)

(** {1 Room event search} *)

type key =
  [ `Content_body  (** ["content.body"]. *)
  | `Content_name  (** ["content.name"]. *)
  | `Content_topic  (** ["content.topic"]. *) ]
(** The type for the event fields a search term is matched against. *)

type order_by =
  [ `Rank  (** ["rank"], by relevance. The server's default. *)
  | `Recent  (** ["recent"], most recent first. *) ]
(** The type for the orders results come back in. *)

type group_by = [ `Room_id  (** ["room_id"]. *) | `Sender  (** ["sender"]. *) ]
(** The type for the keys results are grouped by. *)

type event_context_request = {
  before_limit : int option;  (** Events before the hit. Server default 5. *)
  after_limit : int option;  (** Events after the hit. Server default 5. *)
  include_profile : bool option;
      (** Include the senders' profiles in {!event_context.profile_info}. *)
}
(** The type for how much context is returned around each hit. *)

type criteria = {
  search_term : string;  (** The query. *)
  keys : key list;
      (** The fields to match. The empty list leaves the server its default,
          which is all three. *)
  filter : Sync.Filter.room_event option;
      (** A filter over the events searched, the same shape as the
          [room.timeline] filter of [/sync]. *)
  order_by : order_by option;  (** The order results come back in. *)
  event_context : event_context_request option;
      (** How much context to return around each hit. *)
  include_state : bool option;
      (** Return each matching room's current state in
          {!room_events_result.state}. *)
  group_by : group_by list;
      (** How to group the results. The empty list means no grouping. *)
}
(** The type for a room event search request. *)

val v :
  ?keys:key list ->
  ?filter:Sync.Filter.room_event ->
  ?order_by:order_by ->
  ?event_context:event_context_request ->
  ?include_state:bool ->
  ?group_by:group_by list ->
  string ->
  criteria
(** [v search_term] is a search for [search_term]. [keys] and [group_by] default
    to the empty list and every other argument to absent, which leaves the
    server its own default for each. *)

type user_profile = { displayname : string option; avatar_url : string option }
(** The type for a sender's profile at the time of a hit. *)

type event_context = {
  start : string option;  (** Pagination token before the hit. *)
  end_ : string option;  (** Pagination token after the hit. *)
  profile_info : (Matrix_proto.Id.User_id.t * user_profile) list;
      (** The senders' profiles, sorted by user id. *)
  events_before : Matrix_proto.Event.Raw_event.t list;
      (** The events immediately before the hit. *)
  events_after : Matrix_proto.Event.Raw_event.t list;
      (** The events immediately after the hit. *)
}
(** The type for the events surrounding a hit. *)

type hit = {
  rank : float option;  (** Relevance, where higher is better. *)
  result : Matrix_proto.Event.Raw_event.t option;  (** The matching event. *)
  context : event_context option;
      (** The surrounding events, present when {!criteria.event_context} was
          set. *)
}
(** The type for one search hit. *)

type group = {
  group_next_batch : string option;  (** Token to page within this group. *)
  order : int option;  (** The group's position in the result set. *)
  results : Matrix_proto.Id.Event_id.t list;  (** The group's hits. *)
}
(** The type for one group of hits. *)

type room_events_result = {
  count : int option;  (** Approximate total number of matches. *)
  highlights : string list;  (** Terms a client should highlight. *)
  next_batch : string option;
      (** Token for the next page, to pass to {!room_events}. It is [None] at
          the end of the results. *)
  results : hit list;  (** The hits on this page. *)
  state :
    (Matrix_proto.Id.Room_id.t * Matrix_proto.Event.Raw_event.t list) list;
      (** Current room state, sorted by room id, when {!criteria.include_state}
          was set. *)
  groups : (group_by * (string * group) list) list;
      (** The groups, keyed first by grouping key and then by the value that
          groups them, a room id or a sender. Both levels are sorted by key. *)
}
(** The type for a room event search result. *)

val room_events :
  Client.t ->
  criteria:criteria ->
  ?next_batch:string ->
  unit ->
  (room_events_result, Error.t) result
(** [room_events t ~criteria ()] is [POST /_matrix/client/v3/search] (Matrix
    1.0) with [criteria] as its [search_categories.room_events] body.
    [next_batch] is a {!room_events_result.next_batch} from a previous call and
    defaults to absent, which starts at the first page. It is sent as a query
    parameter, as the specification requires. *)

(** {1 User directory search} *)

type user = {
  user_id : Matrix_proto.Id.User_id.t;  (** The user found. *)
  display_name : string option;  (** The name to show. *)
  avatar_url : string option;  (** An [mxc://] URI for the user's avatar. *)
}
(** The type for one user directory entry. *)

type user_directory_result = {
  users : user list;  (** The entries found. *)
  limited : bool;
      (** [true] when the server truncated the list, so a narrower search term
          may find more. *)
}
(** The type for a user directory search result. *)

val user_directory :
  Client.t ->
  search_term:string ->
  ?limit:int ->
  unit ->
  (user_directory_result, Error.t) result
(** [user_directory t ~search_term ()] is
    [POST /_matrix/client/v3/user_directory/search] (Matrix 1.0). [limit] is the
    maximum number of entries to return and defaults to absent, leaving the
    server's own default of 10. *)
