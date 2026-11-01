open Result.Syntax

let src = Logs.Src.create "matrix.search" ~doc:"Matrix search"

module Log = (val Logs.src_log src : Logs.LOG)

let user_map v =
  Json_codec.keyed_map ~what:"user id"
    ~of_string:Matrix_proto.Id.User_id.of_string
    ~to_string:Matrix_proto.Id.User_id.to_string v

let room_map v =
  Json_codec.keyed_map ~what:"room id"
    ~of_string:Matrix_proto.Id.Room_id.of_string
    ~to_string:Matrix_proto.Id.Room_id.to_string v

type key = [ `Content_body | `Content_name | `Content_topic ]

let key_to_string = function
  | `Content_body -> "content.body"
  | `Content_name -> "content.name"
  | `Content_topic -> "content.topic"

let key_of_string = function
  | "content.body" -> Ok `Content_body
  | "content.name" -> Ok `Content_name
  | "content.topic" -> Ok `Content_topic
  | s -> Error (Printf.sprintf "unknown search key %S" s)

let key_jsont =
  Jsont.of_of_string ~kind:"search_key" ~enc:key_to_string key_of_string

type order_by = [ `Rank | `Recent ]

let order_by_to_string = function `Rank -> "rank" | `Recent -> "recent"

let order_by_jsont =
  Jsont.of_of_string ~kind:"order_by" ~enc:order_by_to_string (function
    | "rank" -> Ok `Rank
    | "recent" -> Ok `Recent
    | s -> Error (Printf.sprintf "unknown order_by %S" s))

type group_by = [ `Room_id | `Sender ]

let group_by_to_string = function `Room_id -> "room_id" | `Sender -> "sender"

let group_by_of_string = function
  | "room_id" -> Ok `Room_id
  | "sender" -> Ok `Sender
  | s -> Error (Printf.sprintf "unknown group_by key %S" s)

let group_by_jsont =
  Jsont.of_of_string ~kind:"group_by_key" ~enc:group_by_to_string
    group_by_of_string

let group_by_map v =
  Json_codec.keyed_map ~what:"group_by key"
    ~of_string:(fun s ->
      Result.map_error (fun m -> `Msg m) (group_by_of_string s))
    ~to_string:group_by_to_string v

type event_context_request = {
  before_limit : int option;
  after_limit : int option;
  include_profile : bool option;
}

let event_context_request_jsont =
  Jsont.Object.(
    map ~kind:"event_context_request"
      (fun before_limit after_limit include_profile ->
        { before_limit; after_limit; include_profile })
    |> opt_mem "before_limit" Matrix_proto.Json.Codec.int ~enc:(fun t ->
        t.before_limit)
    |> opt_mem "after_limit" Matrix_proto.Json.Codec.int ~enc:(fun t ->
        t.after_limit)
    |> opt_mem "include_profile" Jsont.bool ~enc:(fun t -> t.include_profile)
    |> finish)

type criteria = {
  search_term : string;
  keys : key list;
  filter : Sync.Filter.room_event option;
  order_by : order_by option;
  event_context : event_context_request option;
  include_state : bool option;
  group_by : group_by list;
}

let v ?(keys = []) ?filter ?order_by ?event_context ?include_state
    ?(group_by = []) search_term =
  {
    search_term;
    keys;
    filter;
    order_by;
    event_context;
    include_state;
    group_by;
  }

(* [groupings] is an object with one member, an array of single-member
   objects; there is no richer shape in the spec. *)
let grouping_jsont =
  Jsont.Object.(
    map ~kind:"grouping" Fun.id
    |> mem "key" group_by_jsont ~enc:Fun.id
    |> finish)

type groupings = { group_by : group_by list }

let groupings_jsont =
  Jsont.Object.(
    map ~kind:"groupings" (fun group_by -> { group_by })
    |> mem "group_by"
         (Jsont.list grouping_jsont)
         ~dec_absent:(fun () -> [])
         ~enc:(fun t -> t.group_by)
    |> finish)

(* The wire shape of [criteria]: [keys] and [groupings] are omitted rather
   than sent empty, so the server keeps its own defaults. *)
let criteria_jsont =
  Jsont.Object.(
    map ~kind:"room_events_criteria"
      (fun
        search_term
        keys
        filter
        order_by
        event_context
        include_state
        groupings
      ->
        {
          search_term;
          keys = Option.value keys ~default:[];
          filter;
          order_by;
          event_context;
          include_state;
          group_by = (match groupings with Some g -> g.group_by | None -> []);
        })
    |> mem "search_term" Matrix_proto.Json.Codec.string ~enc:(fun t ->
        t.search_term)
    |> opt_mem "keys" (Jsont.list key_jsont) ~enc:(fun t ->
        if t.keys = [] then None else Some t.keys)
    |> opt_mem "filter" Sync.Filter.room_event_jsont ~enc:(fun t -> t.filter)
    |> opt_mem "order_by" order_by_jsont ~enc:(fun t -> t.order_by)
    |> opt_mem "event_context" event_context_request_jsont ~enc:(fun t ->
        t.event_context)
    |> opt_mem "include_state" Jsont.bool ~enc:(fun t -> t.include_state)
    |> opt_mem "groupings" groupings_jsont ~enc:(fun (t : criteria) ->
        if t.group_by = [] then None else Some { group_by = t.group_by })
    |> finish)

type categories_request = { room_events : criteria }

let categories_request_jsont =
  Jsont.Object.(
    map ~kind:"search_categories_request" (fun room_events -> { room_events })
    |> mem "room_events" criteria_jsont ~enc:(fun t -> t.room_events)
    |> finish)

let search_request_jsont =
  Jsont.Object.(
    map ~kind:"search_request" Fun.id
    |> mem "search_categories" categories_request_jsont ~enc:Fun.id
    |> finish)

type user_profile = { displayname : string option; avatar_url : string option }

let user_profile_jsont =
  Jsont.Object.(
    map ~kind:"user_profile" (fun displayname avatar_url ->
        { displayname; avatar_url })
    |> mem "displayname"
         (Jsont.option Matrix_proto.Json.Codec.string)
         ~dec_absent:(fun () -> None)
         ~enc:(fun t -> t.displayname)
    |> mem "avatar_url"
         (Jsont.option Matrix_proto.Json.Codec.string)
         ~dec_absent:(fun () -> None)
         ~enc:(fun t -> t.avatar_url)
    |> finish)

type event_context = {
  start : string option;
  end_ : string option;
  profile_info : (Matrix_proto.Id.User_id.t * user_profile) list;
  events_before : Matrix_proto.Event.Raw_event.t list;
  events_after : Matrix_proto.Event.Raw_event.t list;
}

let raw_events_jsont = Jsont.list Matrix_proto.Event.Raw_event.jsont

let event_context_jsont =
  Jsont.Object.(
    map ~kind:"event_context"
      (fun start end_ profile_info events_before events_after ->
        { start; end_; profile_info; events_before; events_after })
    |> mem "start"
         (Jsont.option Matrix_proto.Json.Codec.string)
         ~dec_absent:(fun () -> None)
         ~enc:(fun t -> t.start)
    |> mem "end"
         (Jsont.option Matrix_proto.Json.Codec.string)
         ~dec_absent:(fun () -> None)
         ~enc:(fun t -> t.end_)
    |> mem "profile_info"
         (user_map user_profile_jsont)
         ~dec_absent:(fun () -> [])
         ~enc:(fun t -> t.profile_info)
    |> mem "events_before" raw_events_jsont
         ~dec_absent:(fun () -> [])
         ~enc:(fun t -> t.events_before)
    |> mem "events_after" raw_events_jsont
         ~dec_absent:(fun () -> [])
         ~enc:(fun t -> t.events_after)
    |> finish)

type hit = {
  rank : float option;
  result : Matrix_proto.Event.Raw_event.t option;
  context : event_context option;
}

let hit_jsont =
  Jsont.Object.(
    map ~kind:"search_result" (fun rank result context ->
        { rank; result; context })
    |> opt_mem "rank" Matrix_proto.Json.Codec.number ~enc:(fun t -> t.rank)
    |> opt_mem "result" Matrix_proto.Event.Raw_event.jsont ~enc:(fun t ->
        t.result)
    |> opt_mem "context" event_context_jsont ~enc:(fun t -> t.context)
    |> finish)

type group = {
  group_next_batch : string option;
  order : int option;
  results : Matrix_proto.Id.Event_id.t list;
}

let group_jsont =
  Jsont.Object.(
    map ~kind:"search_group" (fun group_next_batch order results ->
        { group_next_batch; order; results })
    |> mem "next_batch"
         (Jsont.option Matrix_proto.Json.Codec.string)
         ~dec_absent:(fun () -> None)
         ~enc:(fun t -> t.group_next_batch)
    |> opt_mem "order" Matrix_proto.Json.Codec.int ~enc:(fun t -> t.order)
    |> mem "results"
         (Jsont.list Matrix_proto.Id.Event_id.jsont)
         ~dec_absent:(fun () -> [])
         ~enc:(fun t -> t.results)
    |> finish)

type room_events_result = {
  count : int option;
  highlights : string list;
  next_batch : string option;
  results : hit list;
  state :
    (Matrix_proto.Id.Room_id.t * Matrix_proto.Event.Raw_event.t list) list;
  groups : (group_by * (string * group) list) list;
}

let room_events_result_jsont =
  Jsont.Object.(
    map ~kind:"room_events_result"
      (fun count highlights next_batch results state groups ->
        { count; highlights; next_batch; results; state; groups })
    |> opt_mem "count" Matrix_proto.Json.Codec.int ~enc:(fun t -> t.count)
    |> mem "highlights"
         (Jsont.list Matrix_proto.Json.Codec.string)
         ~dec_absent:(fun () -> [])
         ~enc:(fun t -> t.highlights)
    |> mem "next_batch"
         (Jsont.option Matrix_proto.Json.Codec.string)
         ~dec_absent:(fun () -> None)
         ~enc:(fun t -> t.next_batch)
    |> mem "results" (Jsont.list hit_jsont)
         ~dec_absent:(fun () -> [])
         ~enc:(fun t -> t.results)
    |> mem "state"
         (room_map raw_events_jsont)
         ~dec_absent:(fun () -> [])
         ~enc:(fun t -> t.state)
    |> mem "groups"
         (group_by_map (Json_codec.string_map group_jsont))
         ~dec_absent:(fun () -> [])
         ~enc:(fun t -> t.groups)
    |> finish)

type categories_result = { room_events_result : room_events_result }

(* An empty [room_events] category is a legal reply to a request that asked
   for one, so it decodes to an empty result rather than an error. *)
let empty_room_events_result () =
  {
    count = None;
    highlights = [];
    next_batch = None;
    results = [];
    state = [];
    groups = [];
  }

let categories_result_jsont =
  Jsont.Object.(
    map ~kind:"search_categories_result" (fun room_events_result ->
        { room_events_result })
    |> mem "room_events" room_events_result_jsont
         ~dec_absent:empty_room_events_result ~enc:(fun t ->
           t.room_events_result)
    |> finish)

let search_response_jsont =
  Jsont.Object.(
    map ~kind:"search_response" (fun c -> c.room_events_result)
    |> mem "search_categories" categories_result_jsont ~enc:(fun r ->
        { room_events_result = r })
    |> finish)

let room_events client ~criteria ?next_batch () =
  let query =
    match next_batch with Some b -> Some [ ("next_batch", b) ] | None -> None
  in
  let* body =
    Client.Http.encode_body search_request_jsont { room_events = criteria }
  in
  let* body = Client.Http.post client ~path:"/search" ?query ~body () in
  let+ result = Client.Http.decode_response search_response_jsont body in
  Log.debug (fun m -> m "Search returned %d hits" (List.length result.results));
  result

type user = {
  user_id : Matrix_proto.Id.User_id.t;
  display_name : string option;
  avatar_url : string option;
}

let user_jsont =
  Jsont.Object.(
    map ~kind:"user_directory_user" (fun user_id display_name avatar_url ->
        { user_id; display_name; avatar_url })
    |> mem "user_id" Matrix_proto.Id.User_id.jsont ~enc:(fun t -> t.user_id)
    |> mem "display_name"
         (Jsont.option Matrix_proto.Json.Codec.string)
         ~dec_absent:(fun () -> None)
         ~enc:(fun t -> t.display_name)
    |> mem "avatar_url"
         (Jsont.option Matrix_proto.Json.Codec.string)
         ~dec_absent:(fun () -> None)
         ~enc:(fun t -> t.avatar_url)
    |> finish)

type user_directory_result = { users : user list; limited : bool }

let user_directory_result_jsont =
  Jsont.Object.(
    map ~kind:"user_directory_result" (fun users limited -> { users; limited })
    |> mem "results" (Jsont.list user_jsont)
         ~dec_absent:(fun () -> [])
         ~enc:(fun t -> t.users)
    |> mem "limited" Jsont.bool
         ~dec_absent:(fun () -> false)
         ~enc:(fun t -> t.limited)
    |> finish)

type user_directory_request = { search_term : string; limit : int option }

let user_directory_request_jsont =
  Jsont.Object.(
    map ~kind:"user_directory_request" (fun search_term limit ->
        { search_term; limit })
    |> mem "search_term" Matrix_proto.Json.Codec.string ~enc:(fun t ->
        t.search_term)
    |> opt_mem "limit" Matrix_proto.Json.Codec.int ~enc:(fun t -> t.limit)
    |> finish)

let user_directory client ~search_term ?limit () =
  let* body =
    Client.Http.encode_body user_directory_request_jsont { search_term; limit }
  in
  let* body = Client.Http.post client ~path:"/user_directory/search" ~body () in
  Client.Http.decode_response user_directory_result_jsont body
