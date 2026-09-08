open Result.Syntax
module Id = Matrix_proto.Id
module Event = Matrix_proto.Event
module Page = Matrix_proto.Common.Page

type room_id_or_alias =
  [ `Room_id of Id.Room_id.t | `Room_alias of Id.Room_alias.t ]

let room_id_or_alias_to_string = function
  | `Room_id id -> Id.Room_id.to_string id
  | `Room_alias alias -> Id.Room_alias.to_string alias

type alias_info = { room_id : Id.Room_id.t; servers : string list }

let alias_info_jsont =
  Jsont.Object.(
    map ~kind:"alias_info" (fun room_id servers -> { room_id; servers })
    |> mem "room_id" Id.Room_id.jsont ~enc:(fun t -> t.room_id)
    |> mem "servers"
         (Jsont.list Matrix_proto.Json.Codec.string)
         ~dec_absent:(fun () -> [])
         ~enc:(fun t -> t.servers)
    |> finish)

let alias_route = Route.v "/directory/room/{alias}"

let alias_path alias =
  Route.expand_exn alias_route [ ("alias", Id.Room_alias.to_string alias) ]

let resolve_alias client ~alias =
  let* body = Client.Http.get client ~path:(alias_path alias) () in
  Client.Http.decode_response alias_info_jsont body

let create_alias_request_jsont =
  Jsont.Object.(
    map ~kind:"create_alias" Fun.id
    |> mem "room_id" Id.Room_id.jsont ~enc:Fun.id
    |> finish)

let create_alias client ~alias ~room_id =
  let* body = Client.Http.encode_body create_alias_request_jsont room_id in
  let+ _ = Client.Http.put client ~path:(alias_path alias) ~body () in
  ()

let delete_alias client ~alias =
  let+ _ = Client.Http.delete client ~path:(alias_path alias) () in
  ()

let visibility_jsont =
  Jsont.Object.(
    map ~kind:"visibility" Fun.id
    |> mem "visibility" Matrix_proto.Common.Visibility.jsont ~enc:Fun.id
    |> finish)

let visibility_route = Route.v "/directory/list/room/{room_id}"

let visibility_path room_id =
  Route.expand_exn visibility_route
    [ ("room_id", Id.Room_id.to_string room_id) ]

let get_visibility client ~room_id =
  let* body = Client.Http.get client ~path:(visibility_path room_id) () in
  Client.Http.decode_response visibility_jsont body

let set_visibility client ~room_id ~visibility =
  let* body = Client.Http.encode_body visibility_jsont visibility in
  let+ _ = Client.Http.put client ~path:(visibility_path room_id) ~body () in
  ()

type space_child = {
  child_id : Id.Room_id.t;
  content : Event.Space_child_content.t;
}

let space_child_jsont =
  Jsont.Object.(
    map ~kind:"m.space.child" (fun child_id content -> { child_id; content })
    |> mem "state_key" Id.Room_id.jsont ~enc:(fun t -> t.child_id)
    |> mem "content" Event.Space_child_content.jsont
         ~dec_absent:(fun () -> Event.Space_child_content.make ())
         ~enc:(fun t -> t.content)
    |> finish)

type room_summary = {
  room_id : Id.Room_id.t;
  name : string option;
  topic : string option;
  avatar_url : Media.Mxc.t option;
  canonical_alias : Id.Room_alias.t option;
  num_joined_members : int;
  room_type : string option;
  room_version : string option;
  join_rule : Event.Join_rule.t option;
  guest_can_join : bool;
  world_readable : bool;
  encryption : string option;
  membership : Event.Membership.t option;
  children_state : space_child list;
}

let room_summary_jsont =
  Jsont.Object.(
    map ~kind:"room_summary"
      (fun
        room_id
        name
        topic
        avatar_url
        canonical_alias
        num_joined_members
        room_type
        room_version
        join_rule
        guest_can_join
        world_readable
        encryption
        membership
        children_state
      ->
        {
          room_id;
          name;
          topic;
          avatar_url;
          canonical_alias;
          num_joined_members;
          room_type;
          room_version;
          join_rule;
          guest_can_join;
          world_readable;
          encryption;
          membership;
          children_state;
        })
    |> mem "room_id" Id.Room_id.jsont ~enc:(fun t -> t.room_id)
    |> mem "name"
         (Jsont.option Matrix_proto.Json.Codec.string)
         ~dec_absent:(fun () -> None)
         ~enc:(fun t -> t.name)
    |> mem "topic"
         (Jsont.option Matrix_proto.Json.Codec.string)
         ~dec_absent:(fun () -> None)
         ~enc:(fun t -> t.topic)
    |> mem "avatar_url" Media.mxc_option_jsont
         ~dec_absent:(fun () -> None)
         ~enc_omit:Option.is_none
         ~enc:(fun t -> t.avatar_url)
    |> mem "canonical_alias"
         (Jsont.option Id.Room_alias.jsont)
         ~dec_absent:(fun () -> None)
         ~enc:(fun t -> t.canonical_alias)
    |> mem "num_joined_members" Matrix_proto.Json.Codec.int
         ~dec_absent:(fun () -> 0)
         ~enc:(fun t -> t.num_joined_members)
    |> mem "room_type"
         (Jsont.option Matrix_proto.Json.Codec.string)
         ~dec_absent:(fun () -> None)
         ~enc:(fun t -> t.room_type)
    |> mem "room_version"
         (Jsont.option Matrix_proto.Json.Codec.string)
         ~dec_absent:(fun () -> None)
         ~enc:(fun t -> t.room_version)
    |> opt_mem "join_rule" Event.Join_rule.jsont ~enc:(fun t -> t.join_rule)
    |> mem "guest_can_join" Jsont.bool
         ~dec_absent:(fun () -> false)
         ~enc:(fun t -> t.guest_can_join)
    |> mem "world_readable" Jsont.bool
         ~dec_absent:(fun () -> false)
         ~enc:(fun t -> t.world_readable)
    |> mem "encryption"
         (Jsont.option Matrix_proto.Json.Codec.string)
         ~dec_absent:(fun () -> None)
         ~enc:(fun t -> t.encryption)
    |> opt_mem "membership" Event.Membership.jsont ~enc:(fun t -> t.membership)
    |> mem "children_state"
         (Jsont.list space_child_jsont)
         ~dec_absent:(fun () -> [])
         ~enc_omit:(fun l -> l = [])
         ~enc:(fun t -> t.children_state)
    |> finish)

let summary_route = Route.v "/_matrix/client/v1/room_summary/{room_id_or_alias}"

let get_summary client ~room_id_or_alias ?(via = []) () =
  (* This endpoint lives under [/_matrix/client/v1], so it goes through the
     absolute-path helper rather than {!Client.Http.get}, which prefixes v3. *)
  let path =
    Route.expand_exn summary_route
      [ ("room_id_or_alias", room_id_or_alias_to_string room_id_or_alias) ]
  in
  let query = List.map (fun s -> ("via", s)) via in
  let* body, _content_type = Client.Http.get_bytes client ~path ~query () in
  Client.Http.decode_response room_summary_jsont body

type search_filter = {
  generic_search_term : string option;
  room_types : string list option;
}

type published_rooms = {
  page : room_summary Page.t;
  total_room_count_estimate : int option;
}

let published_rooms_jsont =
  Jsont.Object.(
    map ~kind:"published_rooms"
      (fun chunk next_batch prev_batch total_room_count_estimate ->
        {
          page = { Page.chunk; next_batch; prev_batch };
          total_room_count_estimate;
        })
    |> mem "chunk"
         (Jsont.list room_summary_jsont)
         ~dec_absent:(fun () -> [])
         ~enc:(fun t -> t.page.Page.chunk)
    |> mem "next_batch"
         (Jsont.option Matrix_proto.Json.Codec.string)
         ~dec_absent:(fun () -> None)
         ~enc:(fun t -> t.page.Page.next_batch)
    |> mem "prev_batch"
         (Jsont.option Matrix_proto.Json.Codec.string)
         ~dec_absent:(fun () -> None)
         ~enc:(fun t -> t.page.Page.prev_batch)
    |> opt_mem "total_room_count_estimate" Matrix_proto.Json.Codec.int
         ~enc:(fun t -> t.total_room_count_estimate)
    |> finish)

let get_public_rooms client ?limit ?from ?server () =
  let query =
    List.filter_map Fun.id
      [
        Option.map (fun l -> ("limit", string_of_int l)) limit;
        Option.map (fun f -> ("since", f)) from;
        Option.map (fun s -> ("server", s)) server;
      ]
  in
  let query = if query = [] then None else Some query in
  let* body = Client.Http.get client ~path:"/publicRooms" ?query () in
  Client.Http.decode_response published_rooms_jsont body

type search_request = {
  filter : search_filter option;
  limit : int option;
  since : string option;
}

let search_filter_jsont =
  Jsont.Object.(
    map ~kind:"search_filter" (fun generic_search_term room_types ->
        { generic_search_term; room_types })
    |> opt_mem "generic_search_term" Matrix_proto.Json.Codec.string
         ~enc:(fun (t : search_filter) -> t.generic_search_term)
    |> opt_mem "room_types" (Jsont.list Matrix_proto.Json.Codec.string)
         ~enc:(fun (t : search_filter) -> t.room_types)
    |> finish)

let search_request_jsont =
  Jsont.Object.(
    map ~kind:"search_request" (fun filter limit since ->
        { filter; limit; since })
    |> opt_mem "filter" search_filter_jsont ~enc:(fun (t : search_request) ->
        t.filter)
    |> opt_mem "limit" Matrix_proto.Json.Codec.int
         ~enc:(fun (t : search_request) -> t.limit)
    |> opt_mem "since" Matrix_proto.Json.Codec.string
         ~enc:(fun (t : search_request) -> t.since)
    |> finish)

let search_public_rooms client ?server ?limit ?from ?filter () =
  let query =
    match server with Some s -> Some [ ("server", s) ] | None -> None
  in
  let* body =
    Client.Http.encode_body search_request_jsont { filter; limit; since = from }
  in
  let* body = Client.Http.post client ~path:"/publicRooms" ?query ~body () in
  Client.Http.decode_response published_rooms_jsont body
