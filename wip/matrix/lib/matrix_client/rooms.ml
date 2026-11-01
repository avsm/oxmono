open Result.Syntax
module Id = Matrix_proto.Id
module Event = Matrix_proto.Event

let src = Logs.Src.create "matrix.rooms" ~doc:"Matrix room operations"

module Log = (val Logs.src_log src : Logs.LOG)

type preset = Private_chat | Public_chat | Trusted_private_chat

let preset_to_string = function
  | Private_chat -> "private_chat"
  | Public_chat -> "public_chat"
  | Trusted_private_chat -> "trusted_private_chat"

type state_event = {
  ev_type : string;
  state_key : string;
  content : Jsont.json;
}

let state_event_jsont =
  Jsont.Object.(
    map (fun ev_type state_key content -> { ev_type; state_key; content })
    |> mem "type" Matrix_proto.Json.Codec.string ~enc:(fun t -> t.ev_type)
    |> mem "state_key" Matrix_proto.Json.Codec.string ~enc:(fun t ->
        t.state_key)
    |> mem "content" Matrix_proto.Json.Codec.json ~enc:(fun t -> t.content)
    |> finish)

type create_request = {
  name : string option;
  topic : string option;
  visibility : Matrix_proto.Common.Visibility.t option;
  preset : string option;
  room_alias_local_part : string option;
  invite : Id.User_id.t list;
  is_direct : bool option;
  room_type : string option;
  initial_state : state_event list;
}

let create_request_jsont : create_request Jsont.t =
  let open Jsont.Object in
  map
    (fun
      name
      topic
      visibility
      preset
      room_alias_local_part
      invite
      is_direct
      room_type
      initial_state
    ->
      ({
         name;
         topic;
         visibility;
         preset;
         room_alias_local_part;
         invite;
         is_direct;
         room_type;
         initial_state;
       }
        : create_request))
  |> opt_mem "name" Matrix_proto.Json.Codec.string
       ~enc:(fun (t : create_request) -> t.name)
  |> opt_mem "topic" Matrix_proto.Json.Codec.string
       ~enc:(fun (t : create_request) -> t.topic)
  |> opt_mem "visibility" Matrix_proto.Common.Visibility.jsont
       ~enc:(fun (t : create_request) -> t.visibility)
  |> opt_mem "preset" Matrix_proto.Json.Codec.string
       ~enc:(fun (t : create_request) -> t.preset)
  |> opt_mem "room_alias_local_part" Matrix_proto.Json.Codec.string
       ~enc:(fun (t : create_request) -> t.room_alias_local_part)
  |> mem "invite"
       (Jsont.list Id.User_id.jsont)
       ~dec_absent:(fun () -> [])
       ~enc:(fun (t : create_request) -> t.invite)
  |> opt_mem "is_direct" Jsont.bool ~enc:(fun (t : create_request) ->
      t.is_direct)
  |> opt_mem "room_type" Matrix_proto.Json.Codec.string
       ~enc:(fun (t : create_request) -> t.room_type)
  |> mem "initial_state"
       (Jsont.list state_event_jsont)
       ~dec_absent:(fun () -> [])
       ~enc:(fun (t : create_request) -> t.initial_state)
  |> finish

type room_id_response = { room_id : Id.Room_id.t }

let room_id_response_jsont =
  Jsont.Object.(
    map (fun room_id -> { room_id })
    |> mem "room_id" Id.Room_id.jsont ~enc:(fun t -> t.room_id)
    |> finish)

let default_encryption_algorithm = "m.megolm.v1.aes-sha2"

(* Encryption is turned on at creation rather than afterwards: an
   [m.room.encryption] event in [initial_state] means no event in the room
   predates it. *)
let encryption_state_event algorithm =
  {
    ev_type = "m.room.encryption";
    state_key = "";
    content = Json_codec.obj [ ("algorithm", Jsont.Json.string algorithm) ];
  }

let create client ?name ?topic ?visibility ?preset ?room_alias_local_part
    ?(invite = []) ?is_direct ?room_type ?encrypted () =
  let initial_state =
    match encrypted with
    | Some true -> [ encryption_state_event default_encryption_algorithm ]
    | _ -> []
  in
  let request =
    {
      name;
      topic;
      visibility;
      preset = Option.map preset_to_string preset;
      room_alias_local_part;
      invite;
      is_direct;
      room_type;
      initial_state;
    }
  in
  let* body = Client.Http.encode_body create_request_jsont request in
  let* body = Client.Http.post client ~path:"/createRoom" ~body () in
  let+ resp = Client.Http.decode_response room_id_response_jsont body in
  Log.info (fun m -> m "Created room %a" Id.Room_id.pp resp.room_id);
  resp.room_id

type reason_request = { reason : string option }

let reason_request_jsont =
  Jsont.Object.(
    map (fun reason -> { reason })
    |> opt_mem "reason" Matrix_proto.Json.Codec.string ~enc:(fun t -> t.reason)
    |> finish)

let server_name_query = function
  | [] -> None
  | via -> Some (List.map (fun s -> ("server_name", s)) via)

let enter_route = Route.v "/{action}/{room_id_or_alias}"

let enter client ~what ~room_id_or_alias ~reason ~via =
  let path =
    Route.expand_exn enter_route
      [
        ("action", what);
        ( "room_id_or_alias",
          Directory.room_id_or_alias_to_string room_id_or_alias );
      ]
  in
  let query = server_name_query via in
  let* body = Client.Http.encode_body reason_request_jsont { reason } in
  let* body = Client.Http.post client ~path ?query ~body () in
  let+ resp = Client.Http.decode_response room_id_response_jsont body in
  resp.room_id

let join client ~room_id_or_alias ?(via = []) ?reason () =
  enter client ~what:"join" ~room_id_or_alias ~reason ~via

let knock client ~room_id_or_alias ?reason ?(via = []) () =
  enter client ~what:"knock" ~room_id_or_alias ~reason ~via

let room_action_route = Route.v "/rooms/{room_id}/{action}"

let room_path room_id action =
  Route.expand_exn room_action_route
    [ ("room_id", Id.Room_id.to_string room_id); ("action", action) ]

let leave client ~room_id ?reason () =
  let* body = Client.Http.encode_body reason_request_jsont { reason } in
  let+ _ = Client.Http.post client ~path:(room_path room_id "leave") ~body () in
  ()

let forget client ~room_id =
  let+ _ =
    Client.Http.post client ~path:(room_path room_id "forget") ~body:"{}" ()
  in
  ()

type user_request = { user_id : Id.User_id.t; reason : string option }

let user_request_jsont =
  Jsont.Object.(
    map (fun user_id reason -> { user_id; reason })
    |> mem "user_id" Id.User_id.jsont ~enc:(fun t -> t.user_id)
    |> opt_mem "reason" Matrix_proto.Json.Codec.string ~enc:(fun t -> t.reason)
    |> finish)

let membership_change client ~room_id ~what ~user_id ~reason =
  let* body = Client.Http.encode_body user_request_jsont { user_id; reason } in
  let+ _ = Client.Http.post client ~path:(room_path room_id what) ~body () in
  ()

let invite client ~room_id ~user_id ?reason () =
  membership_change client ~room_id ~what:"invite" ~user_id ~reason

let kick client ~room_id ~user_id ?reason () =
  membership_change client ~room_id ~what:"kick" ~user_id ~reason

let ban client ~room_id ~user_id ?reason () =
  membership_change client ~room_id ~what:"ban" ~user_id ~reason

let unban client ~room_id ~user_id ?reason () =
  membership_change client ~room_id ~what:"unban" ~user_id ~reason

let joined_rooms_response_jsont =
  Jsont.Object.(
    map ~kind:"joined_rooms" Fun.id
    |> mem "joined_rooms" (Jsont.list Id.Room_id.jsont) ~enc:Fun.id
    |> finish)

let get_joined_rooms client =
  let* body = Client.Http.get client ~path:"/joined_rooms" () in
  Client.Http.decode_response joined_rooms_response_jsont body

type member = {
  user_id : Id.User_id.t;
  display_name : string option;
  avatar_url : Media.Mxc.t option;
  membership : Event.Membership.t;
}

type member_content = {
  c_membership : Event.Membership.t;
  c_displayname : string option;
  c_avatar_url : Media.Mxc.t option;
}

let member_content_jsont =
  Jsont.Object.(
    map ~kind:"m.room.member" (fun c_membership c_displayname c_avatar_url ->
        { c_membership; c_displayname; c_avatar_url })
    |> mem "membership" Event.Membership.jsont ~enc:(fun t -> t.c_membership)
    |> mem "displayname"
         (Jsont.option Matrix_proto.Json.Codec.string)
         ~dec_absent:(fun () -> None)
         ~enc_omit:Option.is_none
         ~enc:(fun t -> t.c_displayname)
    |> mem "avatar_url" Media.mxc_option_jsont
         ~dec_absent:(fun () -> None)
         ~enc_omit:Option.is_none
         ~enc:(fun t -> t.c_avatar_url)
    |> finish)

let member_jsont =
  Jsont.Object.(
    map ~kind:"member" (fun user_id c ->
        {
          user_id;
          display_name = c.c_displayname;
          avatar_url = c.c_avatar_url;
          membership = c.c_membership;
        })
    |> mem "state_key" Id.User_id.jsont ~enc:(fun t -> t.user_id)
    |> mem "content" member_content_jsont ~enc:(fun t ->
        {
          c_membership = t.membership;
          c_displayname = t.display_name;
          c_avatar_url = t.avatar_url;
        })
    |> finish)

let members_response_jsont =
  Jsont.Object.(
    map ~kind:"members" Fun.id
    |> mem "chunk" (Jsont.list member_jsont) ~enc:Fun.id
    |> finish)

let get_members client ~room_id ?membership ?not_membership () =
  let named name = function
    | None -> []
    | Some m -> [ (name, Event.Membership.to_string m) ]
  in
  let query =
    named "membership" membership @ named "not_membership" not_membership
  in
  let query = if query = [] then None else Some query in
  let* body =
    Client.Http.get client ~path:(room_path room_id "members") ?query ()
  in
  Client.Http.decode_response members_response_jsont body

type joined_member = {
  display_name : string option;
  avatar_url : Media.Mxc.t option;
}

(* Synapse sends [{"avatar_url": null, "display_name": "..."}] rather than
   omitting a member it has no value for, and [opt_mem] rejects an explicit
   null. Absent and null both decode to [None] here; encoding still omits. *)
let joined_member_jsont =
  Jsont.Object.(
    map ~kind:"joined_member" (fun display_name avatar_url ->
        { display_name; avatar_url })
    |> mem "display_name"
         (Jsont.option Matrix_proto.Json.Codec.string)
         ~dec_absent:(fun () -> None)
         ~enc_omit:Option.is_none
         ~enc:(fun (t : joined_member) -> t.display_name)
    |> mem "avatar_url" Media.mxc_option_jsont
         ~dec_absent:(fun () -> None)
         ~enc_omit:Option.is_none
         ~enc:(fun (t : joined_member) -> t.avatar_url)
    |> finish)

let joined_members_response_jsont =
  Jsont.Object.(
    map ~kind:"joined_members" Fun.id
    |> mem "joined"
         (Json_codec.keyed_map ~what:"user id" ~of_string:Id.User_id.of_string
            ~to_string:Id.User_id.to_string joined_member_jsont)
         ~enc:Fun.id
    |> finish)

let get_joined_members client ~room_id =
  let* body =
    Client.Http.get client ~path:(room_path room_id "joined_members") ()
  in
  Client.Http.decode_response joined_members_response_jsont body

let power_levels_route = Route.v "/rooms/{room_id}/state/m.room.power_levels/"

let power_levels_path room_id =
  Route.expand_exn power_levels_route
    [ ("room_id", Id.Room_id.to_string room_id) ]

let get_power_levels client ~room_id =
  let* body = Client.Http.get client ~path:(power_levels_path room_id) () in
  Client.Http.decode_response Event.Room_power_levels_content.jsont body

let set_power_levels client ~room_id ~power_levels =
  let* body =
    Client.Http.encode_body Event.Room_power_levels_content.jsont power_levels
  in
  let+ _ = Client.Http.put client ~path:(power_levels_path room_id) ~body () in
  ()

let set_user_power_level client ~room_id ~user_id ~level =
  let* pl = get_power_levels client ~room_id in
  set_power_levels client ~room_id
    ~power_levels:
      (Event.Room_power_levels_content.with_user_level pl user_id level)

(* Live-location state is keyed by the sender's user id.  Unlike the state
   content endpoint, [/state] returns the event id as well; that id is needed
   to make a beacon's relation unambiguous. *)
let own_user client =
  match Client.session client with
  | Some session -> Ok session.user_id
  | None -> Error Error.No_session

let beacon_info_event client ~room_id =
  let* user_id = own_user client in
  let* events = State.get_state client ~room_id in
  let key = Id.User_id.to_string user_id in
  match
    List.find_opt
      (fun (event : Event.Raw_event.t) ->
        Event.Event_type.equal event.type_ Event.Event_type.Beacon_info
        && Option.equal String.equal event.state_key (Some key))
      events
  with
  | None ->
      Error (Error.Json_error "live location beacon information is missing")
  | Some event -> (
      match event.event_id with
      | None ->
          Error
            (Error.Json_error "live location beacon information has no event id")
      | Some event_id -> (
          match event.unsigned with
          | Some unsigned
            when Option.is_some (Event.Unsigned.redacted_because unsigned) ->
              Error
                (Error.Json_error "live location beacon information is redacted")
          | _ -> (
              match
                Jsont.Json.decode Event.Beacon_info_content.jsont event.content
              with
              | Error msg -> Error (Error.Json_error msg)
              | Ok content ->
                  Ok (user_id, event_id, content, event.origin_server_ts))))

let encode_beacon_info content =
  match Jsont.Json.encode Event.Beacon_info_content.jsont content with
  | Ok json -> Ok json
  | Error msg -> Error (Error.Json_error msg)

let start_live_location_share client ~room_id ~duration_millis ?description
    ?timestamp () =
  let* user_id = own_user client in
  if Int64.compare duration_millis 0L <= 0 then
    Error (Error.Json_error "live location duration must be positive")
  else
    let timestamp =
      Option.value timestamp
        ~default:(Event.Timestamp.of_ptime (Ptime_clock.now ()))
    in
    let content =
      {
        Event.Beacon_info_content.description;
        live = true;
        timeout = duration_millis;
        timestamp = Some timestamp;
        asset_type = Some "m.self";
      }
    in
    let* json = encode_beacon_info content in
    State.set_state client ~room_id ~event_type:Event.Event_type.Beacon_info
      ~state_key:(Id.User_id.to_string user_id)
      ~content:json ()

let stop_live_location_share client ~room_id () =
  let* _user_id, _event_id, content, _origin =
    beacon_info_event client ~room_id
  in
  if not content.live then
    Error (Error.Json_error "live location share is not live")
  else
    let stopped = { content with live = false } in
    let* json = encode_beacon_info stopped in
    let* user_id = own_user client in
    State.set_state client ~room_id ~event_type:Event.Event_type.Beacon_info
      ~state_key:(Id.User_id.to_string user_id)
      ~content:json ()

let send_location_beacon client ~room_id ~geo_uri ?timestamp ?description ?now
    () =
  let* _user_id, beacon_info_id, content, origin =
    beacon_info_event client ~room_id
  in
  if not content.live then
    Error (Error.Json_error "live location share is not live")
  else
    let started = Option.value content.timestamp ~default:origin in
    let now =
      Option.value now
        ~default:(fun () -> Event.Timestamp.of_ptime (Ptime_clock.now ()))
        ()
    in
    let elapsed =
      Int64.sub (Event.Timestamp.to_ms now) (Event.Timestamp.to_ms started)
    in
    if
      Int64.compare content.timeout 0L <= 0
      || Int64.compare elapsed content.timeout >= 0
    then Error (Error.Json_error "live location share has expired")
    else
      let timestamp = Option.value timestamp ~default:now in
      let content =
        {
          Event.Beacon_content.location = { uri = geo_uri; description };
          timestamp;
          relates_to = Event.Relates_to.reference beacon_info_id;
        }
      in
      let* json =
        match Jsont.Json.encode Event.Beacon_content.jsont content with
        | Ok json -> Ok json
        | Error msg -> Error (Error.Json_error msg)
      in
      Messages.send_event client ~room_id ~event_type:Event.Event_type.Beacon
        ~content:json

type upgrade_request = {
  new_version : string;
  additional_creators : Id.User_id.t list;
}

let upgrade_request_jsont =
  Jsont.Object.(
    map ~kind:"upgrade_request" (fun new_version additional_creators ->
        { new_version; additional_creators })
    |> mem "new_version" Matrix_proto.Json.Codec.string
         ~enc:(fun (t : upgrade_request) -> t.new_version)
    |> mem "additional_creators"
         (Jsont.list Id.User_id.jsont)
         ~dec_absent:(fun () -> [])
         ~enc:(fun (t : upgrade_request) -> t.additional_creators)
    |> finish)

let upgrade_response_jsont =
  Jsont.Object.(
    map ~kind:"upgrade_response" Fun.id
    |> mem "replacement_room" Id.Room_id.jsont ~enc:Fun.id
    |> finish)

let upgrade client ~room_id ~new_version ?(additional_creators = []) () =
  let* body =
    Client.Http.encode_body upgrade_request_jsont
      { new_version; additional_creators }
  in
  let* body =
    Client.Http.post client ~path:(room_path room_id "upgrade") ~body ()
  in
  let+ replacement = Client.Http.decode_response upgrade_response_jsont body in
  Log.info (fun m -> m "Upgraded room to %a" Id.Room_id.pp replacement);
  replacement

let aliases_response_jsont =
  Jsont.Object.(
    map ~kind:"aliases_response" Fun.id
    |> mem "aliases" (Jsont.list Id.Room_alias.jsont) ~enc:Fun.id
    |> finish)

let get_aliases client ~room_id =
  let* body = Client.Http.get client ~path:(room_path room_id "aliases") () in
  Client.Http.decode_response aliases_response_jsont body

type timestamp_event = {
  event_id : Id.Event_id.t;
  origin_server_ts : Event.Timestamp.t;
}

let timestamp_event_jsont =
  Jsont.Object.(
    map ~kind:"timestamp_to_event" (fun event_id origin_server_ts ->
        { event_id; origin_server_ts })
    |> mem "event_id" Id.Event_id.jsont ~enc:(fun t -> t.event_id)
    |> mem "origin_server_ts" Event.Timestamp.jsont ~enc:(fun t ->
        t.origin_server_ts)
    |> finish)

let timestamp_to_event_route =
  Route.v "/_matrix/client/v1/rooms/{room_id}/timestamp_to_event"

let timestamp_to_event client ~room_id ~ts ~dir =
  (* [/_matrix/client/v1], so the absolute-path helper rather than
     {!Client.Http.get}, which prefixes v3. *)
  let path =
    Route.expand_exn timestamp_to_event_route
      [ ("room_id", Id.Room_id.to_string room_id) ]
  in
  let query =
    [
      ("ts", Int64.to_string (Event.Timestamp.to_ms ts));
      ("dir", Matrix_proto.Common.Direction.to_string dir);
    ]
  in
  let* body, _content_type = Client.Http.get_bytes client ~path ~query () in
  Client.Http.decode_response timestamp_event_jsont body
