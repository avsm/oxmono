open Result.Syntax
module Id = Matrix_proto.Id
module Event = Matrix_proto.Event
open Matrix_proto.Json

type t = {
  room_id : Id.Room_id.t;
  canonical_alias : Id.Room_alias.t option;
  name : string option;
  topic : string option;
  avatar_url : Media.Mxc.t option;
  num_joined_members : int;
  num_active_members : int option;
  room_type : string option;
  create : Event.Room_create_content.t option;
  tombstone : Event.Room_tombstone_content.t option;
  service_members : Id.User_id.t list option;
  join_rule : Event.Join_rule.t option;
  is_world_readable : bool option;
  membership : Store.membership option;
  is_direct : bool option;
  heroes : Store.hero list option;
}

let avatar s = Option.bind s (fun s -> Result.to_option (Media.Mxc.of_string s))

let state_member room ty member codec =
  match
    Store.find_state_event room ~event_type:(Event.Event_type.of_string ty) ()
  with
  | None -> None
  | Some e ->
      Option.bind (find_mem member e.content) (fun v ->
          Result.to_option (Jsont.Json.decode codec v))

let room_type room =
  state_member room "m.room.create" "type" Matrix_proto.Json.Codec.string

let state_content room event_type codec =
  Option.bind (Store.find_state_event room ~event_type ()) (fun event ->
      Result.to_option (Jsont.Json.decode codec event.content))

let create room =
  state_content room Event.Event_type.Room_create
    Event.Room_create_content.jsont

let tombstone room =
  state_content room Event.Event_type.Room_tombstone
    Event.Room_tombstone_content.jsont

let service_members room =
  match
    state_content room Event.Event_type.Room_member_hints
      Event.Room_member_hints_content.jsont
  with
  | Some content ->
      Some (Event.Room_member_hints_content.service_members content)
  | None ->
      Option.map Event.Io_element_functional_members_content.service_members
        (state_content room Event.Event_type.Io_element_functional_members
           Event.Io_element_functional_members_content.jsont)

let join_rule room =
  match
    Store.find_state_event room ~event_type:Event.Event_type.Room_join_rules ()
  with
  | None -> None
  | Some e ->
      Option.map Event.Room_join_rules_content.join_rule
        (Result.to_option
           (Jsont.Json.decode Event.Room_join_rules_content.jsont e.content))

let world_readable room =
  match
    Store.find_state_event room
      ~event_type:Event.Event_type.Room_history_visibility ()
  with
  | None -> None
  | Some e ->
      Option.map
        (fun c ->
          Event.History_visibility.equal
            (Event.Room_history_visibility_content.history_visibility c)
            Event.History_visibility.World_readable)
        (Result.to_option
           (Jsont.Json.decode Event.Room_history_visibility_content.jsont
              e.content))

let of_room (room : Store.room_info) =
  {
    room_id = room.room_id;
    canonical_alias = room.canonical_alias;
    name = room.name;
    topic = room.topic;
    avatar_url = avatar room.avatar_url;
    num_joined_members = room.joined_member_count;
    num_active_members =
      Some (room.joined_member_count + room.invited_member_count);
    room_type = room_type room;
    create = create room;
    tombstone = tombstone room;
    service_members = service_members room;
    join_rule = join_rule room;
    is_world_readable = world_readable room;
    membership = Some room.membership;
    is_direct = Some room.is_dm;
    heroes = Some room.heroes;
  }

let of_summary (s : Directory.room_summary) =
  {
    room_id = s.room_id;
    canonical_alias = s.canonical_alias;
    name = s.name;
    topic = s.topic;
    avatar_url = s.avatar_url;
    num_joined_members = s.num_joined_members;
    num_active_members = None;
    room_type = s.room_type;
    create = None;
    tombstone = None;
    service_members = None;
    join_rule = s.join_rule;
    is_world_readable = Some s.world_readable;
    (* MSC3266 reports [leave] for an unknown room on some servers. As in the
       Rust SDK, only a locally cached room establishes the caller's state. *)
    membership = None;
    is_direct = None;
    heroes = None;
  }

let of_remote_state room_id state joined_members =
  let event ty =
    List.find_opt
      (fun (e : Event.Raw_event.t) ->
        Event.Event_type.equal e.type_ ty && e.state_key = Some "")
      state
  in
  let content ty codec =
    Option.bind (event ty) (fun e ->
        Result.to_option (Jsont.Json.decode codec e.content))
  in
  let find ty member codec =
    List.find_map
      (fun (e : Event.Raw_event.t) ->
        if
          Event.Event_type.equal e.type_ (Event.Event_type.of_string ty)
          && e.state_key = Some ""
        then
          Option.map
            (fun x -> Result.to_option (Jsont.Json.decode codec x))
            (find_mem member e.content)
        else None)
      state
    |> Option.join
  in
  let canonical_alias =
    match find "m.room.canonical_alias" "alias" Id.Room_alias.jsont with
    | Some x -> Some x
    | None -> None
  in
  let name = find "m.room.name" "name" Matrix_proto.Json.Codec.string in
  let topic = find "m.room.topic" "topic" Matrix_proto.Json.Codec.string in
  let avatar_url =
    avatar (find "m.room.avatar" "url" Matrix_proto.Json.Codec.string)
  in
  let room_type = find "m.room.create" "type" Matrix_proto.Json.Codec.string in
  let create =
    content Event.Event_type.Room_create Event.Room_create_content.jsont
  in
  let tombstone =
    content Event.Event_type.Room_tombstone Event.Room_tombstone_content.jsont
  in
  let service_members =
    match
      content Event.Event_type.Room_member_hints
        Event.Room_member_hints_content.jsont
    with
    | Some hints -> Some (Event.Room_member_hints_content.service_members hints)
    | None ->
        Option.map Event.Io_element_functional_members_content.service_members
          (content Event.Event_type.Io_element_functional_members
             Event.Io_element_functional_members_content.jsont)
  in
  let join_rule =
    Option.map Event.Room_join_rules_content.join_rule
      (List.find_map
         (fun (e : Event.Raw_event.t) ->
           if
             Event.Event_type.equal e.type_ Event.Event_type.Room_join_rules
             && e.state_key = Some ""
           then
             Some
               (Result.to_option
                  (Jsont.Json.decode Event.Room_join_rules_content.jsont
                     e.content))
           else None)
         state
      |> Option.join)
  in
  let is_world_readable =
    Option.map
      (fun c ->
        Event.History_visibility.equal
          (Event.Room_history_visibility_content.history_visibility c)
          Event.History_visibility.World_readable)
      (List.find_map
         (fun (e : Event.Raw_event.t) ->
           if
             Event.Event_type.equal e.type_
               Event.Event_type.Room_history_visibility
             && e.state_key = Some ""
           then
             Some
               (Result.to_option
                  (Jsont.Json.decode Event.Room_history_visibility_content.jsont
                     e.content))
           else None)
         state
      |> Option.join)
  in
  {
    room_id;
    canonical_alias;
    name;
    topic;
    avatar_url;
    num_joined_members = List.length joined_members;
    num_active_members = None;
    room_type;
    create;
    tombstone;
    service_members;
    join_rule;
    is_world_readable;
    membership = None;
    is_direct = None;
    heroes = None;
  }

let room_id_for store = function
  | `Room_id id -> Some id
  | `Room_alias alias ->
      List.find_map
        (fun (r : Store.room_info) ->
          match r.canonical_alias with
          | Some a when Id.Room_alias.equal a alias -> Some r.room_id
          | _ -> None)
        (Store.rooms store)

let target_server = function
  | `Room_id id -> Id.Room_id.server_name id
  | `Room_alias alias -> Some (Id.Room_alias.server_name alias)

(* As in rust-sdk, give the homeserver one useful federation destination when
   the caller supplied none and the room belongs to another server. *)
let with_default_via client room_id_or_alias via =
  match (via, Client.session client, target_server room_id_or_alias) with
  | _ :: _, _, _ | [], None, _ | [], Some _, None -> via
  | [], Some session, Some target ->
      let own = Id.User_id.server_name session.user_id in
      if Id.Server_name.equal own target then []
      else [ Id.Server_name.to_string target ]

let remote client ~room_id_or_alias ~via =
  match Directory.get_summary client ~room_id_or_alias ~via () with
  | Ok s -> Ok (of_summary s)
  | Error summary_error -> (
      let* room_id =
        match room_id_or_alias with
        | `Room_id id -> Ok id
        | `Room_alias alias ->
            let+ a = Directory.resolve_alias client ~alias in
            a.room_id
      in
      match
        ( State.get_state client ~room_id,
          Rooms.get_joined_members client ~room_id )
      with
      | Ok state, Ok members -> Ok (of_remote_state room_id state members)
      | Error _, Error _ | Error _, Ok _ | Ok _, Error _ -> Error summary_error)

let get client ~store ~room_id_or_alias ?(via = []) () =
  let via = with_default_via client room_id_or_alias via in
  match room_id_for store room_id_or_alias with
  | Some id -> (
      match Store.find_room store id with
      (* Rust trusts cached joined-room state. Invited, knocked and left rooms
         can have changed without another sync, so refresh those remotely and
         use the persisted projection only as a final offline fallback. *)
      | Some room when room.membership = Store.Joined -> Ok (of_room room)
      | Some room -> (
          match remote client ~room_id_or_alias:(`Room_id id) ~via with
          | Ok p ->
              Ok
                {
                  p with
                  membership = Some room.membership;
                  is_direct = Some room.is_dm;
                  heroes = Some room.heroes;
                  num_active_members =
                    Some (room.joined_member_count + room.invited_member_count);
                }
          | Error _ -> Ok (of_room room))
      | None -> remote client ~room_id_or_alias ~via)
  | None -> remote client ~room_id_or_alias ~via
