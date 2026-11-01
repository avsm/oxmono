module Id = Matrix_proto.Id
module Event = Matrix_proto.Event
module Json = Matrix_proto.Json

type t = {
  client : Client.t;
  state : Base_client.state;
  room_id : Id.Room_id.t;
}

let create ~client ~state room_id = { client; state; room_id }
let room_id t = t.room_id

let state_event t event_type =
  Base_client.find_state_event t.state t.room_id ~event_type ()

let string_list json =
  match Json.as_array json with
  | None -> []
  | Some values -> List.filter_map Json.as_string values

let member_has_membership memberships content =
  match Json.find_string "membership" content with
  | None -> false
  | Some membership -> List.exists (String.equal membership) memberships

let members_with_membership t memberships =
  Base_client.state_events t.state t.room_id
  |> List.filter_map (fun (event : Store.state_event) ->
      if
        Event.Event_type.equal event.event_type Event.Event_type.Room_member
        && member_has_membership memberships event.content
      then Result.to_option (Id.User_id.of_string event.state_key)
      else None)

let joined_members t = members_with_membership t [ "join" ]
let active_members t = members_with_membership t [ "join"; "invite" ]

let ipv4_literal host =
  match String.split_on_char '.' host with
  | [ a; b; c; d ] ->
      List.for_all
        (fun part ->
          part <> ""
          && String.for_all (fun char -> char >= '0' && char <= '9') part
          && Option.fold ~none:false
               ~some:(fun n -> n >= 0 && n <= 255)
               (int_of_string_opt part))
        [ a; b; c; d ]
  | _ -> false

let host_part server =
  if String.starts_with ~prefix:"[" server then
    match String.index_opt server ']' with
    | Some close -> String.sub server 0 (close + 1)
    | None -> server
  else
    match String.rindex_opt server ':' with
    | Some colon
      when String.index_opt server ':' = Some colon
           && colon + 1 < String.length server
           && String.for_all
                (fun char -> char >= '0' && char <= '9')
                (String.sub server (colon + 1)
                   (String.length server - colon - 1)) ->
        String.sub server 0 colon
    | Some _ | None -> server

let is_ip_literal server =
  let host = host_part server in
  String.starts_with ~prefix:"[" host || ipv4_literal host

type server_acl = { allow : string list; deny : string list }

let server_acl t =
  match state_event t Event.Event_type.Room_server_acl with
  | None -> None
  | Some event ->
      let list_member name default =
        Option.fold ~none:default ~some:string_list
          (Json.find_mem name event.content)
      in
      Some { allow = list_member "allow" [ "*" ]; deny = list_member "deny" [] }

let server_allowed acl server =
  (* Matrix's permalink routing algorithm always excludes IP literals, even
     when the room ACL would otherwise permit them. *)
  (not (is_ip_literal server))
  &&
  match acl with
  | None -> true
  | Some acl ->
      (not
         (List.exists
            (fun pattern -> Matrix_glob.whole_string ~pattern server)
            acl.deny))
      && List.exists
           (fun pattern -> Matrix_glob.whole_string ~pattern server)
           acl.allow

let power_levels t =
  Option.map
    (fun (event : Store.state_event) -> event.content)
    (state_event t Event.Event_type.Room_power_levels)

let user_power_level power_levels user_id =
  match power_levels with
  | None -> 0
  | Some content ->
      let default =
        Option.value (Json.find_int "users_default" content) ~default:0
      in
      let explicit =
        Option.bind (Json.find_mem "users" content) (fun users ->
            Json.find_int (Id.User_id.to_string user_id) users)
      in
      Option.value explicit ~default

let take count values =
  let rec loop count acc = function
    | _ when count = 0 -> List.rev acc
    | [] -> List.rev acc
    | value :: rest -> loop (count - 1) (value :: acc) rest
  in
  loop count [] values

let routing_candidates t =
  let acl = server_acl t in
  let power_levels = power_levels t in
  let members =
    joined_members t
    |> List.filter_map (fun user_id ->
        let server =
          Id.Server_name.to_string (Id.User_id.server_name user_id)
        in
        if server_allowed acl server then
          Some (user_id, server, user_power_level power_levels user_id)
        else None)
  in
  let high_power_server =
    members
    |> List.sort (fun (left_user, _, left_power) (right_user, _, right_power) ->
        let by_power = Int.compare right_power left_power in
        if by_power <> 0 then by_power
        else
          String.compare
            (Id.User_id.to_string left_user)
            (Id.User_id.to_string right_user))
    |> List.find_opt (fun (_, _, power) -> power >= 50)
    |> Option.map (fun (_, server, _) -> server)
  in
  let populations = Hashtbl.create 8 in
  List.iter
    (fun (_, server, _) ->
      if not (Option.equal String.equal high_power_server (Some server)) then
        Hashtbl.replace populations server
          (1 + Option.value (Hashtbl.find_opt populations server) ~default:0))
    members;
  let by_population =
    Hashtbl.to_seq populations |> List.of_seq
    |> List.sort (fun (left_server, left_count) (right_server, right_count) ->
        let by_count = Int.compare right_count left_count in
        if by_count <> 0 then by_count
        else String.compare left_server right_server)
    |> List.map fst
  in
  take 3 (Option.to_list high_power_server @ by_population)

let room_alias t =
  match Base_client.find_room t.state t.room_id with
  | Some info when Option.is_some info.canonical_alias -> info.canonical_alias
  | Some _ | None -> (
      match state_event t Event.Event_type.Room_canonical_alias with
      | None -> None
      | Some event ->
          Option.bind (Json.find_mem "alt_aliases" event.content)
            (fun aliases ->
              string_list aliases |> List.rev
              |> List.find_map (fun alias ->
                  Result.to_option (Id.Room_alias.of_string alias))))

let append_via base via =
  match via with
  | [] -> base
  | servers ->
      let query =
        servers
        |> List.map (fun server ->
            "via=" ^ Uriz.pct_encode ~component:`Query_value server)
        |> String.concat "&"
      in
      base ^ "?" ^ query

let permalink t ?via () =
  match room_alias t with
  | Some alias ->
      "https://matrix.to/#/"
      ^ Uriz.pct_encode ~component:`Segment (Id.Room_alias.to_string alias)
  | None ->
      let base =
        "https://matrix.to/#/"
        ^ Uriz.pct_encode ~component:`Segment (Id.Room_id.to_string t.room_id)
      in
      append_via base (Option.value via ~default:(routing_candidates t))

let event_permalink t ?via event_id =
  let base =
    Printf.sprintf "https://matrix.to/#/%s/%s"
      (Uriz.pct_encode ~component:`Segment (Id.Room_id.to_string t.room_id))
      (Uriz.pct_encode ~component:`Segment (Id.Event_id.to_string event_id))
  in
  append_via base (Option.value via ~default:(routing_candidates t))

let direct_targets t =
  match Base_client.find_account_data t.state "m.direct" with
  | None -> []
  | Some content -> (
      match Json.as_object content with
      | None -> []
      | Some bindings ->
          List.filter_map
            (fun ((user_id, _), rooms) ->
              let is_target =
                string_list rooms
                |> List.exists (String.equal (Id.Room_id.to_string t.room_id))
              in
              if is_target then Result.to_option (Id.User_id.of_string user_id)
              else None)
            bindings)

let dm_target t =
  match direct_targets t with [ target ] -> Some target | _ -> None

let set_is_direct t is_direct =
  if is_direct then
    let own = Base_client.user_id t.state in
    active_members t
    |> List.filter (fun user -> not (Id.User_id.equal user own))
    |> fun users ->
    Account_data.mark_room_as_dm t.client ~room_id:t.room_id ~users
  else Account_data.unmark_room_as_dm t.client ~room_id:t.room_id

type role = Creator | Administrator | Moderator | User

let explicit_creators t =
  match state_event t Event.Event_type.Room_create with
  | None -> []
  | Some event ->
      let version =
        Option.value
          (Json.find_string "room_version" event.content)
          ~default:"1"
      in
      let version_supports_creators =
        Option.fold ~none:false
          ~some:(fun version -> version >= 12)
          (int_of_string_opt version)
      in
      if not version_supports_creators then []
      else
        let creator = Option.to_list event.sender in
        let additional =
          Option.fold ~none:[] ~some:string_list
            (Json.find_mem "additional_creators" event.content)
          |> List.filter_map (fun user ->
              Result.to_option (Id.User_id.of_string user))
        in
        creator @ additional

let suggested_role t user_id =
  if List.exists (Id.User_id.equal user_id) (explicit_creators t) then Creator
  else
    let level = user_power_level (power_levels t) user_id in
    if level >= 100 then Administrator
    else if level >= 50 then Moderator
    else User

type invite_details = {
  invite_event : Store.state_event;
  inviter : Id.User_id.t;
  inviter_profile : Store.state_event option;
}

let invite_details t =
  match Base_client.find_room t.state t.room_id with
  | None -> Error (Error.Json_error "room is not cached")
  | Some info when info.membership <> Base_client.Invited ->
      Error (Error.Json_error "room is not invited")
  | Some _ ->
      let own = Id.User_id.to_string (Base_client.user_id t.state) in
      let state = Base_client.state_events t.state t.room_id in
      let own_invite =
        List.find_opt
          (fun (event : Store.state_event) ->
            Event.Event_type.equal event.event_type Event.Event_type.Room_member
            && String.equal event.state_key own
            && Option.equal String.equal
                 (Json.find_string "membership" event.content)
                 (Some "invite"))
          state
      in
      Option.fold ~none:(Error (Error.Json_error "own invite is not cached"))
        ~some:(fun (invite_event : Store.state_event) ->
          match invite_event.sender with
          | None -> Error (Error.Json_error "invite has no inviter")
          | Some inviter ->
              let inviter_profile =
                List.find_opt
                  (fun (event : Store.state_event) ->
                    Event.Event_type.equal event.event_type
                      Event.Event_type.Room_member
                    && String.equal event.state_key
                         (Id.User_id.to_string inviter))
                  state
              in
              Ok { invite_event; inviter; inviter_profile })
        own_invite

let mark_as_dm t ~user_id =
  Account_data.mark_as_dm t.client ~user_id ~room_id:t.room_id

let unmark_as_dm t ~user_id =
  Account_data.unmark_as_dm t.client ~user_id ~room_id:t.room_id
