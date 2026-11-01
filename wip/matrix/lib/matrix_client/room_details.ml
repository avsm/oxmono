open Result.Syntax
module Id = Matrix_proto.Id
module Event = Matrix_proto.Event
module Json = Matrix_proto.Json

type t = {
  client : Client.t;
  state : Base_client.state;
  room_id : Id.Room_id.t;
}

type member = {
  user_id : Id.User_id.t;
  display_name : string option;
  display_label : string;
  display_name_ambiguous : bool;
  avatar_url : Media.Mxc.t option;
  membership : Event.Membership.t;
  role : Room.role;
  is_service_member : bool;
  is_account_user : bool;
}

let create ~client ~state room_id = { client; state; room_id }
let state t = t.state
let room_id t = t.room_id
let room_info t = Base_client.find_room t.state t.room_id

let members_complete t =
  Option.exists (fun r -> r.Store.members_complete) (room_info t)

let active_membership = function
  | Event.Membership.Join | Event.Membership.Invite -> true
  | Event.Membership.Leave | Event.Membership.Ban | Event.Membership.Knock ->
      false

let avatar content =
  Option.bind (Json.find_string "avatar_url" content) (fun value ->
      if String.equal value "" then None
      else Result.to_option (Media.Mxc.of_string value))

let decode_member (event : Store.state_event) =
  if not (Event.Event_type.equal event.event_type Event.Event_type.Room_member)
  then None
  else
    let user_id = Result.to_option (Id.User_id.of_string event.state_key) in
    let membership =
      Option.bind (Json.find_string "membership" event.content) (fun value ->
          Result.to_option (Event.Membership.of_string value))
    in
    match (user_id, membership) with
    | Some user_id, Some membership when active_membership membership ->
        Some
          ( user_id,
            Json.find_string "displayname" event.content,
            avatar event.content,
            membership )
    | Some _, Some _ | Some _, None | None, _ -> None

let members t =
  let room = Room.create ~client:t.client ~state:t.state t.room_id in
  let service_members =
    Option.fold ~none:[] ~some:Base_client.service_members (room_info t)
  in
  let account_user = Base_client.user_id t.state in
  let decoded =
    Base_client.state_events t.state t.room_id
    |> List.filter_map decode_member
    |> List.sort (fun (left, _, _, _) (right, _, _, _) ->
        String.compare (Id.User_id.to_string left) (Id.User_id.to_string right))
  in
  let display_name_counts = Hashtbl.create (List.length decoded) in
  List.iter
    (fun (_, display_name, _, _) ->
      Option.iter
        (fun name ->
          if not (String.equal name "") then
            Hashtbl.replace display_name_counts name
              (1
              + Option.value
                  (Hashtbl.find_opt display_name_counts name)
                  ~default:0))
        display_name)
    decoded;
  List.map
    (fun (user_id, display_name, avatar_url, membership) ->
      let display_name_ambiguous =
        Option.fold ~none:false
          ~some:(fun name ->
            (not (String.equal name ""))
            && Option.value
                 (Hashtbl.find_opt display_name_counts name)
                 ~default:0
               > 1)
          display_name
      in
      let base =
        match display_name with
        | Some name when not (String.equal name "") -> name
        | Some _ | None -> Id.User_id.to_string user_id
      in
      let display_label =
        if display_name_ambiguous then
          Printf.sprintf "%s (%s)" base (Id.User_id.to_string user_id)
        else base
      in
      {
        user_id;
        display_name;
        display_label;
        display_name_ambiguous;
        avatar_url;
        membership;
        role = Room.suggested_role room user_id;
        is_service_member =
          List.exists (Id.User_id.equal user_id) service_members;
        is_account_user = Id.User_id.equal user_id account_user;
      })
    decoded

let member_count t = List.length (members t)

let service_member_count t =
  members t
  |> List.fold_left
       (fun count member -> count + if member.is_service_member then 1 else 0)
       0

let human_member_count t = member_count t - service_member_count t

let ensure_members t =
  match room_info t with
  | None -> Error (Error.Json_error "room is not cached")
  | Some info when info.members_complete -> Ok t
  | Some _ ->
      let* members = Rooms.get_members t.client ~room_id:t.room_id () in
      let state = Base_client.replace_members t.state t.room_id members in
      Ok { t with state }
