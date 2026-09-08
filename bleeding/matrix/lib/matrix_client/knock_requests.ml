open Result.Syntax
module Id = Matrix_proto.Id
module Event = Matrix_proto.Event
open Matrix_proto.Json

type t = {
  room_id : Id.Room_id.t;
  event_id : Id.Event_id.t option;
  timestamp : Event.Timestamp.t option;
  user_id : Id.User_id.t;
  display_name : string option;
  avatar_url : Media.Mxc.t option;
  reason : string option;
  is_seen : bool;
}

type seen = (string * string * string option) list

let seen_jsont : seen Jsont.t =
  let item =
    Jsont.Object.(
      map (fun room_id user_id event_id -> (room_id, user_id, event_id))
      |> mem "room_id" Matrix_proto.Json.Codec.string ~enc:(fun (r, _, _) -> r)
      |> mem "user_id" Matrix_proto.Json.Codec.string ~enc:(fun (_, u, _) -> u)
      |> opt_mem "event_id" Matrix_proto.Json.Codec.string
           ~enc:(fun (_, _, e) -> e)
      |> finish)
  in
  Jsont.list item

let seen_slot = Store.Slot.v ~name:"knock_requests.seen" seen_jsont

let seen_entries_result store =
  let+ entries = Store.Slot.find store seen_slot in
  Option.value entries ~default:[]

(* Listing remains a total projection over room state. A damaged auxiliary
   slot makes requests appear unseen, but [mark_seen] below reports the damage
   instead of overwriting it. *)
let seen_entries store = Result.value (seen_entries_result store) ~default:[]

let event_key = function
  | None -> None
  | Some e -> Some (Id.Event_id.to_string e)

let is_seen entries room user event =
  let r = Id.Room_id.to_string room and u = Id.User_id.to_string user in
  List.exists
    (fun (r', u', e') ->
      String.equal r r' && String.equal u u'
      && Option.equal String.equal e' (event_key event))
    entries

let list store ~room_id =
  match Store.find_room store room_id with
  | None -> []
  | Some room ->
      let entries = seen_entries store in
      room.state_events
      |> List.filter_map (fun (e : Store.state_event) ->
          if
            not
              (Event.Event_type.equal e.event_type Event.Event_type.Room_member)
          then None
          else
            match Id.User_id.of_string e.state_key with
            | Error _ -> None
            | Ok user_id -> (
                let content =
                  Result.to_option
                    (Jsont.Json.decode Event.Room_member_content.jsont e.content)
                in
                match content with
                | Some c
                  when Event.Membership.equal
                         (Event.Room_member_content.membership c)
                         Event.Membership.Knock ->
                    Some
                      {
                        room_id;
                        event_id = e.event_id;
                        timestamp = e.origin_server_ts;
                        user_id;
                        display_name = Event.Room_member_content.displayname c;
                        avatar_url =
                          Option.bind (Event.Room_member_content.avatar_url c)
                            (fun s -> Result.to_option (Media.Mxc.of_string s));
                        reason = Event.Room_member_content.reason c;
                        is_seen = is_seen entries room_id user_id e.event_id;
                      }
                | _ -> None))

let all store =
  List.concat_map
    (fun (room : Store.room_info) -> list store ~room_id:room.room_id)
    (Store.rooms store)

let mark_seen store (request : t) =
  let* entries = seen_entries_result store in
  let room = Id.Room_id.to_string request.room_id
  and user = Id.User_id.to_string request.user_id
  and event_id = event_key request.event_id in
  let already =
    is_seen entries request.room_id request.user_id request.event_id
  in
  let entries =
    if already then entries else (room, user, event_id) :: entries
  in
  let* () = Store.Slot.set store seen_slot entries in
  Store.flush store

let accept client (request : t) =
  Rooms.invite client ~room_id:request.room_id ~user_id:request.user_id ()

let decline client (request : t) ?reason () =
  Rooms.kick client ~room_id:request.room_id ~user_id:request.user_id ?reason ()

let decline_and_ban client (request : t) ?reason () =
  Rooms.ban client ~room_id:request.room_id ~user_id:request.user_id ?reason ()
