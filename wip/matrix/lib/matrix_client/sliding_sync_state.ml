module Room_id = Matrix_proto.Id.Room_id
module Raw_event = Matrix_proto.Event.Raw_event
module Event_type = Matrix_proto.Event.Event_type
module Wire = Matrix_proto.Sliding_sync
module Required_state = Wire.Required_state
module String_map = Map.MakePortable (String)

type room = {
  room_id : Room_id.t;
  name : string option;
  avatar_url : string option;
  is_dm : bool option;
  is_invite : bool;
  highlight_count : int;
  notification_count : int;
  timeline : Raw_event.t list;
  required_state : (Required_state.t * Raw_event.t) list;
  prev_batch : string option;
  joined_count : int option;
  invited_count : int option;
  bump_stamp : int option;
  heroes : Wire.Response.hero list;
}

type t = {
  pos : string option;
  to_device_since : string option;
  lists : (string * int) list;
  rooms : room String_map.t;  (** Keyed by [Room_id.to_string]. *)
  profiles : (Matrix_proto.Id.User_id.t * profile) list;
}

and profile = (string * Jsont.json) list

(* A complete snapshot is needed because sliding-sync responses are deltas;
   restoring only [pos] would leave the first post-restart room list partial. *)
type stored_state = {
  format_version : int;
  pos : string option;
  to_device_since : string option;
  lists : (string * int) list;
  rooms : room String_map.t;
  profiles : (Matrix_proto.Id.User_id.t * profile) list;
}

let empty =
  {
    pos = None;
    to_device_since = None;
    lists = [];
    rooms = String_map.of_seq Seq.empty;
    profiles = [];
  }

let timeline_capacity = 100

let new_room room_id =
  {
    room_id;
    name = None;
    avatar_url = None;
    is_dm = None;
    is_invite = false;
    highlight_count = 0;
    notification_count = 0;
    timeline = [];
    required_state = [];
    prev_batch = None;
    joined_count = None;
    invited_count = None;
    bump_stamp = None;
    heroes = [];
  }

let key (e : Raw_event.t) : Required_state.t =
  {
    Required_state.event_type = e.type_;
    state_key = Option.value e.state_key ~default:"";
  }

(* State is keyed by (type, state_key); a later event of the same key
   replaces the earlier one. *)
let merge_state acc events =
  List.fold_left
    (fun acc e ->
      let k = key e in
      (k, e) :: List.filter (fun (k', _) -> not (Required_state.equal k' k)) acc)
    acc events

let take n l =
  let len = List.length l in
  if len <= n then l else List.filteri (fun i _ -> i >= len - n) l

let is_null = function Jsont.Null _ -> true | _ -> false

let apply_room state (u : Wire.Response.room) =
  let state = if u.limited then { state with timeline = [] } else state in
  {
    state with
    name = (match u.name with Some _ as n -> n | None -> state.name);
    avatar_url =
      (match u.avatar with
      | Wire.Response.Unchanged -> state.avatar_url
      | Wire.Response.Removed -> None
      | Wire.Response.Set a -> Some a);
    is_dm = (match u.is_dm with Some _ as d -> d | None -> state.is_dm);
    is_invite = (match u.invite_state with Some _ -> true | None -> false);
    highlight_count =
      Option.value u.highlight_count ~default:state.highlight_count;
    notification_count =
      Option.value u.notification_count ~default:state.notification_count;
    timeline = take timeline_capacity (state.timeline @ u.timeline);
    required_state = merge_state state.required_state u.required_state;
    prev_batch =
      (match u.prev_batch with Some _ as p -> p | None -> state.prev_batch);
    joined_count =
      (match u.joined_count with
      | Some _ as c -> c
      | None -> state.joined_count);
    invited_count =
      (match u.invited_count with
      | Some _ as c -> c
      | None -> state.invited_count);
    bump_stamp =
      (match u.bump_stamp with Some _ as b -> b | None -> state.bump_stamp);
    heroes = (match u.heroes with Some h -> h | None -> state.heroes);
  }

let apply ?(advance_to_device = true) (t : t) (r : Wire.Response.t) =
  let lists =
    List.fold_left
      (fun acc (name, (l : Wire.Response.list_response)) ->
        (name, l.count) :: List.filter (fun (n, _) -> n <> name) acc)
      t.lists r.lists
    |> List.sort (fun (a, _) (b, _) -> String.compare a b)
  in
  let rooms =
    List.fold_left
      (fun acc (room_id, update) ->
        let key = Room_id.to_string room_id in
        let current =
          Option.value (String_map.find_opt key acc) ~default:(new_room room_id)
        in
        String_map.add key (apply_room current update) acc)
      t.rooms r.rooms
  in
  let profiles =
    List.fold_left
      (fun acc (user_id, update) ->
        let previous =
          List.find_map
            (fun (id, profile) ->
              if Matrix_proto.Id.User_id.equal id user_id then Some profile
              else None)
            acc
          |> Option.value ~default:[]
        in
        match update with
        | Wire.Response.Dropped ->
            List.filter
              (fun (id, _) -> not (Matrix_proto.Id.User_id.equal id user_id))
              acc
        | Wire.Response.Updated fields ->
            let fields =
              List.fold_left
                (fun fields (name, value) ->
                  if is_null value then
                    List.filter (fun (name', _) -> name' <> name) fields
                  else
                    (name, value)
                    :: List.filter (fun (name', _) -> name' <> name) fields)
                previous fields
              |> List.sort (fun (a, _) (b, _) -> String.compare a b)
            in
            (user_id, fields)
            :: List.filter
                 (fun (id, _) -> not (Matrix_proto.Id.User_id.equal id user_id))
                 acc)
      t.profiles r.extensions.profiles.users
    |> List.sort (fun (a, _) (b, _) -> Matrix_proto.Id.User_id.compare a b)
  in
  {
    pos = Some r.pos;
    to_device_since =
      (if advance_to_device then
         match Wire.Response.to_device_next_batch r with
         | Some _ as b -> b
         | None -> t.to_device_since
       else t.to_device_since);
    lists;
    rooms;
    profiles;
  }

let pos (t : t) = t.pos
let to_device_since (t : t) = t.to_device_since
let lists (t : t) = t.lists
let profiles (t : t) = t.profiles

(* Rooms without a [bump_stamp] sort last, then by room id, so the order
   is total. *)
let rooms_by_recency (t : t) =
  String_map.bindings t.rooms
  |> List.map snd
  |> List.stable_sort (fun a b ->
      match (a.bump_stamp, b.bump_stamp) with
      | Some x, Some y when x <> y -> Int.compare y x
      | Some _, None -> -1
      | None, Some _ -> 1
      | _ -> Room_id.compare a.room_id b.room_id)

let find_room (t : t) room_id =
  String_map.find_opt (Room_id.to_string room_id) t.rooms

let find_profile (t : t) user_id =
  List.find_map
    (fun (id, profile) ->
      if Matrix_proto.Id.User_id.equal id user_id then Some profile else None)
    t.profiles

let find_profile_field (t : t) user_id field =
  Option.bind (find_profile t user_id) (List.assoc_opt field)

let room_id r = r.room_id
let name r = r.name
let avatar_url r = r.avatar_url
let is_dm r = r.is_dm
let is_invite r = r.is_invite
let highlight_count r = r.highlight_count
let notification_count r = r.notification_count
let timeline r = r.timeline
let required_state r = r.required_state
let prev_batch r = r.prev_batch
let joined_count r = r.joined_count
let invited_count r = r.invited_count
let bump_stamp r = r.bump_stamp
let heroes r = r.heroes

let find_state r k =
  List.find_opt (fun (k', _) -> Required_state.equal k' k) r.required_state
  |> Option.map snd

let profile_jsont : profile Jsont.t =
  Matrix_proto.Json.Codec.as_string_map Matrix_proto.Json.Codec.json
  |> Jsont.map ~dec:String_map.bindings ~enc:(fun fields ->
      String_map.of_seq (List.to_seq fields))

let hero_jsont : Wire.Response.hero Jsont.t =
  Jsont.Object.(
    map (fun user_id displayname avatar_url ->
        { Wire.Response.user_id; displayname; avatar_url })
    |> mem "user_id" Matrix_proto.Id.User_id.jsont
         ~enc:(fun (h : Wire.Response.hero) -> h.user_id)
    |> opt_mem "displayname" Matrix_proto.Json.Codec.string
         ~enc:(fun (h : Wire.Response.hero) -> h.displayname)
    |> opt_mem "avatar_url" Matrix_proto.Json.Codec.string
         ~enc:(fun (h : Wire.Response.hero) -> h.avatar_url)
    |> finish)

let room_jsont : room Jsont.t =
  Jsont.Object.(
    map
      (fun
        room_id
        name
        avatar_url
        is_dm
        is_invite
        highlight_count
        notification_count
        timeline
        required_state
        prev_batch
        joined_count
        invited_count
        bump_stamp
        heroes
      ->
        {
          room_id;
          name;
          avatar_url;
          is_dm;
          is_invite;
          highlight_count;
          notification_count;
          timeline;
          required_state =
            List.map (fun event -> (key event, event)) required_state;
          prev_batch;
          joined_count;
          invited_count;
          bump_stamp;
          heroes;
        })
    |> mem "room_id" Matrix_proto.Id.Room_id.jsont ~enc:(fun (r : room) ->
        r.room_id)
    |> opt_mem "name" Matrix_proto.Json.Codec.string ~enc:(fun r -> r.name)
    |> opt_mem "avatar_url" Matrix_proto.Json.Codec.string ~enc:(fun r ->
        r.avatar_url)
    |> opt_mem "is_dm" Jsont.bool ~enc:(fun r -> r.is_dm)
    |> mem "is_invite" Jsont.bool ~enc:(fun r -> r.is_invite)
    |> mem "highlight_count" Matrix_proto.Json.Codec.Legacy.int ~enc:(fun r ->
        r.highlight_count)
    |> mem "notification_count" Matrix_proto.Json.Codec.Legacy.int
         ~enc:(fun r -> r.notification_count)
    |> mem "timeline"
         (Jsont.list Raw_event.persisted_jsont)
         ~dec_absent:(fun () -> [])
         ~enc:(fun r -> r.timeline)
    |> mem "required_state"
         (Jsont.list Raw_event.persisted_jsont)
         ~dec_absent:(fun () -> [])
         ~enc:(fun r -> List.map snd r.required_state)
    |> opt_mem "prev_batch" Matrix_proto.Json.Codec.string ~enc:(fun r ->
        r.prev_batch)
    |> opt_mem "joined_count" Matrix_proto.Json.Codec.Legacy.int ~enc:(fun r ->
        r.joined_count)
    |> opt_mem "invited_count" Matrix_proto.Json.Codec.Legacy.int ~enc:(fun r ->
        r.invited_count)
    |> opt_mem "bump_stamp" Matrix_proto.Json.Codec.Legacy.int ~enc:(fun r ->
        r.bump_stamp)
    |> mem "heroes" (Jsont.list hero_jsont)
         ~dec_absent:(fun () -> [])
         ~enc:(fun r -> r.heroes)
    |> finish)

let lists_jsont : (string * int) list Jsont.t =
  Matrix_proto.Json.Codec.as_string_map Matrix_proto.Json.Codec.Legacy.int
  |> Jsont.map ~dec:String_map.bindings ~enc:(fun entries ->
      String_map.of_seq (List.to_seq entries))

let rooms_jsont : room String_map.t Jsont.t =
  Jsont.list room_jsont
  |> Jsont.map
       ~dec:(fun rooms ->
         List.fold_left
           (fun acc room ->
             String_map.add (Room_id.to_string room.room_id) room acc)
           (String_map.of_seq Seq.empty)
           rooms)
       ~enc:(fun rooms -> List.map snd (String_map.bindings rooms))

let profiles_jsont : (Matrix_proto.Id.User_id.t * profile) list Jsont.t =
  let decode map =
    List.map
      (fun (name, value) ->
        match Matrix_proto.Id.User_id.of_string name with
        | Ok id -> (id, value)
        | Error (`Msg message) ->
            Jsont.Error.msgf Jsont.Meta.none "profile user id: %s" message)
      (String_map.bindings map)
  in
  let encode entries =
    String_map.of_seq
      (List.to_seq
         (List.map
            (fun (id, fields) -> (Matrix_proto.Id.User_id.to_string id, fields))
            entries))
  in
  Jsont.map ~dec:decode ~enc:encode
    (Matrix_proto.Json.Codec.as_string_map profile_jsont)

let stored_state_jsont : stored_state Jsont.t =
  Jsont.Object.(
    map (fun format_version pos to_device_since lists rooms profiles ->
        if format_version <> 1 then
          Jsont.Error.msgf Jsont.Meta.none
            "unsupported sliding sync state format version %d" format_version;
        { format_version; pos; to_device_since; lists; rooms; profiles })
    |> mem "format_version" Matrix_proto.Json.Codec.Legacy.int
         ~dec_absent:(fun () -> 1)
         ~enc:(fun t -> t.format_version)
    |> opt_mem "pos" Matrix_proto.Json.Codec.string ~enc:(fun t -> t.pos)
    |> opt_mem "to_device_since" Matrix_proto.Json.Codec.string ~enc:(fun t ->
        t.to_device_since)
    |> mem "lists" lists_jsont
         ~dec_absent:(fun () -> [])
         ~enc:(fun t -> t.lists)
    |> mem "rooms" rooms_jsont
         ~dec_absent:(fun () -> String_map.of_seq Seq.empty)
         ~enc:(fun t -> t.rooms)
    |> mem "profiles" profiles_jsont
         ~dec_absent:(fun () -> [])
         ~enc:(fun t -> t.profiles)
    |> finish)

let store_slot = Store.Slot.v ~name:"sliding_sync_state" stored_state_jsont

let load_opt store =
  let open Result.Syntax in
  let+ value = Store.Slot.find store store_slot in
  Option.map
    (fun { pos; to_device_since; lists; rooms; profiles; _ } ->
      { pos; to_device_since; lists; rooms; profiles })
    value

let load store =
  let open Result.Syntax in
  let+ state = load_opt store in
  Option.value state ~default:empty

let discard store = Store.Slot.remove store store_slot

let save store (state : t) =
  let open Result.Syntax in
  let* previous = Store.Slot.find store store_slot in
  let value =
    {
      format_version = 1;
      pos = state.pos;
      to_device_since = state.to_device_since;
      lists = state.lists;
      rooms = state.rooms;
      profiles = state.profiles;
    }
  in
  match Store.Slot.set store store_slot value with
  | Error error -> Error error
  | Ok () -> (
      let rollback () =
        match previous with
        | None ->
            Store.Slot.remove store store_slot;
            Ok ()
        | Some previous -> Store.Slot.set store store_slot previous
      in
      let format_rollback_error original rollback_error =
        Error.Policy_denied
          (Printf.sprintf
             "sliding-sync state flush failed: %s; rollback failed: %s"
             (Error.to_string original)
             (Error.to_string rollback_error))
      in
      match Store.flush store with
      | Ok () -> Ok ()
      | Error error -> (
          match rollback () with
          | Ok () -> Error error
          | Error rollback_error ->
              Error (format_rollback_error error rollback_error))
      | exception exn -> (
          let bt = Printexc.get_raw_backtrace () in
          match rollback () with
          | Ok () -> Printexc.raise_with_backtrace exn bt
          | Error rollback_error ->
              Logs.err (fun m ->
                  m "sliding-sync state rollback failed after exception: %s"
                    (Error.to_string rollback_error));
              Printexc.raise_with_backtrace exn bt))
