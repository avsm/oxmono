open Result.Syntax
module Id = Matrix_proto.Id
module Event = Matrix_proto.Event

let src = Logs.Src.create "matrix.account_data" ~doc:"Matrix account data"

module Log = (val Logs.src_log src : Logs.LOG)

let with_user client f =
  match Client.session client with
  | None -> Error Error.No_session
  | Some { Client.user_id; _ } -> f user_id

let user_account_data_path = Route.v "/user/{user_id}/account_data/{event_type}"

let room_account_data_path =
  Route.v "/user/{user_id}/rooms/{room_id}/account_data/{event_type}"

let path ?room_id ~user_id event_type =
  let bindings =
    [
      ("user_id", Id.User_id.to_string user_id);
      ("event_type", Event.Event_type.to_string event_type);
    ]
  in
  match room_id with
  | None -> Route.expand_exn user_account_data_path bindings
  | Some room_id ->
      Route.expand_exn room_account_data_path
        (("room_id", Id.Room_id.to_string room_id) :: bindings)

let read client ?room_id ~event_type () =
  with_user client @@ fun user_id ->
  let* body =
    Client.Http.get client ~path:(path ?room_id ~user_id event_type) ()
  in
  Client.Http.decode_response Matrix_proto.Json.Codec.json body

let write client ?room_id ~event_type ~content () =
  with_user client @@ fun user_id ->
  let* body = Client.Http.encode_body Matrix_proto.Json.Codec.json content in
  let+ _ =
    Client.Http.put client ~path:(path ?room_id ~user_id event_type) ~body ()
  in
  ()

let get client ~event_type = read client ~event_type ()
let set client ~event_type ~content = write client ~event_type ~content ()
let get_room client ~room_id ~event_type = read client ~room_id ~event_type ()

let set_room client ~room_id ~event_type ~content =
  write client ~room_id ~event_type ~content ()

let set_marked_unread client ~room_id ~unread =
  let content = Event.Marked_unread_content.{ unread } in
  match Jsont.Json.encode Event.Marked_unread_content.jsont content with
  | Error e -> Error (Error.Json_error e)
  | Ok content ->
      set_room client ~room_id ~event_type:Event.Event_type.Marked_unread
        ~content

(* [m.direct] maps a user id to the rooms that are direct messages with
   them. *)
(* Account data is written by every client the user has ever run, so one
   unparseable key must not disable direct messaging. *)
let m_direct_jsont =
  Json_codec.keyed_map ~skip_invalid:true ~what:"user id"
    ~of_string:Id.User_id.of_string ~to_string:Id.User_id.to_string
    (Jsont.list Id.Room_id.jsont)

let get_direct client =
  match get client ~event_type:Event.Event_type.Direct with
  | Error (Error.Matrix_error { errcode = Error.M_NOT_FOUND; _ }) -> Ok []
  | Error e -> Error e
  | Ok json -> (
      match Jsont.Json.decode m_direct_jsont json with
      | Ok bindings -> Ok bindings
      | Error e -> Error (Error.Json_error e))

let find_dm_rooms client ~user_id =
  let+ bindings = get_direct client in
  Option.value (List.assoc_opt user_id bindings) ~default:[]

let add_direct client ~user_id ~room_id =
  let* bindings = get_direct client in
  let existing = Option.value (List.assoc_opt user_id bindings) ~default:[] in
  if List.exists (Id.Room_id.equal room_id) existing then Ok ()
  else
    let bindings =
      (user_id, room_id :: existing) :: List.remove_assoc user_id bindings
    in
    match Jsont.Json.encode m_direct_jsont bindings with
    | Error e -> Error (Error.Json_error e)
    | Ok content -> set client ~event_type:Event.Event_type.Direct ~content

let mark_as_dm = add_direct

let unmark_as_dm client ~user_id ~room_id =
  let* bindings = get_direct client in
  let changed = ref false in
  let updated =
    List.filter_map
      (fun (u, rooms) ->
        if not (Id.User_id.equal u user_id) then Some (u, rooms)
        else
          let old_rooms = rooms in
          let rooms =
            List.filter (fun r -> not (Id.Room_id.equal r room_id)) old_rooms
          in
          if List.length rooms <> List.length old_rooms then changed := true;
          if rooms = [] then None else Some (u, rooms))
      bindings
  in
  if not !changed then Ok ()
  else
    match Jsont.Json.encode m_direct_jsont updated with
    | Error e -> Error (Error.Json_error e)
    | Ok content -> set client ~event_type:Event.Event_type.Direct ~content

let update_direct client update =
  let* bindings = get_direct client in
  let updated = update bindings in
  if updated = bindings then Ok ()
  else
    match Jsont.Json.encode m_direct_jsont updated with
    | Error e -> Error (Error.Json_error e)
    | Ok content -> set client ~event_type:Event.Event_type.Direct ~content

let mark_room_as_dm client ~room_id ~users =
  update_direct client (fun bindings ->
      List.fold_left
        (fun bindings user_id ->
          let rooms =
            Option.value (List.assoc_opt user_id bindings) ~default:[]
          in
          if List.exists (Id.Room_id.equal room_id) rooms then bindings
          else
            (user_id, rooms @ [ room_id ]) :: List.remove_assoc user_id bindings)
        bindings users)

let unmark_room_as_dm client ~room_id =
  update_direct client (fun bindings ->
      List.filter_map
        (fun (user_id, rooms) ->
          let rooms =
            List.filter (fun id -> not (Id.Room_id.equal id room_id)) rooms
          in
          if rooms = [] then None else Some (user_id, rooms))
        bindings)

let get_or_create_dm client ~user_id ?encrypted () =
  let* existing = find_dm_rooms client ~user_id in
  match existing with
  | room_id :: _ -> Ok room_id
  | [] ->
      let* room_id =
        Rooms.create client ~preset:Rooms.Trusted_private_chat ~is_direct:true
          ~invite:[ user_id ] ?encrypted ()
      in
      (* The room exists whatever happens to the account data, so a failed
         [m.direct] update is logged rather than returned. *)
      (match add_direct client ~user_id ~room_id with
      | Ok () -> ()
      | Error e -> Log.warn (fun m -> m "m.direct not updated: %a" Error.pp e));
      Ok room_id
