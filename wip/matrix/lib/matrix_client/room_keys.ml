type version_info = {
  version : string;
  algorithm : string;
  auth_data : Jsont.json;
  count : int;
  etag : string;
}

type key_backup_data = Backup.key_backup_data = {
  first_message_index : int;
  forwarded_count : int;
  is_verified : bool;
  session_data : Backup.encrypted_session_data;
}

let equal_version_info (a : version_info) (b : version_info) =
  String.equal a.version b.version
  && String.equal a.algorithm b.algorithm
  && a.count = b.count && String.equal a.etag b.etag
  && Jsont.Json.equal a.auth_data b.auth_data

type sessions = Backup.sessions
type rooms = Backup.rooms
type update_response = { etag : string; count : int }

let string_map_jsont = Json_codec.string_map
let key_backup_data_jsont = Backup.key_backup_data_jsont
let sessions_jsont : sessions Jsont.t = string_map_jsont key_backup_data_jsont

(* A room's entry is wrapped in an object with a single [sessions] member. *)
let room_key_backup_jsont : sessions Jsont.t =
  Jsont.Object.(
    map (fun sessions -> sessions)
    |> mem "sessions" sessions_jsont ~dec_absent:(fun () -> []) ~enc:Fun.id
    |> finish)

let rooms_jsont : rooms Jsont.t = string_map_jsont room_key_backup_jsont

type rooms_body = { rooms : rooms } [@@warning "-69"]

let rooms_body_jsont =
  Jsont.Object.(
    map (fun rooms -> { rooms })
    |> mem "rooms" rooms_jsont
         ~dec_absent:(fun () -> [])
         ~enc:(fun t -> t.rooms)
    |> finish)

let version_info_jsont =
  Jsont.Object.(
    map (fun version algorithm auth_data count etag ->
        { version; algorithm; auth_data; count; etag })
    |> mem "version" Matrix_proto.Json.Codec.string
         ~dec_absent:(fun () -> "")
         ~enc:(fun (t : version_info) -> t.version)
    |> mem "algorithm" Matrix_proto.Json.Codec.string
         ~enc:(fun (t : version_info) -> t.algorithm)
    |> mem "auth_data" Matrix_proto.Json.Codec.json
         ~enc:(fun (t : version_info) -> t.auth_data)
    |> mem "count" Matrix_proto.Json.Codec.int
         ~dec_absent:(fun () -> 0)
         ~enc:(fun (t : version_info) -> t.count)
    |> mem "etag" Matrix_proto.Json.Codec.string
         ~dec_absent:(fun () -> "")
         ~enc:(fun (t : version_info) -> t.etag)
    |> finish)

type version_request = { algorithm : string; auth_data : Jsont.json }
[@@warning "-69"]

let version_request_jsont =
  Jsont.Object.(
    map (fun algorithm auth_data -> { algorithm; auth_data })
    |> mem "algorithm" Matrix_proto.Json.Codec.string
         ~enc:(fun (t : version_request) -> t.algorithm)
    |> mem "auth_data" Matrix_proto.Json.Codec.json
         ~enc:(fun (t : version_request) -> t.auth_data)
    |> finish)

type version_response = { version : string } [@@warning "-69"]

let version_response_jsont =
  Jsont.Object.(
    map (fun version -> { version })
    |> mem "version" Matrix_proto.Json.Codec.string
         ~enc:(fun (t : version_response) -> t.version)
    |> finish)

let update_response_jsont =
  Jsont.Object.(
    map (fun etag count -> { etag; count })
    |> mem "etag" Matrix_proto.Json.Codec.string
         ~dec_absent:(fun () -> "")
         ~enc:(fun (t : update_response) -> t.etag)
    |> mem "count" Matrix_proto.Json.Codec.int
         ~dec_absent:(fun () -> 0)
         ~enc:(fun (t : update_response) -> t.count)
    |> finish)

let ( let* ) = Result.bind
let room_keys_route = Route.v "/room_keys/keys/{room_id}"
let session_keys_route = Route.v "/room_keys/keys/{room_id}/{session_id}"
let version_route = Route.v "/room_keys/version/{version}"

let keys_path ?room_id ?session_id () =
  match (room_id, session_id) with
  | None, _ -> "/room_keys/keys"
  | Some room, None -> Route.expand_exn room_keys_route [ ("room_id", room) ]
  | Some room, Some session ->
      Route.expand_exn session_keys_route
        [ ("room_id", room); ("session_id", session) ]

let room_str = Matrix_proto.Id.Room_id.to_string
let session_str = Matrix_proto.Id.Session_id.to_string

let create_version client ~algorithm ~auth_data =
  let* body =
    Client.Http.encode_body version_request_jsont { algorithm; auth_data }
  in
  let* reply = Client.Http.post client ~path:"/room_keys/version" ~body () in
  let* r = Client.Http.decode_response version_response_jsont reply in
  Ok r.version

let get_current_version client =
  let* reply = Client.Http.get client ~path:"/room_keys/version" () in
  Client.Http.decode_response version_info_jsont reply

let get_version client ~version =
  let path = Route.expand_exn version_route [ ("version", version) ] in
  let* reply = Client.Http.get client ~path () in
  let* info = Client.Http.decode_response version_info_jsont reply in
  (* The per-version response is specified to carry [version], but fall
     back to what was asked for rather than surfacing an empty one. *)
  Ok (if info.version = "" then { info with version } else info)

let update_version client ~version ~algorithm ~auth_data =
  let path = Route.expand_exn version_route [ ("version", version) ] in
  let* body =
    Client.Http.encode_body version_request_jsont { algorithm; auth_data }
  in
  let* _ = Client.Http.put client ~path ~body () in
  Ok ()

let delete_version client ~version =
  let path = Route.expand_exn version_route [ ("version", version) ] in
  let* _ = Client.Http.delete client ~path () in
  Ok ()

let put_body client ~path ~version ~body =
  let* reply =
    Client.Http.put client ~path ~query:[ ("version", version) ] ~body ()
  in
  Client.Http.decode_response update_response_jsont reply

let put_keys client ~version rooms =
  let* body = Client.Http.encode_body rooms_body_jsont { rooms } in
  put_body client ~path:(keys_path ()) ~version ~body

let get_keys client ~version =
  let* reply =
    Client.Http.get client ~path:(keys_path ())
      ~query:[ ("version", version) ]
      ()
  in
  let* body = Client.Http.decode_response rooms_body_jsont reply in
  Ok body.rooms

let delete_keys client ~version =
  let* reply =
    Client.Http.delete client ~path:(keys_path ())
      ~query:[ ("version", version) ]
      ()
  in
  Client.Http.decode_response update_response_jsont reply

let put_room_keys client ~version ~room_id sessions =
  let* body = Client.Http.encode_body room_key_backup_jsont sessions in
  put_body client
    ~path:(keys_path ~room_id:(room_str room_id) ())
    ~version ~body

let get_room_keys client ~version ~room_id =
  let* reply =
    Client.Http.get client
      ~path:(keys_path ~room_id:(room_str room_id) ())
      ~query:[ ("version", version) ]
      ()
  in
  Client.Http.decode_response room_key_backup_jsont reply

let delete_room_keys client ~version ~room_id =
  let* reply =
    Client.Http.delete client
      ~path:(keys_path ~room_id:(room_str room_id) ())
      ~query:[ ("version", version) ]
      ()
  in
  Client.Http.decode_response update_response_jsont reply

let put_session_key client ~version ~room_id ~session_id data =
  let* body = Client.Http.encode_body key_backup_data_jsont data in
  put_body client
    ~path:
      (keys_path ~room_id:(room_str room_id)
         ~session_id:(session_str session_id) ())
    ~version ~body

let get_session_key client ~version ~room_id ~session_id =
  let* reply =
    Client.Http.get client
      ~path:
        (keys_path ~room_id:(room_str room_id)
           ~session_id:(session_str session_id) ())
      ~query:[ ("version", version) ]
      ()
  in
  Client.Http.decode_response key_backup_data_jsont reply

let delete_session_key client ~version ~room_id ~session_id =
  let* reply =
    Client.Http.delete client
      ~path:
        (keys_path ~room_id:(room_str room_id)
           ~session_id:(session_str session_id) ())
      ~query:[ ("version", version) ]
      ()
  in
  Client.Http.decode_response update_response_jsont reply
