open Result.Syntax
module Id = Matrix_proto.Id
module Event = Matrix_proto.Event

let state_route = Route.v "/rooms/{room_id}/state/{event_type}/{state_key}"
let room_state_route = Route.v "/rooms/{room_id}/state"

let state_path ~room_id ~event_type ~state_key =
  Route.expand_exn state_route
    [
      ("room_id", Id.Room_id.to_string room_id);
      ("event_type", Event.Event_type.to_string event_type);
      ("state_key", state_key);
    ]

let get_state client ~room_id =
  let path =
    Route.expand_exn room_state_route
      [ ("room_id", Id.Room_id.to_string room_id) ]
  in
  let* body = Client.Http.get client ~path () in
  Client.Http.decode_response (Jsont.list Event.Raw_event.jsont) body

let get_state_event client ~room_id ~event_type ?(state_key = "") () =
  let* body =
    Client.Http.get client ~path:(state_path ~room_id ~event_type ~state_key) ()
  in
  Client.Http.decode_response Matrix_proto.Json.Codec.json body

let set_state_response_jsont =
  Jsont.Object.(
    map ~kind:"set_state" Fun.id
    |> mem "event_id" Id.Event_id.jsont ~enc:Fun.id
    |> finish)

let set_state client ~room_id ~event_type ?(state_key = "") ~content () =
  let* body = Client.Http.encode_body Matrix_proto.Json.Codec.json content in
  let* body =
    Client.Http.put client
      ~path:(state_path ~room_id ~event_type ~state_key)
      ~body ()
  in
  Client.Http.decode_response set_state_response_jsont body

(* A room whose name, topic or avatar was never set answers [M_NOT_FOUND],
   which is the absence of the value rather than a failure. A content object
   without the member the specification defines is treated the same way. *)
let get_member client ~room_id ~event_type ~member codec =
  match get_state_event client ~room_id ~event_type () with
  | Error (Error.Matrix_error { errcode = Error.M_NOT_FOUND; _ }) -> Ok None
  | Error e -> Error e
  | Ok json -> (
      match
        Jsont.Json.decode
          (Json_codec.string_map Matrix_proto.Json.Codec.json)
          json
      with
      | Error _ -> Ok None
      | Ok members -> (
          match List.assoc_opt member members with
          | None -> Ok None
          | Some v -> Ok (Result.to_option (Jsont.Json.decode codec v))))

let set_member client ~room_id ~event_type ~member codec value =
  match Jsont.Json.encode codec value with
  | Error e -> Error (Error.Json_error e)
  | Ok v ->
      set_state client ~room_id ~event_type
        ~content:(Json_codec.obj [ (member, v) ])
        ()

let get_name client ~room_id =
  get_member client ~room_id ~event_type:Event.Event_type.Room_name
    ~member:"name" Matrix_proto.Json.Codec.string

let set_name client ~room_id ~name =
  set_member client ~room_id ~event_type:Event.Event_type.Room_name
    ~member:"name" Matrix_proto.Json.Codec.string name

let get_topic client ~room_id =
  get_member client ~room_id ~event_type:Event.Event_type.Room_topic
    ~member:"topic" Matrix_proto.Json.Codec.string

let set_topic client ~room_id ~topic =
  set_member client ~room_id ~event_type:Event.Event_type.Room_topic
    ~member:"topic" Matrix_proto.Json.Codec.string topic

let get_avatar client ~room_id =
  get_member client ~room_id ~event_type:Event.Event_type.Room_avatar
    ~member:"url" Media.Mxc.jsont

let set_avatar client ~room_id ~avatar_url =
  set_member client ~room_id ~event_type:Event.Event_type.Room_avatar
    ~member:"url" Media.Mxc.jsont avatar_url
