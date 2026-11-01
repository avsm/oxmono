open Result.Syntax

let src = Logs.Src.create "matrix.delayed_events" ~doc:"Matrix delayed events"

module Log = (val Logs.src_log src : Logs.LOG)

let unstable_prefix = "org.matrix.msc4140"

(* The delay rides on the stable send endpoints as a query parameter; the
   management endpoints live under the unstable base path. *)
let delay_query = unstable_prefix ^ ".delay"
let base_path = "/_matrix/client/unstable/" ^ unstable_prefix
let send_route = Route.v "/rooms/{room_id}/send/{event_type}/{transaction_id}"
let state_route = Route.v "/rooms/{room_id}/state/{event_type}/{state_key}"
let update_route = Route.v (base_path ^ "/delayed_events/{delay_id}")
let list_route = Route.v (base_path ^ "/delayed_events")

let current_send_route =
  Route.v
    (base_path ^ "/rooms/{room_id}/delayed_event/{event_type}/{transaction_id}")

let current_get_route = Route.v (base_path ^ "/delayed_events/{delay_id}")

let current_update_route =
  Route.v (base_path ^ "/delayed_events/{delay_id}/{action}")

type delay_id = string

let delay_id_of_string s = s
let delay_id_to_string s = s

type delay_response = { delay_id : delay_id }

let delay_response_jsont =
  Jsont.Object.(
    map ~kind:"delayed_event_response" (fun delay_id -> { delay_id })
    |> mem "delay_id" Matrix_proto.Json.Codec.string ~enc:(fun t -> t.delay_id)
    |> finish)

let put_delayed client ~path ~content ~delay_ms =
  let query = [ (delay_query, string_of_int delay_ms) ] in
  let* body = Client.Http.encode_body Matrix_proto.Json.Codec.json content in
  let* body = Client.Http.put client ~path ~query ~body () in
  let+ resp = Client.Http.decode_response delay_response_jsont body in
  Log.debug (fun m -> m "Scheduled delayed event %s" resp.delay_id);
  resp.delay_id

let send client ~room_id ~event_type ~content ~delay_ms ?txn_id () =
  let txn_id =
    match txn_id with
    | Some t -> t
    | None -> Random.txn_id (Client.random client)
  in
  let path =
    Route.expand_exn send_route
      [
        ("room_id", Matrix_proto.Id.Room_id.to_string room_id);
        ("event_type", event_type);
        ("transaction_id", txn_id);
      ]
  in
  put_delayed client ~path ~content ~delay_ms

let send_state client ~room_id ~event_type ~state_key ~content ~delay_ms =
  let path =
    Route.expand_exn state_route
      [
        ("room_id", Matrix_proto.Id.Room_id.to_string room_id);
        ("event_type", event_type);
        ("state_key", state_key);
      ]
  in
  put_delayed client ~path ~content ~delay_ms

type current_send_request = {
  delay : int;
  state_key : string option;
  content : Jsont.json;
}

let current_send_request_jsont =
  Jsont.Object.(
    map ~kind:"current_delayed_event_request" (fun delay state_key content ->
        { delay; state_key; content })
    |> mem "delay" Matrix_proto.Json.Codec.int ~enc:(fun t -> t.delay)
    |> opt_mem "state_key" Matrix_proto.Json.Codec.string ~enc:(fun t ->
        t.state_key)
    |> mem "content" Matrix_proto.Json.Codec.json ~enc:(fun t -> t.content)
    |> finish)

let validate_nonnegative name value =
  if value < 0 then
    invalid_arg ("Matrix_client.Delayed_events: negative " ^ name)

let validate_sticky_duration value =
  validate_nonnegative "sticky_duration_ms" value;
  if value > 3_600_000 then
    invalid_arg
      "Matrix_client.Delayed_events: sticky_duration_ms exceeds one hour"

let put_current client ~room_id ~event_type ~txn_id ~state_key ~content
    ~delay_ms ?sticky_duration_ms () =
  validate_nonnegative "delay_ms" delay_ms;
  Option.iter validate_sticky_duration sticky_duration_ms;
  let path =
    Route.expand_exn current_send_route
      [
        ("room_id", Matrix_proto.Id.Room_id.to_string room_id);
        ("event_type", event_type);
        ("transaction_id", txn_id);
      ]
  in
  let query =
    match sticky_duration_ms with
    | None -> []
    | Some duration ->
        [ ("org.matrix.msc4354.sticky_duration_ms", string_of_int duration) ]
  in
  let request = { delay = delay_ms; state_key; content } in
  let* body = Client.Http.encode_body current_send_request_jsont request in
  let* body = Client.Http.put_absolute client ~path ~query ~body () in
  let+ response = Client.Http.decode_response delay_response_jsont body in
  Log.debug (fun m -> m "Scheduled current delayed event %s" response.delay_id);
  response.delay_id

let send_current client ~room_id ~event_type ~content ~delay_ms ?txn_id
    ?sticky_duration_ms ?state_key () =
  validate_nonnegative "delay_ms" delay_ms;
  Option.iter validate_sticky_duration sticky_duration_ms;
  let txn_id =
    match txn_id with
    | Some t -> t
    | None -> Random.txn_id (Client.random client)
  in
  put_current client ~room_id ~event_type ~txn_id ~state_key ~content ~delay_ms
    ?sticky_duration_ms ()

let send_state_current client ~room_id ~event_type ~state_key ~content ~delay_ms
    ?txn_id ?sticky_duration_ms () =
  send_current client ~room_id ~event_type ~state_key ~content ~delay_ms ?txn_id
    ?sticky_duration_ms ()

type action = Send | Cancel | Restart

let action_to_string = function
  | Send -> "send"
  | Cancel -> "cancel"
  | Restart -> "restart"

type update_request = { action : action }

let update_request_jsont =
  Jsont.Object.(
    map ~kind:"delayed_event_update" (fun action -> { action })
    |> mem "action"
         (Jsont.of_of_string ~kind:"delayed_event_action" ~enc:action_to_string
            (function
           | "send" -> Ok Send
           | "cancel" -> Ok Cancel
           | "restart" -> Ok Restart
           | s -> Error (Printf.sprintf "unknown delayed event action %S" s)))
         ~enc:(fun t -> t.action)
    |> finish)

let update client ~delay_id ~action =
  let path = Route.expand_exn update_route [ ("delay_id", delay_id) ] in
  let* body = Client.Http.encode_body update_request_jsont { action } in
  (* Outside [/_matrix/client/v3], so the absolute-path helper is used. *)
  let+ _ =
    Client.Http.post_bytes client ~path ~content_type:"application/json" ~body
      ()
  in
  Log.debug (fun m ->
      m "Delayed event %s: %s" delay_id (action_to_string action))

let send_now client ~delay_id = update client ~delay_id ~action:Send
let cancel client ~delay_id = update client ~delay_id ~action:Cancel
let restart client ~delay_id = update client ~delay_id ~action:Restart

let update_current client ~delay_id ~action =
  let path =
    Route.expand_exn current_update_route
      [ ("delay_id", delay_id); ("action", action_to_string action) ]
  in
  let+ _ =
    Client.Http.post_absolute_unauthenticated client ~path ~body:"{}" ()
  in
  Log.debug (fun m ->
      m "Current delayed event %s: %s" delay_id (action_to_string action))

let send_now_current client ~delay_id =
  update_current client ~delay_id ~action:Send

let cancel_current client ~delay_id =
  update_current client ~delay_id ~action:Cancel

let restart_current client ~delay_id =
  update_current client ~delay_id ~action:Restart

type delayed_event = {
  delay_id : delay_id;
  room_id : Matrix_proto.Id.Room_id.t;
  event_type : string;
  state_key : string option;
  content : Jsont.json;
  delay : int;
  running_since : Matrix_proto.Event.Timestamp.t;
  event_id : Matrix_proto.Id.Event_id.t option;
  finalised_ts : Matrix_proto.Event.Timestamp.t option;
  error : Error.matrix_error option;
}

let delayed_event_jsont =
  Jsont.Object.(
    map ~kind:"delayed_event"
      (fun
        delay_id
        room_id
        event_type
        state_key
        content
        delay
        running_since
        event_id
        finalised_ts
        error
      ->
        {
          delay_id;
          room_id;
          event_type;
          state_key;
          content;
          delay;
          running_since;
          event_id;
          finalised_ts;
          error;
        })
    |> mem "delay_id" Matrix_proto.Json.Codec.string ~enc:(fun t -> t.delay_id)
    |> mem "room_id" Matrix_proto.Id.Room_id.jsont ~enc:(fun t -> t.room_id)
    |> mem "type" Matrix_proto.Json.Codec.string ~enc:(fun t -> t.event_type)
    |> opt_mem "state_key" Matrix_proto.Json.Codec.string ~enc:(fun t ->
        t.state_key)
    |> mem "content" Matrix_proto.Json.Codec.json ~enc:(fun t -> t.content)
    |> mem "delay" Matrix_proto.Json.Codec.uint ~enc:(fun t -> t.delay)
    |> mem "running_since" Matrix_proto.Event.Timestamp.jsont ~enc:(fun t ->
        t.running_since)
    |> opt_mem "event_id" Matrix_proto.Id.Event_id.jsont ~enc:(fun t ->
        t.event_id)
    |> opt_mem "finalised_ts" Matrix_proto.Event.Timestamp.jsont ~enc:(fun t ->
        t.finalised_ts)
    |> opt_mem "error" Error.matrix_error_jsont ~enc:(fun t -> t.error)
    |> finish)

(* The original paginated endpoint shipped responses with these members absent
   on some homeservers. Keep that compatibility at the legacy API boundary;
   the current get/list endpoints above remain strict. *)
let legacy_delayed_event_jsont =
  let empty_object = Jsont.Json.object' [] in
  Jsont.Object.(
    map ~kind:"legacy_delayed_event"
      (fun
        delay_id
        room_id
        event_type
        state_key
        content
        delay
        running_since
        event_id
        finalised_ts
        error
      ->
        {
          delay_id;
          room_id;
          event_type;
          state_key;
          content;
          delay;
          running_since;
          event_id;
          finalised_ts;
          error;
        })
    |> mem "delay_id" Matrix_proto.Json.Codec.string ~enc:(fun t -> t.delay_id)
    |> mem "room_id" Matrix_proto.Id.Room_id.jsont ~enc:(fun t -> t.room_id)
    |> mem "type" Matrix_proto.Json.Codec.string ~enc:(fun t -> t.event_type)
    |> opt_mem "state_key" Matrix_proto.Json.Codec.string ~enc:(fun t ->
        t.state_key)
    |> mem "content" Matrix_proto.Json.Codec.json
         ~dec_absent:(fun () -> empty_object)
         ~enc:(fun t -> t.content)
    |> mem "delay" Matrix_proto.Json.Codec.int
         ~dec_absent:(fun () -> 0)
         ~enc:(fun t -> t.delay)
    |> mem "running_since" Matrix_proto.Event.Timestamp.jsont
         ~dec_absent:(fun () -> Matrix_proto.Event.Timestamp.of_ms 0L)
         ~enc:(fun t -> t.running_since)
    |> opt_mem "event_id" Matrix_proto.Id.Event_id.jsont ~enc:(fun t ->
        t.event_id)
    |> opt_mem "finalised_ts" Matrix_proto.Event.Timestamp.jsont ~enc:(fun t ->
        t.finalised_ts)
    |> opt_mem "error" Error.matrix_error_jsont ~enc:(fun t -> t.error)
    |> finish)

let delayed_events_jsont =
  Matrix_proto.Common.Page.jsont ~chunk:"delayed_events"
    legacy_delayed_event_jsont

let current_delayed_events_jsont =
  Jsont.Object.(
    map ~kind:"current_delayed_events" Fun.id
    |> mem "delayed_events" (Jsont.list delayed_event_jsont) ~enc:Fun.id
    |> finish)

let list client ?from () =
  let path = Route.expand_exn list_route [] in
  let query = match from with None -> [] | Some f -> [ ("from", f) ] in
  let* body, _content_type = Client.Http.get_bytes client ~path ~query () in
  Client.Http.decode_response delayed_events_jsont body

let get_current client ~delay_id =
  let path = Route.expand_exn current_get_route [ ("delay_id", delay_id) ] in
  let* body, _content_type = Client.Http.get_bytes client ~path () in
  Client.Http.decode_response delayed_event_jsont body

let list_current client () =
  let path = Route.expand_exn list_route [] in
  let* body, _content_type = Client.Http.get_bytes client ~path () in
  let+ response =
    Client.Http.decode_response current_delayed_events_jsont body
  in
  response

type status = Scheduled | Sent | Failed | Cancelled

let status event =
  match (event.finalised_ts, event.event_id, event.error) with
  | None, _, _ -> Scheduled
  | Some _, Some _, _ -> Sent
  | Some _, None, Some _ -> Failed
  | Some _, None, None -> Cancelled
