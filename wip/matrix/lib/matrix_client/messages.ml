open Result.Syntax
module Id = Matrix_proto.Id
module Event = Matrix_proto.Event
module Page = Matrix_proto.Common.Page

type send_response = { event_id : Id.Event_id.t }

let send_response_jsont =
  Jsont.Object.(
    map ~kind:"send_response" (fun event_id -> { event_id })
    |> mem "event_id" Id.Event_id.jsont ~enc:(fun t -> t.event_id)
    |> finish)

let send_route = Route.v "/rooms/{room_id}/send/{event_type}/{transaction_id}"
let redact_route = Route.v "/rooms/{room_id}/redact/{event_id}/{transaction_id}"
let messages_route = Route.v "/rooms/{room_id}/messages"
let event_route = Route.v "/rooms/{room_id}/event/{event_id}"
let context_route = Route.v "/rooms/{room_id}/context/{event_id}"

let send_event client ~room_id ~event_type ~content =
  let path =
    Route.expand_exn send_route
      [
        ("room_id", Id.Room_id.to_string room_id);
        ("event_type", Event.Event_type.to_string event_type);
        ("transaction_id", Random.txn_id (Client.random client));
      ]
  in
  let* body = Client.Http.encode_body Matrix_proto.Json.Codec.json content in
  let* body = Client.Http.put client ~path ~body () in
  let+ resp = Client.Http.decode_response send_response_jsont body in
  resp.event_id

let send_message client ~room_id content =
  send_event client ~room_id ~event_type:Event.Event_type.Room_message ~content

let text_content ~msgtype ~body ?format ?formatted_body ?extra_content () =
  let members =
    [ ("msgtype", Jsont.Json.string msgtype); ("body", Jsont.Json.string body) ]
    @ (match format with
      | Some f -> [ ("format", Jsont.Json.string f) ]
      | None -> [])
    @
    match formatted_body with
    | Some f -> [ ("formatted_body", Jsont.Json.string f) ]
    | None -> []
  in
  Json_codec.merge_extra_content (Json_codec.obj members) ?extra_content ()

let send_text client ~room_id ~body ?format ?formatted_body ?extra_content () =
  send_message client ~room_id
    (text_content ~msgtype:"m.text" ~body ?format ?formatted_body ?extra_content
       ())

let send_emote ?extra_content client ~room_id ~body =
  send_message client ~room_id
    (text_content ~msgtype:"m.emote" ~body ?extra_content ())

let send_notice ?extra_content client ~room_id ~body =
  send_message client ~room_id
    (text_content ~msgtype:"m.notice" ~body ?extra_content ())

let media_content ~msgtype ~body ~url ~info ~extra_content =
  let base =
    [
      ("msgtype", Jsont.Json.string msgtype);
      ("body", Jsont.Json.string body);
      ("url", Jsont.Json.string (Media.Mxc.to_string url));
    ]
  in
  match info with
  | None ->
      Ok
        (Json_codec.merge_extra_content (Json_codec.obj base) ~extra_content ())
  | Some info -> (
      match Jsont.Json.encode Event.Media_info.jsont info with
      | Error e -> Error (Error.Json_error e)
      | Ok json ->
          Ok
            (Json_codec.merge_extra_content
               (Json_codec.obj (base @ [ ("info", json) ]))
               ~extra_content ()))

let send_media client ~room_id ~msgtype ~body ~url ~info ~extra_content =
  let* content = media_content ~msgtype ~body ~url ~info ~extra_content in
  send_message client ~room_id content

let send_image client ~room_id ~body ~url ?info ?extra_content () =
  send_media client ~room_id ~msgtype:"m.image" ~body ~url ~info
    ~extra_content:(Option.value extra_content ~default:(Jsont.Json.object' []))

let send_file client ~room_id ~body ~url ?info ?extra_content () =
  send_media client ~room_id ~msgtype:"m.file" ~body ~url ~info
    ~extra_content:(Option.value extra_content ~default:(Jsont.Json.object' []))

let redact_request_jsont =
  Jsont.Object.(
    map ~kind:"redact" Fun.id
    |> opt_mem "reason" Matrix_proto.Json.Codec.string ~enc:Fun.id
    |> finish)

let redact client ~room_id ~event_id ?reason () =
  let path =
    Route.expand_exn redact_route
      [
        ("room_id", Id.Room_id.to_string room_id);
        ("event_id", Id.Event_id.to_string event_id);
        ("transaction_id", Random.txn_id (Client.random client));
      ]
  in
  let* body = Client.Http.encode_body redact_request_jsont reason in
  let* body = Client.Http.put client ~path ~body () in
  let+ resp = Client.Http.decode_response send_response_jsont body in
  resp.event_id

type messages_response = {
  page : Event.Raw_event.t Page.t;
  state : Event.Raw_event.t list;
}

(* [/messages] spells its tokens [start] and [end]: [start] is where the page
   began and [end] continues in the direction the request walked. *)
let messages_response_jsont =
  Jsont.Object.(
    map ~kind:"messages" (fun chunk prev_batch next_batch state ->
        { page = { Page.chunk; next_batch; prev_batch }; state })
    |> mem "chunk"
         (Jsont.list Event.Raw_event.jsont)
         ~dec_absent:(fun () -> [])
         ~enc:(fun t -> t.page.Page.chunk)
    |> opt_mem "start" Matrix_proto.Json.Codec.string ~enc:(fun t ->
        t.page.Page.prev_batch)
    |> opt_mem "end" Matrix_proto.Json.Codec.string ~enc:(fun t ->
        t.page.Page.next_batch)
    |> mem "state"
         (Jsont.list Event.Raw_event.jsont)
         ~dec_absent:(fun () -> [])
         ~enc:(fun t -> t.state)
    |> finish)

let get_messages client ~room_id ?from
    ?(dir = Matrix_proto.Common.Direction.Backward) ?limit ?filter () =
  let path =
    Route.expand_exn messages_route
      [ ("room_id", Id.Room_id.to_string room_id) ]
  in
  let query =
    ("dir", Matrix_proto.Common.Direction.to_string dir)
    :: List.filter_map Fun.id
         [
           Option.map (fun f -> ("from", f)) from;
           Option.map (fun l -> ("limit", string_of_int l)) limit;
           Option.map (fun f -> ("filter", f)) filter;
         ]
  in
  let* body = Client.Http.get client ~path ~query () in
  Client.Http.decode_response messages_response_jsont body

let get_event client ~room_id ~event_id =
  let path =
    Route.expand_exn event_route
      [
        ("room_id", Id.Room_id.to_string room_id);
        ("event_id", Id.Event_id.to_string event_id);
      ]
  in
  let* body = Client.Http.get client ~path () in
  Client.Http.decode_response Event.Raw_event.jsont body

type context = {
  event : Event.Raw_event.t;
  events_before : Event.Raw_event.t list;
  events_after : Event.Raw_event.t list;
  prev_batch : string option;
  next_batch : string option;
  state : Event.Raw_event.t list;
}

let context_jsont =
  Jsont.Object.(
    map ~kind:"context"
      (fun event events_before events_after prev_batch next_batch state ->
        { event; events_before; events_after; prev_batch; next_batch; state })
    |> mem "event" Event.Raw_event.jsont ~enc:(fun t -> t.event)
    |> mem "events_before"
         (Jsont.list Event.Raw_event.jsont)
         ~dec_absent:(fun () -> [])
         ~enc:(fun t -> t.events_before)
    |> mem "events_after"
         (Jsont.list Event.Raw_event.jsont)
         ~dec_absent:(fun () -> [])
         ~enc:(fun t -> t.events_after)
    |> mem "start"
         (Jsont.option Matrix_proto.Json.Codec.string)
         ~dec_absent:(fun () -> None)
         ~enc:(fun t -> t.prev_batch)
    |> mem "end"
         (Jsont.option Matrix_proto.Json.Codec.string)
         ~dec_absent:(fun () -> None)
         ~enc:(fun t -> t.next_batch)
    |> mem "state"
         (Jsont.list Event.Raw_event.jsont)
         ~dec_absent:(fun () -> [])
         ~enc:(fun t -> t.state)
    |> finish)

let get_context client ~room_id ~event_id ?limit () =
  let path =
    Route.expand_exn context_route
      [
        ("room_id", Id.Room_id.to_string room_id);
        ("event_id", Id.Event_id.to_string event_id);
      ]
  in
  let query =
    match limit with
    | Some l -> Some [ ("limit", string_of_int l) ]
    | None -> None
  in
  let* body = Client.Http.get client ~path ?query () in
  Client.Http.decode_response context_jsont body
