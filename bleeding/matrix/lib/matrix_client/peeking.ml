open Result.Syntax
module Id = Matrix_proto.Id
module Event = Matrix_proto.Event

type membership = Invite | Join | Leave | Ban | Knock

let membership_jsont =
  Jsont.enum ~kind:"peeking membership"
    [
      ("invite", Invite);
      ("join", Join);
      ("leave", Leave);
      ("ban", Ban);
      ("knock", Knock);
    ]

type visibility = Private | Public

let visibility_jsont =
  Jsont.enum ~kind:"peeking visibility"
    [ ("private", Private); ("public", Public) ]

type account_data_event = { type_ : Event.Event_type.t; content : Jsont.json }

let account_data_event_jsont =
  Jsont.Object.(
    map ~kind:"peeking account-data event" (fun type_ content ->
        { type_; content })
    |> mem "type" Event.Event_type.jsont ~enc:(fun t -> t.type_)
    |> mem "content" Matrix_proto.Json.Codec.json ~enc:(fun t -> t.content)
    |> finish)

(* Unlike room timelines, the legacy [ClientEvent] schema always carries both
   identifiers. [Raw_event.jsont] deliberately permits their absence for
   path-scoped room responses and local echoes, so assert the narrower shape at
   this endpoint boundary. *)
let client_event_jsont =
  let validate (event : Event.Raw_event.t) =
    if Option.is_none event.event_id then
      Jsont.Error.msg Jsont.Meta.none "client event has no event_id";
    if Option.is_none event.room_id then
      Jsont.Error.msg Jsont.Meta.none "client event has no room_id"
  in
  Jsont.iter ~kind:"peeking client event" ~dec:validate ~enc:validate
    Event.Raw_event.jsont

type message_page = {
  chunk : Event.Raw_event.t list;
  start : string option;
  end_ : string;
}

let message_page_jsont =
  Jsont.Object.(
    map ~kind:"peeking message page" (fun chunk start end_ ->
        { chunk; start; end_ })
    |> mem "chunk" (Jsont.list client_event_jsont) ~enc:(fun t -> t.chunk)
    |> opt_mem "start" Matrix_proto.Json.Codec.string ~enc:(fun t -> t.start)
    |> mem "end" Matrix_proto.Json.Codec.string ~enc:(fun t -> t.end_)
    |> finish)

type initial_sync_response = {
  room_id : Id.Room_id.t;
  membership : membership option;
  visibility : visibility option;
  account_data : account_data_event list;
  messages : message_page option;
  state : Event.Raw_event.t list;
}

let initial_sync_response_jsont =
  Jsont.Object.(
    map ~kind:"peeking initial sync response"
      (fun room_id membership visibility account_data messages state ->
        { room_id; membership; visibility; account_data; messages; state })
    |> mem "room_id" Id.Room_id.jsont ~enc:(fun t -> t.room_id)
    |> opt_mem "membership" membership_jsont ~enc:(fun t -> t.membership)
    |> opt_mem "visibility" visibility_jsont ~enc:(fun t -> t.visibility)
    |> mem "account_data"
         (Jsont.list account_data_event_jsont)
         ~dec_absent:(fun () -> [])
         ~enc_omit:(fun value -> (( = ) []) value)
         ~enc:(fun t -> t.account_data)
    |> opt_mem "messages" message_page_jsont ~enc:(fun t -> t.messages)
    |> mem "state"
         (Jsont.list client_event_jsont)
         ~dec_absent:(fun () -> [])
         ~enc_omit:(fun value -> (( = ) []) value)
         ~enc:(fun t -> t.state)
    |> finish)

type events_response = {
  chunk : Jsont.json list;
  start : string option;
  end_ : string option;
}

let events_response_jsont =
  Jsont.Object.(
    map ~kind:"peeking events response" (fun chunk start end_ ->
        { chunk; start; end_ })
    |> mem "chunk"
         (Jsont.list Matrix_proto.Json.Codec.json)
         ~dec_absent:(fun () -> [])
         ~enc_omit:(fun value -> (( = ) []) value)
         ~enc:(fun t -> t.chunk)
    |> opt_mem "start" Matrix_proto.Json.Codec.string ~enc:(fun t -> t.start)
    |> opt_mem "end" Matrix_proto.Json.Codec.string ~enc:(fun t -> t.end_)
    |> finish)

let initial_sync_route = Route.v "/rooms/{room_id}/initialSync"
let events_route = Route.v "/events"

let initial_sync client ~room_id =
  let path =
    Route.expand_exn initial_sync_route
      [ ("room_id", Id.Room_id.to_string room_id) ]
  in
  let* body = Client.Http.get client ~path () in
  Client.Http.decode_response initial_sync_response_jsont body

let event_query ?room_id ?from ?timeout () =
  (match timeout with
  | Some timeout when timeout < 0 ->
      invalid_arg "Matrix_client.Peeking: timeout must be non-negative"
  | None | Some _ -> ());
  let required =
    match room_id with
    | None -> []
    | Some room_id -> [ ("room_id", Id.Room_id.to_string room_id) ]
  in
  let optional =
    (match from with None -> [] | Some token -> [ ("from", token) ])
    @
    match timeout with
    | None -> []
    | Some timeout -> [ ("timeout", string_of_int timeout) ]
  in
  required @ optional

let events client ?from ?timeout () =
  let query = event_query ?from ?timeout () in
  let* body =
    Client.Http.get client ~path:(Route.expand_exn events_route []) ~query ()
  in
  Client.Http.decode_response events_response_jsont body

let peek_events client ~room_id ?from ?timeout () =
  let query = event_query ~room_id ?from ?timeout () in
  let* body =
    Client.Http.get client ~path:(Route.expand_exn events_route []) ~query ()
  in
  Client.Http.decode_response events_response_jsont body
