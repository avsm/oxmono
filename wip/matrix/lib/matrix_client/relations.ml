open Result.Syntax
module Id = Matrix_proto.Id
module Event = Matrix_proto.Event
module Page = Matrix_proto.Common.Page

let jstring = Jsont.Json.string

let jobject mems =
  Jsont.Json.object'
    (List.map (fun (n, v) -> Jsont.Json.mem (Jsont.Json.name n) v) mems)

let html_format = "org.matrix.custom.html"

type reaction_content = { relates_to : reaction_relates_to } [@@warning "-69"]

and reaction_relates_to = { rel_type : string; event_id : string; key : string }
[@@warning "-69"]

let reaction_relates_to_jsont =
  Jsont.Object.(
    map (fun rel_type event_id key -> { rel_type; event_id; key })
    |> mem "rel_type" Matrix_proto.Json.Codec.string
         ~enc:(fun (t : reaction_relates_to) -> t.rel_type)
    |> mem "event_id" Matrix_proto.Json.Codec.string
         ~enc:(fun (t : reaction_relates_to) -> t.event_id)
    |> mem "key" Matrix_proto.Json.Codec.string
         ~enc:(fun (t : reaction_relates_to) -> t.key)
    |> finish)

let reaction_content_jsont =
  Jsont.Object.(
    map (fun relates_to -> { relates_to })
    |> mem "m.relates_to" reaction_relates_to_jsont
         ~enc:(fun (t : reaction_content) -> t.relates_to)
    |> finish)

(* These sends use [POST /rooms/{roomId}/send/{eventType}], which lets the
   server pick the transaction ID, rather than the idempotent PUT. *)
let post_route = Route.v "/rooms/{room_id}/send/{event_type}"

let post_content client ~room_id ~event_type jsont content =
  let path =
    Route.expand_exn post_route
      [ ("room_id", Id.Room_id.to_string room_id); ("event_type", event_type) ]
  in
  let* body = Client.Http.encode_body jsont content in
  let* body = Client.Http.post client ~path ~body () in
  let+ resp = Client.Http.decode_response Messages.send_response_jsont body in
  resp.event_id

let send_reaction ?extra_content client ~room_id ~event_id ~key =
  let content =
    Json_codec.merge_extra_content
      (jobject
         [
           ( "m.relates_to",
             jobject
               [
                 ("rel_type", jstring "m.annotation");
                 ("event_id", jstring (Id.Event_id.to_string event_id));
                 ("key", jstring key);
               ] );
         ])
      ?extra_content ()
  in
  post_content client ~room_id ~event_type:"m.reaction"
    Matrix_proto.Json.Codec.json content

(* The formatted half of an [m.room.message]: both members are present or
   both absent, since a [format] without a body names nothing. *)
type formatted = { format : string; formatted_body : string }

let formatted ?formatted_body ?(format = html_format) () =
  Option.map (fun formatted_body -> { format; formatted_body }) formatted_body

type edit_new_content = {
  msgtype : string;
  body : string;
  new_formatted : formatted option;
}
[@@warning "-69"]

and edit_relates_to = { rel_type : string; event_id : string } [@@warning "-69"]

type edit_content = {
  msgtype : string;
  body : string;
  outer_formatted : formatted option;
  new_content : edit_new_content;
  relates_to : edit_relates_to;
}
[@@warning "-69"]

let format_mem enc =
  Jsont.Object.opt_mem "format" Matrix_proto.Json.Codec.string ~enc

let formatted_body_mem enc =
  Jsont.Object.opt_mem "formatted_body" Matrix_proto.Json.Codec.string ~enc

let format_of f = Option.map (fun f -> f.format) f
let formatted_body_of f = Option.map (fun f -> f.formatted_body) f

let of_parts format formatted_body =
  match (format, formatted_body) with
  | Some format, Some formatted_body -> Some { format; formatted_body }
  | _ -> None

let edit_new_content_jsont =
  Jsont.Object.(
    map (fun msgtype body format formatted_body ->
        { msgtype; body; new_formatted = of_parts format formatted_body })
    |> mem "msgtype" Matrix_proto.Json.Codec.string
         ~enc:(fun (t : edit_new_content) -> t.msgtype)
    |> mem "body" Matrix_proto.Json.Codec.string
         ~enc:(fun (t : edit_new_content) -> t.body)
    |> format_mem (fun (t : edit_new_content) -> format_of t.new_formatted)
    |> formatted_body_mem (fun (t : edit_new_content) ->
        formatted_body_of t.new_formatted)
    |> finish)

let edit_relates_to_jsont =
  Jsont.Object.(
    map (fun rel_type event_id -> { rel_type; event_id })
    |> mem "rel_type" Matrix_proto.Json.Codec.string
         ~enc:(fun (t : edit_relates_to) -> t.rel_type)
    |> mem "event_id" Matrix_proto.Json.Codec.string
         ~enc:(fun (t : edit_relates_to) -> t.event_id)
    |> finish)

let edit_content_jsont =
  Jsont.Object.(
    map (fun msgtype body format formatted_body new_content relates_to ->
        {
          msgtype;
          body;
          outer_formatted = of_parts format formatted_body;
          new_content;
          relates_to;
        })
    |> mem "msgtype" Matrix_proto.Json.Codec.string
         ~enc:(fun (t : edit_content) -> t.msgtype)
    |> mem "body" Matrix_proto.Json.Codec.string ~enc:(fun (t : edit_content) ->
        t.body)
    |> format_mem (fun (t : edit_content) -> format_of t.outer_formatted)
    |> formatted_body_mem (fun (t : edit_content) ->
        formatted_body_of t.outer_formatted)
    |> mem "m.new_content" edit_new_content_jsont
         ~enc:(fun (t : edit_content) -> t.new_content)
    |> mem "m.relates_to" edit_relates_to_jsont ~enc:(fun (t : edit_content) ->
        t.relates_to)
    |> finish)

let edit_message client ~room_id ~event_id ~new_body ?formatted_body ?format ()
    =
  let replacement = formatted ?formatted_body ?format () in
  let content =
    {
      msgtype = "m.text";
      (* The outer body is what a client that does not understand
         [m.replace] renders; the spec prescribes the "* " prefix. *)
      body = "* " ^ new_body;
      outer_formatted =
        Option.map
          (fun f -> { f with formatted_body = "* " ^ f.formatted_body })
          replacement;
      new_content =
        { msgtype = "m.text"; body = new_body; new_formatted = replacement };
      relates_to =
        { rel_type = "m.replace"; event_id = Id.Event_id.to_string event_id };
    }
  in
  post_content client ~room_id ~event_type:"m.room.message" edit_content_jsont
    content

type reply_relates_to = { in_reply_to : reply_in_reply_to } [@@warning "-69"]
and reply_in_reply_to = { event_id : string } [@@warning "-69"]

type reply_content = {
  msgtype : string;
  body : string;
  reply_formatted : formatted option;
  relates_to : reply_relates_to;
}
[@@warning "-69"]

let reply_in_reply_to_jsont =
  Jsont.Object.(
    map (fun event_id -> { event_id })
    |> mem "event_id" Matrix_proto.Json.Codec.string
         ~enc:(fun (t : reply_in_reply_to) -> t.event_id)
    |> finish)

let reply_relates_to_jsont =
  Jsont.Object.(
    map (fun in_reply_to -> { in_reply_to })
    |> mem "m.in_reply_to" reply_in_reply_to_jsont
         ~enc:(fun (t : reply_relates_to) -> t.in_reply_to)
    |> finish)

let reply_content_jsont =
  Jsont.Object.(
    map (fun msgtype body format formatted_body relates_to ->
        {
          msgtype;
          body;
          reply_formatted = of_parts format formatted_body;
          relates_to;
        })
    |> mem "msgtype" Matrix_proto.Json.Codec.string
         ~enc:(fun (t : reply_content) -> t.msgtype)
    |> mem "body" Matrix_proto.Json.Codec.string
         ~enc:(fun (t : reply_content) -> t.body)
    |> format_mem (fun (t : reply_content) -> format_of t.reply_formatted)
    |> formatted_body_mem (fun (t : reply_content) ->
        formatted_body_of t.reply_formatted)
    |> mem "m.relates_to" reply_relates_to_jsont
         ~enc:(fun (t : reply_content) -> t.relates_to)
    |> finish)

let send_reply client ~room_id ~event_id ~body ?formatted_body ?format () =
  let content =
    {
      msgtype = "m.text";
      body;
      reply_formatted = formatted ?formatted_body ?format ();
      relates_to =
        { in_reply_to = { event_id = Id.Event_id.to_string event_id } };
    }
  in
  post_content client ~room_id ~event_type:"m.room.message" reply_content_jsont
    content

type thread_relates_to = {
  rel_type : string;
  event_id : string;
  is_falling_back : bool;
  in_reply_to : reply_in_reply_to option;
}
[@@warning "-69"]

type thread_content = {
  msgtype : string;
  body : string;
  relates_to : thread_relates_to;
}
[@@warning "-69"]

let thread_relates_to_jsont =
  Jsont.Object.(
    map (fun rel_type event_id is_falling_back in_reply_to ->
        { rel_type; event_id; is_falling_back; in_reply_to })
    |> mem "rel_type" Matrix_proto.Json.Codec.string
         ~enc:(fun (t : thread_relates_to) -> t.rel_type)
    |> mem "event_id" Matrix_proto.Json.Codec.string
         ~enc:(fun (t : thread_relates_to) -> t.event_id)
    |> mem "is_falling_back" Jsont.bool
         ~dec_absent:(fun () -> true)
         ~enc:(fun (t : thread_relates_to) -> t.is_falling_back)
    |> opt_mem "m.in_reply_to" reply_in_reply_to_jsont
         ~enc:(fun (t : thread_relates_to) -> t.in_reply_to)
    |> finish)

let thread_content_jsont =
  Jsont.Object.(
    map (fun msgtype body relates_to -> { msgtype; body; relates_to })
    |> mem "msgtype" Matrix_proto.Json.Codec.string
         ~enc:(fun (t : thread_content) -> t.msgtype)
    |> mem "body" Matrix_proto.Json.Codec.string
         ~enc:(fun (t : thread_content) -> t.body)
    |> mem "m.relates_to" thread_relates_to_jsont
         ~enc:(fun (t : thread_content) -> t.relates_to)
    |> finish)

let send_in_thread client ~room_id ~thread_root_id ?reply_to_id ~body () =
  (* Without an explicit reply target, [in_reply_to] falls back to the
     thread root itself, so a client that does not understand threading
     still renders this as a reply rather than an orphaned message. *)
  let in_reply_to =
    Some
      {
        event_id =
          Id.Event_id.to_string
            (Option.value reply_to_id ~default:thread_root_id);
      }
  in
  let content =
    {
      msgtype = "m.text";
      body;
      relates_to =
        {
          rel_type = "m.thread";
          event_id = Id.Event_id.to_string thread_root_id;
          (* [is_falling_back] says the [m.in_reply_to] below is only the
             threadless-client fallback, not a reply the user asked for. *)
          is_falling_back = Option.is_none reply_to_id;
          in_reply_to;
        };
    }
  in
  post_content client ~room_id ~event_type:"m.room.message" thread_content_jsont
    content

type related_event = {
  event_id : Id.Event_id.t;
  origin_server_ts : Event.Timestamp.t;
  sender : Id.User_id.t;
  key : string option;
}

let key_of_content content =
  Option.bind
    (Option.bind
       (Matrix_proto.Json.find_mem "m.relates_to" content)
       (Matrix_proto.Json.find_mem "key"))
    Matrix_proto.Json.as_string

(* Only the annotation key is kept, so the round trip rebuilds the smallest
   content that carries it. *)
let content_of_key = function
  | None -> jobject []
  | Some key -> jobject [ ("m.relates_to", jobject [ ("key", jstring key) ]) ]

let related_event_jsont =
  Jsont.Object.(
    map (fun event_id origin_server_ts sender content ->
        { event_id; origin_server_ts; sender; key = key_of_content content })
    |> mem "event_id" Id.Event_id.jsont ~enc:(fun t -> t.event_id)
    |> mem "origin_server_ts" Event.Timestamp.jsont ~enc:(fun t ->
        t.origin_server_ts)
    |> mem "sender" Id.User_id.jsont ~enc:(fun t -> t.sender)
    |> mem "content" Matrix_proto.Json.Codec.json
         ~dec_absent:(fun () -> jobject [])
         ~enc:(fun t -> content_of_key t.key)
    |> finish)

let page_jsont = Page.jsont related_event_jsont

let relations_route =
  Route.v "/_matrix/client/v1/rooms/{room_id}/relations/{event_id}"

let relations_type_route =
  Route.v "/_matrix/client/v1/rooms/{room_id}/relations/{event_id}/{rel_type}"

let relations_event_type_route =
  Route.v
    "/_matrix/client/v1/rooms/{room_id}/relations/{event_id}/{rel_type}/{event_type}"

let relations_path ~room_id ~event_id ?rel_type ?event_type () =
  (* This endpoint lives under [/_matrix/client/v1], so it goes through the
     absolute-path helper rather than {!Client.Http.get}, which prefixes v3. The
     event type may only narrow a relation type, as the path shape shows. *)
  let common =
    [
      ("room_id", Id.Room_id.to_string room_id);
      ("event_id", Id.Event_id.to_string event_id);
    ]
  in
  match (rel_type, event_type) with
  | Some rel_type, Some event_type ->
      Route.expand_exn relations_event_type_route
        (common
        @ [
            ("rel_type", Event.Rel_type.to_string rel_type);
            ("event_type", Event.Event_type.to_string event_type);
          ])
  | Some rel_type, None ->
      Route.expand_exn relations_type_route
        (common @ [ ("rel_type", Event.Rel_type.to_string rel_type) ])
  | None, _ -> Route.expand_exn relations_route common

let relations_query ?limit ?from ?dir ?recurse () =
  List.concat
    [
      (match limit with
      | Some limit -> [ ("limit", string_of_int limit) ]
      | None -> []);
      (match from with Some token -> [ ("from", token) ] | None -> []);
      (match dir with
      | Some dir -> [ ("dir", Matrix_proto.Common.Direction.to_string dir) ]
      | None -> []);
      (match recurse with
      | Some recurse -> [ ("recurse", string_of_bool recurse) ]
      | None -> []);
    ]

let get_relations client ~room_id ~event_id ?rel_type ?event_type ?limit ?from
    ?dir ?recurse () =
  let path = relations_path ~room_id ~event_id ?rel_type ?event_type () in
  let query = relations_query ?limit ?from ?dir ?recurse () in
  let* body, _content_type = Client.Http.get_bytes client ~path ~query () in
  Client.Http.decode_response page_jsont body

let get_raw_relations client ~room_id ~event_id ?rel_type ?event_type ?limit
    ?from ?dir ?recurse () =
  let path = relations_path ~room_id ~event_id ?rel_type ?event_type () in
  let query = relations_query ?limit ?from ?dir ?recurse () in
  let* body, _content_type = Client.Http.get_bytes client ~path ~query () in
  Client.Http.decode_response (Page.jsont Event.Raw_event.jsont) body

let relation content =
  let open Matrix_proto.Json in
  Option.bind (find_mem "m.relates_to" content) (fun relates_to ->
      Option.bind (find_string "rel_type" relates_to) (fun rel_type ->
          Option.map
            (fun event_id -> (rel_type, event_id))
            (find_string "event_id" relates_to)))

let is_replacement content =
  match relation content with Some ("m.replace", _) -> true | _ -> false

let has_object_new_content content =
  Option.bind
    (Matrix_proto.Json.find_mem "m.new_content" content)
    Matrix_proto.Json.as_object
  |> Option.is_some

let is_redacted (event : Event.Raw_event.t) =
  Option.bind event.unsigned Event.Unsigned.redacted_because |> Option.is_some

let get_edit_revisions client ~room_id ~event_id () =
  let* original = Messages.get_event client ~room_id ~event_id in
  if
    (not (Event.Event_type.equal original.type_ Event.Event_type.Room_message))
    || Option.is_some original.state_key
    || is_replacement original.content
  then Ok []
  else
    let target = Id.Event_id.to_string event_id in
    let valid (event : Event.Raw_event.t) =
      Event.Event_type.equal event.type_ Event.Event_type.Room_message
      && Option.is_none event.state_key
      && Option.is_some event.event_id
      && Id.User_id.equal event.sender original.sender
      && Option.equal
           (fun (left_type, left_target) (right_type, right_target) ->
             String.equal left_type right_type
             && String.equal left_target right_target)
           (relation event.content)
           (Some ("m.replace", target))
      && has_object_new_content event.content
      && not (is_redacted event)
    in
    let seen_tokens = Hashtbl.create 8 in
    let seen_events = Hashtbl.create 32 in
    let rec pages from revisions =
      match from with
      | Some token when Hashtbl.mem seen_tokens token -> Ok revisions
      | _ -> (
          Option.iter (fun token -> Hashtbl.replace seen_tokens token ()) from;
          let* page =
            get_raw_relations client ~room_id ~event_id
              ~rel_type:Event.Rel_type.Replace
              ~event_type:Event.Event_type.Room_message ?from ()
          in
          let fresh =
            List.filter_map
              (fun (event : Event.Raw_event.t) ->
                match event.event_id with
                | Some id
                  when valid event
                       && not
                            (Hashtbl.mem seen_events (Id.Event_id.to_string id))
                  ->
                    Hashtbl.replace seen_events (Id.Event_id.to_string id) ();
                    Some event
                | Some _ | None -> None)
              page.chunk
          in
          let revisions = List.rev_append fresh revisions in
          match page.next_batch with
          | None -> Ok revisions
          | Some _ as next -> pages next revisions)
    in
    let* revisions = pages None [] in
    let compare_revision (left : Event.Raw_event.t) (right : Event.Raw_event.t)
        =
      let by_timestamp =
        Event.Timestamp.compare left.origin_server_ts right.origin_server_ts
      in
      if by_timestamp <> 0 then by_timestamp
      else
        String.compare
          (Option.fold ~none:"" ~some:Id.Event_id.to_string left.event_id)
          (Option.fold ~none:"" ~some:Id.Event_id.to_string right.event_id)
    in
    Ok (original :: List.sort compare_revision revisions)

let get_reactions client ~room_id ~event_id =
  get_relations client ~room_id ~event_id ~rel_type:Event.Rel_type.Annotation
    ~event_type:Event.Event_type.Reaction ()

type thread_filter = All | Participated

let thread_filter_to_string = function
  | All -> "all"
  | Participated -> "participated"

let threads_page_jsont = Page.jsont Event.Raw_event.jsont
let stable_threads_route = Route.v "/_matrix/client/v1/rooms/{room_id}/threads"

let unstable_threads_route =
  Route.v "/_matrix/client/unstable/org.matrix.msc3856/rooms/{room_id}/threads"

let list_threads client ~room_id ?filter ?from ?limit () =
  (* Ruma's endpoint history for MSC3856 is an unstable path followed by
     [/_matrix/client/v1] from Matrix 1.4.  Probe [/_matrix/client/versions]
     first so a server which advertises the stable API is never sent the
     unstable spelling. *)
  let* versions = Server.get_versions client in
  let route =
    if Server.supports_version_at_least versions ~major:1 ~minor:4 then
      stable_threads_route
    else unstable_threads_route
  in
  let path =
    Route.expand_exn route [ ("room_id", Id.Room_id.to_string room_id) ]
  in
  let query =
    List.concat
      [
        (match filter with
        | Some i -> [ ("include", thread_filter_to_string i) ]
        | None -> []);
        (match from with Some f -> [ ("from", f) ] | None -> []);
        (match limit with
        | Some l -> [ ("limit", string_of_int l) ]
        | None -> []);
      ]
  in
  let* body, _content_type = Client.Http.get_bytes client ~path ~query () in
  Client.Http.decode_response threads_page_jsont body
