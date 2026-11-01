let ( let* ) = Result.bind
let unit_result result = Result.map (Fun.const ()) result
let encode codec value = Codec.encode codec value
let int_id to_int id = string_of_int (to_int id)

let json_encode codec value =
  match Jsont.Json.encode' codec value with
  | Ok json -> json
  | Error error -> raise (Jsont.Error error)

type anchor =
  | Newest
  | Oldest
  | First_unread
  | Date of string
  | Message_id of Zulip.Id.Message.t

type propagate_mode = Change_one | Change_later | Change_all
type emoji_type = Unicode_emoji | Realm_emoji | Zulip_extra_emoji

type page = {
  messages : Zulip.Message.t list;
  anchor : Zulip.Id.Message.t option;
  found_oldest : bool;
  found_newest : bool;
  found_anchor : bool;
  history_limited : bool;
  raw : Jsont.json;
}

let page_jsont =
  Jsont.Object.map ~kind:"Zulip messages response"
    (fun
      messages
      anchor
      found_oldest
      found_newest
      found_anchor
      history_limited
      unknown
    ->
      let known =
        [
          ( ("messages", Jsont.Meta.none),
            Jsont.Json.list
              (List.map (json_encode Zulip.Message.jsont) messages) );
          (("found_oldest", Jsont.Meta.none), Jsont.Json.bool found_oldest);
          (("found_newest", Jsont.Meta.none), Jsont.Json.bool found_newest);
          (("found_anchor", Jsont.Meta.none), Jsont.Json.bool found_anchor);
          (("history_limited", Jsont.Meta.none), Jsont.Json.bool history_limited);
        ]
        @ Option.fold ~none:[]
            ~some:(fun id ->
              [
                ( ("anchor", Jsont.Meta.none),
                  Jsont.Json.int (Zulip.Id.Message.to_int id) );
              ])
            anchor
      in
      let unknown =
        match unknown with Jsont.Object (members, _) -> members | _ -> []
      in
      {
        messages;
        anchor;
        found_oldest;
        found_newest;
        found_anchor;
        history_limited;
        raw = Jsont.Object (known @ unknown, Jsont.Meta.none);
      })
  |> Jsont.Object.mem "messages" (Jsont.list Zulip.Message.jsont)
       ~enc:(fun (p : page) -> p.messages)
  |> Jsont.Object.opt_mem "anchor" Zulip.Id.Message.jsont
       ~enc:(fun (p : page) -> p.anchor)
  |> Jsont.Object.mem "found_oldest" Jsont.bool
       ~dec_absent:(fun () -> false)
       ~enc:(fun (p : page) -> p.found_oldest)
  |> Jsont.Object.mem "found_newest" Jsont.bool
       ~dec_absent:(fun () -> false)
       ~enc:(fun (p : page) -> p.found_newest)
  |> Jsont.Object.mem "found_anchor" Jsont.bool
       ~dec_absent:(fun () -> false)
       ~enc:(fun (p : page) -> p.found_anchor)
  |> Jsont.Object.mem "history_limited" Jsont.bool
       ~dec_absent:(fun () -> false)
       ~enc:(fun (p : page) -> p.history_limited)
  |> Jsont.Object.keep_unknown Jsont.json_mems ~enc:(fun (p : page) ->
      let known =
        [
          "messages";
          "anchor";
          "found_oldest";
          "found_newest";
          "found_anchor";
          "history_limited";
        ]
      in
      match p.raw with
      | Jsont.Object (members, meta) ->
          Jsont.Object
            ( List.filter
                (fun ((name, _), _) -> not (List.mem name known))
                members,
              meta )
      | _ -> Jsont.Json.object' [])
  |> Jsont.Object.finish

type narrow_match = {
  message_id : Zulip.Id.Message.t;
  match_content : string;
  match_topic : string;
}

type narrow_match_wire = { match_content : string; match_subject : string }

let narrow_match_wire_jsont =
  Jsont.Object.map ~kind:"Zulip narrow match"
    (fun match_content match_subject -> { match_content; match_subject })
  |> Jsont.Object.mem "match_content" Jsont.string
       ~enc:(fun (m : narrow_match_wire) -> m.match_content)
  |> Jsont.Object.mem "match_subject" Jsont.string
       ~enc:(fun (m : narrow_match_wire) -> m.match_subject)
  |> Jsont.Object.finish

let narrow_matches_jsont =
  let dec = function
    | Jsont.Object (members, _) ->
        List.map
          (fun ((id, meta), json) ->
            let message_id =
              match int_of_string_opt id with
              | Some id when id >= 0 -> Zulip.Id.Message.of_int id
              | _ -> Jsont.Error.msgf meta "invalid message ID key %S" id
            in
            match Jsont.Json.decode' narrow_match_wire_jsont json with
            | Ok matched ->
                {
                  message_id;
                  match_content = matched.match_content;
                  match_topic = matched.match_subject;
                }
            | Error error -> raise (Jsont.Error error))
          members
    | json -> Jsont.Json.error_sort ~exp:Jsont.Sort.Object json
  in
  let enc matches =
    let match_json (match_ : narrow_match) =
      Jsont.Json.object'
        [
          ( Jsont.Json.name "match_content",
            Jsont.Json.string match_.match_content );
          (Jsont.Json.name "match_subject", Jsont.Json.string match_.match_topic);
        ]
    in
    Jsont.Object
      ( List.map
          (fun (match_ : narrow_match) ->
            ( ( string_of_int (Zulip.Id.Message.to_int match_.message_id),
                Jsont.Meta.none ),
              match_json match_ ))
          matches,
        Jsont.Meta.none )
  in
  Jsont.map ~kind:"Zulip narrow matches" ~dec ~enc Jsont.json

type narrow_match_result = { matches : narrow_match list; raw : Jsont.json }

let narrow_match_response_jsont =
  Jsont.Object.map ~kind:"Zulip matching messages response"
    (fun matches unknown ->
      let known =
        [
          ( ("messages", Jsont.Meta.none),
            json_encode narrow_matches_jsont matches );
        ]
      in
      let unknown =
        match unknown with Jsont.Object (members, _) -> members | _ -> []
      in
      { matches; raw = Jsont.Object (known @ unknown, Jsont.Meta.none) })
  |> Jsont.Object.mem "messages" narrow_matches_jsont
       ~enc:(fun (r : narrow_match_result) -> r.matches)
  |> Jsont.Object.keep_unknown Jsont.json_mems
       ~enc:(fun (r : narrow_match_result) ->
         match r.raw with
         | Jsont.Object (members, meta) ->
             Jsont.Object
               ( List.filter (fun ((name, _), _) -> name <> "messages") members,
                 meta )
         | _ -> Jsont.Json.object' [])
  |> Jsont.Object.finish

type raw_content_result = { raw_content : string; raw : Jsont.json }

let raw_content_jsont =
  Jsont.Object.map ~kind:"Zulip raw message" (fun raw_content unknown ->
      let known =
        [ (("raw_content", Jsont.Meta.none), Jsont.Json.string raw_content) ]
      in
      let unknown =
        match unknown with Jsont.Object (members, _) -> members | _ -> []
      in
      { raw_content; raw = Jsont.Object (known @ unknown, Jsont.Meta.none) })
  |> Jsont.Object.mem "raw_content" Jsont.string
       ~enc:(fun (r : raw_content_result) -> r.raw_content)
  |> Jsont.Object.keep_unknown Jsont.json_mems
       ~enc:(fun (r : raw_content_result) ->
         match r.raw with
         | Jsont.Object (members, meta) ->
             Jsont.Object
               ( List.filter
                   (fun ((name, _), _) -> name <> "raw_content")
                   members,
                 meta )
         | _ -> Jsont.Json.object' [])
  |> Jsont.Object.finish

type render_result = { rendered : string; raw : Jsont.json }

let rendered_jsont =
  Jsont.Object.map ~kind:"Zulip rendered message" (fun rendered unknown ->
      let known =
        [ (("rendered", Jsont.Meta.none), Jsont.Json.string rendered) ]
      in
      let unknown =
        match unknown with Jsont.Object (members, _) -> members | _ -> []
      in
      { rendered; raw = Jsont.Object (known @ unknown, Jsont.Meta.none) })
  |> Jsont.Object.mem "rendered" Jsont.string ~enc:(fun (r : render_result) ->
      r.rendered)
  |> Jsont.Object.keep_unknown Jsont.json_mems ~enc:(fun (r : render_result) ->
      match r.raw with
      | Jsont.Object (members, meta) ->
          Jsont.Object
            ( List.filter (fun ((name, _), _) -> name <> "rendered") members,
              meta )
      | _ -> Jsont.Json.object' [])
  |> Jsont.Object.finish

let send_detailed client ~type_ ~to_ ?topic ?queue_id ?local_id
    ?(read_by_sender = true) ~content () =
  let type_string =
    match type_ with `Channel -> "stream" | `Direct -> "direct"
  in
  match (type_, topic) with
  | `Channel, None ->
      Error (Error.Invalid_request "channel messages require a topic")
  | `Direct, Some _ ->
      Error (Error.Invalid_request "direct messages cannot have a topic")
  | _, _ when Option.is_some queue_id <> Option.is_some local_id ->
      Error
        (Error.Invalid_request
           "queue_id and local_id must be provided together for local echo")
  | _ ->
      let params =
        [
          ("type", type_string);
          ("to", to_);
          ("content", content);
          ("read_by_sender", string_of_bool read_by_sender);
        ]
        @ Option.fold ~none:[] ~some:(fun v -> [ ("topic", v) ]) topic
        @ Option.fold ~none:[] ~some:(fun v -> [ ("queue_id", v) ]) queue_id
        @ Option.fold ~none:[] ~some:(fun v -> [ ("local_id", v) ]) local_id
      in
      Client.request_typed client ~method_:`POST ~path:"/api/v1/messages"
        ~params ~codec:Zulip.Message_response.jsont ()

let send client ~type_ ~to_ ?topic ?queue_id ?local_id ?read_by_sender ~content
    () =
  let* response =
    send_detailed client ~type_ ~to_ ?topic ?queue_id ?local_id ?read_by_sender
      ~content ()
  in
  Ok (Zulip.Message_response.id response)

let send_channel client ~channel ~topic ?queue_id ?local_id ?read_by_sender
    ~content () =
  send client ~type_:`Channel ~to_:channel ~topic ?queue_id ?local_id
    ?read_by_sender ~content ()

let send_channel_id client ~channel_id ~topic ?queue_id ?local_id
    ?read_by_sender ~content () =
  send client ~type_:`Channel
    ~to_:(string_of_int (Zulip.Id.Channel.to_int channel_id))
    ~topic ?queue_id ?local_id ?read_by_sender ~content ()

let send_direct client ~recipients ?queue_id ?local_id ?read_by_sender ~content
    () =
  let* to_ = encode (Jsont.list Zulip.Id.User.jsont) recipients in
  send client ~type_:`Direct ~to_ ?queue_id ?local_id ?read_by_sender ~content
    ()

let get_raw_detailed client ~message_id ?(apply_markdown = true)
    ?allow_empty_topic_name () =
  Client.request_typed client ~method_:`GET
    ~path:("/api/v1/messages/" ^ int_id Zulip.Id.Message.to_int message_id)
    ~params:
      ([ ("apply_markdown", string_of_bool apply_markdown) ]
      @ Option.fold ~none:[]
          ~some:(fun value ->
            [ ("allow_empty_topic_name", string_of_bool value) ])
          allow_empty_topic_name)
    ~codec:raw_content_jsont ()

let get_raw client ~message_id ?apply_markdown ?allow_empty_topic_name () =
  let* result =
    get_raw_detailed client ~message_id ?apply_markdown ?allow_empty_topic_name
      ()
  in
  Ok result.raw_content

let anchor_to_string = function
  | Newest -> "newest"
  | Oldest -> "oldest"
  | First_unread -> "first_unread"
  | Date _ -> "date"
  | Message_id id -> int_id Zulip.Id.Message.to_int id

let get_messages client ?anchor ?num_before ?num_after ?narrow ?include_anchor
    ?client_gravatar ?apply_markdown ?use_first_unread_anchor ?message_ids
    ?allow_empty_topic_name () =
  let* range =
    match message_ids with
    | Some _ ->
        if
          Option.is_some anchor || Option.is_some num_before
          || Option.is_some num_after
          || Option.is_some include_anchor
          || Option.is_some use_first_unread_anchor
        then
          Error
            (Error.Invalid_request
               "message_ids cannot be combined with range parameters")
        else Ok []
    | None -> (
        match (num_before, num_after) with
        | Some before, Some after when before >= 0 && after >= 0 ->
            if Option.is_some anchor && Option.is_some use_first_unread_anchor
            then
              Error
                (Error.Invalid_request
                   "anchor and use_first_unread_anchor are mutually exclusive")
            else
              let anchor =
                match (anchor, use_first_unread_anchor) with
                | None, None -> Some Newest
                | anchor, _ -> anchor
              in
              let position =
                match anchor with
                | None -> []
                | Some (Date date) ->
                    [ ("anchor", "date"); ("anchor_date", date) ]
                | Some value -> [ ("anchor", anchor_to_string value) ]
              in
              Ok
                (position
                @ [
                    ("num_before", string_of_int before);
                    ("num_after", string_of_int after);
                  ])
        | _ ->
            Error
              (Error.Invalid_request
                 "Range queries require nonnegative num_before and num_after"))
  in
  let* narrow =
    match narrow with
    | None -> Ok []
    | Some narrow ->
        let* value = encode Zulip.Narrow.list_jsont narrow in
        Ok [ ("narrow", value) ]
  in
  let* selected_ids =
    match message_ids with
    | None -> Ok []
    | Some ids ->
        let* value = encode (Jsont.list Zulip.Id.Message.jsont) ids in
        Ok [ ("message_ids", value) ]
  in
  let bool name =
    Option.fold ~none:[] ~some:(fun value -> [ (name, string_of_bool value) ])
  in
  let params =
    range @ narrow @ selected_ids
    @ bool "include_anchor" include_anchor
    @ bool "client_gravatar" client_gravatar
    @ bool "apply_markdown" apply_markdown
    @ bool "use_first_unread_anchor" use_first_unread_anchor
    @ bool "allow_empty_topic_name" allow_empty_topic_name
  in
  Client.request_typed client ~method_:`GET ~path:"/api/v1/messages" ~params
    ~codec:page_jsont ()

let get client ~message_id =
  let* page =
    get_messages client ~anchor:(Message_id message_id) ~num_before:0
      ~num_after:0
      ~narrow:[ Zulip.Narrow.id message_id ]
      ~include_anchor:true ()
  in
  match
    List.find_opt
      (fun (message : Zulip.Message.t) ->
        Zulip.Id.Message.equal message.Zulip.Message.id message_id)
      page.messages
  with
  | Some message -> Ok message
  | None ->
      Error
        (Error.Invalid_request
           (Printf.sprintf "message %d was not returned by Zulip"
              (Zulip.Id.Message.to_int message_id)))

let check_messages_match_narrow_detailed client ~message_ids ~narrow =
  let* ids = encode (Jsont.list Zulip.Id.Message.jsont) message_ids in
  let* narrow = encode Zulip.Narrow.list_jsont narrow in
  Client.request_typed client ~method_:`GET
    ~path:"/api/v1/messages/matches_narrow"
    ~params:[ ("msg_ids", ids); ("narrow", narrow) ]
    ~codec:narrow_match_response_jsont ()

let check_messages_match_narrow client ~message_ids ~narrow =
  let* result =
    check_messages_match_narrow_detailed client ~message_ids ~narrow
  in
  Ok result.matches

type history = { edits : Zulip.Message.edit list; raw : Jsont.json }

let history_jsont =
  Jsont.Object.map ~kind:"Zulip message history response" (fun edits unknown ->
      let known =
        [
          ( ("message_history", Jsont.Meta.none),
            Jsont.Json.list
              (List.map (json_encode Zulip.Message.edit_jsont) edits) );
        ]
      in
      let unknown =
        match unknown with Jsont.Object (members, _) -> members | _ -> []
      in
      { edits; raw = Jsont.Object (known @ unknown, Jsont.Meta.none) })
  |> Jsont.Object.mem "message_history" (Jsont.list Zulip.Message.edit_jsont)
       ~enc:(fun (h : history) -> h.edits)
  |> Jsont.Object.keep_unknown Jsont.json_mems ~enc:(fun (h : history) ->
      match h.raw with
      | Jsont.Object (members, meta) ->
          Jsont.Object
            ( List.filter
                (fun ((name, _), _) -> name <> "message_history")
                members,
              meta )
      | _ -> Jsont.Json.object' [])
  |> Jsont.Object.finish

let get_history client ~message_id ?allow_empty_topic_name () =
  Client.request_typed client ~method_:`GET
    ~path:
      ("/api/v1/messages/"
      ^ int_id Zulip.Id.Message.to_int message_id
      ^ "/history")
    ~params:
      (Option.fold ~none:[]
         ~some:(fun value ->
           [ ("allow_empty_topic_name", string_of_bool value) ])
         allow_empty_topic_name)
    ~codec:history_jsont ()

let propagate_mode_to_string = function
  | Change_one -> "change_one"
  | Change_later -> "change_later"
  | Change_all -> "change_all"

type edit_result = { detached_uploads : Attachments.t list; raw : Jsont.json }

let edit_result_jsont =
  Jsont.Object.map ~kind:"Zulip edit-message response"
    (fun detached_uploads unknown ->
      let known =
        [
          ( ("detached_uploads", Jsont.Meta.none),
            Jsont.Json.list
              (List.map (json_encode Attachments.jsont) detached_uploads) );
        ]
      in
      let unknown =
        match unknown with Jsont.Object (members, _) -> members | _ -> []
      in
      {
        detached_uploads;
        raw = Jsont.Object (known @ unknown, Jsont.Meta.none);
      })
  |> Jsont.Object.mem "detached_uploads"
       (Jsont.list Attachments.jsont)
       ~dec_absent:(fun () -> [])
       ~enc:(fun (r : edit_result) -> r.detached_uploads)
  |> Jsont.Object.keep_unknown Jsont.json_mems ~enc:(fun (r : edit_result) ->
      match r.raw with
      | Jsont.Object (members, meta) ->
          Jsont.Object
            ( List.filter
                (fun ((name, _), _) -> name <> "detached_uploads")
                members,
              meta )
      | _ -> Jsont.Json.object' [])
  |> Jsont.Object.finish

let edit_detailed client ~message_id ?content ?topic ?channel_id ?propagate_mode
    ?send_notification_to_old_thread ?send_notification_to_new_thread
    ?prev_content_sha256 () =
  if Option.is_none content && Option.is_none topic && Option.is_none channel_id
  then
    Error (Error.Invalid_request "edit requires content, topic, or channel_id")
  else
    let params =
      Option.fold ~none:[] ~some:(fun v -> [ ("content", v) ]) content
      @ Option.fold ~none:[] ~some:(fun v -> [ ("topic", v) ]) topic
      @ Option.fold ~none:[]
          ~some:(fun v -> [ ("stream_id", int_id Zulip.Id.Channel.to_int v) ])
          channel_id
      @ Option.fold ~none:[]
          ~some:(fun v -> [ ("propagate_mode", propagate_mode_to_string v) ])
          propagate_mode
      @ Option.fold ~none:[]
          ~some:(fun v ->
            [ ("send_notification_to_old_thread", string_of_bool v) ])
          send_notification_to_old_thread
      @ Option.fold ~none:[]
          ~some:(fun v ->
            [ ("send_notification_to_new_thread", string_of_bool v) ])
          send_notification_to_new_thread
      @ Option.fold ~none:[]
          ~some:(fun value -> [ ("prev_content_sha256", value) ])
          prev_content_sha256
    in
    Client.request_typed client ~method_:`PATCH
      ~path:("/api/v1/messages/" ^ int_id Zulip.Id.Message.to_int message_id)
      ~params ~codec:edit_result_jsont ()

let edit client ~message_id ?content ?topic ?channel_id ?propagate_mode
    ?send_notification_to_old_thread ?send_notification_to_new_thread
    ?prev_content_sha256 () =
  edit_detailed client ~message_id ?content ?topic ?channel_id ?propagate_mode
    ?send_notification_to_old_thread ?send_notification_to_new_thread
    ?prev_content_sha256 ()
  |> unit_result

let delete client ~message_id =
  unit_result
    (Client.request client ~method_:`DELETE
       ~path:("/api/v1/messages/" ^ int_id Zulip.Id.Message.to_int message_id)
       ())

type flag_result = {
  messages : Zulip.Id.Message.t list;
  ignored_because_not_subscribed_channels : Zulip.Id.Channel.t list;
  raw : Jsont.json;
}

let flag_result_jsont =
  Jsont.Object.map ~kind:"Zulip message flags response"
    (fun messages ignored unknown ->
      let known =
        [
          ( ("messages", Jsont.Meta.none),
            Jsont.Json.list
              (List.map
                 (fun id -> Jsont.Json.int (Zulip.Id.Message.to_int id))
                 messages) );
          ( ("ignored_because_not_subscribed_channels", Jsont.Meta.none),
            Jsont.Json.list
              (List.map
                 (fun id -> Jsont.Json.int (Zulip.Id.Channel.to_int id))
                 ignored) );
        ]
      in
      let unknown =
        match unknown with Jsont.Object (members, _) -> members | _ -> []
      in
      {
        messages;
        ignored_because_not_subscribed_channels = ignored;
        raw = Jsont.Object (known @ unknown, Jsont.Meta.none);
      })
  |> Jsont.Object.mem "messages"
       (Jsont.list Zulip.Id.Message.jsont)
       ~dec_absent:(fun () -> [])
       ~enc:(fun (r : flag_result) -> r.messages)
  |> Jsont.Object.mem "ignored_because_not_subscribed_channels"
       (Jsont.list Zulip.Id.Channel.jsont)
       ~dec_absent:(fun () -> [])
       ~enc:(fun (r : flag_result) -> r.ignored_because_not_subscribed_channels)
  |> Jsont.Object.keep_unknown Jsont.json_mems ~enc:(fun (r : flag_result) ->
      match r.raw with
      | Jsont.Object (members, meta) ->
          Jsont.Object
            ( List.filter
                (fun ((name, _), _) ->
                  name <> "messages"
                  && name <> "ignored_because_not_subscribed_channels")
                members,
              meta )
      | _ -> Jsont.Json.object' [])
  |> Jsont.Object.finish

let update_flags_detailed client ~messages ~op ~flag =
  let* messages = encode (Jsont.list Zulip.Id.Message.jsont) messages in
  Client.request_typed client ~method_:`POST ~path:"/api/v1/messages/flags"
    ~params:
      [
        ("messages", messages);
        ("op", Zulip.Message_flag.op_to_string op);
        ("flag", Zulip.Message_flag.to_string flag);
      ]
    ~codec:flag_result_jsont ()

let update_flags client ~messages ~op ~flag =
  update_flags_detailed client ~messages ~op ~flag |> unit_result

type narrow_flag_result = {
  processed_count : int;
  updated_count : int;
  first_processed_id : Zulip.Id.Message.t option;
  last_processed_id : Zulip.Id.Message.t option;
  found_oldest : bool;
  found_newest : bool;
  ignored_because_not_subscribed_channels : Zulip.Id.Channel.t list;
  raw : Jsont.json;
}

let narrow_flag_result_jsont =
  Jsont.Object.map ~kind:"Zulip narrow flags response"
    (fun
      processed_count
      updated_count
      first_processed_id
      last_processed_id
      found_oldest
      found_newest
      ignored
      unknown
    ->
      let optional_id name = function
        | None -> [ ((name, Jsont.Meta.none), Jsont.Json.null ()) ]
        | Some id ->
            [
              ( (name, Jsont.Meta.none),
                Jsont.Json.int (Zulip.Id.Message.to_int id) );
            ]
      in
      let known =
        [
          (("processed_count", Jsont.Meta.none), Jsont.Json.int processed_count);
          (("updated_count", Jsont.Meta.none), Jsont.Json.int updated_count);
          (("found_oldest", Jsont.Meta.none), Jsont.Json.bool found_oldest);
          (("found_newest", Jsont.Meta.none), Jsont.Json.bool found_newest);
          ( ("ignored_because_not_subscribed_channels", Jsont.Meta.none),
            Jsont.Json.list
              (List.map
                 (fun id -> Jsont.Json.int (Zulip.Id.Channel.to_int id))
                 ignored) );
        ]
        @ optional_id "first_processed_id" first_processed_id
        @ optional_id "last_processed_id" last_processed_id
      in
      let unknown =
        match unknown with Jsont.Object (members, _) -> members | _ -> []
      in
      {
        processed_count;
        updated_count;
        first_processed_id;
        last_processed_id;
        found_oldest;
        found_newest;
        ignored_because_not_subscribed_channels = ignored;
        raw = Jsont.Object (known @ unknown, Jsont.Meta.none);
      })
  |> Jsont.Object.mem "processed_count" Jsont.int
       ~enc:(fun (r : narrow_flag_result) -> r.processed_count)
  |> Jsont.Object.mem "updated_count" Jsont.int
       ~enc:(fun (r : narrow_flag_result) -> r.updated_count)
  |> Jsont.Object.mem "first_processed_id" (Jsont.option Zulip.Id.Message.jsont)
       ~enc:(fun (r : narrow_flag_result) -> r.first_processed_id)
  |> Jsont.Object.mem "last_processed_id" (Jsont.option Zulip.Id.Message.jsont)
       ~enc:(fun (r : narrow_flag_result) -> r.last_processed_id)
  |> Jsont.Object.mem "found_oldest" Jsont.bool
       ~enc:(fun (r : narrow_flag_result) -> r.found_oldest)
  |> Jsont.Object.mem "found_newest" Jsont.bool
       ~enc:(fun (r : narrow_flag_result) -> r.found_newest)
  |> Jsont.Object.mem "ignored_because_not_subscribed_channels"
       (Jsont.list Zulip.Id.Channel.jsont)
       ~dec_absent:(fun () -> [])
       ~enc:(fun (r : narrow_flag_result) ->
         r.ignored_because_not_subscribed_channels)
  |> Jsont.Object.keep_unknown Jsont.json_mems
       ~enc:(fun (r : narrow_flag_result) ->
         let known =
           [
             "processed_count";
             "updated_count";
             "first_processed_id";
             "last_processed_id";
             "found_oldest";
             "found_newest";
             "ignored_because_not_subscribed_channels";
           ]
         in
         match r.raw with
         | Jsont.Object (members, meta) ->
             Jsont.Object
               ( List.filter
                   (fun ((name, _), _) -> not (List.mem name known))
                   members,
                 meta )
         | _ -> Jsont.Json.object' [])
  |> Jsont.Object.finish

let update_flags_for_narrow client ~anchor ~num_before ~num_after ~narrow
    ?include_anchor ~op ~flag () =
  let* () =
    if num_before < 0 || num_after < 0 then
      Error (Error.Invalid_request "Message counts must be nonnegative")
    else
      match anchor with
      | Date _ ->
          Error
            (Error.Invalid_request
               "Date anchors are not supported for flag updates")
      | _ -> Ok ()
  in
  let* narrow = encode Zulip.Narrow.list_jsont narrow in
  let params =
    [
      ("anchor", anchor_to_string anchor);
      ("num_before", string_of_int num_before);
      ("num_after", string_of_int num_after);
      ("narrow", narrow);
      ("op", Zulip.Message_flag.op_to_string op);
      ("flag", Zulip.Message_flag.to_string flag);
    ]
    @ Option.fold ~none:[]
        ~some:(fun value -> [ ("include_anchor", string_of_bool value) ])
        include_anchor
  in
  Client.request_typed client ~method_:`POST
    ~path:"/api/v1/messages/flags/narrow" ~params
    ~codec:narrow_flag_result_jsont ()

type read_receipts_result = {
  user_ids : Zulip.Id.User.t list;
  raw : Jsont.json;
}

let user_ids_jsont =
  Jsont.Object.map ~kind:"Zulip read receipts response" (fun user_ids unknown ->
      let known =
        [
          ( ("user_ids", Jsont.Meta.none),
            Jsont.Json.list
              (List.map
                 (fun id -> Jsont.Json.int (Zulip.Id.User.to_int id))
                 user_ids) );
        ]
      in
      let unknown =
        match unknown with Jsont.Object (members, _) -> members | _ -> []
      in
      { user_ids; raw = Jsont.Object (known @ unknown, Jsont.Meta.none) })
  |> Jsont.Object.mem "user_ids" (Jsont.list Zulip.Id.User.jsont)
       ~enc:(fun (r : read_receipts_result) -> r.user_ids)
  |> Jsont.Object.keep_unknown Jsont.json_mems
       ~enc:(fun (r : read_receipts_result) ->
         match r.raw with
         | Jsont.Object (members, meta) ->
             Jsont.Object
               ( List.filter (fun ((name, _), _) -> name <> "user_ids") members,
                 meta )
         | _ -> Jsont.Json.object' [])
  |> Jsont.Object.finish

let get_read_receipts_detailed client ~message_id =
  Client.request_typed client ~method_:`GET
    ~path:
      ("/api/v1/messages/"
      ^ int_id Zulip.Id.Message.to_int message_id
      ^ "/read_receipts")
    ~codec:user_ids_jsont ()

let get_read_receipts client ~message_id =
  let* result = get_read_receipts_detailed client ~message_id in
  Ok result.user_ids

let report client ~message_id ~report_type ?description () =
  let params =
    [ ("report_type", report_type) ]
    @ Option.fold ~none:[]
        ~some:(fun value -> [ ("description", value) ])
        description
  in
  Client.request client ~method_:`POST
    ~path:
      ("/api/v1/messages/"
      ^ int_id Zulip.Id.Message.to_int message_id
      ^ "/report")
    ~params ()
  |> unit_result

let mark_all_as_read client =
  unit_result
    (Client.request client ~method_:`POST ~path:"/api/v1/mark_all_as_read" ())

let mark_channel_as_read client ~channel_id =
  unit_result
    (Client.request client ~method_:`POST ~path:"/api/v1/mark_stream_as_read"
       ~params:[ ("stream_id", int_id Zulip.Id.Channel.to_int channel_id) ]
       ())

let mark_topic_as_read client ~channel_id ~topic =
  unit_result
    (Client.request client ~method_:`POST ~path:"/api/v1/mark_topic_as_read"
       ~params:
         [
           ("stream_id", int_id Zulip.Id.Channel.to_int channel_id);
           ("topic_name", topic);
         ]
       ())

let emoji_type_to_string = function
  | Unicode_emoji -> "unicode_emoji"
  | Realm_emoji -> "realm_emoji"
  | Zulip_extra_emoji -> "zulip_extra_emoji"

let reaction_params ~emoji_name ?emoji_code ?reaction_type () =
  [ ("emoji_name", emoji_name) ]
  @ Option.fold ~none:[] ~some:(fun v -> [ ("emoji_code", v) ]) emoji_code
  @ Option.fold ~none:[]
      ~some:(fun v -> [ ("reaction_type", emoji_type_to_string v) ])
      reaction_type

let add_reaction client ~message_id ~emoji_name ?emoji_code ?reaction_type () =
  unit_result
    (Client.request client ~method_:`POST
       ~path:
         ("/api/v1/messages/"
         ^ int_id Zulip.Id.Message.to_int message_id
         ^ "/reactions")
       ~params:(reaction_params ~emoji_name ?emoji_code ?reaction_type ())
       ())

let remove_reaction client ~message_id ~emoji_name ?emoji_code ?reaction_type ()
    =
  unit_result
    (Client.request client ~method_:`DELETE
       ~path:
         ("/api/v1/messages/"
         ^ int_id Zulip.Id.Message.to_int message_id
         ^ "/reactions")
       ~params:(reaction_params ~emoji_name ?emoji_code ?reaction_type ())
       ())

let render_detailed client ~content =
  Client.request_typed client ~method_:`POST ~path:"/api/v1/messages/render"
    ~params:[ ("content", content) ]
    ~codec:rendered_jsont ()

let render client ~content =
  let* result = render_detailed client ~content in
  Ok result.rendered

let move_topic client ~channel ~new_channel ~topic ?new_topic ?message_id
    ?(propagate_mode = Change_all) ?(notify_old_topic = true)
    ?(notify_new_topic = true) () =
  let* source_id = Channels.get_id client ~name:channel in
  let* target_id = Channels.get_id client ~name:new_channel in
  let* message_id =
    match (message_id, propagate_mode) with
    | Some id, _ -> Ok id
    | None, (Change_one | Change_later) ->
        Error
          (Error.Invalid_request
             "move_topic requires message_id unless propagate_mode is \
              change_all")
    | None, Change_all -> (
        let* page =
          get_messages client ~anchor:Newest ~num_before:1 ~num_after:0
            ~narrow:
              [ Zulip.Narrow.stream_id source_id; Zulip.Narrow.topic topic ]
            ()
        in
        match page.messages with
        | message :: _ -> Ok message.Zulip.Message.id
        | [] ->
            Error
              (Error.Invalid_request ("no messages found in topic " ^ topic)))
  in
  edit client ~message_id ?topic:new_topic ~channel_id:target_id ~propagate_mode
    ~send_notification_to_old_thread:notify_old_topic
    ~send_notification_to_new_thread:notify_new_topic ()
