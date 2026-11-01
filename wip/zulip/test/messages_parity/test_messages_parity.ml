open Zulip_eio

let ok = function
  | Ok value -> value
  | Error error -> Alcotest.fail (Error.error_to_string error)

let headers = Http.Header.of_list [ ("content-type", "application/json") ]

let read_body (request : Fetch.Middleware.request) =
  match request.body with
  | Fetch.Empty -> ""
  | Fetch.String body -> body
  | Fetch.Stream { flow; _ } ->
      Eio.Buf_read.(take_all (of_flow ~max_size:65536 flow))

let form request = Httpz_media.Urlencoded.decode (read_body request)

let query (request : Fetch.Middleware.request) =
  let target = Fetch.Middleware.Url.path_and_query request.url in
  match String.index_opt target '?' with
  | None -> []
  | Some index ->
      Httpz_media.Urlencoded.decode
        (String.sub target (index + 1) (String.length target - index - 1))

let with_client handler run =
  Eio_mock.Backend.run_full @@ fun env ->
  let auth =
    Auth.create ~site:"https://zulip.test" ~email:"bot@zulip.test"
      ~api_key:"key"
    |> ok
  in
  let transport =
    Transport.of_fetch ~clock:env#clock (Fetch_mock.client handler)
  in
  let client = Client.create ~transport ~auth () |> ok in
  run client

let json_has name = function
  | Jsont.Object (members, _) ->
      Option.is_some (Jsont.Json.find_mem name members)
  | _ -> false

let test_full_message_query_and_response () =
  with_client
    (fun request ->
      let params = query request in
      Alcotest.(check string)
        "anchor date" "2026-09-01T00:00:00Z"
        (List.assoc "anchor_date" params);
      Alcotest.(check string) "date anchor" "date" (List.assoc "anchor" params);
      Alcotest.(check bool)
        "range has no IDs" false
        (List.mem_assoc "message_ids" params);
      Alcotest.(check string)
        "Markdown flag" "false"
        (List.assoc "apply_markdown" params);
      Alcotest.(check string)
        "empty topic flag" "true"
        (List.assoc "allow_empty_topic_name" params);
      Fetch_mock.respond ~headers
        {|{"result":"success","msg":"","anchor":8,"found_oldest":true,"found_newest":false,"found_anchor":true,"history_limited":false,"future_page":"kept","messages":[{"id":8,"sender_id":2,"sender_email":"a@example.test","sender_full_name":"A","timestamp":8,"content":"raw","content_type":"text/x-markdown","type":"private","recipient_id":4,"display_recipient":[{"id":2,"email":"a@example.test","full_name":"A","is_mirror_dummy":false,"future_user":1}],"flags":["read","future_flag"],"avatar_url":null,"client":"api","is_me_message":false,"last_edit_timestamp":7,"last_moved_timestamp":6,"sender_realm_str":"test","edit_history":[{"user_id":null,"timestamp":7,"prev_content":"old","future_edit":true}],"reactions":[{"emoji_name":"wave","emoji_code":"1f44b","reaction_type":"unicode_emoji","user_id":3,"future_reaction":true}],"topic_links":[],"submessages":[],"future_message":{"x":1}}]}|}
        request)
    (fun client ->
      let page =
        Messages.get_messages client
          ~anchor:(Messages.Date "2026-09-01T00:00:00Z") ~num_before:0
          ~num_after:10 ~client_gravatar:true ~apply_markdown:false
          ~allow_empty_topic_name:true ()
        |> ok
      in
      Alcotest.(check (option int))
        "response anchor" (Some 8)
        (Option.map Zulip.Id.Message.to_int page.anchor);
      Alcotest.(check bool)
        "page extension" true
        (json_has "future_page" page.raw);
      match page.messages with
      | [ message ] ->
          Alcotest.(check (option string))
            "nullable avatar" None
            (Zulip.Message.avatar_url message);
          Alcotest.(check (option string))
            "client" (Some "api")
            (Zulip.Message.client message);
          Alcotest.(check int)
            "one edit" 1
            (List.length (Option.get (Zulip.Message.edit_history message)));
          Alcotest.(check int)
            "one reaction" 1
            (List.length (Option.get (Zulip.Message.reactions message)));
          Alcotest.(check bool)
            "message extension" true
            (json_has "future_message" message.raw);
          let participants =
            Option.get (Zulip.Message.direct_participants message)
          in
          Alcotest.(check bool)
            "participant extension" true
            (json_has "future_user" (List.hd participants).raw)
      | _ -> Alcotest.fail "expected one message")

let test_edit_and_flag_metadata () =
  let requests = ref 0 in
  with_client
    (fun request ->
      incr requests;
      match !requests with
      | 1 ->
          let params = form request in
          Alcotest.(check string)
            "conflict hash" "abc123"
            (List.assoc "prev_content_sha256" params);
          Fetch_mock.respond ~headers
            {|{"result":"success","msg":"","detached_uploads":[{"id":9,"name":"old.txt","path_id":"1/aa/old.txt","size":12,"create_time":1700000000,"message_ids":[],"future_attachment":true}],"future_edit_response":4}|}
            request
      | 2 ->
          let params = form request in
          Alcotest.(check string)
            "narrow"
            "[{\"operator\":\"is\",\"operand\":\"unread\",\"negated\":false}]"
            (List.assoc "narrow" params);
          Fetch_mock.respond ~headers
            {|{"result":"success","msg":"","processed_count":4,"updated_count":2,"first_processed_id":null,"last_processed_id":null,"found_oldest":true,"found_newest":false,"ignored_because_not_subscribed_channels":[11],"future_flags":true}|}
            request
      | _ -> Alcotest.fail "unexpected request")
    (fun client ->
      let edited =
        Messages.edit_detailed client
          ~message_id:(Zulip.Id.Message.of_int 4)
          ~content:"new" ~prev_content_sha256:"abc123" ()
        |> ok
      in
      Alcotest.(check int)
        "detached upload" 9
        (Attachments.Id.to_int (List.hd edited.detached_uploads).id);
      Alcotest.(check bool)
        "edit result extension" true
        (json_has "future_edit_response" edited.raw);
      let flags =
        Messages.update_flags_for_narrow client ~anchor:Messages.First_unread
          ~num_before:10 ~num_after:20
          ~narrow:[ Zulip.Narrow.is `Unread ]
          ~op:Zulip.Message_flag.Add ~flag:`Read ()
        |> ok
      in
      Alcotest.(check (option int))
        "nullable first ID" None
        (Option.map Zulip.Id.Message.to_int flags.first_processed_id);
      Alcotest.(check (list int))
        "ignored channels" [ 11 ]
        (List.map Zulip.Id.Channel.to_int
           flags.ignored_because_not_subscribed_channels))

let test_scheduled_drafts_and_reminders () =
  let requests = ref 0 in
  with_client
    (fun request ->
      incr requests;
      match !requests with
      | 1 ->
          let params = form request in
          Alcotest.(check string)
            "scheduled type" "channel" (List.assoc "type" params);
          Alcotest.(check string)
            "scheduled channel" "7" (List.assoc "to" params);
          Fetch_mock.respond ~headers
            {|{"result":"success","msg":"","scheduled_message_id":31,"future_create":true}|}
            request
      | 2 ->
          let params = form request in
          Alcotest.(check string)
            "draft JSON"
            {|[{"type":"private","to":[2,3],"topic":"","content":"later"}]|}
            (List.assoc "drafts" params);
          Fetch_mock.respond ~headers
            {|{"result":"success","msg":"","ids":[41]}|} request
      | 3 ->
          let params = form request in
          Alcotest.(check string)
            "reminder message" "8"
            (List.assoc "message_id" params);
          Alcotest.(check string)
            "reminder note" "review" (List.assoc "note" params);
          Fetch_mock.respond ~headers
            {|{"result":"success","msg":"","reminder_id":51,"future_reminder":1}|}
            request
      | _ -> Alcotest.fail "unexpected request")
    (fun client ->
      let scheduled =
        Scheduled_messages.create_detailed client
          ~destination:(Scheduled_messages.Channel (Zulip.Id.Channel.of_int 7))
          ~topic:"release" ~content:"ship" ~scheduled_delivery_timestamp:1000 ()
        |> ok
      in
      Alcotest.(check int)
        "scheduled ID" 31
        (Scheduled_messages.Id.to_int scheduled.id);
      Alcotest.(check bool)
        "scheduled extension" true
        (json_has "future_create" scheduled.raw);
      let drafts =
        Drafts.create client
          [
            {
              Drafts.destination =
                Direct [ Zulip.Id.User.of_int 2; Zulip.Id.User.of_int 3 ];
              topic = "";
              content = "later";
              timestamp = None;
            };
          ]
        |> ok
      in
      Alcotest.(check (list int))
        "draft IDs" [ 41 ]
        (List.map Drafts.Id.to_int drafts);
      let reminder =
        Reminders.create_detailed client
          ~message_id:(Zulip.Id.Message.of_int 8)
          ~scheduled_delivery_timestamp:2000 ~note:"review" ()
        |> ok
      in
      Alcotest.(check int) "reminder ID" 51 (Reminders.Id.to_int reminder.id);
      Alcotest.(check bool)
        "reminder extension" true
        (json_has "future_reminder" reminder.raw))

let test_bad_scheduled_destination () =
  let fixture =
    {|{"scheduled_message_id":1,"type":"stream","to":7.5,"content":"x","rendered_content":"<p>x</p>","scheduled_delivery_timestamp":9,"failed":false}|}
  in
  match Jsont_bytesrw.decode_string Scheduled_messages.jsont fixture with
  | Error _ -> ()
  | Ok _ -> Alcotest.fail "fractional channel ID was accepted"

let test_message_selection_modes () =
  let calls = ref [] in
  with_client
    (fun request ->
      calls := query request :: !calls;
      Fetch_mock.respond ~headers
        {|{"result":"success","msg":"","messages":[]}|} request)
    (fun client ->
      let ids = [ Zulip.Id.Message.of_int 5; Zulip.Id.Message.of_int 8 ] in
      ignore (Messages.get_messages client ~message_ids:ids () |> ok);
      Alcotest.(check (list (pair string string)))
        "selected IDs have no range"
        [ ("message_ids", "[5,8]") ]
        (List.hd !calls);
      ignore (Messages.get_messages client ~num_before:10 ~num_after:0 () |> ok);
      Alcotest.(check (option string))
        "default anchor" (Some "newest")
        (List.assoc_opt "anchor" (List.hd !calls));
      ignore
        (Messages.get_messages client ~num_before:0 ~num_after:10
           ~use_first_unread_anchor:true ()
        |> ok);
      Alcotest.(check bool)
        "legacy selection omits anchor" false
        (List.mem_assoc "anchor" (List.hd !calls));
      let invalid result =
        match result with
        | Error (Error.Invalid_request _) -> ()
        | _ -> Alcotest.fail "invalid selection accepted"
      in
      invalid (Messages.get_messages client ~message_ids:ids ~anchor:Newest ());
      invalid
        (Messages.get_messages client ~message_ids:ids ~include_anchor:false ());
      invalid (Messages.get_messages client ~message_ids:ids ~num_before:0 ());
      invalid (Messages.get_messages client ~message_ids:ids ~num_after:0 ());
      invalid
        (Messages.get_messages client ~message_ids:ids
           ~use_first_unread_anchor:false ());
      invalid
        (Messages.get_messages client ~anchor:Newest ~num_before:0 ~num_after:1
           ~use_first_unread_anchor:false ());
      invalid (Messages.get_messages client ~num_before:(-1) ~num_after:0 ());
      invalid (Messages.get_messages client ());
      Alcotest.(check int)
        "invalid selections perform no I/O" 3 (List.length !calls))

let test_invalid_mutations () =
  with_client
    (fun _ -> Alcotest.fail "invalid mutation performed I/O")
    (fun client ->
      let invalid = function
        | Error (Error.Invalid_request _) -> ()
        | _ -> Alcotest.fail "invalid mutation accepted"
      in
      invalid
        (Scheduled_messages.update client
           ~scheduled_message_id:(Scheduled_messages.Id.of_int 1)
           {
             destination = Some (Direct [ Zulip.Id.User.of_int 2 ]);
             topic = Some "invalid";
             content = None;
             scheduled_delivery_timestamp = None;
           });
      let source = Eio.Flow.string_source "content" in
      invalid
        (Attachments.upload_stream client ~filename:"x"
           ~content_type:"text/plain" ~length:(-1L) source);
      invalid
        (Users.upload_avatar_stream client ~filename:"x"
           ~content_type:"image/png" ~length:(-1L) source);
      invalid
        (Server.upload_emoji_stream client ~name:"x" ~filename:"x"
           ~content_type:"image/png" ~length:(-1L) source);
      invalid
        (Messages.update_flags_for_narrow client ~anchor:(Date "2026-01-01")
           ~num_before:0 ~num_after:1 ~narrow:[] ~op:Zulip.Message_flag.Add
           ~flag:`Starred ()))

let () =
  Alcotest.run "message parity"
    [
      ( "messages",
        [
          Alcotest.test_case "selection modes and validation" `Quick
            test_message_selection_modes;
          Alcotest.test_case "invalid mutations" `Quick test_invalid_mutations;
          Alcotest.test_case "full query and response" `Quick
            test_full_message_query_and_response;
          Alcotest.test_case "edit and flag metadata" `Quick
            test_edit_and_flag_metadata;
          Alcotest.test_case "scheduled drafts reminders" `Quick
            test_scheduled_drafts_and_reminders;
          Alcotest.test_case "bad scheduled destination" `Quick
            test_bad_scheduled_destination;
        ] );
    ]
