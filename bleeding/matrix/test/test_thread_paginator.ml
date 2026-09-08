module Id = Matrix_proto.Id
module Event = Matrix_proto.Event
module Client = Matrix_client.Client
module Paginator = Matrix_client.Thread_paginator
module Relations = Matrix_client.Relations
module Ui = Matrix_ui

type response = { status : int; body : string }

let uid value = Result.get_ok (Id.User_id.of_string value)
let rid value = Result.get_ok (Id.Room_id.of_string value)
let did value = Result.get_ok (Id.Device_id.of_string value)
let room_id = rid "!room:example.org"

let random_env =
  object
    method secure_random = Eio.Flow.string_source (String.make 2048 't')
  end

let mock_seq ?inside responses =
  let requests = ref [] in
  let remaining = ref responses in
  let fetch =
    Fetch_mock.client (fun request ->
        requests := Fetch.Middleware.Url.to_string request.url :: !requests;
        Option.iter (fun f -> f ()) inside;
        match !remaining with
        | response :: rest ->
            remaining := rest;
            Fetch_mock.respond ~status:response.status response.body request
        | [] -> Alcotest.fail "more requests than scripted responses")
  in
  (requests, fetch)

let client fetch =
  Client.create
    ~config:
      (Client.config ~homeserver:(Uriz.of_string_exn "https://hs.example") ())
    ~fetch
    ~random:(Matrix_client.Random.of_env random_env)
  |> fun client ->
  Client.with_session client
    {
      user_id = uid "@alice:example.org";
      access_token = "token";
      device_id = did "DEVICE";
      refresh_token = None;
    }

let event id =
  Printf.sprintf
    {|{"event_id":%S,"sender":"@bob:example.org","origin_server_ts":1234,"type":"m.room.message","content":{"body":"root","msgtype":"m.text"}}|}
    id

let summary_event id count =
  Printf.sprintf
    {|{"event_id":%S,"sender":"@bob:example.org","origin_server_ts":1234,"type":"m.room.message","content":{"body":"root","msgtype":"m.text"},"unsigned":{"m.relations":{"m.thread":{"count":%d,"latest_event":{"event_id":"$latest","sender":"@bob:example.org","origin_server_ts":1235,"type":"m.room.message","content":{"body":"latest","msgtype":"m.text"}}}}}}|}
    id count

let summary_event_with_latest id count latest =
  Printf.sprintf
    {|{"event_id":%S,"sender":"@bob:example.org","origin_server_ts":1234,"type":"m.room.message","content":{"body":"root","msgtype":"m.text"},"unsigned":{"m.relations":{"m.thread":{"count":%d,"latest_event":{"event_id":%S,"sender":"@bob:example.org","origin_server_ts":1235,"type":"m.room.message","content":{"body":"latest","msgtype":"m.text"}}}}}}|}
    id count latest

let malformed_summary_event id =
  Printf.sprintf
    {|{"event_id":%S,"sender":"@bob:example.org","origin_server_ts":1234,"type":"m.room.message","content":{"body":"root","msgtype":"m.text"},"unsigned":{"m.relations":{"m.thread":[]}}}|}
    id

let page ?next events =
  Printf.sprintf {|{"chunk":[%s]%s}|} (String.concat "," events)
    (match next with
    | None -> ""
    | Some token -> Printf.sprintf {|,"next_batch":%S|} token)

let versions = { status = 200; body = {|{"versions":["v1.4"]}|} }
let ok body = { status = 200; body }
let failure = { status = 500; body = "{}" }

let root_ids paginator =
  Paginator.roots paginator
  |> List.filter_map (fun (event : Event.Raw_event.t) ->
      Option.map Id.Event_id.to_string event.event_id)

let raw json =
  match Jsont_bytesrw.decode_string Event.Raw_event.jsont json with
  | Ok event -> event
  | Error error -> Alcotest.fail error

let run test () = Eio_mock.Backend.run test

let test_pages_deduplicate_and_stop () =
  let requests, fetch =
    mock_seq
      [
        versions;
        ok (page ~next:"next" [ event "$a" ]);
        ok (page [ event "$a"; event "$b" ]);
      ]
  in
  let paginator = Paginator.create ~client:(client fetch) ~room_id () in
  Result.get_ok (Paginator.next_page paginator ~limit:1 ());
  Alcotest.(check bool)
    "token visible" true
    (match Paginator.state paginator with
    | Paginator.Next "next" -> true
    | _ -> false);
  Result.get_ok (Paginator.next_page paginator ~limit:2 ());
  Alcotest.(check (list string))
    "duplicate root omitted" [ "$a"; "$b" ] (root_ids paginator);
  Alcotest.(check int)
    "two successful pages" 2
    (Paginator.loaded_pages paginator);
  Result.get_ok (Paginator.next_page paginator ());
  let requests = List.rev !requests in
  Alcotest.(check int) "terminal call makes no request" 3 (List.length requests);
  Alcotest.(check string)
    "first thread request"
    "https://hs.example/_matrix/client/v1/rooms/!room:example.org/threads?include=all&limit=1"
    (List.nth requests 1);
  Alcotest.(check string)
    "next token request"
    "https://hs.example/_matrix/client/v1/rooms/!room:example.org/threads?include=all&from=next&limit=2"
    (List.nth requests 2)

let test_filter_resets_progress () =
  let requests, fetch =
    mock_seq
      [
        versions;
        ok (page ~next:"discarded" [ event "$old" ]);
        ok (page [ event "$new" ]);
      ]
  in
  let paginator = Paginator.create ~client:(client fetch) ~room_id () in
  Result.get_ok (Paginator.next_page paginator ());
  Paginator.set_filter paginator Relations.Participated;
  Alcotest.(check (list string)) "roots cleared" [] (root_ids paginator);
  Alcotest.(check int) "page count cleared" 0 (Paginator.loaded_pages paginator);
  Result.get_ok (Paginator.next_page paginator ~limit:7 ());
  Alcotest.(check (list string))
    "new query result" [ "$new" ] (root_ids paginator);
  let thread_url = List.nth (List.rev !requests) 2 in
  Alcotest.(check string)
    "filter starts at first page"
    "https://hs.example/_matrix/client/v1/rooms/!room:example.org/threads?include=participated&limit=7"
    thread_url

let test_failure_retries_same_token () =
  let requests, fetch =
    mock_seq
      [
        versions;
        ok (page ~next:"retry" [ event "$a" ]);
        failure;
        ok (page [ event "$b" ]);
      ]
  in
  let paginator = Paginator.create ~client:(client fetch) ~room_id () in
  Result.get_ok (Paginator.next_page paginator ());
  Alcotest.(check bool)
    "second page fails" true
    (Result.is_error (Paginator.next_page paginator ()));
  Alcotest.(check (list string)) "roots preserved" [ "$a" ] (root_ids paginator);
  Alcotest.(check int)
    "failed page not counted" 1
    (Paginator.loaded_pages paginator);
  Result.get_ok (Paginator.next_page paginator ());
  Alcotest.(check (list string))
    "retry appends" [ "$a"; "$b" ] (root_ids paginator);
  let requests = List.rev !requests in
  Alcotest.(check string)
    "failed token"
    "https://hs.example/_matrix/client/v1/rooms/!room:example.org/threads?include=all&from=retry&limit=30"
    (List.nth requests 2);
  Alcotest.(check string)
    "retried token" (List.nth requests 2) (List.nth requests 3)

let test_state_updates_and_reentrant_noop () =
  let paginator_ref = ref None in
  let nested = ref None in
  let inside () =
    Option.iter
      (fun paginator -> nested := Some (Paginator.next_page paginator ()))
      !paginator_ref
  in
  let requests, fetch = mock_seq ~inside [ versions; ok (page []) ] in
  let paginator = Paginator.create ~client:(client fetch) ~room_id () in
  paginator_ref := Some paginator;
  let states = ref [] in
  let unsubscribe =
    Paginator.subscribe paginator (fun state -> states := state :: !states)
  in
  Result.get_ok (Paginator.next_page paginator ());
  unsubscribe ();
  Alcotest.(check bool)
    "nested request is a successful no-op" true
    (match !nested with Some (Ok ()) -> true | None | Some (Error _) -> false);
  Alcotest.(check int) "only outer endpoint flow" 2 (List.length !requests);
  Alcotest.(check bool)
    "start/loading/end published" true
    (match List.rev !states with
    | [ Paginator.Start; Paginator.Loading; Paginator.End ] -> true
    | _ -> false);
  Alcotest.check_raises "positive limit"
    (Invalid_argument "Thread_paginator.next_page: limit") (fun () ->
      ignore (Paginator.next_page paginator ~limit:0 ()))

let test_callback_failure_is_retryable () =
  let _, fetch =
    mock_seq
      [
        versions;
        ok (page ~next:"retry" [ event "$a" ]);
        ok (page [ event "$a" ]);
      ]
  in
  let fail_once = ref true in
  let paginator =
    Paginator.create
      ~on_root:(fun _ ->
        if !fail_once then (
          fail_once := false;
          failwith "root callback failed"))
      ~client:(client fetch) ~room_id ()
  in
  Alcotest.check_raises "callback failure restores idle state"
    (Failure "root callback failed") (fun () ->
      ignore (Paginator.next_page paginator ()));
  Alcotest.(check bool)
    "failed callback does not publish a page" true
    (match Paginator.state paginator with
    | Paginator.Start -> true
    | _ -> false);
  Alcotest.(check (list string))
    "failed callback does not commit roots" [] (root_ids paginator);
  Result.get_ok (Paginator.next_page paginator ());
  Alcotest.(check (list string))
    "retry commits the page" [ "$a" ] (root_ids paginator)

let test_ui_ingests_duplicate_pages_and_restarts () =
  let requests, fetch =
    mock_seq
      [
        versions;
        ok (page ~next:"older" [ summary_event "$a" 1 ]);
        ok (page [ summary_event "$a" 4; malformed_summary_event "$b" ]);
      ]
  in
  let store = Matrix_client.Store.memory () in
  let thread_info =
    Ui.Thread_info.create ~store ~user_id:(uid "@alice:example.org") ()
  in
  let list =
    Ui.Thread_list.create ~client:(client fetch) ~thread_info ~room_id ()
  in
  Result.get_ok (Ui.Thread_list.next_page list ());
  Alcotest.(check (option string))
    "backward continuation is exposed" (Some "older")
    (Ui.Thread_list.continuation list);
  Result.get_ok (Ui.Thread_list.next_page list ());
  Alcotest.(check (list string))
    "server roots remain duplicate-free" [ "$a"; "$b" ]
    (Ui.Thread_list.roots list
    |> List.filter_map (fun (event : Event.Raw_event.t) ->
        Option.map Id.Event_id.to_string event.event_id));
  let infos = Ui.Thread_list.snapshot list in
  Alcotest.(check int)
    "both roots reach the UI aggregate" 2 (Array.length infos);
  (match infos.(0).Ui.Thread_info.summary_status with
  | Ui.Thread_info.Known { reply_count = 4; latest_reply_id = Some id } ->
      Alcotest.(check string)
        "bundled latest event id" "$latest" (Id.Event_id.to_string id)
  | _ -> Alcotest.fail "duplicate page did not merge the richer summary");
  Alcotest.(check string)
    "bundled latest event is retained" "$latest"
    ( Option.get infos.(0).latest_reply |> fun event ->
      Option.get event.event_id |> Id.Event_id.to_string );
  (* A later sync update reaches the admitted rich projection without adding
     another list row. *)
  Ui.Thread_info.ingest_root thread_info ~room_id (raw (summary_event "$a" 6));
  let updated = Ui.Thread_list.snapshot list in
  Alcotest.(check int)
    "sync updates retain the admitted row" 2 (Array.length updated);
  Alcotest.(check int)
    "sync updates replace the summary" 6 updated.(0).reply_count;
  (match infos.(1).Ui.Thread_info.summary_status with
  | Ui.Thread_info.Unknown -> ()
  | _ -> Alcotest.fail "malformed page summary was not unknown");
  Alcotest.(check string)
    "continuation is terminal" ""
    (match Ui.Thread_list.state list with
    | Ui.Thread_list.End -> ""
    | _ -> "bad");
  Alcotest.(check (option string))
    "terminal continuation is absent" None
    (Ui.Thread_list.continuation list);
  Ui.Thread_list.set_filter list Relations.Participated;
  Alcotest.(check (list string))
    "filter reset clears paginator roots" []
    (Ui.Thread_list.roots list
    |> List.filter_map (fun (event : Event.Raw_event.t) ->
        Option.map Id.Event_id.to_string event.event_id));
  Alcotest.(check int)
    "filter reset clears the rich projection" 0
    (Array.length (Ui.Thread_list.snapshot list));
  Alcotest.(check string)
    "filter reset returns to start" "start"
    (match Ui.Thread_list.state list with
    | Ui.Thread_list.Start -> "start"
    | _ -> "bad");
  Alcotest.(check (option string))
    "filter reset continuation is absent" None
    (Ui.Thread_list.continuation list);
  Ui.Thread_list.close list;
  Result.get_ok (Ui.Thread_list.next_page list ());
  Alcotest.(check int) "closed list makes no request" 3 (List.length !requests);
  Ui.Thread_info.ingest_root thread_info ~room_id (raw (summary_event "$a" 8));
  Alcotest.(check int)
    "closed list is detached from later sync" 0
    (Array.length (Ui.Thread_list.snapshot list));
  let restored =
    Ui.Thread_info.create ~store ~user_id:(uid "@alice:example.org") ()
  in
  Alcotest.(check int)
    "aggregate survives restart" 2
    (Array.length (Ui.Thread_info.snapshot restored room_id));
  Ui.Thread_info.remove_room restored room_id;
  Alcotest.(check int)
    "forget clears restored aggregate" 0
    (Array.length (Ui.Thread_info.snapshot restored room_id));
  Alcotest.(check int)
    "only version and page requests were made" 3 (List.length !requests)

let test_ui_thread_events_are_shared_and_forget_closes () =
  let requests, fetch =
    mock_seq
      [
        versions;
        ok
          (page ~next:"older"
             [ summary_event_with_latest "$root" 1 "$latest-1" ]);
        ok (page [ summary_event_with_latest "$root" 3 "$latest-2" ]);
      ]
  in
  let cache = Ui.Event_cache.create () in
  let thread_info =
    Ui.Thread_info.create ~user_id:(uid "@alice:example.org") ()
  in
  let list =
    Ui.Thread_list.create ~client:(client fetch) ~thread_info ~event_cache:cache
      ~room_id ()
  in
  Result.get_ok (Ui.Thread_list.next_page list ());
  let root_id = Result.get_ok (Id.Event_id.of_string "$root") in
  let latest_1 = Result.get_ok (Id.Event_id.of_string "$latest-1") in
  Alcotest.(check bool)
    "root is available through shared cache" true
    (Option.is_some (Ui.Event_cache.find_event cache room_id root_id));
  Alcotest.(check bool)
    "bundled latest reply is available through shared cache" true
    (Option.is_some (Ui.Event_cache.find_event cache room_id latest_1));
  Result.get_ok (Ui.Thread_list.next_page list ());
  let latest_2 = Result.get_ok (Id.Event_id.of_string "$latest-2") in
  Alcotest.(check bool)
    "duplicate refresh replaces latest reply in cache" true
    (Option.is_some (Ui.Event_cache.find_event cache room_id latest_2));
  Alcotest.(check int)
    "duplicate refresh remains one list item" 1
    (Array.length (Ui.Thread_list.snapshot list));
  Ui.Event_cache.forget_room cache room_id;
  Alcotest.(check int)
    "forget clears the thread list" 0
    (Array.length (Ui.Thread_list.snapshot list));
  Alcotest.(check bool)
    "forget clears detached root" false
    (Option.is_some (Ui.Event_cache.find_event cache room_id root_id));
  Alcotest.(check bool)
    "forget clears detached latest reply" false
    (Option.is_some (Ui.Event_cache.find_event cache room_id latest_2));
  Result.get_ok (Ui.Thread_list.next_page list ());
  Alcotest.(check int)
    "closed list does not request after forget" 3 (List.length !requests)

let test_ui_thread_list_forget_wins_during_fetch () =
  let cache = Ui.Event_cache.create () in
  let thread_info =
    Ui.Thread_info.create ~user_id:(uid "@alice:example.org") ()
  in
  let fetch =
    Fetch_mock.client (fun request ->
        let url = Fetch.Middleware.Url.to_string request.url in
        if
          String.contains url 't'
          && String.length url >= 8
          && String.sub url (String.length url - 8) 8 = "versions"
        then Fetch_mock.respond ~status:200 {|{"versions":["v1.4"]}|} request
        else begin
          Ui.Event_cache.forget_room cache room_id;
          Fetch_mock.respond ~status:200
            (Printf.sprintf {|{"chunk":[%s]}|}
               (summary_event_with_latest "$late" 1 "$late-reply"))
            request
        end)
  in
  let list =
    Ui.Thread_list.create ~client:(client fetch) ~thread_info ~event_cache:cache
      ~room_id ()
  in
  let states = ref [] in
  let unsubscribe =
    Ui.Thread_list.subscribe list (fun state -> states := state :: !states)
  in
  Result.get_ok (Ui.Thread_list.next_page list ());
  unsubscribe ();
  Alcotest.(check int)
    "late response cannot repopulate list" 0
    (Array.length (Ui.Thread_list.snapshot list));
  Alcotest.(check (list string))
    "late response cannot repopulate roots" []
    (Ui.Thread_list.roots list
    |> List.filter_map (fun (event : Event.Raw_event.t) ->
        Option.map Id.Event_id.to_string event.event_id));
  Alcotest.(check int)
    "late response cannot increment loaded pages" 0
    (Ui.Thread_list.loaded_pages list);
  Alcotest.(check bool)
    "closed paginator is terminal" true
    (Ui.Thread_list.is_at_last_page list);
  Alcotest.(check (list string))
    "late response has no terminal transition" [ "start"; "loading" ]
    (List.rev !states
    |> List.map (function
      | Ui.Thread_list.Start -> "start"
      | Ui.Thread_list.Loading -> "loading"
      | Ui.Thread_list.Next _ -> "next"
      | Ui.Thread_list.End -> "end"
      | Ui.Thread_list.Failed _ -> "failed"));
  let late_id = Result.get_ok (Id.Event_id.of_string "$late") in
  Alcotest.(check bool)
    "late response cannot repopulate cache" false
    (Option.is_some (Ui.Event_cache.find_event cache room_id late_id))

let () =
  Alcotest.run "thread paginator"
    [
      ( "pagination",
        [
          Alcotest.test_case "pages and deduplication" `Quick
            (run test_pages_deduplicate_and_stop);
          Alcotest.test_case "filter reset" `Quick
            (run test_filter_resets_progress);
          Alcotest.test_case "failure retry" `Quick
            (run test_failure_retries_same_token);
          Alcotest.test_case "state and reentrancy" `Quick
            (run test_state_updates_and_reentrant_noop);
          Alcotest.test_case "callback failure retry" `Quick
            (run test_callback_failure_is_retryable);
          Alcotest.test_case "UI ingestion and restart" `Quick
            (run test_ui_ingests_duplicate_pages_and_restarts);
          Alcotest.test_case "UI cache and forget" `Quick
            (run test_ui_thread_events_are_shared_and_forget_closes);
          Alcotest.test_case "UI late fetch after forget" `Quick
            (run test_ui_thread_list_forget_wins_during_fetch);
        ] );
    ]
