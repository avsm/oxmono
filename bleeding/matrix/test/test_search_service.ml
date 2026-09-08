module Id = Matrix_proto.Id
module Event = Matrix_proto.Event
module Client = Matrix_client.Client
module Search = Matrix_client.Search
module Service = Matrix_ui.Search_service
module Json = Matrix_proto.Json

type recorded = { url : string; body : string }
type response = { status : int; body : string }

let uid value = Result.get_ok (Id.User_id.of_string value)
let did value = Result.get_ok (Id.Device_id.of_string value)

let random_env =
  object
    method secure_random = Eio.Flow.string_source (String.make 4096 's')
  end

let body_of_request (request : Fetch.Middleware.request) =
  match request.body with
  | Fetch.String body -> body
  | Fetch.Empty | Fetch.Stream _ -> Alcotest.fail "expected a string body"

let mock_seq ?inside responses =
  let requests = ref [] in
  let remaining = ref responses in
  let fetch =
    Fetch_mock.client (fun request ->
        requests :=
          {
            url = Fetch.Middleware.Url.to_string request.url;
            body = body_of_request request;
          }
          :: !requests;
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

let event id body =
  Printf.sprintf
    {|{"event_id":%S,"sender":"@bob:example.org","origin_server_ts":1234,"type":"m.room.message","content":{"body":%S,"msgtype":"m.text"}}|}
    id body

let hit ?(rank = 0.5) id body =
  Printf.sprintf {|{"rank":%.2f,"result":%s}|} rank (event id body)

let page ?next hits =
  Printf.sprintf {|{"search_categories":{"room_events":{"results":[%s]%s}}}|}
    (String.concat "," hits)
    (match next with
    | None -> ""
    | Some token -> Printf.sprintf {|,"next_batch":%S|} token)

let ok body = { status = 200; body }
let failure = { status = 500; body = "{}" }

let body_json (request : recorded) =
  Result.get_ok
    (Jsont_bytesrw.decode_string Matrix_proto.Json.Codec.json request.body)

let search_term request =
  let categories = Json.find_mem "search_categories" (body_json request) in
  let events = Option.bind categories (Json.find_mem "room_events") in
  Option.bind events (Json.find_string "search_term")

let event_ids service =
  Matrix_ui.Observable.List.snapshot (Service.results service)
  |> Array.to_list
  |> List.map (fun (hit : Search.hit) ->
      match hit.result with
      | Some event ->
          Option.map Id.Event_id.to_string event.Event.Raw_event.event_id
      | None -> None)

let is_loading service =
  match Matrix_ui.Observable.Value.get (Service.state service) with
  | Service.Loading -> true
  | Service.Idle _ -> false

let run test () = Eio_mock.Backend.run test

let test_pages_and_terminal_noop () =
  let requests, fetch =
    mock_seq
      [
        ok
          (page ~next:"second"
             [ hit ~rank:0.1 "$a" "first"; hit ~rank:0.9 "$c" "third" ]);
        ok (page [ hit "$b" "second" ]);
      ]
  in
  let service = Service.create ~client:(client fetch) () in
  Alcotest.(check bool)
    "cannot page before search" true
    (Result.is_error (Service.next_page service));
  Result.get_ok (Service.search service ~criteria:(Search.v "hello"));
  Alcotest.(check (list (option string)))
    "server relevance order is retained" [ Some "$a"; Some "$c" ]
    (event_ids service);
  Alcotest.(check int) "one successful page" 1 (Service.loaded_pages service);
  Result.get_ok (Service.next_page service);
  Alcotest.(check (list (option string)))
    "pages append"
    [ Some "$a"; Some "$c"; Some "$b" ]
    (event_ids service);
  Alcotest.(check int) "two successful pages" 2 (Service.loaded_pages service);
  Result.get_ok (Service.next_page service);
  let requests = List.rev !requests in
  Alcotest.(check int) "terminal no-op" 2 (List.length requests);
  Alcotest.(check string)
    "first URL" "https://hs.example/_matrix/client/v3/search"
    (List.hd requests).url;
  Alcotest.(check string)
    "second token URL"
    "https://hs.example/_matrix/client/v3/search?next_batch=second"
    (List.nth requests 1).url;
  List.iter
    (fun request ->
      Alcotest.(check (option string))
        "criteria retained" (Some "hello") (search_term request))
    requests

let test_new_search_resets () =
  let requests, fetch =
    mock_seq
      [
        ok (page ~next:"old-token" [ hit "$old" "old" ]);
        ok (page [ hit "$new" "new" ]);
      ]
  in
  let service = Service.create ~client:(client fetch) () in
  Result.get_ok (Service.search service ~criteria:(Search.v "old"));
  Result.get_ok (Service.search service ~criteria:(Search.v "new"));
  Alcotest.(check (list (option string)))
    "old hits replaced" [ Some "$new" ] (event_ids service);
  Alcotest.(check int) "page count reset" 1 (Service.loaded_pages service);
  let second = List.nth (List.rev !requests) 1 in
  Alcotest.(check string)
    "new query starts without old token"
    "https://hs.example/_matrix/client/v3/search" second.url;
  Alcotest.(check (option string)) "new term" (Some "new") (search_term second)

let test_failed_page_retries () =
  let requests, fetch =
    mock_seq
      [
        ok (page ~next:"retry-me" [ hit "$a" "first" ]);
        failure;
        ok (page [ hit "$b" "second" ]);
      ]
  in
  let service = Service.create ~client:(client fetch) () in
  Result.get_ok (Service.search service ~criteria:(Search.v "hello"));
  Alcotest.(check bool)
    "page failed" true
    (Result.is_error (Service.next_page service));
  Alcotest.(check (list (option string)))
    "results preserved" [ Some "$a" ] (event_ids service);
  Alcotest.(check int) "failure not counted" 1 (Service.loaded_pages service);
  Alcotest.(check bool)
    "error retained" true
    (Option.is_some
       (Matrix_ui.Observable.Value.get (Service.last_error service)));
  Result.get_ok (Service.next_page service);
  Alcotest.(check (list (option string)))
    "retried page appended once" [ Some "$a"; Some "$b" ] (event_ids service);
  Alcotest.(check bool)
    "error cleared by retry" true
    (Option.is_none
       (Matrix_ui.Observable.Value.get (Service.last_error service)));
  let requests = List.rev !requests in
  List.iter
    (fun index ->
      Alcotest.(check string)
        "same retry token"
        "https://hs.example/_matrix/client/v3/search?next_batch=retry-me"
        (List.nth requests index).url)
    [ 1; 2 ]

let test_reentrant_request_is_rejected () =
  let service = ref None in
  let attempted = ref None in
  let inside () =
    match !service with
    | None -> ()
    | Some service ->
        Alcotest.(check bool)
          "loading is visible to fetch" true (is_loading service);
        attempted := Some (Service.next_page service)
  in
  let _, fetch = mock_seq ~inside [ ok (page []) ] in
  let value = Service.create ~client:(client fetch) () in
  service := Some value;
  Result.get_ok (Service.search value ~criteria:(Search.v "hello"));
  Alcotest.(check bool)
    "nested call rejected" true
    (match !attempted with
    | Some (Error _) -> true
    | None | Some (Ok ()) -> false);
  Alcotest.(check int)
    "outer request still succeeds" 1
    (Service.loaded_pages value)

let () =
  Alcotest.run "search service"
    [
      ( "pagination",
        [
          Alcotest.test_case "pages and terminal no-op" `Quick
            (run test_pages_and_terminal_noop);
          Alcotest.test_case "new search resets" `Quick
            (run test_new_search_resets);
          Alcotest.test_case "failed page retries" `Quick
            (run test_failed_page_retries);
          Alcotest.test_case "reentrant request" `Quick
            (run test_reentrant_request_is_rejected);
        ] );
    ]
