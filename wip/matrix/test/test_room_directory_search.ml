module Id = Matrix_proto.Id
module Client = Matrix_client.Client
module Directory = Matrix_client.Directory
module Search = Matrix_ui.Room_directory_search
module Json = Matrix_proto.Json

type recorded = { url : string; body : string }
type response = { status : int; body : string }

let uid value = Result.get_ok (Id.User_id.of_string value)
let did value = Result.get_ok (Id.Device_id.of_string value)

let random_env =
  object
    method secure_random = Eio.Flow.string_source (String.make 4096 'd')
  end

let body_of_request (request : Fetch.Middleware.request) =
  match request.body with
  | Fetch.String body -> body
  | Fetch.Empty | Fetch.Stream _ -> Alcotest.fail "expected a string body"

let mock_seq responses =
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

let room id name =
  Printf.sprintf
    {|{"room_id":%S,"name":%S,"num_joined_members":1,"join_rule":"public","guest_can_join":false,"world_readable":true}|}
    id name

let page ?next rooms =
  Printf.sprintf {|{"chunk":[%s]%s}|} (String.concat "," rooms)
    (match next with
    | None -> ""
    | Some token -> Printf.sprintf {|,"next_batch":%S|} token)

let ok body = { status = 200; body }
let failure = { status = 500; body = "{}" }

let json_body (request : recorded) =
  Result.get_ok
    (Jsont_bytesrw.decode_string Matrix_proto.Json.Codec.json request.body)

let body_string path request =
  let rec find path value =
    match path with
    | [] -> Json.as_string value
    | name :: rest -> Option.bind (Json.find_mem name value) (find rest)
  in
  find path (json_body request)

let body_int name request = Json.find_int name (json_body request)

let room_ids search =
  Matrix_ui.Observable.List.snapshot (Search.results search)
  |> Array.to_list
  |> List.map (fun (room : Directory.room_summary) ->
      Id.Room_id.to_string room.room_id)

let run test () = Eio_mock.Backend.run test

let test_pages_and_terminal_noop () =
  let requests, fetch =
    mock_seq
      [
        ok (page ~next:"second" [ room "!a:example.org" "A" ]);
        ok (page [ room "!b:example.org" "B" ]);
      ]
  in
  let search = Search.create ~client:(client fetch) () in
  Alcotest.(check bool)
    "starts at Start" true
    (match Matrix_ui.Observable.Value.get (Search.state search) with
    | Search.Start -> true
    | _ -> false);
  Alcotest.(check bool)
    "cannot page before search" true
    (Result.is_error (Search.next_page search));
  Result.get_ok
    (Search.search search ~filter:"cheese" ~batch_size:1
       ~via_server:"remote.example" ());
  Alcotest.(check (list string))
    "first result" [ "!a:example.org" ] (room_ids search);
  Alcotest.(check int) "one loaded page" 1 (Search.loaded_pages search);
  Alcotest.(check bool)
    "next token is observable" true
    (match Matrix_ui.Observable.Value.get (Search.state search) with
    | Search.Next "second" -> true
    | _ -> false);
  Result.get_ok (Search.next_page search);
  Alcotest.(check (list string))
    "pages append in server order"
    [ "!a:example.org"; "!b:example.org" ]
    (room_ids search);
  Alcotest.(check int) "two loaded pages" 2 (Search.loaded_pages search);
  Alcotest.(check bool) "terminal" true (Search.is_at_last_page search);
  Result.get_ok (Search.next_page search);
  let requests = List.rev !requests in
  Alcotest.(check int) "terminal call makes no request" 2 (List.length requests);
  Alcotest.(check string)
    "via server query"
    "https://hs.example/_matrix/client/v3/publicRooms?server=remote.example"
    (List.hd requests).url;
  Alcotest.(check (option int))
    "first limit" (Some 1)
    (body_int "limit" (List.hd requests));
  Alcotest.(check (option string))
    "first filter" (Some "cheese")
    (body_string [ "filter"; "generic_search_term" ] (List.hd requests));
  Alcotest.(check (option string))
    "first has no token" None
    (body_string [ "since" ] (List.hd requests));
  Alcotest.(check (option string))
    "second repeats server token" (Some "second")
    (body_string [ "since" ] (List.nth requests 1))

let test_new_search_resets () =
  let requests, fetch =
    mock_seq
      [
        ok (page ~next:"unused" [ room "!old:example.org" "Old" ]);
        ok (page [ room "!new:example.org" "New" ]);
      ]
  in
  let search = Search.create ~client:(client fetch) () in
  Result.get_ok (Search.search search ~filter:"old" ~batch_size:1 ());
  Result.get_ok (Search.search search ~filter:"new" ~batch_size:2 ());
  Alcotest.(check (list string))
    "new search replaces results" [ "!new:example.org" ] (room_ids search);
  Alcotest.(check int)
    "short terminal result is one page" 1
    (Search.loaded_pages search);
  let second = List.nth (List.rev !requests) 1 in
  Alcotest.(check (option string))
    "new filter" (Some "new")
    (body_string [ "filter"; "generic_search_term" ] second);
  Alcotest.(check (option string))
    "new search drops old token" None
    (body_string [ "since" ] second)

let test_failed_page_is_retryable () =
  let requests, fetch =
    mock_seq
      [
        ok (page ~next:"retry-me" [ room "!a:example.org" "A" ]);
        failure;
        ok (page [ room "!b:example.org" "B" ]);
      ]
  in
  let search = Search.create ~client:(client fetch) () in
  Result.get_ok (Search.search search ~batch_size:1 ());
  Alcotest.(check bool)
    "page failed" true
    (Result.is_error (Search.next_page search));
  Alcotest.(check (list string))
    "successful results survive" [ "!a:example.org" ] (room_ids search);
  Alcotest.(check int)
    "failed page is not counted" 1
    (Search.loaded_pages search);
  Alcotest.(check bool)
    "failure is observable" true
    (match Matrix_ui.Observable.Value.get (Search.state search) with
    | Search.Failed _ -> true
    | _ -> false);
  Result.get_ok (Search.next_page search);
  Alcotest.(check (list string))
    "retry appends once"
    [ "!a:example.org"; "!b:example.org" ]
    (room_ids search);
  let requests = List.rev !requests in
  List.iter
    (fun index ->
      Alcotest.(check (option string))
        "retry keeps token" (Some "retry-me")
        (body_string [ "since" ] (List.nth requests index)))
    [ 1; 2 ]

let test_empty_page_and_validation () =
  let requests, fetch = mock_seq [ ok (page []) ] in
  let search = Search.create ~client:(client fetch) () in
  Alcotest.check_raises "positive batch size"
    (Invalid_argument "Matrix_ui.Room_directory_search.search: batch_size")
    (fun () -> ignore (Search.search search ~batch_size:0 ()));
  Alcotest.(check int)
    "invalid search makes no request" 0 (List.length !requests);
  Result.get_ok (Search.search search ~batch_size:10 ());
  Alcotest.(check int)
    "Rust-compatible empty page count" 0
    (Search.loaded_pages search);
  Alcotest.(check bool)
    "empty response is terminal" true
    (Search.is_at_last_page search)

let () =
  Alcotest.run "room directory search"
    [
      ( "search",
        [
          Alcotest.test_case "pages and terminal no-op" `Quick
            (run test_pages_and_terminal_noop);
          Alcotest.test_case "new search resets" `Quick
            (run test_new_search_resets);
          Alcotest.test_case "failed page retries" `Quick
            (run test_failed_page_is_retryable);
          Alcotest.test_case "empty page and validation" `Quick
            (run test_empty_page_and_validation);
        ] );
    ]
