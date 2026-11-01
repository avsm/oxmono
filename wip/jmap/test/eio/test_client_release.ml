open Jmap_eio

let json_headers = Http.Header.of_list [ ("content-type", "application/json") ]

let respond_json body request =
  Fetch_mock.respond ~headers:json_headers body request

let path (request : Fetch.Middleware.request) =
  Fetch.Middleware.Url.path_and_query request.url

let core ~requests ~calls =
  Printf.sprintf
    {|{"maxSizeUpload":1000,"maxConcurrentUpload":1,"maxSizeRequest":10000,"maxConcurrentRequests":%d,"maxCallsInRequest":%d,"maxObjectsInGet":10,"maxObjectsInSet":10,"collationAlgorithms":[]}|}
    requests calls

let session ?(state = "s1") ?(api = "../old") ?(requests = 1) ?(calls = 4)
    ?(event_source = "../events/{types}/stream?close={closeafter}&ping={ping}")
    () =
  Printf.sprintf
    {|{"capabilities":{"urn:ietf:params:jmap:core":%s},"accounts":{},"primaryAccounts":{},"username":"u","apiUrl":"%s","downloadUrl":"../download/{accountId}/{blobId}/{name}?type={type}","uploadUrl":"../upload/{accountId}","eventSourceUrl":"%s","state":"%s"}|}
    (core ~requests ~calls) api event_source state

let response state =
  Printf.sprintf {|{"methodResponses":[],"sessionState":"%s"}|} state

let request calls =
  let invocation n =
    Jmap.Proto.Invocation.create ~name:"Core/echo"
      ~arguments:(Jsont.Object ([], Jsont.Meta.none))
      ~method_call_id:(string_of_int n)
  in
  Jmap.Proto.Request.create
    ~using:[ Jmap.Proto.Capability.core ]
    ~method_calls:(List.init calls invocation)
    ()

let connect ~sw server =
  match
    Client.connect ~sw
      (Transport.of_fetch (Fetch_mock.client server))
      "https://example.test/base/session"
  with
  | Ok client -> client
  | Error error -> Alcotest.fail (Client.error_to_string error)

let get_ok = function
  | Ok value -> value
  | Error error -> Alcotest.fail (Client.error_to_string error)

let test_expand_then_resolve () =
  Eio_mock.Backend.run @@ fun () ->
  Eio.Switch.run @@ fun sw ->
  let server request = respond_json (session ()) request in
  let client = connect ~sw server in
  Alcotest.(check string)
    "absolute display template"
    "https://example.test/events/{types}/stream?close={closeafter}&ping={ping}"
    (Client.event_source_url client);
  let url =
    get_ok
      (Client.expand_event_source_url client
         [
           ("types", `String "..");
           ("closeafter", `String "no");
           ("ping", `String "0");
         ])
  in
  Alcotest.(check string)
    "dot segments from bindings are removed after expansion"
    "https://example.test/stream?close=no&ping=0" url

let test_refresh_wakes_queue_on_new_endpoint () =
  let entered, set_entered = Eio.Promise.create () in
  let release, set_release = Eio.Promise.create () in
  let gets = ref 0 and old_posts = ref 0 and new_posts = ref 0 in
  let server request =
    match path request with
    | "/base/session" ->
        incr gets;
        if !gets = 1 then respond_json (session ()) request
        else
          respond_json
            (session ~state:"s2" ~api:"../new" ~requests:2 ())
            request
    | "/old" ->
        incr old_posts;
        Eio.Promise.resolve set_entered ();
        Eio.Promise.await release;
        respond_json (response "s1") request
    | "/new" ->
        incr new_posts;
        respond_json (response "s2") request
    | _ -> Fetch_mock.respond ~status:404 "not found" request
  in
  Eio_mock.Backend.run @@ fun () ->
  Eio.Switch.run @@ fun sw ->
  let client = connect ~sw server in
  let first =
    Eio.Fiber.fork_promise ~sw (fun () -> Client.request client (request 1))
  in
  Eio.Promise.await entered;
  let second =
    Eio.Fiber.fork_promise ~sw (fun () -> Client.request client (request 1))
  in
  Eio.Fiber.yield ();
  get_ok (Client.refresh_session client);
  Eio.Fiber.yield ();
  Alcotest.(check int) "queued request uses refreshed endpoint" 1 !new_posts;
  Eio.Promise.resolve set_release ();
  ignore (get_ok (Eio.Promise.await_exn first));
  ignore (get_ok (Eio.Promise.await_exn second));
  Alcotest.(check int) "one request used old endpoint" 1 !old_posts

let test_queued_request_rechecks_lowered_cap () =
  let entered, set_entered = Eio.Promise.create () in
  let release, set_release = Eio.Promise.create () in
  let gets = ref 0 and new_posts = ref 0 in
  let server request =
    match path request with
    | "/base/session" ->
        incr gets;
        if !gets = 1 then respond_json (session ~calls:2 ()) request
        else
          respond_json (session ~state:"s2" ~api:"../new" ~calls:1 ()) request
    | "/old" ->
        Eio.Promise.resolve set_entered ();
        Eio.Promise.await release;
        respond_json (response "s1") request
    | "/new" ->
        incr new_posts;
        respond_json (response "s2") request
    | _ -> Fetch_mock.respond ~status:404 "not found" request
  in
  Eio_mock.Backend.run @@ fun () ->
  Eio.Switch.run @@ fun sw ->
  let client = connect ~sw server in
  let first =
    Eio.Fiber.fork_promise ~sw (fun () -> Client.request client (request 1))
  in
  Eio.Promise.await entered;
  let second =
    Eio.Fiber.fork_promise ~sw (fun () -> Client.request client (request 2))
  in
  Eio.Fiber.yield ();
  get_ok (Client.refresh_session client);
  Eio.Promise.resolve set_release ();
  ignore (get_ok (Eio.Promise.await_exn first));
  (match Eio.Promise.await_exn second with
  | Error (Client.Transport (Fetch.Invalid_request _, _)) -> ()
  | Ok _ -> Alcotest.fail "queued request ignored the lower call cap"
  | Error error -> Alcotest.fail (Client.error_to_string error));
  Alcotest.(check int) "rejected before refreshed endpoint" 0 !new_posts

type Eio.Exn.Backend.t += Callback_failure

let test_with_get_preserves_callback_exception () =
  let failure =
    Eio.Exn.create (Eio.Fs.E (Eio.Fs.Permission_denied Callback_failure))
  in
  Eio_mock.Backend.run @@ fun () ->
  Eio.Switch.run @@ fun sw ->
  let server request =
    match path request with
    | "/base/session" -> respond_json (session ()) request
    | "/value" -> Fetch_mock.respond "value" request
    | _ -> Fetch_mock.respond ~status:404 "not found" request
  in
  let client = connect ~sw server in
  match
    Client.with_get client "https://example.test/value" (fun _ -> raise failure)
  with
  | exception caught when caught == failure -> ()
  | exception caught ->
      Alcotest.failf "callback exception changed to %s"
        (Printexc.to_string caught)
  | _ -> Alcotest.fail "callback exception did not escape"

let () =
  Alcotest.run "client release"
    [
      ( "client",
        [
          Alcotest.test_case "expand before resolve" `Quick
            test_expand_then_resolve;
          Alcotest.test_case "refresh wakes queue" `Quick
            test_refresh_wakes_queue_on_new_endpoint;
          Alcotest.test_case "queued request rechecks caps" `Quick
            test_queued_request_rechecks_lowered_cap;
          Alcotest.test_case "callback exception identity" `Quick
            test_with_get_preserves_callback_exception;
        ] );
    ]
