open Zulip_eio

let ok = function
  | Ok value -> value
  | Error e -> Alcotest.fail (Error.error_to_string e)

let json text =
  match Jsont_bytesrw.decode_string Jsont.json text with
  | Ok j -> j
  | Error e -> failwith e

let success = {|{"result":"success","msg":""}|}
let headers = Http.Header.of_list [ ("content-type", "application/json") ]

let make env handler =
  let auth =
    Auth.create ~site:"https://zulip.test" ~email:"bot@zulip.test"
      ~api_key:"test-key"
    |> ok
  in
  let transport =
    Transport.of_fetch ~clock:env#clock (Fetch_mock.client handler)
  in
  Client.create ~transport ~auth () |> ok

let read_body (req : Fetch.Middleware.request) =
  match req.body with
  | Fetch.Empty -> ""
  | Fetch.String body -> body
  | Fetch.Stream { flow; _ } ->
      Eio.Buf_read.(take_all (of_flow ~max_size:65536 flow))

let with_client handler f =
  Eio_mock.Backend.run_full (fun env -> f env (make env handler))

let test_forms () =
  let fields =
    [
      ("topic", "λ &=+#");
      ("object", {|{"a":[1,true,null,{"x":"a&b"}]}|});
      ("empty", "");
      ("repeat", "one");
      ("repeat", "two");
    ]
  in
  let seen = ref [] in
  with_client
    (fun req ->
      seen := req.meth :: !seen;
      Alcotest.(check (list (pair string string)))
        "lossless form" fields
        (Httpz_media.Urlencoded.decode (read_body req));
      Alcotest.(check string)
        "path" "/api/v1/messages"
        (Fetch.Middleware.Url.path_and_query req.url);
      Alcotest.(check (option string))
        "content type" (Some "application/x-www-form-urlencoded")
        (Http.Header.get req.headers "content-type");
      Alcotest.(check bool)
        "auth" true
        (Option.is_some (Http.Header.get req.headers "authorization"));
      Fetch_mock.respond ~headers success req)
    (fun _ client ->
      List.iter
        (fun method_ ->
          Client.request client ~method_ ~path:"messages" ~params:fields ()
          |> ok |> ignore)
        [ `POST; `PUT; `PATCH; `DELETE ]);
  Alcotest.(check int) "all mutation methods" 4 (List.length !seen)

let test_get_query () =
  with_client
    (fun req ->
      let target = Fetch.Middleware.Url.path_and_query req.url in
      Alcotest.(check string) "GET body" "" (read_body req);
      let query =
        String.sub target
          (String.index target '?' + 1)
          (String.length target - String.index target '?' - 1)
      in
      Alcotest.(check (list (pair string string)))
        "GET query"
        [ ("q", "x&λ+") ]
        (Httpz_media.Urlencoded.decode query);
      Fetch_mock.respond ~headers success req)
    (fun _ client ->
      Client.request client ~method_:`GET ~path:"users"
        ~params:[ ("q", "x&λ+") ]
        ()
      |> ok |> ignore)

let test_errors () =
  with_client
    (Fetch_mock.respond ~status:429
       ~headers:(Http.Header.add headers "retry-after" "7")
       {|{"result":"error","msg":"slow down","code":"RATE_LIMIT_HIT","custom":true}|})
    (fun _ client ->
      match Client.request client ~method_:`POST ~path:"messages" () with
      | Error
          (Error.Api
             {
               status = 429;
               code = "RATE_LIMIT_HIT";
               retry_after = Some 7.;
               extra;
               _;
             }) ->
          Alcotest.(check bool)
            "preserved extra" true
            (match extra with
            | Jsont.Object (fields, _) ->
                List.exists (fun ((k, _), _) -> k = "custom") fields
            | _ -> false)
      | _ -> Alcotest.fail "structured API error missing");
  with_client (Fetch_mock.respond ~status:502 "<h1>proxy error</h1>")
    (fun _ client ->
      match Client.request client ~method_:`GET ~path:"users" () with
      | Error (Error.Http { status = 502; _ }) -> ()
      | _ -> Alcotest.fail "HTTP status lost");
  with_client (Fetch_mock.respond ~headers "{") (fun _ client ->
      match Client.request client ~method_:`GET ~path:"users" () with
      | Error (Error.Json _) -> ()
      | _ -> Alcotest.fail "structured JSON error missing")

let test_response_cleanup () =
  let run ~body ~status check =
    let closed = ref false in
    with_client
      (fun req ->
        Fetch.Middleware.Pi.response ~status ~headers ~version:`HTTP_1_1
          ~body:(Eio.Flow.string_source body)
          ~url:req.url
          ~close:(fun () -> closed := true)
          ())
      (fun _ client ->
        check (Client.request client ~method_:`POST ~path:"messages" ()));
    Alcotest.(check bool) "response closed" true !closed
  in
  run ~body:success ~status:200 (fun result -> ignore (ok result));
  run ~body:"{" ~status:200 (function
    | Error (Error.Json _) -> ()
    | _ -> Alcotest.fail "expected codec failure");
  run ~body:"oops" ~status:503 (function
    | Error (Error.Http _) -> ()
    | _ -> Alcotest.fail "expected HTTP failure")

let test_redirect_scope () =
  let calls = ref 0 in
  with_client
    (fun req ->
      incr calls;
      Fetch_mock.respond ~status:307
        ~headers:
          (Http.Header.of_list [ ("location", "https://elsewhere.test/steal") ])
        "" req)
    (fun _ client ->
      ignore
        (Client.request client ~method_:`POST ~path:"messages"
           ~params:[ ("content", "secret") ]
           ());
      ignore (Client.request client ~method_:`GET ~path:"users" ()));
  Alcotest.(check int) "no cross-origin redirect" 2 !calls

let test_deadline () =
  with_client
    (fun _ -> Eio.Promise.await (fst (Eio.Promise.create ())))
    (fun env client ->
      let start = Eio.Time.now env#clock in
      (match
         Client.request client ~method_:`POST ~path:"messages" ~timeout:2. ()
       with
      | Error (Error.Timeout 2.) -> ()
      | _ -> Alcotest.fail "deadline missing");
      Alcotest.(check (float 0.0001))
        "deadline elapsed" 2.
        (Eio.Time.now env#clock -. start))

module Reader = struct
  type t = unit -> int

  let read_methods = []
  let single_read f _ = f ()
end

let source f = Eio.Resource.T (f, Eio.Flow.Pi.source (module Reader))

let test_interrupted_body () =
  Eio_mock.Backend.run_full (fun env ->
      let reading, mark_reading = Eio.Promise.create () in
      let closed = ref false and cancelled = ref false in
      let body =
        source (fun () ->
            Eio.Promise.resolve mark_reading ();
            Eio.Promise.await (fst (Eio.Promise.create ())))
      in
      let client =
        make env (fun req ->
            Fetch.Middleware.Pi.response ~status:200 ~headers ~version:`HTTP_1_1
              ~body ~url:req.url
              ~close:(fun () -> closed := true)
              ())
      in
      Eio.Fiber.first
        (fun () ->
          try
            ignore (Client.request client ~method_:`POST ~path:"messages" ());
            Alcotest.fail "cancellation was swallowed"
          with Eio.Cancel.Cancelled _ as exn ->
            cancelled := true;
            raise exn)
        (fun () -> Eio.Promise.await reading);
      Alcotest.(check bool) "cancellation propagated" true !cancelled;
      Alcotest.(check bool) "cancelled body closed" true !closed);
  let closed = ref false in
  with_client
    (fun req ->
      Fetch.Middleware.Pi.response ~status:200 ~headers ~version:`HTTP_1_1
        ~body:
          (source (fun () ->
               raise (Fetch.err (Fetch.Protocol_error "truncated body"))))
        ~url:req.url
        ~close:(fun () -> closed := true)
        ())
    (fun _ client ->
      match Client.request client ~method_:`POST ~path:"messages" () with
      | Error (Error.Transport (Fetch.Protocol_error "truncated body")) -> ()
      | _ -> Alcotest.fail "body read error lost");
  Alcotest.(check bool) "failed body closed" true !closed

let test_hosted_download_redirect () =
  let calls = ref 0 in
  with_client
    (fun req ->
      incr calls;
      if !calls = 1 then (
        Alcotest.(check bool)
          "Zulip download authenticates" true
          (Option.is_some (Http.Header.get req.headers "authorization"));
        Fetch_mock.respond ~status:302
          ~headers:
            (Http.Header.of_list
               [ ("location", "https://files.test/signed-object?token=test") ])
          "" req)
      else (
        Alcotest.(check (option string))
          "storage redirect has no API credentials" None
          (Http.Header.get req.headers "authorization");
        Fetch_mock.respond "file bytes" req))
    (fun _ client ->
      let output = Buffer.create 16 in
      Client.download client ~url:"/user_uploads/1/file"
        (Eio.Flow.buffer_sink output)
      |> ok;
      Alcotest.(check string)
        "redirected bytes" "file bytes" (Buffer.contents output));
  Alcotest.(check int) "followed storage redirect" 2 !calls

let test_limits () =
  Eio_mock.Backend.run_full (fun env ->
      let closed = ref false in
      let full =
        make env (fun req ->
            Fetch.Middleware.Pi.response ~status:200 ~headers ~version:`HTTP_1_1
              ~body:(Eio.Flow.string_source (String.make 500 'x'))
              ~url:req.url
              ~close:(fun () -> closed := true)
              ())
      in
      let client =
        Client.create ~max_body:32 ~transport:(Client.transport full)
          ~auth:(Client.auth full) ()
        |> ok
      in
      (match Client.request client ~method_:`GET ~path:"users" () with
      | Error (Error.Http { status = 200; _ }) -> ()
      | _ -> Alcotest.fail "body bound missing");
      Alcotest.(check bool) "oversized body closed" true !closed);
  with_client
    (Fetch_mock.respond
       ("{\"result\":\"success\",\"x\":" ^ String.make 200 '[' ^ "0"
      ^ String.make 200 ']' ^ "}"))
    (fun _ client ->
      match Client.request client ~method_:`GET ~path:"users" () with
      | Error (Error.Json _) -> ()
      | _ -> Alcotest.fail "JSON nesting bound missing")

let test_paths () =
  let calls = ref 0 in
  with_client
    (fun req ->
      incr calls;
      Fetch_mock.respond ~headers success req)
    (fun _ client ->
      List.iter
        (fun path ->
          match Client.request client ~method_:`GET ~path () with
          | Error (Error.Invalid_request _) -> ()
          | _ -> Alcotest.failf "accepted %s" path)
        [
          "../../users";
          "%2e%2e/%2e%2e/users";
          "users?api_key=secret";
          "users#fragment";
        ];
      let sink = Eio.Flow.buffer_sink (Buffer.create 8) in
      match
        Client.download client ~url:"https://elsewhere.test/attachment" sink
      with
      | Error (Error.Invalid_request _) -> ()
      | _ -> Alcotest.fail "external download accepted");
  Alcotest.(check int) "invalid targets did not reach backend" 0 !calls

let test_poll_lane () =
  Eio_mock.Backend.run_full (fun env ->
      let polling, mark_polling = Eio.Promise.create () in
      let finish, allow_finish = Eio.Promise.create () in
      let auth =
        Auth.create ~site:"https://zulip.test" ~email:"bot@zulip.test"
          ~api_key:"test-key"
        |> ok
      in
      let api =
        Fetch_mock.client (fun req ->
            if req.meth = `POST then
              Fetch_mock.respond ~headers
                {|{"result":"success","queue_id":"q","last_event_id":-1,"event_queue_longpoll_timeout_seconds":120}|}
                req
            else Fetch_mock.respond ~headers success req)
      in
      let poll =
        Fetch_mock.client (fun req ->
            Eio.Promise.resolve mark_polling ();
            Eio.Promise.await finish;
            Eio.Time.sleep env#clock 121.;
            Fetch_mock.respond ~headers {|{"result":"success","events":[]}|} req)
      in
      let transport =
        Transport.of_fetch ~clock:env#clock ~poll_fetch:poll api
      in
      let client = Client.create ~transport ~auth () |> ok in
      let queue = Event_queue.register client () |> ok in
      Eio.Fiber.both
        (fun () -> ignore (Event_queue.get_events queue client () |> ok))
        (fun () ->
          Eio.Promise.await polling;
          (match Event_queue.get_events queue client () with
          | Error (Error.Invalid_request _) -> ()
          | _ -> Alcotest.fail "concurrent poll accepted");
          Client.request client ~method_:`GET ~path:"users" () |> ok |> ignore;
          Eio.Promise.resolve allow_finish ()))

let test_queue () =
  let observed = ref [] in
  with_client
    (fun req ->
      let target = Fetch.Middleware.Url.path_and_query req.url in
      observed := (target, read_body req) :: !observed;
      if String.starts_with ~prefix:"/api/v1/register" target then
        Fetch_mock.respond ~headers
          {|{"result":"success","queue_id":"q1","last_event_id":8,"event_queue_longpoll_timeout_seconds":123}|}
          req
      else if req.meth = `DELETE then Fetch_mock.respond ~headers success req
      else
        Fetch_mock.respond ~headers
          {|{"result":"success","events":[{"id":8,"type":"heartbeat"},{"id":9,"type":"future_event","x":1},{"id":10,"type":"heartbeat"}]}|}
          req)
    (fun _ client ->
      let queue =
        Event_queue.register client
          ~narrow:[ Event_queue.Narrow.channel "x&y" ]
          ()
        |> ok
      in
      Alcotest.(check int) "register cursor" 8 (Event_queue.last_event_id queue);
      Alcotest.(check (float 0.))
        "server timeout" 123.
        (Event_queue.longpoll_timeout queue);
      let events = Event_queue.get_events queue client () |> ok in
      Alcotest.(check int)
        "redelivery removed" 2
        (Event_queue.Batch.length events);
      Alcotest.(check int)
        "poll has not acked" 8
        (Event_queue.last_event_id queue);
      (match Event_queue.ack ~count:3 queue events with
      | Error (Error.Invalid_request _) -> ()
      | _ -> Alcotest.fail "out-of-range acknowledgement accepted");
      let other = Event_queue.register client () |> ok in
      (match Event_queue.ack other events with
      | Error (Error.Invalid_request _) -> ()
      | _ -> Alcotest.fail "cross-generation acknowledgement accepted");
      Event_queue.ack ~count:1 queue events |> ok;
      Alcotest.(check int)
        "only accepted prefix" 9
        (Event_queue.last_event_id queue);
      Event_queue.ack ~count:0 queue events |> ok;
      Alcotest.(check int)
        "cursor stays monotonic" 9
        (Event_queue.last_event_id queue);
      Event_queue.ack queue events |> ok;
      Alcotest.(check int) "explicit ack" 10 (Event_queue.last_event_id queue);
      Event_queue.delete queue client |> ok);
  let registration =
    List.assoc "/api/v1/register" (List.rev !observed)
    |> Httpz_media.Urlencoded.decode
  in
  Alcotest.(check string)
    "queue narrow pair encoding" {|[["stream","x&y"]]|}
    (List.assoc "narrow" registration);
  Alcotest.(check string)
    "DELETE queue body" "queue_id=q1"
    (List.assoc "/api/v1/events" !observed)

let test_queue_malformed () =
  let first = ref true in
  with_client
    (fun req ->
      let text =
        if !first then (
          first := false;
          {|{"result":"success","queue_id":"q","last_event_id":-1}|})
        else
          {|{"result":"success","events":[{"id":0,"type":"heartbeat"},{"type":"message"}]}|}
      in
      Fetch_mock.respond ~headers text req)
    (fun _ client ->
      let queue = Event_queue.register client () |> ok in
      (match Event_queue.get_events queue client () with
      | Error (Error.Json _) -> ()
      | _ -> Alcotest.fail "malformed event silently lost");
      Alcotest.(check int)
        "cursor unchanged" (-1)
        (Event_queue.last_event_id queue))

let test_auth () =
  let imported =
    Auth.of_zuliprc
      "[api]\nsite = zulip.test\nemail = b@example.com\nkey = a=b\n"
    |> ok
  in
  Alcotest.(check string)
    "INI value containing equals" "a=b" (Auth.api_key imported);
  Alcotest.(check string)
    "normalized site" "https://zulip.test" (Auth.site imported);
  List.iter
    (fun site ->
      match Auth.create ~site ~email:"b" ~api_key:"k" with
      | Error _ -> ()
      | Ok _ -> Alcotest.failf "accepted invalid site %s" site)
    [
      "https://user:pass@zulip.test";
      "https://zulip.test?key=bad";
      "https://zulip.test#bad";
    ]

let tests =
  [
    ("forms and DELETE bodies", test_forms);
    ("GET query", test_get_query);
    ("structured failures", test_errors);
    ("response cleanup", test_response_cleanup);
    ("credential redirect scope", test_redirect_scope);
    ("deadline", test_deadline);
    ("interrupted body cleanup", test_interrupted_body);
    ("bounded bodies", test_limits);
    ("hosted file redirect", test_hosted_download_redirect);
    ("endpoint and download paths", test_paths);
    ("independent long poll", test_poll_lane);
    ("queue registration and ack", test_queue);
    ("malformed queue event", test_queue_malformed);
    ("zuliprc import", test_auth);
  ]

let () =
  Alcotest.run "Zulip Fetch client"
    [
      ( "client",
        List.map (fun (name, test) -> Alcotest.test_case name `Quick test) tests
      );
    ]
