open Fetch

let check name condition = if not condition then failwith name
let invalid name fn = match fn () with
  | _ -> failwith (name ^ ": accepted invalid value")
  | exception Invalid_argument _ -> ()
let url s = match Middleware.Url.of_string s with Ok u -> u | Error e -> failwith e
let response ?(status = 200) ?(headers = []) ?(close = fun () -> ()) body req =
  Middleware.Pi.response ~close ~status ~headers:(Http.Header.of_list headers)
    ~version:`HTTP_1_1 ~body ~url:req.Middleware.url ()

let test_credentials () =
  Eio_mock.Backend.run @@ fun () ->
  Eio.Switch.run @@ fun sw ->
  List.iter (fun extend ->
    let seen = ref [] in
    let server (req : Middleware.request) =
      seen := (Http.Header.get_multi req.headers "x-api-key",
               Http.Header.get_multi req.headers "x-second") :: !seen;
      match Middleware.Url.path_and_query req.url with
      | "/start" -> Fetch_mock.respond ~status:302
          ~headers:(Http.Header.of_list ["Location", "/same"]) "" req
      | "/same" -> Fetch_mock.respond ~status:302
          ~headers:(Http.Header.of_list ["Location", "https://b.example.com/end"]) "" req
      | _ -> Fetch_mock.respond "done" req
    in
    let client = Fetch_mock.client server
      |> with_credentials ~scope:["https://a.example.com"] ~extend
           Credential.[Header ("X-aPI-kEY", fun _ -> "WRAPPER")]
      |> with_credentials ~scope:["https://a.example.com"] ~extend
           Credential.[Header ("X-SECOND", fun _ -> "SECOND")]
    in
    let r = fetch ~sw ~redirect:Redirect.within_site
      ~headers:Header.[raw "x-api-key" "CALLER"; raw "X-API-KEY" "COPY";
                       raw "x-second" "CALLER2"] client `GET "https://a.example.com/start" in
    check "credential composition and redirect stripping"
      (List.rev !seen = [(["WRAPPER"], ["SECOND"]); (["WRAPPER"], ["SECOND"]);
                       (if extend then (["WRAPPER"], ["SECOND"]) else ([], []))]);
    check "response accumulates names only"
      (List.sort compare (Middleware.sensitive r) = ["x-api-key"; "x-second"])) [false; true];
  let client = Fetch_mock.client (fun _ -> raise (err (Denied "policy")))
    |> with_credentials ~scope:["https://a.example.com"]
         Credential.[Header ("X-Api-Key", fun _ -> "WRAPPER-SECRET")] in
  let diagnostic = try ignore (get ~sw ~headers:Header.[raw "X-Api-Key" "CALLER-SECRET"]
      client "https://a.example.com"); "" with ex -> Printexc.to_string ex in
  let contains hay needle =
    let n = String.length needle in
    let rec loop i = i + n <= String.length hay &&
      (String.sub hay i n = needle || loop (i + 1)) in loop 0 in
  check "automatic diagnostics omit caller and wrapper secrets"
    (diagnostic <> "" && not (contains diagnostic "CALLER-SECRET")
     && not (contains diagnostic "WRAPPER-SECRET"))

let test_scopes () =
  let under prefix path = Middleware.Url.under
    ~prefix:(url ("https://example.com" ^ prefix)) (url ("https://example.com" ^ path)) in
  List.iter (fun (prefix, path, expected) ->
    check ("scope " ^ prefix ^ " / " ^ path) (under prefix path = expected))
    ["/api/admin", "/api//admin", false; "/api/admin", "//api/admin", false;
     "/api/admin", "/api/admin//x", true; "/api//", "/api/", false;
     "/api/", "/api/x", true; "/api//", "/api/x", false;
     "/api/admin", "/api/%2Fadmin", false; "/api", "/api/a%5Cb", false;
     "/api", "/api/../admin", false; "/api", "/api/x/../admin", true;
     "/", "//x/%2F", true];
  check "IPv6 origin canonicalization"
    (Middleware.Url.same_origin (url "https://[2001:0DB8:0:0:0:0:0:1]")
       (url "https://[2001:db8::1]"));
  check "hexadecimal IPv4-mapped host rejected"
    (Result.is_error (Middleware.Url.of_string "https://[::ffff:7f00:1]"));
  check "Unicode root dot"
    (Middleware.Url.host (url "https://é.example.") = "xn--9ca.example");
  Eio_mock.Backend.run @@ fun () ->
  Eio.Switch.run @@ fun sw ->
  let seen = ref [] in
  let client = Fetch_mock.client (fun req ->
    seen := Http.Header.get req.Middleware.headers "x-secret" :: !seen;
    if Middleware.Url.path_and_query req.url = "/api/admin" then
      Fetch_mock.respond ~status:302 ~headers:(Http.Header.of_list ["Location", "/api//admin"]) "" req
    else Fetch_mock.respond "ok" req)
    |> with_credentials ~scope:["https://example.com/api/admin"]
         Credential.[Header ("X-Secret", fun _ -> "secret")] in
  ignore (get ~sw client "https://example.com/api/admin");
  check "redirect does not attach credential at double slash" (List.rev !seen = [Some "secret"; None])

let test_release () =
  Eio_mock.Backend.run_full @@ fun env ->
  Eio.Switch.run @@ fun sw ->
  let closed = ref 0 and issued = ref 0 in
  let module Never_read = struct
    type t = unit
    let read_methods = []
    let single_read () _ = failwith "discarded response was drained"
  end in
  let source = Eio.Resource.T ((), Eio.Flow.Pi.source (module Never_read)) in
  let server req =
    check "predecessor closed before successor" (!closed = !issued);
    incr issued;
    response ~status:(if !issued < 5 then 503 else 200)
      ~close:(fun () -> incr closed) source req in
  let client = Fetch_mock.client server |> with_retry ~clock:env#mono_clock
    ~random:(Eio.Flow.string_source "")
    ~config:(Retry.v ~max_retries:4 ~jitter:false ~backoff_factor:0. ()) in
  let r = get ~sw client "https://example.com" in
  check "final response stays open" (!closed = 4 && !issued = 5);
  close r;
  close (Middleware.Pi.with_metadata ~scope:["test"] r);
  check "close is idempotent across metadata copies" (!closed = 5);
  let released = ref false in
  let client = Fetch_mock.client (fun req ->
    if Middleware.Url.host req.Middleware.url = "example.com" then
      response ~status:302 ~headers:["Location", "https://other.example"]
        ~close:(fun () -> released := true) source req
    else begin check "redirect predecessor closed" !released; Fetch_mock.respond "ok" req end) in
  ignore (get ~sw client "https://example.com"
    |> fun r -> close r);
  check "redirect final close" !released;
  let released = ref false in
  let config = Retry.v ~retry_response:(fun _ _ -> failwith "predicate failed") () in
  let client = Fetch_mock.client (response ~close:(fun () -> released := true) source)
    |> with_retry ~clock:env#mono_clock ~random:(Eio.Flow.string_source "") ~config in
  (try ignore (get ~sw client "https://example.com") with Failure _ -> ());
  check "raising retry predicate closes response" !released

let test_request_retry_release () =
  Eio_mock.Backend.run_full @@ fun env ->
  List.iter (fun approve ->
    let issued = ref 0 and closed = Array.make 2 0 in
    Eio.Switch.run (fun sw ->
      let start = Eio.Time.Mono.now env#mono_clock in
      let module Never_read = struct
        type t = unit
        let read_methods = []
        let single_read () _ = failwith "approved retry drained its response"
      end in
      let unreadable = Eio.Resource.T ((), Eio.Flow.Pi.source (module Never_read)) in
      let server req =
        let index = !issued in
        incr issued;
        if index > 0 then
          check "approved predecessor closed exactly once before successor"
            (index = 1 && closed.(0) = 1);
        let payload =
          if index = 0 && approve then unreadable
          else Eio.Flow.string_source (if index = 0 then "unavailable" else "ok")
        in
        let r = response ~status:(if index = 0 then 503 else 200)
          ~close:(fun () ->
            if index = 0 && approve then
              check "approved predecessor closed before backoff"
                (Mtime.equal start (Eio.Time.Mono.now env#mono_clock));
            closed.(index) <- closed.(index) + 1)
          payload req in
        Eio.Switch.on_release sw (fun () -> close r);
        r
      in
      let config = Retry.v ~max_retries:1 ~jitter:false ~backoff_factor:1.
        ~allowed_methods:(`POST :: Retry.default.allowed_methods)
        ~retry_request:(fun _ -> approve) () in
      let client = Fetch_mock.client server
        |> with_retry ~clock:env#mono_clock ~random:(Eio.Flow.string_source "") ~config in
      let r = post ~sw ~body:(String "query") client "https://example.com/search" in
      check "request gate controls retry response ownership"
        (if approve then status r = 200 && !issued = 2 && closed = [|1; 0|]
         else status r = 503 && !issued = 1 && closed = [|0; 0|]);
      let content = Eio.Buf_read.parse_exn ~max_size:100 Eio.Buf_read.take_all (body r) in
      check "caller can read the returned response"
        (content = if approve then "ok" else "unavailable");
      close r;
      close (Middleware.Pi.with_metadata ~scope:["copy"] r);
      check "caller closes the returned response exactly once"
        (closed = if approve then [|1; 1|] else [|1; 0|]));
    check "switch release does not close responses twice"
      (closed = if approve then [|1; 1|] else [|1; 0|]))
    [false; true]

let test_limits () =
  Eio_mock.Backend.run @@ fun () ->
  Eio.Switch.run @@ fun sw ->
  let codec = Media.of_strings "text/plain" ~encode:Fun.id ~decode:Result.ok in
  let seq = Media.lines "text/plain" codec in
  List.iter (fun size ->
    let data = String.make size 'x' in
    let client = Fetch_mock.client (Fetch_mock.respond data) in
    let accepted = try check "read payload" (read ~limit:5 client "https://example.com" = data); true
      with Eio.Buf_read.Buffer_limit_exceeded -> false in
    check "exact read limit" (accepted = (size <= 5));
    let client = Fetch_mock.client (Fetch_mock.respond ~status:400 data) in
    let accepted = try (match read_as ~limit:5 client codec "https://example.com" with
      | Error r -> close r | Ok _ -> failwith "expected error response"); true
      with Eio.Buf_read.Buffer_limit_exceeded -> false in
    check "exact error read_as limit" (accepted = (size <= 5));
    List.iter (fun ending ->
      let client = Fetch_mock.client (Fetch_mock.respond
        ~headers:(Http.Header.of_list ["Content-Type", "text/plain"]) (data ^ ending)) in
      let r = get ~sw client "https://example.com" in
      let accepted = try check "sequence payload"
        (List.of_seq (decode_seq ~max_line:5 seq r) = [data]); true
        with Eio.Io (E (Decode_failure {error = Media.Too_large 5; _}), _) -> false in
      check "exact sequence line limit" (accepted = (size <= 5))) [""; "\n"; "\r\n"])
    [4; 5; 6];
  check "zero body limit" (read ~limit:0 (Fetch_mock.client (Fetch_mock.respond "")) "https://example.com" = "");
  List.iter (fun body ->
    match fetch ~sw ~body (Fetch_mock.client (Fetch_mock.respond "")) `TRACE "https://example.com" with
    | _ -> failwith "TRACE content accepted"
    | exception Eio.Io (E (Invalid_request _), _) -> ())
    [String ""; stream (Eio.Flow.string_source "body")]

let test_pacing () =
  Eio_mock.Backend.run_full @@ fun env ->
  Eio.Switch.run @@ fun sw ->
  let clock = env#mono_clock in
  let client = Fetch_mock.client (Fetch_mock.respond "ok") |> with_limits ~clock ~min_interval:10. in
  ignore (get ~sw client "https://example.com");
  let start = Eio.Time.Mono.now clock in
  Eio.Fiber.both
    (fun () -> ignore (Eio.Fiber.first
      (fun () -> ignore (get ~sw client "https://example.com"))
      (fun () -> Eio.Time.Mono.sleep clock 1.)))
    (fun () ->
      Eio.Time.Mono.sleep clock 2.;
      ignore (get ~sw client "https://example.com");
      let elapsed = Mtime.Span.to_float_ns (Mtime.span start (Eio.Time.Mono.now clock)) /. 1e9 in
      check "cancelled reservation does not delay successor" (elapsed = 10.))

let test_live_id () =
  Eio_mock.Backend.run_full @@ fun env ->
  Eio.Switch.run @@ fun sw ->
  let closed = ref 0 in
  let flow = Eio_mock.Flow.make "id-only" in
  Eio_mock.Flow.on_read flow [`Return "id: live\n\n"; `Run (fun () -> Eio.Fiber.await_cancel ())];
  let client = Fetch_mock.client (response ~headers:["Content-Type", Sse.media_type]
    ~close:(fun () -> incr closed) (flow :> Eio.Flow.source_ty Eio.Resource.t)) in
  let sub = Sse.subscribe ~sw ~clock:env#mono_clock client "https://example.com" in
  for _ = 1 to 5 do Eio.Fiber.yield () done;
  check "ID-only block published without next event" (Sse.last_event_id sub = Some "live");
  Sse.close sub;
  check "SSE cancellation succeeds" (Eio.Promise.await (Sse.result sub) = Ok ());
  check "SSE cancellation releases exchange" (!closed = 1)

let test_headers () =
  let module H = Header in
  List.iter (fun value ->
    check ("shared media range syntax " ^ value) (H.decode H.accept value = None))
    ["*/html"; "te*xt/html"; "text/h*"; "text/*+" ];
  check "typed field accepts structured suffix range"
    (Option.is_some (H.decode H.accept "application/*+json;q=0.7"));
  List.iter (fun tag -> invalid "constructed entity tag" (fun () ->
    H.encode H.etag {weak = false; tag})) ["a\", \"b"; "has space"; "\127"; "\000"];
  let etag = H.{weak = true; tag = "a\\b\255"} in
  check "opaque entity tag roundtrip" (H.decode H.etag (H.encode H.etag etag) = Some etag);
  List.iter (fun s -> check ("content range " ^ s) (H.decode H.content_range s = None))
    ["bytes */*"; "bytes 0-1/1"; "bytes -1-2/3"; "bytes 2-1/4"];
  invalid "constructed content range" (fun () -> H.encode H.content_range
    {unit = "bytes"; range = None; complete_length = None});
  List.iter (fun s -> check ("authentication info " ^ s) (H.decode H.authentication_info s = None))
    ["nextnonce=\"unterminated"; "nextnonce=ok, garbage"; "nextnonce=\"a\"junk";
     "nextnonce=a, NEXTNONCE=b"; "nextnonce=\"a\000b\""];
  invalid "challenge token68 injection" (fun () -> H.encode H.www_authenticate
    [{scheme = "Basic"; params = ["", "x\r\nInjected: y"]}]);
  check "full auth scheme token" (Option.is_some (H.decode H.www_authenticate "New!Auth realm=\"ok\""));
  check "singleton duplicate rejected" (H.get H.etag
    (Http.Header.of_list ["ETag", "\"a\""; "eTAG", "\"b\""]) = None);
  List.iter (fun s -> check ("cache syntax " ^ s) (H.decode H.cache_status s = None))
    ["not valid; hit"; "Cache; hit=true"; "Cache; key=token"; "Cache; fwd=\"miss\"";
     "Cache; ttl=1.5"; "Cache; ttl=1000000000000000"; "Cache; ttl=?1";
     "Cache; detail=\"a\\q\""; "Cache; detail=\"a\000b\""; "Cache; BAD=1";
     "Cache; hit=?2"; "Cache; x=:====:"; "Cache; x=:A===:";
     "Cache; unknown=(a)"; "Cache,"; "Cache; unknown=1.2345"];
  List.iter (fun s -> match H.decode H.cache_status s with
    | None -> failwith ("valid cache syntax rejected: " ^ s)
    | Some values -> check "cache semantic roundtrip"
        (H.decode H.cache_status (H.encode H.cache_status values) = Some values))
    ["\"Example CDN, edge; west\"; hit; detail=MEMORY; key=\"a\\\"b\"";
     "Cache; fwd=uri-miss; fwd-status=304; ttl=-10; collapsed=?0";
     "Cache; x=1.234; y=:YWJj:; z=?1; detail=one:two/three";
     "Cache; hit=?0; hit"]

let () =
  test_credentials (); test_scopes (); test_release (); test_limits ();
  test_request_retry_release ();
  test_pacing (); test_live_id (); test_headers ();
  print_endline "release regressions passed"
