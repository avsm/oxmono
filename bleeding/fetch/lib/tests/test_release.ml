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
    ~config:(Retry.v ~max_retries:4 ~jitter:false ~backoff_factor:(Duration.of_sec 0) ()) in
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
      let config = Retry.v ~max_retries:1 ~jitter:false ~backoff_factor:(Duration.of_sec 1)
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

let test_get_as_limit () =
  Eio_mock.Backend.run @@ fun () ->
  Eio.Switch.run @@ fun sw ->
  let issued = ref 0 and closed = ref 0 in
  let client = Fetch_mock.client (fun req ->
      incr issued;
      response ~headers:["Content-Type", "text/plain"]
        ~close:(fun () -> incr closed) (Eio.Flow.string_source "hello") req) in
  check "get_as accepts its exact limit"
    (get_as ~sw ~limit:5 client Media.text "https://example.test" = Ok "hello");
  (match get_as ~sw ~limit:4 client Media.text "https://example.test" with
  | _ -> failwith "get_as accepted an oversized body"
  | exception Eio.Io (E (Decode_failure {error = Media.Too_large 4; _}), _) -> ());
  check "get_as closes success and decode failure" (!closed = 2);
  invalid "get_as negative limit" (fun () ->
      get_as ~sw ~limit:(-1) client Media.text "https://example.test");
  check "negative limit sends no request" (!issued = 2)

let test_pacing () =
  Eio_mock.Backend.run_full @@ fun env ->
  Eio.Switch.run @@ fun sw ->
  let clock = env#mono_clock in
  let client = Fetch_mock.client (Fetch_mock.respond "ok") |> with_limits ~clock ~min_interval:(Duration.of_sec 10) in
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

let test_typed_field_grammars () =
  let module H = Header in
  let field name values = Http.Header.of_list (List.map (fun v -> (name, v)) values) in
  (* Base64 blobs are canonical: padded to a multiple of four, with the unused
     bits of the final character clear, and a value where one is required. *)
  List.iter (fun s -> check ("non-canonical basic blob " ^ s)
    (H.decode H.authorization ("Basic " ^ s) = None))
    ["dXNlcjpwdw"; "dXNlcjpwdw="; "dXNlcjpwdw==="; "dXNlcjpwd="];
  check "canonical basic blob"
    (H.decode H.authorization "Basic dXNlcjpwdw==" = Some (`Basic ("user", "pw")));
  List.iter (fun s -> check ("non-canonical digest " ^ s)
    (H.decode H.content_digest ("sha-256=:" ^ s ^ ":") = None))
    [""; "QR=="; "c2h="; "c2hh="; "c2hhc"];
  check "canonical digest"
    (Option.map (List.map (fun d -> d.H.algorithm, d.H.digest))
       (H.decode H.content_digest "sha-256=:c2g=:, sha-512=:QQ==:")
     = Some [`Sha256, "c2g="; `Sha512, "QQ=="]);
  check "repeated digest algorithm takes the last value"
    (Option.map (List.map (fun d -> d.H.algorithm, d.H.digest))
       (H.decode H.content_digest "sha-256=:c2g=:, sha-512=:QQ==:, sha-256=:QQ==:")
     = Some [`Sha512, "QQ=="; `Sha256, "QQ=="]);
  (* A token68 is a whole credential, so a challenge cannot also name
     parameters: the encoder could not write the mixture back. *)
  check "token68 challenge with parameters"
    (H.decode H.www_authenticate {|Negotiate SGVsbG8=, realm="x"|} = None);
  check "token68 challenge alone"
    (Option.map (List.map (fun c -> c.H.scheme, c.H.params))
       (H.decode H.www_authenticate "Negotiate SGVsbG8=")
     = Some ["Negotiate", ["", "SGVsbG8="]]);
  let challenges = H.decode H.www_authenticate
      {|Bearer realm="api", error="x", Negotiate SGVsbG8=|} in
  check "challenge parameters in wire order"
    (Option.map (List.map (fun c -> c.H.scheme, c.H.params)) challenges
     = Some ["Bearer", ["realm", "api"; "error", "x"];
             "Negotiate", ["", "SGVsbG8="]]);
  check "challenge roundtrip"
    (H.decode H.www_authenticate
       (H.encode H.www_authenticate (Option.get challenges)) = challenges);
  (* RFC 9110 s13.1.5: a weak validator cannot bound a range. *)
  invalid "weak if-range validator"
    (fun () -> H.encode H.if_range (`Etag H.{weak = true; tag = "x"}));
  check "strong if-range roundtrip"
    (H.decode H.if_range (H.encode H.if_range (`Etag H.{weak = false; tag = "x"}))
     = Some (`Etag H.{weak = false; tag = "x"}));
  (* Accept-Ranges is [1#range-unit]: field lines join and a value naming two
     units is ambiguous for one scalar. *)
  check "single accept-ranges unit"
    (H.get H.accept_ranges (field "Accept-Ranges" ["bytes"]) = Some `Bytes);
  List.iter (fun vs -> check "ambiguous accept-ranges"
    (H.get H.accept_ranges (field "Accept-Ranges" vs) = None))
    [["bytes, none"]; ["bytes"; "none"]; [""]; ["not a token"]];
  (* Authentication-Info is [#auth-param], so repeated lines join. *)
  check "joined authentication-info"
    (Option.map (fun i -> i.H.nextnonce, i.H.qop)
       (H.get H.authentication_info
          (field "Authentication-Info" [{|nextnonce="a"|}; "qop=auth"]))
     = Some (Some "a", Some "auth"));
  (* RFC 6797 s6.1.2: includeSubDomains takes no value, and an oversized
     delta-seconds saturates rather than discarding the policy. *)
  let hsts s = Option.map (fun h -> h.H.max_age, h.include_subdomains, h.preload)
      (H.decode H.strict_transport_security s) in
  check "valued includeSubDomains" (hsts "max-age=100; includeSubDomains=1" = None);
  check "kept includeSubDomains"
    (hsts "max-age=100; includeSubDomains" = Some (100L, true, false));
  check "saturated hsts max-age"
    (hsts "max-age=99999999999999999999999; includeSubDomains; preload"
     = Some (2147483648L, true, true));
  check "malformed hsts max-age" (hsts "max-age=1e9; includeSubDomains" = None)

let test_pacing_overflow () =
  Eio_mock.Backend.run_full @@ fun env ->
  let client = Fetch_mock.client (Fetch_mock.respond "ok") in
  invalid "a negative pacing duration is rejected before use" (fun () ->
      with_limits ~clock:env#mono_clock ~min_interval:Int64.minus_one client)

let test_failed_close_cleanup () =
  Eio_mock.Backend.run_full @@ fun env ->
  Eio.Switch.run @@ fun sw ->
  List.iter (fun redirect ->
      let calls = ref 0 in
      let close () = incr calls; if !calls = 1 then failwith "close failed" in
      let client = Fetch_mock.client (fun req ->
          response ~status:(if redirect then 302 else 503)
            ~headers:["Location", "/next"] ~close (Eio.Flow.string_source "") req)
      in
      let client = if redirect then client else
          with_retry ~clock:env#mono_clock ~random:(Eio.Flow.string_source "0123456789abcdef") client in
      (match get ~sw client "https://example.test/start" with
      | _ -> failwith "close failure was ignored"
      | exception Failure message -> check "close failure retained" (message = "close failed"));
      check "failed close gets a cleanup attempt before propagation" (!calls = 2))
    [false; true]

let test_header_encoding () =
  Eio_mock.Backend.run @@ fun () ->
  Eio.Switch.run @@ fun sw ->
  let encodings = ref 0 in
  let custom = Header.v "X-Example"
      ~encode:(fun value -> incr encodings; value) ~decode:Option.some in
  let client = Fetch_mock.client (Fetch_mock.respond
      ~headers:(Http.Header.of_list ["Content-Type", "text/plain"]) "hello") in
  ignore (get_as ~sw ~headers:Header.[custom, "value"] client Media.text
            "https://example.test/");
  check "testing header presence does not evaluate encoders" (!encodings = 1)

let () =
  test_credentials (); test_scopes (); test_release (); test_limits (); test_get_as_limit ();
  test_request_retry_release ();
  test_pacing (); test_live_id (); test_headers ();
  test_typed_field_grammars (); test_header_encoding (); test_failed_close_cleanup (); test_pacing_overflow ();
  print_endline "release regressions passed"
