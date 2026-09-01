open Fetch

let checks = ref 0

let check name condition =
  incr checks;
  if not condition then failwith ("test_redirect: " ^ name)

let url value =
  match Middleware.Url.of_string value with
  | Ok value -> value
  | Error reason -> failwith reason

let test_same_site () =
  let same from to_ = Redirect.same_site ~from:(url from) ~to_:(url to_) in
  check "plaintext cannot extend credential scope"
    (not (same "http://api.example.com/a" "https://api.example.com/b"));
  check "same secure host"
    (same "https://api.example.com/a" "https://api.example.com/b");
  check "port is part of credential scope"
    (not (same "https://api.example.com/a" "https://api.example.com:8443/b"));
  check "registrable domain"
    (same "https://example.co.uk" "https://jmap.example.co.uk");
  check "public suffix is not a site"
    (not (same "https://foo.co.uk" "https://bar.co.uk"));
  check "target must be https"
    (not (same "https://a.example.com" "http://b.example.com"));
  check "same IPv4 literal"
    (same "https://127.0.0.1/a" "https://127.0.0.1/b");
  check "different IPv4 literals"
    (not (same "https://127.0.0.1" "https://127.0.0.2"));
  check "different IPv6 literals"
    (not (same "https://[2001:db8::1]" "https://[2001:db8::2]"));
  (* An address is an address in every spelling inet_aton accepts, so it
     never shares a registrable domain with a name that ends the same way. *)
  check "an IPv4 spelling is an address, not a site"
    (not (same "https://0x7f.1" "https://foo.1"));
  check "and spellings of one address are one site"
    (same "https://127.1" "https://127.0.0.1")

let test_validation () =
  let raises name f =
    match f () with
    | _ -> failwith ("test_redirect: " ^ name ^ " did not raise")
    | exception Invalid_argument _ -> ()
  in
  raises "negative retry count" (fun () -> Retry.v ~max_retries:(-1) ());
  raises "unbounded retry count" (fun () ->
    Retry.v ~max_retries:(Retry.max_retries_limit + 1) ());
  raises "NaN backoff" (fun () -> Retry.v ~backoff_factor:Float.nan ());
  raises "invalid retry status" (fun () -> Retry.v ~status_forcelist:[ 42 ] ());
  raises "negative stream length" (fun () ->
    Fetch.stream ~length:(-1L) (Eio.Flow.string_source ""))

let test_head_body_validation () =
  Eio_mock.Backend.run @@ fun () ->
  Eio.Switch.run @@ fun sw ->
  let client = Fetch_mock.client (fun req -> Fetch_mock.respond "sent" req) in
  let rejects meth =
    match
      Fetch.fetch ~sw ~body:(String "surprise") client meth
        "https://example.com/"
    with
    | _ -> false
    | exception Eio.Io (Fetch.E (Fetch.Invalid_request message), _) ->
      String.equal message "a HEAD request cannot carry a body"
  in
  check "HEAD body is refused" (rejects `HEAD);
  check "extension-spelled HEAD body is refused" (rejects (`Other "HEAD"));
  let rejects_alias () =
    match Fetch.fetch ~sw client (`Other "GET") "https://example.com/" with
    | _ -> false
    | exception Eio.Io (Fetch.E (Fetch.Invalid_request message), _) ->
      String.equal message
        "standard method \"GET\" must use its standard constructor"
  in
  check "standard method aliases are refused" (rejects_alias ());
  let raw_negative =
    Stream
      { length = Some (-1L);
        flow =
          (Eio.Flow.string_source ""
            :> Eio.Flow.source_ty Eio.Resource.t) }
  in
  let rejects_negative () =
    match
      Fetch.fetch ~sw ~body:raw_negative client `POST "https://example.com/"
    with
    | _ -> false
    | exception Eio.Io (Fetch.E (Fetch.Invalid_request message), _) ->
      String.equal message "request body length -1 is negative"
  in
  check "raw negative stream lengths are refused" (rejects_negative ())

let test_scope_segments () =
  let prefix = url "https://example.com/a/b" in
  let under value = Middleware.Url.under ~prefix (url value) in
  check "whole segments are in scope" (under "https://example.com/a/b/c");
  check "a shared string prefix is not a scope"
    (not (under "https://example.com/a/bc"));
  check "an encoded slash is not in scope"
    (not (under "https://example.com/a/b/..%2fsecret"));
  check "an encoded backslash is not in scope"
    (not (under "https://example.com/a/b/..%5csecret"));
  check "an origin scope accepts an encoded separator"
    (Middleware.Url.under ~prefix:(url "https://example.com/")
       (url "https://example.com/a%2Fb"))

let test_credentials () =
  let raises name f =
    match f () with
    | _ -> check name false
    | exception Invalid_argument _ -> check name true
  in
  raises "constant bearer validation" (fun () ->
      Credential.bearer "two words");
  raises "constant Basic user-id validation" (fun () ->
      Credential.basic ~user:"bad:user" ~password:"secret");
  Eio_mock.Backend.run @@ fun () ->
  let request credential =
    let seen = ref None in
    let server (req : Middleware.request) =
      seen := Http.Header.get req.headers "authorization";
      Fetch_mock.respond "ok" req
    in
    let client =
      Fetch_mock.client server
      |> Fetch.with_credentials ~scope:[ "https://example.com" ] [ credential ]
    in
    let result =
      match Fetch.read client "https://example.com/" with
      | body -> `Body body
      | exception Eio.Io (Fetch.E (Fetch.Denied _), _) -> `Denied
    in
    (result, !seen)
  in
  check "Basic wire encoding"
    (request (Credential.basic ~user:"user" ~password:"secret")
     = (`Body "ok", Some "Basic dXNlcjpzZWNyZXQ="));
  check "lazy invalid bearer is denied"
    (request (Credential.Bearer (fun () -> "two words")) = (`Denied, None));
  check "lazy invalid Basic user-id is denied"
    (request (Credential.Basic (fun () -> ("bad:user", "secret")))
     = (`Denied, None))

let test_embedded_ipv4 () =
  check "an IPv4-embedded IPv6 URL is refused"
    (Result.is_error
       (Middleware.Url.of_string "http://[::ffff:127.0.0.1]/"))

let test_stop () =
  Eio_mock.Backend.run @@ fun () ->
  Eio.Switch.run @@ fun sw ->
  let calls = ref 0 in
  let server req =
    incr calls;
    Fetch_mock.respond ~status:302
      ~headers:(Http.Header.of_list [ "Location", "http://other.example/" ])
      "unread redirect" req
  in
  let redirect =
    Redirect.v
      ~on_hop:(fun ~from:_ ~to_:_ response ->
        check "policy sees response" (status response = 302);
        Redirect.Stop)
      ()
  in
  let response =
    Fetch.fetch ~sw ~redirect (Fetch_mock.client server) `GET
      "https://start.example/"
  in
  check "Stop returns 3xx" (status response = 302);
  check "Stop does not follow" (!calls = 1);
  check "Stop leaves body unread"
    (Eio.Buf_read.take_all
       (Eio.Buf_read.of_flow ~max_size:100 (body response))
     = "unread redirect")

let test_redirect_survives_drain_reset () =
  Eio_mock.Backend.run @@ fun () ->
  Eio.Switch.run @@ fun sw ->
  let calls = ref 0 in
  let broken_body =
    let module S = struct
      type t = unit
      let read_methods = []
      let single_read () _ =
        raise (Fetch.err (Protocol_error "reset while draining redirect"))
    end in
    Eio.Resource.T ((), Eio.Flow.Pi.source (module S))
  in
  let server (req : Middleware.request) =
    incr calls;
    if !calls = 1 then
      Middleware.Pi.response ~close:(fun () -> ())
        ~status:302
        ~headers:(Http.Header.of_list [ "Location", "https://end.example/" ])
        ~version:`HTTP_1_1
        ~body:broken_body
        ~url:req.url
        ()
    else Fetch_mock.respond "ok" req
  in
  let response = Fetch.get ~sw (Fetch_mock.client server) "https://start.example/" in
  check "drain reset does not cancel redirect" (status response = 200 && !calls = 2)

let test_last_event_id_origin_scope () =
  Eio_mock.Backend.run @@ fun () ->
  Eio.Switch.run @@ fun sw ->
  let seen = ref [] in
  let server (req : Middleware.request) =
    seen := Http.Header.get req.headers "last-event-id" :: !seen;
    if Middleware.Url.host req.url = "start.example" then
      Fetch_mock.respond
        ~status:302
        ~headers:(Http.Header.of_list [ "Location", "https://end.example/" ])
        ""
        req
    else Fetch_mock.respond "ok" req
  in
  let headers = Header.[ raw "Last-Event-ID" "private-cursor" ] in
  ignore (Fetch.fetch ~sw ~headers (Fetch_mock.client server) `GET
            "https://start.example/" : response);
  check "Last-Event-ID stripped cross-origin"
    (List.rev !seen = [ Some "private-cursor"; None ])

let credential_server seen (req : Middleware.request) =
  let host = Middleware.Url.host req.url in
  let auth = Http.Header.get req.headers "authorization" in
  let query =
    match Httpz.Uriz.find_query_param (Middleware.Url.to_uri req.url) "token" with
    | Null -> None
    | This value -> Some value
  in
  seen := (host, auth, query) :: !seen;
  if String.equal host "example.com" then
    Fetch_mock.respond ~status:302
      ~headers:
        (Http.Header.of_list
           [ "Location", "https://jmap.example.com/session" ])
      "" req
  else Fetch_mock.respond "session" req

let credential_client ~extend seen =
  Fetch_mock.client (credential_server seen)
  |> Fetch.with_credentials ~scope:[ "https://example.com/.well-known" ]
       ~extend
       Credential.
         [ Bearer (fun () -> "SECRET"); Query [ "token", "QUERY" ] ]

let test_scope_extension () =
  Eio_mock.Backend.run @@ fun () ->
  Eio.Switch.run @@ fun sw ->
  let seen = ref [] in
  let response =
    Fetch.fetch ~sw ~redirect:Redirect.within_site
      (credential_client ~extend:true seen) `GET
      "https://example.com/.well-known/jmap"
  in
  check "redirect succeeds" (status response = 200);
  check "credential follows approved origin"
    (List.rev !seen
     = [ "example.com", Some "Bearer SECRET", Some "QUERY";
         "jmap.example.com", Some "Bearer SECRET", Some "QUERY" ]);
  check "credential query hidden from response URL"
    (Fetch.url response = "https://jmap.example.com/session");
  check "static scope reported"
    (List.mem "https://example.com/.well-known" (Fetch.scope response));
  check "extended origin reported"
    (List.mem "https://jmap.example.com" (Fetch.scope response))

let test_extension_is_opt_in () =
  Eio_mock.Backend.run @@ fun () ->
  Eio.Switch.run @@ fun sw ->
  let seen = ref [] in
  let response =
    Fetch.fetch ~sw ~redirect:Redirect.within_site
      (credential_client ~extend:false seen) `GET
      "https://example.com/.well-known/jmap"
  in
  check "unextended redirect succeeds" (status response = 200);
  check "credential stays in static scope"
    (List.rev !seen
     = [ "example.com", Some "Bearer SECRET", Some "QUERY";
         "jmap.example.com", None, None ]);
  check "unextended origin not reported"
    (not (List.mem "https://jmap.example.com" (Fetch.scope response)))

let test_explicit_trust () =
  Eio_mock.Backend.run @@ fun () ->
  Eio.Switch.run @@ fun sw ->
  let seen = ref [] in
  let server (req : Middleware.request) =
    let host = Middleware.Url.host req.url in
    seen := (host, Http.Header.get req.headers "authorization") :: !seen;
    if String.equal host "start.example" then
      Fetch_mock.respond ~status:302
        ~headers:
          (Http.Header.of_list [ "Location", "https://unrelated.test/end" ])
        "" req
    else Fetch_mock.respond "ok" req
  in
  let redirect =
    Redirect.v
      ~on_hop:(fun ~from:_ ~to_:_ _ -> Redirect.Follow_within_scope)
      ()
  in
  let client =
    Fetch_mock.client server
    |> Fetch.with_credentials ~scope:[ "https://start.example" ] ~extend:true
         Credential.[ Bearer (fun () -> "SECRET") ]
  in
  let response =
    Fetch.fetch ~sw ~redirect client `GET "https://start.example/"
  in
  check "explicit trust extends across sites"
    (List.rev !seen
     = [ "start.example", Some "Bearer SECRET";
         "unrelated.test", Some "Bearer SECRET" ]);
  check "explicit origin reported"
    (List.mem "https://unrelated.test" (Fetch.scope response))

let test_get_conversion_drops_representation_metadata () =
  Eio_mock.Backend.run @@ fun () ->
  Eio.Switch.run @@ fun sw ->
  let converted = ref None in
  let server (req : Middleware.request) =
    if String.equal (Middleware.Url.path_and_query req.url) "/start" then
      Fetch_mock.respond ~status:303
        ~headers:(Http.Header.of_list [ "Location", "/end" ]) "" req
    else begin
      converted := Some req;
      Fetch_mock.respond "ok" req
    end
  in
  let dropped =
    [ "content-type"; "content-encoding"; "content-language";
      "content-location"; "content-digest"; "repr-digest"; "digest";
      "last-modified" ]
  in
  let headers =
    List.fold_left (fun hs name -> Http.Header.add hs name "value")
      (Http.Header.of_list [ "X-Keep", "yes" ]) dropped
  in
  let response =
    Fetch.fetch ~sw ~headers:(Header.of_http headers) ~body:(String "payload")
      (Fetch_mock.client server) `POST "https://example.com/start"
  in
  check "converted redirect succeeds" (status response = 200);
  match !converted with
  | None -> check "converted request was observed" false
  | Some req ->
    check "303 converts POST to GET" (req.meth = `GET);
    check "unrelated metadata survives"
      (Http.Header.get req.headers "x-keep" = Some "yes");
    check "representation metadata is removed"
      (List.for_all (fun name -> not (Http.Header.mem req.headers name)) dropped)

let test_bearer_syntax_checked_before_backend () =
  Eio_mock.Backend.run @@ fun () ->
  Eio.Switch.run @@ fun sw ->
  let called = ref false in
  let client =
    Fetch_mock.client (fun req ->
        called := true;
        Fetch_mock.respond "unexpected" req)
    |> Fetch.with_credentials ~scope:[ "https://example.com" ]
         Credential.[ Bearer (fun () -> "two words") ]
  in
  check "invalid Bearer token is rejected"
    (match Fetch.get ~sw client "https://example.com/" with
     | _ -> false
     | exception Eio.Io (Fetch.E (Fetch.Denied _), _) -> true);
  check "invalid Bearer token never reaches the backend" (not !called)

let () =
  test_same_site ();
  test_validation ();
  test_head_body_validation ();
  test_scope_segments ();
  test_credentials ();
  test_embedded_ipv4 ();
  test_stop ();
  test_redirect_survives_drain_reset ();
  test_last_event_id_origin_scope ();
  test_scope_extension ();
  test_extension_is_opt_in ();
  test_explicit_trust ();
  test_get_conversion_drops_representation_metadata ();
  test_bearer_syntax_checked_before_backend ();
  Printf.printf "test_redirect: %d checks ok\n" !checks
