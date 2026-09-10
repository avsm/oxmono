(*---------------------------------------------------------------------------
   Copyright (c) 2025 Anil Madhavapeddy. All rights reserved.
   SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

(** Tests for the Eio JMAP client, driven by a mock Fetch backend. *)

open Jmap_eio

(* {1 A recording mock server} *)

type recorded = {
  meth : string;
  url : string;
  authorization : string option;
  content_type : string option;
  accept : string option;
  body : string;
}

let record log (req : Fetch.Middleware.request) =
  let header name = Http.Header.get req.headers name in
  let body =
    match req.body with
    | Fetch.Empty -> ""
    | Fetch.String s -> s
    | Fetch.Stream _ -> "<stream>"
  in
  let entry =
    {
      meth = Http.Method.to_string req.meth;
      url = Fetch.Middleware.Url.to_string req.url;
      authorization = header "authorization";
      content_type = header "content-type";
      accept = header "accept";
      body;
    }
  in
  log := !log @ [ entry ];
  entry

let path (req : Fetch.Middleware.request) =
  Fetch.Middleware.Url.path_and_query req.url

let origin (req : Fetch.Middleware.request) =
  Fetch.Middleware.Url.origin req.url

let media_headers media = Http.Header.of_list [ ("content-type", media) ]
let json_headers = media_headers "application/json"

let respond_json ?status body req =
  Fetch_mock.respond ?status ~headers:json_headers body req

let redirect ~location req =
  Fetch_mock.respond ~status:301
    ~headers:(Http.Header.of_list [ ("location", location) ])
    "" req

let not_found req = Fetch_mock.respond ~status:404 "not found" req

(* {1 Fixtures} *)

let token = "TOKEN"
let auth = Auth.bearer token
let bearer = Some ("Bearer " ^ token)
let well_known = "https://api.example.com/.well-known/jmap"
let default_download = "/jmap/download/{accountId}/{blobId}/{name}?type={type}"

let default_event_source =
  "/jmap/eventsource/?types={types}&closeafter={closeafter}&ping={ping}"

let core_json ?(max_size_upload = 50_000_000) ?(max_size_request = 10_000_000)
    ?(max_calls_in_request = 16) ?(max_objects_in_get = 500) () =
  Printf.sprintf
    {|{
    "maxSizeUpload": %d,
    "maxConcurrentUpload": 4,
    "maxSizeRequest": %d,
    "maxConcurrentRequests": 4,
    "maxCallsInRequest": %d,
    "maxObjectsInGet": %d,
    "maxObjectsInSet": 500,
    "collationAlgorithms": []
  }|}
    max_size_upload max_size_request max_calls_in_request max_objects_in_get

let default_core = core_json ()

let session_json ?(origin = "https://api.example.com")
    ?(blobs = "https://api.example.com") ?(download = default_download)
    ?(upload = "/jmap/upload/{accountId}/")
    ?(event_source = default_event_source) ?(api = "/jmap/api/")
    ?(core = default_core) () =
  let core =
    if String.equal core "" then ""
    else Printf.sprintf "\"urn:ietf:params:jmap:core\": %s" core
  in
  Printf.sprintf
    {|{
  "capabilities": { %s },
  "accounts": {
    "acc1": {
      "name": "Test Account",
      "isPersonal": true,
      "isReadOnly": false,
      "accountCapabilities": {}
    }
  },
  "primaryAccounts": { "urn:ietf:params:jmap:core": "acc1" },
  "username": "test@example.com",
  "apiUrl": "%s%s",
  "downloadUrl": "%s%s",
  "uploadUrl": "%s%s",
  "eventSourceUrl": "%s%s",
  "state": "state-1"
}|}
    core origin api blobs download blobs upload origin event_source

let echo_response =
  {|{ "methodResponses": [ [ "Core/echo", {}, "c1" ] ], "sessionState": "state-1" }|}

let echo_request () =
  Jmap.Proto.Request.create
    ~using:[ Jmap.Proto.Capability.core ]
    ~method_calls:
      [
        Jmap.Proto.Invocation.create ~name:"Core/echo"
          ~arguments:(Jsont.Object ([], Jsont.Meta.none))
          ~method_call_id:"c1";
      ]
    ()

let account_id = Jmap.Proto.Id.of_string_exn "acc1"
let blob_id = Jmap.Proto.Id.of_string_exn "B1"
let check_string = Alcotest.(check string)
let check_opt_string = Alcotest.(check (option string))

let client_exn ~sw ?max_body ?mono_clock server =
  match
    Client.connect ~sw ~auth ?max_body
      (Transport.of_fetch ?mono_clock (Fetch_mock.client server))
      well_known
  with
  | Ok client -> client
  | Error e -> Alcotest.failf "connect failed: %s" (Client.error_to_string e)

let error_string = function
  | Ok _ -> Alcotest.fail "expected an error"
  | Error e -> Client.error_to_string e

let contains ~needle haystack =
  let nl = String.length needle and hl = String.length haystack in
  let rec go i =
    i + nl <= hl && (String.sub haystack i nl = needle || go (i + 1))
  in
  go 0

let api_posts log = List.filter (fun e -> e.meth = "POST") !log
let quoted l = String.concat "," (List.map (fun s -> "\"" ^ s ^ "\"") l)

(* {1 Tests} *)

(* The session is fetched with the credential attached, and the API request
   goes to the session's apiUrl as application/json (RFC 8620 3.3). *)
let test_session_and_request () =
  let log = ref [] in
  let server req =
    ignore (record log req);
    match path req with
    | "/.well-known/jmap" -> respond_json (session_json ()) req
    | "/jmap/api/" -> respond_json echo_response req
    | _ -> not_found req
  in
  Eio_mock.Backend.run @@ fun () ->
  Eio.Switch.run @@ fun sw ->
  let client = client_exn ~sw server in
  check_string "api url" "https://api.example.com/jmap/api/"
    (Client.api_url client);
  let response = Client.request_exn client (echo_request ()) in
  check_string "session state" "state-1"
    response.Jmap.Proto.Response.session_state;
  Alcotest.(check int)
    "one method response" 1
    (List.length response.Jmap.Proto.Response.method_responses);
  match !log with
  | [ session; api ] ->
      check_string "session method" "GET" session.meth;
      check_opt_string "session accept" (Some "application/json") session.accept;
      check_opt_string "session credential" bearer session.authorization;
      check_string "api method" "POST" api.meth;
      check_string "api url" "https://api.example.com/jmap/api/" api.url;
      check_opt_string "api content type" (Some "application/json")
        api.content_type;
      check_opt_string "api accept" (Some "application/json") api.accept;
      check_opt_string "api credential" bearer api.authorization;
      Alcotest.(check bool)
        "request body carries the invocation" true
        (String.length api.body > 0
        && Option.is_some
             (Result.to_option (Codec.decode Jmap.Proto.Request.jsont api.body))
        )
  | l -> Alcotest.failf "expected 2 requests, got %d" (List.length l)

(* RFC 8620 2.2: the well-known session resource may redirect, and the
   credential must survive the hop. The final URL is the one refreshed.
   [jmap.example.com] shares the registrable domain of [api.example.com], so
   the credential's scope is extended to it. *)
let test_well_known_redirect () =
  let log = ref [] in
  let server req =
    ignore (record log req);
    match (origin req, path req) with
    | "https://api.example.com", "/.well-known/jmap" ->
        redirect ~location:"https://jmap.example.com/jmap/session" req
    | "https://jmap.example.com", "/jmap/session" ->
        respond_json (session_json ~origin:"https://jmap.example.com" ()) req
    | _ -> not_found req
  in
  Eio_mock.Backend.run @@ fun () ->
  Eio.Switch.run @@ fun sw ->
  let client = client_exn ~sw server in
  check_string "api url" "https://jmap.example.com/jmap/api/"
    (Client.api_url client);
  Client.refresh_session_exn client;
  match !log with
  | [ hop; session; refresh ] ->
      check_string "first hop" well_known hop.url;
      check_opt_string "first hop credential" bearer hop.authorization;
      check_string "second hop" "https://jmap.example.com/jmap/session"
        session.url;
      check_opt_string "second hop credential" bearer session.authorization;
      check_string "refresh goes straight to the final URL"
        "https://jmap.example.com/jmap/session" refresh.url;
      check_opt_string "refresh credential" bearer refresh.authorization
  | l -> Alcotest.failf "expected 3 requests, got %d" (List.length l)

(* A redirect to a site the credential was never meant for is where a
   compromised or spoofed session endpoint exfiltrates a bearer token, so it
   is refused by name rather than followed. *)
let cross_site_server log req =
  ignore (record log req);
  match (origin req, path req) with
  | "https://api.example.com", "/.well-known/jmap" ->
      redirect ~location:"https://evil.example.net/jmap/session" req
  | "https://evil.example.net", "/jmap/session" ->
      respond_json (session_json ~origin:"https://evil.example.net" ()) req
  | _ -> not_found req

let test_session_redirect_off_site_is_refused () =
  let log = ref [] in
  Eio_mock.Backend.run @@ fun () ->
  Eio.Switch.run @@ fun sw ->
  match
    Client.connect ~sw ~auth
      (Transport.of_fetch (Fetch_mock.client (cross_site_server log)))
      well_known
  with
  | Ok _ -> Alcotest.fail "expected the cross-site redirect to be refused"
  | Error (Client.Session_error msg) ->
      Alcotest.(check bool)
        ("the origin is named: " ^ msg)
        true
        (contains ~needle:"https://evil.example.net" msg);
      Alcotest.(check bool)
        ("and the way to allow it: " ^ msg)
        true
        (contains ~needle:"trust_redirects" msg);
      Alcotest.(check int) "the hop was never made" 1 (List.length !log)
  | Error e ->
      Alcotest.failf "expected Session_error, got %s" (Client.error_to_string e)

(* ~trust_redirects is the opt-in for a deployment that really does move its
   session resource to another site. *)
let test_session_redirect_off_site_with_trust () =
  let log = ref [] in
  Eio_mock.Backend.run @@ fun () ->
  Eio.Switch.run @@ fun sw ->
  match
    Client.connect ~sw ~auth ~trust_redirects:true
      (Transport.of_fetch (Fetch_mock.client (cross_site_server log)))
      well_known
  with
  | Error e -> Alcotest.failf "connect failed: %s" (Client.error_to_string e)
  | Ok client -> (
      check_string "api url" "https://evil.example.net/jmap/api/"
        (Client.api_url client);
      match !log with
      | [ _; session ] ->
          check_opt_string "the credential follows the hop" bearer
            session.authorization
      | l -> Alcotest.failf "expected 2 requests, got %d" (List.length l))

(* An anonymous client has nothing to lose, so its redirects are followed
   whatever the origin. *)
(* The Public Suffix List gives an IP literal a registrable domain, so
   "10.0.0.1" and "192.168.0.1" both answer "0.1" and would otherwise count
   as the same site. An IP host is the same site as itself and nothing
   else. *)
let test_session_redirect_between_ip_literals_is_refused () =
  let log = ref [] in
  let server req =
    ignore (record log req);
    match (origin req, path req) with
    | "https://10.0.0.1", "/.well-known/jmap" ->
        redirect ~location:"https://192.168.0.1/jmap/session" req
    | "https://192.168.0.1", "/jmap/session" ->
        respond_json (session_json ~origin:"https://192.168.0.1" ()) req
    | _ -> not_found req
  in
  Eio_mock.Backend.run @@ fun () ->
  Eio.Switch.run @@ fun sw ->
  match
    Client.connect ~sw ~auth
      (Transport.of_fetch (Fetch_mock.client server))
      "https://10.0.0.1/.well-known/jmap"
  with
  | Ok _ ->
      Alcotest.fail "expected the redirect between IP literals to be refused"
  | Error (Client.Session_error msg) ->
      Alcotest.(check bool)
        ("the origin is named: " ^ msg)
        true
        (contains ~needle:"https://192.168.0.1" msg);
      Alcotest.(check int) "the hop was never made" 1 (List.length !log)
  | Error e ->
      Alcotest.failf "expected Session_error, got %s" (Client.error_to_string e)

let test_session_redirect_without_a_credential () =
  let log = ref [] in
  Eio_mock.Backend.run @@ fun () ->
  Eio.Switch.run @@ fun sw ->
  match
    Client.connect ~sw
      (Transport.of_fetch (Fetch_mock.client (cross_site_server log)))
      well_known
  with
  | Error e -> Alcotest.failf "connect failed: %s" (Client.error_to_string e)
  | Ok client ->
      check_string "api url" "https://evil.example.net/jmap/api/"
        (Client.api_url client);
      check_opt_string "and no credential was sent" None
        (List.nth !log 1).authorization

(* Only the redirect status codes RFC 9110 assigns a Location-based redirect
   semantic enter the session redirect path. A cache response such as 304 is
   an ordinary HTTP failure here, not a redirect missing Location. *)
let test_non_redirect_3xx_is_http_error () =
  let server req =
    match path req with
    | "/.well-known/jmap" -> respond_json ~status:304 "not modified" req
    | _ -> not_found req
  in
  Eio_mock.Backend.run @@ fun () ->
  Eio.Switch.run @@ fun sw ->
  match
    Client.connect ~sw ~auth
      (Transport.of_fetch (Fetch_mock.client server))
      well_known
  with
  | Error (Client.Http_error (304, _)) -> ()
  | Error e ->
      Alcotest.failf "expected HTTP 304, got %s" (Client.error_to_string e)
  | Ok _ -> Alcotest.fail "expected the session fetch to fail"

(* RFC 8620 3.6.1: a request-level error arrives as application/problem+json. *)
let test_problem_json () =
  let server req =
    match path req with
    | "/.well-known/jmap" -> respond_json (session_json ()) req
    | "/jmap/api/" ->
        Fetch_mock.respond ~status:400
          ~headers:(media_headers "application/problem+json")
          {|{ "type": "urn:ietf:params:jmap:error:unknownCapability",
              "status": 400,
              "detail": "unsupported capability" }|}
          req
    | _ -> not_found req
  in
  Eio_mock.Backend.run @@ fun () ->
  Eio.Switch.run @@ fun sw ->
  let client = client_exn ~sw server in
  match Client.request client (echo_request ()) with
  | Ok _ -> Alcotest.fail "expected a JMAP error"
  | Error (Client.Jmap_error err) ->
      check_string "urn" "urn:ietf:params:jmap:error:unknownCapability"
        (Jmap.Proto.Error.Request_error.type_to_string
           err.Jmap.Proto.Error.Request_error.type_);
      Alcotest.(check (option int)) "status" (Some 400) err.status;
      check_opt_string "detail" (Some "unsupported capability") err.detail
  | Error e -> Alcotest.failf "unexpected error: %s" (Client.error_to_string e)

(* An error that is not problem+json stays an HTTP error. *)
let test_http_error () =
  let server req =
    match path req with
    | "/.well-known/jmap" -> respond_json (session_json ()) req
    | "/jmap/api/" ->
        Fetch_mock.respond ~status:500
          ~headers:(media_headers "text/plain")
          "backend \027[2J on fire\nsecond line" req
    | _ -> not_found req
  in
  Eio_mock.Backend.run @@ fun () ->
  Eio.Switch.run @@ fun sw ->
  let client = client_exn ~sw server in
  match Client.request client (echo_request ()) with
  | Error (Client.Http_error (status, body)) ->
      Alcotest.(check int) "status" 500 status;
      check_string "body retained" "backend \027[2J on fire\nsecond line" body;
      let printed = Client.error_to_string (Client.Http_error (status, body)) in
      Alcotest.(check bool)
        "printed error has no escape byte" false
        (String.contains printed '\027');
      Alcotest.(check bool)
        "printed error is one line" false
        (String.contains printed '\n')
  | Ok _ -> Alcotest.fail "expected an HTTP error"
  | Error e -> Alcotest.failf "unexpected error: %s" (Client.error_to_string e)

(* RFC 8620 6.1: uploadUrl is a level-1 template with an accountId. *)
let test_upload () =
  let log = ref [] in
  let server req =
    ignore (record log req);
    match path req with
    | "/.well-known/jmap" -> respond_json (session_json ()) req
    | "/jmap/upload/acc1/" ->
        respond_json
          {|{ "accountId": "acc1", "blobId": "B1", "type": "text/plain", "size": 5 }|}
          req
    | _ -> not_found req
  in
  Eio_mock.Backend.run @@ fun () ->
  Eio.Switch.run @@ fun sw ->
  let client = client_exn ~sw server in
  let response =
    Client.upload_exn client ~account_id ~content_type:"text/plain"
      ~data:"hello"
  in
  check_string "blob id" "B1"
    (Jmap.Proto.Id.to_string response.Jmap.Proto.Blob.blob_id);
  match !log with
  | [ _; upload ] ->
      check_string "method" "POST" upload.meth;
      check_string "url" "https://api.example.com/jmap/upload/acc1/" upload.url;
      check_opt_string "content type" (Some "text/plain") upload.content_type;
      check_opt_string "credential" bearer upload.authorization;
      check_string "body" "hello" upload.body
  | l -> Alcotest.failf "expected 2 requests, got %d" (List.length l)

(* RFC 8620 6.2 with RFC 6570 level-1 expansion: every character outside the
   unreserved set is percent-encoded, so a name keeps its space encoded and a
   type of "image/png" becomes "image%2Fpng". Fetch preserves that encoded
   reserved character in both query and path positions. *)
let download_case ~download ~expect () =
  let log = ref [] in
  let server req =
    ignore (record log req);
    match path req with
    | "/.well-known/jmap" -> respond_json (session_json ~download ()) req
    | p when String.starts_with ~prefix:"/jmap/download/" p ->
        Fetch_mock.respond ~headers:(media_headers "image/png") "PNGDATA" req
    | _ -> not_found req
  in
  Eio_mock.Backend.run @@ fun () ->
  Eio.Switch.run @@ fun sw ->
  let client = client_exn ~sw server in
  let data =
    Client.download_exn client ~account_id ~blob_id ~name:"my photo.png"
      ~accept:"image/png" ()
  in
  check_string "body" "PNGDATA" data;
  match !log with
  | [ _; download ] ->
      check_string "method" "GET" download.meth;
      check_string "expanded url" expect download.url;
      check_opt_string "credential" bearer download.authorization
  | l -> Alcotest.failf "expected 2 requests, got %d" (List.length l)

let test_download () =
  download_case ~download:default_download
    ~expect:
      "https://api.example.com/jmap/download/acc1/B1/my%20photo.png?type=image%2Fpng"
    ();
  download_case ~download:"/jmap/download/{accountId}/{blobId}/{type}/{name}"
    ~expect:
      "https://api.example.com/jmap/download/acc1/B1/image%2Fpng/my%20photo.png"
    ()

let origin_of url =
  match Fetch.Middleware.Url.of_string url with
  | Ok u -> Fetch.Middleware.Url.origin u
  | Error msg -> Alcotest.failf "not a URL: %s" msg

(* The credential covers every origin the session names, and no others: a
   download that redirects off to a CDN loses the token. *)
let test_credential_scope () =
  let log = ref [] in
  let server req =
    ignore (record log req);
    match (origin req, path req) with
    | "https://api.example.com", "/.well-known/jmap" ->
        respond_json (session_json ~blobs:"https://blobs.example.org" ()) req
    | "https://blobs.example.org", _ ->
        redirect ~location:"https://cdn.example.net/b/B1" req
    | "https://cdn.example.net", _ -> Fetch_mock.respond "BLOB" req
    | _ -> not_found req
  in
  Eio_mock.Backend.run @@ fun () ->
  Eio.Switch.run @@ fun sw ->
  let client = client_exn ~sw server in
  let data = Client.download_exn client ~account_id ~blob_id () in
  check_string "body" "BLOB" data;
  match !log with
  | [ _; blobs; cdn ] ->
      check_string "blob host" "https://blobs.example.org" (origin_of blobs.url);
      check_opt_string "in-scope origin keeps the credential" bearer
        blobs.authorization;
      check_string "cdn host" "https://cdn.example.net" (origin_of cdn.url);
      check_opt_string "out-of-scope origin gets none" None cdn.authorization
  | l -> Alcotest.failf "expected 3 requests, got %d" (List.length l)

(* The standard transport uses one jar for all advertised endpoints. Cookie
   selection must still be by host/domain and path, not merely by that shared
   jar identity. Persistent jars must also replace through a private file. *)
let test_cookie_origin_isolation () =
  Eio_main.run @@ fun env ->
  Eio.Switch.run @@ fun sw ->
  let clock = Eio.Stdenv.clock env in
  let jar = Fetch_cookies.Jar.in_memory ~clock () in
  let api_cookies = ref [] in
  let server (req : Fetch.Middleware.request) =
    match (origin req, path req) with
    | "https://session.example", "/.well-known/jmap" ->
        respond_json
          (session_json ~origin:"https://api.example"
             ~blobs:"https://evil.example" ())
          req
    | "https://evil.example", p
      when String.starts_with ~prefix:"/jmap/download/" p ->
        Fetch_mock.respond
          ~headers:
            (Http.Header.of_list
               [
                 ("content-type", "text/plain");
                 ("set-cookie", "evil=1; Path=/");
                 ("set-cookie", "tossed=1; Domain=api.example; Path=/");
               ])
          "blob" req
    | "https://api.example", "/jmap/api/" ->
        api_cookies := Http.Header.get req.headers "cookie" :: !api_cookies;
        respond_json echo_response req
    | _ -> not_found req
  in
  let fetch = Fetch_cookies.with_jar jar (Fetch_mock.client server) in
  let client =
    match
      Client.connect ~sw ~auth (Transport.of_fetch fetch)
        "https://session.example/.well-known/jmap"
    with
    | Ok client -> client
    | Error error ->
        Alcotest.failf "connect failed: %s" (Client.error_to_string error)
  in
  check_string "download" "blob"
    (Client.download_exn client ~account_id ~blob_id ());
  ignore (Client.request_exn client (echo_request ()));
  check_opt_string "evil host keeps its cookie" (Some "evil=1")
    (Fetch_cookies.Jar.header_for jar "https://evil.example/");
  check_opt_string "the hostile Domain cookie was rejected" None
    (Fetch_cookies.Jar.header_for jar "https://api.example/");
  Alcotest.(check (list (option string)))
    "no hostile cookie reaches the API origin" [ None ] (List.rev !api_cookies);
  let root = Filename.temp_dir ~perms:0o700 "jmap-cookie-" "-jar" in
  let root_path = Eio.Path.(Eio.Stdenv.fs env / root) in
  Fun.protect
    ~finally:(fun () -> Eio.Path.rmtree ~missing_ok:true root_path)
    (fun () ->
      let cookie_file = Filename.concat root "cookies.txt" in
      let persistent =
        Fetch_cookies.Jar.of_file ~clock ~save:`Manual
          Eio.Path.(Eio.Stdenv.fs env / cookie_file)
      in
      Fetch_cookies.Jar.set persistent "https://api.example/" "sid=private";
      Fetch_cookies.Jar.flush persistent;
      Alcotest.(check int)
        "persistent jar has no group/other permissions" 0
        ((Unix.stat cookie_file).st_perm land 0o077))

(* A refreshed session replaces the advertised endpoint allowlist. Otherwise
   an endpoint removed after compromise or migration would retain the bearer
   token for the lifetime of the client. *)
let test_refresh_replaces_credential_scope () =
  let log = ref [] in
  let sessions = ref 0 in
  let old_origin = "https://old-blobs.example.net" in
  let new_origin = "https://new-blobs.example.net" in
  let server req =
    ignore (record log req);
    match (origin req, path req) with
    | "https://api.example.com", "/.well-known/jmap" ->
        incr sessions;
        let blobs = if !sessions = 1 then old_origin else new_origin in
        respond_json (session_json ~blobs ()) req
    | ( ("https://old-blobs.example.net" | "https://new-blobs.example.net"),
        "/probe" ) ->
        Fetch_mock.respond "ok" req
    | _ -> not_found req
  in
  let probe client url =
    match Client.with_get client (url ^ "/probe") (fun _ -> Ok ()) with
    | Ok () -> ()
    | Error e -> Alcotest.failf "probe failed: %s" (Client.error_to_string e)
  in
  Eio_mock.Backend.run @@ fun () ->
  Eio.Switch.run @@ fun sw ->
  let client = client_exn ~sw server in
  probe client old_origin;
  Client.refresh_session_exn client;
  probe client old_origin;
  probe client new_origin;
  match !log with
  | [ _session; before; _refresh; after; replacement ] ->
      check_opt_string "old endpoint initially has the credential" bearer
        before.authorization;
      check_opt_string "removed endpoint loses the credential" None
        after.authorization;
      check_opt_string "replacement endpoint has the credential" bearer
        replacement.authorization
  | requests ->
      Alcotest.failf "expected five requests, got %d" (List.length requests)

(* A caller can put an outer Fetch URL boundary around the authenticated
   session document instead of accepting every origin it advertises. *)
let test_transport_restricts_session_endpoints () =
  let requests = ref 0 in
  let server req =
    incr requests;
    match path req with
    | "/.well-known/jmap" ->
        respond_json (session_json ~origin:"https://elsewhere.example" ()) req
    | _ -> respond_json echo_response req
  in
  Eio_mock.Backend.run @@ fun () ->
  Eio.Switch.run @@ fun sw ->
  let transport =
    Transport.of_fetch (Fetch_mock.client server)
    |> Transport.restrict ~under:[ "https://api.example.com" ]
  in
  let client =
    match Client.connect ~sw ~auth transport well_known with
    | Ok client -> client
    | Error e -> Alcotest.failf "connect failed: %s" (Client.error_to_string e)
  in
  (match Client.request client (echo_request ()) with
  | Error (Client.Transport (Fetch.Denied _, _)) -> ()
  | Error e ->
      Alcotest.failf "expected Denied, got %s" (Client.error_to_string e)
  | Ok _ -> Alcotest.fail "the advertised origin escaped the allowlist");
  Alcotest.(check int)
    "only the allowed session request reached the backend" 1 !requests

(* A transport failure becomes a {!Client.Transport} rather than escaping. *)
let test_connection_error () =
  let server req =
    match path req with
    | "/.well-known/jmap" -> respond_json (session_json ()) req
    | _ -> raise (Fetch.err (Fetch.Connection_failure Eio.Net.Timeout))
  in
  Eio_mock.Backend.run @@ fun () ->
  Eio.Switch.run @@ fun sw ->
  let client = client_exn ~sw server in
  let msg = error_string (Client.request client (echo_request ())) in
  Alcotest.(check bool)
    ("connection error: " ^ msg)
    true
    (String.starts_with ~prefix:"Connection error:" msg);
  let server _ = raise (Unix.Unix_error (Unix.EPERM, "socket", "")) in
  match
    Client.connect ~sw ~auth
      (Transport.of_fetch (Fetch_mock.client server))
      well_known
  with
  | Error (Client.Transport (Fetch.Connection_failure _, _)) -> ()
  | Error error ->
      Alcotest.failf "raw Unix error was misclassified: %s"
        (Client.error_to_string error)
  | Ok _ -> Alcotest.fail "raw Unix connection failure was accepted"

let test_codec_depth_limit () =
  (match Codec.decode ~max_depth:3 Jsont.json {|[[[]]]|} with
  | Ok _ -> ()
  | Error error ->
      Alcotest.failf "exact codec depth was rejected: %s"
        (Jsont.Error.to_string error));
  (match Codec.decode ~max_depth:2 Jsont.json {|[[[]]]|} with
  | Error _ -> ()
  | Ok _ -> Alcotest.fail "over-deep convenience decode was accepted");
  match Codec.decode ~max_depth:0 Jsont.json {|"scalar"|} with
  | Ok _ -> ()
  | Error error ->
      Alcotest.failf "scalar at depth zero was rejected: %s"
        (Jsont.Error.to_string error)

(* Response bodies are bounded by [max_body]. *)
let test_body_limit () =
  let server req =
    match path req with
    | "/.well-known/jmap" -> respond_json (session_json ()) req
    | _ -> respond_json (String.make 100_000 'x') req
  in
  Eio_mock.Backend.run @@ fun () ->
  Eio.Switch.run @@ fun sw ->
  let client = client_exn ~sw ~max_body:4096 server in
  let msg = error_string (Client.request client (echo_request ())) in
  Alcotest.(check bool)
    ("body limit: " ^ msg) true
    (String.starts_with ~prefix:"Connection error: response body exceeds" msg)

let test_malformed_json_response () =
  let server req =
    match path req with
    | "/.well-known/jmap" -> respond_json (session_json ()) req
    | _ -> respond_json {|{"methodResponses":|} req
  in
  Eio_mock.Backend.run @@ fun () ->
  Eio.Switch.run @@ fun sw ->
  let client = client_exn ~sw server in
  match Client.request client (echo_request ()) with
  | Error (Client.Json_error _) -> ()
  | Error e ->
      Alcotest.failf "expected Json_error, got %s" (Client.error_to_string e)
  | Ok _ -> Alcotest.fail "malformed JSON was accepted"

let test_json_nesting_limit () =
  let nested depth = String.make depth '[' ^ "0" ^ String.make depth ']' in
  let server req =
    match path req with
    | "/.well-known/jmap" -> respond_json (session_json ()) req
    | _ -> respond_json (nested (Fetch.Json.default_max_depth + 1)) req
  in
  Eio_mock.Backend.run @@ fun () ->
  Eio.Switch.run @@ fun sw ->
  let client = client_exn ~sw server in
  match Client.request client (echo_request ()) with
  | Error (Client.Json_error e) ->
      Alcotest.(check bool)
        "depth is named" true
        (contains ~needle:"JSON nesting deeper than" (Jsont.Error.to_string e))
  | Error e ->
      Alcotest.failf "expected Json_error, got %s" (Client.error_to_string e)
  | Ok _ -> Alcotest.fail "over-deep JSON was accepted"

(* RFC 8620 2 leaves the session's URLs as plain strings and Cyrus sends
   absolute paths, so they are resolved against the session resource. Three of
   them are RFC 6570 templates, and resolution must not touch the braces: a
   percent-encoded "%7BaccountId%7D" is a literal path segment, not a
   variable, and nothing would ever expand it. *)
let relative_session_json =
  Printf.sprintf
    {|{
  "capabilities": { "urn:ietf:params:jmap:core": %s },
  "accounts": {
    "acc1": {
      "name": "Test Account",
      "isPersonal": true,
      "isReadOnly": false,
      "accountCapabilities": {}
    }
  },
  "primaryAccounts": { "urn:ietf:params:jmap:core": "acc1" },
  "username": "test@example.com",
  "apiUrl": "/jmap/",
  "downloadUrl": "/jmap/download/{accountId}/{blobId}/{name}?accept={type}",
  "uploadUrl": "/jmap/upload/{accountId}/",
  "eventSourceUrl": "/jmap/eventsource/?types={types}&closeafter={closeafter}&ping={ping}",
  "state": "state-1"
}|}
    default_core

let test_relative_templates () =
  let log = ref [] in
  let server req =
    ignore (record log req);
    match path req with
    | "/.well-known/jmap" -> respond_json relative_session_json req
    | "/jmap/upload/acc1/" ->
        respond_json
          {|{ "accountId": "acc1", "blobId": "B1", "type": "text/plain", "size": 2 }|}
          req
    | p when String.starts_with ~prefix:"/jmap/download/" p ->
        Fetch_mock.respond ~headers:(media_headers "text/plain") "hi" req
    | _ -> not_found req
  in
  Eio_mock.Backend.run @@ fun () ->
  Eio.Switch.run @@ fun sw ->
  let client = client_exn ~sw server in
  let host = "https://api.example.com" in
  check_string "api url" (host ^ "/jmap/") (Client.api_url client);
  check_string "upload template"
    (host ^ "/jmap/upload/{accountId}/")
    (Client.upload_url client);
  check_string "download template"
    (host ^ "/jmap/download/{accountId}/{blobId}/{name}?accept={type}")
    (Client.download_url client);
  check_string "event source template"
    (host
   ^ "/jmap/eventsource/?types={types}&closeafter={closeafter}&ping={ping}")
    (Client.event_source_url client);
  check_string "typed upload template" (Client.upload_url client)
    (Httpz_uri.Template.to_string (Client.upload_template client));
  check_string "typed download template"
    (Client.download_url client)
    (Httpz_uri.Template.to_string (Client.download_template client));
  check_string "typed event source template"
    (Client.event_source_url client)
    (Httpz_uri.Template.to_string (Client.event_source_template client));
  check_string "expanded event source"
    (host ^ "/jmap/eventsource/?types=Email&closeafter=no&ping=0")
    (Push.event_source_url client ~types:[ "Email" ] ());
  (* The templates are not just printable: they expand, and the requests go to
     the expanded paths rather than to a literal "%7BaccountId%7D". *)
  ignore
    (Client.upload_exn client ~account_id ~content_type:"text/plain" ~data:"hi");
  check_string "downloaded" "hi"
    (Client.download_exn client ~account_id ~blob_id ~name:"x.txt" ());
  match !log with
  | [ _; upload; download ] ->
      check_string "upload url" (host ^ "/jmap/upload/acc1/") upload.url;
      check_string "download url"
        (host ^ "/jmap/download/acc1/B1/x.txt?accept=application%2Foctet-stream")
        download.url
  | l -> Alcotest.failf "expected 3 requests, got %d" (List.length l)

let test_session_template_validation () =
  let cases =
    [
      ( "malformed uploadUrl",
        session_json ~upload:"/upload/{accountId" (),
        "unclosed template expression" );
      ( "non-Level-1 uploadUrl",
        session_json ~upload:"/upload/{+accountId}" (),
        "uploadUrl is invalid: uses Level 2" );
      ( "missing upload variable",
        session_json ~upload:"/upload/static" (),
        "uploadUrl is invalid: does not contain required variable accountId" );
      ( "missing download variable",
        session_json ~download:"/download/{accountId}/{blobId}/{type}/static" (),
        "downloadUrl is invalid: does not contain required variable name" );
      ( "origin-changing event variable",
        session_json ~origin:""
          ~event_source:"https://{types}.example.com/events/{closeafter}/{ping}"
          (),
        "eventSourceUrl is invalid: places a variable in the URL origin" );
      ( "invalid apiUrl",
        session_json ~api:"https://exa mple.com/api" (),
        "apiUrl is invalid" );
      ( "non-HTTP apiUrl",
        session_json ~origin:"" ~api:"ftp://files.example/api" (),
        "JMAP endpoints must use HTTP or HTTPS" );
      ( "non-HTTP uploadUrl",
        session_json ~blobs:"" ~upload:"gopher://files.example/{accountId}" (),
        "JMAP endpoints must use HTTP or HTTPS" );
    ]
  in
  List.iter
    (fun (name, body, expected) ->
      let server req = respond_json body req in
      Eio_mock.Backend.run @@ fun () ->
      Eio.Switch.run @@ fun sw ->
      match
        Client.connect ~sw ~auth
          (Transport.of_fetch (Fetch_mock.client server))
          well_known
      with
      | Error (Client.Session_error message) ->
          Alcotest.(check bool)
            (name ^ ": " ^ message)
            true
            (contains ~needle:expected message)
      | Error error ->
          Alcotest.failf "%s: expected Session_error, got %s" name
            (Client.error_to_string error)
      | Ok _ -> Alcotest.failf "%s: invalid session was accepted" name)
    cases

let test_literal_encoded_template_brace () =
  let body =
    session_json
      ~download:"/jmap/%7B/download/{accountId}/{blobId}/{name}?accept={type}"
      ()
  in
  let server req = respond_json body req in
  Eio_mock.Backend.run @@ fun () ->
  Eio.Switch.run @@ fun sw ->
  let client = client_exn ~sw server in
  check_string "literal brace remains encoded"
    "https://api.example.com/jmap/%7B/download/{accountId}/{blobId}/{name}?accept={type}"
    (Client.download_url client)

(* {1 Push over the event source (RFC 8620 7.3)} *)

(* RFC 8620 7.3 names two events; anything else, and anything that does not
   decode, is reported verbatim rather than dropped. The framing that produced
   the event is Fetch's and is tested there. *)
let sse ?(name = "message") data =
  { Fetch.Sse.name; data; id = None; retry = None }

let test_sse_decode () =
  (match
     Push.decode
       (sse ~name:"state"
          "{\"@type\":\"StateChange\",\"changed\":{\"acc1\":{\"Email\":\"s1\",\"Mailbox\":\"s2\"}}}")
   with
  | Push.State_change change ->
      let types =
        match change.Jmap.Proto.Push.State_change.changed with
        | [ (id, types) ] ->
            check_string "account" "acc1" (Jmap.Proto.Id.to_string id);
            types
        | l -> Alcotest.failf "expected one account, got %d" (List.length l)
      in
      Alcotest.(check (list (pair string string)))
        "type states"
        [ ("Email", "s1"); ("Mailbox", "s2") ]
        (List.map
           (fun ts ->
             ( ts.Jmap.Proto.Push.State_change.type_name,
               ts.Jmap.Proto.Push.State_change.state ))
           types)
  | e -> Alcotest.failf "expected a state change, got %a" Push.pp_event e);
  (match Push.decode (sse ~name:"ping" "{\"interval\":42}") with
  | Push.Ping { interval } -> Alcotest.(check int64) "interval" 42L interval
  | e -> Alcotest.failf "expected a ping, got %a" Push.pp_event e);
  (match Push.decode (sse "hello") with
  | Push.Unknown ("message", "hello") -> ()
  | e -> Alcotest.failf "expected an unnamed event, got %a" Push.pp_event e);
  (* A "state" event whose data is not a StateChange is not silently lost. *)
  (match Push.decode (sse ~name:"state" "{\"@type\":\"Nope\"}") with
  | Push.Unknown ("state", data) ->
      check_string "undecodable data is kept" "{\"@type\":\"Nope\"}" data
  | e -> Alcotest.failf "expected an unknown event, got %a" Push.pp_event e);
  (* Unknown members are normally ignored by the typed ping codec. The shared
     Fetch JSON depth guard still examines them before Jsont recurses. *)
  let depth = Fetch.Json.default_max_depth in
  let nested = String.make depth '[' ^ "0" ^ String.make depth ']' in
  let data = Printf.sprintf {|{"interval":42,"ignored":%s}|} nested in
  match Push.decode (sse ~name:"ping" data) with
  | Push.Unknown ("ping", kept) ->
      check_string "over-deep data is kept" data kept
  | e ->
      Alcotest.failf "expected an over-deep event to be unknown, got %a"
        Push.pp_event e

(* RFC 8620 7.3: eventSourceUrl is a level-1 template over "types",
   "closeafter" and "ping". Level-1 expansion percent-encodes, so a list
   arrives with %2C separators and "*" as %2A. *)
let test_event_source_url () =
  let server req =
    match path req with
    | "/.well-known/jmap" -> respond_json (session_json ()) req
    | _ -> not_found req
  in
  Eio_mock.Backend.run @@ fun () ->
  Eio.Switch.run @@ fun sw ->
  let client = client_exn ~sw server in
  let base = "https://api.example.com/jmap/eventsource/" in
  check_string "defaults"
    (base ^ "?types=%2A&closeafter=no&ping=0")
    (Push.event_source_url client ());
  check_string "all three"
    (base ^ "?types=Email%2CMailbox&closeafter=state&ping=30")
    (Push.event_source_url client ~types:[ "Email"; "Mailbox" ]
       ~close_after:`State ~ping:30 ());
  check_string "an empty list is every type"
    (base ^ "?types=%2A&closeafter=no&ping=5")
    (Push.event_source_url client ~types:[] ~close_after:`No ~ping:5 ());
  Alcotest.check_raises "a negative ping cannot enter the URL"
    (Invalid_argument
       "Jmap_eio.Push.event_source_url: ?ping must be between 0 and 2^53-1 \
        seconds") (fun () ->
      ignore (Push.event_source_url client ~ping:(-1) ()));
  match Push.listen client ~ping:(-1) (fun _ -> `Continue) with
  | Error (Client.Transport (Fetch.Invalid_request _, _)) -> ()
  | Error e ->
      Alcotest.failf "unexpected negative-ping error: %s"
        (Client.error_to_string e)
  | Ok () -> Alcotest.fail "a negative ping was accepted"

(* A response whose body arrives in pieces, ending in an action that fails the
   test: an implementation that waited for the whole body before dispatching,
   or that read on past a [`Stop], would trip over it. *)
let respond_chunks ?(status = 200) chunks req =
  let flow = Eio_mock.Flow.make "event-stream" in
  Eio_mock.Flow.on_read flow
    (List.map (fun c -> `Return c) chunks
    @ [ `Raise (Failure "read past the end of the events under test") ]);
  Fetch.Middleware.Pi.response
    ~close:(fun () -> ())
    ~status
    ~headers:(media_headers "text/event-stream")
    ~version:`HTTP_1_1
    ~body:(flow :> Eio.Flow.source_ty Eio.Resource.t)
    ~url:req.Fetch.Middleware.url ()

let event_stream_server chunks log req =
  ignore (record log req);
  match path req with
  | "/.well-known/jmap" -> respond_json (session_json ()) req
  | p when String.starts_with ~prefix:"/jmap/eventsource/" p ->
      respond_chunks chunks req
  | _ -> not_found req

(* Events are dispatched one at a time as the bytes arrive, across chunk
   boundaries that fall inside a line, and [`Stop] ends the stream there. *)
let test_listen_dispatch () =
  let log = ref [] in
  let chunks =
    [
      ": hi\nevent: pin";
      "g\ndata: {\"interval\":5}\n\nevent: state\ndata: {\"@type\":\"State";
      "Change\",\"changed\":{\"acc1\":{\"Email\":\"e1\"}}}\nid: 42\n\n";
      "event: ping\ndata: {\"interval\":5}\n\n";
    ]
  in
  Eio_mock.Backend.run @@ fun () ->
  Eio.Switch.run @@ fun sw ->
  let client = client_exn ~sw (event_stream_server chunks log) in
  let seen = ref [] in
  let result =
    Push.listen client ~types:[ "Email" ] ~ping:5 ~last_event_id:"7"
      (fun event ->
        seen := event :: !seen;
        match event with Push.State_change _ -> `Stop | _ -> `Continue)
  in
  (match result with
  | Ok () -> ()
  | Error e -> Alcotest.failf "listen failed: %s" (Client.error_to_string e));
  Alcotest.(check (list string))
    "the ping and the change, in order, and nothing after the stop"
    [ "Ping 5s"; "StateChange {acc1: Email=e1}" ]
    (List.rev_map (Fmt.str "%a" Push.pp_event) !seen);
  match !log with
  | [ _; stream ] ->
      check_string "url"
        "https://api.example.com/jmap/eventsource/?types=Email&closeafter=no&ping=5"
        stream.url;
      check_opt_string "credential" bearer stream.authorization;
      check_opt_string "accept" (Some "text/event-stream") stream.accept
  | l -> Alcotest.failf "expected 2 requests, got %d" (List.length l)

(* Exceptions raised by the consumer are the consumer's exceptions, even when
   their constructor also happens to be used for transport failures. *)
let test_listen_callback_exception_propagates () =
  let server req =
    match path req with
    | "/.well-known/jmap" -> respond_json (session_json ()) req
    | p when String.starts_with ~prefix:"/jmap/eventsource/" p ->
        Fetch_mock.respond
          ~headers:(media_headers "text/event-stream")
          "event: ping\ndata: {\"interval\":5}\n\n" req
    | _ -> not_found req
  in
  Eio_mock.Backend.run @@ fun () ->
  Eio.Switch.run @@ fun sw ->
  let client = client_exn ~sw server in
  (match
     Push.listen client (fun _ ->
         raise (Unix.Unix_error (Unix.EIO, "push callback", "mailbox")))
   with
  | exception Unix.Unix_error (Unix.EIO, "push callback", "mailbox") -> ()
  | exception exn ->
      Alcotest.failf "unexpected callback exception: %s"
        (Printexc.to_string exn)
  | Error e ->
      Alcotest.failf "callback was misclassified: %s" (Client.error_to_string e)
  | Ok () -> Alcotest.fail "the callback exception disappeared");
  let callback_error = Fetch.err (Fetch.Denied "callback identity") in
  match Push.listen client (fun _ -> raise callback_error) with
  | exception exn when exn == callback_error -> ()
  | exception exn ->
      Alcotest.failf "callback I/O identity changed: %s"
        (Printexc.to_string exn)
  | Error e ->
      Alcotest.failf "callback I/O was misclassified: %s"
        (Client.error_to_string e)
  | Ok () -> Alcotest.fail "the callback I/O exception disappeared"

(* Under closeafter=state the server ends the response by itself, so one
   connection delivers what it has and [listen] returns. The stream is
   scripted with [Fetch_mock.Sse] rather than written out by hand. *)
let test_listen_close_after () =
  let log = ref [] in
  let server req =
    ignore (record log req);
    match path req with
    | "/.well-known/jmap" -> respond_json (session_json ()) req
    | p when String.starts_with ~prefix:"/jmap/eventsource/" p ->
        Fetch_mock.Sse.respond
          (fun sink ->
            Fetch_mock.Sse.comment sink "keep-alive";
            Fetch_mock.Sse.send sink ~name:"state" ~id:"42"
              {|{"@type":"StateChange","changed":{}}|})
          req
    | _ -> not_found req
  in
  Eio_mock.Backend.run @@ fun () ->
  Eio.Switch.run @@ fun sw ->
  let client = client_exn ~sw server in
  let seen = ref [] in
  (match
     Push.listen client ~close_after:`State (fun event ->
         seen := event :: !seen;
         `Continue)
   with
  | Ok () -> ()
  | Error e -> Alcotest.failf "listen failed: %s" (Client.error_to_string e));
  Alcotest.(check (list string))
    "the state event, and the comment ignored" [ "StateChange {}" ]
    (List.rev_map (Fmt.str "%a" Push.pp_event) !seen);
  match !log with
  | [ _; stream ] ->
      check_string "url"
        "https://api.example.com/jmap/eventsource/?types=%2A&closeafter=state&ping=0"
        stream.url
  | l -> Alcotest.failf "expected 2 requests, got %d" (List.length l)

(* A stream that ends without a stop is not an error; a stream that never
   opens is. *)
let test_listen_errors () =
  let ended = ref [] in
  let server req =
    match path req with
    | "/.well-known/jmap" -> respond_json (session_json ()) req
    | p when String.starts_with ~prefix:"/jmap/eventsource/" p ->
        Fetch_mock.respond
          ~headers:(media_headers "text/event-stream")
          "event: ping\ndata: {\"interval\":1}\n\n" req
    | _ -> not_found req
  in
  Eio_mock.Backend.run @@ fun () ->
  Eio.Switch.run @@ fun sw ->
  let client = client_exn ~sw server in
  (match
     Push.listen client (fun e ->
         ended := e :: !ended;
         `Continue)
   with
  | Ok () -> Alcotest.(check int) "events before the end" 1 (List.length !ended)
  | Error e -> Alcotest.failf "listen failed: %s" (Client.error_to_string e));
  let refusing req =
    match path req with
    | "/.well-known/jmap" -> respond_json (session_json ()) req
    | _ ->
        Fetch_mock.respond ~status:404
          ~headers:(media_headers "text/plain")
          "no push here" req
  in
  let client = client_exn ~sw refusing in
  (match Push.listen client (fun _ -> `Continue) with
  | Error (Client.Http_error (404, body)) ->
      check_string "body" "no push here" body
  | Ok () -> Alcotest.fail "expected an HTTP error"
  | Error e -> Alcotest.failf "unexpected error: %s" (Client.error_to_string e));
  let oversized req =
    match path req with
    | "/.well-known/jmap" -> respond_json (session_json ()) req
    | _ ->
        Fetch_mock.respond ~status:502
          ~headers:(media_headers "text/plain")
          (String.make ((1024 * 1024) + 1) 'x')
          req
  in
  let client = client_exn ~sw oversized in
  match Push.listen client (fun _ -> `Continue) with
  | Error (Client.Http_error (502, body)) ->
      check_string "oversized diagnostic"
        "response body exceeds the 1048576-byte limit" body
  | Ok () -> Alcotest.fail "expected an oversized HTTP error"
  | Error e ->
      Alcotest.failf "unexpected oversized error: %s" (Client.error_to_string e)

let test_listen_body_error_context () =
  let server req =
    match path req with
    | "/.well-known/jmap" -> respond_json (session_json ()) req
    | p when String.starts_with ~prefix:"/jmap/eventsource/" p ->
        let flow = Eio_mock.Flow.make "failed event stream" in
        Eio_mock.Flow.on_read flow
          [ `Raise (Fetch.err (Fetch.Denied "event body unavailable")) ];
        Fetch.Middleware.Pi.response ~status:200
          ~headers:(media_headers "text/event-stream")
          ~version:`HTTP_1_1
          ~body:(flow :> Eio.Flow.source_ty Eio.Resource.t)
          ~close:(fun () -> ())
          ~url:req.Fetch.Middleware.url ()
    | _ -> not_found req
  in
  Eio_mock.Backend.run @@ fun () ->
  Eio.Switch.run @@ fun sw ->
  let client = client_exn ~sw server in
  match Push.listen client (fun _ -> `Continue) with
  | Error (Client.Transport (Fetch.Denied _, diagnostic)) ->
      List.iter
        (fun part ->
          Alcotest.(check bool)
            ("diagnostic contains " ^ part)
            true
            (contains ~needle:part diagnostic))
        [
          "reading JMAP event source";
          "https://api.example.com";
          "event body unavailable";
        ];
      Alcotest.(check bool)
        "diagnostic excludes the event-source path" false
        (contains ~needle:"/jmap/eventsource" diagnostic)
  | Error error ->
      Alcotest.failf "unexpected body error: %s" (Client.error_to_string error)
  | Ok () -> Alcotest.fail "the failed event body was accepted"

(* A platform may reject socket creation before Eio can wrap the Unix error.
   The result-returning one-shot listener must still return a transport error. *)
let test_listen_unix_error () =
  let server req =
    match path req with
    | "/.well-known/jmap" -> respond_json (session_json ()) req
    | _ -> raise (Unix.Unix_error (Unix.EPERM, "socket", ""))
  in
  Eio_mock.Backend.run @@ fun () ->
  Eio.Switch.run @@ fun sw ->
  let client = client_exn ~sw server in
  match Push.listen client (fun _ -> `Continue) with
  | Error (Client.Transport (Fetch.Protocol_error msg, _)) ->
      List.iter
        (fun part ->
          Alcotest.(check bool)
            ("raw Unix diagnostic contains " ^ part)
            true
            (contains ~needle:part msg))
        [ "socket"; "opening JMAP event source"; "https://api.example.com" ];
      Alcotest.(check bool)
        "raw Unix diagnostic excludes the event-source path" false
        (contains ~needle:"/jmap/eventsource" msg)
  | Error e ->
      Alcotest.failf "raw Unix error was misclassified: %s"
        (Client.error_to_string e)
  | Ok () -> Alcotest.fail "raw Unix event-source failure was accepted"

(* RFC 8620 6.2: the type the server actually served is the one to believe. *)
let test_download_with_type () =
  let server req =
    match path req with
    | "/.well-known/jmap" -> respond_json (session_json ()) req
    | p when String.starts_with ~prefix:"/jmap/download/" p ->
        Fetch_mock.respond
          ~headers:(media_headers "text/plain; charset=utf-8")
          "PLAIN" req
    | _ -> not_found req
  in
  Eio_mock.Backend.run @@ fun () ->
  Eio.Switch.run @@ fun sw ->
  let client = client_exn ~sw server in
  match
    Client.download_with_type client ~account_id ~blob_id ~accept:"image/png" ()
  with
  | Ok (media, data) ->
      check_string "served type" "text/plain" media;
      check_string "body" "PLAIN" data
  | Error e -> Alcotest.failf "download failed: %s" (Client.error_to_string e)

(* {1 Chains}

   RFC 8620 3.2: one request carries every method call, so Client.chain
   builds a chain and sends it in a single round trip, returning the handles
   to parse the response with. *)

let chain_response =
  {|{ "methodResponses": [
       [ "Email/query",
         { "accountId": "acc1", "queryState": "q1", "canCalculateChanges": false,
           "position": 0, "ids": ["M1"] },
         "c0" ],
       [ "error", { "type": "unsupportedSort" }, "c1" ] ],
     "sessionState": "state-1" }|}

let email_chain account_id =
  Jmap.Chain.(
    let* q = email_query ~account_id ~limit:1L () in
    let* g =
      email_get ~account_id ~ids:(from_query q) ~properties:[ `Id; `Subject ] ()
    in
    return (q, g))

let test_chain_round_trip () =
  let log = ref [] in
  let server req =
    ignore (record log req);
    match path req with
    | "/.well-known/jmap" -> respond_json (session_json ()) req
    | "/jmap/api/" -> respond_json chain_response req
    | _ -> not_found req
  in
  Eio_mock.Backend.run @@ fun () ->
  Eio.Switch.run @@ fun sw ->
  let client = client_exn ~sw server in
  match
    Client.chain client
      ~capabilities:[ Jmap.Proto.Capability.mail ]
      (email_chain account_id)
  with
  | Error e -> Alcotest.failf "chain failed: %s" (Client.error_to_string e)
  | Ok ((q, g), response) -> (
      check_string "session state" "state-1"
        response.Jmap.Proto.Response.session_state;
      (* Both calls of the chain went out in the one POST. *)
      let body = (List.nth !log 1).body in
      Alcotest.(check bool)
        "one request for the whole chain" true
        (List.length !log = 2);
      Alcotest.(check bool)
        "Email/query in the body" true
        (contains ~needle:{|"Email/query"|} body);
      Alcotest.(check bool)
        "Email/get in the body" true
        (contains ~needle:{|"Email/get"|} body);
      Alcotest.(check bool)
        "typed properties on the wire" true
        (contains ~needle:{|"properties":["id","subject"]|} body);
      Alcotest.(check bool)
        "back-reference on the wire" true
        (contains ~needle:{|"#ids"|} body);
      (* The first call answered; the second was refused. *)
      (match Jmap.Chain.parse q response with
      | Ok r -> check_string "queryState" "q1" r.Jmap.Proto.Method.query_state
      | Error e ->
          Alcotest.failf "Email/query: %s" (Jmap.Chain.parse_error_to_string e));
      Alcotest.(check (option string))
        "no error for Email/query" None
        (Option.map
           (fun e -> Jmap.Proto.Error.Method_error.to_string e)
           (Jmap.Chain.method_error q response));
      (match Jmap.Chain.method_error g response with
      | None -> Alcotest.fail "the Email/get error was not reported"
      | Some e ->
          check_string "error type" "unsupportedSort"
            (Jmap.Proto.Error.Method_error.type_to_string
               e.Jmap.Proto.Error.Method_error.type_));
      match Jmap.Chain.parse g response with
      | Ok _ -> Alcotest.fail "an error response was decoded as an Email/get"
      | Error (Jmap.Chain.Json_error e) ->
          Alcotest.failf "reported as a decode failure: %s"
            (Jsont.Error.to_string e)
      | Error (Jmap.Chain.Method_error _) -> ())

(* A transport failure is the chain's error, not a per-call one. *)
let test_chain_error () =
  let server req =
    match path req with
    | "/.well-known/jmap" -> respond_json (session_json ()) req
    | _ -> Fetch_mock.respond ~status:503 "busy" req
  in
  Eio_mock.Backend.run @@ fun () ->
  Eio.Switch.run @@ fun sw ->
  let client = client_exn ~sw server in
  let result =
    Client.chain client
      ~capabilities:[ Jmap.Proto.Capability.mail ]
      (email_chain account_id)
  in
  check_string "http error" "HTTP error 503: busy" (error_string result);
  check_string "a long body is cut to its first line"
    ("HTTP error 503: " ^ String.make 120 'x' ^ "...")
    (Client.error_to_string
       (Client.Http_error (503, String.make 200 'x' ^ "\n<html>")));
  Alcotest.check_raises "chain_exn raises"
    (Client.Jmap_client_error (Client.Http_error (503, "busy")))
    (fun () ->
      ignore
        (Client.chain_exn client
           ~capabilities:[ Jmap.Proto.Capability.mail ]
           (email_chain account_id)))

(* Chain builders execute when Client builds the request. Their documented
   Invalid_argument failures stay inside the result-returning client API and
   no malformed request reaches the server. *)
let test_invalid_chain_is_client_error () =
  let requests = ref 0 in
  let server req =
    incr requests;
    match path req with
    | "/.well-known/jmap" -> respond_json (session_json ()) req
    | _ -> Alcotest.fail "an invalid chain reached the server"
  in
  Eio_mock.Backend.run @@ fun () ->
  Eio.Switch.run @@ fun sw ->
  let client = client_exn ~sw server in
  let invalid =
    Jmap.Chain.mailbox_changes ~account_id ~since_state:"s0" ~max_changes:0L ()
  in
  (match Client.chain client invalid with
  | Error (Client.Transport (Fetch.Invalid_request _, _)) -> ()
  | Error e -> Alcotest.failf "unexpected error: %s" (Client.error_to_string e)
  | Ok _ -> Alcotest.fail "expected the invalid chain to fail");
  Alcotest.(check int) "only the session was fetched" 1 !requests

let test_client_enforces_advertised_sizes () =
  let expect_invalid name = function
    | Error (Client.Transport (Fetch.Invalid_request _, _)) -> ()
    | Error error ->
        Alcotest.failf "%s: unexpected error: %s" name
          (Client.error_to_string error)
    | Ok _ -> Alcotest.failf "%s: expected a local rejection" name
  in
  Eio_mock.Backend.run @@ fun () ->
  Eio.Switch.run @@ fun sw ->
  let make core =
    let log = ref [] in
    let server req =
      let entry = record log req in
      match path req with
      | "/.well-known/jmap" -> respond_json (session_json ~core ()) req
      | p when String.starts_with ~prefix:"/jmap/upload/" p -> (
          match req.Fetch.Middleware.body with
          | Fetch.Stream { flow; _ } ->
              ignore
                (Eio.Buf_read.take_all
                   (Eio.Buf_read.of_flow ~max_size:1024 flow));
              respond_json
                {|{"accountId":"acc1","blobId":"B1","type":"text/plain","size":0}|}
                req
          | Fetch.Empty | Fetch.String _ ->
              Alcotest.failf "an oversized upload reached the server: %s"
                entry.body)
      | "/jmap/api/" ->
          Alcotest.failf "an oversized request reached the server: %s"
            entry.body
      | _ -> not_found req
    in
    (client_exn ~sw server, log)
  in
  let calls_client, calls_log = make (core_json ~max_calls_in_request:1 ()) in
  expect_invalid "maxCallsInRequest"
    (Client.chain calls_client (email_chain account_id));
  Alcotest.(check int)
    "no over-wide request" 0
    (List.length (api_posts calls_log));
  let size_client, size_log = make (core_json ~max_size_request:1 ()) in
  expect_invalid "maxSizeRequest"
    (Client.call size_client (Jmap.Chain.echo (Jsont.Json.object' [])));
  Alcotest.(check int)
    "no oversized request" 0
    (List.length (api_posts size_log));
  let upload_client, upload_log = make (core_json ~max_size_upload:2 ()) in
  expect_invalid "known maxSizeUpload"
    (Client.upload upload_client ~account_id ~content_type:"text/plain"
       ~data:"abc");
  expect_invalid "declared maxSizeUpload"
    (Client.upload_flow upload_client ~account_id ~content_type:"text/plain"
       ~length:3L
       (Eio.Flow.string_source "abc"));
  expect_invalid "streamed maxSizeUpload"
    (Client.upload_flow upload_client ~account_id ~content_type:"text/plain"
       (Eio.Flow.string_source "abc"));
  Alcotest.(check int)
    "only the unknown-length upload starts" 1
    (List.length (api_posts upload_log))

(* {1 call and run}

   RFC 8620 3.6.2 answers a failed method call with a response named "error".
   Client.call and Client.run read the handles of a chain rather than handing
   back the response, so that failure becomes Client.Method_error. *)

let mail_only = [ Jmap.Proto.Capability.mail ]

let chain_both_response =
  {|{ "methodResponses": [
       [ "Email/query",
         { "accountId": "acc1", "queryState": "q1", "canCalculateChanges": false,
           "position": 0, "ids": ["M1"] },
         "c0" ],
       [ "Email/get",
         { "accountId": "acc1", "state": "g1",
           "list": [ { "id": "M1", "subject": "Hi" } ], "notFound": [] },
         "c1" ] ],
     "sessionState": "state-1" }|}

let one_handle_chain account_id =
  Jmap.Chain.(
    let* q = email_query ~account_id ~limit:1L () in
    email_get ~account_id ~ids:(from_query q) ~properties:[ `Id; `Subject ] ())

let two_handle_chain account_id =
  Jmap.Chain.(
    let* q = email_query ~account_id ~limit:1L () in
    let+ g =
      email_get ~account_id ~ids:(from_query q) ~properties:[ `Id; `Subject ] ()
    in
    Handles.[ q; g ])

let attempted_chain account_id =
  Jmap.Chain.(
    let* q = email_query ~account_id ~limit:1L () in
    let+ g =
      email_get ~account_id ~ids:(from_query q) ~properties:[ `Id; `Subject ] ()
    in
    Handles.[ q; attempt g ])

let chain_client ~sw ?(reply = chain_both_response) log =
  let server req =
    ignore (record log req);
    match path req with
    | "/.well-known/jmap" -> respond_json (session_json ()) req
    | "/jmap/api/" -> respond_json reply req
    | _ -> not_found req
  in
  client_exn ~sw server

(* The one handle a chain ends in is decoded and handed back on its own. *)
let test_call_decodes () =
  let log = ref [] in
  Eio_mock.Backend.run @@ fun () ->
  Eio.Switch.run @@ fun sw ->
  let client = chain_client ~sw log in
  match
    Client.call client ~capabilities:mail_only (one_handle_chain account_id)
  with
  | Error e -> Alcotest.failf "call failed: %s" (Client.error_to_string e)
  | Ok (got : Jmap.Proto.Email.t Jmap.Proto.Method.get_response) ->
      Alcotest.(check int)
        "one request for the whole chain" 1
        (List.length (api_posts log));
      check_string "the Email/get state" "g1" got.state;
      check_opt_string "the subject of the one Email" (Some "Hi")
        (match got.list with
        | [ e ] -> e.Jmap.Proto.Email.subject
        | _ -> Some "<not one Email>");
      Alcotest.(check int)
        "and call_exn is the same value" 1
        (List.length
           (Client.call_exn client ~capabilities:mail_only
              (one_handle_chain account_id))
             .list)

(* A call the server answered with an error is Method_error, which prints as
   the error type does. The request itself succeeded. *)
let test_call_method_error () =
  let log = ref [] in
  Eio_mock.Backend.run @@ fun () ->
  Eio.Switch.run @@ fun sw ->
  let client = chain_client ~sw ~reply:chain_response log in
  (match
     Client.call client ~capabilities:mail_only (one_handle_chain account_id)
   with
  | Ok _ -> Alcotest.fail "an error response was decoded as an Email/get"
  | Error (Client.Method_error e) ->
      check_string "the error type" "unsupportedSort"
        (Jmap.Proto.Error.Method_error.type_to_string
           e.Jmap.Proto.Error.Method_error.type_)
  | Error e ->
      Alcotest.failf "reported as something else: %s" (Client.error_to_string e));
  check_string "printed as the method error" "unsupportedSort"
    (error_string
       (Client.call client ~capabilities:mail_only
          (one_handle_chain account_id)));
  match
    Client.call_exn client ~capabilities:mail_only (one_handle_chain account_id)
  with
  | _ -> Alcotest.fail "call_exn returned an error response"
  | exception Client.Jmap_client_error (Client.Method_error e) ->
      check_string "call_exn raises it" "unsupportedSort"
        (Jmap.Proto.Error.Method_error.type_to_string
           e.Jmap.Proto.Error.Method_error.type_)

(* A request that never completed is the error it always was. *)
let test_call_transport_error () =
  let server req =
    match path req with
    | "/.well-known/jmap" -> respond_json (session_json ()) req
    | _ -> Fetch_mock.respond ~status:503 "busy" req
  in
  Eio_mock.Backend.run @@ fun () ->
  Eio.Switch.run @@ fun sw ->
  let client = client_exn ~sw server in
  match
    Client.call client ~capabilities:mail_only (one_handle_chain account_id)
  with
  | Ok _ -> Alcotest.fail "expected an error"
  | Error (Client.Http_error (503, "busy")) -> ()
  | Error e ->
      Alcotest.failf "the transport error was rewritten: %s"
        (Client.error_to_string e)

(* Two handles, two results, in the order the chain names them. *)
let test_run_two_handles () =
  let log = ref [] in
  Eio_mock.Backend.run @@ fun () ->
  Eio.Switch.run @@ fun sw ->
  let client = chain_client ~sw log in
  match
    Client.run client ~capabilities:mail_only (two_handle_chain account_id)
  with
  | Error e -> Alcotest.failf "run failed: %s" (Client.error_to_string e)
  | Ok Jmap.Chain.Results.[ query; got ] ->
      Alcotest.(check int)
        "one request for both calls" 1
        (List.length (api_posts log));
      check_string "the Email/query state" "q1" query.query_state;
      check_string "the Email/get state" "g1" got.state;
      Alcotest.(check int)
        "the ids the query answered" 1 (List.length query.ids);
      Alcotest.(check int)
        "the Emails the get answered" 1 (List.length got.list)

(* The response is handed back beside the results, so a caller can also read a
   response no handle of the chain stands for. *)
let test_run_with_response () =
  let log = ref [] in
  Eio_mock.Backend.run @@ fun () ->
  Eio.Switch.run @@ fun sw ->
  let client = chain_client ~sw log in
  (match
     Client.run_with_response client ~capabilities:mail_only
       (two_handle_chain account_id)
   with
  | Error e ->
      Alcotest.failf "run_with_response failed: %s" (Client.error_to_string e)
  | Ok (_, Jmap.Chain.Results.[ query; got ], response) -> (
      Alcotest.(check int)
        "one request for both calls" 1
        (List.length (api_posts log));
      check_string "the Email/query state" "q1" query.query_state;
      check_string "the Email/get state" "g1" got.state;
      check_string "the session state of the response" "state-1"
        response.Jmap.Proto.Response.session_state;
      match Jmap.Proto.Response.find_responses "c1" response with
      | [ inv ] ->
          check_string "the raw response of the second call" "Email/get"
            inv.Jmap.Proto.Invocation.name
      | l -> Alcotest.failf "%d responses for the second call" (List.length l)));
  let client = chain_client ~sw ~reply:chain_response log in
  match
    Client.run_with_response_exn client ~capabilities:mail_only
      (two_handle_chain account_id)
  with
  | _ -> Alcotest.fail "run_with_response_exn returned an error response"
  | exception Client.Jmap_client_error (Client.Method_error e) ->
      check_string "run_with_response_exn raises the method error"
        "unsupportedSort"
        (Jmap.Proto.Error.Method_error.type_to_string
           e.Jmap.Proto.Error.Method_error.type_)

(* An attempt-wrapped handle keeps its own failure, so the handle beside it is
   still read. Without the wrapper the whole read fails. *)
let test_run_attempt () =
  let log = ref [] in
  Eio_mock.Backend.run @@ fun () ->
  Eio.Switch.run @@ fun sw ->
  let client = chain_client ~sw ~reply:chain_response log in
  (match
     Client.run client ~capabilities:mail_only (attempted_chain account_id)
   with
  | Error e ->
      Alcotest.failf "the attempt did not survive: %s"
        (Client.error_to_string e)
  | Ok Jmap.Chain.Results.[ query; got ] -> (
      check_string "the call that succeeded was read" "q1" query.query_state;
      match got with
      | Ok _ -> Alcotest.fail "the Email/get error was swallowed"
      | Error e ->
          check_string "and the one that failed carries its error"
            "unsupportedSort"
            (Jmap.Proto.Error.Method_error.type_to_string
               e.Jmap.Proto.Error.Method_error.type_)));
  match
    Client.run client ~capabilities:mail_only (two_handle_chain account_id)
  with
  | Ok _ -> Alcotest.fail "an unwrapped error response was read"
  | Error e ->
      check_string "unwrapped, the same error fails the whole read"
        "unsupportedSort" (Client.error_to_string e)

(* RFC 8620 3.2: "using" names the capabilities the method calls come from.
   Client.chain builds it from the session when the caller names none. *)
let capability_session_json ~state capabilities =
  Printf.sprintf
    {|{
  "capabilities": { %s },
  "accounts": {
    "acc1": {
      "name": "Test Account",
      "isPersonal": true,
      "isReadOnly": false,
      "accountCapabilities": {}
    }
  },
  "primaryAccounts": { "urn:ietf:params:jmap:core": "acc1" },
  "username": "test@example.com",
  "apiUrl": "https://api.example.com/jmap/api/",
  "downloadUrl": "https://api.example.com%s",
  "uploadUrl": "https://api.example.com/jmap/upload/{accountId}/",
  "eventSourceUrl": "https://api.example.com%s",
  "state": "%s"
}|}
    (String.concat ",\n    "
       (List.map
          (fun uri ->
            let value =
              if String.equal uri Jmap.Proto.Capability.core then default_core
              else "{}"
            in
            Printf.sprintf "%S: %s" uri value)
          capabilities))
    default_download default_event_source state

let using_needle capabilities = {|"using":[|} ^ quoted capabilities ^ "]"

let test_chain_default_capabilities () =
  let log = ref [] in
  let fetches = ref 0 in
  let server req =
    ignore (record log req);
    match path req with
    | "/.well-known/jmap" ->
        incr fetches;
        let capabilities =
          if !fetches = 1 then
            [
              Jmap.Proto.Capability.vacation_response;
              "urn:example:unknown";
              Jmap.Proto.Capability.core;
              Jmap.Proto.Capability.mail;
            ]
          else [ Jmap.Proto.Capability.core; Jmap.Proto.Capability.submission ]
        in
        respond_json (capability_session_json ~state:"state-1" capabilities) req
    | "/jmap/api/" -> respond_json chain_response req
    | _ -> not_found req
  in
  Eio_mock.Backend.run @@ fun () ->
  Eio.Switch.run @@ fun sw ->
  let client = client_exn ~sw server in
  let body_of_last_post () = (List.hd (List.rev (api_posts log))).body in
  (match Client.chain client (email_chain account_id) with
  | Error e -> Alcotest.failf "chain failed: %s" (Client.error_to_string e)
  | Ok _ ->
      let body = body_of_last_post () in
      Alcotest.(check bool)
        ("the four known capabilities in their own order, the unknown one left \
          out: " ^ body)
        true
        (contains
           ~needle:
             (using_needle
                [
                  Jmap.Proto.Capability.core;
                  Jmap.Proto.Capability.mail;
                  Jmap.Proto.Capability.vacation_response;
                ])
           body));
  (match
     Client.chain client ~capabilities:[ "urn:example:unknown" ]
       (email_chain account_id)
   with
  | Error e -> Alcotest.failf "chain failed: %s" (Client.error_to_string e)
  | Ok _ ->
      let body = body_of_last_post () in
      Alcotest.(check bool)
        ("an explicit list reaches a capability outside the four: " ^ body)
        true
        (contains ~needle:(using_needle [ "urn:example:unknown" ]) body));
  (* The default is read from the session at the time of the call, so a
     refresh moves it. *)
  (match Client.refresh_session client with
  | Ok () -> ()
  | Error e -> Alcotest.failf "refresh failed: %s" (Client.error_to_string e));
  match Client.chain_exn client (email_chain account_id) with
  | exception Client.Jmap_client_error e ->
      Alcotest.failf "chain failed: %s" (Client.error_to_string e)
  | _ ->
      let body = body_of_last_post () in
      Alcotest.(check bool)
        ("the refreshed session decides the next default: " ^ body)
        true
        (contains
           ~needle:
             (using_needle
                [ Jmap.Proto.Capability.core; Jmap.Proto.Capability.submission ])
           body)

(* An event bigger than the client will hold is a failure the next
   connection would meet again, so it is fatal rather than retried. *)
let test_listen_oversized_event () =
  let log = ref [] in
  (* One event of 1200 data lines of a kilobyte each, arriving a line at a
     time, which is over the one mebibyte a single event may hold. *)
  let chunks =
    List.init 1200 (fun _ -> "data: " ^ String.make 1000 'x' ^ "\n")
  in
  Eio_mock.Backend.run @@ fun () ->
  Eio.Switch.run @@ fun sw ->
  let client = client_exn ~sw (event_stream_server chunks log) in
  match Push.listen client (fun _ -> `Continue) with
  | Ok () -> Alcotest.fail "expected the oversized event to fail the stream"
  | Error
      (Client.Transport
         (Fetch.Decode_failure { error = Fetch.Media.Too_large limit; _ }, msg))
    ->
      Alcotest.(check int) "the event bound" (1024 * 1024) limit;
      Alcotest.(check bool)
        ("and it is named: " ^ msg)
        true
        (contains ~needle:"1048576" msg)
  | Error e -> Alcotest.failf "unexpected error: %s" (Client.error_to_string e)

(* {1 Subscriptions}

   Push.subscribe is the reconnecting loop of RFC 8620 7.3: one fiber that
   connects, reads, remembers the last event id and comes back after the
   connection ends. The mock backend's monotonic clock advances by itself
   whenever every fiber is blocked, so a backoff of five seconds costs the
   test nothing and is still observable in the timestamps below. *)

(* A response body made of these chunks and then a clean end of stream.
   [respond_chunks] above ends its flow with a failure instead, which is what
   a test of a single [listen] wants; a subscription is expected to read to
   the end and reconnect. *)
let respond_events ?(close = fun () -> ()) ?(status = 200) actions req =
  let flow = Eio_mock.Flow.make "event-stream" in
  Eio_mock.Flow.on_read flow actions;
  Fetch.Middleware.Pi.response ~close ~status
    ~headers:(media_headers "text/event-stream")
    ~version:`HTTP_1_1
    ~body:(flow :> Eio.Flow.source_ty Eio.Resource.t)
    ~url:req.Fetch.Middleware.url ()

let eof = `Raise End_of_file

(* A connection the server never says anything more on, and never closes:
   only cancellation - a close, or the switch finishing - ends it. *)
let held = `Run (fun () -> Eio.Fiber.await_cancel ())

let state_frame ~id state =
  Printf.sprintf
    "id: %s\n\
     event: state\n\
     data: \
     {\"@type\":\"StateChange\",\"changed\":{\"acc1\":{\"Email\":\"%s\"}}}\n\n"
    id state

let ping_frame = "event: ping\ndata: {\"interval\":5}\n\n"

(* Every event-source connection in order: the time it was made and the
   Last-Event-ID it quoted. *)
type connection = { at : float; last_event_id : string option }

(* [connections] answers the nth event-source request with the nth element of
   [answers], and the last element again thereafter. *)
let subscribe_server ?close ~clock ~log answers req =
  match path req with
  | "/.well-known/jmap" -> respond_json (session_json ()) req
  | p when String.starts_with ~prefix:"/jmap/eventsource/" p ->
      let n = List.length !log in
      log :=
        !log
        @ [
            {
              at = Eio.Time.now clock;
              last_event_id =
                Http.Header.get req.Fetch.Middleware.headers "last-event-id";
            };
          ];
      let answers = Array.of_list answers in
      respond_events ?close answers.(min n (Array.length answers - 1)) req
  | _ -> not_found req

(* Wait until [n] connections have been made. Sleeping rather than yielding
   matters: the mock clock only advances when every fiber is blocked, and a
   subscription between connections is asleep on it. *)
let wait_for_connections clock log n =
  while List.length !log < n do
    Eio.Time.Mono.sleep clock 0.5
  done

let take sub = Push.next sub

let event_summary sub =
  match take sub with `End -> "End" | `Event e -> Fmt.str "%a" Push.pp_event e

(* A connection that ends is followed by another one, after the backoff, and
   that one quotes the id of the last frame of the first (RFC 8620 7.3). *)
let test_subscribe_reconnect () =
  Eio_mock.Backend.run_full @@ fun env ->
  let clock = Eio.Stdenv.mono_clock env in
  let wall = Eio.Stdenv.clock env in
  let log = ref [] in
  let answers =
    [
      (* the first connection: a ping, a state event with an id, then EOF *)
      [ `Return ping_frame; `Return (state_frame ~id:"e1" "s1"); eof ];
      (* the second: one more state event, then a connection that just hangs *)
      [ `Return (state_frame ~id:"e2" "s2"); held ];
      [ held ];
    ]
  in
  Eio.Switch.run @@ fun sw ->
  let client =
    client_exn ~sw ~mono_clock:clock (subscribe_server ~clock:wall ~log answers)
  in
  let sub =
    Push.subscribe ~sw client ~types:[ "Email" ] ~ping:5 ~backoff_initial:5.
      ~backoff_max:60. ()
  in
  check_string "the ping arrives first" "Ping 5s" (event_summary sub);
  check_string "then the state change" "StateChange {acc1: Email=s1}"
    (event_summary sub);
  check_opt_string "the id is remembered" (Some "e1") (Push.last_event_id sub);
  check_string "the reconnection delivers the next change"
    "StateChange {acc1: Email=s2}" (event_summary sub);
  check_opt_string "and the id moves on" (Some "e2") (Push.last_event_id sub);
  (match !log with
  | [ first; second ] ->
      check_opt_string "nothing to replay on the first connection" None
        first.last_event_id;
      check_opt_string "the second quotes the last id seen" (Some "e1")
        second.last_event_id;
      Alcotest.(check bool)
        (Printf.sprintf "the backoff of 5s elapsed first (%.1fs)"
           (second.at -. first.at))
        true
        (second.at -. first.at >= 5.)
  | l -> Alcotest.failf "expected 2 connections, got %d" (List.length l));
  Push.close sub;
  check_string "the stream ends" "End" (event_summary sub);
  Alcotest.(check bool)
    "and the result is a clean stop" true
    (Eio.Promise.await (Push.result sub) = Ok ())

(* The stream is bounded: a subscription whose consumer is not reading blocks
   in the middle of the connection rather than buffering without limit. *)
let test_subscribe_backpressure () =
  Eio_mock.Backend.run_full @@ fun env ->
  let clock = Eio.Stdenv.mono_clock env in
  let wall = Eio.Stdenv.clock env in
  let log = ref [] in
  let answers =
    [
      [
        `Return (state_frame ~id:"e1" "s1");
        `Return (state_frame ~id:"e2" "s2");
        `Return (state_frame ~id:"e3" "s3");
        held;
      ];
    ]
  in
  Eio.Switch.run @@ fun sw ->
  let client =
    client_exn ~sw ~mono_clock:clock (subscribe_server ~clock:wall ~log answers)
  in
  let sub = Push.subscribe ~sw client ~capacity:1 () in
  (* Let the subscription run until it can make no further progress. *)
  for _ = 1 to 10 do
    Eio.Fiber.yield ()
  done;
  Alcotest.(check int)
    "one event buffered, the rest still on the connection" 1
    (Eio.Stream.length (Push.events sub));
  (* The connection has read every event the server sent while the reader
     holds one, so the id it would reconnect with is already the last. *)
  check_opt_string "the reader is behind the connection" (Some "e3")
    (Push.last_event_id sub);
  List.iter
    (fun state ->
      check_string "in order"
        (Printf.sprintf "StateChange {acc1: Email=%s}" state)
        (event_summary sub))
    [ "s1"; "s2"; "s3" ];
  Push.close sub;
  check_string "the stream ends" "End" (event_summary sub)

(* Close ends a connection that is going nowhere, and does not open another. *)
let test_subscribe_close () =
  Eio_mock.Backend.run_full @@ fun env ->
  let clock = Eio.Stdenv.mono_clock env in
  let wall = Eio.Stdenv.clock env in
  let log = ref [] in
  let answers = [ [ `Return (state_frame ~id:"e1" "s1"); held ] ] in
  Eio.Switch.run @@ fun sw ->
  let client =
    client_exn ~sw ~mono_clock:clock (subscribe_server ~clock:wall ~log answers)
  in
  let sub = Push.subscribe ~sw client () in
  check_string "the change arrives" "StateChange {acc1: Email=s1}"
    (event_summary sub);
  Push.close sub;
  check_string "closing ends the stream" "End" (event_summary sub);
  Alcotest.(check bool)
    "a clean stop" true
    (Eio.Promise.await (Push.result sub) = Ok ());
  Push.close sub (* idempotent *);
  Alcotest.(check int)
    "no connection was made after the close" 1 (List.length !log)

(* Closing also cancels the JMAP forwarding fiber. It may be parked behind a
   full outer stream even though the underlying Fetch subscription has already
   accepted several events. *)
let test_subscribe_close_with_full_stream () =
  Eio_mock.Backend.run_full @@ fun env ->
  let clock = Eio.Stdenv.mono_clock env in
  let wall = Eio.Stdenv.clock env in
  let log = ref [] in
  let answers =
    [
      [
        `Return (state_frame ~id:"e1" "s1");
        `Return (state_frame ~id:"e2" "s2");
        `Return (state_frame ~id:"e3" "s3");
        held;
      ];
    ]
  in
  Eio.Switch.run @@ fun sw ->
  let client =
    client_exn ~sw ~mono_clock:clock (subscribe_server ~clock:wall ~log answers)
  in
  let sub = Push.subscribe ~sw client ~capacity:1 () in
  for _ = 1 to 10 do
    Eio.Fiber.yield ()
  done;
  Alcotest.(check int)
    "the consumer stream is full" 1
    (Eio.Stream.length (Push.events sub));
  Push.close sub;
  let result =
    try
      Eio.Time.Timeout.run_exn (Eio.Time.Timeout.seconds clock 1.) (fun () ->
          Some (Eio.Promise.await (Push.result sub)))
    with Eio.Time.Timeout -> None
  in
  Alcotest.(check (option (result unit string)))
    "close settles without waiting for stream room" (Some (Ok ()))
    (Option.map (Result.map_error Client.error_to_string) result);
  Eio.Time.Timeout.run_exn (Eio.Time.Timeout.seconds clock 1.) (fun () ->
      check_string "the buffered event remains" "StateChange {acc1: Email=s1}"
        (event_summary sub);
      check_string "the full stream still terminates" "End" (event_summary sub);
      check_string "termination can be read again" "End" (event_summary sub))

let test_subscribe_fatal_with_full_inner_stream () =
  Eio_mock.Backend.run_full @@ fun env ->
  let clock = Eio.Stdenv.mono_clock env in
  let wall = Eio.Stdenv.clock env in
  let log = ref [] in
  let failed, failed_u = Eio.Promise.create () in
  let answers =
    [
      [
        `Return (state_frame ~id:"e1" "s1");
        `Return (state_frame ~id:"e2" "s2");
        `Return (state_frame ~id:"e3" "s3");
        `Run
          (fun () ->
            Eio.Promise.resolve failed_u ();
            raise (Fetch.err (Fetch.Denied "fatal after three events")));
      ];
    ]
  in
  Eio.Switch.run @@ fun sw ->
  let client =
    client_exn ~sw ~mono_clock:clock (subscribe_server ~clock:wall ~log answers)
  in
  let sub = Push.subscribe ~sw client ~capacity:1 () in
  Eio.Time.Timeout.run_exn (Eio.Time.Timeout.seconds clock 1.) (fun () ->
      Eio.Promise.await failed;
      Alcotest.(check int)
        "the outer queue is full" 1
        (Eio.Stream.length (Push.events sub));
      List.iter
        (fun state ->
          check_string "buffered events survive inner termination"
            ("StateChange {acc1: Email=" ^ state ^ "}")
            (event_summary sub))
        [ "s1"; "s2"; "s3" ];
      check_string "inner termination reaches the consumer" "End"
        (event_summary sub);
      check_string "end remains observable" "End" (event_summary sub);
      match Eio.Promise.await (Push.result sub) with
      | Error (Client.Transport (Fetch.Denied _, _)) -> ()
      | Error error ->
          Alcotest.failf "unexpected error: %s" (Client.error_to_string error)
      | Ok () -> Alcotest.fail "the fatal error was lost")

let test_subscribe_rejection_body_before_close () =
  let run ?poll ~media ~body check_error =
    Eio_mock.Backend.run_full @@ fun env ->
    let clock = Eio.Stdenv.mono_clock env in
    let closed = ref false in
    let closes = ref 0 in
    let server req =
      match path req with
      | "/.well-known/jmap" -> respond_json (session_json ()) req
      | p when String.starts_with ~prefix:"/jmap/eventsource/" p ->
          let flow = Eio_mock.Flow.make "refused event-stream" in
          Eio_mock.Flow.on_read flow
            [
              `Run
                (fun () ->
                  if !closed then failwith "response read after close";
                  body);
              eof;
            ];
          Fetch.Middleware.Pi.response ~status:401
            ~headers:(media_headers media) ~version:`HTTP_1_1
            ~body:(flow :> Eio.Flow.source_ty Eio.Resource.t)
            ~close:(fun () ->
              closed := true;
              incr closes)
            ~url:req.Fetch.Middleware.url ()
      | _ -> not_found req
    in
    Eio.Switch.run @@ fun sw ->
    let client = client_exn ~sw ~mono_clock:clock server in
    let sub = Push.subscribe ~sw client ?poll () in
    Eio.Time.Timeout.run_exn (Eio.Time.Timeout.seconds clock 5.) (fun () ->
        check_string "refusal terminates" "End" (event_summary sub);
        match Eio.Promise.await (Push.result sub) with
        | Error error -> check_error error
        | Ok () -> Alcotest.fail "expected a refusal");
    Alcotest.(check int) "the response is released once" 1 !closes
  in
  run ~media:"text/plain" ~body:"credential expired" (function
    | Client.Http_error (401, body) ->
        check_string "the error body survives release" "credential expired" body
    | error ->
        Alcotest.failf "unexpected error: %s" (Client.error_to_string error));
  run ~poll:2. ~media:"application/problem+json"
    ~body:{|{"type":"about:blank","status":401,"detail":"log in again"}|}
    (function
    | Client.Jmap_error error ->
        check_opt_string "problem detail survives the polling wrapper"
          (Some "log in again") error.Jmap.Proto.Error.Request_error.detail
    | error ->
        Alcotest.failf "unexpected error: %s" (Client.error_to_string error))

let test_subscribe_error_context () =
  let run ~status ~failure check =
    Eio_mock.Backend.run_full @@ fun env ->
    let clock = Eio.Stdenv.mono_clock env in
    let server req =
      match path req with
      | "/.well-known/jmap" -> respond_json (session_json ()) req
      | p when String.starts_with ~prefix:"/jmap/eventsource/" p ->
          respond_events ~status
            [ `Raise (Fetch.err (Fetch.Denied failure)) ]
            req
      | _ -> not_found req
    in
    Eio.Switch.run @@ fun sw ->
    let client = client_exn ~sw ~mono_clock:clock server in
    let sub = Push.subscribe ~sw client () in
    check_string "failed subscription ends" "End" (event_summary sub);
    match Eio.Promise.await (Push.result sub) with
    | Error error -> check error
    | Ok () -> Alcotest.fail "the failed subscription stopped cleanly"
  in
  let check_diagnostic operation failure diagnostic =
    List.iter
      (fun part ->
        Alcotest.(check bool)
          ("diagnostic contains " ^ part)
          true
          (contains ~needle:part diagnostic))
      [ operation; "https://api.example.com"; failure ];
    Alcotest.(check bool)
      "diagnostic excludes the event-source path" false
      (contains ~needle:"/jmap/eventsource" diagnostic)
  in
  run ~status:200 ~failure:"subscription body unavailable" (function
    | Client.Transport (Fetch.Denied _, diagnostic) ->
        check_diagnostic "subscribing to JMAP event source"
          "subscription body unavailable" diagnostic
    | error ->
        Alcotest.failf "unexpected subscription error: %s"
          (Client.error_to_string error));
  run ~status:401 ~failure:"refusal body unavailable" (function
    | Client.Http_error (401, diagnostic) ->
        check_diagnostic "reading refused JMAP event source response"
          "refusal body unavailable" diagnostic
    | error ->
        Alcotest.failf "unexpected refusal error: %s"
          (Client.error_to_string error))

(* Credentials that the event source refuses are not going to be accepted on
   the next connection either: the subscription ends and says why. *)
let test_subscribe_unauthorised () =
  Eio_mock.Backend.run_full @@ fun env ->
  let clock = Eio.Stdenv.mono_clock env in
  let attempts = ref 0 in
  let server req =
    match path req with
    | "/.well-known/jmap" -> respond_json (session_json ()) req
    | p when String.starts_with ~prefix:"/jmap/eventsource/" p ->
        incr attempts;
        Fetch_mock.respond ~status:401
          ~headers:(media_headers "text/plain")
          "who are you" req
    | _ -> not_found req
  in
  Eio.Switch.run @@ fun sw ->
  let client = client_exn ~sw ~mono_clock:clock server in
  let sub = Push.subscribe ~sw client ~backoff_initial:1. ~backoff_max:1. () in
  check_string "the stream ends without an event" "End" (event_summary sub);
  (match Eio.Promise.await (Push.result sub) with
  | Error (Client.Http_error (401, body)) ->
      check_string "body" "who are you" body
  | Ok () -> Alcotest.fail "expected the subscription to give up"
  | Error e -> Alcotest.failf "unexpected error: %s" (Client.error_to_string e));
  Alcotest.(check int) "it was not retried" 1 !attempts

(* The switch finishing cancels the daemon wherever it is. [result] must be
   resolved on that path too, or a fiber outside the switch - the natural
   place to await it - waits for ever. The hard case is a consumer that
   stopped reading: the daemon is then parked in a full [Stream.add], and the
   cancellation arrives through the HTTP stack rather than as a value. *)
let test_subscribe_switch_cancels_a_parked_daemon () =
  Eio_mock.Backend.run_full @@ fun env ->
  let clock = Eio.Stdenv.mono_clock env in
  let wall = Eio.Stdenv.clock env in
  let log = ref [] in
  let answers =
    [
      [
        `Return (state_frame ~id:"e1" "s1");
        `Return (state_frame ~id:"e2" "s2");
        `Return (state_frame ~id:"e3" "s3");
        held;
      ];
    ]
  in
  let sub =
    Eio.Switch.run @@ fun sw ->
    let client =
      client_exn ~sw ~mono_clock:clock
        (subscribe_server ~clock:wall ~log answers)
    in
    let sub = Push.subscribe ~sw client ~capacity:1 () in
    (* Nothing reads, so the daemon fills the one slot and parks. *)
    for _ = 1 to 10 do
      Eio.Fiber.yield ()
    done;
    Alcotest.(check int)
      "the stream is full" 1
      (Eio.Stream.length (Push.events sub));
    sub
  in
  Alcotest.(check (option (result unit string)))
    "the cancelled daemon still resolves its result" (Some (Ok ()))
    (Option.map
       (Result.map_error Client.error_to_string)
       (Eio.Promise.peek (Push.result sub)))

(* With room in the stream the sentinel is placed on that path as well, so a
   consumer inside the switch sees the subscription end. *)
let test_subscribe_switch_ends_the_stream () =
  Eio_mock.Backend.run_full @@ fun env ->
  let clock = Eio.Stdenv.mono_clock env in
  let wall = Eio.Stdenv.clock env in
  let log = ref [] in
  let answers = [ [ `Return (state_frame ~id:"e1" "s1"); held ] ] in
  let sub =
    Eio.Switch.run @@ fun sw ->
    let client =
      client_exn ~sw ~mono_clock:clock
        (subscribe_server ~clock:wall ~log answers)
    in
    let sub = Push.subscribe ~sw client () in
    check_string "the change arrives" "StateChange {acc1: Email=s1}"
      (event_summary sub);
    sub
  in
  Alcotest.(check bool)
    "the stream ends" true
    (Eio.Stream.take_nonblocking (Push.events sub) = Some `End);
  Alcotest.(check (option (result unit string)))
    "and the result is a clean stop" (Some (Ok ()))
    (Option.map
       (Result.map_error Client.error_to_string)
       (Eio.Promise.peek (Push.result sub)))

(* A transport failure that another connection cannot fix - here the [Denied]
   of a credential the event source will not take - ends the subscription at
   once. Retrying it would spin at the backoff cap for the life of the
   switch, telling no one why. *)
let test_subscribe_fatal_transport_error () =
  Eio_mock.Backend.run_full @@ fun env ->
  let clock = Eio.Stdenv.mono_clock env in
  let attempts = ref 0 in
  let server req =
    match path req with
    | "/.well-known/jmap" -> respond_json (session_json ()) req
    | p when String.starts_with ~prefix:"/jmap/eventsource/" p ->
        incr attempts;
        raise (Fetch.err (Fetch.Denied "the event source is out of scope"))
    | _ -> not_found req
  in
  Eio.Switch.run @@ fun sw ->
  let client = client_exn ~sw ~mono_clock:clock server in
  let sub = Push.subscribe ~sw client ~backoff_initial:1. ~backoff_max:60. () in
  (* A subscription that retried this would never end, so the wait is
     bounded; the mock clock advances on its own while it is. *)
  let ended =
    try
      Eio.Time.Timeout.run_exn (Eio.Time.Timeout.seconds clock 600.) (fun () ->
          event_summary sub)
    with Eio.Time.Timeout -> "still retrying"
  in
  check_string "the stream ends without an event" "End" ended;
  (match Eio.Promise.await (Push.result sub) with
  | Error (Client.Transport (Fetch.Denied _, _)) -> ()
  | Ok () -> Alcotest.fail "expected the subscription to give up"
  | Error e -> Alcotest.failf "unexpected error: %s" (Client.error_to_string e));
  Alcotest.(check int) "it was not retried" 1 !attempts

(* The other half of the table: a 503 is the server asking to be asked
   again, and is. *)
let test_subscribe_retries_server_error () =
  Eio_mock.Backend.run_full @@ fun env ->
  let clock = Eio.Stdenv.mono_clock env in
  let attempts = ref 0 in
  let server req =
    match path req with
    | "/.well-known/jmap" -> respond_json (session_json ()) req
    | p when String.starts_with ~prefix:"/jmap/eventsource/" p ->
        incr attempts;
        Fetch_mock.respond ~status:503
          ~headers:(media_headers "text/plain")
          "come back later" req
    | _ -> not_found req
  in
  Eio.Switch.run @@ fun sw ->
  let client = client_exn ~sw ~mono_clock:clock server in
  let sub = Push.subscribe ~sw client ~backoff_initial:1. ~backoff_max:4. () in
  while !attempts < 3 do
    Eio.Time.Mono.sleep clock 0.5
  done;
  Push.close sub;
  check_string "and it is still running" "End" (event_summary sub);
  Alcotest.(check bool) "a 5xx is retried" true (!attempts >= 3)

(* A backoff that never waits is a hot reconnect loop, and a stream of no
   capacity is a rendezvous nothing can be delivered to. Both are rejected
   where the mistake is made rather than at the first reconnection. *)
let test_subscribe_validates_its_arguments () =
  Eio_mock.Backend.run_full @@ fun env ->
  let clock = Eio.Stdenv.mono_clock env in
  Eio.Switch.run @@ fun sw ->
  let server req =
    match path req with
    | "/.well-known/jmap" -> respond_json (session_json ()) req
    | _ -> not_found req
  in
  let client = client_exn ~sw ~mono_clock:clock server in
  let backoff_error =
    Invalid_argument
      "Jmap_eio.Push.subscribe: ?backoff_initial and ?backoff_max must be \
       finite with 0 < backoff_initial <= backoff_max"
  in
  let rejects name (initial, max) =
    Alcotest.check_raises name backoff_error (fun () ->
        ignore
          (Push.subscribe ~sw client ~backoff_initial:initial ~backoff_max:max
             ()))
  in
  rejects "zero initial delay" (0., 60.);
  rejects "negative initial delay" (-1., 60.);
  rejects "a NaN" (Float.nan, 60.);
  rejects "an infinite cap" (1., Float.infinity);
  rejects "initial above the cap" (120., 60.);
  let poll_error =
    Invalid_argument
      "Jmap_eio.Push.subscribe: ?poll must be finite and positive"
  in
  let rejects_poll name poll =
    Alcotest.check_raises name poll_error (fun () ->
        ignore (Push.subscribe ~sw client ~poll ()))
  in
  rejects_poll "a zero poll interval" 0.;
  rejects_poll "a negative poll interval" (-1.);
  rejects_poll "a NaN poll interval" Float.nan;
  rejects_poll "an infinite poll interval" Float.infinity;
  Alcotest.check_raises "a sub-nanosecond poll interval"
    (Invalid_argument
       "Jmap_eio.Push.subscribe: ?poll is too small for the monotonic clock")
    (fun () ->
      ignore (Push.subscribe ~sw client ~poll:1e-12 ());
      ());
  Alcotest.check_raises "an unrepresentable poll interval"
    (Invalid_argument
       "Jmap_eio.Push.subscribe: ?poll is too large for the monotonic clock")
    (fun () -> ignore (Push.subscribe ~sw client ~poll:Float.max_float ()));
  Alcotest.check_raises "a negative ping interval"
    (Invalid_argument
       "Jmap_eio.Push.subscribe: ?ping must be between 0 and 2^53-1 seconds")
    (fun () -> ignore (Push.subscribe ~sw client ~ping:(-1) ()));
  Alcotest.check_raises "no capacity"
    (Invalid_argument "Jmap_eio.Push.subscribe: ?capacity must be at least 1")
    (fun () -> ignore (Push.subscribe ~sw client ~capacity:0 ()))

(* The backoff is measured on a monotonic clock, so a transport that carries
   none cannot be subscribed to. *)
let test_subscribe_needs_a_mono_clock () =
  Eio_mock.Backend.run @@ fun () ->
  Eio.Switch.run @@ fun sw ->
  let server req =
    match path req with
    | "/.well-known/jmap" -> respond_json (session_json ()) req
    | _ -> not_found req
  in
  let client = client_exn ~sw server in
  Alcotest.(check bool)
    "no monotonic clock" true
    (Client.mono_clock client = None);
  Alcotest.check_raises "subscribe without one"
    (Invalid_argument
       "Jmap_eio.Push.subscribe: the client's transport carries no monotonic \
        clock") (fun () -> ignore (Push.subscribe ~sw client ()))

(* A failure that reaches the daemon as an exception rather than as a value
   still settles [result], and settles it with an error: only a cancellation
   is a clean stop. *)
let test_subscribe_stray_exception () =
  Eio_mock.Backend.run_full @@ fun env ->
  let clock = Eio.Stdenv.mono_clock env in
  let server req =
    match path req with
    | "/.well-known/jmap" -> respond_json (session_json ()) req
    | p when String.starts_with ~prefix:"/jmap/eventsource/" p ->
        raise Not_found
    | _ -> not_found req
  in
  let promise = ref None in
  ( Eio.Switch.run @@ fun sw ->
    let client = client_exn ~sw ~mono_clock:clock server in
    let sub = Push.subscribe ~sw client () in
    promise := Some (Push.result sub);
    (* The subscription ends rather than escaping, so the stream's sentinel is
      what says it is over. *)
    check_string "the stream ends without an event" "End" (event_summary sub) );
  match Option.bind !promise Eio.Promise.peek with
  | Some (Error (Client.Transport (Fetch.Protocol_error msg, _))) ->
      Alcotest.(check bool)
        ("the exception is reported: " ^ msg)
        true
        (contains ~needle:"Not_found" msg)
  | Some (Ok ()) ->
      Alcotest.fail "a stray exception must not look like a clean stop"
  | Some (Error e) ->
      Alcotest.failf "unexpected error: %s" (Client.error_to_string e)
  | None -> Alcotest.fail "result was left unresolved"

(* A server that keeps failing is retried, with the wait doubling up to the
   maximum and starting again once a connection delivers something. *)
let test_subscribe_backoff_growth () =
  Eio_mock.Backend.run_full @@ fun env ->
  let clock = Eio.Stdenv.mono_clock env in
  let wall = Eio.Stdenv.clock env in
  let log = ref [] in
  let answers =
    [
      [ eof ];
      [ eof ];
      [ eof ];
      [ eof ];
      [ `Return (state_frame ~id:"e1" "s1"); eof ];
      [ eof ];
      [ held ];
    ]
  in
  Eio.Switch.run @@ fun sw ->
  let client =
    client_exn ~sw ~mono_clock:clock (subscribe_server ~clock:wall ~log answers)
  in
  let sub = Push.subscribe ~sw client ~backoff_initial:1. ~backoff_max:4. () in
  check_string "the connection that worked" "StateChange {acc1: Email=s1}"
    (event_summary sub);
  (* Wait until the connection after that one has been made, so that the gap
     following the successful read is in the log. *)
  wait_for_connections clock log 7;
  Push.close sub;
  let gaps =
    match !log with
    | first :: rest ->
        List.rev
          (snd
             (List.fold_left
                (fun (prev, acc) c -> (c, (c.at -. prev.at) :: acc))
                (first, []) rest))
    | [] -> Alcotest.fail "no connections"
  in
  let round g = Float.round (g *. 100.) /. 100. in
  Alcotest.(check (list (float 0.01)))
    "doubling, capped at 4, back to 1 once a connection delivered an event"
    [ 1.; 2.; 4.; 4.; 1.; 2. ] (List.map round gaps)

(* With ~poll the connection is not held: it is closed after at most that
   many seconds, and reopened that many seconds later quoting the last id.
   On Cyrus this is the only workable mode, because an open event source
   there blocks delivery for the account. *)
(* A connection that is redirected is one attempt. The redirect walk of Fetch
   runs below the wrappers this module installs, so the discarded hop keeps
   its body unread and the events of the target arrive as they would have on
   the first URL (RFC 9110 15.4). *)
let test_subscribe_follows_a_redirect () =
  Eio_mock.Backend.run_full @@ fun env ->
  let clock = Eio.Stdenv.mono_clock env in
  let hops = ref 0 in
  let server req =
    match path req with
    | "/.well-known/jmap" -> respond_json (session_json ()) req
    | p when String.starts_with ~prefix:"/jmap/eventsource/" p ->
        incr hops;
        let flow = Eio_mock.Flow.make "redirect body" in
        Eio_mock.Flow.on_read flow
          [ `Raise (Failure "the redirect body was read") ];
        Fetch.Middleware.Pi.response ~status:302
          ~headers:
            (Http.Header.of_list
               [ ("location", "/jmap/moved/"); ("content-type", "text/plain") ])
          ~version:`HTTP_1_1
          ~body:(flow :> Eio.Flow.source_ty Eio.Resource.t)
          ~close:(fun () -> ())
          ~url:req.Fetch.Middleware.url ()
    | p when String.starts_with ~prefix:"/jmap/moved/" p ->
        respond_events [ `Return (state_frame ~id:"e1" "s1"); held ] req
    | _ -> not_found req
  in
  Eio.Switch.run @@ fun sw ->
  let client = client_exn ~sw ~mono_clock:clock server in
  let sub = Push.subscribe ~sw client ~types:[ "Email" ] () in
  check_string "the redirected connection delivers its events"
    "StateChange {acc1: Email=s1}" (event_summary sub);
  Alcotest.(check int) "one connection, one hop" 1 !hops;
  Push.close sub;
  check_string "the stream ends" "End" (event_summary sub);
  Alcotest.(check bool)
    "and the result is a clean stop" true
    (Eio.Promise.await (Push.result sub) = Ok ())

let test_subscribe_poll () =
  Eio_mock.Backend.run_full @@ fun env ->
  let clock = Eio.Stdenv.mono_clock env in
  let wall = Eio.Stdenv.clock env in
  let log = ref [] in
  let closes = ref 0 in
  (* Every connection hangs: it is the poll deadline that ends them. *)
  let answers = [ [ `Return (state_frame ~id:"e1" "s1"); held ]; [ held ] ] in
  Eio.Switch.run @@ fun sw ->
  let client =
    client_exn ~sw ~mono_clock:clock
      (subscribe_server ~close:(fun () -> incr closes) ~clock:wall ~log answers)
  in
  let sub =
    Push.subscribe ~sw client ~types:[ "Email" ] ~poll:10. ~last_event_id:"1" ()
  in
  check_string "the first connection reports the state"
    "StateChange {acc1: Email=s1}" (event_summary sub);
  wait_for_connections clock log 2;
  Alcotest.(check int) "the expired polling response was released" 1 !closes;
  Push.close sub;
  ignore (Eio.Promise.await (Push.result sub));
  Alcotest.(check int) "closing releases the active polling response" 2 !closes;
  (match !log with
  | first :: second :: _ ->
      check_opt_string "the seed id opens the first connection" (Some "1")
        first.last_event_id;
      check_opt_string "and the last id seen the second" (Some "e1")
        second.last_event_id;
      Alcotest.(check bool)
        (Printf.sprintf "10s held plus 10s idle (%.1fs)" (second.at -. first.at))
        true
        (second.at -. first.at >= 20.)
  | l -> Alcotest.failf "expected 2 connections, got %d" (List.length l));
  check_string "closeafter=state, so that the connection is short"
    "https://api.example.com/jmap/eventsource/?types=Email&closeafter=state&ping=0"
    (Push.event_source_url client ~types:[ "Email" ] ~close_after:`State ())

let test_subscribe_poll_with_full_stream () =
  Eio_mock.Backend.run_full @@ fun env ->
  let clock = Eio.Stdenv.mono_clock env in
  let wall = Eio.Stdenv.clock env in
  let log = ref [] in
  let released, released_u = Eio.Promise.create () in
  let answers =
    [
      List.init 4 (fun i ->
          let suffix = string_of_int (i + 1) in
          `Return (state_frame ~id:("e" ^ suffix) ("s" ^ suffix)))
      @ [ held ];
    ]
  in
  Eio.Switch.run @@ fun sw ->
  let client =
    client_exn ~sw ~mono_clock:clock
      (subscribe_server
         ~close:(fun () -> ignore (Eio.Promise.try_resolve released_u ()))
         ~clock:wall ~log answers)
  in
  let sub = Push.subscribe ~sw client ~poll:10. ~capacity:1 () in
  Eio.Time.Timeout.run_exn (Eio.Time.Timeout.seconds clock 15.) (fun () ->
      Eio.Promise.await released;
      Alcotest.(check int)
        "release does not require queue space" 1
        (Eio.Stream.length (Push.events sub));
      List.iter
        (fun state ->
          check_string "decoded events survive the polling deadline"
            ("StateChange {acc1: Email=" ^ state ^ "}")
            (event_summary sub))
        [ "s1"; "s2"; "s3"; "s4" ];
      Push.close sub;
      check_string "the subscription closes" "End" (event_summary sub))

let test_subscribe_poll_bounds_response_headers () =
  Eio_mock.Backend.run_full @@ fun env ->
  let clock = Eio.Stdenv.mono_clock env in
  let attempts = ref 0 in
  let cancelled = ref 0 in
  let server req =
    match path req with
    | "/.well-known/jmap" -> respond_json (session_json ()) req
    | p when String.starts_with ~prefix:"/jmap/eventsource/" p ->
        incr attempts;
        Fun.protect ~finally:(fun () -> incr cancelled) Eio.Fiber.await_cancel
    | _ -> not_found req
  in
  Eio.Switch.run @@ fun sw ->
  let client = client_exn ~sw ~mono_clock:clock server in
  let sub = Push.subscribe ~sw client ~poll:10. () in
  Eio.Time.Timeout.run_exn (Eio.Time.Timeout.seconds clock 25.) (fun () ->
      while !attempts < 2 do
        Eio.Time.Mono.sleep clock 0.5
      done;
      Alcotest.(check int) "the first header wait was cancelled" 1 !cancelled;
      Push.close sub;
      check_string "closing cancels the second attempt" "End"
        (event_summary sub);
      Alcotest.(check int)
        "both header waits released their resources" 2 !cancelled)

(* wait_for_state is the loop a program that watches one type in one account
   writes: it discards everything else and is the new state string. *)
let test_wait_for_state () =
  Eio_mock.Backend.run_full @@ fun env ->
  let clock = Eio.Stdenv.mono_clock env in
  let wall = Eio.Stdenv.clock env in
  let log = ref [] in
  let other =
    "event: state\n\
     data: \
     {\"@type\":\"StateChange\",\"changed\":{\"acc2\":{\"Email\":\"nope\"}}}\n\n"
  in
  let mailbox =
    "event: state\n\
     data: \
     {\"@type\":\"StateChange\",\"changed\":{\"acc1\":{\"Mailbox\":\"m1\"}}}\n\n"
  in
  let answers =
    [
      [
        `Return ping_frame;
        `Return other;
        `Return mailbox;
        `Return (state_frame ~id:"e9" "s9");
        held;
      ];
    ]
  in
  Eio.Switch.run @@ fun sw ->
  let client =
    client_exn ~sw ~mono_clock:clock (subscribe_server ~clock:wall ~log answers)
  in
  let sub = Push.subscribe ~sw client ~types:[ "Email" ] () in
  check_opt_string "the Email state for this account" (Some "s9")
    (Push.wait_for_state sub ~timeout:60. ~type_:"Email" ~account_id ());
  (* Nothing more is coming, so the timeout is what ends the second wait. *)
  check_opt_string "nothing further before the deadline" None
    (Push.wait_for_state sub ~timeout:30. ~type_:"Email" ~account_id ());
  Push.close sub;
  check_opt_string "and none once the subscription has ended" None
    (Push.wait_for_state sub ~type_:"Email" ~account_id ())

(* A caller that holds a state waits for the next one: RFC 8620 7.3 has the
   server report the current state of each type when a connection opens, so
   the state already in hand arrives again and is skipped. *)
let test_wait_for_state_since () =
  Eio_mock.Backend.run_full @@ fun env ->
  let clock = Eio.Stdenv.mono_clock env in
  let wall = Eio.Stdenv.clock env in
  let log = ref [] in
  let answers =
    [
      [
        `Return (state_frame ~id:"e1" "s1");
        `Return (state_frame ~id:"e2" "s2");
        `Return (state_frame ~id:"e3" "s2");
        `Return (state_frame ~id:"e4" "s3");
        held;
      ];
    ]
  in
  Eio.Switch.run @@ fun sw ->
  let client =
    client_exn ~sw ~mono_clock:clock (subscribe_server ~clock:wall ~log answers)
  in
  let sub = Push.subscribe ~sw client ~types:[ "Email" ] () in
  check_opt_string "the state already held is passed over" (Some "s2")
    (Push.wait_for_state sub ~timeout:60. ~since:"s1" ~type_:"Email" ~account_id
       ());
  check_opt_string "and so is a repeat of it" (Some "s3")
    (Push.wait_for_state sub ~timeout:60. ~since:"s2" ~type_:"Email" ~account_id
       ());
  check_opt_string "nothing further before the deadline" None
    (Push.wait_for_state sub ~timeout:30. ~since:"s3" ~type_:"Email" ~account_id
       ());
  Push.close sub

(* {1 Sync: the loops RFC 8620 leaves to the client}

   Every test here drives Jmap_eio.Sync against a scripted server and counts
   the POSTs, since what Sync adds over Client is exactly the decision of how
   many requests to send. *)

module Sync = Jmap_eio.Sync

let sync_caps = [ Jmap.Proto.Capability.core; Jmap.Proto.Capability.mail ]

(* A session advertising the core limits of RFC 8620 2; every member is
   mandatory. *)
let sync_session_json ?(max_calls_in_request = 16) ~max_objects_in_get () =
  Printf.sprintf
    {|{
  "capabilities": {
    "urn:ietf:params:jmap:core": {
      "maxSizeUpload": 50000000,
      "maxConcurrentUpload": 4,
      "maxSizeRequest": 10000000,
      "maxConcurrentRequests": 2,
      "maxCallsInRequest": %d,
      "maxObjectsInGet": %d,
      "maxObjectsInSet": 500,
      "collationAlgorithms": []
    },
    "urn:ietf:params:jmap:mail": {}
  },
  "accounts": {
    "acc1": {
      "name": "Test Account",
      "isPersonal": true,
      "isReadOnly": false,
      "accountCapabilities": {}
    }
  },
  "primaryAccounts": { "urn:ietf:params:jmap:core": "acc1" },
  "username": "test@example.com",
  "apiUrl": "https://api.example.com/jmap/api/",
  "downloadUrl": "https://api.example.com%s",
  "uploadUrl": "https://api.example.com/jmap/upload/{accountId}/",
  "eventSourceUrl": "https://api.example.com%s",
  "state": "state-1"
}|}
    max_calls_in_request max_objects_in_get default_download
    default_event_source

(* [reply body] is the JSON answer to the API POST carrying [body]. *)
let sync_server ?(max_objects_in_get = 4) ?(max_calls_in_request = 16) log reply
    req =
  let entry = record log req in
  match path req with
  | "/.well-known/jmap" ->
      respond_json
        (sync_session_json ~max_calls_in_request ~max_objects_in_get ())
        req
  | "/jmap/api/" -> respond_json (reply entry.body) req
  | _ -> not_found req

let id_strings l = List.map Jmap.Proto.Id.to_string l

(* The "ids" argument of the single method call in a request body. *)
let ids_in_body body =
  let needle = {|"ids":[|} in
  let n = String.length needle and l = String.length body in
  let rec start i =
    if i + n > l then None
    else if String.sub body i n = needle then Some (i + n)
    else start (i + 1)
  in
  match start 0 with
  | None -> []
  | Some i ->
      let j = String.index_from body i ']' in
      String.split_on_char ',' (String.sub body i (j - i))
      |> List.filter_map (fun s ->
          let s = String.trim s in
          let len = String.length s in
          if len >= 2 then Some (String.sub s 1 (len - 2)) else None)

let query_page ?(state = "q1") ~position ~limit ids =
  Printf.sprintf
    {|{ "methodResponses": [ [ "Email/query",
          { "accountId": "acc1", "queryState": "%s", "canCalculateChanges": true,
            "position": %d, "ids": [%s], "limit": %d }, "c0" ] ],
        "sessionState": "state-1" }|}
    state position (quoted ids) limit

(* RFC 8620 5.5: the response "limit" is the limit the server applied, which
   may be smaller than the one asked for. Paging with the requested limit
   would step past two ids of every page here. *)
let test_sync_pages () =
  let log = ref [] in
  let round = ref 0 in
  let reply _ =
    let i = !round in
    incr round;
    match i with
    | 0 -> query_page ~position:0 ~limit:2 [ "e1"; "e2" ]
    | 1 -> query_page ~position:2 ~limit:2 [ "e3"; "e4" ]
    | _ -> query_page ~position:4 ~limit:2 [ "e5" ]
  in
  Eio_mock.Backend.run @@ fun () ->
  Eio.Switch.run @@ fun sw ->
  let client = client_exn ~sw (sync_server log reply) in
  let pages =
    Sync.pages client ~page_size:5L (fun ~position ~limit ->
        Jmap.Chain.email_query ~account_id ~position ~limit ())
    |> Seq.map (function
      | Ok ids -> ids
      | Error e -> Alcotest.failf "paging failed: %s" (Sync.error_to_string e))
    |> List.of_seq
  in
  Alcotest.(check int) "three pages" 3 (List.length pages);
  Alcotest.(check (list (list string)))
    "pages in order"
    [ [ "e1"; "e2" ]; [ "e3"; "e4" ]; [ "e5" ] ]
    (List.map id_strings pages);
  (* The short last page ends the walk: no fourth request. *)
  match api_posts log with
  | [ p1; p2; p3 ] ->
      Alcotest.(check bool)
        "first page asks for the caller's limit" true
        (contains ~needle:{|"limit":5|} p1.body);
      Alcotest.(check bool)
        ("every page uses the session's capabilities: " ^ p1.body)
        true
        (List.for_all
           (fun (p : recorded) ->
             contains ~needle:(using_needle sync_caps) p.body)
           [ p1; p2; p3 ]);
      (* RFC 8620 5.5 makes position default to 0, so the first page either
         asks for 0 or leaves it out. *)
      Alcotest.(check bool)
        "first page starts at position 0" true
        (contains ~needle:{|"position":0|} p1.body
        || not (contains ~needle:{|"position"|} p1.body));
      Alcotest.(check bool)
        "second page uses the server's limit" true
        (contains ~needle:{|"limit":2|} p2.body);
      Alcotest.(check bool)
        "second page starts after the first" true
        (contains ~needle:{|"position":2|} p2.body);
      Alcotest.(check bool)
        "third page starts after the second" true
        (contains ~needle:{|"position":4|} p3.body)
  | l -> Alcotest.failf "expected 3 API requests, got %d" (List.length l)

(* An empty page ends the walk too, and all_ids honours ?max. *)
let test_sync_all_ids () =
  let log = ref [] in
  let round = ref 0 in
  let reply _ =
    let i = !round in
    incr round;
    if i = 0 then query_page ~position:0 ~limit:2 [ "e1"; "e2" ]
    else query_page ~position:2 ~limit:2 []
  in
  Eio_mock.Backend.run @@ fun () ->
  Eio.Switch.run @@ fun sw ->
  let client = client_exn ~sw (sync_server log reply) in
  let query ~position ~limit =
    Jmap.Chain.email_query ~account_id ~position ~limit ()
  in
  (match Sync.all_ids client ~page_size:2L query with
  | Ok ids ->
      Alcotest.(check (list string))
        "concatenated" [ "e1"; "e2" ] (id_strings ids)
  | Error e -> Alcotest.failf "all_ids failed: %s" (Sync.error_to_string e));
  Alcotest.(check int)
    "the empty page cost one extra request" 2
    (List.length (api_posts log));
  round := 0;
  let log = ref [] in
  let client = client_exn ~sw (sync_server log reply) in
  match
    Sync.all_ids client ~capabilities:sync_caps ~page_size:2L ~max:1 query
  with
  | Error e -> Alcotest.failf "all_ids failed: %s" (Sync.error_to_string e)
  | Ok ids ->
      Alcotest.(check (list string))
        "truncated to max" [ "e1" ] (id_strings ids);
      Alcotest.(check int) "and stopped paging" 1 (List.length (api_posts log))

(* A server that ignores "position" answers the same page for every request,
   and the shorter-page test never fires. This is an error rather than a
   successful but truncated walk. *)
let test_sync_pages_repeated_page () =
  let log = ref [] in
  let round = ref 0 in
  let reply _ =
    incr round;
    (* After a few rounds the server gives up, so that a walk which failed to
       stop fails the test rather than hanging it. *)
    if !round > 5 then
      {|{ "methodResponses": [ [ "error",
          { "type": "serverFail" }, "c0" ] ], "sessionState": "state-1" }|}
    else query_page ~position:0 ~limit:2 [ "e1"; "e2" ]
  in
  Eio_mock.Backend.run @@ fun () ->
  Eio.Switch.run @@ fun sw ->
  let client = client_exn ~sw (sync_server log reply) in
  let query ~position ~limit =
    Jmap.Chain.email_query ~account_id ~position ~limit ()
  in
  (match Sync.all_ids client ~page_size:2L query with
  | Error (Sync.Nonadvancing_query { requested = 2L; returned = 0L }) ->
      Alcotest.(check int)
        "the repeat ends the walk" 2
        (List.length (api_posts log))
  | Error e -> Alcotest.failf "unexpected error: %s" (Sync.error_to_string e)
  | Ok ids ->
      Alcotest.failf "returned a truncated success with %d ids"
        (List.length ids));
  (* A server that echoes the position asked for while re-serving the same
     page defeats the position check, so the ids themselves are the evidence. *)
  let log = ref [] in
  let round = ref 0 in
  let reply _ =
    let position = !round * 2 in
    incr round;
    if !round > 5 then
      {|{ "methodResponses": [ [ "error",
          { "type": "serverFail" }, "c0" ] ], "sessionState": "state-1" }|}
    else query_page ~position ~limit:2 [ "e1"; "e2" ]
  in
  let client = client_exn ~sw (sync_server log reply) in
  match Sync.all_ids client ~page_size:2L query with
  | Error (Sync.Nonadvancing_query { requested = 2L; returned = 2L }) ->
      Alcotest.(check int)
        "the re-served page ends the walk" 2
        (List.length (api_posts log))
  | Error e -> Alcotest.failf "unexpected error: %s" (Sync.error_to_string e)
  | Ok ids ->
      Alcotest.failf "an echoed position hid a repeat of %d ids"
        (List.length ids)

(* A server can advance the echoed position and mint fresh ids forever. That
   defeats repeat detection, so request fuel is the final bound. *)
let test_sync_pages_fuel () =
  let log = ref [] in
  let round = ref 0 in
  let reply _ =
    let position = !round * 2 in
    incr round;
    query_page ~position ~limit:2
      [
        Printf.sprintf "e%d" (position + 1); Printf.sprintf "e%d" (position + 2);
      ]
  in
  Eio_mock.Backend.run @@ fun () ->
  Eio.Switch.run @@ fun sw ->
  let client = client_exn ~sw (sync_server log reply) in
  let query ~position ~limit =
    Jmap.Chain.email_query ~account_id ~position ~limit ()
  in
  match Sync.all_ids client ~page_size:2L ~fuel:3 query with
  | Error (Sync.Page_fuel_exhausted 3) ->
      Alcotest.(check int)
        "fuel is the exact request bound" 3
        (List.length (api_posts log))
  | Error error ->
      Alcotest.failf "unexpected error: %s" (Sync.error_to_string error)
  | Ok ids ->
      Alcotest.failf "accepted an unending query after %d ids" (List.length ids)

(* Pages from different queryState snapshots cannot safely be concatenated:
   records moving between them could otherwise be skipped or repeated. *)
let test_sync_pages_query_state_change () =
  let log = ref [] in
  let round = ref 0 in
  let reply _ =
    let state = if !round = 0 then "q1" else "q2" in
    let position = !round * 2 in
    incr round;
    query_page ~state ~position ~limit:2
      [
        Printf.sprintf "e%d" (position + 1); Printf.sprintf "e%d" (position + 2);
      ]
  in
  Eio_mock.Backend.run @@ fun () ->
  Eio.Switch.run @@ fun sw ->
  let client = client_exn ~sw (sync_server log reply) in
  let query ~position ~limit =
    Jmap.Chain.email_query ~account_id ~position ~limit ()
  in
  match Sync.all_ids client ~page_size:2L query with
  | Error (Sync.Query_state_changed { previous = "q1"; current = "q2" }) ->
      Alcotest.(check int)
        "the differing page ends the walk" 2
        (List.length (api_posts log))
  | Error e -> Alcotest.failf "unexpected error: %s" (Sync.error_to_string e)
  | Ok ids -> Alcotest.failf "mixed snapshots into %d ids" (List.length ids)

(* Protocol UnsignedInt bounds and the local collection cap are checked before
   a lazy page or an empty get can defer the mistake until an HTTP request. *)
let test_sync_argument_bounds () =
  let log = ref [] in
  Eio_mock.Backend.run @@ fun () ->
  Eio.Switch.run @@ fun sw ->
  let client =
    client_exn ~sw (sync_server log (fun _ -> Alcotest.fail "request sent"))
  in
  let query ~position ~limit =
    Jmap.Chain.email_query ~account_id ~position ~limit ()
  in
  let too_large = Int64.succ Jmap.Proto.Int53.Unsigned.max_value in
  Alcotest.check_raises "page size above UnsignedInt"
    (Invalid_argument
       "Jmap_eio.Sync.pages: page_size must be between 1 and 2^53-1") (fun () ->
      let (_ : _ Seq.t) = Sync.pages client ~page_size:too_large query in
      ());
  Alcotest.check_raises "negative all_ids cap"
    (Invalid_argument "Jmap_eio.Sync.all_ids: max must be non-negative")
    (fun () -> ignore (Sync.all_ids client ~max:(-1) query));
  Alcotest.check_raises "zero page fuel"
    (Invalid_argument "Jmap_eio.Sync.pages: fuel must be at least 1") (fun () ->
      ignore (Sync.all_ids client ~fuel:0 query));
  Alcotest.check_raises "maxChanges above UnsignedInt"
    (Invalid_argument "Jmap_eio.Sync: max_changes must be between 1 and 2^53-1")
    (fun () ->
      ignore
        (Sync.email_changes client ~account_id ~since:"s0"
           ~max_changes:too_large ()));
  Alcotest.check_raises "batch above UnsignedInt, even with no ids"
    (Invalid_argument
       "Jmap_eio.Sync.get_all: batch must be between 1 and 2^53-1") (fun () ->
      ignore
        (Sync.get_all client ~batch:too_large [] (fun ~ids ->
             Jmap.Chain.email_get ~account_id ~ids:(Jmap.Chain.ids ids) ())));
  Alcotest.(check int) "no API request was sent" 0 (List.length (api_posts log))

let changes_round ~old_state ~new_state ~more ~created ~updated ~destroyed =
  Printf.sprintf
    {|{ "methodResponses": [ [ "Email/changes",
          { "accountId": "acc1", "oldState": "%s", "newState": "%s",
            "hasMoreChanges": %b, "created": [%s], "updated": [%s],
            "destroyed": [%s] }, "c0" ] ],
        "sessionState": "state-1" }|}
    old_state new_state more (quoted created) (quoted updated)
    (quoted destroyed)

(* RFC 8620 5.2: hasMoreChanges means call again from the newState just
   returned, and the three lists of the rounds fold together under the
   section's rules - created then updated is created, created then destroyed
   is not reported at all. *)
let test_sync_changes_drain () =
  let log = ref [] in
  let round = ref 0 in
  let reply _ =
    let i = !round in
    incr round;
    match i with
    | 0 ->
        changes_round ~old_state:"s0" ~new_state:"s1" ~more:true
          ~created:[ "a"; "b" ] ~updated:[] ~destroyed:[]
    | 1 ->
        changes_round ~old_state:"s1" ~new_state:"s2" ~more:true
          ~created:[ "c" ] ~updated:[ "a" ] ~destroyed:[ "b"; "z" ]
    | _ ->
        changes_round ~old_state:"s2" ~new_state:"s3" ~more:false ~created:[]
          ~updated:[ "d" ] ~destroyed:[ "c" ]
  in
  Eio_mock.Backend.run @@ fun () ->
  Eio.Switch.run @@ fun sw ->
  let client = client_exn ~sw (sync_server log reply) in
  match
    Sync.changes client ~since:"s0" (fun ~since_state ~max_changes ->
        Jmap.Chain.email_changes ~account_id ~since_state ~max_changes ())
  with
  | Error e -> Alcotest.failf "changes failed: %s" (Sync.error_to_string e)
  | Ok `Cannot_calculate_changes -> Alcotest.fail "unexpected resync signal"
  | Ok (`Changes c) ->
      (* a: created then updated. b, c: created then destroyed, so gone.
         z: destroyed without having been created here. d: updated. *)
      Alcotest.(check (list string))
        "created" [ "a" ]
        (id_strings c.Sync.created);
      Alcotest.(check (list string))
        "updated" [ "d" ]
        (id_strings c.Sync.updated);
      Alcotest.(check (list string))
        "destroyed" [ "z" ]
        (id_strings c.Sync.destroyed);
      check_string "newState of the last round" "s3" c.Sync.new_state;
      let posts = api_posts log in
      Alcotest.(check int) "three rounds" 3 (List.length posts);
      List.iteri
        (fun i (p : recorded) ->
          Alcotest.(check bool)
            (Printf.sprintf "round %d resumes from the previous newState" (i + 1))
            true
            (contains ~needle:(Printf.sprintf {|"sinceState":"s%d"|} i) p.body);
          Alcotest.(check bool)
            (Printf.sprintf "round %d uses the session's capabilities" (i + 1))
            true
            (contains ~needle:(using_needle sync_caps) p.body))
        posts

(* RFC 8620 5.2 lets a server return an updated-and-destroyed id in the
   updated list as well, and servers repeat ids around the hasMoreChanges
   boundary. Folding the rounds must therefore be a join with [Destroyed] and
   [Gone] absorbing: an id this drain has already seen destroyed cannot come
   back as created or updated, or the caller keeps a record the server
   deleted. *)
let test_sync_changes_absorbing () =
  let log = ref [] in
  let round = ref 0 in
  let reply _ =
    let i = !round in
    incr round;
    match i with
    (* x is destroyed and y created in the first round ... *)
    | 0 ->
        changes_round ~old_state:"s0" ~new_state:"s1" ~more:true
          ~created:[ "y" ] ~updated:[] ~destroyed:[ "x" ]
    (* ... then x is named again as updated, and y is destroyed ... *)
    | 1 ->
        changes_round ~old_state:"s1" ~new_state:"s2" ~more:true ~created:[]
          ~updated:[ "x" ] ~destroyed:[ "y" ]
    (* ... and the last round names both again, every way it can. *)
    | _ ->
        changes_round ~old_state:"s2" ~new_state:"s3" ~more:false
          ~created:[ "y" ] ~updated:[ "x"; "y" ] ~destroyed:[]
  in
  Eio_mock.Backend.run @@ fun () ->
  Eio.Switch.run @@ fun sw ->
  let client = client_exn ~sw (sync_server log reply) in
  match Sync.email_changes client ~account_id ~since:"s0" () with
  | Error e -> Alcotest.failf "changes failed: %s" (Sync.error_to_string e)
  | Ok `Cannot_calculate_changes -> Alcotest.fail "unexpected resync signal"
  | Ok (`Changes c) ->
      Alcotest.(check (list string))
        "a destroyed id stays destroyed" [ "x" ]
        (id_strings c.Sync.destroyed);
      Alcotest.(check (list string))
        "and is not reported as updated" []
        (id_strings c.Sync.updated);
      Alcotest.(check (list string))
        "an id created and destroyed here is in no list" []
        (id_strings c.Sync.created);
      Alcotest.(check bool) "the drain completed" false c.Sync.has_more

(* The fuel running out is not the same answer as the server saying there is
   nothing more: [has_more] is how a caller that loops "drain, apply, wait
   for a push" knows it must drain again first. *)
let test_sync_changes_has_more () =
  let log = ref [] in
  let round = ref 0 in
  let reply _ =
    let i = !round in
    incr round;
    changes_round ~old_state:(Printf.sprintf "s%d" i)
      ~new_state:(Printf.sprintf "s%d" (i + 1))
      ~more:true
      ~created:[ Printf.sprintf "e%d" i ]
      ~updated:[] ~destroyed:[]
  in
  Eio_mock.Backend.run @@ fun () ->
  Eio.Switch.run @@ fun sw ->
  let client = client_exn ~sw (sync_server log reply) in
  (match Sync.email_changes client ~account_id ~since:"s0" ~fuel:2 () with
  | Ok (`Changes c) ->
      Alcotest.(check bool) "the fuel ran out" true c.Sync.has_more;
      check_string "and says where to resume" "s2" c.Sync.new_state;
      Alcotest.(check int) "two rounds" 2 (List.length (api_posts log))
  | Ok `Cannot_calculate_changes -> Alcotest.fail "unexpected resync signal"
  | Error e -> Alcotest.failf "changes failed: %s" (Sync.error_to_string e));
  (* A server that reports hasMoreChanges without advancing newState stops
     the drain the same way, and says so. *)
  let log = ref [] in
  let reply _ =
    changes_round ~old_state:"s0" ~new_state:"s0" ~more:true ~created:[ "a" ]
      ~updated:[] ~destroyed:[]
  in
  let client = client_exn ~sw (sync_server log reply) in
  (match Sync.email_changes client ~account_id ~since:"s0" () with
  | Error (Sync.Nonadvancing_changes "s0") ->
      Alcotest.(check int)
        "the stuck response is not retried" 1
        (List.length (api_posts log))
  | Error e -> Alcotest.failf "unexpected error: %s" (Sync.error_to_string e)
  | Ok _ -> Alcotest.fail "a nonadvancing cursor was returned as resumable");
  (* A longer cycle is just as nonadvancing as returning the current state. *)
  let log = ref [] in
  let round = ref 0 in
  let reply _ =
    let old_state, new_state =
      if !round = 0 then ("s0", "s1") else ("s1", "s0")
    in
    incr round;
    changes_round ~old_state ~new_state ~more:true ~created:[] ~updated:[]
      ~destroyed:[]
  in
  let client = client_exn ~sw (sync_server log reply) in
  match Sync.email_changes client ~account_id ~since:"s0" () with
  | Error (Sync.Nonadvancing_changes "s0") ->
      Alcotest.(check int)
        "the state cycle ended after two rounds" 2
        (List.length (api_posts log))
  | Error e ->
      Alcotest.failf "unexpected cycle error: %s" (Sync.error_to_string e)
  | Ok _ -> Alcotest.fail "a cyclic changes cursor was returned as resumable"

(* RFC 8620 5.2: cannotCalculateChanges is an instruction to resync, not a
   failure, so it comes back as a value and the drain stops at once. *)
let test_sync_cannot_calculate_changes () =
  let log = ref [] in
  let reply _ =
    {|{ "methodResponses": [ [ "error",
          { "type": "cannotCalculateChanges", "description": "state too old" },
          "c0" ] ], "sessionState": "state-1" }|}
  in
  Eio_mock.Backend.run @@ fun () ->
  Eio.Switch.run @@ fun sw ->
  let client = client_exn ~sw (sync_server log reply) in
  match Sync.email_changes client ~account_id ~since:"ancient" () with
  | Ok `Cannot_calculate_changes ->
      Alcotest.(check int)
        "one request, then stop" 1
        (List.length (api_posts log))
  | Ok (`Changes _) -> Alcotest.fail "the resync signal was swallowed"
  | Error e ->
      Alcotest.failf "reported as an error: %s" (Sync.error_to_string e)

(* Any other method error is a failure of the loop. *)
let test_sync_changes_error () =
  let log = ref [] in
  let reply _ =
    {|{ "methodResponses": [ [ "error", { "type": "accountNotFound" }, "c0" ] ],
        "sessionState": "state-1" }|}
  in
  Eio_mock.Backend.run @@ fun () ->
  Eio.Switch.run @@ fun sw ->
  let client = client_exn ~sw (sync_server log reply) in
  match Sync.email_changes client ~account_id ~since:"s0" () with
  | Ok _ -> Alcotest.fail "expected an error"
  | Error e ->
      check_string "method error" "accountNotFound" (Sync.error_to_string e)

let get_response ~found ~not_found =
  Printf.sprintf
    {|{ "methodResponses": [ [ "Email/get",
          { "accountId": "acc1", "state": "g1", "list": [%s], "notFound": [%s] },
          "c0" ] ], "sessionState": "state-1" }|}
    (String.concat "," (List.map (Printf.sprintf {|{"id":"%s"}|}) found))
    (quoted not_found)

(* RFC 8620 5.1: a /get may not carry more ids than maxObjectsInGet, so ten
   ids at a limit of four is three calls; the objects come back in input
   order however the batches interleave. *)
let test_sync_get_all_batches () =
  let log = ref [] in
  let ids = List.init 10 (fun i -> Printf.sprintf "e%d" (i + 1)) in
  (* The server answers whatever ids the batch asked for, minus one it does
     not know about. *)
  let reply body =
    let asked = ids_in_body body in
    let missing = List.filter (fun i -> i = "e7") asked in
    get_response
      ~found:(List.filter (fun i -> i <> "e7") asked)
      ~not_found:missing
  in
  Eio_mock.Backend.run @@ fun () ->
  Eio.Switch.run @@ fun sw ->
  let client = client_exn ~sw (sync_server log reply) in
  Alcotest.(check (option int64))
    "the session's maxObjectsInGet" (Some 4L)
    (Sync.max_objects_in_get client);
  match
    Sync.get_all client ~batch:100L (List.map Jmap.Proto.Id.of_string_exn ids)
      (fun ~ids ->
        Jmap.Chain.email_get ~account_id ~ids:(Jmap.Chain.ids ids)
          ~properties:[ `Id ] ())
  with
  | Error e -> Alcotest.failf "get_all failed: %s" (Sync.error_to_string e)
  | Ok (objects, not_found) ->
      let posts = api_posts log in
      Alcotest.(check int)
        "10 ids at maxObjectsInGet 4 is 3 requests" 3 (List.length posts);
      Alcotest.(check bool)
        "every batch uses the session's capabilities" true
        (List.for_all
           (fun (p : recorded) ->
             contains ~needle:(using_needle sync_caps) p.body)
           posts);
      (* The batches may be sent in any order, so compare them as a set;
         the input order of the objects is checked below. *)
      Alcotest.(check (list (list string)))
        "batched by four, covering every id"
        (List.sort compare
           [
             [ "e1"; "e2"; "e3"; "e4" ];
             [ "e5"; "e6"; "e7"; "e8" ];
             [ "e9"; "e10" ];
           ])
        (List.sort compare
           (List.map (fun (p : recorded) -> ids_in_body p.body) posts));
      Alcotest.(check (list string))
        "objects in input order"
        [ "e1"; "e2"; "e3"; "e4"; "e5"; "e6"; "e8"; "e9"; "e10" ]
        (List.filter_map
           (fun (e : Jmap.Proto.Email.t) ->
             Option.map Jmap.Proto.Id.to_string e.id)
           objects);
      Alcotest.(check (list string))
        "notFound collected from every batch" [ "e7" ] (id_strings not_found)

(* With one worker, a failed first batch must prevent every later batch from
   being assigned. Returning errors as ordinary worker values and mapping the
   whole list would still send them all. *)
let test_sync_get_all_stops_assigning_after_error () =
  let log = ref [] in
  let reply _ =
    {|{ "methodResponses": [ [ "error", { "type": "serverFail" }, "c0" ] ],
        "sessionState": "state-1" }|}
  in
  Eio_mock.Backend.run @@ fun () ->
  Eio.Switch.run @@ fun sw ->
  let client = client_exn ~sw (sync_server log reply) in
  let ids =
    List.init 6 (fun i -> Jmap.Proto.Id.of_string_exn (Fmt.str "e%d" i))
  in
  let result =
    Sync.get_all client ~batch:2L ~max_concurrent:1 ids (fun ~ids ->
        Jmap.Chain.email_get ~account_id ~ids:(Jmap.Chain.ids ids) ())
  in
  (match result with
  | Error (Sync.Client_error (Client.Method_error _)) -> ()
  | Error e -> Alcotest.failf "unexpected error: %s" (Sync.error_to_string e)
  | Ok _ -> Alcotest.fail "expected the first batch to fail");
  Alcotest.(check int)
    "only the failed batch was sent" 1
    (List.length (api_posts log))

(* A short id list below the advertised maxObjectsInGet is a single request;
   an empty list still sends none. *)
let test_sync_get_all_short () =
  let log = ref [] in
  let server req =
    ignore (record log req);
    match path req with
    | "/.well-known/jmap" ->
        respond_json (sync_session_json ~max_objects_in_get:4096 ()) req
    | "/jmap/api/" ->
        respond_json (get_response ~found:[ "e1"; "e2" ] ~not_found:[]) req
    | _ -> not_found req
  in
  Eio_mock.Backend.run @@ fun () ->
  Eio.Switch.run @@ fun sw ->
  let client = client_exn ~sw server in
  Alcotest.(check (option int64))
    "maxObjectsInGet" (Some 4096L)
    (Sync.max_objects_in_get client);
  let get ~ids =
    Jmap.Chain.email_get ~account_id ~ids:(Jmap.Chain.ids ids)
      ~properties:[ `Id ] ()
  in
  (match Sync.get_all client ~capabilities:sync_caps [] get with
  | Ok ([], []) ->
      Alcotest.(check int) "no ids, no request" 0 (List.length (api_posts log))
  | Ok _ -> Alcotest.fail "an empty id list returned objects"
  | Error e -> Alcotest.failf "get_all failed: %s" (Sync.error_to_string e));
  match
    Sync.get_all client ~capabilities:[ "urn:example:unknown" ]
      (List.map Jmap.Proto.Id.of_string_exn [ "e1"; "e2" ])
      get
  with
  | Error e -> Alcotest.failf "get_all failed: %s" (Sync.error_to_string e)
  | Ok (objects, _) -> (
      Alcotest.(check int) "both objects" 2 (List.length objects);
      match api_posts log with
      | [ post ] ->
          Alcotest.(check bool)
            ("an explicit list is sent as given: " ^ post.body)
            true
            (contains
               ~needle:(using_needle [ "urn:example:unknown" ])
               post.body)
      | l -> Alcotest.failf "expected one batch, got %d" (List.length l))

(* A zero maxObjectsInGet does not license a one-id request. Empty input still
   needs no server support; non-empty input fails locally. *)
let test_sync_get_all_zero_limit () =
  let log = ref [] in
  Eio_mock.Backend.run @@ fun () ->
  Eio.Switch.run @@ fun sw ->
  let client =
    client_exn ~sw
      (sync_server ~max_objects_in_get:0 log (fun _ ->
           Alcotest.fail "API request sent"))
  in
  let get ~ids =
    Jmap.Chain.email_get ~account_id ~ids:(Jmap.Chain.ids ids) ()
  in
  (match Sync.get_all client [] get with
  | Ok ([], []) -> ()
  | Ok _ -> Alcotest.fail "empty input returned objects"
  | Error e -> Alcotest.failf "empty input failed: %s" (Sync.error_to_string e));
  let ids = [ Jmap.Proto.Id.of_string_exn "e1" ] in
  (match Sync.get_all client ids get with
  | Error (Sync.Client_error (Client.Transport (Fetch.Invalid_request _, _))) ->
      ()
  | Error e -> Alcotest.failf "unexpected error: %s" (Sync.error_to_string e)
  | Ok _ -> Alcotest.fail "a non-empty get ignored maxObjectsInGet=0");
  Alcotest.(check int) "no API request" 0 (List.length (api_posts log))

(* RFC 8621 2 gives a role to at most one Mailbox, and RFC 8620 3.7 lets the
   Mailbox/get name the query's ids by back reference, so the lookup is one
   request. *)
let mailbox_by_role_response ~ids ~objects =
  Printf.sprintf
    {|{ "methodResponses": [
         [ "Mailbox/query",
           { "accountId": "acc1", "queryState": "mq1",
             "canCalculateChanges": false, "position": 0, "ids": [%s] },
           "c0" ],
         [ "Mailbox/get",
           { "accountId": "acc1", "state": "ms1", "list": [%s],
             "notFound": [] },
           "c1" ] ],
       "sessionState": "state-1" }|}
    (quoted ids) objects

let mailbox_query_response ids =
  Printf.sprintf
    {|{ "methodResponses": [
         [ "Mailbox/query",
           { "accountId": "acc1", "queryState": "mq1",
             "canCalculateChanges": false, "position": 0, "ids": [%s] },
           "c0" ] ],
       "sessionState": "state-1" }|}
    (quoted ids)

let mailbox_get_response objects =
  Printf.sprintf
    {|{ "methodResponses": [
         [ "Mailbox/get",
           { "accountId": "acc1", "state": "ms1", "list": [%s],
             "notFound": [] },
           "c0" ] ],
       "sessionState": "state-1" }|}
    objects

let test_sync_mailbox_with_role () =
  let log = ref [] in
  let reply _ =
    mailbox_by_role_response ~ids:[ "mb1" ]
      ~objects:{|{ "id": "mb1", "name": "Inbox", "role": "inbox" }|}
  in
  Eio_mock.Backend.run @@ fun () ->
  Eio.Switch.run @@ fun sw ->
  let client = client_exn ~sw (sync_server log reply) in
  (match Sync.mailbox_with_role client ~account_id `Inbox with
  | Error e -> Alcotest.failf "lookup failed: %s" (Sync.error_to_string e)
  | Ok None -> Alcotest.fail "expected the Inbox"
  | Ok (Some mailbox) ->
      check_opt_string "the Mailbox is the one queried for" (Some "mb1")
        (Option.map Jmap.Proto.Id.to_string mailbox.Jmap.Proto.Mailbox.id);
      check_opt_string "with its other properties" (Some "Inbox")
        mailbox.Jmap.Proto.Mailbox.name;
      Alcotest.(check bool)
        "and the role asked for" true
        (mailbox.Jmap.Proto.Mailbox.role = Some `Inbox));
  (match api_posts log with
  | [ post ] ->
      Alcotest.(check bool)
        ("core and mail by default: " ^ post.body)
        true
        (contains
           ~needle:
             (using_needle
                [ Jmap.Proto.Capability.core; Jmap.Proto.Capability.mail ])
           post.body);
      Alcotest.(check bool)
        "the query filters on the role" true
        (contains ~needle:{|"Mailbox/query"|} post.body
        && contains ~needle:{|"role":"inbox"|} post.body);
      Alcotest.(check bool)
        "and the get takes its ids by reference" true
        (contains ~needle:{|"Mailbox/get"|} post.body
        && contains ~needle:{|"#ids"|} post.body)
  | l -> Alcotest.failf "expected one request, got %d" (List.length l));
  (* An account with no Mailbox in that role is not an error. *)
  let log = ref [] in
  let reply _ = mailbox_by_role_response ~ids:[] ~objects:"" in
  let client = client_exn ~sw (sync_server log reply) in
  match Sync.mailbox_with_role client ~account_id `Snoozed with
  | Ok None ->
      Alcotest.(check int) "still one request" 1 (List.length (api_posts log))
  | Ok (Some _) -> Alcotest.fail "expected no Mailbox"
  | Error e -> Alcotest.failf "lookup failed: %s" (Sync.error_to_string e)

(* A server allowing one call per request cannot accept the back-reference
   chain, so the lookup falls back to a query followed by a get. *)
let test_sync_mailbox_with_role_at_one_call_limit () =
  let log = ref [] in
  let reply body =
    if contains ~needle:{|"Mailbox/query"|} body then
      mailbox_query_response [ "mb1" ]
    else
      mailbox_get_response {|{ "id": "mb1", "name": "Inbox", "role": "inbox" }|}
  in
  Eio_mock.Backend.run @@ fun () ->
  Eio.Switch.run @@ fun sw ->
  let client = client_exn ~sw (sync_server ~max_calls_in_request:1 log reply) in
  (match Sync.mailbox_with_role client ~account_id `Inbox with
  | Ok (Some { Jmap.Proto.Mailbox.id = Some id; _ }) ->
      check_string "the mailbox" "mb1" (Jmap.Proto.Id.to_string id)
  | Ok _ -> Alcotest.fail "expected the Inbox with an id"
  | Error e -> Alcotest.failf "lookup failed: %s" (Sync.error_to_string e));
  match api_posts log with
  | [ query; get ] ->
      Alcotest.(check bool)
        "the first request only queries" true
        (contains ~needle:{|"Mailbox/query"|} query.body
        && not (contains ~needle:{|"Mailbox/get"|} query.body));
      Alcotest.(check bool)
        "the second request only gets" true
        (contains ~needle:{|"Mailbox/get"|} get.body
        && not (contains ~needle:{|"Mailbox/query"|} get.body))
  | posts -> Alcotest.failf "expected two requests, got %d" (List.length posts)

(* The same lookup reduced to the id, where an account without that role is a
   failure rather than an empty result. *)
let test_sync_mailbox_id () =
  let log = ref [] in
  let reply _ =
    mailbox_by_role_response ~ids:[ "mb2" ]
      ~objects:{|{ "id": "mb2", "name": "Drafts", "role": "drafts" }|}
  in
  Eio_mock.Backend.run @@ fun () ->
  Eio.Switch.run @@ fun sw ->
  let client = client_exn ~sw (sync_server log reply) in
  (match Sync.mailbox_id client ~account_id `Drafts with
  | Error e -> Alcotest.failf "lookup failed: %s" (Sync.error_to_string e)
  | Ok id ->
      Alcotest.(check string)
        "the id of the Mailbox with the role" "mb2"
        (Jmap.Proto.Id.to_string id);
      Alcotest.(check int) "one request" 1 (List.length (api_posts log)));
  Alcotest.(check string)
    "the exception carries it too" "mb2"
    (Jmap.Proto.Id.to_string (Sync.mailbox_id_exn client ~account_id `Drafts));
  let log = ref [] in
  let reply _ = mailbox_by_role_response ~ids:[] ~objects:"" in
  let client = client_exn ~sw (sync_server log reply) in
  (match Sync.mailbox_id client ~account_id `Archive with
  | Ok _ -> Alcotest.fail "expected no Mailbox"
  | Error (Sync.No_mailbox_with_role `Archive) ->
      Alcotest.(check int) "still one request" 1 (List.length (api_posts log))
  | Error e -> Alcotest.failf "unexpected error: %s" (Sync.error_to_string e));
  Alcotest.(check string)
    "the printed form" "no mailbox has the role archive"
    (Sync.error_to_string (Sync.No_mailbox_with_role `Archive));
  match Sync.mailbox_id_exn client ~account_id `Archive with
  | id ->
      Alcotest.failf "expected the exception, got %s"
        (Jmap.Proto.Id.to_string id)
  | exception Sync.Sync_error e ->
      Alcotest.(check string)
        "the exception prints as the error does"
        "Jmap_eio.Sync.Sync_error: no mailbox has the role archive"
        (Printexc.to_string (Sync.Sync_error e))

let test_sync_mailbox_id_missing_id () =
  let log = ref [] in
  let reply _ =
    mailbox_by_role_response ~ids:[ "mb3" ]
      ~objects:{|{ "name": "Drafts", "role": "drafts" }|}
  in
  Eio_mock.Backend.run @@ fun () ->
  Eio.Switch.run @@ fun sw ->
  let client = client_exn ~sw (sync_server log reply) in
  match Sync.mailbox_id client ~account_id `Drafts with
  | Error (Sync.Mailbox_missing_id `Drafts) ->
      check_string "the distinct error" "the mailbox with role drafts has no id"
        (Sync.error_to_string (Sync.Mailbox_missing_id `Drafts))
  | Error e -> Alcotest.failf "unexpected error: %s" (Sync.error_to_string e)
  | Ok id ->
      Alcotest.failf "expected a missing-id error, got %s"
        (Jmap.Proto.Id.to_string id)

(* RFC 8620 3.7: a back-reference cannot cross a request boundary, so a chain
   is never split; it can only be measured against maxCallsInRequest. *)
let test_sync_chain_limits () =
  let log = ref [] in
  Eio_mock.Backend.run @@ fun () ->
  Eio.Switch.run @@ fun sw ->
  let client = client_exn ~sw (sync_server log (fun _ -> echo_response)) in
  Alcotest.(check (option int64))
    "maxCallsInRequest" (Some 16L)
    (Sync.max_calls_in_request client);
  Alcotest.(check (option int64))
    "maxConcurrentRequests" (Some 2L)
    (Sync.max_concurrent_requests client);
  Alcotest.(check int)
    "two calls in the chain" 2
    (Sync.calls_in_chain ~capabilities:sync_caps (email_chain account_id));
  Alcotest.(check bool)
    "and they fit" true
    (Sync.chain_fits client (email_chain account_id))

(* {1 Command line configuration} *)

(* A malformed command line is a usage error rather than an internal one, so
   [Cmd.eval] reports it and exits with the CLI error status. *)

let cli_vars =
  [
    "JMAP_SESSION_URL";
    "JMAP_API_KEY";
    "JMAP_API_KEY_FILE";
    "JMAP_AUTH";
    "JMAP_ACCOUNT_ID";
    "JMAP_PROFILE";
  ]

(* [Unix.putenv] cannot unset, and the configuration treats an empty setting
   as absent, so "" is how a variable is cleared here. *)
let with_cli_env bindings f =
  let saved =
    List.map
      (fun v -> (v, Option.value ~default:"" (Sys.getenv_opt v)))
      cli_vars
  in
  let set = List.iter (fun (name, value) -> Unix.putenv name value) in
  set (List.map (fun v -> (v, "")) cli_vars);
  set bindings;
  Fun.protect ~finally:(fun () -> set saved) f

let cli_eval args =
  let buf = Buffer.create 256 in
  let ppf = Format.formatter_of_buffer buf in
  let cmd = Cmdliner.Cmd.v (Cmdliner.Cmd.info "jmap-test") Cli.config_term in
  let r =
    Cmdliner.Cmd.eval_value
      ~argv:(Array.of_list ("jmap-test" :: args))
      ~err:ppf ~help:ppf cmd
  in
  Format.pp_print_flush ppf ();
  (r, Buffer.contents buf)

let cli_usage_error name ~needle args =
  match cli_eval args with
  | Error `Term, msg ->
      Alcotest.(check bool) (name ^ ": " ^ msg) true (contains ~needle msg)
  | Error _, msg -> Alcotest.failf "%s: expected a term error, got %S" name msg
  | Ok _, _ -> Alcotest.failf "%s: expected a usage error" name

let cli_config name args =
  match cli_eval args with
  | Ok (`Ok cfg), _ -> cfg
  | _, msg -> Alcotest.failf "%s: expected a configuration, got %S" name msg

let test_cli_usage_errors () =
  with_cli_env [] (fun () ->
      cli_usage_error "no url" ~needle:"no session URL" [];
      cli_usage_error "no key" ~needle:"no API key" [ "--url"; well_known ]);
  with_cli_env
    [ ("JMAP_AUTH", "digest") ]
    (fun () ->
      cli_usage_error "unknown scheme"
        ~needle:"expected \"bearer\" or \"basic\""
        [ "--url"; well_known; "--api-key"; "k" ]);
  with_cli_env [] (fun () ->
      cli_usage_error "a colonless basic key" ~needle:"user:password"
        [ "--url"; well_known; "--api-key"; "nocolon"; "--auth"; "basic" ];
      cli_usage_error "two command-line key sources"
        ~needle:"cannot be used together"
        [
          "--url"; well_known; "--api-key"; "token"; "--api-key-file"; "secret";
        ]);
  with_cli_env
    [ ("JMAP_AUTH", "basic"); ("JMAP_API_KEY", "nocolon") ]
    (fun () ->
      cli_usage_error "a colonless basic key from the environment"
        ~needle:"JMAP_API_KEY" [ "--url"; well_known ])

let test_cli_config () =
  with_cli_env
    [ ("JMAP_API_KEY", "TOKEN") ]
    (fun () ->
      let cfg = cli_config "url and key" [ "--url"; well_known ] in
      check_string "url" well_known cfg.Cli.session_url;
      Alcotest.(check bool)
        "the key came from the environment" true
        (cfg.Cli.api_key_source = Cli.Env "JMAP_API_KEY");
      Alcotest.(check bool)
        "and the scheme from the default" true
        (cfg.Cli.auth_source = Cli.Default);
      let printed = Fmt.str "%a" Cli.pp_config cfg in
      Alcotest.(check bool)
        ("the scheme is printed: " ^ printed)
        true
        (contains ~needle:"auth" printed && contains ~needle:"bearer" printed);
      Alcotest.(check bool)
        ("and the debug flag: " ^ printed)
        true
        (contains ~needle:"debug" printed);
      Alcotest.(check bool)
        "the secret is not" true
        (not (contains ~needle:"TOKEN" printed)));
  with_cli_env
    [ ("JMAP_API_KEY_FILE", "environment-secret") ]
    (fun () ->
      let cfg =
        cli_config "command-line key precedence"
          [ "--url"; well_known; "--api-key"; "COMMAND" ]
      in
      check_string "command-line key" "COMMAND" cfg.api_key;
      Alcotest.(check (option string))
        "environment file ignored" None cfg.api_key_file;
      Alcotest.(check bool)
        "command-line source" true
        (cfg.api_key_source = Cli.Cmdline));
  with_cli_env
    [ ("JMAP_API_KEY", "ENVIRONMENT") ]
    (fun () ->
      let cfg =
        cli_config "command-line file precedence"
          [ "--url"; well_known; "--api-key-file"; "command-secret" ]
      in
      check_string "direct key cleared" "" cfg.api_key;
      Alcotest.(check (option string))
        "command-line file" (Some "command-secret") cfg.api_key_file;
      Alcotest.(check bool)
        "command-line source" true
        (cfg.api_key_source = Cli.Cmdline));
  with_cli_env [] (fun () ->
      let cfg =
        cli_config "secure transport by default"
          [ "--url"; well_known; "--api-key"; "TOKEN" ]
      in
      Alcotest.(check bool) "cleartext refused" false cfg.allow_insecure;
      let cfg =
        cli_config "explicit cleartext opt-in"
          [ "--url"; well_known; "--api-key"; "TOKEN"; "--allow-insecure" ]
      in
      Alcotest.(check bool) "cleartext allowed" true cfg.allow_insecure)

let test_cli_terminal_text () =
  let role = Jmap.Proto.Mailbox.role_of_string "custom\027[2J" in
  check_string "custom mailbox roles cannot control the terminal"
    "custom\\x1B[2J"
    (Cli.terminal_text (Jmap.Proto.Mailbox.role_to_string role));
  check_string "sync diagnostics use the same escaping"
    "no mailbox has the role custom\\x1B[2J"
    (Sync.error_to_string (Sync.No_mailbox_with_role role))

let test_cli_auth_value_reports_invalid_config () =
  with_cli_env
    [ ("JMAP_API_KEY", "TOKEN") ]
    (fun () ->
      let cfg = cli_config "manual config" [ "--url"; well_known ] in
      let cfg =
        {
          cfg with
          Cli.auth = Basic;
          auth_source = Cmdline;
          api_key = "no-separator";
          api_key_file = None;
        }
      in
      match Cli.auth_value cfg with
      | Error msg ->
          Alcotest.(check bool)
            ("error names the required form: " ^ msg)
            true
            (contains ~needle:"user:password" msg)
      | Ok _ -> Alcotest.fail "expected malformed config to be rejected")

(* An account id that is not one, and a session with no primary mail account,
   are both values to report rather than reasons to exit. *)
let test_cli_account_id () =
  let server req =
    match path req with
    | "/.well-known/jmap" -> respond_json (session_json ()) req
    | _ -> not_found req
  in
  Eio_mock.Backend.run @@ fun () ->
  Eio.Switch.run @@ fun sw ->
  let client = client_exn ~sw server in
  let cfg =
    with_cli_env
      [ ("JMAP_API_KEY", "TOKEN") ]
      (fun () -> cli_config "account" [ "--url"; well_known ])
  in
  (match Cli.account_id cfg client with
  | Ok id ->
      Alcotest.failf "expected no account, got %s" (Jmap.Proto.Id.to_string id)
  | Error msg ->
      Alcotest.(check bool)
        ("no primary mail account: " ^ msg)
        true
        (contains ~needle:"--account" msg));
  let cfg = { cfg with Cli.account_id = Some "acc1" } in
  (match Cli.account_id cfg client with
  | Ok id ->
      check_string "the named account" "acc1" (Jmap.Proto.Id.to_string id)
  | Error msg -> Alcotest.failf "expected acc1, got %S" msg);
  let cfg = { cfg with Cli.account_id = Some "" } in
  match Cli.account_id cfg client with
  | Ok _ -> Alcotest.fail "expected the empty account id to be refused"
  | Error msg ->
      Alcotest.(check bool)
        ("an invalid id is reported: " ^ msg)
        true
        (contains ~needle:"invalid account id" msg)

(* [Cli.main] ends in [exit], so the frame it puts around a program is
   exercised by re-running this binary in a child process and reading its
   status. *)
let cli_main_var = "JMAP_TEST_CLI_MAIN"

let () =
  match Sys.getenv_opt cli_main_var with
  | None | Some "" -> ()
  | Some _ ->
      let args =
        Cmdliner.Arg.(
          value & opt int 1 & info [ "limit" ] ~docv:"N" ~doc:"How many.")
      in
      Cli.main' "jmap-main-test" ~doc:"Exercise the command frame" ~args
      @@ fun ctx n ->
      Fmt.pr "%s %d@." (Jmap.Proto.Id.to_string ctx.Cli.account_id) n

let run_cli_main argv =
  let out = Filename.temp_file "jmap-main" ".out" in
  let fd = Unix.openfile out [ Unix.O_WRONLY; Unix.O_TRUNC ] 0o600 in
  let inherited =
    List.filter
      (fun binding ->
        not
          (List.exists
             (fun v -> String.starts_with ~prefix:(v ^ "=") binding)
             cli_vars))
      (Array.to_list (Unix.environment ()))
  in
  let env = Array.of_list ((cli_main_var ^ "=1") :: inherited) in
  let pid =
    Unix.create_process_env Sys.executable_name
      (Array.of_list (Sys.executable_name :: argv))
      env Unix.stdin fd fd
  in
  Unix.close fd;
  let rec wait () =
    try Unix.waitpid [] pid with Unix.Unix_error (Unix.EINTR, _, _) -> wait ()
  in
  let code =
    match snd (wait ()) with
    | Unix.WEXITED c -> c
    | Unix.WSIGNALED n | Unix.WSTOPPED n -> -n
  in
  let ic = open_in_bin out in
  let text = really_input_string ic (in_channel_length ic) in
  close_in ic;
  Sys.remove out;
  (code, text)

let test_cli_main_manual () =
  let code, out = run_cli_main [ "--help=plain" ] in
  Alcotest.(check int) ("the manual is printed: " ^ out) 0 code;
  Alcotest.(check bool)
    ("the command's own options are in it: " ^ out)
    true
    (contains ~needle:"--limit" out);
  Alcotest.(check bool)
    ("and the environment variables: " ^ out)
    true
    (contains ~needle:"JMAP_SESSION_URL" out
    && contains ~needle:"Exercise the command frame" out)

let test_cli_main_reports_failures () =
  let code, out = run_cli_main [] in
  Alcotest.(check int)
    ("a missing session URL is a usage error: " ^ out)
    Cmdliner.Cmd.Exit.cli_error code;
  Alcotest.(check bool)
    ("naming what is missing: " ^ out)
    true
    (contains ~needle:"no session URL" out);
  let code, out =
    run_cli_main
      [ "--url"; "http://127.0.0.1:1/.well-known/jmap"; "--api-key"; "k" ]
  in
  Alcotest.(check int) ("a failed connection is status 1: " ^ out) 1 code;
  Alcotest.(check bool)
    ("reported under the command name: " ^ out)
    true
    (contains ~needle:"jmap-main-test:" out)

let () =
  Alcotest.run "jmap-eio"
    [
      ( "client",
        [
          Alcotest.test_case "session and request" `Quick
            test_session_and_request;
          Alcotest.test_case "well-known redirect" `Quick
            test_well_known_redirect;
          Alcotest.test_case "a cross-site session redirect is refused" `Quick
            test_session_redirect_off_site_is_refused;
          Alcotest.test_case "trust_redirects allows it" `Quick
            test_session_redirect_off_site_with_trust;
          Alcotest.test_case "a redirect between IP literals is refused" `Quick
            test_session_redirect_between_ip_literals_is_refused;
          Alcotest.test_case "an anonymous client follows any redirect" `Quick
            test_session_redirect_without_a_credential;
          Alcotest.test_case "non-redirect 3xx is an HTTP error" `Quick
            test_non_redirect_3xx_is_http_error;
          Alcotest.test_case "problem+json" `Quick test_problem_json;
          Alcotest.test_case "http error" `Quick test_http_error;
          Alcotest.test_case "upload" `Quick test_upload;
          Alcotest.test_case "download" `Quick test_download;
          Alcotest.test_case "credential scope" `Quick test_credential_scope;
          Alcotest.test_case "cookie origins and file mode" `Quick
            test_cookie_origin_isolation;
          Alcotest.test_case "refresh replaces credential scope" `Quick
            test_refresh_replaces_credential_scope;
          Alcotest.test_case "transport endpoint allowlist" `Quick
            test_transport_restricts_session_endpoints;
          Alcotest.test_case "connection error" `Quick test_connection_error;
          Alcotest.test_case "body limit" `Quick test_body_limit;
          Alcotest.test_case "malformed JSON response" `Quick
            test_malformed_json_response;
          Alcotest.test_case "JSON nesting limit" `Quick test_json_nesting_limit;
          Alcotest.test_case "download with type" `Quick test_download_with_type;
          Alcotest.test_case "relative url templates" `Quick
            test_relative_templates;
          Alcotest.test_case "session template validation" `Quick
            test_session_template_validation;
          Alcotest.test_case "literal encoded template brace" `Quick
            test_literal_encoded_template_brace;
          Alcotest.test_case "chain round trip" `Quick test_chain_round_trip;
          Alcotest.test_case "chain error" `Quick test_chain_error;
          Alcotest.test_case "invalid chain is returned" `Quick
            test_invalid_chain_is_client_error;
          Alcotest.test_case "session request and upload limits" `Quick
            test_client_enforces_advertised_sizes;
          Alcotest.test_case "the using array defaults from the session" `Quick
            test_chain_default_capabilities;
          Alcotest.test_case "call decodes the one handle" `Quick
            test_call_decodes;
          Alcotest.test_case "call reports a method error" `Quick
            test_call_method_error;
          Alcotest.test_case "call leaves a transport error alone" `Quick
            test_call_transport_error;
          Alcotest.test_case "run reads a two handle chain" `Quick
            test_run_two_handles;
          Alcotest.test_case "run survives an attempted call" `Quick
            test_run_attempt;
          Alcotest.test_case "run_with_response keeps the response" `Quick
            test_run_with_response;
        ] );
      ( "codec",
        [
          Alcotest.test_case "JSON nesting limit" `Quick test_codec_depth_limit;
        ] );
      ( "push",
        [
          Alcotest.test_case "event decoding" `Quick test_sse_decode;
          Alcotest.test_case "event source url" `Quick test_event_source_url;
          Alcotest.test_case "streaming dispatch and stop" `Quick
            test_listen_dispatch;
          Alcotest.test_case "callback exceptions propagate" `Quick
            test_listen_callback_exception_propagates;
          Alcotest.test_case "closeafter ends the connection" `Quick
            test_listen_close_after;
          Alcotest.test_case "end of stream and errors" `Quick
            test_listen_errors;
          Alcotest.test_case "body read error context" `Quick
            test_listen_body_error_context;
          Alcotest.test_case "raw Unix connection error" `Quick
            test_listen_unix_error;
          Alcotest.test_case "an oversized event is fatal" `Quick
            test_listen_oversized_event;
        ] );
      ( "push subscribe",
        [
          Alcotest.test_case "reconnect with Last-Event-ID" `Quick
            test_subscribe_reconnect;
          Alcotest.test_case "bounded stream" `Quick test_subscribe_backpressure;
          Alcotest.test_case "close" `Quick test_subscribe_close;
          Alcotest.test_case "close with a full consumer stream" `Quick
            test_subscribe_close_with_full_stream;
          Alcotest.test_case "fatal error with a full inner stream" `Quick
            test_subscribe_fatal_with_full_inner_stream;
          Alcotest.test_case "refusal bodies are read before release" `Quick
            test_subscribe_rejection_body_before_close;
          Alcotest.test_case "fatal error context" `Quick
            test_subscribe_error_context;
          Alcotest.test_case "unauthorised ends it" `Quick
            test_subscribe_unauthorised;
          Alcotest.test_case "a fatal transport error ends it" `Quick
            test_subscribe_fatal_transport_error;
          Alcotest.test_case "a 5xx is retried" `Quick
            test_subscribe_retries_server_error;
          Alcotest.test_case "a cancelled daemon resolves its result" `Quick
            test_subscribe_switch_cancels_a_parked_daemon;
          Alcotest.test_case "the switch ends the stream" `Quick
            test_subscribe_switch_ends_the_stream;
          Alcotest.test_case "backoff, poll and capacity are validated" `Quick
            test_subscribe_validates_its_arguments;
          Alcotest.test_case "a transport with no monotonic clock is refused"
            `Quick test_subscribe_needs_a_mono_clock;
          Alcotest.test_case "a stray exception is an error" `Quick
            test_subscribe_stray_exception;
          Alcotest.test_case "backoff growth" `Quick
            test_subscribe_backoff_growth;
          Alcotest.test_case "a redirected connection is one attempt" `Quick
            test_subscribe_follows_a_redirect;
          Alcotest.test_case "poll cycle" `Quick test_subscribe_poll;
          Alcotest.test_case "polling closes a full stream on time" `Quick
            test_subscribe_poll_with_full_stream;
          Alcotest.test_case "polling bounds response headers" `Quick
            test_subscribe_poll_bounds_response_headers;
          Alcotest.test_case "wait_for_state" `Quick test_wait_for_state;
          Alcotest.test_case "wait_for_state skips a state already held" `Quick
            test_wait_for_state_since;
        ] );
      ( "cli",
        [
          Alcotest.test_case "usage errors" `Quick test_cli_usage_errors;
          Alcotest.test_case "sources and printing" `Quick test_cli_config;
          Alcotest.test_case "terminal text" `Quick test_cli_terminal_text;
          Alcotest.test_case "manual invalid config is rejected" `Quick
            test_cli_auth_value_reports_invalid_config;
          Alcotest.test_case "account id" `Quick test_cli_account_id;
          Alcotest.test_case "main prints a manual" `Quick test_cli_main_manual;
          Alcotest.test_case "main reports failures" `Quick
            test_cli_main_reports_failures;
        ] );
      ( "sync",
        [
          Alcotest.test_case "query paging follows the server limit" `Quick
            test_sync_pages;
          Alcotest.test_case "a repeated page is an error" `Quick
            test_sync_pages_repeated_page;
          Alcotest.test_case "query paging has finite fuel" `Quick
            test_sync_pages_fuel;
          Alcotest.test_case "query state is stable across pages" `Quick
            test_sync_pages_query_state_change;
          Alcotest.test_case "all_ids concatenates and caps" `Quick
            test_sync_all_ids;
          Alcotest.test_case "sync arguments are bounded early" `Quick
            test_sync_argument_bounds;
          Alcotest.test_case "changes drains hasMoreChanges" `Quick
            test_sync_changes_drain;
          Alcotest.test_case "destroyed and gone are absorbing" `Quick
            test_sync_changes_absorbing;
          Alcotest.test_case "an incomplete drain says so" `Quick
            test_sync_changes_has_more;
          Alcotest.test_case "cannotCalculateChanges is a value" `Quick
            test_sync_cannot_calculate_changes;
          Alcotest.test_case "a method error stops the drain" `Quick
            test_sync_changes_error;
          Alcotest.test_case "get_all batches by maxObjectsInGet" `Quick
            test_sync_get_all_batches;
          Alcotest.test_case "get_all stops assigning after an error" `Quick
            test_sync_get_all_stops_assigning_after_error;
          Alcotest.test_case "get_all short and empty input" `Quick
            test_sync_get_all_short;
          Alcotest.test_case "get_all rejects a zero server limit" `Quick
            test_sync_get_all_zero_limit;
          Alcotest.test_case "chain call limits" `Quick test_sync_chain_limits;
          Alcotest.test_case "mailbox_with_role is one request" `Quick
            test_sync_mailbox_with_role;
          Alcotest.test_case "mailbox lookup obeys a one-call limit" `Quick
            test_sync_mailbox_with_role_at_one_call_limit;
          Alcotest.test_case "mailbox_id fails on a missing role" `Quick
            test_sync_mailbox_id;
          Alcotest.test_case "mailbox_id distinguishes a missing id" `Quick
            test_sync_mailbox_id_missing_id;
        ] );
    ]
