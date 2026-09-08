(** Client-Server API endpoint modules against a mock homeserver:
    {!Matrix_client.Server}, {!Matrix_client.Search},
    {!Matrix_client.Notifications}, {!Matrix_client.Relations},
    {!Matrix_client.Report}, {!Matrix_client.Tags}, {!Matrix_client.Openid},
    {!Matrix_client.Thirdparty}, {!Matrix_client.Delayed_events}, and part of
    {!Matrix_client.Rooms}, {!Matrix_client.Auth}, {!Matrix_client.Profile} and
    {!Matrix_client.Directory}.

    Each test pins the request that actually leaves the library — method, URL
    including percent-encoding and query order, and body — and decodes a canned
    reply into the module's typed record. The harness is the same {!Fetch_mock}
    one [test_matrix_client.ml] uses, copied rather than shared so the two files
    stay independent. *)

module Client = Matrix_client.Client
module Error = Matrix_client.Error
module Server = Matrix_client.Server
module Search = Matrix_client.Search
module Notifications = Matrix_client.Notifications
module Push = Matrix_client.Push
module Push_rules = Matrix_proto.Push
module Relations = Matrix_client.Relations
module Report = Matrix_client.Report
module Tags = Matrix_client.Tags
module Openid = Matrix_client.Openid
module Thirdparty = Matrix_client.Thirdparty
module Delayed_events = Matrix_client.Delayed_events
module Rooms = Matrix_client.Rooms
module Auth = Matrix_client.Auth
module Uiaa = Matrix_client.Uiaa
module Profile = Matrix_client.Profile
module Directory = Matrix_client.Directory
module Id = Matrix_proto.Id

(* {1 Harness} *)

let mock_env =
  object
    method secure_random =
      Eio.Flow.string_source (String.init 4096 (fun i -> Char.chr (i land 255)))
  end

type recorded = {
  meth : string;
  url : string;
  headers : Http.Header.t;
  body : string option;
}

let body_of_request (req : Fetch.Middleware.request) =
  match req.body with
  | Fetch.Empty -> None
  | Fetch.String s -> Some s
  | Fetch.Stream _ -> Some "<stream>"

let record log (req : Fetch.Middleware.request) =
  log :=
    {
      meth = Http.Method.to_string req.meth;
      url = Fetch.Middleware.Url.to_string req.url;
      headers = req.headers;
      body = body_of_request req;
    }
    :: !log

let mock handler =
  let log = ref [] in
  let client =
    Fetch_mock.client (fun req ->
        record log req;
        handler req)
  in
  (log, client)

(* A client that answers a scripted sequence of bodies, one per request. *)
let mock_seq bodies =
  let log = ref [] in
  let remaining = ref bodies in
  let client =
    Fetch_mock.client (fun req ->
        record log req;
        match !remaining with
        | [] -> Alcotest.fail "more requests than scripted responses"
        | body :: rest ->
            remaining := rest;
            Fetch_mock.respond body req)
  in
  (log, client)

let default_homeserver = "https://hs.example"

let client_of ?(homeserver = default_homeserver) fetch =
  let config = Client.config ~homeserver:(Uriz.of_string_exn homeserver) () in
  Client.create ~config ~fetch ~random:(Matrix_client.Random.of_env mock_env)

let uid s = Result.get_ok (Id.User_id.of_string s)
let did s = Result.get_ok (Id.Device_id.of_string s)
let rid s = Result.get_ok (Id.Room_id.of_string s)
let eid s = Result.get_ok (Id.Event_id.of_string s)
let alias s = Result.get_ok (Id.Room_alias.of_string s)

let test_session : Client.session =
  {
    user_id = uid "@alice:example.org";
    access_token = "syt_secret_token";
    device_id = did "TESTDEVICE";
    refresh_token = None;
  }

(* Every endpoint here but the discovery ones is authenticated. *)
let logged_in fetch = Client.with_session (client_of fetch) test_session
let requests log = List.rev !log

let one_request log =
  match requests log with
  | [ r ] -> r
  | rs -> Alcotest.failf "expected exactly one request, got %d" (List.length rs)

let check_string = Alcotest.(check string)
let check_str_opt = Alcotest.(check (option string))
let check_int = Alcotest.(check int)
let check_bool = Alcotest.(check bool)
let run f () = Eio_mock.Backend.run f
let json body = Fetch_mock.respond body

let contains haystack needle =
  let hn = String.length needle and hh = String.length haystack in
  let rec go i =
    i + hn <= hh && (String.sub haystack i hn = needle || go (i + 1))
  in
  hn = 0 || go 0

let ok = function
  | Ok v -> v
  | Error e -> Alcotest.failf "expected Ok, got error: %s" (Error.to_string e)

(* [check_request] asserts on the single recorded exchange. *)
let check_request log ~meth ~url ?body () =
  let r = one_request log in
  check_string "method" meth r.meth;
  check_string "url" url r.url;
  match body with None -> () | Some b -> check_str_opt "body" (Some b) r.body

let check_discovery_then_request log ~meth ~url ?body () =
  match requests log with
  | [ discovery; request ] -> (
      check_string "discovery method" "GET" discovery.meth;
      check_string "discovery url" "https://hs.example/_matrix/client/versions"
        discovery.url;
      check_string "method" meth request.meth;
      check_string "url" url request.url;
      match body with
      | None -> ()
      | Some b -> check_str_opt "body" (Some b) request.body)
  | rs ->
      Alcotest.failf "expected discovery and request, got %d" (List.length rs)

(* Compare a decoded [Jsont.json] by re-encoding it. *)
let json_string j =
  match Jsont_bytesrw.encode_string Matrix_proto.Json.Codec.json j with
  | Ok s -> s
  | Error e -> Alcotest.failf "re-encoding failed: %s" e

(* {1 Server: versions, capabilities, well-known, discovery} *)

let test_versions () =
  let log, fetch =
    mock
      (json
         {|{"versions":["v1.1","v1.11"],
            "unstable_features":{"org.matrix.msc3575":true,
                                 "org.matrix.msc4140":false}}|})
  in
  let v = ok (Server.get_versions (client_of fetch)) in
  check_request log ~meth:"GET"
    ~url:"https://hs.example/_matrix/client/versions" ();
  Alcotest.(check (list string)) "versions" [ "v1.1"; "v1.11" ] v.versions;
  check_bool "supports v1.11" true (Server.supports_version v "v1.11");
  check_bool "no v1.12" false (Server.supports_version v "v1.12");
  check_bool "v1.11 meets v1.10" true
    (Server.supports_version_at_least v ~major:1 ~minor:10);
  check_bool "v1.11 does not meet v1.12" false
    (Server.supports_version_at_least v ~major:1 ~minor:12);
  check_bool "legacy versions do not masquerade as stable" false
    (Server.supports_version_at_least
       { v with versions = [ "r0.6.1" ] }
       ~major:1 ~minor:0);
  check_bool "msc3575 enabled" true
    (Server.has_unstable_feature v "org.matrix.msc3575");
  check_bool "msc4140 disabled" false
    (Server.has_unstable_feature v "org.matrix.msc4140");
  check_bool "unknown feature" false (Server.has_unstable_feature v "nope")

let test_capabilities () =
  let log, fetch =
    mock
      (json
         {|{"capabilities":{
              "m.change_password":{"enabled":false},
              "m.room_versions":{"default":"10",
                                 "available":{"1":"stable","11":"unstable"}},
              "m.set_displayname":{"enabled":true},
              "m.3pid_changes":{"enabled":false},
              "m.get_login_token":{"enabled":true},
              "m.forget_forced_upon_leave":{"enabled":true},
              "com.example.custom":{"whatever":1}}}|})
  in
  let c = ok (Server.get_capabilities (logged_in fetch)) in
  check_request log ~meth:"GET"
    ~url:"https://hs.example/_matrix/client/v3/capabilities" ();
  Alcotest.(check (option bool))
    "change_password" (Some false) c.change_password;
  Alcotest.(check (option bool)) "set_displayname" (Some true) c.set_displayname;
  Alcotest.(check (option bool)) "set_avatar_url absent" None c.set_avatar_url;
  Alcotest.(check (option bool))
    "3pid_changes" (Some false) c.thirdparty_id_changes;
  Alcotest.(check (option bool)) "get_login_token" (Some true) c.get_login_token;
  (match c.room_versions with
  | None -> Alcotest.fail "expected m.room_versions"
  | Some rv ->
      check_string "default room version" "10" rv.default;
      Alcotest.(check (list string))
        "available versions" [ "1"; "11" ]
        (List.map fst rv.available);
      check_bool "v1 stable" true (List.assoc "1" rv.available = `Stable);
      check_bool "v11 unstable" true (List.assoc "11" rv.available = `Unstable));
  (* Capabilities the module does not model survive verbatim, sorted. *)
  Alcotest.(check (list string))
    "custom keys"
    [ "com.example.custom"; "m.forget_forced_upon_leave" ]
    (List.map fst c.custom);
  check_str_opt "custom value" (Some {|{"whatever":1}|})
    (Option.map json_string
       (Server.find_capability c ~name:"com.example.custom"))

let test_homeserver_capability_helpers () =
  (* These defaults mirror the pinned Ruma capability structs: password and
     3PID changes default to enabled, while login tokens, forced forgetting,
     and account moderation actions default to disabled. *)
  let log, fetch = mock (json {|{"capabilities":{}}|}) in
  let client = logged_in fetch in
  check_bool "absent password capability defaults true" true
    (ok (Server.can_change_password client));
  check_bool "absent 3PID capability defaults true" true
    (ok (Server.can_change_thirdparty_ids client));
  check_bool "absent login-token capability defaults false" false
    (ok (Server.can_get_login_token client));
  let room_versions = ok (Server.room_versions client) in
  check_string "absent room-version default" "1" room_versions.default;
  Alcotest.(check (list string))
    "absent room-version availability" [ "1" ]
    (List.map fst room_versions.available);
  check_bool "absent room version 1 is stable" true
    (List.assoc "1" room_versions.available = `Stable);
  let moderation = ok (Server.account_moderation client) in
  check_bool "absent moderation suspend defaults false" false moderation.suspend;
  check_bool "absent moderation lock defaults false" false moderation.lock;
  check_bool "absent forget capability defaults false" false
    (ok (Server.forgets_room_when_leaving client));
  check_int "helpers share the cached capability response" 1
    (List.length (requests log));

  let log, fetch =
    mock
      (json
         {|{"capabilities":{
              "m.change_password":{"enabled":false},
              "m.3pid_changes":{"enabled":false},
              "m.get_login_token":{"enabled":true},
              "m.room_versions":{"default":"11",
                                   "available":{"1":"stable","11":"unstable"}},
              "m.account_moderation":{"suspend":true},
              "m.forget_forced_upon_leave":{"enabled":true}}}|})
  in
  let client = logged_in fetch in
  check_bool "explicit password capability" false
    (ok (Server.can_change_password client));
  check_bool "explicit 3PID capability" false
    (ok (Server.can_change_thirdparty_ids client));
  check_bool "explicit login-token capability" true
    (ok (Server.can_get_login_token client));
  let room_versions = ok (Server.room_versions client) in
  check_string "explicit room-version default" "11" room_versions.default;
  check_bool "explicit room version 11 is unstable" true
    (List.assoc "11" room_versions.available = `Unstable);
  let moderation = ok (Server.account_moderation client) in
  check_bool "explicit moderation suspend" true moderation.suspend;
  check_bool "omitted moderation lock defaults false" false moderation.lock;
  check_bool "explicit forget capability" true
    (ok (Server.forgets_room_when_leaving client));
  check_int "explicit helpers share the cached capability response" 1
    (List.length (requests log));
  check_bool "moderation remains available as raw custom JSON" true
    (Option.is_some
       (Server.find_capability
          (ok (Server.get_capabilities client))
          ~name:"m.account_moderation"));

  (* A present malformed capability is a protocol error, not its absent
     default. Both failures still reuse the one cached response. *)
  let log, fetch =
    mock
      (json
         {|{"capabilities":{
              "m.account_moderation":[],
              "m.forget_forced_upon_leave":{"enabled":"yes"}}}|})
  in
  let client = logged_in fetch in
  (match Server.account_moderation client with
  | Error (Error.Json_error _) -> ()
  | Error error ->
      Alcotest.failf "malformed moderation returned %s" (Error.to_string error)
  | Ok _ -> Alcotest.fail "malformed moderation capability was accepted");
  (match Server.forgets_room_when_leaving client with
  | Error (Error.Json_error _) -> ()
  | Error error ->
      Alcotest.failf "malformed forced-forget returned %s"
        (Error.to_string error)
  | Ok _ -> Alcotest.fail "malformed forced-forget capability was accepted");
  check_int "malformed helpers share the cached capability response" 1
    (List.length (requests log))

let test_server_metadata_cache () =
  let log, fetch =
    mock_seq [ {|{"versions":["v1.11"]}|}; {|{"versions":["v1.12"]}|} ]
  in
  let client = client_of fetch in
  ignore (ok (Server.get_versions client));
  ignore (ok (Server.get_versions client));
  check_int "cached versions makes one request" 1 (List.length (requests log));
  Server.invalidate_cache client;
  let v = ok (Server.get_versions client) in
  check_int "invalidation refetches" 2 (List.length (requests log));
  Alcotest.(check (list string)) "refetched versions" [ "v1.12" ] v.versions

let test_server_metadata_failure_is_retried () =
  let log, fetch =
    mock_seq [ {|{"versions":not-json}|}; {|{"versions":["v1.11"]}|} ]
  in
  let client = client_of fetch in
  (match Server.get_versions client with
  | Error (Error.Json_error _) -> ()
  | Error e -> Alcotest.failf "expected JSON error, got %s" (Error.to_string e)
  | Ok _ -> Alcotest.fail "malformed metadata unexpectedly decoded");
  ignore (ok (Server.get_versions client));
  check_int "failed response is retried" 2 (List.length (requests log))

let test_server_metadata_http_failure_is_retried () =
  let log = ref [] in
  let first = ref true in
  let fetch =
    Fetch_mock.client (fun req ->
        record log req;
        if !first then begin
          first := false;
          Fetch_mock.respond ~status:503 {|{"errcode":"M_UNAVAILABLE"}|} req
        end
        else Fetch_mock.respond {|{"versions":["v1.11"]}|} req)
  in
  let client = client_of fetch in
  (match Server.get_versions client with
  | Error (Error.Matrix_error _) -> ()
  | Error e -> Alcotest.failf "expected HTTP error, got %s" (Error.to_string e)
  | Ok _ -> Alcotest.fail "HTTP failure unexpectedly succeeded");
  ignore (ok (Server.get_versions client));
  check_int "HTTP failure is retried" 2 (List.length (requests log))

let test_server_capabilities_cache () =
  let log, fetch =
    mock_seq
      [
        {|{"versions":["v1.11"]}|};
        {|{"capabilities":{"m.set_avatar_url":{"enabled":true}}}|};
        {|{"versions":["v1.12"]}|};
        {|{"capabilities":{"m.set_avatar_url":{"enabled":false}}}|};
      ]
  in
  let client = logged_in fetch in
  ignore (ok (Server.get_versions client));
  ignore (ok (Server.get_capabilities client));
  ignore (ok (Server.get_versions client));
  ignore (ok (Server.get_capabilities client));
  check_int "both metadata values are cached" 2 (List.length (requests log));
  Server.invalidate_cache client;
  ignore (ok (Server.get_versions client));
  ignore (ok (Server.get_capabilities client));
  check_int "invalidation clears both values" 4 (List.length (requests log))

let test_server_capabilities_refresh () =
  let log, fetch =
    mock_seq
      [
        {|{"capabilities":{"m.change_password":{"enabled":true}}}|};
        {|{"capabilities":{"m.change_password":{"enabled":false}}}|};
      ]
  in
  let client = logged_in fetch in
  check_bool "initial cached capability" true
    (ok (Server.can_change_password client));
  check_bool "forced refresh returns new capability" false
    (Option.value ~default:true
       (ok (Server.refresh_capabilities client)).change_password);
  check_bool "refreshed capability replaces cache" false
    (ok (Server.can_change_password client));
  check_int "refresh performs exactly one additional request" 2
    (List.length (requests log))

let test_profile_capability_helpers () =
  let check_present body ~displayname ~avatar_url ~label =
    let log, fetch = mock (json body) in
    let client = logged_in fetch in
    check_bool (label ^ " displayname") displayname
      (ok (Server.can_change_displayname client));
    check_bool (label ^ " avatar_url") avatar_url
      (ok (Server.can_change_avatar client));
    check_bool
      (label ^ " displayname cached")
      displayname
      (ok (Server.can_change_displayname client));
    check_int
      (label ^ " only capabilities request")
      1
      (List.length (requests log))
  in
  (* A disabled capability denies every profile field. *)
  check_present
    {|{"capabilities":{
         "m.profile_fields":{"enabled":false},
         "m.set_displayname":{"enabled":true},
         "m.set_avatar_url":{"enabled":true}}}|}
    ~displayname:false ~avatar_url:false ~label:"disabled";
  (* An allowlist takes precedence over a disallowlist. Unknown field names
     are harmless and are ignored by these two helpers. *)
  check_present
    {|{"capabilities":{
         "m.profile_fields":{"enabled":true,
                              "allowed":["displayname","m.future"],
                              "disallowed":["displayname","avatar_url"]}}}|}
    ~displayname:true ~avatar_url:false ~label:"allowed precedence";
  (* With no allowlist, a disallowlist only denies its named fields. *)
  check_present
    {|{"capabilities":{
         "m.profile_fields":{"enabled":true,
                              "disallowed":["displayname","m.future"]}}}|}
    ~displayname:false ~avatar_url:true ~label:"disallowed only";
  (* Once Matrix 1.16 is advertised, an absent profile capability means both
     fields are unrestricted and the deprecated flags are ignored. *)
  let log, fetch =
    mock_seq
      [
        {|{"capabilities":{
             "m.set_displayname":{"enabled":false},
             "m.set_avatar_url":{"enabled":false}}}|};
        {|{"versions":["v1.16"]}|};
      ]
  in
  let client = logged_in fetch in
  for _ = 1 to 2 do
    check_bool "1.16 displayname unrestricted" true
      (ok (Server.can_change_displayname client));
    check_bool "1.16 avatar unrestricted" true
      (ok (Server.can_change_avatar client))
  done;
  check_int "1.16 caches capabilities and versions" 2
    (List.length (requests log));
  (* Before Matrix 1.16, the deprecated flags decide each field. *)
  let log, fetch =
    mock_seq
      [
        {|{"capabilities":{
             "m.set_displayname":{"enabled":true},
             "m.set_avatar_url":{"enabled":false}}}|};
        {|{"versions":["v1.15"]}|};
      ]
  in
  let client = logged_in fetch in
  check_bool "pre-1.16 legacy displayname" true
    (ok (Server.can_change_displayname client));
  check_bool "pre-1.16 legacy avatar" false
    (ok (Server.can_change_avatar client));
  check_int "pre-1.16 caches capabilities and versions" 2
    (List.length (requests log));
  (* An absent legacy capability has the historical default of true. *)
  let log, fetch =
    mock_seq [ {|{"capabilities":{}}|}; {|{"versions":["v1.15"]}|} ]
  in
  let client = logged_in fetch in
  check_bool "pre-1.16 absent displayname defaults true" true
    (ok (Server.can_change_displayname client));
  check_bool "pre-1.16 absent avatar defaults true" true
    (ok (Server.can_change_avatar client));
  check_int "pre-1.16 absent caches capabilities and versions" 2
    (List.length (requests log));
  (* A present but malformed profile capability must not silently fall back to
     the deprecated capability or trigger a versions request. *)
  let log, fetch =
    mock
      (json
         {|{"capabilities":{
              "m.profile_fields":{"enabled":true,"allowed":"displayname"},
              "m.set_displayname":{"enabled":true}}}|})
  in
  let client = logged_in fetch in
  (match Server.can_change_displayname client with
  | Error (Error.Json_error _) -> ()
  | Error error ->
      Alcotest.failf "malformed profile capability returned %s"
        (Error.to_string error)
  | Ok value -> Alcotest.failf "malformed profile capability returned %b" value);
  check_int "malformed profile capability only requests capabilities" 1
    (List.length (requests log))

let test_extended_profile_fields () =
  let log, fetch =
    mock
      (json
         {|{"capabilities":{
              "m.profile_fields":{"enabled":true,
                                   "allowed":["displayname"],
                                   "disallowed":["avatar_url"]}}}|})
  in
  let fields = ok (Server.extended_profile_fields (logged_in fetch)) in
  check_bool "advertised extended fields enabled" true fields.enabled;
  Alcotest.(check (option (list string)))
    "advertised allowlist" (Some [ "displayname" ]) fields.allowed;
  Alcotest.(check (option (list string)))
    "advertised disallowlist" (Some [ "avatar_url" ]) fields.disallowed;
  check_int "advertised policy only requests capabilities" 1
    (List.length (requests log));

  let log, fetch =
    mock_seq [ {|{"capabilities":{}}|}; {|{"versions":["v1.16"]}|} ]
  in
  let fields = ok (Server.extended_profile_fields (logged_in fetch)) in
  check_bool "1.16 absent policy is unrestricted" true fields.enabled;
  Alcotest.(check (option (list string)))
    "1.16 absent allowlist" None fields.allowed;
  check_int "1.16 fallback requests capabilities and versions" 2
    (List.length (requests log));

  let log, fetch =
    mock_seq [ {|{"capabilities":{}}|}; {|{"versions":["v1.15"]}|} ]
  in
  let fields = ok (Server.extended_profile_fields (logged_in fetch)) in
  check_bool "pre-1.16 absent policy is disabled" false fields.enabled;
  check_int "pre-1.16 fallback requests capabilities and versions" 2
    (List.length (requests log))

let test_server_metadata_cache_is_credential_scoped () =
  let log, fetch =
    mock_seq [ {|{"versions":["v1.11"]}|}; {|{"versions":["v1.12"]}|} ]
  in
  let anonymous = client_of fetch in
  ignore (ok (Server.get_versions anonymous));
  let authenticated = Client.with_access_token anonymous "syt_token" in
  let v = ok (Server.get_versions authenticated) in
  check_int "credential-derived client refetches" 2 (List.length (requests log));
  Alcotest.(check (list string)) "authenticated versions" [ "v1.12" ] v.versions

let test_well_known () =
  let log, fetch =
    mock
      (json
         {|{"m.homeserver":{"base_url":"https://hs.example"},
            "m.identity_server":{"base_url":"https://is.example"},
            "org.matrix.msc2965.authentication":
              {"issuer":"https://auth.example/",
               "account":"https://auth.example/account"}}|})
  in
  let wk = ok (Server.get_well_known (client_of fetch)) in
  check_request log ~meth:"GET"
    ~url:"https://hs.example/.well-known/matrix/client" ();
  match wk with
  | None -> Alcotest.fail "expected a well-known"
  | Some wk ->
      check_string "homeserver" "https://hs.example"
        (Uriz.to_string wk.homeserver.base_url);
      check_str_opt "identity server" (Some "https://is.example")
        (Option.map
           (fun (i : Server.server_info) -> Uriz.to_string i.base_url)
           wk.identity_server);
      (* The MSC2965 unstable name is read when the stable one is absent. *)
      check_str_opt "issuer" (Some "https://auth.example/")
        (Option.map
           (fun (a : Server.authentication_info) -> a.issuer)
           wk.authentication)

let test_well_known_absent () =
  let _, fetch =
    mock (fun req ->
        Fetch_mock.respond ~status:404 {|{"errcode":"M_NOT_FOUND"}|} req)
  in
  (* A 404 is the spec's "this origin has no well-known", not an error. *)
  Alcotest.(check bool)
    "no well-known" true
    (ok (Server.get_well_known (client_of fetch)) = None)

let test_well_known_url_validation () =
  let rejects =
    [
      "https://user:password@hs.example";
      "https://hs.example?tenant=one";
      "https://hs.example#fragment";
      "//hs.example";
      "ftp://hs.example";
      "https://hs.example:";
      "https://hs.example:not-a-port";
    ]
  in
  List.iter
    (fun base_url ->
      let _, fetch =
        mock
          (json (Printf.sprintf {|{"m.homeserver":{"base_url":%S}}|} base_url))
      in
      match Server.get_well_known (client_of fetch) with
      | Error (Error.Json_error _) -> ()
      | Error error ->
          Alcotest.failf "unexpected error for %S: %s" base_url
            (Error.to_string error)
      | Ok _ -> Alcotest.failf "invalid well-known URL accepted: %S" base_url)
    rejects;
  let _, fetch =
    mock (json {|{"m.homeserver":{"base_url":"https://bücher.example:443/"}}|})
  in
  match ok (Server.get_well_known (client_of fetch)) with
  | None -> Alcotest.fail "expected a validated well-known"
  | Some well_known ->
      check_string "canonical well-known URL" "https://xn--bcher-kva.example"
        (Uriz.to_string well_known.homeserver.base_url);
      let _, fetch =
        mock
          (json
             {|{"m.homeserver":{"base_url":"https://hs.example/prefix/"},"m.identity_server":{"base_url":"https://id.example/identity/v2"}}|})
      in
      let prefixed =
        Option.get (ok (Server.get_well_known (client_of fetch)))
      in
      check_string "homeserver base path retained" "https://hs.example/prefix/"
        (Uriz.to_string prefixed.homeserver.base_url);
      check_string "identity-server base path retained"
        "https://id.example/identity/v2"
        (Uriz.to_string (Option.get prefixed.identity_server).base_url)

let test_well_known_authentication_url_validation () =
  let bodies =
    [
      {|{"m.homeserver":{"base_url":"https://hs.example"},"m.authentication":{"issuer":"https://auth.example/?tenant=one"}}|};
      {|{"m.homeserver":{"base_url":"https://hs.example"},"m.authentication":{"issuer":"https://user@auth.example/"}}|};
      {|{"m.homeserver":{"base_url":"https://hs.example"},"m.authentication":{"issuer":"https://auth.example/","account":"relative"}}|};
    ]
  in
  List.iter
    (fun body ->
      let _, fetch = mock (json body) in
      match Server.get_well_known (client_of fetch) with
      | Error (Error.Json_error _) -> ()
      | Error error ->
          Alcotest.failf "unexpected authentication URL error: %s"
            (Error.to_string error)
      | Ok _ -> Alcotest.fail "invalid authentication URL was accepted")
    bodies

let test_discover () =
  let log, fetch =
    mock_seq
      [
        {|{"m.homeserver":{"base_url":"https://hs.example"}}|};
        {|{"versions":["v1.11"]}|};
      ]
  in
  let d = ok (Server.discover (client_of fetch)) in
  (match requests log with
  | [ wk; versions ] ->
      check_string "well-known url"
        "https://hs.example/.well-known/matrix/client" wk.url;
      check_string "versions url" "https://hs.example/_matrix/client/versions"
        versions.url
  | rs -> Alcotest.failf "expected 2 requests, got %d" (List.length rs));
  check_string "base url" "https://hs.example" (Uriz.to_string d.base_url);
  check_bool "well-known present" true (d.well_known <> None);
  (match d.server_versions with
  | Some v -> Alcotest.(check (list string)) "versions" [ "v1.11" ] v.versions
  | None -> Alcotest.fail "expected versions from the same origin");
  let prefixed_log, prefixed_fetch =
    mock_seq
      [
        {|{"m.homeserver":{"base_url":"https://hs.example/prefix/"}}|};
        {|{"versions":["v1.12"]}|};
      ]
  in
  let prefixed = ok (Server.discover (client_of prefixed_fetch)) in
  (match requests prefixed_log with
  | [ _well_known; versions ] ->
      check_string "prefixed versions URL"
        "https://hs.example/prefix/_matrix/client/versions" versions.url
  | requests ->
      Alcotest.failf "expected two prefixed discovery requests, got %d"
        (List.length requests));
  check_string "prefixed discovered base" "https://hs.example/prefix/"
    (Uriz.to_string prefixed.base_url);
  match prefixed.server_versions with
  | Some v ->
      Alcotest.(check (list string)) "prefixed versions" [ "v1.12" ] v.versions
  | None -> Alcotest.fail "expected versions beneath the discovered base path"

let test_discover_off_origin () =
  (* When the well-known points at another origin the client cannot follow
     it: [Client.create] restricted it to its own. *)
  let log, fetch =
    mock_seq [ {|{"m.homeserver":{"base_url":"https://other.example"}}|} ]
  in
  let d = ok (Server.discover (client_of fetch)) in
  check_string "one request only" "https://hs.example/.well-known/matrix/client"
    (one_request log).url;
  check_string "base url" "https://other.example" (Uriz.to_string d.base_url);
  check_bool "no versions" true (d.server_versions = None)

let test_discover_off_origin_callback () =
  let log, fetch =
    mock (fun req ->
        let url = Fetch.Middleware.Url.to_string req.url in
        if String.ends_with ~suffix:"/_matrix/client/versions" url then
          json {|{"versions":["v1.12"]}|} req
        else json {|{"m.homeserver":{"base_url":"https://other.example"}}|} req)
  in
  let client = logged_in fetch in
  let get_versions_at uri =
    let delegated = client_of ~homeserver:(Uriz.to_string uri) fetch in
    Server.get_versions delegated
  in
  let d = ok (Server.discover ~get_versions_at client) in
  (match requests log with
  | [ well_known; versions ] ->
      check_string "well-known origin"
        "https://hs.example/.well-known/matrix/client" well_known.url;
      check_string "delegated versions origin"
        "https://other.example/_matrix/client/versions" versions.url;
      check_str_opt "well-known authorization" None
        (Http.Header.get well_known.headers "authorization");
      check_str_opt "delegated versions authorization" None
        (Http.Header.get versions.headers "authorization")
  | rs ->
      Alcotest.failf "expected two discovery requests, got %d" (List.length rs));
  check_string "delegated base url" "https://other.example"
    (Uriz.to_string d.base_url);
  match d.server_versions with
  | Some v ->
      Alcotest.(check (list string)) "delegated versions" [ "v1.12" ] v.versions
  | None -> Alcotest.fail "expected delegated versions"

let test_discover_off_origin_callback_error () =
  let _, fetch =
    mock (json {|{"m.homeserver":{"base_url":"https://other.example"}}|})
  in
  let expected = Error.Policy_denied "delegated by test" in
  match
    Server.discover ~get_versions_at:(fun _ -> Error expected) (client_of fetch)
  with
  | Error error ->
      Alcotest.(check bool)
        "callback error propagates" true
        (Error.equal expected error)
  | Ok _ -> Alcotest.fail "callback error unexpectedly succeeded"

let test_discover_off_origin_callback_transport_error () =
  let _, fetch =
    mock (json {|{"m.homeserver":{"base_url":"https://other.example"}}|})
  in
  let expected = Error.Network_error "delegated transport" in
  match
    Server.discover ~get_versions_at:(fun _ -> Error expected) (client_of fetch)
  with
  | Error error ->
      Alcotest.(check bool)
        "transport error propagates" true
        (Error.equal expected error)
  | Ok _ -> Alcotest.fail "transport error unexpectedly succeeded"

(* {1 Search} *)

let event_json =
  {|{"event_id":"$e1","sender":"@bob:example.org","origin_server_ts":1234,
     "type":"m.room.message","content":{"body":"hello","msgtype":"m.text"}}|}

let test_search_room_events () =
  let log, fetch =
    mock
      (json
         (Printf.sprintf
            {|{"search_categories":{"room_events":{
                 "count":2,
                 "highlights":["hello"],
                 "next_batch":"page2",
                 "results":[{"rank":0.5,"result":%s,
                             "context":{"start":"s","end":"e",
                                        "profile_info":{"@bob:example.org":
                                          {"displayname":"Bob"}},
                                        "events_before":[],
                                        "events_after":[%s]}}],
                 "state":{"!r:example.org":[%s]},
                 "groups":{"room_id":{"!r:example.org":
                    {"next_batch":"g2","order":1,"results":["$e1"]}}}}}}|}
            event_json event_json event_json))
  in
  let criteria =
    Search.v ~keys:[ `Content_body ] ~order_by:`Recent
      ~event_context:
        {
          before_limit = Some 2;
          after_limit = None;
          include_profile = Some true;
        }
      ~include_state:true ~group_by:[ `Room_id ] "hello"
  in
  let r =
    ok (Search.room_events (logged_in fetch) ~criteria ~next_batch:"p1" ())
  in
  check_request log ~meth:"POST"
    ~url:"https://hs.example/_matrix/client/v3/search?next_batch=p1"
    ~body:
      ({|{"search_categories":{"room_events":{"search_term":"hello",|}
     ^ {|"keys":["content.body"],"order_by":"recent",|}
     ^ {|"event_context":{"before_limit":2,"include_profile":true},|}
     ^ {|"include_state":true,|}
     ^ {|"groupings":{"group_by":[{"key":"room_id"}]}}}}|})
    ();
  Alcotest.(check (option int)) "count" (Some 2) r.count;
  Alcotest.(check (list string)) "highlights" [ "hello" ] r.highlights;
  check_str_opt "next_batch" (Some "page2") r.next_batch;
  check_int "one hit" 1 (List.length r.results);
  let hit = List.hd r.results in
  Alcotest.(check (option (float 0.001))) "rank" (Some 0.5) hit.rank;
  check_bool "result decoded" true (hit.result <> None);
  (match hit.context with
  | None -> Alcotest.fail "expected a context"
  | Some ctx ->
      check_str_opt "context start" (Some "s") ctx.start;
      check_str_opt "context end" (Some "e") ctx.end_;
      check_int "one events_after" 1 (List.length ctx.events_after);
      check_int "no events_before" 0 (List.length ctx.events_before);
      check_str_opt "profile displayname" (Some "Bob")
        (match List.assoc_opt (uid "@bob:example.org") ctx.profile_info with
        | Some p -> p.displayname
        | None -> None));
  Alcotest.(check (list string))
    "state rooms" [ "!r:example.org" ]
    (List.map (fun (r, _) -> Id.Room_id.to_string r) r.state);
  match r.groups with
  | [ (`Room_id, [ ("!r:example.org", g) ]) ] ->
      check_str_opt "group next_batch" (Some "g2") g.group_next_batch;
      Alcotest.(check (option int)) "group order" (Some 1) g.order;
      Alcotest.(check (list string))
        "group results" [ "$e1" ]
        (List.map Id.Event_id.to_string g.results)
  | _ -> Alcotest.fail "unexpected groups shape"

let test_search_user_directory () =
  let log, fetch =
    mock
      (json
         {|{"results":[{"user_id":"@bob:example.org","display_name":"Bob",
                        "avatar_url":"mxc://example.org/abc"}],
            "limited":true}|})
  in
  let r =
    ok (Search.user_directory (logged_in fetch) ~search_term:"bob" ~limit:5 ())
  in
  check_request log ~meth:"POST"
    ~url:"https://hs.example/_matrix/client/v3/user_directory/search"
    ~body:{|{"search_term":"bob","limit":5}|} ();
  check_bool "limited" true r.limited;
  match r.users with
  | [ u ] ->
      check_string "user_id" "@bob:example.org" (Id.User_id.to_string u.user_id);
      check_str_opt "display_name" (Some "Bob") u.display_name;
      check_str_opt "avatar" (Some "mxc://example.org/abc") u.avatar_url
  | _ -> Alcotest.fail "expected one result"

(* {1 Notifications} *)

let test_notifications () =
  let log, fetch =
    mock
      (json
         (Printf.sprintf
            {|{"next_token":"tok",
               "notifications":[{"actions":["notify",
                                   {"set_tweak":"sound","value":"default"}],
                                 "event":%s,
                                 "profile_tag":"work",
                                 "read":false,
                                 "room_id":"!r:example.org",
                                 "ts":1699999999000}]}|}
            event_json))
  in
  let r =
    ok
      (Notifications.get (logged_in fetch) ~from:"prev" ~limit:20
         ~only:`Highlight ())
  in
  check_request log ~meth:"GET"
    ~url:
      "https://hs.example/_matrix/client/v3/notifications?from=prev&limit=20&only=highlight"
    ();
  check_str_opt "next_token" (Some "tok") r.next_token;
  match r.chunk with
  | [ n ] ->
      check_bool "both action forms decoded" true
        (List.equal Matrix_proto.Push.Action.equal n.actions
           [
             Matrix_proto.Push.Action.Notify;
             Matrix_proto.Push.Action.Set_tweak
               (Matrix_proto.Push.Tweak.Sound "default");
           ]);
      check_str_opt "profile_tag" (Some "work") n.profile_tag;
      check_bool "unread" false n.read;
      check_string "room_id" "!r:example.org" (Id.Room_id.to_string n.room_id);
      Alcotest.(check int64)
        "ts" 1699999999000L
        (Matrix_proto.Event.Timestamp.to_ms n.ts)
  | _ -> Alcotest.fail "expected one notification"

(* {1 Push rules} *)

let test_get_push_rules () =
  let log, fetch =
    mock
      (json
         {|{"global":{
              "override":[{"rule_id":".m.rule.master","default":true,
                           "enabled":false,"conditions":[],"actions":[]}],
              "content":[{"rule_id":".m.rule.contains_user_name",
                          "default":true,"enabled":true,"pattern":"alice",
                          "actions":["notify",
                                     {"set_tweak":"sound","value":"default"},
                                     {"set_tweak":"highlight"}]}],
              "underride":[{"rule_id":".m.rule.message","default":true,
                            "enabled":true,
                            "conditions":[{"kind":"event_match","key":"type",
                                           "pattern":"m.room.message"}],
                            "actions":["notify"]}]}}|})
  in
  let r = ok (Push.get_push_rules (logged_in fetch)) in
  check_request log ~meth:"GET"
    ~url:"https://hs.example/_matrix/client/v3/pushrules/" ();
  check_int "one override rule" 1 (List.length r.override);
  check_int "one underride rule" 1 (List.length r.underride);
  match r.content with
  | [ rule ] ->
      check_string "rule id" ".m.rule.contains_user_name"
        (Push_rules.Rule_id.id rule.rule_id);
      (* Both wire forms of an action decode, the object form included. *)
      check_bool "actions" true
        (List.equal Push_rules.Action.equal rule.actions
           [
             Push_rules.Action.Notify;
             Push_rules.Action.Set_tweak (Push_rules.Tweak.Sound "default");
             Push_rules.Action.Set_tweak (Push_rules.Tweak.Highlight true);
           ])
  | _ -> Alcotest.fail "expected one content rule"

let test_set_push_rule_enabled () =
  let log, fetch = mock (json "{}") in
  ok
    (Push.set_enabled (logged_in fetch)
       (Push_rules.Rule_id.content "foo")
       ~enabled:false);
  check_request log ~meth:"PUT"
    ~url:
      "https://hs.example/_matrix/client/v3/pushrules/global/content/foo/enabled"
    ~body:{|{"enabled":false}|} ()

(* {1 Threads} *)

let test_threads () =
  let log, fetch =
    mock_seq
      [
        {|{"versions":["v1.4"]}|};
        Printf.sprintf {|{"chunk":[%s],"next_batch":"t2"}|} event_json;
      ]
  in
  let r =
    ok
      (Relations.list_threads (logged_in fetch) ~room_id:(rid "!r:example.org")
         ~filter:Relations.Participated ~from:"t1" ~limit:10 ())
  in
  (* [v1], not [v3]. *)
  check_discovery_then_request log ~meth:"GET"
    ~url:
      "https://hs.example/_matrix/client/v1/rooms/!r:example.org/threads?include=participated&from=t1&limit=10"
    ();
  check_int "one thread root" 1 (List.length r.Matrix_proto.Common.Page.chunk);
  check_str_opt "next_batch" (Some "t2") r.Matrix_proto.Common.Page.next_batch

let test_threads_unstable () =
  let log, fetch =
    mock_seq
      [
        {|{"versions":["v1.3"],"unstable_features":{"org.matrix.msc3856":true}}|};
        Printf.sprintf {|{"chunk":[%s]}|} event_json;
      ]
  in
  ignore
    (ok
       (Relations.list_threads (logged_in fetch) ~room_id:(rid "!r:example.org")
          ~filter:Relations.All ~limit:2 ()));
  check_discovery_then_request log ~meth:"GET"
    ~url:
      "https://hs.example/_matrix/client/unstable/org.matrix.msc3856/rooms/!r:example.org/threads?include=all&limit=2"
    ()

(* {1 Reporting} *)

let test_report_event () =
  let log, fetch = mock (json "{}") in
  ok
    (Report.event (logged_in fetch) ~room_id:(rid "!r:example.org")
       ~event_id:(eid "$e1") ~reason:"spam" ~score:(-100) ());
  check_request log ~meth:"POST"
    ~url:"https://hs.example/_matrix/client/v3/rooms/!r:example.org/report/$e1"
    ~body:{|{"reason":"spam"}|} ()

let test_report_room () =
  let log, fetch = mock_seq [ {|{"versions":["v1.13"]}|}; "{}" ] in
  ok
    (Report.room (logged_in fetch) ~room_id:(rid "!r:example.org")
       ~reason:"abuse" ());
  check_discovery_then_request log ~meth:"POST"
    ~url:"https://hs.example/_matrix/client/v3/rooms/!r:example.org/report"
    ~body:{|{"reason":"abuse"}|} ()

let test_report_user () =
  let log, fetch = mock_seq [ {|{"versions":["v1.14"]}|}; "{}" ] in
  ok (Report.user (logged_in fetch) ~user_id:(uid "@bob:example.org") ());
  check_discovery_then_request log ~meth:"POST"
    ~url:"https://hs.example/_matrix/client/v3/users/@bob:example.org/report"
    ~body:"{}" ()

let test_report_room_unstable () =
  let log, fetch = mock_seq [ {|{"versions":["v1.12"]}|}; "{}" ] in
  ok
    (Report.room (logged_in fetch) ~room_id:(rid "!r:example.org")
       ~reason:"abuse" ());
  check_discovery_then_request log ~meth:"POST"
    ~url:
      "https://hs.example/_matrix/client/unstable/org.matrix.msc4151/rooms/!r:example.org/report"
    ~body:{|{"reason":"abuse"}|} ()

let test_report_user_unstable () =
  let log, fetch = mock_seq [ {|{"versions":["v1.13"]}|}; "{}" ] in
  ok
    (Report.user (logged_in fetch) ~user_id:(uid "@bob:example.org")
       ~reason:"abuse" ());
  check_discovery_then_request log ~meth:"POST"
    ~url:
      "https://hs.example/_matrix/client/unstable/org.matrix.msc4260/users/@bob:example.org/report"
    ~body:{|{"reason":"abuse"}|} ()

(* {1 Tags} *)

let tags_url =
  "https://hs.example/_matrix/client/v3/user/@alice:example.org/rooms/!r:example.org/tags"

let test_tags_get () =
  let log, fetch =
    mock (json {|{"tags":{"m.favourite":{"order":0.25},"work":{}}}|})
  in
  let tags =
    ok
      (Tags.get (logged_in fetch) ~user_id:(uid "@alice:example.org")
         ~room_id:(rid "!r:example.org"))
  in
  check_request log ~meth:"GET" ~url:tags_url ();
  Alcotest.(check (list string))
    "tag names" [ "m.favourite"; "work" ] (List.map fst tags);
  Alcotest.(check (option (float 0.001)))
    "favourite order" (Some 0.25)
    (List.assoc "m.favourite" tags);
  Alcotest.(check (option (float 0.001)))
    "work has no order" None (List.assoc "work" tags)

(* A historical user id (uppercase localpart) and a member that is not a user
   id at all must not stop the lookup for a well-formed partner. *)
let test_find_dm_rooms_tolerates_bad_keys () =
  let _log, fetch =
    mock
      (json
         {|{"@Samoht:recoil.org":["!dm1:recoil.org"],
            "not a user id":["!junk:example.org"],
            "@bob:example.org":["!dm2:example.org","!dm3:example.org"]}|})
  in
  let client = logged_in fetch in
  let rooms user =
    ok (Matrix_client.Account_data.find_dm_rooms client ~user_id:(uid user))
  in
  Alcotest.(check (list string))
    "historical partner" [ "!dm1:recoil.org" ]
    (List.map Id.Room_id.to_string (rooms "@Samoht:recoil.org"));
  Alcotest.(check (list string))
    "conformant partner"
    [ "!dm2:example.org"; "!dm3:example.org" ]
    (List.map Id.Room_id.to_string (rooms "@bob:example.org"));
  Alcotest.(check (list string))
    "unknown partner" []
    (List.map Id.Room_id.to_string (rooms "@nobody:x.org"))

let test_tags_set () =
  let log, fetch = mock (json "{}") in
  ok
    (Tags.set (logged_in fetch) ~user_id:(uid "@alice:example.org")
       ~room_id:(rid "!r:example.org") ~tag:Tags.low_priority ~order:0.5 ());
  check_request log ~meth:"PUT"
    ~url:(tags_url ^ "/m.lowpriority")
    ~body:{|{"order":0.5}|} ()

let test_tags_remove () =
  let log, fetch = mock (json "{}") in
  ok
    (Tags.remove (logged_in fetch) ~user_id:(uid "@alice:example.org")
       ~room_id:(rid "!r:example.org") ~tag:Tags.favourite);
  check_request log ~meth:"DELETE" ~url:(tags_url ^ "/m.favourite") ()

let test_tags_favourite_helper () =
  (* The helper acts as the logged-in user, so it needs no user ID. *)
  let log, fetch = mock (json "{}") in
  ok
    (Tags.set_favourite (logged_in fetch) ~room_id:(rid "!r:example.org")
       ~favourite:false ());
  check_request log ~meth:"DELETE" ~url:(tags_url ^ "/m.favourite") ()

let test_tags_helper_needs_session () =
  let _, fetch = mock (json "{}") in
  match
    Tags.set_favourite (client_of fetch) ~room_id:(rid "!r:example.org")
      ~favourite:true ()
  with
  | Ok () -> Alcotest.fail "expected a not-logged-in error"
  | Error Error.No_session -> ()
  | Error e -> Alcotest.failf "unexpected error: %s" (Error.to_string e)

(* {1 OpenID} *)

let test_openid_request_token () =
  let log, fetch =
    mock
      (json
         {|{"access_token":"opaque","token_type":"Bearer",
            "matrix_server_name":"example.org","expires_in":3600}|})
  in
  let t = ok (Openid.request_own_token (logged_in fetch)) in
  check_request log ~meth:"POST"
    ~url:
      "https://hs.example/_matrix/client/v3/user/@alice:example.org/openid/request_token"
    ~body:"{}" ();
  check_string "access_token" "opaque" t.access_token;
  check_string "server name" "example.org" t.matrix_server_name;
  check_int "expires_in" 3600 t.expires_in

(* {1 Third-party lookup} *)

let protocol_json =
  {|{"user_fields":["network","nickname"],
     "location_fields":["network","channel"],
     "icon":"mxc://example.org/irc",
     "field_types":{"network":{"regexp":"([a-z0-9]+)",
                               "placeholder":"irc.example.org"}},
     "instances":[{"network_id":"freenode","desc":"Freenode",
                   "icon":"mxc://example.org/fn",
                   "fields":{"network":"freenode"},
                   "instance_id":"i1"}]}|}

let test_thirdparty_protocols () =
  let log, fetch = mock (json (Printf.sprintf {|{"irc":%s}|} protocol_json)) in
  let ps = ok (Thirdparty.protocols (logged_in fetch)) in
  check_request log ~meth:"GET"
    ~url:"https://hs.example/_matrix/client/v3/thirdparty/protocols" ();
  Alcotest.(check (list string)) "protocol names" [ "irc" ] (List.map fst ps);
  let p = List.assoc "irc" ps in
  Alcotest.(check (list string))
    "user_fields" [ "network"; "nickname" ] p.user_fields;
  check_string "icon" "mxc://example.org/irc" p.icon;
  check_string "field regexp" "([a-z0-9]+)"
    (List.assoc "network" p.field_types).regexp;
  match p.instances with
  | [ i ] ->
      check_string "network_id" "freenode" i.network_id;
      check_str_opt "instance_id" (Some "i1") i.instance_id;
      check_str_opt "instance field" (Some "freenode")
        (List.assoc_opt "network" i.fields)
  | _ -> Alcotest.fail "expected one instance"

let test_thirdparty_protocol () =
  let log, fetch = mock (json protocol_json) in
  let p = ok (Thirdparty.get_protocol (logged_in fetch) ~name:"irc") in
  check_request log ~meth:"GET"
    ~url:"https://hs.example/_matrix/client/v3/thirdparty/protocol/irc" ();
  Alcotest.(check (list string))
    "location_fields" [ "network"; "channel" ] p.location_fields

let test_thirdparty_location_by_alias () =
  let log, fetch =
    mock
      (json
         {|[{"alias":"#irc_matrix:example.org","protocol":"irc",
             "fields":{"channel":"#matrix"}}]|})
  in
  let ls =
    ok
      (Thirdparty.locations_of_alias (logged_in fetch)
         ~alias:(alias "#irc_matrix:example.org"))
  in
  check_request log ~meth:"GET"
    ~url:
      "https://hs.example/_matrix/client/v3/thirdparty/location?alias=%23irc_matrix:example.org"
    ();
  match ls with
  | [ l ] ->
      check_string "alias" "#irc_matrix:example.org"
        (Id.Room_alias.to_string l.alias);
      check_string "protocol" "irc" l.protocol
  | _ -> Alcotest.fail "expected one location"

let test_thirdparty_location_by_protocol () =
  let log, fetch = mock (json "[]") in
  let ls =
    ok
      (Thirdparty.locations (logged_in fetch) ~protocol:"irc"
         ~fields:[ ("network", "freenode"); ("channel", "#matrix") ]
         ())
  in
  (* The protocol's own field names become query parameters. *)
  check_request log ~meth:"GET"
    ~url:
      "https://hs.example/_matrix/client/v3/thirdparty/location/irc?network=freenode&channel=%23matrix"
    ();
  check_int "no locations" 0 (List.length ls)

let test_thirdparty_user_by_user_id () =
  let log, fetch =
    mock
      (json
         {|[{"userid":"@irc_bob:example.org","protocol":"irc",
             "fields":{"nickname":"bob"}}]|})
  in
  let us =
    ok
      (Thirdparty.users_of_user_id (logged_in fetch)
         ~user_id:(uid "@irc_bob:example.org"))
  in
  check_request log ~meth:"GET"
    ~url:
      "https://hs.example/_matrix/client/v3/thirdparty/user?userid=@irc_bob:example.org"
    ();
  match us with
  | [ u ] ->
      check_string "userid" "@irc_bob:example.org"
        (Id.User_id.to_string u.userid);
      check_string "protocol" "irc" u.protocol
  | _ -> Alcotest.fail "expected one user"

let test_thirdparty_user_by_protocol () =
  let log, fetch = mock (json "[]") in
  ignore
    (ok
       (Thirdparty.users (logged_in fetch) ~protocol:"irc"
          ~fields:[ ("nickname", "bob") ]
          ()));
  check_request log ~meth:"GET"
    ~url:"https://hs.example/_matrix/client/v3/thirdparty/user/irc?nickname=bob"
    ()

(* {1 Delayed events (MSC4140)} *)

let hello_content =
  match
    Jsont_bytesrw.decode_string Matrix_proto.Json.Codec.json
      {|{"body":"hi","msgtype":"m.text"}|}
  with
  | Ok j -> j
  | Error e -> failwith e

let test_delayed_send () =
  let log, fetch = mock (json {|{"delay_id":"d1"}|}) in
  let id =
    ok
      (Delayed_events.send (logged_in fetch) ~room_id:(rid "!r:example.org")
         ~event_type:"m.room.message" ~content:hello_content ~delay_ms:2000
         ~txn_id:"txn1" ())
  in
  check_request log ~meth:"PUT"
    ~url:
      "https://hs.example/_matrix/client/v3/rooms/!r:example.org/send/m.room.message/txn1?org.matrix.msc4140.delay=2000"
    ~body:{|{"body":"hi","msgtype":"m.text"}|} ();
  check_string "delay_id" "d1" (Delayed_events.delay_id_to_string id)

let test_delayed_send_state () =
  let log, fetch = mock (json {|{"delay_id":"d2"}|}) in
  let id =
    ok
      (Delayed_events.send_state (logged_in fetch)
         ~room_id:(rid "!r:example.org") ~event_type:"m.room.topic"
         ~state_key:"" ~content:hello_content ~delay_ms:500)
  in
  check_request log ~meth:"PUT"
    ~url:
      "https://hs.example/_matrix/client/v3/rooms/!r:example.org/state/m.room.topic/?org.matrix.msc4140.delay=500"
    ();
  check_string "delay_id" "d2" (Delayed_events.delay_id_to_string id)

let test_delayed_send_current () =
  let log, fetch = mock (json {|{"delay_id":"d3"}|}) in
  let id =
    ok
      (Delayed_events.send_current (logged_in fetch)
         ~room_id:(rid "!r:example.org") ~event_type:"m.room/message"
         ~content:hello_content ~delay_ms:2000 ~txn_id:"txn/current"
         ~sticky_duration_ms:3000 ~state_key:"" ())
  in
  check_request log ~meth:"PUT"
    ~url:
      "https://hs.example/_matrix/client/unstable/org.matrix.msc4140/rooms/!r:example.org/delayed_event/m.room%2Fmessage/txn%2Fcurrent?org.matrix.msc4354.sticky_duration_ms=3000"
    ~body:
      {|{"delay":2000,"state_key":"","content":{"body":"hi","msgtype":"m.text"}}|}
    ();
  check_string "delay_id" "d3" (Delayed_events.delay_id_to_string id)

let test_delayed_send_current_state_empty_key () =
  let log, fetch = mock (json {|{"delay_id":"d4"}|}) in
  ignore
    (ok
       (Delayed_events.send_state_current (logged_in fetch)
          ~room_id:(rid "!r:example.org") ~event_type:"m.room.topic"
          ~state_key:"" ~content:hello_content ~delay_ms:500 ~txn_id:"txn4" ()));
  check_request log ~meth:"PUT"
    ~url:
      "https://hs.example/_matrix/client/unstable/org.matrix.msc4140/rooms/!r:example.org/delayed_event/m.room.topic/txn4"
    ~body:
      {|{"delay":500,"state_key":"","content":{"body":"hi","msgtype":"m.text"}}|}
    ()

let test_delayed_update () =
  let log, fetch = mock (json "{}") in
  ok
    (Delayed_events.restart (logged_in fetch)
       ~delay_id:(Delayed_events.delay_id_of_string "d1"));
  check_request log ~meth:"POST"
    ~url:
      "https://hs.example/_matrix/client/unstable/org.matrix.msc4140/delayed_events/d1"
    ~body:{|{"action":"restart"}|} ()

let test_delayed_update_current_is_unauthenticated () =
  let log, fetch = mock (json "{}") in
  ok
    (Delayed_events.cancel_current (logged_in fetch)
       ~delay_id:(Delayed_events.delay_id_of_string "d/current"));
  let request = one_request log in
  check_string "method" "POST" request.meth;
  check_string "url"
    "https://hs.example/_matrix/client/unstable/org.matrix.msc4140/delayed_events/d%2Fcurrent/cancel"
    request.url;
  check_str_opt "body" (Some "{}") request.body;
  check_str_opt "no authorization" None
    (Http.Header.get request.headers "authorization")

let test_delayed_update_current_errors () =
  let log, fetch =
    mock (fun req ->
        Fetch_mock.respond ~status:409
          {|{"errcode":"M_INVALID_PARAM","error":"already finalised"}|} req)
  in
  match
    Delayed_events.update_current (logged_in fetch)
      ~delay_id:(Delayed_events.delay_id_of_string "d1")
      ~action:Send
  with
  | Error (Error.Matrix_error { errcode = Error.M_INVALID_PARAM; _ }) ->
      check_int "one request" 1 (List.length (requests log))
  | Error e -> Alcotest.failf "wrong error: %s" (Error.to_string e)
  | Ok () -> Alcotest.fail "expected update error"

let test_delayed_current_required_fields () =
  let _log, fetch =
    mock
      (json
         {|{"delayed_events":[{"delay_id":"d1","room_id":"!r:example.org","type":"m.room.message","content":{},"running_since":1}]}|})
  in
  (match Delayed_events.list_current (logged_in fetch) () with
  | Error (Error.Json_error _) -> ()
  | Error e -> Alcotest.failf "wrong error: %s" (Error.to_string e)
  | Ok _ -> Alcotest.fail "missing required delay was accepted");
  let _log, fetch =
    mock
      (json
         {|{"delayed_events":[{"delay_id":"d1","room_id":"!r:example.org","type":"m.room.message","content":{},"delay":-1,"running_since":1}]}|})
  in
  (match Delayed_events.list_current (logged_in fetch) () with
  | Error (Error.Json_error _) -> ()
  | Error e -> Alcotest.failf "wrong error: %s" (Error.to_string e)
  | Ok _ -> Alcotest.fail "negative current delay was accepted");
  let legacy_log, legacy_fetch =
    mock
      (json
         {|{"delayed_events":[{"delay_id":"d2","room_id":"!r:example.org","type":"m.room.message"}],"next_batch":"next"}|})
  in
  let response = ok (Delayed_events.list (logged_in legacy_fetch) ()) in
  check_request legacy_log ~meth:"GET"
    ~url:
      "https://hs.example/_matrix/client/unstable/org.matrix.msc4140/delayed_events"
    ();
  match response.chunk with
  | [ event ] ->
      check_bool "legacy content defaults to object" true
        (event.content = Jsont.Json.object' []);
      check_int "legacy delay defaults to zero" 0 event.delay;
      Alcotest.(check int64)
        "legacy running_since defaults to zero" 0L
        (Matrix_proto.Event.Timestamp.to_ms event.running_since)
  | _ -> Alcotest.fail "expected one legacy delayed event"

let test_delayed_current_negative_values () =
  let log, fetch = mock (json {|{"delay_id":"unused"}|}) in
  let client = logged_in fetch in
  let raises f =
    match f () with
    | exception Invalid_argument _ -> ()
    | _ -> Alcotest.fail "expected Invalid_argument"
  in
  raises (fun () ->
      Delayed_events.send_current client ~room_id:(rid "!r:example.org")
        ~event_type:"m.room.message" ~content:hello_content ~delay_ms:(-1) ());
  raises (fun () ->
      Delayed_events.send_current client ~room_id:(rid "!r:example.org")
        ~event_type:"m.room.message" ~content:hello_content ~delay_ms:1
        ~sticky_duration_ms:(-1) ());
  raises (fun () ->
      Delayed_events.send_current client ~room_id:(rid "!r:example.org")
        ~event_type:"m.room.message" ~content:hello_content ~delay_ms:1
        ~sticky_duration_ms:3_600_001 ());
  check_int "no request" 0 (List.length (requests log))

let test_delayed_list () =
  let log, fetch =
    mock
      (json
         {|{"delayed_events":[{"delay_id":"d1","room_id":"!r:example.org",
              "type":"m.room.message","content":{"body":"hi"},
              "delay":2000,"running_since":1699999999000,
              "error":{"errcode":"M_FORBIDDEN","error":"no"}}]}|})
  in
  let r = ok (Delayed_events.list (logged_in fetch) ()) in
  check_request log ~meth:"GET"
    ~url:
      "https://hs.example/_matrix/client/unstable/org.matrix.msc4140/delayed_events"
    ();
  match r.chunk with
  | [ d ] ->
      check_string "delay_id" "d1"
        (Delayed_events.delay_id_to_string d.delay_id);
      check_string "type" "m.room.message" d.event_type;
      check_int "delay" 2000 d.delay;
      Alcotest.(check int64)
        "running_since" 1699999999000L
        (Matrix_proto.Event.Timestamp.to_ms d.running_since);
      check_bool "no event_id yet" true (d.event_id = None);
      check_str_opt "error" (Some "no")
        (Option.map (fun (e : Error.matrix_error) -> e.error) d.error)
  | _ -> Alcotest.fail "expected one delayed event"

let test_delayed_current_get_list_and_status () =
  let event =
    {|{"delay_id":"d1","room_id":"!r:example.org","type":"m.room.message","content":{"body":"hi"},"delay":2000,"running_since":1699999999000,"event_id":"$e:example.org","finalised_ts":1700000000000}|}
  in
  let log, fetch = mock (json event) in
  let d =
    ok
      (Delayed_events.get_current (logged_in fetch)
         ~delay_id:(Delayed_events.delay_id_of_string "d1"))
  in
  check_string "get id" "d1" (Delayed_events.delay_id_to_string d.delay_id);
  check_bool "get sent status" true
    (Delayed_events.status d = Delayed_events.Sent);
  check_request log ~meth:"GET"
    ~url:
      "https://hs.example/_matrix/client/unstable/org.matrix.msc4140/delayed_events/d1"
    ();
  let list_log, list_fetch =
    mock (json (Printf.sprintf {|{"delayed_events":[%s]}|} event))
  in
  let events = ok (Delayed_events.list_current (logged_in list_fetch) ()) in
  check_int "current list" 1 (List.length events);
  check_request list_log ~meth:"GET"
    ~url:
      "https://hs.example/_matrix/client/unstable/org.matrix.msc4140/delayed_events"
    ()

(* {1 Rooms: upgrade, aliases, joined members, timestamp lookup} *)

let test_room_upgrade () =
  let log, fetch = mock (json {|{"replacement_room":"!new:example.org"}|}) in
  let room =
    ok
      (Rooms.upgrade (logged_in fetch) ~room_id:(rid "!r:example.org")
         ~new_version:"11" ())
  in
  check_request log ~meth:"POST"
    ~url:"https://hs.example/_matrix/client/v3/rooms/!r:example.org/upgrade"
    ~body:{|{"new_version":"11","additional_creators":[]}|} ();
  check_string "replacement" "!new:example.org" (Id.Room_id.to_string room)

let test_room_aliases () =
  let log, fetch =
    mock (json {|{"aliases":["#lobby:example.org","#main:example.org"]}|})
  in
  let aliases =
    ok (Rooms.get_aliases (logged_in fetch) ~room_id:(rid "!r:example.org"))
  in
  check_request log ~meth:"GET"
    ~url:"https://hs.example/_matrix/client/v3/rooms/!r:example.org/aliases" ();
  Alcotest.(check (list string))
    "aliases"
    [ "#lobby:example.org"; "#main:example.org" ]
    (List.map Id.Room_alias.to_string aliases)

let test_room_joined_members () =
  let log, fetch =
    mock
      (json
         {|{"joined":{"@alice:example.org":{"display_name":"Alice",
                        "avatar_url":"mxc://example.org/a"},
                      "@bob:example.org":{}}}|})
  in
  let members =
    ok
      (Rooms.get_joined_members (logged_in fetch)
         ~room_id:(rid "!r:example.org"))
  in
  check_request log ~meth:"GET"
    ~url:
      "https://hs.example/_matrix/client/v3/rooms/!r:example.org/joined_members"
    ();
  Alcotest.(check (list string))
    "members"
    [ "@alice:example.org"; "@bob:example.org" ]
    (List.map (fun (u, _) -> Id.User_id.to_string u) members);
  check_str_opt "alice display name" (Some "Alice")
    (snd (List.hd members)).display_name

let test_room_joined_members_bad_key () =
  let _, fetch = mock (json {|{"joined":{"not-a-user":{}}}|}) in
  match
    Rooms.get_joined_members (logged_in fetch) ~room_id:(rid "!r:example.org")
  with
  | Error (Matrix_client.Error.Json_error _) -> ()
  | Error e ->
      Alcotest.failf "expected a decode error, got %s"
        (Matrix_client.Error.to_string e)
  | Ok _ -> Alcotest.fail "a malformed user id should not decode"

let test_room_timestamp_to_event () =
  let log, fetch =
    mock (json {|{"event_id":"$e1","origin_server_ts":1699999999000}|})
  in
  let r =
    ok
      (Rooms.timestamp_to_event (logged_in fetch)
         ~room_id:(rid "!r:example.org")
         ~ts:(Matrix_proto.Event.Timestamp.of_ms 1699999999000L)
         ~dir:Matrix_proto.Common.Direction.Backward)
  in
  (* [v1], not [v3]. *)
  check_request log ~meth:"GET"
    ~url:
      "https://hs.example/_matrix/client/v1/rooms/!r:example.org/timestamp_to_event?ts=1699999999000&dir=b"
    ();
  check_string "event_id" "$e1" (Id.Event_id.to_string r.event_id);
  Alcotest.(check int64)
    "ts" 1699999999000L
    (Matrix_proto.Event.Timestamp.to_ms r.origin_server_ts)

(* {1 Auth additions} *)

let test_get_login_token () =
  let log, fetch =
    mock_seq
      [
        {|{"versions":["v1.7"]}|};
        {|{"login_token":"lt_abc","expires_in_ms":120000}|};
      ]
  in
  let t = ok (Auth.get_login_token (logged_in fetch) ()) in
  check_discovery_then_request log ~meth:"POST"
    ~url:"https://hs.example/_matrix/client/v1/login/get_token" ~body:"{}" ();
  check_string "login_token" "lt_abc" t.login_token;
  check_int "expires_in_ms" 120000 t.expires_in_ms

let test_get_login_token_with_auth () =
  let log, fetch =
    mock_seq
      [
        {|{"versions":["v1.7"]}|};
        {|{"login_token":"lt_abc","expires_in_ms":1}|};
      ]
  in
  let auth = Matrix_client.Uiaa.dummy_auth ~session:"sess" () in
  ignore (ok (Auth.get_login_token (logged_in fetch) ~auth ()));
  check_str_opt "auth in body"
    (Some {|{"auth":{"type":"m.login.dummy","session":"sess"}}|})
    (match requests log with [ _; request ] -> request.body | _ -> None)

let test_get_login_token_unstable () =
  let log, fetch =
    mock_seq
      [
        {|{"versions":["v1.6"],"unstable_features":{"org.matrix.msc3882":true}}|};
        {|{"login_token":"lt_abc","expires_in_ms":1}|};
      ]
  in
  ignore (ok (Auth.get_login_token (logged_in fetch) ()));
  check_discovery_then_request log ~meth:"POST"
    ~url:
      "https://hs.example/_matrix/client/unstable/org.matrix.msc3882/login/get_token"
    ~body:"{}" ()

let test_register_available () =
  let log, fetch = mock (json {|{"available":true}|}) in
  let available =
    ok (Auth.register_available (client_of fetch) ~username:"bob")
  in
  check_request log ~meth:"GET"
    ~url:"https://hs.example/_matrix/client/v3/register/available?username=bob"
    ();
  check_bool "available" true available

let test_check_registration_token () =
  let log, fetch = mock (json {|{"valid":false}|}) in
  let valid =
    ok (Auth.check_registration_token (client_of fetch) ~token:"tok123")
  in
  check_request log ~meth:"GET"
    ~url:
      "https://hs.example/_matrix/client/v1/register/m.login.registration_token/validity?token=tok123"
    ();
  check_bool "valid" false valid

let test_registration_email_token () =
  let log, fetch =
    mock (json {|{"sid":"sid1","submit_url":"https://hs.example/submit"}|})
  in
  let r =
    ok
      (Uiaa.request_email_token (client_of fetch) ~use:Uiaa.Register
         ~email:"bob@example.org" ~client_secret:"secret" ~send_attempt:1
         ~next_link:"https://app.example/done" ())
  in
  check_request log ~meth:"POST"
    ~url:"https://hs.example/_matrix/client/v3/register/email/requestToken"
    ~body:
      ({|{"client_secret":"secret","email":"bob@example.org",|}
     ^ {|"send_attempt":1,"next_link":"https://app.example/done"}|})
    ();
  check_string "sid" "sid1" r.sid;
  check_str_opt "submit_url" (Some "https://hs.example/submit") r.submit_url

let test_password_msisdn_token () =
  let log, fetch = mock (json {|{"sid":"sid2"}|}) in
  let r =
    ok
      (Uiaa.request_msisdn_token (client_of fetch) ~use:Uiaa.Password
         ~country:"GB" ~phone_number:"7700900000" ~client_secret:"secret"
         ~send_attempt:2 ())
  in
  check_request log ~meth:"POST"
    ~url:
      "https://hs.example/_matrix/client/v3/account/password/msisdn/requestToken"
    ~body:
      ({|{"client_secret":"secret","country":"GB",|}
     ^ {|"phone_number":"7700900000","send_attempt":2}|})
    ();
  check_string "sid" "sid2" r.sid;
  check_str_opt "no submit_url" None r.submit_url

let test_requesttoken_is_unauthenticated () =
  (* Registration and password-reset token requests must not leak the
     current session's bearer token. *)
  let log, fetch = mock (json {|{"sid":"sid1"}|}) in
  ignore
    (ok
       (Uiaa.request_email_token (logged_in fetch) ~use:Uiaa.Register
          ~email:"bob@example.org" ~client_secret:"s" ~send_attempt:1 ()));
  check_str_opt "no authorization" None
    (Http.Header.get (one_request log).headers "authorization")

(* A [Logs] reporter that appends every formatted message to [buf], so a test
   can check what a debug-level log line actually contains. *)
let buffering_reporter buf =
  let report _src _level ~over k msgf =
    let k _ =
      over ();
      k ()
    in
    msgf (fun ?header:_ ?tags:_ fmt ->
        Format.kasprintf
          (fun s ->
            Buffer.add_string buf s;
            Buffer.add_char buf '\n';
            k ())
          fmt)
  in
  { Logs.report }

let test_credentials_are_not_logged () =
  (* Client.Http logs every request and response body at debug level. A
     password sent to log in, and the access token the server hands back,
     must never appear in that log verbatim. *)
  let _, fetch =
    mock
      (json
         {|{"user_id":"@alice:example.org","access_token":"syt_the_secret_token","device_id":"ABCDEF"}|})
  in
  let captured = Buffer.create 1024 in
  let old_reporter = Logs.reporter () and old_level = Logs.level () in
  Logs.set_reporter (buffering_reporter captured);
  Logs.set_level (Some Logs.Debug);
  Fun.protect
    ~finally:(fun () ->
      Logs.set_reporter old_reporter;
      Logs.set_level old_level)
    (fun () ->
      ignore
        (ok
           (Auth.login_password (client_of fetch) ~user:"alice"
              ~password:"hunter2-the-plaintext-password" ())));
  let logged = Buffer.contents captured in
  check_bool "the password never appears in the log" false
    (contains logged "hunter2-the-plaintext-password");
  check_bool "the access token never appears in the log" false
    (contains logged "syt_the_secret_token");
  check_bool "something was actually logged" true (String.length logged > 0)

(* A homeserver that answers registration with a malformed [device_id] must
   not crash the caller: [register] reports it as an [Error], the same as
   any other malformed response. *)
let test_register_malformed_device_id_is_an_error () =
  let _, fetch =
    mock
      (json
         {|{"user_id":"@alice:example.org","access_token":"syt_abc",
            "device_id":""}|})
  in
  match Auth.register (client_of fetch) ~username:"alice" ~password:"pw" () with
  | Error _ -> ()
  | Ok _ -> Alcotest.fail "an empty device_id should not be accepted"

let registration_response =
  {|{"user_id":"@alice:example.org","access_token":"syt_abc","device_id":"DEVICEID"}|}

let registration_body =
  {|{"username":"alice","password":"pw","device_id":"DEVICEID","initial_device_display_name":"integration","inhibit_login":false}|}

let registration_params : Auth.login_params =
  {
    device_id = Some "DEVICEID";
    initial_device_display_name = Some "integration";
  }

let test_register_uiaa_immediate_success () =
  let log, fetch = mock (json registration_response) in
  let result =
    Auth.register_uiaa (client_of fetch) ~username:"alice" ~password:"pw"
      ~params:registration_params ~inhibit_login:false
      ~auth_callback:(fun _ -> None)
      ()
  in
  (match result with
  | Uiaa.Uiaa_success session ->
      check_string "user id" "@alice:example.org"
        (Id.User_id.to_string session.user_id)
  | Uiaa.Uiaa_auth_required _ -> Alcotest.fail "unexpected UIAA challenge"
  | Uiaa.Uiaa_error e ->
      Alcotest.failf "registration failed: %s" (Error.to_string e));
  check_request log ~meth:"POST"
    ~url:"https://hs.example/_matrix/client/v3/register" ~body:registration_body
    ()

let test_register_uiaa_dummy_retry_retains_body () =
  let attempts = ref 0 in
  let log, fetch =
    mock (fun req ->
        incr attempts;
        if !attempts = 1 then
          Fetch_mock.respond ~status:401
            {|{"session":"sess","flows":[{"stages":["m.login.dummy"]}],"completed":[],"params":{}}|}
            req
        else Fetch_mock.respond registration_response req)
  in
  let seen_session = ref None in
  let result =
    Auth.register_uiaa (client_of fetch) ~username:"alice" ~password:"pw"
      ~params:registration_params ~inhibit_login:false
      ~auth_callback:(fun challenge ->
        seen_session := challenge.session;
        Some (Uiaa.dummy_auth ?session:challenge.session ()))
      ()
  in
  (match result with
  | Uiaa.Uiaa_success _ -> ()
  | Uiaa.Uiaa_auth_required _ -> Alcotest.fail "still challenged after retry"
  | Uiaa.Uiaa_error e ->
      Alcotest.failf "registration failed: %s" (Error.to_string e));
  check_str_opt "challenge session" (Some "sess") !seen_session;
  check_int "attempts" 2 !attempts;
  match requests log with
  | [ first; second ] ->
      check_str_opt "first body" (Some registration_body) first.body;
      check_str_opt "retry body"
        (Some
           {|{"auth":{"type":"m.login.dummy","session":"sess"},"username":"alice","password":"pw","device_id":"DEVICEID","initial_device_display_name":"integration","inhibit_login":false}|})
        second.body
  | rs -> Alcotest.failf "expected two requests, got %d" (List.length rs)

let test_register_uiaa_unsupported_is_auth_required () =
  let challenge =
    {|{"session":"sess","flows":[{"stages":["m.login.password"]},{"stages":["m.login.dummy","m.login.password"]}],"completed":[],"params":{}}|}
  in
  let log, fetch =
    mock (fun req -> Fetch_mock.respond ~status:401 challenge req)
  in
  match
    Auth.register_uiaa (client_of fetch) ~username:"alice" ~password:"pw"
      ~auth_callback:(fun _ -> None)
      ()
  with
  | Uiaa.Uiaa_auth_required got ->
      check_str_opt "session" (Some "sess") got.session;
      check_int "number of offered flows" 2 (List.length got.flows);
      check_int "one request" 1 (List.length (requests log))
  | Uiaa.Uiaa_success _ -> Alcotest.fail "unsupported challenge was accepted"
  | Uiaa.Uiaa_error e ->
      Alcotest.failf "challenge was lost: %s" (Error.to_string e)

let test_register_uiaa_malformed_and_non_401_errors () =
  let check_error name fetch expected_status =
    match
      Auth.register_uiaa (client_of fetch) ~username:"alice" ~password:"pw"
        ~auth_callback:(fun _ -> None)
        ()
    with
    | Uiaa.Uiaa_error (Error.Http_error { status; _ }) ->
        check_int name expected_status status
    | Uiaa.Uiaa_error e ->
        Alcotest.failf "%s: expected HTTP error, got %s" name
          (Error.to_string e)
    | Uiaa.Uiaa_success _ -> Alcotest.failf "%s: unexpected success" name
    | Uiaa.Uiaa_auth_required _ ->
        Alcotest.failf "%s: malformed/non-401 error became a challenge" name
  in
  let _, malformed =
    mock (fun req -> Fetch_mock.respond ~status:401 "not a UIAA body" req)
  in
  check_error "malformed 401" malformed 401;
  let _, non_401 =
    mock (fun req -> Fetch_mock.respond ~status:503 "temporarily down" req)
  in
  check_error "non-401" non_401 503

let test_register_uiaa_stops_after_second_challenge () =
  let attempts = ref 0 in
  let callback_calls = ref 0 in
  let log, fetch =
    mock (fun req ->
        incr attempts;
        if !attempts = 1 then
          Fetch_mock.respond ~status:401
            {|{"session":"first","flows":[{"stages":["m.login.dummy"]}],"completed":[],"params":{}}|}
            req
        else
          Fetch_mock.respond ~status:401
            {|{"session":"second","flows":[{"stages":["m.login.password"]}],"completed":[],"params":{}}|}
            req)
  in
  match
    Auth.register_uiaa (client_of fetch) ~username:"alice" ~password:"pw"
      ~auth_callback:(fun challenge ->
        incr callback_calls;
        Some (Uiaa.dummy_auth ?session:challenge.session ()))
      ()
  with
  | Uiaa.Uiaa_auth_required challenge ->
      check_str_opt "second challenge session" (Some "second") challenge.session;
      check_int "requests" 2 !attempts;
      check_int "callback called once" 1 !callback_calls;
      check_int "recorded requests" 2 (List.length (requests log))
  | Uiaa.Uiaa_success _ -> Alcotest.fail "second challenge was ignored"
  | Uiaa.Uiaa_error e ->
      Alcotest.failf "second challenge was an error: %s" (Error.to_string e)

(* {1 Profile: extended fields (MSC4133)} *)

let test_extended_profile () =
  let log, fetch =
    mock
      (json
         {|{"displayname":"Alice","avatar_url":"mxc://example.org/a",
            "m.tz":"Europe/London","com.example.rank":3}|})
  in
  let p =
    ok
      (Profile.get_profile (logged_in fetch) ~user_id:(uid "@alice:example.org"))
  in
  check_request log ~meth:"GET"
    ~url:"https://hs.example/_matrix/client/v3/profile/@alice:example.org" ();
  check_str_opt "displayname" (Some "Alice") p.displayname;
  check_str_opt "avatar_url" (Some "mxc://example.org/a")
    (Option.map Matrix_client.Media.Mxc.to_string p.avatar_url);
  Alcotest.(check (list string))
    "extra fields"
    [ "com.example.rank"; "m.tz" ]
    (List.map fst p.fields);
  check_str_opt "timezone" (Some {|"Europe/London"|})
    (Option.map json_string (List.assoc_opt "m.tz" p.fields))

let test_profile_get_field () =
  let log, fetch =
    mock_seq [ {|{"versions":["v1.16"]}|}; {|{"m.tz":"Europe/London"}|} ]
  in
  let v =
    ok
      (Profile.find_field (logged_in fetch) ~user_id:(uid "@alice:example.org")
         ~key:"m.tz")
  in
  check_discovery_then_request log ~meth:"GET"
    ~url:"https://hs.example/_matrix/client/v3/profile/@alice:example.org/m.tz"
    ();
  check_str_opt "value" (Some {|"Europe/London"|}) (Option.map json_string v)

let test_profile_get_field_unstable () =
  let log, fetch =
    mock_seq
      [
        {|{"versions":["v1.15"],"unstable_features":{"uk.tcpip.msc4133":true}}|};
        {|{"m.tz":"Europe/London"}|};
      ]
  in
  ignore
    (ok
       (Profile.find_field (logged_in fetch) ~user_id:(uid "@alice:example.org")
          ~key:"m.tz"));
  check_discovery_then_request log ~meth:"GET"
    ~url:
      "https://hs.example/_matrix/client/unstable/uk.tcpip.msc4133/profile/@alice:example.org/m.tz"
    ()

let test_profile_get_field_unset () =
  let _, fetch = mock_seq [ {|{"versions":["v1.16"]}|}; "{}" ] in
  let v =
    ok
      (Profile.find_field (logged_in fetch) ~user_id:(uid "@alice:example.org")
         ~key:"m.tz")
  in
  check_bool "unset field is None" true (v = None)

let test_profile_set_field () =
  let log, fetch = mock_seq [ {|{"versions":["v1.16"]}|}; "{}" ] in
  let value =
    Result.get_ok
      (Jsont_bytesrw.decode_string Matrix_proto.Json.Codec.json
         {|"Europe/London"|})
  in
  ok (Profile.set_field (logged_in fetch) ~key:"m.tz" ~value);
  check_discovery_then_request log ~meth:"PUT"
    ~url:"https://hs.example/_matrix/client/v3/profile/@alice:example.org/m.tz"
    ~body:{|{"m.tz":"Europe/London"}|} ()

let test_profile_delete_field () =
  let log, fetch = mock_seq [ {|{"versions":["v1.16"]}|}; "{}" ] in
  ok (Profile.delete_field (logged_in fetch) ~key:"m.tz");
  check_discovery_then_request log ~meth:"DELETE"
    ~url:"https://hs.example/_matrix/client/v3/profile/@alice:example.org/m.tz"
    ()

(* {1 Directory aliases}

    These predate this work; the tests pin their paths so the [#] encoding
    is not changed by accident. *)

let test_directory_resolve_alias () =
  let log, fetch =
    mock (json {|{"room_id":"!r:example.org","servers":["example.org"]}|})
  in
  let info =
    ok
      (Directory.resolve_alias (logged_in fetch)
         ~alias:(alias "#lobby:example.org"))
  in
  check_request log ~meth:"GET"
    ~url:
      "https://hs.example/_matrix/client/v3/directory/room/%23lobby:example.org"
    ();
  check_string "room_id" "!r:example.org" (Id.Room_id.to_string info.room_id)

let test_directory_create_alias () =
  let log, fetch = mock (json "{}") in
  ok
    (Directory.create_alias (logged_in fetch)
       ~alias:(alias "#lobby:example.org")
       ~room_id:(rid "!r:example.org"));
  check_request log ~meth:"PUT"
    ~url:
      "https://hs.example/_matrix/client/v3/directory/room/%23lobby:example.org"
    ~body:{|{"room_id":"!r:example.org"}|} ()

let test_directory_delete_alias () =
  let log, fetch = mock (json "{}") in
  ok
    (Directory.delete_alias (logged_in fetch)
       ~alias:(alias "#lobby:example.org"));
  check_request log ~meth:"DELETE"
    ~url:
      "https://hs.example/_matrix/client/v3/directory/room/%23lobby:example.org"
    ()

(* {1 Encoder regressions}

    Five request codecs declared a required member with no [~enc], so
    {!Jsont} refused to encode the body and the call failed before it ever
    reached the network. These pin the bodies that are now produced. *)

let test_directory_set_visibility () =
  let log, fetch = mock (json "{}") in
  ok
    (Directory.set_visibility (logged_in fetch) ~room_id:(rid "!r:example.org")
       ~visibility:Matrix_proto.Common.Visibility.Public);
  check_request log ~meth:"PUT"
    ~url:
      "https://hs.example/_matrix/client/v3/directory/list/room/!r:example.org"
    ~body:{|{"visibility":"public"}|} ()

let test_rooms_invite () =
  let log, fetch = mock (json "{}") in
  ok
    (Rooms.invite (logged_in fetch) ~room_id:(rid "!r:example.org")
       ~user_id:(uid "@bob:example.org") ~reason:"join us" ());
  check_request log ~meth:"POST"
    ~url:"https://hs.example/_matrix/client/v3/rooms/!r:example.org/invite"
    ~body:{|{"user_id":"@bob:example.org","reason":"join us"}|} ()

let test_rooms_kick () =
  let log, fetch = mock (json "{}") in
  ok
    (Rooms.kick (logged_in fetch) ~room_id:(rid "!r:example.org")
       ~user_id:(uid "@bob:example.org") ());
  check_request log ~meth:"POST"
    ~url:"https://hs.example/_matrix/client/v3/rooms/!r:example.org/kick"
    ~body:{|{"user_id":"@bob:example.org"}|} ()

let test_rooms_ban_and_unban () =
  let log, fetch = mock (json "{}") in
  ok
    (Rooms.ban (logged_in fetch) ~room_id:(rid "!r:example.org")
       ~user_id:(uid "@bob:example.org") ~reason:"spam" ());
  check_request log ~meth:"POST"
    ~url:"https://hs.example/_matrix/client/v3/rooms/!r:example.org/ban"
    ~body:{|{"user_id":"@bob:example.org","reason":"spam"}|} ();
  let log, fetch = mock (json "{}") in
  ok
    (Rooms.unban (logged_in fetch) ~room_id:(rid "!r:example.org")
       ~user_id:(uid "@bob:example.org") ());
  check_request log ~meth:"POST"
    ~url:"https://hs.example/_matrix/client/v3/rooms/!r:example.org/unban"
    ~body:{|{"user_id":"@bob:example.org"}|} ()

let test_profile_set_displayname () =
  let log, fetch = mock (json "{}") in
  ok (Profile.set_displayname (logged_in fetch) ~displayname:"Alice");
  check_request log ~meth:"PUT"
    ~url:
      "https://hs.example/_matrix/client/v3/profile/@alice:example.org/displayname"
    ~body:{|{"displayname":"Alice"}|} ()

let test_profile_clear_displayname () =
  let log, fetch = mock (json "{}") in
  ok (Profile.clear_displayname (logged_in fetch));
  check_request log ~meth:"PUT"
    ~url:
      "https://hs.example/_matrix/client/v3/profile/@alice:example.org/displayname"
    ~body:{|{"displayname":null}|} ()

let test_profile_set_avatar_url () =
  let log, fetch = mock (json "{}") in
  ok
    (Profile.set_avatar_url (logged_in fetch)
       ~avatar_url:
         (Result.get_ok
            (Matrix_client.Media.Mxc.of_string "mxc://example.org/a")));
  check_request log ~meth:"PUT"
    ~url:
      "https://hs.example/_matrix/client/v3/profile/@alice:example.org/avatar_url"
    ~body:{|{"avatar_url":"mxc://example.org/a"}|} ()

let test_profile_clear_avatar_url () =
  let log, fetch = mock (json "{}") in
  ok (Profile.clear_avatar_url (logged_in fetch));
  check_request log ~meth:"PUT"
    ~url:
      "https://hs.example/_matrix/client/v3/profile/@alice:example.org/avatar_url"
    ~body:{|{"avatar_url":null}|} ()

(* {1 Runner} *)

let () =
  Alcotest.run "matrix.client cs-api"
    [
      ( "server",
        [
          Alcotest.test_case "versions" `Quick (run test_versions);
          Alcotest.test_case "capabilities" `Quick (run test_capabilities);
          Alcotest.test_case "capability helpers" `Quick
            (run test_homeserver_capability_helpers);
          Alcotest.test_case "metadata cache" `Quick
            (run test_server_metadata_cache);
          Alcotest.test_case "metadata failures are retried" `Quick
            (run test_server_metadata_failure_is_retried);
          Alcotest.test_case "metadata HTTP failures are retried" `Quick
            (run test_server_metadata_http_failure_is_retried);
          Alcotest.test_case "capabilities cache" `Quick
            (run test_server_capabilities_cache);
          Alcotest.test_case "capabilities refresh" `Quick
            (run test_server_capabilities_refresh);
          Alcotest.test_case "profile capability helpers" `Quick
            (run test_profile_capability_helpers);
          Alcotest.test_case "extended profile fields" `Quick
            (run test_extended_profile_fields);
          Alcotest.test_case "metadata cache is credential-scoped" `Quick
            (run test_server_metadata_cache_is_credential_scoped);
          Alcotest.test_case "well-known" `Quick (run test_well_known);
          Alcotest.test_case "well-known 404 is None" `Quick
            (run test_well_known_absent);
          Alcotest.test_case "well-known URL validation" `Quick
            (run test_well_known_url_validation);
          Alcotest.test_case "well-known authentication URL validation" `Quick
            (run test_well_known_authentication_url_validation);
          Alcotest.test_case "discover" `Quick (run test_discover);
          Alcotest.test_case "discover off-origin" `Quick
            (run test_discover_off_origin);
          Alcotest.test_case "discover delegated versions" `Quick
            (run test_discover_off_origin_callback);
          Alcotest.test_case "discover delegated error" `Quick
            (run test_discover_off_origin_callback_error);
          Alcotest.test_case "discover delegated transport error" `Quick
            (run test_discover_off_origin_callback_transport_error);
        ] );
      ( "search",
        [
          Alcotest.test_case "room events" `Quick (run test_search_room_events);
          Alcotest.test_case "user directory" `Quick
            (run test_search_user_directory);
        ] );
      ( "notifications",
        [ Alcotest.test_case "list" `Quick (run test_notifications) ] );
      ( "push",
        [
          Alcotest.test_case "get rules" `Quick (run test_get_push_rules);
          Alcotest.test_case "set enabled" `Quick
            (run test_set_push_rule_enabled);
        ] );
      ( "threads",
        [
          Alcotest.test_case "list stable" `Quick (run test_threads);
          Alcotest.test_case "list unstable" `Quick (run test_threads_unstable);
        ] );
      ( "reporting",
        [
          Alcotest.test_case "event" `Quick (run test_report_event);
          Alcotest.test_case "room" `Quick (run test_report_room);
          Alcotest.test_case "user" `Quick (run test_report_user);
          Alcotest.test_case "room unstable" `Quick
            (run test_report_room_unstable);
          Alcotest.test_case "user unstable" `Quick
            (run test_report_user_unstable);
        ] );
      ( "tags",
        [
          Alcotest.test_case "get" `Quick (run test_tags_get);
          Alcotest.test_case "set" `Quick (run test_tags_set);
          Alcotest.test_case "remove" `Quick (run test_tags_remove);
          Alcotest.test_case "favourite helper" `Quick
            (run test_tags_favourite_helper);
          Alcotest.test_case "helper needs a session" `Quick
            (run test_tags_helper_needs_session);
        ] );
      ( "openid",
        [
          Alcotest.test_case "request_token" `Quick
            (run test_openid_request_token);
        ] );
      ( "thirdparty",
        [
          Alcotest.test_case "protocols" `Quick (run test_thirdparty_protocols);
          Alcotest.test_case "protocol" `Quick (run test_thirdparty_protocol);
          Alcotest.test_case "location by alias" `Quick
            (run test_thirdparty_location_by_alias);
          Alcotest.test_case "location by protocol" `Quick
            (run test_thirdparty_location_by_protocol);
          Alcotest.test_case "user by user id" `Quick
            (run test_thirdparty_user_by_user_id);
          Alcotest.test_case "user by protocol" `Quick
            (run test_thirdparty_user_by_protocol);
        ] );
      ( "delayed events",
        [
          Alcotest.test_case "send" `Quick (run test_delayed_send);
          Alcotest.test_case "send state" `Quick (run test_delayed_send_state);
          Alcotest.test_case "send current" `Quick
            (run test_delayed_send_current);
          Alcotest.test_case "send current state empty key" `Quick
            (run test_delayed_send_current_state_empty_key);
          Alcotest.test_case "update" `Quick (run test_delayed_update);
          Alcotest.test_case "update current unauthenticated" `Quick
            (run test_delayed_update_current_is_unauthenticated);
          Alcotest.test_case "update current errors" `Quick
            (run test_delayed_update_current_errors);
          Alcotest.test_case "list" `Quick (run test_delayed_list);
          Alcotest.test_case "current get list and status" `Quick
            (run test_delayed_current_get_list_and_status);
          Alcotest.test_case "current required fields" `Quick
            (run test_delayed_current_required_fields);
          Alcotest.test_case "current negative values" `Quick
            (run test_delayed_current_negative_values);
        ] );
      ( "rooms",
        [
          Alcotest.test_case "upgrade" `Quick (run test_room_upgrade);
          Alcotest.test_case "aliases" `Quick (run test_room_aliases);
          Alcotest.test_case "joined_members" `Quick
            (run test_room_joined_members);
          Alcotest.test_case "joined_members with a malformed key" `Quick
            (run test_room_joined_members_bad_key);
          Alcotest.test_case "timestamp_to_event" `Quick
            (run test_room_timestamp_to_event);
        ] );
      ( "auth",
        [
          Alcotest.test_case "login get_token" `Quick (run test_get_login_token);
          Alcotest.test_case "login get_token with UIAA" `Quick
            (run test_get_login_token_with_auth);
          Alcotest.test_case "login get_token unstable" `Quick
            (run test_get_login_token_unstable);
          Alcotest.test_case "register available" `Quick
            (run test_register_available);
          Alcotest.test_case "registration token validity" `Quick
            (run test_check_registration_token);
          Alcotest.test_case "register email requestToken" `Quick
            (run test_registration_email_token);
          Alcotest.test_case "password msisdn requestToken" `Quick
            (run test_password_msisdn_token);
          Alcotest.test_case "requestToken sends no bearer token" `Quick
            (run test_requesttoken_is_unauthenticated);
          Alcotest.test_case "credentials are not logged" `Quick
            (run test_credentials_are_not_logged);
          Alcotest.test_case "a malformed device_id is an error, not a raise"
            `Quick
            (run test_register_malformed_device_id_is_an_error);
          Alcotest.test_case "register UIAA immediate success" `Quick
            (run test_register_uiaa_immediate_success);
          Alcotest.test_case "register UIAA dummy retry retains body" `Quick
            (run test_register_uiaa_dummy_retry_retains_body);
          Alcotest.test_case "register UIAA unsupported challenge" `Quick
            (run test_register_uiaa_unsupported_is_auth_required);
          Alcotest.test_case "register UIAA malformed and non-401 errors" `Quick
            (run test_register_uiaa_malformed_and_non_401_errors);
          Alcotest.test_case "register UIAA stops after second challenge" `Quick
            (run test_register_uiaa_stops_after_second_challenge);
        ] );
      ( "profile",
        [
          Alcotest.test_case "extended profile" `Quick
            (run test_extended_profile);
          Alcotest.test_case "get field" `Quick (run test_profile_get_field);
          Alcotest.test_case "get field unstable" `Quick
            (run test_profile_get_field_unstable);
          Alcotest.test_case "get unset field" `Quick
            (run test_profile_get_field_unset);
          Alcotest.test_case "set field" `Quick (run test_profile_set_field);
          Alcotest.test_case "delete field" `Quick
            (run test_profile_delete_field);
        ] );
      ( "directory",
        [
          Alcotest.test_case "resolve alias" `Quick
            (run test_directory_resolve_alias);
          Alcotest.test_case "create alias" `Quick
            (run test_directory_create_alias);
          Alcotest.test_case "delete alias" `Quick
            (run test_directory_delete_alias);
        ] );
      ( "encoder regressions",
        [
          Alcotest.test_case "Directory.set_visibility" `Quick
            (run test_directory_set_visibility);
          Alcotest.test_case "Rooms.invite" `Quick (run test_rooms_invite);
          Alcotest.test_case "Rooms.kick" `Quick (run test_rooms_kick);
          Alcotest.test_case "Account_data.find_dm_rooms tolerates bad keys"
            `Quick
            (run test_find_dm_rooms_tolerates_bad_keys);
          Alcotest.test_case "Rooms.ban and unban" `Quick
            (run test_rooms_ban_and_unban);
          Alcotest.test_case "Profile.set_displayname" `Quick
            (run test_profile_set_displayname);
          Alcotest.test_case "Profile.clear_displayname" `Quick
            (run test_profile_clear_displayname);
          Alcotest.test_case "Profile.set_avatar_url" `Quick
            (run test_profile_set_avatar_url);
          Alcotest.test_case "Profile.clear_avatar_url" `Quick
            (run test_profile_clear_avatar_url);
        ] );
    ]
