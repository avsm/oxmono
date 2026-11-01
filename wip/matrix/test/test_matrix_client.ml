(** Tests for {!Matrix_client}'s HTTP layer against a mock homeserver.

    Every test builds a fresh {!Fetch_mock} client that records the requests it
    is handed and answers them from a canned script, so the assertions are on
    the request that actually leaves {!Matrix_client.Client} — after
    [Fetch.restrict] and [Fetch.with_credentials] have run — and on the value
    the library decodes from the reply.

    The mock tests run under [Eio_mock.Backend.run]; the session store tests
    need a real filesystem and run under [Eio_main.run]. The default-path test
    points XDG at a temporary directory; the explicit-root test does not touch
    the XDG environment. *)

module Client = Matrix_client.Client
module Auth = Matrix_client.Auth
module Rooms = Matrix_client.Rooms
module Messages = Matrix_client.Messages
module Media = Matrix_client.Media
module Dehydrated_device = Matrix_client.Dehydrated_device
module Thread_subscriptions = Matrix_client.Thread_subscriptions
module Store = Matrix_client.Store
module Attachment = Matrix_client.Encrypted_attachment
module Error = Matrix_client.Error
module Session = Matrix_client.Session
module Profile_store = Matrix_client.Profile_store
module Presence = Matrix_client.Presence
module Sync = Matrix_client.Sync
module Sliding_sync = Matrix_client.Sliding_sync
module Route = Matrix_client.Route
module Id = Matrix_proto.Id

(* [Client.create] only reads [secure_random] from the environment, and none
   of the code under test draws from it, so a fixed source is enough. *)
let mock_env =
  object
    method secure_random =
      Eio.Flow.string_source (String.init 4096 (fun i -> Char.chr (i land 255)))
  end

(* One recorded exchange as the backend saw it. *)
type recorded = {
  meth : string;
  url : string;
  headers : Http.Header.t;
  body : string option;  (** [None] for a request with no body. *)
}

let body_of_request (req : Fetch.Middleware.request) =
  match req.body with
  | Fetch.Empty -> None
  | Fetch.String s -> Some s
  | Fetch.Stream _ -> Some "<stream>"

(* [mock handler] is a client that records each request and answers it with
   [handler]. Recorded requests come back oldest first. *)
let mock handler =
  let log = ref [] in
  let client =
    Fetch_mock.client (fun (req : Fetch.Middleware.request) ->
        log :=
          {
            meth = Http.Method.to_string req.meth;
            url = Fetch.Middleware.Url.to_string req.url;
            headers = req.headers;
            body = body_of_request req;
          }
          :: !log;
        handler req)
  in
  (log, client)

let default_homeserver = "https://hs.example"
let user_agent = "matrix-test/1.0"

let client_of ?(homeserver = default_homeserver) ?(user_agent = Some user_agent)
    fetch =
  let config =
    Client.config ~homeserver:(Uriz.of_string_exn homeserver) ?user_agent ()
  in
  Client.create ~config ~fetch ~random:(Matrix_client.Random.of_env mock_env)

(* A client whose requests carry a bearer token. *)
let uid s = Result.get_ok (Id.User_id.of_string s)
let did s = Result.get_ok (Id.Device_id.of_string s)
let rid s = Result.get_ok (Id.Room_id.of_string s)
let server s = Result.get_ok (Id.Server_name.of_string s)

let test_session : Client.session =
  {
    user_id = uid "@alice:example.org";
    access_token = "syt_secret_token";
    device_id = did "TESTDEVICE";
    refresh_token = None;
  }

let requests log = List.rev !log

let one_request log =
  match requests log with
  | [ r ] -> r
  | rs -> Alcotest.failf "expected exactly one request, got %d" (List.length rs)

let header r name = Http.Header.get r.headers name
let check_string = Alcotest.(check string)
let check_int = Alcotest.(check int)
let check_bool = Alcotest.(check bool)
let check_str_opt = Alcotest.(check (option string))
let run f () = Eio_mock.Backend.run f
let run_full f () = Eio_mock.Backend.run_full f

let ok_body = function
  | Ok b -> b
  | Error e -> Alcotest.failf "expected Ok, got error: %s" (Error.to_string e)

let get_error = function
  | Ok _ -> Alcotest.fail "expected Error, got Ok"
  | Error e -> e

let json body = Fetch_mock.respond body

let presence_name = function
  | `Online -> "online"
  | `Offline -> "offline"
  | `Unavailable -> "unavailable"

let has_substring ~needle s =
  let n = String.length needle and m = String.length s in
  let rec go i = i + n <= m && (String.sub s i n = needle || go (i + 1)) in
  n = 0 || go 0

let test_client_owned_presence () =
  let _, fetch = mock (json "{}") in
  let client = client_of fetch in
  check_string "default presence" "online"
    (presence_name (Client.sync_presence client));
  let derived = Client.with_session client test_session in
  Client.set_sync_presence derived `Offline;
  check_string "derived client shares presence" "offline"
    (presence_name (Client.sync_presence client));
  let calls = ref 0 in
  let unregister =
    Client.register_presence_wakeup client (fun () -> incr calls)
  in
  Client.set_sync_presence client `Offline;
  check_int "same presence does not wake" 0 !calls;
  Client.set_sync_presence client `Unavailable;
  check_int "effective presence wakes" 1 !calls;
  unregister ();
  unregister ();
  Client.set_sync_presence client `Online;
  check_int "unregister is idempotent" 1 !calls

let test_presence_immediate_false () =
  let log, fetch = mock (json "{}") in
  let client = client_of fetch in
  match
    Presence.set_presence client ~presence:Presence.Offline ~immediate:false ()
  with
  | Error error ->
      Alcotest.failf "local presence update failed: %s" (Error.to_string error)
  | Ok () ->
      check_string "local presence" "offline"
        (presence_name (Client.sync_presence client));
      check_int "no request for non-immediate update" 0
        (List.length (requests log))

let test_presence_failure_retains_local_state () =
  let _, fetch = mock (fun req -> Fetch_mock.respond ~status:500 "{}" req) in
  let client = Client.with_session (client_of fetch) test_session in
  match Presence.set_presence client ~presence:Presence.Unavailable () with
  | Ok () -> Alcotest.fail "presence request unexpectedly succeeded"
  | Error _ ->
      check_string "failed request retains local presence" "unavailable"
        (presence_name (Client.sync_presence client))

let test_sync_uses_client_presence () =
  let log, fetch = mock (json {|{"next_batch":"batch"}|}) in
  let client = client_of fetch in
  ignore
    (match Sync.sync_once client () with
    | Ok response -> response
    | Error error -> Alcotest.failf "sync failed: %s" (Error.to_string error));
  let default_request = List.hd (requests log) in
  Alcotest.(check bool)
    "default online presence is omitted" false
    (has_substring ~needle:"set_presence=" default_request.url);
  Client.set_sync_presence client `Unavailable;
  ignore
    (match Sync.sync_once client () with
    | Ok response -> response
    | Error error -> Alcotest.failf "sync failed: %s" (Error.to_string error));
  let nondefault_request = List.nth (requests log) 1 in
  Alcotest.(check bool)
    "nondefault presence is sent" true
    (has_substring ~needle:"set_presence=unavailable" nondefault_request.url);
  ignore
    (match
       Sync.sync_once client
         ~params:{ Sync.default_params with set_presence = Some `Online }
         ()
     with
    | Ok response -> response
    | Error error ->
        Alcotest.failf "sync override failed: %s" (Error.to_string error));
  Alcotest.(check bool)
    "explicit online presence follows wire default" false
    (has_substring ~needle:"set_presence=" (List.nth (requests log) 2).url);
  ignore
    (match
       Sync.sync_once client
         ~params:{ Sync.default_params with set_presence = Some `Offline }
         ()
     with
    | Ok response -> response
    | Error error ->
        Alcotest.failf "sync override failed: %s" (Error.to_string error));
  Alcotest.(check bool)
    "explicit offline overrides client" true
    (has_substring ~needle:"set_presence=offline"
       (List.nth (requests log) 3).url)

let test_sliding_sync_uses_client_presence () =
  let log, fetch = mock (json {|{"pos":"next"}|}) in
  let client = client_of fetch in
  ignore
    (match
       Sliding_sync.sync_once client (Matrix_proto.Sliding_sync.Request.v ())
     with
    | Ok response -> response
    | Error error ->
        Alcotest.failf "sliding sync failed: %s" (Error.to_string error));
  Alcotest.(check bool)
    "sliding online default is omitted" false
    (has_substring ~needle:"set_presence=" (one_request log).url)

let test_get_unauthenticated () =
  let log, fetch = mock (json {|{"user_id":"@alice:example.org"}|}) in
  let t = client_of fetch in
  let body = ok_body (Client.Http.get t ~path:"/account/whoami" ()) in
  check_string "body" {|{"user_id":"@alice:example.org"}|} body;
  let r = one_request log in
  check_string "method" "GET" r.meth;
  check_string "url" "https://hs.example/_matrix/client/v3/account/whoami" r.url;
  check_str_opt "accept" (Some "application/json") (header r "accept");
  check_str_opt "user-agent" (Some user_agent) (header r "user-agent");
  check_str_opt "no authorization" None (header r "authorization");
  Alcotest.(check (option string)) "no body" None r.body

let test_get_authenticated () =
  let log, fetch = mock (json "{}") in
  let t = Client.with_session (client_of fetch) test_session in
  ignore (ok_body (Client.Http.get t ~path:"/account/whoami" ()));
  let r = one_request log in
  check_str_opt "authorization" (Some "Bearer syt_secret_token")
    (header r "authorization")

let test_post_unauthenticated_carries_no_token () =
  (* [post_unauthenticated] goes through the pre-credential client even on a
     logged-in [t]. *)
  let log, fetch = mock (json "{}") in
  let t = Client.with_session (client_of fetch) test_session in
  ignore
    (ok_body (Client.Http.post_unauthenticated t ~path:"/login" ~body:"{}" ()));
  let r = one_request log in
  check_str_opt "no authorization" None (header r "authorization")

let test_no_user_agent () =
  let log, fetch = mock (json "{}") in
  let t = client_of ~user_agent:None fetch in
  ignore (ok_body (Client.Http.get t ~path:"/account/whoami" ()));
  let r = one_request log in
  check_str_opt "user-agent" None (header r "user-agent")

let test_query_params () =
  let log, fetch = mock (json "{}") in
  let t = client_of fetch in
  ignore
    (ok_body
       (Client.Http.get t ~path:"/rooms"
          ~query:[ ("limit", "10"); ("filter", "a b"); ("q", "x&y=z") ]
          ()));
  let r = one_request log in
  check_string "url"
    "https://hs.example/_matrix/client/v3/rooms?limit=10&filter=a%20b&q=x%26y%3Dz"
    r.url

let test_post_json () =
  let log, fetch = mock (json {|{"ok":true}|}) in
  let t = client_of fetch in
  let sent = {|{"reason":"because"}|} in
  ignore (ok_body (Client.Http.post t ~path:"/join/!r:x" ~body:sent ()));
  let r = one_request log in
  check_string "method" "POST" r.meth;
  check_str_opt "content-type" (Some "application/json")
    (header r "content-type");
  check_str_opt "accept" (Some "application/json") (header r "accept");
  check_str_opt "body verbatim" (Some sent) r.body

let test_put_json () =
  let log, fetch = mock (json "{}") in
  let t = client_of fetch in
  ignore (ok_body (Client.Http.put t ~path:"/x" ~body:{|{"a":1}|} ()));
  let r = one_request log in
  check_string "method" "PUT" r.meth;
  check_str_opt "content-type" (Some "application/json")
    (header r "content-type");
  check_str_opt "body" (Some {|{"a":1}|}) r.body

let test_delete_without_body () =
  let log, fetch = mock (json "{}") in
  let t = client_of fetch in
  ignore (ok_body (Client.Http.delete t ~path:"/devices/D" ()));
  let r = one_request log in
  check_string "method" "DELETE" r.meth;
  check_str_opt "no body" None r.body;
  check_str_opt "no content-type" None (header r "content-type");
  check_str_opt "accept" (Some "application/json") (header r "accept")

let test_delete_with_body () =
  let log, fetch = mock (json "{}") in
  let t = client_of fetch in
  ignore
    (ok_body (Client.Http.delete t ~path:"/devices/D" ~body:{|{"auth":{}}|} ()));
  let r = one_request log in
  check_string "method" "DELETE" r.meth;
  check_str_opt "body" (Some {|{"auth":{}}|}) r.body;
  check_str_opt "content-type" (Some "application/json")
    (header r "content-type")

let test_raw_paths_are_absolute () =
  (* [get_bytes]/[post_bytes] must not get the /_matrix/client/v3 prefix. *)
  let log, fetch = mock (json "{}") in
  let t = client_of fetch in
  ignore (Client.Http.get_bytes t ~path:"/_matrix/client/v1/media/config" ());
  ignore
    (Client.Http.post_bytes t ~path:"/_matrix/media/v3/upload"
       ~content_type:"text/plain" ~body:"hi" ());
  match requests log with
  | [ g; p ] ->
      check_string "get url" "https://hs.example/_matrix/client/v1/media/config"
        g.url;
      check_str_opt "get sends no accept" None (header g "accept");
      check_string "post url" "https://hs.example/_matrix/media/v3/upload" p.url;
      check_str_opt "post content-type" (Some "text/plain")
        (header p "content-type")
  | rs -> Alcotest.failf "expected 2 requests, got %d" (List.length rs)

let test_absolute_json_paths () =
  let log, fetch = mock (json "{}") in
  let t = Client.with_session (client_of fetch) test_session in
  let prefix = "/_matrix/client/unstable/org.matrix.msc3814.v1/device" in
  ignore (Client.Http.get_absolute t ~path:prefix ~query:[ ("from", "a b") ] ());
  ignore
    (Client.Http.post_absolute t ~path:prefix
       ~query:[ ("txn", "x&y") ]
       ~body:{|{"post":true}|} ());
  ignore
    (Client.Http.put_absolute t ~path:prefix
       ~query:[ ("version", "1") ]
       ~body:{|{"put":true}|} ());
  ignore
    (Client.Http.delete_absolute t ~path:prefix
       ~query:[ ("force", "true") ]
       ~body:{|{"delete":true}|} ());
  match requests log with
  | [ g; p; u; d ] ->
      check_string "get method" "GET" g.meth;
      check_string "get url"
        "https://hs.example/_matrix/client/unstable/org.matrix.msc3814.v1/device?from=a%20b"
        g.url;
      check_str_opt "get authorization" (Some "Bearer syt_secret_token")
        (header g "authorization");
      check_str_opt "get body" None g.body;
      check_string "post method" "POST" p.meth;
      check_string "post url"
        "https://hs.example/_matrix/client/unstable/org.matrix.msc3814.v1/device?txn=x%26y"
        p.url;
      check_str_opt "post authorization" (Some "Bearer syt_secret_token")
        (header p "authorization");
      check_str_opt "post body" (Some {|{"post":true}|}) p.body;
      check_string "put method" "PUT" u.meth;
      check_string "put url"
        "https://hs.example/_matrix/client/unstable/org.matrix.msc3814.v1/device?version=1"
        u.url;
      check_str_opt "put authorization" (Some "Bearer syt_secret_token")
        (header u "authorization");
      check_str_opt "put body" (Some {|{"put":true}|}) u.body;
      check_string "delete method" "DELETE" d.meth;
      check_string "delete url"
        "https://hs.example/_matrix/client/unstable/org.matrix.msc3814.v1/device?force=true"
        d.url;
      check_str_opt "delete authorization" (Some "Bearer syt_secret_token")
        (header d "authorization");
      check_str_opt "delete body" (Some {|{"delete":true}|}) d.body
  | rs -> Alcotest.failf "expected 4 requests, got %d" (List.length rs)

let test_get_stream () =
  let payload = "streamed\000binary body" in
  let log, fetch =
    mock (fun req ->
        Fetch_mock.respond
          ~headers:(Http.Header.of_list [ ("content-type", "video/test") ])
          payload req)
  in
  let t = Client.with_session (client_of fetch) test_session in
  let body = Buffer.create 32 in
  let content_type = ref None in
  match
    Client.Http.get_stream t ~path:"/_matrix/media/v1/media/download/x/y"
      ~on_response:(fun ~content_type:ct source ->
        content_type := ct;
        Eio.Flow.copy source (Eio.Flow.buffer_sink body))
      ()
  with
  | Error e -> Alcotest.failf "stream GET failed: %s" (Error.to_string e)
  | Ok () ->
      check_string "body" payload (Buffer.contents body);
      check_str_opt "content-type" (Some "video/test") !content_type;
      let r = one_request log in
      check_string "method" "GET" r.meth;
      check_str_opt "authorization" (Some "Bearer syt_secret_token")
        (header r "authorization")

let test_get_stream_http_error_does_not_call_callback () =
  let called = ref false in
  let _, fetch =
    mock (fun req ->
        Fetch_mock.respond ~status:403
          {|{"errcode":"M_FORBIDDEN","error":"no"}|} req)
  in
  let t = client_of fetch in
  match
    Client.Http.get_stream t ~path:"/_matrix/media/v1/media/download/x/y"
      ~on_response:(fun ~content_type:_ _source -> called := true)
      ()
  with
  | Error (Error.Matrix_error e) ->
      Alcotest.(check bool) "callback not called" false !called;
      check_string "errcode" "M_FORBIDDEN" (Error.errcode_to_string e.errcode)
  | Error e -> Alcotest.failf "wrong stream error: %s" (Error.to_string e)
  | Ok () -> Alcotest.fail "stream HTTP error was accepted"

let test_2xx_is_ok () =
  let _, fetch = mock (fun req -> Fetch_mock.respond ~status:204 "" req) in
  let t = client_of fetch in
  check_string "empty body" "" (ok_body (Client.Http.get t ~path:"/x" ()))

let test_matrix_error () =
  let _, fetch =
    mock (fun req ->
        Fetch_mock.respond ~status:403
          {|{"errcode":"M_FORBIDDEN","error":"Bad password"}|} req)
  in
  let t = client_of fetch in
  match get_error (Client.Http.get t ~path:"/x" ()) with
  | Error.Matrix_error e ->
      check_string "errcode" "M_FORBIDDEN" (Error.errcode_to_string e.errcode);
      check_string "error" "Bad password" e.error;
      Alcotest.(check (option int)) "no retry_after_ms" None e.retry_after_ms
  | e -> Alcotest.failf "expected Matrix_error, got %s" (Error.to_string e)

let test_unknown_errcode_preserved () =
  let _, fetch =
    mock (fun req ->
        Fetch_mock.respond ~status:400
          {|{"errcode":"M_WEIRD_THING","error":"?"}|} req)
  in
  let t = client_of fetch in
  match get_error (Client.Http.get t ~path:"/x" ()) with
  | Error.Matrix_error e ->
      check_string "errcode" "M_WEIRD_THING" (Error.errcode_to_string e.errcode)
  | e -> Alcotest.failf "expected Matrix_error, got %s" (Error.to_string e)

let test_http_error_non_json () =
  let _, fetch =
    mock (fun req ->
        Fetch_mock.respond ~status:502 "<html>bad gateway</html>" req)
  in
  let t = client_of fetch in
  match get_error (Client.Http.get t ~path:"/x" ()) with
  | Error.Http_error { status; body } ->
      Alcotest.(check int) "status" 502 status;
      check_string "body" "<html>bad gateway</html>" body
  | e -> Alcotest.failf "expected Http_error, got %s" (Error.to_string e)

let test_rate_limit_retry_after () =
  let _, fetch =
    mock (fun req ->
        Fetch_mock.respond ~status:429
          {|{"errcode":"M_LIMIT_EXCEEDED","error":"Too many requests","retry_after_ms":2000}|}
          req)
  in
  let t = client_of fetch in
  match get_error (Client.Http.get t ~path:"/x" ()) with
  | Error.Matrix_error e ->
      check_string "errcode" "M_LIMIT_EXCEEDED"
        (Error.errcode_to_string e.errcode);
      Alcotest.(check (option int))
        "retry_after_ms" (Some 2000) e.retry_after_ms
  | e -> Alcotest.failf "expected Matrix_error, got %s" (Error.to_string e)

let test_soft_logout_flag () =
  let _, fetch =
    mock (fun req ->
        Fetch_mock.respond ~status:401
          {|{"errcode":"M_UNKNOWN_TOKEN","error":"gone","soft_logout":true}|}
          req)
  in
  let t = client_of fetch in
  match get_error (Client.Http.get t ~path:"/x" ()) with
  | Error.Matrix_error e ->
      Alcotest.(check (option bool)) "soft_logout" (Some true) e.soft_logout
  | e -> Alcotest.failf "expected Matrix_error, got %s" (Error.to_string e)

let unknown_token_response =
  Fetch_mock.respond ~status:401
    {|{"errcode":"M_UNKNOWN_TOKEN","error":"expired"}|}

let refresh_session = { test_session with refresh_token = Some "syr_refresh" }

let auto_client ?on_session_update
    ?(refresh =
      fun _ ->
        Ok { Client.access_token = "syt_new"; refresh_token = Some "syr_new" })
    fetch =
  Client.with_auto_refresh ?on_session_update ~refresh
    (Client.with_session (client_of fetch) refresh_session)

let test_auto_refresh_disabled () =
  let log, fetch = mock (fun req -> unknown_token_response req) in
  let t = Client.with_session (client_of fetch) refresh_session in
  ignore (get_error (Client.Http.get t ~path:"/x" ()));
  check_int "disabled makes one request" 1 (List.length (requests log))

let test_auto_refresh_get_replay () =
  let log, fetch =
    mock (fun req ->
        match
          header
            { meth = ""; url = ""; headers = req.headers; body = None }
            "authorization"
        with
        | Some "Bearer syt_secret_token" -> unknown_token_response req
        | Some "Bearer syt_new" -> Fetch_mock.respond {|{"ok":true}|} req
        | _ -> Alcotest.fail "unexpected bearer token")
  in
  let calls = ref 0 in
  let t =
    auto_client
      ~refresh:(fun session ->
        incr calls;
        check_string "refresh sees old access token" "syt_secret_token"
          session.access_token;
        Ok { Client.access_token = "syt_new"; refresh_token = Some "syr_new" })
      fetch
  in
  check_string "replayed GET body" {|{"ok":true}|}
    (ok_body (Client.Http.get t ~path:"/x" ()));
  check_int "one refresh" 1 !calls;
  check_int "initial plus replay" 2 (List.length (requests log));
  check_str_opt "session access rotated" (Some "syt_new")
    (Option.map (fun (s : Client.session) -> s.access_token) (Client.session t))

let test_auto_refresh_post_replay () =
  let log, fetch =
    mock (fun req ->
        match req.body with
        | Fetch.String {|{"same":true}|} ->
            if
              header
                { meth = ""; url = ""; headers = req.headers; body = None }
                "authorization"
              = Some "Bearer syt_secret_token"
            then unknown_token_response req
            else Fetch_mock.respond {|{"ok":true}|} req
        | _ -> Alcotest.fail "replayed POST body changed")
  in
  let t = auto_client fetch in
  ignore (ok_body (Client.Http.post t ~path:"/x" ~body:{|{"same":true}|} ()));
  match requests log with
  | [ first; second ] ->
      check_str_opt "first token" (Some "Bearer syt_secret_token")
        (header first "authorization");
      check_str_opt "second token" (Some "Bearer syt_new")
        (header second "authorization");
      check_str_opt "first body" (Some {|{"same":true}|}) first.body;
      check_str_opt "second body" (Some {|{"same":true}|}) second.body
  | rs ->
      Alcotest.failf "expected initial POST and replay, got %d" (List.length rs)

let test_auto_refresh_only_once () =
  let log, fetch = mock (fun req -> unknown_token_response req) in
  let calls = ref 0 in
  let t =
    auto_client
      ~refresh:(fun _ ->
        incr calls;
        Ok { Client.access_token = "syt_new"; refresh_token = Some "syr_new" })
      fetch
  in
  ignore (get_error (Client.Http.get t ~path:"/x" ()));
  check_int "one refresh" 1 !calls;
  check_int "no second retry" 2 (List.length (requests log))

let test_auto_refresh_missing_token () =
  let log, fetch = mock (fun req -> unknown_token_response req) in
  let calls = ref 0 in
  let t =
    Client.with_auto_refresh
      ~refresh:(fun _ ->
        incr calls;
        Ok { Client.access_token = "never"; refresh_token = None })
      (Client.with_session (client_of fetch) test_session)
  in
  ignore (get_error (Client.Http.get t ~path:"/x" ()));
  check_int "missing refresh token makes one request" 1
    (List.length (requests log));
  check_int "refresh callback not called" 0 !calls

let test_auto_refresh_retains_refresh_token () =
  let _, fetch =
    mock (fun req ->
        if
          header
            { meth = ""; url = ""; headers = req.headers; body = None }
            "authorization"
          = Some "Bearer syt_secret_token"
        then unknown_token_response req
        else Fetch_mock.respond "{}" req)
  in
  let t =
    auto_client
      ~refresh:(fun _ ->
        Ok { Client.access_token = "syt_new"; refresh_token = None })
      fetch
  in
  ignore (ok_body (Client.Http.get t ~path:"/x" ()));
  match Client.session t with
  | Some session ->
      check_str_opt "old refresh token retained" (Some "syr_refresh")
        session.refresh_token
  | None -> Alcotest.fail "automatic refresh dropped the session"

let test_auto_refresh_failure_allows_later_retry () =
  let log, fetch =
    mock (fun req ->
        if
          header
            { meth = ""; url = ""; headers = req.headers; body = None }
            "authorization"
          = Some "Bearer syt_secret_token"
        then unknown_token_response req
        else Fetch_mock.respond "{}" req)
  in
  let calls = ref 0 in
  let t =
    auto_client
      ~refresh:(fun _ ->
        incr calls;
        if !calls = 1 then Error (Error.Json_error "refresh failed")
        else
          Ok { Client.access_token = "syt_new"; refresh_token = Some "syr_new" })
      fetch
  in
  (match get_error (Client.Http.get t ~path:"/first" ()) with
  | Error.Json_error "refresh failed" -> ()
  | error -> Alcotest.failf "wrong refresh error: %s" (Error.to_string error));
  check_str_opt "failed refresh does not mutate access token"
    (Some "syt_secret_token")
    (Option.map (fun (s : Client.session) -> s.access_token) (Client.session t));
  ignore (ok_body (Client.Http.get t ~path:"/second" ()));
  check_int "later request gets another refresh" 2 !calls;
  check_int "failed request plus later retry" 3 (List.length (requests log))

let test_auto_refresh_hook_sees_atomic_session () =
  let seen = ref None in
  let log, fetch =
    mock (fun req ->
        if
          header
            { meth = ""; url = ""; headers = req.headers; body = None }
            "authorization"
          = Some "Bearer syt_secret_token"
        then unknown_token_response req
        else Fetch_mock.respond "{}" req)
  in
  let t =
    auto_client
      ~on_session_update:(fun session ->
        seen := Some session;
        Ok ())
      fetch
  in
  ignore (ok_body (Client.Http.get t ~path:"/x" ()));
  match !seen with
  | Some session ->
      check_string "hook access token" "syt_new" session.access_token;
      check_str_opt "hook refresh token" (Some "syr_new") session.refresh_token
  | None -> Alcotest.fail "session update hook was not called"

let test_auto_refresh_concurrent_single_attempt () =
  let log, fetch =
    mock (fun req ->
        if
          header
            { meth = ""; url = ""; headers = req.headers; body = None }
            "authorization"
          = Some "Bearer syt_secret_token"
        then unknown_token_response req
        else Fetch_mock.respond "{}" req)
  in
  let release, release_resolver = Eio.Promise.create () in
  let calls = ref 0 in
  let t =
    auto_client
      ~refresh:(fun _ ->
        incr calls;
        Eio.Promise.await release;
        Ok { Client.access_token = "syt_new"; refresh_token = Some "syr_new" })
      fetch
  in
  let first = ref None and second = ref None in
  Eio.Fiber.both
    (fun () -> first := Some (Client.Http.get t ~path:"/one" ()))
    (fun () ->
      Eio.Promise.resolve release_resolver ();
      second := Some (Client.Http.get t ~path:"/two" ()));
  check_int "concurrent refresh callback once" 1 !calls;
  check_int "two initial requests and two replays" 4
    (List.length (requests log));
  check_bool "first succeeded" true (Option.is_some !first);
  check_bool "second succeeded" true (Option.is_some !second)

let test_auto_refresh_unauthenticated_bypass () =
  let log, fetch = mock (fun req -> unknown_token_response req) in
  let calls = ref 0 in
  let t =
    auto_client
      ~refresh:(fun _ ->
        incr calls;
        Ok { Client.access_token = "never"; refresh_token = None })
      fetch
  in
  ignore
    (get_error
       (Client.Http.post_unauthenticated t ~path:"/login" ~body:"{}" ()));
  check_int "unauthenticated request is not retried" 1
    (List.length (requests log));
  check_int "unauthenticated request does not refresh" 0 !calls;
  check_str_opt "unauthenticated request has no bearer" None
    (header (one_request log) "authorization")

let test_auto_refresh_post_stream_no_retry () =
  let log, fetch = mock (fun req -> unknown_token_response req) in
  let calls = ref 0 in
  let t =
    auto_client
      ~refresh:(fun _ ->
        incr calls;
        Ok { Client.access_token = "never"; refresh_token = None })
      fetch
  in
  ignore
    (get_error
       (Client.Http.post_stream t ~path:"/upload" ~content_type:"text/plain"
          ~body:(Eio.Flow.string_source "one-shot")
          ()));
  check_int "stream POST is not retried" 1 (List.length (requests log));
  check_int "stream POST does not refresh" 0 !calls

let test_auto_refresh_get_stream_replay () =
  let log, fetch =
    mock (fun req ->
        if
          header
            { meth = ""; url = ""; headers = req.headers; body = None }
            "authorization"
          = Some "Bearer syt_secret_token"
        then unknown_token_response req
        else
          Fetch_mock.respond
            ~headers:(Http.Header.of_list [ ("content-type", "text/plain") ])
            "streamed" req)
  in
  let callbacks = ref 0 and received = ref "" in
  let t = auto_client fetch in
  let result =
    Client.Http.get_stream t ~path:"/_matrix/media/v3/download/a/b"
      ~on_response:(fun ~content_type body ->
        incr callbacks;
        check_str_opt "stream content type" (Some "text/plain") content_type;
        received := Eio.Buf_read.(parse_exn ~max_size:65536 take_all body))
      ()
  in
  (match result with
  | Ok () -> ()
  | Error e ->
      Alcotest.failf "stream GET refresh failed: %s" (Error.to_string e));
  check_int "initial stream GET plus replay" 2 (List.length (requests log));
  check_int "callback called only for replay" 1 !callbacks;
  check_string "replayed stream body" "streamed" !received

let test_auto_refresh_state_reset () =
  let log, fetch = mock (fun req -> unknown_token_response req) in
  let calls = ref 0 in
  let t =
    auto_client
      ~refresh:(fun _ ->
        incr calls;
        Ok { Client.access_token = "never"; refresh_token = None })
      fetch
  in
  let ordinary = Client.with_session t refresh_session in
  ignore (get_error (Client.Http.get ordinary ~path:"/ordinary" ()));
  check_int "with_session disables refresh" 0 !calls;
  let anonymous = Client.without_session t in
  check_bool "without_session removes session" false
    (Option.is_some (Client.session anonymous));
  let static = Client.with_access_token t "syt_static" in
  ignore (get_error (Client.Http.get static ~path:"/static" ()));
  check_int "with_access_token disables refresh" 0 !calls;
  match requests log with
  | [ ordinary_request; static_request ] ->
      check_str_opt "ordinary token" (Some "Bearer syt_secret_token")
        (header ordinary_request "authorization");
      check_str_opt "static token" (Some "Bearer syt_static")
        (header static_request "authorization")
  | rs -> Alcotest.failf "expected two reset requests, got %d" (List.length rs)

let ptime_seconds seconds =
  Option.get (Ptime.add_span Ptime.epoch (Ptime.Span.of_int_s seconds))

let expiry_client ?on_session_update ?(expires_at = ptime_seconds 10)
    ?(early_refresh = Ptime.Span.of_int_s 60) ?(now = fun () -> Ptime.epoch)
    ?(refresh =
      fun _ ->
        Ok
          {
            Client.refreshed_tokens =
              { access_token = "syt_expiry"; refresh_token = Some "syr_expiry" };
            expires_at = Some (ptime_seconds 1000);
          }) fetch =
  Client.with_auto_refresh_expiry ?on_session_update ~expires_at ~early_refresh
    ~now ~refresh
    (Client.with_session (client_of fetch) refresh_session)

let test_auto_refresh_expiry_post_stream_one_send () =
  let log, fetch = mock (fun req -> Fetch_mock.respond "{}" req) in
  let calls = ref 0 in
  let t =
    expiry_client
      ~refresh:(fun _ ->
        incr calls;
        Ok
          {
            Client.refreshed_tokens =
              { access_token = "unexpected"; refresh_token = None };
            expires_at = Some (ptime_seconds 1000);
          })
      fetch
  in
  ignore
    (ok_body
       (Client.Http.post_stream t ~path:"/upload" ~content_type:"text/plain"
          ~body:(Eio.Flow.string_source "one-shot")
          ()));
  check_int "stream POST does not proactively refresh" 0 !calls;
  check_int "stream POST is sent once" 1 (List.length (requests log))

let test_auto_refresh_expiry_due_before_request () =
  let log, fetch =
    mock (fun req ->
        check_str_opt "proactive bearer" (Some "Bearer syt_expiry")
          (header
             { meth = ""; url = ""; headers = req.headers; body = None }
             "authorization");
        Fetch_mock.respond "{}" req)
  in
  let seen = ref None in
  let t =
    expiry_client ~expires_at:(ptime_seconds 60)
      ~early_refresh:(Ptime.Span.of_int_s 60)
      ~on_session_update:(fun session expires_at ->
        seen := Some (session, expires_at);
        Ok ())
      fetch
  in
  ignore (ok_body (Client.Http.get t ~path:"/expiry" ()));
  check_int "one proactive refresh" 1 (List.length (requests log));
  match !seen with
  | Some (session, Some expiry) ->
      check_string "persisted access token" "syt_expiry" session.access_token;
      check_bool "persisted expiry" true
        (Ptime.equal expiry (ptime_seconds 1000))
  | _ -> Alcotest.fail "proactive persistence did not see session and expiry"

let test_auto_refresh_expiry_not_due () =
  let log, fetch = mock (fun req -> Fetch_mock.respond "{}" req) in
  let calls = ref 0 in
  let t =
    expiry_client ~expires_at:(ptime_seconds 1000)
      ~refresh:(fun _ ->
        incr calls;
        Ok
          {
            Client.refreshed_tokens =
              { access_token = "unexpected"; refresh_token = None };
            expires_at = Some (ptime_seconds 2000);
          })
      fetch
  in
  ignore (ok_body (Client.Http.get t ~path:"/expiry" ()));
  check_int "not-due request does not refresh" 0 !calls;
  check_int "not-due request is sent" 1 (List.length (requests log))

let test_auto_refresh_expiry_without_refresh_token () =
  let log, fetch = mock (fun req -> Fetch_mock.respond "{}" req) in
  let calls = ref 0 in
  let t =
    Client.with_auto_refresh_expiry ~expires_at:(ptime_seconds 1)
      ~now:(fun () -> Ptime.epoch)
      ~refresh:(fun _ ->
        incr calls;
        Alcotest.fail "refresh callback called without a refresh token")
      (Client.with_session (client_of fetch) test_session)
  in
  ignore (ok_body (Client.Http.get t ~path:"/expiry" ()));
  check_int "missing refresh token skips proactive refresh" 0 !calls;
  check_int "request still reaches server" 1 (List.length (requests log))

let test_auto_refresh_expiry_failure_retries_later () =
  let log, fetch = mock (fun req -> Fetch_mock.respond "{}" req) in
  let calls = ref 0 in
  let t =
    expiry_client
      ~refresh:(fun _ ->
        incr calls;
        if !calls = 1 then Error (Error.Json_error "temporary")
        else
          Ok
            {
              Client.refreshed_tokens =
                { access_token = "syt_retry"; refresh_token = None };
              expires_at = Some (ptime_seconds 1000);
            })
      fetch
  in
  (match Client.Http.get t ~path:"/first" () with
  | Error (Error.Json_error "temporary") -> ()
  | _ -> Alcotest.fail "failed proactive refresh should stop the request");
  ignore (ok_body (Client.Http.get t ~path:"/second" ()));
  check_int "later request retries refresh" 2 !calls;
  check_int "only successful request reaches server" 1
    (List.length (requests log))

let test_auto_refresh_expiry_concurrent_single_attempt () =
  let log, fetch = mock (fun req -> Fetch_mock.respond "{}" req) in
  let release, resolver = Eio.Promise.create () in
  let calls = ref 0 in
  let t =
    expiry_client
      ~refresh:(fun _ ->
        incr calls;
        Eio.Promise.await release;
        Ok
          {
            Client.refreshed_tokens =
              { access_token = "syt_concurrent"; refresh_token = None };
            expires_at = Some (ptime_seconds 1000);
          })
      fetch
  in
  let first = ref None and second = ref None in
  Eio.Fiber.both
    (fun () -> first := Some (Client.Http.get t ~path:"/one" ()))
    (fun () ->
      Eio.Promise.resolve resolver ();
      second := Some (Client.Http.get t ~path:"/two" ()));
  check_int "concurrent proactive refresh callback once" 1 !calls;
  check_int "both requests reach server after shared refresh" 2
    (List.length (requests log));
  check_bool "first succeeds" true (Option.is_some !first);
  check_bool "second succeeds" true (Option.is_some !second)

let test_auto_refresh_expiry_reentrant_persistence () =
  let log, fetch = mock (fun req -> Fetch_mock.respond "{}" req) in
  let client = ref None in
  let hook_calls = ref 0 in
  let refresh_calls = ref 0 in
  let t =
    Client.with_auto_refresh_expiry
      ~on_session_update:(fun _ _ ->
        incr hook_calls;
        if !hook_calls = 1 then
          ignore
            (ok_body (Client.Http.get (Option.get !client) ~path:"/hook" ()));
        Ok ())
      ~expires_at:(ptime_seconds 1)
      ~now:(fun () -> Ptime.epoch)
      ~refresh:(fun _ ->
        incr refresh_calls;
        if !refresh_calls = 1 then
          Ok
            {
              Client.refreshed_tokens =
                { access_token = "syt_reentrant_1"; refresh_token = None };
              expires_at = Some (ptime_seconds 1);
            }
        else
          Ok
            {
              Client.refreshed_tokens =
                { access_token = "syt_reentrant_2"; refresh_token = None };
              expires_at = Some (ptime_seconds 1000);
            })
      (Client.with_session (client_of fetch) refresh_session)
  in
  client := Some t;
  ignore (ok_body (Client.Http.get t ~path:"/outer" ()));
  check_int "reentrant persistence refreshes without deadlock" 2 !refresh_calls;
  check_int "reentrant persistence hook runs for each committed update" 2
    !hook_calls;
  check_int "reentrant requests are sent once each" 2
    (List.length (requests log))

let test_json_decode_error () =
  (* A 2xx body that is not the expected shape is a [Json_error], not an
     [Http_error]. *)
  let _, fetch = mock (json {|{"nope":1}|}) in
  let t = Client.with_session (client_of fetch) test_session in
  match get_error (Auth.whoami t) with
  | Error.Json_error _ -> ()
  | e -> Alcotest.failf "expected Json_error, got %s" (Error.to_string e)

let test_json_decode_depth_boundary () =
  let rec nested count value =
    if count = 0 then value else nested (count - 1) ("[" ^ value ^ "]")
  in
  (match
     Client.Http.decode_response Matrix_proto.Json.Codec.json (nested 128 "0")
   with
  | Ok _ -> ()
  | Error error ->
      Alcotest.failf "depth 128 was rejected: %s" (Error.to_string error));
  match
    Client.Http.decode_response Matrix_proto.Json.Codec.json (nested 129 "0")
  with
  | Error (Error.Json_error _) -> ()
  | Error error ->
      Alcotest.failf "depth 129 returned the wrong error: %s"
        (Error.to_string error)
  | Ok _ -> Alcotest.fail "depth 129 was accepted"

let test_json_decode_rejects_duplicate_members () =
  let codec =
    Jsont.Object.(
      map Fun.id |> mem "value" Matrix_proto.Json.Codec.int |> finish)
  in
  match Client.Http.decode_response codec {|{"value":1,"value":2}|} with
  | Error (Error.Json_error _) -> ()
  | Error error ->
      Alcotest.failf "duplicate member returned the wrong error: %s"
        (Error.to_string error)
  | Ok _ -> Alcotest.fail "duplicate typed member was accepted"

let test_json_decode_checks_unknown_members () =
  let codec =
    Jsont.Object.(
      map Fun.id |> mem "value" Matrix_proto.Json.Codec.int |> finish)
  in
  match Client.Http.decode_response codec {|{"value":1,"ignored":1e999}|} with
  | Error (Error.Json_error _) -> ()
  | Error error ->
      Alcotest.failf "invalid unknown member returned the wrong error: %s"
        (Error.to_string error)
  | Ok _ -> Alcotest.fail "non-finite number in an unknown member was accepted"

let test_required_member_missing () =
  (* These CS API result containers are required. Treating malformed successes
     as empty state would make callers act on false membership or alias data. *)
  let _, fetch = mock (json "{}") in
  let t = Client.with_session (client_of fetch) test_session in
  let expect label result =
    match get_error result with
    | Error.Json_error _ -> ()
    | e ->
        Alcotest.failf "%s: expected Json_error, got %s" label
          (Error.to_string e)
  in
  let room_id = rid "!room:example.org" in
  expect "joined_rooms" (Rooms.get_joined_rooms t);
  expect "members" (Rooms.get_members t ~room_id ());
  expect "joined_members" (Rooms.get_joined_members t ~room_id);
  expect "aliases" (Rooms.get_aliases t ~room_id)

let test_connection_failure () =
  let _, fetch =
    mock (fun _req ->
        raise (Fetch.err (Fetch.Connection_failure Eio.Net.Timeout)))
  in
  let t = client_of fetch in
  match get_error (Client.Http.get t ~path:"/x" ()) with
  | Error.Network_error _ -> ()
  | e -> Alcotest.failf "expected Network_error, got %s" (Error.to_string e)

(* [contains ~needle s] is [true] if [needle] occurs in [s]. *)
let contains ~needle s =
  let n = String.length needle and m = String.length s in
  let rec go i = i + n <= m && (String.sub s i n = needle || go (i + 1)) in
  n = 0 || go 0

(* Keep this reporter local to the redaction test: installing it around the
   real request path verifies both request and response debug logging without
   making the implementation's private redaction helper public. *)
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

let test_debug_log_redacts_nested_e2ee_secrets () =
  let secret_names =
    [
      "session_key";
      "session_data";
      "recovery_key";
      "secret";
      "secrets";
      "key";
      "keys";
      "pickle";
      "pickle_key";
      "device_pickle";
      "dehydrated_pickle";
      "dehydrated_pickle_key";
      "private_key";
      "master_key";
      "self_signing_key";
      "user_signing_key";
      "backup_key";
      "room_key";
      "seed";
      "seeds";
      "decryption_key";
      "iv";
      "mac";
    ]
  in
  let fields =
    List.mapi
      (fun index name ->
        Printf.sprintf "\"%s\":\"e2ee-secret-%02d\"" name index)
      secret_names
    |> String.concat ","
  in
  let body =
    Printf.sprintf
      "{\"visible\":\"keep-this-diagnostic\",\"outer\":[{\"nested_visible\":\"keep-this-too\",%s}]}"
      fields
  in
  let _, fetch = mock (json body) in
  let captured = Buffer.create 4096 in
  let old_reporter = Logs.reporter () and old_level = Logs.level () in
  Logs.set_reporter (buffering_reporter captured);
  Logs.set_level (Some Logs.Debug);
  Fun.protect
    ~finally:(fun () ->
      Logs.set_reporter old_reporter;
      Logs.set_level old_level)
    (fun () ->
      ignore
        (ok_body (Client.Http.post (client_of fetch) ~path:"/secrets" ~body ())));
  let logged = Buffer.contents captured in
  List.iteri
    (fun index name ->
      let marker = Printf.sprintf "e2ee-secret-%02d" index in
      check_bool
        (name ^ " never appears in the log")
        false
        (contains ~needle:marker logged))
    secret_names;
  check_bool "redaction marker is visible" true
    (contains ~needle:"<redacted>" logged);
  check_bool "non-secret request/response context is retained" true
    (contains ~needle:"keep-this-diagnostic" logged);
  check_bool "nested non-secret context is retained" true
    (contains ~needle:"keep-this-too" logged)

let test_protocol_error () =
  let _, fetch =
    mock (fun _req -> raise (Fetch.err (Fetch.Protocol_error "truncated")))
  in
  let t = client_of fetch in
  match get_error (Client.Http.get t ~path:"/x" ()) with
  | Error.Network_error msg ->
      if not (contains ~needle:"truncated" msg) then
        Alcotest.failf "message lost: %s" msg
  | e -> Alcotest.failf "expected Network_error, got %s" (Error.to_string e)

let test_transport_diagnostic_redacts_query () =
  let marker = "matrix-query-secret" in
  let fragment_marker = "matrix-fragment-secret" in
  let _, fetch =
    mock (fun _req -> raise (Fetch.err (Fetch.Protocol_error "truncated")))
  in
  let t = client_of fetch in
  (match
     get_error (Client.Http.get t ~path:"/x" ~query:[ ("opaque", marker) ] ())
   with
  | Error.Network_error msg ->
      check_bool "operation remains useful" true
        (contains ~needle:"GET https://hs.example/_matrix/client/v3/x" msg);
      check_bool "query value is absent" false (contains ~needle:marker msg);
      check_bool "typed cause remains useful" true
        (contains ~needle:"truncated" msg)
  | e -> Alcotest.failf "expected Network_error, got %s" (Error.to_string e));
  let url =
    Result.get_ok
      (Client.Url.of_string
         ("https://hs.example/x?opaque=" ^ marker ^ "#" ^ fragment_marker))
  in
  (match
     get_error (Client.Http.request_url_unauthenticated t ~meth:`GET ~url ())
   with
  | Error.Network_error msg ->
      check_bool "absolute URL query is absent" false
        (contains ~needle:marker msg);
      check_bool "absolute URL fragment is absent" false
        (contains ~needle:fragment_marker msg)
  | e -> Alcotest.failf "expected Network_error, got %s" (Error.to_string e));
  let denied_url =
    Result.get_ok
      (Client.Url.of_string
         ("https://elsewhere.example/x?opaque=" ^ marker ^ "#" ^ fragment_marker))
  in
  match
    get_error
      (Client.Http.request_url_unauthenticated t ~meth:`GET ~url:denied_url ())
  with
  | Error.Policy_denied msg ->
      check_bool "denial retains a safe operation" true
        (contains ~needle:"GET (unauth) https://elsewhere.example/x" msg);
      List.iter
        (fun secret ->
          check_bool "policy diagnostic omits URL secrets" false
            (contains ~needle:secret msg))
        [ marker; fragment_marker ]
  | e -> Alcotest.failf "expected Policy_denied, got %s" (Error.to_string e)

let test_tls_failure () =
  let _, fetch =
    mock (fun _req -> raise (Fetch.err (Fetch.Tls_failure "bad certificate")))
  in
  let t = client_of fetch in
  match get_error (Client.Http.get t ~path:"/x" ()) with
  | Error.Tls_error msg ->
      if not (contains ~needle:"bad certificate" msg) then
        Alcotest.failf "TLS reason lost: %s" msg
  | e -> Alcotest.failf "expected Tls_error, got %s" (Error.to_string e)

exception Test_cancel

let test_cancellation_propagates () =
  (* Cancelling the fiber that is mid-request must raise out of
     [Client.Http.get], never be converted to [Error.Network_error]. *)
  let raised = ref None in
  (try
     Eio.Cancel.sub (fun cc ->
         let _, fetch =
           mock (fun _req ->
               Eio.Cancel.cancel cc Test_cancel;
               Eio.Cancel.check cc;
               assert false)
         in
         let t = client_of fetch in
         match Client.Http.get t ~path:"/x" () with
         | Ok _ -> Alcotest.fail "request should not have completed"
         | Error e ->
             Alcotest.failf "cancellation was swallowed as %s"
               (Error.to_string e))
   with
  | Eio.Cancel.Cancelled e -> raised := Some e
  | Test_cancel -> raised := Some Test_cancel);
  match !raised with
  | Some Test_cancel -> ()
  | Some e -> Alcotest.failf "unexpected exception %s" (Printexc.to_string e)
  | None -> Alcotest.fail "expected Cancelled to propagate"

let test_request_timeout_success env =
  let _, fetch = mock (json "{}") in
  let client =
    Client.with_request_timeout ~mono_clock:env#mono_clock 1. (client_of fetch)
  in
  match Client.Http.get client ~path:"/_matrix/client/v3/whoami" () with
  | Ok body -> check_string "response body" "{}" body
  | Error error ->
      Alcotest.failf "request unexpectedly timed out: %s"
        (Error.to_string error)

let test_request_timeout_response env =
  let _, fetch =
    mock (fun req ->
        Eio.Time.Mono.sleep env#mono_clock 1.;
        json "{}" req)
  in
  let client =
    Client.with_request_timeout ~mono_clock:env#mono_clock 0.1 (client_of fetch)
  in
  match Client.Http.get client ~path:"/_matrix/client/v3/whoami" () with
  | Error (Error.Network_error message) ->
      check_bool "deadline message" true
        (has_substring ~needle:"timed out" message)
  | Ok _ -> Alcotest.fail "delayed response unexpectedly succeeded"
  | Error error ->
      Alcotest.failf "expected timeout, got %s" (Error.to_string error)

let test_request_timeout_stream_callback env =
  let _, fetch = mock (json "stream") in
  let client =
    Client.with_request_timeout ~mono_clock:env#mono_clock 0.1 (client_of fetch)
  in
  match
    Client.Http.get_stream client ~path:"/_matrix/media/v3/download"
      ~on_response:(fun ~content_type:_ _flow ->
        Eio.Time.Mono.sleep env#mono_clock 1.)
      ()
  with
  | Error (Error.Network_error message) ->
      check_bool "stream deadline message" true
        (has_substring ~needle:"timed out" message)
  | Ok () -> Alcotest.fail "delayed stream callback unexpectedly succeeded"
  | Error error ->
      Alcotest.failf "expected stream timeout, got %s" (Error.to_string error)

let test_request_timeout_covers_retry_backoff env =
  let attempts = ref 0 in
  let _, fetch =
    mock (fun req ->
        incr attempts;
        Fetch_mock.respond ~status:503 "unavailable" req)
  in
  let retry =
    Fetch.Retry.v ~max_retries:3 ~backoff_factor:(Duration.of_sec 1)
      ~jitter:false ()
  in
  let fetch =
    Fetch.with_retry ~clock:env#mono_clock
      ~random:(Eio.Flow.string_source "")
      ~config:retry fetch
  in
  let client =
    Client.with_request_timeout ~mono_clock:env#mono_clock 0.1 (client_of fetch)
  in
  (match Client.Http.get client ~path:"/retry" () with
  | Error (Error.Network_error message) ->
      check_bool "retry deadline message" true
        (has_substring ~needle:"timed out" message)
  | Ok _ -> Alcotest.fail "retry backoff outlived the request deadline"
  | Error error ->
      Alcotest.failf "expected retry timeout, got %s" (Error.to_string error));
  check_int "deadline expires before a second attempt" 1 !attempts

let test_request_timeout_covers_refresh env =
  let log, fetch = mock (fun req -> unknown_token_response req) in
  let refresh_calls = ref 0 in
  let client =
    Client.with_session (client_of fetch) refresh_session
    |> Client.with_auto_refresh ~refresh:(fun _ ->
        incr refresh_calls;
        Eio.Time.Mono.sleep env#mono_clock 1.;
        Ok { Client.access_token = "syt_after_deadline"; refresh_token = None })
    |> Client.with_request_timeout ~mono_clock:env#mono_clock 0.1
  in
  (match Client.Http.get client ~path:"/refresh" () with
  | Error (Error.Network_error message) ->
      check_bool "refresh deadline message" true
        (has_substring ~needle:"timed out" message)
  | Ok _ -> Alcotest.fail "refresh outlived the request deadline"
  | Error error ->
      Alcotest.failf "expected refresh timeout, got %s" (Error.to_string error));
  check_int "refresh began once" 1 !refresh_calls;
  check_int "timed-out refresh was not replayed" 1 (List.length (requests log))

let test_request_timeout_preserves_parent_cancellation env =
  let raised = ref None in
  (try
     Eio.Cancel.sub (fun cc ->
         let _, fetch =
           mock (fun _req ->
               Eio.Cancel.cancel cc Test_cancel;
               Eio.Cancel.check cc;
               assert false)
         in
         let client =
           Client.with_request_timeout ~mono_clock:env#mono_clock 10.
             (client_of fetch)
         in
         match Client.Http.get client ~path:"/cancel" () with
         | Ok _ -> Alcotest.fail "cancelled request completed"
         | Error error ->
             Alcotest.failf "parent cancellation became %s"
               (Error.to_string error))
   with
  | Eio.Cancel.Cancelled exn -> raised := Some exn
  | Test_cancel -> raised := Some Test_cancel);
  match !raised with
  | Some Test_cancel -> ()
  | Some exn ->
      Alcotest.failf "unexpected cancellation exception: %s"
        (Printexc.to_string exn)
  | None -> Alcotest.fail "parent cancellation did not propagate"

let test_auto_refresh_hook_cancellation_propagates () =
  let raised = ref None in
  (try
     Eio.Cancel.sub (fun cc ->
         let _, fetch =
           mock (fun req ->
               if
                 header
                   { meth = ""; url = ""; headers = req.headers; body = None }
                   "authorization"
                 = Some "Bearer syt_secret_token"
               then unknown_token_response req
               else Fetch_mock.respond "{}" req)
         in
         let t =
           auto_client
             ~on_session_update:(fun _ ->
               Eio.Cancel.cancel cc Test_cancel;
               Eio.Cancel.check cc;
               assert false)
             fetch
         in
         match Client.Http.get t ~path:"/x" () with
         | Ok _ -> Alcotest.fail "cancelled refresh should not complete"
         | Error e ->
             Alcotest.failf "refresh cancellation was swallowed as %s"
               (Error.to_string e))
   with
  | Eio.Cancel.Cancelled e -> raised := Some e
  | Test_cancel -> raised := Some Test_cancel);
  match !raised with
  | Some Test_cancel -> ()
  | Some e -> Alcotest.failf "unexpected exception %s" (Printexc.to_string e)
  | None -> Alcotest.fail "expected refresh cancellation to propagate"

let test_insecure_origin_with_bearer () =
  (* An http:// homeserver still gets the bearer token: [with_session]
     passes [~allow_insecure:true] for it. *)
  let log, fetch = mock (json "{}") in
  let t =
    Client.with_session
      (client_of ~homeserver:"http://hs.example:8008" fetch)
      test_session
  in
  ignore (ok_body (Client.Http.get t ~path:"/account/whoami" ()));
  let r = one_request log in
  check_string "url" "http://hs.example:8008/_matrix/client/v3/account/whoami"
    r.url;
  check_str_opt "authorization" (Some "Bearer syt_secret_token")
    (header r "authorization")

let test_cross_origin_redirect_denied () =
  (* [Fetch.restrict ~under:[origin]] is applied at [Client.create], so a
     redirect off the homeserver is refused before it is followed. *)
  let log, fetch =
    mock (fun req ->
        Fetch_mock.respond ~status:302
          ~headers:
            (Http.Header.of_list [ ("location", "https://evil.example/steal") ])
          "" req)
  in
  let t = Client.with_session (client_of fetch) test_session in
  (match get_error (Client.Http.get t ~path:"/account/whoami" ()) with
  | Error.Policy_denied reason ->
      if not (contains ~needle:"not permitted" reason) then
        Alcotest.failf "policy reason lost: %s" reason
  | e -> Alcotest.failf "expected Policy_denied, got %s" (Error.to_string e));
  (* The off-origin hop must never reach the backend. *)
  List.iter
    (fun r ->
      if not (String.starts_with ~prefix:"https://hs.example/" r.url) then
        Alcotest.failf "request escaped the origin: %s" r.url)
    (requests log)

let test_bad_homeserver_scheme () =
  Alcotest.check_raises "ftp scheme"
    (Invalid_argument
       "Matrix_client.Client.config: unsupported scheme \"ftp\" (must be http \
        or https)") (fun () ->
      let _, fetch = mock (json "{}") in
      ignore (client_of ~homeserver:"ftp://hs.example" fetch))

let test_homeserver_without_host () =
  Alcotest.check_raises "no host"
    (Invalid_argument
       "Matrix_client.Client.config: not an absolute URL (missing scheme)")
    (fun () ->
      let _, fetch = mock (json "{}") in
      ignore (client_of ~homeserver:"/relative" fetch))

let test_homeserver_url_boundary () =
  let rejects =
    [
      ("userinfo", "https://user:password@hs.example");
      ("query", "https://hs.example?tenant=one");
      ("empty query", "https://hs.example?");
      ("fragment", "https://hs.example/#client");
      ("empty fragment", "https://hs.example#");
      ("network path", "//hs.example");
      ("malformed port", "https://hs.example:not-a-port");
    ]
  in
  List.iter
    (fun (label, value) ->
      match
        try
          ignore
            (Client.config ~homeserver:(Uriz.of_string_exn value)
               ~well_known_policy:Client.Do_not_query ());
          None
        with Invalid_argument message -> Some message
      with
      | Some _ -> ()
      | None -> Alcotest.failf "%s homeserver was accepted: %s" label value)
    rejects;
  let log, fetch = mock (json "{}") in
  let client = client_of ~homeserver:"HTTPS://HS.Example:443/" fetch in
  ignore (ok_body (Client.Http.get client ~path:"/account/whoami" ()));
  check_string "canonical request origin"
    "https://hs.example/_matrix/client/v3/account/whoami" (one_request log).url;
  check_string "canonical public homeserver" "https://hs.example"
    (Uriz.to_string (Client.homeserver client));
  let prefixed_log, prefixed_fetch = mock (json "{}") in
  let prefixed =
    client_of ~homeserver:"https://hs.example/matrix/" prefixed_fetch
  in
  ignore (ok_body (Client.Http.get prefixed ~path:"/account/whoami" ()));
  check_string "base path is retained" "https://hs.example/matrix/"
    (Uriz.to_string (Client.homeserver prefixed));
  check_string "endpoint is appended beneath base path"
    "https://hs.example/matrix/_matrix/client/v3/account/whoami"
    (one_request prefixed_log).url;
  let base = Client.homeserver_url prefixed in
  List.iter
    (fun path ->
      match Client.Url.append_path base ~path () with
      | Error _ -> ()
      | Ok _ -> Alcotest.failf "endpoint path delimiter accepted: %s" path)
    [ "/room?admin=true"; "/room#fragment" ];
  check_bool "alternate IPv4 spelling shares an origin" true
    (Client.same_origin
       (client_of ~homeserver:"http://127.1" (snd (mock (json "{}"))))
       (Uriz.of_string_exn "http://2130706433:80/elsewhere"));
  let idna =
    match Client.Url.of_string "https://bücher.example:443/" with
    | Ok url -> url
    | Error reason -> Alcotest.failf "IDNA URL rejected: %s" reason
  in
  check_string "IDNA canonicalization" "https://xn--bcher-kva.example/"
    (Client.Url.to_string idna);
  List.iter
    (fun value ->
      match Client.Url.of_string value with
      | Error _ -> ()
      | Ok _ -> Alcotest.failf "invalid port accepted: %s" value)
    [ "https://hs.example:"; "https://hs.example:not-a-port" ]

let test_validated_url_preserves_query () =
  let log, fetch = mock (json "{}") in
  let client = client_of fetch in
  let url =
    match
      Client.Url.of_string
        "https://HS.EXAMPLE:443/oauth/token?scope=a&scope=b&return=x%2Fy"
    with
    | Ok url -> url
    | Error reason -> Alcotest.failf "URL rejected: %s" reason
  in
  ignore (Client.Http.request_url_unauthenticated client ~meth:`GET ~url ());
  check_string "canonical URL keeps repeated query"
    "https://hs.example/oauth/token?scope=a&scope=b&return=x%2Fy"
    (one_request log).url

let login_response =
  {|{"user_id":"@alice:example.org","access_token":"syt_abc","device_id":"ABCDEF","refresh_token":"syr_xyz"}|}

(* A decoder for the request body {!Auth.login_password} sends, so the test
   asserts the wire shape rather than a byte string. *)
type sent_identifier = { id_type : string; id_user : string option }

let sent_identifier_jsont =
  Jsont.Object.(
    map (fun id_type id_user -> { id_type; id_user })
    |> mem "type" Matrix_proto.Json.Codec.string ~enc:(fun t -> t.id_type)
    |> opt_mem "user" Matrix_proto.Json.Codec.string ~enc:(fun t -> t.id_user)
    |> finish)

type sent_login = {
  login_type : string;
  identifier : sent_identifier;
  login_password : string option;
  login_token : string option;
  login_device_id : string option;
  display_name : string option;
  request_refresh_token : bool option;
}

let sent_login_jsont =
  Jsont.Object.(
    map
      (fun
        login_type
        identifier
        login_password
        login_token
        login_device_id
        display_name
        request_refresh_token
      ->
        {
          login_type;
          identifier;
          login_password;
          login_token;
          login_device_id;
          display_name;
          request_refresh_token;
        })
    |> mem "type" Matrix_proto.Json.Codec.string ~enc:(fun t -> t.login_type)
    |> mem "identifier" sent_identifier_jsont ~enc:(fun t -> t.identifier)
    |> opt_mem "password" Matrix_proto.Json.Codec.string ~enc:(fun t ->
        t.login_password)
    |> opt_mem "token" Matrix_proto.Json.Codec.string ~enc:(fun t ->
        t.login_token)
    |> opt_mem "device_id" Matrix_proto.Json.Codec.string ~enc:(fun t ->
        t.login_device_id)
    |> opt_mem "initial_device_display_name" Matrix_proto.Json.Codec.string
         ~enc:(fun t -> t.display_name)
    |> opt_mem "refresh_token" Jsont.bool ~enc:(fun t ->
        t.request_refresh_token)
    |> finish)

let test_login_password () =
  let log, fetch = mock (json login_response) in
  let t = client_of fetch in
  let session =
    match
      Auth.login_password t ~user:"alice" ~password:"hunter2"
        ~params:
          { device_id = None; initial_device_display_name = Some "test client" }
        ()
    with
    | Ok s -> s
    | Error e -> Alcotest.failf "login failed: %s" (Error.to_string e)
  in
  let r = one_request log in
  check_string "method" "POST" r.meth;
  check_string "url" "https://hs.example/_matrix/client/v3/login" r.url;
  check_str_opt "no authorization" None (header r "authorization");
  let sent =
    match Jsont_bytesrw.decode_string sent_login_jsont (Option.get r.body) with
    | Ok v -> v
    | Error e -> Alcotest.failf "login request body: %s" e
  in
  check_string "type" "m.login.password" sent.login_type;
  check_string "identifier.type" "m.id.user" sent.identifier.id_type;
  check_str_opt "identifier.user" (Some "alice") sent.identifier.id_user;
  check_str_opt "password" (Some "hunter2") sent.login_password;
  check_str_opt "no token" None sent.login_token;
  check_str_opt "no device_id" None sent.login_device_id;
  check_str_opt "display name" (Some "test client") sent.display_name;
  Alcotest.(check (option bool))
    "refresh token request omitted by default" None sent.request_refresh_token;
  check_string "user_id" "@alice:example.org"
    (Id.User_id.to_string session.user_id);
  check_string "access_token" "syt_abc" session.access_token;
  check_string "device_id" "ABCDEF" (Id.Device_id.to_string session.device_id);
  check_str_opt "refresh_token" (Some "syr_xyz") session.refresh_token

let test_login_password_requests_refresh_token () =
  let log, fetch = mock (json login_response) in
  let t = client_of fetch in
  ignore
    (match
       Auth.login_password t ~user:"alice" ~password:"hunter2"
         ~request_refresh_token:true ()
     with
    | Ok session -> session
    | Error e -> Alcotest.failf "login failed: %s" (Error.to_string e));
  let request = one_request log in
  let sent =
    match
      Jsont_bytesrw.decode_string sent_login_jsont (Option.get request.body)
    with
    | Ok value -> value
    | Error e -> Alcotest.failf "login request body: %s" e
  in
  check_bool "refresh token requested" true
    (sent.request_refresh_token = Some true)

let test_login_password_expiry_optional () =
  let _, fetch =
    mock (fun req ->
        Fetch_mock.respond
          {|{"user_id":"@alice:example.org","access_token":"syt_abc","device_id":"ABCDEF","refresh_token":"syr_xyz","expires_in_ms":60000}|}
          req)
  in
  match
    Auth.login_password_with_expiry (client_of fetch) ~user:"alice"
      ~password:"hunter2" ~request_refresh_token:true ()
  with
  | Error e -> Alcotest.failf "login failed: %s" (Error.to_string e)
  | Ok { Auth.session; expires_at } ->
      check_string "access token" "syt_abc" session.access_token;
      check_bool "optional login expiry is decoded" true
        (Option.is_some expires_at)

let test_refresh_token_expiry_optional () =
  let _, fetch =
    mock (fun req ->
        Fetch_mock.respond
          {|{"access_token":"new","refresh_token":"next","expires_in_ms":60000}|}
          req)
  in
  let result =
    Auth.refresh_token_with_expiry (client_of fetch) ~refresh_token:"old"
  in
  (match result with
  | Error e -> Alcotest.failf "refresh decode failed: %s" (Error.to_string e)
  | Ok { Auth.refreshed = { access_token; refresh_token }; expires_at } ->
      check_string "refreshed access token" "new" access_token;
      check_str_opt "refreshed refresh token" (Some "next") refresh_token;
      check_bool "optional expiry is decoded" true (Option.is_some expires_at));
  let _, old_fetch =
    mock (fun req ->
        Fetch_mock.respond
          {|{"access_token":"new","refresh_token":"next","expires_in_ms":60000}|}
          req)
  in
  match Auth.refresh_token (client_of old_fetch) ~refresh_token:"old" with
  | Error e ->
      Alcotest.failf "legacy refresh API rejected optional expiry: %s"
        (Error.to_string e)
  | Ok { access_token; refresh_token } ->
      check_string "legacy refresh access token" "new" access_token;
      check_str_opt "legacy refresh token" (Some "next") refresh_token

let test_login_token_requests_refresh_token () =
  let log, fetch = mock (json login_response) in
  let t = client_of fetch in
  ignore
    (match
       Auth.login_token t ~token:"login-token" ~request_refresh_token:true ()
     with
    | Ok session -> session
    | Error e -> Alcotest.failf "login failed: %s" (Error.to_string e));
  let request = one_request log in
  let sent =
    match
      Jsont_bytesrw.decode_string sent_login_jsont (Option.get request.body)
    with
    | Ok value -> value
    | Error e -> Alcotest.failf "login request body: %s" e
  in
  check_string "login type" "m.login.token" sent.login_type;
  check_bool "refresh token requested" true
    (sent.request_refresh_token = Some true)

let test_login_failure () =
  let _, fetch =
    mock (fun req ->
        Fetch_mock.respond ~status:403
          {|{"errcode":"M_FORBIDDEN","error":"Invalid password"}|} req)
  in
  let t = client_of fetch in
  match Auth.login_password t ~user:"alice" ~password:"wrong" () with
  | Ok _ -> Alcotest.fail "login should have failed"
  | Error (Error.Matrix_error e) ->
      check_string "errcode" "M_FORBIDDEN" (Error.errcode_to_string e.errcode)
  | Error e -> Alcotest.failf "unexpected error %s" (Error.to_string e)

let test_login_flows () =
  let log, fetch =
    mock
      (json
         {|{"flows":[{"type":"m.login.password"},{"type":"m.login.sso"},{"type":"org.example.custom"}]}|})
  in
  let t = client_of fetch in
  let flows =
    match Auth.get_login_flows t with
    | Ok f -> f
    | Error e -> Alcotest.failf "flows failed: %s" (Error.to_string e)
  in
  check_string "url" "https://hs.example/_matrix/client/v3/login"
    (one_request log).url;
  Alcotest.(check int) "count" 3 (List.length flows);
  match flows with
  | [ Auth.Password; Auth.Sso; Auth.Unknown "org.example.custom" ] -> ()
  | _ -> Alcotest.fail "unexpected flow list"

let test_whoami () =
  let log, fetch = mock (json {|{"user_id":"@bob:example.org"}|}) in
  let t = Client.with_session (client_of fetch) test_session in
  let id =
    match Auth.whoami t with
    | Ok id -> id
    | Error e -> Alcotest.failf "whoami failed: %s" (Error.to_string e)
  in
  check_string "url" "https://hs.example/_matrix/client/v3/account/whoami"
    (one_request log).url;
  check_string "user_id" "@bob:example.org" (Id.User_id.to_string id)

let test_rooms_join () =
  let log, fetch = mock (json {|{"room_id":"!abc:example.org"}|}) in
  let t = Client.with_session (client_of fetch) test_session in
  let room =
    match
      Rooms.join t
        ~room_id_or_alias:
          (`Room_alias
             (Result.get_ok (Id.Room_alias.of_string "#lobby:example.org")))
        ~via:[ "example.org" ] ~reason:"invited" ()
    with
    | Ok r -> r
    | Error e -> Alcotest.failf "join failed: %s" (Error.to_string e)
  in
  let r = one_request log in
  check_string "method" "POST" r.meth;
  (* Segment encoding escapes [#] while retaining the legal [:] character. *)
  check_string "url"
    "https://hs.example/_matrix/client/v3/join/%23lobby:example.org?server_name=example.org"
    r.url;
  check_str_opt "body" (Some {|{"reason":"invited"}|}) r.body;
  check_string "room_id" "!abc:example.org" (Id.Room_id.to_string room)

let test_route_matrix_path_characters () =
  let route = Route.v "/rooms/{room_id}/state/{event_type}/{state_key}" in
  check_string "Matrix identifiers retain allowed reserved characters"
    "/rooms/!room:example.org/state/m.room%2Fname/"
    (Route.expand_exn route
       [
         ("room_id", "!room:example.org");
         ("event_type", "m.room/name");
         ("state_key", "");
       ]);
  check_string "data delimiters cannot change the route"
    "/rooms/!%23:@/state/a%3Fb/%252F%20%F0%9F%98%80"
    (Route.expand_exn route
       [ ("room_id", "!#:@"); ("event_type", "a?b"); ("state_key", "%2F 😀") ])

let test_route_binding_validation () =
  let route = Route.v "/rooms/{room_id}/event/{event_id}" in
  let expect_error label bindings fragment =
    match Route.expand route bindings with
    | Ok path -> Alcotest.failf "%s: unexpectedly expanded to %s" label path
    | Error message ->
        if not (String.starts_with ~prefix:fragment message) then
          Alcotest.failf "%s: unexpected error %S" label message
  in
  expect_error "missing"
    [ ("room_id", "!room:example.org") ]
    "missing route binding";
  expect_error "duplicate"
    [
      ("room_id", "!room:example.org");
      ("event_id", "$one");
      ("event_id", "$two");
    ]
    "duplicate route binding";
  expect_error "unknown"
    [
      ("room_id", "!room:example.org"); ("event_id", "$one"); ("other", "value");
    ]
    "unknown route binding";
  expect_error "invalid UTF-8"
    [ ("room_id", String.make 1 (Char.chr 255)); ("event_id", "$one") ]
    "route binding"

let test_route_template_validation () =
  let rejects source =
    match Route.v source with
    | _ -> Alcotest.failf "accepted invalid route template %S" source
    | exception Invalid_argument _ -> ()
  in
  List.iter rejects
    [
      "rooms/{room_id}";
      "/rooms/{room_id}?via={server}";
      "/rooms/{room_id}#fragment";
      "/rooms/{+room_id}";
      "/rooms/{room_id,event_id}";
      "/rooms/" ^ String.make 1 (Char.chr 255);
    ]

let test_route_base_prefix_and_repeated_query () =
  let base =
    Result.get_ok (Client.Url.homeserver_string "https://hs.example/prefix/")
  in
  let path =
    Route.expand_exn
      (Route.v "/rooms/{room_id}")
      [ ("room_id", "!room:example.org") ]
  in
  let url =
    Result.get_ok
      (Client.Url.append_path base ~path
         ~query:[ ("via", "one.example"); ("via", "two.example") ]
         ())
  in
  check_string "base prefix and repeated query order"
    "https://hs.example/prefix/rooms/!room:example.org?via=one.example&via=two.example"
    (Client.Url.to_string url)

let test_query_delimiters () =
  let log, fetch = mock (json "{}") in
  let t = client_of fetch in
  let params = [ ("a,b=c&d", "x,y=z&+ %2F"); ("via", "one"); ("via", "two") ] in
  ignore (ok_body (Client.Http.get t ~path:"/rooms" ~query:params ()));
  let uri = Uriz.of_string_exn (one_request log).url in
  Alcotest.(check (list (pair string (option string))))
    "query bindings survive request construction"
    (List.map (fun (key, value) -> (key, Some value)) params)
    (Uriz.query_params ~plus_as_space:true uri);
  check_string "query delimiters encoded independently"
    "a%2Cb%3Dc%26d=x%2Cy%3Dz%26%2B%20%252F&via=one&via=two"
    (match Uriz.query uri with
    | This query -> query
    | Null -> Alcotest.fail "request has no query")

let test_messages_send_text () =
  let log, fetch = mock (json {|{"event_id":"$evt1:example.org"}|}) in
  let t = Client.with_session (client_of fetch) test_session in
  let event =
    match
      Messages.send_text t ~room_id:(rid "!room:example.org") ~body:"hello" ()
    with
    | Ok e -> e
    | Error e -> Alcotest.failf "send failed: %s" (Error.to_string e)
  in
  let r = one_request log in
  check_string "method" "PUT" r.meth;
  let prefix =
    "https://hs.example/_matrix/client/v3/rooms/!room:example.org/send/m.room.message/"
  in
  if not (String.starts_with ~prefix r.url) then
    Alcotest.failf "unexpected url %s" r.url;
  let txn =
    String.sub r.url (String.length prefix)
      (String.length r.url - String.length prefix)
  in
  if txn = "" then Alcotest.fail "no transaction id segment in the URL";
  check_str_opt "body" (Some {|{"msgtype":"m.text","body":"hello"}|}) r.body;
  check_string "event_id" "$evt1:example.org" (Id.Event_id.to_string event)

let test_send_text_txn_ids_differ () =
  let log, fetch = mock (json {|{"event_id":"$e:example.org"}|}) in
  let t = Client.with_session (client_of fetch) test_session in
  let room = rid "!room:example.org" in
  ignore (Messages.send_text t ~room_id:room ~body:"one" ());
  ignore (Messages.send_text t ~room_id:room ~body:"two" ());
  match requests log with
  | [ a; b ] ->
      if String.equal a.url b.url then
        Alcotest.fail "two sends reused the same transaction id"
  | rs -> Alcotest.failf "expected 2 requests, got %d" (List.length rs)

let test_txn_id_is_path_safe () =
  let random =
    Matrix_client.Random.of_source
      (Eio.Flow.string_source (String.make 16 '\xff'))
  in
  let txn_id = Matrix_client.Random.txn_id random in
  check_string "no path escaping needed" txn_id
    (Uriz.pct_encode ~component:`Segment txn_id);
  if String.contains txn_id '=' then
    Alcotest.fail "transaction id contains base64 padding"

let test_get_joined_rooms () =
  let log, fetch =
    mock (json {|{"joined_rooms":["!a:example.org","!b:example.org"]}|})
  in
  let t = Client.with_session (client_of fetch) test_session in
  let rooms =
    match Rooms.get_joined_rooms t with
    | Ok r -> r
    | Error e -> Alcotest.failf "joined_rooms failed: %s" (Error.to_string e)
  in
  check_string "url" "https://hs.example/_matrix/client/v3/joined_rooms"
    (one_request log).url;
  Alcotest.(check (list string))
    "rooms"
    [ "!a:example.org"; "!b:example.org" ]
    (List.map Id.Room_id.to_string rooms)

let test_dehydrated_device_support_probe () =
  let path =
    "https://hs.example/_matrix/client/unstable/org.matrix.msc3814.v1/dehydrated_device"
  in
  let check_case name ~status ~body expected =
    let log, fetch = mock (fun req -> Fetch_mock.respond ~status body req) in
    let client = Client.with_session (client_of fetch) test_session in
    (match Dehydrated_device.is_supported client with
    | Ok value -> check_bool name expected value
    | Error error ->
        Alcotest.failf "%s returned an error: %s" name (Error.to_string error));
    let request = one_request log in
    check_string (name ^ " method") "GET" request.meth;
    check_string (name ^ " URL") path request.url
  in
  check_case "successful probe" ~status:200 ~body:"{}" true;
  check_case "missing device probe" ~status:404
    ~body:{|{"errcode":"M_NOT_FOUND","error":"no device"}|} true;
  check_case "unrecognized probe" ~status:404
    ~body:{|{"errcode":"M_UNRECOGNIZED","error":"unknown endpoint"}|} false;
  let log, fetch =
    mock (fun req ->
        Fetch_mock.respond ~status:403
          {|{"errcode":"M_FORBIDDEN","error":"denied"}|} req)
  in
  let client = Client.with_session (client_of fetch) test_session in
  (match Dehydrated_device.is_supported client with
  | Error error -> (
      match Error.errcode error with
      | Some Error.M_FORBIDDEN -> ()
      | _ ->
          Alcotest.failf "unexpected propagated error: %s"
            (Error.to_string error))
  | Ok _ -> Alcotest.fail "forbidden probe unexpectedly succeeded");
  let request = one_request log in
  check_string "propagated error method" "GET" request.meth;
  check_string "propagated error URL" path request.url

let test_thread_subscriptions_transport () =
  let room = rid "!room:example.org" in
  let root = Result.get_ok (Id.Event_id.of_string "$thread/segment") in
  let automatic_event =
    Result.get_ok (Id.Event_id.of_string "$cause/segment")
  in
  let current_url =
    "https://hs.example/_matrix/client/unstable/io.element.msc4306/rooms/!room:example.org/thread/$thread%2Fsegment/subscription"
  in
  let changes_path =
    "/_matrix/client/unstable/io.element.msc4308/thread_subscriptions"
  in
  let log, fetch =
    mock (fun req ->
        let path =
          Uriz.path
            (Uriz.of_string_exn (Fetch.Middleware.Url.to_string req.url))
        in
        match (Http.Method.to_string req.meth, path) with
        | "GET", p
          when String.starts_with
                 ~prefix:
                   "/_matrix/client/unstable/io.element.msc4306/rooms/!room:example.org/thread/"
                 p ->
            Fetch_mock.respond {|{"automatic":true,"unknown":42}|} req
        | "PUT", p
          when String.starts_with
                 ~prefix:
                   "/_matrix/client/unstable/io.element.msc4306/rooms/!room:example.org/thread/"
                 p ->
            Fetch_mock.respond "{}" req
        | "DELETE", p
          when String.starts_with
                 ~prefix:
                   "/_matrix/client/unstable/io.element.msc4306/rooms/!room:example.org/thread/"
                 p ->
            Fetch_mock.respond "{}" req
        | "GET", p when String.equal p changes_path ->
            Fetch_mock.respond
              {|{"subscribed":{"!room:example.org":{"$thread/segment":{"automatic":true,"bump_stamp":42}}},"unsubscribed":{"!other:example.org":{"$other/segment":{"bump_stamp":43}}},"end":"next","unknown":true}|}
              req
        | _ -> Fetch_mock.respond ~status:404 "{}" req)
  in
  let t = Client.with_session (client_of fetch) test_session in
  let current =
    match Thread_subscriptions.get t ~room_id:room ~thread_root:root with
    | Ok (Some status) -> status
    | Ok None -> Alcotest.fail "expected current thread subscription"
    | Error e ->
        Alcotest.failf "get thread subscription failed: %s" (Error.to_string e)
  in
  check_bool "current subscription is automatic" true current.automatic;
  (match
     Thread_subscriptions.subscribe t ~room_id:room ~thread_root:root
       ~automatic:automatic_event ()
   with
  | Ok () -> ()
  | Error e -> Alcotest.failf "subscribe thread failed: %s" (Error.to_string e));
  (match
     Thread_subscriptions.unsubscribe t ~room_id:room ~thread_root:root ()
   with
  | Ok () -> ()
  | Error e ->
      Alcotest.failf "unsubscribe thread failed: %s" (Error.to_string e));
  let page =
    match
      Thread_subscriptions.changes t ~from:"from token" ~to_:"to/token" ~limit:7
        ()
    with
    | Ok page -> page
    | Error e ->
        Alcotest.failf "thread subscription changes failed: %s"
          (Error.to_string e)
  in
  let requests = requests log in
  match requests with
  | [ get; put; delete; changes ] -> (
      check_string "current status method" "GET" get.meth;
      check_string "current status path" current_url get.url;
      check_string "subscribe method" "PUT" put.meth;
      check_string "subscribe path" current_url put.url;
      check_str_opt "automatic subscribe body"
        (Some {|{"automatic":"$cause/segment"}|}) put.body;
      check_string "unsubscribe method" "DELETE" delete.meth;
      check_str_opt "unsubscribe body" None delete.body;
      check_string "changes method" "GET" changes.meth;
      check_string "changes URL"
        ("https://hs.example" ^ changes_path
       ^ "?dir=b&from=from%20token&to=to/token&limit=7")
        changes.url;
      check_string "next token" "next" (Option.get page.end_token);
      (match page.subscribed with
      | [ item ] ->
          check_string "subscribed room" "!room:example.org"
            (Id.Room_id.to_string item.room_id);
          check_string "subscribed root" "$thread/segment"
            (Id.Event_id.to_string item.thread_root);
          check_bool "subscription automatic" true item.automatic;
          Alcotest.(check int64) "subscription bump stamp" 42L item.bump_stamp
      | _ -> Alcotest.fail "expected one subscription change");
      match page.unsubscribed with
      | [ item ] ->
          check_string "unsubscribed root" "$other/segment"
            (Id.Event_id.to_string item.thread_root);
          Alcotest.(check int64) "unsubscription bump stamp" 43L item.bump_stamp
      | _ -> Alcotest.fail "expected one unsubscription change")
  | rs ->
      Alcotest.failf "expected four thread subscription requests, got %d"
        (List.length rs)

let test_thread_subscriptions_reject_bad_changes () =
  let room = rid "!room:example.org" in
  let root = Result.get_ok (Id.Event_id.of_string "$thread") in
  let limit_log, limit_fetch = mock (fun req -> Fetch_mock.respond "{}" req) in
  Alcotest.check_raises "oversized limit is rejected"
    (Invalid_argument
       "Matrix_client.Thread_subscriptions.changes: limit is not a \
        non-negative JavaScript-safe integer") (fun () ->
      ignore
        (Thread_subscriptions.changes
           (Client.with_session (client_of limit_fetch) test_session)
           ~limit:9007199254740992 ()));
  Alcotest.(check int)
    "oversized limit made no request" 0
    (List.length (requests limit_log));
  let log, fetch =
    mock (fun req ->
        Fetch_mock.respond
          {|{"subscribed":{"!room:example.org":{"$thread":{"automatic":true,"bump_stamp":-1}}}}|}
          req)
  in
  let t = Client.with_session (client_of fetch) test_session in
  (match Thread_subscriptions.changes t () with
  | Error (Error.Json_error _) -> ()
  | Error error ->
      Alcotest.failf "bad bump stamp had wrong error: %s"
        (Error.to_string error)
  | Ok _ -> Alcotest.fail "negative bump stamp was accepted");
  Alcotest.(check int)
    "bad changes made one request" 1
    (List.length (requests log));
  let too_large_log, too_large_fetch =
    mock (fun req ->
        Fetch_mock.respond
          {|{"subscribed":{"!room:example.org":{"$thread":{"automatic":true,"bump_stamp":9007199254740992}}}}|}
          req)
  in
  let too_large =
    Client.with_session (client_of too_large_fetch) test_session
  in
  (match Thread_subscriptions.changes too_large () with
  | Error (Error.Json_error _) -> ()
  | Error error ->
      Alcotest.failf "large bump stamp had wrong error: %s"
        (Error.to_string error)
  | Ok _ -> Alcotest.fail "large bump stamp was accepted");
  Alcotest.(check int)
    "large stamp made one request" 1
    (List.length (requests too_large_log));
  let bad_id_log, bad_id_fetch =
    mock (fun req ->
        Fetch_mock.respond
          {|{"subscribed":{"not-a-room":{"$thread":{"automatic":true,"bump_stamp":1}}}}|}
          req)
  in
  let bad_id = Client.with_session (client_of bad_id_fetch) test_session in
  (match Thread_subscriptions.changes bad_id () with
  | Error (Error.Json_error _) -> ()
  | Error error ->
      Alcotest.failf "malformed room id had wrong error: %s"
        (Error.to_string error)
  | Ok _ -> Alcotest.fail "malformed room id was accepted");
  Alcotest.(check int)
    "malformed id made one request" 1
    (List.length (requests bad_id_log));
  let matrix_missing_log, matrix_missing_fetch =
    mock (fun req ->
        Fetch_mock.respond ~status:404
          {|{"errcode":"M_NOT_FOUND","error":"missing"}|} req)
  in
  let matrix_missing =
    Client.with_session (client_of matrix_missing_fetch) test_session
  in
  (match
     Thread_subscriptions.get matrix_missing ~room_id:room ~thread_root:root
   with
  | Ok None -> ()
  | Ok (Some _) -> Alcotest.fail "M_NOT_FOUND returned a subscription"
  | Error error ->
      Alcotest.failf "M_NOT_FOUND was not treated as missing: %s"
        (Error.to_string error));
  Alcotest.(check int)
    "M_NOT_FOUND made one request" 1
    (List.length (requests matrix_missing_log));
  let missing_log, missing_fetch =
    mock (fun req -> Fetch_mock.respond ~status:404 "not json" req)
  in
  let missing = Client.with_session (client_of missing_fetch) test_session in
  (match Thread_subscriptions.get missing ~room_id:room ~thread_root:root with
  | Error (Error.Http_error { status = 404; _ }) -> ()
  | Ok None -> Alcotest.fail "bare HTTP 404 was treated as missing"
  | Ok (Some _) -> Alcotest.fail "404 returned a subscription"
  | Error error ->
      Alcotest.failf "bare HTTP 404 had wrong error: %s" (Error.to_string error));
  Alcotest.(check int)
    "missing status made one request" 1
    (List.length (requests missing_log))

let test_thread_subscriptions_support () =
  let make advertised =
    let fetch =
      Fetch_mock.client (fun req -> Fetch_mock.respond advertised req)
    in
    Client.with_session (client_of fetch) test_session
  in
  let enabled =
    make {|{"versions":[],"unstable_features":{"org.matrix.msc4306":true}}|}
  in
  let disabled =
    make {|{"versions":[],"unstable_features":{"org.matrix.msc4306":false}}|}
  in
  (match Thread_subscriptions.is_supported enabled with
  | Ok true -> ()
  | Ok false -> Alcotest.fail "enabled MSC4306 was reported unsupported"
  | Error error ->
      Alcotest.failf "support query failed: %s" (Error.to_string error));
  match Thread_subscriptions.is_supported disabled with
  | Ok false -> ()
  | Ok true -> Alcotest.fail "disabled MSC4306 was reported supported"
  | Error error ->
      Alcotest.failf "support query failed: %s" (Error.to_string error)

let test_thread_subscriptions_persistence_and_catchup () =
  let room = rid "!room:example.org" in
  let root = Result.get_ok (Id.Event_id.of_string "$thread") in
  let store = Store.memory () in
  let persisted status bump_stamp =
    { Thread_subscriptions.status; bump_stamp }
  in
  let upsert status bump_stamp =
    match
      Thread_subscriptions.upsert store ~room_id:room ~thread_root:root
        (persisted status bump_stamp)
    with
    | Ok () -> ()
    | Error error ->
        Alcotest.failf "store upsert failed: %s" (Error.to_string error)
  in
  upsert Thread_subscriptions.Automatic (Some 10L);
  upsert Thread_subscriptions.Manual None;
  (match
     Thread_subscriptions.find_stored store ~room_id:room ~thread_root:root
   with
  | Ok (Some { status = Thread_subscriptions.Manual; bump_stamp = Some 10L }) ->
      ()
  | Ok _ -> Alcotest.fail "unstamped local acknowledgement did not retain stamp"
  | Error error ->
      Alcotest.failf "store lookup failed: %s" (Error.to_string error));
  upsert Thread_subscriptions.Unsubscribed (Some 9L);
  (match
     Thread_subscriptions.find_stored store ~room_id:room ~thread_root:root
   with
  | Ok (Some { status = Thread_subscriptions.Manual; bump_stamp = Some 10L }) ->
      ()
  | Ok _ -> Alcotest.fail "older stamped update replaced current state"
  | Error error ->
      Alcotest.failf "store lookup failed: %s" (Error.to_string error));
  let log, fetch =
    mock (fun req ->
        let uri = Uriz.of_string_exn (Fetch.Middleware.Url.to_string req.url) in
        match Uriz.find_query ~plus_as_space:true uri "from" with
        | This "new" ->
            Fetch_mock.respond
              {|{"subscribed":{"!room:example.org":{"$thread":{"automatic":true,"bump_stamp":20}}}}|}
              req
        | This "old" ->
            Fetch_mock.respond
              {|{"unsubscribed":{"!room:example.org":{"$thread":{"bump_stamp":19}}}}|}
              req
        | _ -> Fetch_mock.respond ~status:400 "{}" req)
  in
  let client = Client.with_session (client_of fetch) test_session in
  let queue from_ to_ =
    match Thread_subscriptions.queue_catchup_token store ~from_ ~to_ with
    | Ok () -> ()
    | Error error ->
        Alcotest.failf "queue token failed: %s" (Error.to_string error)
  in
  queue "old" None;
  queue "new" (Some "pos");
  (match Thread_subscriptions.catch_up client ~store with
  | Ok () -> ()
  | Error error -> Alcotest.failf "catch-up failed: %s" (Error.to_string error));
  (match Thread_subscriptions.catchup_tokens store with
  | Ok [] -> ()
  | Ok _ -> Alcotest.fail "catch-up tokens were not drained"
  | Error error ->
      Alcotest.failf "token load failed: %s" (Error.to_string error));
  (match
     Thread_subscriptions.find_stored store ~room_id:room ~thread_root:root
   with
  | Ok (Some { status = Thread_subscriptions.Automatic; bump_stamp = Some 20L })
    ->
      ()
  | Ok _ -> Alcotest.fail "newest catch-up update did not win"
  | Error error ->
      Alcotest.failf "store lookup failed: %s" (Error.to_string error));
  (match requests log with
  | [ newest; oldest ] ->
      check_string "newest catch-up first"
        "https://hs.example/_matrix/client/unstable/io.element.msc4308/thread_subscriptions?dir=b&from=new&to=pos"
        newest.url;
      check_string "older catch-up second"
        "https://hs.example/_matrix/client/unstable/io.element.msc4308/thread_subscriptions?dir=b&from=old"
        oldest.url
  | rs ->
      Alcotest.failf "expected two catch-up requests, got %d" (List.length rs));
  (match Thread_subscriptions.remove_room store ~room_id:room with
  | Ok () -> ()
  | Error error ->
      Alcotest.failf "room cleanup failed: %s" (Error.to_string error));
  (match
     Thread_subscriptions.find_stored store ~room_id:room ~thread_root:root
   with
  | Ok None -> ()
  | Ok (Some _) -> Alcotest.fail "room cleanup retained subscription"
  | Error error ->
      Alcotest.failf "post-cleanup lookup failed: %s" (Error.to_string error));
  let ack_store = Store.memory () in
  let _, ack_fetch = mock (json "{}") in
  let ack_client = Client.with_session (client_of ack_fetch) test_session in
  (match
     Thread_subscriptions.subscribe_and_store ack_client ~store:ack_store
       ~room_id:room ~thread_root:root ()
   with
  | Ok () -> ()
  | Error error ->
      Alcotest.failf "subscribe acknowledgement failed: %s"
        (Error.to_string error));
  (match
     Thread_subscriptions.find_stored ack_store ~room_id:room ~thread_root:root
   with
  | Ok (Some { status = Thread_subscriptions.Manual; bump_stamp = None }) -> ()
  | Ok _ -> Alcotest.fail "manual acknowledgement was not persisted"
  | Error error ->
      Alcotest.failf "ack lookup failed: %s" (Error.to_string error));
  (match
     Thread_subscriptions.unsubscribe_and_store ack_client ~store:ack_store
       ~room_id:room ~thread_root:root ()
   with
  | Ok () -> ()
  | Error error ->
      Alcotest.failf "unsubscribe acknowledgement failed: %s"
        (Error.to_string error));
  match
    Thread_subscriptions.find_stored ack_store ~room_id:room ~thread_root:root
  with
  | Ok (Some { status = Thread_subscriptions.Unsubscribed; bump_stamp = None })
    ->
      ()
  | Ok _ -> Alcotest.fail "unsubscribe acknowledgement was not persisted"
  | Error error ->
      Alcotest.failf "ack lookup failed: %s" (Error.to_string error)

let test_thread_subscriptions_load_or_fetch_and_if_needed () =
  let room = rid "!room:example.org" in
  let root = Result.get_ok (Id.Event_id.of_string "$thread") in
  let stored status bump_stamp = { Thread_subscriptions.status; bump_stamp } in
  let store = Store.memory () in
  let put status bump_stamp =
    match
      Thread_subscriptions.upsert store ~room_id:room ~thread_root:root
        (stored status bump_stamp)
    with
    | Ok () -> ()
    | Error error ->
        Alcotest.failf "setup upsert failed: %s" (Error.to_string error)
  in
  put Thread_subscriptions.Manual None;
  let log, fetch = mock (json "{}") in
  let client = Client.with_session (client_of fetch) test_session in
  (match
     Thread_subscriptions.subscribe_if_needed client ~store ~room_id:room
       ~thread_root:root ()
   with
  | Ok () -> ()
  | Error error ->
      Alcotest.failf "duplicate manual request failed: %s"
        (Error.to_string error));
  (match
     Thread_subscriptions.subscribe_if_needed client ~store ~room_id:room
       ~thread_root:root ~automatic:root ()
   with
  | Ok () -> ()
  | Error error ->
      Alcotest.failf "duplicate automatic request failed: %s"
        (Error.to_string error));
  check_int "duplicate persisted requests" 0 (List.length (requests log));
  put Thread_subscriptions.Automatic (Some 10L);
  (match
     Thread_subscriptions.subscribe_if_needed client ~store ~room_id:room
       ~thread_root:root ()
   with
  | Ok () -> ()
  | Error error ->
      Alcotest.failf "manual upgrade failed: %s" (Error.to_string error));
  check_int "manual upgrade request" 1 (List.length (requests log));
  (match
     Thread_subscriptions.find_stored store ~room_id:room ~thread_root:root
   with
  | Ok (Some { status = Thread_subscriptions.Manual; bump_stamp = Some 10L }) ->
      ()
  | Ok _ -> Alcotest.fail "manual upgrade was not persisted"
  | Error error ->
      Alcotest.failf "upgrade lookup failed: %s" (Error.to_string error));
  let stale_store = Store.memory () in
  (match
     Thread_subscriptions.queue_catchup_token stale_store ~from_:"batch"
       ~to_:None
   with
  | Ok () -> ()
  | Error error ->
      Alcotest.failf "stale token setup failed: %s" (Error.to_string error));
  let stale_log, stale_fetch =
    mock (fun req -> Fetch_mock.respond {|{"automatic":true,"unknown":1}|} req)
  in
  let stale_client = Client.with_session (client_of stale_fetch) test_session in
  (match
     Thread_subscriptions.load_or_fetch stale_client ~store:stale_store
       ~room_id:room ~thread_root:root
   with
  | Ok (Some { automatic = true }) -> ()
  | Ok _ -> Alcotest.fail "stale load_or_fetch returned wrong status"
  | Error error ->
      Alcotest.failf "stale load_or_fetch failed: %s" (Error.to_string error));
  check_int "stale load_or_fetch fetched" 1 (List.length (requests stale_log));
  let conflict_store = Store.memory () in
  let conflict_subscription =
    stored Thread_subscriptions.Unsubscribed (Some 20L)
  in
  (match
     Thread_subscriptions.upsert conflict_store ~room_id:room ~thread_root:root
       conflict_subscription
   with
  | Ok () -> ()
  | Error error ->
      Alcotest.failf "conflict setup failed: %s" (Error.to_string error));
  let conflict_log, conflict_fetch =
    mock (fun req ->
        Fetch_mock.respond ~status:409
          {|{"errcode":"IO.ELEMENT.MSC4306.M_CONFLICTING_UNSUBSCRIPTION","error":"stale automatic request"}|}
          req)
  in
  let conflict_client =
    Client.with_session (client_of conflict_fetch) test_session
  in
  (match
     Thread_subscriptions.subscribe_if_needed conflict_client
       ~store:conflict_store ~room_id:room ~thread_root:root ~automatic:root ()
   with
  | Ok () -> ()
  | Error error ->
      Alcotest.failf "conflicting automatic request failed: %s"
        (Error.to_string error));
  check_int "conflicting automatic request sent" 1
    (List.length (requests conflict_log));
  match
    Thread_subscriptions.find_stored conflict_store ~room_id:room
      ~thread_root:root
  with
  | Ok
      (Some
         { status = Thread_subscriptions.Unsubscribed; bump_stamp = Some 20L })
    ->
      ()
  | Ok _ -> Alcotest.fail "conflicting automatic request overwrote newer state"
  | Error error ->
      Alcotest.failf "conflict lookup failed: %s" (Error.to_string error)

let test_media_upload () =
  let log, fetch = mock (json {|{"content_uri":"mxc://hs.example/AbCdEf"}|}) in
  let t = Client.with_session (client_of fetch) test_session in
  let data = "\x89PNG\r\n\x1a\n binary \x00 bytes" in
  let uri =
    match
      Media.upload t ~content_type:"image/png" ~data ~filename:"cat pic.png" ()
    with
    | Ok u -> u
    | Error e -> Alcotest.failf "upload failed: %s" (Error.to_string e)
  in
  let r = one_request log in
  check_string "method" "POST" r.meth;
  check_string "url"
    "https://hs.example/_matrix/media/v3/upload?filename=cat%20pic.png" r.url;
  check_str_opt "content-type" (Some "image/png") (header r "content-type");
  check_str_opt "raw bytes" (Some data) r.body;
  check_str_opt "authorization" (Some "Bearer syt_secret_token")
    (header r "authorization");
  check_string "content_uri" "mxc://hs.example/AbCdEf" (Media.Mxc.to_string uri)

let test_media_upload_no_filename () =
  let log, fetch = mock (json {|{"content_uri":"mxc://hs.example/x"}|}) in
  let t = Client.with_session (client_of fetch) test_session in
  ignore (Media.upload t ~content_type:"text/plain" ~data:"hi" ());
  check_string "url" "https://hs.example/_matrix/media/v3/upload"
    (one_request log).url

let test_media_create_content_uri () =
  let log, fetch =
    mock
      (json
         {|{"content_uri":"mxc://hs.example/reserved","unused_expires_at":1700000000123}|})
  in
  let t = Client.with_session (client_of fetch) test_session in
  let reservation =
    match Media.create_content_uri t with
    | Ok reservation -> reservation
    | Error e ->
        Alcotest.failf "create content URI failed: %s" (Error.to_string e)
  in
  let r = one_request log in
  check_string "method" "POST" r.meth;
  check_string "url" "https://hs.example/_matrix/media/v1/create" r.url;
  check_str_opt "empty body" None r.body;
  check_str_opt "no content-type" None (header r "content-type");
  check_str_opt "accept" (Some "application/json") (header r "accept");
  check_str_opt "authorization" (Some "Bearer syt_secret_token")
    (header r "authorization");
  check_string "reserved URI" "mxc://hs.example/reserved"
    (Media.Mxc.to_string reservation.uri);
  Alcotest.(check (option int64))
    "expiry" (Some 1_700_000_000_123L)
    (Option.map Matrix_proto.Event.Timestamp.to_ms reservation.unused_expires_at)

let test_media_upload_preallocated () =
  let log, fetch = mock (json {|{}|}) in
  let t = Client.with_session (client_of fetch) test_session in
  let uri =
    Result.get_ok (Media.Mxc.of_string "mxc://hs.example:8448/folder_id-42")
  in
  let reservation = { Media.uri; unused_expires_at = None } in
  (match
     Media.upload_preallocated t reservation ~content_type:"image/png"
       ~data:"raw\000bytes" ~filename:"cat pic.png" ()
   with
  | Ok () -> ()
  | Error e ->
      Alcotest.failf "preallocated upload failed: %a"
        Media.pp_preallocated_upload_error e);
  let r = one_request log in
  check_string "method" "PUT" r.meth;
  check_string "url"
    "https://hs.example/_matrix/media/v3/upload/hs.example:8448/folder_id-42?filename=cat%20pic.png"
    r.url;
  check_str_opt "content-type" (Some "image/png") (header r "content-type");
  check_str_opt "body" (Some "raw\000bytes") r.body;
  check_str_opt "authorization" (Some "Bearer syt_secret_token")
    (header r "authorization")

let test_media_upload_preallocated_local_expiry () =
  let log, fetch = mock (json {|{}|}) in
  let t = Client.with_session (client_of fetch) test_session in
  let reservation =
    {
      Media.uri = Result.get_ok (Media.Mxc.of_string "mxc://hs.example/old");
      unused_expires_at =
        Some (Matrix_proto.Event.Timestamp.of_ms 1_700_000_000_000L);
    }
  in
  (match
     Media.upload_preallocated
       ~now:(Matrix_proto.Event.Timestamp.of_ms 1_700_000_000_000L)
       t reservation ~content_type:"text/plain" ~data:"too late" ()
   with
  | Error Media.Preallocated_expired -> ()
  | Error e ->
      Alcotest.failf "wrong expiry error: %a" Media.pp_preallocated_upload_error
        e
  | Ok () -> Alcotest.fail "expired reservation was uploaded");
  Alcotest.(check int) "no request" 0 (List.length (requests log))

let test_media_upload_preallocated_server_errors () =
  let reservation =
    {
      Media.uri = Result.get_ok (Media.Mxc.of_string "mxc://hs.example/id");
      unused_expires_at = None;
    }
  in
  let upload ~status body =
    let _, fetch = mock (fun req -> Fetch_mock.respond ~status body req) in
    let t = Client.with_session (client_of fetch) test_session in
    Media.upload_preallocated t reservation ~content_type:"text/plain"
      ~data:"body" ()
  in
  (match
     upload ~status:409
       {|{"errcode":"M_CANNOT_OVERWRITE_MEDIA","error":"already uploaded"}|}
   with
  | Error Media.Cannot_overwrite -> ()
  | Error e ->
      Alcotest.failf "wrong overwrite error: %a"
        Media.pp_preallocated_upload_error e
  | Ok () -> Alcotest.fail "overwrite was accepted");
  (match
     upload ~status:404
       {|{"errcode":"M_UNKNOWN","error":"The media id has expired"}|}
   with
  | Error Media.Preallocated_expired -> ()
  | Error e ->
      Alcotest.failf "wrong legacy expiry error: %a"
        Media.pp_preallocated_upload_error e
  | Ok () -> Alcotest.fail "legacy expiry was accepted");
  match
    upload ~status:404
      {|{"errcode":"M_NOT_FOUND","error":"invalid or expired"}|}
  with
  | Error (Media.Upload_error (Error.Matrix_error e)) ->
      Alcotest.(check string)
        "ambiguous not-found remains generic" "M_NOT_FOUND"
        (Error.errcode_to_string e.errcode)
  | Error e ->
      Alcotest.failf "wrong not-found error: %a"
        Media.pp_preallocated_upload_error e
  | Ok () -> Alcotest.fail "missing reservation was accepted"

let test_media_encrypted_upload_download () =
  let plaintext = "secret attachment bytes\000with binary data" in
  let uploaded = ref None in
  let tamper = ref false in
  let log, fetch =
    mock (fun req ->
        match Http.Method.to_string req.meth with
        | "POST" ->
            uploaded := body_of_request req;
            json {|{"content_uri":"mxc://hs.example/encrypted"}|} req
        | "GET" ->
            let url = Fetch.Middleware.Url.to_string req.url in
            if String.ends_with ~suffix:"/_matrix/client/versions" url then
              json {|{"versions":["v1.12"]}|} req
            else
              let body = Option.get !uploaded in
              let body =
                if not !tamper then body
                else
                  let b = Bytes.of_string body in
                  Bytes.set b 0 (Char.chr (Char.code (Bytes.get b 0) lxor 1));
                  Bytes.unsafe_to_string b
              in
              Fetch_mock.respond body req
        | _ -> Fetch_mock.respond "{}" req)
  in
  let t = Client.with_session (client_of fetch) test_session in
  let mxc, file =
    match
      Media.upload_encrypted t ~data:plaintext ~filename:"secret.bin" ()
    with
    | Ok x -> x
    | Error e ->
        Alcotest.failf "encrypted upload failed: %a" Media.pp_encrypted_error e
  in
  check_string "encrypted URL" "mxc://hs.example/encrypted"
    (Media.Mxc.to_string mxc);
  check_string "event file URL" "mxc://hs.example/encrypted" file.url;
  let body = Option.get !uploaded in
  if String.equal body plaintext then Alcotest.fail "plaintext was uploaded";
  check_str_opt "ciphertext content type" (Some "application/octet-stream")
    (header (List.hd (requests log)) "content-type");
  (match Media.download_encrypted t file with
  | Ok decoded -> check_string "decrypted body" plaintext decoded
  | Error e ->
      Alcotest.failf "encrypted download failed: %a" Media.pp_encrypted_error e);
  tamper := true;
  (match Media.download_encrypted t file with
  | Error (Media.Attachment_error Attachment.Hash_mismatch) -> ()
  | Error e ->
      Alcotest.failf "wrong tamper error: %a" Media.pp_encrypted_error e
  | Ok _ -> Alcotest.fail "tampered encrypted download was accepted");
  let unsupported = { file with v = "v1" } in
  (match Media.download_encrypted t unsupported with
  | Error (Media.Attachment_error (Attachment.Unsupported_version "v1")) -> ()
  | Error e ->
      Alcotest.failf "wrong unsupported error: %a" Media.pp_encrypted_error e
  | Ok _ -> Alcotest.fail "unsupported encrypted metadata was accepted");
  let malformed = { file with hashes = [] } in
  match Media.download_encrypted t malformed with
  | Error (Media.Attachment_error (Attachment.Malformed_metadata _)) -> ()
  | Error e ->
      Alcotest.failf "wrong malformed error: %a" Media.pp_encrypted_error e
  | Ok _ -> Alcotest.fail "malformed encrypted metadata was accepted"

let test_media_encrypted_stream_upload () =
  let plaintext = String.init 257 (fun i -> Char.chr (i land 255)) in
  let ciphertext = ref None in
  let request_length = ref None in
  let log, fetch =
    mock (fun req ->
        match Http.Method.to_string req.meth with
        | "POST" -> (
            match req.body with
            | Fetch.Stream { length; flow } ->
                request_length := length;
                let body = Buffer.create (String.length plaintext) in
                let buf = Cstruct.create 7 in
                let rec read () =
                  try
                    let n = Eio.Flow.single_read flow buf in
                    Buffer.add_string body (Cstruct.to_string ~off:0 ~len:n buf);
                    read ()
                  with End_of_file -> ()
                in
                read ();
                ciphertext := Some (Buffer.contents body);
                json {|{"content_uri":"mxc://hs.example/streamed"}|} req
            | _ -> Fetch_mock.respond "{}" req)
        | _ -> Fetch_mock.respond "{}" req)
  in
  let t = Client.with_session (client_of fetch) test_session in
  let mxc, file =
    match
      Media.upload_encrypted_stream t
        ~source:(Eio.Flow.string_source plaintext)
        ~length:(Int64.of_int (String.length plaintext))
        ~filename:"stream.bin" ()
    with
    | Ok x -> x
    | Error e ->
        Alcotest.failf "stream upload failed: %a" Media.pp_encrypted_error e
  in
  check_string "streamed URL" "mxc://hs.example/streamed"
    (Media.Mxc.to_string mxc);
  check_string "streamed event URL" "mxc://hs.example/streamed" file.url;
  Alcotest.(check (option int64))
    "declared length"
    (Some (Int64.of_int (String.length plaintext)))
    !request_length;
  let body = Option.get !ciphertext in
  if String.equal body plaintext then Alcotest.fail "plaintext was streamed";
  (match Attachment.Metadata.of_event_file file with
  | Error e -> Alcotest.failf "stream metadata: %a" Attachment.pp_error e
  | Ok metadata -> (
      match Attachment.decrypt metadata body with
      | Ok decoded -> check_string "streamed decrypt" plaintext decoded
      | Error e ->
          Alcotest.failf "streamed ciphertext: %a" Attachment.pp_error e));
  check_string "streamed upload path"
    "https://hs.example/_matrix/media/v3/upload?filename=stream.bin"
    (one_request log).url

let test_media_download () =
  let payload = "\x00\x01\x02 raw" in
  let log, fetch =
    mock (fun req ->
        let url = Fetch.Middleware.Url.to_string req.url in
        if String.ends_with ~suffix:"/_matrix/client/versions" url then
          json
            {|{"versions":["v1.10"],"unstable_features":{"org.matrix.msc3916.stable":true}}|}
            req
        else
          Fetch_mock.respond
            ~headers:(Http.Header.of_list [ ("content-type", "image/jpeg") ])
            payload req)
  in
  let t = Client.with_session (client_of fetch) test_session in
  let content =
    match
      Media.download t ~server_name:(server "hs.example") ~media_id:"AbCdEf"
    with
    | Ok v -> v
    | Error e -> Alcotest.failf "download failed: %s" (Error.to_string e)
  in
  (match requests log with
  | [ _versions; media_request ] ->
      check_string "url"
        "https://hs.example/_matrix/client/v1/media/download/hs.example/AbCdEf"
        media_request.url;
      check_str_opt "download bearer" (Some "Bearer syt_secret_token")
        (header media_request "authorization")
  | rs ->
      Alcotest.failf "expected versions and download, got %d" (List.length rs));
  check_string "data" payload content.body;
  check_str_opt "content-type" (Some "image/jpeg") content.content_type

let test_media_download_without_content_type () =
  let _, fetch =
    mock (fun req ->
        let url = Fetch.Middleware.Url.to_string req.url in
        if String.ends_with ~suffix:"/_matrix/client/versions" url then
          json {|{"versions":["v1.12"]}|} req
        else json "raw" req)
  in
  let t = Client.with_session (client_of fetch) test_session in
  match Media.download t ~server_name:(server "hs.example") ~media_id:"x" with
  | Ok c -> check_str_opt "no content-type" None c.content_type
  | Error e -> Alcotest.failf "download failed: %s" (Error.to_string e)

let test_media_thumbnail () =
  let log, fetch =
    mock (fun req ->
        let url = Fetch.Middleware.Url.to_string req.url in
        if String.ends_with ~suffix:"/_matrix/client/versions" url then
          json {|{"versions":["v1.12"]}|} req
        else json "thumb" req)
  in
  let t = Client.with_session (client_of fetch) test_session in
  ignore
    (Media.thumbnail t ~server_name:(server "hs.example") ~media_id:"AbCdEf"
       ~width:96 ~height:64 ~resize:`Crop ());
  match requests log with
  | [ _versions; media_request ] ->
      check_string "url"
        "https://hs.example/_matrix/client/v1/media/thumbnail/hs.example/AbCdEf?width=96&height=64&method=crop"
        media_request.url;
      check_str_opt "thumbnail bearer" (Some "Bearer syt_secret_token")
        (header media_request "authorization")
  | rs ->
      Alcotest.failf "expected versions and thumbnail, got %d" (List.length rs)

let test_media_config () =
  let log, fetch =
    mock (fun req ->
        let url = Fetch.Middleware.Url.to_string req.url in
        if String.ends_with ~suffix:"/_matrix/client/versions" url then
          json {|{"versions":["v1.12"]}|} req
        else
          json
            {|{"m.upload.size":52428800,"com.example.scan":{"enabled":true}}|}
            req)
  in
  let t = Client.with_session (client_of fetch) test_session in
  let cfg =
    match Media.get_config t with
    | Ok c -> c
    | Error e -> Alcotest.failf "config failed: %s" (Error.to_string e)
  in
  (match requests log with
  | [ _versions; config_request ] ->
      check_string "url" "https://hs.example/_matrix/client/v1/media/config"
        config_request.url;
      check_str_opt "config bearer" (Some "Bearer syt_secret_token")
        (header config_request "authorization")
  | rs -> Alcotest.failf "expected versions and config, got %d" (List.length rs));
  Alcotest.(check (option int)) "upload_size" (Some 52428800) cfg.upload_size;
  let custom =
    match List.assoc_opt "com.example.scan" cfg.custom with
    | Some value ->
        Result.get_ok
          (Jsont_bytesrw.encode_string Matrix_proto.Json.Codec.json value)
    | None -> Alcotest.fail "custom media config member was dropped"
  in
  check_string "custom config" {|{"enabled":true}|} custom

let test_media_legacy_endpoints_without_bearer () =
  let log, fetch =
    mock (fun req ->
        let url = Fetch.Middleware.Url.to_string req.url in
        if String.ends_with ~suffix:"/_matrix/client/versions" url then
          json {|{"versions":["v1.10"]}|} req
        else if String.ends_with ~suffix:"/download/hs.example/old" url then
          json "legacy body" req
        else if String.ends_with ~suffix:"/thumbnail/hs.example/old" url then
          json "legacy thumb" req
        else json {|{}|} req)
  in
  let t = Client.with_session (client_of fetch) test_session in
  ignore (Media.download t ~server_name:(server "hs.example") ~media_id:"old");
  ignore
    (Media.thumbnail t ~server_name:(server "hs.example") ~media_id:"old"
       ~width:32 ~height:24 ~resize:`Scale ());
  ignore (Media.get_config t);
  match requests log with
  | [ versions; download_request; thumbnail_request; config_request ] ->
      check_str_opt "versions bearer" (Some "Bearer syt_secret_token")
        (header versions "authorization");
      check_string "legacy download path"
        "https://hs.example/_matrix/media/v3/download/hs.example/old"
        download_request.url;
      check_str_opt "legacy download has no bearer" None
        (header download_request "authorization");
      check_string "legacy thumbnail path"
        "https://hs.example/_matrix/media/v3/thumbnail/hs.example/old?width=32&height=24&method=scale"
        thumbnail_request.url;
      check_str_opt "legacy thumbnail has no bearer" None
        (header thumbnail_request "authorization");
      check_string "legacy config path"
        "https://hs.example/_matrix/media/v3/config" config_request.url;
      check_str_opt "legacy config has no bearer" None
        (header config_request "authorization")
  | rs ->
      Alcotest.failf "expected four capability/media requests, got %d"
        (List.length rs)

let preview_response = {|{"og:title":"Matrix","matrix:image:size":102400}|}

let preview_field preview name =
  match List.assoc_opt name preview with
  | None -> None
  | Some value ->
      Some
        (Result.get_ok
           (Jsont_bytesrw.encode_string Matrix_proto.Json.Codec.json value))

let test_media_preview_authenticated () =
  let log, fetch =
    mock (fun req ->
        let url = Fetch.Middleware.Url.to_string req.url in
        if String.ends_with ~suffix:"/_matrix/client/versions" url then
          json {|{"versions":["v1.12"]}|} req
        else json preview_response req)
  in
  let t = Client.with_session (client_of fetch) test_session in
  let preview =
    match
      Media.get_url_preview t ~url:"https://matrix.org/post?a=1&b=2"
        ~ts:(Matrix_proto.Event.Timestamp.of_ms 1_700_000_000_123L)
        ()
    with
    | Ok p -> p
    | Error e -> Alcotest.failf "preview failed: %s" (Error.to_string e)
  in
  check_str_opt "title" (Some {|"Matrix"|}) (preview_field preview "og:title");
  match requests log with
  | [ versions; preview_request ] ->
      check_string "versions path" "https://hs.example/_matrix/client/versions"
        versions.url;
      check_string "authenticated preview path"
        "https://hs.example/_matrix/client/v1/media/preview_url?url=https://matrix.org/post?a%3D1%26b%3D2&ts=1700000000123"
        preview_request.url;
      check_str_opt "preview bearer" (Some "Bearer syt_secret_token")
        (header preview_request "authorization")
  | rs ->
      Alcotest.failf "expected two preview requests, got %d" (List.length rs)

let test_media_preview_legacy_without_bearer () =
  let log, fetch =
    mock (fun req ->
        let url = Fetch.Middleware.Url.to_string req.url in
        if String.ends_with ~suffix:"/_matrix/client/versions" url then
          json {|{"versions":["v1.10"]}|} req
        else json {|{}|} req)
  in
  let t = Client.with_session (client_of fetch) test_session in
  let preview =
    match Media.get_url_preview t ~url:"https://example.org/" () with
    | Ok p -> p
    | Error e -> Alcotest.failf "legacy preview failed: %s" (Error.to_string e)
  in
  Alcotest.(check int) "empty object" 0 (List.length preview);
  match requests log with
  | [ _versions; preview_request ] ->
      check_string "legacy preview path"
        "https://hs.example/_matrix/media/v3/preview_url?url=https://example.org/"
        preview_request.url;
      check_str_opt "legacy preview has no bearer" None
        (header preview_request "authorization")
  | rs ->
      Alcotest.failf "expected two preview requests, got %d" (List.length rs)

let test_parse_mxc () =
  let t = Alcotest.(option (pair string string)) in
  let parse s =
    Media.Mxc.of_string s |> Result.to_option
    |> Option.map (fun m ->
        ( Id.Server_name.to_string (Media.Mxc.server_name m),
          Media.Mxc.media_id m ))
  in
  Alcotest.check t "simple"
    (Some ("hs.example", "AbCdEf"))
    (parse "mxc://hs.example/AbCdEf");
  Alcotest.check t "media id with slash" None (parse "mxc://hs.example/a/b");
  Alcotest.check t "media id with punctuation" None
    (parse "mxc://hs.example/a?#");
  Alcotest.check t "wrong scheme" None (parse "https://hs.example/AbCdEf");
  Alcotest.check t "no media id" None (parse "mxc://hs.example");
  Alcotest.check t "empty media id" None (parse "mxc://hs.example/");
  Alcotest.check t "empty server" None (parse "mxc:///AbCdEf")

let test_mxc_to_http () =
  let _, fetch = mock (json "{}") in
  let t = client_of fetch in
  let mxc = Result.get_ok (Media.Mxc.of_string "mxc://other.example/AbCdEf") in
  let url u = Uriz.to_string u in
  check_string "download"
    "https://hs.example/_matrix/client/v1/media/download/other.example/AbCdEf"
    (url (Media.mxc_to_http t ~mxc ()));
  check_string "thumbnail"
    "https://hs.example/_matrix/client/v1/media/thumbnail/other.example/AbCdEf?width=32&height=32"
    (url (Media.mxc_to_http t ~mxc ~width:32 ~height:32 ()));
  check_string "width only falls back to download"
    "https://hs.example/_matrix/client/v1/media/download/other.example/AbCdEf"
    (url (Media.mxc_to_http t ~mxc ~width:32 ()));
  check_string "legacy unauthenticated download"
    "https://hs.example/_matrix/media/v3/download/other.example/AbCdEf"
    (url (Media.mxc_to_http_unauthenticated t ~mxc ()))

let test_mxc_to_http_resolved () =
  let mxc =
    Result.get_ok (Media.Mxc.of_string "mxc://other.example/resolved")
  in
  let check_route name versions ?width ?height expected =
    let log, fetch =
      mock (fun req ->
          let url = Fetch.Middleware.Url.to_string req.url in
          if String.ends_with ~suffix:"/_matrix/client/versions" url then
            json versions req
          else Fetch_mock.respond "unexpected media request" req)
    in
    let t = Client.with_session (client_of fetch) test_session in
    let resolved =
      match Media.mxc_to_http_resolved t ~mxc ?width ?height () with
      | Ok uri -> Uriz.to_string uri
      | Error error ->
          Alcotest.failf "%s resolution failed: %s" name (Error.to_string error)
    in
    check_string (name ^ " URL") expected resolved;
    match requests log with
    | [ versions_request ] ->
        check_string
          (name ^ " versions endpoint")
          "https://hs.example/_matrix/client/versions" versions_request.url;
        check_str_opt
          (name ^ " versions bearer")
          (Some "Bearer syt_secret_token")
          (header versions_request "authorization")
    | rs ->
        Alcotest.failf "%s expected one versions request, got %d" name
          (List.length rs)
  in
  check_route "stable" {|{"versions":["v1.11"]}|} ~width:32 ~height:24
    "https://hs.example/_matrix/client/v1/media/thumbnail/other.example/resolved?width=32&height=24";
  check_route "MSC3916"
    {|{"versions":["v1.10"],"unstable_features":{"org.matrix.msc3916.stable":true}}|}
    ~width:32 ~height:24
    "https://hs.example/_matrix/client/v1/media/thumbnail/other.example/resolved?width=32&height=24";
  check_route "legacy" {|{"versions":["v1.10"]}|}
    "https://hs.example/_matrix/media/v3/download/other.example/resolved"

let ptime s =
  match Ptime.of_rfc3339 s with Ok (t, _, _) -> t | Error _ -> invalid_arg s

let sample_session_file : Session.Session_file.t =
  {
    server =
      {
        homeserver = Uriz.of_string_exn "https://hs.example";
        user_id = uid "@alice:example.org";
      };
    auth =
      {
        access_token = "syt_secret_token";
        device_id = did "TESTDEVICE";
        refresh_token = Some "syr_refresh";
        access_token_expires_at = None;
        method_ = Session.Auth.Matrix;
      };
    sync = { next_batch = Some "s1_2"; filter_id = Some "7" };
    metadata =
      {
        created_at = ptime "2024-01-02T03:04:05Z";
        last_used_at = ptime "2024-05-06T07:08:09Z";
        client_name = "test";
      };
  }

let with_temp_xdg f =
  let dir = Filename.temp_dir "matrix-test-" "" in
  let saved =
    List.map
      (fun v -> (v, Sys.getenv_opt v))
      [
        "HOME";
        "XDG_DATA_HOME";
        "XDG_CONFIG_HOME";
        "XDG_CACHE_HOME";
        "XDG_STATE_HOME";
        "XDG_RUNTIME_DIR";
      ]
  in
  let restore () =
    List.iter
      (fun (v, old) ->
        match old with Some s -> Unix.putenv v s | None -> Unix.putenv v "")
      saved
  in
  List.iter (fun (v, _) -> Unix.putenv v dir) saved;
  Fun.protect
    ~finally:(fun () ->
      restore ();
      ignore (Sys.command (Filename.quote_command "rm" [ "-rf"; dir ])))
    (fun () -> f dir)

let with_temp_root f =
  let dir = Filename.temp_dir "matrix-profile-root-" "" in
  Fun.protect
    ~finally:(fun () ->
      ignore (Sys.command (Filename.quote_command "rm" [ "-rf"; dir ])))
    (fun () -> f dir)

let test_thread_subscriptions_disk_restart () =
  with_temp_root @@ fun dir ->
  Eio_main.run @@ fun env ->
  let root = Eio.Path.(Eio.Stdenv.fs env / dir) in
  let room = rid "!room:example.org" in
  let thread_root = Result.get_ok (Id.Event_id.of_string "$thread") in
  let store = Store.on_disk ~dir:root in
  let subscription =
    {
      Thread_subscriptions.status = Thread_subscriptions.Automatic;
      bump_stamp = Some 17L;
    }
  in
  (match
     Thread_subscriptions.upsert store ~room_id:room ~thread_root subscription
   with
  | Ok () -> ()
  | Error error ->
      Alcotest.failf "disk upsert failed: %s" (Error.to_string error));
  (match
     Thread_subscriptions.queue_catchup_token store ~from_:"batch" ~to_:None
   with
  | Ok () -> ()
  | Error error ->
      Alcotest.failf "disk token save failed: %s" (Error.to_string error));
  (* The same [from] with a different [to] is a distinct range. *)
  (match
     Thread_subscriptions.queue_catchup_token store ~from_:"batch"
       ~to_:(Some "pos")
   with
  | Ok () -> ()
  | Error error ->
      Alcotest.failf "second disk token save failed: %s" (Error.to_string error));
  let reopened = Store.on_disk ~dir:root in
  (match
     Thread_subscriptions.find_stored reopened ~room_id:room ~thread_root
   with
  | Ok (Some { status = Thread_subscriptions.Automatic; bump_stamp = Some 17L })
    ->
      ()
  | Ok _ -> Alcotest.fail "subscription did not survive disk restart"
  | Error error ->
      Alcotest.failf "disk lookup failed: %s" (Error.to_string error));
  match Thread_subscriptions.catchup_tokens reopened with
  | Ok
      [ { from_ = "batch"; to_ = None }; { from_ = "batch"; to_ = Some "pos" } ]
    ->
      ()
  | Ok _ -> Alcotest.fail "catch-up token order or duplicate semantics changed"
  | Error error ->
      Alcotest.failf "disk token reload failed: %s" (Error.to_string error)

let test_thread_subscriptions_failed_write_keeps_token () =
  with_temp_root @@ fun dir ->
  Eio_main.run @@ fun env ->
  let root = Eio.Path.(Eio.Stdenv.fs env / dir) in
  let room = rid "!room:example.org" in
  let thread_root = Result.get_ok (Id.Event_id.of_string "$thread") in
  let store = Store.on_disk ~dir:root in
  (match
     Thread_subscriptions.queue_catchup_token store ~from_:"batch" ~to_:None
   with
  | Ok () -> ()
  | Error error ->
      Alcotest.failf "initial token save failed: %s" (Error.to_string error));
  let stale = Store.on_disk ~dir:root in
  let other = Store.on_disk ~dir:root in
  (match
     Thread_subscriptions.upsert other ~room_id:room ~thread_root
       {
         Thread_subscriptions.status = Thread_subscriptions.Automatic;
         bump_stamp = Some 3L;
       }
   with
  | Ok () -> ()
  | Error error ->
      Alcotest.failf "conflicting write failed: %s" (Error.to_string error));
  let _, fetch =
    mock (fun req ->
        Fetch_mock.respond
          {|{"subscribed":{"!room:example.org":{"$thread":{"automatic":true,"bump_stamp":4}}}}|}
          req)
  in
  let client = Client.with_session (client_of fetch) test_session in
  (match Thread_subscriptions.catch_up_once client ~store:stale with
  | Error (Error.Policy_denied _) -> ()
  | Ok _ -> Alcotest.fail "stale catch-up write unexpectedly succeeded"
  | Error error ->
      Alcotest.failf "stale catch-up had wrong error: %s"
        (Error.to_string error));
  (match Thread_subscriptions.catchup_tokens stale with
  | Ok [ { from_ = "batch"; to_ = None } ] -> ()
  | Ok _ -> Alcotest.fail "failed catch-up changed its in-memory retry token"
  | Error error ->
      Alcotest.failf "in-memory retry-token lookup failed: %s"
        (Error.to_string error));
  (match Thread_subscriptions.find_stored stale ~room_id:room ~thread_root with
  | Ok None -> ()
  | Ok (Some _) -> Alcotest.fail "failed catch-up retained an unflushed update"
  | Error error ->
      Alcotest.failf "in-memory subscription lookup failed: %s"
        (Error.to_string error));
  let reopened = Store.on_disk ~dir:root in
  (match Thread_subscriptions.catchup_tokens reopened with
  | Ok [ { from_ = "batch"; to_ = None } ] -> ()
  | Ok _ -> Alcotest.fail "failed catch-up write lost its retry token"
  | Error error ->
      Alcotest.failf "retry-token reload failed: %s" (Error.to_string error));
  match
    Thread_subscriptions.find_stored reopened ~room_id:room ~thread_root
  with
  | Ok (Some { status = Thread_subscriptions.Automatic; bump_stamp = Some 3L })
    ->
      ()
  | Ok _ -> Alcotest.fail "failed catch-up write damaged prior persisted state"
  | Error error ->
      Alcotest.failf "prior-state reload failed: %s" (Error.to_string error)

let test_session_store () =
  with_temp_xdg @@ fun dir ->
  Eio_main.run @@ fun env ->
  let xdg = Xdge.create (Eio.Stdenv.fs env) "matrix" in
  let store = Profile_store.create ~xdg ~profile:"testprofile" in
  let ok = function
    | Ok v -> v
    | Error e ->
        Alcotest.failf "profile store: %s" (Matrix_client.Error.to_string e)
  in
  Alcotest.(check bool)
    "empty profile does not exist" false
    (Profile_store.exists store);
  Alcotest.(check (option string))
    "nothing to load" None
    (Option.map
       (fun (s : Session.Session_file.t) -> s.auth.access_token)
       (ok (Profile_store.load_session store)));
  ok (Profile_store.save_session store sample_session_file);
  Alcotest.(check bool) "profile now exists" true (Profile_store.exists store);
  (* The file is named session.json and is mode 0600. *)
  let path =
    Filename.concat
      (Filename.concat (Filename.concat dir "matrix") "profiles")
      "testprofile"
  in
  let file = Filename.concat path "session.json" in
  let profile_st = Unix.stat path in
  Alcotest.(check string)
    "profile directory mode" "0o700"
    (Printf.sprintf "0o%o" (profile_st.Unix.st_perm land 0o777));
  Alcotest.(check bool) "session.json exists" true (Sys.file_exists file);
  let st = Unix.stat file in
  Alcotest.(check string)
    "mode" "0o600"
    (Printf.sprintf "0o%o" (st.Unix.st_perm land 0o777));
  (* Renaming a freshly-created temporary file also tightens an existing file
     that was made too permissive between saves. *)
  Unix.chmod file 0o644;
  ok (Profile_store.save_session store sample_session_file);
  let st = Unix.stat file in
  Alcotest.(check string)
    "overwritten mode" "0o600"
    (Printf.sprintf "0o%o" (st.Unix.st_perm land 0o777));
  (match ok (Profile_store.load_session store) with
  | None -> Alcotest.fail "session did not load back"
  | Some (s : Session.Session_file.t) ->
      check_string "access_token" "syt_secret_token" s.auth.access_token;
      check_string "user_id" "@alice:example.org"
        (Id.User_id.to_string s.server.user_id);
      check_string "device_id" "TESTDEVICE"
        (Id.Device_id.to_string s.auth.device_id);
      check_str_opt "refresh_token" (Some "syr_refresh") s.auth.refresh_token;
      check_str_opt "next_batch" (Some "s1_2") s.sync.next_batch;
      check_str_opt "filter_id" (Some "7") s.sync.filter_id;
      check_string "homeserver" "https://hs.example"
        (Uriz.to_string s.server.homeserver);
      check_string "client_name" "test" s.metadata.client_name);
  Profile_store.clear store;
  Alcotest.(check bool) "cleared" false (Profile_store.exists store);
  Alcotest.(check bool) "session.json removed" false (Sys.file_exists file)

let test_session_store_explicit_root () =
  with_temp_root @@ fun dir ->
  Eio_main.run @@ fun env ->
  let root = Eio.Path.(Eio.Stdenv.fs env / dir / "explicit-root") in
  let store = Profile_store.create_at ~root ~profile:"rootprofile" in
  let path = Filename.concat dir "explicit-root/profiles/rootprofile" in
  let file = Filename.concat path "session.json" in
  Alcotest.(check bool) "root placement" true (Sys.file_exists path);
  Alcotest.(check bool)
    "does not use xdg data directory" false
    (Sys.file_exists (Filename.concat dir "matrix/profiles/rootprofile"));
  Alcotest.(check string)
    "profile directory mode" "0o700"
    (Printf.sprintf "0o%o" ((Unix.stat path).Unix.st_perm land 0o777));
  (match Profile_store.save_session store sample_session_file with
  | Ok () -> ()
  | Error e -> Alcotest.failf "profile store save: %s" (Error.to_string e));
  Alcotest.(check string)
    "new file mode" "0o600"
    (Printf.sprintf "0o%o" ((Unix.stat file).Unix.st_perm land 0o777))

let test_profile_name_validation () =
  List.iter
    (fun profile ->
      try
        Profile_store.validate_profile_name profile;
        Alcotest.failf "accepted unsafe profile name %S" profile
      with Invalid_argument _ -> ())
    [
      "";
      ".";
      "..";
      "nested/profile";
      "..\\escape";
      "/tmp/profile";
      "nul\000name";
    ];
  List.iter Profile_store.validate_profile_name [ "default"; "bot-1" ]

let test_atomic_write_requires_basename () =
  Eio_main.run @@ fun env ->
  match
    Profile_store.atomic_write ~path:(Eio.Stdenv.fs env) ~data:"not written"
  with
  | Error (Error.Policy_denied _) -> ()
  | Error error ->
      Alcotest.failf "wrong basename error: %s" (Error.to_string error)
  | Ok () -> Alcotest.fail "atomic write accepted a filesystem root"

let test_profile_store_io_context () =
  with_temp_root @@ fun dir ->
  Eio_main.run @@ fun env ->
  let root = Eio.Path.(Eio.Stdenv.fs env / dir) in
  let store = Profile_store.create_at ~root ~profile:"context" in
  Unix.mkdir (Filename.concat dir "profiles/context/session.json") 0o700;
  try
    match Profile_store.save_session store sample_session_file with
    | Error error ->
        Alcotest.failf "profile write returned an error: %s"
          (Error.to_string error)
    | Ok () -> Alcotest.fail "profile write unexpectedly replaced a directory"
  with
  | Eio.Io _ as exn ->
      let rendered = Fmt.str "%a" Eio.Exn.pp exn in
      check_bool "profile write has operation context" true
        (contains ~needle:"committing Matrix profile file" rendered)
  | exn ->
      Alcotest.failf "unexpected profile write exception: %s"
        (Printexc.to_string exn)

let test_session_update_is_serialized () =
  with_temp_root @@ fun dir ->
  Eio_main.run @@ fun env ->
  let root = Eio.Path.(Eio.Stdenv.fs env / dir) in
  let first = Profile_store.create_at ~root ~profile:"shared" in
  let second = Profile_store.create_at ~root ~profile:"shared" in
  let fail_result label = function
    | Ok () -> ()
    | Error error -> Alcotest.failf "%s: %s" label (Error.to_string error)
  in
  fail_result "initial save"
    (Profile_store.save_session first sample_session_file);
  Eio.Switch.run @@ fun sw ->
  let first_entered, first_entered_r = Eio.Promise.create () in
  let first_release, first_release_r = Eio.Promise.create () in
  let first_done, first_done_r = Eio.Promise.create () in
  let second_done, second_done_r = Eio.Promise.create () in
  let second_entered = ref false in
  let first_result = ref None and second_result = ref None in
  Eio.Fiber.fork ~sw (fun () ->
      first_result :=
        Some
          (Profile_store.update_session first (fun session ->
               Eio.Promise.resolve first_entered_r ();
               Eio.Promise.await first_release;
               {
                 session with
                 auth = { session.auth with access_token = "syt_first" };
               }));
      Eio.Promise.resolve first_done_r ());
  Eio.Promise.await first_entered;
  Eio.Fiber.fork ~sw (fun () ->
      second_result :=
        Some
          (Profile_store.update_session second (fun session ->
               second_entered := true;
               {
                 session with
                 metadata = { session.metadata with client_name = "second" };
               }));
      Eio.Promise.resolve second_done_r ());
  (* The first callback still owns the native and process-local locks. *)
  Eio.Fiber.yield ();
  check_bool "second update waits for first" false !second_entered;
  Eio.Promise.resolve first_release_r ();
  Eio.Promise.await first_done;
  Eio.Promise.await second_done;
  check_bool "second update eventually runs" true !second_entered;
  let check_result label = function
    | Some (Ok ()) -> ()
    | Some (Error error) ->
        Alcotest.failf "%s: %s" label (Error.to_string error)
    | None -> Alcotest.failf "%s did not run" label
  in
  check_result "first update" !first_result;
  check_result "second update" !second_result;
  (match Profile_store.load_session first with
  | Error error -> Alcotest.failf "reload: %s" (Error.to_string error)
  | Ok None -> Alcotest.fail "session disappeared"
  | Ok (Some session) ->
      check_string "updated access token" "syt_first" session.auth.access_token;
      check_str_opt "refresh token preserved" (Some "syr_refresh")
        session.auth.refresh_token;
      check_string "unrelated metadata preserved from second update" "second"
        session.metadata.client_name;
      check_str_opt "sync preserved" (Some "s1_2") session.sync.next_batch);
  let lock_file = Filename.concat dir "profiles/shared/.profile.lock" in
  Alcotest.(check bool) "lock inode remains" true (Sys.file_exists lock_file);
  Alcotest.(check string)
    "lock mode" "0o600"
    (Printf.sprintf "0o%o" ((Unix.stat lock_file).Unix.st_perm land 0o777));
  let raised =
    try
      ignore (Profile_store.with_lock first (fun () -> failwith "callback"));
      false
    with Failure message -> String.equal message "callback"
  in
  check_bool "callback exception propagates" true raised;
  fail_result "lock released after exception"
    (Profile_store.with_lock first (fun () -> ()))

let test_profile_lock_rejects_symlink () =
  with_temp_root @@ fun dir ->
  Eio_main.run @@ fun env ->
  let root = Eio.Path.(Eio.Stdenv.fs env / dir) in
  let store = Profile_store.create_at ~root ~profile:"symlinked" in
  let profile_path = Filename.concat dir "profiles/symlinked" in
  let target = Filename.concat dir "outside.lock" in
  let lock = Filename.concat profile_path ".profile.lock" in
  let fd = Unix.openfile target [ Unix.O_CREAT; Unix.O_RDWR ] 0o600 in
  Unix.close fd;
  Unix.symlink target lock;
  match Profile_store.with_lock store (fun () -> ()) with
  | Error (Error.Network_error _) -> ()
  | Error error ->
      Alcotest.failf "symlink lock had wrong error: %s" (Error.to_string error)
  | Ok () -> Alcotest.fail "profile lock followed a symlink"

let () =
  Alcotest.run "matrix.client"
    [
      ( "request construction",
        [
          Alcotest.test_case "GET whoami, no session" `Quick
            (run test_get_unauthenticated);
          Alcotest.test_case "GET carries bearer token" `Quick
            (run test_get_authenticated);
          Alcotest.test_case "post_unauthenticated drops the token" `Quick
            (run test_post_unauthenticated_carries_no_token);
          Alcotest.test_case "no User-Agent when unconfigured" `Quick
            (run test_no_user_agent);
          Alcotest.test_case "query params are encoded" `Quick
            (run test_query_params);
          Alcotest.test_case "POST sends JSON verbatim" `Quick
            (run test_post_json);
          Alcotest.test_case "PUT sends JSON verbatim" `Quick
            (run test_put_json);
          Alcotest.test_case "DELETE without body" `Quick
            (run test_delete_without_body);
          Alcotest.test_case "DELETE with body" `Quick
            (run test_delete_with_body);
          Alcotest.test_case "raw helpers take absolute paths" `Quick
            (run test_raw_paths_are_absolute);
          Alcotest.test_case "absolute helpers send authenticated JSON" `Quick
            (run test_absolute_json_paths);
          Alcotest.test_case "streaming GET keeps body scoped" `Quick
            (run test_get_stream);
          Alcotest.test_case "streaming GET maps HTTP errors" `Quick
            (run test_get_stream_http_error_does_not_call_callback);
        ] );
      ( "response handling",
        [
          Alcotest.test_case "2xx is Ok" `Quick (run test_2xx_is_ok);
          Alcotest.test_case "4xx Matrix error" `Quick (run test_matrix_error);
          Alcotest.test_case "unknown errcode is preserved" `Quick
            (run test_unknown_errcode_preserved);
          Alcotest.test_case "5xx non-JSON is Http_error" `Quick
            (run test_http_error_non_json);
          Alcotest.test_case "debug logs redact nested E2EE secrets" `Quick
            (run test_debug_log_redacts_nested_e2ee_secrets);
          Alcotest.test_case "429 keeps retry_after_ms" `Quick
            (run test_rate_limit_retry_after);
          Alcotest.test_case "401 keeps soft_logout" `Quick
            (run test_soft_logout_flag);
          Alcotest.test_case "automatic refresh is opt-in" `Quick
            (run test_auto_refresh_disabled);
          Alcotest.test_case "automatic refresh replays GET" `Quick
            (run test_auto_refresh_get_replay);
          Alcotest.test_case "automatic refresh replays POST" `Quick
            (run test_auto_refresh_post_replay);
          Alcotest.test_case "automatic refresh retries only once" `Quick
            (run test_auto_refresh_only_once);
          Alcotest.test_case "automatic refresh without token" `Quick
            (run test_auto_refresh_missing_token);
          Alcotest.test_case "automatic refresh retains token" `Quick
            (run test_auto_refresh_retains_refresh_token);
          Alcotest.test_case "refresh failure is retryable later" `Quick
            (run test_auto_refresh_failure_allows_later_retry);
          Alcotest.test_case "persistence sees atomic session" `Quick
            (run test_auto_refresh_hook_sees_atomic_session);
          Alcotest.test_case "concurrent refresh is shared" `Quick
            (run test_auto_refresh_concurrent_single_attempt);
          Alcotest.test_case "unauthenticated bypasses refresh" `Quick
            (run test_auto_refresh_unauthenticated_bypass);
          Alcotest.test_case "stream POST bypasses refresh" `Quick
            (run test_auto_refresh_post_stream_no_retry);
          Alcotest.test_case "proactive stream POST sends once" `Quick
            (run test_auto_refresh_expiry_post_stream_one_send);
          Alcotest.test_case "automatic refresh replays stream GET" `Quick
            (run test_auto_refresh_get_stream_replay);
          Alcotest.test_case "refresh persistence propagates cancellation"
            `Quick
            (run test_auto_refresh_hook_cancellation_propagates);
          Alcotest.test_case "session transformations reset refresh" `Quick
            (run test_auto_refresh_state_reset);
          Alcotest.test_case "proactive refreshes before expiry" `Quick
            (run test_auto_refresh_expiry_due_before_request);
          Alcotest.test_case "proactive refresh skips a fresh token" `Quick
            (run test_auto_refresh_expiry_not_due);
          Alcotest.test_case "proactive refresh needs a refresh token" `Quick
            (run test_auto_refresh_expiry_without_refresh_token);
          Alcotest.test_case "proactive failure is retryable" `Quick
            (run test_auto_refresh_expiry_failure_retries_later);
          Alcotest.test_case "concurrent proactive refresh is shared" `Quick
            (run test_auto_refresh_expiry_concurrent_single_attempt);
          Alcotest.test_case "proactive persistence is reentrant" `Quick
            (run test_auto_refresh_expiry_reentrant_persistence);
          Alcotest.test_case "bad JSON shape is Json_error" `Quick
            (run test_json_decode_error);
          Alcotest.test_case "JSON depth boundary is enforced" `Quick
            test_json_decode_depth_boundary;
          Alcotest.test_case "duplicate JSON members are rejected" `Quick
            test_json_decode_rejects_duplicate_members;
          Alcotest.test_case "unknown JSON members are checked" `Quick
            test_json_decode_checks_unknown_members;
          Alcotest.test_case "missing required result members are rejected"
            `Quick
            (run test_required_member_missing);
        ] );
      ( "presence",
        [
          Alcotest.test_case "client-owned state is shared and wakes" `Quick
            (run test_client_owned_presence);
          Alcotest.test_case "non-immediate update is local" `Quick
            (run test_presence_immediate_false);
          Alcotest.test_case "failed update retains local state" `Quick
            (run test_presence_failure_retains_local_state);
          Alcotest.test_case "classic sync uses client presence" `Quick
            (run test_sync_uses_client_presence);
          Alcotest.test_case "sliding sync uses client presence" `Quick
            (run test_sliding_sync_uses_client_presence);
        ] );
      ( "transport failures",
        [
          Alcotest.test_case "connection failure" `Quick
            (run test_connection_failure);
          Alcotest.test_case "protocol error" `Quick (run test_protocol_error);
          Alcotest.test_case "transport diagnostic redacts query" `Quick
            (run test_transport_diagnostic_redacts_query);
          Alcotest.test_case "TLS failure" `Quick (run test_tls_failure);
          Alcotest.test_case "cancellation propagates" `Quick
            (run test_cancellation_propagates);
          Alcotest.test_case "request timeout allows success" `Quick
            (run_full test_request_timeout_success);
          Alcotest.test_case "request timeout covers response" `Quick
            (run_full test_request_timeout_response);
          Alcotest.test_case "request timeout covers stream callback" `Quick
            (run_full test_request_timeout_stream_callback);
          Alcotest.test_case "request timeout covers retry backoff" `Quick
            (run_full test_request_timeout_covers_retry_backoff);
          Alcotest.test_case "request timeout covers refresh" `Quick
            (run_full test_request_timeout_covers_refresh);
          Alcotest.test_case "request timeout preserves cancellation" `Quick
            (run_full test_request_timeout_preserves_parent_cancellation);
        ] );
      ( "origin restriction",
        [
          Alcotest.test_case "insecure origin keeps the bearer token" `Quick
            (run test_insecure_origin_with_bearer);
          Alcotest.test_case "cross-origin redirect is denied" `Quick
            (run test_cross_origin_redirect_denied);
          Alcotest.test_case "unsupported scheme is rejected" `Quick
            (run test_bad_homeserver_scheme);
          Alcotest.test_case "host-less homeserver is rejected" `Quick
            (run test_homeserver_without_host);
          Alcotest.test_case "homeserver URL boundary" `Quick
            (run test_homeserver_url_boundary);
          Alcotest.test_case "validated URL preserves query" `Quick
            (run test_validated_url_preserves_query);
        ] );
      ( "auth",
        [
          Alcotest.test_case "login_password" `Quick (run test_login_password);
          Alcotest.test_case "login_password requests refresh token" `Quick
            (run test_login_password_requests_refresh_token);
          Alcotest.test_case "login_password decodes optional expiry" `Quick
            (run test_login_password_expiry_optional);
          Alcotest.test_case "refresh decodes optional expiry" `Quick
            (run test_refresh_token_expiry_optional);
          Alcotest.test_case "login_token requests refresh token" `Quick
            (run test_login_token_requests_refresh_token);
          Alcotest.test_case "login failure maps the errcode" `Quick
            (run test_login_failure);
          Alcotest.test_case "login flows" `Quick (run test_login_flows);
          Alcotest.test_case "whoami" `Quick (run test_whoami);
        ] );
      ( "json api",
        [
          Alcotest.test_case "Route path encoding" `Quick
            test_route_matrix_path_characters;
          Alcotest.test_case "Route binding validation" `Quick
            test_route_binding_validation;
          Alcotest.test_case "Route template validation" `Quick
            test_route_template_validation;
          Alcotest.test_case "Route base prefix and repeated query" `Quick
            test_route_base_prefix_and_repeated_query;
          Alcotest.test_case "query delimiters" `Quick
            (run test_query_delimiters);
          Alcotest.test_case "Rooms.join" `Quick (run test_rooms_join);
          Alcotest.test_case "Messages.send_text" `Quick
            (run test_messages_send_text);
          Alcotest.test_case "transaction ids differ" `Quick
            (run test_send_text_txn_ids_differ);
          Alcotest.test_case "transaction ids are path safe" `Quick
            test_txn_id_is_path_safe;
          Alcotest.test_case "Rooms.get_joined_rooms" `Quick
            (run test_get_joined_rooms);
          Alcotest.test_case "Dehydrated device support" `Quick
            (run test_dehydrated_device_support_probe);
          Alcotest.test_case "Thread subscriptions" `Quick
            (run test_thread_subscriptions_transport);
          Alcotest.test_case "Thread subscription validation" `Quick
            (run test_thread_subscriptions_reject_bad_changes);
          Alcotest.test_case "Thread subscription support" `Quick
            (run test_thread_subscriptions_support);
          Alcotest.test_case "Thread subscription persistence" `Quick
            (run test_thread_subscriptions_persistence_and_catchup);
          Alcotest.test_case "Thread subscription load/fetch" `Quick
            (run test_thread_subscriptions_load_or_fetch_and_if_needed);
        ] );
      ( "media",
        [
          Alcotest.test_case "upload" `Quick (run test_media_upload);
          Alcotest.test_case "upload without filename" `Quick
            (run test_media_upload_no_filename);
          Alcotest.test_case "create preallocated URI" `Quick
            (run test_media_create_content_uri);
          Alcotest.test_case "upload preallocated media" `Quick
            (run test_media_upload_preallocated);
          Alcotest.test_case "preallocated local expiry" `Quick
            (run test_media_upload_preallocated_local_expiry);
          Alcotest.test_case "preallocated server errors" `Quick
            (run test_media_upload_preallocated_server_errors);
          Alcotest.test_case "encrypted upload/download" `Quick
            (run test_media_encrypted_upload_download);
          Alcotest.test_case "encrypted streaming upload" `Quick
            (run test_media_encrypted_stream_upload);
          Alcotest.test_case "download" `Quick (run test_media_download);
          Alcotest.test_case "download without Content-Type" `Quick
            (run test_media_download_without_content_type);
          Alcotest.test_case "thumbnail" `Quick (run test_media_thumbnail);
          Alcotest.test_case "config" `Quick (run test_media_config);
          Alcotest.test_case "legacy media omits bearer" `Quick
            (run test_media_legacy_endpoints_without_bearer);
          Alcotest.test_case "URL preview authenticated" `Quick
            (run test_media_preview_authenticated);
          Alcotest.test_case "URL preview legacy" `Quick
            (run test_media_preview_legacy_without_bearer);
          Alcotest.test_case "parse_mxc" `Quick test_parse_mxc;
          Alcotest.test_case "mxc_to_http" `Quick (run test_mxc_to_http);
          Alcotest.test_case "capability-resolved mxc_to_http" `Quick
            (run test_mxc_to_http_resolved);
        ] );
      ( "session store",
        [
          Alcotest.test_case "thread subscriptions restart" `Quick
            test_thread_subscriptions_disk_restart;
          Alcotest.test_case "thread subscriptions failed write" `Quick
            test_thread_subscriptions_failed_write_keeps_token;
          Alcotest.test_case "save/load/clear" `Quick test_session_store;
          Alcotest.test_case "explicit root" `Quick
            test_session_store_explicit_root;
          Alcotest.test_case "profile name validation" `Quick
            test_profile_name_validation;
          Alcotest.test_case "atomic write requires basename" `Quick
            test_atomic_write_requires_basename;
          Alcotest.test_case "filesystem error context" `Quick
            test_profile_store_io_context;
          Alcotest.test_case "serialized read-modify-write" `Quick
            test_session_update_is_serialized;
          Alcotest.test_case "lock rejects symlink" `Quick
            test_profile_lock_rejects_symlink;
        ] );
    ]
