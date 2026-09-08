(* [Fetch_httpz.std] exposes the policy knobs of the HTTPz backend.
   This test only constructs the backend: the custom connection and TLS
   wrappers are never called, so it does not depend on a network or trust
   store. *)

let test_policy_arguments () =
  Eio_main.run @@ fun env ->
  let retry = Fetch.Retry.v ~max_retries:0 () in
  let connect ~sw:_ ~host:_ ~port:_ = assert false in
  let https _uri conn = conn in
  ignore
    (Fetch_httpz.std ~connect ~https ~cookies:`Off ~retry ~max_concurrent:1
       ~min_interval:(Duration.of_sec 0) ~connect_timeout:(Duration.of_sec 1)
       ~idle_timeout:(Duration.of_sec 1) env)

let test_custom_authenticator () =
  Eio_main.run @@ fun env ->
  let authenticator ?ip:_ ~host:_ _chain = Error `InvalidChain in
  let https = Httpz_tls.client ~authenticator in
  let homeserver = Uriz.of_string_exn "https://hs.example" in
  ignore
    (Fetch_httpz.std ~https
       ~retry:(Matrix_client.Http_retry.default ~homeserver)
       env)

let request meth url : Fetch.Middleware.request =
  {
    meth;
    url = Result.get_ok (Fetch.Middleware.Url.of_string url);
    headers = Http.Header.init ();
    body = Fetch.String "{}";
    sensitive = [];
    sensitive_query = [];
  }

let test_matrix_route_policy () =
  let root =
    Result.get_ok (Matrix_client.Client.Url.of_string "https://hs.example")
  in
  let prefix =
    Result.get_ok
      (Matrix_client.Client.Url.of_string "https://hs.example/tenant")
  in
  let check label expected homeserver meth path =
    Alcotest.(check bool)
      label expected
      (Matrix_client.Http_retry.retry_request ~homeserver (request meth path))
  in
  check "ordinary GET remains eligible" true root `GET
    "https://oauth.example/anything";
  check "key query" true root `POST
    "https://hs.example/_matrix/client/v3/keys/query";
  check "key query below deployment prefix" true prefix `POST
    "https://hs.example/tenant/_matrix/client/v3/keys/query";
  check "trailing slash deployment prefix" true
    (Result.get_ok
       (Matrix_client.Client.Url.of_string "https://hs.example/tenant/"))
    `POST "https://hs.example/tenant/_matrix/client/v3/keys/query";
  check "outside deployment prefix" false prefix `POST
    "https://hs.example/_matrix/client/v3/keys/query";
  check "extra prefix below homeserver" false root `POST
    "https://hs.example/tenant/_matrix/client/v3/keys/query";
  check "off-origin key-shaped path" false root `POST
    "https://oauth.example/_matrix/client/v3/keys/query";
  check "key claim" false root `POST
    "https://hs.example/_matrix/client/v3/keys/claim";
  check "key upload" false root `POST
    "https://hs.example/_matrix/client/v3/keys/upload";
  check "near miss" false root `POST
    "https://hs.example/_matrix/client/v3/keys/query-more";
  check "extra segment" false root `POST
    "https://hs.example/_matrix/client/v3/keys/query/extra";
  check "trailing separator" false root `POST
    "https://hs.example/_matrix/client/v3/keys/query/";
  check "repeated separator" false root `POST
    "https://hs.example/_matrix/client/v3/keys//query";
  check "query is not exact" false root `POST
    "https://hs.example/_matrix/client/v3/keys/query?mode=other";
  check "fragment is not exact" false root `POST
    "https://hs.example/_matrix/client/v3/keys/query#other";
  check "dot segments canonicalize" true root `POST
    "https://hs.example/ignored/../_matrix/client/v3/keys/query";
  check "different port" false root `POST
    "https://hs.example:8448/_matrix/client/v3/keys/query";
  check "encoded unreserved route" true root `POST
    "https://hs.example/_matrix/client/v3/keys/%71uery";
  check "encoded separator stays one segment" false root `POST
    "https://hs.example/_matrix/client/v3/keys%2Fquery";
  let rejects_homeserver value =
    try
      ignore
        (Matrix_client.Http_retry.v ~homeserver:(Uriz.of_string_exn value) ());
      false
    with Invalid_argument _ -> true
  in
  Alcotest.(check bool)
    "homeserver query rejected" true
    (rejects_homeserver "https://hs.example?tenant=other");
  Alcotest.(check bool)
    "homeserver fragment rejected" true
    (rejects_homeserver "https://hs.example#tenant")

let retry_client env handler =
  Fetch.with_retry ~clock:env#mono_clock
    ~random:(Eio.Flow.string_source (String.make 8 '\000'))
    ~config:
      (Matrix_client.Http_retry.v ~max_retries:1
         ~homeserver:(Uriz.of_string_exn "https://hs.example")
         ())
    (Fetch_mock.client handler)

let post_status ~sw client url =
  let response = Fetch.post ~sw client ~body:(Fetch.String "{}") url in
  let status = Fetch.status response in
  Fetch.close response;
  status

let test_selective_status_retry () =
  Eio_mock.Backend.run_full @@ fun env ->
  let probe path =
    let attempts = ref 0 in
    let client =
      retry_client env (fun req ->
          incr attempts;
          Fetch_mock.respond ~status:(if !attempts = 1 then 503 else 200) "" req)
    in
    let status =
      Eio.Switch.run @@ fun sw ->
      post_status ~sw client ("https://hs.example" ^ path)
    in
    (!attempts, status)
  in
  Alcotest.(check (pair int int))
    "query retried" (2, 200)
    (probe "/_matrix/client/v3/keys/query");
  Alcotest.(check (pair int int))
    "claim vetoed" (1, 503)
    (probe "/_matrix/client/v3/keys/claim");
  Alcotest.(check (pair int int))
    "upload vetoed" (1, 503)
    (probe "/_matrix/client/v3/keys/upload");
  Alcotest.(check (pair int int))
    "sync vetoed" (1, 503)
    (probe "/_matrix/client/v3/sync")

let test_selective_connection_retry () =
  Eio_mock.Backend.run_full @@ fun env ->
  let probe path =
    let attempts = ref 0 in
    let client =
      retry_client env (fun req ->
          incr attempts;
          if !attempts = 1 then
            raise (Fetch.err (Fetch.Connection_failure Eio.Net.Timeout))
          else Fetch_mock.respond "" req)
    in
    let outcome =
      Eio.Switch.run @@ fun sw ->
      try `Status (post_status ~sw client ("https://hs.example" ^ path))
      with Eio.Io (Fetch.E (Fetch.Connection_failure _), _) ->
        `Connection_failure
    in
    (!attempts, outcome)
  in
  Alcotest.(check (pair int (result int string)))
    "query retried" (2, Ok 200)
    (match probe "/_matrix/client/v3/keys/query" with
    | attempts, `Status status -> (attempts, Ok status)
    | attempts, `Connection_failure -> (attempts, Error "connection failure"));
  Alcotest.(check (pair int (result int string)))
    "claim vetoed"
    (1, Error "connection failure")
    (match probe "/_matrix/client/v3/keys/claim" with
    | attempts, `Status status -> (attempts, Ok status)
    | attempts, `Connection_failure -> (attempts, Error "connection failure"))

let () =
  Alcotest.run "matrix.eio HTTP policy"
    [
      ( "client",
        [
          Alcotest.test_case "policy arguments" `Quick test_policy_arguments;
          Alcotest.test_case "custom authenticator" `Quick
            test_custom_authenticator;
          Alcotest.test_case "Matrix route allowlist" `Quick
            test_matrix_route_policy;
          Alcotest.test_case "selective status retry" `Quick
            test_selective_status_retry;
          Alcotest.test_case "selective connection retry" `Quick
            test_selective_connection_retry;
        ] );
    ]
