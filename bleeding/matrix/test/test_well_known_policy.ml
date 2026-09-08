module Client = Matrix_client.Client
module Error = Matrix_client.Error
module Metadata = Matrix_client.Oauth.Metadata
module Server = Matrix_client.Server

let mock_env =
  object
    method secure_random = Eio.Flow.string_source (String.make 4096 '\000')
  end

type recorded = { meth : string; url : string }

let mock_seq replies =
  let log = ref [] in
  let remaining = ref replies in
  let fetch =
    Fetch_mock.client (fun (req : Fetch.Middleware.request) ->
        log :=
          {
            meth = Http.Method.to_string req.meth;
            url = Fetch.Middleware.Url.to_string req.url;
          }
          :: !log;
        match !remaining with
        | [] -> Alcotest.fail "more requests than scripted replies"
        | (status, body) :: rest ->
            remaining := rest;
            Fetch_mock.respond ~status body req)
  in
  (log, fetch)

let client_of ?(policy = Client.Query) fetch =
  let config =
    Client.config
      ~homeserver:(Uriz.of_string_exn "https://hs.example")
      ~well_known_policy:policy ()
  in
  Client.create ~config ~fetch ~random:(Matrix_client.Random.of_env mock_env)

let requests log = List.rev !log
let urls log = List.map (fun r -> r.url) (requests log)
let run f () = Eio_mock.Backend.run f

let check_urls expected log =
  Alcotest.(check (list string)) "request URLs" expected (urls log)

let well_known_json = {|{"m.homeserver":{"base_url":"https://hs.example"}}|}
let versions_json = {|{"versions":["v1.11"]}|}
let unrecognized = {|{"errcode":"M_UNRECOGNIZED"}|}

let test_disabled_get () =
  let log, fetch = mock_seq [] in
  let client = client_of ~policy:Client.Do_not_query fetch in
  let derived = Client.with_access_token client "token" in
  Alcotest.(check bool)
    "policy is retained by derived clients" true
    (Client.well_known_policy derived = Client.Do_not_query);
  let got = Server.get_well_known derived in
  Alcotest.(check bool) "disabled lookup is absent" true (got = Ok None);
  check_urls [] log

let test_disabled_discover () =
  let log, fetch = mock_seq [ (200, versions_json) ] in
  let got = Server.discover (client_of ~policy:Client.Do_not_query fetch) in
  let d =
    match got with
    | Ok d -> d
    | Error e -> Alcotest.failf "discover failed: %s" (Error.to_string e)
  in
  check_urls [ "https://hs.example/_matrix/client/versions" ] log;
  Alcotest.(check bool) "well-known is absent" true (d.well_known = None);
  Alcotest.(check bool) "versions still fetched" true (d.server_versions <> None)

let test_disabled_oauth_fallback () =
  let log, fetch = mock_seq [ (404, unrecognized); (404, unrecognized) ] in
  match Metadata.fetch (client_of ~policy:Client.Do_not_query fetch) with
  | Error (Error.Http_error { status = 404; _ }) ->
      check_urls
        [
          "https://hs.example/_matrix/client/v1/auth_metadata";
          "https://hs.example/_matrix/client/unstable/org.matrix.msc2965/auth_metadata";
        ]
        log
  | Ok _ -> Alcotest.fail "expected no-metadata error"
  | Error e -> Alcotest.failf "expected typed 404, got %s" (Error.to_string e)

let test_default_still_queries () =
  let log, fetch = mock_seq [ (200, well_known_json) ] in
  let got = Server.get_well_known (client_of fetch) in
  Alcotest.(check bool) "default lookup succeeds" true (Result.is_ok got);
  check_urls [ "https://hs.example/.well-known/matrix/client" ] log

let () =
  Alcotest.run "well-known policy"
    [
      ( "server",
        [
          Alcotest.test_case "disabled get" `Quick (run test_disabled_get);
          Alcotest.test_case "disabled discover" `Quick
            (run test_disabled_discover);
          Alcotest.test_case "default still queries" `Quick
            (run test_default_still_queries);
        ] );
      ( "oauth",
        [
          Alcotest.test_case "disabled fallback" `Quick
            (run test_disabled_oauth_fallback);
        ] );
    ]
