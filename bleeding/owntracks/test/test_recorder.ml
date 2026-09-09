module R = Owntracks_recorder_client

let test_queries () =
  let http =
    Fetch_mock.client (fun request ->
        let uri = Uri.of_string (Fetch.Middleware.Url.to_string request.url) in
        Alcotest.(check string)
          "base path" "/recorder/api/0/locations" (Uri.path uri);
        List.iter
          (fun (key, expected) ->
            Alcotest.(check (option string))
              key (Some expected)
              (Uri.get_query_param uri key))
          [
            ("user", "a&b");
            ("device", "phone/one");
            ("from", "2026-01-01");
            ("to", "2026-02-01");
          ];
        Fetch_mock.respond {|{"data":[]}|} request)
  in
  let client = R.v http ~url:"https://example.com/recorder" in
  match
    R.locations client ~user:"a&b" ~device:"phone/one" ~from_date:"2026-01-01"
      ~to_date:"2026-02-01"
  with
  | Ok [] -> ()
  | _ -> Alcotest.fail "wrong response"

let test_errors () =
  let client status body limit =
    R.v ~max_response:limit
      (Fetch_mock.client (Fetch_mock.respond ~status body))
      ~url:"https://example.com"
  in
  (match R.list_users (client 403 "denied" 100) with
  | Error (R.Http_status 403) -> ()
  | _ -> Alcotest.fail "lost status");
  (match R.list_users (client 200 "not json" 100) with
  | Error (R.Invalid_response _) -> ()
  | _ -> Alcotest.fail "lost JSON error");
  match R.list_users (client 200 "[\"too long\"]" 4) with
  | Error (R.Invalid_response _) -> ()
  | _ -> Alcotest.fail "lost byte limit"

let () =
  Eio_main.run @@ fun _ ->
  Alcotest.run "OwnTracks Recorder HTTP"
    [
      ( "requests",
        [
          Alcotest.test_case "encoded query" `Quick test_queries;
          Alcotest.test_case "explicit errors and bounds" `Quick test_errors;
        ] );
    ]
