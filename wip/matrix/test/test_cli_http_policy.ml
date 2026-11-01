open Cmdliner
module C = Matrix_cli

let test_defaults () =
  let o = C.http_options C.http_policy_default in
  Alcotest.(check (option int))
    "unscoped retry omitted" None
    (Option.map (fun _ -> 1) o.retry);
  let scoped =
    C.http_options
      ~homeserver:(Uriz.of_string_exn "https://hs.example")
      C.http_policy_default
  in
  let retry = Option.get scoped.retry in
  Alcotest.(check int)
    "scoped default retry count" Fetch.Retry.default.max_retries
    retry.max_retries;
  Alcotest.(check bool)
    "scoped POST enabled behind route veto" true
    (List.mem `POST retry.allowed_methods);
  Alcotest.(check bool)
    "scoped request veto installed" true
    (Option.is_some retry.retry_request);
  Alcotest.(check (option (float 0.0001)))
    "rate omitted" None
    (Option.map Duration.to_f o.min_interval);
  Alcotest.(check (option int)) "concurrency omitted" None o.max_concurrent;
  Alcotest.(check (option (float 0.0001)))
    "connect timeout omitted" None
    (Option.map Duration.to_f o.connect_timeout);
  Alcotest.(check (option (float 0.0001)))
    "idle timeout omitted" None
    (Option.map Duration.to_f o.idle_timeout)

let test_mapping () =
  let p =
    {
      C.retries = Some 3;
      rate_limit = Some 2.;
      max_concurrent = Some 4;
      connect_timeout = Some 7.;
      idle_timeout = Some 9.;
    }
  in
  let o =
    C.http_options ~homeserver:(Uriz.of_string_exn "https://hs.example") p
  in
  let retry = Option.get o.retry in
  Alcotest.(check int) "retry count" 3 retry.max_retries;
  Alcotest.(check (float 0.0001))
    "requests/sec to interval" 0.5
    (Duration.to_f (Option.get o.min_interval));
  Alcotest.(check (option int)) "concurrency" (Some 4) o.max_concurrent;
  Alcotest.(check (option (float 0.0001)))
    "connect timeout" (Some 7.)
    (Option.map Duration.to_f o.connect_timeout);
  Alcotest.(check (option (float 0.0001)))
    "idle timeout" (Some 9.)
    (Option.map Duration.to_f o.idle_timeout)

let test_invalid_durations () =
  List.iter
    (fun policy ->
      match C.http_options policy with
      | _ -> Alcotest.fail "invalid duration policy accepted"
      | exception Invalid_argument _ -> ())
    [
      { C.http_policy_default with connect_timeout = Some nan };
      { C.http_policy_default with idle_timeout = Some infinity };
      { C.http_policy_default with connect_timeout = Some Float.max_float };
      { C.http_policy_default with rate_limit = Some 0. };
      { C.http_policy_default with rate_limit = Some nan };
      { C.http_policy_default with rate_limit = Some (Float.next_after 0. 1.) };
      { C.http_policy_default with rate_limit = Some Float.max_float };
    ]

let command = Cmd.v (Cmd.info "policy-test") C.http_policy_term
let eval argv = Cmd.eval_value ~catch:false ~argv command

let test_parse () =
  match eval [| "policy-test"; "--retries"; "3"; "--rate-limit"; "2" |] with
  | Ok (`Ok p) ->
      Alcotest.(check (option int)) "parsed retries" (Some 3) p.C.retries;
      Alcotest.(check (option (float 0.0001)))
        "parsed rate" (Some 2.) p.rate_limit
  | Ok _ -> Alcotest.fail "expected parsed value"
  | Error _ -> Alcotest.fail "valid policy rejected"

let test_reject_invalid () =
  match eval [| "policy-test"; "--max-concurrent"; "0" |] with
  | Error `Parse -> ()
  | _ -> Alcotest.fail "zero max-concurrent accepted"

let test_uri_arguments () =
  let parse = Arg.conv_parser C.uri_conv in
  (match parse "https://HS.example/?return=x%2Fy" with
  | Ok uri ->
      Alcotest.(check string)
        "canonical Uriz value" "https://hs.example/?return=x%2Fy"
        (Uriz.to_string uri)
  | Error _ -> Alcotest.fail "valid URI rejected");
  List.iter
    (fun value ->
      match parse value with
      | Error _ -> ()
      | Ok _ -> Alcotest.failf "malformed URI accepted: %s" value)
    [ "https://hs.example:invalid/"; "https://hs.example/%xx" ]

let test_fake_backend () =
  Eio_main.run @@ fun env ->
  let p =
    {
      C.http_policy_default with
      retries = Some 1;
      max_concurrent = Some 1;
      connect_timeout = Some 1.;
      idle_timeout = Some 1.;
    }
  in
  let o =
    C.http_options ~homeserver:(Uriz.of_string_exn "https://hs.example") p
  in
  let connect ~sw:_ ~host:_ ~port:_ = assert false in
  let https _uri conn = conn in
  ignore
    (Fetch_httpz.std ~connect ~https ?retry:o.retry
       ?max_concurrent:o.max_concurrent ?min_interval:o.min_interval
       ?connect_timeout:o.connect_timeout ?idle_timeout:o.idle_timeout env)

let () =
  Alcotest.run "CLI HTTP policy"
    [
      ( "policy",
        [
          Alcotest.test_case "defaults" `Quick test_defaults;
          Alcotest.test_case "mapping" `Quick test_mapping;
          Alcotest.test_case "invalid durations" `Quick test_invalid_durations;
          Alcotest.test_case "parse" `Quick test_parse;
          Alcotest.test_case "reject invalid" `Quick test_reject_invalid;
          Alcotest.test_case "URI arguments" `Quick test_uri_arguments;
          Alcotest.test_case "fake backend" `Quick test_fake_backend;
        ] );
    ]
