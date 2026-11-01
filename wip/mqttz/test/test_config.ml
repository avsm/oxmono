let test_valid () =
  match Mqttz_config.of_string "client_id = 'test'\ntls = true\n" with
  | Error e -> Alcotest.fail e
  | Ok c ->
      Alcotest.(check int) "TLS port" 8883 c.port;
      Alcotest.(check string) "client id" "test" c.client.client_id

let test_invalid () =
  List.iter
    (fun text ->
      match Mqttz_config.of_string text with
      | Error _ -> ()
      | Ok _ -> Alcotest.failf "accepted %S" text)
    [
      "";
      "client_id='x'\nunknown=1";
      "client_id='x'\nport=0";
      "client_id=\"\\u0000\"";
      "client_id='x'\nusername=\"\\u0000\"";
      "client_id='x'\nversion='3'";
      "client_id='x'\nkeep_alive=-1";
      "client_id='x'\nmessage_capacity=0";
      "client_id='x'\noperation_timeout=nan";
      "client_id='x'\nversion='3.1.1'\npassword='secret'";
    ]

let () =
  Alcotest.run "mqttz config"
    [
      ( "TOML",
        [
          Alcotest.test_case "defaults" `Quick test_valid;
          Alcotest.test_case "strict errors" `Quick test_invalid;
        ] );
    ]
