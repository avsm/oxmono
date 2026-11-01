let with_lmtp spec f =
  let previous = Sys.getenv_opt "JMAP_ORACLE_LMTP" in
  Fun.protect
    ~finally:(fun () ->
      Unix.putenv "JMAP_ORACLE_LMTP" (Option.value ~default:"" previous))
    (fun () ->
      Unix.putenv "JMAP_ORACLE_LMTP" spec;
      f ())

let valid_lmtp () =
  List.iter
    (fun (spec, expected) ->
      with_lmtp spec (fun () ->
          Alcotest.(check (pair string int))
            spec expected (Oracle_harness.lmtp ())))
    [
      ("", ("localhost", 18024));
      ("mail.example", ("mail.example", 24));
      ("localhost:18024", ("localhost", 18024));
      ("127.0.0.1:1", ("127.0.0.1", 1));
      ("localhost:65535", ("localhost", 65535));
    ]

let invalid_lmtp () =
  List.iter
    (fun spec ->
      with_lmtp spec (fun () ->
          Alcotest.match_raises spec
            (fun exn ->
              let message = Printexc.to_string exn in
              String.starts_with ~prefix:"Alcotest assertion failure" message
              && List.mem "JMAP_ORACLE_LMTP" (String.split_on_char ' ' message))
            (fun () -> ignore (Oracle_harness.lmtp ()))))
    [
      "localhost:abc";
      "localhost:";
      "localhost:0";
      "localhost:-1";
      "localhost:65536";
      "localhost:9999999999999999999999999999999";
      ":18024";
    ]

let () =
  Alcotest.run "Oracle configuration"
    [
      ( "LMTP",
        [
          Alcotest.test_case "valid listener" `Quick valid_lmtp;
          Alcotest.test_case "invalid listener diagnostic" `Quick invalid_lmtp;
        ] );
    ]
