open Mail_flag

let parse wire =
  match Imap_flag.of_wire wire with
  | Ok flag -> flag
  | Error why -> Alcotest.failf "%S: %s" wire why

let test_identity () =
  let wires =
    [ "\\Seen"; "\\sEeN"; "Seen"; "$seen"; "$Forwarded";
      "\\Recent"; "\\X-Unknown"; "CustomCase" ]
  in
  List.iter
    (fun wire ->
      let flag = parse wire in
      let expected =
        if String.lowercase_ascii wire = "\\seen" then "\\Seen" else wire
      in
      Alcotest.(check string) wire expected (Imap_flag.to_wire flag))
    wires;
  Alcotest.(check bool) "system distinct from keyword" false
    (Imap_flag.equal (parse "\\Seen") (parse "Seen"))

let test_semantics () =
  Alcotest.(check bool) "system seen" true
    (Imap_flag.semantic (parse "\\Seen") = Some `Seen);
  Alcotest.(check bool) "bare seen unresolved" true
    (Imap_flag.semantic (parse "Seen") = None);
  Alcotest.(check bool) "dollar seen unresolved" true
    (Imap_flag.semantic (parse "$seen") = None);
  Alcotest.(check bool) "recent unresolved" true
    (Imap_flag.semantic (parse "\\Recent") = None);
  Alcotest.(check bool) "unknown extension unresolved" true
    (Imap_flag.semantic (parse "\\X-Unknown") = None)

let test_reject_injection () =
  List.iter
    (fun wire ->
      Alcotest.(check bool) wire true (Result.is_error (Imap_flag.of_wire wire)))
    [ ""; "\\"; "\\*"; "x y"; "x\r\nNOOP"; "x]"; "\000" ]

let test_durable_sets () =
  let flags = List.map parse in
  Alcotest.(check bool) "case, order, duplicates and Recent are immaterial" true
    (Imap_flag.equal_durable
       (flags ["$Label"; "\\X-Custom"; "\\Seen"; "$label"; "\\Recent"])
       (flags ["\\Seen"; "\\x-custom"; "$LABEL"]));
  Alcotest.(check bool) "system and keyword remain distinct" false
    (Imap_flag.equal_durable (flags ["\\Seen"]) (flags ["Seen"]));
  Alcotest.(check bool) "different flags remain different" false
    (Imap_flag.equal_durable (flags ["$Label"]) (flags ["$Other"]))

let () =
  Alcotest.run "imap-flag"
    [ ("wire", [ Alcotest.test_case "identity" `Quick test_identity;
                    Alcotest.test_case "durable sets" `Quick test_durable_sets;
                    Alcotest.test_case "semantics" `Quick test_semantics;
                    Alcotest.test_case "reject injection" `Quick test_reject_injection ]) ]
