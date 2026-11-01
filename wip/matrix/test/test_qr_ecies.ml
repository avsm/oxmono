module Ecies = Matrix_client.Qr_login.Msc4108.Ecies
module Ck = Matrix_client.Crypto_key
module R = Matrix_client.Random

let check_string = Alcotest.(check string)
let check_int = Alcotest.(check int)
let check_bool = Alcotest.(check bool)

let rust_initial =
  "692LdQB34g50xGstKM7/HEWU6aBCjht4RLFKXg|pOCSkrZRwni5dyxWn1+puxPZBrRqtoyd+dwrRAn4ogk"

let random bytes = R.of_source (Eio.Flow.string_source bytes)

let channel () =
  let initiator = Ecies.create ~random:(random (String.make 64 'a')) () in
  let recipient = Ecies.create ~random:(random (String.make 64 'b')) () in
  (initiator, recipient)

let unwrap = function
  | Ok value -> value
  | Error error ->
      Alcotest.failf "unexpected ECIES error: %a" Ecies.pp_error error

let test_round_trip () =
  Eio_main.run @@ fun _env ->
  let initiator, recipient = channel () in
  let recipient_key = Ecies.public_key recipient in
  let initiator, initial =
    unwrap
      (Ecies.establish_outbound initiator ~recipient:recipient_key
         ~initial_plaintext:"initial")
  in
  let recipient, plaintext =
    unwrap (Ecies.establish_inbound recipient initial)
  in
  check_string "initial plaintext" "initial" plaintext;
  check_int "check code agrees"
    (Ecies.check_code initiator)
    (Ecies.check_code recipient);
  check_bool "check code is two digits" true
    (Ecies.check_code initiator >= 0 && Ecies.check_code initiator <= 99);
  let message = unwrap (Ecies.encrypt initiator "hello") in
  check_string "reply" "hello" (unwrap (Ecies.decrypt recipient message));
  let reply = unwrap (Ecies.encrypt recipient "back") in
  check_string "reverse direction" "back"
    (unwrap (Ecies.decrypt initiator reply))

let test_wire_shapes_and_malformed () =
  Eio_main.run @@ fun _env ->
  let initiator, recipient = channel () in
  let _, initial =
    unwrap
      (Ecies.establish_outbound initiator
         ~recipient:(Ecies.public_key recipient)
         ~initial_plaintext:"payload")
  in
  let separator = String.index initial '|' in
  check_bool "initial has one separator" true
    (String.index_from_opt initial (separator + 1) '|' = None);
  let recipient = Ecies.create ~random:(random (String.make 64 'b')) () in
  (match Ecies.establish_inbound recipient (initial ^ "|") with
  | Error Ecies.Malformed_initial_message -> ()
  | Error error ->
      Alcotest.failf "wrong malformed error: %a" Ecies.pp_error error
  | Ok _ -> Alcotest.fail "accepted an initial message with two separators");
  (match Ecies.establish_inbound recipient "%%%|%%%" with
  | Error Ecies.Malformed_initial_message -> ()
  | Error error -> Alcotest.failf "wrong base64 error: %a" Ecies.pp_error error
  | Ok _ -> Alcotest.fail "accepted malformed base64");
  let initiator, recipient = channel () in
  let initiator, initial =
    unwrap
      (Ecies.establish_outbound initiator
         ~recipient:(Ecies.public_key recipient)
         ~initial_plaintext:"payload")
  in
  let recipient, _ = unwrap (Ecies.establish_inbound recipient initial) in
  let message = unwrap (Ecies.encrypt initiator "payload") in
  let forged =
    String.init (String.length message) (fun i ->
        if i = 0 then if message.[i] = 'A' then 'B' else 'A' else message.[i])
  in
  (match Ecies.decrypt recipient forged with
  | Error Ecies.Authentication_failed -> ()
  | Error error -> Alcotest.failf "wrong forged error: %a" Ecies.pp_error error
  | Ok _ -> Alcotest.fail "accepted forged ciphertext");
  let next = unwrap (Ecies.encrypt initiator "next") in
  check_string "receive counter advances after authentication failure" "next"
    (unwrap (Ecies.decrypt recipient next));
  let initiator, recipient = channel () in
  let initiator, initial =
    unwrap
      (Ecies.establish_outbound initiator
         ~recipient:(Ecies.public_key recipient)
         ~initial_plaintext:"payload")
  in
  let recipient, _ = unwrap (Ecies.establish_inbound recipient initial) in
  let message = unwrap (Ecies.encrypt initiator "replay") in
  check_string "first replay message" "replay"
    (unwrap (Ecies.decrypt recipient message));
  match Ecies.decrypt recipient message with
  | Error Ecies.Authentication_failed -> ()
  | Error error -> Alcotest.failf "wrong replay error: %a" Ecies.pp_error error
  | Ok _ -> Alcotest.fail "accepted replay"

let test_non_contributory_key () =
  Eio_main.run @@ fun _env ->
  let initiator = Ecies.create ~random:(random (String.make 64 'a')) () in
  let recipient =
    Ck.Curve25519.Public.of_bytes (String.make 32 '\000') |> Result.get_ok
  in
  match
    Ecies.establish_outbound initiator ~recipient ~initial_plaintext:"payload"
  with
  | Error Ecies.Non_contributory_key -> ()
  | Error error ->
      Alcotest.failf "wrong low-order error: %a" Ecies.pp_error error
  | Ok _ -> Alcotest.fail "accepted a low-order public key"

let test_pending_consumption () =
  Eio_main.run @@ fun _env ->
  (* Outbound establishment consumes its pending key even when DH rejects the
     recipient key. *)
  let outbound = Ecies.create ~random:(random (String.make 32 '\003')) () in
  let low_order =
    Ck.Curve25519.Public.of_bytes (String.make 32 '\000') |> Result.get_ok
  in
  (match
     Ecies.establish_outbound outbound ~recipient:low_order
       ~initial_plaintext:"payload"
   with
  | Error Ecies.Non_contributory_key -> ()
  | Error error ->
      Alcotest.failf "wrong outbound low-order error: %a" Ecies.pp_error error
  | Ok _ -> Alcotest.fail "accepted outbound low-order key");
  let recipient = Ecies.create ~random:(random (String.make 32 '\004')) () in
  (match
     Ecies.establish_outbound outbound
       ~recipient:(Ecies.public_key recipient)
       ~initial_plaintext:"payload"
   with
  | Error Ecies.Pending_consumed -> ()
  | Error error ->
      Alcotest.failf "wrong outbound reuse error: %a" Ecies.pp_error error
  | Ok _ -> Alcotest.fail "reused outbound pending channel");
  (* A malformed inbound wire value does not consume the pending key. *)
  let inbound = Ecies.create ~random:(random (String.make 32 '\002')) () in
  (match Ecies.establish_inbound inbound "not a wire message" with
  | Error Ecies.Malformed_initial_message -> ()
  | Error error ->
      Alcotest.failf "wrong malformed error: %a" Ecies.pp_error error
  | Ok _ -> Alcotest.fail "accepted malformed inbound message");
  let _, plaintext = unwrap (Ecies.establish_inbound inbound rust_initial) in
  check_string "inbound remains usable after malformed input" "rust fixture"
    plaintext;
  (* A valid inbound wire value consumes the pending key before a DH failure. *)
  let inbound = Ecies.create ~random:(random (String.make 32 '\002')) () in
  let low_order_wire = "|" ^ Ck.Curve25519.Public.to_base64 low_order in
  (match Ecies.establish_inbound inbound low_order_wire with
  | Error Ecies.Non_contributory_key -> ()
  | Error error ->
      Alcotest.failf "wrong inbound low-order error: %a" Ecies.pp_error error
  | Ok _ -> Alcotest.fail "accepted inbound low-order key");
  (match Ecies.establish_inbound inbound rust_initial with
  | Error Ecies.Pending_consumed -> ()
  | Error error ->
      Alcotest.failf "wrong inbound reuse error: %a" Ecies.pp_error error
  | Ok _ -> Alcotest.fail "reused inbound pending channel");
  (* Authentication is also an establishment attempt, so a valid-key message
     with forged ciphertext spends the pending key. *)
  let inbound = Ecies.create ~random:(random (String.make 32 '\002')) () in
  let forged =
    String.mapi
      (fun index byte ->
        if index = 0 then if byte = 'A' then 'B' else 'A' else byte)
      rust_initial
  in
  (match Ecies.establish_inbound inbound forged with
  | Error Ecies.Authentication_failed -> ()
  | Error error ->
      Alcotest.failf "wrong inbound authentication error: %a" Ecies.pp_error
        error
  | Ok _ -> Alcotest.fail "accepted forged initial ciphertext");
  match Ecies.establish_inbound inbound rust_initial with
  | Error Ecies.Pending_consumed -> ()
  | Error error ->
      Alcotest.failf "wrong authenticated-reuse error: %a" Ecies.pp_error error
  | Ok _ -> Alcotest.fail "reused pending channel after authentication failure"

(* Recorded with an independent Rust algorithm helper matching the pinned
   Matrix Rust SDK's vodozemac 0.9 ECIES implementation.
   The two fixed private keys are 32 bytes of 0x01 (initiator) and 0x02
   (recipient); key derivation and ChaCha20-Poly1305 were performed by the
   Rust helper used while recording this fixture. *)
let test_vodozemac_fixture () =
  Eio_main.run @@ fun _env ->
  let initial = rust_initial in
  let recipient = Ecies.create ~random:(random (String.make 32 '\002')) () in
  let recipient, plaintext =
    unwrap (Ecies.establish_inbound recipient initial)
  in
  check_string "Rust initial plaintext" "rust fixture" plaintext;
  check_string "Rust check code bytes" "+fc"
    (Matrix_proto.Base64.encode (Ecies.check_code_bytes recipient));
  check_int "Rust check code" 97 (Ecies.check_code recipient);
  check_string "Rust recipient ciphertext" "OSQuibFoeSzjYL0C276QLnao/eaWqw0+lEk"
    (unwrap (Ecies.encrypt recipient "rust reply"))

let () =
  Alcotest.run "qr-ecies"
    [
      ( "ECIES",
        [
          Alcotest.test_case "round trip" `Quick test_round_trip;
          Alcotest.test_case "wire and malformed" `Quick
            test_wire_shapes_and_malformed;
          Alcotest.test_case "non-contributory key" `Quick
            test_non_contributory_key;
          Alcotest.test_case "pending consumption" `Quick
            test_pending_consumption;
          Alcotest.test_case "vodozemac fixture" `Quick test_vodozemac_fixture;
        ] );
    ]
