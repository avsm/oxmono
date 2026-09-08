(** Verification tests: the SAS state machine run end to end between two
    in-process machines, the emoji and decimal tables against the vectors in the
    specification and in vodozemac, the QR binary format, and real cross-signing
    signature chains. *)

module V = Matrix_client.Verification
module Cs = Matrix_client.Cross_signing
module Keys = Matrix_client.Keys
module Crypto_key = Matrix_client.Crypto_key
module Key_id = Crypto_key.Key_id
module Sas = V.Sas
module Qr = V.Qr
module Message = V.Message
module Transaction = V.Transaction
module Cancel_code = V.Cancel_code
module Ev = Matrix_proto.Event
module R = Matrix_client.Random
module User_id = Matrix_proto.Id.User_id
module Device_id = Matrix_proto.Id.Device_id
module Event_id = Matrix_proto.Id.Event_id
module Room_id = Matrix_proto.Id.Room_id
module Txn_id = Matrix_proto.Id.Transaction_id
module Oracle = Vodozemac_oracle_client

(* A [Random.t] over a block of bytes read once from the operating
   system, so the tests need no Eio scheduler. *)
let random =
  let n = 1 lsl 16 in
  let buf = Bytes.create n in
  let ic = open_in_bin "/dev/urandom" in
  Fun.protect
    ~finally:(fun () -> close_in_noerr ic)
    (fun () -> really_input ic buf 0 n);
  R.of_source (Eio.Flow.string_source (Bytes.unsafe_to_string buf))

let b64e s = Base64.encode_string ~pad:false s
let user u = User_id.of_string_exn u
let device d = Device_id.of_string_exn d
let txn_id s = Txn_id.v s
let kid s = Result.get_ok (Key_id.of_string s)

let ok = function
  | Ok v -> v
  | Error (`Msg e) -> Alcotest.failf "unexpected error: %s" e

let ok_jsont = function
  | Ok v -> v
  | Error e -> Alcotest.failf "unexpected JSON codec error: %s" e

let check_string = Alcotest.(check string)
let check_int = Alcotest.(check int)
let check_bool = Alcotest.(check bool)
let jstring = Oracle.jstring
let jint = Oracle.jint
let get_oracle = Oracle.get_oracle
let call_oracle = Oracle.call
let oracle_cmd = Oracle.cmd
let os = Oracle.s
let oi = Oracle.i

(* A tiny substring test, to keep the tests free of extra dependencies. *)
let contains haystack needle =
  let n = String.length needle and h = String.length haystack in
  let rec go i =
    i + n <= h && (String.sub haystack i n = needle || go (i + 1))
  in
  n = 0 || go 0

let json_of_string s =
  match Jsont_bytesrw.decode_string Matrix_proto.Json.Codec.json s with
  | Ok j -> j
  | Error e -> Alcotest.failf "bad JSON: %s" e

let alice = user "@alice:example.com"
let bob = user "@bob:example.com"
let alice_device = device "ALICEDEV"
let bob_device = device "BOBDEV"

(* A fresh Ed25519 key pair for whatever a test needs. *)
let fresh_ed25519 () = Crypto_key.Ed25519.generate ~random ()
let pub_b64 = Crypto_key.Ed25519.Public.to_base64
let fresh_public () = snd (fresh_ed25519 ())

let device_keys ?(signatures = []) ~user_id ~device_id ~keys ~algorithms () =
  {
    Keys.user_id;
    device_id;
    algorithms;
    keys;
    signatures;
    dehydrated = None;
    unsigned = None;
  }

let canonical_json = Matrix_proto.Signed_json.canonical_json
let canonical_json_result = Matrix_proto.Signed_json.canonical_json_result

let test_canonical_json () =
  check_string "sorted, no whitespace" {|{"a":1,"b":[1,2],"c":{"d":null}}|}
    (canonical_json
       (json_of_string {| { "c" : { "d" : null }, "b" : [1, 2], "a" : 1 } |}));
  check_string "escapes" {|{"k":"a\"b\\c\nd"}|}
    (canonical_json (json_of_string {|{"k": "a\"b\\c\nd"}|}));
  check_string "strips signatures and unsigned" {|{"a":1}|}
    (canonical_json
       (Matrix_proto.Signed_json.json_for_signing
          (json_of_string {|{"a":1,"signatures":{"x":{}},"unsigned":{}}|})))

(* JSON has no token for NaN or an infinity; encoding one must not silently
   emit the invalid bareword ["nan"]/["inf"]. *)
let test_canonical_json_rejects_non_finite () =
  List.iter
    (fun f ->
      match canonical_json (Jsont.Json.number f) with
      | (_ : string) -> Alcotest.fail "a non-finite number must not encode"
      | exception Invalid_argument _ -> ())
    [ Float.nan; Float.infinity; Float.neg_infinity ]

let test_canonical_json_matrix_numbers () =
  check_string "safe integer maximum" "9007199254740991"
    (canonical_json (Jsont.Json.number 9_007_199_254_740_991.));
  check_string "safe integer minimum" "-9007199254740991"
    (canonical_json (Jsont.Json.number (-9_007_199_254_740_991.)));
  check_string "negative zero" "0" (canonical_json (Jsont.Json.number (-0.)));
  List.iter
    (fun value ->
      match canonical_json_result (Jsont.Json.number value) with
      | Error (`Msg _) -> ()
      | Ok encoded -> Alcotest.failf "invalid number encoded as %s" encoded)
    [ 0.5; -0.5; 9_007_199_254_740_992.; -9_007_199_254_740_992. ]

let test_canonical_json_rejects_ambiguous_values () =
  let duplicate =
    Jsont.Json.object'
      [
        Jsont.Json.mem (Jsont.Json.name "a") (Jsont.Json.number 1.);
        Jsont.Json.mem (Jsont.Json.name "a") (Jsont.Json.number 2.);
      ]
  in
  let invalid_utf8 = Jsont.Json.string "\xC3" in
  List.iter
    (fun (name, value) ->
      match canonical_json_result value with
      | Error (`Msg _) -> ()
      | Ok encoded -> Alcotest.failf "%s encoded as %s" name encoded)
    [ ("duplicate member", duplicate); ("invalid UTF-8", invalid_utf8) ];
  List.iter
    (fun value ->
      match canonical_json value with
      | _ -> Alcotest.fail "canonical_json accepted an invalid value"
      | exception Invalid_argument _ -> ())
    [ duplicate; invalid_utf8 ]

let test_canonical_json_unicode_order () =
  let value =
    Jsont.Json.object'
      [
        Jsont.Json.mem (Jsont.Json.name "é") (Jsont.Json.number 2.);
        Jsont.Json.mem (Jsont.Json.name "z") (Jsont.Json.number 1.);
      ]
  in
  check_string "UTF-8 byte-wise key order" {|{"z":1,"é":2}|}
    (canonical_json value)

(* The known answer from matrix-rust-sdk's
   [verification::sas::helpers::commitment_calculation]. *)
let test_commitment_vector () =
  let start =
    json_of_string
      {|{"from_device":"XOWLHHFSWM",
         "transaction_id":"bYxBsirjUJO9osar6ST4i2M2NjrYLA7l",
         "method":"m.sas.v1",
         "key_agreement_protocols":["curve25519-hkdf-sha256","curve25519"],
         "hashes":["sha256"],
         "message_authentication_codes":["hkdf-hmac-sha256","hmac-sha256"],
         "short_authentication_string":["decimal","emoji"]}|}
  in
  let public_key =
    ok
      (Crypto_key.Curve25519.Public.of_base64
         "Q/NmNFEUS1fS+YeEmiZkjjblKTitrKOAk7cPEumcMlg")
  in
  check_string "commitment" "CCQmB4JCdB0FW21FdAnHj/Hu8+W9+Nb0vgwPEnZZQ4g"
    (Sas.commitment ~public_key ~start_json:start)

let test_emoji_table () =
  check_int "64 entries" 64 (Array.length Sas.emoji_table);
  Array.iteri
    (fun i e -> check_int "numbered in order" i e.Sas.number)
    Sas.emoji_table;
  let first = Sas.emoji_table.(0) and last = Sas.emoji_table.(63) in
  check_string "first symbol" "\xf0\x9f\x90\xb6" first.Sas.symbol;
  check_string "first description" "Dog" first.Sas.description;
  check_string "last symbol" "\xf0\x9f\x93\x8c" last.Sas.symbol;
  check_string "last description" "Pin" last.Sas.description;
  (* Every description is distinct: the users have to tell them apart. *)
  let names =
    List.sort_uniq String.compare
      (Array.to_list (Array.map (fun e -> e.Sas.description) Sas.emoji_table))
  in
  check_int "distinct descriptions" 64 (List.length names)

(* vodozemac's [emoji_generation] and [decimal_generation] vectors. *)
let test_sas_bytes_vectors () =
  let zeros = String.make 6 '\x00' and ones = String.make 6 '\xff' in
  Alcotest.(check (list int))
    "all zero bytes give index 0" [ 0; 0; 0; 0; 0; 0; 0 ]
    (Sas.emoji_indices zeros);
  Alcotest.(check (list int))
    "all one bytes give index 63"
    [ 63; 63; 63; 63; 63; 63; 63 ]
    (Sas.emoji_indices ones);
  let d0 = Sas.decimals_of_bytes zeros and d1 = Sas.decimals_of_bytes ones in
  Alcotest.(check (triple int int int)) "zero decimals" (1000, 1000, 1000) d0;
  Alcotest.(check (triple int int int)) "one decimals" (9191, 9191, 9191) d1;
  List.iter
    (fun e -> check_string "index 0 is the dog" "Dog" e.Sas.description)
    (Sas.emoji_of_bytes zeros);
  List.iter
    (fun e -> check_string "index 63 is the pin" "Pin" e.Sas.description)
    (Sas.emoji_of_bytes ones)

let now = Ev.Timestamp.of_ms 1_700_000_000_000L
let after ms = Ev.Timestamp.of_ms (Int64.add (Ev.Timestamp.to_ms now) ms)
let txn = Transaction.to_device (txn_id "txn-1")

let identities () =
  ( Sas.identity ~user_id:alice ~device_id:alice_device
      ~device_key:(fresh_public ()) ~master_key:(fresh_public ()) (),
    Sas.identity ~user_id:bob ~device_id:bob_device
      ~device_key:(fresh_public ()) ~master_key:(fresh_public ()) () )

let one_message what (o : Sas.step) =
  match o.Sas.send with
  | [ m ] -> m
  | _ -> Alcotest.failf "expected a single %s" what

let start_content_of (o : Sas.step) =
  match Message.payload (one_message "m.key.verification.start" o) with
  | Message.Start c -> c
  | _ -> Alcotest.fail "expected a single m.key.verification.start"

let cancel_code_of (o : Sas.step) =
  match Message.payload (one_message "m.key.verification.cancel" o) with
  | Message.Cancel c ->
      Cancel_code.of_string (Ev.Key_verification_cancel_content.code c)
  | _ -> Alcotest.fail "expected a single m.key.verification.cancel"

(* Deliver messages back and forth until both sides go quiet. [a_out] are
   the messages a still owes b, [b_out] the other way round. *)
let pump a b a_out b_out =
  let rec loop a b a_out b_out fuel =
    if fuel = 0 then Alcotest.fail "verification did not settle"
    else
      match (a_out, b_out) with
      | [], [] -> (a, b)
      | m :: rest, _ ->
          let o = Sas.handle b ~now m in
          loop a o.Sas.sas rest (b_out @ o.Sas.send) (fuel - 1)
      | [], m :: rest ->
          let o = Sas.handle a ~now m in
          loop o.Sas.sas b (a_out @ o.Sas.send) rest (fuel - 1)
  in
  loop a b a_out b_out 64

(* Run a whole flow. [starter_is_alice] picks which side sends the start,
   so the same code exercises both roles. *)
let run_flow ?mac_methods ?(expect_mac = Sas.Mac_method.Hkdf_hmac_sha256_v2)
    ~starter_is_alice () =
  let a_id, b_id = identities () in
  let ours, theirs = if starter_is_alice then (a_id, b_id) else (b_id, a_id) in
  let s =
    Sas.start ~random ~now ~transaction:txn ~ours ~theirs ?mac_methods ()
  in
  let start = start_content_of s in
  let r =
    Sas.from_start ~random ~now ~transaction:txn ~ours:theirs ~theirs:ours start
  in
  let starter, responder = pump s.Sas.sas r.Sas.sas [] r.Sas.send in
  check_bool "starter reached the SAS" true (Sas.stage starter = Sas.Sas_ready);
  check_bool "responder reached the SAS" true
    (Sas.stage responder = Sas.Sas_ready);
  (* Both sides show the same short authentication string. *)
  let emoji t =
    List.map (fun e -> e.Sas.description) (Option.get (Sas.emoji t))
  in
  Alcotest.(check (list string)) "same emoji" (emoji starter) (emoji responder);
  check_int "seven emoji" 7 (List.length (emoji starter));
  Alcotest.(check (triple int int int))
    "same decimals"
    (Option.get (Sas.decimals starter))
    (Option.get (Sas.decimals responder));
  check_bool "negotiated MAC method" true
    (Sas.Mac_method.equal expect_mac (Sas.mac_method starter));
  check_bool "both sides agree on it" true
    (Sas.Mac_method.equal expect_mac (Sas.mac_method responder));
  (* The users say the strings match. *)
  let cs = Sas.confirm starter and cr = Sas.confirm responder in
  let starter, responder = pump cs.Sas.sas cr.Sas.sas cs.Sas.send cr.Sas.send in
  check_bool "starter done" true (Sas.is_done starter);
  check_bool "responder done" true (Sas.is_done responder);
  (* Each side verified the other's device key and master key. *)
  let ids l =
    List.sort compare (List.map (fun (k, _) -> Key_id.to_string k) l)
  in
  let verified t = ids (Sas.verified_keys t) in
  let expect i = ids (Sas.identity_keys i) in
  Alcotest.(check (list string))
    "starter verified their keys" (expect theirs) (verified starter);
  Alcotest.(check (list string))
    "responder verified their keys" (expect ours) (verified responder)

let test_sas_alice_starts () = run_flow ~starter_is_alice:true ()
let test_sas_bob_starts () = run_flow ~starter_is_alice:false ()

let test_sas_single_method method_name () =
  let a_id, b_id = identities () in
  let s = Sas.start ~random ~now ~transaction:txn ~ours:a_id ~theirs:b_id () in
  let start = start_content_of s in
  let r =
    Sas.from_start ~random ~now ~transaction:txn ~ours:b_id ~theirs:a_id start
  in
  let accept =
    match Message.payload (one_message "accept" r) with
    | Message.Accept c -> c
    | _ -> Alcotest.fail "expected an accept"
  in
  let accept =
    {
      accept with
      Ev.Key_verification_accept_content.short_authentication_string =
        [ method_name ];
    }
  in
  Alcotest.(check (list string))
    "accepts only the offered SAS method" [ method_name ]
    (Ev.Key_verification_accept_content.short_authentication_string accept);
  let accept_message = Message.v txn (Message.Accept accept) in
  let starter, _responder = pump s.Sas.sas r.Sas.sas [] [ accept_message ] in
  check_bool "starter reached the SAS" true (Sas.stage starter = Sas.Sas_ready);
  let has_emoji t = Option.is_some (Sas.emoji t) in
  let has_decimals t = Option.is_some (Sas.decimals t) in
  check_bool "emoji availability follows negotiation" (method_name = "emoji")
    (has_emoji starter);
  check_bool "decimal availability follows negotiation"
    (method_name = "decimal") (has_decimals starter);
  check_bool "unnegotiated method is unavailable" false
    (if method_name = "emoji" then has_decimals starter else has_emoji starter)

let test_sas_decimal_only () = test_sas_single_method "decimal" ()
let test_sas_emoji_only () = test_sas_single_method "emoji" ()

(* The same flow with a peer that only knows the deprecated MAC method,
   which reproduces libolm's broken base64. *)
let test_sas_flow_with_v1_mac () =
  run_flow
    ~mac_methods:[ Sas.Mac_method.Hkdf_hmac_sha256 ]
    ~expect_mac:Sas.Mac_method.Hkdf_hmac_sha256 ~starter_is_alice:true ()

(* Keep this small independent calculation in the test so the live vodozemac
   oracle can also exercise arbitrary inputs.  The production helper is
   intentionally private: protocol users only need [start]/[confirm]. *)
let test_mac_base64_libolm mac =
  let out = Buffer.create 43 in
  Buffer.add_string out (b64e (String.sub mac 0 3));
  let bytes_from_mac = ref 2 in
  List.iter
    (fun i ->
      let from_mac = String.sub mac (i - !bytes_from_mac) !bytes_from_mac in
      let current = Buffer.contents out in
      let take = 3 - !bytes_from_mac in
      let from_out = String.sub current (String.length current - take) take in
      Buffer.add_string out (b64e (from_out ^ from_mac));
      decr bytes_from_mac)
    [ 6; 9 ];
  List.iter
    (fun i ->
      let current = Buffer.contents out in
      Buffer.add_string out (b64e (String.sub current i 3)))
    [ 9; 12; 15; 18; 21; 24; 27 ];
  let current = Buffer.contents out in
  Buffer.add_string out (b64e (String.sub current 30 2));
  Buffer.contents out

let test_mac ~shared ~legacy ~info input =
  let prk = Hkdf.extract ~hash:`SHA256 ~salt:"" shared in
  let key = Hkdf.expand ~hash:`SHA256 ~prk ~info 32 in
  let raw = Digestif.SHA256.(to_raw_string (hmac_string ~key input)) in
  if legacy then test_mac_base64_libolm raw else b64e raw

let test_sas_vodozemac_oracle o =
  let run method_ ~legacy =
    let created = call_oracle o (oracle_cmd "sas_create" []) in
    let sas_handle = jint created "sas" in
    let oracle_public =
      ok (Crypto_key.Curve25519.Public.of_base64 (jstring created "public_key"))
    in
    let ours, theirs = identities () in
    let start =
      Sas.start ~random ~now ~transaction:txn ~ours ~theirs
        ~mac_methods:[ method_ ] ()
    in
    let start_message = one_message "start" start in
    let start_content = start_content_of start in
    let start_json = ok (Message.to_json start_message) in
    let responder =
      Sas.from_start ~random ~now ~transaction:txn ~ours:theirs ~theirs:ours
        start_content
    in
    let accept =
      match Message.payload (one_message "accept" responder) with
      | Message.Accept content -> content
      | _ -> Alcotest.fail "expected an accept"
    in
    (* Replace the responder's ephemeral key with the oracle's key while
       retaining a valid commitment.  This makes the real OCaml state and
       the external implementation share exactly one DH secret. *)
    let accept =
      {
        accept with
        Ev.Key_verification_accept_content.commitment =
          Sas.commitment ~public_key:oracle_public ~start_json;
      }
    in
    let first =
      Sas.handle start.sas ~now (Message.v txn (Message.Accept accept))
    in
    let our_key = one_message "key" first in
    let our_public =
      match Message.payload our_key with
      | Message.Key content ->
          ok
            (Crypto_key.Curve25519.Public.of_base64
               (Ev.Key_verification_key_content.key content))
      | _ -> Alcotest.fail "expected our key"
    in
    let established =
      call_oracle o
        (oracle_cmd "sas_establish"
           [
             ("sas", oi sas_handle);
             ( "public_key",
               os (Crypto_key.Curve25519.Public.to_base64 our_public) );
           ])
    in
    let established_handle = jint established "established" in
    let oracle_key =
      Message.v txn
        (Message.Key
           (Ev.Key_verification_key_content.make
              ~transaction_id:(txn_id "txn-1")
              ~key:(Crypto_key.Curve25519.Public.to_base64 oracle_public)
              ()))
    in
    let ready = Sas.handle first.sas ~now oracle_key in
    check_bool "oracle key establishes SAS" true
      (Sas.stage ready.sas = Sas.Sas_ready);
    let mac_step = Sas.confirm ready.sas in
    let mac_content =
      match Message.payload (one_message "mac" mac_step) with
      | Message.Mac content -> content
      | _ -> Alcotest.fail "expected a MAC"
    in
    (* Use a separate explicitly-held OCaml key pair for arbitrary vectors;
       the flow's ephemeral secret is intentionally private to [Sas]. *)
    let vector_secret, vector_public =
      Crypto_key.Curve25519.generate ~random ()
    in
    let vector_created = call_oracle o (oracle_cmd "sas_create" []) in
    let vector_sas = jint vector_created "sas" in
    let vector_oracle_public =
      ok
        (Crypto_key.Curve25519.Public.of_base64
           (jstring vector_created "public_key"))
    in
    let vector_established =
      call_oracle o
        (oracle_cmd "sas_establish"
           [
             ("sas", oi vector_sas);
             ( "public_key",
               os (Crypto_key.Curve25519.Public.to_base64 vector_public) );
           ])
    in
    let vector_established = jint vector_established "established" in
    let vector_shared =
      ok
        (Crypto_key.Curve25519.key_exchange ~secret:vector_secret
           ~public:vector_oracle_public)
    in
    let vectors =
      [
        ("", "");
        ("ascii input", "ascii info");
        ("héllö 🐇", "π info");
        (String.make 4096 'x', String.make 257 'i');
      ]
    in
    List.iter
      (fun (input, info) ->
        let expected =
          jstring
            (call_oracle o
               (oracle_cmd "sas_mac"
                  [
                    ("established", oi vector_established);
                    ("input", os input);
                    ("info", os info);
                    ("legacy", if legacy then "true" else "false");
                  ]))
            "mac"
        in
        (* The independent OCaml calculation is deliberately checked against
           every oracle vector, including the historical broken encoding. *)
        check_string "vodozemac SAS MAC vector"
          (test_mac ~shared:vector_shared ~legacy ~info input)
          expected)
      vectors;
    let info =
      "MATRIX_KEY_VERIFICATION_MAC"
      ^ User_id.to_string ours.user_id
      ^ Device_id.to_string ours.device_id
      ^ User_id.to_string theirs.user_id
      ^ Device_id.to_string theirs.device_id
      ^ Transaction.id txn
    in
    let key_macs = Ev.Key_verification_mac_content.mac mac_content in
    List.iter
      (fun (key_id, actual) ->
        let key =
          match
            List.find_opt
              (fun (id, _) -> String.equal (Key_id.to_string id) key_id)
              (Sas.identity_keys ours)
          with
          | Some (_, key) -> key
          | None -> Alcotest.failf "flow MAC has unknown key %S" key_id
        in
        let expected =
          jstring
            (call_oracle o
               (oracle_cmd "sas_mac"
                  [
                    ("established", oi established_handle);
                    ("input", os key);
                    ("info", os (info ^ key_id));
                    ("legacy", if legacy then "true" else "false");
                  ]))
            "mac"
        in
        check_string "flow MAC agrees with vodozemac" expected actual)
      key_macs;
    let key_ids = List.sort String.compare (List.map fst key_macs) in
    let expected_keys =
      jstring
        (call_oracle o
           (oracle_cmd "sas_mac"
              [
                ("established", oi established_handle);
                ("input", os (String.concat "," key_ids));
                ("info", os (info ^ "KEY_IDS"));
                ("legacy", if legacy then "true" else "false");
              ]))
        "mac"
    in
    check_string "flow key-id MAC agrees with vodozemac" expected_keys
      (Ev.Key_verification_mac_content.keys mac_content)
  in
  run Sas.Mac_method.Hkdf_hmac_sha256_v2 ~legacy:false;
  run Sas.Mac_method.Hkdf_hmac_sha256 ~legacy:true

let test_sas_vodozemac_oracle () =
  match get_oracle () with
  | None -> Printf.printf "SKIP SAS oracle: vodozemac-oracle not built\n%!"
  | Some o -> test_sas_vodozemac_oracle o

let test_sas_mismatched_commitment () =
  let a_id, b_id = identities () in
  let s = Sas.start ~random ~now ~transaction:txn ~ours:a_id ~theirs:b_id () in
  let start = start_content_of s in
  let r =
    Sas.from_start ~random ~now ~transaction:txn ~ours:b_id ~theirs:a_id start
  in
  (* Corrupt the commitment Bob sent before Alice sees it. *)
  let accept =
    match Message.payload (one_message "accept" r) with
    | Message.Accept c -> c
    | _ -> Alcotest.fail "expected an accept"
  in
  let tampered =
    Message.v txn
      (Message.Accept
         {
           accept with
           Ev.Key_verification_accept_content.commitment =
             b64e (String.make 32 '\x00');
         })
  in
  let a1 = Sas.handle s.Sas.sas ~now tampered in
  check_bool "alice sent her key" true (Sas.stage a1.Sas.sas = Sas.Key_sent);
  (* Bob answers Alice's key with his own; the commitment check fails. *)
  let b1 = Sas.handle r.Sas.sas ~now (one_message "key" a1) in
  let a2 = Sas.handle a1.Sas.sas ~now (one_message "bob's key" b1) in
  check_string "cancelled with m.mismatched_commitment"
    "m.mismatched_commitment"
    (Cancel_code.to_string (cancel_code_of a2));
  check_bool "cancelled" true (Sas.is_cancelled a2.Sas.sas)

let test_sas_unknown_method () =
  let a_id, b_id = identities () in
  (* A start that offers nothing we can agree to. *)
  let start =
    Ev.Key_verification_start_content.make
      ~from_device:(Device_id.to_string alice_device)
      ~method_:"m.sas.v1" ~transaction_id:(txn_id "txn-1")
      ~key_agreement_protocols:[ "curve25519" ] ~hashes:[ "sha512" ]
      ~message_authentication_codes:[ "hmac-sha256" ]
      ~short_authentication_string:[ "decimal" ] ()
  in
  let r =
    Sas.from_start ~random ~now ~transaction:txn ~ours:b_id ~theirs:a_id start
  in
  check_string "m.unknown_method" "m.unknown_method"
    (Cancel_code.to_string (cancel_code_of r));
  (* And a method we do not implement at all. *)
  let start' =
    Ev.Key_verification_start_content.make
      ~from_device:(Device_id.to_string alice_device)
      ~method_:"org.example.wibble" ~transaction_id:(txn_id "txn-1") ()
  in
  let r' =
    Sas.from_start ~random ~now ~transaction:txn ~ours:b_id ~theirs:a_id start'
  in
  check_string "unknown method name" "m.unknown_method"
    (Cancel_code.to_string (cancel_code_of r'))

let test_sas_mac_v1_fallback () =
  let a_id, b_id = identities () in
  (* A peer that only knows the deprecated MAC method. *)
  let start =
    Ev.Key_verification_start_content.make
      ~from_device:(Device_id.to_string alice_device)
      ~method_:"m.sas.v1" ~transaction_id:(txn_id "txn-1")
      ~key_agreement_protocols:[ "curve25519-hkdf-sha256" ]
      ~hashes:[ "sha256" ] ~message_authentication_codes:[ "hkdf-hmac-sha256" ]
      ~short_authentication_string:[ "decimal"; "emoji" ] ()
  in
  let r =
    Sas.from_start ~random ~now ~transaction:txn ~ours:b_id ~theirs:a_id start
  in
  check_bool "falls back to v1" true
    (Sas.Mac_method.equal Sas.Mac_method.Hkdf_hmac_sha256
       (Sas.mac_method r.Sas.sas));
  match Message.payload (one_message "accept" r) with
  | Message.Accept c ->
      check_string "accept names the v1 MAC" "hkdf-hmac-sha256"
        (Ev.Key_verification_accept_content.message_authentication_code c)
  | _ -> Alcotest.fail "expected an accept"

let test_sas_unexpected_message () =
  let a_id, b_id = identities () in
  let s = Sas.start ~random ~now ~transaction:txn ~ours:a_id ~theirs:b_id () in
  (* A key before the accept is out of order. *)
  let key =
    Message.v txn
      (Message.Key
         (Ev.Key_verification_key_content.make ~transaction_id:(txn_id "txn-1")
            ~key:(b64e (String.make 32 '\x01'))
            ()))
  in
  let o = Sas.handle s.Sas.sas ~now key in
  check_string "m.unexpected_message" "m.unexpected_message"
    (Cancel_code.to_string (cancel_code_of o))

let test_sas_timeout () =
  let a_id, b_id = identities () in
  let s = Sas.start ~random ~now ~transaction:txn ~ours:a_id ~theirs:b_id () in
  let o = Sas.tick s.Sas.sas ~now:(after 600_001L) in
  check_string "m.timeout" "m.timeout"
    (Cancel_code.to_string (cancel_code_of o));
  let quiet = Sas.tick s.Sas.sas ~now:(after 1000L) in
  check_int "no message before the timeout" 0 (List.length quiet.Sas.send);
  (* A shorter timeout is honoured. *)
  let short =
    Sas.start ~random ~now ~timeout:1000L ~transaction:txn ~ours:a_id
      ~theirs:b_id ()
  in
  check_string "the given timeout" "m.timeout"
    (Cancel_code.to_string
       (cancel_code_of (Sas.tick short.Sas.sas ~now:(after 1001L))))

let test_sas_user_cancel () =
  let a_id, b_id = identities () in
  let s = Sas.start ~random ~now ~transaction:txn ~ours:a_id ~theirs:b_id () in
  let o = Sas.cancel s.Sas.sas Cancel_code.User in
  check_string "m.user" "m.user" (Cancel_code.to_string (cancel_code_of o));
  (* The other side records the cancellation without answering it. *)
  let r =
    Sas.from_start ~random ~now ~transaction:txn ~ours:b_id ~theirs:a_id
      (start_content_of s)
  in
  let seen = Sas.handle r.Sas.sas ~now (one_message "cancel" o) in
  check_int "no reply to a cancel" 0 (List.length seen.Sas.send);
  Alcotest.(check (option string))
    "records the code" (Some "m.user")
    (Option.map Cancel_code.to_string (Sas.cancel_code seen.Sas.sas))

let test_message_to_device_roundtrip () =
  let m = Message.cancel txn Cancel_code.User in
  check_string "event type" "m.key.verification.cancel" (Message.event_type m);
  let json = ok (Message.to_json m) in
  let m' = ok (Message.of_json ~event_type:"m.key.verification.cancel" json) in
  check_bool "same transaction" true
    (Transaction.equal (Message.transaction m') txn);
  match Message.payload m' with
  | Message.Cancel c ->
      check_string "code" "m.user" (Ev.Key_verification_cancel_content.code c);
      check_string "reason"
        (Cancel_code.reason Cancel_code.User)
        (Ev.Key_verification_cancel_content.reason c)
  | _ -> Alcotest.fail "expected a cancel"

let start_with_next_method c =
  Ev.Key_verification_start_content.make
    ~from_device:(Ev.Key_verification_start_content.from_device c)
    ~method_:(Ev.Key_verification_start_content.method_ c)
    ?transaction_id:(Ev.Key_verification_start_content.transaction_id c)
    ~next_method:"m.sas.v1"
    ?key_agreement_protocols:
      (Ev.Key_verification_start_content.key_agreement_protocols c)
    ?hashes:(Ev.Key_verification_start_content.hashes c)
    ?message_authentication_codes:
      (Ev.Key_verification_start_content.message_authentication_codes c)
    ?short_authentication_string:
      (Ev.Key_verification_start_content.short_authentication_string c)
    ?secret:(Ev.Key_verification_start_content.secret c)
    ?relates_to:(Ev.Key_verification_start_content.relates_to c)
    ()

let test_next_method_compatibility () =
  let a_id, b_id = identities () in
  let sas_start =
    Sas.start ~random ~now ~transaction:txn ~ours:a_id ~theirs:b_id ()
  in
  let with_next = start_with_next_method (start_content_of sas_start) in
  let json =
    ok_jsont
      (Jsont.Json.encode Ev.Key_verification_start_content.jsont with_next)
  in
  let decoded =
    ok_jsont (Jsont.Json.decode Ev.Key_verification_start_content.jsont json)
  in
  check_string "next method survives the codec" "m.sas.v1"
    (Option.get (Ev.Key_verification_start_content.next_method decoded));
  let lookup ~user_id ~device_id =
    if User_id.equal user_id alice && Device_id.equal device_id alice_device
    then Some a_id
    else None
  in
  let sas_flow = V.Flow.create () in
  let sas_step =
    V.Flow.handle sas_flow ~random ~now ~ours:b_id ~lookup ~sender:alice
      (Message.v txn (Message.Start decoded))
  in
  let sas_session = Option.get sas_step.V.Flow.session in
  check_bool "next method does not prevent SAS" true
    (Option.is_some (V.Flow.session_sas sas_session));
  (match sas_step.V.Flow.send with
  | [ m ] -> (
      match Message.payload m with
      | Message.Accept _ -> ()
      | _ -> Alcotest.fail "next method changed the SAS response")
  | _ -> Alcotest.fail "expected one SAS accept");
  let qr_flow = V.Flow.create () in
  let qr_request =
    V.Flow.request ~random ~now ~from_device:bob_device ~their_user_id:alice
      ~devices:[ Matrix_client.To_device.Device alice_device ]
      qr_flow
  in
  let qr_session = qr_request.V.Flow.session in
  let qr =
    ok
      (Qr.for_other_user ~random ~flow_id:"txn-qr"
         ~our_master_key:(pub_b64 (fresh_public ()))
         ~their_master_key:(pub_b64 (fresh_public ())))
  in
  V.Flow.show_qr qr_session qr;
  let qr_message =
    Qr.reciprocate_start
      ~transaction:(V.Flow.session_transaction qr_session)
      ~from_device:alice_device qr
  in
  let qr_content =
    match Message.payload qr_message with
    | Message.Start c -> start_with_next_method c
    | _ -> Alcotest.fail "expected a QR start"
  in
  let qr_step =
    V.Flow.handle qr_flow ~random ~now ~ours:b_id ~lookup ~sender:alice
      (Message.v
         (V.Flow.session_transaction qr_session)
         (Message.Start qr_content))
  in
  check_bool "next method does not prevent QR reciprocation" true
    (match V.Flow.session_stage qr_session with
    | V.Flow.Done -> true
    | _ -> false);
  match qr_step.V.Flow.send with
  | [ m ] -> (
      match Message.payload m with
      | Message.Done _ -> ()
      | _ -> Alcotest.fail "next method changed the QR response")
  | _ -> Alcotest.fail "expected one QR done"

let room = Room_id.of_string_exn "!room:example.com"

let test_message_in_room_roundtrip () =
  let t =
    Transaction.in_room ~room_id:room ~event_id:(Event_id.of_string_exn "$abc")
  in
  let m = Message.done_ t in
  let json = ok (Message.to_json m) in
  check_string "carries m.relates_to"
    {|{"m.relates_to":{"event_id":"$abc","rel_type":"m.reference"}}|}
    (canonical_json json);
  let m' =
    ok
      (Message.of_json ~event_type:"m.key.verification.done" ~room_id:room json)
  in
  check_bool "same transaction" true
    (Transaction.equal (Message.transaction m') t);
  check_string "flow id is stable across the round trip" (Transaction.id t)
    (Transaction.id (Message.transaction m'));
  (* Two different rooms sharing the same event id are different flows, and
     their identifiers must disagree even though the bare event id is the
     same. *)
  let other_room = Room_id.of_string_exn "!other:example.com" in
  let other_t =
    Transaction.in_room ~room_id:other_room
      ~event_id:(Event_id.of_string_exn "$abc")
  in
  check_bool "different rooms are different flows" false
    (Transaction.equal t other_t);
  check_bool "different rooms have different flow ids" false
    (String.equal (Transaction.id t) (Transaction.id other_t));
  (* A room follow-up must use m.reference. A transaction_id belongs to the
     to-device transport and must never make a room event cross transports. *)
  let to_device_ready =
    Message.ready txn ~from_device:alice_device ~methods:[ V.Method.Sas_v1 ]
    |> Message.to_json |> ok
  in
  (match
     Message.of_json ~event_type:"m.key.verification.ready" ~room_id:room
       to_device_ready
   with
  | Ok _ -> Alcotest.fail "a room ready accepted a to-device transaction id"
  | Error (`Msg _) -> ());
  let to_device_request =
    V.request_to_device ~random ~now ~from_device:alice_device
      ~their_user_id:bob
      ~devices:[ Matrix_client.To_device.All ]
      ()
  in
  let request_json = ok (Message.to_json to_device_request.message) in
  match
    Message.of_json ~event_type:"m.key.verification.request" ~room_id:room
      request_json
  with
  | Ok _ -> Alcotest.fail "a to-device request was accepted as a room event"
  | Error (`Msg _) -> ()

let test_request_to_device_fanout () =
  let r =
    V.request_to_device ~random ~now ~from_device:alice_device
      ~their_user_id:bob
      ~devices:
        [
          Matrix_client.To_device.Device (device "DEV1");
          Matrix_client.To_device.Device (device "DEV2");
        ]
      ()
  in
  check_string "event type" "m.key.verification.request"
    (Message.event_type r.V.message);
  (match r.V.to_device with
  | [ (u, devices) ] ->
      check_string "addressed to bob" "@bob:example.com" (User_id.to_string u);
      check_int "two devices" 2 (List.length devices);
      Alcotest.(check (list string))
        "device ids" [ "DEV1"; "DEV2" ]
        (List.map
           (fun (d, _) ->
             match d with
             | Matrix_client.To_device.All -> "*"
             | Matrix_client.To_device.Device d -> Device_id.to_string d)
           devices)
  | _ -> Alcotest.fail "expected one user in the to-device map");
  match Message.payload r.V.message with
  | Message.Request c ->
      Alcotest.(check (list string))
        "advertises every method"
        [
          "m.sas.v1";
          "m.qr_code.show.v1";
          "m.qr_code.scan.v1";
          "m.reciprocate.v1";
        ]
        (Ev.Key_verification_request_content.methods c);
      Alcotest.(check (option int64))
        "timestamped"
        (Some (Ev.Timestamp.to_ms now))
        (Option.map Ev.Timestamp.to_ms
           (Ev.Key_verification_request_content.timestamp c))
  | _ -> Alcotest.fail "expected a request"

let test_request_in_room () =
  let c =
    V.request_in_room ~from_device:alice_device ~their_user_id:bob
      ~methods:[ V.Method.Sas_v1 ] ()
  in
  let json =
    match
      Jsont.Json.encode Ev.Key_verification_request_message_content.jsont c
    with
    | Ok j -> j
    | Error e -> Alcotest.failf "unexpected error: %s" e
  in
  let s = canonical_json json in
  check_bool "carries the msgtype" true
    (contains s {|"msgtype":"m.key.verification.request"|});
  check_bool "names the recipient" true (contains s {|"to":"@bob:example.com"|});
  (* The flow id is the event id of the request, which the server assigns
     and the caller supplies. *)
  (match Message.of_json ~event_type:"m.room.message" ~room_id:room json with
  | Ok _ -> Alcotest.fail "an in-room request needs its event id"
  | Error (`Msg _) -> ());
  let m =
    ok
      (Message.of_json ~event_type:"m.room.message" ~room_id:room
         ~event_id:(Event_id.of_string_exn "$request")
         json)
  in
  check_bool "flow id is stable" true
    (String.equal
       (Transaction.id
          (Transaction.in_room ~room_id:room
             ~event_id:(Event_id.of_string_exn "$request")))
       (Transaction.id (Message.transaction m)));
  (* An ordinary message is not a verification request. *)
  match
    Message.of_json ~event_type:"m.room.message" ~room_id:room
      ~event_id:(Event_id.of_string_exn "$request")
      (json_of_string {|{"msgtype":"m.text","body":"hi"}|})
  with
  | Ok _ -> Alcotest.fail "an m.text message is not a verification request"
  | Error (`Msg _) -> ()

let test_flow_tracks_in_room_request () =
  let content =
    V.request_in_room ~from_device:alice_device ~their_user_id:bob
      ~methods:[ V.Method.Sas_v1 ] ()
  in
  let event_id = Event_id.of_string_exn "$request" in
  let flow = V.Flow.create () in
  let request =
    V.Flow.request_in_room ~now ~from_device:alice_device ~room_id:room
      ~event_id ~their_user_id:bob ~content flow
  in
  check_bool "tracked transaction is in-room" true
    (Transaction.equal
       (Transaction.in_room ~room_id:room ~event_id)
       (V.Flow.session_transaction request.session));
  check_int "in-room request has no to-device sends" 0
    (List.length request.to_device);
  check_string "in-room request event type" "m.room.message"
    (Message.event_type request.message);
  check_bool "session is retained in the flow" true
    (V.Flow.find flow
       (Transaction.id (V.Flow.session_transaction request.session))
    <> None)

let test_ready_response () =
  match
    V.ready_response ~transaction:txn ~from_device:bob_device
      ~our_methods:V.Method.all
      ~their_methods:[ V.Method.Sas_v1; V.Method.Other "org.example.other" ]
  with
  | Error c -> Alcotest.failf "unexpected cancel %s" (Cancel_code.to_string c)
  | Ok (common, msg) -> (
      Alcotest.(check (list string))
        "only the shared method" [ "m.sas.v1" ]
        (List.map V.Method.to_string common);
      match Message.payload msg with
      | Message.Ready c ->
          check_string "from device" "BOBDEV"
            (Ev.Key_verification_ready_content.from_device c)
      | _ -> Alcotest.fail "expected a ready")

let test_ready_no_common_method () =
  match
    V.ready_response ~transaction:txn ~from_device:bob_device
      ~our_methods:[ V.Method.Sas_v1 ]
      ~their_methods:[ V.Method.Other "org.example.other" ]
  with
  | Ok _ -> Alcotest.fail "expected a cancel"
  | Error c ->
      check_string "m.unknown_method" "m.unknown_method"
        (Cancel_code.to_string c)

let key_a = b64e (String.init 32 (fun i -> Char.chr i))
let key_b = b64e (String.init 32 (fun i -> Char.chr (0x40 + i)))

let test_qr_roundtrip () =
  List.iter
    (fun mode ->
      let qr =
        ok
          (Qr.make ~mode ~flow_id:"$flow:example.com" ~first_key:key_a
             ~second_key:key_b ~shared_secret:(b64e "SECRET!!"))
      in
      let bytes = ok (Qr.encode qr) in
      check_string "header" "MATRIX" (String.sub bytes 0 6);
      check_int "version byte" 2 (Char.code bytes.[6]);
      check_int "mode byte" (Qr.mode_to_int mode) (Char.code bytes.[7]);
      let len = String.length "$flow:example.com" in
      check_int "length high byte" (len lsr 8) (Char.code bytes.[8]);
      check_int "length low byte" (len land 0xff) (Char.code bytes.[9]);
      check_int "total length" (10 + len + 64 + 8) (String.length bytes);
      let qr' = ok (Qr.decode bytes) in
      check_bool "round trips" true (Qr.equal qr qr');
      check_string "first key" key_a (Qr.first_key qr');
      check_string "second key" key_b (Qr.second_key qr');
      check_string "flow id" "$flow:example.com" (Qr.flow_id qr');
      Alcotest.(check (option int))
        "the mode byte names the mode"
        (Some (Qr.mode_to_int mode))
        (Option.map Qr.mode_to_int (Qr.mode_of_int (Qr.mode_to_int mode))))
    [
      Qr.Verifying_another_user;
      Qr.Self_verifying_master_key_trusted;
      Qr.Self_verifying_master_key_untrusted;
    ]

let test_qr_generators () =
  let qr =
    ok
      (Qr.for_other_user ~random ~flow_id:"$f" ~our_master_key:key_a
         ~their_master_key:key_b)
  in
  check_int "mode 0" 0 (Qr.mode_to_int (Qr.mode qr));
  let qr =
    ok
      (Qr.for_self_trusted ~random ~flow_id:"$f" ~master_key:key_a
         ~their_device_key:key_b)
  in
  check_int "mode 1" 1 (Qr.mode_to_int (Qr.mode qr));
  let qr =
    ok
      (Qr.for_self_untrusted ~random ~flow_id:"$f" ~our_device_key:key_a
         ~master_key:key_b)
  in
  check_int "mode 2" 2 (Qr.mode_to_int (Qr.mode qr));
  check_int "8 byte secret" 8 (String.length (Qr.shared_secret_raw qr));
  check_bool "pp keeps the secret to itself" false
    (contains (Format.asprintf "%a" Qr.pp qr) (Qr.shared_secret qr))

let test_qr_rejects_bad_input () =
  let good =
    ok
      (Qr.encode
         (ok
            (Qr.make ~mode:Qr.Verifying_another_user ~flow_id:"FLOW_ID"
               ~first_key:key_a ~second_key:key_b
               ~shared_secret:(b64e "SHARED_SECRET"))))
  in
  let bad_header = "XATRIX" ^ String.sub good 6 (String.length good - 6) in
  let patch s i c = String.mapi (fun j x -> if i = j then c else x) s in
  let expect_error what s =
    match Qr.decode s with
    | Ok _ -> Alcotest.failf "%s: expected a decoding failure" what
    | Error (`Msg _) -> ()
  in
  expect_error "bad header" bad_header;
  expect_error "bad version" (patch good 6 '\x03');
  expect_error "unknown mode" (patch good 7 '\x09');
  expect_error "truncated" (String.sub good 0 (String.length good - 10));
  expect_error "too short" "MATRIX";
  (* A secret shorter than 8 bytes is refused. *)
  expect_error "short secret" (String.sub good 0 (10 + 7 + 64 + 3));
  (* But the good one still decodes. *)
  let qr = ok (Qr.decode good) in
  check_string "secret survives" (b64e "SHARED_SECRET") (Qr.shared_secret qr)

let test_qr_check_and_reciprocate () =
  let our_msk = pub_b64 (fresh_public ()) in
  let their_msk = pub_b64 (fresh_public ()) in
  let our_dev = pub_b64 (fresh_public ()) in
  (* Bob shows a mode 0 code: his master key first, what he thinks Alice's
     master key is second. Alice scans it. *)
  let shown =
    ok
      (Qr.for_other_user ~random ~flow_id:"$flow" ~our_master_key:their_msk
         ~their_master_key:our_msk)
  in
  let scanned = ok (Qr.decode (ok (Qr.encode shown))) in
  let check ~their_master_key =
    Qr.check scanned ~flow_id:"$flow" ~our_master_key:(Some our_msk)
      ~our_device_key:(Some our_dev) ~their_master_key ~their_device_key:None
  in
  check_bool "keys match" true (check ~their_master_key:(Some their_msk) = Ok ());
  (match check ~their_master_key:(Some our_msk) with
  | Error c ->
      check_string "m.key_mismatch" "m.key_mismatch" (Cancel_code.to_string c)
  | Ok () -> Alcotest.fail "expected a key mismatch");
  (match
     Qr.check scanned ~flow_id:"$other" ~our_master_key:(Some our_msk)
       ~our_device_key:None ~their_master_key:(Some their_msk)
       ~their_device_key:None
   with
  | Error c ->
      check_string "wrong flow" "m.unknown_transaction"
        (Cancel_code.to_string c)
  | Ok () -> Alcotest.fail "expected a flow mismatch");
  (* Alice tells Bob she scanned it, echoing the secret. *)
  let start =
    Qr.reciprocate_start ~transaction:txn ~from_device:alice_device scanned
  in
  (match Message.payload start with
  | Message.Start c ->
      check_string "method" "m.reciprocate.v1"
        (Ev.Key_verification_start_content.method_ c);
      check_bool "secret matches" true
        (Qr.check_reciprocate shown
           ~secret:(Option.get (Ev.Key_verification_start_content.secret c))
        = Ok ())
  | _ -> Alcotest.fail "expected a start");
  match Qr.check_reciprocate shown ~secret:(b64e "not-it!!") with
  | Ok () -> Alcotest.fail "expected a secret mismatch"
  | Error c ->
      check_string "m.key_mismatch" "m.key_mismatch" (Cancel_code.to_string c)

let cross_signing_identity user_id =
  let priv = Cs.create_private_identity ~user_id in
  Cs.generate_private_keys ~random priv;
  match Cs.build_upload priv with
  | Some u -> (priv, u)
  | None -> Alcotest.fail "could not build the upload"

let secret what = function
  | Some k -> k
  | None -> Alcotest.failf "no %s key" what

let test_cross_signing_chain () =
  let priv, upload = cross_signing_identity alice in
  let master = Cs.key ~role:Cs.Master upload.Cs.master_key in
  let self_signing = Cs.key ~role:Cs.Self_signing upload.Cs.self_signing_key in
  let user_signing = Cs.key ~role:Cs.User_signing upload.Cs.user_signing_key in
  check_bool "the roles are recorded" true (Cs.role master = Cs.Master);
  check_bool "master signed the self-signing key" true
    (Cs.verify_key ~signer:master ~signed:self_signing);
  check_bool "master signed the user-signing key" true
    (Cs.verify_key ~signer:master ~signed:user_signing);
  (* The self-signing key signs a device. *)
  let device_key = pub_b64 (fresh_public ()) in
  let curve_key = b64e (String.make 32 '\x07') in
  let dev =
    Cs.create_device
      (device_keys ~user_id:alice ~device_id:alice_device
         ~keys:
           [
             (kid "ed25519:ALICEDEV", device_key);
             (kid "curve25519:ALICEDEV", curve_key);
           ]
         ~algorithms:[ "m.olm.v1.curve25519-aes-sha2"; "m.megolm.v1.aes-sha2" ]
         ())
  in
  check_bool "unsigned device is not trusted" false
    (Cs.verify_device_signature ~self_signing_key:self_signing ~device:dev);
  let signed =
    Cs.sign_device
      ~signer:(secret "self-signing" (Cs.self_signing_secret priv))
      ~signer_user_id:alice dev
  in
  check_bool "signed device verifies" true
    (Cs.verify_device_signature ~self_signing_key:self_signing ~device:signed);
  check_bool "update_device_trust records it" true
    (Cs.update_device_trust ~self_signing_key:self_signing signed);
  check_bool "and it is readable afterwards" true
    (Cs.device_cross_signing_trusted signed);
  check_bool "and is_device_verified follows" true
    (Cs.is_device_verified signed);
  (* Tampering with any signed field breaks the signature. *)
  let tampered =
    Cs.create_device
      {
        (Cs.device_keys signed) with
        Keys.keys = [ (kid "ed25519:ALICEDEV", curve_key) ];
      }
  in
  check_bool "tampered device fails" false
    (Cs.verify_device_signature ~self_signing_key:self_signing ~device:tampered);
  let wrong_sig =
    Cs.create_device
      {
        (Cs.device_keys signed) with
        Keys.signatures =
          [
            ( alice,
              [
                ( kid "ed25519:nope",
                  ok (Crypto_key.Signature.of_bytes (String.make 64 '\x00')) );
              ] );
          ];
      }
  in
  check_bool "wrong signature fails" false
    (Cs.verify_device_signature ~self_signing_key:self_signing ~device:wrong_sig);
  (* Local trust is an independent route to the same answer. *)
  let local =
    Cs.create_device
      (device_keys ~user_id:alice ~device_id:alice_device
         ~keys:[ (kid "ed25519:ALICEDEV", device_key) ]
         ~algorithms:[] ())
  in
  check_bool "untrusted" false (Cs.is_device_verified local);
  Cs.set_device_local_trust local Cs.Verified;
  check_bool "locally trusted" true (Cs.is_device_verified local)

let test_cross_signing_other_user () =
  let alice_priv, alice_up = cross_signing_identity alice in
  let bob_priv, bob_up = cross_signing_identity bob in
  let ours =
    Cs.own_identity ~user_id:alice
      ~master_key:(Cs.key ~role:Cs.Master alice_up.Cs.master_key)
      ~self_signing_key:
        (Cs.key ~role:Cs.Self_signing alice_up.Cs.self_signing_key)
      ~user_signing_key:
        (Cs.key ~role:Cs.User_signing alice_up.Cs.user_signing_key)
      ()
  in
  let unsigned_bob =
    Cs.other_identity ~user_id:bob
      ~master_key:(Cs.key ~role:Cs.Master bob_up.Cs.master_key)
      ~self_signing_key:
        (Cs.key ~role:Cs.Self_signing bob_up.Cs.self_signing_key)
      ()
  in
  check_bool "not trusted before we sign" false
    (Cs.verify_master_trust ~ours ~theirs:unsigned_bob);
  (* Alice's user-signing key signs Bob's master key. *)
  let signed_bob_master =
    Cs.sign_cross_signing_key
      ~signer:(secret "user-signing" (Cs.user_signing_secret alice_priv))
      ~signer_user_id:alice bob_up.Cs.master_key
  in
  let theirs =
    Cs.other_identity ~user_id:bob
      ~master_key:(Cs.key ~role:Cs.Master signed_bob_master)
      ~self_signing_key:
        (Cs.key ~role:Cs.Self_signing bob_up.Cs.self_signing_key)
      ()
  in
  check_bool "the whole chain checks out" true
    (Cs.verify_master_trust ~ours ~theirs);
  check_bool "and so does is_other_identity_verified" true
    (Cs.is_other_identity_verified
       ~our_user_signing_key:
         (Cs.key ~role:Cs.User_signing alice_up.Cs.user_signing_key)
       theirs);
  (* One of Bob's devices, signed by his self-signing key. *)
  let device_key = pub_b64 (fresh_public ()) in
  let dev =
    Cs.create_device
      (device_keys ~user_id:bob ~device_id:bob_device
         ~keys:[ (kid "ed25519:BOBDEV", device_key) ]
         ~algorithms:[ "m.megolm.v1.aes-sha2" ] ())
  in
  let dev =
    Cs.sign_device
      ~signer:(secret "self-signing" (Cs.self_signing_secret bob_priv))
      ~signer_user_id:bob dev
  in
  check_bool "device trusted through the chain" true
    (Cs.verify_device_trust_chain ~ours ~theirs ~device:dev);
  (* Pinning notices a master key change. *)
  Cs.pin_master_key theirs;
  check_bool "unchanged" false (Cs.has_identity_changed theirs);
  check_bool "a blacklisted device is never verified" false
    (Cs.set_device_local_trust dev Cs.Blacklisted;
     Cs.is_device_verified dev)

(* A caller refreshing another user's identity from a fresh [/keys/query]
   response builds a new [other_identity] every time; the pin only survives
   the refresh if the caller carries it forward explicitly. *)
let test_cross_signing_identity_change () =
  let _, bob_up = cross_signing_identity bob in
  let master = Cs.key ~role:Cs.Master bob_up.Cs.master_key in
  let self_signing = Cs.key ~role:Cs.Self_signing bob_up.Cs.self_signing_key in
  let theirs =
    Cs.other_identity ~user_id:bob ~master_key:master
      ~self_signing_key:self_signing ()
  in
  Cs.pin_master_key theirs;
  check_bool "unchanged before any refresh" false
    (Cs.has_identity_changed theirs);
  (* A refresh with the same master key carries the pin forward and still
     reports unchanged. *)
  let refreshed_same =
    Cs.other_identity ~user_id:bob ~master_key:master
      ~self_signing_key:self_signing
      ?pinned_master_key:(Cs.pinned_master_key theirs)
      ()
  in
  check_bool "still unchanged after a same-key refresh" false
    (Cs.has_identity_changed refreshed_same);
  (* A refresh with a rotated (attacker-substituted) master key is caught. *)
  let _, rotated_up = cross_signing_identity bob in
  let refreshed_rotated =
    Cs.other_identity ~user_id:bob
      ~master_key:(Cs.key ~role:Cs.Master rotated_up.Cs.master_key)
      ~self_signing_key:
        (Cs.key ~role:Cs.Self_signing rotated_up.Cs.self_signing_key)
      ?pinned_master_key:(Cs.pinned_master_key theirs)
      ()
  in
  check_bool "a rotated master key is detected" true
    (Cs.has_identity_changed refreshed_rotated)

let test_cross_signing_wrong_user () =
  let _, alice_up = cross_signing_identity alice in
  let bob_priv, _ = cross_signing_identity bob in
  (* Bob's self-signing key signing one of Alice's devices must not make
     that device trusted for Alice. *)
  let device_key = pub_b64 (fresh_public ()) in
  let dev =
    Cs.create_device
      (device_keys ~user_id:alice ~device_id:alice_device
         ~keys:[ (kid "ed25519:ALICEDEV", device_key) ]
         ~algorithms:[] ())
  in
  let dev =
    Cs.sign_device
      ~signer:(secret "self-signing" (Cs.self_signing_secret bob_priv))
      ~signer_user_id:bob dev
  in
  check_bool "a signature from the wrong user does not count" false
    (Cs.verify_device_signature
       ~self_signing_key:
         (Cs.key ~role:Cs.Self_signing alice_up.Cs.self_signing_key)
       ~device:dev)

let test_cross_signing_private_import () =
  let master, expected_master = fresh_ed25519 () in
  let self_signing, expected_self_signing = fresh_ed25519 () in
  let user_signing, expected_user_signing = fresh_ed25519 () in
  let seed ~pad secret =
    Base64.encode_string ~pad (Crypto_key.Ed25519.Private.to_bytes secret)
  in
  let imported =
    match
      Cs.private_identity_of_secrets ~user_id:alice ~expected_master
        ~expected_self_signing ~expected_user_signing
        ~master:(Some (seed ~pad:true master))
        ~self_signing:(Some (seed ~pad:false self_signing))
        ~user_signing:(Some (seed ~pad:true user_signing))
    with
    | Ok identity -> identity
    | Error error ->
        Alcotest.failf "full private identity import failed: %a"
          Cs.pp_private_identity_import_error error
  in
  Alcotest.(check (option string))
    "full import preserves master"
    (Some (Crypto_key.Ed25519.Private.to_bytes master))
    (Option.map Crypto_key.Ed25519.Private.to_bytes (Cs.master_secret imported));
  Alcotest.(check (option string))
    "full import preserves self-signing"
    (Some (Crypto_key.Ed25519.Private.to_bytes self_signing))
    (Option.map Crypto_key.Ed25519.Private.to_bytes
       (Cs.self_signing_secret imported));
  Alcotest.(check (option string))
    "full import preserves user-signing"
    (Some (Crypto_key.Ed25519.Private.to_bytes user_signing))
    (Option.map Crypto_key.Ed25519.Private.to_bytes
       (Cs.user_signing_secret imported));
  let partial =
    match
      Cs.private_identity_of_secrets ~user_id:alice ~expected_master
        ~expected_self_signing ~expected_user_signing
        ~master:(Some (seed ~pad:false master))
        ~self_signing:None ~user_signing:None
    with
    | Ok identity -> identity
    | Error error ->
        Alcotest.failf "partial private identity import failed: %a"
          Cs.pp_private_identity_import_error error
  in
  check_bool "partial import has the supplied key" true
    (Option.is_some (Cs.master_secret partial));
  check_bool "partial import leaves self-signing absent" true
    (Option.is_none (Cs.self_signing_secret partial));
  check_bool "partial import leaves user-signing absent" true
    (Option.is_none (Cs.user_signing_secret partial))

let test_cross_signing_private_import_rejects_bad_secrets () =
  let master, expected_master = fresh_ed25519 () in
  let self_signing, expected_self_signing = fresh_ed25519 () in
  let user_signing, expected_user_signing = fresh_ed25519 () in
  let seed secret =
    Base64.encode_string ~pad:false (Crypto_key.Ed25519.Private.to_bytes secret)
  in
  let import ?master ?self_signing ?user_signing () =
    Cs.private_identity_of_secrets ~user_id:alice ~expected_master
      ~expected_self_signing ~expected_user_signing ~master ~self_signing
      ~user_signing
  in
  let check_invalid role result =
    match result with
    | Error (Cs.Invalid_secret (got, _)) ->
        check_bool "invalid secret role" true (got = role)
    | Error error ->
        Alcotest.failf "expected an invalid-secret error, got %a"
          Cs.pp_private_identity_import_error error
    | Ok _ -> Alcotest.fail "invalid secret unexpectedly imported"
  in
  check_invalid Cs.Master (import ~master:"not base64!" ());
  check_invalid Cs.Self_signing (import ~self_signing:"AQ" ());
  check_invalid Cs.User_signing (import ~user_signing:"AQ" ());
  let check_mismatch role result =
    match result with
    | Error (Cs.Public_key_mismatch got) ->
        check_bool "mismatched secret role" true (got = role)
    | Error error ->
        Alcotest.failf "expected a public-key mismatch, got %a"
          Cs.pp_private_identity_import_error error
    | Ok _ -> Alcotest.fail "mismatched secret unexpectedly imported"
  in
  (* Each case also supplies another valid key: a failure must not return a
     usable prefix of the identity. *)
  check_mismatch Cs.Master
    (import ~master:(seed self_signing) ~self_signing:(seed self_signing) ());
  check_mismatch Cs.Self_signing
    (import ~master:(seed master) ~self_signing:(seed user_signing) ());
  check_mismatch Cs.User_signing
    (import ~master:(seed master) ~user_signing:(seed self_signing) ());
  match
    import ~master:(seed master) ~self_signing:"AQ"
      ~user_signing:(seed user_signing) ()
  with
  | Error (Cs.Invalid_secret (Cs.Self_signing, _)) -> ()
  | Error error ->
      Alcotest.failf "expected atomic invalid self-signing import, got %a"
        Cs.pp_private_identity_import_error error
  | Ok _ -> Alcotest.fail "atomic import returned a partial identity"

let test_flow_routes_two_transactions () =
  let flow = V.Flow.create () in
  let a_id, b_id = identities () in
  let lookup ~user_id ~device_id =
    ignore user_id;
    ignore device_id;
    Some a_id
  in
  (* Two concurrent starts from Alice, with different transaction ids. *)
  let start_from id =
    let o =
      Sas.start ~random ~now
        ~transaction:(Transaction.to_device (txn_id id))
        ~ours:a_id ~theirs:b_id ()
    in
    one_message "start" o
  in
  let m1 = start_from "one" and m2 = start_from "two" in
  let o1 =
    V.Flow.handle flow ~random ~now ~ours:b_id ~lookup ~sender:alice m1
  in
  let o2 =
    V.Flow.handle flow ~random ~now ~ours:b_id ~lookup ~sender:alice m2
  in
  check_int "two sessions" 2 (List.length (V.Flow.sessions flow));
  check_int "each answered with an accept" 1 (List.length o1.V.Flow.send);
  check_int "each answered with an accept" 1 (List.length o2.V.Flow.send);
  let id_of s = Transaction.id (Transaction.to_device (txn_id s)) in
  check_bool "session one is running SAS" true
    (Option.is_some
       (Option.bind (V.Flow.find flow (id_of "one")) V.Flow.session_sas));
  check_bool "session two is running SAS" true
    (Option.is_some
       (Option.bind (V.Flow.find flow (id_of "two")) V.Flow.session_sas));
  (* An event for a transaction we never saw is refused. *)
  let stray =
    Message.v
      (Transaction.to_device (txn_id "three"))
      (Message.Key
         (Ev.Key_verification_key_content.make ~transaction_id:(txn_id "three")
            ~key:(b64e (String.make 32 '\x02'))
            ()))
  in
  let o3 =
    V.Flow.handle flow ~random ~now ~ours:b_id ~lookup ~sender:alice stray
  in
  (match o3.V.Flow.send with
  | [ m ] -> (
      match Message.payload m with
      | Message.Cancel c ->
          check_string "m.unknown_transaction" "m.unknown_transaction"
            (Ev.Key_verification_cancel_content.code c)
      | _ -> Alcotest.fail "expected an unknown-transaction cancel")
  | _ -> Alcotest.fail "expected an unknown-transaction cancel");
  (* Stale sessions time out. *)
  let cancels = V.Flow.tick flow ~now:(after 600_001L) in
  check_int "both timed out" 2 (List.length cancels)

let test_flow_bounds_inbound_requests () =
  let flow = V.Flow.create () in
  let _alice_identity, bob_identity = identities () in
  let lookup ~user_id:_ ~device_id:_ = Some (fst (identities ())) in
  let start id =
    one_message "start"
      (Sas.start ~random ~now
         ~transaction:(Transaction.to_device (txn_id id))
         ~ours:(fst (identities ()))
         ~theirs:bob_identity ())
  in
  for i = 1 to V.Flow.max_active_inbound_per_device do
    let result =
      V.Flow.handle flow ~random ~now ~ours:bob_identity ~lookup ~sender:alice
        (start (Printf.sprintf "bounded-%d" i))
    in
    check_bool "an inbound flow under the per-device cap is tracked" true
      (Option.is_some result.V.Flow.session);
    check_int "no capacity cancellation below the cap" 0
      (List.length (V.Flow.take_pending_sends flow))
  done;
  let rejected =
    V.Flow.handle flow ~random ~now ~ours:bob_identity ~lookup ~sender:alice
      (start "bounded-overflow")
  in
  let rejected_session = Option.get rejected.V.Flow.session in
  (match V.Flow.session_stage rejected_session with
  | V.Flow.Cancelled V.Cancel_code.User -> ()
  | _ -> Alcotest.fail "the over-cap inbound flow was not cancelled");
  check_int "the over-cap request emits one cancellation" 1
    (List.length (V.Flow.take_pending_sends flow));
  check_int "the cap does not evict existing inbound flows"
    V.Flow.max_active_inbound_per_device
    (List.length (V.Flow.sessions flow))

let test_flow_bounds_global_inbound_requests () =
  let flow = V.Flow.create () in
  let _alice_identity, bob_identity = identities () in
  let attackers =
    List.init (V.Flow.max_active_inbound + 1) (fun i ->
        let user = user (Printf.sprintf "@attacker-%03d:example.com" i) in
        let device = device (Printf.sprintf "ATTACKER%03d" i) in
        let identity =
          Sas.identity ~user_id:user ~device_id:device
            ~device_key:(fresh_public ()) ~master_key:(fresh_public ()) ()
        in
        (user, device, identity))
  in
  let lookup ~user_id ~device_id =
    List.find_map
      (fun (candidate_user, candidate_device, identity) ->
        if
          User_id.equal user_id candidate_user
          && Device_id.equal device_id candidate_device
        then Some identity
        else None)
      attackers
  in
  let start (user, _device, identity) id =
    ( user,
      one_message "start"
        (Sas.start ~random ~now
           ~transaction:(Transaction.to_device (txn_id id))
           ~ours:identity ~theirs:bob_identity ()) )
  in
  List.iteri
    (fun i (sender, message) ->
      let result =
        V.Flow.handle flow ~random ~now ~ours:bob_identity ~lookup ~sender
          message
      in
      if i < V.Flow.max_active_inbound then
        check_bool "a distinct inbound flow under the global cap is tracked"
          true
          (Option.is_some result.V.Flow.session)
      else
        let session = Option.get result.V.Flow.session in
        match V.Flow.session_stage session with
        | V.Flow.Cancelled V.Cancel_code.User -> ()
        | _ -> Alcotest.fail "the global inbound cap did not cancel overflow")
    (List.mapi
       (fun i attacker -> start attacker (Printf.sprintf "global-%03d" i))
       attackers);
  check_int "the global cap does not evict existing inbound flows"
    V.Flow.max_active_inbound
    (List.length (V.Flow.sessions flow));
  check_int "the global overflow emits one cancellation" 1
    (List.length (V.Flow.take_pending_sends flow))

let test_flow_rejects_sender_mismatch () =
  let flow = V.Flow.create () in
  let a_id, b_id = identities () in
  let lookup ~user_id ~device_id =
    ignore user_id;
    ignore device_id;
    Some a_id
  in
  let start =
    one_message "start"
      (Sas.start ~random ~now
         ~transaction:(Transaction.to_device (txn_id "hijack"))
         ~ours:a_id ~theirs:b_id ())
  in
  let o1 =
    V.Flow.handle flow ~random ~now ~ours:b_id ~lookup ~sender:alice start
  in
  let session = Option.get o1.V.Flow.session in
  check_bool "session belongs to alice" true
    (User_id.equal (V.Flow.session_their_user_id session) alice);
  let mallory = user "@mallory:example.com" in
  let forged =
    Message.v
      (Transaction.to_device (txn_id "hijack"))
      (Message.Key
         (Ev.Key_verification_key_content.make ~transaction_id:(txn_id "hijack")
            ~key:(b64e (String.make 32 '\x03'))
            ()))
  in
  let o2 =
    V.Flow.handle flow ~random ~now ~ours:b_id ~lookup ~sender:mallory forged
  in
  check_bool "the forged event is not routed to alice's session" true
    (Option.is_none o2.V.Flow.session);
  (match o2.V.Flow.send with
  | [ m ] -> (
      match Message.payload m with
      | Message.Cancel c ->
          check_string "cancelled as m.user_mismatch" "m.user_mismatch"
            (Ev.Key_verification_cancel_content.code c)
      | _ -> Alcotest.fail "expected a user-mismatch cancel")
  | _ -> Alcotest.fail "expected a user-mismatch cancel");
  (* Alice's session is untouched by Mallory's forged event. *)
  check_bool "alice's session still belongs to alice" true
    (User_id.equal (V.Flow.session_their_user_id session) alice)

let test_flow_request_and_ready () =
  let flow = V.Flow.create () in
  let a_id, b_id = identities () in
  let lookup ~user_id ~device_id =
    if User_id.equal user_id alice && Device_id.equal device_id alice_device
    then Some a_id
    else None
  in
  let request =
    Ev.Key_verification_request_content.make
      ~from_device:(Device_id.to_string alice_device)
      ~methods:[ "m.sas.v1"; "m.qr_code.show.v1" ]
      ~transaction_id:(txn_id "req") ~timestamp:now ()
  in
  let msg =
    Message.v (Transaction.to_device (txn_id "req")) (Message.Request request)
  in
  let o =
    V.Flow.handle flow ~random ~now ~ours:b_id ~lookup ~sender:alice msg
  in
  let session = Option.get o.V.Flow.session in
  check_int "no reply yet" 0 (List.length o.V.Flow.send);
  Alcotest.(check (option string))
    "records their device" (Some "ALICEDEV")
    (Option.map Device_id.to_string (V.Flow.session_their_device_id session));
  Alcotest.(check (list string))
    "records their methods"
    [ "m.sas.v1"; "m.qr_code.show.v1" ]
    (List.map V.Method.to_string (V.Flow.session_their_methods session));
  (* Answering it offers only what both sides support. *)
  let answered =
    V.Flow.accept session ~from_device:bob_device
      ~our_methods:[ V.Method.Sas_v1 ]
  in
  check_int "one ready to send" 1 (List.length answered.V.Flow.send);
  check_bool "and the session is ready" true
    (V.Flow.session_stage session = V.Flow.Ready [ V.Method.Sas_v1 ])

let test_flow_request_validation () =
  let a_id, b_id = identities () in
  let known ~user_id ~device_id =
    if User_id.equal user_id alice && Device_id.equal device_id alice_device
    then Some a_id
    else None
  in
  let unknown ~user_id:_ ~device_id:_ = None in
  let request timestamp =
    let content =
      Ev.Key_verification_request_content.make
        ~from_device:(Device_id.to_string alice_device)
        ~methods:[ "m.sas.v1" ] ~transaction_id:(txn_id "validated") ?timestamp
        ()
    in
    Message.v
      (Transaction.to_device (txn_id "validated"))
      (Message.Request content)
  in
  let deliver ?event_timestamp ~lookup flow message =
    V.Flow.handle flow ~random ~now ?event_timestamp ~ours:b_id ~lookup
      ~sender:alice message
  in
  let flow = V.Flow.create () in
  check_bool "unknown request device is ignored" true
    (Option.is_none (deliver ~lookup:unknown flow (request (Some now))).session);
  check_int "unknown device creates no session" 0
    (List.length (V.Flow.sessions flow));
  let stale =
    Ev.Timestamp.of_ms (Int64.sub (Ev.Timestamp.to_ms now) 600_001L)
  in
  let future =
    Ev.Timestamp.of_ms (Int64.add (Ev.Timestamp.to_ms now) 300_001L)
  in
  List.iter
    (fun (label, timestamp) ->
      check_bool label true
        (Option.is_none (deliver ~lookup:known flow (request timestamp)).session))
    [
      ("missing request timestamp is ignored", None);
      ("stale request timestamp is ignored", Some stale);
      ("future request timestamp is ignored", Some future);
    ];
  let room_content =
    V.request_in_room ~from_device:alice_device ~their_user_id:bob
      ~methods:[ V.Method.Sas_v1 ] ()
  in
  let room_message =
    Message.v
      (Transaction.in_room ~room_id:room
         ~event_id:(Event_id.of_string_exn "$validated-room"))
      (Message.Request_in_room room_content)
  in
  check_bool "room request without an envelope timestamp is ignored" true
    (Option.is_none (deliver ~lookup:known flow room_message).session);
  check_bool "stale room request is ignored" true
    (Option.is_none
       (deliver ~event_timestamp:stale ~lookup:known flow room_message).session);
  let first = deliver ~event_timestamp:now ~lookup:known flow room_message in
  let first_session = Option.get first.session in
  let duplicate =
    deliver ~event_timestamp:now ~lookup:known flow room_message
  in
  check_bool "fresh room request is accepted" true
    (Option.is_some first.session);
  check_bool "duplicate request retains the active session" true
    (match duplicate.session with
    | Some duplicate_session -> duplicate_session == first_session
    | None -> false)

let test_flow_competing_requests () =
  let a_id, b_id = identities () in
  let lookup ~user_id ~device_id =
    if User_id.equal user_id alice && Device_id.equal device_id alice_device
    then Some a_id
    else None
  in
  let deliver ?event_timestamp flow message =
    V.Flow.handle flow ~random ~now ?event_timestamp ~ours:b_id ~lookup
      ~sender:alice message
  in
  let to_device_request id =
    let content =
      Ev.Key_verification_request_content.make
        ~from_device:(Device_id.to_string alice_device)
        ~methods:[ "m.sas.v1" ] ~transaction_id:(txn_id id) ~timestamp:now ()
    in
    Message.v (Transaction.to_device (txn_id id)) (Message.Request content)
  in
  let room_request room_id event_id =
    let content =
      V.request_in_room ~from_device:alice_device ~their_user_id:bob
        ~methods:[ V.Method.Sas_v1 ] ()
    in
    Message.v
      (Transaction.in_room ~room_id ~event_id:(Event_id.of_string_exn event_id))
      (Message.Request_in_room content)
  in
  let check_cancelled label session =
    match V.Flow.session_stage session with
    | V.Flow.Cancelled code ->
        check_string label "m.user" (Cancel_code.to_string code)
    | _ -> Alcotest.failf "%s: expected a cancelled session" label
  in
  let check_pending label flow first second =
    let pending = V.Flow.take_pending_sends flow in
    check_int (label ^ " cancellation count") 2 (List.length pending);
    let by_id =
      List.map
        (fun (session, message) ->
          let transaction = V.Flow.session_transaction session in
          check_string
            (label ^ " message/session transaction")
            (Transaction.id transaction)
            (Transaction.id (Message.transaction message));
          (match Message.payload message with
          | Message.Cancel content ->
              check_string
                (label ^ " cancellation code")
                "m.user"
                (Ev.Key_verification_cancel_content.code content)
          | _ -> Alcotest.failf "%s: expected cancellation messages" label);
          (Transaction.id transaction, Transaction.room_id transaction))
        pending
    in
    let find id = List.assoc id by_id in
    check_bool
      (label ^ " first transport")
      true
      (Option.equal Room_id.equal
         (find (Transaction.id first))
         (Transaction.room_id first));
    check_bool
      (label ^ " second transport")
      true
      (Option.equal Room_id.equal
         (find (Transaction.id second))
         (Transaction.room_id second))
  in
  let check_incoming label first second =
    let flow = V.Flow.create () in
    let first_step = deliver ~event_timestamp:now flow first in
    let first_session = Option.get first_step.session in
    check_int
      (label ^ " first has no cancellation")
      0
      (List.length (V.Flow.take_pending_sends flow));
    let second_step = deliver ~event_timestamp:now flow second in
    let second_session = Option.get second_step.session in
    check_cancelled (label ^ " old request") first_session;
    check_cancelled (label ^ " new request") second_session;
    check_pending label flow
      (Message.transaction first)
      (Message.transaction second);
    let duplicate = deliver ~event_timestamp:now flow second in
    check_bool
      (label ^ " duplicate retains new session")
      true
      (match duplicate.session with
      | Some session -> session == second_session
      | None -> false);
    check_int
      (label ^ " duplicate emits nothing")
      0
      (List.length (V.Flow.take_pending_sends flow))
  in
  check_incoming "to-device"
    (to_device_request "competition-one")
    (to_device_request "competition-two");
  let other_room = Room_id.of_string_exn "!other:example.com" in
  check_incoming "in-room"
    (room_request room "$competition-room-one")
    (room_request other_room "$competition-room-two");
  check_incoming "mixed transport"
    (to_device_request "competition-mixed")
    (room_request other_room "$competition-mixed-room");
  let outgoing = V.Flow.create () in
  let devices = [ Matrix_client.To_device.Device alice_device ] in
  let first =
    V.Flow.request ~random ~now ~from_device:bob_device ~their_user_id:alice
      ~devices outgoing
  in
  check_int "first outgoing request has no cancellation" 0
    (List.length (V.Flow.take_pending_sends outgoing));
  let second =
    V.Flow.request ~random ~now ~from_device:bob_device ~their_user_id:alice
      ~devices outgoing
  in
  check_cancelled "old outgoing request" first.session;
  check_cancelled "new outgoing request" second.session;
  check_pending "outgoing" outgoing
    (V.Flow.session_transaction first.session)
    (V.Flow.session_transaction second.session)

let test_flow_request_acceptance_fanout () =
  let flow = V.Flow.create () in
  let ours, _theirs = identities () in
  let devices =
    [ device "DEV1"; device "DEV2"; device "DEV3" ]
    |> List.map (fun d -> Matrix_client.To_device.Device d)
  in
  let request =
    V.Flow.request ~random ~now ~from_device:alice_device ~their_user_id:bob
      ~devices flow
  in
  let session = request.V.Flow.session in
  let transaction = V.Flow.session_transaction session in
  let ready =
    Message.ready transaction ~from_device:(device "DEV2")
      ~methods:[ V.Method.Sas_v1 ]
  in
  let no_lookup ~user_id:_ ~device_id:_ = None in
  let accepted =
    V.Flow.handle flow ~random ~now ~ours ~lookup:no_lookup ~sender:bob ready
  in
  check_bool "the accepting device becomes selected" true
    (V.Flow.session_their_device_id session = Some (device "DEV2"));
  check_int "the other concrete recipients are cancelled" 2
    (List.length accepted.V.Flow.send_to);
  List.iter2
    (fun expected (actual, message) ->
      check_bool "the expected device gets m.accepted" true
        (Device_id.equal expected actual);
      match Message.payload message with
      | Message.Cancel c ->
          check_string "fan-out cancellation code" "m.accepted"
            (Ev.Key_verification_cancel_content.code c);
          check_string "fan-out uses the request transaction"
            (Txn_id.to_string
               (Option.get (Transaction.transaction_id transaction)))
            (Txn_id.to_string
               (Option.get
                  (Ev.Key_verification_cancel_content.transaction_id c)))
      | _ -> Alcotest.fail "expected a cancellation")
    [ device "DEV1"; device "DEV3" ]
    accepted.V.Flow.send_to;
  let duplicate =
    V.Flow.handle flow ~random ~now ~ours ~lookup:no_lookup ~sender:bob ready
  in
  check_int "a duplicate ready does not fan out again" 0
    (List.length duplicate.V.Flow.send_to);
  let rejected_flow = V.Flow.create () in
  let rejected =
    V.Flow.request ~random ~now ~from_device:alice_device ~their_user_id:bob
      ~devices rejected_flow
  in
  let rejection =
    Message.cancel
      (V.Flow.session_transaction rejected.V.Flow.session)
      V.Cancel_code.User
  in
  let rejection_result =
    V.Flow.handle rejected_flow ~random ~now ~ours ~lookup:no_lookup ~sender:bob
      rejection
  in
  check_int "a rejection is mirrored to every concrete recipient" 3
    (List.length rejection_result.V.Flow.send_to);
  List.iter
    (fun (_, message) ->
      match Message.payload message with
      | Message.Cancel c ->
          check_string "mirrored cancellation code" "m.user"
            (Ev.Key_verification_cancel_content.code c)
      | _ -> Alcotest.fail "expected a mirrored cancellation")
    rejection_result.V.Flow.send_to;
  let duplicate_rejection =
    V.Flow.handle rejected_flow ~random ~now ~ours ~lookup:no_lookup ~sender:bob
      rejection
  in
  check_int "a duplicate rejection is not mirrored again" 0
    (List.length duplicate_rejection.V.Flow.send_to);
  let singleton_flow = V.Flow.create () in
  let singleton =
    V.Flow.request ~random ~now ~from_device:alice_device ~their_user_id:bob
      ~devices:[ Matrix_client.To_device.Device (device "ONLY") ]
      singleton_flow
  in
  let singleton_ready =
    Message.ready
      (V.Flow.session_transaction singleton.V.Flow.session)
      ~from_device:(device "ONLY") ~methods:[ V.Method.Sas_v1 ]
  in
  let singleton_result =
    V.Flow.handle singleton_flow ~random ~now ~ours ~lookup:no_lookup
      ~sender:bob singleton_ready
  in
  check_int "a singleton request has no fan-out" 0
    (List.length singleton_result.V.Flow.send_to);
  let wildcard_flow = V.Flow.create () in
  let wildcard =
    V.Flow.request ~random ~now ~from_device:alice_device ~their_user_id:bob
      ~devices:[ Matrix_client.To_device.All ]
      wildcard_flow
  in
  let wildcard_ready =
    Message.ready
      (V.Flow.session_transaction wildcard.V.Flow.session)
      ~from_device:(device "ANY") ~methods:[ V.Method.Sas_v1 ]
  in
  let wildcard_result =
    V.Flow.handle wildcard_flow ~random ~now ~ours ~lookup:no_lookup ~sender:bob
      wildcard_ready
  in
  check_int "a wildcard request has no inexpressible fan-out" 0
    (List.length wildcard_result.V.Flow.send_to)

let () =
  let case name f = Alcotest.test_case name `Quick f in
  Alcotest.run "verification"
    [
      ( "canonical json",
        [
          case "encoding" test_canonical_json;
          case "rejects non-finite numbers"
            test_canonical_json_rejects_non_finite;
          case "Matrix integer numbers" test_canonical_json_matrix_numbers;
          case "rejects ambiguous values"
            test_canonical_json_rejects_ambiguous_values;
          case "Unicode key order" test_canonical_json_unicode_order;
          case "commitment vector" test_commitment_vector;
        ] );
      ( "sas strings",
        [
          case "emoji table" test_emoji_table;
          case "known vectors" test_sas_bytes_vectors;
        ] );
      ( "sas flow",
        [
          case "alice starts" test_sas_alice_starts;
          case "bob starts" test_sas_bob_starts;
          case "decimal-only negotiation" test_sas_decimal_only;
          case "emoji-only negotiation" test_sas_emoji_only;
          case "mismatched commitment" test_sas_mismatched_commitment;
          case "unknown method" test_sas_unknown_method;
          case "mac v1 fallback" test_sas_mac_v1_fallback;
          case "full flow with the v1 MAC" test_sas_flow_with_v1_mac;
          case "vodozemac SAS MAC oracle" test_sas_vodozemac_oracle;
          case "unexpected message" test_sas_unexpected_message;
          case "timeout" test_sas_timeout;
          case "user cancel" test_sas_user_cancel;
        ] );
      ( "messages",
        [
          case "to-device round trip" test_message_to_device_roundtrip;
          case "next method compatibility" test_next_method_compatibility;
          case "in-room round trip" test_message_in_room_roundtrip;
          case "request fan-out" test_request_to_device_fanout;
          case "in-room request" test_request_in_room;
          case "flow tracks in-room request" test_flow_tracks_in_room_request;
          case "ready" test_ready_response;
          case "ready with no common method" test_ready_no_common_method;
        ] );
      ( "qr",
        [
          case "round trip" test_qr_roundtrip;
          case "generators" test_qr_generators;
          case "rejects bad input" test_qr_rejects_bad_input;
          case "check and reciprocate" test_qr_check_and_reciprocate;
        ] );
      ( "cross-signing",
        [
          case "own chain" test_cross_signing_chain;
          case "other user" test_cross_signing_other_user;
          case "wrong signer" test_cross_signing_wrong_user;
          case "identity change across a refresh"
            test_cross_signing_identity_change;
          case "private identity import" test_cross_signing_private_import;
          case "private identity import rejects bad secrets"
            test_cross_signing_private_import_rejects_bad_secrets;
        ] );
      ( "flow",
        [
          case "routes transactions" test_flow_routes_two_transactions;
          case "bounds inbound requests" test_flow_bounds_inbound_requests;
          case "bounds global inbound requests"
            test_flow_bounds_global_inbound_requests;
          case "rejects sender mismatch" test_flow_rejects_sender_mismatch;
          case "request and ready" test_flow_request_and_ready;
          case "request validation" test_flow_request_validation;
          case "competing requests" test_flow_competing_requests;
          case "request acceptance fan-out" test_flow_request_acceptance_fanout;
        ] );
    ]
