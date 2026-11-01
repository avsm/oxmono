(** Olm/Megolm tests: self round-trips, recorded vodozemac vectors, and live
    cross-implementation tests against the [vodozemac-oracle] binary.

    The oracle is a small Rust program under [test/vodozemac-oracle]. When it
    has not been built and [$VODOZEMAC_ORACLE] is unset the cross-implementation
    cases print [SKIP] and pass; the recorded vectors in [test/fixtures/olm]
    still exercise both wire formats against real vodozemac output. *)

module Olm = Matrix_client.Olm
module Ck = Matrix_client.Crypto_key
module R = Matrix_client.Random
module Ev = Matrix_proto.Event
module Id = Matrix_proto.Id
module Pickle = Matrix_client.Session_pickle
module Oracle = Vodozemac_oracle_client
module Smap = Oracle.Smap

let b64e s = Base64.encode_string ~pad:false s

let b64d s =
  match Base64.decode ~pad:false s with
  | Ok s -> s
  | Error (`Msg m) -> Alcotest.failf "bad base64: %s" m

(* A [Random.t] backed by a block of bytes drawn once from the operating
   system. Reading it needs no Eio scheduler, so the tests are plain
   executables. *)
let random =
  let n = 1 lsl 20 in
  let buf = Bytes.create n in
  let ic = open_in_bin "/dev/urandom" in
  Fun.protect
    ~finally:(fun () -> close_in_noerr ic)
    (fun () -> really_input ic buf 0 n);
  R.of_source (Eio.Flow.string_source (Bytes.unsafe_to_string buf))

let ok = function
  | Ok v -> v
  | Error e -> Alcotest.failf "unexpected error: %s" e

let ok_olm = function
  | Ok v -> v
  | Error e -> Alcotest.failf "unexpected error: %a" Olm.pp_error e

let ok_msg = function
  | Ok v -> v
  | Error (`Msg m) -> Alcotest.failf "unexpected error: %s" m

let curve_b64 s = ok_msg (Ck.Curve25519.Public.of_base64 s)
let b64_of_curve = Ck.Curve25519.Public.to_base64
let rid = Id.Room_id.of_string_exn
let sid = Id.Session_id.to_string
let curve_key b = ok_msg (Ck.Curve25519.Public.of_bytes b)
let some_sender = curve_key (String.make 32 '\x07')
let mtype (m : Olm.Session.message) = Ev.Olm_message_type.to_int m.message_type

let message ~message_type ~ciphertext : Olm.Session.message =
  {
    message_type = ok_msg (Ev.Olm_message_type.of_int message_type);
    ciphertext;
  }

let check_string = Alcotest.(check string)
let check_int = Alcotest.(check int)
let check_bool = Alcotest.(check bool)

let contains needle haystack =
  let n = String.length needle and h = String.length haystack in
  let rec loop i =
    i + n <= h && (String.sub haystack i n = needle || loop (i + 1))
  in
  loop 0

let parse_object = Oracle.parse_object
let field = Oracle.field
let jstring = Oracle.jstring
let jint = Oracle.jint
let jobject = Oracle.jobject
let jlist = Oracle.jlist
let jstring_esc = Oracle.jstring_esc

type oracle = Oracle.oracle

let get_oracle = Oracle.get_oracle
let call = Oracle.call
let cmd = Oracle.cmd
let s = Oracle.s
let i = Oracle.i
let with_oracle = Oracle.with_oracle

let new_account ?(otks = 5) () =
  let a = Olm.Account.create ~random () in
  Olm.Account.generate_one_time_keys ~random a otks;
  a

let first_otk a =
  match Olm.Account.one_time_keys a with
  | (_, k) :: _ -> k
  | [] -> Alcotest.fail "account has no one-time keys"

let test_one_time_key_private_retention () =
  let account = Olm.Account.create ~random () in
  Olm.Account.generate_one_time_keys ~random account 1;
  let oldest_id =
    match Olm.Account.one_time_keys account with
    | (key_id, _) :: _ -> Ck.Key_id.id key_id
    | [] -> Alcotest.fail "expected the first generated key"
  in
  (* The public target is 50, but vodozemac retains 100 times that many
     private keys. This verifies generation does not stop at 50. *)
  Olm.Account.generate_one_time_keys ~random account 5000;
  check_int "private retention cap" 5000
    (Olm.Account.one_time_keys_count account);
  check_bool "oldest private key is evicted" true
    (not
       (List.exists
          (fun (key_id, _) -> String.equal (Ck.Key_id.id key_id) oldest_id)
          (Olm.Account.one_time_keys account)));
  check_int "public capacity remains 50" 50
    (Olm.Account.max_one_time_keys account)

let test_olm_self_round_trip () =
  let alice = new_account ~otks:0 () in
  let bob = new_account ~otks:2 () in
  let session_a =
    ok_olm
      (Olm.Session.create_outbound ~random alice
         ~their_identity_key:(Olm.Account.curve25519_key bob)
         ~their_one_time_key:(first_otk bob))
  in
  let m1 = ok_olm (Olm.Session.encrypt ~random session_a "first message") in
  check_int "first message is a pre-key message" 0 (mtype m1);
  let session_b, p1 =
    ok_olm
      (Olm.Session.create_inbound bob
         ~their_identity_key:(Olm.Account.curve25519_key alice)
         ~ciphertext:m1.Olm.Session.ciphertext)
  in
  check_string "pre-key plaintext" "first message" p1;
  check_string "session ids agree"
    (Olm.Session.session_id session_a)
    (Olm.Session.session_id session_b);
  check_int "one-time key was consumed" 1 (Olm.Account.one_time_keys_count bob);
  (* Alice keeps sending pre-key messages until she hears back. *)
  let m2 = ok_olm (Olm.Session.encrypt ~random session_a "second message") in
  check_int "still a pre-key message" 0 (mtype m2);
  check_string "second plaintext" "second message"
    (ok_olm (Olm.Session.decrypt ~random session_b m2));
  (* Bob replies: a normal message, and the ratchet turns. *)
  let r1 = ok_olm (Olm.Session.encrypt ~random session_b "reply one") in
  check_int "reply is a normal message" 1 (mtype r1);
  check_string "reply plaintext" "reply one"
    (ok_olm (Olm.Session.decrypt ~random session_a r1));
  (* Now Alice's messages are normal too. *)
  let m3 = ok_olm (Olm.Session.encrypt ~random session_a "third message") in
  check_int "third is a normal message" 1 (mtype m3);
  check_string "third plaintext" "third message"
    (ok_olm (Olm.Session.decrypt ~random session_b m3))

let test_fallback_rotation_retains_previous () =
  let alice = new_account ~otks:0 () in
  let bob = new_account ~otks:0 () in
  Olm.Account.generate_fallback_key ~random bob;
  let old_key = snd (Option.get (Olm.Account.fallback_key bob)) in
  let prekey =
    let session =
      ok_olm
        (Olm.Session.create_outbound ~random alice
           ~their_identity_key:(Olm.Account.curve25519_key bob)
           ~their_one_time_key:old_key)
    in
    ok_olm (Olm.Session.encrypt ~random session "before rotation")
  in
  Olm.Account.generate_fallback_key ~random bob;
  let current_key = Option.get (Olm.Account.fallback_key bob) |> snd in
  check_bool "rotation changes the current key" true
    (not (Ck.Curve25519.Public.equal old_key current_key));
  check_int "only current fallback is returned" 1
    (List.length (Option.to_list (Olm.Account.fallback_key bob)));
  let _, plaintext =
    ok_olm
      (Olm.Session.create_inbound bob
         ~their_identity_key:(Olm.Account.curve25519_key alice)
         ~ciphertext:prekey.ciphertext)
  in
  check_string "pre-key sent before rotation still decrypts" "before rotation"
    plaintext

let test_fallback_forget_and_pickle () =
  let account = new_account ~otks:0 () in
  Olm.Account.generate_fallback_key ~random account;
  let first = Option.get (Olm.Account.fallback_key account) in
  Olm.Account.generate_fallback_key ~random account;
  let second = Option.get (Olm.Account.fallback_key account) in
  check_bool "first rotation changes key" true (fst first <> fst second);
  let restored =
    match Pickle.pickle_account account with
    | Error (`Msg msg) -> Alcotest.fail msg
    | Ok pickle -> (
        match Pickle.unpickle_account pickle with
        | Error (`Msg msg) -> Alcotest.fail msg
        | Ok account -> account)
  in
  check_string "pickle preserves identity key"
    (Ck.Curve25519.Public.to_base64 (Olm.Account.curve25519_key account))
    (Ck.Curve25519.Public.to_base64 (Olm.Account.curve25519_key restored));
  check_string "pickle preserves current fallback"
    (Ck.Curve25519.Public.to_base64 (snd second))
    (Ck.Curve25519.Public.to_base64
       (snd (Option.get (Olm.Account.fallback_key restored))));
  check_bool "pickle preserves previous fallback" true
    (Olm.Account.forget_previous_fallback_key restored);
  check_bool "forget reports and removes previous key" true
    (Olm.Account.forget_previous_fallback_key account);
  check_bool "forget is idempotent" false
    (Olm.Account.forget_previous_fallback_key account)

let test_olm_out_of_order () =
  let alice = new_account ~otks:0 () in
  let bob = new_account ~otks:1 () in
  let a =
    ok_olm
      (Olm.Session.create_outbound ~random alice
         ~their_identity_key:(Olm.Account.curve25519_key bob)
         ~their_one_time_key:(first_otk bob))
  in
  let m0 = ok_olm (Olm.Session.encrypt ~random a "zero") in
  let b, _ =
    ok_olm
      (Olm.Session.create_inbound bob
         ~their_identity_key:(Olm.Account.curve25519_key alice)
         ~ciphertext:m0.Olm.Session.ciphertext)
  in
  let msgs =
    List.init 5 (fun n ->
        ok_olm (Olm.Session.encrypt ~random a (Printf.sprintf "msg %d" n)))
  in
  (* Deliver 4, 0, 3, 1, 2. *)
  let order = [ 4; 0; 3; 1; 2 ] in
  List.iter
    (fun n ->
      let m = List.nth msgs n in
      check_string
        (Printf.sprintf "out-of-order %d" n)
        (Printf.sprintf "msg %d" n)
        (ok_olm (Olm.Session.decrypt ~random b m)))
    order;
  (* A replay is refused: the message key has been used up. *)
  let m = List.nth msgs 0 in
  match Olm.Session.decrypt ~random b m with
  | Ok _ -> Alcotest.fail "a replayed Olm message was accepted"
  | Error _ -> ()

let test_olm_ping_pong () =
  let alice = new_account ~otks:0 () in
  let bob = new_account ~otks:1 () in
  let a =
    ok_olm
      (Olm.Session.create_outbound ~random alice
         ~their_identity_key:(Olm.Account.curve25519_key bob)
         ~their_one_time_key:(first_otk bob))
  in
  let m0 = ok_olm (Olm.Session.encrypt ~random a "0") in
  let b, _ =
    ok_olm
      (Olm.Session.create_inbound bob
         ~their_identity_key:(Olm.Account.curve25519_key alice)
         ~ciphertext:m0.Olm.Session.ciphertext)
  in
  (* Twenty changes of direction, each of which turns the DH ratchet. *)
  for n = 1 to 20 do
    let from_, to_ = if n mod 2 = 1 then (b, a) else (a, b) in
    let text = Printf.sprintf "turn %d" n in
    let m = ok_olm (Olm.Session.encrypt ~random from_ text) in
    check_string text text (ok_olm (Olm.Session.decrypt ~random to_ m))
  done

let test_olm_rejects_tampering () =
  let alice = new_account ~otks:0 () in
  let bob = new_account ~otks:1 () in
  let a =
    ok_olm
      (Olm.Session.create_outbound ~random alice
         ~their_identity_key:(Olm.Account.curve25519_key bob)
         ~their_one_time_key:(first_otk bob))
  in
  let m = ok_olm (Olm.Session.encrypt ~random a "tamper with me") in
  let raw = Bytes.of_string (b64d m.Olm.Session.ciphertext) in
  let last = Bytes.length raw - 1 in
  Bytes.set raw last (Char.chr (Char.code (Bytes.get raw last) lxor 0xff));
  let ciphertext = b64e (Bytes.unsafe_to_string raw) in
  match
    Olm.Session.create_inbound bob
      ~their_identity_key:(Olm.Account.curve25519_key alice)
      ~ciphertext
  with
  | Ok _ -> Alcotest.fail "a corrupted pre-key message was accepted"
  | Error _ ->
      check_int "the one-time key survives a bad message" 1
        (Olm.Account.one_time_keys_count bob)

let test_megolm_self_round_trip () =
  let out =
    Olm.Megolm.Outbound.create ~random ~room_id:(rid "!room:example.org") ()
  in
  let key = Olm.Megolm.Outbound.session_key out in
  let inb =
    ok_olm
      (Olm.Megolm.Inbound.of_session_key ~sender_key:some_sender
         ~room_id:(rid "!room:example.org") ~session_key:key ())
  in
  check_string "session ids agree"
    (sid (Olm.Megolm.Outbound.session_id out))
    (sid (Olm.Megolm.Inbound.session_id inb));
  Alcotest.(check bool)
    "signed key is verified" true
    (Olm.Megolm.Inbound.signing_key_verified inb);
  for n = 0 to 4 do
    let e = Olm.Megolm.Outbound.encrypt out (Printf.sprintf "message %d" n) in
    let ct = e.ciphertext in
    check_int "message index" n e.message_index;
    let d = ok_olm (Olm.Megolm.Inbound.decrypt inb ~ciphertext:ct) in
    check_string "plaintext"
      (Printf.sprintf "message %d" n)
      d.Olm.Megolm.plaintext;
    check_int "decrypted index" n d.Olm.Megolm.message_index
  done

let test_megolm_export_at () =
  let out =
    Olm.Megolm.Outbound.create ~random ~room_id:(rid "!room:example.org") ()
  in
  let key = Olm.Megolm.Outbound.session_key out in
  let inb =
    ok_olm
      (Olm.Megolm.Inbound.of_session_key ~sender_key:some_sender
         ~room_id:(rid "!r:x") ~session_key:key ())
  in
  let cts =
    List.init 6 (fun n ->
        (Olm.Megolm.Outbound.encrypt out (Printf.sprintf "m%d" n)).ciphertext)
  in
  check_int "first known index" 0 (Olm.Megolm.Inbound.first_known_index inb);
  (* Exporting at index 3 and re-importing gives a session that starts at 3,
     not at 0: this is the bug the old implementation had. *)
  let export3 = ok_olm (Olm.Megolm.Inbound.export_at inb ~index:3) in
  let inb3 =
    ok_olm
      (Olm.Megolm.Inbound.of_exported_session_key ~sender_key:some_sender
         ~room_id:(rid "!r:x") ~session_key:export3 ())
  in
  check_int "re-imported first known index" 3
    (Olm.Megolm.Inbound.first_known_index inb3);
  Alcotest.(check bool)
    "unsigned export is not verified" false
    (Olm.Megolm.Inbound.signing_key_verified inb3);
  check_string "session id survives export"
    (sid (Olm.Megolm.Inbound.session_id inb))
    (sid (Olm.Megolm.Inbound.session_id inb3));
  (match Olm.Megolm.Inbound.decrypt inb3 ~ciphertext:(List.nth cts 2) with
  | Ok _ -> Alcotest.fail "a session exported at index 3 decrypted message 2"
  | Error _ -> ());
  let d =
    ok_olm (Olm.Megolm.Inbound.decrypt inb3 ~ciphertext:(List.nth cts 4))
  in
  check_string "later message still decrypts" "m4" d.Olm.Megolm.plaintext;
  (* Export at index 0 round-trips too. *)
  let export0 = ok_olm (Olm.Megolm.Inbound.export_at inb ~index:0) in
  let inb0 =
    ok_olm
      (Olm.Megolm.Inbound.of_exported_session_key ~sender_key:some_sender
         ~room_id:(rid "!r:x") ~session_key:export0 ())
  in
  check_int "export at 0" 0 (Olm.Megolm.Inbound.first_known_index inb0);
  check_string "message 0 decrypts" "m0"
    (ok_olm (Olm.Megolm.Inbound.decrypt inb0 ~ciphertext:(List.nth cts 0)))
      .Olm.Megolm.plaintext

let test_megolm_rejects_bad_signature () =
  let out = Olm.Megolm.Outbound.create ~random ~room_id:(rid "!r:x") () in
  let key = Olm.Megolm.Outbound.session_key out in
  (match
     Olm.Megolm.Inbound.of_session_key ~sender_key:some_sender
       ~room_id:(rid "!r:x")
       ~session_key:(b64e (b64d key ^ "x"))
       ()
   with
  | Ok _ -> Alcotest.fail "an overlong signed session key was accepted"
  | Error _ -> ());
  let raw = Bytes.of_string (b64d key) in
  let last = Bytes.length raw - 1 in
  Bytes.set raw last (Char.chr (Char.code (Bytes.get raw last) lxor 0x01));
  (match
     Olm.Megolm.Inbound.of_session_key ~sender_key:some_sender
       ~room_id:(rid "!r:x")
       ~session_key:(b64e (Bytes.unsafe_to_string raw))
       ()
   with
  | Ok _ -> Alcotest.fail "a session key with a broken signature was accepted"
  | Error _ -> ());
  (* And a tampered message. *)
  let inb =
    ok_olm
      (Olm.Megolm.Inbound.of_session_key ~sender_key:some_sender
         ~room_id:(rid "!r:x") ~session_key:key ())
  in
  let ct = (Olm.Megolm.Outbound.encrypt out "hello").ciphertext in
  let raw = Bytes.of_string (b64d ct) in
  Bytes.set raw 8 (Char.chr (Char.code (Bytes.get raw 8) lxor 0xff));
  match
    Olm.Megolm.Inbound.decrypt inb
      ~ciphertext:(b64e (Bytes.unsafe_to_string raw))
  with
  | Ok _ -> Alcotest.fail "a tampered Megolm message was accepted"
  | Error _ -> ()

let test_megolm_from_room_key () =
  let out = Olm.Megolm.Outbound.create ~random ~room_id:(rid "!r:x") () in
  let key = Olm.Megolm.Outbound.session_key out in
  let id = Olm.Megolm.Outbound.session_id out in
  let good =
    ok_olm
      (Olm.Megolm.Inbound.from_room_key ~sender_key:some_sender
         ~room_id:(rid "!r:x") ~session_id:id ~session_key:key
         ~signing_key:(Olm.Megolm.Outbound.signing_key out)
         ())
  in
  check_string "session id" (sid id) (sid (Olm.Megolm.Inbound.session_id good));
  match
    Olm.Megolm.Inbound.from_room_key ~sender_key:some_sender
      ~room_id:(rid "!r:x")
      ~session_id:(Id.Session_id.of_string_exn "AAAA")
      ~session_key:key ()
  with
  | Ok _ -> Alcotest.fail "a mismatched session id was accepted"
  | Error _ -> ()

let encode codec v =
  match Jsont_bytesrw.encode_string codec v with
  | Ok s -> s
  | Error m -> Alcotest.failf "cannot encode: %s" m

let decode codec str =
  match Jsont_bytesrw.decode_string codec str with
  | Ok v -> v
  | Error m -> Alcotest.failf "cannot decode: %s" m

let uid = Id.User_id.of_string_exn

(* The wire codecs the ratchets are carried in live in matrix-chat.proto. *)
let test_event_helpers () =
  let plaintext =
    encode Ev.Olm_plaintext.jsont
      {
        event_type = "m.room_key";
        content =
          decode Matrix_proto.Json.Codec.json
            {|{"algorithm":"m.megolm.v1.aes-sha2","room_id":"!r:x"}|};
        sender = uid "@alice:example.org";
        sender_ed25519 = "AAAA";
        recipient = uid "@bob:example.org";
        recipient_ed25519 = "BBBB";
        sender_device_keys = None;
      }
  in
  let p = decode Ev.Olm_plaintext.jsont plaintext in
  check_string "type" "m.room_key" p.event_type;
  check_string "sender" "@alice:example.org" (Id.User_id.to_string p.sender);
  check_string "sender key" "AAAA" p.sender_ed25519;
  check_string "recipient" "@bob:example.org" (Id.User_id.to_string p.recipient);
  check_string "recipient key" "BBBB" p.recipient_ed25519;
  let inner = parse_object (encode Matrix_proto.Json.Codec.json p.content) in
  check_string "content survives" "!r:x" (jstring inner "room_id");
  let unstable =
    decode Ev.Olm_plaintext.jsont
      {|{"type":"m.dummy","content":{},"sender":"@alice:example.org","keys":{"ed25519":"AAAA"},"recipient":"@bob:example.org","recipient_keys":{"ed25519":"BBBB"},"org.matrix.msc4147.device_keys":{"user_id":"@alice:example.org","device_id":"ALICE","algorithms":[],"keys":{},"signatures":{}}}|}
  in
  check_bool "unstable sender device keys alias decodes" true
    (unstable.sender_device_keys <> None);
  (* m.room.encrypted, olm flavour *)
  let content =
    encode Ev.Encrypted.Olm.jsont
      {
        sender_key = "SENDER";
        ciphertext =
          [
            {
              recipient_key = "THEIRS";
              message_type = Ev.Olm_message_type.Pre_key;
              body = "CIPHER";
            };
          ];
      }
  in
  let obj = parse_object content in
  check_string "algorithm" "m.olm.v1.curve25519-aes-sha2"
    (jstring obj "algorithm");
  check_string "sender_key" "SENDER" (jstring obj "sender_key");
  let f = decode Ev.Encrypted.Olm.jsont content in
  let m =
    match Ev.Olm_ciphertext.find f.ciphertext ~recipient_key:"THEIRS" with
    | Some e -> e
    | None -> Alcotest.fail "the event is not addressed to this device"
  in
  check_int "message type" 0 (Ev.Olm_message_type.to_int m.message_type);
  check_string "body" "CIPHER" m.body;
  Alcotest.(check bool)
    "an event for another device is not found" true
    (Ev.Olm_ciphertext.find f.ciphertext ~recipient_key:"SOMEONE" = None);
  (* m.room.encrypted, megolm flavour *)
  let content =
    encode Ev.Encrypted.Megolm.jsont
      {
        sender_key = Some "SENDER";
        session_id = Id.Session_id.of_string_exn "SID";
        device_id = Some (Id.Device_id.of_string_exn "DEV");
        ciphertext = "CT";
      }
  in
  let obj = parse_object content in
  check_string "megolm algorithm" "m.megolm.v1.aes-sha2"
    (jstring obj "algorithm");
  check_string "session_id" "SID" (jstring obj "session_id");
  check_string "device_id" "DEV" (jstring obj "device_id");
  let f = decode Ev.Encrypted.Megolm.jsont content in
  check_string "ciphertext" "CT" f.ciphertext;
  (* Matrix 1.3 dropped sender_key and device_id; parsing must still work. *)
  let f =
    decode Ev.Encrypted.Megolm.jsont
      {|{"algorithm":"m.megolm.v1.aes-sha2","ciphertext":"CT","session_id":"SID"}|}
  in
  Alcotest.(check bool) "sender_key optional" true (f.sender_key = None);
  (* m.room_key *)
  let content =
    encode Ev.Room_key_content.jsont
      {
        room_id = rid "!r:x";
        session_id = Id.Session_id.of_string_exn "SID";
        session_key = "KEY";
        shared_history = true;
      }
  in
  let f = decode Ev.Room_key_content.jsont content in
  check_string "room_id" "!r:x" (Id.Room_id.to_string f.room_id);
  check_string "session_key" "KEY" f.session_key;
  check_bool "stable shared history" true f.shared_history;
  let legacy =
    decode Ev.Room_key_content.jsont
      {|{"algorithm":"m.megolm.v1.aes-sha2","room_id":"!r:x","session_id":"SID","session_key":"KEY","org.matrix.msc3061.shared_history":true}|}
  in
  check_bool "legacy shared history" true legacy.shared_history;
  (* m.forwarded_room_key *)
  let content =
    encode Ev.Forwarded_room_key_content.jsont
      {
        room_id = rid "!r:x";
        sender_key = "SK";
        session_id = Id.Session_id.of_string_exn "SID";
        session_key = "KEY";
        sender_claimed_ed25519_key = "ED";
        forwarding_curve25519_key_chain = [ "A"; "B" ];
      }
  in
  let f = decode Ev.Forwarded_room_key_content.jsont content in
  check_string "forwarded sender key" "SK" f.sender_key;
  check_int "chain length" 2 (List.length f.forwarding_curve25519_key_chain);
  check_bool "forwarded wire omits shared history" false
    (contains "shared_history" content)

let test_machine_round_trip () =
  let alice = Olm.Machine.create ~random () in
  let bob = Olm.Machine.create ~random () in
  Olm.Account.generate_one_time_keys ~random (Olm.Machine.account bob) 5;
  let bob_identity = Olm.Account.curve25519_key (Olm.Machine.account bob) in
  let bob_otk =
    match Olm.Account.signed_one_time_keys (Olm.Machine.account bob) with
    | (_, k, _) :: _ -> k
    | [] -> Alcotest.fail "no one-time keys"
  in
  let m =
    ok_olm
      (Olm.Machine.encrypt_to_device ~random alice
         ~their_identity_key:bob_identity ~their_one_time_key:bob_otk
         ~plaintext:"to-device hello")
  in
  check_string "decrypted" "to-device hello"
    (ok_olm
       (Olm.Machine.decrypt_to_device ~random bob
          ~their_identity_key:
            (Olm.Account.curve25519_key (Olm.Machine.account alice))
          m))

(* Persisting a machine's sessions with [olm_sessions] and replaying them
   into a fresh machine with [store_olm_session] must not reorder them: the
   restored machine has to keep preferring the same, most recently created
   session per peer that the original did. *)
let test_machine_restore_keeps_newest_session () =
  let alice = Olm.Machine.create ~random () in
  let bob = Olm.Machine.create ~random () in
  Olm.Account.generate_one_time_keys ~random (Olm.Machine.account bob) 5;
  let bob_identity = Olm.Account.curve25519_key (Olm.Machine.account bob) in
  let otk1, otk2 =
    match Olm.Account.signed_one_time_keys (Olm.Machine.account bob) with
    | (_, k1, _) :: (_, k2, _) :: _ -> (k1, k2)
    | _ -> Alcotest.fail "need at least two one-time keys"
  in
  let s1 =
    ok_olm
      (Olm.Machine.create_olm_session ~random alice
         ~their_identity_key:bob_identity ~their_one_time_key:otk1)
  in
  let s2 =
    ok_olm
      (Olm.Machine.create_olm_session ~random alice
         ~their_identity_key:bob_identity ~their_one_time_key:otk2)
  in
  Alcotest.(check bool)
    "two distinct sessions were created" false
    (String.equal (Olm.Session.session_id s1) (Olm.Session.session_id s2));
  check_string "s2 is the newest session"
    (Olm.Session.session_id s2)
    (Olm.Session.session_id
       (Option.get
          (Olm.Machine.find_olm_session alice ~their_identity_key:bob_identity)));
  let restored = Olm.Machine.of_account (Olm.Machine.account alice) in
  List.iter
    (Olm.Machine.store_olm_session restored)
    (Olm.Machine.olm_sessions alice);
  check_string "the restored machine still prefers s2"
    (Olm.Session.session_id s2)
    (Olm.Session.session_id
       (Option.get
          (Olm.Machine.find_olm_session restored
             ~their_identity_key:bob_identity)))

let test_session_last_use_and_lru () =
  let alice = Olm.Machine.create ~random () in
  let bob = Olm.Machine.create ~random () in
  Olm.Account.generate_one_time_keys ~random (Olm.Machine.account bob) 6;
  let bob_identity = Olm.Account.curve25519_key (Olm.Machine.account bob) in
  let otks =
    List.map
      (fun (_, key, _) -> key)
      (Olm.Account.signed_one_time_keys (Olm.Machine.account bob))
  in
  let sessions =
    List.map
      (fun one_time_key ->
        ok_olm
          (Olm.Machine.create_olm_session ~random alice
             ~their_identity_key:bob_identity ~their_one_time_key:one_time_key))
      otks
  in
  let at n = Option.get (Ptime.of_float_s (1_700_000_000. +. float n)) in
  let adjusted =
    List.mapi
      (fun n session ->
        let pickle = Olm.Session.to_pickle session in
        Olm.Session.of_pickle
          ~last_used_at:(at (List.length sessions - n))
          ~last_received_at:(at n) pickle)
      sessions
  in
  let restored = Olm.Machine.of_account (Olm.Machine.account alice) in
  (* Deliberately restore newest-first and oldest-first mixes: selection must
     use the persisted timestamp, not insertion order. *)
  List.iter (Olm.Machine.store_olm_session restored) (List.rev adjusted);
  check_int "at most four sessions are retained" 4
    (List.length (Olm.Machine.olm_sessions restored));
  let newest = List.nth adjusted 3 in
  check_string "greatest received timestamp is selected"
    (Olm.Session.session_id newest)
    (Olm.Session.session_id
       (Option.get
          (Olm.Machine.find_olm_session restored
             ~their_identity_key:bob_identity)));
  (* A successful encryption promotes last-use but not the receive-selection
     timestamp. *)
  let newest_pickle = Olm.Session.to_pickle newest in
  let session =
    Olm.Session.of_pickle ~last_used_at:(at 0) ~last_received_at:(at 0)
      newest_pickle
  in
  ignore (ok_olm (Olm.Session.encrypt ~random session "promotion"));
  check_bool "successful encryption updates last-use" true
    (Ptime.compare (Olm.Session.last_used_at session) (at 0) > 0);
  check_bool "encryption does not promote received selection" true
    (Ptime.equal (Olm.Session.last_received_at session) (at 0));
  (* Authentication failure must leave both timestamps untouched. *)
  let peer = new_account ~otks:1 () in
  let peer_identity = Olm.Account.curve25519_key peer in
  let peer_otk = first_otk peer in
  let sender =
    ok_olm
      (Olm.Session.create_outbound ~random
         (Olm.Machine.account alice)
         ~their_identity_key:peer_identity ~their_one_time_key:peer_otk)
  in
  let valid = ok_olm (Olm.Session.encrypt ~random sender "authenticated") in
  let receiver, _ =
    ok_olm
      (Olm.Session.create_inbound peer
         ~their_identity_key:
           (Olm.Account.curve25519_key (Olm.Machine.account alice))
         ~ciphertext:valid.ciphertext)
  in
  let promoted =
    let p = Olm.Session.to_pickle receiver in
    Olm.Session.of_pickle ~last_used_at:(at 0) ~last_received_at:(at 0) p
  in
  let valid_second =
    ok_olm (Olm.Session.encrypt ~random sender "authenticated second")
  in
  ignore (ok_olm (Olm.Session.decrypt ~random promoted valid_second));
  check_bool "successful decrypt promotes selection" true
    (Ptime.compare (Olm.Session.last_received_at promoted) (at 0) > 0);
  let failed =
    let p = Olm.Session.to_pickle receiver in
    Olm.Session.of_pickle ~last_used_at:(at 0) ~last_received_at:(at 0) p
  in
  let bad = { valid with ciphertext = valid.ciphertext ^ "tampered" } in
  (match Olm.Session.decrypt ~random failed bad with
  | Ok _ -> Alcotest.fail "tampered message was accepted"
  | Error _ -> ());
  check_bool "failed decrypt does not update last-use" true
    (Ptime.equal (Olm.Session.last_used_at failed) (at 0));
  check_bool "failed decrypt does not update selection" true
    (Ptime.equal (Olm.Session.last_received_at failed) (at 0))

let test_olm_ocaml_to_rust o =
  let bob = call o (cmd "create_account" [ ("one_time_keys", "2") ]) in
  let bob_identity = curve_b64 (jstring bob "curve25519") in
  let bob_account = jint bob "account" in
  let bob_otk =
    match Smap.bindings (jobject bob "one_time_keys") with
    | (_, v) :: _ ->
        curve_b64 (ok (Jsont.Json.decode Matrix_proto.Json.Codec.string v))
    | [] -> Alcotest.fail "oracle returned no one-time keys"
  in
  let alice = new_account ~otks:0 () in
  let alice_identity = b64_of_curve (Olm.Account.curve25519_key alice) in
  let session =
    ok_olm
      (Olm.Session.create_outbound ~random alice
         ~their_identity_key:bob_identity ~their_one_time_key:bob_otk)
  in
  let m = ok_olm (Olm.Session.encrypt ~random session "hello from OCaml") in
  check_int "pre-key" 0 (mtype m);
  let r =
    call o
      (cmd "create_inbound_session"
         [
           ("account", i bob_account);
           ("identity_key", s alice_identity);
           ("ciphertext", s m.Olm.Session.ciphertext);
         ])
  in
  check_string "vodozemac decrypted the pre-key message" "hello from OCaml"
    (jstring r "plaintext");
  check_string "session ids agree"
    (Olm.Session.session_id session)
    (jstring r "session_id");
  let rust_session = jint r "session" in
  (* A second pre-key message before Bob replies. *)
  let m2 = ok_olm (Olm.Session.encrypt ~random session "second from OCaml") in
  let r2 =
    call o
      (cmd "session_decrypt"
         [
           ("session", i rust_session);
           ("message_type", i (mtype m2));
           ("ciphertext", s m2.Olm.Session.ciphertext);
         ])
  in
  check_string "second pre-key message" "second from OCaml"
    (jstring r2 "plaintext");
  (* Bob replies with a normal message, which turns our ratchet. *)
  let reply =
    call o
      (cmd "session_encrypt"
         [ ("session", i rust_session); ("plaintext", s "hi back") ])
  in
  check_int "reply is normal" 1 (jint reply "message_type");
  check_string "OCaml decrypts the reply" "hi back"
    (ok_olm
       (Olm.Session.decrypt ~random session
          (message
             ~message_type:(jint reply "message_type")
             ~ciphertext:(jstring reply "ciphertext"))));
  (* And now OCaml sends normal messages. *)
  let m3 = ok_olm (Olm.Session.encrypt ~random session "third from OCaml") in
  check_int "third is normal" 1 (mtype m3);
  let r3 =
    call o
      (cmd "session_decrypt"
         [
           ("session", i rust_session);
           ("message_type", i (mtype m3));
           ("ciphertext", s m3.Olm.Session.ciphertext);
         ])
  in
  check_string "third message" "third from OCaml" (jstring r3 "plaintext")

let test_olm_rust_to_ocaml o =
  let bob = new_account ~otks:3 () in
  let bob_identity = b64_of_curve (Olm.Account.curve25519_key bob) in
  let bob_otk = b64_of_curve (first_otk bob) in
  let alice = call o (cmd "create_account" [ ("one_time_keys", "0") ]) in
  let alice_account = jint alice "account" in
  let alice_identity = curve_b64 (jstring alice "curve25519") in
  let r =
    call o
      (cmd "create_outbound_session"
         [
           ("account", i alice_account);
           ("identity_key", s bob_identity);
           ("one_time_key", s bob_otk);
         ])
  in
  let rust_session = jint r "session" in
  let rust_session_id = jstring r "session_id" in
  let m =
    call o
      (cmd "session_encrypt"
         [ ("session", i rust_session); ("plaintext", s "hello from Rust") ])
  in
  check_int "pre-key" 0 (jint m "message_type");
  let session, plaintext =
    ok_olm
      (Olm.Session.create_inbound bob ~their_identity_key:alice_identity
         ~ciphertext:(jstring m "ciphertext"))
  in
  check_string "OCaml decrypted the pre-key message" "hello from Rust" plaintext;
  check_string "session ids agree" rust_session_id
    (Olm.Session.session_id session);
  check_int "one-time key consumed" 2 (Olm.Account.one_time_keys_count bob);
  (* OCaml replies with a normal message. *)
  let reply =
    ok_olm (Olm.Session.encrypt ~random session "hi back from OCaml")
  in
  check_int "reply is normal" 1 (mtype reply);
  let d =
    call o
      (cmd "session_decrypt"
         [
           ("session", i rust_session);
           ("message_type", i (mtype reply));
           ("ciphertext", s reply.Olm.Session.ciphertext);
         ])
  in
  check_string "vodozemac decrypts the reply" "hi back from OCaml"
    (jstring d "plaintext");
  (* And Rust sends a normal message back. *)
  let m2 =
    call o
      (cmd "session_encrypt"
         [ ("session", i rust_session); ("plaintext", s "normal from Rust") ])
  in
  check_int "now normal" 1 (jint m2 "message_type");
  check_string "OCaml decrypts a normal message" "normal from Rust"
    (ok_olm
       (Olm.Session.decrypt ~random session
          (message ~message_type:(jint m2 "message_type")
             ~ciphertext:(jstring m2 "ciphertext"))))

let test_megolm_ocaml_to_rust o =
  let out = Olm.Megolm.Outbound.create ~random ~room_id:(rid "!r:x") () in
  let key = Olm.Megolm.Outbound.session_key out in
  let r = call o (cmd "megolm_inbound_import" [ ("session_key", s key) ]) in
  check_string "session ids agree"
    (sid (Olm.Megolm.Outbound.session_id out))
    (jstring r "session_id");
  check_int "first known index" 0 (jint r "first_known_index");
  let rust_session = jint r "session" in
  for n = 0 to 3 do
    let ct =
      (Olm.Megolm.Outbound.encrypt out (Printf.sprintf "megolm %d" n))
        .ciphertext
    in
    let d =
      call o
        (cmd "megolm_decrypt"
           [ ("session", i rust_session); ("ciphertext", s ct) ])
    in
    check_string "plaintext"
      (Printf.sprintf "megolm %d" n)
      (jstring d "plaintext");
    check_int "index" n (jint d "message_index")
  done;
  (* Share the session at its current index: vodozemac must start there. *)
  let key4 = Olm.Megolm.Outbound.session_key out in
  let r4 = call o (cmd "megolm_inbound_import" [ ("session_key", s key4) ]) in
  check_int "shared at index 4" 4 (jint r4 "first_known_index");
  let ct = (Olm.Megolm.Outbound.encrypt out "megolm 4").ciphertext in
  check_string "message 4" "megolm 4"
    (jstring
       (call o
          (cmd "megolm_decrypt"
             [ ("session", i (jint r4 "session")); ("ciphertext", s ct) ]))
       "plaintext");
  (* And the unsigned export imports as an ExportedSessionKey. *)
  let exported = Olm.Megolm.Outbound.exported_session_key out in
  let re =
    call o
      (cmd "megolm_inbound_import"
         [ ("session_key", s exported); ("exported", "true") ])
  in
  check_int "exported at index 5" 5 (jint re "first_known_index")

let test_megolm_rust_to_ocaml o =
  let r = call o (cmd "megolm_create" []) in
  let rust_session = jint r "session" in
  let key = jstring r "session_key" in
  let inb =
    ok_olm
      (Olm.Megolm.Inbound.of_session_key ~sender_key:some_sender
         ~room_id:(rid "!r:x") ~session_key:key ())
  in
  check_string "session ids agree" (jstring r "session_id")
    (sid (Olm.Megolm.Inbound.session_id inb));
  let cts =
    List.init 5 (fun n ->
        let m =
          call o
            (cmd "megolm_encrypt"
               [
                 ("session", i rust_session);
                 ("plaintext", s (Printf.sprintf "rust %d" n));
               ])
        in
        (jint m "message_index", jstring m "ciphertext"))
  in
  List.iter
    (fun (n, ct) ->
      let d = ok_olm (Olm.Megolm.Inbound.decrypt inb ~ciphertext:ct) in
      check_string "plaintext"
        (Printf.sprintf "rust %d" n)
        d.Olm.Megolm.plaintext;
      check_int "index" n d.Olm.Megolm.message_index)
    cts;
  (* OCaml exports at index 3, vodozemac imports it and decrypts from there. *)
  let export3 = ok_olm (Olm.Megolm.Inbound.export_at inb ~index:3) in
  let ri =
    call o
      (cmd "megolm_inbound_import"
         [ ("session_key", s export3); ("exported", "true") ])
  in
  check_int "imported at 3" 3 (jint ri "first_known_index");
  let session3 = jint ri "session" in
  let _, ct3 = List.nth cts 3 in
  check_string "message 3 through the export" "rust 3"
    (jstring
       (call o
          (cmd "megolm_decrypt"
             [ ("session", i session3); ("ciphertext", s ct3) ]))
       "plaintext");
  (* Conversely, vodozemac exports at 2 and OCaml imports it. *)
  let rust_inb =
    call o (cmd "megolm_inbound_import" [ ("session_key", s key) ])
  in
  let re =
    call o
      (cmd "megolm_export_at"
         [ ("session", i (jint rust_inb "session")); ("index", "2") ])
  in
  let inb2 =
    ok_olm
      (Olm.Megolm.Inbound.of_exported_session_key ~sender_key:some_sender
         ~room_id:(rid "!r:x") ~session_key:(jstring re "session_key") ())
  in
  check_int "OCaml imported at 2" 2 (Olm.Megolm.Inbound.first_known_index inb2);
  let _, ct2 = List.nth cts 2 in
  check_string "message 2 through the import" "rust 2"
    (ok_olm (Olm.Megolm.Inbound.decrypt inb2 ~ciphertext:ct2))
      .Olm.Megolm.plaintext;
  (* And at index 0. *)
  let rust_inb =
    call o (cmd "megolm_inbound_import" [ ("session_key", s key) ])
  in
  let re =
    call o
      (cmd "megolm_export_at"
         [ ("session", i (jint rust_inb "session")); ("index", "0") ])
  in
  let inb0 =
    ok_olm
      (Olm.Megolm.Inbound.of_exported_session_key ~sender_key:some_sender
         ~room_id:(rid "!r:x") ~session_key:(jstring re "session_key") ())
  in
  check_int "OCaml imported at 0" 0 (Olm.Megolm.Inbound.first_known_index inb0);
  let _, ct0 = List.nth cts 0 in
  check_string "message 0 through the import" "rust 0"
    (ok_olm (Olm.Megolm.Inbound.decrypt inb0 ~ciphertext:ct0))
      .Olm.Megolm.plaintext

let test_megolm_ratchet_conformance o =
  (* Compare our four-part ratchet against vodozemac's at indices that cross
     every reseeding boundary. Both sides import the same session key at
     index 0 and export at the same index; the 165-byte blobs must be equal
     byte for byte. *)
  let r = call o (cmd "megolm_create" []) in
  let key = jstring r "session_key" in
  List.iter
    (fun index ->
      let rust_inb =
        call o (cmd "megolm_inbound_import" [ ("session_key", s key) ])
      in
      let re =
        call o
          (cmd "megolm_export_at"
             [ ("session", i (jint rust_inb "session")); ("index", i index) ])
      in
      let ours =
        ok_olm
          (Olm.Megolm.Inbound.of_session_key ~sender_key:some_sender
             ~room_id:(rid "!r:x") ~session_key:key ())
      in
      check_string
        (Printf.sprintf "ratchet at index %d" index)
        (jstring re "session_key")
        (ok_olm (Olm.Megolm.Inbound.export_at ours ~index)))
    [ 0; 1; 2; 255; 256; 257; 511; 512; 65535; 65536; 65537; 1000000 ]

let test_megolm_long_run o =
  (* Our outbound ratchet steps one message at a time; vodozemac's inbound
     ratchet jumps straight to each index. Three hundred messages take both
     across the 2^8 reseeding boundary. *)
  let out = Olm.Megolm.Outbound.create ~random ~room_id:(rid "!r:x") () in
  let r =
    call o
      (cmd "megolm_inbound_import"
         [ ("session_key", s (Olm.Megolm.Outbound.session_key out)) ])
  in
  let rust_session = jint r "session" in
  for n = 0 to 299 do
    let e = Olm.Megolm.Outbound.encrypt out (Printf.sprintf "long %d" n) in
    let ct = e.ciphertext in
    check_int "index" n e.message_index;
    if n mod 37 = 0 || n > 250 then begin
      let d =
        call o
          (cmd "megolm_decrypt"
             [ ("session", i rust_session); ("ciphertext", s ct) ])
      in
      check_string "plaintext"
        (Printf.sprintf "long %d" n)
        (jstring d "plaintext");
      check_int "decrypted index" n (jint d "message_index")
    end
  done

let test_signature_accepted_by_vodozemac o =
  (* The Ed25519 signature over a Megolm session key must verify under
     vodozemac's own verifier. *)
  let out = Olm.Megolm.Outbound.create ~random ~room_id:(rid "!r:x") () in
  let r =
    call o
      (cmd "megolm_inbound_import"
         [ ("session_key", s (Olm.Megolm.Outbound.session_key out)) ])
  in
  check_string "accepted"
    (sid (Olm.Megolm.Outbound.session_id out))
    (jstring r "session_id")

let fixture name =
  let rel = Filename.concat "fixtures" (Filename.concat "olm" name) in
  let rels = [ rel; Filename.concat "test" rel ] in
  let rec up dir n =
    if n = 0 then None
    else
      match
        List.find_opt (fun r -> Sys.file_exists (Filename.concat dir r)) rels
      with
      | Some r -> Some (Filename.concat dir r)
      | None ->
          let parent = Filename.dirname dir in
          if parent = dir then None else up parent (n - 1)
  in
  match up (Sys.getcwd ()) 8 with
  | None -> None
  | Some path ->
      let ic = open_in_bin path in
      Fun.protect
        ~finally:(fun () -> close_in_noerr ic)
        (fun () -> Some (really_input_string ic (in_channel_length ic)))

let test_megolm_vector () =
  match fixture "megolm_vodozemac.json" with
  | None -> Printf.printf "SKIP megolm vector: fixture missing\n%!"
  | Some contents ->
      let m = parse_object contents in
      let inb =
        ok_olm
          (Olm.Megolm.Inbound.of_session_key ~sender_key:some_sender
             ~room_id:(rid (jstring m "room_id"))
             ~session_key:(jstring m "session_key") ())
      in
      check_string "session id" (jstring m "session_id")
        (sid (Olm.Megolm.Inbound.session_id inb));
      List.iter
        (fun msg ->
          let d =
            ok_olm
              (Olm.Megolm.Inbound.decrypt inb
                 ~ciphertext:(jstring msg "ciphertext"))
          in
          check_string "plaintext" (jstring msg "plaintext")
            d.Olm.Megolm.plaintext;
          check_int "index" (jint msg "message_index")
            d.Olm.Megolm.message_index)
        (jlist m "messages");
      (* The recorded ExportedSessionKey at index 2 must import at index 2. *)
      let inb2 =
        ok_olm
          (Olm.Megolm.Inbound.of_exported_session_key ~sender_key:some_sender
             ~room_id:(rid (jstring m "room_id"))
             ~session_key:(jstring m "exported_at_2")
             ())
      in
      check_int "exported at 2" 2 (Olm.Megolm.Inbound.first_known_index inb2);
      let msg2 = List.nth (jlist m "messages") 2 in
      check_string "message 2 from the export" (jstring msg2 "plaintext")
        (ok_olm
           (Olm.Megolm.Inbound.decrypt inb2
              ~ciphertext:(jstring msg2 "ciphertext")))
          .Olm.Megolm.plaintext

let account_of_fixture m =
  let curve_secret field =
    ok_msg (Ck.Curve25519.Secret.of_bytes (b64d (jstring m field)))
  in
  Olm.Account.of_pickle
    {
      ed25519 =
        ok_msg
          (Ck.Ed25519.Private.of_bytes (b64d (jstring m "our_ed25519_priv")));
      curve25519 = curve_secret "our_curve25519_secret";
      stored_one_time_keys =
        [
          {
            key_id = "AAAAAAAAAAA";
            secret = curve_secret "our_one_time_secret";
          };
        ];
      stored_fallback_key = None;
      stored_previous_fallback_key = None;
      next_key_id = 1;
      max_one_time_keys = 50;
    }

let test_olm_vector () =
  match fixture "olm_vodozemac.json" with
  | None -> Printf.printf "SKIP olm vector: fixture missing\n%!"
  | Some contents ->
      let m = parse_object contents in
      let account = account_of_fixture m in
      let their_identity = curve_b64 (jstring m "their_curve25519") in
      let messages = jlist m "messages" in
      let first = List.hd messages in
      let session, plaintext =
        ok_olm
          (Olm.Session.create_inbound account ~their_identity_key:their_identity
             ~ciphertext:(jstring first "ciphertext"))
      in
      check_string "recorded pre-key message"
        (jstring first "plaintext")
        plaintext;
      check_string "recorded session id" (jstring m "session_id")
        (Olm.Session.session_id session);
      List.iter
        (fun msg ->
          check_string "recorded message" (jstring msg "plaintext")
            (ok_olm
               (Olm.Session.decrypt ~random session
                  (message ~message_type:(jint msg "message_type")
                     ~ciphertext:(jstring msg "ciphertext")))))
        (List.tl messages)

(* Set [OLM_FIXTURE_OUT] to a directory to re-record the vectors above from a
   live oracle. This is not part of the test run. *)
let regenerate_fixtures dir o =
  let write name contents =
    let path = Filename.concat dir name in
    let oc = open_out_bin path in
    output_string oc contents;
    close_out oc;
    Printf.printf "wrote %s\n%!" path
  in
  (* Megolm: vodozemac is the sender, so no secrets need recording. *)
  let r = call o (cmd "megolm_create" []) in
  let rust_session = jint r "session" in
  let session_key = jstring r "session_key" in
  let msgs =
    List.init 4 (fun n ->
        let m =
          call o
            (cmd "megolm_encrypt"
               [
                 ("session", i rust_session);
                 ("plaintext", s (Printf.sprintf "vector %d" n));
               ])
        in
        ( jint m "message_index",
          jstring m "ciphertext",
          Printf.sprintf "vector %d" n ))
  in
  let inb =
    call o (cmd "megolm_inbound_import" [ ("session_key", s session_key) ])
  in
  let export2 =
    jstring
      (call o
         (cmd "megolm_export_at"
            [ ("session", i (jint inb "session")); ("index", "2") ]))
      "session_key"
  in
  let buf = Buffer.create 1024 in
  Buffer.add_string buf "{\n";
  Buffer.add_string buf
    (Printf.sprintf "  \"comment\": %s,\n"
       (jstring_esc "Recorded from vodozemac 0.9 via test/vodozemac-oracle."));
  Buffer.add_string buf
    (Printf.sprintf "  \"sender_key\": %s,\n" (s "recorded-sender-key"));
  Buffer.add_string buf
    (Printf.sprintf "  \"room_id\": %s,\n" (s "!recorded:example.org"));
  Buffer.add_string buf
    (Printf.sprintf "  \"session_id\": %s,\n" (s (jstring r "session_id")));
  Buffer.add_string buf
    (Printf.sprintf "  \"session_key\": %s,\n" (s session_key));
  Buffer.add_string buf
    (Printf.sprintf "  \"exported_at_2\": %s,\n" (s export2));
  Buffer.add_string buf "  \"messages\": [\n";
  Buffer.add_string buf
    (String.concat ",\n"
       (List.map
          (fun (idx, ct, pt) ->
            Printf.sprintf
              "    { \"message_index\": %d, \"ciphertext\": %s, \"plaintext\": \
               %s }"
              idx (s ct) (s pt))
          msgs));
  Buffer.add_string buf "\n  ]\n}\n";
  write "megolm_vodozemac.json" (Buffer.contents buf);
  (* Olm: OCaml is the recipient, so the fixture must carry our secrets. *)
  let account = Olm.Account.create ~random () in
  Olm.Account.generate_one_time_keys ~random account 1;
  let pickle = Olm.Account.to_pickle account in
  let otk_secret =
    match pickle.stored_one_time_keys with
    | k :: _ -> k.secret
    | [] -> Alcotest.fail "no one-time key"
  in
  let otk_public = Ck.Curve25519.Secret.public otk_secret in
  let alice = call o (cmd "create_account" [ ("one_time_keys", "0") ]) in
  let alice_account = jint alice "account" in
  let alice_identity = jstring alice "curve25519" in
  let sess =
    call o
      (cmd "create_outbound_session"
         [
           ("account", i alice_account);
           ( "identity_key",
             s (b64_of_curve (Olm.Account.curve25519_key account)) );
           ("one_time_key", s (b64_of_curve otk_public));
         ])
  in
  let rust_session = jint sess "session" in
  let olm_msgs =
    List.init 3 (fun n ->
        let m =
          call o
            (cmd "session_encrypt"
               [
                 ("session", i rust_session);
                 ("plaintext", s (Printf.sprintf "olm vector %d" n));
               ])
        in
        ( jint m "message_type",
          jstring m "ciphertext",
          Printf.sprintf "olm vector %d" n ))
  in
  let buf = Buffer.create 1024 in
  Buffer.add_string buf "{\n";
  Buffer.add_string buf
    (Printf.sprintf "  \"comment\": %s,\n"
       (jstring_esc
          "Recorded from vodozemac 0.9 via test/vodozemac-oracle. The \
           recipient keys were generated by this test and are throwaway."));
  Buffer.add_string buf
    (Printf.sprintf "  \"our_ed25519_priv\": %s,\n"
       (s (b64e (Ck.Ed25519.Private.to_bytes pickle.ed25519))));
  Buffer.add_string buf
    (Printf.sprintf "  \"our_curve25519_secret\": %s,\n"
       (s (b64e (Ck.Curve25519.Secret.to_bytes pickle.curve25519))));
  Buffer.add_string buf
    (Printf.sprintf "  \"our_curve25519_public\": %s,\n"
       (s (b64_of_curve (Olm.Account.curve25519_key account))));
  Buffer.add_string buf
    (Printf.sprintf "  \"our_one_time_secret\": %s,\n"
       (s (b64e (Ck.Curve25519.Secret.to_bytes otk_secret))));
  Buffer.add_string buf
    (Printf.sprintf "  \"our_one_time_public\": %s,\n"
       (s (b64_of_curve otk_public)));
  Buffer.add_string buf
    (Printf.sprintf "  \"their_curve25519\": %s,\n" (s alice_identity));
  Buffer.add_string buf
    (Printf.sprintf "  \"session_id\": %s,\n" (s (jstring sess "session_id")));
  Buffer.add_string buf "  \"messages\": [\n";
  Buffer.add_string buf
    (String.concat ",\n"
       (List.map
          (fun (ty, ct, pt) ->
            Printf.sprintf
              "    { \"message_type\": %d, \"ciphertext\": %s, \"plaintext\": \
               %s }"
              ty (s ct) (s pt))
          olm_msgs));
  Buffer.add_string buf "\n  ]\n}\n";
  write "olm_vodozemac.json" (Buffer.contents buf)

(* ------------------------------------------------------------------ *)

let () =
  match Sys.getenv_opt "OLM_FIXTURE_OUT" with
  | Some dir -> (
      match get_oracle () with
      | None ->
          prerr_endline "OLM_FIXTURE_OUT set but the oracle is not available"
      | Some o -> regenerate_fixtures dir o)
  | None ->
      Alcotest.run "olm"
        [
          ( "self",
            [
              Alcotest.test_case "olm round trip" `Quick
                test_olm_self_round_trip;
              Alcotest.test_case "one-time-key private retention" `Quick
                test_one_time_key_private_retention;
              Alcotest.test_case "fallback rotation retains previous" `Quick
                test_fallback_rotation_retains_previous;
              Alcotest.test_case "fallback forget and pickle" `Quick
                test_fallback_forget_and_pickle;
              Alcotest.test_case "olm out of order" `Quick test_olm_out_of_order;
              Alcotest.test_case "olm ping pong" `Quick test_olm_ping_pong;
              Alcotest.test_case "olm rejects tampering" `Quick
                test_olm_rejects_tampering;
              Alcotest.test_case "megolm round trip" `Quick
                test_megolm_self_round_trip;
              Alcotest.test_case "megolm export at index" `Quick
                test_megolm_export_at;
              Alcotest.test_case "megolm rejects bad signatures" `Quick
                test_megolm_rejects_bad_signature;
              Alcotest.test_case "megolm from m.room_key" `Quick
                test_megolm_from_room_key;
              Alcotest.test_case "event content helpers" `Quick
                test_event_helpers;
              Alcotest.test_case "machine round trip" `Quick
                test_machine_round_trip;
              Alcotest.test_case "machine restore keeps the newest session"
                `Quick test_machine_restore_keeps_newest_session;
              Alcotest.test_case "session last-use and LRU" `Quick
                test_session_last_use_and_lru;
            ] );
          ( "vectors",
            [
              Alcotest.test_case "recorded megolm vector" `Quick
                test_megolm_vector;
              Alcotest.test_case "recorded olm vector" `Quick test_olm_vector;
            ] );
          ( "interop",
            [
              Alcotest.test_case "olm OCaml to vodozemac" `Quick
                (with_oracle "olm OCaml to vodozemac" test_olm_ocaml_to_rust);
              Alcotest.test_case "olm vodozemac to OCaml" `Quick
                (with_oracle "olm vodozemac to OCaml" test_olm_rust_to_ocaml);
              Alcotest.test_case "megolm OCaml to vodozemac" `Quick
                (with_oracle "megolm OCaml to vodozemac"
                   test_megolm_ocaml_to_rust);
              Alcotest.test_case "megolm vodozemac to OCaml" `Quick
                (with_oracle "megolm vodozemac to OCaml"
                   test_megolm_rust_to_ocaml);
              Alcotest.test_case "megolm ratchet conformance" `Quick
                (with_oracle "megolm ratchet conformance"
                   test_megolm_ratchet_conformance);
              Alcotest.test_case "megolm long run" `Quick
                (with_oracle "megolm long run" test_megolm_long_run);
              Alcotest.test_case "vodozemac accepts our signatures" `Quick
                (with_oracle "vodozemac accepts our signatures"
                   test_signature_accepted_by_vodozemac);
            ] );
        ]
