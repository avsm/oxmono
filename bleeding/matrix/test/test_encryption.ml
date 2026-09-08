(** Tests for the encryption service.

    Most of these run two whole crypto machines — an "Alice" and a "Bob" — in
    one process and pass messages between them by hand, so what is under test is
    the real path a message takes: device keys queried, a one-time key claimed,
    an Olm session opened, an [m.room_key] sent to-device, a Megolm session
    imported on the other side, and a room event decrypted. Nothing is stubbed
    out but the homeserver, which is a {!Fetch_mock} that records what the
    library sends and answers from a canned script — the same harness shape as
    [test_matrix_client.ml] and [test_e2ee_api.ml].

    The randomness is a SHA-256 keystream from a fixed seed rather than the
    system CSPRNG, so a failure reproduces exactly; each machine gets its own
    seed so that two "identical" devices do not accidentally share keys. *)

module Client = Matrix_client.Client
module Error = Matrix_client.Error
module Encryption = Matrix_client.Encryption
module Crypto_store = Matrix_client.Crypto_store
module Driver = Matrix_client.Encryption_driver
module Keys = Matrix_client.Keys
module Backup = Matrix_client.Backup
module Room_key_export = Matrix_client.Room_key_export
module Olm = Matrix_client.Olm
module Cross_signing = Matrix_client.Cross_signing
module Ck = Matrix_client.Crypto_key
module Key_id = Ck.Key_id
module Rnd = Matrix_client.Random
module Id = Matrix_proto.Id
module Did = Id.Device_id
module Sync = Matrix_proto.Sync
module Sliding = Matrix_proto.Sliding_sync
module Ev = Matrix_proto.Event
module String_map = Map.MakePortable (String)

(* {1 Harness} *)

let uid s = Result.get_ok (Id.User_id.of_string s)
let rid s = Result.get_ok (Id.Room_id.of_string s)
let did s = Result.get_ok (Id.Device_id.of_string s)
let alice_id = uid "@alice:example.org"
let bob_id = uid "@bob:example.org"
let room = rid "!room:example.org"
let room_str = Id.Room_id.to_string room

(* A deterministic keystream: SHA-256 of the seed and a counter, concatenated.
   Long enough for an account, a full pool of one-time keys, several Olm
   sessions and a handful of Megolm sessions. *)
let keystream seed n =
  let b = Buffer.create (n + 32) in
  let i = ref 0 in
  while Buffer.length b < n do
    Buffer.add_string b
      Digestif.SHA256.(to_raw_string (digest_string (seed ^ string_of_int !i)));
    incr i
  done;
  Buffer.sub b 0 n

let random_of seed =
  Rnd.of_source (Eio.Flow.string_source (keystream seed 400_000))

let mock_env =
  object
    method secure_random = Eio.Flow.string_source (keystream "env" 65_536)
  end

type recorded = { meth : string; url : string; body : string option }

let body_of_request (req : Fetch.Middleware.request) =
  match req.body with
  | Fetch.Empty -> None
  | Fetch.String s -> Some s
  | Fetch.Stream _ -> Some "<stream>"

let mock handler =
  let log = ref [] in
  let client =
    Fetch_mock.client (fun (req : Fetch.Middleware.request) ->
        log :=
          {
            meth = Http.Method.to_string req.meth;
            url = Fetch.Middleware.Url.to_string req.url;
            body = body_of_request req;
          }
          :: !log;
        handler req)
  in
  (log, client)

let test_session : Client.session =
  {
    user_id = alice_id;
    access_token = "syt_secret_token";
    device_id = did "ALICEDEV";
    refresh_token = None;
  }

let client_of fetch =
  let config =
    Client.config ~homeserver:(Uriz.of_string_exn "https://hs.example") ()
  in
  Client.with_session
    (Client.create ~config ~fetch
       ~random:(Matrix_client.Random.of_env mock_env))
    test_session

let requests log = List.rev !log
let run f () = Eio_mock.Backend.run f
let check_string = Alcotest.(check string)
let check_int = Alcotest.(check int)
let check_bool = Alcotest.(check bool)

let ok_value = function
  | Ok v -> v
  | Error e -> Alcotest.failf "expected Ok, got error: %s" (Error.to_string e)

let contains needle haystack =
  let n = String.length needle and h = String.length haystack in
  let rec go i =
    i + n <= h && (String.sub haystack i n = needle || go (i + 1))
  in
  go 0

(* A homeserver that says yes to everything the machine sends it. *)
let permissive_handler (req : Fetch.Middleware.request) =
  let url = Fetch.Middleware.Url.to_string req.url in
  let contains needle = contains needle url in
  if contains "/keys/upload" then
    Fetch_mock.respond {|{"one_time_key_counts":{"signed_curve25519":50}}|} req
  else if contains "/keys/query" then
    Fetch_mock.respond {|{"device_keys":{}}|} req
  else if contains "/keys/claim" then
    Fetch_mock.respond {|{"one_time_keys":{}}|} req
  else Fetch_mock.respond "{}" req

let permissive_client () =
  let log, fetch = mock permissive_handler in
  (log, client_of fetch)

(* {1 JSON helpers} *)

let jname n = (n, Jsont.Meta.none)
let jstr s = Jsont.Json.string s

let jobj l =
  Jsont.Json.object' (List.map (fun (k, v) -> Jsont.Json.mem (jname k) v) l)

let json_to_string j =
  Result.get_ok (Jsont_bytesrw.encode_string Matrix_proto.Json.Codec.json j)

let json_of_string s =
  Result.get_ok (Jsont_bytesrw.decode_string Matrix_proto.Json.Codec.json s)

let string_map_jsont value =
  let m = Matrix_proto.Json.Codec.as_string_map value in
  Jsont.map ~dec:String_map.bindings
    ~enc:(fun l -> List.to_seq l |> String_map.of_seq)
    m

(* The [messages] of a [/sendToDevice] body, flattened to
   (user, device, content). *)
let to_device_messages body =
  let codec =
    Jsont.Object.(
      map Fun.id
      |> mem "messages"
           (string_map_jsont (string_map_jsont Matrix_proto.Json.Codec.json))
           ~dec_absent:(fun () -> [])
           ~enc:Fun.id
      |> finish)
  in
  match Jsont_bytesrw.decode_string codec body with
  | Error msg -> Alcotest.failf "not a sendToDevice body: %s" msg
  | Ok users ->
      List.concat_map
        (fun (u, devices) -> List.map (fun (d, c) -> (u, d, c)) devices)
        users

let last_to_device log =
  match
    List.rev
      (List.filter (fun r -> contains "/sendToDevice/" r.url) (requests log))
  with
  | [] -> Alcotest.fail "no /sendToDevice request was made"
  | r :: _ -> to_device_messages (Option.get r.body)

(* {1 Wiring two machines together by hand} *)

let did = Id.Device_id.of_string_exn

let make ~seed ~user ~device () =
  Encryption.create ~random:(random_of seed) ~user_id:user ~device_id:device ()

(* The pure machine is what the tests drive; a driver is wrapped around it
   wherever a request has to reach the mock homeserver. *)
let drive ?store m = Driver.v ?store m

(* The [/keys/query] answer a homeserver would give for one machine's own
   device: exactly the object the machine would have uploaded. *)
let query_response_for m =
  let dk = Encryption.device_keys_for_upload m in
  {
    Keys.failures = [];
    device_keys = [ (dk.user_id, [ (dk.device_id, dk) ]) ];
    master_keys = [];
    self_signing_keys = [];
    user_signing_keys = [];
  }

let cross_signed_query_for ?(user_id = bob_id) ~seed m =
  let identity = Cross_signing.create_private_identity ~user_id in
  Cross_signing.generate_private_keys ~random:(random_of seed) identity;
  let upload = Option.get (Cross_signing.build_upload identity) in
  let device =
    Cross_signing.create_device (Encryption.device_keys_for_upload m)
    |> Cross_signing.sign_device
         ~signer:(Option.get (Cross_signing.self_signing_secret identity))
         ~signer_user_id:user_id
  in
  let dk = Cross_signing.device_keys device in
  {
    (query_response_for m) with
    device_keys = [ (user_id, [ (dk.device_id, dk) ]) ];
    master_keys = [ (user_id, upload.master_key) ];
    self_signing_keys = [ (user_id, upload.self_signing_key) ];
  }

let test_reset_cross_signing_invalidates_own_trust () =
  let device_id = did "ALICEDEV" in
  let alice =
    make ~seed:"identity-reset-alice" ~user:alice_id ~device:device_id ()
  in
  Encryption.receive_keys_query alice
    (cross_signed_query_for ~user_id:alice_id ~seed:"identity-reset-old" alice);
  Encryption.trust_user_identity alice alice_id;
  Encryption.set_device_trust alice alice_id ~device_id Encryption.Verified;
  check_bool "old own identity starts verified" true
    (Encryption.identity_status alice alice_id
    = Some Encryption.Identity_verified);
  Encryption.reset_cross_signing alice;
  check_bool "old own identity is forgotten" true
    (Encryption.identity_status alice alice_id = None);
  match Encryption.find_device alice alice_id ~device_id with
  | Some device ->
      check_bool "old own-device trust is invalidated" true
        (device.trust = Encryption.Unverified)
  | None -> Alcotest.fail "identity reset dropped the device record"

let query_response_body (response : Keys.query_keys_response) =
  let devices =
    List.map
      (fun (user_id, devices) ->
        ( Id.User_id.to_string user_id,
          jobj
            (List.map
               (fun (device_id, keys) ->
                 ( Id.Device_id.to_string device_id,
                   Result.get_ok (Jsont.Json.encode Keys.device_keys_jsont keys)
                 ))
               devices) ))
      response.device_keys
  in
  let identities entries =
    jobj
      (List.map
         (fun (user_id, key) ->
           ( Id.User_id.to_string user_id,
             Result.get_ok (Jsont.Json.encode Keys.cross_signing_key_jsont key)
           ))
         entries)
  in
  json_to_string
    (jobj
       [
         ("failures", jobj []);
         ("device_keys", jobj devices);
         ("master_keys", identities response.master_keys);
         ("self_signing_keys", identities response.self_signing_keys);
         ("user_signing_keys", identities response.user_signing_keys);
       ])

(* A [/keys/claim] answer that hands over one of a machine's real one-time
   keys, signed the way the server would have received it. *)
let claim_response_for m =
  let offered =
    List.concat_map
      (function
        | Encryption.Keys_upload { one_time_keys; _ } -> one_time_keys | _ -> [])
      (Encryption.outgoing_requests m)
  in
  match offered with
  | [] -> Alcotest.fail "the machine generated no one-time keys"
  | key :: _ ->
      {
        Keys.failures = [];
        one_time_keys =
          [ (Encryption.user_id m, [ (Encryption.device_id m, [ key ]) ]) ];
      }

let introduce ~from ~into =
  Encryption.receive_keys_query into (query_response_for from)

let sync_of ?rooms ?(to_device = []) ?device_lists ?(otk_counts = [])
    ?fallback_types () : Sync.Response.t =
  {
    next_batch = "s_next";
    rooms;
    presence = None;
    account_data = None;
    to_device = Some { events = to_device };
    device_lists;
    device_one_time_keys_count = otk_counts;
    device_unused_fallback_key_types = fallback_types;
  }

let sliding_sync_of ?(otk_counts = []) () : Sliding.Response.t =
  let e2ee : Sliding.Response.e2ee =
    {
      device_lists = { changed = []; left = [] };
      device_one_time_keys_count = otk_counts;
      device_unused_fallback_key_types = None;
    }
  in
  let extensions : Sliding.Response.extensions =
    {
      to_device = None;
      e2ee;
      account_data = { global = []; rooms = [] };
      receipts = { rooms = [] };
      typing = { rooms = [] };
      profiles = { users = [] };
      thread_subscriptions =
        { subscribed = []; unsubscribed = []; prev_batch = None };
      other = [];
    }
  in
  { pos = "s_next"; txn_id = None; lists = []; rooms = []; extensions }

let to_device_event ~sender ~event_type ~content =
  jobj
    [ ("type", jstr event_type); ("sender", jstr sender); ("content", content) ]

let encrypted_raw ~sender ~content : Matrix_proto.Event.Raw_event.t =
  {
    event_id = Some (Result.get_ok (Id.Event_id.of_string "$evt1"));
    sender;
    origin_server_ts = Matrix_proto.Event.Timestamp.of_ms 0L;
    type_ = Matrix_proto.Event.Event_type.of_string "m.room.encrypted";
    state_key = None;
    redacts = None;
    content;
    unsigned = None;
    room_id = Some room;
  }

let at seconds = Option.get (Ptime.of_float_s seconds)

(* Make an existing Olm session appear old without changing its ratchet.  The
   public pickle deliberately carries the creation time, which also lets this
   exercise the persisted-session path rather than a test-only hook. *)
let age_machine machine ~creation ~seed =
  let snapshot = Encryption.snapshot machine in
  let olm_sessions =
    List.map
      (fun session ->
        let pickle = Olm.Session.to_pickle session in
        Olm.Session.of_pickle { pickle with creation_time = creation })
      snapshot.olm_sessions
  in
  Encryption.of_snapshot ~random:(random_of seed)
    ~user_id:(Encryption.user_id machine)
    ~device_id:(Encryption.device_id machine)
    { snapshot with olm_sessions }

let bad_olm_event ~sender ~sender_curve ~recipient_curve =
  to_device_event ~sender ~event_type:"m.room.encrypted"
    ~content:
      (jobj
         [
           ("algorithm", jstr "m.olm.v1.curve25519-aes-sha2");
           ("sender_key", jstr sender_curve);
           ( "ciphertext",
             jobj
               [
                 ( recipient_curve,
                   jobj [ ("type", Jsont.Json.int 1); ("body", jstr "bad") ] );
               ] );
         ])

let claim_request_for machine =
  match
    List.find_opt
      (function Encryption.Keys_claim _ -> true | _ -> false)
      (Encryption.ensure_sessions machine ~members:[ bob_id ])
  with
  | Some (Encryption.Keys_claim keys) -> keys
  | _ -> Alcotest.fail "expected a forced Olm keys claim"

let has_claim machine =
  List.exists
    (function Encryption.Keys_claim _ -> true | _ -> false)
    (Encryption.ensure_sessions machine ~members:[ bob_id ])

let dummy_requests machine =
  List.filter_map
    (function
      | Encryption.To_device
          { event_type = "m.room.encrypted"; txn_id; messages } ->
          Some (txn_id, messages)
      | _ -> None)
    (Encryption.outgoing_requests machine)

let decrypt_dummy bob sender messages =
  let content =
    match messages with
    | [ (_, [ (Matrix_client.To_device.Device _, content) ]) ] -> content
    | _ -> Alcotest.fail "dummy request has unexpected recipients"
  in
  let encrypted =
    match Jsont.Json.decode Ev.Encrypted.Olm.jsont content with
    | Ok value -> value
    | Error msg -> Alcotest.failf "dummy is not Olm ciphertext: %s" msg
  in
  let recipient_key =
    Ck.Curve25519.Public.to_base64 (snd (Encryption.identity_keys bob))
  in
  let entry =
    match Ev.Olm_ciphertext.find encrypted.ciphertext ~recipient_key with
    | Some entry -> entry
    | None -> Alcotest.fail "dummy ciphertext does not target Bob"
  in
  let receiver = Olm.Machine.of_account (Encryption.snapshot bob).account in
  let plaintext =
    match
      Olm.Machine.decrypt_to_device
        ~random:(random_of "unwedge-dummy-decrypt")
        receiver
        ~their_identity_key:(snd (Encryption.identity_keys sender))
        {
          Olm.Session.message_type = entry.message_type;
          ciphertext = entry.body;
        }
    with
    | Ok plaintext -> plaintext
    | Error error ->
        Alcotest.failf "dummy did not decrypt: %a" Olm.pp_error error
  in
  match Jsont_bytesrw.decode_string Ev.Olm_plaintext.jsont plaintext with
  | Ok value -> value.event_type
  | Error msg -> Alcotest.failf "dummy plaintext is malformed: %s" msg

let setup_aged_olm ~age ~seed =
  let alice =
    make ~seed:(seed ^ "-alice") ~user:alice_id ~device:(did "ALICEDEV") ()
  in
  let bob = make ~seed:(seed ^ "-bob") ~user:bob_id ~device:(did "BOBDEV") () in
  introduce ~from:bob ~into:alice;
  ignore (Encryption.receive_keys_claim alice (claim_response_for bob));
  ( age_machine alice
      ~creation:(at (1_700_000_000. -. age))
      ~seed:(seed ^ "-aged"),
    bob )

let fail_olm_decrypt machine bob ~now =
  let _, alice_curve = Encryption.identity_keys machine in
  let bob_curve = snd (Encryption.identity_keys bob) in
  Encryption.process_sync ~now machine
    (sync_of
       ~to_device:
         [
           bad_olm_event
             ~sender:(Id.User_id.to_string bob_id)
             ~sender_curve:(Ck.Curve25519.Public.to_base64 bob_curve)
             ~recipient_curve:(Ck.Curve25519.Public.to_base64 alice_curve);
         ]
       ())

let test_olm_unwedge_cutoff_and_clock_rollback () =
  let now = at 1_700_000_000. in
  let exact, bob = setup_aged_olm ~age:3600. ~seed:"unwedge-exact" in
  ignore (fail_olm_decrypt exact bob ~now);
  check_bool "exactly one hour suppresses unwedge" false (has_claim exact);

  let old, bob = setup_aged_olm ~age:3601. ~seed:"unwedge-old" in
  ignore (fail_olm_decrypt old bob ~now);
  let claim = claim_request_for old in
  check_bool "old session is force-claimed" true
    (List.exists
       (fun (_, devices) ->
         List.exists (fun (d, _) -> Did.equal d (did "BOBDEV")) devices)
       claim);

  let future, bob = setup_aged_olm ~age:(-1.) ~seed:"unwedge-rollback" in
  ignore (fail_olm_decrypt future bob ~now);
  ignore (claim_request_for future)

let test_olm_unwedge_claim_retry_dummy_and_mark_sent () =
  let now = at 1_700_000_000. in
  let machine, bob = setup_aged_olm ~age:3601. ~seed:"unwedge-retry" in
  ignore (fail_olm_decrypt machine bob ~now);
  ignore (claim_request_for machine);
  (* An empty response does not acknowledge the repair. *)
  ignore
    (Encryption.receive_keys_claim machine
       { Keys.failures = []; one_time_keys = [] });
  ignore (claim_request_for machine);
  (* A valid response opens the replacement and queues one dummy. *)
  ignore (Encryption.receive_keys_claim machine (claim_response_for bob));
  let dummies = dummy_requests machine in
  check_int "one encrypted dummy is queued" 1 (List.length dummies);
  check_string "the queued message is m.dummy" "m.dummy"
    (decrypt_dummy bob machine (snd (List.hd dummies)));
  let restarted =
    Encryption.of_snapshot
      ~random:(random_of "unwedge-restart")
      ~user_id:alice_id ~device_id:(did "ALICEDEV")
      (Encryption.snapshot machine)
  in
  let restarted_dummies = dummy_requests restarted in
  check_int "the dummy survives restart" 1 (List.length restarted_dummies);
  let txn_id, messages = List.hd dummies in
  check_bool "retry keeps the same transaction and ciphertext" true
    (List.mem_assoc txn_id restarted_dummies
    && List.assoc txn_id restarted_dummies = messages);
  let request =
    Encryption.To_device { event_type = "m.room.encrypted"; txn_id; messages }
  in
  (* It remains pending until the driver explicitly acknowledges it. *)
  check_int "dummy is still pending before mark_sent" 1
    (List.length (dummy_requests machine));
  Encryption.mark_sent machine request;
  check_int "mark_sent clears only the dummy" 0
    (List.length (dummy_requests machine))

let empty_claim_response ?(failures = []) () =
  { Keys.failures; one_time_keys = [] }

let claim_present ?now machine ~members =
  List.exists
    (function Encryption.Keys_claim _ -> true | _ -> false)
    (Encryption.ensure_sessions ?now machine ~members)

let test_olm_claim_exhaustion_backoff () =
  let now = at 1_700_000_000. in
  let alice =
    make ~seed:"claim-backoff-alice" ~user:alice_id ~device:(did "ALICEDEV") ()
  in
  let bob =
    make ~seed:"claim-backoff-bob" ~user:bob_id ~device:(did "BOBDEV") ()
  in
  introduce ~from:bob ~into:alice;
  ignore (Encryption.ensure_sessions ~now alice ~members:[ bob_id ]);
  ignore (Encryption.receive_keys_claim ~now alice (empty_claim_response ()));
  check_bool "an exhausted device is immediately suppressed" false
    (claim_present ~now alice ~members:[ bob_id ]);
  check_bool "the first retry is suppressed before 15 seconds" false
    (claim_present ~now:(at 1_700_000_014.) alice ~members:[ bob_id ]);
  let expected = [ 15.; 30.; 60.; 120.; 240.; 480.; 900. ] in
  let elapsed = ref 0. in
  List.iter
    (fun delay ->
      elapsed := !elapsed +. delay;
      let retry_at = at (1_700_000_000. +. !elapsed) in
      check_bool "retry is allowed at exact expiry" true
        (claim_present ~now:retry_at alice ~members:[ bob_id ]);
      ignore
        (Encryption.receive_keys_claim ~now:retry_at alice
           (empty_claim_response ()));
      check_bool "retry is suppressed again after the response" false
        (claim_present
           ~now:(at (1_700_000_000. +. !elapsed +. delay -. 1.))
           alice ~members:[ bob_id ]))
    expected

let test_olm_claim_exhaustion_server_and_unrelated_response () =
  let now = at 1_700_000_000. in
  let alice =
    make ~seed:"claim-server-alice" ~user:alice_id ~device:(did "ALICEDEV") ()
  in
  let bob =
    make ~seed:"claim-server-bob" ~user:bob_id ~device:(did "BOBDEV") ()
  in
  introduce ~from:bob ~into:alice;
  ignore (Encryption.ensure_sessions ~now alice ~members:[ bob_id ]);
  let server = Id.Server_name.of_string_exn "example.org" in
  ignore
    (Encryption.receive_keys_claim ~now alice
       (empty_claim_response ~failures:[ (server, jstr "unreachable") ] ()));
  check_bool "server failure does not poison its devices" true
    (claim_present ~now alice ~members:[ bob_id ]);

  (* A response entry for a device that was not requested must not suppress it
     when it is encountered in a later, independent claim. *)
  let carol_id = uid "@carol:example.org" in
  let carol =
    make ~seed:"claim-unrelated-carol" ~user:carol_id ~device:(did "CAROLDEV")
      ()
  in
  ignore (Encryption.ensure_sessions ~now alice ~members:[ bob_id ]);
  ignore
    (Encryption.receive_keys_claim ~now alice
       {
         Keys.failures = [];
         one_time_keys = [ (carol_id, [ (Encryption.device_id carol, []) ]) ];
       });
  check_bool "the actually requested device is suppressed" false
    (claim_present ~now alice ~members:[ bob_id ]);
  introduce ~from:carol ~into:alice;
  check_bool "unrelated response entries are not cached as failures" true
    (claim_present ~now alice ~members:[ carol_id ])

let test_olm_claim_exhaustion_forced_unwedge_bypasses_backoff () =
  let now = at 1_700_000_000. in
  let machine, bob = setup_aged_olm ~age:3601. ~seed:"claim-forced-backoff" in
  ignore (fail_olm_decrypt machine bob ~now);
  ignore (claim_request_for machine);
  ignore (Encryption.receive_keys_claim ~now machine (empty_claim_response ()));
  check_bool "forced stale-session repair bypasses exhaustion backoff" true
    (has_claim machine)

(* Alice encrypts, the to-device traffic is fished out of the mock and handed
   to Bob as a sync, and Bob is left holding the Megolm session. *)
let share_and_encrypt alice bob client log ~body =
  let content = jobj [ ("msgtype", jstr "m.text"); ("body", jstr body) ] in
  let encrypted =
    ok_value
      (Driver.encrypt_room_event (drive alice) client room
         ~event_type:"m.room.message" ~content ~members:[ alice_id; bob_id ])
  in
  let events =
    List.filter_map
      (fun (u, d, c) ->
        if
          u = Id.User_id.to_string (Encryption.user_id bob)
          && d = Id.Device_id.to_string (Encryption.device_id bob)
        then
          Some
            (to_device_event
               ~sender:(Id.User_id.to_string (Encryption.user_id alice))
               ~event_type:"m.room.encrypted" ~content:c)
        else None)
      (last_to_device log)
  in
  let outcome = Encryption.process_sync bob (sync_of ~to_device:events ()) in
  (encrypted, outcome)

let one_withheld_request = function
  | [
      Encryption.To_device
        {
          event_type = "m.room_key.withheld";
          txn_id;
          messages =
            [ (user, [ (Matrix_client.To_device.Device device, content) ]) ];
        };
    ] ->
      (txn_id, user, device, content)
  | _ -> Alcotest.fail "expected one m.room_key.withheld request"

let encrypt_test_event machine =
  Encryption.encrypt_room_event machine room ~event_type:"m.room.message"
    ~content:(jobj [ ("msgtype", jstr "m.text"); ("body", jstr "hello") ])
    ~members:[ bob_id ]
  |> ok_value

let test_blacklisted_withheld () =
  let alice =
    make ~seed:"withheld-alice" ~user:alice_id ~device:(did "ALICEDEV") ()
  in
  let bob = make ~seed:"withheld-bob" ~user:bob_id ~device:(did "BOBDEV") () in
  introduce ~from:bob ~into:alice;
  Encryption.set_device_trust alice bob_id ~device_id:(did "BOBDEV")
    Encryption.Blacklisted;
  let _, first = encrypt_test_event alice in
  let txn_id, user, device, content = one_withheld_request first in
  check_string "recipient"
    (Id.User_id.to_string bob_id)
    (Id.User_id.to_string user);
  check_string "device" "BOBDEV" (Id.Device_id.to_string device);
  check_string "content"
    (Printf.sprintf
       {|{"room_id":"%s","session_id":"%s","algorithm":"m.megolm.v1.aes-sha2","code":"m.blacklisted","reason":"The sender has blocked you.","sender_key":"%s","from_device":"ALICEDEV"}|}
       room_str
       (Id.Session_id.to_string
          (Option.get (Encryption.outbound_session_id alice room)))
       (Ck.Curve25519.Public.to_base64 (snd (Encryption.identity_keys alice))))
    (json_to_string content);
  let _, second = encrypt_test_event alice in
  let txn_id', _, _, content' = one_withheld_request second in
  check_string "retry retains transaction id" txn_id txn_id';
  check_string "retry retains body" (json_to_string content)
    (json_to_string content');
  let alice_unsent =
    Encryption.of_snapshot
      ~random:(random_of "withheld-unsent-restart")
      ~user_id:alice_id ~device_id:(did "ALICEDEV")
      (Encryption.snapshot alice)
  in
  let _, after_unsent_restart = encrypt_test_event alice_unsent in
  let restarted_txn, _, _, restarted_content =
    one_withheld_request after_unsent_restart
  in
  check_string "unsent restart retains transaction id" txn_id restarted_txn;
  check_string "unsent restart retains body" (json_to_string content)
    (json_to_string restarted_content);
  Encryption.mark_sent alice_unsent (List.hd after_unsent_restart);
  let alice_sent =
    Encryption.of_snapshot
      ~random:(random_of "withheld-sent-restart")
      ~user_id:alice_id ~device_id:(did "ALICEDEV")
      (Encryption.snapshot alice_unsent)
  in
  let _, after_sent_restart = encrypt_test_event alice_sent in
  check_bool "sent notice stays suppressed after restart" true
    (not
       (List.exists
          (function Encryption.To_device _ -> true | _ -> false)
          after_sent_restart));
  Encryption.mark_sent alice (List.hd first);
  let _, third = encrypt_test_event alice in
  check_bool "sent notice is not repeated" true
    (not
       (List.exists
          (function Encryption.To_device _ -> true | _ -> false)
          third))

let test_no_olm_withheld () =
  let alice =
    make ~seed:"no-olm-alice" ~user:alice_id ~device:(did "ALICEDEV") ()
  in
  let bob = make ~seed:"no-olm-bob" ~user:bob_id ~device:(did "BOBDEV") () in
  introduce ~from:bob ~into:alice;
  let _, first = encrypt_test_event alice in
  let txn_id, _, _, content = one_withheld_request first in
  ignore txn_id;
  check_string "content"
    (Printf.sprintf
       {|{"algorithm":"m.megolm.v1.aes-sha2","code":"m.no_olm","reason":"Unable to establish a secure channel.","sender_key":"%s","from_device":"ALICEDEV"}|}
       (Ck.Curve25519.Public.to_base64 (snd (Encryption.identity_keys alice))))
    (json_to_string content);
  Encryption.mark_sent alice (List.hd first);
  ok_value
    (Encryption.set_room_encryption_settings alice room
       (Encryption.room_encryption_content
          (Encryption.enable_room_encryption ~rotation_period_msgs:0 ())));
  let _, second = encrypt_test_event alice in
  check_bool "sent notice is not repeated" true
    (not
       (List.exists
          (function Encryption.To_device _ -> true | _ -> false)
          second))

let test_blacklisting_shared_device_rotates_session () =
  let alice =
    make ~seed:"rotate-alice" ~user:alice_id ~device:(did "ALICEDEV") ()
  in
  let bob = make ~seed:"rotate-bob" ~user:bob_id ~device:(did "BOBDEV") () in
  introduce ~from:bob ~into:alice;
  ignore (Encryption.receive_keys_claim alice (claim_response_for bob));
  let _, first = encrypt_test_event alice in
  let old_session = Option.get (Encryption.outbound_session_id alice room) in
  List.iter (Encryption.mark_sent alice) first;
  Encryption.set_device_trust alice bob_id ~device_id:(did "BOBDEV")
    Encryption.Blacklisted;
  let _, second = encrypt_test_event alice in
  let new_session = Option.get (Encryption.outbound_session_id alice room) in
  check_bool "blacklisting rotates the shared session" true
    (not (Id.Session_id.equal old_session new_session));
  let _, _, _, content = one_withheld_request second in
  check_string "new session is named in the notice"
    (Printf.sprintf
       {|{"room_id":"%s","session_id":"%s","algorithm":"m.megolm.v1.aes-sha2","code":"m.blacklisted","reason":"The sender has blocked you.","sender_key":"%s","from_device":"ALICEDEV"}|}
       room_str
       (Id.Session_id.to_string new_session)
       (Ck.Curve25519.Public.to_base64 (snd (Encryption.identity_keys alice))))
    (json_to_string content)

(* {1 Tests} *)

let test_round_trip () =
  let log, client = permissive_client () in
  let alice = make ~seed:"alice" ~user:alice_id ~device:(did "ALICEDEV") () in
  let bob = make ~seed:"bob" ~user:bob_id ~device:(did "BOBDEV") () in
  introduce ~from:bob ~into:alice;
  introduce ~from:alice ~into:bob;
  introduce ~from:alice ~into:alice;
  check_int "Alice opens Bob's Olm session" 1
    (Encryption.receive_keys_claim alice (claim_response_for bob));
  let encrypted, outcome =
    share_and_encrypt alice bob client log ~body:"hello bob"
  in
  check_int "Bob learned one Megolm session" 1
    (List.length outcome.new_sessions);
  (match (Encryption.snapshot bob).state.session_meta with
  | [ metadata ] ->
      check_bool "a newly received session is not legacy" false metadata.legacy
  | _ -> Alcotest.fail "expected metadata for Bob's received session");
  let session_id = snd (List.hd outcome.new_sessions) in
  let withheld_event =
    to_device_event
      ~sender:(Id.User_id.to_string alice_id)
      ~event_type:"m.room_key.withheld"
      ~content:
        (jobj
           [
             ("algorithm", jstr "m.megolm.v1.aes-sha2");
             ( "sender_key",
               jstr
                 (Ck.Curve25519.Public.to_base64
                    (snd (Encryption.identity_keys alice))) );
             ("room_id", jstr room_str);
             ("session_id", jstr (Id.Session_id.to_string session_id));
             ("code", jstr "m.unverified");
           ])
  in
  ignore
    (Encryption.process_sync bob (sync_of ~to_device:[ withheld_event ] ()));
  check_bool "withheld does not replace an already held session" true
    (Encryption.withheld_for bob ~room_id:room ~session_id = None);
  (match outcome.events with
  | [ Encryption.Room_key { room_id; _ } ] ->
      check_string "the key is for the right room" room_str
        (Id.Room_id.to_string room_id)
  | _ -> Alcotest.fail "expected exactly one m.room_key");
  let raw = encrypted_raw ~sender:alice_id ~content:encrypted in
  match Encryption.decrypt_room_event bob room raw with
  | Error e ->
      Alcotest.failf "decryption failed: %a" Encryption.pp_decrypt_error e
  | Ok d ->
      check_string "event type survives" "m.room.message" d.decrypted_type;
      check_string "content survives"
        {|{"msgtype":"m.text","body":"hello bob"}|}
        (json_to_string d.decrypted_content);
      check_string "the sender key is Alice's"
        (Ck.Curve25519.Public.to_base64 (snd (Encryption.identity_keys alice)))
        (Ck.Curve25519.Public.to_base64 d.decrypted_sender_key);
      check_string "the claimed Ed25519 key is Alice's"
        (Ck.Ed25519.Public.to_base64 (fst (Encryption.identity_keys alice)))
        (match d.decrypted_claimed_ed25519 with
        | Some k -> Ck.Ed25519.Public.to_base64 k
        | None -> "-");
      check_bool "Alice's signed device info is known" true
        (d.decrypted_verification = Encryption.Device_info)

let test_trust_requirements () =
  let log, client = permissive_client () in
  let alice =
    make ~seed:"trust-alice" ~user:alice_id ~device:(did "ALICEDEV") ()
  in
  let bob = make ~seed:"trust-bob" ~user:bob_id ~device:(did "BOBDEV") () in
  introduce ~from:bob ~into:alice;
  introduce ~from:alice ~into:bob;
  ignore (Encryption.receive_keys_claim alice (claim_response_for bob));
  let encrypted, _ = share_and_encrypt alice bob client log ~body:"trust" in
  let raw = encrypted_raw ~sender:alice_id ~content:encrypted in
  (match Encryption.decrypt_room_event bob room raw with
  | Ok { decrypted_verification = Encryption.Device_info; _ } -> ()
  | Ok _ -> Alcotest.fail "an unsigned device got the wrong sender state"
  | Error e ->
      Alcotest.failf "default requirement rejected the event: %a"
        Encryption.pp_decrypt_error e);
  Encryption.set_trust_requirement bob Encryption.Cross_signed_or_legacy;
  (match Encryption.decrypt_room_event bob room raw with
  | Error (Encryption.Not_trusted { verification = Encryption.Device_info; _ })
    ->
      ()
  | Error e ->
      Alcotest.failf "expected Device_info rejection, got %a"
        Encryption.pp_decrypt_error e
  | Ok _ -> Alcotest.fail "an unsigned sender must not pass strict trust");
  (* A legacy session with no sender metadata remains readable only at the
     middle requirement. This is the migration rule for old crypto stores. *)
  let snapshot = Encryption.snapshot bob in
  let legacy =
    Encryption.of_snapshot ~random:(random_of "trust-legacy") ~user_id:bob_id
      ~device_id:(did "BOBDEV")
      { snapshot with state = { snapshot.state with session_meta = [] } }
  in
  Encryption.set_trust_requirement legacy Encryption.Cross_signed_or_legacy;
  check_bool "legacy unsigned session passes the compatibility policy" true
    (Result.is_ok (Encryption.decrypt_room_event legacy room raw));
  Encryption.set_trust_requirement legacy Encryption.Cross_signed;
  check_bool "legacy alone does not satisfy cross-signed" false
    (Result.is_ok (Encryption.decrypt_room_event legacy room raw));
  (* Once the same device is vouched for by a valid owner chain, an unverified
     identity is cross-signed enough for both strict Rust policies. *)
  Encryption.receive_keys_query bob
    (cross_signed_query_for ~user_id:alice_id ~seed:"trust-alice-identity" alice);
  Encryption.set_trust_requirement bob Encryption.Cross_signed;
  (match Encryption.decrypt_room_event bob room raw with
  | Ok { decrypted_verification = Encryption.Sender_unverified; _ } -> ()
  | Ok _ -> Alcotest.fail "cross-signed sender got the wrong unverified state"
  | Error e ->
      Alcotest.failf "cross-signed unverified identity was rejected: %a"
        Encryption.pp_decrypt_error e);
  Encryption.trust_user_identity bob alice_id;
  (match Encryption.decrypt_room_event bob room raw with
  | Ok { decrypted_verification = Encryption.Sender_verified; _ } -> ()
  | Ok _ -> Alcotest.fail "verified sender got the wrong sender state"
  | Error e ->
      Alcotest.failf "verified sender was rejected: %a"
        Encryption.pp_decrypt_error e);
  (* The user that sent the authenticated room key is part of the session's
     ownership evidence. A copied ciphertext must not become trusted merely
     because its Curve25519 key maps to the apparent event sender. *)
  let snapshot = Encryption.snapshot bob in
  let mismatched =
    {
      snapshot with
      state =
        {
          snapshot.state with
          session_meta =
            List.map
              (fun (m : Encryption.Session_meta.t) ->
                { m with sender = Some bob_id })
              snapshot.state.session_meta;
        };
    }
  in
  let mismatched =
    Encryption.of_snapshot
      ~random:(random_of "trust-mismatched-sender")
      ~user_id:bob_id ~device_id:(did "BOBDEV") mismatched
  in
  Encryption.set_trust_requirement mismatched Encryption.Untrusted;
  (match Encryption.decrypt_room_event mismatched room raw with
  | Ok { decrypted_verification = Encryption.Unknown_device; _ } -> ()
  | Ok _ -> Alcotest.fail "mismatched session owner was trusted"
  | Error e ->
      Alcotest.failf "untrusted mode rejected mismatched ownership: %a"
        Encryption.pp_decrypt_error e);
  Encryption.receive_keys_query bob
    (cross_signed_query_for ~user_id:alice_id ~seed:"trust-alice-rotated" alice);
  (match Encryption.decrypt_room_event bob room raw with
  | Error
      (Encryption.Not_trusted
         { verification = Encryption.Verification_violation; _ }) ->
      ()
  | Error e ->
      Alcotest.failf "wrong violation error: %a" Encryption.pp_decrypt_error e
  | Ok _ -> Alcotest.fail "a verification violation passed strict trust");
  Encryption.set_trust_requirement bob Encryption.Untrusted;
  match Encryption.decrypt_room_event bob room raw with
  | Ok { decrypted_verification = Encryption.Verification_violation; _ } -> ()
  | Ok _ -> Alcotest.fail "untrusted mode hid the verification violation"
  | Error e ->
      Alcotest.failf "untrusted mode rejected a decryptable violation: %a"
        Encryption.pp_decrypt_error e

let test_utd_cause_classifier () =
  let sid = Result.get_ok (Id.Session_id.of_string "sess") in
  let unknown =
    Encryption.Unknown_session
      { room_id = room; session_id = sid; sender_key = None; sender = alice_id }
  in
  let event = encrypted_raw ~sender:alice_id ~content:(jobj []) in
  let check name expected actual = check_bool name true (expected = actual) in
  let context =
    {
      Encryption.default_utd_context with
      device_created_at = Some (Option.get (Ptime.of_float_s 100.));
    }
  in
  let pre_join =
    {
      event with
      origin_server_ts = Matrix_proto.Event.Timestamp.of_ms 101_000L;
      unsigned = Some (Matrix_proto.Event.Unsigned.make ~membership:"leave" ());
    }
  in
  check "before joining" Encryption.Sent_before_we_joined
    (Encryption.classify_utd pre_join unknown context);
  check "backup disabled" Encryption.Historical_message_and_backup_is_disabled
    (Encryption.classify_utd event unknown context);
  check "device unverified"
    Encryption.Historical_message_and_device_is_unverified
    (Encryption.classify_utd event unknown
       { context with backup_exists = true; backup_configured = false });
  let old_index =
    Encryption.Unknown_message_index
      {
        room_id = room;
        session_id = sid;
        sender_key = None;
        sender = alice_id;
        message_index = 2;
        first_known = 5;
      }
  in
  check "old message index is historical too"
    Encryption.Historical_message_and_backup_is_disabled
    (Encryption.classify_utd event old_index context);
  let trusted_context =
    {
      context with
      backup_exists = true;
      backup_configured = true;
      local_device_verified = true;
    }
  in
  check "otherwise unknown" Encryption.Unknown
    (Encryption.classify_utd event unknown trusted_context);
  let withhold code =
    {
      trusted_context with
      withheld =
        Some
          {
            room_id = room;
            session_id = sid;
            code;
            reason = None;
            sender_key = None;
            from_device = None;
            sender_user = None;
          };
    }
  in
  check "unverified withholding"
    Encryption.Withheld_for_unverified_or_insecure_device
    (Encryption.classify_utd event unknown (withhold "m.unverified"));
  check "sender withholding" Encryption.Withheld_by_sender
    (Encryption.classify_utd event unknown (withhold "m.no_olm"));
  check "history not shared falls through" Encryption.Unknown
    (Encryption.classify_utd
       {
         event with
         origin_server_ts = Matrix_proto.Event.Timestamp.of_ms 101_000L;
       }
       unknown
       (withhold "m.history_not_shared"));
  let not_trusted verification =
    Encryption.Not_trusted
      { requirement = Encryption.Cross_signed; sender = alice_id; verification }
  in
  check "verification violation"
    (Encryption.Verification_violation : Encryption.utd_cause)
    (Encryption.classify_utd event
       (not_trusted Encryption.Verification_violation)
       trusted_context);
  check "unsigned device" Encryption.Unsigned_device
    (Encryption.classify_utd event
       (not_trusted Encryption.Device_info)
       trusted_context);
  check "unknown device"
    (Encryption.Unknown_device : Encryption.utd_cause)
    (Encryption.classify_utd event
       (not_trusted Encryption.Unknown_device)
       trusted_context);
  check "unverified identity is not a historical cause" Encryption.Unknown
    (Encryption.classify_utd event
       (not_trusted Encryption.Sender_unverified)
       trusted_context);
  let machine =
    make ~seed:"withheld-store" ~user:alice_id ~device:(did "ALICEDEV") ()
  in
  let _, sender_key = Encryption.identity_keys machine in
  let invalid_sid = Result.get_ok (Id.Session_id.of_string "bad-sess") in
  let invalid_withheld_event =
    to_device_event
      ~sender:(Id.User_id.to_string bob_id)
      ~event_type:"m.room_key.withheld"
      ~content:
        (jobj
           [
             ("algorithm", jstr "m.megolm.v2.aes-sha2");
             ("sender_key", jstr (Ck.Curve25519.Public.to_base64 sender_key));
             ("room_id", jstr room_str);
             ("session_id", jstr (Id.Session_id.to_string invalid_sid));
             ("code", jstr "m.unverified");
           ])
  in
  ignore
    (Encryption.process_sync machine
       (sync_of ~to_device:[ invalid_withheld_event ] ()));
  check_bool "unsupported withheld evidence is ignored" true
    (Encryption.withheld_for machine ~room_id:room ~session_id:invalid_sid
    = None);
  let withheld_event =
    to_device_event
      ~sender:(Id.User_id.to_string bob_id)
      ~event_type:"m.room_key.withheld"
      ~content:
        (jobj
           [
             ("algorithm", jstr "m.megolm.v1.aes-sha2");
             ("sender_key", jstr (Ck.Curve25519.Public.to_base64 sender_key));
             ("room_id", jstr room_str);
             ("session_id", jstr (Id.Session_id.to_string sid));
             ("code", jstr "m.unverified");
             ("reason", jstr "the receiving device is unverified");
           ])
  in
  ignore
    (Encryption.process_sync machine (sync_of ~to_device:[ withheld_event ] ()));
  let snapshot = Encryption.snapshot machine in
  let restarted =
    Encryption.of_snapshot
      ~random:(random_of "withheld-restart")
      ~user_id:alice_id ~device_id:(did "ALICEDEV") snapshot
  in
  check_bool "withheld evidence survives snapshot" true
    (Option.is_some
       (Encryption.withheld_for restarted ~room_id:room ~session_id:sid))

let test_rotation () =
  let log, client = permissive_client () in
  let alice =
    make ~seed:"alice-rot" ~user:alice_id ~device:(did "ALICEDEV") ()
  in
  let bob = make ~seed:"bob-rot" ~user:bob_id ~device:(did "BOBDEV") () in
  introduce ~from:bob ~into:alice;
  introduce ~from:alice ~into:alice;
  ignore (Encryption.receive_keys_claim alice (claim_response_for bob));
  ok_value
    (Encryption.set_room_encryption_settings alice room
       (Encryption.room_encryption_content
          (Encryption.enable_room_encryption ~rotation_period_msgs:3 ())));
  let send n =
    let content = jobj [ ("body", jstr (string_of_int n)) ] in
    ignore
      (ok_value
         (Driver.encrypt_room_event (drive alice) client room
            ~event_type:"m.room.message" ~content ~members:[ alice_id; bob_id ]))
  in
  send 1;
  let first = Option.get (Encryption.outbound_session_id alice room) in
  send 2;
  send 3;
  check_string "three messages fit in one session"
    (Id.Session_id.to_string first)
    (Id.Session_id.to_string
       (Option.get (Encryption.outbound_session_id alice room)));
  check_int "the counter reached the limit" 3
    (Encryption.outbound_message_count alice room);
  send 4;
  let second = Option.get (Encryption.outbound_session_id alice room) in
  check_bool "the fourth message rotated the session" false
    (Id.Session_id.equal first second);
  check_int "the new session starts again" 1
    (Encryption.outbound_message_count alice room);
  ignore (requests log)

let test_unknown_session () =
  let alice =
    make ~seed:"alice-unk" ~user:alice_id ~device:(did "ALICEDEV") ()
  in
  let content =
    jobj
      [
        ("algorithm", jstr "m.megolm.v1.aes-sha2");
        ("ciphertext", jstr "AwgAEnB1cmVseSBub3QgYSByZWFsIG1lc3NhZ2U");
        ("session_id", jstr "NoSuchSession");
        ("sender_key", jstr "aGVsbG8");
      ]
  in
  (* An algorithm we do not implement is reported as such, not as a broken
     body. *)
  (match
     Encryption.decrypt_room_event alice room
       (encrypted_raw ~sender:bob_id
          ~content:
            (jobj
               [
                 ("algorithm", jstr "m.olm.v1.curve25519-aes-sha2");
                 ("ciphertext", jstr "x");
                 ("session_id", jstr "s");
               ]))
   with
  | Error (Encryption.Unsupported_algorithm a) ->
      check_string "the algorithm is named" "m.olm.v1.curve25519-aes-sha2" a
  | Error e -> Alcotest.failf "wrong error: %a" Encryption.pp_decrypt_error e
  | Ok _ -> Alcotest.fail "a foreign algorithm must not decrypt");
  let raw = encrypted_raw ~sender:bob_id ~content in
  (match Encryption.decrypt_room_event alice room raw with
  | Error (Encryption.Unknown_session { session_id; room_id; _ }) ->
      check_string "the error names the session" "NoSuchSession"
        (Id.Session_id.to_string session_id);
      check_string "and the room" room_str (Id.Room_id.to_string room_id)
  | Error e -> Alcotest.failf "wrong error: %a" Encryption.pp_decrypt_error e
  | Ok _ -> Alcotest.fail "an unknown session must not decrypt");
  match
    Encryption.request_room_key alice ~room_id:room
      ~session_id:(Id.Session_id.of_string_exn "NoSuchSession")
      ()
  with
  | Encryption.To_device { event_type; messages; _ } -> (
      check_string "the request is an m.room_key_request" "m.room_key_request"
        event_type;
      match messages with
      | [ (u, [ (d, content) ]) ] ->
          check_string "addressed to ourselves"
            (Id.User_id.to_string alice_id)
            (Id.User_id.to_string u);
          check_bool "and to all our devices" true
            (d = Matrix_client.To_device.All);
          check_bool "it asks for the session" true
            (contains "NoSuchSession" (json_to_string content))
      | _ -> Alcotest.fail "unexpected message shape")
  | _ -> Alcotest.fail "expected a to-device request"

let key_request_content ~device ~session_id =
  jobj
    [
      ("action", jstr "request");
      ("requesting_device_id", jstr (Id.Device_id.to_string device));
      ("request_id", jstr "req-1");
      ( "body",
        jobj
          [
            ("algorithm", jstr "m.megolm.v1.aes-sha2");
            ("room_id", jstr room_str);
            ("session_id", jstr (Id.Session_id.to_string session_id));
          ] );
    ]

let test_gossip_refused_for_other_user () =
  let log, client = permissive_client () in
  let alice =
    make ~seed:"alice-gossip" ~user:alice_id ~device:(did "ALICEDEV") ()
  in
  let bob = make ~seed:"bob-gossip" ~user:bob_id ~device:(did "BOBDEV") () in
  introduce ~from:bob ~into:alice;
  introduce ~from:alice ~into:alice;
  ignore (Encryption.receive_keys_claim alice (claim_response_for bob));
  let _, _ = share_and_encrypt alice bob client log ~body:"secret" in
  let session_id = Option.get (Encryption.outbound_session_id alice room) in
  (* Bob is verified, but he is not us: his request must still be refused. *)
  Encryption.set_device_trust alice bob_id ~device_id:(did "BOBDEV")
    Encryption.Verified;
  let event =
    to_device_event
      ~sender:(Id.User_id.to_string bob_id)
      ~event_type:"m.room_key_request"
      ~content:(key_request_content ~device:(did "BOBDEV") ~session_id)
  in
  let outcome =
    Encryption.process_sync alice (sync_of ~to_device:[ event ] ())
  in
  (match outcome.events with
  | [ Encryption.Room_key_request { answered; session_id = s; _ } ] ->
      check_bool "the request is refused" false answered;
      check_string "but it is reported" (Id.Session_id.to_string session_id) s
  | _ -> Alcotest.fail "expected one m.room_key_request");
  check_bool "and nothing is forwarded" false
    (List.exists
       (function
         | Encryption.To_device { event_type = "m.room.encrypted"; _ } -> true
         | _ -> false)
       outcome.requests)

(* [m.secret.send] must arrive over Olm. Requests are intentionally plaintext
   (as in the Rust gossip machine), but an untrusted/unknown request never
   causes a response. *)
let test_secret_events_require_olm () =
  let alice =
    make ~seed:"alice-secret" ~user:alice_id ~device:(did "ALICEDEV") ()
  in
  let send_event =
    to_device_event
      ~sender:(Id.User_id.to_string bob_id)
      ~event_type:"m.secret.send"
      ~content:
        (jobj
           [ ("request_id", jstr "req-1"); ("secret", jstr "forged-secret") ])
  in
  let request_event =
    to_device_event
      ~sender:(Id.User_id.to_string bob_id)
      ~event_type:"m.secret.request"
      ~content:
        (jobj
           [
             ("action", jstr "request");
             ("requesting_device_id", jstr "BOBDEV");
             ("request_id", jstr "req-2");
             ("name", jstr "m.cross_signing.master");
           ])
  in
  let outcome =
    Encryption.process_sync alice
      (sync_of ~to_device:[ send_event; request_event ] ())
  in
  match outcome.events with
  | [ Encryption.Undecryptable _; Encryption.Secret_request _ ] -> ()
  | events ->
      Alcotest.failf
        "expected one Undecryptable and one Secret_request, got: %a"
        Fmt.(list Encryption.pp_to_device_event)
        events

let own_olm_pair () =
  let alice =
    make ~seed:"secret-request-alice" ~user:alice_id ~device:(did "ALICEDEV") ()
  in
  let alice2 =
    make ~seed:"secret-request-alice2" ~user:alice_id ~device:(did "ALICE2") ()
  in
  introduce ~from:alice2 ~into:alice;
  introduce ~from:alice ~into:alice2;
  Encryption.set_device_trust alice alice_id ~device_id:(did "ALICE2")
    Encryption.Verified;
  Encryption.set_device_trust alice2 alice_id ~device_id:(did "ALICEDEV")
    Encryption.Verified;
  ignore (Encryption.receive_keys_claim alice (claim_response_for alice2));
  ignore (Encryption.receive_keys_claim alice2 (claim_response_for alice));
  (alice, alice2)

let replace_json_member name value = function
  | Jsont.Object (members, meta) ->
      Jsont.Object
        ( Jsont.Json.mem (Jsont.Json.name name) value
          :: List.filter
               (fun ((key, _), _) -> not (String.equal key name))
               members,
          meta )
  | _ -> Alcotest.fail "expected a JSON object"

let json_member name = function
  | Jsont.Object (members, _) ->
      Option.map snd
        (List.find_opt (fun ((key, _), _) -> String.equal key name) members)
  | _ -> None

let sender_device_keys_json sender =
  Result.get_ok
    (Jsont.Json.encode Keys.device_keys_jsont
       (Encryption.device_keys_for_upload sender))

let resign_sender_device_keys sender json =
  let keys = Result.get_ok (Jsont.Json.decode Keys.device_keys_jsont json) in
  let unsigned = { keys with signatures = []; unsigned = None } in
  let signing_json =
    Result.get_ok (Jsont.Json.encode Keys.device_keys_jsont unsigned)
  in
  let signature =
    Olm.Account.sign (Encryption.snapshot sender).account
      (Matrix_proto.Signed_json.canonical_json signing_json)
  in
  let key_id = Ck.Key_id.of_device ~algorithm:"ed25519" unsigned.device_id in
  Result.get_ok
    (Jsont.Json.encode Keys.device_keys_jsont
       {
         unsigned with
         signatures = [ (unsigned.user_id, [ (key_id, signature) ]) ];
       })

let encrypted_bundle_event_with_device_keys ~sender_device_keys ~sender
    ~receiver ~event_type content =
  let sender_ed, sender_curve = Encryption.identity_keys sender in
  let receiver_ed, receiver_curve = Encryption.identity_keys receiver in
  let session =
    List.find
      (fun session ->
        Ck.Curve25519.Public.equal
          (Olm.Session.their_identity_key session)
          receiver_curve)
      (Encryption.snapshot sender).olm_sessions
  in
  let plaintext : Ev.Olm_plaintext.t =
    {
      event_type;
      content;
      sender = Encryption.user_id sender;
      sender_ed25519 = Ck.Ed25519.Public.to_base64 sender_ed;
      recipient = Encryption.user_id receiver;
      recipient_ed25519 = Ck.Ed25519.Public.to_base64 receiver_ed;
      sender_device_keys;
    }
  in
  let plaintext =
    Result.get_ok (Jsont_bytesrw.encode_string Ev.Olm_plaintext.jsont plaintext)
  in
  let message =
    match
      Olm.Session.encrypt ~random:(random_of "bundle-wire") session plaintext
    with
    | Ok message -> message
    | Error error ->
        Alcotest.failf "could not encrypt bundle: %a" Olm.pp_error error
  in
  let body : Ev.Encrypted.Olm.t =
    {
      sender_key = Ck.Curve25519.Public.to_base64 sender_curve;
      ciphertext =
        [
          {
            Ev.Olm_ciphertext.recipient_key =
              Ck.Curve25519.Public.to_base64 receiver_curve;
            message_type = message.message_type;
            body = message.ciphertext;
          };
        ];
    }
  in
  to_device_event
    ~sender:(Id.User_id.to_string (Encryption.user_id sender))
    ~event_type:"m.room.encrypted"
    ~content:(Result.get_ok (Jsont.Json.encode Ev.Encrypted.Olm.jsont body))

let encrypted_bundle_event ~sender ~receiver ~event_type content =
  encrypted_bundle_event_with_device_keys
    ~sender_device_keys:(Some (sender_device_keys_json sender))
    ~sender ~receiver ~event_type content

let bundle_file seed =
  let encrypted =
    Matrix_client.Encrypted_attachment.encrypt ~random:(random_of seed)
      "room-key-bundle"
  in
  Matrix_client.Encrypted_attachment.Metadata.to_event_file
    ~url:("mxc://hs.example/" ^ seed)
    encrypted.metadata

let encrypted_bundle_file seed plaintext =
  let encrypted =
    Matrix_client.Encrypted_attachment.encrypt ~random:(random_of seed)
      plaintext
  in
  ( Matrix_client.Encrypted_attachment.Metadata.to_event_file
      ~url:("mxc://hs.example/" ^ seed)
      encrypted.metadata,
    encrypted.ciphertext )

let bundle_content ?(room_id = room) file =
  Result.get_ok
    (Jsont.Json.encode Encryption.room_key_bundle_content_jsont
       { Encryption.room_id; file })

let test_room_key_bundle_receipt_and_replacement () =
  let sender, receiver = own_olm_pair () in
  let first = bundle_content (bundle_file "bundle-first") in
  let outcome =
    Encryption.process_sync receiver
      (sync_of
         ~to_device:
           [
             encrypted_bundle_event ~sender ~receiver
               ~event_type:"m.room_key_bundle" first;
           ]
         ())
  in
  (match outcome.events with
  | [ Encryption.Room_key_bundle received ] ->
      check_string "bundle sender"
        (Id.User_id.to_string alice_id)
        (Id.User_id.to_string received.sender);
      check_string "bundle url" "mxc://hs.example/bundle-first"
        received.file.url
  | _ -> Alcotest.fail "valid room-key bundle was not surfaced");
  check_int "one bundle persisted" 1
    (List.length (Encryption.received_key_bundles receiver));
  let other_room = rid "!bundle-other:example.org" in
  let other = bundle_content ~room_id:other_room (bundle_file "bundle-other") in
  ignore
    (Encryption.process_sync receiver
       (sync_of
          ~to_device:
            [
              encrypted_bundle_event ~sender ~receiver
                ~event_type:"m.room_key_bundle" other;
            ]
          ()));
  let second = bundle_content (bundle_file "bundle-second") in
  let outcome =
    Encryption.process_sync receiver
      (sync_of
         ~to_device:
           [
             encrypted_bundle_event ~sender ~receiver
               ~event_type:"io.element.msc4268.room_key_bundle" second;
           ]
         ())
  in
  (match outcome.events with
  | [ Encryption.Room_key_bundle received ] ->
      check_string "unstable bundle replaces stable bundle"
        "mxc://hs.example/bundle-second" received.file.url
  | _ -> Alcotest.fail "unstable room-key bundle was not surfaced");
  let stored = Encryption.received_key_bundles receiver in
  check_int "replacement retains unrelated room" 2 (List.length stored);
  Alcotest.(check bool)
    "unrelated room retained" true
    (List.exists
       (fun (bundle : Encryption.received_key_bundle) ->
         Id.Room_id.equal bundle.room_id other_room)
       stored)

let test_room_key_bundle_rejects_unauthenticated_and_malformed () =
  let sender, receiver = own_olm_pair () in
  let valid = bundle_content (bundle_file "bundle-malformed") in
  let plaintext_without_file = jobj [ ("room_id", jstr room_str) ] in
  let clear =
    to_device_event
      ~sender:(Id.User_id.to_string alice_id)
      ~event_type:"m.room_key_bundle" ~content:valid
  in
  let clear_outcome =
    Encryption.process_sync receiver (sync_of ~to_device:[ clear ] ())
  in
  (match clear_outcome.events with
  | [ Encryption.Undecryptable _ ] -> ()
  | _ -> Alcotest.fail "clear room-key bundle was accepted");
  ignore
    (Encryption.process_sync receiver
       (sync_of
          ~to_device:
            [
              encrypted_bundle_event ~sender ~receiver
                ~event_type:"m.room_key_bundle" plaintext_without_file;
            ]
          ()));
  let invalid_file =
    { (bundle_file "bundle-invalid-metadata") with hashes = [] }
  in
  let invalid_outcome =
    Encryption.process_sync receiver
      (sync_of
         ~to_device:
           [
             encrypted_bundle_event ~sender ~receiver
               ~event_type:"m.room_key_bundle"
               (bundle_content invalid_file);
           ]
         ())
  in
  (match invalid_outcome.events with
  | [ Encryption.Undecryptable _ ] -> ()
  | _ -> Alcotest.fail "invalid bundle metadata was accepted");
  let invalid_url_file =
    { (bundle_file "bundle-invalid-url") with url = "https://example.org/file" }
  in
  let invalid_url_outcome =
    Encryption.process_sync receiver
      (sync_of
         ~to_device:
           [
             encrypted_bundle_event ~sender ~receiver
               ~event_type:"m.room_key_bundle"
               (bundle_content invalid_url_file);
           ]
         ())
  in
  (match invalid_url_outcome.events with
  | [ Encryption.Undecryptable _ ] -> ()
  | _ -> Alcotest.fail "non-MXC bundle URL was accepted");
  check_int "malformed bundle is not persisted" 0
    (List.length (Encryption.received_key_bundles receiver));
  let reject label device_keys =
    let outcome =
      Encryption.process_sync receiver
        (sync_of
           ~to_device:
             [
               encrypted_bundle_event_with_device_keys
                 ~sender_device_keys:device_keys ~sender ~receiver
                 ~event_type:"m.room_key_bundle" valid;
             ]
           ())
    in
    (match outcome.events with
    | [ Encryption.Undecryptable _ ] -> ()
    | _ -> Alcotest.failf "%s room-key bundle was accepted" label);
    check_int
      (label ^ " is not persisted")
      0
      (List.length (Encryption.received_key_bundles receiver))
  in
  let device_keys = sender_device_keys_json sender in
  reject "missing sender device keys" None;
  reject "wrong sender user"
    (Some
       (replace_json_member "user_id"
          (jstr (Id.User_id.to_string bob_id))
          device_keys));
  let keys = Option.get (json_member "keys" device_keys) in
  let wrong_curve =
    replace_json_member "keys"
      (replace_json_member "curve25519:ALICEDEV"
         (jstr
            (Ck.Curve25519.Public.to_base64
               (snd (Encryption.identity_keys receiver))))
         keys)
      device_keys
  in
  reject "wrong sender curve"
    (Some (resign_sender_device_keys sender wrong_curve));
  reject "wrong sender signature"
    (Some (replace_json_member "signatures" (jobj []) device_keys))

let encrypted_message_for requests ~device =
  match
    List.find_map
      (function
        | Encryption.To_device
            {
              event_type = "m.room.encrypted";
              messages = [ (_, [ (Matrix_client.To_device.Device d, c) ]) ];
              _;
            }
          when Did.equal d device ->
            Some c
        | _ -> None)
      requests
  with
  | Some content -> content
  | None -> Alcotest.fail "no encrypted secret message for device"

let plain_message_for requests ~device =
  let _ = device in
  match
    List.find_map
      (function
        | Encryption.To_device
            {
              event_type = "m.secret.request";
              messages = [ (_, [ (Matrix_client.To_device.All, c) ]) ];
              _;
            } ->
            Some c
        | _ -> None)
      requests
  with
  | Some content -> content
  | None -> Alcotest.fail "no plaintext secret request for device"

let test_secret_request_round_trip_and_policy () =
  let alice, alice2 = own_olm_pair () in
  Encryption.store_secret alice2 ~name:"m.cross_signing.master"
    ~value:"master-key";
  let request_id =
    Result.get_ok
      (Encryption.request_secret alice ~name:"m.cross_signing.master")
  in
  let request_content =
    plain_message_for
      (Encryption.outgoing_requests alice)
      ~device:(did "ALICE2")
  in
  let received =
    Encryption.process_sync alice2
      (sync_of
         ~to_device:
           [
             to_device_event
               ~sender:(Id.User_id.to_string alice_id)
               ~event_type:"m.secret.request" ~content:request_content;
           ]
         ())
  in
  (match received.events with
  | [ Encryption.Secret_request _ ] -> ()
  | _ -> Alcotest.fail "secret request was not surfaced");
  let send_content =
    encrypted_message_for received.requests ~device:(did "ALICEDEV")
  in
  let accepted =
    Encryption.process_sync alice
      (sync_of
         ~to_device:
           [
             to_device_event
               ~sender:(Id.User_id.to_string alice_id)
               ~event_type:"m.room.encrypted" ~content:send_content;
           ]
         ())
  in
  (match accepted.events with
  | [ Encryption.Secret_send _ ] -> ()
  | _ -> Alcotest.fail "secret send was not surfaced");
  check_string "matching request id is accepted" "master-key"
    (Option.get (Encryption.secret alice ~name:"m.cross_signing.master"));
  check_bool "request is no longer retried" true
    (not
       (List.exists
          (function
            | Encryption.To_device { event_type = "m.room.encrypted"; _ } ->
                true
            | _ -> false)
          (Encryption.outgoing_requests alice)));
  let automatic_cancellation =
    plain_message_for accepted.requests ~device:(did "ALICE2")
  in
  check_bool "accepted request queues a plaintext cancellation" true
    (contains "request_cancellation" (json_to_string automatic_cancellation));
  check_bool "cancellation names the accepted request" true
    (contains
       (Printf.sprintf {|"request_id":"%s"|} request_id)
       (json_to_string automatic_cancellation));
  check_bool "request id was nonempty" true (request_id <> "")

let test_secret_request_replacement_and_retry () =
  let alice, _alice2 = own_olm_pair () in
  let first =
    Result.get_ok (Encryption.request_secret alice ~name:"m.recovery.key")
  in
  let first_request =
    plain_message_for
      (Encryption.outgoing_requests alice)
      ~device:(did "ALICE2")
  in
  let second =
    Result.get_ok (Encryption.request_secret alice ~name:"m.recovery.key")
  in
  check_bool "replacement gets a new request id" true (first <> second);
  let requests = Encryption.outgoing_requests alice in
  check_bool "replacement queues cancellation and new request" true
    (List.length
       (List.filter
          (function
            | Encryption.To_device { event_type = "m.secret.request"; _ } ->
                true
            | _ -> false)
          requests)
    = 2);
  (* Before the send is acknowledged, repeating the poll retains the exact
     request content and transaction id. *)
  let second_request = plain_message_for requests ~device:(did "ALICE2") in
  check_bool "the current request is not the replaced ciphertext" true
    (json_to_string first_request <> json_to_string second_request);
  let retry = Encryption.outgoing_requests alice in
  check_string "retry content is stable"
    (json_to_string second_request)
    (json_to_string (plain_message_for retry ~device:(did "ALICE2")))

let test_secret_request_unverified_and_cancellation () =
  let alice, alice2 = own_olm_pair () in
  Encryption.set_device_trust alice2 alice_id ~device_id:(did "ALICEDEV")
    Encryption.Unverified;
  Encryption.store_secret alice2 ~name:"m.cross_signing.self_signing"
    ~value:"self-signing";
  ignore (Encryption.request_secret alice ~name:"m.cross_signing.self_signing");
  let request_content =
    plain_message_for
      (Encryption.outgoing_requests alice)
      ~device:(did "ALICE2")
  in
  let refused =
    Encryption.process_sync alice2
      (sync_of
         ~to_device:
           [
             to_device_event
               ~sender:(Id.User_id.to_string alice_id)
               ~event_type:"m.secret.request" ~content:request_content;
           ]
         ())
  in
  check_bool "unverified requester is still observable" true
    (match refused.events with
    | [ Encryption.Secret_request _ ] -> true
    | _ -> false);
  check_bool "unverified requester receives no secret" true
    (not
       (List.exists
          (function
            | Encryption.To_device { event_type = "m.room.encrypted"; _ } ->
                true
            | _ -> false)
          refused.requests));
  let forged =
    Encryption.process_sync alice2
      (sync_of
         ~to_device:
           [
             to_device_event
               ~sender:(Id.User_id.to_string alice_id)
               ~event_type:"m.secret.request"
               ~content:
                 (jobj
                    [
                      ("action", jstr "request");
                      ("requesting_device_id", jstr "ALICE2");
                      ("request_id", jstr "forged-request");
                      ("name", jstr "m.cross_signing.self_signing");
                    ]);
           ]
         ())
  in
  check_bool "unknown requesting device gets no response" true
    (not
       (List.exists
          (function
            | Encryption.To_device { event_type = "m.room.encrypted"; _ } ->
                true
            | _ -> false)
          forged.requests));
  (* A verified request may be answered, but its cancellation removes the
     unsent response before it can leave the machine. *)
  let alice, alice2 = own_olm_pair () in
  Encryption.store_secret alice2 ~name:"m.cross_signing.self_signing"
    ~value:"self-signing";
  ignore (Encryption.request_secret alice ~name:"m.cross_signing.self_signing");
  let request_content =
    plain_message_for
      (Encryption.outgoing_requests alice)
      ~device:(did "ALICE2")
  in
  let answered =
    Encryption.process_sync alice2
      (sync_of
         ~to_device:
           [
             to_device_event
               ~sender:(Id.User_id.to_string alice_id)
               ~event_type:"m.secret.request" ~content:request_content;
           ]
         ())
  in
  check_bool "verified requester gets a queued response" true
    (List.exists
       (function
         | Encryption.To_device { event_type = "m.room.encrypted"; _ } -> true
         | _ -> false)
       answered.requests);
  check_bool "cancellation is accepted" true
    (Encryption.cancel_secret_request alice ~name:"m.cross_signing.self_signing");
  let cancellation =
    plain_message_for
      (Encryption.outgoing_requests alice)
      ~device:(did "ALICE2")
  in
  let canceled =
    Encryption.process_sync alice2
      (sync_of
         ~to_device:
           [
             to_device_event
               ~sender:(Id.User_id.to_string alice_id)
               ~event_type:"m.secret.request" ~content:cancellation;
           ]
         ())
  in
  check_bool "cancellation suppresses the pending response" true
    (not
       (List.exists
          (function
            | Encryption.To_device { event_type = "m.room.encrypted"; _ } ->
                true
            | _ -> false)
          canceled.requests))

let test_secret_request_cancel_after_send () =
  let alice, _alice2 = own_olm_pair () in
  ignore (Encryption.request_secret alice ~name:"m.recovery.key");
  let sent_request =
    List.find
      (function
        | Encryption.To_device { event_type = "m.secret.request"; _ } -> true
        | _ -> false)
      (Encryption.outgoing_requests alice)
  in
  Encryption.mark_sent alice sent_request;
  check_bool "a sent request can still be cancelled" true
    (Encryption.cancel_secret_request alice ~name:"m.recovery.key");
  check_bool "the cancellation is plaintext" true
    (List.exists
       (function
         | Encryption.To_device { event_type = "m.secret.request"; _ } -> true
         | _ -> false)
       (Encryption.outgoing_requests alice))

(* The store tests need a real filesystem, so they run their own Eio_main. *)
let with_store_xdg f =
  Eio_main.run @@ fun env ->
  let base = Filename.temp_file "matrix-crypto-" "" in
  Sys.remove base;
  Unix.mkdir base 0o700;
  Unix.putenv "XDG_DATA_HOME" (Filename.concat base "data");
  Unix.putenv "XDG_CONFIG_HOME" (Filename.concat base "config");
  Unix.putenv "XDG_CACHE_HOME" (Filename.concat base "cache");
  Unix.putenv "XDG_STATE_HOME" (Filename.concat base "state");
  let runtime = Filename.concat base "runtime" in
  Unix.mkdir runtime 0o700;
  Unix.putenv "XDG_RUNTIME_DIR" runtime;
  let xdg = Xdge.create (Eio.Stdenv.fs env) "matrix-test" in
  f xdg (Crypto_store.create ~xdg ~profile:"default")

let with_store f = with_store_xdg (fun _xdg store -> f store)

let test_room_key_bundle_persistence () =
  let sender, receiver = own_olm_pair () in
  let content = bundle_content (bundle_file "bundle-persisted") in
  ignore
    (Encryption.process_sync receiver
       (sync_of
          ~to_device:
            [
              encrypted_bundle_event ~sender ~receiver
                ~event_type:"m.room_key_bundle" content;
            ]
          ()));
  with_store (fun store ->
      let snapshot = Encryption.snapshot receiver in
      let sender_ed, sender_curve = Encryption.identity_keys sender in
      let invalid : Encryption.received_key_bundle =
        {
          room_id = rid "!invalid-bundle:example.org";
          sender = alice_id;
          sender_key = sender_curve;
          sender_ed25519 = "not-base64";
          file = bundle_file "invalid-persisted-bundle";
        }
      in
      let snapshot =
        {
          snapshot with
          state =
            {
              snapshot.state with
              received_key_bundles =
                invalid :: snapshot.state.received_key_bundles;
            };
        }
      in
      ignore sender_ed;
      ok_value (Crypto_store.save store snapshot);
      let restored =
        match Crypto_store.load store with
        | Ok (Some snapshot) ->
            Encryption.of_snapshot
              ~random:(random_of "bundle-restored")
              ~user_id:alice_id ~device_id:(did "ALICE2") snapshot
        | _ -> Alcotest.fail "bundle snapshot was not restored"
      in
      check_int "bundle survives persistence" 1
        (List.length (Encryption.received_key_bundles restored));
      let restored_bundle =
        List.hd (Encryption.received_key_bundles restored)
      in
      check_string "persisted bundle URL" "mxc://hs.example/bundle-persisted"
        restored_bundle.file.url)

let test_secret_gossip_snapshot_and_store () =
  let alice, alice2 = own_olm_pair () in
  let request_id =
    Result.get_ok (Encryption.request_secret alice ~name:"m.recovery.key")
  in
  let request =
    List.find
      (function
        | Encryption.To_device { event_type = "m.secret.request"; _ } -> true
        | _ -> false)
      (Encryption.outgoing_requests alice)
  in
  let request_content = plain_message_for [ request ] ~device:(did "ALICE2") in
  let request_txn =
    match request with
    | Encryption.To_device { txn_id; _ } -> txn_id
    | _ -> Alcotest.fail "expected a secret request"
  in
  let alice_pending =
    Encryption.of_snapshot
      ~random:(random_of "secret-request-pending-restart")
      ~user_id:alice_id ~device_id:(did "ALICEDEV")
      (Encryption.snapshot alice)
  in
  let pending_retry =
    List.find
      (function
        | Encryption.To_device { event_type = "m.secret.request"; _ } -> true
        | _ -> false)
      (Encryption.outgoing_requests alice_pending)
  in
  (match pending_retry with
  | Encryption.To_device { txn_id; _ } ->
      check_string "unsent request keeps its transaction after restart"
        request_txn txn_id
  | _ -> Alcotest.fail "expected a retried secret request");
  check_string "unsent request keeps its body after restart"
    (json_to_string request_content)
    (json_to_string
       (plain_message_for [ pending_retry ] ~device:(did "ALICE2")));
  Encryption.mark_sent alice request;
  let alice_restarted =
    Encryption.of_snapshot
      ~random:(random_of "secret-request-restart")
      ~user_id:alice_id ~device_id:(did "ALICEDEV")
      (Encryption.snapshot alice)
  in
  (* The response is delivered below, after the provider has generated it;
     the request itself was already acknowledged before the requester restart. *)
  (* Registering a secret and queuing an encrypted response is persisted with
     the ciphertext cache, so a retry does not advance the Olm ratchet. *)
  Encryption.store_secret alice2 ~name:"m.recovery.key" ~value:"recovery-key";
  let received =
    Encryption.process_sync alice2
      (sync_of
         ~to_device:
           [
             to_device_event
               ~sender:(Id.User_id.to_string alice_id)
               ~event_type:"m.secret.request" ~content:request_content;
           ]
         ())
  in
  let send_request =
    List.find
      (function
        | Encryption.To_device { event_type = "m.room.encrypted"; _ } -> true
        | _ -> false)
      received.requests
  in
  let send_content =
    encrypted_message_for [ send_request ] ~device:(did "ALICEDEV")
  in
  let accepted =
    Encryption.process_sync alice_restarted
      (sync_of
         ~to_device:
           [
             to_device_event
               ~sender:(Id.User_id.to_string alice_id)
               ~event_type:"m.room.encrypted" ~content:send_content;
           ]
         ())
  in
  check_string "a sent request accepts its response after restart"
    "recovery-key"
    (Option.get (Encryption.secret alice_restarted ~name:"m.recovery.key"));
  let alice_accepted_restart =
    Encryption.of_snapshot
      ~random:(random_of "accepted-secret-restart")
      ~user_id:alice_id ~device_id:(did "ALICEDEV")
      (Encryption.snapshot alice_restarted)
  in
  check_string "an accepted secret survives another restart" "recovery-key"
    (Option.get
       (Encryption.secret alice_accepted_restart ~name:"m.recovery.key"));
  ignore accepted;
  with_store @@ fun store ->
  ok_value (Driver.save (drive ~store alice2));
  let alice2_restarted =
    Driver.machine
      (ok_value
         (Driver.create
            ~random:(random_of "secret-send-restart")
            ~user_id:alice_id ~device_id:(did "ALICE2") ~store ()))
  in
  check_string "registered secret survives store reload" "recovery-key"
    (Option.get (Encryption.secret alice2_restarted ~name:"m.recovery.key"));
  let retry_request =
    List.find
      (function
        | Encryption.To_device { event_type = "m.room.encrypted"; _ } -> true
        | _ -> false)
      (Encryption.outgoing_requests alice2_restarted)
  in
  check_string "encrypted secret retry keeps its exact body"
    (json_to_string send_content)
    (json_to_string
       (encrypted_message_for [ retry_request ] ~device:(did "ALICEDEV")));
  Encryption.mark_sent alice2_restarted retry_request;
  ok_value (Driver.save (drive ~store alice2_restarted));
  let alice2_sent_restart =
    Driver.machine
      (ok_value
         (Driver.create
            ~random:(random_of "secret-send-sent-restart")
            ~user_id:alice_id ~device_id:(did "ALICE2") ~store ()))
  in
  check_bool "sent encrypted secret is removed after reload" true
    (not
       (List.exists
          (function
            | Encryption.To_device { event_type = "m.room.encrypted"; _ } ->
                true
            | _ -> false)
          (Encryption.outgoing_requests alice2_sent_restart)));
  (* Cancellation is itself durable and is removed only after its transaction
     is acknowledged. *)
  let cancel_request =
    List.find
      (function
        | Encryption.To_device { event_type = "m.secret.request"; _ } -> true
        | _ -> false)
      (Encryption.outgoing_requests alice_restarted)
  in
  let cancel_content =
    plain_message_for [ cancel_request ] ~device:(did "ALICE2")
  in
  let alice_cancel_restart =
    Encryption.of_snapshot
      ~random:(random_of "secret-cancel-restart")
      ~user_id:alice_id ~device_id:(did "ALICEDEV")
      (Encryption.snapshot alice_restarted)
  in
  check_string "accepted secret survives pure restart" "recovery-key"
    (Option.get (Encryption.secret alice_cancel_restart ~name:"m.recovery.key"));
  check_string "cancellation survives pure restart"
    (json_to_string cancel_content)
    (json_to_string
       (plain_message_for
          (Encryption.outgoing_requests alice_cancel_restart)
          ~device:(did "ALICE2")));
  Encryption.mark_sent alice_cancel_restart cancel_request;
  check_bool "sent cancellation is removed" true
    (not
       (List.exists
          (function
            | Encryption.To_device { event_type = "m.secret.request"; _ } ->
                true
            | _ -> false)
          (Encryption.outgoing_requests alice_cancel_restart)));
  check_bool "request id remains stable across the workflow" true
    (request_id <> "")

(* A device that a re-query fails to validate keeps whatever this client
   already recorded for it, rather than being deleted along with its trust
   decision. *)
let test_failed_requery_keeps_existing_device () =
  let alice =
    make ~seed:"alice-requery" ~user:alice_id ~device:(did "ALICEDEV") ()
  in
  let bob = make ~seed:"bob-requery" ~user:bob_id ~device:(did "BOBDEV") () in
  introduce ~from:bob ~into:alice;
  Encryption.set_device_trust alice bob_id ~device_id:(did "BOBDEV")
    Encryption.Verified;
  (match Encryption.find_device alice bob_id ~device_id:(did "BOBDEV") with
  | Some d -> check_bool "bob starts verified" true (d.trust = Verified)
  | None -> Alcotest.fail "bob's device is not known yet");
  (* A re-query response for bob with no self-signature at all: it fails
     validation, and must not erase the device or its trust. *)
  let good = query_response_for bob in
  let corrupted =
    {
      good with
      Keys.device_keys =
        List.map
          (fun (u, ds) ->
            ( u,
              List.map
                (fun (d, (dk : Keys.device_keys)) ->
                  (d, { dk with Keys.signatures = [] }))
                ds ))
          good.Keys.device_keys;
    }
  in
  Encryption.receive_keys_query alice corrupted;
  match Encryption.find_device alice bob_id ~device_id:(did "BOBDEV") with
  | Some d -> check_bool "bob is still verified" true (d.trust = Verified)
  | None -> Alcotest.fail "a failed requery deleted bob's device"

let test_cross_signed_identity_chain_and_rotation () =
  let alice =
    make ~seed:"identity-alice" ~user:alice_id ~device:(did "ALICEDEV") ()
  in
  let bob = make ~seed:"identity-bob" ~user:bob_id ~device:(did "BOBDEV") () in
  Encryption.receive_keys_query alice
    (cross_signed_query_for ~seed:"identity-one" bob);
  check_bool "valid chain starts unverified" true
    (Encryption.identity_status alice bob_id
    = Some Encryption.Identity_unverified);
  check_bool "first identity is pinned without violation" false
    (Encryption.identity_has_pin_violation alice bob_id);
  Encryption.receive_keys_query alice
    (cross_signed_query_for ~seed:"identity-unverified-rotation" bob);
  check_bool "unverified rotation is a pin violation" true
    (Encryption.identity_has_pin_violation alice bob_id);
  check_bool "unverified rotation stays unverified" true
    (Encryption.identity_status alice bob_id
    = Some Encryption.Identity_unverified);
  Encryption.acknowledge_user_identity alice bob_id;
  check_bool "pin acknowledgement clears only pin violation" false
    (Encryption.identity_has_pin_violation alice bob_id);
  check_bool "pin acknowledgement does not verify" true
    (Encryption.identity_status alice bob_id
    = Some Encryption.Identity_unverified);
  Encryption.trust_user_identity alice bob_id;
  check_bool "identity can be explicitly acknowledged" true
    (Encryption.identity_status alice bob_id = Some Encryption.Identity_verified);
  let changed = cross_signed_query_for ~seed:"identity-two" bob in
  Encryption.receive_keys_query alice changed;
  check_bool "master rotation is a verification violation" true
    (Encryption.identity_status alice bob_id
    = Some Encryption.Verification_violation);
  check_bool "identity change is deterministic" true
    (List.exists
       (fun (c : Encryption.identity_change) ->
         c.user_id = bob_id && c.status = Encryption.Verification_violation)
       (Encryption.identity_changes alice));
  let restarted =
    Encryption.of_snapshot
      ~random:(random_of "identity-restart")
      ~user_id:alice_id ~device_id:(did "ALICEDEV")
      (Encryption.snapshot alice)
  in
  check_bool "verification violation survives restart" true
    (Encryption.identity_status restarted bob_id
    = Some Encryption.Verification_violation);
  check_bool "verified rotation also violates retained pin" true
    (Encryption.identity_has_pin_violation restarted bob_id);
  Encryption.pin_user_identity restarted bob_id;
  check_bool "pin acknowledgement leaves verification violation" true
    (Encryption.identity_status restarted bob_id
    = Some Encryption.Verification_violation);
  Encryption.trust_user_identity restarted bob_id;
  check_bool "re-verifying the replacement clears the violation" true
    (Encryption.identity_status restarted bob_id
    = Some Encryption.Identity_verified);
  Encryption.receive_keys_query restarted
    (cross_signed_query_for ~seed:"identity-three" bob);
  check_bool "another rotation becomes a fresh violation" true
    (Encryption.identity_status restarted bob_id
    = Some Encryption.Verification_violation)

let test_gossip_allowed_for_own_verified_device () =
  let log, client = permissive_client () in
  let alice =
    make ~seed:"alice-own" ~user:alice_id ~device:(did "ALICEDEV") ()
  in
  let alice2 =
    make ~seed:"alice2-own" ~user:alice_id ~device:(did "ALICE2") ()
  in
  let bob = make ~seed:"bob-own" ~user:bob_id ~device:(did "BOBDEV") () in
  introduce ~from:bob ~into:alice;
  introduce ~from:alice ~into:alice;
  Encryption.receive_keys_query alice
    (let r = query_response_for alice2 in
     (* Merge, so that adopting Alice's second device does not drop the
        first: the real server returns both in one answer. *)
     let mine = query_response_for alice in
     {
       r with
       device_keys =
         [
           ( alice_id,
             List.concat_map snd mine.device_keys
             @ List.concat_map snd r.device_keys );
         ];
     });
  Encryption.set_device_trust alice alice_id ~device_id:(did "ALICE2")
    Encryption.Verified;
  ignore (Encryption.receive_keys_claim alice (claim_response_for bob));
  ignore (Encryption.receive_keys_claim alice (claim_response_for alice2));
  let _, _ =
    share_and_encrypt alice bob client log ~body:"for my other device"
  in
  let session_id = Option.get (Encryption.outbound_session_id alice room) in
  let event =
    to_device_event
      ~sender:(Id.User_id.to_string alice_id)
      ~event_type:"m.room_key_request"
      ~content:(key_request_content ~device:(did "ALICE2") ~session_id)
  in
  let outcome =
    Encryption.process_sync alice (sync_of ~to_device:[ event ] ())
  in
  (match outcome.events with
  | [ Encryption.Room_key_request { answered; _ } ] ->
      check_bool "our own verified device is answered" true answered
  | _ -> Alcotest.fail "expected one m.room_key_request");
  check_bool "a forwarded key is queued" true
    (List.exists
       (function
         | Encryption.To_device { event_type = "m.room.encrypted"; _ } -> true
         | _ -> false)
       outcome.requests)

let test_one_time_key_upload_queued () =
  let _, client = permissive_client () in
  let alice =
    make ~seed:"alice-otk" ~user:alice_id ~device:(did "ALICEDEV") ()
  in
  (match Encryption.outgoing_requests alice with
  | Encryption.Keys_upload { device_keys; one_time_keys; _ } :: _ ->
      check_bool "a new device publishes its identity keys" true
        (device_keys <> None);
      check_int "and the complete pool of one-time keys" 50
        (List.length one_time_keys)
  | _ -> Alcotest.fail "a fresh machine must want to upload its keys");
  let d = drive alice in
  Driver.execute_requests d client (Encryption.outgoing_requests alice);
  check_int "nothing more is owed once the server has them" 0
    (List.length (Encryption.outgoing_requests alice));
  (* The server now reports the pool exhausted. *)
  let outcome =
    Encryption.process_sync alice
      (sync_of ~otk_counts:[ ("signed_curve25519", 0) ] ())
  in
  match
    List.filter
      (function Encryption.Keys_upload _ -> true | _ -> false)
      outcome.requests
  with
  | [ Encryption.Keys_upload { device_keys; one_time_keys; _ } ] ->
      check_bool "the identity keys are already published" true
        (device_keys = None);
      check_int "but the pool is topped back up to capacity" 50
        (List.length one_time_keys)
  | _ -> Alcotest.fail "an exhausted pool must queue an upload"

let one_time_key_ids = function
  | Encryption.Keys_upload { one_time_keys; _ } ->
      List.map (fun (key_id, _) -> Key_id.id key_id) one_time_keys
  | _ -> Alcotest.fail "expected a one-time-key upload"

let test_one_time_key_replenishment_counts () =
  let machine () =
    make ~seed:"alice-otk-counts" ~user:alice_id ~device:(did "ALICEDEV") ()
  in
  let first =
    match Encryption.outgoing_requests (machine ()) with
    | request :: _ -> request
    | [] -> Alcotest.fail "a fresh machine must queue an upload"
  in
  check_int "fresh upload fills the account capacity" 50
    (List.length (one_time_key_ids first));
  let half_full = machine () in
  let outcome =
    Encryption.process_sync half_full
      (sync_of ~otk_counts:[ ("signed_curve25519", 24) ] ())
  in
  let request =
    match
      List.find_opt
        (function Encryption.Keys_upload _ -> true | _ -> false)
        outcome.requests
    with
    | Some request -> request
    | None -> Alcotest.fail "a low aggregate count must queue an upload"
  in
  check_int "aggregate count 24 replenishes to 50" 26
    (List.length (one_time_key_ids request));
  let exhausted = machine () in
  let initial =
    match Encryption.outgoing_requests exhausted with
    | request :: _ -> request
    | [] -> Alcotest.fail "a fresh machine must queue an upload"
  in
  let initial_ids = one_time_key_ids initial in
  Encryption.mark_sent exhausted initial;
  let outcome =
    Encryption.process_sync exhausted
      (sync_of ~otk_counts:[ ("signed_curve25519", 0) ] ())
  in
  let replacement =
    match
      List.find_opt
        (function Encryption.Keys_upload _ -> true | _ -> false)
        outcome.requests
    with
    | Some request -> request
    | None -> Alcotest.fail "an exhausted published pool must queue an upload"
  in
  let replacement_ids = one_time_key_ids replacement in
  check_int "zero aggregate count replenishes the full pool" 50
    (List.length replacement_ids);
  check_bool "replacement keys have fresh identifiers" true
    (List.for_all (fun id -> not (List.mem id initial_ids)) replacement_ids);
  let pending = machine () in
  Olm.Account.generate_one_time_keys ~random:(random_of "pending-otks")
    (Encryption.snapshot pending).account 10;
  let outcome =
    Encryption.process_sync pending
      (sync_of ~otk_counts:[ ("signed_curve25519", 0) ] ())
  in
  let request =
    match
      List.find_opt
        (function Encryption.Keys_upload _ -> true | _ -> false)
        outcome.requests
    with
    | Some request -> request
    | None -> Alcotest.fail "an unpublished batch must remain retryable"
  in
  check_int "an existing unpublished batch is not extended" 10
    (List.length (one_time_key_ids request));
  Encryption.mark_sent pending request;
  Encryption.mark_sent pending request;
  check_int "acknowledging a batch twice is idempotent" 10
    (List.length (Encryption.snapshot pending).state.published_one_time_keys)

let test_one_time_key_count_validation () =
  let rejects codec body =
    match Jsont_bytesrw.decode_string codec body with
    | Error _ -> true
    | Ok _ -> false
  in
  check_bool "classic sync rejects a negative OTK count" true
    (rejects Sync.Response.jsont
       {|{"next_batch":"n","device_one_time_keys_count":{"signed_curve25519":-1}}|});
  check_bool "classic sync rejects an unsafe OTK count" true
    (rejects Sync.Response.jsont
       {|{"next_batch":"n","device_one_time_keys_count":{"signed_curve25519":9007199254740992}}|});
  check_bool "sliding sync rejects a negative OTK count" true
    (rejects Matrix_proto.Sliding_sync.Response.jsont
       {|{"pos":"p","extensions":{"e2ee":{"device_one_time_keys_count":{"signed_curve25519":-1}}}}|});
  let _, fetch =
    mock (fun request ->
        Fetch_mock.respond {|{"one_time_key_counts":{"signed_curve25519":-1}}|}
          request)
  in
  check_bool "/keys/upload rejects a negative OTK count" true
    (Result.is_error (Keys.upload_keys (client_of fetch) ()))

let fallback_upload m =
  match
    List.find_map
      (function
        | Encryption.Keys_upload { fallback_keys = _ :: _; _ } as request ->
            Some request
        | _ -> None)
      (Encryption.outgoing_requests m)
  with
  | Some (Encryption.Keys_upload { fallback_keys = key :: _; _ }) -> key
  | _ -> Alcotest.fail "expected a fallback-key upload"

let time_after t seconds =
  Option.get (Ptime.add_span t (Ptime.Span.of_int_s seconds))

let test_fallback_key_age_rotation () =
  let t0 = Option.get (Ptime.of_float_s 1_700_000_000.) in
  let machine () =
    make ~seed:"fallback-age" ~user:alice_id ~device:(did "ALICEDEV") ()
  in
  let m = machine () in
  ignore (Encryption.process_sync ~now:t0 m (sync_of ~fallback_types:[] ()));
  let first = fallback_upload m in
  let first_id = Key_id.id (fst first) in
  let first_key = snd first in
  check_bool "fallback upload carries its signed marker" true
    (first_key.Keys.fallback = Some true);
  let signing_key, _ = Encryption.identity_keys m in
  let signature_id =
    Key_id.of_device ~algorithm:"ed25519" (Encryption.device_id m)
  in
  let signature =
    List.assoc alice_id (Option.get first_key.signatures)
    |> List.assoc signature_id
  in
  check_bool "fallback upload signs the marker" true
    (Ck.Ed25519.Public.verify signing_key ~signature
       ~data:(Keys.one_time_key_signing_json ~fallback:true first_key.Keys.key));
  let first_request =
    List.find
      (function
        | Encryption.Keys_upload { fallback_keys = _ :: _; _ } -> true
        | _ -> false)
      (Encryption.outgoing_requests m)
  in
  Encryption.mark_sent m first_request;
  let before_expiry =
    Encryption.process_sync
      ~now:(time_after t0 (7 * 24 * 3600))
      m
      (sync_of ~fallback_types:[] ())
  in
  check_bool "a fallback key is not rotated at exactly one week" true
    (not
       (List.exists
          (function
            | Encryption.Keys_upload { fallback_keys = _ :: _; _ } -> true
            | _ -> false)
          before_expiry.requests));
  let after_expiry =
    Encryption.process_sync
      ~now:(time_after t0 ((7 * 24 * 3600) + 1))
      m
      (sync_of ~fallback_types:[] ())
  in
  let second =
    match
      List.find_map
        (function
          | Encryption.Keys_upload { fallback_keys = key :: _; _ } -> Some key
          | _ -> None)
        after_expiry.requests
    with
    | Some key -> key
    | None -> Alcotest.fail "expired fallback key was not rotated"
  in
  check_bool "expired fallback key gets a new key id" true
    (not (String.equal first_id (Key_id.id (fst second))));
  let rollback = machine () in
  ignore
    (Encryption.process_sync ~now:t0 rollback (sync_of ~fallback_types:[] ()));
  let rollback_request =
    List.find
      (function
        | Encryption.Keys_upload { fallback_keys = _ :: _; _ } -> true
        | _ -> false)
      (Encryption.outgoing_requests rollback)
  in
  Encryption.mark_sent rollback rollback_request;
  let rolled_back =
    Encryption.process_sync ~now:(time_after t0 (-1)) rollback (sync_of ())
  in
  check_bool "clock rollback rotates the fallback key" true
    (List.exists
       (function
         | Encryption.Keys_upload { fallback_keys = _ :: _; _ } -> true
         | _ -> false)
       rolled_back.requests)

let test_fallback_key_pending_restart () =
  let t0 = Option.get (Ptime.of_float_s 1_700_100_000.) in
  let machine =
    make ~seed:"fallback-persist" ~user:alice_id ~device:(did "ALICEDEV") ()
  in
  ignore
    (Encryption.process_sync ~now:t0 machine (sync_of ~fallback_types:[] ()));
  let pending = fallback_upload machine in
  let restarted =
    Encryption.of_snapshot
      ~random:(random_of "fallback-persist-restart")
      ~user_id:alice_id ~device_id:(did "ALICEDEV")
      (Encryption.snapshot machine)
  in
  let pending_after = fallback_upload restarted in
  check_string "pending fallback key survives snapshot"
    (Key_id.id (fst pending))
    (Key_id.id (fst pending_after));
  check_bool "support history survives omitted reports" true
    (List.exists
       (function
         | Encryption.Keys_upload { fallback_keys = _ :: _; _ } -> true
         | _ -> false)
       (Encryption.process_sync
          ~now:(time_after t0 ((7 * 24 * 3600) + 1))
          restarted (sync_of ()))
         .requests);
  with_store @@ fun store ->
  ok_value (Driver.save (drive ~store machine));
  let disk_restart =
    Driver.machine
      (ok_value
         (Driver.create
            ~random:(random_of "fallback-disk-restart")
            ~user_id:alice_id ~device_id:(did "ALICEDEV") ~store ()))
  in
  let disk_pending = fallback_upload disk_restart in
  check_string "pending fallback key survives disk restart"
    (Key_id.id (fst pending))
    (Key_id.id (fst disk_pending));
  Encryption.mark_sent disk_restart
    (List.find
       (function
         | Encryption.Keys_upload { fallback_keys = _ :: _; _ } -> true
         | _ -> false)
       (Encryption.outgoing_requests disk_restart));
  ok_value (Driver.save (drive ~store disk_restart));
  let disk_sent_restart =
    Driver.machine
      (ok_value
         (Driver.create
            ~random:(random_of "fallback-disk-sent")
            ~user_id:alice_id ~device_id:(did "ALICEDEV") ~store ()))
  in
  check_bool "published fallback key is not reoffered before expiry" true
    (not
       (List.exists
          (function
            | Encryption.Keys_upload { fallback_keys = _ :: _; _ } -> true
            | _ -> false)
          (Encryption.outgoing_requests disk_sent_restart)))

let test_one_time_key_count_absence_source_semantics () =
  let fresh seed =
    let m = make ~seed ~user:alice_id ~device:(did "ALICEDEV") () in
    (* Establish the same state as a device whose initial key upload was
       accepted, so the only request below can be caused by the count. *)
    List.iter (Encryption.mark_sent m) (Encryption.outgoing_requests m);
    Encryption.receive_keys_upload m
      { Keys.one_time_key_counts = [ ("signed_curve25519", 50) ] };
    m
  in
  let has_upload requests =
    List.exists
      (function Encryption.Keys_upload _ -> true | _ -> false)
      requests
  in
  let classic = fresh "otk-classic-absent" in
  let _ = Encryption.process_sync classic (sync_of ()) in
  check_bool "classic absent count means zero" true
    (has_upload (Encryption.outgoing_requests classic));
  let sliding = fresh "otk-sliding-absent" in
  let _ = Encryption.process_sliding_sync sliding (sliding_sync_of ()) in
  check_bool "sliding absent count leaves it unchanged" false
    (has_upload (Encryption.outgoing_requests sliding))

let test_device_lists_changed_queues_query () =
  let alice =
    make ~seed:"alice-dl" ~user:alice_id ~device:(did "ALICEDEV") ()
  in
  let bob = make ~seed:"bob-dl" ~user:bob_id ~device:(did "BOBDEV") () in
  introduce ~from:bob ~into:alice;
  check_int "Bob is tracked and up to date" 0
    (List.length (Encryption.outdated_users alice));
  let outcome =
    Encryption.process_sync alice
      (sync_of ~device_lists:{ changed = [ bob_id ]; left = [] } ())
  in
  check_int "Bob is now outdated" 1
    (List.length (Encryption.outdated_users alice));
  check_bool "and a query is queued" true
    (List.exists
       (function
         | Encryption.Keys_query [ u ] ->
             Id.User_id.to_string u = Id.User_id.to_string bob_id
         | _ -> false)
       outcome.requests);
  (* [device_lists.left] is the other direction. *)
  let _ =
    Encryption.process_sync alice
      (sync_of ~device_lists:{ changed = []; left = [ bob_id ] } ())
  in
  check_int "a departed user is no longer tracked" 0
    (List.length (Encryption.devices_of alice bob_id))

let test_invite_and_knock_encryption_state () =
  let alice =
    make ~seed:"alice-stripped-encryption" ~user:alice_id
      ~device:(did "ALICEDEV") ()
  in
  let knock_room = rid "!knock:example.org" in
  let left_room = rid "!left:example.org" in
  let event : Matrix_proto.Event.Stripped_event.t =
    {
      sender = bob_id;
      type_ = Matrix_proto.Event.Event_type.Room_encryption;
      state_key = "";
      content =
        Encryption.room_encryption_content
          (Encryption.enable_room_encryption ());
    }
  in
  let rooms : Sync.Rooms.t =
    {
      join = [];
      invite =
        [
          ( room_str,
            { Sync.Invited_room.invite_state = Some { events = [ event ] } } );
        ];
      leave =
        [
          ( Id.Room_id.to_string left_room,
            {
              Sync.Left_room.state =
                Some
                  {
                    events =
                      [
                        {
                          Matrix_proto.Event.Raw_event.event_id =
                            Some
                              (Id.Event_id.of_string_exn
                                 "$left-encryption:example.org");
                          sender = bob_id;
                          origin_server_ts =
                            Matrix_proto.Event.Timestamp.of_ms 1L;
                          type_ = Matrix_proto.Event.Event_type.Room_encryption;
                          state_key = Some "";
                          content = event.content;
                          unsigned = None;
                          room_id = None;
                          redacts = None;
                        };
                      ];
                  };
              timeline = None;
              account_data = None;
            } );
        ];
      knock =
        [
          ( Id.Room_id.to_string knock_room,
            { Sync.Knocked_room.knock_state = Some { events = [ event ] } } );
        ];
    }
  in
  ignore (Encryption.process_sync alice (sync_of ~rooms ()));
  check_bool "invite state marks the room encrypted" true
    (Encryption.is_room_encrypted alice room);
  check_bool "knock state marks the room encrypted" true
    (Encryption.is_room_encrypted alice knock_room);
  check_bool "left state marks the room encrypted" true
    (Encryption.is_room_encrypted alice left_room)

let test_execute_requests_endpoints () =
  let log, client = permissive_client () in
  let alice =
    make ~seed:"alice-http" ~user:alice_id ~device:(did "ALICEDEV") ()
  in
  let bob = make ~seed:"bob-http" ~user:bob_id ~device:(did "BOBDEV") () in
  introduce ~from:bob ~into:alice;
  let content = jobj [ ("body", jstr "hi") ] in
  Driver.execute_requests (drive alice) client
    [
      Encryption.Keys_upload
        {
          device_keys = Some (Encryption.device_keys_for_upload alice);
          one_time_keys = [];
          fallback_keys = [];
        };
      Encryption.Keys_query [ bob_id ];
      Encryption.Keys_claim
        [ (bob_id, [ (did "BOBDEV", "signed_curve25519") ]) ];
      Encryption.To_device
        {
          event_type = "m.room.encrypted";
          txn_id = "txn1";
          messages =
            [
              ( bob_id,
                [ (Matrix_client.To_device.Device (did "BOBDEV"), content) ] );
            ];
        };
    ];
  let urls = List.map (fun r -> r.url) (requests log) in
  let hit needle = List.exists (contains needle) urls in
  check_bool "/keys/upload" true (hit "/_matrix/client/v3/keys/upload");
  check_bool "/keys/query" true (hit "/_matrix/client/v3/keys/query");
  check_bool "/keys/claim" true (hit "/_matrix/client/v3/keys/claim");
  check_bool "/sendToDevice" true
    (hit "/_matrix/client/v3/sendToDevice/m.room.encrypted/txn1");
  check_int "one request each" 4 (List.length urls);
  let methods = List.map (fun r -> r.meth) (requests log) in
  check_string "the to-device send is a PUT" "PUT" (List.nth methods 3)

let test_execute_requests_error_callback () =
  let client =
    client_of
      (Fetch_mock.client (fun req ->
           Fetch_mock.respond ~status:503
             {|{"errcode":"M_UNAVAILABLE","error":"offline"}|} req))
  in
  let alice =
    make ~seed:"alice-error-callback" ~user:alice_id ~device:(did "ALICEDEV") ()
  in
  let errors = ref [] in
  Driver.execute_requests
    ~on_error:(fun error -> errors := error :: !errors)
    (drive alice) client
    [
      Encryption.Keys_upload
        {
          device_keys = Some (Encryption.device_keys_for_upload alice);
          one_time_keys = [];
          fallback_keys = [];
        };
      Encryption.Keys_query [ bob_id ];
      Encryption.Keys_claim
        [ (bob_id, [ (did "BOBDEV", "signed_curve25519") ]) ];
    ];
  check_int "every failed request is reported" 3 (List.length !errors);
  (* A rejected upload remains retryable: transport failure must not mark the
     keys as published or cause a replacement set to be generated. *)
  let retry_machine =
    make ~seed:"alice-otk-upload-retry" ~user:alice_id ~device:(did "ALICEDEV")
      ()
  in
  let before =
    match Encryption.outgoing_requests retry_machine with
    | request :: _ -> one_time_key_ids request
    | [] -> Alcotest.fail "expected a one-time-key upload to retry"
  in
  Driver.execute_requests (drive retry_machine) client
    (Encryption.outgoing_requests retry_machine);
  let after =
    match Encryption.outgoing_requests retry_machine with
    | request :: _ -> one_time_key_ids request
    | [] -> Alcotest.fail "failed upload was incorrectly acknowledged"
  in
  check_bool "failed upload retries the exact same key IDs" true (before = after)

(* A homeserver that keeps whatever is PUT to /room_keys/keys and hands it
   back on GET, which is all a backup round trip needs. *)
let backup_client () =
  let stored = ref {|{"rooms":{}}|} in
  let handler (req : Fetch.Middleware.request) =
    if contains "/room_keys/keys/" (Fetch.Middleware.Url.to_string req.url) then
      Fetch_mock.respond {|{"sessions":{}}|} req
    else if contains "/room_keys/keys" (Fetch.Middleware.Url.to_string req.url)
    then
      match Http.Method.to_string req.meth with
      | "PUT" ->
          stored := Option.value (body_of_request req) ~default:{|{"rooms":{}}|};
          Fetch_mock.respond {|{"etag":"e1","count":1}|} req
      | _ -> Fetch_mock.respond !stored req
    else permissive_handler req
  in
  let log, fetch = mock handler in
  (log, client_of fetch)

(* Build one non-shared session and its MSC4268 withholding entry.  Keeping
   this fixture small makes the provenance tests exercise the real session
   and bundle paths without depending on the larger backup tests below. *)
let history_bundle_fixture seed =
  let log, client = backup_client () in
  let alice =
    make ~seed:(seed ^ "-alice") ~user:alice_id ~device:(did "ALICEDEV") ()
  in
  let bob = make ~seed:(seed ^ "-bob") ~user:bob_id ~device:(did "BOBDEV") () in
  introduce ~from:bob ~into:alice;
  introduce ~from:alice ~into:bob;
  ignore (Encryption.receive_keys_claim alice (claim_response_for bob));
  ignore (share_and_encrypt alice bob client log ~body:"history-provenance");
  let key = List.hd (Encryption.export_room_keys bob) in
  (Encryption.build_room_key_bundle bob ~room_id:room, key)

let test_withheld_provenance_direct () =
  let machine =
    make ~seed:"withheld-provenance-direct" ~user:alice_id
      ~device:(did "ALICEDEV") ()
  in
  let _, sender_key = Encryption.identity_keys machine in
  let session_id = Id.Session_id.of_string_exn "provenance-direct" in
  let event =
    to_device_event
      ~sender:(Id.User_id.to_string bob_id)
      ~event_type:"m.room_key.withheld"
      ~content:
        (jobj
           [
             ("algorithm", jstr "m.megolm.v1.aes-sha2");
             ("sender_key", jstr (Ck.Curve25519.Public.to_base64 sender_key));
             ("room_id", jstr room_str);
             ("session_id", jstr (Id.Session_id.to_string session_id));
             ("code", jstr "m.history_not_shared");
           ])
  in
  ignore (Encryption.process_sync machine (sync_of ~to_device:[ event ] ()));
  match Encryption.withheld_for machine ~room_id:room ~session_id with
  | None -> Alcotest.fail "withheld event was not retained"
  | Some withheld ->
      check_string "withheld sender provenance"
        (Id.User_id.to_string bob_id)
        (Option.map Id.User_id.to_string withheld.sender_user
        |> Option.value ~default:"<missing>")

let test_withheld_provenance_bundle () =
  let bundle, _key = history_bundle_fixture "withheld-provenance-bundle" in
  let target =
    make ~seed:"withheld-provenance-bundle-target" ~user:alice_id
      ~device:(did "TARGET") ()
  in
  let withheld_only = { bundle with room_keys = [] } in
  ignore
    (Encryption.import_room_key_bundle target ~room_id:room ~sender:bob_id
       withheld_only);
  match bundle.withheld with
  | [ entry ] -> (
      match
        Encryption.withheld_for target ~room_id:room
          ~session_id:entry.session_id
      with
      | Some stored ->
          check_string "bundle sender provenance"
            (Id.User_id.to_string bob_id)
            (Option.map Id.User_id.to_string stored.sender_user
            |> Option.value ~default:"<missing>")
      | None -> Alcotest.fail "bundle withholding was not retained")
  | _ -> Alcotest.fail "fixture did not contain one withholding entry"

let test_withheld_provenance_duplicate () =
  let bundle, _key = history_bundle_fixture "withheld-provenance-duplicate" in
  let target =
    make ~seed:"withheld-provenance-duplicate-target" ~user:alice_id
      ~device:(did "TARGET") ()
  in
  let entry = List.hd bundle.withheld in
  let _, sender_key = Encryption.identity_keys target in
  let direct =
    to_device_event
      ~sender:(Id.User_id.to_string alice_id)
      ~event_type:"m.room_key.withheld"
      ~content:
        (jobj
           [
             ("algorithm", jstr "m.megolm.v1.aes-sha2");
             ("sender_key", jstr (Ck.Curve25519.Public.to_base64 sender_key));
             ("room_id", jstr room_str);
             ("session_id", jstr (Id.Session_id.to_string entry.session_id));
             ("code", jstr "m.unverified");
           ])
  in
  ignore (Encryption.process_sync target (sync_of ~to_device:[ direct ] ()));
  let withheld_only = { bundle with room_keys = [] } in
  ignore
    (Encryption.import_room_key_bundle target ~room_id:room ~sender:bob_id
       withheld_only);
  let state = Encryption.snapshot target in
  check_int "duplicate withholding is replaced, not appended" 1
    (List.length state.state.withheld);
  match state.state.withheld with
  | [ stored ] ->
      check_string "new code replaces the old same-session code"
        "m.history_not_shared" stored.code;
      check_string "latest bundle sender is retained"
        (Id.User_id.to_string bob_id)
        (Option.map Id.User_id.to_string stored.sender_user
        |> Option.value ~default:"<missing>")
  | _ -> Alcotest.fail "expected one deduplicated withholding entry"

let test_withheld_provenance_wire () =
  let bundle, _key = history_bundle_fixture "withheld-provenance-wire" in
  let target =
    make ~seed:"withheld-provenance-wire-target" ~user:alice_id
      ~device:(did "TARGET") ()
  in
  ignore
    (Encryption.import_room_key_bundle target ~room_id:room ~sender:bob_id
       { bundle with room_keys = [] });
  let outgoing = Encryption.build_room_key_bundle target ~room_id:room in
  let json =
    match Jsont.Json.encode Room_key_export.room_key_bundle_jsont outgoing with
    | Ok json -> json_to_string json
    | Error error -> Alcotest.failf "could not encode bundle: %s" error
  in
  check_bool "bundle wire excludes sender provenance" false
    (contains "sender_user" json);
  check_string "bundle keeps Rust withholding reason"
    "The sender disabled sharing encrypted history."
    (Option.get (List.hd outgoing.withheld).reason)

let test_withheld_provenance_clear_on_key () =
  let bundle, key = history_bundle_fixture "withheld-provenance-clear" in
  let target =
    make ~seed:"withheld-provenance-clear-target" ~user:alice_id
      ~device:(did "TARGET") ()
  in
  let withheld_only = { bundle with room_keys = [] } in
  ignore
    (Encryption.import_room_key_bundle target ~room_id:room ~sender:bob_id
       withheld_only);
  check_bool "withholding starts pending" true
    (Option.is_some
       (Encryption.withheld_for target ~room_id:room ~session_id:key.session_id));
  let result = Encryption.import_room_keys target [ key ] in
  check_int "replacement session imports" 1 result.imported_count;
  check_bool "replacement session clears withholding" true
    (Encryption.withheld_for target ~room_id:room ~session_id:key.session_id
    = None)

let test_withheld_provenance_invalid_entries () =
  let bundle, _key = history_bundle_fixture "withheld-provenance-invalid" in
  let target =
    make ~seed:"withheld-provenance-invalid-target" ~user:alice_id
      ~device:(did "TARGET") ()
  in
  let withheld_only = { bundle with room_keys = [] } in
  ignore
    (Encryption.import_room_key_bundle target ~room_id:room ~sender:bob_id
       withheld_only);
  let original = List.hd bundle.withheld in
  let wrong_room =
    { original with room_id = Id.Room_id.of_string_exn "!wrong:example.org" }
  in
  let malformed = { original with sender_key = "not-a-curve25519-key" } in
  ignore
    (Encryption.import_room_key_bundle target ~room_id:room ~sender:alice_id
       {
         Room_key_export.room_keys = [];
         withheld =
           [ { wrong_room with algorithm = "m.megolm.unsupported" }; malformed ];
       });
  check_int "invalid withholding entries do not append state" 1
    (List.length (Encryption.snapshot target).state.withheld);
  match
    Encryption.withheld_for target ~room_id:room ~session_id:original.session_id
  with
  | Some stored ->
      check_string "invalid entries preserve sender provenance"
        (Id.User_id.to_string bob_id)
        (Option.map Id.User_id.to_string stored.sender_user
        |> Option.value ~default:"<missing>")
  | None -> Alcotest.fail "valid withholding was unexpectedly removed"

let test_withheld_provenance_persistence () =
  let bundle, _key = history_bundle_fixture "withheld-provenance-persist" in
  let target =
    make ~seed:"withheld-provenance-persist-target" ~user:alice_id
      ~device:(did "TARGET") ()
  in
  ignore
    (Encryption.import_room_key_bundle target ~room_id:room ~sender:bob_id
       { bundle with room_keys = [] });
  with_store_xdg @@ fun xdg store ->
  ok_value (Crypto_store.save store (Encryption.snapshot target));
  let snapshot =
    match ok_value (Crypto_store.load store) with
    | Some snapshot -> snapshot
    | None -> Alcotest.fail "withheld provenance snapshot was not stored"
  in
  match snapshot.state.withheld with
  | [ stored ] -> (
      check_string "stored sender provenance"
        (Id.User_id.to_string bob_id)
        (Option.map Id.User_id.to_string stored.sender_user
        |> Option.value ~default:"<missing>");
      let path =
        Eio.Path.(
          Xdge.data_dir xdg / "profiles" / "default" / "crypto_state.json")
      in
      let legacy =
        match
          Jsont_bytesrw.decode_string Matrix_proto.Json.Codec.json
            (Eio.Path.load path)
        with
        | Error error -> Alcotest.failf "decode stored crypto state: %s" error
        | Ok (Jsont.Object (members, meta)) ->
            let strip_withheld_member ((name, name_meta), value) =
              if not (String.equal name "withheld") then
                ((name, name_meta), value)
              else
                let value =
                  match value with
                  | Jsont.Array (entries, array_meta) ->
                      Jsont.Array
                        ( List.map
                            (function
                              | Jsont.Object (fields, object_meta) ->
                                  Jsont.Object
                                    ( List.filter
                                        (fun ((field, _), _) ->
                                          not (String.equal field "sender_user"))
                                        fields,
                                      object_meta )
                              | value -> value)
                            entries,
                          array_meta )
                  | value -> value
                in
                ((name, name_meta), value)
            in
            Jsont.Object (List.map strip_withheld_member members, meta)
        | Ok _ -> Alcotest.fail "stored crypto state is not an object"
      in
      let legacy =
        match
          Jsont_bytesrw.encode_string ~format:Jsont.Indent
            Matrix_proto.Json.Codec.json legacy
        with
        | Ok encoded -> encoded
        | Error error -> Alcotest.failf "encode legacy crypto state: %s" error
      in
      Eio.Path.save ~create:(`Or_truncate 0o600) path legacy;
      let legacy_store = Crypto_store.create ~xdg ~profile:"default" in
      let legacy_snapshot =
        match ok_value (Crypto_store.load legacy_store) with
        | Some snapshot -> snapshot
        | None -> Alcotest.fail "legacy crypto snapshot was not restored"
      in
      match legacy_snapshot.state.withheld with
      | [ stored ] ->
          check_bool "legacy withholding defaults to no sender provenance" true
            (stored.sender_user = None)
      | _ -> Alcotest.fail "expected one legacy withholding entry")
  | _ -> Alcotest.fail "expected one persisted withholding entry"

let test_pending_key_bundle_acceptance () =
  let t =
    make ~seed:"pending-bundle" ~user:alice_id ~device:(did "TARGET") ()
  in
  let t0 = at 1_700_000_000. in
  let other_room = Id.Room_id.of_string_exn "!pending-other:example.org" in
  Encryption.record_invite_acceptance ~now:t0 t ~room_id:room ~inviter:bob_id;
  Encryption.record_invite_acceptance ~now:(time_after t0 10) t ~room_id:room
    ~inviter:alice_id;
  check_bool "recording replaces the room's inviter" true
    (match Encryption.pending_key_bundle t ~room_id:room with
    | Some p -> Id.User_id.equal p.inviter alice_id
    | None -> false);
  check_bool "wrong sender is rejected" false
    (Encryption.should_accept_room_key_bundle ~now:(time_after t0 11) t
       ~room_id:room ~joined:true ~sender:bob_id ());
  check_bool "not joined is rejected" false
    (Encryption.should_accept_room_key_bundle ~now:(time_after t0 11) t
       ~room_id:room ~joined:false ~sender:alice_id ());
  check_bool "an accepted invite is usable before 24 hours" true
    (Encryption.should_accept_room_key_bundle ~now:(time_after t0 10) t
       ~room_id:room ~joined:true ~sender:alice_id ());
  check_bool "exactly 24 hours is rejected" false
    (Encryption.should_accept_room_key_bundle
       ~now:(time_after t0 (10 + 86400))
       t ~room_id:room ~joined:true ~sender:alice_id ());
  check_bool "a future acceptance timestamp is rejected" false
    (Encryption.should_accept_room_key_bundle ~now:t0 t ~room_id:room
       ~joined:true ~sender:alice_id ());
  let future_room = Id.Room_id.of_string_exn "!pending-future:example.org" in
  Encryption.record_invite_acceptance ~now:(time_after t0 100) t
    ~room_id:future_room ~inviter:bob_id;
  let future_removed =
    Encryption.clear_expired_pending_key_bundles ~now:(time_after t0 10) t
  in
  check_int "future pending records are invalidated" 1
    (List.length future_removed);
  check_bool "future pending record is cleared" true
    (Encryption.pending_key_bundle t ~room_id:future_room = None);
  Encryption.record_invite_acceptance ~now:t0 t ~room_id:other_room
    ~inviter:bob_id;
  let removed =
    Encryption.clear_expired_pending_key_bundles ~now:(time_after t0 86400) t
  in
  check_int "expired pending records are removed" 1 (List.length removed);
  check_bool "the refreshed room remains pending" true
    (Encryption.pending_key_bundle t ~room_id:room <> None);
  check_bool "the expired other room is no longer pending" true
    (Encryption.pending_key_bundle t ~room_id:other_room = None);
  let accepted_room =
    Id.Room_id.of_string_exn "!pending-accepted:example.org"
  in
  Encryption.record_invite_acceptance ~now:t0 t ~room_id:accepted_room
    ~inviter:bob_id;
  let empty_bundle = { Room_key_export.room_keys = []; withheld = [] } in
  (match
     Encryption.accept_room_key_bundle ~now:(time_after t0 1) t
       ~room_id:accepted_room ~joined:true ~sender:alice_id empty_bundle
   with
  | None -> ()
  | Some _ -> Alcotest.fail "wrong sender unexpectedly consumed pending bundle");
  check_bool "wrong sender leaves pending state" true
    (Encryption.pending_key_bundle t ~room_id:accepted_room <> None);
  (match
     Encryption.accept_room_key_bundle ~now:(time_after t0 1) t
       ~room_id:accepted_room ~joined:true ~sender:bob_id empty_bundle
   with
  | Some 0 -> ()
  | Some count -> Alcotest.failf "empty bundle imported %d keys" count
  | None -> Alcotest.fail "accepted empty bundle was rejected");
  check_bool "accepted empty bundle consumes pending state" true
    (Encryption.pending_key_bundle t ~room_id:accepted_room = None);
  with_store_xdg @@ fun xdg store ->
  let persisted_room =
    Id.Room_id.of_string_exn "!pending-persist:example.org"
  in
  let expired_room = Id.Room_id.of_string_exn "!pending-expired:example.org" in
  let fresh_now =
    Option.value ~default:Ptime.epoch (Ptime.of_float_s (Unix.gettimeofday ()))
  in
  Encryption.record_invite_acceptance ~now:fresh_now t ~room_id:persisted_room
    ~inviter:bob_id;
  Encryption.record_invite_acceptance ~now:Ptime.epoch t ~room_id:expired_room
    ~inviter:bob_id;
  ok_value (Crypto_store.save store (Encryption.snapshot t));
  let restored =
    match ok_value (Crypto_store.load store) with
    | Some snapshot ->
        Encryption.of_snapshot
          ~random:(random_of "pending-restored")
          ~user_id:alice_id ~device_id:(did "TARGET") snapshot
    | None -> Alcotest.fail "pending bundle snapshot was not restored"
  in
  check_bool "pending acceptance survives restart" true
    (Encryption.pending_key_bundle restored ~room_id:persisted_room <> None);
  let driver =
    ok_value
      (Driver.create
         ~random:(random_of "pending-driver-restored")
         ~user_id:alice_id ~device_id:(did "TARGET") ~store ())
  in
  check_bool "startup retains a fresh pending acceptance" true
    (Encryption.pending_key_bundle (Driver.machine driver)
       ~room_id:persisted_room
    <> None);
  check_bool "startup clears an expired pending acceptance" true
    (Encryption.pending_key_bundle (Driver.machine driver) ~room_id:expired_room
    = None);
  let cleaned =
    match ok_value (Crypto_store.load store) with
    | Some snapshot -> snapshot
    | None -> Alcotest.fail "cleaned pending snapshot was not persisted"
  in
  check_bool "startup cleanup is durable" true
    (List.for_all
       (fun (p : Encryption.pending_key_bundle) ->
         not (Id.Room_id.equal p.room_id expired_room))
       cleaned.state.pending_key_bundles);
  let path =
    Eio.Path.(Xdge.data_dir xdg / "profiles" / "default" / "crypto_state.json")
  in
  let legacy =
    match
      Jsont_bytesrw.decode_string Matrix_proto.Json.Codec.json
        (Eio.Path.load path)
    with
    | Error error -> Alcotest.failf "decode pending snapshot: %s" error
    | Ok (Jsont.Object (members, meta)) ->
        Jsont.Object
          ( List.filter
              (fun ((name, _), _) ->
                not (String.equal name "pending_key_bundles"))
              members,
            meta )
    | Ok _ -> Alcotest.fail "pending snapshot is not an object"
  in
  Eio.Path.save ~create:(`Or_truncate 0o600) path
    (Result.get_ok
       (Jsont_bytesrw.encode_string ~format:Jsont.Indent
          Matrix_proto.Json.Codec.json legacy));
  let migrated =
    match
      ok_value (Crypto_store.load (Crypto_store.create ~xdg ~profile:"default"))
    with
    | Some snapshot -> snapshot
    | None -> Alcotest.fail "legacy pending snapshot was not restored"
  in
  check_bool "old codec defaults pending records to empty" true
    (migrated.state.pending_key_bundles = [])

let test_room_key_bundle_download_lifecycle () =
  let _withheld_bundle, portable =
    history_bundle_fixture "bundle-download-source"
  in
  let historic : Room_key_export.historic_room_key =
    {
      algorithm = portable.algorithm;
      room_id = portable.room_id;
      sender_key = portable.sender_key;
      session_id = portable.session_id;
      session_key = portable.session_key;
      sender_claimed_keys = portable.sender_claimed_keys;
    }
  in
  let payload =
    Result.get_ok
      (Jsont_bytesrw.encode_string Room_key_export.room_key_bundle_jsont
         { room_keys = [ historic ]; withheld = [] })
  in
  let valid_file, valid_ciphertext =
    encrypted_bundle_file "bundle-download-valid" payload
  in
  let malformed_file, malformed_ciphertext =
    encrypted_bundle_file "bundle-download-malformed" "not JSON"
  in
  let bob =
    make ~seed:"bundle-download-bob" ~user:bob_id ~device:(did "BOBDEV") ()
  in
  let bob_ed25519, bob_curve25519 = Encryption.identity_keys bob in
  let received file : Encryption.received_key_bundle =
    {
      room_id = room;
      sender = bob_id;
      sender_key = bob_curve25519;
      sender_ed25519 = Ck.Ed25519.Public.to_base64 bob_ed25519;
      file;
    }
  in
  let t0 = at 1_700_000_000. in
  let target seed bundle =
    let device_id = did "TARGET" in
    let machine = make ~seed ~user:alice_id ~device:device_id () in
    Encryption.record_invite_acceptance ~now:t0 machine ~room_id:room
      ~inviter:bob_id;
    let snapshot = Encryption.snapshot machine in
    Encryption.of_snapshot
      ~random:(random_of (seed ^ "-restored"))
      ~user_id:alice_id ~device_id
      {
        snapshot with
        state = { snapshot.state with received_key_bundles = [ bundle ] };
      }
  in
  let versions = {|{"versions":["v1.11"],"unstable_features":{}}|} in
  let client ~query ~media =
    let _log, fetch =
      mock (fun request ->
          let url = Fetch.Middleware.Url.to_string request.url in
          if contains "/keys/query" url then query request
          else if contains "/versions" url then
            Fetch_mock.respond versions request
          else if contains "/media/download/" url then media request
          else Alcotest.failf "unexpected bundle lifecycle request: %s" url)
    in
    client_of fetch
  in
  let cross_signed_response =
    cross_signed_query_for ~seed:"bundle-download-cross-signing" bob
  in
  let cross_signed = query_response_body cross_signed_response in
  let success_bundle = received valid_file in
  let success = target "bundle-download-success" success_bundle in
  let success_client =
    client
      ~query:(Fetch_mock.respond cross_signed)
      ~media:(Fetch_mock.respond valid_ciphertext)
  in
  (match
     Driver.accept_received_room_key_bundle ~now:t0 (drive success)
       success_client ~joined:true success_bundle
   with
  | Driver.Bundle_imported 1 -> ()
  | Driver.Bundle_imported count ->
      Alcotest.failf "bundle lifecycle imported %d keys" count
  | _ -> Alcotest.fail "valid bundle was not imported");
  check_bool "downloaded bundle installs its session" true
    (Encryption.has_inbound_session success room ~session_id:historic.session_id);
  check_bool "successful import clears both durable records" true
    (Encryption.pending_key_bundle success ~room_id:room = None
    && Encryption.received_key_bundles success = []);
  let retry_bundle = received valid_file in
  let retry = target "bundle-download-retry" retry_bundle in
  let retry_client =
    client
      ~query:
        (Fetch_mock.respond ~status:503
           {|{"errcode":"M_UNKNOWN","error":"try later"}|}) ~media:(fun _ ->
        Alcotest.fail "media fetched after failed key query")
  in
  (match
     Driver.accept_received_room_key_bundle ~now:t0 (drive retry) retry_client
       ~joined:true retry_bundle
   with
  | Driver.Bundle_retry_key_query _ -> ()
  | _ -> Alcotest.fail "key-query failure was not retryable");
  check_bool "transient query failure retains both records" true
    (Encryption.pending_key_bundle retry ~room_id:room <> None
    && Encryption.received_key_bundles retry <> []);
  let rejected_bundle = received valid_file in
  let rejected = target "bundle-download-rejected" rejected_bundle in
  Encryption.receive_keys_query rejected cross_signed_response;
  check_bool "the pre-query sender starts trusted" true
    (Encryption.room_key_bundle_sender_is_trusted rejected rejected_bundle);
  let rejected_client =
    client
      ~query:
        (Fetch_mock.respond
           {|{"failures":{},"device_keys":{},"master_keys":{},"self_signing_keys":{},"user_signing_keys":{}}|})
      ~media:(fun _ -> Alcotest.fail "untrusted sender media was downloaded")
  in
  (match
     Driver.accept_received_room_key_bundle ~now:t0 (drive rejected)
       rejected_client ~joined:true rejected_bundle
   with
  | Driver.Bundle_rejected_sender -> ()
  | _ -> Alcotest.fail "sender omitted by its fresh query was accepted");
  check_bool "an authoritative omission clears stale trust and both records"
    true
    (Encryption.pending_key_bundle rejected ~room_id:room = None
    && Encryption.received_key_bundles rejected = []);
  let missing_bundle = received valid_file in
  let missing = target "bundle-download-missing" missing_bundle in
  let missing_client =
    client
      ~query:(Fetch_mock.respond cross_signed)
      ~media:
        (Fetch_mock.respond ~status:404
           {|{"errcode":"M_NOT_FOUND","error":"gone"}|})
  in
  (match
     Driver.accept_received_room_key_bundle ~now:t0 (drive missing)
       missing_client ~joined:true missing_bundle
   with
  | Driver.Bundle_discarded_not_found -> ()
  | _ -> Alcotest.fail "404 bundle media was not discarded");
  check_bool "404 clears both records" true
    (Encryption.pending_key_bundle missing ~room_id:room = None
    && Encryption.received_key_bundles missing = []);
  let malformed_bundle = received malformed_file in
  let malformed = target "bundle-download-malformed" malformed_bundle in
  let malformed_client =
    client
      ~query:(Fetch_mock.respond cross_signed)
      ~media:(Fetch_mock.respond malformed_ciphertext)
  in
  (match
     Driver.accept_received_room_key_bundle ~now:t0 (drive malformed)
       malformed_client ~joined:true malformed_bundle
   with
  | Driver.Bundle_discarded_malformed _ -> ()
  | _ -> Alcotest.fail "malformed bundle was not discarded");
  check_bool "malformed bundle clears both records" true
    (Encryption.pending_key_bundle malformed ~room_id:room = None
    && Encryption.received_key_bundles malformed = [])

let test_backup_round_trip () =
  let log, client = backup_client () in
  let alice =
    make ~seed:"alice-backup" ~user:alice_id ~device:(did "ALICEDEV") ()
  in
  let bob = make ~seed:"bob-backup" ~user:bob_id ~device:(did "BOBDEV") () in
  introduce ~from:bob ~into:alice;
  introduce ~from:alice ~into:alice;
  ignore (Encryption.receive_keys_claim alice (claim_response_for bob));
  let content = jobj [ ("body", jstr "keep me") ] in
  let alice_d = drive alice in
  let encrypted =
    ok_value
      (Driver.encrypt_room_event alice_d client room
         ~event_type:"m.room.message" ~content ~members:[ alice_id; bob_id ])
  in
  let session_id = Option.get (Encryption.outbound_session_id alice room) in
  let key = Backup.Decryption_key.generate ~random:(random_of "backup-key") in
  check_int "nothing is pending before a backup is enabled" 0
    (Encryption.backup_pending_count alice);
  Encryption.enable_backup alice ~version:"v1"
    (Backup.Decryption_key.public key);
  check_int "our own session is pending" 1
    (Encryption.backup_pending_count alice);
  check_int "and it is uploaded" 1
    (ok_value (Driver.backup_pending alice_d client));
  check_int "then nothing is left" 0 (Encryption.backup_pending_count alice);
  check_int "uploading twice sends nothing" 0
    (ok_value (Driver.backup_pending alice_d client));
  Encryption.enable_backup alice ~version:"v1"
    (Backup.Decryption_key.public key);
  check_int "re-enabling the same version keeps the upload ledger" 0
    (Encryption.backup_pending_count alice);
  check_int "re-enabling the same version is upload-idempotent" 0
    (ok_value (Driver.backup_pending alice_d client));
  check_bool "the upload went to /room_keys/keys" true
    (List.exists
       (fun r -> r.meth = "PUT" && contains "/room_keys/keys" r.url)
       (requests log));
  (* A device that has never seen the session recovers it from the backup. *)
  let carol =
    make ~seed:"carol-backup" ~user:alice_id ~device:(did "CAROL") ()
  in
  check_bool "Carol starts without the session" false
    (Encryption.has_inbound_session carol room ~session_id);
  Encryption.enable_backup carol ~version:"v1" ~decryption_key:key
    (Backup.Decryption_key.public key);
  check_int "one session is restored" 1
    (ok_value (Driver.restore_from_backup (drive carol) client));
  check_bool "and Carol now holds it" true
    (Encryption.has_inbound_session carol room ~session_id);
  match
    Encryption.decrypt_room_event carol room
      (encrypted_raw ~sender:alice_id ~content:encrypted)
  with
  | Error e ->
      Alcotest.failf "a restored session must decrypt: %a"
        Encryption.pp_decrypt_error e
  | Ok d ->
      check_string "the plaintext comes back" {|{"body":"keep me"}|}
        (json_to_string d.decrypted_content)

let test_backup_import_requires_exported_v1_key () =
  let backup_key =
    Backup.Decryption_key.generate ~random:(random_of "backup-v1-policy")
  in
  let signing_private, signing_public =
    Ck.Ed25519.generate ~random:(random_of "backup-v1-signing") ()
  in
  let _sender_secret, sender_public =
    Ck.Curve25519.generate ~random:(random_of "backup-v1-sender") ()
  in
  let sid =
    Id.Session_id.of_string_exn (Ck.Ed25519.Public.to_base64 signing_public)
  in
  let exported_body =
    String.concat ""
      [
        String.make 1 '\001';
        String.make 4 '\000';
        String.make 128 '\000';
        Ck.Ed25519.Public.to_bytes signing_public;
      ]
  in
  let exported_key = Matrix_proto.Base64.encode exported_body in
  let signed_v2_body =
    String.concat ""
      [
        String.make 1 '\002';
        String.sub exported_body 1 132;
        Ck.Ed25519.Public.to_bytes signing_public;
      ]
  in
  let signed_v2_key =
    Matrix_proto.Base64.encode
      (signed_v2_body
      ^ Ck.Signature.to_bytes
          (Ck.Ed25519.Private.sign signing_private signed_v2_body))
  in
  let make_rooms ?(forwarding_chain = []) ~map_session_id ~session_key () =
    let payload : Backup.backed_up_session_data =
      {
        algorithm = "m.megolm.v1.aes-sha2";
        forwarding_curve25519_key_chain = forwarding_chain;
        sender_key = Ck.Curve25519.Public.to_base64 sender_public;
        sender_claimed_keys = [];
        session_key;
        shared_history = false;
      }
    in
    let plaintext =
      match
        Jsont_bytesrw.encode_string Backup.backed_up_session_data_jsont payload
      with
      | Ok value -> value
      | Error error ->
          Alcotest.failf "could not encode backup fixture: %s" error
    in
    let session_data =
      match
        Backup.encrypt_session_data
          ~random:(random_of "backup-v1-encrypt")
          (Backup.Decryption_key.public backup_key)
          plaintext
      with
      | Ok value -> value
      | Error (`Msg error) ->
          Alcotest.failf "could not encrypt backup fixture: %s" error
    in
    [
      ( room_str,
        [
          ( Id.Session_id.to_string map_session_id,
            {
              Backup.first_message_index = 999;
              forwarded_count = 999;
              is_verified = true;
              session_data;
            } );
        ] );
    ]
  in
  let machine seed =
    let machine =
      Encryption.create ~random:(random_of seed) ~user_id:alice_id
        ~device_id:(did "BACKUP-V1") ()
    in
    Encryption.enable_backup machine ~version:"v1" ~decryption_key:backup_key
      (Backup.Decryption_key.public backup_key);
    machine
  in
  let imported = machine "backup-v1-import" in
  check_int "unsigned exported v1 backup imports" 1
    (ok_value
       (Encryption.import_backup imported
          (make_rooms ~map_session_id:sid ~session_key:exported_key ())));
  let imported_session =
    match (Encryption.snapshot imported).megolm_inbound with
    | [ session ] -> session
    | sessions ->
        Alcotest.failf "expected one imported session, got %d"
          (List.length sessions)
  in
  check_int "outer first_message_index is ignored" 0
    (Olm.Megolm.Inbound.first_known_index imported_session);
  check_bool "outer is_verified is ignored" false
    (Olm.Megolm.Inbound.signing_key_verified imported_session);
  let v2_machine = machine "backup-v2-rejected" in
  check_int "signed v2 room-key blobs are rejected" 0
    (ok_value
       (Encryption.import_backup v2_machine
          (make_rooms ~map_session_id:sid ~session_key:signed_v2_key ())));
  let oversized_chain_machine = machine "backup-oversized-chain" in
  let forwarding_key = Ck.Curve25519.Public.to_base64 sender_public in
  check_int "oversized backup forwarding chain is rejected" 0
    (ok_value
       (Encryption.import_backup oversized_chain_machine
          (make_rooms
             ~forwarding_chain:(List.init 101 (Fun.const forwarding_key))
             ~map_session_id:sid ~session_key:exported_key ())));
  let mismatch_machine = machine "backup-session-id-mismatch" in
  let _other_private, other_public =
    Ck.Ed25519.generate ~random:(random_of "backup-other-session-id") ()
  in
  let other_sid =
    Id.Session_id.of_string_exn (Ck.Ed25519.Public.to_base64 other_public)
  in
  check_int "backup map/session ID mismatch is skipped" 0
    (ok_value
       (Encryption.import_backup mismatch_machine
          (make_rooms ~map_session_id:other_sid ~session_key:exported_key ())))

let test_scoped_backup_restore_and_history_gates () =
  let log, client = backup_client () in
  let machine =
    make ~seed:"scoped-backup" ~user:alice_id ~device:(did "ALICEDEV") ()
  in
  let driver = drive machine in
  let backup_key =
    Backup.Decryption_key.generate ~random:(random_of "scoped-backup-key")
  in
  Encryption.enable_backup machine ~version:"v1" ~decryption_key:backup_key
    (Backup.Decryption_key.public backup_key);
  (match
     Driver.share_room_history driver client ~room_id:room ~recipient:bob_id
       ~history_visibility:Ev.History_visibility.Shared
   with
  | Ok Driver.History_not_shared_identity -> ()
  | Ok _ -> Alcotest.fail "missing cross-signing identity was not skipped"
  | Error _ -> Alcotest.fail "identity gate performed I/O");
  Encryption.receive_keys_query machine
    (cross_signed_query_for ~user_id:alice_id ~seed:"scoped-identity" machine);
  (match
     Driver.share_room_history driver client ~room_id:room ~recipient:bob_id
       ~history_visibility:Ev.History_visibility.Joined
   with
  | Ok Driver.History_not_shared_visibility -> ()
  | Ok _ -> Alcotest.fail "joined history visibility was not skipped"
  | Error _ -> Alcotest.fail "visibility gate performed I/O");
  (* The scoped restore calls are intentionally exercised against an empty
     backup: they must use the room/session endpoints and remain harmless when
     no encrypted session is present. *)
  let restored_room = Driver.restore_room_from_backup driver client room in
  let restore_error = function
    | Ok n -> n
    | Error e ->
        Alcotest.failf "scoped room restore: %s (urls: %s)" (Error.to_string e)
          (String.concat "," (List.map (fun r -> r.url) (requests log)))
  in
  check_int "empty room restore" 0 (restore_error restored_room);
  let session_id = Id.Session_id.of_string_exn "missing-scoped-session" in
  (match
     Driver.restore_session_from_backup driver client ~room_id:room ~session_id
   with
  | Error _ -> ()
  | Ok count ->
      Alcotest.failf "missing scoped session unexpectedly restored %d keys"
        count);
  let urls = List.map (fun r -> r.url) (requests log) in
  check_bool "room restore endpoint used" true
    (List.exists (fun url -> contains "/room_keys/keys/" url) urls)

let test_room_backup_download_failure_is_retryable () =
  let attempts = ref 0 in
  let log, fetch =
    mock (fun request ->
        let url = Fetch.Middleware.Url.to_string request.url in
        if contains "/room_keys/keys/" url then begin
          incr attempts;
          if !attempts = 1 then
            Fetch_mock.respond ~status:503
              {|{"errcode":"M_UNAVAILABLE","error":"offline"}|} request
          else Fetch_mock.respond {|{"sessions":{}}|} request
        end
        else permissive_handler request)
  in
  let machine =
    make ~seed:"backup-download-retry" ~user:alice_id ~device:(did "ALICEDEV")
      ()
  in
  let backup_key =
    Backup.Decryption_key.generate ~random:(random_of "backup-download-key")
  in
  Encryption.enable_backup machine ~version:"v1" ~decryption_key:backup_key
    (Backup.Decryption_key.public backup_key);
  let driver = drive machine in
  (match Driver.restore_room_from_backup driver (client_of fetch) room with
  | Error _ -> ()
  | Ok count ->
      Alcotest.failf "failed room backup unexpectedly restored %d keys" count);
  check_int "failed room backup attempted once" 1 !attempts;
  check_bool "failed room backup is not marked complete" false
    (Encryption.room_key_backup_is_fully_downloaded machine room);
  (match Driver.restore_room_from_backup driver (client_of fetch) room with
  | Ok 0 -> ()
  | Ok count -> Alcotest.failf "empty retry unexpectedly restored %d keys" count
  | Error e -> Alcotest.failf "room backup retry failed: %s" (Error.to_string e));
  check_int "room backup retry downloads again" 2 !attempts;
  let urls_before = List.length (requests log) in
  ignore (Driver.restore_room_from_backup driver (client_of fetch) room);
  check_int "successful room backup is then cached" urls_before
    (List.length (requests log))

let test_share_room_history_payload_and_order () =
  let source_log, source_client = backup_client () in
  let alice =
    make ~seed:"share-history-alice" ~user:alice_id ~device:(did "ALICEDEV") ()
  in
  let bob =
    make ~seed:"share-history-bob" ~user:bob_id ~device:(did "BOBDEV") ()
  in
  let bob2 =
    make ~seed:"share-history-bob2" ~user:bob_id ~device:(did "BOB2DEV") ()
  in
  introduce ~from:alice ~into:bob;
  introduce ~from:bob ~into:alice;
  introduce ~from:bob2 ~into:alice;
  ignore (Encryption.receive_keys_claim bob (claim_response_for alice));
  ignore (share_and_encrypt bob alice source_client source_log ~body:"history");
  ignore (Encryption.receive_keys_claim alice (claim_response_for bob));
  ignore (Encryption.receive_keys_claim alice (claim_response_for bob2));
  Encryption.set_device_trust alice bob_id ~device_id:(did "BOB2DEV")
    Encryption.Blacklisted;
  Encryption.receive_keys_query alice
    (cross_signed_query_for ~user_id:alice_id ~seed:"share-history-identity"
       alice);
  Encryption.trust_user_identity alice alice_id;
  let backup_key =
    Backup.Decryption_key.generate ~random:(random_of "share-history-backup")
  in
  Encryption.enable_backup alice ~version:"v1" ~decryption_key:backup_key
    (Backup.Decryption_key.public backup_key);
  let empty_query = ref false in
  let sends = ref 0 in
  let recipient_query =
    let one = query_response_for bob in
    let two = query_response_for bob2 in
    {
      one with
      device_keys =
        [ (bob_id, List.concat_map snd (one.device_keys @ two.device_keys)) ];
    }
  in
  let log, fetch =
    mock (fun request ->
        let url = Fetch.Middleware.Url.to_string request.url in
        if contains "/room_keys/keys/" url then
          Fetch_mock.respond {|{"sessions":{}}|} request
        else if contains "/_matrix/media/v3/upload" url then
          Fetch_mock.respond
            {|{"content_uri":"mxc://hs.example/history-bundle"}|} request
        else if contains "/keys/query" url then
          if !empty_query then
            Fetch_mock.respond
              {|{"failures":{},"device_keys":{},"master_keys":{},"self_signing_keys":{},"user_signing_keys":{}}|}
              request
          else Fetch_mock.respond (query_response_body recipient_query) request
        else if contains "/sendToDevice/" url then (
          incr sends;
          Fetch_mock.respond "{}" request)
        else Alcotest.failf "unexpected history share request: %s" url)
  in
  let client = client_of fetch in
  match
    Driver.share_room_history (drive alice) client ~room_id:room
      ~recipient:bob_id ~history_visibility:Ev.History_visibility.Shared
  with
  | Error error ->
      Alcotest.failf "history share failed: %s"
        (match error with
        | Driver.Share_encryption_error e -> Error.to_string e
        | Driver.Share_media_error _ -> "media error")
  | Ok (Driver.History_shared count) ->
      check_int "one non-blacklisted device received history" 1 count;
      let urls = List.map (fun r -> r.url) (requests log) in
      let positions needle =
        match List.find_index (contains needle) urls with
        | Some i -> i
        | None -> Alcotest.failf "history request %s was not made" needle
      in
      check_bool "backup room download precedes media upload" true
        (positions "/room_keys/keys/" < positions "/_matrix/media/v3/upload");
      let backup_downloads () =
        List.length
          (List.filter
             (contains "/room_keys/keys/")
             (List.map (fun r -> r.url) (requests log)))
      in
      check_int "first share downloads one room backup" 1 (backup_downloads ());
      check_bool "upload precedes fresh device query" true
        (positions "/_matrix/media/v3/upload" < positions "/keys/query");
      check_bool "fresh device query precedes to-device send" true
        (positions "/keys/query" < positions "/sendToDevice/");
      let sent = last_to_device log in
      (match sent with
      | [ (user, device, content) ] ->
          check_string "bundle recipient" (Id.User_id.to_string bob_id) user;
          check_string "bundle device"
            (Id.Device_id.to_string (did "BOBDEV"))
            device;
          let envelope =
            match Jsont.Json.decode Ev.Encrypted.Olm.jsont content with
            | Ok value -> value
            | Error msg -> Alcotest.failf "bundle is not Olm ciphertext: %s" msg
          in
          check_bool "bundle ciphertext has one recipient" true
            (List.length envelope.ciphertext = 1)
      | _ -> Alcotest.fail "history bundle addressed an unexpected device");
      check_bool "blacklisted device was excluded" true
        (not
           (List.exists
              (fun (_, device, _) -> device = "BOB2DEV")
              (last_to_device log)));
      let sends_before_empty = !sends in
      empty_query := true;
      let downloads_before_repeat = backup_downloads () in
      (match
         Driver.share_room_history (drive alice) client ~room_id:room
           ~recipient:bob_id ~history_visibility:Ev.History_visibility.Shared
       with
      | Ok Driver.History_no_keys -> ()
      | Ok _ -> Alcotest.fail "empty fresh query retained stale recipient"
      | Error _ -> Alcotest.fail "empty fresh query unexpectedly failed");
      check_int "empty fresh query sends nothing" sends_before_empty !sends;
      check_int "repeat share skips complete room backup"
        downloads_before_repeat (backup_downloads ());
      check_bool "empty fresh query clears stale recipient" true
        (Encryption.find_device alice bob_id ~device_id:(did "BOBDEV") = None
        && Encryption.find_device alice bob_id ~device_id:(did "BOB2DEV") = None
        );
      (* A different backup version invalidates the room completion marker and
         makes the next share refresh that version's room keys. *)
      Encryption.enable_backup alice ~version:"v2" ~decryption_key:backup_key
        (Backup.Decryption_key.public backup_key);
      let downloads_before_version = backup_downloads () in
      (match
         Driver.share_room_history (drive alice) client ~room_id:room
           ~recipient:bob_id ~history_visibility:Ev.History_visibility.Shared
       with
      | Ok Driver.History_no_keys -> ()
      | Ok _ -> Alcotest.fail "version switch retained stale devices"
      | Error _ -> Alcotest.fail "version switch share unexpectedly failed");
      check_int "backup version switch refetches room"
        (downloads_before_version + 1)
        (backup_downloads ());
      (* The authoritative empty answer is durable too: a restart must not
         resurrect devices that were present before the fresh query. *)
      with_store (fun store ->
          let persisted = drive ~store alice in
          (match Driver.save persisted with
          | Ok () -> ()
          | Error e ->
              Alcotest.failf "saving empty-query state failed: %s"
                (Error.to_string e));
          let restarted =
            ok_value
              (Driver.create
                 ~random:(random_of "share-history-restart")
                 ~user_id:alice_id ~device_id:(did "ALICEDEV") ~store ())
          in
          let downloads_before_restart = backup_downloads () in
          (match
             Driver.share_room_history restarted client ~room_id:room
               ~recipient:bob_id
               ~history_visibility:Ev.History_visibility.Shared
           with
          | Ok Driver.History_no_keys -> ()
          | Ok _ -> Alcotest.fail "restart retained stale recipient devices"
          | Error _ -> Alcotest.fail "restart share unexpectedly failed");
          check_int "reloaded complete room backup is not refetched"
            downloads_before_restart (backup_downloads ());
          check_bool "empty authoritative query survives restart" true
            (Encryption.find_device (Driver.machine restarted) bob_id
               ~device_id:(did "BOBDEV")
             = None
            && Encryption.find_device (Driver.machine restarted) bob_id
                 ~device_id:(did "BOB2DEV")
               = None))
  | Ok Driver.History_no_keys ->
      Alcotest.fail "history bundle unexpectedly had no keys"
  | Ok Driver.History_not_shared_visibility
  | Ok Driver.History_not_shared_identity ->
      Alcotest.fail "history share was unexpectedly skipped"

let invite_history_fixture seed =
  let source_log, source_client = backup_client () in
  let alice =
    make ~seed:(seed ^ "-alice") ~user:alice_id ~device:(did "ALICEDEV") ()
  in
  let bob = make ~seed:(seed ^ "-bob") ~user:bob_id ~device:(did "BOBDEV") () in
  introduce ~from:alice ~into:bob;
  introduce ~from:bob ~into:alice;
  ignore (Encryption.receive_keys_claim bob (claim_response_for alice));
  ignore (share_and_encrypt bob alice source_client source_log ~body:"history");
  ignore (Encryption.receive_keys_claim alice (claim_response_for bob));
  Encryption.receive_keys_query alice
    (cross_signed_query_for ~user_id:alice_id ~seed:(seed ^ "-identity") alice);
  Encryption.trust_user_identity alice alice_id;
  let backup_key =
    Backup.Decryption_key.generate ~random:(random_of (seed ^ "-backup"))
  in
  Encryption.enable_backup alice ~version:"v1" ~decryption_key:backup_key
    (Backup.Decryption_key.public backup_key);
  (alice, bob)

let test_invite_user_by_id_order_and_errors () =
  (* A missing identity is a successful sharing no-op, so the ordinary invite
     is still sent, and an invite failure remains distinguishable. *)
  let no_identity =
    make ~seed:"invite-no-identity" ~user:alice_id ~device:(did "ALICEDEV") ()
  in
  let invite_log, invite_fetch =
    mock (fun request ->
        let url = Fetch.Middleware.Url.to_string request.url in
        if contains "/invite" url then Fetch_mock.respond "{}" request
        else Alcotest.failf "unexpected no-op invite request: %s" url)
  in
  (match
     Driver.invite_user_by_id (drive no_identity) (client_of invite_fetch)
       ~room_id:room ~user_id:bob_id
       ~history_visibility:Ev.History_visibility.Shared ()
   with
  | Ok (Driver.Invite_sent Driver.History_not_shared_identity) -> ()
  | Ok _ -> Alcotest.fail "missing identity was not a sharing no-op"
  | Error _ -> Alcotest.fail "missing identity prevented the invite");
  check_bool "identity no-op sends only invite" true
    (List.length (requests invite_log) = 1
    && contains "/invite" (List.hd (requests invite_log)).url);
  let failing_log, failing_fetch =
    mock (fun request ->
        let url = Fetch.Middleware.Url.to_string request.url in
        if contains "/invite" url then
          Fetch_mock.respond ~status:403
            {|{"errcode":"M_FORBIDDEN","error":"denied"}|} request
        else Alcotest.failf "unexpected invite error request: %s" url)
  in
  (match
     Driver.invite_user_by_id (drive no_identity) (client_of failing_fetch)
       ~room_id:room ~user_id:bob_id
       ~history_visibility:Ev.History_visibility.Shared ()
   with
  | Error (Driver.Invite_request_error _) -> ()
  | Error (Driver.Invite_share_error _) ->
      Alcotest.fail "invite error was reported as a share error"
  | Ok _ -> Alcotest.fail "forbidden invite unexpectedly succeeded");
  check_bool "invite failure does not repeat request" true
    (List.length (requests failing_log) = 1);

  (* With keys available, all history-share requests precede /invite. *)
  let alice, bob = invite_history_fixture "invite-order" in
  let recipient_query = query_response_for bob in
  let log, fetch =
    mock (fun request ->
        let url = Fetch.Middleware.Url.to_string request.url in
        if contains "/room_keys/keys/" url then
          Fetch_mock.respond {|{"sessions":{}}|} request
        else if contains "/_matrix/media/v3/upload" url then
          Fetch_mock.respond
            {|{"content_uri":"mxc://hs.example/invite-history"}|} request
        else if contains "/keys/query" url then
          Fetch_mock.respond (query_response_body recipient_query) request
        else if contains "/keys/claim" url then Fetch_mock.respond "{}" request
        else if contains "/sendToDevice/" url then
          Fetch_mock.respond "{}" request
        else if contains "/invite" url then Fetch_mock.respond "{}" request
        else Alcotest.failf "unexpected invite-order request: %s" url)
  in
  (match
     Driver.invite_user_by_id (drive alice) (client_of fetch) ~room_id:room
       ~user_id:bob_id ~history_visibility:Ev.History_visibility.Shared ()
   with
  | Ok (Driver.Invite_sent (Driver.History_shared count)) ->
      check_int "one device receives shared history" 1 count
  | Ok _ -> Alcotest.fail "history was unexpectedly skipped"
  | Error (Driver.Invite_share_error _) ->
      Alcotest.fail "history share failed before invite"
  | Error (Driver.Invite_request_error error) ->
      Alcotest.failf "ordinary invite failed: %s" (Error.to_string error));
  let urls = List.map (fun request -> request.url) (requests log) in
  let position needle =
    match List.find_index (contains needle) urls with
    | Some index -> index
    | None -> Alcotest.failf "request %s was not sent" needle
  in
  check_bool "share precedes invite" true
    (position "/sendToDevice/" < position "/invite");

  (* A real media failure is typed as a share failure and prevents /invite. *)
  let alice, _bob = invite_history_fixture "invite-media-failure" in
  let failure_log, failure_fetch =
    mock (fun request ->
        let url = Fetch.Middleware.Url.to_string request.url in
        if contains "/room_keys/keys/" url then
          Fetch_mock.respond {|{"sessions":{}}|} request
        else if contains "/_matrix/media/v3/upload" url then
          Fetch_mock.respond ~status:500
            {|{"errcode":"M_UNKNOWN","error":"upload failed"}|} request
        else Alcotest.failf "unexpected share failure request: %s" url)
  in
  (match
     Driver.invite_user_by_id (drive alice) (client_of failure_fetch)
       ~room_id:room ~user_id:bob_id
       ~history_visibility:Ev.History_visibility.Shared ()
   with
  | Error (Driver.Invite_share_error (Driver.Share_media_error _)) -> ()
  | Error (Driver.Invite_request_error _) ->
      Alcotest.fail "media failure was reported as an invite error"
  | Error (Driver.Invite_share_error _) ->
      Alcotest.fail "wrong sharing error was reported"
  | Ok _ -> Alcotest.fail "media failure unexpectedly sent invite");
  check_bool "share failure prevents invite" true
    (not
       (List.exists
          (fun request -> contains "/invite" request.url)
          (requests failure_log)));

  (* Visibility is another successful no-op once identity is present. *)
  let alice, _bob = invite_history_fixture "invite-visibility" in
  let visibility_log, visibility_fetch =
    mock (fun request ->
        let url = Fetch.Middleware.Url.to_string request.url in
        if contains "/invite" url then Fetch_mock.respond "{}" request
        else Alcotest.failf "unexpected visibility request: %s" url)
  in
  (match
     Driver.invite_user_by_id (drive alice)
       (client_of visibility_fetch)
       ~room_id:room ~user_id:bob_id
       ~history_visibility:Ev.History_visibility.Joined ()
   with
  | Ok (Driver.Invite_sent Driver.History_not_shared_visibility) -> ()
  | Ok _ -> Alcotest.fail "joined visibility was not a sharing no-op"
  | Error _ -> Alcotest.fail "visibility no-op prevented invite");
  check_bool "visibility no-op sends invite" true
    (List.length (requests visibility_log) = 1)

let test_portable_room_key_export_import () =
  let log, client = backup_client () in
  let alice =
    make ~seed:"portable-alice" ~user:alice_id ~device:(did "ALICEDEV") ()
  in
  let bob = make ~seed:"portable-bob" ~user:bob_id ~device:(did "BOBDEV") () in
  introduce ~from:bob ~into:alice;
  introduce ~from:alice ~into:bob;
  ignore (Encryption.receive_keys_claim alice (claim_response_for bob));
  let encrypted, outcome =
    share_and_encrypt alice bob client log ~body:"portable"
  in
  check_int "portable source received one room key" 1
    (List.length outcome.new_sessions);
  let exported = Encryption.export_room_keys bob in
  check_int "one inbound session is exported" 1 (List.length exported);
  check_int "export predicate filters sessions" 0
    (List.length (Encryption.export_room_keys ~predicate:(Fun.const false) bob));
  let key = List.hd exported in
  check_string "export uses Megolm" "m.megolm.v1.aes-sha2" key.algorithm;
  check_bool "export defaults shared history off" false key.shared_history;
  let withheld_bundle = Encryption.build_room_key_bundle bob ~room_id:room in
  check_int "non-shared session is withheld from history bundle" 0
    (List.length withheld_bundle.room_keys);
  check_int "history withholding is bundled" 1
    (List.length withheld_bundle.withheld);
  check_string "history withholding uses Rust reason"
    "The sender disabled sharing encrypted history."
    (Option.get (List.hd withheld_bundle.withheld).reason);
  let later_key =
    let source_session = List.hd (Encryption.snapshot bob).megolm_inbound in
    let session_key =
      match Olm.Megolm.Inbound.export_at source_session ~index:1 with
      | Ok value -> value
      | Error error ->
          Alcotest.failf "could not build later export: %a" Olm.pp_error error
    in
    { key with session_key }
  in
  let carol =
    make ~seed:"portable-carol" ~user:bob_id ~device:(did "CAROL") ()
  in
  let import_result =
    Encryption.import_room_keys carol
      [
        { key with algorithm = "m.megolm.unsupported" };
        { key with sender_claimed_keys = [ ("ed25519", "bad") ] };
        later_key;
        key;
        { key with session_id = Id.Session_id.of_string_exn "wrong" };
      ]
  in
  check_int "malformed and unsupported entries are skipped" 2
    import_result.imported_count;
  check_int "all supplied entries are counted" 5 import_result.total_count;
  check_bool "imported session is held" true
    (Encryption.has_inbound_session carol room ~session_id:key.session_id);
  introduce ~from:alice ~into:carol;
  (match
     Encryption.decrypt_room_event carol room
       (encrypted_raw ~sender:alice_id ~content:encrypted)
   with
  | Ok d ->
      check_string "portable export decrypts"
        {|{"msgtype":"m.text","body":"portable"}|}
        (json_to_string d.decrypted_content)
  | Error e ->
      Alcotest.failf "portable export did not decrypt: %a"
        Encryption.pp_decrypt_error e);
  let shared =
    let _, forwarder =
      Ck.Curve25519.generate ~random:(random_of "portable-forwarder") ()
    in
    let forwarder = Ck.Curve25519.Public.to_base64 forwarder in
    {
      key with
      shared_history = true;
      (* Accept a padded spelling at the import boundary, but retain the
         canonical unpadded form in session metadata. *)
      forwarding_curve25519_key_chain = [ forwarder ^ "=" ];
    }
  in
  check_int "equal duplicate cannot upgrade shared-history permission" 0
    (Encryption.import_room_keys carol [ shared ]).imported_count;
  check_bool "existing shared-history permission remains false" true
    (match (Encryption.snapshot carol).state.session_meta with
    | [ metadata ] -> not metadata.shared_history
    | _ -> false);
  let imported_shared =
    make ~seed:"portable-shared" ~user:bob_id ~device:(did "SHARED") ()
  in
  check_int "shared-history entry imports" 1
    (Encryption.import_room_keys imported_shared [ shared ]).imported_count;
  check_bool "shared-history metadata survives import" true
    (match (Encryption.snapshot imported_shared).state.session_meta with
    | [ metadata ] -> metadata.shared_history
    | _ -> false);
  let canonical_forwarder =
    match (Encryption.snapshot imported_shared).state.session_meta with
    | [ metadata ] -> List.hd metadata.forwarding_chain
    | _ -> Alcotest.fail "expected one imported session metadata"
  in
  check_bool "forwarding chain is canonicalized" true
    (not (String.ends_with ~suffix:"=" canonical_forwarder));
  let invalid_chain_machine =
    make ~seed:"portable-invalid-forwarding-chain" ~user:bob_id
      ~device:(did "INVALIDCHAIN") ()
  in
  check_int "invalid forwarding chain is rejected" 0
    (Encryption.import_room_keys invalid_chain_machine
       [ { key with forwarding_curve25519_key_chain = [ "not-a-key" ] } ])
      .imported_count;
  let oversized_chain_machine =
    make ~seed:"portable-oversized-forwarding-chain" ~user:bob_id
      ~device:(did "OVERSIZEDCHAIN") ()
  in
  check_int "oversized forwarding chain is rejected" 0
    (Encryption.import_room_keys oversized_chain_machine
       [
         {
           key with
           forwarding_curve25519_key_chain =
             List.init 101 (Fun.const canonical_forwarder);
         };
       ])
      .imported_count;
  let tainted_snapshot = Encryption.snapshot imported_shared in
  let tainted_snapshot =
    {
      tainted_snapshot with
      state =
        {
          tainted_snapshot.state with
          session_meta =
            List.map
              (fun (metadata : Encryption.Session_meta.t) ->
                { metadata with forwarding_chain = [ "not-a-key" ] })
              tainted_snapshot.state.session_meta;
        };
    }
  in
  let sanitized =
    Encryption.of_snapshot
      ~random:(random_of "portable-sanitize-legacy-metadata")
      ~user_id:bob_id ~device_id:(did "SHARED") tainted_snapshot
  in
  (match Encryption.export_room_keys sanitized with
  | [ restored ] ->
      check_bool "invalid persisted forwarding metadata is not re-emitted" true
        (restored.forwarding_curve25519_key_chain = [])
  | keys ->
      Alcotest.failf "expected one sanitized portable key, got %d"
        (List.length keys));
  with_store @@ fun store ->
  ok_value (Crypto_store.save store (Encryption.snapshot imported_shared));
  let stored =
    match ok_value (Crypto_store.load store) with
    | Some snapshot -> snapshot
    | None -> Alcotest.fail "portable key snapshot was not stored"
  in
  let restarted =
    Encryption.of_snapshot
      ~random:(random_of "portable-restart")
      ~user_id:bob_id ~device_id:(did "SHARED") stored
  in
  (match Encryption.export_room_keys restarted with
  | [ restored ] ->
      check_bool "shared-history survives store restart" true
        restored.shared_history;
      Alcotest.(check (list string))
        "forwarding chain survives store restart" [ canonical_forwarder ]
        restored.forwarding_curve25519_key_chain
  | keys ->
      Alcotest.failf "expected one portable key after restart, got %d"
        (List.length keys));
  let bundle = Encryption.build_room_key_bundle imported_shared ~room_id:room in
  let bundle_for_codec = { bundle with withheld = withheld_bundle.withheld } in
  let bundle_json =
    match
      Jsont.Json.encode Room_key_export.room_key_bundle_jsont bundle_for_codec
    with
    | Ok json -> json
    | Error error -> Alcotest.failf "could not encode room-key bundle: %s" error
  in
  let bundle_roundtrip =
    match
      Jsont.Json.decode Room_key_export.room_key_bundle_jsont bundle_json
    with
    | Ok value -> value
    | Error error -> Alcotest.failf "could not decode room-key bundle: %s" error
  in
  let bundle_wire = json_to_string bundle_json in
  check_bool "historic bundle omits portable forwarding metadata" false
    (contains "forwarding_curve25519_key_chain" bundle_wire);
  check_bool "historic bundle omits shared-history flag" false
    (contains "shared_history" bundle_wire);
  check_int "room-key bundle codec preserves room keys" 1
    (List.length bundle_roundtrip.room_keys);
  check_int "room-key bundle codec preserves withheld entries" 1
    (List.length bundle_roundtrip.withheld);
  check_int "shared session is included in room-key bundle" 1
    (List.length bundle.room_keys);
  check_int "shared session is not withheld" 0 (List.length bundle.withheld);
  let bundle_target =
    make ~seed:"portable-bundle-target" ~user:bob_id ~device:(did "TARGET") ()
  in
  check_int "bundle import records one session" 1
    (Encryption.import_room_key_bundle bundle_target ~room_id:room
       ~sender:alice_id bundle);
  check_bool "bundle records forwarding provenance" true
    (match (Encryption.snapshot bundle_target).state.session_meta with
    | [ metadata ] -> metadata.sender = Some alice_id
    | _ -> false);
  check_int "bundle-imported session is shareable" 1
    (List.length
       (Encryption.build_room_key_bundle bundle_target ~room_id:room).room_keys)

let test_portable_export_fixture () =
  let fixture =
    if Sys.file_exists "test/fixtures/portable-room-keys.ocaml.megolm" then
      "test/fixtures/portable-room-keys.ocaml.megolm"
    else "fixtures/portable-room-keys.ocaml.megolm"
  in
  let armor = In_channel.with_open_bin fixture In_channel.input_all in
  match
    Room_key_export.decrypt ~passphrase:"ocaml-rust-portable-export" armor
  with
  | Error error ->
      Alcotest.failf "OCaml could not decrypt its portable fixture: %a"
        Room_key_export.pp_error error
  | Ok [ key ] ->
      check_string "fixture room" "!interop:example.org"
        (Id.Room_id.to_string key.room_id);
      check_string "fixture sender key"
        "BwcHBwcHBwcHBwcHBwcHBwcHBwcHBwcHBwcHBwcHBwc" key.sender_key;
      check_bool "fixture shared history" true key.shared_history;
      check_int "fixture forwarding chain" 2
        (List.length key.forwarding_curve25519_key_chain)
  | Ok keys ->
      Alcotest.failf "expected one fixture key, got %d" (List.length keys)

let test_forwarded_room_key_validates_identity_and_canonicalizes_sender () =
  let log, client = permissive_client () in
  let alice =
    make ~seed:"forwarded-validation-alice" ~user:alice_id
      ~device:(did "ALICEDEV") ()
  in
  let bob =
    make ~seed:"forwarded-validation-bob" ~user:bob_id ~device:(did "BOBDEV") ()
  in
  introduce ~from:bob ~into:alice;
  introduce ~from:alice ~into:bob;
  ignore (Encryption.receive_keys_claim alice (claim_response_for bob));
  ignore (share_and_encrypt alice bob client log ~body:"forwarded");
  let key =
    match Encryption.export_room_keys bob with
    | [ key ] -> key
    | keys ->
        Alcotest.failf "expected one exported room key, got %d"
          (List.length keys)
  in
  let forwarded_content ~session_id ~sender_key =
    let content : Ev.Forwarded_room_key_content.t =
      {
        room_id = key.room_id;
        sender_key;
        session_id;
        session_key = key.session_key;
        sender_claimed_ed25519_key = "";
        forwarding_curve25519_key_chain = [];
      }
    in
    match Jsont.Json.encode Ev.Forwarded_room_key_content.jsont content with
    | Ok content -> content
    | Error error -> Alcotest.failf "could not encode forwarded key: %s" error
  in
  let padded_sender_key = key.sender_key ^ "=" in
  let canonical_target =
    make ~seed:"forwarded-validation-canonical" ~user:bob_id
      ~device:(did "CANONICAL") ()
  in
  let canonical_event =
    to_device_event
      ~sender:(Id.User_id.to_string alice_id)
      ~event_type:"m.forwarded_room_key"
      ~content:
        (forwarded_content ~session_id:key.session_id
           ~sender_key:padded_sender_key)
  in
  let canonical_outcome =
    Encryption.process_sync canonical_target
      (sync_of ~to_device:[ canonical_event ] ())
  in
  (match canonical_outcome.events with
  | [ Encryption.Forwarded_room_key { sender_key; _ } ] ->
      check_string "forwarded sender key is canonical" key.sender_key sender_key
  | _ -> Alcotest.fail "expected a canonicalized forwarded room-key outcome");
  check_bool "canonical forwarded room key is stored" true
    (Encryption.has_inbound_session canonical_target key.room_id
       ~session_id:key.session_id);
  let mismatch_target =
    make ~seed:"forwarded-validation-mismatch" ~user:bob_id
      ~device:(did "MISMATCH") ()
  in
  let wrong_session_id = Id.Session_id.of_string_exn (String.make 43 'A') in
  let mismatch_event =
    to_device_event
      ~sender:(Id.User_id.to_string alice_id)
      ~event_type:"m.forwarded_room_key"
      ~content:
        (forwarded_content ~session_id:wrong_session_id
           ~sender_key:padded_sender_key)
  in
  let mismatch_outcome =
    Encryption.process_sync mismatch_target
      (sync_of ~to_device:[ mismatch_event ] ())
  in
  (match mismatch_outcome.events with
  | [ Encryption.Undecryptable { reason; _ } ] ->
      check_string "forwarded session-id mismatch is rejected"
        "forwarded room key session ID mismatch" reason
  | _ -> Alcotest.fail "expected mismatched forwarded room key to be rejected");
  check_bool "mismatched forwarded room key is not stored" false
    (Encryption.has_inbound_session mismatch_target key.room_id
       ~session_id:key.session_id)

let test_stale_backup_upload_does_not_ack_new_version () =
  let alice =
    make ~seed:"stale-backup-upload" ~user:alice_id ~device:(did "ALICEDEV") ()
  in
  ok_value
    (Encryption.set_room_encryption_settings alice room
       (Encryption.room_encryption_content
          (Encryption.enable_room_encryption ~rotation_period_msgs:1 ())));
  let key_v1 = Backup.Decryption_key.generate ~random:(random_of "stale-v1") in
  Encryption.enable_backup alice ~version:"v1"
    (Backup.Decryption_key.public key_v1);
  ignore
    (ok_value
       (Encryption.encrypt_room_event alice room ~event_type:"m.room.message"
          ~content:(jobj [ ("body", jstr "stale") ])
          ~members:[]));
  let old_request =
    match ok_value (Encryption.pending_backup alice) with
    | Some request -> request
    | None -> Alcotest.fail "expected a pending v1 backup upload"
  in
  let key_v2 = Backup.Decryption_key.generate ~random:(random_of "stale-v2") in
  Encryption.enable_backup alice ~version:"v2"
    (Backup.Decryption_key.public key_v2);
  Encryption.mark_sent alice old_request;
  check_int "stale v1 response does not acknowledge v2" 1
    (Encryption.backup_pending_count alice);
  let current_request =
    match ok_value (Encryption.pending_backup alice) with
    | Some request -> request
    | None -> Alcotest.fail "expected a pending v2 backup upload"
  in
  Encryption.mark_sent alice current_request;
  check_int "matching v2 response acknowledges the batch" 0
    (Encryption.backup_pending_count alice)

(* A backup request is deliberately bounded, so a large local keyring does not
   produce an oversized PUT.  This also exercises the driver's durable
   checkpoint between requests: after a later request fails, the first batch
   must be absent after reopening the crypto store. *)
let backup_batch_machine () =
  let alice =
    make ~seed:"alice-backup-batches" ~user:alice_id ~device:(did "ALICEDEV") ()
  in
  ok_value
    (Encryption.set_room_encryption_settings alice room
       (Encryption.room_encryption_content
          (Encryption.enable_room_encryption ~rotation_period_msgs:1 ())));
  let backup_key =
    Backup.Decryption_key.generate ~random:(random_of "backup-batches-key")
  in
  Encryption.enable_backup alice ~version:"v1"
    (Backup.Decryption_key.public backup_key);
  for _ = 1 to 205 do
    ignore
      (ok_value
         (Encryption.encrypt_room_event alice room ~event_type:"m.room.message"
            ~content:(jobj [ ("body", jstr "batch") ])
            ~members:[]))
  done;
  alice

let session_entries body =
  let needle = "\"session_data\":" in
  let n = String.length needle in
  let rec count i total =
    if i + n > String.length body then total
    else if String.sub body i n = needle then count (i + n) (total + 1)
    else count (i + 1) total
  in
  count 0 0

let backup_batch_client ?fail_on_request () =
  let attempts = ref 0 in
  let handler (req : Fetch.Middleware.request) =
    let url = Fetch.Middleware.Url.to_string req.url in
    if contains "/room_keys/keys" url && Http.Method.to_string req.meth = "PUT"
    then begin
      incr attempts;
      match fail_on_request with
      | Some n when !attempts = n ->
          Fetch_mock.respond ~status:500
            {|{"errcode":"M_UNKNOWN","error":"backup failed"}|} req
      | _ -> Fetch_mock.respond {|{"etag":"batch","count":1}|} req
    end
    else permissive_handler req
  in
  let log, fetch = mock handler in
  (log, client_of fetch, attempts)

let test_backup_batches () =
  with_store @@ fun store ->
  let alice = backup_batch_machine () in
  let log, client, attempts = backup_batch_client () in
  let driver = drive ~store alice in
  (* Persist the unmodified keyring and backup configuration before uploading;
     each successful request below then advances this same durable snapshot. *)
  ok_value (Driver.save driver);
  check_int "three bounded uploads contain all sessions" 205
    (ok_value (Driver.backup_pending driver client));
  let batches =
    List.filter_map
      (fun r ->
        if r.meth = "PUT" && contains "/room_keys/keys" r.url then
          Some (session_entries (Option.get r.body))
        else None)
      (requests log)
  in
  check_int "three batches were sent" 3 (List.length batches);
  List.iter
    (fun n ->
      check_bool "every batch has at most one hundred sessions" true (n <= 100))
    batches;
  check_int "all 205 sessions were uploaded" 205
    (List.fold_left ( + ) 0 batches);
  ignore attempts

let test_backup_batch_failure () =
  with_store @@ fun store ->
  let alice = backup_batch_machine () in
  let log, client, attempts = backup_batch_client ~fail_on_request:2 () in
  let driver = drive ~store alice in
  ok_value (Driver.save driver);
  (match Driver.backup_pending driver client with
  | Ok n -> Alcotest.failf "backup unexpectedly completed with %d keys" n
  | Error _ -> ());
  check_int "the failed call attempted its second batch" 2 !attempts;
  check_int "only the first batch was marked pending-free" 105
    (Encryption.backup_pending_count alice);
  let restarted =
    ok_value
      (Driver.create
         ~random:(random_of "alice-backup-batches-restart")
         ~user_id:alice_id ~device_id:(did "ALICEDEV") ~store ())
  in
  check_int "the first batch was persisted before failure" 105
    (Encryption.backup_pending_count (Driver.machine restarted));
  let _log2, client2, attempts2 = backup_batch_client () in
  check_int "the retry uploads the remaining 105 keys" 105
    (ok_value (Driver.backup_pending restarted client2));
  check_int "the retry uses two bounded uploads" 2 !attempts2;
  check_int "no sessions remain after retry" 0
    (Encryption.backup_pending_count (Driver.machine restarted));
  ignore log

let test_backup_batches_are_serialized () =
  with_store @@ fun store ->
  let alice = backup_batch_machine () in
  let first_entered, first_entered_r = Eio.Promise.create () in
  let release_first, release_first_r = Eio.Promise.create () in
  let attempts = ref 0 in
  let _log, fetch =
    mock (fun req ->
        let url = Fetch.Middleware.Url.to_string req.url in
        if
          contains "/room_keys/keys" url
          && Http.Method.to_string req.meth = "PUT"
        then begin
          incr attempts;
          if !attempts = 1 then begin
            Eio.Promise.resolve first_entered_r ();
            Eio.Promise.await release_first
          end;
          Fetch_mock.respond {|{"etag":"batch","count":1}|} req
        end
        else permissive_handler req)
  in
  let driver = drive ~store alice in
  ok_value (Driver.save driver);
  Eio.Switch.run @@ fun sw ->
  let first_done, first_done_r = Eio.Promise.create () in
  let second_done, second_done_r = Eio.Promise.create () in
  Eio.Fiber.fork ~sw (fun () ->
      Eio.Promise.resolve first_done_r
        (Driver.backup_pending driver (client_of fetch)));
  Eio.Promise.await first_entered;
  Eio.Fiber.fork ~sw (fun () ->
      Eio.Promise.resolve second_done_r
        (Driver.backup_pending driver (client_of fetch)));
  Eio.Fiber.yield ();
  check_int "the concurrent caller cannot duplicate the first batch" 1 !attempts;
  Eio.Promise.resolve release_first_r ();
  let first_count = ok_value (Eio.Promise.await first_done) in
  let second_count = ok_value (Eio.Promise.await second_done) in
  check_int "the serialized calls upload every session once" 205
    (first_count + second_count);
  check_int "only three requests reached the server" 3 !attempts

let test_backup_version_loss_disables_uploads () =
  let cases =
    [
      ( "wrong version",
        400,
        {|{"errcode":"M_WRONG_ROOM_KEYS_VERSION","error":"rotated","current_version":"v2"}|}
      );
      ("missing version", 404, {|{"errcode":"M_NOT_FOUND","error":"deleted"}|});
      ("bare missing version", 404, "not json");
    ]
  in
  List.iter
    (fun (label, status, body) ->
      with_store @@ fun store ->
      let machine = backup_batch_machine () in
      let driver = drive ~store machine in
      ok_value (Driver.save driver);
      let _log, fetch =
        mock (fun req ->
            let url = Fetch.Middleware.Url.to_string req.url in
            if
              contains "/room_keys/keys" url
              && Http.Method.to_string req.meth = "PUT"
            then Fetch_mock.respond ~status body req
            else permissive_handler req)
      in
      (match Driver.backup_pending driver (client_of fetch) with
      | Ok count ->
          Alcotest.failf "%s unexpectedly uploaded %d sessions" label count
      | Error
          (Error.Matrix_error { errcode = Error.M_WRONG_ROOM_KEYS_VERSION; _ })
        when String.equal label "wrong version" ->
          ()
      | Error _ when not (String.equal label "wrong version") -> ()
      | Error error ->
          Alcotest.failf "%s returned the wrong error kind: %s" label
            (Error.to_string error));
      check_bool
        (label ^ " disables the active version")
        true
        (Encryption.backup_version machine = None);
      let restarted =
        ok_value
          (Driver.create
             ~random:(random_of ("backup-loss-" ^ label))
             ~user_id:alice_id ~device_id:(did "ALICEDEV") ~store ())
      in
      check_bool
        (label ^ " persists the disabled state")
        true
        (Encryption.backup_version (Driver.machine restarted) = None))
    cases

let test_store_round_trip () =
  with_store @@ fun store ->
  let log, client = permissive_client () in
  let alice_d =
    ok_value
      (Driver.create ~random:(random_of "alice-store") ~user_id:alice_id
         ~device_id:(did "ALICEDEV") ~store ())
  in
  let alice = Driver.machine alice_d in
  let initial_otk_upload =
    match Encryption.outgoing_requests alice with
    | request :: _ -> request
    | [] -> Alcotest.fail "new stored machine has no key upload"
  in
  Encryption.mark_sent alice initial_otk_upload;
  Encryption.receive_keys_upload alice
    { Keys.one_time_key_counts = [ ("signed_curve25519", 50) ] };
  let account = (Encryption.snapshot alice).account in
  Olm.Account.generate_fallback_key
    ~random:(random_of "fallback-current")
    account;
  Olm.Account.generate_fallback_key ~random:(random_of "fallback-next") account;
  let bob = make ~seed:"bob-store" ~user:bob_id ~device:(did "BOBDEV") () in
  introduce ~from:bob ~into:alice;
  introduce ~from:alice ~into:alice;
  ignore (Encryption.receive_keys_claim alice (claim_response_for bob));
  ok_value
    (Encryption.set_room_encryption_settings alice room
       (Encryption.room_encryption_content
          (Encryption.enable_room_encryption ~rotation_period_msgs:42 ())));
  let content = jobj [ ("body", jstr "persisted") ] in
  let encrypted =
    ok_value
      (Driver.encrypt_room_event alice_d client room
         ~event_type:"m.room.message" ~content ~members:[ alice_id; bob_id ])
  in
  Encryption.set_device_trust alice bob_id ~device_id:(did "BOBDEV")
    Encryption.Verified;
  Encryption.set_trust_requirement alice Encryption.Cross_signed_or_legacy;
  ok_value (Driver.save alice_d);
  ignore (requests log);
  (* Reopen from disk with a different randomness source: nothing that
     matters may be regenerated. *)
  let alice' =
    Driver.machine
      (ok_value
         (Driver.create
            ~random:(random_of "alice-store-reload")
            ~user_id:alice_id ~device_id:(did "ALICEDEV") ~store ()))
  in
  check_int "the uploaded OTK count survives disk restart" 50
    (Encryption.snapshot alice').state.uploaded_one_time_key_count;
  check_bool "restart does not generate a redundant OTK batch" true
    (not
       (List.exists
          (function Encryption.Keys_upload _ -> true | _ -> false)
          (Encryption.outgoing_requests alice')));
  check_string "the identity keys survive"
    (Ck.Ed25519.Public.to_base64 (fst (Encryption.identity_keys alice)))
    (Ck.Ed25519.Public.to_base64 (fst (Encryption.identity_keys alice')));
  check_string "and so does the Curve25519 key"
    (Ck.Curve25519.Public.to_base64 (snd (Encryption.identity_keys alice)))
    (Ck.Curve25519.Public.to_base64 (snd (Encryption.identity_keys alice')));
  let olm_before = List.hd (Encryption.snapshot alice).olm_sessions in
  let olm_after =
    List.find
      (fun s ->
        String.equal (Olm.Session.session_id s)
          (Olm.Session.session_id olm_before))
      (Encryption.snapshot alice').olm_sessions
  in
  check_bool "Olm last-use survives disk restart" true
    (Ptime.equal
       (Olm.Session.last_used_at olm_before)
       (Olm.Session.last_used_at olm_after));
  check_bool "Olm last-receive survives disk restart" true
    (Ptime.equal
       (Olm.Session.last_received_at olm_before)
       (Olm.Session.last_received_at olm_after));
  check_int "the room settings survive" 42
    (Option.get (Encryption.find_room_settings alice' room))
      .rotation_period_msgs;
  check_bool "Bob is still verified" true
    (match Encryption.find_device alice' bob_id ~device_id:(did "BOBDEV") with
    | Some d -> d.trust = Encryption.Verified
    | None -> false);
  check_bool "the trust requirement survives" true
    (Encryption.trust_requirement alice' = Encryption.Cross_signed_or_legacy);
  check_bool "the previous fallback key survives" true
    (Olm.Account.forget_previous_fallback_key
       (Encryption.snapshot alice').account);
  Encryption.set_trust_requirement alice' Encryption.Untrusted;
  check_string "the outbound session survives"
    (Id.Session_id.to_string
       (Option.get (Encryption.outbound_session_id alice room)))
    (Id.Session_id.to_string
       (Option.get (Encryption.outbound_session_id alice' room)));
  let raw = encrypted_raw ~sender:alice_id ~content:encrypted in
  match Encryption.decrypt_room_event alice' room raw with
  | Error e ->
      Alcotest.failf "a reloaded machine cannot read its own history: %a"
        Encryption.pp_decrypt_error e
  | Ok d ->
      check_string "the plaintext is intact" {|{"body":"persisted"}|}
        (json_to_string d.decrypted_content);
      Encryption.set_device_trust alice' bob_id ~device_id:(did "BOBDEV")
        Encryption.Blacklisted;
      let alice_d' = drive ~store alice' in
      let _, pending =
        ok_value
          (Encryption.encrypt_room_event alice' room
             ~event_type:"m.room.message" ~content ~members:[ alice_id; bob_id ])
      in
      let pending_txn, _, _, pending_content = one_withheld_request pending in
      ok_value (Driver.save alice_d');
      let alice'' =
        Driver.machine
          (ok_value
             (Driver.create
                ~random:(random_of "alice-store-reload-pending")
                ~user_id:alice_id ~device_id:(did "ALICEDEV") ~store ()))
      in
      let _, pending_after_restart =
        ok_value
          (Encryption.encrypt_room_event alice'' room
             ~event_type:"m.room.message" ~content ~members:[ alice_id; bob_id ])
      in
      let restarted_txn, _, _, restarted_content =
        one_withheld_request pending_after_restart
      in
      check_string "pending notice survives store restart" pending_txn
        restarted_txn;
      check_string "pending notice body survives store restart"
        (json_to_string pending_content)
        (json_to_string restarted_content);
      Encryption.mark_sent alice'' (List.hd pending_after_restart);
      ok_value (Driver.save (drive ~store alice''));
      let alice''' =
        Driver.machine
          (ok_value
             (Driver.create
                ~random:(random_of "alice-store-reload-sent")
                ~user_id:alice_id ~device_id:(did "ALICEDEV") ~store ()))
      in
      let _, after_sent_restart =
        ok_value
          (Encryption.encrypt_room_event alice''' room
             ~event_type:"m.room.message" ~content ~members:[ alice_id; bob_id ])
      in
      check_bool "sent notice stays suppressed after store restart" true
        (not
           (List.exists
              (function Encryption.To_device _ -> true | _ -> false)
              after_sent_restart))

let test_crypto_store_stale_generation () =
  Eio_main.run @@ fun env ->
  let base = Filename.temp_file "matrix-crypto-generation-" "" in
  Sys.remove base;
  Unix.mkdir base 0o700;
  Unix.putenv "XDG_DATA_HOME" (Filename.concat base "data");
  let fs = Eio.Stdenv.fs env in
  let xdg = Xdge.create fs "matrix-generation" in
  let first = Crypto_store.create ~xdg ~profile:"shared" in
  let stale = Crypto_store.create ~xdg ~profile:"shared" in
  let first_driver =
    ok_value
      (Driver.create
         ~random:(random_of "generation-first")
         ~user_id:alice_id ~device_id:(did "ALICEDEV") ~store:first ())
  in
  let stale_driver =
    ok_value
      (Driver.create
         ~random:(random_of "generation-stale")
         ~user_id:alice_id ~device_id:(did "ALICEDEV") ~store:stale ())
  in
  ok_value (Driver.save first_driver);
  (match Driver.save stale_driver with
  | Error (Error.Policy_denied _) -> ()
  | Ok () -> Alcotest.fail "stale crypto save unexpectedly succeeded"
  | Error error ->
      Alcotest.failf "wrong stale crypto error: %s" (Error.to_string error));
  let marker =
    Eio.Path.(Xdge.data_dir xdg / "profiles" / "shared" / ".crypto_generation")
  in
  Eio.Path.save ~create:(`Or_truncate 0o600) marker "3\n";
  let interrupted = Crypto_store.create ~xdg ~profile:"shared" in
  (match Crypto_store.load interrupted with
  | Error (Error.Policy_denied _) -> ()
  | Ok _ -> Alcotest.fail "an interrupted crypto snapshot loaded"
  | Error error ->
      Alcotest.failf "wrong interrupted-snapshot error: %s"
        (Error.to_string error));
  ok_value
    (Crypto_store.save interrupted
       (Encryption.snapshot (Driver.machine first_driver)));
  let current = Crypto_store.create ~xdg ~profile:"shared" in
  let before_clear = Crypto_store.create ~xdg ~profile:"shared" in
  let before_clear_driver =
    ok_value
      (Driver.create
         ~random:(random_of "generation-before-clear")
         ~user_id:alice_id ~device_id:(did "ALICEDEV") ~store:before_clear ())
  in
  ok_value (Crypto_store.clear current);
  (match Driver.save before_clear_driver with
  | Error (Error.Policy_denied _) -> ()
  | Ok () -> Alcotest.fail "pre-clear crypto handle resurrected state"
  | Error error ->
      Alcotest.failf "wrong pre-clear crypto error: %s" (Error.to_string error));
  match Crypto_store.load before_clear with
  | Ok None -> ()
  | Ok (Some _) -> Alcotest.fail "cleared crypto state was restored"
  | Error error -> Alcotest.failf "load after clear: %s" (Error.to_string error)

let () =
  Alcotest.run "encryption"
    [
      ( "two machines",
        [
          Alcotest.test_case "room key share and decrypt" `Quick
            (run test_round_trip);
          Alcotest.test_case "blacklisted device gets withheld" `Quick
            (run test_blacklisted_withheld);
          Alcotest.test_case "blacklisting rotates a shared session" `Quick
            (run test_blacklisting_shared_device_rotates_session);
          Alcotest.test_case "missing Olm session gets withheld" `Quick
            (run test_no_olm_withheld);
          Alcotest.test_case "trust requirements" `Quick
            (run test_trust_requirements);
          Alcotest.test_case "megolm session rotation" `Quick
            (run test_rotation);
        ] );
      ( "decryption",
        [
          Alcotest.test_case "unknown session and key request" `Quick
            (run test_unknown_session);
          Alcotest.test_case "UTD cause classifier" `Quick
            (run test_utd_cause_classifier);
          Alcotest.test_case "Olm unwedge cutoff and clock rollback" `Quick
            (run test_olm_unwedge_cutoff_and_clock_rollback);
          Alcotest.test_case "Olm unwedge retry and dummy" `Quick
            (run test_olm_unwedge_claim_retry_dummy_and_mark_sent);
          Alcotest.test_case "Olm claim exhaustion backoff" `Quick
            (run test_olm_claim_exhaustion_backoff);
          Alcotest.test_case "Olm claim server and unrelated response" `Quick
            (run test_olm_claim_exhaustion_server_and_unrelated_response);
          Alcotest.test_case "forced unwedge bypasses claim backoff" `Quick
            (run test_olm_claim_exhaustion_forced_unwedge_bypasses_backoff);
        ] );
      ( "gossiping",
        [
          Alcotest.test_case "refused for another user" `Quick
            (run test_gossip_refused_for_other_user);
          Alcotest.test_case "allowed for our own verified device" `Quick
            (run test_gossip_allowed_for_own_verified_device);
          Alcotest.test_case "secret events require Olm" `Quick
            (run test_secret_events_require_olm);
          Alcotest.test_case "trusted secret request round trip" `Quick
            (run test_secret_request_round_trip_and_policy);
          Alcotest.test_case "secret request replacement and retry" `Quick
            (run test_secret_request_replacement_and_retry);
          Alcotest.test_case "secret request trust and cancellation" `Quick
            (run test_secret_request_unverified_and_cancellation);
          Alcotest.test_case "secret request cancellation after send" `Quick
            (run test_secret_request_cancel_after_send);
          Alcotest.test_case "withheld sender provenance from direct events"
            `Quick
            (run test_withheld_provenance_direct);
          Alcotest.test_case "withheld sender provenance from bundles" `Quick
            (run test_withheld_provenance_bundle);
          Alcotest.test_case "withheld provenance duplicate replacement" `Quick
            (run test_withheld_provenance_duplicate);
          Alcotest.test_case "withheld provenance stays off bundle wire" `Quick
            (run test_withheld_provenance_wire);
          Alcotest.test_case "withheld clears on replacement key" `Quick
            (run test_withheld_provenance_clear_on_key);
          Alcotest.test_case "invalid withheld entries are ignored" `Quick
            (run test_withheld_provenance_invalid_entries);
          Alcotest.test_case "withheld provenance persists" `Quick
            (run test_withheld_provenance_persistence);
          Alcotest.test_case "pending room-key bundle acceptance" `Quick
            (run test_pending_key_bundle_acceptance);
          Alcotest.test_case "room-key bundle download lifecycle" `Quick
            (run test_room_key_bundle_download_lifecycle);
          Alcotest.test_case "room-key bundle receipt and replacement" `Quick
            (run test_room_key_bundle_receipt_and_replacement);
          Alcotest.test_case "room-key bundle rejection" `Quick
            (run test_room_key_bundle_rejects_unauthenticated_and_malformed);
          Alcotest.test_case "room-key bundle persistence" `Quick
            (run test_room_key_bundle_persistence);
          Alcotest.test_case "secret gossip snapshot and store" `Quick
            (run test_secret_gossip_snapshot_and_store);
          Alcotest.test_case "a failed requery keeps the existing device" `Quick
            (run test_failed_requery_keeps_existing_device);
          Alcotest.test_case "cross-signed identity rotation is a violation"
            `Quick
            (run test_cross_signed_identity_chain_and_rotation);
          Alcotest.test_case "identity reset invalidates own trust" `Quick
            (run test_reset_cross_signing_invalidates_own_trust);
        ] );
      ( "sync",
        [
          Alcotest.test_case "one-time key upload is queued" `Quick
            (run test_one_time_key_upload_queued);
          Alcotest.test_case "one-time key replenishment reaches capacity"
            `Quick
            (run test_one_time_key_replenishment_counts);
          Alcotest.test_case "one-time key counts reject invalid integers"
            `Quick
            (run test_one_time_key_count_validation);
          Alcotest.test_case "fallback key age rotation" `Quick
            (run test_fallback_key_age_rotation);
          Alcotest.test_case "fallback key pending restart" `Quick
            (run test_fallback_key_pending_restart);
          Alcotest.test_case "one-time key count absence is source-specific"
            `Quick
            (run test_one_time_key_count_absence_source_semantics);
          Alcotest.test_case "device_lists drives /keys/query" `Quick
            (run test_device_lists_changed_queues_query);
          Alcotest.test_case "invite, knock and leave carry encryption state"
            `Quick
            (run test_invite_and_knock_encryption_state);
        ] );
      ( "http",
        [
          Alcotest.test_case "execute_requests hits every endpoint" `Quick
            (run test_execute_requests_endpoints);
          Alcotest.test_case "execute_requests reports failures" `Quick
            (run test_execute_requests_error_callback);
        ] );
      ( "key backup",
        [
          Alcotest.test_case "upload and restore" `Quick
            (run test_backup_round_trip);
          Alcotest.test_case "backup imports exported v1 keys only" `Quick
            (run test_backup_import_requires_exported_v1_key);
          Alcotest.test_case "stale upload does not acknowledge new version"
            `Quick
            (run test_stale_backup_upload_does_not_ack_new_version);
          Alcotest.test_case "scoped backup restore and history gates" `Quick
            (run test_scoped_backup_restore_and_history_gates);
          Alcotest.test_case "room backup download failure is retryable" `Quick
            (run test_room_backup_download_failure_is_retryable);
          Alcotest.test_case "share room history payload and ordering" `Quick
            (run test_share_room_history_payload_and_order);
          Alcotest.test_case "invite shares history before inviting" `Quick
            (run test_invite_user_by_id_order_and_errors);
          Alcotest.test_case "portable room-key export and import" `Quick
            (run test_portable_room_key_export_import);
          Alcotest.test_case
            "forwarded room-key identity and sender canonicalization" `Quick
            (run
               test_forwarded_room_key_validates_identity_and_canonicalizes_sender);
          Alcotest.test_case "portable export fixture" `Quick
            (run test_portable_export_fixture);
          Alcotest.test_case "uploads are bounded into batches" `Quick
            test_backup_batches;
          Alcotest.test_case "failed backup batches remain retryable" `Quick
            test_backup_batch_failure;
          Alcotest.test_case "concurrent backup calls are serialized" `Quick
            test_backup_batches_are_serialized;
          Alcotest.test_case "deleted or rotated backup disables uploads" `Quick
            test_backup_version_loss_disables_uploads;
        ] );
      ( "persistence",
        [
          Alcotest.test_case "crypto store round trip" `Quick
            test_store_round_trip;
          Alcotest.test_case "stale crypto generation is rejected" `Quick
            test_crypto_store_stale_generation;
        ] );
    ]
