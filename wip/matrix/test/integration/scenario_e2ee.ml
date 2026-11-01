(** End-to-end encryption against a live homeserver.

    [test_e2ee_integration.ml] runs the same wiring against a mock that behaves
    as a homeserver ought to. This runs it against one that behaves as Synapse
    actually does: every [/keys/upload], [/keys/query], [/keys/claim] and
    [/sendToDevice] here is a real request, and the [m.room.encrypted] events
    are fetched back out of a real timeline.

    Ten scenarios:

    + Alice creates a room with [m.room.encryption] in its initial state, Bob
      joins, and a message through Alice's send queue comes out of Bob's sync as
      plaintext. What is on the wire is checked too: an [m.room.encrypted] event
      whose content does not contain the body, and a device list on each side
      that names the other's device — which is only true if the key upload and
      the key query both worked.
    + A second message in the same Megolm session, and one back the other way in
      Bob's own outbound session.
    + An attachment is encrypted and streamed to Synapse, its encrypted-file
      metadata is sent through the encrypted room, and Bob streams, verifies and
      decrypts the media from the metadata he received.
    + SAS verification over the sync loops, driven by
      {!Matrix_eio.Verification_service} with both sides confirming, after which
      each machine marks the other's device
      {!Matrix_client.Encryption.Verified}.
    + Key backup: Alice creates a version, uploads her Megolm sessions, and a
      second device of hers — a real second login — restores from the backup and
      decrypts the message the first device sent, fetched from the server by
      event id. The same real late-key transition drives the public UTD hook,
      including grace-period collapse, timing and durable deduplication.
    + Shared history: a verified inviter uploads a room-key bundle before an
      invite, and the joining device downloads and imports it before decrypting
      the pre-join event.
    + Secret storage: the backup's decryption key stored in SSSS under a
      passphrase-derived key and read back by a client that has only the
      passphrase and what the server holds.
    + Recovery lifecycle: enable recovery, probe its state, rotate its SSSS key
      without replacing the backup, reset the cross-signing identity through
      password UIAA, recreate recovery around the conditionally replaced backup,
      then remove every server backup and clear the recovery account data.
    + Backup replacement: delete an active server-side backup underneath a
      device with a pending room key and verify that the failed upload disables
      the stale local version instead of retrying it forever.
    + Legacy dehydration: create and upload an independent MSC3814 V1 Olm
      account, have a peer send an encrypted room key to it, then rehydrate the
      queued key into a primary-side machine and confirm that its identity is
      unchanged. *)

module Id = Matrix_proto.Id
module Base = Matrix_client.Base_client
module Enc = Matrix_eio.Encryption
module Vs = Matrix_eio.Verification_service
module Backup = Matrix_client.Backup
module Keys = Matrix_client.Keys
module Ssss = Matrix_client.Secret_storage
module Cs = Matrix_client.Cross_signing
module Secrets = Matrix_client.Secrets
module Recovery = Matrix_eio.Recovery
module Uiaa = Matrix_client.Uiaa
module Ck = Matrix_client.Crypto_key
module Dehydrated_device = Matrix_client.Dehydrated_device
module Rooms = Matrix_client.Rooms
module Room_keys = Matrix_client.Room_keys
module Messages = Matrix_client.Messages
module Media = Matrix_client.Media
module Utd = Matrix_ui.Utd_hook
module Event_type = Matrix_proto.Event.Event_type
module Verification = Matrix_client.Verification

let check_string = Alcotest.(check string)
let check_bool = Alcotest.(check bool)
let check_int = Alcotest.(check int)
let rid = Id.Room_id.to_string
let uid = Id.User_id.to_string
let did = Id.Device_id.to_string
let eid = Id.Event_id.to_string
let type_of (e : Matrix_proto.Event.Raw_event.t) = Event_type.to_string e.type_

let json_to_string j =
  match Jsont_bytesrw.encode_string Matrix_proto.Json.Codec.json j with
  | Ok s -> s
  | Error msg -> Alcotest.failf "cannot re-encode an event's content: %s" msg

let json_of_codec what codec value =
  match Jsont_bytesrw.encode_string codec value with
  | Error msg -> Alcotest.failf "cannot encode %s: %s" what msg
  | Ok body -> (
      match Jsont_bytesrw.decode_string Matrix_proto.Json.Codec.json body with
      | Ok json -> json
      | Error msg -> Alcotest.failf "cannot decode encoded %s: %s" what msg)

let codec_of_json what codec json =
  match Jsont_bytesrw.decode_string codec (json_to_string json) with
  | Ok value -> value
  | Error msg -> Alcotest.failf "cannot decode %s: %s" what msg

let contains needle haystack =
  let n = String.length needle and h = String.length haystack in
  let rec go i =
    i + n <= h && (String.sub haystack i n = needle || go (i + 1))
  in
  go 0

(* The {!Matrix_eio} wrappers raise where their {!Matrix_client}
   counterparts return a result, so a scenario calling one needs the same
   naming that {!Harness.ok} gives the others. *)
let raising what f =
  try f ()
  with Eio.Io (Matrix_eio.Error.E err, _) ->
    Alcotest.failf "%s: %a" what Matrix_eio.Error.pp_err err

(* {1 An encrypting peer}

   A user, the machine that holds their Olm account, a sync loop running the
   machine over every response, a verification service the loop routes the
   [m.key.verification.*] events to, and a send queue that encrypts for the
   rooms the machine knows are encrypted. This is the whole of what an
   application has to assemble, which is what makes it worth assembling
   against a real server. *)

type peer = {
  user : Harness.user;
  enc : Enc.t;
  ver : Vs.t;
  prompts : Vs.prompt list ref;  (** Every SAS this peer was asked about. *)
  results : Vs.result list ref;
  answer : bool ref;  (** What this peer's user says to the next prompt. *)
  sync : Harness.sync;
  queue : Matrix_eio.Send_queue.t;
}

let make_peer h user =
  let enc =
    Enc.of_env (Harness.env h) ~user_id:user.Harness.user_id
      ~device_id:user.Harness.device_id ()
  in
  let prompts = ref [] and results = ref [] and answer = ref true in
  let clock = Harness.clock h in
  let ver =
    Vs.create ~client:user.Harness.client ~encryption:enc
      ~now:(fun () ->
        Matrix_proto.Event.Timestamp.of_ms
          (Int64.of_float (Eio.Time.now clock *. 1000.)))
      ~on_result:(fun r -> results := !results @ [ r ])
      ~confirm:(fun p ->
        prompts := !prompts @ [ p ];
        !answer)
      ()
  in
  let sync = Harness.start_sync h ~encryption:enc ~verification:ver user in
  let queue = Harness.start_send_queue h ~encryption:enc ~sync user in
  { user; enc; ver; prompts; results; answer; sync; queue }

let devices_of p (other : peer) =
  Enc.devices_of p.enc other.user.Harness.user_id

let holds_device p (other : peer) =
  List.exists
    (fun (d : Enc.device) ->
      Id.Device_id.equal d.device_id other.user.Harness.device_id)
    (devices_of p other)

let holds_device_id p user_id device_id =
  List.exists
    (fun (d : Enc.device) -> Id.Device_id.equal d.device_id device_id)
    (Enc.devices_of p.enc user_id)

let trust_of p (other : peer) =
  match
    Enc.find_device p.enc other.user.Harness.user_id
      ~device_id:other.user.Harness.device_id
  with
  | Some d -> d.Enc.trust
  | None ->
      Alcotest.failf "%s holds no device of %s's" p.user.Harness.localpart
        other.user.Harness.localpart

(* Everything this peer's sync decrypted in a room, oldest first. *)
let decrypted_in p room_id =
  List.concat_map
    (fun (c : Base.room_change) ->
      if String.equal (rid c.changed_room_id) (rid room_id) then c.decrypted
      else [])
    (Harness.room_changes p.sync)

let wait_for_plaintext h ?label p room_id body =
  Harness.wait_for h ?label (fun () ->
      List.find_opt
        (fun (d : Base.decrypted) ->
          Harness.string_member "body" d.plaintext.content = Some body)
        (decrypted_in p room_id))

(* {1 A room two peers can talk in}

   Both machines publish their device and one-time keys on their first sync
   response, and the room is created only once they have: a [/keys/query]
   that beat the other side's [/keys/upload] would answer with no devices,
   and nothing would be encrypted to them. *)

type stage = { alice : peer; bob : peer; room_id : Id.Room_id.t }

let encrypted_room h =
  let alice = make_peer h (Harness.register_user h ~prefix:"alice" ()) in
  let bob = make_peer h (Harness.register_user h ~prefix:"bob" ()) in
  List.iter
    (fun p ->
      Harness.wait_until h ~label:"the first sync response, which uploads keys"
        (fun () -> Harness.responses p.sync >= 1))
    [ alice; bob ];
  let room_id =
    Harness.ok "create an encrypted room"
      (Rooms.create (Harness.base alice.user) ~preset:Rooms.Trusted_private_chat
         ~invite:[ bob.user.Harness.user_id ]
         ~encrypted:true ())
  in
  ignore
    (Harness.wait_for_room h ~label:"bob's invite" bob.sync room_id (fun info ->
         info.membership = Base.Invited));
  ignore
    (Harness.ok "bob joins"
       (Rooms.join (Harness.base bob.user) ~room_id_or_alias:(`Room_id room_id)
          ()));
  ignore
    (Harness.wait_for_room h ~label:"bob's join" bob.sync room_id (fun info ->
         info.membership = Base.Joined));
  (* The send queue encrypts only what the machine says is an encrypted
     room, and encrypts it to the members the sync service reports. *)
  List.iter
    (fun p ->
      Harness.wait_until h
        ~label:"the room to be known encrypted, with both members" (fun () ->
          Enc.is_room_encrypted p.enc room_id
          && List.length
               (Matrix_eio.Sync_service.members (Harness.service p.sync) room_id)
             >= 2))
    [ alice; bob ];
  { alice; bob; room_id }

let send h p ~room_id ~body =
  let request = Matrix_eio.Send_queue.send_text p.queue ~room_id ~body in
  Harness.wait_sent h request

(* {1 One encrypted message, end to end} *)

let test_encrypted_round_trip () =
  Harness.run @@ fun h ->
  let s = encrypted_room h in
  let body = "hello bob " ^ Harness.hex h 4 in
  let event_id = send h s.alice ~room_id:s.room_id ~body in

  let d =
    wait_for_plaintext h ~label:"bob to decrypt alice's message" s.bob s.room_id
      body
  in
  check_string "it arrived encrypted" "m.room.encrypted" (type_of d.encrypted);
  check_string "and came out as a message" "m.room.message"
    (type_of d.plaintext);
  check_string "the body is alice's" body
    (Option.value
       (Harness.string_member "body" d.plaintext.content)
       ~default:"");
  check_string "the sender survives the envelope"
    (uid s.alice.user.Harness.user_id)
    (uid d.plaintext.sender);
  check_string "and so does the event id" (eid event_id)
    (eid (Option.get d.plaintext.event_id));
  check_bool "the session's sender key is alice's device's" true
    (Matrix_client.Crypto_key.Curve25519.Public.equal
       (snd (Enc.identity_keys s.alice.enc))
       d.info.decrypted_sender_key);
  check_bool "alice's device has signed device info but no owner chain" true
    (d.info.decrypted_verification = Matrix_client.Encryption.Device_info);

  (* What the server actually holds for that event id. *)
  let wire =
    Harness.wait_for_event h ~label:"the event on bob's timeline" s.bob.sync
      s.room_id (fun e ->
        match e.event_id with
        | Some id -> String.equal (eid id) (eid event_id)
        | None -> false)
  in
  check_string "the wire event is m.room.encrypted" "m.room.encrypted"
    (type_of wire);
  check_string "with the megolm algorithm" "m.megolm.v1.aes-sha2"
    (Option.value (Harness.string_member "algorithm" wire.content) ~default:"");
  check_bool "and the plaintext is nowhere in its content" false
    (contains body (json_to_string wire.content));

  (* The device lists: alice can only have encrypted to bob because
     [/keys/upload] and [/keys/query] both worked against Synapse. *)
  Harness.wait_until h ~label:"bob to hold alice's device" (fun () ->
      holds_device s.bob s.alice);
  check_bool "alice holds a device of bob's" true (holds_device s.alice s.bob);
  check_bool "bob holds a device of alice's" true (holds_device s.bob s.alice);
  check_int "and only the one device each" 1
    (List.length (devices_of s.alice s.bob))

(* {1 Encrypted attachment streaming}

   Alice streams ciphertext to the authenticated media endpoint, then sends
   the resulting [file] object inside an encrypted room event. Bob must first
   decrypt that event and then use exactly the metadata it carried to stream,
   authenticate and decrypt the attachment. *)

let test_encrypted_attachment_stream () =
  Harness.run @@ fun h ->
  let s = encrypted_room h in
  let plaintext = "streamed encrypted attachment " ^ Harness.hex h 16 in
  let c = Harness.base s.alice.user in
  let mxc, file =
    match
      Media.upload_encrypted_stream c
        ~source:(Eio.Flow.string_source plaintext)
        ~length:(Int64.of_int (String.length plaintext))
        ~filename:"stream.bin" ()
    with
    | Ok result -> result
    | Error e ->
        Alcotest.failf "stream encrypted attachment upload: %a"
          Media.pp_encrypted_error e
  in
  check_string "uploaded MXC is retained in event metadata"
    (Media.Mxc.to_string mxc) file.url;
  let body = "stream.bin" in
  let content : Matrix_proto.Event.Media_message_content.t =
    {
      body;
      msgtype = Matrix_proto.Event.Msgtype.File;
      url = None;
      info = None;
      file = Some file;
    }
  in
  let event_id =
    raising "send encrypted attachment event" (fun () ->
        Enc.send_encrypted s.alice.enc s.alice.user.Harness.client s.room_id
          ~event_type:"m.room.message"
          ~content:
            (json_of_codec "encrypted attachment event"
               Matrix_proto.Event.Media_message_content.jsont content)
          ~members:
            (Matrix_eio.Sync_service.members
               (Harness.service s.alice.sync)
               s.room_id))
  in
  let received =
    wait_for_plaintext h ~label:"bob to decrypt the attachment event" s.bob
      s.room_id body
  in
  check_string "the attachment arrived as an encrypted room event"
    "m.room.encrypted"
    (type_of received.encrypted);
  check_string "the attachment event id survives decryption" (eid event_id)
    (eid (Option.get received.plaintext.event_id));
  let received_content =
    codec_of_json "decrypted attachment event"
      Matrix_proto.Event.Media_message_content.jsont received.plaintext.content
  in
  let file =
    match received_content.file with
    | Some file -> file
    | None -> Alcotest.fail "decrypted attachment event has no file metadata"
  in
  check_string "bob received the uploaded MXC" (Media.Mxc.to_string mxc)
    file.url;
  let spool_path =
    Eio.Path.(
      Eio.Stdenv.fs (Harness.env h)
      / Filename.get_temp_dir_name ()
      / ("ocaml-matrix-attachment-" ^ Harness.hex h 8))
  in
  Eio.Path.with_open_out ~create:(`Or_truncate 0o600) spool_path (fun spool ->
      let output = Buffer.create (String.length plaintext) in
      Harness.wait_for h ~label:"streamed attachment to download" (fun () ->
          Buffer.clear output;
          match
            Media.download_encrypted_stream (Harness.base s.bob.user) file
              ~spool
              ~output:(Eio.Flow.buffer_sink output)
          with
          | Ok () -> Some ()
          | Error (Media.Media_error _) -> None
          | Error (Media.Attachment_error e) ->
              Alcotest.failf "stream attachment authentication: %a"
                Matrix_client.Encrypted_attachment.pp_error e);
      check_string "streamed attachment plaintext" plaintext
        (Buffer.contents output));
  Eio.Path.unlink ~missing_ok:true spool_path

(* {1 More messages}

   The second message in the same Megolm session exercises the ratchet
   rather than the key exchange; the reply exercises Bob's own outbound
   session and Alice's inbound copy of it. *)

let test_more_messages () =
  Harness.run @@ fun h ->
  let s = encrypted_room h in
  let first = "first " ^ Harness.hex h 4 in
  let second = "second " ^ Harness.hex h 4 in
  let reply = "reply " ^ Harness.hex h 4 in
  ignore (send h s.alice ~room_id:s.room_id ~body:first);
  ignore
    (wait_for_plaintext h ~label:"bob to decrypt the first" s.bob s.room_id
       first);
  ignore (send h s.alice ~room_id:s.room_id ~body:second);
  let d2 =
    wait_for_plaintext h ~label:"bob to decrypt the second" s.bob s.room_id
      second
  in
  check_string "the second message is in the same session"
    (Id.Session_id.to_string
       (Option.get (Enc.outbound_session_id s.alice.enc s.room_id)))
    (Id.Session_id.to_string d2.info.decrypted_session_id);
  check_int "and nothing was left undecrypted" 0
    (List.length
       (List.concat_map
          (fun (c : Base.room_change) -> c.undecrypted)
          (Harness.room_changes s.bob.sync)));

  ignore (send h s.bob ~room_id:s.room_id ~body:reply);
  let back =
    wait_for_plaintext h ~label:"alice to decrypt bob's reply" s.alice s.room_id
      reply
  in
  check_string "the reply came from bob"
    (uid s.bob.user.Harness.user_id)
    (uid back.plaintext.sender);
  check_bool "in bob's own session" true
    (Matrix_client.Crypto_key.Curve25519.Public.equal
       (snd (Enc.identity_keys s.bob.enc))
       back.info.decrypted_sender_key);
  check_bool "alice decrypted her own message too" true
    (List.exists
       (fun (d : Base.decrypted) ->
         Harness.string_member "body" d.plaintext.content = Some second)
       (decrypted_in s.alice s.room_id))

(* {1 SAS verification}

   Nothing here drives a state machine: Alice asks, Bob accepts, and the two
   sync loops carry the rest — ready, start, accept, key, key, the one
   prompt each side shows its user, the MACs and the dones — as
   [/sendToDevice] traffic through Synapse. *)

let test_sas () =
  Harness.run @@ fun h ->
  let s = encrypted_room h in
  check_bool "alice holds bob's device before starting" true
    (holds_device s.alice s.bob);
  Harness.wait_until h ~label:"bob to hold alice's device" (fun () ->
      holds_device s.bob s.alice);

  let _session =
    Vs.request s.alice.ver s.alice.user.Harness.client
      ~device_id:s.bob.user.Harness.device_id s.bob.user.Harness.user_id
  in
  let pending =
    Harness.wait_for h ~label:"bob's sync to carry the request" (fun () ->
        List.find_opt
          (fun (sess : Matrix_client.Verification.Flow.session) ->
            Matrix_client.Verification.Flow.session_stage sess
            = Matrix_client.Verification.Flow.Requested)
          (Vs.sessions s.bob.ver))
  in
  (* Accepting is a decision, so nothing answered it for him. *)
  Vs.accept s.bob.ver s.bob.user.Harness.client pending;

  Harness.wait_until h ~label:"both flows to finish" (fun () ->
      !(s.alice.results) <> [] && !(s.bob.results) <> []);

  check_int "alice was asked once" 1 (List.length !(s.alice.prompts));
  check_int "bob was asked once" 1 (List.length !(s.bob.prompts));
  let a = List.hd !(s.alice.prompts) and b = List.hd !(s.bob.prompts) in
  check_int "seven emoji" 7 (List.length a.Vs.emoji);
  Alcotest.(check (list string))
    "both sides saw the same emoji"
    (List.map (fun (e : Vs.emoji) -> e.symbol) a.Vs.emoji)
    (List.map (fun (e : Vs.emoji) -> e.symbol) b.Vs.emoji);
  check_bool "and the same decimals" true (a.Vs.decimals = b.Vs.decimals);
  check_bool "alice knows she started it" true a.Vs.we_started;
  check_bool "bob knows he did not" false b.Vs.we_started;

  let verified = function
    | Vs.Verified _ -> true
    | Vs.Publication_failed _ | Vs.Cancelled _ -> false
  in
  check_bool "alice's flow succeeded" true
    (List.exists verified !(s.alice.results));
  check_bool "bob's flow succeeded" true (List.exists verified !(s.bob.results));
  check_bool "alice marked bob's device verified" true
    (trust_of s.alice s.bob = Enc.Verified);
  check_bool "bob marked alice's device verified" true
    (trust_of s.bob s.alice = Enc.Verified)

let test_sas_in_room () =
  Harness.run @@ fun h ->
  let s = encrypted_room h in
  Harness.wait_until h ~label:"both peers to know the verification devices"
    (fun () -> holds_device s.alice s.bob && holds_device s.bob s.alice);
  let requested =
    Vs.request_in_room s.alice.ver s.alice.user.Harness.client
      ~room_id:s.room_id s.bob.user.Harness.user_id
  in
  let transaction = Verification.Flow.session_transaction requested in
  let pending =
    Harness.wait_for h ~label:"bob's sync to carry the in-room request"
      (fun () ->
        List.find_opt
          (fun (sess : Verification.Flow.session) ->
            Verification.Transaction.equal
              (Verification.Flow.session_transaction sess)
              transaction
            && Verification.Flow.session_stage sess
               = Verification.Flow.Requested)
          (Vs.sessions s.bob.ver))
  in
  Vs.accept s.bob.ver s.bob.user.Harness.client pending;
  let verified = function
    | Vs.Verified _ -> true
    | Vs.Publication_failed _ | Vs.Cancelled _ -> false
  in
  Harness.wait_until h ~label:"both in-room flows to finish" (fun () ->
      List.exists verified !(s.alice.results)
      && List.exists verified !(s.bob.results));
  check_bool "alice marked bob's in-room device verified" true
    (trust_of s.alice s.bob = Enc.Verified);
  check_bool "bob marked alice's in-room device verified" true
    (trust_of s.bob s.alice = Enc.Verified);
  (* Both syncs retain the raw encrypted envelopes and their decrypted
     plaintext. Combine them so the assertions do not depend on which side
     happened to report a local echo first, then deduplicate by event id. *)
  let expected_types =
    [
      "m.room.message";
      "m.key.verification.ready";
      "m.key.verification.start";
      "m.key.verification.accept";
      "m.key.verification.key";
      "m.key.verification.mac";
      "m.key.verification.done";
    ]
  in
  let collect_flow_events () =
    let all_decrypted =
      decrypted_in s.alice s.room_id @ decrypted_in s.bob s.room_id
    in
    let unique_decrypted =
      List.fold_left
        (fun acc (d : Base.decrypted) ->
          match d.encrypted.event_id with
          | Some event_id
            when List.exists
                   (fun (old : Base.decrypted) ->
                     old.encrypted.event_id = Some event_id)
                   acc ->
              acc
          | _ -> d :: acc)
        [] all_decrypted
      |> List.rev
    in
    List.filter_map
      (fun (d : Base.decrypted) ->
        match d.plaintext.event_id with
        | None -> None
        | Some event_id -> (
            let event_type = type_of d.plaintext in
            match
              Verification.Message.of_json ~event_type ~room_id:s.room_id
                ~event_id d.plaintext.content
            with
            | Ok message
              when Verification.Transaction.equal
                     (Verification.Message.transaction message)
                     transaction ->
                Some (d, message)
            | Ok _ | Error _ -> None))
      unique_decrypted
  in
  let has_expected_types flow_events =
    List.for_all
      (fun event_type ->
        List.exists
          (fun (_, message) ->
            String.equal (Verification.Message.event_type message) event_type)
          flow_events)
      expected_types
  in
  let flow_events =
    Harness.wait_for h ~label:"all in-room verification events" (fun () ->
        let events = collect_flow_events () in
        if
          List.length events >= List.length expected_types
          && has_expected_types events
        then Some events
        else None)
  in
  check_bool "in-room request and followups were decrypted" true
    (List.length flow_events >= List.length expected_types);
  check_bool "every in-room flow event was sent encrypted" true
    (List.for_all
       (fun ((d : Base.decrypted), _) ->
         String.equal (type_of d.encrypted) "m.room.encrypted")
       flow_events);
  List.iter
    (fun event_type ->
      check_bool
        ("in-room flow contains " ^ event_type)
        true
        (List.exists
           (fun (_, message) ->
             String.equal (Verification.Message.event_type message) event_type)
           flow_events))
    expected_types

(* {1 Key backup}

   The point of a backup is a device that was not there when the message was
   sent, so this registers one: a second login as Alice, with its own
   device id and its own Olm account, which has never held the room's Megolm
   session and cannot have been sent it. *)

let test_backup () =
  Harness.run @@ fun h ->
  let s = encrypted_room h in
  let body = "backed up " ^ Harness.hex h 4 in
  let event_id = send h s.alice ~room_id:s.room_id ~body in
  ignore
    (wait_for_plaintext h ~label:"bob to decrypt the message" s.bob s.room_id
       body);

  (* Recovery owns the backup version and the SSSS store, but the account's
     cross-signing identity is still published by the client.  Publish all
     three keys before enabling recovery, as a fresh device would otherwise
     recover private material that the homeserver does not know. *)
  let c = Harness.base s.alice.user in
  let identity = Cs.create_private_identity ~user_id:s.alice.user.user_id in
  Cs.generate_private_keys ~random:(Matrix_client.Client.random c) identity;
  let upload =
    match Cs.build_upload identity with
    | Some upload -> upload
    | None -> Alcotest.fail "the recovery identity is incomplete"
  in
  ignore
    (Harness.ok "publish Alice's complete cross-signing identity"
       (Keys.upload_signing_keys c ~master_key:upload.master_key
          ~self_signing_key:upload.self_signing_key
          ~user_signing_key:upload.user_signing_key ()));

  let passphrase = "correct horse battery staple" in
  let enabled =
    Recovery.enable s.alice.user.Harness.client ~encryption:s.alice.enc
      ~private_identity:identity ~passphrase ~wait_for_backups_to_upload:true ()
  in
  let version = enabled.backup_version in
  let info =
    Harness.ok "read the recovery backup" (Room_keys.get_current_version c)
  in
  check_string "recovery made the server version current" version info.version;
  check_string "recovery uses the Megolm backup algorithm"
    Backup.backup_algorithm info.algorithm;
  let uploaded =
    match enabled.backup_upload with
    | Recovery.Uploaded count -> count
    | Recovery.Not_waited -> Alcotest.fail "recovery did not await key upload"
    | Recovery.Upload_failed error ->
        Alcotest.failf "recovery backup upload failed: %s"
          (Matrix_client.Error.to_string error)
  in
  check_bool "recovery uploaded Alice's pending room keys" true (uploaded > 0);
  let info =
    Harness.ok "read the version back" (Room_keys.get_current_version c)
  in
  check_bool "and the server counts them" true (info.count >= uploaded);

  (* A second device of alice's: a real login, so a real device id. *)
  let fresh = Harness.connect h in
  let session =
    Harness.ok "log in a second device"
      (Matrix_client.Auth.login_password
         (Matrix_eio.Client.base fresh)
         ~user:s.alice.user.Harness.localpart
         ~password:s.alice.user.Harness.password ~request_refresh_token:true ())
  in
  let fresh = Matrix_eio.Client.with_session fresh session in
  check_bool "the server issued a requested refresh token" true
    (Option.is_some session.refresh_token);
  check_bool "the second login is a different device" false
    (Id.Device_id.equal session.device_id s.alice.user.Harness.device_id);
  let fresh_enc =
    Enc.of_env (Harness.env h) ~user_id:session.user_id
      ~device_id:session.device_id ()
  in
  check_bool "which holds none of the room's sessions" false
    (Enc.has_inbound_session fresh_enc s.room_id
       ~session_id:(Option.get (Enc.outbound_session_id s.alice.enc s.room_id)));

  (* The event as the server holds it, which is all the new device has. *)
  let encrypted =
    Harness.ok "fetch the encrypted event"
      (Messages.get_event
         (Matrix_eio.Client.base fresh)
         ~room_id:s.room_id ~event_id)
  in
  check_string "the server holds it encrypted" "m.room.encrypted"
    (type_of encrypted);
  let first_error =
    match Enc.decrypt_room_event fresh_enc s.room_id encrypted with
    | Error error -> error
    | Ok _ -> Alcotest.fail "the new device decrypted before restoring a key"
  in

  (* The public hook sees the real event that this new device cannot decrypt.
     A repeated observation while it is pending collapses, and restoring the
     backup turns that pending UTD into one timed late-decryption report. *)
  let utd_store = Matrix_client.Store.memory () in
  let utd_reports = ref [] in
  let own_homeserver =
    Id.Server_name.to_string (Id.User_id.server_name session.user_id)
  in
  let utd =
    Utd.create ~sw:(Harness.switch h) ~clock:(Harness.clock h) ~store:utd_store
      ~max_delay:30. ~own_homeserver
      ~on_utd:(fun report -> utd_reports := report :: !utd_reports)
      ()
  in
  let utd_context =
    Matrix_client.Encryption.utd_context (Enc.machine fresh_enc) first_error
  in
  let cause =
    Matrix_client.Encryption.classify_utd encrypted first_error utd_context
  in
  let observe_utd () =
    Utd.on_utd utd ~event:encrypted ~cause
      ~user_trusts_own_identity:utd_context.local_device_verified ()
  in
  observe_utd ();
  observe_utd ();
  check_int "duplicate UTD observations remain pending once" 0
    (List.length !utd_reports);

  let recovered =
    Recovery.recover fresh ~encryption:fresh_enc ~credential:passphrase
  in
  check_bool "recovery restored the complete cross-signing identity" true
    (Option.is_some (Cs.master_secret recovered.private_identity)
    && Option.is_some (Cs.self_signing_secret recovered.private_identity)
    && Option.is_some (Cs.user_signing_secret recovered.private_identity));
  let imported =
    raising "restore from the backup" (fun () ->
        Enc.restore_from_backup fresh_enc fresh)
  in
  check_bool "the backup gave the new device a session" true (imported > 0);
  match Enc.decrypt_room_event fresh_enc s.room_id encrypted with
  | Error e ->
      Alcotest.failf "the restored session did not decrypt the event: %a"
        Enc.pp_decrypt_error e
  | Ok plaintext ->
      Utd.on_late_decrypt utd event_id;
      Utd.on_late_decrypt utd event_id;
      check_int "the late key emits one UTD report" 1 (List.length !utd_reports);
      let report = List.hd !utd_reports in
      check_string "the UTD report names the restored event" (eid event_id)
        (eid report.event_id);
      check_bool "the UTD report includes time to decrypt" true
        (Option.exists (fun seconds -> seconds >= 0.) report.time_to_decrypt);
      check_string "the UTD report names our homeserver" own_homeserver
        (Option.value report.own_homeserver ~default:"");
      let reloaded =
        Utd.create ~sw:(Harness.switch h) ~clock:(Harness.clock h)
          ~store:utd_store
          ~on_utd:(fun report -> utd_reports := report :: !utd_reports)
          ()
      in
      Utd.on_utd reloaded ~event:encrypted ~cause
        ~user_trusts_own_identity:utd_context.local_device_verified ();
      check_int "the reported event stays deduplicated after reload" 1
        (List.length !utd_reports);
      check_string "the new device reads what the old one sent" body
        (Option.value
           (Harness.string_member "body" plaintext.decrypted_content)
           ~default:"");
      check_string "as an m.room.message" "m.room.message"
        plaintext.decrypted_type

(* {1 Shared room history bundles}

   This is the live MSC4268 path. Alice publishes a cross-signing identity and
   sends a message while she is the room's only member. She then backs up that
   Megolm session, invites Bob, and shares the encrypted history bundle after
   Bob joins. Bob had no room key before the bundle arrived; his sync loop
   re-queries Alice's device, checks the cross-signature, downloads the
   encrypted media, imports the key and decrypts the pre-join event.

   The current room API exposes the Rust workflow as
   [Encryption.share_room_history], so the scenario invokes that operation at
   the point where Rust's invite path would invoke it. *)

let test_shared_history_bundle () =
  Harness.run @@ fun h ->
  let alice_user = Harness.register_user h ~prefix:"alice-history" () in
  let bob_user = Harness.register_user h ~prefix:"bob-history" () in
  let private_identity =
    Cs.create_private_identity ~user_id:alice_user.Harness.user_id
  in
  Cs.generate_private_keys
    ~random:(Matrix_client.Client.random (Harness.base alice_user))
    private_identity;
  let upload =
    match Cs.build_upload private_identity with
    | Some upload -> upload
    | None -> Alcotest.fail "cross-signing identity is incomplete"
  in
  Harness.ok "publish Alice's cross-signing identity"
    (Keys.upload_signing_keys (Harness.base alice_user)
       ~master_key:upload.master_key ~self_signing_key:upload.self_signing_key
       ~user_signing_key:upload.user_signing_key ());
  let alice = make_peer h alice_user in
  let bob = make_peer h bob_user in
  List.iter
    (fun p ->
      Harness.wait_until h ~label:"the first sync response" (fun () ->
          Harness.responses p.sync >= 1))
    [ alice; bob ];
  let signed_device_keys =
    match Cs.self_signing_secret private_identity with
    | None -> Alcotest.fail "Alice has no self-signing secret"
    | Some signer ->
        Cs.sign_device_keys ~signer ~signer_user_id:alice_user.Harness.user_id
          (Enc.device_keys_for_upload alice.enc)
  in
  ignore
    (Harness.ok "publish Alice's self-signing device signature"
       (Keys.upload_keys (Harness.base alice_user)
          ~device_keys:signed_device_keys ()));
  let room_id =
    Harness.ok "create the encrypted history room"
      (Rooms.create (Harness.base alice.user) ~preset:Rooms.Trusted_private_chat
         ~encrypted:true ())
  in
  ignore
    (Harness.wait_for_room h ~label:"Alice to join the history room" alice.sync
       room_id (fun info -> info.membership = Base.Joined));
  Harness.wait_until h ~label:"Alice to know the room is encrypted" (fun () ->
      Enc.is_room_encrypted alice.enc room_id);
  let body = "pre-join history " ^ Harness.hex h 4 in
  let event_id = send h alice ~room_id ~body in
  let session_id =
    match Enc.outbound_session_id alice.enc room_id with
    | Some session_id -> session_id
    | None -> Alcotest.fail "Alice did not create a Megolm session"
  in
  check_bool "Bob has no pre-join room key" false
    (Enc.has_inbound_session bob.enc room_id ~session_id);
  let c = Harness.base alice.user in
  let random = Matrix_client.Client.random c in
  let key = Backup.Decryption_key.generate ~random in
  let auth_data =
    Enc.sign alice.enc
      (Backup.auth_data_to_json
         { public_key = Backup.Decryption_key.public key; signatures = [] })
  in
  let version =
    Harness.ok "create the history backup version"
      (Room_keys.create_version c ~algorithm:Backup.backup_algorithm ~auth_data)
  in
  Enc.enable_backup alice.enc ~version ~decryption_key:key
    (Backup.Decryption_key.public key);
  let uploaded =
    raising "upload Alice's history key" (fun () ->
        Enc.backup_pending alice.enc alice.user.Harness.client)
  in
  check_bool "the pre-join session is backed up" true (uploaded > 0);
  (* The sender's currently-open inbound copy is deliberately not marked as
     shareable history. Use a second, real Alice login and seed its crypto
     machine by portable room-key import; this carries the stable
     [shared_history] provenance into the bundle without claiming that this
     scenario exercises server-backup restore on that device. *)
  let sharer_client = Harness.connect h in
  let sharer_session =
    Harness.ok "log in Alice's history-sharing device"
      (Matrix_client.Auth.login_password
         (Matrix_eio.Client.base sharer_client)
         ~user:alice_user.Harness.localpart
         ~password:alice_user.Harness.password ())
  in
  let sharer_client =
    Matrix_eio.Client.with_session sharer_client sharer_session
  in
  let sharer_user : Harness.user =
    {
      localpart = alice_user.localpart;
      password = alice_user.password;
      user_id = sharer_session.user_id;
      device_id = sharer_session.device_id;
      client = sharer_client;
    }
  in
  let sharer_enc =
    Enc.of_env (Harness.env h) ~user_id:sharer_user.user_id
      ~device_id:sharer_user.device_id ()
  in
  let signed_sharer_device_keys =
    match Cs.self_signing_secret private_identity with
    | None -> Alcotest.fail "Alice has no self-signing secret"
    | Some signer ->
        Cs.sign_device_keys ~signer ~signer_user_id:alice_user.Harness.user_id
          (Enc.device_keys_for_upload sharer_enc)
  in
  ignore
    (Harness.ok "publish the history-sharing device signature"
       (Keys.upload_keys (Harness.base sharer_user)
          ~device_keys:signed_sharer_device_keys ()));
  let own_keys =
    Harness.ok "query Alice's identity for the history-sharing device"
      (Keys.query_keys (Harness.base sharer_user)
         ~users:[ (alice_user.Harness.user_id, []) ]
         ())
  in
  Enc.receive_keys_query sharer_enc own_keys;
  let source_key =
    match
      List.find_opt
        (fun (key : Matrix_client.Room_key_export.room_key) ->
          Id.Room_id.equal key.room_id room_id
          && Id.Session_id.equal key.session_id session_id)
        (Matrix_client.Encryption.export_room_keys (Enc.machine alice.enc))
    with
    | Some key -> { key with shared_history = true }
    | None -> Alcotest.fail "Alice's history key was not exportable"
  in
  let imported =
    Matrix_client.Encryption.import_room_keys (Enc.machine sharer_enc)
      [ source_key ]
  in
  check_int "history-sharing device restores one room key" 1
    imported.imported_count;
  Enc.enable_backup sharer_enc ~version ~decryption_key:key
    (Backup.Decryption_key.public key);
  Harness.ok "invite Bob to the history room"
    (Rooms.invite (Harness.base alice.user) ~room_id
       ~user_id:bob.user.Harness.user_id ());
  ignore
    (Harness.wait_for_room h ~label:"Bob's history-room invite" bob.sync room_id
       (fun info -> info.membership = Base.Invited));
  let inviter =
    match Base.inviter (Harness.state bob.sync) room_id with
    | Some inviter -> inviter
    | None -> Alcotest.fail "Bob's invite had no authenticated inviter"
  in
  ignore
    (Harness.ok "Bob joins the history room"
       (Rooms.join (Harness.base bob.user) ~room_id_or_alias:(`Room_id room_id)
          ()));
  ignore
    (Harness.wait_for_room h ~label:"Bob to join the history room" bob.sync
       room_id (fun info -> info.membership = Base.Joined));
  (* The UI runtime records this at the successful join boundary. The harness
     drives the same boundary directly so the crypto service can exercise its
     automatic receive-side processing. *)
  Matrix_client.Encryption.record_invite_acceptance (Enc.machine bob.enc)
    ~room_id ~inviter;
  Harness.wait_until h ~label:"Bob to know Alice's cross-signing identity"
    (fun () ->
      holds_device bob alice
      && Option.is_some
           (Enc.identity_master_key bob.enc alice.user.Harness.user_id));
  let shared =
    match
      Enc.share_room_history sharer_enc sharer_user.client ~room_id
        ~recipient:bob.user.Harness.user_id
        ~history_visibility:Matrix_proto.Event.History_visibility.Shared
    with
    | Ok result -> result
    | Error (Enc.Share_encryption_error error) ->
        Alcotest.failf "share history encryption: %s"
          (Matrix_client.Error.to_string error)
    | Error (Enc.Share_media_error error) ->
        Alcotest.failf "share history media: %a" Media.pp_encrypted_error error
  in
  (match shared with
  | Enc.History_shared count ->
      check_bool "bundle carries a room key" true (count > 0)
  | Enc.History_no_keys -> Alcotest.fail "history bundle had no keys"
  | Enc.History_not_shared_identity ->
      Alcotest.fail "Alice's published identity was not available"
  | Enc.History_not_shared_visibility ->
      Alcotest.fail "history visibility unexpectedly blocked sharing");
  Harness.wait_until h ~label:"Bob to import the history bundle" (fun () ->
      Enc.has_inbound_session bob.enc room_id ~session_id);
  let encrypted =
    Harness.wait_for_event h ~label:"the pre-join event on Bob's timeline"
      bob.sync room_id (fun event ->
        match event.event_id with
        | Some id -> Id.Event_id.equal id event_id
        | None -> false)
  in
  match Enc.decrypt_room_event bob.enc room_id encrypted with
  | Error error ->
      Alcotest.failf "Bob could not decrypt the imported history: %a"
        Enc.pp_decrypt_error error
  | Ok plaintext ->
      check_string "Bob decrypts the pre-join event" body
        (Option.value
           (Harness.string_member "body" plaintext.decrypted_content)
           ~default:"")

(* {1 Secret storage}

   The backup's decryption key is exactly what SSSS exists to move between a
   user's devices: the server stores it, the server cannot read it, and a
   device that knows the passphrase can. *)

let test_secret_storage () =
  Harness.run @@ fun h ->
  let alice = Harness.register_user h ~prefix:"alice" () in
  let c = Harness.base alice in
  let random = Matrix_client.Client.random c in
  let backup_key = Backup.Decryption_key.generate ~random in
  (* Matrix secrets are UTF-8 strings.  Rust exports a backup decryption key as
     unpadded Base64 here, not as its raw Curve25519 bytes. *)
  let secret = Backup.Decryption_key.to_base64 backup_key in

  (* 10 000 rounds rather than the spec's suggested 500 000: this is a test,
     and what is under test is the storage rather than the KDF's cost. *)
  let passphrase = "correct horse " ^ Harness.hex h 4 in
  let info = Ssss.Passphrase_info.v ~random ~iterations:10_000 () in
  let key =
    match Ssss.key_of_passphrase ~passphrase info with
    | Ok k -> k
    | Error (`Msg msg) ->
        Alcotest.failf "cannot derive a key from the passphrase: %s" msg
  in
  let key_id = "ocaml-matrix-" ^ Harness.hex h 4 in
  let description =
    Ssss.Key_description.v ~random ~name:"integration test key" ~passphrase:info
      key
  in
  check_string "the description names the only algorithm there is"
    Ssss.algorithm description.algorithm;
  Harness.ok "publish the key description"
    (Secrets.put_key_description c ~key_id description);
  Harness.ok "make it the default key" (Secrets.set_default_key_id c ~key_id);
  Harness.ok "store the backup key"
    (Secrets.store_secret c ~random ~key_id ~key
       ~name:Ssss.secret_megolm_backup_v1 secret);

  (* Now read it back as another device would: nothing but the passphrase
     and what the server holds. *)
  check_string "the server reports the default key" key_id
    (Option.get
       (Harness.ok "read the default key id" (Secrets.get_default_key_id c)));
  let published =
    Harness.ok "read the key description back"
      (Secrets.get_key_description c ~key_id)
  in
  let published_info =
    match published.passphrase with
    | Some p -> p
    | None ->
        Alcotest.fail "the description came back without its passphrase info"
  in
  let derive p =
    match Ssss.key_of_passphrase ~passphrase:p published_info with
    | Ok k -> k
    | Error (`Msg msg) -> Alcotest.failf "cannot re-derive the key: %s" msg
  in
  let derived = derive passphrase in
  let wrong = derive "not the passphrase" in
  check_bool "the re-derived key passes the description's check" true
    (Ssss.check_key derived published = Ssss.Correct);
  check_bool "and a wrong passphrase does not" true
    (Ssss.check_key wrong published = Ssss.Incorrect);
  let read =
    Harness.ok "read the secret back"
      (Secrets.get_secret c ~key_id ~key:derived
         ~name:Ssss.secret_megolm_backup_v1)
  in
  check_string "the secret survives the round trip" secret read;
  check_bool "and a wrong key cannot read it" true
    (Harness.is_error
       (Secrets.get_secret c ~key_id ~key:wrong
          ~name:Ssss.secret_megolm_backup_v1));
  (* And what came back really is the backup's decryption key rather than
     merely the same bytes: the public half recomputed from it is the one the
     backup version would have been created with. *)
  let recovered =
    let read_key =
      match Backup.Decryption_key.of_base64 read with
      | Ok k -> k
      | Error (`Msg msg) ->
          Alcotest.failf "the recovered secret is not a backup key: %s" msg
    in
    match Backup.Recovery_key.decode (Backup.Recovery_key.encode read_key) with
    | Ok k -> k
    | Error (`Msg msg) ->
        Alcotest.failf "cannot read the recovered key back: %s" msg
  in
  check_string "and the public half it implies is the backup's"
    (Ck.Curve25519.Public.to_base64 (Backup.Decryption_key.public backup_key))
    (Ck.Curve25519.Public.to_base64 (Backup.Decryption_key.public recovered))

(* {1 Recovery lifecycle}

   This covers the state-changing recovery workflow against Synapse without
   pretending that the project's private JSON session pickle can rehydrate a
   libolm/vodozemac device. *)

let test_recovery_lifecycle () =
  Harness.run @@ fun h ->
  let alice = Harness.register_user h ~prefix:"recovery" () in
  let client = alice.Harness.client in
  let base = Harness.base alice in
  let encryption =
    Enc.of_env (Harness.env h) ~user_id:alice.user_id ~device_id:alice.device_id
      ()
  in
  let identity = Cs.create_private_identity ~user_id:alice.user_id in
  Cs.generate_private_keys ~random:(Matrix_client.Client.random base) identity;
  let enabled =
    Recovery.enable client ~encryption ~private_identity:identity ()
  in
  check_string "recovery created the active server backup"
    enabled.backup_version
    (Harness.ok "read the enabled backup" (Room_keys.get_current_version base))
      .version;
  check_bool "enabled recovery probes as enabled" true
    (Recovery.check_state client ~encryption ~private_identity:identity
    = Recovery.Enabled);
  let rotated =
    Recovery.reset_key client ~encryption ~private_identity:identity ()
  in
  check_bool "reset publishes a distinct default recovery key" true
    (not (String.equal enabled.key_id rotated.key_id));
  check_string "the rotated key is now the server default" rotated.key_id
    (Option.get
       (Harness.ok "read the rotated default key"
          (Secrets.get_default_key_id base)));
  check_string "rotating recovery does not replace the backup"
    enabled.backup_version
    (Harness.ok "read the backup after rotation"
       (Room_keys.get_current_version base))
      .version;
  check_bool "rotated recovery still probes as enabled" true
    (Recovery.check_state client ~encryption ~private_identity:identity
    = Recovery.Enabled);
  let old_master = Option.get (Cs.master_public identity) in
  let manager =
    Recovery.Manager.create client ~encryption ~private_identity:identity
  in
  let replacement_identity =
    match
      Recovery.Manager.reset_identity manager
        ~auth_callback:(fun challenge ->
          Some
            (Uiaa.password_auth ~user:(uid alice.user_id)
               ~password:alice.password ?session:challenge.session ()))
        ()
    with
    | Uiaa.Uiaa_success identity -> identity
    | Uiaa.Uiaa_auth_required challenge ->
        Alcotest.failf "identity reset still requires UIAA (%d flows)"
          (List.length challenge.flows)
    | Uiaa.Uiaa_error error ->
        Alcotest.failf "identity reset failed: %s"
          (Matrix_client.Error.to_string error)
  in
  check_bool "identity reset rotates the cross-signing master key" true
    (not
       (Ck.Ed25519.Public.equal old_master
          (Option.get (Cs.master_public replacement_identity))));
  let replacement_backup =
    (Harness.ok "read the replacement backup"
       (Room_keys.get_current_version base))
      .version
  in
  check_bool "identity reset conditionally replaces the active backup" true
    (not (String.equal enabled.backup_version replacement_backup));
  check_bool "identity reset leaves SSSS truthfully disabled" true
    (Recovery.Manager.state manager = Recovery.Disabled);
  let reenabled = Recovery.Manager.enable manager () in
  check_string "re-enabling recovery keeps the replacement backup"
    replacement_backup reenabled.backup_version;
  check_bool "recovery is usable after publishing the replacement identity" true
    (Recovery.Manager.refresh manager = Recovery.Enabled
    && not (String.equal reenabled.recovery_key ""));
  Recovery.disable_and_delete_backups client ~encryption;
  check_bool "all server backups were deleted" true
    (Harness.is_error (Room_keys.get_current_version base));
  check_bool "the local backup was disabled" true
    (Enc.backup_version encryption = None);
  Recovery.disable_account_data ~default_key_id:reenabled.key_id client;
  check_bool "cleared recovery metadata probes as disabled" true
    (Recovery.check_state client ~encryption
       ~private_identity:replacement_identity
    = Recovery.Disabled)

let test_deleted_backup_disables_upload () =
  Harness.run @@ fun h ->
  let alice = Harness.register_user h ~prefix:"backup-loss" () in
  let base = Harness.base alice in
  let encryption =
    Enc.of_env (Harness.env h) ~user_id:alice.user_id ~device_id:alice.device_id
      ()
  in
  let room_id =
    Harness.ok "create the backup-loss room" (Rooms.create base ())
  in
  Enc.set_room_encryption_settings encryption room_id
    (Enc.room_encryption_content (Enc.enable_room_encryption ()));
  let content =
    Jsont.Json.object'
      [
        Jsont.Json.mem (Jsont.Json.name "msgtype") (Jsont.Json.string "m.text");
        Jsont.Json.mem (Jsont.Json.name "body")
          (Jsont.Json.string "pending backup key");
      ]
  in
  ignore
    (Enc.encrypt_room_event encryption alice.client room_id
       ~event_type:"m.room.message" ~content ~members:[]);
  let key =
    Backup.Decryption_key.generate ~random:(Matrix_client.Client.random base)
  in
  let auth_data =
    Enc.sign encryption
      (Backup.auth_data_to_json
         { public_key = Backup.Decryption_key.public key; signatures = [] })
  in
  let version =
    Harness.ok "create the soon-deleted backup"
      (Room_keys.create_version base ~algorithm:Backup.backup_algorithm
         ~auth_data)
  in
  Enc.enable_backup encryption ~version ~decryption_key:key
    (Backup.Decryption_key.public key);
  check_bool "one local room key awaits backup" true
    (Enc.backup_pending_count encryption > 0);
  Harness.ok "delete the active backup" (Room_keys.delete_version base ~version);
  let expected_version_error =
    try
      ignore (Enc.backup_pending encryption alice.client);
      false
    with
    | Eio.Io
        ( Matrix_eio.Error.E
            (Matrix_eio.Error.Matrix
               {
                 errcode =
                   ( Matrix_client.Error.M_NOT_FOUND
                   | Matrix_client.Error.M_WRONG_ROOM_KEYS_VERSION );
                 _;
               }),
          _ ) ->
        true
    | Eio.Io (Matrix_eio.Error.E (Matrix_eio.Error.Http { status = 404; _ }), _)
      ->
        true
  in
  check_bool "Synapse rejects the deleted backup version" true
    expected_version_error;
  check_bool "the rejected version is disabled locally" true
    (Enc.backup_version encryption = None)

(* {1 Legacy dehydrated device}

   This deliberately exercises the complete live lifecycle. The device is
   created from a same-user private cross-signing identity, so the upload
   includes the signed device and one-time keys that another user can claim.
   Bob sends an encrypted room message after Synapse publishes that device;
   the resulting room key is drained and imported by [rehydrate]. *)

let test_dehydrated_device_live () =
  Harness.run @@ fun h ->
  let s = encrypted_room h in
  let alice = s.alice.user in
  let client = Harness.base alice in
  (* Keep Alice's live peer running so this remains a real room flow, but use a
     fresh driver for rehydration. Otherwise Alice's active sync can import the
     same Megolm session first, making the exact import count non-deterministic.
     It has the same user/device identifiers and is the machine that receives
     the dehydrated account's imported room key. *)
  let encryption =
    Enc.of_env (Harness.env h) ~user_id:alice.user_id ~device_id:alice.device_id
      ()
  in
  let primary_device = Enc.device_id encryption in
  let primary_ed25519, primary_curve25519 = Enc.identity_keys encryption in
  let supported =
    Harness.ok "probe MSC3814 support" (Dehydrated_device.is_supported client)
  in
  check_bool "Synapse advertises the MSC3814 endpoint" true supported;
  let random = Matrix_client.Client.random client in
  let private_identity = Cs.create_private_identity ~user_id:alice.user_id in
  Cs.generate_private_keys ~random private_identity;
  let pickle_key = Dehydrated_device.Pickle_key.generate ~random in
  let upload () =
    Harness.ok "create and upload the dehydrated device"
      (Dehydrated_device.create_and_upload encryption client ~private_identity
         ~pickle_key ~random ())
  in
  (* Exercise an explicit DELETE as well as the DELETE performed by
     [rehydrate]. The finally clause also removes the second upload when a
     later assertion fails. *)
  let cleanup () = ignore (Dehydrated_device.delete_if_present client) in
  Fun.protect
    (fun () ->
      let first_device_id = upload () in
      let first =
        Harness.ok "get the uploaded dehydrated device"
          (Dehydrated_device.get client)
      in
      check_string "GET returns the uploaded device id" (did first_device_id)
        (did first.device_id);
      ignore
        (Harness.ok "delete the first dehydrated device"
           (Dehydrated_device.delete client));
      check_bool "explicit DELETE removes the device" true
        (Harness.ok "GET after explicit DELETE"
           (Dehydrated_device.get_if_present client)
        = None);
      (* Re-upload a fresh account: this is the one rehydrated below after Bob
         has sent it an encrypted room key. *)
      let uploaded_device_id = upload () in
      let uploaded =
        Harness.ok "get the second uploaded dehydrated device"
          (Dehydrated_device.get client)
      in
      check_string "second GET returns the uploaded device id"
        (did uploaded_device_id) (did uploaded.device_id);
      let events =
        Harness.ok "read the new dehydrated-device event queue"
          (Dehydrated_device.get_events client ~device_id:uploaded_device_id ())
      in
      check_int "new dehydrated device starts with no queued events" 0
        (List.length events.events);
      (* A room key is sent to every device of the recipient. Wait for Bob's
         device list to include the dehydrated device before sending, so the
         request really addresses it rather than relying on a propagation
         race. *)
      Harness.wait_until h ~label:"bob to learn the dehydrated device"
        (fun () ->
          holds_device_id s.bob s.alice.user.Harness.user_id uploaded_device_id);
      let body = "dehydrated continuity " ^ Harness.hex h 8 in
      let event_id = send h s.bob ~room_id:s.room_id ~body in
      let wire =
        Harness.wait_for_event h ~label:"Bob to receive his ciphertext echo"
          s.bob.sync s.room_id (fun event ->
            match event.event_id with
            | Some id -> String.equal (eid id) (eid event_id)
            | None -> false)
      in
      check_string "the continuity event stays encrypted on the wire"
        "m.room.encrypted" (type_of wire);
      let queued =
        Harness.wait_for h ~label:"Synapse to queue a real room key" (fun () ->
            let page =
              Harness.ok "read queued dehydrated-device events"
                (Dehydrated_device.get_events client
                   ~device_id:uploaded_device_id ())
            in
            if page.events = [] then None else Some page)
      in
      check_bool "the dehydrated queue contains an Olm-encrypted event" true
        (List.exists
           (fun event -> contains "m.room.encrypted" (json_to_string event))
           queued.events);
      let outcome =
        Harness.ok "rehydrate the queued dehydrated device"
          (Dehydrated_device.rehydrate encryption client ~pickle_key ~random ())
      in
      let outcome =
        match outcome with
        | Some outcome -> outcome
        | None -> Alcotest.fail "rehydrate unexpectedly found no device"
      in
      check_string "rehydrate reports the uploaded device id"
        (did uploaded_device_id) (did outcome.device_id);
      check_bool "rehydrate imports the peer's room key" true
        (outcome.room_keys_imported > 0);
      check_bool "rehydrate drains the encrypted to-device event" true
        (outcome.to_device_events > 0);
      let plaintext =
        match Enc.decrypt_room_event encryption s.room_id wire with
        | Ok plaintext -> plaintext
        | Error error ->
            Alcotest.failf
              "rehydrated room key does not decrypt Bob's event: %a"
              Enc.pp_decrypt_error error
      in
      check_string "the rehydrated key decrypts a real room message"
        "m.room.message" plaintext.decrypted_type;
      check_string "the decrypted message survived rehydration" body
        (Option.value
           (Harness.string_member "body" plaintext.decrypted_content)
           ~default:"");
      check_bool "rehydrate deletes the server-side device" true
        (outcome.delete_error = None);
      check_bool "GET after rehydrate finds no device" true
        (Harness.ok "confirm rehydrate deletion"
           (Dehydrated_device.get_if_present client)
        = None);
      check_string "primary device id is unchanged" (did primary_device)
        (did (Enc.device_id encryption));
      let ed25519, curve25519 = Enc.identity_keys encryption in
      check_string "primary Ed25519 identity is unchanged"
        (Ck.Ed25519.Public.to_base64 primary_ed25519)
        (Ck.Ed25519.Public.to_base64 ed25519);
      check_string "primary Curve25519 identity is unchanged"
        (Ck.Curve25519.Public.to_base64 primary_curve25519)
        (Ck.Curve25519.Public.to_base64 curve25519))
    ~finally:cleanup

let tests =
  [
    Alcotest.test_case "an encrypted room, end to end" `Quick
      test_encrypted_round_trip;
    Alcotest.test_case "an encrypted attachment streams end to end" `Quick
      test_encrypted_attachment_stream;
    Alcotest.test_case "the session ratchets and answers back" `Quick
      test_more_messages;
    Alcotest.test_case "SAS over the sync loops" `Quick test_sas;
    Alcotest.test_case "SAS in an encrypted room" `Quick test_sas_in_room;
    Alcotest.test_case "a key backup a new device restores" `Quick test_backup;
    Alcotest.test_case "a trusted join restores shared room history" `Quick
      test_shared_history_bundle;
    Alcotest.test_case "the backup key in secret storage" `Quick
      test_secret_storage;
    Alcotest.test_case "recovery enable, rotate, and disable" `Quick
      test_recovery_lifecycle;
    Alcotest.test_case "a deleted backup disables stale uploads" `Quick
      test_deleted_backup_disables_upload;
    Alcotest.test_case "legacy dehydrated device live lifecycle" `Quick
      test_dehydrated_device_live;
  ]
