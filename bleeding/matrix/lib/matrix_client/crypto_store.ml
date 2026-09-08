let src = Logs.Src.create "matrix.crypto_store" ~doc:"E2EE state persistence"

module Log = (val Logs.src_log src : Logs.LOG)
module Uid = Matrix_proto.Id.User_id
module Rid = Matrix_proto.Id.Room_id
module Sid = Matrix_proto.Id.Session_id
module Did = Matrix_proto.Id.Device_id
module Curve25519 = Crypto_key.Curve25519

let ( let* ) = Result.bind

type snapshot = Encryption.snapshot

let json_err msg = Error (Error.Json_error msg)

(* An entry naming an identifier this library cannot parse names a room or a
   session it could not address anyway, so it is dropped rather than failing
   the whole file and costing the device everything else it holds. *)
let id_map_jsont ~of_string ~to_string value =
  Json_codec.keyed_map ~skip_invalid:true ~what:"identifier" ~of_string
    ~to_string value

let trust_jsont : Encryption.trust Jsont.t =
  Jsont.enum ~kind:"device trust"
    [
      ("unverified", Encryption.Unverified);
      ("verified", Encryption.Verified);
      ("blacklisted", Encryption.Blacklisted);
    ]

let trust_requirement_jsont : Encryption.trust_requirement Jsont.t =
  Jsont.enum
    [
      ("untrusted", Encryption.Untrusted);
      ("cross_signed_or_legacy", Encryption.Cross_signed_or_legacy);
      ("cross_signed", Encryption.Cross_signed);
    ]

let identity_status_jsont : Encryption.identity_status Jsont.t =
  Jsont.enum
    [
      ("unverified", Encryption.Identity_unverified);
      ("verified", Encryption.Identity_verified);
      ("verification_violation", Encryption.Verification_violation);
    ]

let device_jsont : Encryption.device Jsont.t =
  Jsont.Object.(
    map (fun user_id device_id algorithms keys signatures dehydrated trust ->
        {
          Encryption.user_id;
          device_id;
          algorithms;
          keys;
          signatures;
          dehydrated;
          trust;
        })
    |> mem "user_id" Uid.jsont ~enc:(fun (d : Encryption.device) -> d.user_id)
    |> mem "device_id" Matrix_proto.Id.Device_id.jsont
         ~enc:(fun (d : Encryption.device) -> d.device_id)
    |> mem "algorithms"
         (Jsont.list Matrix_proto.Json.Codec.string)
         ~dec_absent:(fun () -> [])
         ~enc:(fun (d : Encryption.device) -> d.algorithms)
    |> mem "keys"
         (Keys.key_id_map Matrix_proto.Json.Codec.string)
         ~dec_absent:(fun () -> [])
         ~enc:(fun (d : Encryption.device) -> d.keys)
    |> mem "signatures" Keys.signatures_jsont
         ~dec_absent:(fun () -> [])
         ~enc:(fun (d : Encryption.device) -> d.signatures)
    |> opt_mem "dehydrated" Jsont.bool ~enc:(fun (d : Encryption.device) ->
        d.dehydrated)
    |> mem "trust" trust_jsont
         ~dec_absent:(fun () -> Encryption.Unverified)
         ~enc:(fun (d : Encryption.device) -> d.trust)
    |> finish)

let identity_jsont : Encryption.identity Jsont.t =
  Jsont.Object.(
    map
      (fun
        identity_user_id
        identity_master_key
        identity_self_signing_key
        identity_user_signing_key
        identity_was_previously_verified
        identity_status
        identity_pinned_master_key
      ->
        {
          Encryption.identity_user_id;
          identity_master_key;
          identity_self_signing_key;
          identity_user_signing_key;
          identity_was_previously_verified;
          identity_status;
          identity_pinned_master_key =
            Option.value identity_pinned_master_key ~default:identity_master_key;
        })
    |> mem "user_id" Uid.jsont ~enc:(fun (i : Encryption.identity) ->
        i.identity_user_id)
    |> mem "master_key" Matrix_proto.Json.Codec.string
         ~enc:(fun (i : Encryption.identity) -> i.identity_master_key)
    |> mem "self_signing_key" Matrix_proto.Json.Codec.string
         ~enc:(fun (i : Encryption.identity) -> i.identity_self_signing_key)
    |> opt_mem "user_signing_key" Matrix_proto.Json.Codec.string
         ~enc:(fun (i : Encryption.identity) -> i.identity_user_signing_key)
    |> mem "was_previously_verified" Jsont.bool
         ~dec_absent:(fun () -> false)
         ~enc:(fun (i : Encryption.identity) ->
           i.identity_was_previously_verified)
    |> mem "status" identity_status_jsont
         ~dec_absent:(fun () -> Encryption.Identity_unverified)
         ~enc:(fun (i : Encryption.identity) -> i.identity_status)
    |> opt_mem "pinned_master_key" Matrix_proto.Json.Codec.string
         ~enc:(fun (i : Encryption.identity) ->
           Some i.identity_pinned_master_key)
    |> finish)

let withheld_jsont : Encryption.withheld Jsont.t =
  Jsont.Object.(
    map
      (fun room_id session_id code reason sender_key from_device sender_user ->
        match (Rid.of_string room_id, Sid.of_string session_id) with
        | Ok room_id, Ok session_id ->
            {
              Encryption.room_id;
              session_id;
              code;
              reason;
              sender_key;
              from_device =
                Option.bind from_device (fun value ->
                    Result.to_option (Did.of_string value));
              sender_user;
            }
        | Error (`Msg m), _ | _, Error (`Msg m) ->
            Jsont.Error.msg Jsont.Meta.none m)
    |> mem "room_id" Matrix_proto.Json.Codec.string
         ~enc:(fun (w : Encryption.withheld) -> Rid.to_string w.room_id)
    |> mem "session_id" Matrix_proto.Json.Codec.string
         ~enc:(fun (w : Encryption.withheld) -> Sid.to_string w.session_id)
    |> mem "code" Matrix_proto.Json.Codec.string
         ~enc:(fun (w : Encryption.withheld) -> w.code)
    |> opt_mem "reason" Matrix_proto.Json.Codec.string
         ~enc:(fun (w : Encryption.withheld) -> w.reason)
    |> opt_mem "sender_key" Matrix_proto.Json.Codec.string
         ~enc:(fun (w : Encryption.withheld) -> w.sender_key)
    |> opt_mem "from_device" Matrix_proto.Json.Codec.string
         ~enc:(fun (w : Encryption.withheld) ->
           Option.map Did.to_string w.from_device)
    |> opt_mem "sender_user" Uid.jsont ~enc:(fun (w : Encryption.withheld) ->
        w.sender_user)
    |> finish)

let outbound_withheld_jsont : Encryption.outbound_withheld Jsont.t =
  Jsont.Object.(
    map
      (fun
        ow_room_id
        ow_session_id
        ow_user_id
        ow_device_id
        ow_code
        ow_txn_id
        ow_content
        ow_sent
      ->
        {
          Encryption.ow_room_id;
          ow_session_id;
          ow_user_id;
          ow_device_id;
          ow_code;
          ow_txn_id;
          ow_content;
          ow_sent;
        })
    |> mem "room_id" Rid.jsont ~enc:(fun (w : Encryption.outbound_withheld) ->
        w.ow_room_id)
    |> mem "session_id" Sid.jsont
         ~enc:(fun (w : Encryption.outbound_withheld) -> w.ow_session_id)
    |> mem "user_id" Uid.jsont ~enc:(fun (w : Encryption.outbound_withheld) ->
        w.ow_user_id)
    |> mem "device_id" Did.jsont ~enc:(fun (w : Encryption.outbound_withheld) ->
        w.ow_device_id)
    |> mem "code" Matrix_proto.Json.Codec.string
         ~enc:(fun (w : Encryption.outbound_withheld) -> w.ow_code)
    |> mem "txn_id" Matrix_proto.Json.Codec.string
         ~enc:(fun (w : Encryption.outbound_withheld) -> w.ow_txn_id)
    |> mem "content" Matrix_proto.Json.Codec.json
         ~enc:(fun (w : Encryption.outbound_withheld) -> w.ow_content)
    |> mem "sent" Jsont.bool
         ~dec_absent:(fun () -> false)
         ~enc:(fun (w : Encryption.outbound_withheld) -> w.ow_sent)
    |> finish)

(* Secret-gossip messages are cached after encryption.  They must be persisted
   as the exact JSON tree produced by Olm: regenerating them after a restart
   would advance the ratchet and make a retry under the same transaction ID
   invalid.  Unlike the wire codec, this codec validates all identifier keys
   while decoding, so corrupt state cannot silently target a different device. *)
let to_device_messages_jsont : To_device.messages Jsont.t =
  let raw =
    Json_codec.string_map (Json_codec.string_map Matrix_proto.Json.Codec.json)
  in
  Jsont.map
    ~dec:(fun entries ->
      List.map
        (fun (raw_user_id, raw_targets) ->
          let user_id =
            match Uid.of_string raw_user_id with
            | Ok user_id -> user_id
            | Error (`Msg m) -> Jsont.Error.msg Jsont.Meta.none m
          in
          let targets =
            List.map
              (fun (raw_recipient, content) ->
                let recipient =
                  if String.equal raw_recipient "*" then To_device.All
                  else
                    match Did.of_string raw_recipient with
                    | Ok device_id -> To_device.Device device_id
                    | Error (`Msg m) -> Jsont.Error.msg Jsont.Meta.none m
                in
                (recipient, content))
              raw_targets
          in
          (user_id, targets))
        entries)
    ~enc:(fun messages ->
      List.map
        (fun (user_id, targets) ->
          ( Uid.to_string user_id,
            List.map
              (fun (recipient, content) ->
                ( (match recipient with
                  | To_device.All -> "*"
                  | To_device.Device device_id -> Did.to_string device_id),
                  content ))
              targets ))
        messages)
    raw

let secret_cancel_jsont : Encryption.secret_cancel Jsont.t =
  Jsont.Object.(
    map (fun sc_txn_id sc_content sc_sent sc_messages ->
        { Encryption.sc_txn_id; sc_content; sc_sent; sc_messages })
    |> mem "txn_id" Matrix_proto.Json.Codec.string
         ~enc:(fun (c : Encryption.secret_cancel) -> c.sc_txn_id)
    |> mem "content" Matrix_proto.Json.Codec.json
         ~enc:(fun (c : Encryption.secret_cancel) -> c.sc_content)
    |> mem "sent" Jsont.bool
         ~dec_absent:(fun () -> false)
         ~enc:(fun (c : Encryption.secret_cancel) -> c.sc_sent)
    |> opt_mem "messages" to_device_messages_jsont
         ~enc:(fun (c : Encryption.secret_cancel) -> c.sc_messages)
    |> finish)

let secret_request_jsont : Encryption.secret_request Jsont.t =
  Jsont.Object.(
    map
      (fun
        sr_name
        sr_request_id
        sr_txn_id
        sr_content
        sr_sent
        sr_messages
        sr_cancel
      ->
        {
          Encryption.sr_name;
          sr_request_id;
          sr_txn_id;
          sr_content;
          sr_sent;
          sr_messages;
          sr_cancel;
        })
    |> mem "name" Matrix_proto.Json.Codec.string
         ~enc:(fun (r : Encryption.secret_request) -> r.sr_name)
    |> mem "request_id" Matrix_proto.Json.Codec.string
         ~enc:(fun (r : Encryption.secret_request) -> r.sr_request_id)
    |> mem "txn_id" Matrix_proto.Json.Codec.string
         ~enc:(fun (r : Encryption.secret_request) -> r.sr_txn_id)
    |> mem "content" Matrix_proto.Json.Codec.json
         ~enc:(fun (r : Encryption.secret_request) -> r.sr_content)
    |> mem "sent" Jsont.bool
         ~dec_absent:(fun () -> false)
         ~enc:(fun (r : Encryption.secret_request) -> r.sr_sent)
    |> opt_mem "messages" to_device_messages_jsont
         ~enc:(fun (r : Encryption.secret_request) -> r.sr_messages)
    |> opt_mem "cancel" secret_cancel_jsont
         ~enc:(fun (r : Encryption.secret_request) -> r.sr_cancel)
    |> finish)

let secret_send_jsont : Encryption.secret_send Jsont.t =
  Jsont.Object.(
    map
      (fun
        ss_request_id
        ss_user_id
        ss_device_id
        ss_txn_id
        ss_content
        ss_sent
        ss_messages
      ->
        {
          Encryption.ss_request_id;
          ss_user_id;
          ss_device_id;
          ss_txn_id;
          ss_content;
          ss_sent;
          ss_messages;
        })
    |> mem "request_id" Matrix_proto.Json.Codec.string
         ~enc:(fun (s : Encryption.secret_send) -> s.ss_request_id)
    |> mem "user_id" Uid.jsont ~enc:(fun (s : Encryption.secret_send) ->
        s.ss_user_id)
    |> mem "device_id" Did.jsont ~enc:(fun (s : Encryption.secret_send) ->
        s.ss_device_id)
    |> mem "txn_id" Matrix_proto.Json.Codec.string
         ~enc:(fun (s : Encryption.secret_send) -> s.ss_txn_id)
    |> mem "content" Matrix_proto.Json.Codec.json
         ~enc:(fun (s : Encryption.secret_send) -> s.ss_content)
    |> mem "sent" Jsont.bool
         ~dec_absent:(fun () -> false)
         ~enc:(fun (s : Encryption.secret_send) -> s.ss_sent)
    |> opt_mem "messages" to_device_messages_jsont
         ~enc:(fun (s : Encryption.secret_send) -> s.ss_messages)
    |> finish)

let room_settings_jsont : Encryption.room_settings Jsont.t =
  let d = Encryption.default_room_settings in
  Jsont.Object.(
    map (fun algorithm rotation_period_ms rotation_period_msgs ->
        { Encryption.algorithm; rotation_period_ms; rotation_period_msgs })
    |> mem "algorithm" Matrix_proto.Json.Codec.string
         ~dec_absent:(fun () -> d.algorithm)
         ~enc:(fun (r : Encryption.room_settings) -> r.algorithm)
    |> mem "rotation_period_ms" Matrix_proto.Json.Codec.Legacy.int64
         ~dec_absent:(fun () -> d.rotation_period_ms)
         ~enc:(fun (r : Encryption.room_settings) -> r.rotation_period_ms)
    |> mem "rotation_period_msgs" Matrix_proto.Json.Codec.Legacy.int
         ~dec_absent:(fun () -> d.rotation_period_msgs)
         ~enc:(fun (r : Encryption.room_settings) -> r.rotation_period_msgs)
    |> finish)

let backed_up_jsont : (Rid.t * Sid.t) list Jsont.t =
  Jsont.list (Jsont.list Matrix_proto.Json.Codec.string)
  |> Jsont.map
       ~dec:(fun value ->
         (List.filter_map (function
           | [ room; session ] -> (
               match (Rid.of_string room, Sid.of_string session) with
               | Ok r, Ok s -> Some (r, s)
               | _ -> None)
           | _ -> None))
           value)
       ~enc:(fun value ->
         (List.map (fun (r, s) -> [ Rid.to_string r; Sid.to_string s ])) value)

let downloaded_rooms_jsont : Rid.t list Jsont.t =
  Jsont.list Matrix_proto.Json.Codec.string
  |> Jsont.map
       ~dec:(fun value ->
         (List.filter_map (fun room -> Result.to_option (Rid.of_string room)))
           value)
       ~enc:(fun value -> (List.map Rid.to_string) value)

let decryption_key_jsont : Backup.Decryption_key.t Jsont.t =
  Jsont.map
    ~dec:(fun s ->
      match Backup.Decryption_key.of_base64 s with
      | Ok k -> k
      | Error (`Msg m) -> Jsont.Error.msg Jsont.Meta.none m)
    ~enc:Backup.Decryption_key.to_base64 Matrix_proto.Json.Codec.string

let backup_state_jsont : Encryption.backup_state Jsont.t =
  Jsont.Object.(
    map
      (fun
        version
        encryption_key
        decryption_key
        backed_up
        room_key_backups_fully_downloaded
      ->
        {
          Encryption.version;
          encryption_key;
          decryption_key;
          backed_up;
          room_key_backups_fully_downloaded;
        })
    |> opt_mem "version" Matrix_proto.Json.Codec.string
         ~enc:(fun (b : Encryption.backup_state) -> b.version)
    |> opt_mem "public_key" Crypto_key.Curve25519.Public.jsont
         ~enc:(fun (b : Encryption.backup_state) -> b.encryption_key)
    |> opt_mem "private_key" decryption_key_jsont
         ~enc:(fun (b : Encryption.backup_state) -> b.decryption_key)
    |> mem "backed_up" backed_up_jsont
         ~dec_absent:(fun () -> [])
         ~enc:(fun (b : Encryption.backup_state) -> b.backed_up)
    |> mem "room_key_backups_fully_downloaded" downloaded_rooms_jsont
         ~dec_absent:(fun () -> [])
         ~enc:(fun (b : Encryption.backup_state) ->
           b.room_key_backups_fully_downloaded)
    |> finish)

type raw_meta = {
  raw_room_id : string;
  raw_session_id : string;
  raw_chain : string list;
  raw_sender : string option;
  raw_sender_ed25519 : string option;
  raw_shared_history : bool;
  raw_legacy : bool;
}

let raw_meta_jsont =
  Jsont.Object.(
    map
      (fun
        raw_room_id
        raw_session_id
        raw_chain
        raw_sender
        raw_sender_ed25519
        raw_shared_history
        raw_legacy
      ->
        {
          raw_room_id;
          raw_session_id;
          raw_chain;
          raw_sender;
          raw_sender_ed25519;
          raw_shared_history;
          raw_legacy;
        })
    |> mem "room_id" Matrix_proto.Json.Codec.string ~enc:(fun m ->
        m.raw_room_id)
    |> mem "session_id" Matrix_proto.Json.Codec.string ~enc:(fun m ->
        m.raw_session_id)
    |> mem "forwarding_curve25519_key_chain"
         (Jsont.list Matrix_proto.Json.Codec.string)
         ~dec_absent:(fun () -> [])
         ~enc:(fun m -> m.raw_chain)
    |> opt_mem "sender" Matrix_proto.Json.Codec.string ~enc:(fun m ->
        m.raw_sender)
    |> opt_mem "sender_ed25519" Matrix_proto.Json.Codec.string ~enc:(fun m ->
        m.raw_sender_ed25519)
    |> mem "shared_history" Jsont.bool
         ~dec_absent:(fun () -> false)
         ~enc:(fun m -> m.raw_shared_history)
    |> mem "legacy" Jsont.bool
         ~dec_absent:(fun () -> true)
         ~enc:(fun m -> m.raw_legacy)
    |> finish)

let session_meta_jsont : Encryption.Session_meta.t list Jsont.t =
  Jsont.list raw_meta_jsont
  |> Jsont.map
       ~dec:(fun value ->
         (List.filter_map (fun m ->
              match
                (Rid.of_string m.raw_room_id, Sid.of_string m.raw_session_id)
              with
              | Ok room_id, Ok session_id ->
                  Some
                    {
                      Encryption.Session_meta.room_id;
                      session_id;
                      forwarding_chain = m.raw_chain;
                      sender =
                        Option.bind m.raw_sender (fun s ->
                            Result.to_option (Uid.of_string s));
                      sender_ed25519 = m.raw_sender_ed25519;
                      shared_history = m.raw_shared_history;
                      legacy = m.raw_legacy;
                    }
              | _ -> None))
           value)
       ~enc:(fun value ->
         (List.map (fun (m : Encryption.Session_meta.t) ->
              {
                raw_room_id = Rid.to_string m.room_id;
                raw_session_id = Sid.to_string m.session_id;
                raw_chain = m.forwarding_chain;
                raw_sender = Option.map Uid.to_string m.sender;
                raw_sender_ed25519 = m.sender_ed25519;
                raw_shared_history = m.shared_history;
                raw_legacy = m.legacy;
              }))
           value)

let pending_key_bundle_jsont : Encryption.pending_key_bundle Jsont.t =
  Jsont.Object.(
    map (fun room_id inviter invite_accepted_at ->
        { Encryption.room_id; inviter; invite_accepted_at })
    |> mem "room_id" Rid.jsont ~enc:(fun (p : Encryption.pending_key_bundle) ->
        p.room_id)
    |> mem "inviter" Uid.jsont ~enc:(fun (p : Encryption.pending_key_bundle) ->
        p.inviter)
    |> mem "invite_accepted_at" Json_codec.ptime
         ~enc:(fun (p : Encryption.pending_key_bundle) -> p.invite_accepted_at)
    |> finish)

let received_key_bundle_jsont : Encryption.received_key_bundle Jsont.t =
  Jsont.Object.(
    map
      (fun
        sender
        sender_key
        sender_ed25519
        (content : Encryption.room_key_bundle_content)
      ->
        match Curve25519.Public.of_base64 sender_key with
        | Ok sender_key ->
            {
              Encryption.room_id = content.room_id;
              sender;
              sender_key;
              sender_ed25519;
              file = content.file;
            }
        | Error (`Msg m) -> Jsont.Error.msg Jsont.Meta.none m)
    |> mem "sender" Uid.jsont ~enc:(fun (b : Encryption.received_key_bundle) ->
        b.sender)
    |> mem "sender_key" Matrix_proto.Json.Codec.string
         ~enc:(fun (b : Encryption.received_key_bundle) ->
           Curve25519.Public.to_base64 b.sender_key)
    |> mem "sender_ed25519" Matrix_proto.Json.Codec.string
         ~enc:(fun (b : Encryption.received_key_bundle) -> b.sender_ed25519)
    |> mem "content" Encryption.room_key_bundle_content_jsont
         ~enc:(fun (b : Encryption.received_key_bundle) ->
           { Encryption.room_id = b.room_id; file = b.file })
    |> finish)

let nonnegative_int_jsont =
  Jsont.iter
    ~dec:(fun value ->
      if value < 0 then
        Jsont.Error.msg Jsont.Meta.none "value must be a non-negative integer")
    Matrix_proto.Json.Codec.Legacy.int

let state_jsont : Encryption.state Jsont.t =
  Jsont.Object.(
    map
      (fun
        devices
        tracked_users
        outdated_users
        rooms
        backup
        session_meta
        trust_requirement
        identities
        withheld
        pending_key_bundles
        received_key_bundles
        outbound_withheld
        secrets
        secret_requests
        secret_sends
        fallback_key_created_at
        fallback_key_pending
        published_one_time_keys
        uploaded_one_time_key_count
        device_keys_uploaded
        dehydrated_pickle_key
        last_uploaded_device_id
      ->
        {
          Encryption.devices;
          tracked_users;
          outdated_users;
          rooms;
          backup;
          session_meta;
          trust_requirement;
          identities;
          withheld;
          pending_key_bundles;
          received_key_bundles;
          outbound_withheld;
          secrets;
          secret_requests;
          secret_sends;
          fallback_key_created_at;
          fallback_key_pending;
          published_one_time_keys;
          uploaded_one_time_key_count;
          device_keys_uploaded;
          dehydrated_pickle_key;
          last_uploaded_device_id;
        })
    |> mem "devices" (Jsont.list device_jsont)
         ~dec_absent:(fun () -> [])
         ~enc:(fun (s : Encryption.state) -> s.devices)
    |> mem "tracked_users" (Jsont.list Uid.jsont)
         ~dec_absent:(fun () -> [])
         ~enc:(fun (s : Encryption.state) -> s.tracked_users)
    |> mem "outdated_users" (Jsont.list Uid.jsont)
         ~dec_absent:(fun () -> [])
         ~enc:(fun (s : Encryption.state) -> s.outdated_users)
    |> mem "rooms"
         (id_map_jsont ~of_string:Rid.of_string ~to_string:Rid.to_string
            room_settings_jsont)
         ~dec_absent:(fun () -> [])
         ~enc:(fun (s : Encryption.state) -> s.rooms)
    |> mem "backup" backup_state_jsont
         ~dec_absent:(fun () ->
           {
             Encryption.version = None;
             encryption_key = None;
             decryption_key = None;
             backed_up = [];
             room_key_backups_fully_downloaded = [];
           })
         ~enc:(fun (s : Encryption.state) -> s.backup)
    |> mem "session_meta" session_meta_jsont
         ~dec_absent:(fun () -> [])
         ~enc:(fun (s : Encryption.state) -> s.session_meta)
    |> mem "trust_requirement" trust_requirement_jsont
         ~dec_absent:(fun () -> Encryption.Untrusted)
         ~enc:(fun (s : Encryption.state) -> s.trust_requirement)
    |> mem "identities"
         (Jsont.list identity_jsont)
         ~dec_absent:(fun () -> [])
         ~enc:(fun (s : Encryption.state) -> s.identities)
    |> mem "withheld"
         (Jsont.list withheld_jsont)
         ~dec_absent:(fun () -> [])
         ~enc:(fun (s : Encryption.state) -> s.withheld)
    |> mem "pending_key_bundles"
         (Jsont.list pending_key_bundle_jsont)
         ~dec_absent:(fun () -> [])
         ~enc:(fun (s : Encryption.state) -> s.pending_key_bundles)
    |> mem "received_key_bundles"
         (Jsont.list received_key_bundle_jsont)
         ~dec_absent:(fun () -> [])
         ~enc:(fun (s : Encryption.state) -> s.received_key_bundles)
    |> mem "outbound_withheld"
         (Jsont.list outbound_withheld_jsont)
         ~dec_absent:(fun () -> [])
         ~enc:(fun (s : Encryption.state) -> s.outbound_withheld)
    |> mem "secrets"
         (Json_codec.string_map Matrix_proto.Json.Codec.string)
         ~dec_absent:(fun () -> [])
         ~enc:(fun (s : Encryption.state) -> s.secrets)
    |> mem "secret_requests"
         (Jsont.list secret_request_jsont)
         ~dec_absent:(fun () -> [])
         ~enc:(fun (s : Encryption.state) -> s.secret_requests)
    |> mem "secret_sends"
         (Jsont.list secret_send_jsont)
         ~dec_absent:(fun () -> [])
         ~enc:(fun (s : Encryption.state) -> s.secret_sends)
    |> opt_mem "fallback_key_created_at" Json_codec.ptime
         ~enc:(fun (s : Encryption.state) -> s.fallback_key_created_at)
    |> mem "fallback_key_pending" Jsont.bool
         ~dec_absent:(fun () -> false)
         ~enc:(fun (s : Encryption.state) -> s.fallback_key_pending)
    |> mem "published_one_time_keys"
         (Jsont.list Matrix_proto.Json.Codec.string)
         ~dec_absent:(fun () -> [])
         ~enc:(fun (s : Encryption.state) -> s.published_one_time_keys)
    |> mem "uploaded_one_time_key_count" nonnegative_int_jsont
         ~dec_absent:(fun () -> 0)
         ~enc:(fun (s : Encryption.state) -> s.uploaded_one_time_key_count)
    |> mem "device_keys_uploaded" Jsont.bool
         ~dec_absent:(fun () -> false)
         ~enc:(fun (s : Encryption.state) -> s.device_keys_uploaded)
    |> opt_mem "dehydrated_pickle_key" Matrix_proto.Json.Codec.string
         ~enc:(fun (s : Encryption.state) -> s.dehydrated_pickle_key)
    |> opt_mem "last_uploaded_device_id" Did.jsont
         ~enc:(fun (s : Encryption.state) -> s.last_uploaded_device_id)
    |> finish)

type t = { store : Profile_store.t; mutable generation : int64 }

let generation_file = ".crypto_generation"
let generation_path t = Eio.Path.(Profile_store.dir t.store / generation_file)

let read_generation t =
  let path = generation_path t in
  Io_context.with_context "reading Matrix crypto-store generation" (fun () ->
      if not (Eio.Path.is_file path) then Ok 0L
      else
        try
          let generation = Int64.of_string (String.trim (Eio.Path.load path)) in
          if generation < 0L then
            Error (Error.Json_error "crypto generation marker is negative")
          else Ok generation
        with Failure _ ->
          Error (Error.Json_error "crypto generation marker is invalid"))

let generation_at_create store =
  let t = { store; generation = 0L } in
  match read_generation t with
  | Ok generation -> generation
  | Error error ->
      Log.warn (fun m ->
          m "Ignoring invalid crypto generation marker: %s"
            (Error.to_string error));
      0L

let create ~xdg ~profile =
  let store = Profile_store.create ~xdg ~profile in
  { store; generation = generation_at_create store }

let state_path t = Eio.Path.(Profile_store.dir t.store / "crypto_state.json")

(* A file that will not parse still means the profile has an account. Saying
   otherwise would have the caller generate a fresh one and lose the ability
   to read everything this device has already received. *)
let exists t =
  Eio.Path.is_file
    Eio.Path.(Profile_store.dir t.store / ".crypto_transaction.json")
  ||
  match read_generation t with
  | Error _ -> true
  | Ok generation when Int64.rem generation 2L <> 0L -> true
  | Ok _ -> (
      match Profile_store.load_device_keys t.store with
      | Ok None -> false
      | Ok (Some _) | Error _ -> true)

let load_state t =
  let path = state_path t in
  Io_context.with_context "loading Matrix crypto state" (fun () ->
      if not (Eio.Path.is_file path) then Encryption.empty_state
      else
        let file =
          Option.value (Eio.Path.native path) ~default:Jsont.Textloc.file_none
        in
        match
          Jsont_bytesrw.decode_string ~file state_jsont (Eio.Path.load path)
        with
        | Ok v -> v
        | Error msg ->
            Log.warn (fun m ->
                m "Ignoring unreadable crypto state %a: %s" Eio.Path.pp path msg);
            Encryption.empty_state)

let save_generation t generation =
  let path = generation_path t in
  Profile_store.atomic_write ~path ~data:(Int64.to_string generation ^ "\n")

let generation_in_progress generation = Int64.rem generation 2L <> 0L

(* The journal contains a complete redo image before any component changes.
   Its fixed file order cannot name paths outside the profile. The even target
   generation commits all replacements; a surviving journal at that generation
   needs only cleanup, never replay over subsequently modified files. *)
type transaction = { generation : int64; clear : bool; files : string list }

let transaction_jsont =
  Jsont.Object.(
    map (fun generation clear files -> { generation; clear; files })
    |> mem "generation" Matrix_proto.Json.Codec.Legacy.int64 ~enc:(fun t ->
        t.generation)
    |> mem "clear" Jsont.bool ~enc:(fun t -> t.clear)
    |> mem "files" (Jsont.list Matrix_proto.Json.Codec.string) ~enc:(fun t ->
        t.files)
    |> finish)

let transaction_file = ".crypto_transaction.json"
let transaction_path t = Eio.Path.(Profile_store.dir t.store / transaction_file)

let snapshot_files =
  [
    "device.json";
    "one_time_keys.json";
    "olm_sessions.json";
    "megolm_inbound.json";
    "megolm_outbound.json";
    "crypto_state.json";
  ]

let decode_file codec contents =
  match Jsont_bytesrw.decode_string codec contents with
  | Ok _ -> Ok ()
  | Error _ -> json_err "crypto transaction contains an invalid snapshot file"

let validate_transaction transaction =
  if
    transaction.generation <= 0L
    || generation_in_progress transaction.generation
  then json_err "crypto transaction has an invalid generation"
  else if transaction.clear then
    if transaction.files = [] then Ok ()
    else json_err "invalid crypto clear transaction"
  else
    match transaction.files with
    | [ device; otk; olm; inbound; outbound; state ] ->
        let* () = decode_file Session.Device_keys.jsont device in
        let* () = decode_file Session.One_time_keys_file.jsont otk in
        let* () = decode_file Session.Olm_sessions_file.jsont olm in
        let* () = decode_file Session.Megolm_inbound_file.jsont inbound in
        let* () = decode_file Session.Megolm_outbound_file.jsont outbound in
        decode_file state_jsont state
    | _ -> json_err "crypto transaction is missing snapshot files"

let apply_transaction t transaction =
  let* () =
    if transaction.clear then begin
      List.iter
        (fun name ->
          Eio.Path.unlink ~missing_ok:true
            Eio.Path.(Profile_store.dir t.store / name))
        ("session.json" :: snapshot_files);
      Ok ()
    end
    else
      List.fold_left2
        (fun result name data ->
          let* () = result in
          Profile_store.atomic_write
            ~path:Eio.Path.(Profile_store.dir t.store / name)
            ~data)
        (Ok ()) snapshot_files transaction.files
  in
  save_generation t transaction.generation

let recover_transaction t =
  let path = transaction_path t in
  if not (Eio.Path.is_file path) then Ok ()
  else
    let* transaction =
      match
        Jsont_bytesrw.decode_string transaction_jsont (Eio.Path.load path)
      with
      | Ok transaction -> Ok transaction
      | Error _ -> json_err "crypto transaction journal is unreadable"
    in
    let* () = validate_transaction transaction in
    let* current = read_generation t in
    let* () =
      if current = transaction.generation then Ok ()
      else if
        current = Int64.pred transaction.generation
        || current = Int64.sub transaction.generation 2L
      then apply_transaction t transaction
      else
        Error
          (Error.Policy_denied
             "crypto transaction generation does not match the store")
    in
    Eio.Path.unlink ~missing_ok:true path;
    Ok ()

let commit_transaction t ~clear files =
  let* () = recover_transaction t in
  let* current = read_generation t in
  if current <> t.generation then
    Error
      (Error.Policy_denied "crypto state changed since this store was opened")
  else if current > Int64.sub Int64.max_int 2L then
    Error (Error.Policy_denied "crypto generation marker exhausted")
  else
    let generation =
      if generation_in_progress current then Int64.succ current
      else Int64.add current 2L
    in
    let transaction = { generation; clear; files } in
    let* () = validate_transaction transaction in
    let* data =
      match Jsont_bytesrw.encode_string transaction_jsont transaction with
      | Ok data -> Ok data
      | Error _ -> json_err "could not encode crypto transaction"
    in
    let* () = Profile_store.atomic_write ~path:(transaction_path t) ~data in
    t.generation <- generation;
    let* () = save_generation t (Int64.pred generation) in
    let* () = apply_transaction t transaction in
    Eio.Path.unlink ~missing_ok:true (transaction_path t);
    Ok ()

let now () = Ptime_clock.now ()
let b64 = Matrix_proto.Base64.encode

let unb64 what s =
  match Matrix_proto.Base64.decode s with
  | Ok v -> Ok v
  | Error (`Msg m) -> json_err (Printf.sprintf "%s: bad base64: %s" what m)

let device_keys_of_account a : Session.Device_keys.t =
  let p = Olm.Account.to_pickle a in
  let ed, curve = Olm.Account.identity_keys a in
  {
    ed25519_public = Crypto_key.Ed25519.Public.to_base64 ed;
    ed25519_private = b64 (Crypto_key.Ed25519.Private.to_bytes p.ed25519);
    curve25519_public = Crypto_key.Curve25519.Public.to_base64 curve;
    curve25519_private =
      b64 (Crypto_key.Curve25519.Secret.to_bytes p.curve25519);
    uploaded_at = None;
    algorithms =
      Matrix_proto.Event.Encryption_algorithm.
        [ to_string Olm_v1_curve25519_aes_sha2; to_string Megolm_v1_aes_sha2 ];
  }

let one_time_key_of (k : Olm.Account.stored_key) : Session.One_time_key.t =
  {
    key_id = k.key_id;
    public =
      Crypto_key.Curve25519.Public.to_base64
        (Crypto_key.Curve25519.Secret.public k.secret);
    private_ = b64 (Crypto_key.Curve25519.Secret.to_bytes k.secret);
    created_at = now ();
  }

let one_time_keys_of_account a : Session.One_time_keys_file.t =
  let p = Olm.Account.to_pickle a in
  {
    target_count = Olm.Account.max_one_time_keys a;
    last_upload_at = None;
    next_key_id = p.next_key_id;
    keys = List.map one_time_key_of p.stored_one_time_keys;
    fallback = Option.map one_time_key_of p.stored_fallback_key;
    previous_fallback =
      Option.map one_time_key_of p.stored_previous_fallback_key;
    fallback_used = false;
  }

let curve25519_secret what s =
  let* bytes = unb64 what s in
  match Crypto_key.Curve25519.Secret.of_bytes bytes with
  | Ok k -> Ok k
  | Error (`Msg m) -> json_err (Printf.sprintf "%s: %s" what m)

let otk_of_file (k : Session.One_time_key.t) =
  let* secret = curve25519_secret "one-time key" k.private_ in
  Ok { Olm.Account.key_id = k.key_id; secret }

let account_of_files (dk : Session.Device_keys.t)
    (otk : Session.One_time_keys_file.t option) =
  let* ed25519_bytes = unb64 "device signing key" dk.ed25519_private in
  let* ed25519 =
    match Crypto_key.Ed25519.Private.of_stored_bytes ed25519_bytes with
    | Ok k -> Ok k
    | Error (`Msg m) -> json_err ("device signing key: " ^ m)
  in
  let* curve25519 = curve25519_secret "identity key" dk.curve25519_private in
  let otk =
    Option.value otk
      ~default:
        Session.One_time_keys_file.
          {
            target_count = 50;
            last_upload_at = None;
            next_key_id = 0;
            keys = [];
            fallback = None;
            previous_fallback = None;
            fallback_used = false;
          }
  in
  (* A one-time key that fails to parse is dropped rather than failing the
     whole load, the same policy [load] already applies to Olm and Megolm
     sessions: one corrupt record should not cost the device its identity. *)
  let drop_bad what (k : Session.One_time_key.t) =
    match otk_of_file k with
    | Ok kv -> Some kv
    | Error e ->
        Log.warn (fun m ->
            m "Dropping unreadable %s %s: %s" what k.key_id (Error.to_string e));
        None
  in
  let stored_one_time_keys =
    List.filter_map (drop_bad "one-time key") otk.keys
  in
  let stored_fallback_key =
    Option.bind otk.fallback (drop_bad "fallback key")
  in
  let stored_previous_fallback_key =
    Option.bind otk.previous_fallback (drop_bad "previous fallback key")
  in
  Ok
    (Olm.Account.of_pickle
       {
         ed25519;
         curve25519;
         stored_one_time_keys;
         stored_fallback_key;
         stored_previous_fallback_key;
         next_key_id = otk.next_key_id;
         max_one_time_keys =
           (if otk.target_count > 0 then otk.target_count else 50);
       })

(* A session that fails to pickle is dropped from what gets saved, and
   logged, rather than written with an empty [pickle] that would silently
   overwrite a previously-good record with an unreadable one. *)
let olm_session_record (s : Olm.Session.t) : Session.Olm_session.t option =
  match Session_pickle.pickle_session s with
  | Error (`Msg msg) ->
      Log.warn (fun m ->
          m "Not saving Olm session %s: %s" (Olm.Session.session_id s) msg);
      None
  | Ok pickle ->
      let created = Olm.Session.creation_time s in
      let last_used = Olm.Session.last_used_at s in
      Some
        {
          Session.Olm_session.their_identity_key =
            Crypto_key.Curve25519.Public.to_base64
              (Olm.Session.their_identity_key s);
          session_id = Olm.Session.session_id s;
          pickle;
          created_at = created;
          last_used_at = last_used;
        }

let megolm_inbound_record (s : Olm.Megolm.Inbound.t) :
    Session.Megolm_inbound.t option =
  match Session_pickle.pickle_megolm_inbound s with
  | Error (`Msg msg) ->
      Log.warn (fun m ->
          m "Not saving inbound Megolm session %s: %s"
            (Sid.to_string (Olm.Megolm.Inbound.session_id s))
            msg);
      None
  | Ok pickle ->
      Some
        {
          Session.Megolm_inbound.room_id = Olm.Megolm.Inbound.room_id s;
          session_id = Sid.to_string (Olm.Megolm.Inbound.session_id s);
          sender_key =
            Crypto_key.Curve25519.Public.to_base64
              (Olm.Megolm.Inbound.sender_key s);
          signing_key =
            Crypto_key.Ed25519.Public.to_base64
              (Olm.Megolm.Inbound.signing_key s);
          pickle;
          first_known_index = Olm.Megolm.Inbound.first_known_index s;
          created_at = Olm.Megolm.Inbound.creation_time s;
        }

let megolm_outbound_record (s : Olm.Megolm.Outbound.t) :
    Session.Megolm_outbound.t option =
  match Session_pickle.pickle_megolm_outbound s with
  | Error (`Msg msg) ->
      Log.warn (fun m ->
          m "Not saving outbound Megolm session %s: %s"
            (Sid.to_string (Olm.Megolm.Outbound.session_id s))
            msg);
      None
  | Ok pickle ->
      let created = Olm.Megolm.Outbound.creation_time s in
      Some
        {
          Session.Megolm_outbound.room_id = Olm.Megolm.Outbound.room_id s;
          session_id = Sid.to_string (Olm.Megolm.Outbound.session_id s);
          pickle;
          message_index = Olm.Megolm.Outbound.message_index s;
          created_at = created;
          message_count = Olm.Megolm.Outbound.message_count s;
          max_age_ms =
            Int64.of_float
              (Ptime.Span.to_float_s (Olm.Megolm.Outbound.rotation_period s)
              *. 1000.);
          shared_with =
            List.map
              (fun (user_id, device_id) ->
                { Session.Shared_with.user_id; device_id; shared_at = created })
              (Olm.Megolm.Outbound.shared_with s);
        }

let load_unlocked t =
  let* () = recover_transaction t in
  let* generation = read_generation t in
  if generation_in_progress generation then
    Error
      (Error.Policy_denied
         "crypto snapshot update was interrupted; save or clear it again")
  else
    let* device_keys = Profile_store.load_device_keys t.store in
    match device_keys with
    | None ->
        t.generation <- generation;
        Ok None
    | Some dk ->
        let* one_time_keys = Profile_store.load_one_time_keys t.store in
        let* account = account_of_files dk one_time_keys in
        let unpickle what f l =
          List.filter_map
            (fun x ->
              match f x with
              | Ok v -> Some v
              | Error (`Msg msg) ->
                  Log.warn (fun m -> m "Dropping unreadable %s: %s" what msg);
                  None)
            l
        in
        let* olm = Profile_store.load_olm_sessions t.store in
        let olm_sessions =
          match olm with
          | None -> []
          | Some f ->
              unpickle "Olm session"
                (fun (s : Session.Olm_session.t) ->
                  Session_pickle.unpickle_session s.pickle)
                f.sessions
        in
        let* inbound = Profile_store.load_megolm_inbound t.store in
        let megolm_inbound =
          match inbound with
          | None -> []
          | Some f ->
              unpickle "inbound Megolm session"
                (fun (s : Session.Megolm_inbound.t) ->
                  Session_pickle.unpickle_megolm_inbound s.pickle)
                f.sessions
        in
        let* outbound = Profile_store.load_megolm_outbound t.store in
        let megolm_outbound =
          match outbound with
          | None -> []
          | Some f ->
              unpickle "outbound Megolm session"
                (fun (s : Session.Megolm_outbound.t) ->
                  Session_pickle.unpickle_megolm_outbound s.pickle)
                f.sessions
        in
        t.generation <- generation;
        Ok
          (Some
             {
               Encryption.account;
               olm_sessions;
               megolm_inbound;
               megolm_outbound;
               state = load_state t;
             })

let load t =
  match Profile_store.with_lock t.store (fun () -> load_unlocked t) with
  | Ok result -> result
  | Error error -> Error error

let save_unlocked t (s : snapshot) =
  let encode codec value =
    match Jsont_bytesrw.encode_string ~format:Jsont.Indent codec value with
    | Ok data -> Ok data
    | Error _ -> json_err "could not encode crypto snapshot"
  in
  let* device =
    encode Session.Device_keys.jsont (device_keys_of_account s.account)
  in
  let* otk =
    encode Session.One_time_keys_file.jsont (one_time_keys_of_account s.account)
  in
  let* olm =
    encode Session.Olm_sessions_file.jsont
      { sessions = List.filter_map olm_session_record s.olm_sessions }
  in
  let* inbound =
    encode Session.Megolm_inbound_file.jsont
      { sessions = List.filter_map megolm_inbound_record s.megolm_inbound }
  in
  let* outbound =
    encode Session.Megolm_outbound_file.jsont
      { sessions = List.filter_map megolm_outbound_record s.megolm_outbound }
  in
  let* state = encode state_jsont s.state in
  commit_transaction t ~clear:false
    [ device; otk; olm; inbound; outbound; state ]

let save t s =
  match
    Profile_store.with_lock t.store (fun () ->
        Io_context.with_context "saving Matrix crypto state" (fun () ->
            save_unlocked t s))
  with
  | Ok result -> result
  | Error error -> Error error

let clear_unlocked t =
  Io_context.with_context "clearing Matrix crypto state" (fun () ->
      commit_transaction t ~clear:true [])

let clear t =
  match Profile_store.with_lock t.store (fun () -> clear_unlocked t) with
  | Ok result -> result
  | Error error -> Error error
