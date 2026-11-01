module E = Matrix_client.Encryption
module D = Matrix_client.Encryption_driver

type t = D.t
type trust = E.trust = Unverified | Verified | Blacklisted

type identity_status = E.identity_status =
  | Identity_unverified
  | Identity_verified
  | Verification_violation

type device = E.device = {
  user_id : Matrix_proto.Id.User_id.t;
  device_id : Matrix_proto.Id.Device_id.t;
  algorithms : string list;
  keys : (Matrix_client.Crypto_key.Key_id.t * string) list;
  signatures : Matrix_client.Keys.signatures;
  dehydrated : bool option;
  trust : trust;
}

type room_settings = E.room_settings = {
  algorithm : string;
  rotation_period_ms : int64;
  rotation_period_msgs : int;
}

type request = E.request
type to_device_event = E.to_device_event
type outcome = E.outcome
type room_key_bundle_outcome = D.room_key_bundle_outcome

type share_room_history_outcome = D.share_room_history_outcome =
  | History_not_shared_visibility
  | History_not_shared_identity
  | History_no_keys
  | History_shared of int

type share_room_history_error = D.share_room_history_error =
  | Share_encryption_error of Matrix_client.Error.t
  | Share_media_error of Matrix_client.Media.encrypted_error

type invite_outcome = D.invite_outcome =
  | Invite_sent of share_room_history_outcome

type invite_error = D.invite_error =
  | Invite_share_error of share_room_history_error
  | Invite_request_error of Matrix_client.Error.t

type decrypted_event = E.decrypted_event
type decrypt_error = E.decrypt_error

let machine = D.machine

let create ~random ~user_id ~device_id ?store () =
  Error.with_context "creating Matrix encryption state" (fun () ->
      Error.unwrap (D.create ~random ~user_id ~device_id ?store ()))

let create_with_account ~random ~user_id ~device_id ~account ?store () =
  Error.with_context "creating Matrix encryption state from an account"
    (fun () ->
      Error.unwrap
        (D.create_with_account ~random ~user_id ~device_id ~account ?store ()))

let of_env env ~user_id ~device_id ?store () =
  create ~random:(Matrix_client.Random.of_env env) ~user_id ~device_id ?store ()

let save t =
  Error.with_context "saving Matrix encryption state" (fun () ->
      Error.unwrap (D.save t))

let user_id t = E.user_id (machine t)
let device_id t = E.device_id (machine t)
let identity_keys t = E.identity_keys (machine t)
let sign t json = E.sign (machine t) json
let device_keys_for_upload t = E.device_keys_for_upload (machine t)
let device_key = E.device_key
let device_ed25519 = E.device_ed25519
let device_curve25519 = E.device_curve25519
let snapshot t = E.snapshot (machine t)
let track_users t users = E.track_users (machine t) users
let untrack_users t users = E.untrack_users (machine t) users
let tracked_users t = E.tracked_users (machine t)
let outdated_users t = E.outdated_users (machine t)
let devices_of t u = E.devices_of (machine t) u
let find_device t u ~device_id = E.find_device (machine t) u ~device_id
let find_device_by_curve25519 t k = E.find_device_by_curve25519 (machine t) k
let identity_master_key t u = E.identity_master_key (machine t) u
let identity_self_signing_key t u = E.identity_self_signing_key (machine t) u
let identity_user_signing_key t u = E.identity_user_signing_key (machine t) u
let identity_status t u = E.identity_status (machine t) u
let identity_has_pin_violation t u = E.identity_has_pin_violation (machine t) u
let pin_user_identity t u = E.pin_user_identity (machine t) u
let trust_user_identity t u = E.trust_user_identity (machine t) u

let set_device_trust t u ~device_id trust =
  E.set_device_trust (machine t) u ~device_id trust

let receive_keys_query t resp = E.receive_keys_query (machine t) resp
let receive_keys_claim t resp = E.receive_keys_claim (machine t) resp
let enable_room_encryption = E.enable_room_encryption
let room_encryption_content = E.room_encryption_content

let set_room_encryption_settings t room content =
  Error.unwrap ~context:"setting room encryption parameters"
    (E.set_room_encryption_settings (machine t) room content)

let find_room_settings t room = E.find_room_settings (machine t) room
let is_room_encrypted t room = E.is_room_encrypted (machine t) room
let outgoing_requests t = E.outgoing_requests (machine t)
let pp_request = E.pp_request

let execute_requests ?on_error t client requests =
  Error.with_context "executing Matrix encryption requests" (fun () ->
      D.execute_requests
        ?on_error:(Option.map (fun f e -> f (Error.of_client_error e)) on_error)
        t (Client.base client) requests)

let accept_received_room_key_bundle ?now t client ~joined bundle =
  Error.with_context "accepting a received room-key bundle" (fun () ->
      D.accept_received_room_key_bundle ?now t (Client.base client) ~joined
        bundle)

let process_sync t response = E.process_sync (machine t) response

let process_sliding_sync t response =
  E.process_sliding_sync (machine t) response

let pp_to_device_event = E.pp_to_device_event

let sync_hook ?on_error t client response =
  Error.with_context "processing encryption work from sync" (fun () ->
      D.sync_hook
        ?on_error:(Option.map (fun f e -> f (Error.of_client_error e)) on_error)
        t (Client.base client) response)

let sync_hook_sliding ?on_error t client response =
  Error.with_context "processing encryption work from sliding sync" (fun () ->
      D.sync_hook_sliding
        ?on_error:(Option.map (fun f e -> f (Error.of_client_error e)) on_error)
        t (Client.base client) response)

let decrypt_room_event t room event =
  E.decrypt_room_event (machine t) room event

let pp_decrypt_error = E.pp_decrypt_error

let request_room_key t ~room_id ~session_id ?sender_key () =
  E.request_room_key (machine t) ~room_id ~session_id ?sender_key ()

let encrypt_room_event t client room ~event_type ~content ~members =
  Error.unwrap ~context:"encrypting a Matrix room event"
    (D.encrypt_room_event t (Client.base client) room ~event_type ~content
       ~members)

let send_encrypted t client room ~event_type ~content ~members =
  Error.unwrap ~context:"sending an encrypted Matrix room event"
    (D.send_encrypted t (Client.base client) room ~event_type ~content ~members)

let send_encrypted_text t client room ~body ~members =
  let name n = (n, Jsont.Meta.none) in
  let content =
    Jsont.Json.object'
      [
        Jsont.Json.mem (name "msgtype") (Jsont.Json.string "m.text");
        Jsont.Json.mem (name "body") (Jsont.Json.string body);
      ]
  in
  send_encrypted t client room ~event_type:"m.room.message" ~content ~members

let enable_backup t ~version ?decryption_key key =
  E.enable_backup (machine t) ~version ?decryption_key key

let disable_backup t = E.disable_backup (machine t)
let backup_version t = E.backup_version (machine t)
let backup_pending_count t = E.backup_pending_count (machine t)

let backup_pending t client =
  Error.with_context "backing up pending room keys" (fun () ->
      Error.unwrap (D.backup_pending t (Client.base client)))

let restore_from_backup t client =
  Error.with_context "restoring room keys from backup" (fun () ->
      Error.unwrap (D.restore_from_backup t (Client.base client)))

let restore_room_from_backup t client room_id =
  Error.with_context "restoring a room from key backup" (fun () ->
      Error.unwrap (D.restore_room_from_backup t (Client.base client) room_id))

let restore_session_from_backup t client ~room_id ~session_id =
  Error.with_context "restoring a session from room-key backup" (fun () ->
      Error.unwrap
        (D.restore_session_from_backup t (Client.base client) ~room_id
           ~session_id))

let share_room_history t client ~room_id ~recipient ~history_visibility =
  Error.with_context "sharing encrypted room history" (fun () ->
      D.share_room_history t (Client.base client) ~room_id ~recipient
        ~history_visibility)

let invite_user_by_id t client ~room_id ~user_id ?reason ~history_visibility ()
    =
  Error.with_context "sharing room history and inviting a user" (fun () ->
      D.invite_user_by_id t (Client.base client) ~room_id ~user_id ?reason
        ~history_visibility ())

let inbound_sessions t = E.inbound_sessions (machine t)

let has_inbound_session t room ~session_id =
  E.has_inbound_session (machine t) room ~session_id

let outbound_session_id t room = E.outbound_session_id (machine t) room
let outbound_message_count t room = E.outbound_message_count (machine t) room
