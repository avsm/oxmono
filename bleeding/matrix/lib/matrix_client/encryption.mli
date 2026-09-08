(** encryption — one device's end-to-end encryption state.

    A machine holds this device's Olm account, the device lists of everybody it
    shares an encrypted room with, the Megolm sessions in both directions, and
    which key backup the device writes to. It performs no I/O and holds no
    {!Client.t}. Every function that would need the server instead returns the
    {!request}s the new state calls for, and the caller performs them, folds the
    answers back with {!receive_keys_query}, {!receive_keys_claim} and
    {!receive_keys_upload}, and reports what reached the server with
    {!mark_sent}. {!Encryption_driver} is that caller for a client that has a
    {!Client.t}.

    Interactive verification belongs to {!module-Verification}. The
    [m.key.verification.*] and [m.secret.*] events decrypted here are surfaced
    in the {!outcome} for the caller to route.

    @see <https://spec.matrix.org/v1.11/client-server-api/#end-to-end-encryption>
      End-to-end encryption
    @see <https://spec.matrix.org/v1.11/client-server-api/#sending-encrypted-events>
      Sending encrypted events *)

(** {1 Devices} *)

(** The local trust decision about another device's keys, never sent to the
    server. Only a {!Verified} device is eligible to receive a forwarded room
    key. Room-event sender trust also follows a validated cross-signing chain
    and the persisted identity decision described below. *)
type trust =
  | Unverified  (** The default for a device merely seen. *)
  | Verified  (** Verified interactively, or signed by a trusted identity. *)
  | Blacklisted  (** Explicitly refused. Never share a room key with it. *)

type utd_cause =
  | Unknown
  | Sent_before_we_joined
  | Verification_violation
  | Unsigned_device
  | Unknown_device
  | Historical_message_and_backup_is_disabled
  | Withheld_for_unverified_or_insecure_device
  | Withheld_by_sender
  | Historical_message_and_device_is_unverified

type withheld = {
  room_id : Matrix_proto.Id.Room_id.t;
  session_id : Matrix_proto.Id.Session_id.t;
  code : string;
  reason : string option;
  sender_key : string option;
      (** The original session sender key, when supplied by the notice. *)
  from_device : Matrix_proto.Id.Device_id.t option;
      (** The device that sent the notice, when supplied. *)
  sender_user : Matrix_proto.Id.User_id.t option;
      (** The authenticated user that carried the withholding notice. This is
          persistence provenance and is not included in bundle wire content.
          [None] is reserved for snapshots written before provenance was
          recorded. *)
}

type pending_key_bundle = {
  room_id : Matrix_proto.Id.Room_id.t;
  inviter : Matrix_proto.Id.User_id.t;
  invite_accepted_at : Ptime.t;
}
(** The durable MSC4268 invitation acceptance record for one room. *)

type room_key_bundle_content = {
  room_id : Matrix_proto.Id.Room_id.t;
  file : Matrix_proto.Event.Media_message_content.encrypted_file;
}
(** The authenticated plaintext content of an MSC4268 room-key bundle. *)

type received_key_bundle = {
  room_id : Matrix_proto.Id.Room_id.t;
  sender : Matrix_proto.Id.User_id.t;
  sender_key : Crypto_key.Curve25519.Public.t;
  sender_ed25519 : string;
  file : Matrix_proto.Event.Media_message_content.encrypted_file;
}
(** A validated room-key bundle received over an authenticated Olm session. *)

val room_key_bundle_content_jsont : room_key_bundle_content Jsont.t
(** The MSC4268 plaintext bundle content codec. *)

type outbound_withheld = {
  ow_room_id : Matrix_proto.Id.Room_id.t;
  ow_session_id : Matrix_proto.Id.Session_id.t;
  ow_user_id : Matrix_proto.Id.User_id.t;
  ow_device_id : Matrix_proto.Id.Device_id.t;
  ow_code : string;
  ow_txn_id : string;
  ow_content : Jsont.json;
  ow_sent : bool;
}

type secret_cancel = {
  sc_txn_id : string;
  sc_content : Jsont.json;
  sc_sent : bool;
  sc_messages : To_device.messages option;
}

type secret_request = {
  sr_name : string;
  sr_request_id : string;
  sr_txn_id : string;
  sr_content : Jsont.json;
  sr_sent : bool;
  sr_messages : To_device.messages option;
  sr_cancel : secret_cancel option;
}

type secret_send = {
  ss_request_id : string;
  ss_user_id : Matrix_proto.Id.User_id.t;
  ss_device_id : Matrix_proto.Id.Device_id.t;
  ss_txn_id : string;
  ss_content : Jsont.json;
  ss_sent : bool;
  ss_messages : To_device.messages option;
}

type utd_context = {
  device_created_at : Ptime.t option;
  backup_exists : bool;
  backup_configured : bool;
  local_device_verified : bool;
  withheld : withheld option;
}

type trust_requirement =
  | Untrusted
  | Cross_signed_or_legacy
  | Cross_signed
      (** How much sender identity a room decryption requires. [Untrusted]
          preserves the historical behaviour and is the default. *)

type identity_status =
  | Identity_unverified
  | Identity_verified
  | Verification_violation

type identity_change = {
  user_id : Matrix_proto.Id.User_id.t;
  status : identity_status;
}

type device = {
  user_id : Matrix_proto.Id.User_id.t;  (** The device's owner. *)
  device_id : Matrix_proto.Id.Device_id.t;
  algorithms : string list;
      (** Encryption algorithms the device claims to support. *)
  keys : (Crypto_key.Key_id.t * string) list;
      (** Key identifier ([ed25519:DEVICE], [curve25519:DEVICE]) to unpadded
          base64 public key, exactly as [/keys/query] returned it. *)
  signatures : Keys.signatures;
  dehydrated : bool option;
      (** The signed MSC3814 dehydrated-device marker, when present. *)
  trust : trust;
}
(** The type for one device of one user, as [/keys/query] describes it plus the
    local trust decision.

    @see <https://spec.matrix.org/v1.11/client-server-api/#device-keys>
      Device keys *)

val device_key : device -> algorithm:string -> string option
(** [device_key d ~algorithm] is the unpadded base64 public key [d] published
    for [algorithm], which is ["ed25519"] or ["curve25519"]. It is looked up
    under the key identifier [algorithm] names for [d]'s device identifier. *)

val device_ed25519 : device -> Crypto_key.Ed25519.Public.t option
(** [device_ed25519 d] is [d]'s signing key, and [None] when it published none
    or published one that is not a key. *)

val device_curve25519 : device -> Crypto_key.Curve25519.Public.t option
(** [device_curve25519 d] is [d]'s identity key, and [None] when it published
    none or published one that is not a key. *)

(** {1 Room encryption settings} *)

type room_settings = {
  algorithm : string;
      (** The [algorithm] of the room's [m.room.encryption] event. *)
  rotation_period_ms : int64;
      (** How long an outbound Megolm session may be used, in milliseconds. *)
  rotation_period_msgs : int;
      (** How many messages an outbound Megolm session may encrypt. *)
}
(** The type for a room's [m.room.encryption] content.

    @see <https://spec.matrix.org/v1.11/client-server-api/#mroomencryption>
      m.room.encryption *)

val default_room_settings : room_settings
(** [default_room_settings] is what the spec gives for a room whose
    [m.room.encryption] omits them, namely [m.megolm.v1.aes-sha2], one week and
    100 messages. *)

val enable_room_encryption :
  ?rotation_period_ms:int64 ->
  ?rotation_period_msgs:int ->
  unit ->
  room_settings
(** [enable_room_encryption ()] is the settings that turn encryption on for a
    room, with [m.megolm.v1.aes-sha2] as the algorithm. [rotation_period_ms] and
    [rotation_period_msgs] each default to the {!default_room_settings} value.
    Send {!room_encryption_content} of the result as room state with
    {!State.set_state}. *)

val room_encryption_content : room_settings -> Jsont.json
(** [room_encryption_content s] is [s] as an [m.room.encryption] content. *)

(** {1 The machine} *)

type t
(** The type for one device's cryptographic state. It is not thread-safe. Give
    each machine to one fiber, as with a {!Client.t}. *)

val create :
  random:Random.t ->
  user_id:Matrix_proto.Id.User_id.t ->
  device_id:Matrix_proto.Id.Device_id.t ->
  unit ->
  t
(** [create ~random ~user_id ~device_id ()] is a machine with a fresh Olm
    account drawn from [random]. It has published nothing, so its first
    {!outgoing_requests} includes a {!Keys_upload}. Restoring a device that
    already exists is {!of_snapshot}. *)

val create_with_account :
  random:Random.t ->
  user_id:Matrix_proto.Id.User_id.t ->
  device_id:Matrix_proto.Id.Device_id.t ->
  account:Olm.Account.t ->
  unit ->
  t
(** [create_with_account ~random ~user_id ~device_id ~account ()] is a fresh,
    empty machine around exactly [account]. Its identity keys are retained and
    its first {!outgoing_requests} publishes them. The account is not copied;
    the caller must stop separately mutating the handed-off account. *)

val user_id : t -> Matrix_proto.Id.User_id.t
(** [user_id t] is the user this device belongs to. *)

val device_id : t -> Matrix_proto.Id.Device_id.t
(** [device_id t] is this device's identifier. *)

val dehydrated_pickle_key : t -> string option
(** [dehydrated_pickle_key t] is the locally cached MSC3814 pickle key in
    canonical Base64 form, when one has been persisted. *)

val set_dehydrated_pickle_key : t -> string -> unit
(** [set_dehydrated_pickle_key t key] caches a canonical pickle-key string. *)

val last_uploaded_device_id : t -> Matrix_proto.Id.Device_id.t option
(** [last_uploaded_device_id t] is the last successfully uploaded dehydrated
    device id, when remembered. *)

val set_last_uploaded_device_id : t -> Matrix_proto.Id.Device_id.t -> unit
(** [set_last_uploaded_device_id t device_id] remembers an uploaded device id.
*)

val identity_keys :
  t -> Crypto_key.Ed25519.Public.t * Crypto_key.Curve25519.Public.t
(** [identity_keys t] is [(ed25519, curve25519)] of this device. These are the
    keys other devices know it by. *)

val sign : t -> Jsont.json -> Jsont.json
(** [sign t json] is [json] with this device's Ed25519 signature added to its
    [signatures] member, under this user and [ed25519:<device id>]. The
    signature covers the canonical JSON of [json] without [signatures] and
    [unsigned], as the spec requires. A [json] that is not an object is returned
    unchanged.

    @see <https://spec.matrix.org/v1.11/appendices/#signing-details>
      Signing details *)

val device_keys_for_upload : ?dehydrated:bool -> t -> Keys.device_keys
(** [device_keys_for_upload t] is the signed [device_keys] object of
    [/keys/upload], namely this device's algorithms and public keys, signed by
    its own Ed25519 key.

    @see <https://spec.matrix.org/v1.11/client-server-api/#device-keys>
      Device keys *)

(** {1 Device lists} *)

val track_users : t -> Matrix_proto.Id.User_id.t list -> unit
(** [track_users t users] starts following [users]' device lists, marking each
    newly tracked user outdated so that the next {!outgoing_requests} fetches
    them. Call it on joining an encrypted room, and when a member joins one.

    @see <https://spec.matrix.org/v1.11/client-server-api/#tracking-the-device-list-for-a-user>
      Tracking the device list for a user *)

val untrack_users : t -> Matrix_proto.Id.User_id.t list -> unit
(** [untrack_users t users] stops following [users], and forgets their devices.
*)

val tracked_users : t -> Matrix_proto.Id.User_id.t list
(** [tracked_users t] is every user whose device list is followed. *)

val outdated_users : t -> Matrix_proto.Id.User_id.t list
(** [outdated_users t] is every tracked user whose device list needs refetching.
*)

val devices_of : t -> Matrix_proto.Id.User_id.t -> device list
(** [devices_of t user] is every device known for [user], this device included
    when [user] is this client's own. *)

val find_device :
  t ->
  Matrix_proto.Id.User_id.t ->
  device_id:Matrix_proto.Id.Device_id.t ->
  device option
(** [find_device t user ~device_id] is that user's device, when one is known. *)

val find_device_by_curve25519 :
  t -> Crypto_key.Curve25519.Public.t -> device option
(** [find_device_by_curve25519 t key] is the device that published [key] as its
    Curve25519 identity key. This is how an Olm message's [sender_key] is
    attributed to a device. *)

val set_device_trust :
  t ->
  Matrix_proto.Id.User_id.t ->
  device_id:Matrix_proto.Id.Device_id.t ->
  trust ->
  unit
(** [set_device_trust t user ~device_id trust] records a trust decision. Only a
    {!Verified} device belonging to this client's own user is ever sent a
    forwarded room key, and a {!Blacklisted} device is never sent a room key at
    all. *)

val set_trust_requirement : t -> trust_requirement -> unit
(** [set_trust_requirement t r] sets the requirement used by subsequent room
    decryptions. It defaults to {!Untrusted}. *)

val trust_requirement : t -> trust_requirement

(** {1 Trusted secret gossip} *)

val store_secret : t -> name:string -> value:string -> unit
(** [store_secret t ~name ~value] makes [value] available to another known,
    verified device of this user that requests [name]. Secrets and their
    in-flight request state are included in {!val-snapshot}. *)

val secret : t -> name:string -> string option
(** [secret t ~name] is the locally stored value, including one accepted from a
    matching verified-device response. *)

val request_secret : t -> name:string -> (string, Error.t) result
(** [request_secret t ~name] creates a stable-id plaintext to-device request to
    all other known devices of this user. A newer request for the same name
    cancels the old one. The returned request id is also the id expected in a
    matching [m.secret.send]. Replies are sent over Olm only; missing Olm
    sessions leave them pending for retry. *)

val cancel_secret_request : t -> name:string -> bool
(** [cancel_secret_request t ~name] queues cancellation of the current request,
    returning [true] when one was outstanding, even if its request event was
    already sent. *)

val trust_user_identity : t -> Matrix_proto.Id.User_id.t -> unit
(** [trust_user_identity t user] verifies the currently known cross-signing
    identity of [user]. It can clear a violation after the replacement identity
    has been verified; a subsequent master-key rotation becomes a fresh
    {!Verification_violation}. It does nothing when no valid identity is known.
*)

val reset_cross_signing : t -> unit
(** [reset_cross_signing t] forgets the local user's cached cross-signing
    identity and clears cross-signing-derived device trust. Existing Olm and
    Megolm ratchets remain usable; callers must publish a replacement identity
    before treating devices as trusted again. *)

val acknowledge_user_identity : t -> Matrix_proto.Id.User_id.t -> unit
(** Acknowledge the currently observed master key by pinning it. This does not
    mark the identity verified; use {!trust_user_identity} for that. *)

val identity_has_pin_violation : t -> Matrix_proto.Id.User_id.t -> bool
(** Whether another user's current validated master key differs from the pinned
    TOFU key. This is always [false] for the local user. *)

val pin_user_identity : t -> Matrix_proto.Id.User_id.t -> unit
(** Pin another user's current validated master key without changing
    verification state. This is a no-op for the local user. *)

val identity_status : t -> Matrix_proto.Id.User_id.t -> identity_status option

val identity_master_key :
  t -> Matrix_proto.Id.User_id.t -> Keys.cross_signing_key option
(** [identity_master_key t user] is the validated master key currently held for
    [user], reconstructed as a wire object suitable for signing. It is absent
    when no valid master/self-signing identity has been received. *)

val identity_self_signing_key :
  t -> Matrix_proto.Id.User_id.t -> Keys.cross_signing_key option
(** [identity_self_signing_key t user] is the validated self-signing key
    currently held for [user], reconstructed as a wire object. *)

val identity_user_signing_key :
  t -> Matrix_proto.Id.User_id.t -> Keys.cross_signing_key option
(** [identity_user_signing_key t user] is the validated user-signing key held
    for the local user, when the server supplied one. Other users' user-signing
    keys are never accepted or exposed. *)

val identity_changes : t -> identity_change list
(** [identity_changes t] is the deterministic list of identity statuses changed
    by the most recent key queries. *)

val receive_keys_query : t -> Keys.query_keys_response -> unit
(** [receive_keys_query t response] folds a [/keys/query] response into the
    device list and marks the users it covers no longer outdated. A device whose
    self-signature does not verify is dropped, and one that comes back with
    different identity keys is refused, since a device identifier may not be
    reused with new keys. A trust decision already recorded for a device
    survives.

    @see <https://spec.matrix.org/v1.11/client-server-api/#post_matrixclientv3keysquery>
      POST /keys/query *)

val receive_keys_claim : ?now:Ptime.t -> t -> Keys.claim_keys_response -> int
(** [receive_keys_claim t response] opens an outbound Olm session with each
    device in a [/keys/claim] response and is how many it opened. A key whose
    signature does not verify under the device's Ed25519 key is skipped, as is a
    key for a device no [/keys/query] has covered. A session created to repair a
    stale (wedged) peer also queues one encrypted [m.dummy] in
    {!outgoing_requests}; its transaction is retained until {!mark_sent}. When
    the response omits a requested device, further ordinary claims for that
    device are transiently suppressed with exponential backoff. [now] is a
    deterministic clock override for that cache.

    @see <https://spec.matrix.org/v1.11/client-server-api/#post_matrixclientv3keysclaim>
      POST /keys/claim *)

val receive_keys_upload : t -> Keys.upload_keys_response -> unit
(** [receive_keys_upload t response] records how many one-time keys the server
    holds for this device, which is what decides whether the next
    {!outgoing_requests} tops the pool up. *)

val withheld_for :
  t ->
  room_id:Matrix_proto.Id.Room_id.t ->
  session_id:Matrix_proto.Id.Session_id.t ->
  withheld option
(** [withheld_for t ~room_id ~session_id] is the latest room-key withholding
    evidence received for that session, if any. Only well-formed
    [m.megolm.v1.aes-sha2] events with a valid Curve25519 sender key are
    retained. *)

val record_invite_acceptance :
  ?now:Ptime.t ->
  t ->
  room_id:Matrix_proto.Id.Room_id.t ->
  inviter:Matrix_proto.Id.User_id.t ->
  unit
(** [record_invite_acceptance t ~room_id ~inviter] records that the local device
    accepted an invitation from [inviter]. There is at most one record per room;
    recording again replaces it and refreshes the timestamp. *)

val pending_key_bundle :
  t -> room_id:Matrix_proto.Id.Room_id.t -> pending_key_bundle option
(** [pending_key_bundle t ~room_id] is the pending MSC4268 acceptance record, if
    one has been recorded for [room_id]. *)

val pending_key_bundles : t -> pending_key_bundle list
(** [pending_key_bundles t] is every pending MSC4268 acceptance record. There is
    at most one entry per room. *)

val received_key_bundles : t -> received_key_bundle list
(** [received_key_bundles t] is the latest validated bundle from each (room,
    sender) pair. *)

val clear_received_key_bundle :
  t ->
  room_id:Matrix_proto.Id.Room_id.t ->
  sender:Matrix_proto.Id.User_id.t ->
  bool
(** [clear_received_key_bundle t ~room_id ~sender] removes the retained bundle
    for that pair and returns [true] if one was present. *)

val room_key_bundle_sender_is_trusted : t -> received_key_bundle -> bool
(** [room_key_bundle_sender_is_trusted t bundle] is [true] when the bundle's
    authenticated Olm identity keys match a currently known device which is
    signed by the sender's current cross-signing identity. The identity need not
    be locally verified, matching MSC4268's SenderUnverified and SenderVerified
    acceptance boundary; a verification violation is rejected. Callers should
    refresh the sender's device and identity keys immediately before this check.
*)

val clear_pending_key_bundle : t -> room_id:Matrix_proto.Id.Room_id.t -> bool
(** [clear_pending_key_bundle t ~room_id] removes the pending record and returns
    [true] if one was present. *)

val should_accept_room_key_bundle :
  ?now:Ptime.t ->
  t ->
  room_id:Matrix_proto.Id.Room_id.t ->
  joined:bool ->
  sender:Matrix_proto.Id.User_id.t ->
  unit ->
  bool
(** [should_accept_room_key_bundle t ~room_id ~joined ~sender ()] accepts only a
    bundle from the recorded inviter while joined, and only before the
    invitation acceptance is 24 hours old. Future timestamps are rejected. *)

val accept_room_key_bundle :
  ?now:Ptime.t ->
  t ->
  room_id:Matrix_proto.Id.Room_id.t ->
  joined:bool ->
  sender:Matrix_proto.Id.User_id.t ->
  Room_key_export.room_key_bundle ->
  int option
(** [accept_room_key_bundle ?now t ~room_id ~joined ~sender bundle] applies the
    acceptance gate and imports [bundle] when allowed. It returns [None] when
    rejected, or [Some imported_count] when processed. An accepted bundle
    consumes the pending record even when it contains no valid room keys.

    [sender] must be the authenticated sender of the event carrying the bundle,
    not a user ID taken from untrusted event content. *)

val clear_expired_pending_key_bundles :
  ?now:Ptime.t -> t -> Matrix_proto.Id.Room_id.t list
(** [clear_expired_pending_key_bundles t] removes records at least 24 hours old
    or future-dated records and returns their room IDs. *)

(** {1 Rooms} *)

val set_room_encryption_settings :
  t -> Matrix_proto.Id.Room_id.t -> Jsont.json -> (unit, Error.t) result
(** [set_room_encryption_settings t room content] records [room]'s
    [m.room.encryption] content as seen in room state. {!process_sync} calls it
    for every such event in a response, so a caller that syncs need not.

    An algorithm other than [m.megolm.v1.aes-sha2] is an [Error], since the room
    is encrypted with something this library cannot speak and treating it as
    unencrypted would leak. *)

val find_room_settings : t -> Matrix_proto.Id.Room_id.t -> room_settings option
(** [find_room_settings t room] is [room]'s settings, and [None] when its
    [m.room.encryption] has not been seen. *)

val is_room_encrypted : t -> Matrix_proto.Id.Room_id.t -> bool
(** [is_room_encrypted t room] is [true] when [room]'s [m.room.encryption] has
    been seen. *)

(** {1 Outgoing requests} *)

(** Something the machine needs the server to do. The machine decides what and
    builds the body; the caller decides when, and owns the transport. *)
type request =
  | Keys_upload of {
      device_keys : Keys.device_keys option;
          (** Present while this device's identity keys are unpublished. *)
      one_time_keys : (Crypto_key.Key_id.t * Keys.one_time_key) list;
      fallback_keys : (Crypto_key.Key_id.t * Keys.one_time_key) list;
          (** Non-empty when the server said the previous fallback key was used.
          *)
    }  (** Publish this device's keys. [POST /_matrix/client/v3/keys/upload]. *)
  | Keys_query of Matrix_proto.Id.User_id.t list
      (** Refresh these users' device lists.
          [POST /_matrix/client/v3/keys/query]. *)
  | Keys_claim of
      (Matrix_proto.Id.User_id.t * (Matrix_proto.Id.Device_id.t * string) list)
      list
      (** Claim a one-time key per (device, algorithm), to open Olm sessions.
          [POST /_matrix/client/v3/keys/claim]. *)
  | To_device of {
      event_type : string;
      txn_id : string;
      messages : To_device.messages;
    }
      (** Deliver to-device messages. The transaction identifier is fixed when
          the request is built, so a retry is idempotent. An
          [m.room_key.withheld] request is tracked by {!mark_sent} so that a
          delivered notice is not repeated.
          [PUT /_matrix/client/v3/sendToDevice/{eventType}/{txnId}]. *)
  | Room_key_share of {
      room_id : Matrix_proto.Id.Room_id.t;
      session_id : Matrix_proto.Id.Session_id.t;
      txn_id : string;
      messages : To_device.messages;
    }
      (** Deliver a room's Megolm session key to the devices that lack it, as
          [m.room.encrypted] to-device messages. {!mark_sent} records the
          session as shared with the recipients, so a request that is dropped is
          built again.
          [PUT /_matrix/client/v3/sendToDevice/m.room.encrypted/{txnId}]. *)
  | Room_key_bundle_share of {
      room_id : Matrix_proto.Id.Room_id.t;
      txn_id : string;
      messages : To_device.messages;
    }
      (** Deliver an authenticated MSC4268 room-key bundle to recipient devices
          as [m.room.encrypted] to-device messages. *)
  | Room_keys_upload of { version : string; rooms : Backup.rooms }
      (** Put the encrypted Megolm sessions into the key backup.
          [PUT /_matrix/client/v3/room_keys/keys]. *)

val pp_request : Format.formatter -> request -> unit
(** [pp_request ppf r] prints a one-line description of [r] on [ppf]. It names
    no key material. *)

val outgoing_requests : t -> request list
(** [outgoing_requests t] is what the machine's current state calls for,
    recomputed on each call. It is a {!Keys_upload} while the device keys are
    unpublished or the one-time key pool is low, and a {!Keys_query} while any
    tracked user is outdated. Generating the one-time keys the request offers is
    part of computing it.

    It also includes pending plaintext [m.secret.request] and cancellation
    events, and encrypted [m.secret.send] replies. These retain their
    transaction ids and exact content until {!mark_sent} is called, including
    across a snapshot/store reload. A forced [/keys/claim] for a wedged device
    is included even when an older Olm session still exists; after a successful
    claim, its one-shot encrypted [m.dummy] is included here too.

    It is idempotent. Calling it twice without performing anything is the same
    list, and it never includes the one-shot requests {!process_sync},
    {!encrypt_room_event} and {!pending_backup} produce. *)

val mark_sent : t -> request -> unit
(** [mark_sent t r] records that [r] reached the server. It marks published
    one-time keys as published, a {!Room_key_share} as shared with its
    recipients and a {!Room_keys_upload} as backed up, so that a request which
    is never performed is offered again. A request whose answer carries state
    also needs the matching [receive_] function. *)

val share_room_key_bundle :
  t ->
  room_id:Matrix_proto.Id.Room_id.t ->
  recipient:Matrix_proto.Id.User_id.t ->
  file:Matrix_proto.Event.Media_message_content.encrypted_file ->
  (request option, Error.t) result
(** [share_room_key_bundle t ~room_id ~recipient ~file] builds one authenticated
    [m.room_key_bundle] payload for every currently addressable, non-blacklisted
    device of [recipient] that has an Olm session. It returns [None] when there
    are no such devices. The caller must refresh and claim recipient devices
    first. *)

(** {1 Processing a sync} *)

(** How much a decrypted event's sender is authenticated. *)
type verification_state =
  | Verified_device  (** The signing device is known and {!Verified}. *)
  | Unverified_device
      (** Legacy compatibility state for a known, locally unverified device. New
          sender-data derivation reports {!Device_info} instead. *)
  | Unknown_device
      (** No device in the list published the Curve25519 key that sent this, so
          the sender is unauthenticated. *)
  | Device_info
      (** The signed device keys own the session, but no valid owner
          cross-signing chain is known. *)
  | Sender_unverified
      (** The owner cross-signed the device, but that owner identity has not
          been verified locally. *)
  | Sender_verified
      (** The owner cross-signed the device and its identity is verified. *)
  | Verification_violation
      (** This sender was verified before its master identity key changed. *)

(** What a to-device event turned into. Every event in a sync's [to_device]
    section produces exactly one of these, so a caller can log or count them
    without re-parsing. *)
type to_device_event =
  | Room_key of {
      room_id : Matrix_proto.Id.Room_id.t;
      session_id : Matrix_proto.Id.Session_id.t;
      sender : Matrix_proto.Id.User_id.t;
      sender_key : Crypto_key.Curve25519.Public.t;
    }  (** A Megolm session arrived and was stored. *)
  | Forwarded_room_key of {
      room_id : Matrix_proto.Id.Room_id.t;
      session_id : Matrix_proto.Id.Session_id.t;
      sender : Matrix_proto.Id.User_id.t;
      sender_key : string;
          (** The Curve25519 key of the device that created the session,
              unpadded base64, as the forwarding device claimed it. *)
      forwarding_chain : string list;
    }  (** A Megolm session was gossiped to this device and stored. *)
  | Room_key_bundle of received_key_bundle
      (** A validated MSC4268 bundle arrived and was persisted. *)
  | Room_key_request of {
      sender : Matrix_proto.Id.User_id.t;
      requesting_device_id : string;
      request_id : string;
      room_id : string;
      session_id : string;
      action : string;  (** [request] or [request_cancellation]. *)
      answered : bool;
          (** Whether a {!Forwarded_room_key} reply is in the outcome's
              requests. It is [false] whenever the gossip policy refused, and
              the reason is then in the log. *)
    }
      (** Another device asked for a room key. The identifiers are left as the
          sender wrote them, since a request that names nothing this device
          holds is still reported. *)
  | Verification of {
      event_type : string;  (** The [m.key.verification.*] type. *)
      sender : Matrix_proto.Id.User_id.t;
      sender_device : Matrix_proto.Id.Device_id.t option;
          (** The sending device, when the event arrived over Olm from a known
              device. *)
      content : Jsont.json;
    }  (** For {!module-Verification} to drive; this module only decrypts it. *)
  | Secret_request of {
      sender : Matrix_proto.Id.User_id.t;
      content : Jsont.json;
    }
      (** [m.secret.request], which may be plaintext or already decrypted out of
          an Olm envelope. A matching request from another known, verified
          device of this user may queue an encrypted [m.secret.send] reply. *)
  | Secret_send of { sender : Matrix_proto.Id.User_id.t; content : Jsont.json }
      (** [m.secret.send], already decrypted out of its Olm envelope. A send is
          accepted into {!secret} only when it matches an outstanding request
          and comes from another known, verified device of this user. *)
  | Undecryptable of { sender : Matrix_proto.Id.User_id.t; reason : string }
      (** An [m.room.encrypted] to-device event that would not open. *)
  | Ignored of { event_type : string; reason : string }
      (** Something well-formed that this machine deliberately drops, such as
          [m.dummy] or an event addressed to another device. *)

val pp_to_device_event : Format.formatter -> to_device_event -> unit
(** [pp_to_device_event ppf e] prints a one-line description of [e] on [ppf]. It
    names no key material and no event content. *)

type outcome = {
  requests : request list;
      (** What this sync calls for, {!outgoing_requests} included, in the order
          they should be performed. *)
  events : to_device_event list;  (** One per to-device event, in order. *)
  changed_users : Matrix_proto.Id.User_id.t list;
      (** Tracked users [device_lists.changed] invalidated. *)
  left_users : Matrix_proto.Id.User_id.t list;
      (** Users from [device_lists.left], no longer sharing an encrypted room
          with this client and no longer tracked. *)
  new_sessions : (Matrix_proto.Id.Room_id.t * Matrix_proto.Id.Session_id.t) list;
      (** Megolm sessions this sync delivered, as (room, session id). A caller
          holding undecryptable events should retry them. *)
}
(** The type for everything one sync response changed. *)

val process_sync : ?now:Ptime.t -> t -> Matrix_proto.Sync.Response.t -> outcome
(** [process_sync t response] folds a [/sync] response into the machine. It
    takes in the one-time key counts, the device-list changes, every
    [m.room.encryption] in room state, and every [to_device] event, whose
    disposition is the returned {!to_device_event} list. An absent
    [signed_curve25519] count means zero for this classic-sync entry point.
    [now] is an optional clock seam for fallback-key age checks; it defaults to
    the wall clock.

    An [m.room.encrypted] to-device event is opened with Olm and checked against
    the spec's requirements on the decrypted [sender], [keys.ed25519],
    [recipient] and [recipient_keys.ed25519] before its inner event is handled.
    MSC4268 room-key bundles additionally require a valid MSC4147
    [sender_device_keys] object binding the signed device keys to the Olm
    sender. No request is performed. The work is in the outcome's [requests].

    {2 Gossip policy}

    An [m.room_key_request] is answered with [m.forwarded_room_key] only when it
    comes from another device of {e this client's own} user marked {!Verified},
    the session is held, and an Olm session with that device already exists. Any
    other request is reported in the outcome and otherwise ignored, so that a
    room's history is not handed to whoever asks.

    @see <https://spec.matrix.org/v1.11/client-server-api/#key-requests>
      Key requests *)

val process_sliding_sync :
  ?now:Ptime.t -> t -> Matrix_proto.Sliding_sync.Response.t -> outcome
(** [process_sliding_sync t response] folds an MSC4186 sliding-sync response
    into the machine. Unlike {!process_sync}, an absent [signed_curve25519]
    one-time-key count leaves the previously reported server count unchanged, as
    sliding sync sends deltas. [now] has the same meaning as in {!process_sync}.
*)

(** {1 Room events} *)

type decrypted_event = {
  decrypted_type : string;  (** The plaintext event's [type]. *)
  decrypted_content : Jsont.json;  (** The plaintext event's [content]. *)
  decrypted_room_id : Matrix_proto.Id.Room_id.t;
  decrypted_sender : Matrix_proto.Id.User_id.t;
  decrypted_sender_key : Crypto_key.Curve25519.Public.t;
      (** The Curve25519 key of the device that created the Megolm session. *)
  decrypted_claimed_ed25519 : Crypto_key.Ed25519.Public.t option;
      (** The Ed25519 key that device claimed when it shared the session, and
          [None] when it claimed none. *)
  decrypted_session_id : Matrix_proto.Id.Session_id.t;
  decrypted_message_index : int;
  decrypted_verification : verification_state;
}
(** The type for a room event recovered from [m.room.encrypted]. *)

(** Why a room event did not decrypt. *)
type decrypt_error =
  | Not_encrypted  (** The event is not an [m.room.encrypted]. *)
  | Unsupported_algorithm of string
  | Malformed of string  (** The content did not parse. *)
  | Unknown_session of {
      room_id : Matrix_proto.Id.Room_id.t;
      session_id : Matrix_proto.Id.Session_id.t;
      sender_key : string option;
          (** The Curve25519 key the event named, when it named one. *)
      sender : Matrix_proto.Id.User_id.t;
    }
      (** No Megolm session with this identifier is held. This is the normal
          case for history from before this device joined, and is worth
          retrying. Pass the same fields to {!request_room_key}. *)
  | Unknown_message_index of {
      room_id : Matrix_proto.Id.Room_id.t;
      session_id : Matrix_proto.Id.Session_id.t;
      sender_key : string option;
      sender : Matrix_proto.Id.User_id.t;
      message_index : int;
      first_known : int;
    }
      (** The session is held, but its ratchet starts after this event. A key
          forwarded or restored from an earlier index can make the same event
          decryptable, so this is retryable like {!Unknown_session}. *)
  | Megolm_error of string
      (** The session is known but the message did not decrypt, from a bad MAC
          or signature, or a ratchet already advanced past it. *)
  | Room_mismatch of { expected : Matrix_proto.Id.Room_id.t; got : string }
      (** The plaintext claims a different room from the one the event arrived
          in, which is the attack the [room_id] inside the payload exists to
          stop. *)
  | Not_trusted of {
      requirement : trust_requirement;
      sender : Matrix_proto.Id.User_id.t;
      verification : verification_state;
    }  (** The sender identity did not meet the configured trust requirement. *)

val default_utd_context : utd_context
(** A conservative empty context for {!classify_utd}. *)

val classify_utd :
  Matrix_proto.Event.Raw_event.t -> decrypt_error -> utd_context -> utd_cause
(** [classify_utd event error context] gives the stable reason an encrypted
    event could not be opened. It is pure. Event time and MSC4115 membership
    come from [event]; callers provide device age, backup, local verification
    and withholding evidence in [context]. A missing session is historical only
    when the event predates this device, which is distinct from a pre-join event
    whose unsigned membership is [leave]. *)

val utd_context :
  ?device_created_at:Ptime.t -> t -> decrypt_error -> utd_context
(** [utd_context ?device_created_at t error] derives the backup, local identity
    trust and room-key withholding facts needed by {!classify_utd}. The device
    creation time is optional because older stores do not retain it; omitting it
    conservatively leaves historical-message classification unknown. *)

val pp_decrypt_error : Format.formatter -> decrypt_error -> unit
(** [pp_decrypt_error ppf e] prints a one-line description of [e] on [ppf]. *)

val decrypt_room_event :
  ?trust_requirement:trust_requirement ->
  t ->
  Matrix_proto.Id.Room_id.t ->
  Matrix_proto.Event.Raw_event.t ->
  (decrypted_event, decrypt_error) result
(** [decrypt_room_event t room event] decrypts a Megolm [m.room.encrypted]
    timeline event.

    The session is found by [session_id] alone. The event's [sender_key] and
    [device_id] are advisory from Matrix 1.3 onwards and are checked only when
    present. The plaintext's [room_id] must equal [room], or the result is
    {!Room_mismatch}. [trust_requirement] defaults to the machine's configured
    {!val-trust_requirement}, initially {!Untrusted}; stricter requirements
    reject a sender with {!Not_trusted}.

    @see <https://spec.matrix.org/v1.11/client-server-api/#mmegolmv1aes-sha2>
      m.megolm.v1.aes-sha2 *)

val request_room_key :
  t ->
  room_id:Matrix_proto.Id.Room_id.t ->
  session_id:Matrix_proto.Id.Session_id.t ->
  ?sender_key:Crypto_key.Curve25519.Public.t ->
  unit ->
  request
(** [request_room_key t ~room_id ~session_id ()] is the [m.room_key_request] to
    send to every other device of this user. Its [request_id] is drawn from the
    machine's randomness, so keep it to cancel the request later. [sender_key]
    names the device that created the session, and defaults to absent.

    @see <https://spec.matrix.org/v1.11/client-server-api/#mroom_key_request>
      m.room_key_request *)

val ensure_sessions :
  ?now:Ptime.t -> t -> members:Matrix_proto.Id.User_id.t list -> request list
(** [ensure_sessions t ~members] is what must reach the server before
    {!encrypt_room_event} can share a room's Megolm session with every device of
    [members]. It is a {!Keys_query} for members whose device list is unknown or
    outdated, then a {!Keys_claim} for the known devices this machine has no Olm
    session with, or for a known device whose old session was detected as
    wedged, and it starts following any member not already tracked. It is [[]]
    once every device is reachable. Performing it and calling again picks up the
    devices the query revealed. Ordinary claims honor the transient exhaustion
    backoff learned by {!receive_keys_claim}; a wedged device always bypasses
    it. [now] is a deterministic clock override for that cache. *)

val encrypt_room_event :
  t ->
  Matrix_proto.Id.Room_id.t ->
  event_type:string ->
  content:Jsont.json ->
  members:Matrix_proto.Id.User_id.t list ->
  (Jsont.json * request list, Error.t) result
(** [encrypt_room_event t room ~event_type ~content ~members] is the
    [m.room.encrypted] content to send in [room], and the {!Room_key_share}
    requests that hand the session key to the devices of [members] that lack it.
    Perform the requests, or those devices cannot read the event.

    It rotates the room's outbound Megolm session when the room's
    [rotation_period_msgs] or [rotation_period_ms] is spent or a member has left
    since the session was created. A {!Blacklisted} device, this device itself,
    and a device this machine has no Olm session with are not shared with. The
    first receives a plaintext [m.room_key.withheld] with code [m.blacklisted];
    the second is never addressed; the third receives [m.no_olm] when it has a
    Curve25519 key. {!ensure_sessions} is what opens those sessions. A dropped
    withheld request is retried with its original transaction identifier, and
    {!mark_sent} suppresses further notices. Both pending and acknowledged
    notice state are part of {!val-snapshot}, so this remains true after
    restoring a persisted crypto store.

    [members] must be the room's joined members, and its invited members when
    the room shares history with them, so everybody who should be able to read
    the message. Nothing here holds room state or reads a member list of its
    own.

    @see <https://spec.matrix.org/v1.11/client-server-api/#sending-encrypted-events>
      Sending encrypted events *)

(** {1 Key backup} *)

val enable_backup :
  t ->
  version:string ->
  ?decryption_key:Backup.Decryption_key.t ->
  Backup.encryption_key ->
  unit
(** [enable_backup t ~version key] points the machine at the backup [version],
    whose public key is [key]. [decryption_key] defaults to absent, and without
    it the machine can upload to the backup but not read it back.

    Every inbound Megolm session held, and every one that arrives afterwards,
    becomes pending for {!pending_backup}. *)

val disable_backup : t -> unit
(** [disable_backup t] forgets the backup version and its keys. Nothing is
    deleted server-side. *)

val backup_version : t -> string option
(** [backup_version t] is the version the machine writes to, when a backup is
    enabled. *)

val backup_decryption_enabled : t -> bool
(** [backup_decryption_enabled t] is [true] when [t] has the private key needed
    to read its configured server-side backup. *)

val room_key_backup_is_fully_downloaded : t -> Matrix_proto.Id.Room_id.t -> bool
(** [room_key_backup_is_fully_downloaded t room] reports whether the complete
    readable backup for [room] has already been downloaded for the active backup
    version. *)

val mark_room_key_backup_fully_downloaded :
  t -> Matrix_proto.Id.Room_id.t -> unit
(** [mark_room_key_backup_fully_downloaded t room] records a successfully
    downloaded complete backup for [room]. *)

val clear_room_key_backup_fully_downloaded :
  t -> Matrix_proto.Id.Room_id.t -> unit
(** [clear_room_key_backup_fully_downloaded t room] removes the room marker. *)

val backup_pending_count : t -> int
(** [backup_pending_count t] is the total number of inbound Megolm sessions
    which remain to be uploaded. It is [0] while no backup is enabled, since
    there is nowhere to put them. *)

val pending_backup : t -> (request option, Error.t) result
(** [pending_backup t] is a deterministic batch of at most 100 inbound Megolm
    sessions not yet in the backup, each encrypted with
    {!Backup.encrypt_session_data}, as a {!Room_keys_upload}. It is [None] when
    no backup is enabled or every session is already there. {!mark_sent} on the
    result records exactly that batch as backed up.

    @see <https://spec.matrix.org/v1.11/client-server-api/#server-side-key-backups>
      Server-side key backups *)

val import_backup : t -> Backup.rooms -> (int, Error.t) result
(** [import_backup t rooms] decrypts the backup entries in [rooms] with the
    machine's backup decryption key and imports those not already held at an
    equal or lower message index, and is how many it imported. A session that
    fails to decrypt is skipped and logged, so one bad entry does not lose the
    rest. The decrypted [session_key] must be an unsigned version-1 exported
    Megolm key; signed version-2 [m.room_key] blobs are rejected. The derived
    session identifier must also match the backup map key. The legacy backup MAC
    authenticates neither the ciphertext nor the outer [first_message_index],
    [forwarded_count], and [is_verified] fields, so import deliberately derives
    the session and trust state from the inner exported metadata while ignoring
    those outer trust hints. Sender and forwarding Curve25519 keys must decode,
    and forwarding chains longer than 100 entries are skipped; accepted
    encodings are canonicalized before storage. The 100-entry resource bound is
    local hardening beyond the currently unbounded Matrix/Rust representation.
    The backup version's [auth_data] and public key must be validated and pinned
    by the caller before invoking this function. Every entry it could read is
    recorded as backed up.

    It is an [Error] when no backup decryption key is enabled. *)

val export_room_keys :
  ?predicate:(Room_key_export.room_key -> bool) ->
  t ->
  Room_key_export.room_key list
(** [export_room_keys t] is the deterministic, portable export of every inbound
    Megolm session held by [t] for which [predicate] returns [true]. The
    predicate defaults to accepting all entries. The session key starts at the
    first known message index and includes the recorded sender and forwarding
    metadata. *)

type room_key_import_result = { imported_count : int; total_count : int }
(** Counts returned by a portable key import. *)

val import_room_keys :
  t -> Room_key_export.room_key list -> room_key_import_result
(** [import_room_keys t keys] imports valid portable Megolm sessions.
    [imported_count] counts entries that replaced no existing session or reached
    an earlier first-known index; [total_count] counts every supplied entry.
    Unsupported, malformed, session-ID-mismatched, non-Curve25519 forwarding, or
    greater-than-100-entry forwarding chains are skipped individually. Accepted
    sender and forwarding key encodings are canonicalized before storage. *)

val build_room_key_bundle :
  t -> room_id:Matrix_proto.Id.Room_id.t -> Room_key_export.room_key_bundle
(** [build_room_key_bundle t ~room_id] deterministically builds the pure MSC4268
    bundle for [room_id]. Sessions marked [shared_history] are shareable; the
    rest become [m.history_not_shared] entries. *)

val import_room_key_bundle :
  t ->
  room_id:Matrix_proto.Id.Room_id.t ->
  sender:Matrix_proto.Id.User_id.t ->
  Room_key_export.room_key_bundle ->
  int
(** [import_room_key_bundle t ~room_id ~sender bundle] validates and imports
    only entries for [room_id], recording [sender] as the forwarding provenance.
    Historic room keys are marked shareable on import; forwarding metadata and
    caller-controlled history flags are not accepted from this wire format. The
    bounded pure API assumes the caller has authenticated and authorized the
    forwarder (the full Rust trust gate distinguishes SenderUnverified and
    SenderVerified). Invalid entries and unsupported withholding records are
    ignored individually; the result counts imported sessions. *)

(** {1 Inspecting sessions} *)

val inbound_sessions :
  t -> (Matrix_proto.Id.Room_id.t * Matrix_proto.Id.Session_id.t) list
(** [inbound_sessions t] is every inbound Megolm session, as (room, session id).
*)

val has_inbound_session :
  t ->
  Matrix_proto.Id.Room_id.t ->
  session_id:Matrix_proto.Id.Session_id.t ->
  bool
(** [has_inbound_session t room ~session_id] is [true] when that session is
    held. *)

val outbound_session_id :
  t -> Matrix_proto.Id.Room_id.t -> Matrix_proto.Id.Session_id.t option
(** [outbound_session_id t room] is the identifier of [room]'s current outbound
    Megolm session, when one has been created. *)

val outbound_message_count : t -> Matrix_proto.Id.Room_id.t -> int
(** [outbound_message_count t room] is how many messages [room]'s outbound
    session has encrypted, and [0] when there is none. *)

(** {1 Persistence}

    A machine is written out as a {!val-snapshot} and read back with
    {!of_snapshot}. {!Crypto_store} is what puts one on disk. *)

module Session_meta : sig
  (** Per-inbound-session bookkeeping the session store has no field for. *)

  type t = {
    room_id : Matrix_proto.Id.Room_id.t;
    session_id : Matrix_proto.Id.Session_id.t;
    forwarding_chain : string list;
        (** The [forwarding_curve25519_key_chain] of the [m.forwarded_room_key]
            that delivered the session, empty for a session that arrived as
            [m.room_key]. Entries loaded through the public import and restore
            paths are at most 100 canonical unpadded Curve25519 public keys. *)
    sender : Matrix_proto.Id.User_id.t option;
    sender_ed25519 : string option;
    shared_history : bool;
        (** Whether the session was received as shared room history. *)
    legacy : bool;
        (** Whether this session came from pre-metadata storage. Newly received
            or imported sessions are never legacy. *)
  }
  (** The type for session bookkeeping. *)
end

type backup_state = {
  version : string option;  (** The server-assigned backup version, if any. *)
  encryption_key : Backup.encryption_key option;
  decryption_key : Backup.Decryption_key.t option;
      (** Present only when this device can read the backup as well as write to
          it. *)
  backed_up : (Matrix_proto.Id.Room_id.t * Matrix_proto.Id.Session_id.t) list;
      (** The sessions already uploaded to [version]. *)
  room_key_backups_fully_downloaded : Matrix_proto.Id.Room_id.t list;
      (** Rooms whose complete readable backup has been downloaded for
          [version]. Legacy snapshots decode this as the empty list. *)
}
(** The type for what is known about the server-side key backup. *)

type identity = {
  identity_user_id : Matrix_proto.Id.User_id.t;
  identity_master_key : string;
  identity_self_signing_key : string;
  identity_user_signing_key : string option;
  identity_was_previously_verified : bool;
  identity_status : identity_status;
  identity_pinned_master_key : string;
}

type state = {
  devices : device list;
  tracked_users : Matrix_proto.Id.User_id.t list;
      (** Users sharing an encrypted room, whose device lists are followed. *)
  outdated_users : Matrix_proto.Id.User_id.t list;
      (** Tracked users whose device list [device_lists.changed] has invalidated
          and a [/keys/query] has yet to refresh. *)
  rooms : (Matrix_proto.Id.Room_id.t * room_settings) list;
  backup : backup_state;
  session_meta : Session_meta.t list;
  trust_requirement : trust_requirement;
  identities : identity list;
  withheld : withheld list;
  pending_key_bundles : pending_key_bundle list;
  received_key_bundles : received_key_bundle list;
  outbound_withheld : outbound_withheld list;
  secrets : (string * string) list;
  secret_requests : secret_request list;
  secret_sends : secret_send list;
  fallback_key_created_at : Ptime.t option;
      (** Creation time of the current fallback key, when fallback support has
          been observed. Used for weekly age-based rotation. *)
  fallback_key_pending : bool;
      (** A generated fallback key has not yet been acknowledged by upload. *)
  published_one_time_keys : string list;
      (** Identifiers of the one-time keys already accepted by [/keys/upload],
          so they are not offered a second time. *)
  uploaded_one_time_key_count : int;
      (** Last aggregate [signed_curve25519] count reported by the homeserver.
          Legacy snapshots default to zero. *)
  device_keys_uploaded : bool;
      (** Whether this device's identity keys have reached the server. *)
  dehydrated_pickle_key : string option;
      (** Canonical Base64 MSC3814 pickle key, when cached locally. *)
  last_uploaded_device_id : Matrix_proto.Id.Device_id.t option;
      (** The last dehydrated device id successfully uploaded. *)
}
(** The type for everything a machine holds apart from its keys and sessions. *)

val empty_state : state
(** [empty_state] is the state of a machine that has never synced. *)

type snapshot = {
  account : Olm.Account.t;
  olm_sessions : Olm.Session.t list;
      (** Each carries the peer's Curve25519 identity key, which is what they
          are filed under. *)
  megolm_inbound : Olm.Megolm.Inbound.t list;
  megolm_outbound : Olm.Megolm.Outbound.t list;
  state : state;
}
(** The type for the complete cryptographic state of one device. *)

val snapshot : t -> snapshot
(** [snapshot t] is [t]'s state. The Olm and Megolm sessions in it are the live
    ones, not copies, so a later change to [t] shows through. *)

val of_snapshot :
  random:Random.t ->
  user_id:Matrix_proto.Id.User_id.t ->
  device_id:Matrix_proto.Id.Device_id.t ->
  snapshot ->
  t
(** [of_snapshot ~random ~user_id ~device_id snapshot] is the machine [snapshot]
    came from. It keeps its identity keys, so the homeserver sees the same
    device. Legacy session metadata with an invalid or greater-than-100-entry
    forwarding-key chain is discarded without discarding the corresponding
    Megolm session, preventing untyped or unbounded persisted data from being
    re-exported. *)
