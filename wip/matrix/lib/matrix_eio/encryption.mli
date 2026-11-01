(** encryption — one device's end-to-end encryption state, raising instead of
    returning a result.

    A value here is a {!Matrix_client.Encryption} machine joined to a client and
    a store. The pure operations of that machine are re-exported unchanged, and
    the ones that reach the server raise [Eio.Io] carrying an {!Error.type-err}
    where their {!Matrix_client.Encryption_driver} counterpart returns an error.
    {!decrypt_room_event} is the exception, its failures being facts about an
    event rather than I/O problems. {!machine} reaches the operations this
    module does not re-export.

    The two a client wires in are {!sync_hook}, which folds a sync response into
    the machine and performs what it asks for, and {!send_encrypted}.

    A machine is not thread-safe. Give each one to a single fiber, as with a
    {!Client.t}.

    @see <https://spec.matrix.org/v1.11/client-server-api/#end-to-end-encryption>
      End-to-end encryption *)

(** {1 The machine} *)

type t = Matrix_client.Encryption_driver.t
(** The type for a machine joined to a client. It is
    {!Matrix_client.Encryption_driver.t}. *)

val create :
  random:Matrix_client.Random.t ->
  user_id:Matrix_proto.Id.User_id.t ->
  device_id:Matrix_proto.Id.Device_id.t ->
  ?store:Matrix_client.Crypto_store.t ->
  unit ->
  t
(** [create ~random ~user_id ~device_id ()] restores the device's Olm account
    from [store] when it holds one for this profile, and otherwise generates a
    fresh account from [random]. [store] defaults to absent, and a machine
    without one runs in memory and writes nothing. A stored account that is
    present but unreadable raises [Eio.Io] rather than being overwritten, since
    overwriting it would lose the ability to decrypt existing history. *)

val create_with_account :
  random:Matrix_client.Random.t ->
  user_id:Matrix_proto.Id.User_id.t ->
  device_id:Matrix_proto.Id.Device_id.t ->
  account:Matrix_client.Olm.Account.t ->
  ?store:Matrix_client.Crypto_store.t ->
  unit ->
  t
(** [create_with_account ~random ~user_id ~device_id ~account ?store ()] wraps a
    fresh, empty machine around exactly [account], retaining its identity keys
    so its first outgoing request publishes them. The account is not copied; the
    caller must stop separately mutating the handed-off account. With no store
    or an empty store, the store is associated with the new machine. A non-empty
    store raises [Eio.Io] rather than being loaded or overwritten: this
    constructor is for a pre-OAuth new account, while {!create} is the resume
    path.

    Raises [Eio.Io] on a filesystem failure. *)

val of_env :
  < secure_random : _ Eio.Flow.source ; .. > ->
  user_id:Matrix_proto.Id.User_id.t ->
  device_id:Matrix_proto.Id.Device_id.t ->
  ?store:Matrix_client.Crypto_store.t ->
  unit ->
  t
(** [of_env env ~user_id ~device_id ()] is {!create} with the randomness taken
    from [env]'s [secure_random]. *)

val machine : t -> Matrix_client.Encryption.t
(** [machine t] is the pure machine [t] drives, for the operations not
    re-exported here. *)

val save : t -> unit
(** [save t] writes the machine's state through the store it was created with,
    and does nothing when it has none. It is the only thing here that touches
    the disk. State that cannot be encoded raises [Eio.Io]. *)

val user_id : t -> Matrix_proto.Id.User_id.t
(** [user_id t] is the user this device belongs to. *)

val device_id : t -> Matrix_proto.Id.Device_id.t
(** [device_id t] is this device's identifier. *)

val identity_keys :
  t ->
  Matrix_client.Crypto_key.Ed25519.Public.t
  * Matrix_client.Crypto_key.Curve25519.Public.t
(** [identity_keys t] is [(ed25519, curve25519)] of this device. These are the
    keys other devices know it by. *)

val sign : t -> Jsont.json -> Jsont.json
(** [sign t json] is [json] with this device's Ed25519 signature added to its
    [signatures] member. A [json] that is not an object is returned unchanged.
*)

val device_keys_for_upload : t -> Keys.device_keys
(** [device_keys_for_upload t] is the signed [device_keys] object
    {!Keys.upload_keys} takes. *)

val snapshot : t -> Matrix_client.Encryption.snapshot
(** [snapshot t] is the complete cryptographic state of the device. The Olm and
    Megolm sessions in it are the live ones, not copies. *)

(** {1 Device lists} *)

(** The local trust decision about another device's keys, never sent to the
    server. It is {!Matrix_client.Encryption.type-trust}. *)
type trust = Matrix_client.Encryption.trust =
  | Unverified
  | Verified
  | Blacklisted

type identity_status = Matrix_client.Encryption.identity_status =
  | Identity_unverified
  | Identity_verified
  | Verification_violation
      (** The validation state of a user's current cross-signing identity. *)

type device = Matrix_client.Encryption.device = {
  user_id : Matrix_proto.Id.User_id.t;
  device_id : Matrix_proto.Id.Device_id.t;
  algorithms : string list;
  keys : (Matrix_client.Crypto_key.Key_id.t * string) list;
  signatures : Keys.signatures;
  dehydrated : bool option;
  trust : trust;
}
(** The type for one device of one user, as [/keys/query] describes it plus the
    local trust decision. It is {!Matrix_client.Encryption.device}, which
    documents the fields. *)

val device_key : device -> algorithm:string -> string option
(** [device_key d ~algorithm] is the unpadded base64 public key [d] published
    for [algorithm], which is ["ed25519"] or ["curve25519"]. *)

val device_ed25519 : device -> Matrix_client.Crypto_key.Ed25519.Public.t option
(** [device_ed25519 d] is [d]'s signing key, and [None] when it published none
    or published one that is not a key. *)

val device_curve25519 :
  device -> Matrix_client.Crypto_key.Curve25519.Public.t option
(** [device_curve25519 d] is [d]'s identity key, and [None] when it published
    none or published one that is not a key. *)

val track_users : t -> Matrix_proto.Id.User_id.t list -> unit
(** [track_users t users] starts following [users]' device lists, marking each
    newly tracked user outdated so that the next {!outgoing_requests} fetches
    them. Call it on joining an encrypted room, and when a member joins one. *)

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
  t -> Matrix_client.Crypto_key.Curve25519.Public.t -> device option
(** [find_device_by_curve25519 t key] is the device that published [key] as its
    Curve25519 identity key. *)

val identity_master_key :
  t -> Matrix_proto.Id.User_id.t -> Keys.cross_signing_key option
(** [identity_master_key t user] is the validated master key currently held for
    [user], reconstructed as a wire object suitable for signing. *)

val identity_self_signing_key :
  t -> Matrix_proto.Id.User_id.t -> Keys.cross_signing_key option
(** [identity_self_signing_key t user] is the validated self-signing key
    currently held for [user], reconstructed as a wire object. *)

val identity_user_signing_key :
  t -> Matrix_proto.Id.User_id.t -> Keys.cross_signing_key option
(** [identity_user_signing_key t user] is the validated local user-signing key,
    when one was supplied by the server. *)

val identity_status : t -> Matrix_proto.Id.User_id.t -> identity_status option
(** [identity_status t user] is the current validated identity state for [user],
    if one is known. *)

val identity_has_pin_violation : t -> Matrix_proto.Id.User_id.t -> bool
(** Whether another user's current master key differs from the pinned TOFU key.
*)

val pin_user_identity : t -> Matrix_proto.Id.User_id.t -> unit
(** Pin another user's current master key without verifying the identity. *)

val trust_user_identity : t -> Matrix_proto.Id.User_id.t -> unit
(** [trust_user_identity t user] records an interactive verification of the
    currently validated identity. *)

val set_device_trust :
  t ->
  Matrix_proto.Id.User_id.t ->
  device_id:Matrix_proto.Id.Device_id.t ->
  trust ->
  unit
(** [set_device_trust t user ~device_id trust] records a trust decision. Only a
    {!Verified} device of this client's own user is ever sent a forwarded room
    key, and a {!Blacklisted} device is never sent a room key at all. *)

val receive_keys_query : t -> Keys.query_keys_response -> unit
(** [receive_keys_query t response] folds a [/keys/query] response into the
    device list and marks the users it covers no longer outdated. {!sync_hook}
    and {!execute_requests} do this for the requests they perform. *)

val receive_keys_claim : t -> Keys.claim_keys_response -> int
(** [receive_keys_claim t response] opens an outbound Olm session with each
    device in a [/keys/claim] response and is how many it opened. *)

(** {1 Room settings} *)

type room_settings = Matrix_client.Encryption.room_settings = {
  algorithm : string;
  rotation_period_ms : int64;
  rotation_period_msgs : int;
}
(** The type for a room's [m.room.encryption] content. It is
    {!Matrix_client.Encryption.room_settings}, which documents the fields. *)

val enable_room_encryption :
  ?rotation_period_ms:int64 ->
  ?rotation_period_msgs:int ->
  unit ->
  room_settings
(** [enable_room_encryption ()] is the settings that turn encryption on for a
    room, with [m.megolm.v1.aes-sha2] as the algorithm. [rotation_period_ms] and
    [rotation_period_msgs] each default to the specification's value, one week
    and 100 messages. Send {!room_encryption_content} of the result as room
    state. *)

val room_encryption_content : room_settings -> Jsont.json
(** [room_encryption_content s] is [s] as an [m.room.encryption] content. *)

val set_room_encryption_settings :
  t -> Matrix_proto.Id.Room_id.t -> Jsont.json -> unit
(** [set_room_encryption_settings t room content] records [room]'s
    [m.room.encryption] content as seen in room state. {!sync_hook} calls it for
    every such event, so a caller that syncs need not. A room encrypted with an
    algorithm this library cannot speak raises [Eio.Io], since treating it as
    unencrypted would leak. *)

val find_room_settings : t -> Matrix_proto.Id.Room_id.t -> room_settings option
(** [find_room_settings t room] is [room]'s settings, and [None] when its
    [m.room.encryption] has not been seen. *)

val is_room_encrypted : t -> Matrix_proto.Id.Room_id.t -> bool
(** [is_room_encrypted t room] is [true] when [room]'s [m.room.encryption] has
    been seen. *)

(** {1 Requests} *)

type request = Matrix_client.Encryption.request
(** The type for something the machine needs the server to do. It is
    {!Matrix_client.Encryption.type-request}. *)

val pp_request : Format.formatter -> request -> unit
(** [pp_request ppf r] prints a one-line description of [r] on [ppf]. It names
    no key material. *)

val outgoing_requests : t -> request list
(** [outgoing_requests t] is what the machine's current state calls for,
    recomputed on each call. Performing nothing and calling again is the same
    list. *)

val execute_requests :
  ?on_error:(Error.err -> unit) -> t -> Client.t -> request list -> unit
(** [execute_requests t c requests] performs each request in order, folds the
    answer back into the machine and marks it sent. A request that fails is
    logged and left for a later call to retry rather than stopping the rest of
    the batch. [on_error], when supplied, receives each failed request after it
    is logged. *)

type room_key_bundle_outcome =
  Matrix_client.Encryption_driver.room_key_bundle_outcome

val accept_received_room_key_bundle :
  ?now:Ptime.t ->
  t ->
  Client.t ->
  joined:bool ->
  Matrix_client.Encryption.received_key_bundle ->
  room_key_bundle_outcome
(** Advances one retained MSC4268 bundle through its fresh sender-key query,
    encrypted-media download and import lifecycle. See
    {!Matrix_client.Encryption_driver.accept_received_room_key_bundle}. *)

(** {1 Processing a sync} *)

type to_device_event = Matrix_client.Encryption.to_device_event
(** The type for what a to-device event turned into. It is
    {!Matrix_client.Encryption.type-to_device_event}. *)

val pp_to_device_event : Format.formatter -> to_device_event -> unit
(** [pp_to_device_event ppf e] prints a one-line description of [e] on [ppf]. It
    names no key material and no event content. *)

type outcome = Matrix_client.Encryption.outcome
(** The type for everything one sync response changed. It is
    {!Matrix_client.Encryption.outcome}. *)

val process_sync : t -> Matrix_proto.Sync.Response.t -> outcome
(** [process_sync t response] folds a sync response into the machine and is what
    it asks for next. It performs no request. *)

val process_sliding_sync : t -> Matrix_proto.Sliding_sync.Response.t -> outcome
(** [process_sliding_sync t response] folds an MSC4186 response into the
    machine. An absent one-time-key count leaves the prior count unchanged. *)

val sync_hook :
  ?on_error:(Error.err -> unit) ->
  t ->
  Client.t ->
  Matrix_proto.Sync.Response.t ->
  outcome
(** [sync_hook t c response] is {!process_sync} followed by {!execute_requests}
    on everything it asked for, and is what a sync loop calls for each response.
    The outcome is always returned, even when a request in the batch fails, so
    that a caller can route the verification and secret events the machine does
    not handle itself; those are one-shot and would otherwise be lost. *)

val sync_hook_sliding :
  ?on_error:(Error.err -> unit) ->
  t ->
  Client.t ->
  Matrix_proto.Sliding_sync.Response.t ->
  outcome
(** [sync_hook_sliding t client response] is the MSC4186 counterpart of
    {!sync_hook}. *)

(** {1 Room events} *)

type decrypted_event = Matrix_client.Encryption.decrypted_event
(** The type for a room event recovered from [m.room.encrypted]. It is
    {!Matrix_client.Encryption.decrypted_event}. *)

type decrypt_error = Matrix_client.Encryption.decrypt_error
(** The type for why a room event did not decrypt. It is
    {!Matrix_client.Encryption.type-decrypt_error}. *)

val pp_decrypt_error : Format.formatter -> decrypt_error -> unit
(** [pp_decrypt_error ppf e] prints a one-line description of [e] on [ppf]. *)

val decrypt_room_event :
  t ->
  Matrix_proto.Id.Room_id.t ->
  Matrix_proto.Event.Raw_event.t ->
  (decrypted_event, decrypt_error) result
(** [decrypt_room_event t room event] decrypts a Megolm [m.room.encrypted]
    timeline event. It returns a result rather than raising. An unknown session
    is a normal state of affairs rather than an I/O failure, and the answer to
    it is {!request_room_key}. *)

val request_room_key :
  t ->
  room_id:Matrix_proto.Id.Room_id.t ->
  session_id:Matrix_proto.Id.Session_id.t ->
  ?sender_key:Matrix_client.Crypto_key.Curve25519.Public.t ->
  unit ->
  request
(** [request_room_key t ~room_id ~session_id ()] is the [m.room_key_request] to
    send to every other device of this user, for {!execute_requests} to perform.
    [sender_key] names the device that created the session and defaults to
    absent. *)

val encrypt_room_event :
  t ->
  Client.t ->
  Matrix_proto.Id.Room_id.t ->
  event_type:string ->
  content:Jsont.json ->
  members:Matrix_proto.Id.User_id.t list ->
  Jsont.json
(** [encrypt_room_event t c room ~event_type ~content ~members] shares the
    room's Megolm session with any device of [members] that lacks it and is the
    [m.room.encrypted] content to send. A key query, key claim or to-device send
    that fails raises [Eio.Io]. *)

val send_encrypted :
  t ->
  Client.t ->
  Matrix_proto.Id.Room_id.t ->
  event_type:string ->
  content:Jsont.json ->
  members:Matrix_proto.Id.User_id.t list ->
  Matrix_proto.Id.Event_id.t
(** [send_encrypted t c room ~event_type ~content ~members] encrypts an event
    with {!encrypt_room_event}, sends it as [m.room.encrypted], and is the
    identifier the server gave it.

    [members] is everybody who should be able to read the message. Nothing here
    holds room state, so the caller supplies it. *)

val send_encrypted_text :
  t ->
  Client.t ->
  Matrix_proto.Id.Room_id.t ->
  body:string ->
  members:Matrix_proto.Id.User_id.t list ->
  Matrix_proto.Id.Event_id.t
(** [send_encrypted_text t c room ~body ~members] is {!send_encrypted} for a
    plain [m.room.message] of msgtype [m.text]. *)

(** {1 Key backup} *)

val enable_backup :
  t ->
  version:string ->
  ?decryption_key:Backup.Decryption_key.t ->
  Backup.encryption_key ->
  unit
(** [enable_backup t ~version key] points the machine at the backup [version],
    whose public key is [key]. [decryption_key] defaults to absent, and without
    it the machine can upload to the backup but not read it back. Every inbound
    Megolm session held, and every one that arrives afterwards, becomes pending
    for {!backup_pending}. *)

val disable_backup : t -> unit
(** [disable_backup t] forgets the backup version and its keys. Nothing is
    deleted server-side. *)

val backup_version : t -> string option
(** [backup_version t] is the version the machine writes to, when a backup is
    enabled. *)

val backup_pending_count : t -> int
(** [backup_pending_count t] is how many inbound Megolm sessions
    {!backup_pending} would upload. It is [0] while no backup is enabled. *)

val backup_pending : t -> Client.t -> int
(** [backup_pending t c] uploads the inbound Megolm sessions not yet in the key
    backup and is how many it sent. A failed upload raises [Eio.Io]. *)

val restore_from_backup : t -> Client.t -> int
(** [restore_from_backup t c] imports the whole key backup the machine is
    pointed at and is how many sessions it added. A machine with no backup
    enabled, or with one it has no decryption key for, raises [Eio.Io], as does
    a backup that cannot be fetched. *)

val restore_room_from_backup : t -> Client.t -> Matrix_proto.Id.Room_id.t -> int
(** [restore_room_from_backup t c room] imports only [room]'s backup keys and
    returns how many sessions it added. It returns [0] when backup reading is
    not enabled. *)

val restore_session_from_backup :
  t ->
  Client.t ->
  room_id:Matrix_proto.Id.Room_id.t ->
  session_id:Matrix_proto.Id.Session_id.t ->
  int
(** [restore_session_from_backup t c ~room_id ~session_id] imports exactly one
    backup session and returns [0] when backup reading is not enabled. *)

type share_room_history_outcome =
      Matrix_client.Encryption_driver.share_room_history_outcome =
  | History_not_shared_visibility
  | History_not_shared_identity
  | History_no_keys
  | History_shared of int

type share_room_history_error =
      Matrix_client.Encryption_driver.share_room_history_error =
  | Share_encryption_error of Matrix_client.Error.t
  | Share_media_error of Matrix_client.Media.encrypted_error

type invite_outcome = Matrix_client.Encryption_driver.invite_outcome =
  | Invite_sent of share_room_history_outcome

type invite_error = Matrix_client.Encryption_driver.invite_error =
  | Invite_share_error of share_room_history_error
  | Invite_request_error of Matrix_client.Error.t

val share_room_history :
  t ->
  Client.t ->
  room_id:Matrix_proto.Id.Room_id.t ->
  recipient:Matrix_proto.Id.User_id.t ->
  history_visibility:Matrix_proto.Event.History_visibility.t ->
  (share_room_history_outcome, share_room_history_error) result
(** [share_room_history t c ...] shares available room history using an
    authenticated MSC4268 room-key bundle after refreshing and claiming the
    recipient's devices. *)

val invite_user_by_id :
  t ->
  Client.t ->
  room_id:Matrix_proto.Id.Room_id.t ->
  user_id:Matrix_proto.Id.User_id.t ->
  ?reason:string ->
  history_visibility:Matrix_proto.Event.History_visibility.t ->
  unit ->
  (invite_outcome, invite_error) result
(** [invite_user_by_id t c ...] shares available MSC4268 history before the
    ordinary invite and then sends that invite. A non-applicable share is a
    successful no-op; sharing and invite failures remain typed separately. *)

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
