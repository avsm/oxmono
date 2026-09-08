(** encryption_driver — an {!Encryption} machine joined to a client.

    A driver owns one {!Encryption.t} and, optionally, the {!Crypto_store.t} it
    is persisted through. It performs the requests the machine asks for, folds
    the answers back, and reports what reached the server. Everything a caller
    does that is not I/O is done on {!machine}.

    @see <https://spec.matrix.org/v1.11/client-server-api/#end-to-end-encryption>
      End-to-end encryption *)

type t
(** The type for drivers. Like the machine it holds, a driver is not
    thread-safe. *)

val v : ?store:Crypto_store.t -> Encryption.t -> t
(** [v machine] is a driver for [machine]. [store] defaults to absent, and
    without one {!save} writes nothing. *)

val create :
  random:Random.t ->
  user_id:Matrix_proto.Id.User_id.t ->
  device_id:Matrix_proto.Id.Device_id.t ->
  ?store:Crypto_store.t ->
  unit ->
  (t, Error.t) result
(** [create ~random ~user_id ~device_id ?store ()] restores the machine from
    [store] when it holds an account for this profile, and otherwise generates a
    fresh Olm account from [random]. [store] defaults to absent, and a driver
    without one runs entirely in memory and writes nothing. Restored MSC4268
    invite-acceptance records that are future-dated or at least 24 hours old are
    removed and the cleaned snapshot is persisted before returning.

    It is an [Error] when the stored account is present but unreadable, or when
    an expired-record cleanup cannot be persisted. Overwriting an unreadable
    account would silently lose the ability to decrypt existing history, so the
    decision is the caller's.

    Raises [Eio.Io] on a filesystem failure. *)

val create_with_account :
  random:Random.t ->
  user_id:Matrix_proto.Id.User_id.t ->
  device_id:Matrix_proto.Id.Device_id.t ->
  account:Olm.Account.t ->
  ?store:Crypto_store.t ->
  unit ->
  (t, Error.t) result
(** [create_with_account ~random ~user_id ~device_id ~account ?store ()] wraps a
    fresh, empty machine around exactly [account], retaining its identity keys
    so its first outgoing request publishes them. The account is not copied; the
    caller must stop separately mutating the handed-off account. With no store
    or an empty store, the store is associated with the new machine. A non-empty
    store is rejected with {!Error.Json_error}: this constructor is for a
    pre-OAuth new account, while {!create} is the resume path, and neither loads
    nor overwrites that snapshot.

    Raises [Eio.Io] on a filesystem failure. *)

val machine : t -> Encryption.t
(** [machine t] is the machine [t] drives. *)

val store : t -> Crypto_store.t option
(** [store t] is where {!save} writes, when [t] was given one. *)

val save : t -> (unit, Error.t) result
(** [save t] writes the machine's state through [t]'s store, and is [Ok ()] when
    there is none. Nothing else here writes to disk.

    Raises [Eio.Io] on a filesystem failure. *)

val execute_requests :
  ?on_error:(Error.t -> unit) ->
  t ->
  Client.t ->
  Encryption.request list ->
  unit
(** [execute_requests t client requests] performs each request in order, folds
    the answer back into the machine and marks it sent. A request that fails is
    logged and left for a later call to retry, rather than stopping the rest of
    the batch. [on_error], when supplied, receives each failed request after it
    is logged; exceptions from it are logged and ignored. *)

type room_key_bundle_outcome =
  | Bundle_not_applicable
  | Bundle_imported of int
  | Bundle_rejected_sender
  | Bundle_discarded_not_found
  | Bundle_discarded_malformed of string
  | Bundle_retry_key_query of Error.t
  | Bundle_retry_download of Media.encrypted_error
      (** The result of advancing one received MSC4268 bundle. Retry outcomes
          retain both the received bundle and its invite-acceptance gate; every
          terminal outcome removes them. *)

type share_room_history_outcome =
  | History_not_shared_visibility
  | History_not_shared_identity
  | History_no_keys
  | History_shared of int
      (** The result of attempting to share the room's historic keys. *)

type share_room_history_error =
  | Share_encryption_error of Error.t
  | Share_media_error of Media.encrypted_error
      (** Errors from the required backup, key, media, or to-device steps. *)

type invite_outcome =
  | Invite_sent of share_room_history_outcome
      (** The ordinary invite was sent after the history-share attempt. *)

type invite_error =
  | Invite_share_error of share_room_history_error
  | Invite_request_error of Error.t
      (** A history-share or ordinary invite failure, respectively. *)

val accept_received_room_key_bundle :
  ?now:Ptime.t ->
  t ->
  Client.t ->
  joined:bool ->
  Encryption.received_key_bundle ->
  room_key_bundle_outcome
(** [accept_received_room_key_bundle t client ~joined bundle] rechecks the
    invite/join/time gate, forcibly refreshes the sender's device and
    cross-signing keys, downloads and authenticates the encrypted media, and
    imports same-room keys. A self-signed but non-cross-signed sender is
    rejected. Query and non-404 media failures remain retryable; a 404 or
    malformed bundle is terminal. *)

val sync_hook :
  ?on_error:(Error.t -> unit) ->
  t ->
  Client.t ->
  Matrix_proto.Sync.Response.t ->
  Encryption.outcome
(** [sync_hook t client response] is {!Encryption.process_sync} followed by
    {!execute_requests} on everything it asked for. The outcome is always
    returned, even when a request in the batch fails, so that a caller can route
    the verification and secret events the machine does not handle itself; those
    are one-shot and would otherwise be lost. A failed request is logged and
    left for the next sync to retry rather than failing the rest of the batch.
*)

val sync_hook_sliding :
  ?on_error:(Error.t -> unit) ->
  t ->
  Client.t ->
  Matrix_proto.Sliding_sync.Response.t ->
  Encryption.outcome
(** [sync_hook_sliding t client response] is the MSC4186 counterpart of
    {!sync_hook}, using {!Encryption.process_sliding_sync}. *)

val encrypt_room_event :
  t ->
  Client.t ->
  Matrix_proto.Id.Room_id.t ->
  event_type:string ->
  content:Jsont.json ->
  members:Matrix_proto.Id.User_id.t list ->
  (Jsont.json, Error.t) result
(** [encrypt_room_event t client room ~event_type ~content ~members] is the
    [m.room.encrypted] content to send in [room]. It performs
    {!Encryption.ensure_sessions} first, so a device of [members] this client
    has never spoken to is queried and claimed, then shares the room's Megolm
    session with every device that lacks it.

    [members] must be everybody who should be able to read the message. Nothing
    here holds room state or reads a member list of its own. *)

val send_encrypted :
  t ->
  Client.t ->
  Matrix_proto.Id.Room_id.t ->
  event_type:string ->
  content:Jsont.json ->
  members:Matrix_proto.Id.User_id.t list ->
  (Matrix_proto.Id.Event_id.t, Error.t) result
(** [send_encrypted t client room ~event_type ~content ~members] encrypts an
    event with {!encrypt_room_event}, sends it as [m.room.encrypted], and is the
    identifier the server gave it. *)

val backup_pending : t -> Client.t -> (int, Error.t) result
(** [backup_pending t client] uploads every inbound Megolm session not yet in
    the key backup in deterministic batches of at most 100, and is how many it
    sent. Calls on the same driver are serialized, and each successful batch is
    marked and saved before the next request, so a later failure leaves only the
    remaining sessions retryable. A server [M_NOT_FOUND], bare HTTP 404, or
    [M_WRONG_ROOM_KEYS_VERSION] disables and persists the local backup before
    returning that error, matching matrix-rust-sdk's response to a deleted or
    concurrently rotated version. It is [Ok 0] when there is nothing to do or no
    backup is enabled. *)

val restore_from_backup : t -> Client.t -> (int, Error.t) result
(** [restore_from_backup t client] downloads the whole backup the machine is
    pointed at, decrypts it and imports the sessions not already held at an
    equal or lower message index, and is how many it imported.

    It is an [Error] when no backup is enabled, or when the one that is enabled
    came without a decryption key. {!Encryption.enable_backup} is what supplies
    both. *)

val restore_room_from_backup :
  t -> Client.t -> Matrix_proto.Id.Room_id.t -> (int, Error.t) result
(** [restore_room_from_backup t c room] fetches and imports only [room]'s
    configured backup keys. It is [Ok 0] when backup reading is not enabled. *)

val restore_session_from_backup :
  t ->
  Client.t ->
  room_id:Matrix_proto.Id.Room_id.t ->
  session_id:Matrix_proto.Id.Session_id.t ->
  (int, Error.t) result
(** [restore_session_from_backup t c ~room_id ~session_id] fetches and imports
    exactly one configured backup session. *)

val share_room_history :
  t ->
  Client.t ->
  room_id:Matrix_proto.Id.Room_id.t ->
  recipient:Matrix_proto.Id.User_id.t ->
  history_visibility:Matrix_proto.Event.History_visibility.t ->
  (share_room_history_outcome, share_room_history_error) result
(** [share_room_history t c ...] builds and sends an authenticated MSC4268
    room-key bundle after restoring the room's backup keys and refreshing and
    claiming the recipient's devices. Required transport failures are returned
    and leave unsent machine requests retryable. *)

val invite_user_by_id :
  t ->
  Client.t ->
  room_id:Matrix_proto.Id.Room_id.t ->
  user_id:Matrix_proto.Id.User_id.t ->
  ?reason:string ->
  history_visibility:Matrix_proto.Event.History_visibility.t ->
  unit ->
  (invite_outcome, invite_error) result
(** [invite_user_by_id t c ...] attempts MSC4268 history sharing before the
    ordinary [/rooms/{roomId}/invite] request. Missing identity, unsupported
    visibility, and an empty key set are successful no-ops and still allow the
    invite. A real sharing or invite failure prevents later steps and remains
    typed as either [Invite_share_error] or [Invite_request_error]. *)
