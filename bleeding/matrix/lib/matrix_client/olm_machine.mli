(** olm_machine — one device's Olm account and its sessions with other devices.

    A machine owns to-device encryption only. The Megolm sessions of a room
    belong to {!Encryption}, which rotates them according to that room's
    [m.room.encryption] settings.

    Sessions are held in memory. A caller that wants them to outlive the process
    writes them out with {!olm_sessions} and feeds them back with
    {!store_olm_session}. *)

type t
(** The type for Olm machines. *)

val create : random:Random.t -> unit -> t
(** [create ~random ()] is a machine holding a fresh account and no session. *)

val of_account : Olm_account.t -> t
(** [of_account account] is a machine that adopts [account] rather than
    generating one, which is what restoring a device from a store needs. The
    identity keys of a Matrix device outlive any one process, and regenerating
    them would publish a new device.

    It starts with no session. *)

val account : t -> Olm_account.t
(** [account t] is the account [t] encrypts under. *)

(** {1 Sessions} *)

val find_olm_session :
  t -> their_identity_key:Crypto_key.Curve25519.Public.t -> Olm_session.t option
(** [find_olm_session t ~their_identity_key] is the session with the greatest
    successful-decrypt timestamp with that device, or [None] when [t] holds
    none. A session which has never received a message uses its creation time.
    The machine retains at most four sessions per peer. This policy does not
    expire sessions by age and does not implement [m.dummy] keepalive events. *)

val olm_sessions : t -> Olm_session.t list
(** [olm_sessions t] is every session [t] holds, across all devices. Each
    carries the peer's identity key, so nothing further is needed to file them.
*)

val store_olm_session : t -> Olm_session.t -> unit
(** [store_olm_session t session] adopts [session], filing it under
    {!Olm_session.val-their_identity_key}. An existing record with the same
    session ID is replaced, and the least-recently-used sessions are evicted
    beyond four. *)

val create_olm_session :
  random:Random.t ->
  t ->
  their_identity_key:Crypto_key.Curve25519.Public.t ->
  their_one_time_key:Crypto_key.Curve25519.Public.t ->
  (Olm_session.t, Olm_error.t) result
(** [create_olm_session ~random t ~their_identity_key ~their_one_time_key] opens
    a session against keys claimed from [/keys/claim] and stores it. *)

val create_inbound_session :
  t ->
  their_identity_key:Crypto_key.Curve25519.Public.t ->
  ciphertext:string ->
  (Olm_session.t * string, Olm_error.t) result
(** [create_inbound_session t ~their_identity_key ~ciphertext] opens a session
    from a base64 pre-key message, stores it, and returns it with that message's
    plaintext. *)

(** {1 To-device messages} *)

val encrypt_to_device :
  random:Random.t ->
  t ->
  their_identity_key:Crypto_key.Curve25519.Public.t ->
  their_one_time_key:Crypto_key.Curve25519.Public.t ->
  plaintext:string ->
  (Olm_session.message, Olm_error.t) result
(** [encrypt_to_device ~random t ~their_identity_key ~their_one_time_key
     ~plaintext] encrypts for one device, opening a session from
    [their_one_time_key] when [t] holds none yet. *)

val decrypt_to_device :
  random:Random.t ->
  t ->
  their_identity_key:Crypto_key.Curve25519.Public.t ->
  Olm_session.message ->
  (string, Olm_error.t) result
(** [decrypt_to_device ~random t ~their_identity_key message] tries each session
    held for that device and, for a {!Olm_session.Pre_key} message, falls back
    to opening a new inbound session.

    It is [Error No_session] when [message] is {!Olm_session.Normal} and no
    session held for that device decrypts it, since nothing can be opened from a
    message that does not carry the material to start one. For a
    {!Olm_session.Pre_key} message, a failed attempt to open a new session
    instead reports whatever specific {!Olm_error.t} that attempt failed with.
*)
