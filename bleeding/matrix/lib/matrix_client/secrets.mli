(** secrets — the account-data endpoints secret storage is kept in.

    A secret is one account-data event, holding one encrypted copy per secret
    storage key id, so that a user with several keys can read the same secret
    with any of them. The key descriptions live in account data as well, and
    [m.secret_storage.default_key] names the one a client should reach for
    first. {!Secret_storage} holds the cryptography these endpoints move.

    @see <https://spec.matrix.org/v1.11/client-server-api/#secret-storage>
      Secret storage *)

val default_key_event_type : string
(** [default_key_event_type] is ["m.secret_storage.default_key"], the
    account-data event naming the user's default key. *)

val key_event_type : key_id:string -> string
(** [key_event_type ~key_id] is ["m.secret_storage.key." ^ key_id], the
    account-data event type a key description lives under. *)

val get_default_key_id : Client.t -> (string option, Error.t) result
(** [get_default_key_id client] is the id of the user's default secret storage
    key, and [None] when no default is set. Uses
    [GET
     /_matrix/client/v3/user/{userId}/account_data/m.secret_storage.default_key].
*)

val set_default_key_id : Client.t -> key_id:string -> (unit, Error.t) result
(** [set_default_key_id client ~key_id] marks [key_id] as the default key. Uses
    [PUT
     /_matrix/client/v3/user/{userId}/account_data/m.secret_storage.default_key].
*)

val get_key_description :
  Client.t ->
  key_id:string ->
  (Secret_storage.Key_description.t, Error.t) result
(** [get_key_description client ~key_id] is the description of the key [key_id]
    names. Uses
    [GET
     /_matrix/client/v3/user/{userId}/account_data/m.secret_storage.key.{keyId}].
*)

val put_key_description :
  Client.t ->
  key_id:string ->
  Secret_storage.Key_description.t ->
  (unit, Error.t) result
(** [put_key_description client ~key_id description] publishes [description],
    which holds no secret material and is what makes a key usable from the
    user's other devices. Uses
    [PUT
     /_matrix/client/v3/user/{userId}/account_data/m.secret_storage.key.{keyId}].
*)

val get_secret :
  Client.t ->
  key_id:string ->
  key:Secret_storage.key ->
  name:string ->
  (string, Error.t) result
(** [get_secret client ~key_id ~key ~name] is the secret [name], decrypted from
    the copy stored under [key_id]. It is [Error (Json_error _)] when there is
    no copy for [key_id] or [key] does not decrypt it. Uses
    [GET /_matrix/client/v3/user/{userId}/account_data/{name}]. *)

val get_secret_opt :
  Client.t ->
  key_id:string ->
  key:Secret_storage.key ->
  name:string ->
  (string option, Error.t) result
(** [get_secret_opt] is like {!get_secret}, but returns [None] when the
    account-data event is absent or has no copy for [key_id]. Malformed data and
    decryption failures, including plaintext that is not valid UTF-8, remain
    errors. *)

val store_secret :
  Client.t ->
  random:Random.t ->
  key_id:string ->
  key:Secret_storage.key ->
  name:string ->
  string ->
  (unit, Error.t) result
(** [store_secret client ~random ~key_id ~key ~name secret] encrypts [secret]
    under [key] and stores it as the copy for [key_id], leaving copies under
    other key ids in place. Rejects a [secret] that is not valid UTF-8. Uses
    [PUT /_matrix/client/v3/user/{userId}/account_data/{name}]. *)

type store
(** A validated, in-memory handle to one secret-storage key. The credential used
    to open it is never retained. *)

type created_store = { store : store; key_id : string; recovery_key : string }
(** The result of {!create_secret_store}. [recovery_key] is a Base58
    representation suitable for handing to the user; it is not retained by the
    returned store. *)

val create_secret_store :
  Client.t ->
  random:Random.t ->
  ?passphrase:string ->
  ?secrets:(string * string) list ->
  unit ->
  (created_store, Error.t) result
(** [create_secret_store client ~random ?passphrase ?secrets] creates a fresh
    SSSS key and its in-memory {!type-store}. A passphrase makes the key PBKDF2-
    derived; without one, the returned [recovery_key] is the only recovery
    credential. The key description is published first, supplied secrets are
    encrypted and published next, and [m.secret_storage.default_key] is set
    last, matching Rust's creation order. [secrets] is a list of
    [(event_type, plaintext)] values and defaults to empty; it is a bounded
    primitive and does not discover or export crypto identity keys itself.

    Account-data writes are not transactional: a failure leaves successful
    earlier writes in place. In particular, the default key is never changed
    when description or secret publication fails. *)

val open_secret_store : Client.t -> credential:string -> (store, Error.t) result
(** [open_secret_store client ~credential] fetches the default key description
    and validates [credential] as either its PBKDF2 passphrase or Base58
    recovery key. Unsupported algorithms, absent key metadata, and an incorrect
    credential are errors. [client] must carry a session. *)

val create_recovery_store :
  Client.t ->
  random:Random.t ->
  ?passphrase:string ->
  private_identity:Cross_signing.private_identity ->
  ?backup_key:Backup.Decryption_key.t ->
  unit ->
  (created_store, Error.t) result
(** [create_recovery_store client ~random ~private_identity ?backup_key ()]
    creates a secret store containing the present cross-signing seeds in Rust's
    export order: master, user-signing, self-signing, and then the optional
    room-key backup key. Seeds use unpadded standard Base64 and are never
    logged. Missing seeds are omitted, so a store made from a partial identity
    provides correspondingly incomplete recovery. The identity must belong to
    the authenticated session; this is checked before any account-data write. *)

val store_key_id : store -> string
(** [store_key_id store] is the validated default key id. *)

val store_user_id : store -> Matrix_proto.Id.User_id.t
(** [store_user_id store] is the account which opened [store]. *)

val store_homeserver : store -> Uriz.t
(** [store_homeserver store] is the homeserver from which [store] was opened. *)

val get_store_secret : store -> name:string -> (string option, Error.t) result
(** [get_store_secret store ~name] decrypts an optional account-data secret. *)

type backup_import_result =
  | Not_configured
  | Imported
      (** The result of importing the optional room-key backup secret. *)

type backup_import_error =
  | Inconsistent_backup_key
  | Missing_or_invalid_backup_secret
  | Other of Error.t
      (** Typed failure classes used by recovery repair. Invalid Base64,
          unsupported backup algorithms/authentication data, transport failures,
          and persistence failures are [Other]; a public-key mismatch is
          [Inconsistent_backup_key]. *)

val import_backup_typed :
  store ->
  encryption:Encryption_driver.t ->
  (backup_import_result, backup_import_error) result
(** [import_backup_typed] validates and imports the optional current backup,
    preserving the distinction between an absent secret/server backup
    ([Not_configured], with no repair needed), an unreadable encrypted copy
    ([Missing_or_invalid_backup_secret], repairable), and errors that must be
    propagated. *)

val put_store_secret :
  store -> random:Random.t -> name:string -> string -> (unit, Error.t) result
(** [put_store_secret store ~random ~name secret] encrypts and stores [secret]
    under the validated key held by [store]. The account-data event is read
    first and copies encrypted under other key ids are preserved. Invalid UTF-8
    is rejected before any request is made. *)

val export_recovery_secrets :
  store ->
  random:Random.t ->
  private_identity:Cross_signing.private_identity ->
  ?backup_key:Backup.Decryption_key.t ->
  unit ->
  (unit, Error.t) result
(** [export_recovery_secrets] writes the available master, user-signing,
    self-signing, and optional room-key backup seeds into this already-opened
    store, in Rust's fixed order. Writes are sequential and not transactional.
*)

val import_backup :
  store -> encryption:Encryption_driver.t -> (unit, Error.t) result
(** [import_backup store ~encryption] reads the optional
    {!Secret_storage.secret_megolm_backup_v1} secret, validates it against the
    current server backup version and its public key, then enables that exact
    version and persists [encryption]. It is a no-op when the secret is absent;
    it does not download room keys. Validation completes before the encryption
    machine is changed. *)

val import_cross_signing :
  store ->
  encryption:Encryption.t ->
  (Cross_signing.private_identity, Error.t) result
(** [import_cross_signing] refreshes and validates this user's public
    cross-signing identity, then atomically imports any available private seeds
    from the three well-known secret-storage names. When the self-signing seed
    and this device's queried keys are available, it also signs the current
    device and refreshes the query, matching the Rust recovery path. The store
    and encryption machine must belong to the same user. *)
