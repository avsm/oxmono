(** secrets — the account-data endpoints secret storage is kept in, raising
    instead of returning a result.

    A secret is one account-data event holding one encrypted copy per secret
    storage key id. {!Secret_storage} holds the cryptography these endpoints
    move. Every function here raises [Eio.Io] carrying an {!Error.type-err}
    where its {!Matrix_client.Secrets} counterpart returns an error. *)

val default_key_event_type : string
(** [default_key_event_type] is ["m.secret_storage.default_key"], the
    account-data event naming the user's default key. *)

val key_event_type : key_id:string -> string
(** [key_event_type ~key_id] is ["m.secret_storage.key." ^ key_id], the
    account-data event type a key description lives under. *)

val get_default_key_id : Client.t -> string option
(** [get_default_key_id c] is {!Matrix_client.Secrets.get_default_key_id} with
    the result unwrapped. It is [None] when no default is set. *)

val set_default_key_id : Client.t -> key_id:string -> unit
(** [set_default_key_id c ~key_id] is
    {!Matrix_client.Secrets.set_default_key_id} with the result unwrapped. *)

val get_key_description :
  Client.t -> key_id:string -> Secret_storage.Key_description.t
(** [get_key_description c ~key_id] is
    {!Matrix_client.Secrets.get_key_description} with the result unwrapped. *)

val put_key_description :
  Client.t -> key_id:string -> Secret_storage.Key_description.t -> unit
(** [put_key_description c ~key_id description] is
    {!Matrix_client.Secrets.put_key_description} with the result unwrapped.
    [description] holds no secret material and is what makes a key usable from
    the user's other devices. *)

val get_secret :
  Client.t -> key_id:string -> key:Secret_storage.key -> name:string -> string
(** [get_secret c ~key_id ~key ~name] is {!Matrix_client.Secrets.get_secret}
    with the result unwrapped. A [key] that does not decrypt the copy stored
    under [key_id], and a secret with no copy for [key_id], both raise. *)

val get_secret_opt :
  Client.t ->
  key_id:string ->
  key:Secret_storage.key ->
  name:string ->
  string option
(** [get_secret_opt] is {!Matrix_client.Secrets.get_secret_opt} with the result
    unwrapped. It returns [None] for an absent event or copy. *)

val store_secret :
  Client.t ->
  random:Matrix_client.Random.t ->
  key_id:string ->
  key:Secret_storage.key ->
  name:string ->
  string ->
  unit
(** [store_secret c ~random ~key_id ~key ~name secret] is
    {!Matrix_client.Secrets.store_secret} with the result unwrapped. Copies of
    [name] under other key ids are left in place. *)

type store = Matrix_client.Secrets.store
type created_store = Matrix_client.Secrets.created_store

val create_secret_store :
  Client.t ->
  random:Matrix_client.Random.t ->
  ?passphrase:string ->
  ?secrets:(string * string) list ->
  unit ->
  created_store
(** [create_secret_store c ~random ?passphrase ?secrets] creates and publishes a
    fresh SSSS key, returning its in-memory store, key id, and Base58 recovery
    key. It raises after the first failed account-data write; earlier writes are
    not rolled back. *)

val open_secret_store : Client.t -> credential:string -> store
(** [open_secret_store c ~credential] validates and opens the default SSSS key,
    raising on missing metadata, an unsupported algorithm, or a wrong
    passphrase/recovery key. *)

val create_recovery_store :
  Client.t ->
  random:Matrix_client.Random.t ->
  ?passphrase:string ->
  private_identity:Matrix_client.Cross_signing.private_identity ->
  ?backup_key:Matrix_client.Backup.Decryption_key.t ->
  unit ->
  created_store
(** [create_recovery_store c ~random ~private_identity ?backup_key ()] publishes
    the available cross-signing seeds and optional room-key backup key in
    recovery order, raising on the first account-data failure. *)

val store_key_id : store -> string
val store_user_id : store -> Matrix_proto.Id.User_id.t
val store_homeserver : store -> Uriz.t
val get_store_secret : store -> name:string -> string option

val put_store_secret :
  store -> random:Matrix_client.Random.t -> name:string -> string -> unit
(** [put_store_secret store ~random ~name secret] encrypts and publishes a
    secret with the validated key held by [store], preserving other key copies.
    It raises on malformed existing account data, invalid UTF-8, or an HTTP
    failure. *)

val export_recovery_secrets :
  store ->
  random:Matrix_client.Random.t ->
  private_identity:Matrix_client.Cross_signing.private_identity ->
  ?backup_key:Matrix_client.Backup.Decryption_key.t ->
  unit ->
  unit
(** Exports the available recovery seeds into this already-opened store, in
    Rust's fixed order. Writes are sequential and not transactional. *)

val import_backup : store -> encryption:Encryption.t -> unit
(** [import_backup store ~encryption] imports and validates the optional
    server-side room-key backup secret, enables the matching current server
    version, and persists the machine. An absent secret or server backup is a
    no-op; an unreadable encrypted copy raises. It performs no room-key
    download. *)

val import_cross_signing :
  store ->
  encryption:Encryption.t ->
  Matrix_client.Cross_signing.private_identity
(** Imports the optional well-known cross-signing private seeds after a fresh
    validated [/keys/query], self-signs the current device when possible, and
    persists the resulting public-identity refresh. *)
