(** backup — encrypting and decrypting backed-up room keys.

    Room keys are encrypted client-side under a Curve25519 key the homeserver
    never learns, so that a new device can recover the user's history. Only
    [m.megolm_backup.v1.curve25519-aes-sha2] is implemented. {!Room_keys}
    carries the results to and from the server, and every function needing
    unpredictable bytes takes an explicit {!Random.t}.

    A device reaches a backup's {!Decryption_key.t} in one of three ways. It
    generates one with {!Decryption_key.generate} when it creates the backup, it
    decodes one the user typed with {!Recovery_key.decode}, or it reads one out
    of secret storage, where {!Secrets.get_secret} under
    {!Secret_storage.secret_megolm_backup_v1} answers the unpadded Base64 text
    that {!Decryption_key.of_base64} takes.

    @see <https://spec.matrix.org/v1.11/client-server-api/#server-side-key-backups>
      Server-side key backups *)

type error = [ `Msg of string ]
(** The type for a failure to decode or decrypt. The message names what was
    rejected. *)

val backup_algorithm : string
(** [backup_algorithm] is ["m.megolm_backup.v1.curve25519-aes-sha2"]. *)

(** {1 Backup keys} *)

type encryption_key = Crypto_key.Curve25519.Public.t
(** The public half of a backup key. A device holding only this can write to the
    backup but not read it. *)

module Decryption_key : sig
  (** The private half of a backup key. A device holding it can read the backup.
  *)

  type t
  (** The type for backup decryption keys. *)

  val generate : random:Random.t -> t
  (** [generate ~random] is a fresh key drawn from [random]. *)

  val of_bytes : string -> (t, [> error ]) result @@ portable
  (** [of_bytes b] is the key the 32 bytes [b] hold. [b] is secret material. *)

  val of_base64 : string -> (t, [> error ]) result @@ portable
  (** [of_base64 s] is the key [s] encodes. [s] may be padded or unpadded and is
      the representation stored under {!Secret_storage.secret_megolm_backup_v1}.
  *)

  val to_base64 : t -> string @@ portable
  (** [to_base64 t] is [t] in unpadded base64. It is secret material, and a
      caller that writes it anywhere but its own key store has leaked the
      backup. *)

  val public : t -> encryption_key
  (** [public t] is the half of [t] that encrypts. *)
end

module Recovery_key : sig
  (** The backup key in the form a user writes down. *)

  val encode : Decryption_key.t -> string
  (** [encode key] is [key] in the cryptographic key representation, base58 in
      groups of four.

      @see <https://spec.matrix.org/v1.11/appendices/#cryptographic-key-representation>
        Cryptographic key representation *)

  val decode : string -> (Decryption_key.t, [> error ]) result
  (** [decode s] is the key {!encode} was given. Whitespace in [s] is ignored,
      so the grouping need not be reproduced, and a mistyped character fails the
      parity check rather than yielding a wrong key. *)
end

(** {1 Encrypting and decrypting room keys} *)

type encrypted_session_data = {
  ephemeral : string;
  ciphertext : string;
  mac : string;
}
(** The [session_data] of a backed-up key. All three fields are unpadded base64.
    The v1 backup format authenticates the empty string as its MAC input, not
    the ciphertext or the surrounding [key_backup_data] fields. Successful
    decryption therefore proves only possession of the backup key and valid
    padding; callers must validate and pin the backup version's [auth_data] and
    public key separately. *)

val encrypted_session_data_jsont : encrypted_session_data Jsont.t
(** [encrypted_session_data_jsont] is the JSON codec for
    {!encrypted_session_data}. *)

type key_backup_data = {
  first_message_index : int;
      (** Index of the first message the key can decrypt. *)
  forwarded_count : int;
      (** How many times the key was forwarded between devices. *)
  is_verified : bool;
      (** Whether the backing-up device had verified the key's sender. *)
  session_data : encrypted_session_data;
}
(** One backed-up Megolm session, as [/room_keys/keys] holds it. The legacy
    empty-string MAC does not authenticate [first_message_index],
    [forwarded_count], or [is_verified]; import derives its session and trust
    metadata from the decrypted inner {!backed_up_session_data} instead. *)

val key_backup_data_jsont : key_backup_data Jsont.t
(** [key_backup_data_jsont] is the JSON codec for {!key_backup_data}. *)

type sessions = (string * key_backup_data) list
(** The type for the backed-up sessions of one room, keyed by Megolm session
    identifier as the backup files them. The identifiers are left as the server
    gave them, since a backup written by another client may hold anything. *)

type rooms = (string * sessions) list
(** The type for backed-up sessions keyed by room identifier as the backup files
    them. *)

type backed_up_session_data = {
  algorithm : string;  (** [m.megolm.v1.aes-sha2] for this backup. *)
  forwarding_curve25519_key_chain : string list;
      (** Curve25519 keys the session was forwarded through, if any. *)
  sender_key : string;  (** Unpadded base64 device Curve25519 key. *)
  sender_claimed_keys : (string * string) list;
      (** Algorithm name ([ed25519]) to the sender's signing key. *)
  session_key : string;
      (** Unpadded base64 unsigned version-1 Megolm exported session key. A
          signed version-2 [m.room_key] blob is not valid here. *)
  shared_history : bool;
}
(** The plaintext under {!encrypted_session_data}, the spec's
    [BackedUpSessionData]. The room and session identifiers are not in here. In
    the backup they are the keys of the [/room_keys/keys] maps. *)

val backed_up_session_data_jsont : backed_up_session_data Jsont.t
(** [backed_up_session_data_jsont] is the JSON codec for
    {!backed_up_session_data}. *)

val encrypt_session_data :
  random:Random.t ->
  encryption_key ->
  string ->
  (encrypted_session_data, [> error ]) result
(** [encrypt_session_data ~random key plaintext] encrypts a serialised
    {!backed_up_session_data} for the backup whose public key is [key]. It
    performs an ECDH against a fresh ephemeral key, HKDF-SHA256 to an AES key, a
    MAC key and an IV, then AES-256-CBC and a truncated HMAC-SHA256.

    The MAC covers the empty string rather than the ciphertext, which is what
    the wire format specifies and what every other client checks. It therefore
    does not authenticate the ciphertext or the outer backup metadata; callers
    must validate the backup version's [auth_data] and public key separately. *)

val encrypt_room_key :
  random:Random.t ->
  encryption_key ->
  session_key:string ->
  sender_key:string ->
  (encrypted_session_data, [> error ]) result
(** [encrypt_room_key ~random key ~session_key ~sender_key] wraps [session_key]
    in a minimal {!backed_up_session_data} and encrypts it. [session_key] must
    be an exact unsigned version-1 exported Megolm key or the function returns
    an error before consuming randomness. [sender_key] must be a Curve25519
    public key and is stored in canonical unpadded Base64 form. Build a
    {!backed_up_session_data} and call {!encrypt_session_data} to carry a
    forwarding chain or the sender's claimed keys. *)

val decrypt_room_key :
  Decryption_key.t -> encrypted_session_data -> (string, [> error ]) result
(** [decrypt_room_key key session_data] is the serialised
    {!backed_up_session_data} [session_data] holds. It fails when the MAC does
    not verify or the padding is malformed. The successful result is not a proof
    that the outer [key_backup_data] metadata was authenticated. Parse the
    result with {!parse_recovered_key}. *)

(** {1 Recovering keys} *)

type recovered_room_key = {
  room_id : Matrix_proto.Id.Room_id.t;
  session_id : Matrix_proto.Id.Session_id.t;
  session_key : string;
  sender_key : string;
  algorithm : string;
  forwarded : bool;  (** Whether the session reached the backup forwarded. *)
  sender_claimed_keys : (string * string) list;
      (** Claimed Ed25519 keys carried by the backup entry. *)
  forwarding_curve25519_key_chain : string list;
      (** Forwarding chain carried by the backup entry. *)
  shared_history : bool;
      (** Whether the session was marked as shared room history. *)
}
(** A room key recovered from the backup. *)

val parse_recovered_key :
  room_id:Matrix_proto.Id.Room_id.t ->
  session_id:Matrix_proto.Id.Session_id.t ->
  string ->
  (recovered_room_key, [> error ]) result
(** [parse_recovered_key ~room_id ~session_id json] parses the plaintext
    {!decrypt_room_key} returned, pairing it with the identifiers it was filed
    under in the backup. It fails when [json] is not a
    {!backed_up_session_data}. *)

(** {1 Backup versions} *)

type megolm_v1_auth_data = {
  public_key : encryption_key;
  signatures : Keys.signatures;
}
(** The [auth_data] of an [m.megolm_backup.v1.curve25519-aes-sha2] backup. *)

val megolm_v1_auth_data_jsont : megolm_v1_auth_data Jsont.t
(** [megolm_v1_auth_data_jsont] is the JSON codec for {!megolm_v1_auth_data}. *)

val auth_data_to_json : megolm_v1_auth_data -> Jsont.json
(** [auth_data_to_json d] is [d] in the shape {!Room_keys.create_version} and
    {!Room_keys.update_version} take as [auth_data]. *)

val sign_auth_data :
  signing_key:Crypto_key.Ed25519.Private.t ->
  user_id:Matrix_proto.Id.User_id.t ->
  key_id:Crypto_key.Key_id.t ->
  megolm_v1_auth_data ->
  megolm_v1_auth_data
(** [sign_auth_data ~signing_key ~user_id ~key_id auth_data] adds an Ed25519
    signature over the auth data under [user_id] and [key_id], which names the
    device for a device signature and the master key itself for a cross-signing
    one. Any signature already under that identifier is replaced. *)

(** How a signature over a backup's auth data stands. *)
type signature_state =
  | Missing
  | Invalid
  | Valid_but_not_trusted  (** Verifies, but the signer may not be trusted. *)
  | Valid_and_trusted

val verify_auth_data_signature :
  verify_key:Crypto_key.Ed25519.Public.t ->
  megolm_v1_auth_data ->
  user_id:Matrix_proto.Id.User_id.t ->
  key_id:Crypto_key.Key_id.t ->
  signature_state
(** [verify_auth_data_signature ~verify_key auth_data ~user_id ~key_id] checks
    one signature. It never answers {!Valid_and_trusted}, since whether
    [verify_key] is itself trusted is the caller's judgement. *)

(** How the homeserver's current backup version and this device's persisted
    target line up. *)
type version_state =
  | Absent  (** Neither side has a backup. Creating one is safe. *)
  | Current of string  (** Both sides name this version. *)
  | Server_only of string
      (** The server has this version; the device does not. *)
  | Local_only of string
      (** The device names this version; the server does not. *)
  | Diverged of { server : string; local : string }
      (** Both sides have a version, but they differ. *)

val version_state : server:string option -> local:string option -> version_state
(** [version_state ~server ~local] classifies whether enabling backup may create
    a version, resume the current one, or must stop for recovery. A caller must
    create only on {!Absent}; every other non-current state protects an existing
    local key or server backup from being orphaned. *)

type current_version_state =
  | Compatible
      (** Algorithm and public key agree with this device's persisted target. *)
  | Missing_local_key
  | Unsupported_algorithm of string
  | Malformed_auth_data of string
  | Different_public_key
      (** Whether a server version that has the expected version identifier is
          also safe for this device to write to. Matching the identifier alone
          is not enough: a replaced server version or damaged local state must
          fail closed. *)

val current_version_state :
  algorithm:string ->
  auth_data:Jsont.json ->
  local_key:encryption_key option ->
  current_version_state
(** [current_version_state ~algorithm ~auth_data ~local_key] validates the
    server's algorithm-specific authentication data and compares its public
    encryption key with the locally persisted one. *)
