(** backup — encrypting and decrypting backed-up room keys, raising instead of
    returning a result.

    Backup entries are encrypted for a Curve25519 key whose private half never
    reaches the server, and {!Recovery_key.encode} renders that half as the
    recovery key a user writes down. {!Room_keys} carries the results to and
    from the server. The pure functions are re-exported unchanged from
    {!Matrix_client.Backup}; the ones that can reject their input raise [Eio.Io]
    carrying an {!Error.type-err} where that module returns an error.

    @see <https://spec.matrix.org/v1.11/client-server-api/#server-side-key-backups>
      Server-side key backups *)

val backup_algorithm : string
(** [backup_algorithm] is ["m.megolm_backup.v1.curve25519-aes-sha2"], the only
    algorithm implemented. *)

(** {1 Backup keys} *)

type encryption_key = Matrix_client.Backup.encryption_key
(** The public half of a backup key. A device holding only this can write to the
    backup but not read it. *)

module Decryption_key : sig
  (** The private half of a backup key. A device holding it can read the backup.
  *)

  type t = Matrix_client.Backup.Decryption_key.t
  (** The type for backup decryption keys. *)

  val generate : random:Matrix_client.Random.t -> t
  (** [generate ~random] is a fresh key drawn from [random]. *)

  val of_bytes : string -> t
  (** [of_bytes b] is the key the 32 bytes [b] hold. A [b] of any other length
      raises [Eio.Io]. *)

  val of_base64 : string -> t
  (** [of_base64 s] is the key the padded or unpadded base64 [s] encodes. This
      is the representation secret storage keeps under
      {!Secret_storage.secret_megolm_backup_v1}. An [s] that is not a key raises
      [Eio.Io]. *)

  val to_base64 : t -> string
  (** [to_base64 t] is [t] in unpadded base64. It is secret material, and a
      caller that writes it anywhere but its own key store has leaked the
      backup. *)

  val public : t -> encryption_key
  (** [public t] is the half of [t] that encrypts. *)
end

module Recovery_key : sig
  (** The backup key in the form a user writes down. *)

  val encode : Decryption_key.t -> string
  (** [encode key] is [key] in the specification's cryptographic key
      representation, base58 in groups of four. *)

  val decode : string -> Decryption_key.t
  (** [decode s] is the key {!encode} was given. Whitespace in [s] is ignored,
      so the grouping need not be reproduced. A malformed [s], and one that
      fails its parity check, raise [Eio.Io] rather than yielding a wrong key.
  *)
end

(** {1 Backup entries} *)

type encrypted_session_data = Matrix_client.Backup.encrypted_session_data = {
  ephemeral : string;
  ciphertext : string;
  mac : string;
}
(** The [session_data] of a backed-up key. All three fields are unpadded base64.
*)

type key_backup_data = Matrix_client.Backup.key_backup_data = {
  first_message_index : int;
  forwarded_count : int;
  is_verified : bool;
  session_data : encrypted_session_data;
}
(** The type for one backed-up Megolm session. It is
    {!Matrix_client.Backup.key_backup_data}, which documents the fields. *)

type sessions = Matrix_client.Backup.sessions
(** The type for the backed-up sessions of one room, keyed by Megolm session
    identifier as the backup files them. *)

type rooms = Matrix_client.Backup.rooms
(** The type for backed-up sessions keyed by room identifier as the backup files
    them. *)

type backed_up_session_data = Matrix_client.Backup.backed_up_session_data = {
  algorithm : string;
  forwarding_curve25519_key_chain : string list;
  sender_key : string;
  sender_claimed_keys : (string * string) list;
  session_key : string;
  shared_history : bool;
}
(** The plaintext under {!encrypted_session_data}. It is
    {!Matrix_client.Backup.backed_up_session_data}, which documents the fields.
*)

type recovered_room_key = Matrix_client.Backup.recovered_room_key = {
  room_id : Matrix_proto.Id.Room_id.t;
  session_id : Matrix_proto.Id.Session_id.t;
  session_key : string;
  sender_key : string;
  algorithm : string;
  forwarded : bool;
  sender_claimed_keys : (string * string) list;
  forwarding_curve25519_key_chain : string list;
  shared_history : bool;
}
(** A room key recovered from the backup. It is
    {!Matrix_client.Backup.recovered_room_key}, which documents the fields. *)

val encrypt_session_data :
  random:Matrix_client.Random.t ->
  encryption_key ->
  string ->
  encrypted_session_data
(** [encrypt_session_data ~random key plaintext] encrypts a serialised
    {!backed_up_session_data} for the backup whose public key is [key]. An
    unusable [key] raises [Eio.Io]. *)

val encrypt_room_key :
  random:Matrix_client.Random.t ->
  encryption_key ->
  session_key:string ->
  sender_key:string ->
  encrypted_session_data
(** [encrypt_room_key ~random key ~session_key ~sender_key] wraps [session_key]
    in a minimal {!backed_up_session_data} and encrypts it. [session_key] must
    be an exact unsigned version-1 Megolm export and [sender_key] a Curve25519
    public key. Build a {!backed_up_session_data} and call
    {!encrypt_session_data} to carry a forwarding chain or the sender's claimed
    keys. An unusable key or malformed session key raises [Eio.Io]. *)

val decrypt_room_key : Decryption_key.t -> encrypted_session_data -> string
(** [decrypt_room_key key session_data] is the serialised
    {!backed_up_session_data} [session_data] holds. A MAC that does not verify,
    and malformed padding, raise [Eio.Io]. Parse the result with
    {!parse_recovered_key}. *)

val parse_recovered_key :
  room_id:Matrix_proto.Id.Room_id.t ->
  session_id:Matrix_proto.Id.Session_id.t ->
  string ->
  recovered_room_key
(** [parse_recovered_key ~room_id ~session_id plaintext] parses what
    {!decrypt_room_key} returned, pairing it with the identifiers it was filed
    under in the backup. A [plaintext] that is not a {!backed_up_session_data}
    raises [Eio.Io]. *)

(** {1 Codecs} *)

val encrypted_session_data_jsont : encrypted_session_data Jsont.t
(** [encrypted_session_data_jsont] is the JSON codec for
    {!encrypted_session_data}. *)

val key_backup_data_jsont : key_backup_data Jsont.t
(** [key_backup_data_jsont] is the JSON codec for {!key_backup_data}. *)

val backed_up_session_data_jsont : backed_up_session_data Jsont.t
(** [backed_up_session_data_jsont] is the JSON codec for
    {!backed_up_session_data}. *)

(** {1 Backup versions} *)

type megolm_v1_auth_data = Matrix_client.Backup.megolm_v1_auth_data = {
  public_key : encryption_key;
  signatures : Matrix_client.Keys.signatures;
}
(** The [auth_data] of an [m.megolm_backup.v1.curve25519-aes-sha2] backup. *)

val megolm_v1_auth_data_jsont : megolm_v1_auth_data Jsont.t
(** [megolm_v1_auth_data_jsont] is the JSON codec for {!megolm_v1_auth_data}. *)

val auth_data_to_json : megolm_v1_auth_data -> Jsont.json
(** [auth_data_to_json d] is [d] in the shape {!Room_keys.create_version} and
    {!Room_keys.update_version} take as [auth_data]. *)

val sign_auth_data :
  signing_key:Matrix_client.Crypto_key.Ed25519.Private.t ->
  user_id:Matrix_proto.Id.User_id.t ->
  key_id:Matrix_client.Crypto_key.Key_id.t ->
  megolm_v1_auth_data ->
  megolm_v1_auth_data
(** [sign_auth_data ~signing_key ~user_id ~key_id auth_data] adds an Ed25519
    signature over the auth data under [user_id] and [key_id], which names the
    device for a device signature and the master key itself for a cross-signing
    one. Any signature already under that identifier is replaced. *)

(** How a signature over a backup's auth data stands. It is
    {!Matrix_client.Backup.type-signature_state}. *)
type signature_state = Matrix_client.Backup.signature_state =
  | Missing
  | Invalid
  | Valid_but_not_trusted
  | Valid_and_trusted

val verify_auth_data_signature :
  verify_key:Matrix_client.Crypto_key.Ed25519.Public.t ->
  megolm_v1_auth_data ->
  user_id:Matrix_proto.Id.User_id.t ->
  key_id:Matrix_client.Crypto_key.Key_id.t ->
  signature_state
(** [verify_auth_data_signature ~verify_key auth_data ~user_id ~key_id] checks
    one signature. It never answers {!Valid_and_trusted}, since whether
    [verify_key] is itself trusted is the caller's judgement. *)
