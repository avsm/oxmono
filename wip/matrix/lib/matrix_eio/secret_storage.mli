(** secret_storage — the secure secret storage cryptography (SSSS), raising
    instead of returning a result.

    A single key, derived from a passphrase or written down as a recovery key,
    encrypts each secret, and {!Secrets} moves the results through account data.
    The pure functions are re-exported unchanged from
    {!Matrix_client.Secret_storage}; the ones that can reject their input raise
    [Eio.Io] carrying an {!Error.type-err} where that module returns an error.

    @see <https://spec.matrix.org/v1.11/client-server-api/#secrets> Secrets *)

(** {1 Algorithm and secret names} *)

val algorithm : string
(** [algorithm] is ["m.secret_storage.v1.aes-hmac-sha2"], the only secret
    storage algorithm implemented. *)

val pbkdf2_algorithm : string
(** [pbkdf2_algorithm] is ["m.pbkdf2"], the only passphrase KDF the
    specification defines. *)

val secret_cross_signing_master : string
(** [secret_cross_signing_master] is ["m.cross_signing.master"], the private
    half of the user's master cross-signing key. *)

val secret_cross_signing_self_signing : string
(** [secret_cross_signing_self_signing] is ["m.cross_signing.self_signing"], the
    private half of the key that signs the user's own devices. *)

val secret_cross_signing_user_signing : string
(** [secret_cross_signing_user_signing] is ["m.cross_signing.user_signing"], the
    private half of the key that signs other users' master keys. *)

val secret_megolm_backup_v1 : string
(** [secret_megolm_backup_v1] is ["m.megolm_backup.v1"]. The secret stored under
    it is the unpadded Base64 text {!Backup.Decryption_key.of_base64} takes. *)

(** {1 The secret storage key} *)

type key = Matrix_client.Secret_storage.key
(** The type for secret storage keys. It is {!Matrix_client.Secret_storage.key},
    32 bytes that never leave the client. *)

val generate_key : random:Matrix_client.Random.t -> key
(** [generate_key ~random] is a fresh key. Show it to the user with
    {!Recovery_key.encode}, since it cannot be recovered once lost. *)

val key_of_bytes : string -> key
(** [key_of_bytes b] is the 32 bytes [b] as a key. A [b] of any other length
    raises [Eio.Io]. *)

val to_bytes : key -> string
(** [to_bytes k] is [k]'s raw bytes. They are secret material, and the one thing
    to do with them is put them in a key store of the caller's own. *)

module Recovery_key : sig
  (** The secret storage key in the form a user writes down. *)

  val encode : key -> string
  (** [encode k] is [k] in the specification's cryptographic key representation,
      base58 in groups of four. *)

  val decode : string -> key
  (** [decode s] is the key {!encode} was given. Whitespace in [s] is ignored. A
      malformed [s], and one that fails its parity check, raise [Eio.Io] rather
      than yielding a wrong key. *)
end

(** {1 Key descriptions}

    A key description says how a key can be re-derived and how a client tells
    the right key from a wrong one. It holds no secret material and lives in
    account data under [m.secret_storage.key.<key id>]. *)

module Passphrase_info : sig
  (** How to re-derive a key from a passphrase, per [m.pbkdf2]. *)

  type t = Matrix_client.Secret_storage.Passphrase_info.t = {
    algorithm : string;
    salt : string;
    iterations : int;
    bits : int;
  }
  (** The type for passphrase descriptions. It is
      {!Matrix_client.Secret_storage.Passphrase_info.t}, which documents the
      fields. *)

  val default_bits : int
  (** [default_bits] is [256], the length assumed when a [passphrase] object
      omits [bits]. *)

  val v :
    random:Matrix_client.Random.t -> ?iterations:int -> ?bits:int -> unit -> t
  (** [v ~random ()] is a fresh [m.pbkdf2] description with a 32-byte random
      salt. [iterations] defaults to 500 000. [bits] defaults to
      {!default_bits}. *)

  val jsont : t Jsont.t
  (** [jsont] is the JSON codec for {!t}. *)
end

module Key_description : sig
  (** A key description, as stored in [m.secret_storage.key.<key id>]. *)

  type t = Matrix_client.Secret_storage.Key_description.t = {
    name : string option;
    algorithm : string;
    passphrase : Passphrase_info.t option;
    iv : string option;
    mac : string option;
  }
  (** The type for key descriptions. It is
      {!Matrix_client.Secret_storage.Key_description.t}, which documents the
      fields. *)

  val v :
    random:Matrix_client.Random.t ->
    ?name:string ->
    ?passphrase:Passphrase_info.t ->
    key ->
    t
  (** [v ~random key] describes [key], with a check value over a fresh random
      IV. [name] defaults to absent. [passphrase] is the value given to
      {!Secret_storage.key_of_passphrase} when the key came from one, so that
      another device can re-derive it, and defaults to absent. *)

  val jsont : t Jsont.t
  (** [jsont] is the JSON codec for {!t}. *)
end

val key_of_passphrase : passphrase:string -> Passphrase_info.t -> key
(** [key_of_passphrase ~passphrase info] derives the key with PBKDF2-HMAC-SHA512
    over [passphrase], using [info]'s salt, iteration count and length. An
    [info] naming another KDF, or a length that is not 256 bits, raises
    [Eio.Io]. *)

(** What a key description says about a key. It is
    {!Matrix_client.Secret_storage.type-key_check}. *)
type key_check = Matrix_client.Secret_storage.key_check =
  | Correct
  | Incorrect
  | Unchecked

val check_key : key -> Key_description.t -> key_check
(** [check_key key description] is what [description] says about [key]. A check
    value that is present but malformed is {!Incorrect}. *)

(** {1 Encrypting a secret} *)

module Encrypted : sig
  (** An encrypted secret, as stored under one key id. *)

  type t = Matrix_client.Secret_storage.Encrypted.t = {
    iv : string;
    ciphertext : string;
    mac : string;
  }
  (** The type for encrypted secrets. It is
      {!Matrix_client.Secret_storage.Encrypted.t}, which documents the fields.
  *)

  val jsont : t Jsont.t
  (** [jsont] is the JSON codec for {!t}. *)
end

val encrypt :
  random:Matrix_client.Random.t -> key -> name:string -> string -> Encrypted.t
(** [encrypt ~random key ~name secret] encrypts [secret] for storage under the
    secret named [name], which is both the account-data event type and the HKDF
    info, so a key derived for one secret cannot decrypt another. *)

val decrypt : key -> name:string -> Encrypted.t -> string
(** [decrypt key ~name data] is the secret [data] holds. The MAC is checked
    before anything is decrypted, and failing it raises [Eio.Io], which is what
    a wrong [key] looks like. *)
