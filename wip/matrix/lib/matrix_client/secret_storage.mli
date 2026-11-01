(** secret_storage — the secure secret storage cryptography (SSSS, "4S").

    SSSS keeps the secrets a user's devices share, the cross-signing private
    keys and the key-backup decryption key, encrypted under a
    {e secret storage key} the homeserver never learns. The key is either
    random, shown to the user as a recovery key, or derived from a passphrase.
    The cryptography lives here and {!Secrets} moves the results through account
    data. Every function needing unpredictable bytes takes an explicit
    {!Random.t}.

    @see <https://spec.matrix.org/v1.11/client-server-api/#secrets> Secrets *)

type error = [ `Msg of string ]
(** The type for a failure to decode or decrypt. The message names what was
    rejected. *)

(** {1 Algorithm names} *)

val algorithm : string
(** [algorithm] is ["m.secret_storage.v1.aes-hmac-sha2"], the only secret
    storage algorithm the spec defines and the only one implemented here. It is
    HKDF-SHA256 per secret name, AES-CTR-256, then HMAC-SHA256 over the
    ciphertext. *)

val pbkdf2_algorithm : string
(** [pbkdf2_algorithm] is ["m.pbkdf2"], the only passphrase KDF the spec
    defines. *)

(** {1 Well-known secret names}

    Each is the account-data event type the secret is stored under, and the HKDF
    info its per-secret keys are derived with. *)

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

type key
(** The type for secret storage keys. A key is 32 bytes that never leave the
    client. *)

val generate_key : random:Random.t -> key
(** [generate_key ~random] is a fresh key. Show it to the user with
    {!Recovery_key.encode}, since it cannot be recovered once lost. *)

val key_of_bytes : string -> (key, [> error ]) result
(** [key_of_bytes b] is [b] as a key. [b] must be exactly 32 bytes. *)

val to_bytes : key -> string
(** [to_bytes k] is [k]'s raw bytes. They are secret material, and the one thing
    to do with them is put them in a key store of the caller's own. *)

module Recovery_key : sig
  (** The secret storage key in the form a user writes down. *)

  val encode : key -> string
  (** [encode k] is [k] in the cryptographic key representation, base58 in
      groups of four.

      @see <https://spec.matrix.org/v1.11/appendices/#cryptographic-key-representation>
        Cryptographic key representation *)

  val decode : string -> (key, [> error ]) result
  (** [decode s] is the key {!encode} was given. Whitespace in [s] is ignored,
      and a mistyped character fails the parity check rather than yielding a
      wrong key. *)
end

(** {1 Key descriptions}

    A key description says how a key can be re-derived and how a client can tell
    the right key from a wrong one. It holds no secret material and lives in
    account data under [m.secret_storage.key.<key id>]. *)

module Passphrase_info : sig
  (** How to re-derive a key from a passphrase, per [m.pbkdf2]. *)

  type t = {
    algorithm : string;  (** {!Secret_storage.pbkdf2_algorithm}. *)
    salt : string;  (** The PBKDF2 salt, used as raw bytes. *)
    iterations : int;  (** The PBKDF2 iteration count. *)
    bits : int;  (** Key length in bits. *)
  }
  (** The type for passphrase descriptions. *)

  val default_bits : int
  (** [default_bits] is [256], the length assumed when a [passphrase] object
      omits [bits]. *)

  val v : random:Random.t -> ?iterations:int -> ?bits:int -> unit -> t
  (** [v ~random ()] is a fresh [m.pbkdf2] description with a 32-byte random
      salt. [iterations] defaults to 500 000. [bits] defaults to
      {!default_bits}. *)

  val jsont : t Jsont.t
  (** [jsont] is the JSON codec for {!t}. *)
end

module Key_description : sig
  (** A key description, as stored in [m.secret_storage.key.<key id>]. *)

  type t = {
    name : string option;  (** A human-readable name for the key. *)
    algorithm : string;  (** {!Secret_storage.algorithm}. *)
    passphrase : Passphrase_info.t option;
        (** Present when the key is derived from a passphrase. *)
    iv : string option;  (** base64 of the 16-byte check IV. *)
    mac : string option;  (** base64 of the check MAC. *)
  }
  (** The type for key descriptions. [iv] and [mac] are the check value
      {!Secret_storage.check_key} reproduces. A description carrying neither
      must be assumed valid. *)

  val v :
    random:Random.t -> ?name:string -> ?passphrase:Passphrase_info.t -> key -> t
  (** [v ~random key] describes [key], with a check value over a fresh random
      IV. [name] defaults to absent. Pass [passphrase], the same value given to
      {!Secret_storage.key_of_passphrase}, when the key came from one, so that
      another device can re-derive it; it defaults to absent. *)

  val jsont : t Jsont.t
  (** [jsont] is the JSON codec for {!t}. *)
end

val key_of_passphrase :
  passphrase:string -> Passphrase_info.t -> (key, [> error ]) result
(** [key_of_passphrase ~passphrase info] derives the key with PBKDF2-HMAC-SHA512
    over [passphrase], using [info]'s salt, iteration count and length. It fails
    if [info] names a KDF other than {!pbkdf2_algorithm}, or asks for a length
    that is not 256 bits. *)

(** What a key description says about a key. *)
type key_check =
  | Correct  (** The key reproduces the description's check value. *)
  | Incorrect  (** The check value is present and the key does not match it. *)
  | Unchecked
      (** The description carries no check value, and the spec says such a key
          must be assumed valid. *)

val check_key : key -> Key_description.t -> key_check
(** [check_key key description] is what [description] says about [key]. A check
    value that is present but malformed is {!Incorrect}. *)

(** {1 Encrypting a secret} *)

module Encrypted : sig
  (** An encrypted secret, as stored under one key id. *)

  type t = {
    iv : string;  (** The 16-byte AES-CTR initialisation vector. *)
    ciphertext : string;  (** The AES-CTR-256 ciphertext. *)
    mac : string;  (** HMAC-SHA256 of the ciphertext. *)
  }
  (** The type for encrypted secrets. All three fields are base64. The encoder
      emits the unpadded form the spec asks for and the decoder accepts the
      padded form too. *)

  val jsont : t Jsont.t
  (** [jsont] is the JSON codec for {!t}. *)
end

val encrypt : random:Random.t -> key -> name:string -> string -> Encrypted.t
(** [encrypt ~random key ~name secret] encrypts [secret] for storage under the
    secret named [name], which serves as both the account-data event type and
    the HKDF info, so a key derived for one secret cannot decrypt another. The
    IV is drawn from [random] with bit 63 cleared, as the spec requires, because
    AES-CTR implementations disagree about the 64-bit counter rolling over. *)

val decrypt : key -> name:string -> Encrypted.t -> (string, [> error ]) result
(** [decrypt key ~name data] is the secret [data] holds. The MAC is checked
    before anything is decrypted, and failing it is what a wrong [key] looks
    like. *)
