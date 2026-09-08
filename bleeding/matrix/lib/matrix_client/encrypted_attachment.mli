(** Matrix encrypted attachment bytes.

    This is the byte-level primitive used by encrypted media. Encryption is
    AES-256 in CTR mode with a 16-byte, big-endian counter block: the first
    eight bytes of the IV are random and the remaining eight bytes start at
    zero. The SHA-256 digest is over the ciphertext, not the plaintext.

    The low-level incremental decryptor emits plaintext before the digest is
    checked, just like the Matrix Rust SDK. Consequently, chunks returned by
    {!Decryptor.feed} are not authenticated until {!Decryptor.finish} succeeds.
    The high-level {!decrypt} and {!decrypt_chunks} helpers release plaintext
    only after the complete input has been checked. *)

type error =
  | Malformed_metadata of string
  | Unsupported_version of string
  | Unsupported_algorithm of string
  | Hash_mismatch
  | Invalid_state of string
      (** Errors raised by metadata validation or streaming finalisation. *)

val pp_error : Format.formatter -> error -> unit @@ portable
(** [pp_error] prints an attachment error. *)

type metadata
(** The wire metadata for an encrypted attachment, excluding its [url].

    [key], [iv], and [hash] are their unpadded Matrix wire encodings. [key] is
    base64url (the JWK [k] member); [iv] and [hash] use Matrix's standard base64
    alphabet. *)

module Metadata : sig
  @@ portable
  type t = metadata

  val make :
    ?version:string ->
    ?algorithm:string ->
    ?key_ops:string list ->
    ?ext:bool ->
    key:string ->
    iv:string ->
    hash:string ->
    unit ->
    (t, error) result
  (** [make] validates and constructs wire metadata. *)

  val key : t -> string
  val iv : t -> string
  val hash : t -> string
  val version : t -> string
  val algorithm : t -> string
  val key_ops : t -> string list
  val ext : t -> bool

  val jsont : t Jsont.t
  (** The exact encrypted-file metadata JSON shape, without [url]. *)

  val of_json : Jsont.json -> (t, error) result
  val to_json : t -> Jsont.json
  val of_json_string : string -> (t, error) result
  val to_json_string : t -> string

  val of_event_file :
    Matrix_proto.Event.Media_message_content.encrypted_file -> (t, error) result
  (** Convert the existing event codec representation into validated metadata.
  *)

  val to_event_file :
    url:string -> t -> Matrix_proto.Event.Media_message_content.encrypted_file
  (** Convert metadata to the event codec representation. *)
end

module Encryptor : sig
  type t

  val create : random:Random.t -> unit -> t
  (** [create] makes a fresh encryptor with a fresh key and Matrix IV. *)

  val feed : t -> string -> string
  (** Encrypt one chunk. Arbitrary chunk boundaries are supported. *)

  val finish : t -> metadata
  (** Finish hashing and return the metadata. Calling [feed] after [finish]
      raises [Invalid_argument]. *)
end

module Decryptor : sig
  type t

  val create : metadata -> (t, error) result
  (** Validate metadata and create an incremental decryptor. *)

  val feed : t -> string -> string
  (** Decrypt one ciphertext chunk. The digest is checked by [finish]. *)

  val finish : t -> (unit, error) result
  (** Verify the ciphertext digest. No successful decryption is complete until
      this function returns [Ok ()]. *)
end

type encrypted = { ciphertext : string; metadata : metadata }

val encrypt : random:Random.t -> string -> encrypted
(** Encrypt one string. Incremental callers should use {!Encryptor}. *)

val decrypt : metadata -> string -> (string, error) result
(** Decrypt and authenticate a complete ciphertext. *)

val decrypt_chunks :
  metadata ->
  chunks:(unit -> string option) ->
  on_chunk:(string -> unit) ->
  (unit, error) result
(** Decrypt and authenticate chunks supplied by [chunks]. Plaintext is buffered
    until the digest succeeds, then delivered to [on_chunk] using the original
    chunk boundaries. On error [on_chunk] is never called. *)

val decrypt_verified : metadata -> string -> (string, error) result
(** Alias of {!decrypt}, useful at call sites where authentication is a security
    boundary. *)

val decrypt_spooled :
  metadata ->
  spool:_ Eio.File.rw ->
  output:_ Eio.Flow.sink ->
  (unit, error) result
(** [decrypt_spooled metadata ~spool ~output] authenticates the complete
    ciphertext currently in [spool], rewinds it, and decrypts it to [output].
    The ciphertext is authenticated once before any plaintext is emitted and the
    decrypting pass is authenticated again. The caller retains ownership of both
    flows; [spool] must be seekable and is left at offset zero on every ordinary
    result. I/O and cancellation exceptions are allowed to propagate. The spool
    and output sink must not be mutated concurrently. *)
