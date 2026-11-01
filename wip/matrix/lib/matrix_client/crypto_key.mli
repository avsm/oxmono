(** crypto_key — the key material end-to-end encryption is built from.

    Matrix uses Ed25519 for signatures and Curve25519 (X25519) for key
    agreement, and writes every public key, secret key and signature as unpadded
    base64. Each type here is abstract, so a value that exists is a key of that
    kind and of the right length, and a caller cannot pass a signature where a
    public key belongs.

    Secret material has no [pp], no [equal] and no [jsont]. It leaves this
    module only through [to_bytes], which a caller uses to write a pickle and
    should not use for anything else.

    OCaml strings and the key values held by [mirage-crypto] are managed by the
    garbage collector and may be copied. Consequently this module cannot offer
    reliable zeroisation of long-lived keys or of every intermediate copy. It
    bounds avoidable retention (for example, one-shot protocols discard their
    key reference as soon as it is consumed), but callers must not treat value
    reachability or process-local deletion as a memory-wipe guarantee. A
    deployment whose threat model includes memory inspection should isolate the
    crypto process and apply operating-system controls for core dumps, swap and
    debugging access.

    Every generator takes an explicit {!Random.t}, so nothing here reads a
    global generator.

    @see <https://spec.matrix.org/v1.11/appendices/#signing-details>
      Signing details *)

type error = [ `Msg of string ]
(** The type for a decoding failure. The message names what was wrong with the
    input. *)

(** {1 Signatures} *)

module Signature : sig
  @@ portable
  (** An Ed25519 signature, 64 bytes. *)

  type t
  (** The type for signatures. *)

  val of_bytes : string -> (t, [> error ]) result
  (** [of_bytes b] is the signature the 64 bytes [b] hold. *)

  val to_bytes : t -> string
  (** [to_bytes t] is the 64 bytes of [t]. *)

  val of_base64 : string -> (t, [> error ]) result
  (** [of_base64 s] is the signature [s] encodes. [s] may be padded or unpadded.
  *)

  val to_base64 : t -> string
  (** [to_base64 t] is [t] in the unpadded base64 Matrix puts on the wire. *)

  val equal : t -> t -> bool
  (** [equal a b] is [true] when [a] and [b] are the same signature. *)

  val compare : t -> t -> int
  (** [compare a b] orders signatures by their bytes. *)

  val pp : Format.formatter -> t -> unit
  (** [pp ppf t] prints [to_base64 t] on [ppf]. *)

  val jsont : t Jsont.t
  (** [jsont] is the JSON codec for {!t}, a base64 string. *)
end

(** {1 Signing keys} *)

module Ed25519 : sig
  (** Ed25519, the signature scheme every Matrix signature uses. *)

  module Public : sig
    @@ portable
    (** An Ed25519 public key, 32 bytes. *)

    type t
    (** The type for Ed25519 public keys. *)

    val of_bytes : string -> (t, [> error ]) result
    (** [of_bytes b] is the public key the 32 bytes [b] hold. *)

    val to_bytes : t -> string
    (** [to_bytes t] is the 32 bytes of [t]. *)

    val of_base64 : string -> (t, [> error ]) result
    (** [of_base64 s] is the public key [s] encodes. [s] may be padded or
        unpadded. *)

    val to_base64 : t -> string
    (** [to_base64 t] is [t] in the unpadded base64 Matrix puts on the wire. *)

    val verify : t -> signature:Signature.t -> data:string -> bool
    (** [verify t ~signature ~data] is [true] when [signature] is [t]'s
        signature over the bytes of [data]. *)

    val equal : t -> t -> bool
    (** [equal a b] is [true] when [a] and [b] are the same key. *)

    val compare : t -> t -> int
    (** [compare a b] orders keys by their bytes. *)

    val pp : Format.formatter -> t -> unit
    (** [pp ppf t] prints [to_base64 t] on [ppf]. *)

    val jsont : t Jsont.t
    (** [jsont] is the JSON codec for {!t}, a base64 string. *)
  end

  module Private : sig
    @@ portable
    (** An Ed25519 private key. Fresh keys retain their 32-byte seed; keys
        imported from a legacy libolm pickle retain the 64-byte expanded
        representation because that representation cannot be reversed to a seed.
        Both forms are secret material. *)

    type t
    (** The type for Ed25519 private keys. *)

    val of_bytes : string -> (t, [> error ]) result
    (** [of_bytes b] is the private key the 32 bytes [b] hold. [b] is secret
        material. *)

    val of_expanded_bytes : string -> (t, [> error ]) result
    (** [of_expanded_bytes b] restores a 64-byte libolm expanded key. [b] is
        secret material. *)

    val of_stored_bytes : string -> (t, [> error ]) result
    (** [of_stored_bytes b] accepts either the 32-byte seed or the 64-byte
        expanded representation used by persistence formats. *)

    val to_bytes : t -> string
    (** [to_bytes t] is the private key bytes. Fresh keys use the 32-byte seed;
        keys restored from a legacy pickle use the 64-byte expanded form. *)

    val to_expanded_bytes : t -> string
    (** [to_expanded_bytes t] is libolm's 64-byte expanded private key
        representation. *)

    val public : t -> Public.t
    (** [public t] is the public key of [t]. *)

    val sign : t -> string -> Signature.t
    (** [sign t data] is [t]'s signature over the bytes of [data]. A Matrix
        signature covers canonical JSON with [signatures] and [unsigned]
        removed, which {!Matrix_proto.Signed_json.canonical_json} produces. *)
  end

  val generate : random:Random.t -> unit -> Private.t * Public.t
  (** [generate ~random ()] is a fresh signing key pair drawn from [random]. *)
end

(** {1 Key-agreement keys} *)

module Curve25519 : sig
  (** Curve25519, the key agreement the Olm and Megolm ratchets are built on. *)

  module Public : sig
    @@ portable
    (** A Curve25519 public key, 32 bytes. *)

    type t
    (** The type for Curve25519 public keys. *)

    val of_bytes : string -> (t, [> error ]) result
    (** [of_bytes b] is the public key the 32 bytes [b] hold. *)

    val to_bytes : t -> string
    (** [to_bytes t] is the 32 bytes of [t]. *)

    val of_base64 : string -> (t, [> error ]) result
    (** [of_base64 s] is the public key [s] encodes. [s] may be padded or
        unpadded. *)

    val to_base64 : t -> string
    (** [to_base64 t] is [t] in the unpadded base64 Matrix puts on the wire. *)

    val equal : t -> t -> bool
    (** [equal a b] is [true] when [a] and [b] are the same key. *)

    val compare : t -> t -> int
    (** [compare a b] orders keys by their bytes. *)

    val pp : Format.formatter -> t -> unit
    (** [pp ppf t] prints [to_base64 t] on [ppf]. *)

    val jsont : t Jsont.t
    (** [jsont] is the JSON codec for {!t}, a base64 string. *)
  end

  module Secret : sig
    @@ portable
    (** A Curve25519 secret key, 32 bytes of secret material. *)

    type t
    (** The type for Curve25519 secret keys. *)

    val of_bytes : string -> (t, [> error ]) result
    (** [of_bytes b] is the secret key the 32 bytes [b] hold. [b] is secret
        material. *)

    val to_bytes : t -> string
    (** [to_bytes t] is the 32 bytes of [t]. They are secret material, and a
        caller that writes them anywhere but a pickle has leaked the session. *)

    val public : t -> Public.t
    (** [public t] is the public key of [t]. *)
  end

  val generate : random:Random.t -> unit -> Secret.t * Public.t
  (** [generate ~random ()] is a fresh key-agreement key pair drawn from
      [random]. *)

  val key_exchange :
    secret:Secret.t -> public:Public.t -> (string, [> error ]) result
  (** [key_exchange ~secret ~public] is the 32-byte shared secret of a
      Diffie-Hellman between [secret] and [public].

      A public key that drives the shared secret to zero is refused, since such
      a key contributes nothing and lets a peer fix the result. *)
end

(** {1 Key identifiers} *)

module Key_id : sig
  @@ portable
  (** The [algorithm:identifier] name a key is published under.

      A device key is named after the device, as in [ed25519:JLAFKJWSCS]. A
      one-time key is named after the identifier the account gave it, as in
      [signed_curve25519:AAAAAQ]. A cross-signing key is named after the key
      itself, as in [ed25519:] followed by its unpadded base64.

      @see <https://spec.matrix.org/v1.11/client-server-api/#device-keys>
        Device keys *)

  type t
  (** The type for key identifiers. *)

  val v : algorithm:string -> id:string -> t
  (** [v ~algorithm ~id] is the identifier naming [id] under [algorithm].

      Raises [Invalid_argument] if [algorithm] is empty or contains a colon, or
      if [id] is empty. *)

  val of_device : algorithm:string -> Matrix_proto.Id.Device_id.t -> t
  (** [of_device ~algorithm device] names [device]'s key under [algorithm]. *)

  val of_string : string -> (t, [> error ]) result
  (** [of_string s] is the identifier [s] names. [s] must hold a colon, with a
      non-empty part on each side. *)

  val to_string : t -> string
  (** [to_string t] is [algorithm t], a colon, then [id t]. *)

  val algorithm : t -> string
  (** [algorithm t] is the part of [t] before the colon, such as [ed25519]. *)

  val id : t -> string
  (** [id t] is the part of [t] after the colon. *)

  val equal : t -> t -> bool
  (** [equal a b] is [true] when [a] and [b] name the same key. *)

  val compare : t -> t -> int
  (** [compare a b] orders identifiers by algorithm, then by identifier. *)

  val pp : Format.formatter -> t -> unit
  (** [pp ppf t] prints [to_string t] on [ppf]. *)

  val jsont : t Jsont.t
  (** [jsont] is the JSON codec for {!t}, an [algorithm:identifier] string. *)
end
