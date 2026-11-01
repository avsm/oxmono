(* Copyright (c) 2025 Anil Madhavapeddy. SPDX-License-Identifier: ISC *)

(** Bounded CWT with COSE_Sign1 or COSE_Mac0 protection. Key trust and
    application claims are caller policy. No function discovers keys. *)

type error : immutable_data =
  | Invalid_cbor of string | Invalid_cose of string | Invalid_claims of string
  | Unsupported_algorithm of string | Algorithm_not_allowed of string
  | Signature_mismatch | Token_expired | Token_not_yet_valid
  | Invalid_issuer | Invalid_audience | Key_type_mismatch of string
val pp_error : Format.formatter -> error -> unit
val error_to_string : error -> string

module Algorithm : sig
  type t : immediate =
    | ES256 | ES384 | ES512 | EdDSA
    | HMAC_256_64 | HMAC_256 | HMAC_384 | HMAC_512
  val to_cose_int : t -> int @@ portable
  val of_cose_int : int -> (t, error) result @@ portable
  val to_string : t -> string @@ portable
  val all : t list
  (** [all] lists supported algorithms, not a recommended application policy.
      The polymorphic ECDSA/EdDSA identifiers are deprecated by RFC 9864.
      This implementation restricts each ECDSA identifier to its NIST curve. *)
end

module Cose_key : sig
  type kty = Okp | Ec2 | Symmetric
  type t : immutable_data
  val symmetric : string -> t @@ portable
  (** [symmetric bytes] creates an unbound HMAC key. Bind it with [with_alg]
      before use. HMAC requires at least 32, 48 or 64 random key bytes for
      SHA-256, SHA-384 or SHA-512, including the truncated SHA-256 variant. *)
  val ed25519_pub : string -> t
  val ed25519_priv : pub:string -> priv:string -> t
  val p256_pub : x:string -> y:string -> t
  val p256_priv : x:string -> y:string -> d:string -> t
  val p384_pub : x:string -> y:string -> t
  val p384_priv : x:string -> y:string -> d:string -> t
  val p521_pub : x:string -> y:string -> t
  val p521_priv : x:string -> y:string -> d:string -> t
  (** These constructors validate key widths, points and private/public
      agreement and bind the algorithm. Invalid keys raise
      [Invalid_argument]. *)
  val kty : t -> kty @@ portable
  val kid : t -> string option @@ portable
  val alg : t -> Algorithm.t option @@ portable
  val with_kid : string -> t -> t @@ portable
  (** [with_kid bytes key] sets the opaque byte-string key identifier. *)
  val with_alg : Algorithm.t -> t -> t @@ portable
  (** [with_alg alg key] binds the key to one algorithm. Incompatible key types
      raise [Invalid_argument]. Operations enforce this binding. *)
  val of_cbor : string -> (t, error) result
  (** [of_cbor bytes] validates a COSE key, including any algorithm and key_ops
      restrictions. IDs and key material must be byte strings. Unknown
      algorithms, duplicates and malformed recognized fields are errors.
      Compressed EC coordinates and key operations other than signing,
      verification and MAC creation/verification are unsupported. Base IV is
      unsupported. *)
  val to_cbor : t -> string @@ portable
  (** [to_cbor key] encodes the key, including private material if present. *)
end

module Claims : sig
  type t : immutable_data
  type builder : immutable_data
  val iss : t -> string option @@ portable
  val sub : t -> string option @@ portable
  val aud : t -> string list @@ portable
  val exp : t -> Ptime.t option @@ portable
  val nbf : t -> Ptime.t option @@ portable
  val iat : t -> Ptime.t option @@ portable
  val cti : t -> string option @@ portable
  (** [cti claims] is the raw byte-string token ID, when present. *)
  val get_int_key : int -> t -> Cbort.Cbor.t option @@ portable
  val get_string_key : string -> t -> Cbort.Cbor.t option @@ portable
  (** These accessors preserve original CBOR claim values, including fractional
      NumericDates. Registered claim labels are available through
      [get_int_key]. *)
  val empty : builder
  val set_iss : string -> builder -> builder @@ portable
  val set_sub : string -> builder -> builder @@ portable
  val set_aud : string list -> builder -> builder @@ portable
  val set_exp : Ptime.t -> builder -> builder @@ portable
  val set_nbf : Ptime.t -> builder -> builder @@ portable
  val set_iat : Ptime.t -> builder -> builder @@ portable
  val set_cti : string -> builder -> builder @@ portable
  val set_int_key : int -> Cbort.Cbor.t -> builder -> builder @@ portable
  val set_string_key : string -> Cbort.Cbor.t -> builder -> builder @@ portable
  (** Setters replace any existing member with the same label. *)
  val build : builder -> t @@ portable
  (** [build builder] validates the claims. Invalid or excessive values raise
      [Invalid_argument]. Fractional times are retained. *)
  val of_cbor : string -> (t, error) result @@ portable
  (** [of_cbor bytes] decodes exactly one claims map. Registered claim types,
      UTF-8, StringOrURI and NumericDates within Ptime's range are checked.
      Tags on registered claims and duplicate labels are rejected. *)
  val to_cbor : t -> string @@ portable
end

type t : immutable_data
(** Parsed claims and headers remain tied to their original signed bytes.
    They are untrusted until signature and application checks succeed. *)
val parse : ?max_size:int -> string -> (t, error) result @@ portable
(** [parse bytes] decodes a tagged or untagged COSE_Sign1/COSE_Mac0 token,
    optionally enclosed by CWT tag 61. [max_size] defaults to 8192 bytes.
    Each decoded CBOR item is limited to 64 KiB, 4096 items and 32 nested
    containers. Maps use integer or text labels, and claim integer labels must
    fit native integers. Integer values use CBOR's 64-bit argument range.
    Tags are preserved. Definite and indefinite lengths are accepted.
    Trailing bytes, duplicate headers, critical extensions, countersignatures,
    tag/algorithm disagreement and malformed fields are errors. Detached
    payloads, external AAD, nested protection and encryption are unsupported. *)
val claims : t -> Claims.t @@ portable
val algorithm : t -> Algorithm.t option @@ portable
(** [algorithm token] is [Some alg] for every successfully parsed token. *)
val kid : t -> string option @@ portable
val raw : t -> string @@ portable
val encode : t -> string @@ portable
(** [encode token] returns the original COSE serialization. *)
val verify : key:Cose_key.t -> allowed_algs:Algorithm.t list -> t ->
  (unit, error) result @@ portable
(** [verify ~key ~allowed_algs token] authenticates the original payload and
    protected-header bytes using a trusted key and explicit algorithm policy.
    It checks key type, algorithm binding, length and operation restrictions.
    Claims and replay policy are not checked. *)
val validate : now:Ptime.t -> ?iss:string -> ?aud:string ->
  ?leeway:Ptime.Span.t -> t -> (unit, error) result @@ portable
(** [validate ~now token] checks any expiration/not-before time and the
    supplied issuer/audience. Expiration is exclusive. Negative leeway raises
    [Invalid_argument]. Callers must require their necessary claims, token
    lifetime, issuance time, token purpose and replay policy. *)
val verify_and_validate : key:Cose_key.t -> now:Ptime.t ->
  allowed_algs:Algorithm.t list -> ?iss:string -> ?aud:string ->
  ?leeway:Ptime.Span.t -> t -> (unit, error) result @@ portable
val create : algorithm:Algorithm.t -> claims:Claims.t -> key:Cose_key.t ->
  (t, error) result
(** [create ~algorithm ~claims ~key] signs a tagged COSE token with an empty
    external AAD. The algorithm and any key ID are protected headers. ECDSA
    signing needs an initialized Mirage Crypto RNG for masking. *)
val is_expired : now:Ptime.t -> ?leeway:Ptime.Span.t -> t -> bool @@ portable
val time_to_expiry : now:Ptime.t -> t -> Ptime.Span.t option @@ portable
