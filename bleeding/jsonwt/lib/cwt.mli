(*---------------------------------------------------------------------------
  Copyright (c) 2025 Anil Madhavapeddy <anil@recoil.org>. All rights reserved.
  SPDX-License-Identifier: ISC
 ---------------------------------------------------------------------------*)

(** CBOR Web Token (CWT) - RFC 8392

    This module implements CBOR Web Tokens as specified in
    {{:https://datatracker.ietf.org/doc/html/rfc8392}RFC 8392}.

    CWTs are the CBOR-based equivalent of JWTs, designed for constrained
    environments where compact binary representation is important. CWTs use COSE
    ({{:https://datatracker.ietf.org/doc/html/rfc9052}RFC 9052}) for
    cryptographic protection.

    {2 Quick Start}

    {[
      (* Create claims *)
      let claims =
        Cwt.Claims.(
          empty
          |> set_iss "https://example.com"
          |> set_sub "user123"
          |> set_exp
               (Ptime.add_span (Ptime_clock.now ()) (Ptime.Span.of_int_s 3600)
               |> Option.get)
          |> build)

      (* Create a symmetric key *)
      let key =
        Cwt.Cose_key.symmetric
          (Bytes.of_string "my-secret-key-32-bytes-long!!!!!")

      (* Create and encode the CWT *)
      let cwt =
        Cwt.create ~algorithm:Cwt.Algorithm.HMAC_256 ~claims ~key
        |> Result.get_ok

      let encoded = Cwt.encode cwt

      (* Parse and verify *)
      let parsed = Cwt.parse encoded |> Result.get_ok
      let () = Cwt.verify ~key parsed |> Result.get_ok
    ]}

    {2 References}
    - {{:https://datatracker.ietf.org/doc/html/rfc8392}RFC 8392} - CBOR Web
      Token (CWT)
    - {{:https://datatracker.ietf.org/doc/html/rfc9052}RFC 9052} - CBOR Object
      Signing and Encryption (COSE) Structures
    - {{:https://datatracker.ietf.org/doc/html/rfc9053}RFC 9053} - CBOR Object
      Signing and Encryption (COSE) Algorithms
    - {{:https://datatracker.ietf.org/doc/html/rfc8949}RFC 8949} - Concise
      Binary Object Representation (CBOR) *)

(** {1 Error Handling} *)

type error =
  | Invalid_cbor of string  (** CBOR parsing failed *)
  | Invalid_cose of string  (** COSE structure validation failed *)
  | Invalid_claims of string  (** Claims validation failed *)
  | Unsupported_algorithm of string  (** Unknown COSE algorithm identifier *)
  | Algorithm_not_allowed of string
      (** Algorithm rejected by allowed_algs policy *)
  | Signature_mismatch  (** Signature/MAC verification failed *)
  | Token_expired
      (** exp claim validation failed per
          {{:https://datatracker.ietf.org/doc/html/rfc8392#section-3.1.4}RFC
           8392 Section 3.1.4} *)
  | Token_not_yet_valid
      (** nbf claim validation failed per
          {{:https://datatracker.ietf.org/doc/html/rfc8392#section-3.1.5}RFC
           8392 Section 3.1.5} *)
  | Invalid_issuer
      (** iss claim mismatch per
          {{:https://datatracker.ietf.org/doc/html/rfc8392#section-3.1.1}RFC
           8392 Section 3.1.1} *)
  | Invalid_audience
      (** aud claim mismatch per
          {{:https://datatracker.ietf.org/doc/html/rfc8392#section-3.1.3}RFC
           8392 Section 3.1.3} *)
  | Key_type_mismatch of string  (** Key doesn't match algorithm *)

val pp_error : Format.formatter -> error -> unit
(** Pretty-print an error. *)

val error_to_string : error -> string
(** Convert error to human-readable string. *)

(** {1 COSE Algorithms}

    Cryptographic algorithms for COSE as specified in
    {{:https://datatracker.ietf.org/doc/html/rfc9053}RFC 9053}.

    Each algorithm has a registered integer identifier in the IANA COSE
    Algorithms registry. *)

module Algorithm : sig
  type t =
    | ES256  (** ECDSA w/ SHA-256, COSE alg = -7 *)
    | ES384  (** ECDSA w/ SHA-384, COSE alg = -35 *)
    | ES512  (** ECDSA w/ SHA-512, COSE alg = -36 *)
    | EdDSA  (** EdDSA (Ed25519), COSE alg = -8 *)
    | HMAC_256_64  (** HMAC w/ SHA-256 truncated to 64 bits, COSE alg = 4 *)
    | HMAC_256  (** HMAC w/ SHA-256 (256 bits), COSE alg = 5 *)
    | HMAC_384  (** HMAC w/ SHA-384, COSE alg = 6 *)
    | HMAC_512  (** HMAC w/ SHA-512, COSE alg = 7 *)

  val to_cose_int : t -> int
  (** Convert to COSE algorithm identifier (negative for signatures). *)

  val of_cose_int : int -> (t, error) result
  (** Parse from COSE algorithm identifier. *)

  val to_string : t -> string
  (** Human-readable name for the algorithm. *)

  val all : t list
  (** All supported algorithms. *)
end

(** {1 COSE Key}

    Key representation for COSE operations. See
    {{:https://datatracker.ietf.org/doc/html/rfc9052#section-7}RFC 9052 Section
     7} and {{:https://datatracker.ietf.org/doc/html/rfc9053}RFC 9053}. *)

module Cose_key : sig
  (** Key type per COSE Key Type registry. See
      {{:https://www.iana.org/assignments/cose/cose.xhtml#key-type}IANA COSE Key
       Types}. *)
  type kty =
    | Okp  (** Octet Key Pair (kty = 1), used for EdDSA *)
    | Ec2  (** Elliptic Curve with x,y coordinates (kty = 2) *)
    | Symmetric  (** Symmetric key (kty = 4) *)

  type t
  (** A COSE key.

      Supported key types and curves:
      - Symmetric keys for HMAC algorithms
      - P-256 (NIST, crv = 1) for ES256
      - P-384 (NIST, crv = 2) for ES384
      - P-521 (NIST, crv = 3) for ES512
      - Ed25519 (crv = 6) for EdDSA *)

  (** {2 Constructors} *)

  val symmetric : string -> t
  (** [symmetric k] creates a symmetric COSE key from raw bytes. Used for HMAC
      algorithms. The key should be at least as long as the hash output (32
      bytes for HMAC_256, etc.). *)

  val ed25519_pub : string -> t
  (** [ed25519_pub pub] creates an Ed25519 public key from the 32-byte public
      key value. *)

  val ed25519_priv : pub:string -> priv:string -> t
  (** [ed25519_priv ~pub ~priv] creates an Ed25519 private key. [pub] is the
      32-byte public key, [priv] is the 32-byte seed. *)

  val p256_pub : x:string -> y:string -> t
  (** [p256_pub ~x ~y] creates a P-256 public key from the x and y coordinates
      (each 32 bytes). *)

  val p256_priv : x:string -> y:string -> d:string -> t
  (** [p256_priv ~x ~y ~d] creates a P-256 private key. [d] is the 32-byte
      private key value. *)

  val p384_pub : x:string -> y:string -> t
  (** [p384_pub ~x ~y] creates a P-384 public key (coordinates are 48 bytes
      each). *)

  val p384_priv : x:string -> y:string -> d:string -> t
  (** [p384_priv ~x ~y ~d] creates a P-384 private key. *)

  val p521_pub : x:string -> y:string -> t
  (** [p521_pub ~x ~y] creates a P-521 public key (coordinates are 66 bytes
      each). *)

  val p521_priv : x:string -> y:string -> d:string -> t
  (** [p521_priv ~x ~y ~d] creates a P-521 private key. *)

  (** {2 Accessors} *)

  val kty : t -> kty
  (** Get the key type. *)

  val kid : t -> string option
  (** Get the key ID if set (COSE label 2). *)

  val alg : t -> Algorithm.t option
  (** Get the intended algorithm if set (COSE label 3). *)

  val with_kid : string -> t -> t
  (** [with_kid id key] returns key with kid set to [id]. *)

  val with_alg : Algorithm.t -> t -> t
  (** [with_alg alg key] returns key with alg set to [alg]. *)

  (** {2 Serialization} *)

  val of_cbor : string -> (t, error) result
  (** Parse a COSE key from CBOR bytes. *)

  val to_cbor : t -> string
  (** Serialize a COSE key to CBOR bytes. *)
end

(** {1 CWT Claims}

    CWT Claims Set using CBOR integer keys for compactness. See
    {{:https://datatracker.ietf.org/doc/html/rfc8392#section-3}RFC 8392 Section
     3}.

    {2 Claim Key Mapping}

    | Claim | Integer Key | Type | |-------|-------------|------| | iss | 1 |
    text string | | sub | 2 | text string | | aud | 3 | text string | | exp | 4
    | integer (NumericDate) | | nbf | 5 | integer (NumericDate) | | iat | 6 |
    integer (NumericDate) | | cti | 7 | byte string | *)

module Claims : sig
  type t

  (** {2 Registered Claim Names}

      See
      {{:https://datatracker.ietf.org/doc/html/rfc8392#section-3.1}RFC 8392
       Section 3.1}. *)

  val iss : t -> string option
  (** Issuer claim (key 1) per
      {{:https://datatracker.ietf.org/doc/html/rfc8392#section-3.1.1}Section
       3.1.1}. *)

  val sub : t -> string option
  (** Subject claim (key 2) per
      {{:https://datatracker.ietf.org/doc/html/rfc8392#section-3.1.2}Section
       3.1.2}. *)

  val aud : t -> string list
  (** Audience claim (key 3) per
      {{:https://datatracker.ietf.org/doc/html/rfc8392#section-3.1.3}Section
       3.1.3}. Returns empty list if not present. May be single string or array
      in CWT. *)

  val exp : t -> Ptime.t option
  (** Expiration time claim (key 4) per
      {{:https://datatracker.ietf.org/doc/html/rfc8392#section-3.1.4}Section
       3.1.4}. *)

  val nbf : t -> Ptime.t option
  (** Not Before claim (key 5) per
      {{:https://datatracker.ietf.org/doc/html/rfc8392#section-3.1.5}Section
       3.1.5}. *)

  val iat : t -> Ptime.t option
  (** Issued At claim (key 6) per
      {{:https://datatracker.ietf.org/doc/html/rfc8392#section-3.1.6}Section
       3.1.6}. *)

  val cti : t -> string option
  (** CWT ID claim (key 7) per
      {{:https://datatracker.ietf.org/doc/html/rfc8392#section-3.1.7}Section
       3.1.7}. Note: Unlike JWT's jti which is a string, CWT's cti is a byte
      string. *)

  (** {2 Custom Claims}

      CWT supports both integer and text string keys for custom claims. *)

  val get_int_key : int -> t -> Cbort.Cbor.t option
  (** [get_int_key key claims] returns the CBOR value of custom claim with
      integer key [key]. *)

  val get_string_key : string -> t -> Cbort.Cbor.t option
  (** [get_string_key key claims] returns the CBOR value of custom claim with
      string key [key]. *)

  (** {2 Construction} *)

  type builder
  (** Builder for constructing claims. *)

  val empty : builder
  (** Empty claims builder. *)

  val set_iss : string -> builder -> builder
  (** Set issuer claim. *)

  val set_sub : string -> builder -> builder
  (** Set subject claim. *)

  val set_aud : string list -> builder -> builder
  (** Set audience claim. *)

  val set_exp : Ptime.t -> builder -> builder
  (** Set expiration time claim. *)

  val set_nbf : Ptime.t -> builder -> builder
  (** Set not-before claim. *)

  val set_iat : Ptime.t -> builder -> builder
  (** Set issued-at claim. *)

  val set_cti : string -> builder -> builder
  (** Set CWT ID claim (raw bytes). *)

  val set_int_key : int -> Cbort.Cbor.t -> builder -> builder
  (** [set_int_key key value builder] sets a custom claim with integer key.
      [value] is a CBOR value that will be serialized. *)

  val set_string_key : string -> Cbort.Cbor.t -> builder -> builder
  (** [set_string_key key value builder] sets a custom claim with string key.
      [value] is a CBOR value that will be serialized. *)

  val build : builder -> t
  (** Build the claims set. *)

  (** {2 Serialization} *)

  val of_cbor : string -> (t, error) result
  (** Parse claims from CBOR-encoded CWT Claims Set. *)

  val to_cbor : t -> string
  (** Serialize claims to CBOR-encoded CWT Claims Set. *)
end

(** {1 CWT Token} *)

type t
(** A parsed CWT token (COSE_Sign1 or COSE_Mac0 structure). Note: COSE_Encrypt0
    is not currently supported. *)

(** {2 Parsing}

    Parse CWT from CBOR bytes. The CWT may be tagged (with COSE tag) or untagged
    per
    {{:https://datatracker.ietf.org/doc/html/rfc8392#section-2}RFC 8392 Section
     2}. *)

val parse : string -> (t, error) result
(** [parse cwt_bytes] parses a CWT from CBOR bytes.

    This parses the COSE structure and extracts the claims, but does NOT verify
    the signature/MAC. Use {!verify} to validate cryptographic protection after
    parsing. *)

(** {2 Accessors} *)

val claims : t -> Claims.t
(** [claims t] returns the claims set. *)

val algorithm : t -> Algorithm.t option
(** [algorithm t] returns the COSE algorithm from the protected header. *)

val kid : t -> string option
(** [kid t] returns the key ID from headers if present. *)

val raw : t -> string
(** [raw t] returns the original CBOR bytes. *)

(** {2 Verification}

    Verify cryptographic protection and validate claims. *)

val verify :
  key:Cose_key.t -> ?allowed_algs:Algorithm.t list -> t -> (unit, error) result
(** [verify ~key ?allowed_algs t] verifies the COSE signature or MAC.

    @param key The key to verify with (must match algorithm)
    @param allowed_algs List of acceptable algorithms. Default: all. *)

val validate :
  now:Ptime.t ->
  ?iss:string ->
  ?aud:string ->
  ?leeway:Ptime.Span.t ->
  t ->
  (unit, error) result
(** [validate ~now ?iss ?aud ?leeway t] validates CWT claims.

    @param now Current time (required, no implicit clock)
    @param iss Expected issuer (if provided, must match exactly)
    @param aud Expected audience (if provided, must be in audience list)
    @param leeway Clock skew tolerance for exp/nbf checks (default 0s) *)

val verify_and_validate :
  key:Cose_key.t ->
  now:Ptime.t ->
  ?allowed_algs:Algorithm.t list ->
  ?iss:string ->
  ?aud:string ->
  ?leeway:Ptime.Span.t ->
  t ->
  (unit, error) result
(** [verify_and_validate ~key ~now ...] verifies signature and validates claims.
*)

(** {2 Creation}

    Create and sign CWTs. *)

val create :
  algorithm:Algorithm.t ->
  claims:Claims.t ->
  key:Cose_key.t ->
  (t, error) result
(** [create ~algorithm ~claims ~key] creates and signs a new CWT.

    Creates a COSE_Sign1 structure for signature algorithms (ES256, ES384,
    ES512, EdDSA) or COSE_Mac0 for MAC algorithms (HMAC_256, HMAC_384,
    HMAC_512).

    The [key] must be appropriate for the algorithm:
    - HMAC algorithms: symmetric key
    - ES256: P-256 private key
    - ES384: P-384 private key
    - ES512: P-521 private key
    - EdDSA: Ed25519 private key *)

val encode : t -> string
(** [encode t] returns the CBOR serialization of the CWT. The result is a tagged
    COSE structure (COSE_Sign1 or COSE_Mac0). *)

(** {1 Utilities} *)

val is_expired : now:Ptime.t -> ?leeway:Ptime.Span.t -> t -> bool
(** [is_expired ~now ?leeway t] checks if the token has expired. Returns false
    if no exp claim present. *)

val time_to_expiry : now:Ptime.t -> t -> Ptime.Span.t option
(** [time_to_expiry ~now t] returns time until expiration, or [None] if no
    expiration claim or already expired. *)
