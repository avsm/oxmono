(* Copyright (c) 2025 Anil Madhavapeddy. SPDX-License-Identifier: ISC *)

(** Bounded compact JWTs. Signing, parsing, signature verification and claim
    policy are separate operations. See [spec/REVIEW.md] for the supported
    RFC profile and its limits. No function discovers or fetches keys. *)

type error : immutable_data =
  | Invalid_json of string
  | Invalid_base64url of string
  | Invalid_structure of string
  | Invalid_header of string
  | Invalid_claims of string
  | Invalid_uri of string
  | Duplicate_claim of string
  | Unsupported_algorithm of string
  | Algorithm_not_allowed of string
  | Signature_mismatch
  | Token_expired
  | Token_not_yet_valid
  | Invalid_issuer
  | Invalid_audience
  | Key_type_mismatch of string
  | Unsecured_not_allowed

val pp_error : Format.formatter -> error -> unit
val error_to_string : error -> string

module Algorithm : sig
  type t : immediate =
    | None | HS256 | HS384 | HS512 | RS256 | RS384 | RS512
    | ES256 | ES256K | ES384 | ES512 | EdDSA
  val to_string : t -> string @@ portable
  val of_string : string -> (t, error) result @@ portable
  val all : t list
  (** [all] contains supported verification algorithms, excluding [None].
      RSA names are recognized but RSA operations are unsupported. *)
  val all_with_none : t list
end

module Jwk : sig
  type kty = Oct | Ec | Okp
  type crv = P256 | P384 | P521 | Secp256k1 | Ed25519
  type t : immutable_data

  val symmetric : string -> t @@ portable
  (** [symmetric bytes] is an HMAC key. Operations require at least 32, 48 or
      64 bytes for HS256, HS384 or HS512 respectively. Use random key bytes
      and bind the algorithm with {!with_alg} before signing or verification. *)
  val ed25519_pub : string -> t
  val ed25519_priv : pub:string -> priv:string -> t
  val p256_pub : x:string -> y:string -> t
  val p256_priv : x:string -> y:string -> d:string -> t
  val p384_pub : x:string -> y:string -> t
  val p384_priv : x:string -> y:string -> d:string -> t
  val p521_pub : x:string -> y:string -> t
  val p521_priv : x:string -> y:string -> d:string -> t
  (** These constructors validate public points, coordinate/scalar widths and
      private/public agreement. Invalid keys raise [Invalid_argument]. *)
  val secp256k1_pub : string -> (t, error) result @@ portable
  (** [secp256k1_pub sec1] validates a 33-byte compressed or 65-byte
      uncompressed SEC1 point. ES256K supports verification only. *)

  val kty : t -> kty @@ portable
  val kid : t -> string option @@ portable
  val alg : t -> Algorithm.t option @@ portable
  val with_kid : string -> t -> t @@ portable
  val with_alg : Algorithm.t -> t -> t @@ portable
  (** [with_alg alg key] binds [key] to [alg]. Incompatible algorithms raise
      [Invalid_argument]. Both signing and verification enforce the binding. *)
  val of_json : string -> (t, error) result
  (** [of_json json] parses one JWK, rejecting duplicates, malformed fields,
      unknown algorithms, incompatible curves and invalid keys. [use], when
      present, must be [sig]. [key_ops] may restrict [sign] and [verify].
      Certificate parameters, RSA, other operations and secp256k1 private
      keys are unsupported. *)
  val to_json : t -> string @@ portable
  (** [to_json key] encodes [key], including private material when present. *)
end

module Header : sig
  type t : immutable_data = private {
    alg : Algorithm.t;
    typ : string option;
    kid : string option;
    cty : string option;
  }
  val make : ?typ:string -> ?kid:string -> ?cty:string -> Algorithm.t -> t
    @@ portable
  val of_json : string -> (t, error) result @@ portable
  (** [of_json json] requires an algorithm and rejects duplicate members,
      [crit], [b64] and nested JWTs. No JOSE extension is implemented. *)
  val to_json : t -> string @@ portable
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
  val jti : t -> string option @@ portable
  val get : string -> t -> Jsont.json option @@ portable
  val get_string : string -> t -> string option @@ portable
  val get_number : string -> t -> float option @@ portable
  (** [get_number name claims] returns the original JSON number, including
      registered NumericDates, without rounding through [Ptime]. *)
  val get_int : string -> t -> int option @@ portable
  (** [get_int name claims] is present only for an integral in-range number. *)
  val get_bool : string -> t -> bool option @@ portable

  val empty : builder
  val set_iss : string -> builder -> builder @@ portable
  val set_sub : string -> builder -> builder @@ portable
  val set_aud : string list -> builder -> builder @@ portable
  val set_exp : Ptime.t -> builder -> builder @@ portable
  val set_nbf : Ptime.t -> builder -> builder @@ portable
  val set_iat : Ptime.t -> builder -> builder @@ portable
  val set_jti : string -> builder -> builder @@ portable
  val set : string -> Jsont.json -> builder -> builder @@ portable
  (** [set name value builder] replaces any existing member named [name]. *)
  val set_string : string -> string -> builder -> builder @@ portable
  val set_int : string -> int -> builder -> builder @@ portable
  val set_bool : string -> bool -> builder -> builder @@ portable
  val build : builder -> t @@ portable
  (** [build builder] validates claims. Invalid values raise
      [Invalid_argument], including malformed registered claims. *)
  val of_json : string -> (t, error) result @@ portable
  (** [of_json json] rejects duplicates, invalid registered claim types and
      NumericDates outside [Ptime]'s range. Fractional dates are supported.
      JSON is limited to 64 KiB and 32 levels of nesting. *)
  val to_json : t -> string @@ portable
end

type t : immutable_data
(** A token whose parsed fields agree with its retained compact serialization.
    Its claims are untrusted until signature and application checks succeed. *)

val parse : ?max_size:int -> string -> (t, error) result @@ portable
(** [parse token] parses an unpadded, canonical base64url compact JWT.
    [max_size] defaults to 8192 bytes. Each decoded JSON object is limited to
    64 KiB and 32 levels. Duplicate members are always errors. No signature
    is verified. JWE, nested JWTs, CWT and unencoded payloads are
    unsupported. *)
val header : t -> Header.t @@ portable
val claims : t -> Claims.t @@ portable
val signature : t -> string @@ portable
val raw : t -> string @@ portable
val encode : t -> string @@ portable

val verify :
  key:Jwk.t -> ?allow_none:bool -> allowed_algs:Algorithm.t list -> t ->
  (unit, error) result @@ portable
(** [verify ~key ~allowed_algs token] verifies its signature using an explicitly
    trusted key and algorithm allowlist. It enforces key type, algorithm and
    operation constraints. [None] requires both [allow_none:true] and an
    allowlist entry. Claims, key discovery and replay policy are unchecked. *)
val validate :
  now:Ptime.t -> ?iss:string -> ?aud:string -> ?leeway:Ptime.Span.t -> t ->
  (unit, error) result @@ portable
(** [validate ~now token] checks any [exp] and [nbf], and the supplied issuer
    and audience. [exp = now] is expired with zero leeway. Negative leeway
    raises [Invalid_argument]. Applications must require necessary claims,
    constrain [iat] and lifetime, and check token type and custom claims. *)
val verify_and_validate :
  key:Jwk.t -> now:Ptime.t -> ?allow_none:bool ->
  allowed_algs:Algorithm.t list -> ?iss:string -> ?aud:string ->
  ?leeway:Ptime.Span.t -> t -> (unit, error) result @@ portable
(** [verify_and_validate ~key ~now ~allowed_algs token] verifies the signature
    before applying {!validate}. *)
val create :
  ?allow_none:bool -> header:Header.t -> claims:Claims.t -> key:Jwk.t -> unit ->
  (t, error) result
(** [create ~header ~claims ~key ()] signs a token with the selected algorithm.
    Unsecured tokens require [allow_none:true]. ECDSA signing requires an
    initialized Mirage Crypto RNG for masking. Creation is not portable. *)
val is_expired : now:Ptime.t -> ?leeway:Ptime.Span.t -> t -> bool @@ portable
val time_to_expiry : now:Ptime.t -> t -> Ptime.Span.t option @@ portable

val base64url_encode : string -> string @@ portable
val base64url_decode : string -> (string, error) result @@ portable
