(*---------------------------------------------------------------------------
  Copyright (c) 2025 Anil Madhavapeddy <anil@recoil.org>. All rights reserved.
  SPDX-License-Identifier: ISC
 ---------------------------------------------------------------------------*)

(** JSON Web Token (JWT) - RFC 7519

    This module implements JSON Web Tokens as specified in
    {{:https://datatracker.ietf.org/doc/html/rfc7519}RFC 7519}.

    JWTs are compact, URL-safe means of representing claims to be transferred
    between two parties. The claims are encoded as a JSON object that is used as
    the payload of a JSON Web Signature (JWS) structure, enabling the claims to
    be digitally signed or integrity protected with a Message Authentication
    Code (MAC).

    {2 References}
    - {{:https://datatracker.ietf.org/doc/html/rfc7519}RFC 7519} - JSON Web
      Token (JWT)
    - {{:https://datatracker.ietf.org/doc/html/rfc7515}RFC 7515} - JSON Web
      Signature (JWS)
    - {{:https://datatracker.ietf.org/doc/html/rfc7517}RFC 7517} - JSON Web Key
      (JWK)
    - {{:https://datatracker.ietf.org/doc/html/rfc7518}RFC 7518} - JSON Web
      Algorithms (JWA) *)

(** {1 Error Handling} *)

type error =
  | Invalid_json of string  (** JSON parsing failed *)
  | Invalid_base64url of string  (** Base64url decoding failed *)
  | Invalid_structure of string
      (** Wrong number of parts or malformed structure *)
  | Invalid_header of string  (** Header validation failed *)
  | Invalid_claims of string  (** Claims validation failed *)
  | Invalid_uri of string
      (** StringOrURI validation failed per
          {{:https://datatracker.ietf.org/doc/html/rfc7519#section-2}RFC 7519
           Section 2} *)
  | Duplicate_claim of string
      (** Duplicate claim name found in strict mode per
          {{:https://datatracker.ietf.org/doc/html/rfc7519#section-4}RFC 7519
           Section 4} *)
  | Unsupported_algorithm of string  (** Unknown algorithm identifier *)
  | Algorithm_not_allowed of string
      (** Algorithm rejected by allowed_algs policy *)
  | Signature_mismatch  (** Signature verification failed *)
  | Token_expired
      (** exp claim validation failed per
          {{:https://datatracker.ietf.org/doc/html/rfc7519#section-4.1.4}RFC
           7519 Section 4.1.4} *)
  | Token_not_yet_valid
      (** nbf claim validation failed per
          {{:https://datatracker.ietf.org/doc/html/rfc7519#section-4.1.5}RFC
           7519 Section 4.1.5} *)
  | Invalid_issuer
      (** iss claim mismatch per
          {{:https://datatracker.ietf.org/doc/html/rfc7519#section-4.1.1}RFC
           7519 Section 4.1.1} *)
  | Invalid_audience
      (** aud claim mismatch per
          {{:https://datatracker.ietf.org/doc/html/rfc7519#section-4.1.3}RFC
           7519 Section 4.1.3} *)
  | Key_type_mismatch of string  (** Key doesn't match algorithm *)
  | Unsecured_not_allowed
      (** alg:none used without explicit opt-in per
          {{:https://datatracker.ietf.org/doc/html/rfc7519#section-6}RFC 7519
           Section 6} *)
  | Nesting_too_deep  (** Nested JWT exceeds max_depth *)

val pp_error : Format.formatter -> error -> unit
(** Pretty-print an error. *)

val error_to_string : error -> string
(** Convert error to human-readable string. *)

(** {1 Algorithms}

    Signature and MAC algorithms for JWT. See
    {{:https://datatracker.ietf.org/doc/html/rfc7518#section-3}RFC 7518 Section
     3}. *)

module Algorithm : sig
  type t =
    | None
        (** No digital signature or MAC per
            {{:https://datatracker.ietf.org/doc/html/rfc7518#section-3.6}RFC
             7518 Section 3.6} *)
    | HS256
        (** HMAC using SHA-256 per
            {{:https://datatracker.ietf.org/doc/html/rfc7518#section-3.2}RFC
             7518 Section 3.2} *)
    | HS384  (** HMAC using SHA-384 *)
    | HS512  (** HMAC using SHA-512 *)
    | RS256
        (** RSASSA-PKCS1-v1_5 using SHA-256 per
            {{:https://datatracker.ietf.org/doc/html/rfc7518#section-3.3}RFC
             7518 Section 3.3} *)
    | RS384  (** RSASSA-PKCS1-v1_5 using SHA-384 *)
    | RS512  (** RSASSA-PKCS1-v1_5 using SHA-512 *)
    | ES256
        (** ECDSA using P-256 and SHA-256 per
            {{:https://datatracker.ietf.org/doc/html/rfc7518#section-3.4}RFC
             7518 Section 3.4} *)
    | ES384  (** ECDSA using P-384 and SHA-384 *)
    | ES512  (** ECDSA using P-521 and SHA-512 *)
    | EdDSA
        (** EdDSA using Ed25519 per
            {{:https://datatracker.ietf.org/doc/html/rfc8037}RFC 8037} *)

  val to_string : t -> string
  (** Convert algorithm to JWA identifier string. *)

  val of_string : string -> (t, error) result
  (** Parse algorithm from JWA identifier string. *)

  val all : t list
  (** All supported algorithms (excluding None). *)

  val all_with_none : t list
  (** All supported algorithms (including None). *)
end

(** {1 JSON Web Key}

    Key representation for JWT signature verification. See
    {{:https://datatracker.ietf.org/doc/html/rfc7517}RFC 7517}. *)

module Jwk : sig
  (** Key type per
      {{:https://datatracker.ietf.org/doc/html/rfc7517#section-4.1}RFC 7517
       Section 4.1}. *)
  type kty =
    | Oct  (** Octet sequence (symmetric key) *)
    | Rsa  (** RSA key *)
    | Ec  (** Elliptic Curve key *)
    | Okp  (** Octet Key Pair (Ed25519, X25519) *)

  (** Elliptic curve identifiers per
      {{:https://datatracker.ietf.org/doc/html/rfc7518#section-6.2.1.1}RFC 7518
       Section 6.2.1.1}. *)
  type crv =
    | P256  (** NIST P-256 curve *)
    | P384  (** NIST P-384 curve *)
    | P521  (** NIST P-521 curve *)
    | Ed25519
        (** Ed25519 curve per
            {{:https://datatracker.ietf.org/doc/html/rfc8037}RFC 8037} *)

  type t
  (** A JSON Web Key. *)

  (** {2 Constructors} *)

  val symmetric : string -> t
  (** [symmetric k] creates a symmetric key from raw bytes. Used for HMAC
      algorithms (HS256, HS384, HS512). *)

  val ed25519_pub : string -> t
  (** [ed25519_pub pub] creates an Ed25519 public key from 32-byte public key.
  *)

  val ed25519_priv : pub:string -> priv:string -> t
  (** [ed25519_priv ~pub ~priv] creates an Ed25519 private key. *)

  val p256_pub : x:string -> y:string -> t
  (** [p256_pub ~x ~y] creates a P-256 public key from coordinates. *)

  val p256_priv : x:string -> y:string -> d:string -> t
  (** [p256_priv ~x ~y ~d] creates a P-256 private key. *)

  val p384_pub : x:string -> y:string -> t
  (** [p384_pub ~x ~y] creates a P-384 public key from coordinates. *)

  val p384_priv : x:string -> y:string -> d:string -> t
  (** [p384_priv ~x ~y ~d] creates a P-384 private key. *)

  val p521_pub : x:string -> y:string -> t
  (** [p521_pub ~x ~y] creates a P-521 public key from coordinates. *)

  val p521_priv : x:string -> y:string -> d:string -> t
  (** [p521_priv ~x ~y ~d] creates a P-521 private key. *)

  val rsa_pub : n:string -> e:string -> t
  (** [rsa_pub ~n ~e] creates an RSA public key from modulus and exponent. *)

  val rsa_priv :
    n:string ->
    e:string ->
    d:string ->
    p:string ->
    q:string ->
    dp:string ->
    dq:string ->
    qi:string ->
    t
  (** [rsa_priv ~n ~e ~d ~p ~q ~dp ~dq ~qi] creates an RSA private key. *)

  (** {2 Accessors} *)

  val kty : t -> kty
  (** Get the key type. *)

  val kid : t -> string option
  (** Get the key ID if set. *)

  val alg : t -> Algorithm.t option
  (** Get the intended algorithm if set. *)

  val with_kid : string -> t -> t
  (** [with_kid id key] returns key with kid set to [id]. *)

  val with_alg : Algorithm.t -> t -> t
  (** [with_alg alg key] returns key with alg set to [alg]. *)

  (** {2 Serialization} *)

  val of_json : string -> (t, error) result
  (** Parse a JWK from JSON string. *)

  val to_json : t -> string
  (** Serialize a JWK to JSON string. *)
end

(** {1 JOSE Header}

    The JOSE (JSON Object Signing and Encryption) Header. See
    {{:https://datatracker.ietf.org/doc/html/rfc7519#section-5}RFC 7519 Section
     5}. *)

module Header : sig
  type t = {
    alg : Algorithm.t;  (** Algorithm used (REQUIRED) *)
    typ : string option;
        (** Type - RECOMMENDED to be "JWT" per
            {{:https://datatracker.ietf.org/doc/html/rfc7519#section-5.1}RFC
             7519 Section 5.1} *)
    kid : string option;  (** Key ID for key lookup *)
    cty : string option;
        (** Content type - MUST be "JWT" for nested JWTs per
            {{:https://datatracker.ietf.org/doc/html/rfc7519#section-5.2}RFC
             7519 Section 5.2} *)
  }

  val make : ?typ:string -> ?kid:string -> ?cty:string -> Algorithm.t -> t
  (** [make ?typ ?kid ?cty alg] creates a JOSE header. *)

  val of_json : string -> (t, error) result
  (** Parse header from JSON string. *)

  val to_json : t -> string
  (** Serialize header to JSON string. *)

  val is_nested : t -> bool
  (** [is_nested h] returns true if [cty] is "JWT" (case-insensitive),
      indicating a nested JWT. *)
end

(** {1 Claims}

    JWT Claims Set. See
    {{:https://datatracker.ietf.org/doc/html/rfc7519#section-4}RFC 7519 Section
     4}. *)

module Claims : sig
  type t

  (** {2 Registered Claim Names}

      See
      {{:https://datatracker.ietf.org/doc/html/rfc7519#section-4.1}RFC 7519
       Section 4.1}. *)

  val iss : t -> string option
  (** Issuer claim per
      {{:https://datatracker.ietf.org/doc/html/rfc7519#section-4.1.1}Section
       4.1.1}. *)

  val sub : t -> string option
  (** Subject claim per
      {{:https://datatracker.ietf.org/doc/html/rfc7519#section-4.1.2}Section
       4.1.2}. *)

  val aud : t -> string list
  (** Audience claim per
      {{:https://datatracker.ietf.org/doc/html/rfc7519#section-4.1.3}Section
       4.1.3}. Returns empty list if not present. May be single string or array
      in JWT. *)

  val exp : t -> Ptime.t option
  (** Expiration time claim per
      {{:https://datatracker.ietf.org/doc/html/rfc7519#section-4.1.4}Section
       4.1.4}. *)

  val nbf : t -> Ptime.t option
  (** Not Before claim per
      {{:https://datatracker.ietf.org/doc/html/rfc7519#section-4.1.5}Section
       4.1.5}. *)

  val iat : t -> Ptime.t option
  (** Issued At claim per
      {{:https://datatracker.ietf.org/doc/html/rfc7519#section-4.1.6}Section
       4.1.6}. *)

  val jti : t -> string option
  (** JWT ID claim per
      {{:https://datatracker.ietf.org/doc/html/rfc7519#section-4.1.7}Section
       4.1.7}. *)

  (** {2 Custom Claims}

      For Public and Private claims per
      {{:https://datatracker.ietf.org/doc/html/rfc7519#section-4.2}Sections 4.2}
      and {{:https://datatracker.ietf.org/doc/html/rfc7519#section-4.3}4.3}. *)

  val get : string -> t -> Jsont.json option
  (** [get name claims] returns the value of custom claim [name]. *)

  val get_string : string -> t -> string option
  (** [get_string name claims] returns the string value of custom claim [name].
  *)

  val get_int : string -> t -> int option
  (** [get_int name claims] returns the integer value of custom claim [name]. *)

  val get_bool : string -> t -> bool option
  (** [get_bool name claims] returns the boolean value of custom claim [name].
  *)

  (** {2 Construction} *)

  type builder
  (** Builder for constructing claims. *)

  val empty : builder
  (** Empty claims builder. *)

  val set_iss : string -> builder -> builder
  (** Set issuer claim. Value is validated as StringOrURI. *)

  val set_sub : string -> builder -> builder
  (** Set subject claim. Value is validated as StringOrURI. *)

  val set_aud : string list -> builder -> builder
  (** Set audience claim. *)

  val set_exp : Ptime.t -> builder -> builder
  (** Set expiration time claim. *)

  val set_nbf : Ptime.t -> builder -> builder
  (** Set not-before claim. *)

  val set_iat : Ptime.t -> builder -> builder
  (** Set issued-at claim. *)

  val set_jti : string -> builder -> builder
  (** Set JWT ID claim. *)

  val set : string -> Jsont.json -> builder -> builder
  (** [set name value builder] sets a custom claim. *)

  val set_string : string -> string -> builder -> builder
  (** Set a custom string claim. *)

  val set_int : string -> int -> builder -> builder
  (** Set a custom integer claim. *)

  val set_bool : string -> bool -> builder -> builder
  (** Set a custom boolean claim. *)

  val build : builder -> t
  (** Build the claims set. *)

  (** {2 Serialization} *)

  val of_json : ?strict:bool -> string -> (t, error) result
  (** [of_json ?strict json] parses claims from JSON string.
      @param strict
        If true (default), reject duplicate claim names per
        {{:https://datatracker.ietf.org/doc/html/rfc7519#section-4}RFC 7519
         Section 4}. If false, use lexically last duplicate. *)

  val to_json : t -> string
  (** Serialize claims to JSON string. *)
end

(** {1 JWT Token} *)

type t = {
  header : Header.t;  (** JOSE header *)
  claims : Claims.t;  (** Claims set *)
  signature : string;  (** Raw signature bytes *)
  raw : string;  (** Original compact serialization *)
}
(** A parsed JWT token. *)

(** {2 Parsing}

    See
    {{:https://datatracker.ietf.org/doc/html/rfc7519#section-7.2}RFC 7519
     Section 7.2}. *)

val parse : ?strict:bool -> string -> (t, error) result
(** [parse ?strict token_string] parses a JWT from its compact serialization.

    This parses the token structure but does NOT verify the signature. Use
    {!verify} to validate the signature after parsing.

    @param strict If true (default), reject duplicate claim names. *)

val parse_unsafe : string -> (t, error) result
(** [parse_unsafe token_string] parses a JWT without strict validation.
    Equivalent to [parse ~strict:false]. *)

(** {2 Nested JWTs}

    See
    {{:https://datatracker.ietf.org/doc/html/rfc7519#section-7.2}RFC 7519
     Section 7.2 step 8} and
    {{:https://datatracker.ietf.org/doc/html/rfc7519#appendix-A.2}Appendix A.2}.
*)

val parse_nested :
  ?strict:bool -> ?max_depth:int -> string -> (t list, error) result
(** [parse_nested ?strict ?max_depth token] parses a potentially nested JWT.
    Returns a list of JWTs from outermost to innermost.
    @param max_depth Maximum nesting depth (default 5). *)

val is_nested : t -> bool
(** [is_nested t] returns true if the JWT has [cty: "JWT"] header, indicating it
    contains a nested JWT. *)

(** {2 Accessors} *)

val header : t -> Header.t
(** [header t] returns the JOSE header. *)

val claims : t -> Claims.t
(** [claims t] returns the claims set. *)

val signature : t -> string
(** [signature t] returns the raw signature bytes. *)

val raw : t -> string
(** [raw t] returns the original token string. *)

(** {2 Verification}

    See
    {{:https://datatracker.ietf.org/doc/html/rfc7519#section-7.2}RFC 7519
     Section 7.2}. *)

val verify :
  key:Jwk.t ->
  ?allow_none:bool ->
  ?allowed_algs:Algorithm.t list ->
  t ->
  (unit, error) result
(** [verify ~key ?allow_none ?allowed_algs t] verifies the JWT signature.

    @param key The key to verify with (must match algorithm)
    @param allow_none
      If true, accept [alg:"none"]. Default: false. Per
      {{:https://datatracker.ietf.org/doc/html/rfc7519#section-6}RFC 7519
       Section 6}, unsecured JWTs should only be used when security is provided
      by other means.
    @param allowed_algs
      List of acceptable algorithms. Default: all except none. Note: "none" is
      only allowed if BOTH in this list AND [allow_none=true]. *)

val validate :
  now:Ptime.t ->
  ?iss:string ->
  ?aud:string ->
  ?leeway:Ptime.Span.t ->
  t ->
  (unit, error) result
(** [validate ~now ?iss ?aud ?leeway t] validates JWT claims.

    @param now Current time (required, no implicit clock)
    @param iss Expected issuer (if provided, must match exactly)
    @param aud Expected audience (if provided, must be in audience list)
    @param leeway Clock skew tolerance for exp/nbf checks (default 0s) *)

val verify_and_validate :
  key:Jwk.t ->
  now:Ptime.t ->
  ?allow_none:bool ->
  ?allowed_algs:Algorithm.t list ->
  ?iss:string ->
  ?aud:string ->
  ?leeway:Ptime.Span.t ->
  t ->
  (unit, error) result
(** [verify_and_validate ~key ~now ...] verifies signature and validates claims.
*)

(** {2 Creation}

    See
    {{:https://datatracker.ietf.org/doc/html/rfc7519#section-7.1}RFC 7519
     Section 7.1}. *)

val create :
  header:Header.t -> claims:Claims.t -> key:Jwk.t -> (t, error) result
(** [create ~header ~claims ~key] creates and signs a new JWT.

    The [key] must be appropriate for the algorithm specified in [header]. For
    [alg:none], pass any key (it will be ignored). *)

val encode : t -> string
(** [encode t] returns the compact serialization of the JWT. *)

(** {1 Utilities} *)

val is_expired : now:Ptime.t -> ?leeway:Ptime.Span.t -> t -> bool
(** [is_expired ~now ?leeway t] checks if the token has expired. Returns false
    if no exp claim present. *)

val time_to_expiry : now:Ptime.t -> t -> Ptime.Span.t option
(** [time_to_expiry ~now t] returns time until expiration, or [None] if no
    expiration claim or already expired. *)

(** {1 Base64url Utilities}

    Exposed for testing with RFC test vectors. *)

val base64url_encode : string -> string
(** Base64url encode without padding per
    {{:https://datatracker.ietf.org/doc/html/rfc7515#appendix-C}RFC 7515
     Appendix C}. *)

val base64url_decode : string -> (string, error) result
(** Base64url decode, handling missing padding. *)

(** {1 CBOR Web Token (CWT)}

    See {{:https://datatracker.ietf.org/doc/html/rfc8392}RFC 8392}. *)

module Cwt = Cwt
