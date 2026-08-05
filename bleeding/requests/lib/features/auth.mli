(*---------------------------------------------------------------------------
  Copyright (c) 2025 Anil Madhavapeddy <anil@recoil.org>. All rights reserved.
  SPDX-License-Identifier: ISC
 ---------------------------------------------------------------------------*)

(** HTTP authentication mechanisms

    This module provides authentication schemes for HTTP requests:

    - {b Basic}: {{:https://datatracker.ietf.org/doc/html/rfc7617}RFC 7617} - Base64 username:password
    - {b Bearer}: {{:https://datatracker.ietf.org/doc/html/rfc6750}RFC 6750} - OAuth 2.0 tokens
    - {b Digest}: {{:https://datatracker.ietf.org/doc/html/rfc7616}RFC 7616} - Challenge-response with MD5/SHA-256
    - {b Signature}: {{:https://datatracker.ietf.org/doc/html/rfc9421}RFC 9421} - HTTP Message Signatures

    For OAuth 2.0 with automatic token refresh, see the [requests.oauth] subpackage.

    {2 Security}

    Per {{:https://datatracker.ietf.org/doc/html/rfc7617#section-4}RFC 7617 Section 4} and
    {{:https://datatracker.ietf.org/doc/html/rfc6750#section-5.1}RFC 6750 Section 5.1},
    Basic, Bearer, and Digest authentication transmit credentials that MUST be
    protected by TLS. The library enforces HTTPS by default for these schemes. *)

(** Log source for authentication operations *)
val src : Logs.Src.t

type t
(** Abstract authentication type *)

val none : t
(** No authentication *)

val basic : username:string -> password:string -> t
(** HTTP Basic authentication *)

val bearer : token:string -> t
(** Bearer token authentication (e.g., OAuth 2.0) *)

val digest : username:string -> password:string -> t
(** HTTP Digest authentication (RFC 7616).

    Digest authentication is automatically handled: when a request returns
    a 401 response with a WWW-Authenticate: Digest header, the library will
    parse the challenge and retry the request with proper digest credentials.

    Supports:
    - Algorithms: MD5, SHA-256, SHA-512 (not SHA-512-256)
    - QoP: auth, auth-int (body hashing)
    - userhash parameter (username hashing)

    Note: SHA-512-256 is not supported as it requires special initialization
    vectors not available in standard libraries. *)

val signature : Signature.config -> t
(** HTTP Message Signatures (RFC 9421).

    Creates cryptographic signatures over HTTP message components.
    The signature covers selected headers and derived values like
    the method, path, and authority.

    Use {!val:Signature.config} to create the configuration:
    {[
      let key = Signature.Key.ed25519 ~priv:... ~pub:... in
      let config = Signature.config ~key ~keyid:"my-key" () in
      let auth = Auth.signature config
    ]}

    The signature is computed and added when the request is made,
    as it requires the full request context (method, URI, headers). *)

val bearer_form : token:string -> t
(** Bearer token in form-encoded body (RFC 6750 Section 2.2).

    This sends the Bearer token as an "access_token" form parameter
    instead of in the Authorization header. Less preferred than the
    header method but required by some APIs.

    When using this, set Content-Type to application/x-www-form-urlencoded
    and use {!get_bearer_form_body} to get the body content. *)

val custom : (Headers.t -> Headers.t) -> t
(** Custom authentication handler *)

val apply : t -> Headers.t -> Headers.t
(** Apply authentication to headers.
    Note: This does not validate transport security. Use [apply_secure] for
    HTTPS enforcement per RFC 7617/6750. *)

val apply_secure : ?allow_insecure_auth:bool -> url:string -> t -> Headers.t -> Headers.t
(** Apply authentication with HTTPS validation.
    Per RFC 7617 Section 4 (Basic) and RFC 6750 Section 5.1 (Bearer):
    Basic, Bearer, and Digest authentication MUST be used over TLS.

    @param allow_insecure_auth If [true], skip the HTTPS check (not recommended,
           only for testing environments). Default: [false]
    @param url The request URL (used for security check)
    @raise Error.Insecure_auth if sensitive auth is used over HTTP *)

val validate_secure_transport : ?allow_insecure_auth:bool -> url:string -> t -> unit
(** Validate that sensitive authentication would be safe to use.
    Raises [Error.Insecure_auth] if Basic/Bearer/Digest auth would be used over HTTP.

    @param allow_insecure_auth If [true], skip the check. Default: [false] *)

val requires_https : t -> bool
(** Returns [true] if the authentication type requires HTTPS transport.
    Basic, Bearer, and Digest require HTTPS; No_auth and Custom do not. *)

(** {1 Digest Authentication Support} *)

(** Digest authentication challenge parsed from WWW-Authenticate header *)
type digest_challenge = {
  realm : string;
  nonce : string;
  qop : string option;
  algorithm : string;  (** MD5, SHA-256, etc. *)
  opaque : string option;
  stale : bool;
  (** If true, the nonce is stale but credentials are valid. Client should
      retry with the new nonce. Per RFC 7616 Section 3.2.2. *)
  userhash : bool;
  (** If true, the server wants the username to be hashed.
      Per RFC 7616 Section 3.4.4. *)
}

val parse_www_authenticate : string -> digest_challenge option
(** [parse_www_authenticate header] parses a WWW-Authenticate header value
    and returns the Digest challenge if present. Returns [None] if the header
    is not a Digest challenge or cannot be parsed. *)

(** {2 Nonce Count Tracking}

    Per RFC 7616, the nonce count (nc) must be incremented for each request
    using the same server nonce to prevent replay attacks. *)

module Nonce_counter : sig
  type t
  (** Mutable nonce count tracker, keyed by server nonce *)

  val create : unit -> t
  (** Create a new nonce counter *)

  val next : t -> nonce:string -> string
  (** [next t ~nonce] gets and increments the count for the given server nonce.
      Returns the count formatted as 8 hex digits (e.g., "00000001"). *)

  val clear : t -> unit
  (** Clear all tracked nonces (e.g., on session reset) *)
end

val apply_digest :
  ?nonce_counter:Nonce_counter.t ->
  ?body:string ->
  username:string ->
  password:string ->
  method_:string ->
  uri:string ->
  challenge:digest_challenge ->
  Headers.t ->
  Headers.t
(** [apply_digest ?nonce_counter ?body ~username ~password ~method_ ~uri ~challenge headers]
    applies Digest authentication to [headers] using the given credentials
    and server challenge.

    @param nonce_counter Optional nonce counter for replay protection.
           When provided, the nonce count is tracked and incremented per-nonce
           across multiple requests in a session. When not provided, defaults
           to "00000001" (suitable for single-request/one-shot mode).
    @param body Optional request body for auth-int qop support.
           When provided and the server supports auth-int qop, the body hash
           is included in the digest calculation per RFC 7616. *)

val is_digest : t -> bool
(** [is_digest auth] returns [true] if [auth] is Digest authentication. *)

val get_digest_credentials : t -> (string * string) option
(** [get_digest_credentials auth] returns [Some (username, password)] if
    [auth] is Digest authentication, [None] otherwise. *)

val is_bearer_form : t -> bool
(** [is_bearer_form auth] returns [true] if [auth] is Bearer form authentication. *)

val get_bearer_form_body : t -> string option
(** [get_bearer_form_body auth] returns [Some "access_token=<token>"] if
    [auth] is Bearer form authentication, [None] otherwise.
    Use this to get the form-encoded body content for RFC 6750 Section 2.2. *)

val digest_is_stale : digest_challenge -> bool
(** [digest_is_stale challenge] returns [true] if the challenge has stale=true.
    Per RFC 7616 Section 3.2.2: If stale=true, the nonce is expired but the
    credentials are still valid. The client should retry with the same
    credentials using the new nonce. If stale=false or not present, the
    credentials themselves are wrong. *)

(** {1 HTTP Message Signatures (RFC 9421)} *)

val is_signature : t -> bool
(** [is_signature auth] returns [true] if [auth] is HTTP Message Signature authentication. *)

val get_signature_config : t -> Signature.config option
(** [get_signature_config auth] returns [Some config] if [auth] is HTTP Message
    Signature authentication, [None] otherwise. *)

val apply_signature :
  clock:_ Eio.Time.clock ->
  method_:Method.t ->
  uri:Uri.t ->
  headers:Headers.t ->
  t ->
  Headers.t
(** [apply_signature ~clock ~method_ ~uri ~headers auth] applies HTTP Message Signature
    to [headers] if [auth] is Signature authentication. Returns the headers with
    [Signature-Input] and [Signature] headers added.

    This function computes the signature based on the request context and adds
    the appropriate headers per RFC 9421.

    @param clock Eio clock for timestamp generation in the signature.

    If [auth] is not Signature authentication, returns [headers] unchanged.
    If signature computation fails, logs an error and returns [headers] unchanged. *)

