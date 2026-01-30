(*---------------------------------------------------------------------------
  Copyright (c) 2025 Anil Madhavapeddy <anil@recoil.org>. All rights reserved.
  SPDX-License-Identifier: ISC
 ---------------------------------------------------------------------------*)

(** RFC 6749 OAuth 2.0 Authorization Framework.

    This module implements the OAuth 2.0 authorization framework as specified in
    {{:https://datatracker.ietf.org/doc/html/rfc6749}RFC 6749}.

    {2 Supported Grant Types}
    - {{:https://datatracker.ietf.org/doc/html/rfc6749#section-4.1}Authorization Code} (Section 4.1)
    - {{:https://datatracker.ietf.org/doc/html/rfc6749#section-4.3}Resource Owner Password Credentials} (Section 4.3)
    - {{:https://datatracker.ietf.org/doc/html/rfc6749#section-4.4}Client Credentials} (Section 4.4)

    The Implicit Grant (Section 4.2) is intentionally not supported as it is
    deprecated per {{:https://datatracker.ietf.org/doc/html/rfc8996}RFC 8996}.

    {2 PKCE Support}

    This module supports Proof Key for Code Exchange (PKCE) per
    {{:https://datatracker.ietf.org/doc/html/rfc7636}RFC 7636} to protect
    against authorization code interception attacks, especially for public clients.

    {2 Usage Example}

    {[
      (* Client credentials grant *)
      let config = Oauth.make_config
        ~client_id:"my-client"
        ~client_secret:"my-secret"
        ~token_endpoint:"https://auth.example.com/token"
        () in
      match Oauth.client_credentials session config with
      | Ok token -> Printf.printf "Got token: %s\n" (Oauth.get_access_token token)
      | Error e -> Printf.printf "Error: %a\n" Oauth.pp_error e

      (* Authorization code flow with PKCE *)
      let pkce = Oauth.generate_pkce () in
      let state = Oauth.generate_state () in
      let auth_url = Oauth.authorization_url ~config ~state ~pkce () in
      (* ... redirect user to auth_url, receive code ... *)
      match Oauth.exchange_code session config ~code ~pkce_verifier:pkce.verifier () with
      | Ok token -> ...
      | Error e -> ...
    ]}

    {2 References}
    {ul
    {- {{:https://datatracker.ietf.org/doc/html/rfc6749}RFC 6749} - OAuth 2.0 Authorization Framework}
    {- {{:https://datatracker.ietf.org/doc/html/rfc7636}RFC 7636} - PKCE (Proof Key for Code Exchange)}
    {- {{:https://datatracker.ietf.org/doc/html/rfc6750}RFC 6750} - Bearer Token Usage}
    {- {{:https://datatracker.ietf.org/doc/html/rfc8996}RFC 8996} - Deprecates Implicit Grant}} *)

(** {1 Client Configuration} *)

(** OAuth 2.0 client configuration.

    Per {{:https://datatracker.ietf.org/doc/html/rfc6749#section-2}RFC 6749 Section 2},
    clients are identified by a client ID and optionally authenticated with a client secret. *)
type config = {
  client_id : string;
  (** The client identifier issued during registration.
      Per {{:https://datatracker.ietf.org/doc/html/rfc6749#section-2.2}Section 2.2}. *)

  client_secret : string option;
  (** The client secret for confidential clients. [None] for public clients.
      Per {{:https://datatracker.ietf.org/doc/html/rfc6749#section-2.3.1}Section 2.3.1}. *)

  token_endpoint : string;
  (** The authorization server's token endpoint URL.
      Per {{:https://datatracker.ietf.org/doc/html/rfc6749#section-3.2}Section 3.2}. *)

  authorization_endpoint : string option;
  (** The authorization server's authorization endpoint URL.
      Required for Authorization Code grant.
      Per {{:https://datatracker.ietf.org/doc/html/rfc6749#section-3.1}Section 3.1}. *)

  redirect_uri : string option;
  (** The client's redirection endpoint for Authorization Code grant.
      Per {{:https://datatracker.ietf.org/doc/html/rfc6749#section-3.1.2}Section 3.1.2}. *)

  scopes : string list;
  (** The requested access token scope.
      Per {{:https://datatracker.ietf.org/doc/html/rfc6749#section-3.3}Section 3.3}. *)
}

val make_config :
  client_id:string ->
  ?client_secret:string ->
  token_endpoint:string ->
  ?authorization_endpoint:string ->
  ?redirect_uri:string ->
  ?scopes:string list ->
  unit ->
  config
(** [make_config ~client_id ~token_endpoint ...] creates an OAuth client configuration. *)

(** {1 Token Types} *)

(** Token response from the authorization server.
    Per {{:https://datatracker.ietf.org/doc/html/rfc6749#section-5.1}Section 5.1}. *)
type token = {
  access_token : string;
  (** The access token issued by the authorization server. *)

  token_type : string;
  (** The type of the token, typically "Bearer". *)

  expires_at : Ptime.t option;
  (** When the token expires. [None] if no expiry was provided. *)

  refresh_token : string option;
  (** The refresh token for obtaining new access tokens. *)

  scope : string option;
  (** The scope of the access token. *)
}

val get_access_token : token -> string
(** [get_access_token token] returns the access token string. *)

val get_refresh_token : token -> string option
(** [get_refresh_token token] returns the refresh token if present. *)

val is_expired : token -> bool
(** [is_expired token] returns [true] if the token has expired.
    Returns [false] if the token has no expiry information. *)

val expires_within : Ptime.Span.t -> token -> bool
(** [expires_within span token] returns [true] if the token expires within [span].
    Returns [false] if the token has no expiry information. *)

(** {1 Error Types} *)

(** OAuth 2.0 error codes per RFC 6749 Section 5.2. *)
type error_code =
  | Invalid_request
  | Invalid_client
  | Invalid_grant
  | Unauthorized_client
  | Unsupported_grant_type
  | Invalid_scope
  | Unknown_error of string

(** OAuth error response. *)
type error = {
  code : error_code;
  description : string option;
  uri : string option;
}

val pp_error : Format.formatter -> error -> unit
(** Pretty printer for OAuth errors. *)

val error_code_to_string : error_code -> string
(** [error_code_to_string code] returns the RFC 6749 string representation. *)

(** {1 PKCE Support}

    Per {{:https://datatracker.ietf.org/doc/html/rfc7636}RFC 7636}. *)

(** PKCE challenge method. *)
type pkce_method =
  | Plain  (** code_challenge = code_verifier (not recommended) *)
  | S256   (** code_challenge = BASE64URL(SHA256(code_verifier)) *)

(** PKCE state for authorization code flow. *)
type pkce = {
  verifier : string;
  (** The code verifier (43-128 URL-safe characters). *)

  challenge : string;
  (** The code challenge derived from the verifier. *)

  method_ : pkce_method;
  (** The challenge derivation method. *)
}

val generate_pkce : ?method_:pkce_method -> unit -> pkce
(** [generate_pkce ()] generates PKCE verifier and challenge.
    Default method is [S256]. *)

val pkce_method_to_string : pkce_method -> string
(** Returns "plain" or "S256". *)

(** {1 State Parameter} *)

val generate_state : unit -> string
(** [generate_state ()] generates a cryptographically random state value
    for CSRF protection per RFC 6749 Section 10.12. *)

val validate_state : expected:string -> received:string -> bool
(** [validate_state ~expected ~received] performs constant-time comparison. *)

(** {1 Authorization URL} *)

val authorization_url :
  config:config ->
  state:string ->
  ?pkce:pkce ->
  ?extra_params:(string * string) list ->
  unit ->
  string
(** [authorization_url ~config ~state ()] builds the authorization URL.

    @raise Invalid_argument if [authorization_endpoint] is not configured. *)

(** {1 Token Operations}

    These functions use a {!Requests.t} session to make HTTP calls. *)

val client_credentials :
  Requests.t ->
  config ->
  (token, error) result
(** [client_credentials session config] performs the client credentials grant.
    Per {{:https://datatracker.ietf.org/doc/html/rfc6749#section-4.4}Section 4.4}. *)

val password_grant :
  Requests.t ->
  config ->
  username:string ->
  password:string ->
  (token, error) result
(** [password_grant session config ~username ~password] performs the resource owner
    password credentials grant.

    Per {{:https://datatracker.ietf.org/doc/html/rfc6749#section-4.3}Section 4.3}.

    {b Warning}: This grant type should only be used for legacy or high-trust scenarios. *)

val exchange_code :
  Requests.t ->
  config ->
  code:string ->
  ?pkce_verifier:string ->
  unit ->
  (token, error) result
(** [exchange_code session config ~code ()] exchanges an authorization code for tokens.
    Per {{:https://datatracker.ietf.org/doc/html/rfc6749#section-4.1.3}Section 4.1.3}. *)

val refresh :
  Requests.t ->
  config ->
  refresh_token:string ->
  (token, error) result
(** [refresh session config ~refresh_token] exchanges a refresh token for a new access token.
    Per {{:https://datatracker.ietf.org/doc/html/rfc6749#section-6}Section 6}. *)

(** {1 Managed Token State}

    Thread-safe automatic token refresh. *)

(** Managed OAuth state with automatic token refresh. *)
type t

val create :
  Requests.t ->
  config ->
  token ->
  ?on_refresh:(token -> unit) ->
  unit ->
  t
(** [create session config token ()] creates managed OAuth state.
    @param on_refresh Optional callback when tokens are refreshed. *)

val get_token : t -> token
(** [get_token t] returns the current token, refreshing if needed. Thread-safe. *)

val get_access_token_managed : t -> string
(** [get_access_token_managed t] returns the current access token, refreshing if needed. *)

val force_refresh : t -> (token, error) result
(** [force_refresh t] forces a token refresh. Thread-safe. *)

val with_client_credentials :
  Requests.t ->
  config ->
  ?on_refresh:(token -> unit) ->
  unit ->
  (t, error) result
(** [with_client_credentials session config ()] performs client credentials grant
    and returns managed state ready for use. *)
