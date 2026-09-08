(** oauth — the OAuth 2.0 API, raising instead of returning a result.

    {!Matrix_client.Oauth} documents the protocol and the types re-exported
    here, and every wrapper below raises [Eio.Io] where its counterpart returns
    a result. A transport, HTTP or decoding failure raises [Eio.Io] carrying an
    {!Error.type-err}; a protocol failure, which is a state that does not match
    or a user who refuses consent, raises one carrying {!E}.

    {!login_with_browser} adds what needs a runtime. It binds a loopback
    listener, hands the authorisation URL to the caller to open, waits for the
    browser and exchanges the code for a session.

    @see <https://spec.matrix.org/v1.15/client-server-api/#oauth-20-api>
      Client-Server API, "OAuth 2.0 API" *)

(** {1 OAuth-shaped errors} *)

type oauth_error = Matrix_client.Oauth.oauth_error = {
  error : string;
  error_description : string option;
  error_uri : string option;
}
(** The type for an RFC 6749 section 5.2 error response. It is
    {!Matrix_client.Oauth.oauth_error}, which documents the fields. *)

val pp_oauth_error : Format.formatter -> oauth_error -> unit
(** [pp_oauth_error ppf e] prints the code of [e] and, when present, its
    description. *)

val oauth_error_of_json : string -> oauth_error option
(** [oauth_error_of_json body] is the RFC 6749 error [body] carries, and [None]
    when [body] is not one. *)

val oauth_error_of_error : Matrix_client.Error.t -> oauth_error option
(** [oauth_error_of_error e] is the OAuth error inside [e]. *)

(** Why an authorisation flow failed on the protocol rather than in the
    transport. *)
type err =
  | State_mismatch
      (** The redirect carried a [state] other than the one sent. The response
          belongs to a different authorisation request, or to an attacker's. *)
  | Denied of oauth_error
      (** The authorisation server refused. [access_denied] is the user
          declining consent. *)
  | Expired  (** A device authorisation code reached its expiry. *)
  | OAuth_error of oauth_error
      (** An unrecognised OAuth error from a device-code poll. *)
  | Malformed_redirect of string  (** No usable [code] in the redirect. *)
  | Not_registered
      (** No [client_id] was supplied and the server offers no registration
          endpoint. *)
  | Timeout  (** The browser did not follow the redirect before [timeout]. *)

type session_invalid_reason =
  | Invalid_grant
      (** Why an OAuth session was reported invalid by its token endpoint. *)

type Eio.Exn.err +=
  | E of err
        (** The [Eio.Io] payload for an {!type-err}. It is registered with
            [Eio.Exn.register_pp], so it prints as {!pp_err} renders it. *)

val pp_err : Format.formatter -> err -> unit
(** [pp_err ppf e] prints [e] on one line, without a trailing newline. *)

(** {1 Scopes} *)

val scope_api : string
(** [scope_api] is [urn:matrix:client:api:*], the token granting full
    Client-Server API access. *)

val scope_device_prefix : string
(** [scope_device_prefix] is [urn:matrix:client:device:], the prefix of the
    device-allocation scope token. *)

val scope_api_unstable : string
(** [scope_api_unstable] is the MSC2967 spelling
    [urn:matrix:org.matrix.msc2967.client:api:*]. *)

val scope_device_prefix_unstable : string
(** [scope_device_prefix_unstable] is the MSC2967 prefix
    [urn:matrix:org.matrix.msc2967.client:device:]. *)

val scope_device : Matrix_proto.Id.Device_id.t -> string
(** [scope_device id] is [urn:matrix:client:device:<id>], the token asking the
    server to allocate [id] as this session's device. *)

val scope_device_unstable : Matrix_proto.Id.Device_id.t -> string
(** [scope_device_unstable id] is the MSC2967 device-allocation token for [id].
*)

val device_id_of_scope : string -> Matrix_proto.Id.Device_id.t option
(** [device_id_of_scope scope] is the device id named by the single stable or
    MSC2967 device token in the space-separated [scope]. It is [None] unless
    there is exactly one such token across both dialects. *)

val generate_device_id :
  random:Matrix_client.Random.t -> Matrix_proto.Id.Device_id.t
(** [generate_device_id ~random] is a fresh device id of ten characters drawn
    uniformly from [A-Z0-9]. *)

(** {1 Account management actions} *)

(** The type for the values of the account management URL's [action] parameter.
    It is {!Matrix_client.Oauth.type-account_action}. *)
type account_action = Matrix_client.Oauth.account_action =
  | Profile
  | Devices_list
  | Device_view
  | Device_delete
  | Account_deactivate
  | Cross_signing_reset
  | Other_action of string

val account_action_to_string : account_action -> string
(** [account_action_to_string a] is the wire name of [a]. *)

val account_action_of_string : string -> account_action
(** [account_action_of_string s] is the action [s] names. An unrecognised name
    is {!Other_action}. Stable and deployed MSC4191 aliases decode to the same
    typed constructors. *)

(** {1 Authorisation server metadata} *)

module Metadata : sig
  (** RFC 8414 authorisation server metadata, restricted to the fields the
      Matrix specification gives meaning to. *)

  type t = Matrix_client.Oauth.Metadata.t = {
    issuer : Uriz.t;
    authorization_endpoint : Uriz.t;
    token_endpoint : Uriz.t;
    registration_endpoint : Uriz.t option;
    revocation_endpoint : Uriz.t option;
    device_authorization_endpoint : Uriz.t option;
    account_management_uri : Uriz.t option;
    account_management_actions_supported : string list;
    response_types_supported : string list;
    grant_types_supported : string list;
    response_modes_supported : string list;
    code_challenge_methods_supported : string list;
    prompt_values_supported : string list;
    scopes_supported : string list;
  }
  (** The type for an authorisation server's metadata document. It is
      {!Matrix_client.Oauth.Metadata.t}, which documents the fields. *)

  val jsont : t Jsont.t
  (** [jsont] is the codec for the metadata document. *)

  val v1_path : string
  (** [v1_path] is ["/_matrix/client/v1/auth_metadata"], Matrix 1.15. *)

  val unstable_path : string
  (** [unstable_path] is
      ["/_matrix/client/unstable/org.matrix.msc2965/auth_metadata"], the name a
      server older than Matrix 1.15 serves. *)

  val openid_configuration_path : string
  (** [openid_configuration_path] is ["/.well-known/openid-configuration"],
      appended to the issuer named by the well-known [m.authentication] object.
  *)

  val fetch : ?http:_ Fetch.t -> Client.t -> t
  (** [fetch c] is {!Matrix_client.Oauth.Metadata.fetch} with the result
      unwrapped. A homeserver with no OAuth 2.0 API raises [Eio.Io]. [http]
      reaches an authorisation server off the homeserver's origin, and defaults
      to absent, which restricts discovery to the homeserver's own origin. The
      result is {b not} validated. Call {!validate}. *)

  val fetch_cached : ?http:_ Fetch.t -> Client.t -> t
  (** [fetch_cached c] unwraps {!Matrix_client.Oauth.Metadata.fetch_cached}.
      Successful metadata is reused for 24 hours; stale metadata is retained if
      its synchronous refresh fails. *)

  val invalidate_cache : Client.t -> unit
  (** [invalidate_cache c] clears only [c]'s cached OAuth metadata. *)

  val validate : ?allow_insecure:bool -> t -> unit
  (** [validate m] checks that [m] advertises everything the authorisation code
      grant with PKCE needs, including both query and fragment response modes,
      and that its issuer and endpoints use [https]. The first missing guarantee
      raises [Eio.Io] naming it. [allow_insecure=true] also accepts [http] for a
      deliberate local-development deployment. *)

  val validate_loopback : ?allow_insecure:bool -> t -> unit
  (** [validate_loopback m] checks the subset needed by the native loopback
      browser flow. It requires query response mode but not fragment response
      mode. Other authorisation-code, refresh-token, PKCE, and URL requirements
      are unchanged. *)

  val validate_device : ?allow_insecure:bool -> t -> unit
  (** [validate_device m] checks common URL invariants and requires the device-
      authorisation endpoint, RFC 8628 device-code grant advertisement, and
      Matrix-mandatory refresh-token support. It does not depend on browser
      response modes or PKCE. *)

  val supports_response_type : t -> string -> bool
  (** [supports_response_type m r] is [true] when [m] lists [r] in its supported
      response types. *)

  val supports_grant_type : t -> string -> bool
  (** [supports_grant_type m g] is [true] when [m] lists [g] in its supported
      grant types. *)

  val supports_response_mode : t -> string -> bool
  (** [supports_response_mode m r] is [true] when [m] lists [r] in its supported
      response modes. *)

  val supports_code_challenge_method : t -> string -> bool
  (** [supports_code_challenge_method m c] is [true] when [m] lists [c] in its
      supported code challenge methods. *)

  val supports_prompt : t -> string -> bool
  (** [supports_prompt m p] is [true] when [m] lists [p] in its supported prompt
      values. [supports_prompt m "create"] tells whether the server can be asked
      for a registration interface. *)

  val supports_account_action : t -> account_action -> bool
  (** [supports_account_action m a] is [true] when [m] lists [a] in its
      supported account management actions. *)

  val account_management_url :
    t ->
    ?action:account_action ->
    ?device_id:Matrix_proto.Id.Device_id.t ->
    unit ->
    Uriz.t option
  (** [account_management_url m ()] is the account management URI with [action]
      and, where the action names a device, [device_id] added as query
      parameters. It is [None] when the server advertises no such URL. [action]
      and [device_id] each default to absent, and an [action] is added whether
      or not the server lists it. *)
end

(** {1 Dynamic client registration} *)

module Registration : sig
  (** RFC 7591 dynamic client registration with the Matrix client-metadata
      profile. A Matrix client has no secret to keep, so it registers as a
      public client and the redirect URI is what proves its identity. *)

  type localized = Matrix_client.Oauth.Registration.localized = {
    value : string;
    translations : (string * string) list;
  }
  (** The type for a human-readable metadata value and its translations. It is
      {!Matrix_client.Oauth.Registration.localized}. *)

  val plain : string -> localized
  (** [plain v] is [v] with no translations. *)

  type client_metadata = Matrix_client.Oauth.Registration.client_metadata = {
    client_uri : string;
    client_name : localized option;
    logo_uri : localized option;
    tos_uri : localized option;
    policy_uri : localized option;
    contacts : string list;
    redirect_uris : string list;
    response_types : string list;
    grant_types : string list;
    token_endpoint_auth_method : string;
    application_type : string;
    sector_identifier_uri : string option;
    software_id : string option;
    software_version : string option;
    software_statement : string option;
    jwks_uri : string option;
    jwks : Jsont.json option;
  }
  (** The type for the client metadata document sent at registration. It is
      {!Matrix_client.Oauth.Registration.client_metadata}, which documents the
      fields. *)

  val v :
    ?client_name:localized ->
    ?logo_uri:localized ->
    ?tos_uri:localized ->
    ?policy_uri:localized ->
    ?contacts:string list ->
    ?response_types:string list ->
    ?grant_types:string list ->
    ?token_endpoint_auth_method:string ->
    ?application_type:string ->
    ?sector_identifier_uri:string ->
    ?software_id:string ->
    ?software_version:string ->
    ?software_statement:string ->
    ?jwks_uri:string ->
    ?jwks:Jsont.json ->
    client_uri:string ->
    redirect_uris:string list ->
    unit ->
    client_metadata
  (** [v ~client_uri ~redirect_uris ()] is client metadata. [client_name],
      [logo_uri], [tos_uri] and [policy_uri] default to absent and [contacts] to
      the empty list. The rest default to the Matrix profile, namely
      [response_types] of [["code"]], [grant_types] of
      [["authorization_code"; "refresh_token"]], [token_endpoint_auth_method] of
      ["none"] and [application_type] of ["native"]. The software, sector,
      statement and JWKS fields default to absent. [jwks] and [jwks_uri] are
      mutually exclusive, and [jwks] must be a JSON object. *)

  val loopback_redirect_uri : string
  (** [loopback_redirect_uri] is ["http://127.0.0.1/callback"], the redirect URI
      a command-line client registers for the loopback flow. It carries no port,
      which RFC 8252 section 7.3 requires, so the registered URI and the one
      {!Loopback.redirect_uri} builds differ by the ephemeral port. *)

  val to_json : client_metadata -> Jsont.json
  (** [to_json m] is the RFC 7591 client metadata document for [m]. *)

  type response = Matrix_client.Oauth.Registration.response = {
    client_id : string;
    client_id_issued_at : Ptime.t option;
    registered : (string * Jsont.json) list;
  }
  (** The type for a registration response. It is
      {!Matrix_client.Oauth.Registration.response}, which documents the fields.
  *)

  val register :
    ?http:_ Fetch.t -> Client.t -> Metadata.t -> client_metadata -> response
  (** [register c metadata cm] is {!Matrix_client.Oauth.Registration.register}
      with the result unwrapped, what the server answered with the [client_id]
      it assigned. A server with no registration endpoint raises [Eio.Io].
      [http] is as in {!Metadata.fetch}. *)
end

(** {1 PKCE} *)

module Pkce = Matrix_client.Oauth.Pkce
(** Proof Key for Code Exchange, RFC 7636, with the [S256] method. *)

(** {1 The authorisation code grant} *)

module Authorization : sig
  (** Building the URL to open in a browser and reading the code off the
      redirect. *)

  type request = Matrix_client.Oauth.Authorization.request = {
    url : Uriz.t;
    state : string;
    pkce : Pkce.t;
    device_id : Matrix_proto.Id.Device_id.t;
    redirect_uri : string;
    scope : string;
  }
  (** The type for an authorisation request in flight. It is
      {!Matrix_client.Oauth.Authorization.type-request}, which documents the
      fields. *)

  val build_url :
    ?scope:string list ->
    ?prompt:string ->
    ?login_hint:string ->
    ?response_mode:string ->
    Metadata.t ->
    client_id:string ->
    redirect_uri:string ->
    device_id:Matrix_proto.Id.Device_id.t ->
    state:string ->
    pkce:Pkce.t ->
    unit ->
    Uriz.t
  (** [build_url m ~client_id ~redirect_uri ~device_id ~state ~pkce ()] is the
      URL to open in the user's browser. [scope] defaults to full Client-Server
      API access plus the allocation of [device_id], and passing it replaces
      that list wholesale. [response_mode] defaults to ["query"], which is what
      a loopback listener can read. [prompt] and [login_hint] default to absent.
  *)

  val request :
    ?scope:string list ->
    ?prompt:string ->
    ?login_hint:string ->
    ?response_mode:string ->
    ?device_id:Matrix_proto.Id.Device_id.t ->
    ?state:string ->
    ?pkce:Pkce.t ->
    random:Matrix_client.Random.t ->
    Metadata.t ->
    client_id:string ->
    redirect_uri:string ->
    unit ->
    request
  (** [request ~random m ~client_id ~redirect_uri ()] is {!build_url} with the
      one-time values drawn from [random]. [device_id] defaults to a fresh
      {!generate_device_id}, [state] to sixteen random bytes and [pkce] to a
      fresh {!Pkce.create}. The result is kept until the redirect arrives. *)

  type redirect = Matrix_client.Oauth.Authorization.redirect = {
    code : string;
    state : string;
  }
  (** The type for the authorisation response carried by the redirect URI. *)

  (** The type for the reasons a redirect URI carried no authorisation code. It
      is {!Matrix_client.Oauth.Authorization.type-redirect_error}. *)
  type redirect_error = Matrix_client.Oauth.Authorization.redirect_error =
    | Denied of oauth_error
    | Malformed of string

  val pp_redirect_error : Format.formatter -> redirect_error -> unit
  (** [pp_redirect_error ppf e] prints why the redirect carried no code. *)

  val parse_redirect : Uriz.t -> redirect
  (** [parse_redirect uri] reads the authorisation code and state out of the URI
      the browser was redirected to. A server that refused raises [Eio.Io]
      carrying {!Denied}, and a redirect with no usable code one carrying
      {!Malformed_redirect}.

      {b The returned [state] is unverified.} Compare it against the one from
      {!val-request} before using the code. *)
end

(** {1 Tokens} *)

module Token : sig
  (** The token endpoint of RFC 6749 and the revocation endpoint of RFC 7009,
      plus the whoami call that turns a token into a session. *)

  type t = Matrix_client.Oauth.Token.t = {
    access_token : string;
    token_type : string;
    refresh_token : string option;
    scope : string option;
    expires_at : Ptime.t option;
  }
  (** The type for the token set an OAuth 2.0 grant yields. It is
      {!Matrix_client.Oauth.Token.t}, which documents the fields. *)

  type token_type_hint = Matrix_client.Oauth.Token.token_type_hint
  (** The type for the [token_type_hint] of RFC 7009 section 2.1. *)

  val jsont : t Jsont.t
  (** [jsont] is the codec for an RFC 6749 token response. Decoding turns the
      relative [expires_in] into [expires_at] against the system clock, so a
      value must be decoded when it is received. *)

  val is_expired : ?now:Ptime.t -> t -> bool
  (** [is_expired t] is [true] when [t]'s expiry is at or before [now], which
      defaults to the system clock. A token with no expiry is never expired. *)

  val exchange :
    ?http:_ Fetch.t ->
    Client.t ->
    Metadata.t ->
    client_id:string ->
    redirect_uri:string ->
    code:string ->
    pkce:Pkce.t ->
    unit ->
    t
  (** [exchange c metadata ~client_id ~redirect_uri ~code ~pkce ()] is
      {!Matrix_client.Oauth.Token.exchange} with the result unwrapped. It swaps
      the authorisation code for an access token, proving possession of the PKCE
      verifier. A code is single-use, and [redirect_uri] must be byte-identical
      to the one in the authorisation request. [http] is as in
      {!Metadata.fetch}. *)

  val refresh :
    ?http:_ Fetch.t ->
    Client.t ->
    Metadata.t ->
    client_id:string ->
    refresh_token:string ->
    unit ->
    t
  (** [refresh c metadata ~client_id ~refresh_token ()] is
      {!Matrix_client.Oauth.Token.refresh} with the result unwrapped, the new
      pair of tokens. A refresh token the server has already rotated away
      raises. *)

  val revoke :
    ?http:_ Fetch.t ->
    ?token_type_hint:token_type_hint ->
    Client.t ->
    Metadata.t ->
    client_id:string ->
    token:string ->
    unit ->
    unit
  (** [revoke c metadata ~client_id ~token ()] is
      {!Matrix_client.Oauth.Token.revoke} with the result unwrapped.
      [token_type_hint] defaults to absent and tells the server which kind of
      token it is. A server with no revocation endpoint raises. *)

  val logout :
    ?http:_ Fetch.t -> Client.t -> Metadata.t -> client_id:string -> t -> unit
  (** [logout c metadata ~client_id tokens] is
      {!Matrix_client.Oauth.Token.logout} with the result unwrapped. It revokes
      both of a session's tokens, which is what ends an OAuth session. *)

  val finish_login :
    Client.t ->
    device_id:Matrix_proto.Id.Device_id.t ->
    t ->
    Matrix_client.Client.session
  (** [finish_login c ~device_id tokens] is
      {!Matrix_client.Oauth.Token.finish_login} with the result unwrapped. It
      asks [/whoami] who the token belongs to, and the session it is still has
      to be installed with {!Client.with_session}. *)
end

(** {1 Automatic access-token refresh} *)

val with_auto_refresh :
  ?store:Matrix_client.Profile_store.t ->
  ?allow_insecure:bool ->
  ?on_session_update:
    (Matrix_client.Client.session -> (unit, Matrix_client.Error.t) result) ->
  ?on_session_invalid:(session_invalid_reason -> unit) ->
  Client.t ->
  client_id:string ->
  Client.t
(** [with_auto_refresh c ~client_id] opts [c] into automatic OAuth refresh. When
    an authenticated request receives [M_UNKNOWN_TOKEN], the client fetches
    fresh authorisation-server metadata without credentials, validates it,
    exchanges the session's current refresh token, and retries the request once
    with the new access token. The operation is opt-in and requires a session
    with a refresh token. [allow_insecure] permits HTTP metadata and token
    endpoints for local development. [on_session_update], when supplied, is
    handed each rotated session so callers can persist it. If the token endpoint
    returns OAuth [invalid_grant], [on_session_invalid] is called once by the
    refresh leader for that shared refresh attempt. Callback exceptions are
    isolated. Other refresh failures do not call it. With [store], the Eio
    client coordinates and persists refreshes across processes;
    [on_session_update] must only notify, not write credentials. Discovery and
    validation run before marking the token uncertain, so errors or cancellation
    during that preparation remain retryable. Failed or interrupted token
    exchanges require a new login; see
    {!Matrix_client.Profile_store.refresh_session_prepared}. Persist new login
    credentials with {!Matrix_client.Profile_store.save_login} to retire an old
    marker, including malformed data. *)

val with_auto_refresh_expiry :
  ?store:Matrix_client.Profile_store.t ->
  ?allow_insecure:bool ->
  ?on_session_update:
    (Matrix_client.Client.session ->
    Ptime.t option ->
    (unit, Matrix_client.Error.t) result) ->
  ?expires_at:Ptime.t ->
  ?early_refresh:Ptime.Span.t ->
  ?now:(unit -> Ptime.t) ->
  ?on_session_invalid:(session_invalid_reason -> unit) ->
  Client.t ->
  client_id:string ->
  Client.t
(** [with_auto_refresh_expiry c ~client_id] is the proactive-expiry variant of
    {!with_auto_refresh}. It carries the absolute expiry returned by the OAuth
    token endpoint through the refresh callback and refreshes before the
    configured deadline. With [store], discovery/validation failures remain
    retryable and exchange failures require a new login, as in
    {!with_auto_refresh}. *)

(** {1 RFC 8628 device authorisation} *)

module Device_authorization : sig
  type t = Matrix_client.Oauth.Device_authorization.t = {
    device_code : string;
    user_code : string;
    verification_uri : Uriz.t;
    verification_uri_complete : Uriz.t option;
    expires_in : int;
    interval : int;
  }
  (** The codes and expiry returned before the user completes authorisation. *)

  val jsont : t Jsont.t

  type poll_error = Matrix_client.Oauth.Device_authorization.poll_error =
    | Authorization_pending
    | Slow_down
    | Access_denied
    | Expired_token
    | OAuth_error of oauth_error
    | Transport_error of Matrix_client.Error.t

  val request :
    ?http:_ Fetch.t ->
    ?scope:string list ->
    Client.t ->
    Metadata.t ->
    client_id:string ->
    device_id:Matrix_proto.Id.Device_id.t ->
    unit ->
    t
  (** [request c metadata ~client_id ~device_id ()] posts the exact RFC 8628
      device-authorisation form. The default scope uses the same stable versus
      MSC2967 dialect selection as browser authorisation. *)

  val poll :
    ?http:_ Fetch.t ->
    Client.t ->
    Metadata.t ->
    client_id:string ->
    device_code:string ->
    unit ->
    (Token.t, poll_error) result
  (** [poll c metadata ~client_id ~device_code ()] performs one token poll;
      standard pending, slowdown, denial and expiry responses are classified. *)
end

(** {1 The loopback redirect listener}

    A native client cannot host an [https] page, so RFC 8252 section 7.3 has it
    listen on the loopback interface and register [http://127.0.0.1/...] without
    a port, leaving the authorisation server to accept whichever ephemeral port
    the client bound. *)

module Loopback : sig
  (** A listener that serves exactly the authorisation redirect.

      It serves one exact GET route through Proffer and is bound to the loopback
      interface, so nothing off the machine can reach it. Proffer owns request
      parsing, limits and response framing; malformed requests are answered and
      ignored until the valid callback arrives. *)

  type t
  (** The type for loopback listeners. *)

  val create :
    ?path:string ->
    sw:Eio.Switch.t ->
    env:< clock : _ Eio.Time.clock ; mono_clock : _ Eio.Time.Mono.t ; .. > ->
    [> [> `Generic ] Eio.Net.ty ] Eio.Resource.t ->
    t
  (** [create ~sw ~env net] is a listener bound on [127.0.0.1] and an ephemeral
      port. It is closed when [sw] finishes. [env]'s monotonic clock bounds
      request processing and measures server events. [path] is an absolute,
      non-empty route with no query, fragment, empty, or dot segments; it
      defaults to ["/callback"]. A request for any other path is answered 404
      and ignored. *)

  val port : t -> int
  (** [port t] is the ephemeral port [t] bound. *)

  val redirect_uri : t -> string
  (** [redirect_uri t] is the URI to put in the authorisation request. Unlike
      the registered {!Registration.loopback_redirect_uri} it carries the port
      that was actually bound. *)

  val wait : t -> Uriz.t
  (** [wait t] blocks until the browser follows the redirect, serves it a page
      saying the tab can be closed, and is the URI it asked for. A request for
      any other path is answered and ignored, so [wait] returns only once the
      real callback arrives.

      There is no timeout of its own. Bound the wait with
      [Eio.Time.with_timeout], or by cancelling the switch. *)
end

(** {1 The whole flow} *)

val default_client_metadata : Registration.client_metadata
(** [default_client_metadata] is the metadata of a command-line client,
    registered as a [native] public client with the port-less loopback redirect
    URI. It is a portable template; {!login_with_browser} specializes its
    [redirect_uris] to the exact ephemeral port already bound for that flow when
    dynamic registration uses the default metadata. *)

val default_device_client_metadata : Registration.client_metadata
(** [default_device_client_metadata] is the native public-client registration
    metadata used by {!login_with_device}; it additionally advertises the RFC
    8628 device-code grant. [default_client_metadata] remains unchanged for
    browser login. *)

val default_timeout : float
(** [default_timeout] is [300.], the time in seconds {!login_with_browser} waits
    for the browser redirect by default. *)

val browser_opener :
  sw:Eio.Switch.t ->
  env:< process_mgr : _ Eio.Process.mgr ; .. > ->
  string ->
  unit
(** [browser_opener ~sw ~env url] hands [url] to the desktop's URL handler,
    [xdg-open] or [open] on macOS, as a child process of [sw]. It is
    best-effort. Where no handler can be spawned it returns having done nothing
    but log, so a caller should print the URL as well. *)

type browser_login = {
  session : Matrix_client.Client.session;
  client_id : string;
}
(** The session and OAuth client identifier obtained by browser login. *)

type browser_login_with_expiry = {
  session : Matrix_client.Client.session;
  client_id : string;
  expires_at : Ptime.t option;
}
(** A browser login that also retains the access-token deadline supplied by the
    token endpoint. *)

val login_with_browser_full_expiry :
  env:
    < net : [> [> `Generic | `Unix ] Eio.Net.ty ] Eio.Resource.t
    ; clock : _ Eio.Time.clock
    ; mono_clock : _ Eio.Time.Mono.t
    ; secure_random : _ Eio.Flow.source
    ; .. > ->
  ?http:Fetch.plain ->
  ?client_id:string ->
  ?client_metadata:Registration.client_metadata ->
  ?prompt:string ->
  ?login_hint:string ->
  ?scope:string list ->
  ?path:string ->
  ?allow_insecure:bool ->
  ?timeout:float ->
  Client.t ->
  open_url:(string -> unit) ->
  unit ->
  browser_login_with_expiry
(** [login_with_browser_full_expiry] is {!login_with_browser_full} and also
    returns the absolute access-token expiry. *)

val login_with_browser_full :
  env:
    < net : [> [> `Generic | `Unix ] Eio.Net.ty ] Eio.Resource.t
    ; clock : _ Eio.Time.clock
    ; mono_clock : _ Eio.Time.Mono.t
    ; secure_random : _ Eio.Flow.source
    ; .. > ->
  ?http:Fetch.plain ->
  ?client_id:string ->
  ?client_metadata:Registration.client_metadata ->
  ?prompt:string ->
  ?login_hint:string ->
  ?scope:string list ->
  ?path:string ->
  ?allow_insecure:bool ->
  ?timeout:float ->
  Client.t ->
  open_url:(string -> unit) ->
  unit ->
  browser_login
(** [login_with_browser_full ~env c ~open_url ()] runs the authorisation code
    flow end to end and returns the session and client identifier it yields. It
    discovers and validates the metadata, binds a {!Loopback} listener, obtains
    a [client_id], builds the authorisation URL and hands it to [open_url],
    waits for the browser (for at most [timeout] seconds), checks the state and
    exchanges the code. The listener is released before it returns. [timeout]
    defaults to {!default_timeout} and is measured with [env]'s clock.

    The session is not installed on [c]. Pass it to {!Client.with_session} and
    persist it. Its refresh token is normally set, because OAuth access tokens
    expire, so a caller that keeps the session must be ready to call
    {!Token.refresh}.

    [http] reaches the authorisation server, which is off the homeserver's
    origin, and defaults to
    [Fetch_httpz.std ~retry:(Matrix_client.Http_retry.default
     ~homeserver:(Client.homeserver client)) env]. The POST retry exception
    remains scoped to the homeserver, not the OAuth issuer. Without [client_id]
    the client registers itself with [client_metadata], which defaults to
    {!default_client_metadata}; for this browser flow its redirect URI is
    specialized to the exact ephemeral port already bound, while caller-
    supplied metadata is used unchanged. [prompt] of ["create"] asks for the
    registration interface rather than the login one. [login_hint] suggests an
    account, which the Matrix profile spells ["mxid:@user:server"]. [scope]
    replaces the default scope list wholesale. [path] is the loopback path and
    defaults to ["/callback"]. [allow_insecure=true] permits metadata with
    [http] URLs for a deliberate local-development deployment; production
    callers should keep the secure default.

    A protocol failure raises [Eio.Io] carrying {!E}, which is
    {!State_mismatch}, {!Denied} when the user refuses, {!Not_registered}, or
    {!Timeout} when the browser does not return in time. A transport, HTTP or
    decoding failure raises one carrying {!Error.type-err}. *)

val login_with_browser :
  env:
    < net : [> [> `Generic | `Unix ] Eio.Net.ty ] Eio.Resource.t
    ; clock : _ Eio.Time.clock
    ; mono_clock : _ Eio.Time.Mono.t
    ; secure_random : _ Eio.Flow.source
    ; .. > ->
  ?http:Fetch.plain ->
  ?client_id:string ->
  ?client_metadata:Registration.client_metadata ->
  ?prompt:string ->
  ?login_hint:string ->
  ?scope:string list ->
  ?path:string ->
  ?allow_insecure:bool ->
  ?timeout:float ->
  Client.t ->
  open_url:(string -> unit) ->
  unit ->
  Matrix_client.Client.session
(** [login_with_browser] is the compatibility wrapper around
    {!login_with_browser_full} that returns only its session. *)

type device_login = browser_login
(** The session and OAuth client identifier obtained by device login. *)

type device_login_with_expiry = browser_login_with_expiry
(** A device login that also retains the access-token deadline. *)

val login_with_device_expiry :
  env:
    < net : [> [> `Generic | `Unix ] Eio.Net.ty ] Eio.Resource.t
    ; clock : _ Eio.Time.clock
    ; mono_clock : _ Eio.Time.Mono.t
    ; secure_random : _ Eio.Flow.source
    ; .. > ->
  ?http:Fetch.plain ->
  ?client_id:string ->
  ?client_metadata:Registration.client_metadata ->
  ?scope:string list ->
  ?device_id:Matrix_proto.Id.Device_id.t ->
  ?allow_insecure:bool ->
  ?timeout:float ->
  Client.t ->
  show:(Device_authorization.t -> unit) ->
  unit ->
  device_login_with_expiry
(** [login_with_device_expiry] is {!login_with_device} and also returns the
    absolute access-token expiry. *)

val login_with_device :
  env:
    < net : [> [> `Generic | `Unix ] Eio.Net.ty ] Eio.Resource.t
    ; clock : _ Eio.Time.clock
    ; mono_clock : _ Eio.Time.Mono.t
    ; secure_random : _ Eio.Flow.source
    ; .. > ->
  ?http:Fetch.plain ->
  ?client_id:string ->
  ?client_metadata:Registration.client_metadata ->
  ?scope:string list ->
  ?device_id:Matrix_proto.Id.Device_id.t ->
  ?allow_insecure:bool ->
  ?timeout:float ->
  Client.t ->
  show:(Device_authorization.t -> unit) ->
  unit ->
  device_login
(** [login_with_device ~env c ~show ()] registers when necessary, obtains and
    shows the user and verification codes through [show], then polls after the
    advertised initial interval until login succeeds. The absolute deadline is
    the shorter of the response expiry and [timeout], if supplied; pending polls
    retain their interval and each [slow_down] adds five seconds. The granted
    device scope overrides the requested device id. Cancellation is propagated,
    while denial, expiry and timeout raise {!E}. [timeout], when supplied, must
    be finite and positive. *)
