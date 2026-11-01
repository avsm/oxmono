(** oauth — the OAuth 2.0 authorisation code grant, Matrix 1.15.

    The homeserver delegates authentication to an authorisation server. The user
    authorises the client in a browser and the client receives an access token
    and a refresh token. Nothing here opens a browser or listens for the
    redirect. A caller does both and hands the redirect URI to
    {!Authorization.parse_redirect}.

    The flow, in order.

    + {!Metadata.fetch} discovers the authorisation server's endpoints.
    + {!Registration.register} obtains a [client_id], unless one is configured
      out of band.
    + {!Pkce.create} and {!Authorization.val-request} build the URL to open. It
      carries a PKCE challenge and a scope naming the device id wanted.
    + {!Authorization.parse_redirect} reads the [code] off the redirect URI.
    + {!Token.exchange} swaps the code for tokens.
    + {!Token.finish_login} asks [/account/whoami] who they belong to and builds
      a {!Client.type-session}.

    {!Token.refresh} renews an expired access token. {!Token.revoke} and
    {!Token.logout} end the session.

    {2:requests Where the requests go}

    Only {!Metadata.fetch}'s first two attempts are Client-Server endpoints on
    the homeserver. Registration, token and revocation requests go to the
    authorisation server, which is usually a different origin, whereas a
    {!Client.t} is restricted to the homeserver's by {!Client.create}. Every
    call that may leave that origin therefore takes an optional [?http].

    Given [http], the request goes through it, against the absolute URL from the
    metadata, carrying no Matrix credentials. Omitted, the URL must share the
    homeserver's origin, and an off-origin one is refused with
    {!Error.Policy_denied} rather than escaping the restriction silently.

    {2 Errors}

    The authorisation server's endpoints answer failures in the OAuth 2.0 shape
    of RFC 6749 section 5.2, not the Matrix
    {{!Error.matrix_error}standard error response}. Such a failure is an
    {!Error.Http_error} carrying the body verbatim, which
    {!oauth_error_of_error} parses.

    @see <https://spec.matrix.org/v1.15/client-server-api/#oauth-20-api>
      Client-Server API, "OAuth 2.0 API"
    @see <https://datatracker.ietf.org/doc/html/rfc6749> RFC 6749, OAuth 2.0
    @see <https://datatracker.ietf.org/doc/html/rfc7591>
      RFC 7591, Dynamic Client Registration
    @see <https://datatracker.ietf.org/doc/html/rfc7636> RFC 7636, PKCE
    @see <https://datatracker.ietf.org/doc/html/rfc7009>
      RFC 7009, Token Revocation
    @see <https://datatracker.ietf.org/doc/html/rfc8414>
      RFC 8414, Authorization Server Metadata *)

(** {1 OAuth-shaped errors} *)

type oauth_error = {
  error : string;  (** The error code, such as ["invalid_grant"]. *)
  error_description : string option;  (** Human-readable detail. *)
  error_uri : string option;  (** Where a human can read more. *)
}
(** The type for error responses as defined by
    {{:https://datatracker.ietf.org/doc/html/rfc6749#section-5.2} RFC 6749
     section 5.2}. The token, registration and revocation endpoints answer with
    one, and so does a failed authorisation redirect. *)

val pp_oauth_error : Format.formatter -> oauth_error -> unit
(** [pp_oauth_error ppf e] prints the code of [e] and, when present, its
    description. *)

val oauth_error_of_json : string -> oauth_error option
(** [oauth_error_of_json body] is the RFC 6749 error [body] carries. It is
    [None] when [body] is not one. *)

val oauth_error_of_error : Error.t -> oauth_error option
(** [oauth_error_of_error e] is the OAuth error inside [e]. It reads the
    {!Error.Http_error} values the authorisation server's endpoints return. *)

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
    server to allocate [id] as this session's device.

    @see <https://spec.matrix.org/v1.15/client-server-api/#device-id-allocation>
      Device ID allocation *)

val scope_device_unstable : Matrix_proto.Id.Device_id.t -> string
(** [scope_device_unstable id] is the MSC2967 device-allocation token for [id].
*)

val device_id_of_scope : string -> Matrix_proto.Id.Device_id.t option
(** [device_id_of_scope scope] is the device id named by the single stable or
    MSC2967 device token in the space-separated [scope]. It is [None] unless
    there is exactly one such token across both dialects, which is what a login
    request is required to carry. *)

val generate_device_id : random:Random.t -> Matrix_proto.Id.Device_id.t
(** [generate_device_id ~random] is a fresh device id of ten characters drawn
    uniformly from [A-Z0-9] using [random]. The specification asks for a random
    string of unreserved characters and calls ten enough to be unique per user.
*)

(** {1 Account management actions} *)

(** The type for the values of the account management URL's [action] parameter.
    A server advertises which it honours in
    {!Metadata.t.account_management_actions_supported}. *)
type account_action =
  | Profile  (** [org.matrix.profile], viewing or editing the profile. *)
  | Devices_list  (** [org.matrix.devices_list], listing the user's devices. *)
  | Device_view  (** [org.matrix.device_view], inspecting one device. *)
  | Device_delete  (** [org.matrix.device_delete], signing one device out. *)
  | Account_deactivate  (** [org.matrix.account_deactivate]. *)
  | Cross_signing_reset  (** [org.matrix.cross_signing_reset]. *)
  | Other_action of string  (** Any action this module does not model. *)

val account_action_to_string : account_action -> string
(** [account_action_to_string a] is the wire name of [a]. *)

val account_action_of_string : string -> account_action
(** [account_action_of_string s] is the action [s] names. An unrecognised name
    is {!Other_action}. Stable and deployed MSC4191 action aliases decode to the
    same typed constructors. *)

(** {1 Authorisation server metadata} *)

module Metadata : sig
  (** RFC 8414 authorisation server metadata, restricted to the fields the
      Matrix specification gives meaning to.

      @see <https://spec.matrix.org/v1.15/client-server-api/#server-metadata-discovery>
        Server metadata discovery *)

  type t = {
    issuer : Uriz.t;
        (** The authorisation server's issuer identifier, an [https] URL with no
            query or fragment. *)
    authorization_endpoint : Uriz.t;  (** Where the browser is sent. *)
    token_endpoint : Uriz.t;  (** Where codes and refresh tokens are spent. *)
    registration_endpoint : Uriz.t option;
        (** RFC 7591 dynamic client registration. *)
    revocation_endpoint : Uriz.t option;  (** RFC 7009 token revocation. *)
    device_authorization_endpoint : Uriz.t option;
        (** RFC 8628 device authorisation grant, Matrix 1.18. It is discovered
            and consumed by {!Device_authorization} and the Eio flow. *)
    account_management_uri : Uriz.t option;
        (** The homeserver's account-management web interface, Matrix 1.18. *)
    account_management_actions_supported : string list;
        (** The [action] values {!account_management_uri} honours. *)
    response_types_supported : string list;
        (** Contains ["code"] for the authorisation code grant. *)
    grant_types_supported : string list;
        (** Contains ["authorization_code"] and ["refresh_token"]. *)
    response_modes_supported : string list;
        (** Contains ["query"] and ["fragment"]. *)
    code_challenge_methods_supported : string list;  (** Contains ["S256"]. *)
    prompt_values_supported : string list;
        (** OpenID Connect prompt values. ["create"] means the server can show a
            registration interface. *)
    scopes_supported : string list;
        (** The scopes an OpenID Connect discovery document offers. The Matrix
            profile does not require it. *)
  }
  (** The type for authorisation server metadata. *)

  val jsont : t Jsont.t
  (** [jsont] is the codec for the metadata document. Unknown members are
      ignored, as RFC 8414 asks of clients. Every endpoint member must be an
      absolute URL. *)

  val v1_path : string
  (** [v1_path] is ["/_matrix/client/v1/auth_metadata"], Matrix 1.15. *)

  val unstable_path : string
  (** [unstable_path] is
      ["/_matrix/client/unstable/org.matrix.msc2965/auth_metadata"], the MSC2965
      name a server older than Matrix 1.15 serves. *)

  val openid_configuration_path : string
  (** [openid_configuration_path] is ["/.well-known/openid-configuration"],
      appended to the issuer named by the well-known [m.authentication] object.
  *)

  val fetch :
    ?http:_ Fetch.t -> ?now:(unit -> float) -> Client.t -> (t, Error.t) result
  (** [fetch client] discovers the authorisation server metadata, trying in
      order:

      + [GET /_matrix/client/v1/auth_metadata] on the homeserver;
      + [GET /_matrix/client/unstable/org.matrix.msc2965/auth_metadata];
      + [GET /.well-known/matrix/client] through {!Server.get_well_known} for an
        issuer, then [GET <issuer>/.well-known/openid-configuration].

      A 404, or [M_UNRECOGNIZED], moves on to the next candidate. Any other
      failure is returned as is. When every candidate is exhausted the result is
      {!Error.Http_error} with status 404.

      The last step leaves the homeserver, since the issuer is a different
      origin in most deployments and {!Client.t} is origin-restricted. [http]
      reaches it, as {!section-requests} describes. Without [http] the step is
      attempted only when the issuer shares the homeserver's origin, and
      otherwise fails with {!Error.Policy_denied}.

      The result is {b not} validated. Call {!validate}. *)

  val fetch_cached :
    ?http:_ Fetch.t -> ?now:(unit -> float) -> Client.t -> (t, Error.t) result
  (** [fetch_cached client] returns successful discovery metadata cached by this
      client for the response's [Cache-Control: max-age], capped at 24 hours, or
      [Expires] when no explicit Cache-Control lifetime is present. The fallback
      is 24 hours when neither header supplies a usable lifetime. [no-cache]
      causes an immediate revalidation and [no-store] leaves no cached entry. A
      malformed or negative [max-age] is treated as immediate expiry. A miss
      performs {!fetch}. An expired entry is refreshed synchronously; if that
      refresh fails, the stale valid entry is returned. Cache age uses the
      monotonic [now] clock (which defaults to the process monotonic clock), so
      wall-clock adjustments cannot extend or prematurely expire an entry. The
      Rust SDK performs that refresh in the background, but this result-level
      API has no scheduler whose lifetime could own it.

      The cache is scoped to the exact {!Client.t}; deriving a client with new
      credentials starts with an empty cache. *)

  val invalidate_cache : Client.t -> unit
  (** [invalidate_cache client] removes its cached OAuth metadata. It does not
      affect another client or the versions/capabilities caches. *)

  val validate : ?allow_insecure:bool -> t -> (unit, Error.t) result
  (** [validate m] checks the Matrix server-metadata invariants used by the
      authorisation code grant. The issuer and every advertised endpoint must
      use [https], and the issuer must have neither a query nor a fragment.
      ["code"] in {!t.response_types_supported}, ["authorization_code"] and
      ["refresh_token"] in {!t.grant_types_supported}, both ["query"] and
      ["fragment"] in {!t.response_modes_supported}, and ["S256"] in
      {!t.code_challenge_methods_supported} are also required. A missing
      guarantee is {!Error.Json_error} naming what is absent.

      [allow_insecure] defaults to [false]. Setting it accepts [http] URLs for a
      deliberately insecure local development server; it does not relax any
      other validation. *)

  val validate_loopback : ?allow_insecure:bool -> t -> (unit, Error.t) result
  (** [validate_loopback m] checks the subset of {!validate} needed by the
      high-level native loopback browser flow. It requires ["query"], because
      the local HTTP listener cannot receive a URI fragment, but does not
      require ["fragment"]. All URL, grant, response-type, refresh-token, and
      PKCE checks from {!validate} still apply. *)

  val validate_device : ?allow_insecure:bool -> t -> (unit, Error.t) result
  (** [validate_device m] checks the common URL invariants, then requires the
      RFC 8628 [device_authorization_endpoint], the
      [urn:ietf:params:oauth:grant-type:device_code] grant, and the Matrix-
      mandatory [refresh_token] grant. It does not require authorization-code
      response modes or PKCE, which the device flow does not use. *)

  val supports_response_type : t -> string -> bool
  (** [supports_response_type m r] is [true] when [m] lists [r] in
      {!t.response_types_supported}. *)

  val supports_grant_type : t -> string -> bool
  (** [supports_grant_type m g] is [true] when [m] lists [g] in
      {!t.grant_types_supported}. *)

  val supports_response_mode : t -> string -> bool
  (** [supports_response_mode m r] is [true] when [m] lists [r] in
      {!t.response_modes_supported}. *)

  val supports_code_challenge_method : t -> string -> bool
  (** [supports_code_challenge_method m c] is [true] when [m] lists [c] in
      {!t.code_challenge_methods_supported}. *)

  val supports_prompt : t -> string -> bool
  (** [supports_prompt m p] is [true] when [m] lists [p] in
      {!t.prompt_values_supported}. [supports_prompt m "create"] tells whether
      the server can be asked for a registration interface. *)

  val supports_account_action : t -> account_action -> bool
  (** [supports_account_action m a] is [true] when [m] lists [a] in
      {!t.account_management_actions_supported}, including the deployed MSC4191
      aliases for device listing/view/end and account deactivation. *)

  val account_management_url :
    t ->
    ?action:account_action ->
    ?device_id:Matrix_proto.Id.Device_id.t ->
    unit ->
    Uriz.t option
  (** [account_management_url m ()] is {!t.account_management_uri} with [action]
      and, where the action names a device, [device_id] added as query
      parameters. It is [None] when the server advertises no account-management
      URL. [action] defaults to absent, which yields the bare URL, and
      [device_id] defaults to absent. For a typed action, the canonical stable
      spelling is used when advertised; otherwise a deployed MSC4191 alias is
      used when advertised.

      The action is added whether or not the server lists it in
      {!t.account_management_actions_supported}. Call {!supports_account_action}
      first to fall back to the bare URL instead.

      @see <https://spec.matrix.org/v1.15/client-server-api/#oauth-20-account-management>
        Account management *)
end

(** {1 Dynamic client registration} *)

module Registration : sig
  (** RFC 7591 dynamic client registration with the Matrix client-metadata
      profile.

      A Matrix client has no secret to keep, so it registers as a public client.
      [token_endpoint_auth_method] is ["none"] and the redirect URI is what
      proves its identity. The specification asks clients to re-register at the
      start of each authorisation flow. A deployment that provisions a
      [client_id] out of band skips this module and passes that id to
      {!Authorization.build_url}.

      @see <https://spec.matrix.org/v1.15/client-server-api/#client-registration>
        Client registration *)

  type localized = { value : string; translations : (string * string) list }
  (** The type for a human-readable metadata value and its translations. Each
      [(tag, translation)] pair is written as [<field>#<tag>], so
      [("fr", "Mon application")] becomes ["client_name#fr"]. *)

  val plain : string -> localized
  (** [plain v] is [v] with no translations. *)

  type client_metadata = {
    client_uri : string;
        (** An [https] page describing the client. Every other URI in the
            metadata must be on this host or a subdomain of it. *)
    client_name : localized option;  (** The name shown to the user. *)
    logo_uri : localized option;  (** An image representing the client. *)
    tos_uri : localized option;  (** The client's terms of service. *)
    policy_uri : localized option;  (** The client's privacy policy. *)
    contacts : string list;  (** Maintainer email addresses. *)
    redirect_uris : string list;
        (** Where the authorisation server may send the browser back. The
            authorisation code grant needs at least one. *)
    response_types : string list;  (** Includes ["code"]. *)
    grant_types : string list;
        (** Includes ["authorization_code"] and ["refresh_token"]. *)
    token_endpoint_auth_method : string;  (** ["none"] for a public client. *)
    application_type : string;
        (** ["native"] or ["web"]. It decides how the server validates the
            redirect URIs. *)
    sector_identifier_uri : string option;
        (** Optional URI grouping clients into a sector for pairwise subject
            identifiers. *)
    software_id : string option;
        (** Optional identifier for the client software. *)
    software_version : string option;
        (** Optional version of the client software. *)
    software_statement : string option;
        (** Optional JWT software statement from the authorisation server. *)
    jwks_uri : string option;
        (** Optional URI serving the client's JSON Web Key Set. Mutually
            exclusive with {!jwks}. *)
    jwks : Jsont.json option;
        (** Optional inline JSON Web Key Set object. Mutually exclusive with
            {!jwks_uri}. *)
  }
  (** The type for the client metadata document sent at registration. *)

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
      the empty list. The rest default to the Matrix profile. [response_types]
      to [["code"]], [grant_types] to [["authorization_code"; "refresh_token"]],
      [token_endpoint_auth_method] to ["none"] and [application_type] to
      ["native"]. The software, sector, statement and JWKS fields default to
      absent. [jwks] and [jwks_uri] are mutually exclusive, and [jwks] must be a
      JSON object. *)

  val loopback_redirect_uri : string
  (** [loopback_redirect_uri] is ["http://127.0.0.1/callback"], the redirect URI
      a command-line client registers for the loopback flow.

      It carries no port. RFC 8252 section 7.3, which the Matrix specification
      adopts, forbids a port in a registered loopback redirect URI and requires
      the server to accept any port at authorisation time. The URI registered
      here and the one passed to {!Authorization.build_url} therefore differ by
      the ephemeral port the listener bound. *)

  val to_json : client_metadata -> Jsont.json
  (** [to_json m] is the RFC 7591 client metadata document for [m], with the
      localised fields expanded into their [#tag] members. *)

  type response = {
    client_id : string;  (** The allocated client identifier. *)
    client_id_issued_at : Ptime.t option;  (** When the id was issued. *)
    registered : (string * Jsont.json) list;
        (** Every other member of the response, sorted by name. The server
            echoes the metadata it accepted, so a value that was sent and is
            missing here was not registered. *)
  }
  (** The type for a registration response. *)

  val register :
    ?http:_ Fetch.t ->
    Client.t ->
    Metadata.t ->
    client_metadata ->
    (response, Error.t) result
  (** [register client m cm] is [POST <registration_endpoint>] with [cm] as its
      JSON body. [http] reaches the authorisation server, as {!section-requests}
      describes.

      Fails with {!Error.Json_error} when [m] advertises no registration
      endpoint. *)
end

(** {1 PKCE} *)

module Pkce : sig
  (** Proof Key for Code Exchange, RFC 7636, with the [S256] method, the only
      one the Matrix specification allows.

      The verifier is kept by the client and sent only with the token request.
      The challenge is its SHA-256, base64url-encoded without padding, and
      travels in the authorisation URL. A code intercepted in the redirect is
      useless without the verifier. *)

  type t = private { verifier : string; challenge : string }
  (** The type for a code verifier and the challenge derived from it. *)

  val challenge_method : string
  (** [challenge_method] is ["S256"]. *)

  val create : ?bytes:int -> random:Random.t -> unit -> t
  (** [create ~random ()] is a fresh verifier and its challenge. The verifier is
      the base64url-no-padding encoding of [bytes] bytes drawn from [random],
      which yields 43 characters of the unreserved alphabet, the shortest length
      RFC 7636 section 4.1 permits. [bytes] defaults to 32.

      Raises [Invalid_argument] when [bytes] is outside [32 .. 96], the range
      whose encodings fall within the required 43 to 128 characters. *)

  val of_verifier : string -> (t, Error.t) result
  (** [of_verifier v] recomputes the challenge for an existing verifier, for a
      flow whose two halves are in different processes. [v] must be 43 to 128
      characters of [A-Za-z0-9-._~]. Anything else is {!Error.Json_error}. *)
end

(** {1 The authorisation request} *)

module Authorization : sig
  (** Building the URL to open in a browser and reading the code off the
      redirect.

      @see <https://spec.matrix.org/v1.15/client-server-api/#authorisation-code-flow>
        Authorisation code flow *)

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
      URL to open in the user's browser.

      [scope] defaults to [[scope_api; scope_device device_id]], full
      Client-Server API access plus the allocation of [device_id], unless
      metadata advertises a complete MSC2967 API/device pair, in which case the
      matching unstable pair is used. Stable is preferred when both pairs are
      advertised, and incomplete or absent metadata retains the stable default.
      Passing [scope] replaces that list wholesale, so a caller adding a scope
      must include the desired API and device tokens itself. A login request
      carries exactly one device token from one dialect.

      [response_mode] defaults to ["query"], which is what a loopback listener
      can read. A client whose redirect URI is a remote [https] page should pass
      ["fragment"] so the code never reaches that web server.

      [prompt] defaults to absent and is ["create"] to ask for the registration
      interface. See {!Metadata.supports_prompt}. [login_hint] defaults to
      absent and suggests an account to the server, which the Matrix profile
      writes as a full user id. *)

  type request = {
    url : Uriz.t;  (** What to open in the browser. *)
    state : string;  (** What the redirect is matched against. *)
    pkce : Pkce.t;  (** What {!Token.exchange} spends. *)
    device_id : Matrix_proto.Id.Device_id.t;  (** The id being claimed. *)
    redirect_uri : string;  (** What {!Token.exchange} repeats. *)
    scope : string;  (** The space-separated scope that was asked for. *)
  }
  (** The type for an authorisation request in flight. It holds the one-time
      values the redirect and the token exchange need. *)

  val request :
    ?scope:string list ->
    ?prompt:string ->
    ?login_hint:string ->
    ?response_mode:string ->
    ?device_id:Matrix_proto.Id.Device_id.t ->
    ?state:string ->
    ?pkce:Pkce.t ->
    random:Random.t ->
    Metadata.t ->
    client_id:string ->
    redirect_uri:string ->
    unit ->
    request
  (** [request ~random m ~client_id ~redirect_uri ()] is {!build_url} with the
      one-time values drawn from [random]. [device_id] defaults to a fresh
      {!generate_device_id}, [state] to sixteen random bytes and [pkce] to a
      fresh {!Pkce.create}. [scope], [prompt], [login_hint] and [response_mode]
      are as in {!build_url}. The result is kept until the redirect arrives. *)

  type redirect = { code : string; state : string }
  (** The type for the authorisation response carried by the redirect URI. *)

  (** The type for the reasons a redirect URI carried no authorisation code. *)
  type redirect_error =
    | Denied of oauth_error
        (** The authorisation server reported a failure. [access_denied] is the
            user refusing consent. *)
    | Malformed of string
        (** No [code], no [state], or a duplicated response parameter. *)

  val pp_redirect_error : Format.formatter -> redirect_error -> unit
  (** [pp_redirect_error ppf e] prints why the redirect carried no code. *)

  val parse_redirect : Uriz.t -> (redirect, redirect_error) result
    @@ portable
  (** [parse_redirect uri] reads the authorisation response off the URI the
      browser was sent to.

      Parameters are taken from the query string and from the fragment, which is
      parsed as though it were one, covering both [response_mode] values.
      Duplicate response parameters, including one copy in each location, are
      rejected rather than choosing an attacker-controlled value.

      An [error] parameter yields {!Denied}, carrying [error_description] and
      [error_uri] when the server sent them.

      {b The returned [state] is unverified.} Compare it against the one from
      {!val-request} before using the code. *)
end

(** {1 Tokens} *)

module Token : sig
  (** The token endpoint of RFC 6749 and the revocation endpoint of RFC 7009,
      plus the whoami call that turns a token into a session. *)

  type t = {
    access_token : string;  (** The credential every request carries. *)
    token_type : string;  (** ["Bearer"] in practice. *)
    refresh_token : string option;
        (** What {!refresh} spends for a new access token. *)
    scope : string option;
        (** What was granted, which may be narrower than what was asked for.
            {!device_id_of_scope} reads the device id back out. *)
    expires_at : Ptime.t option;
        (** When the access token stops being accepted. It is [None] when the
            server named no lifetime. *)
  }
  (** The type for the token set an OAuth 2.0 grant yields. *)

  val jsont : t Jsont.t
  (** [jsont] is the codec for an RFC 6749 token response. Decoding turns the
      relative [expires_in] into {!t.expires_at} by adding it to the system
      clock, so a value must be decoded when it is received. Encoding writes
      [expires_in] back as the seconds remaining, and zero once {!t.expires_at}
      has passed. *)

  val is_expired : ?now:Ptime.t -> t -> bool
  (** [is_expired t] is [true] when {!t.expires_at} is at or before [now], which
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
    (t, Error.t) result
  (** [exchange client m ~client_id ~redirect_uri ~code ~pkce ()] is
      [POST <token_endpoint>] with an [application/x-www-form-urlencoded] body
      binding [grant_type=authorization_code], [code], [redirect_uri],
      [client_id] and [code_verifier].

      [redirect_uri] must be byte-identical to the one in the authorisation
      request. [http] reaches the authorisation server, as {!section-requests}
      describes. *)

  val refresh :
    ?http:_ Fetch.t ->
    Client.t ->
    Metadata.t ->
    client_id:string ->
    refresh_token:string ->
    unit ->
    (t, Error.t) result
  (** [refresh client m ~client_id ~refresh_token ()] is the refresh token
      grant, [grant_type=refresh_token] at the token endpoint. The reply carries
      a new access token and normally a new refresh token, which replaces the
      one just spent. [http] is as in {!exchange}.

      @see <https://spec.matrix.org/v1.15/client-server-api/#token-refresh-flow>
        Token refresh flow *)

  type token_type_hint = [ `Access_token | `Refresh_token ]
  (** The type for the [token_type_hint] of RFC 7009 section 2.1. *)

  val revoke :
    ?http:_ Fetch.t ->
    ?token_type_hint:token_type_hint ->
    Client.t ->
    Metadata.t ->
    client_id:string ->
    token:string ->
    unit ->
    (unit, Error.t) result
  (** [revoke client m ~client_id ~token ()] is [POST <revocation_endpoint>]
      with [token] and [client_id] form-encoded. [token_type_hint] defaults to
      absent and tells the server which kind of token it is. [http] is as in
      {!exchange}.

      The server revokes both the access and the refresh token of the session
      the given token belongs to, and answers 200 even for a token that was
      already invalid.

      Fails with {!Error.Json_error} when [m] advertises no revocation endpoint.
  *)

  val logout :
    ?http:_ Fetch.t ->
    Client.t ->
    Metadata.t ->
    client_id:string ->
    t ->
    (unit, Error.t) result
  (** [logout client m ~client_id t] revokes the access token of [t] and, if
      there is one, its refresh token. Revoking either ends the session. Both
      are sent so that a server which does not link them forgets both. The first
      failure is returned. [http] is as in {!exchange}. *)

  val finish_login :
    Client.t ->
    device_id:Matrix_proto.Id.Device_id.t ->
    t ->
    (Client.session, Error.t) result
  (** [finish_login client ~device_id t] calls
      [GET /_matrix/client/v3/account/whoami] with the access token of [t] and
      is the {!Client.type-session} to hand to {!Client.with_session} or to
      persist. The OAuth API has no login response to read a user id from.

      [device_id] is the one the client allocated in the scope rather than
      something the server returns. Prefer [device_id_of_scope] over {!t.scope}
      when the server echoed a scope, and fall back to the requested id.

      @see <https://spec.matrix.org/v1.15/client-server-api/#get_matrixclientv3accountwhoami>
        [/account/whoami] *)
end

(** {1 RFC 8628 device authorisation} *)

module Device_authorization : sig
  type t = {
    device_code : string;
    user_code : string;
    verification_uri : Uriz.t;
    verification_uri_complete : Uriz.t option;
    expires_in : int;
    interval : int;
  }
  (** The codes returned before the user completes authorisation on another
      device. *)

  val jsont : t Jsont.t
  (** [jsont] validates the RFC 8628 response. Codes are non-empty, both
      verification URIs are absolute, [expires_in] is positive, and [interval]
      defaults to five seconds and must be positive when present. *)

  type poll_error =
    | Authorization_pending
    | Slow_down
    | Access_denied
    | Expired_token
    | OAuth_error of oauth_error
    | Transport_error of Error.t
        (** The result of a device-code poll that did not return tokens. Unknown
            OAuth errors and transport/HTTP errors retain their original
            details. *)

  val request :
    ?http:_ Fetch.t ->
    ?scope:string list ->
    Client.t ->
    Metadata.t ->
    client_id:string ->
    device_id:Matrix_proto.Id.Device_id.t ->
    unit ->
    (t, Error.t) result
  (** [request client m ~client_id ~device_id ()] posts [client_id] and the
      dialect-compatible default scope to [device_authorization_endpoint].
      Passing [scope] replaces the default scope list. *)

  val poll :
    ?http:_ Fetch.t ->
    Client.t ->
    Metadata.t ->
    client_id:string ->
    device_code:string ->
    unit ->
    (Token.t, poll_error) result
  (** [poll client m ~client_id ~device_code ()] makes one RFC 8628 token poll.
      [Authorization_pending], [Slow_down], [Access_denied] and [Expired_token]
      classify the standard OAuth errors. *)
end
