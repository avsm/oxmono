(** auth — logging in, logging out and registering.

    {!Matrix_client.Auth} documents the endpoints and the types re-exported
    here. Every call below raises [Eio.Io] with [Error.E e] where its
    counterpart returns [Error e'], [e] being {!Error.of_client_error} of [e'].
    The calls that authenticate return the client with the session already
    attached, so there is no {!Client.with_session} to remember. *)

(** {1 Login flows} *)

(** How a homeserver will accept a login. *)
type login_flow = Matrix_client.Auth.login_flow =
  | Password  (** ["m.login.password"]. *)
  | Token  (** ["m.login.token"]. *)
  | Sso  (** ["m.login.sso"]. *)
  | Unknown of string  (** A flow type this library does not know. *)

val get_login_flows : Client.t -> login_flow list
(** [get_login_flows client] is the ways this homeserver will accept a login.
    Raises [Eio.Io] with [Error.E e] on failure. *)

(** {1 Logging in} *)

type login_params = Matrix_client.Auth.login_params = {
  device_id : string option;
      (** Reuse an existing device rather than have the server mint one. Reusing
          it invalidates that device's previous access token. *)
  initial_device_display_name : string option;
      (** Shown to the user in the device list, for a device the server is about
          to create. *)
}
(** Which device a login or a registration claims. *)

val default_login_params : login_params
(** [default_login_params] has both fields absent, so the server picks a device
    id and leaves it unnamed. *)

val login_password_with_expiry :
  Client.t ->
  user:string ->
  password:string ->
  ?params:login_params ->
  ?request_refresh_token:bool ->
  unit ->
  Client.t * Ptime.t option
(** [login_password_with_expiry] is {!login_password} and also returns the
    optional absolute access-token expiry. *)

val login_password :
  Client.t ->
  user:string ->
  password:string ->
  ?params:login_params ->
  ?request_refresh_token:bool ->
  unit ->
  Client.t
(** [login_password client ~user ~password ()] logs in with ["m.login.password"]
    and is [client] with the new session attached. [user] may be a localpart or
    a full [@user:server]. [params] defaults to {!default_login_params}.

    Raises [Eio.Io] with [Error.E e] on failure. Wrong credentials arrive as
    [Error.Matrix] with [M_FORBIDDEN]. [request_refresh_token=true] asks for a
    refresh token and is omitted by default. *)

val login_token_with_expiry :
  Client.t ->
  token:string ->
  ?params:login_params ->
  ?request_refresh_token:bool ->
  unit ->
  Client.t * Ptime.t option
(** [login_token_with_expiry] is {!val-login_token} and also returns the
    optional absolute access-token expiry. *)

val login_token :
  Client.t ->
  token:string ->
  ?params:login_params ->
  ?request_refresh_token:bool ->
  unit ->
  Client.t
(** [login_token client ~token ()] spends a single-use ["m.login.token"] and is
    [client] with the new session attached. [params] defaults to
    {!default_login_params}.

    Raises [Eio.Io] with [Error.E e] on failure. An expired or spent token
    arrives as [Error.Matrix] with [M_FORBIDDEN]. [request_refresh_token=true]
    asks for a refresh token and is omitted by default. *)

(** {1 Refreshing and ending a session} *)

val refresh_token :
  Client.t -> refresh_token:string -> Matrix_client.Auth.refreshed
(** [refresh_token client ~refresh_token] is the new access token and the
    refresh token to use next, the latter being absent when the current one
    stays valid. It does not attach the result to [client].

    Raises [Eio.Io] with [Error.E e] on failure. *)

val refresh_token_with_expiry :
  Client.t -> refresh_token:string -> Matrix_client.Auth.refreshed_with_expiry
(** [refresh_token_with_expiry] is {!refresh_token} and also decodes the
    optional [expires_in_ms] lifetime. *)

val logout : Client.t -> unit
(** [logout client] invalidates this session's access token and deletes its
    device. Raises [Eio.Io] with [Error.E e] on failure. *)

val logout_session :
  ?http:Fetch.plain -> Client.t -> Matrix_client.Session.Auth.t -> unit
(** [logout_session client auth] ends the stored session represented by [auth].
    Matrix sessions use the Matrix [/logout] endpoint. OAuth sessions discover
    and validate metadata, then revoke their access and refresh tokens at the
    authorisation server, never calling Matrix [/logout]. [http] overrides the
    transport captured by [client] before its Matrix client-server view was
    origin-restricted; this is needed when the issuer is on a different origin.
    Raises [Eio.Io] with [Error.E e] on failure. *)

val logout_all : Client.t -> unit
(** [logout_all client] does the same for every session the user has. Raises
    [Eio.Io] with [Error.E e] on failure. *)

(** {1 Registering} *)

(** What kind of account a registration creates. *)
type registration_kind = Matrix_client.Auth.registration_kind =
  | User  (** A normal account. *)
  | Guest  (** A guest account, which most homeservers decline to create. *)

val register :
  Client.t ->
  ?kind:registration_kind ->
  ?username:string ->
  ?password:string ->
  ?params:login_params ->
  ?inhibit_login:bool ->
  ?auth:Matrix_client.Uiaa.auth_data ->
  unit ->
  Client.t
(** [register client ()] registers an account and is [client] with the new
    session attached. [kind] defaults to {!User}. Omitting [username] asks the
    server to choose one. [params] names the device the new session claims and
    defaults to {!default_login_params}. [inhibit_login] registers without
    creating a session, and so has nothing to return.

    Registration is user-interactive. A first call without [auth] normally fails
    with a 401 whose body {!Matrix_client.Uiaa.parse_uiaa_response} decodes into
    the flows on offer; call again with an [auth] built from one of them.

    Raises [Eio.Io] with [Error.E e] on failure, that first challenge included,
    which arrives as [Error.Http] with status [401]. *)

val register_uiaa :
  Client.t ->
  ?kind:registration_kind ->
  ?username:string ->
  ?password:string ->
  ?params:login_params ->
  ?inhibit_login:bool ->
  auth_callback:
    (Matrix_client.Uiaa.uiaa_response -> Matrix_client.Uiaa.auth_data option) ->
  unit ->
  Client.t Matrix_client.Uiaa.uiaa_result
(** [register_uiaa client ~auth_callback ()] is the bounded UIAA registration
    flow with one callback-produced retry. A successful result carries [client]
    with the new session attached; [Uiaa_auth_required] carries the challenge
    when the callback declines it or the retry needs another stage. Other
    failures raise [Eio.Io] with [Error.E e], rather than being flattened into
    an authentication-required result. Registration fields are retained on the
    retry, as in {!Matrix_client.Auth.register_uiaa}. *)

val register_available : Client.t -> username:string -> bool
(** [register_available client ~username] is [true] when the localpart is free.

    Raises [Eio.Io] with [Error.E e] on failure. A taken or invalid name is
    [Error.Matrix] with [M_USER_IN_USE], [M_INVALID_USERNAME] or [M_EXCLUSIVE]
    rather than [false]. *)

val check_registration_token : Client.t -> token:string -> bool
(** [check_registration_token client ~token] reports whether [token] would still
    be accepted, without spending it. Raises [Eio.Io] with [Error.E e] on
    failure. *)

(** {1 Account information} *)

val whoami : Client.t -> Matrix_proto.Id.User_id.t
(** [whoami client] is the user the access token belongs to, and is the cheapest
    check that a stored session is still valid.

    Raises [Eio.Io] with [Error.E e] on failure. An expired session arrives as
    [Error.Matrix] with [M_UNKNOWN_TOKEN]. *)

(** {1 Login tokens} *)

type token_login = Matrix_client.Auth.token_login = {
  login_token : string;  (** Pass to {!val-login_token} as [~token]. *)
  expires_in_ms : int;  (** Lifetime in milliseconds from issue. *)
}
(** A single-use token a second device can log in with. *)

val get_login_token :
  Client.t -> ?auth:Matrix_client.Uiaa.auth_data -> unit -> token_login
(** [get_login_token client ()] mints an ["m.login.token"] the user can hand to
    a second device. The endpoint is user-interactive, as {!register} is.

    Raises [Eio.Io] with [Error.E e] on failure. *)

(** {1 Third-party identifier validation}

    Proving an email address or a phone number takes two calls. Ask the server
    to send a token to the address with one of the calls below, then prove the
    token arrived with {!Matrix_client.Uiaa.validate_email_token}.
    [client_secret] must be the same opaque string across every retry of one
    validation, and [send_attempt] must increase for the server to send again.
*)

val request_registration_email_token :
  Client.t ->
  email:string ->
  client_secret:string ->
  send_attempt:int ->
  ?next_link:string ->
  ?id_server:string ->
  ?id_access_token:string ->
  unit ->
  Matrix_client.Uiaa.request_token_response
(** [request_registration_email_token client ~email ~client_secret ~send_attempt
     ()] asks the server to send a validation token to [email] for a
    registration. [next_link] is where the server sends the user's browser once
    the address is validated. [id_server] and [id_access_token] name an identity
    server to delegate the validation to.

    Raises [Eio.Io] with [Error.E e] on failure. *)

val request_registration_msisdn_token :
  Client.t ->
  country:string ->
  phone_number:string ->
  client_secret:string ->
  send_attempt:int ->
  ?next_link:string ->
  ?id_server:string ->
  ?id_access_token:string ->
  unit ->
  Matrix_client.Uiaa.request_token_response
(** [request_registration_msisdn_token client ~country ~phone_number
     ~client_secret ~send_attempt ()] is {!request_registration_email_token} for
    a phone number. [country] is the ISO 3166-1 alpha-2 code [phone_number]
    belongs to.

    Raises [Eio.Io] with [Error.E e] on failure. *)

val request_password_email_token :
  Client.t ->
  email:string ->
  client_secret:string ->
  send_attempt:int ->
  ?next_link:string ->
  ?id_server:string ->
  ?id_access_token:string ->
  unit ->
  Matrix_client.Uiaa.request_token_response
(** [request_password_email_token client ~email ~client_secret ~send_attempt ()]
    is {!request_registration_email_token} for a password reset rather than a
    registration.

    Raises [Eio.Io] with [Error.E e] on failure. *)

val request_password_msisdn_token :
  Client.t ->
  country:string ->
  phone_number:string ->
  client_secret:string ->
  send_attempt:int ->
  ?next_link:string ->
  ?id_server:string ->
  ?id_access_token:string ->
  unit ->
  Matrix_client.Uiaa.request_token_response
(** [request_password_msisdn_token client ~country ~phone_number ~client_secret
     ~send_attempt ()] is {!request_password_email_token} for a phone number.
    [country] is the ISO 3166-1 alpha-2 code [phone_number] belongs to.

    Raises [Eio.Io] with [Error.E e] on failure. *)
