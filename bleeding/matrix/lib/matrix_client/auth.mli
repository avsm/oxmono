(** auth — logging in, logging out and registering.

    Every call here takes an unauthenticated {!Client.t} and, where it succeeds,
    yields a {!Client.type-session} to hand to {!Client.with_session}. {!Oauth}
    is the OAuth 2.0 alternative to this module, and {!Uiaa} answers the
    challenges the protected calls here return. *)

(** {1 Login flows} *)

type login_flow =
  | Password  (** ["m.login.password"]. *)
  | Token  (** ["m.login.token"]. *)
  | Sso  (** ["m.login.sso"]. *)
  | Unknown of string  (** A flow type this module does not know. *)

val get_login_flows : Client.t -> (login_flow list, Error.t) result
(** [get_login_flows t] is the ways this homeserver will accept a login. Uses
    [GET /_matrix/client/v3/login] (Matrix 1.0), which needs no access token. *)

(** {1 Logging in} *)

type login_params = {
  device_id : string option;
      (** Reuse an existing device rather than have the server mint one. Reusing
          it invalidates that device's previous access token. *)
  initial_device_display_name : string option;
      (** Shown to the user in the device list, for a device the server is about
          to create. *)
}
(** Which device a login or a registration claims. *)

val default_login_params : login_params
(** Both fields absent, so the server picks a device id and leaves it unnamed.
*)

type login = { session : Client.session; expires_at : Ptime.t option }
(** A login session together with the absolute access-token expiry derived from
    [expires_in_ms]. Older servers may omit the lifetime. *)

val login_password_with_expiry :
  Client.t ->
  user:string ->
  password:string ->
  ?params:login_params ->
  ?request_refresh_token:bool ->
  unit ->
  (login, Error.t) result
(** [login_password_with_expiry] is {!login_password} and also retains the
    optional access-token lifetime returned with refresh-token-capable login. *)

val login_password :
  Client.t ->
  user:string ->
  password:string ->
  ?params:login_params ->
  ?request_refresh_token:bool ->
  unit ->
  (Client.session, Error.t) result
(** [login_password t ~user ~password ()] logs in with type
    ["m.login.password"], identifying the account by ["m.id.user"], so [user]
    may be a localpart or a full [@user:server]. [params] defaults to
    {!default_login_params}. Uses [POST /_matrix/client/v3/login] (Matrix 1.0).

    Wrong credentials are [M_FORBIDDEN]. [request_refresh_token=true] asks the
    homeserver to issue a refresh token, as defined by Matrix 1.3; it is omitted
    by default for compatibility. *)

val login_token_with_expiry :
  Client.t ->
  token:string ->
  ?params:login_params ->
  ?request_refresh_token:bool ->
  unit ->
  (login, Error.t) result
(** [login_token_with_expiry] is {!val-login_token} and also retains the
    optional access-token lifetime. *)

val login_token :
  Client.t ->
  token:string ->
  ?params:login_params ->
  ?request_refresh_token:bool ->
  unit ->
  (Client.session, Error.t) result
(** [login_token t ~token ()] logs in with type ["m.login.token"], spending a
    single-use token from an SSO redirect or from {!get_login_token}. [params]
    defaults to {!default_login_params}. Uses [POST /_matrix/client/v3/login]
    (Matrix 1.0).

    An expired or spent token is [M_FORBIDDEN]. [request_refresh_token=true]
    asks the homeserver to issue a refresh token and is omitted by default. *)

(** {1 Refreshing and ending a session} *)

type refreshed = {
  access_token : string;  (** The token to send from now on. *)
  refresh_token : string option;
      (** The refresh token to use next. Absent when the server keeps the
          current one valid. *)
}
(** What a refresh yields. *)

type refreshed_with_expiry = {
  refreshed : refreshed;
  expires_at : Ptime.t option;
}
(** What an expiry-aware refresh yields. [expires_at] is decoded from the
    optional Matrix [expires_in_ms] response member. *)

val refresh_token :
  Client.t -> refresh_token:string -> (refreshed, Error.t) result
(** [refresh_token t ~refresh_token] exchanges [refresh_token] for a fresh
    access token. Uses [POST /_matrix/client/v3/refresh] (Matrix 1.3), which
    needs no access token, since the refresh token is the credential. *)

val refresh_token_with_expiry :
  Client.t -> refresh_token:string -> (refreshed_with_expiry, Error.t) result
(** [refresh_token_with_expiry] is {!val-refresh_token} with the optional
    [expires_in_ms] lifetime converted to an absolute deadline. *)

val logout : Client.t -> (unit, Error.t) result
(** [logout t] invalidates this session's access token and deletes its device.
    Uses [POST /_matrix/client/v3/logout] (Matrix 1.0). *)

val logout_all : Client.t -> (unit, Error.t) result
(** [logout_all t] does the same for every session the user has. Uses
    [POST /_matrix/client/v3/logout/all] (Matrix 1.0). *)

(** {1 Registering} *)

type registration_kind =
  | User  (** A normal account. *)
  | Guest  (** A guest account, which most homeservers decline to create. *)

val register :
  Client.t ->
  ?kind:registration_kind ->
  ?username:string ->
  ?password:string ->
  ?params:login_params ->
  ?inhibit_login:bool ->
  ?auth:Uiaa.auth_data ->
  unit ->
  (Client.session, Error.t) result
(** [register t ()] creates an account. Uses [POST /_matrix/client/v3/register]
    (Matrix 1.0).

    [kind] defaults to {!User}. Omitting [username] asks the server to choose
    one. [params] names the device the new session claims, and defaults to
    {!default_login_params}.

    Registration is user-interactive. A first call without [auth] normally
    answers 401 with the flows, which {!Uiaa.parse_uiaa_response} decodes from
    the {!Error.Http_error} body. Call again with an [auth] built from one of
    them, carrying the session the challenge named.

    [inhibit_login] registers the account without creating a session, in which
    case there is nothing to return and the result is {!Error.No_content}. *)

val register_uiaa :
  Client.t ->
  ?kind:registration_kind ->
  ?username:string ->
  ?password:string ->
  ?params:login_params ->
  ?inhibit_login:bool ->
  auth_callback:(Uiaa.uiaa_response -> Uiaa.auth_data option) ->
  unit ->
  Client.session Uiaa.uiaa_result
(** [register_uiaa t ~auth_callback ()] creates an account through the bounded
    {!Uiaa.with_uiaa} driver. The initial request carries the registration
    fields; when it receives a parseable 401 challenge, [auth_callback] may
    return one completed stage and the same registration fields are retained on
    the single retry. A successful response is {!Uiaa.Uiaa_success}; a declined
    callback or a challenge that survives that retry is
    {!Uiaa.Uiaa_auth_required}; malformed challenges and all other failures are
    {!Uiaa.Uiaa_error}. [kind], [username], [password], [params] and
    [inhibit_login] have the same meanings as {!register}. *)

val register_available : Client.t -> username:string -> (bool, Error.t) result
(** [register_available t ~username] is [Ok true] when the localpart is free.
    Uses [GET /_matrix/client/v3/register/available] (Matrix 1.0).

    A taken or invalid name is an [Error] carrying [M_USER_IN_USE],
    [M_INVALID_USERNAME] or [M_EXCLUSIVE], rather than [Ok false]. *)

val check_registration_token :
  Client.t -> token:string -> (bool, Error.t) result
(** [check_registration_token t ~token] reports whether a registration token
    would still be accepted, without spending it. Uses
    [GET /_matrix/client/v1/register/m.login.registration_token/validity]
    (Matrix 1.2), under the [v1] base path. *)

(** {1 Account information} *)

val whoami : Client.t -> (Matrix_proto.Id.User_id.t, Error.t) result
(** [whoami t] is the user the access token belongs to. Uses
    [GET /_matrix/client/v3/account/whoami] (Matrix 1.0).

    This is the cheapest way to tell whether a stored session is still valid,
    since an expired one is [M_UNKNOWN_TOKEN]. *)

(** {1 Login tokens} *)

type token_login = {
  login_token : string;  (** Pass to {!val-login_token} as [~token]. *)
  expires_in_ms : int;  (** Lifetime in milliseconds from issue. *)
}
(** A single-use token a second device can log in with. *)

val get_login_token :
  Client.t -> ?auth:Uiaa.auth_data -> unit -> (token_login, Error.t) result
(** [get_login_token t ()] mints an ["m.login.token"] the user can hand to a
    second device. Uses [POST /_matrix/client/v1/login/get_token] on Matrix 1.7+
    servers, or MSC3882's unstable path on older servers, selected from
    [/versions].

    The endpoint is user-interactive. A first call without [auth] normally
    answers 401 with the flows, which {!Uiaa.parse_uiaa_response} decodes. Call
    again with an [auth] built from one of them.

    Servers advertise support through the [m.get_login_token] capability, in
    {!type:Server.capabilities}. *)
