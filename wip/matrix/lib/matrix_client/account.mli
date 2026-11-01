(** account — third-party identifiers, the password and deactivation.

    Every call here acts as the logged-in user. The user identifier comes from
    the client's session, and a client with no session fails with
    {!Error.No_session}. Account data itself lives in {!Account_data}.

    A homeserver guards the password change and the deactivation with
    user-interactive authentication. The first call gets a 401 carrying a
    challenge, which {!Uiaa.parse_uiaa_response} reads out of the
    {!Error.Http_error}, and the call is repeated with the answer in [auth]. *)

(** {1 Third-party identifiers} *)

(** The kinds of third-party identifier an account may be bound to. *)
type medium =
  | Email
  | Msisdn  (** A phone number in international format, without a [+]. *)

val medium_to_string : medium -> string
(** [medium_to_string m] is ["email"] or ["msisdn"]. *)

val medium_of_string : string -> (medium, [> `Msg of string ]) result
(** [medium_of_string s] is the medium [s] names. *)

type threepid = {
  medium : medium;
  address : string;
  validated_at : Matrix_proto.Event.Timestamp.t;
  added_at : Matrix_proto.Event.Timestamp.t;
}
(** An email address or phone number bound to the account. *)

val get_threepids : Client.t -> (threepid list, Error.t) result
(** [get_threepids t] is [GET /_matrix/client/v3/account/3pid] (Matrix 1.0). *)

val request_email_token :
  Client.t ->
  email:string ->
  client_secret:string ->
  send_attempt:int ->
  (string, Error.t) result
(** [request_email_token t ~email ~client_secret ~send_attempt] is
    [POST /_matrix/client/v3/account/3pid/email/requestToken] (Matrix 1.0) and
    is the session identifier to pass to {!add_threepid}.

    [client_secret] is an opaque string of the caller's choosing that ties the
    token request to the {!add_threepid} that follows. Repeating a request with
    the same [send_attempt] does not send a second mail, and incrementing it
    makes the server try again. An address already bound to an account is
    [M_THREEPID_IN_USE], one the server refuses [M_THREEPID_DENIED]. *)

val request_msisdn_token :
  Client.t ->
  country:string ->
  phone_number:string ->
  client_secret:string ->
  send_attempt:int ->
  (string, Error.t) result
(** [request_msisdn_token t ~country ~phone_number ~client_secret ~send_attempt]
    is [POST /_matrix/client/v3/account/3pid/msisdn/requestToken] (Matrix 1.0)
    and is the session identifier. [country] is a two-letter ISO 3166-1 alpha-2
    code, which fixes how [phone_number] is read. *)

val add_threepid :
  Client.t -> client_secret:string -> sid:string -> (unit, Error.t) result
(** [add_threepid t ~client_secret ~sid] is
    [POST /_matrix/client/v3/account/3pid/add] (Matrix 1.0), binding the
    identifier whose token request returned [sid]. An identifier the user has
    not yet validated is [M_THREEPID_AUTH_FAILED]. *)

val delete_threepid :
  Client.t -> medium:medium -> address:string -> (unit, Error.t) result
(** [delete_threepid t ~medium ~address] is
    [POST /_matrix/client/v3/account/3pid/delete] (Matrix 1.0). *)

(** {1 Password and deactivation} *)

val change_password :
  Client.t ->
  new_password:string ->
  ?logout_devices:bool ->
  ?auth:Uiaa.auth_data ->
  unit ->
  (unit, Error.t) result
(** [change_password t ~new_password ()] is
    [POST /_matrix/client/v3/account/password] (Matrix 1.0).

    [logout_devices] invalidates every other access token as well. It defaults
    to [false], where the specification's own default is [true], so it is always
    sent. [auth] answers one stage of a user-interactive challenge and defaults
    to absent, which is what draws the challenge out of the server. *)

val deactivate :
  Client.t ->
  ?erase:bool ->
  ?auth:Uiaa.auth_data ->
  unit ->
  (unit, Error.t) result
(** [deactivate t ()] is [POST /_matrix/client/v3/account/deactivate] (Matrix
    1.0). The account can never be used again and its user identifier is never
    reissued.

    [erase] asks the server to redact the user's messages as well. It defaults
    to [false], and a server may ignore it. [auth] is as for {!change_password}.
*)

(** {1 Ignored users}

    The ignore list is the [m.ignored_user_list] account data event rather than
    an endpoint of its own. Each change reads the list and writes it back whole,
    so two clients racing can lose one of the two edits. *)

val get_ignored_users :
  Client.t -> (Matrix_proto.Id.User_id.t list, Error.t) result
(** [get_ignored_users t] is the current ignore list, and [[]] when the user has
    never set one. A list holding something that is not a well-formed user
    identifier is an {!Error.Json_error}. *)

val ignore_user :
  Client.t -> user_id:Matrix_proto.Id.User_id.t -> (unit, Error.t) result
(** [ignore_user t ~user_id] adds [user_id] to the ignore list, and does nothing
    when it is already there. The server drops the user's events from subsequent
    syncs, and events already in a local timeline stay. *)

val unignore_user :
  Client.t -> user_id:Matrix_proto.Id.User_id.t -> (unit, Error.t) result
(** [unignore_user t ~user_id] removes [user_id] from the ignore list. *)
