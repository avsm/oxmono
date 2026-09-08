(** uiaa — user-interactive authentication.

    A homeserver protects its sensitive endpoints by answering the first request
    with 401 and a challenge naming the {!type-auth_flow}s it will accept.
    Deleting a device, changing a password, binding a third-party identifier and
    deactivating an account are all guarded this way. The client picks a flow,
    completes its stages one at a time, and repeats the request with an [auth]
    object for each. This module decodes the challenge, builds those objects,
    and splices them into a request body.

    @see <https://spec.matrix.org/v1.11/client-server-api/#user-interactive-authentication-api>
      User-Interactive Authentication API *)

(** {1 Stages and flows} *)

(** One authentication stage. *)
type auth_type =
  | Password  (** The account password. *)
  | Recaptcha  (** A completed reCAPTCHA challenge. *)
  | OAuth2  (** Delegated authentication through an OAuth 2.0 provider. *)
  | OAuth  (** The Matrix [m.oauth] delegated-authentication stage. *)
  | Email_identity  (** A validated email address. *)
  | Msisdn  (** Phone number. *)
  | Dummy  (** No proof at all, the stage every flow can fall back to. *)
  | Registration_token  (** A token the server operator issued out of band. *)
  | Terms  (** Acceptance of the server's terms of service. *)
  | Sso  (** Delegated authentication through the server's own SSO flow. *)
  | Sso_fallback
      (** {!Sso}, completed through the server's fallback web page rather than a
          native flow. *)
  | Custom of string  (** A stage type this module does not know. *)

val auth_type_of_string : string -> auth_type
(** [auth_type_of_string s] is the constructor a wire name such as
    ["m.login.password"] names. An unrecognised name becomes {!Custom}, so
    decoding never fails. *)

val auth_type_to_string : auth_type -> string
(** [auth_type_to_string t] is the wire name of [t], inverting
    {!auth_type_of_string}. *)

type auth_flow = { stages : auth_type list }
(** One route through the challenge. Its stages must be completed in the order
    given. *)

val auth_flow_jsont : auth_flow Jsont.t
(** Reads and writes a flow, whose wire form is an object with a [stages]
    member. *)

(** {1 The challenge} *)

type uiaa_response = {
  session : string option;
      (** The server's identifier for this attempt. Every subsequent [auth]
          object must carry it, or the completed stages are lost. *)
  flows : auth_flow list;  (** Any one of these completes the request. *)
  completed : auth_type list;  (** Stages already accepted in this session. *)
  params : Jsont.json option;
      (** Per-stage parameters, keyed by stage type. A reCAPTCHA site key and a
          terms document with its URL arrive this way. *)
  error : string option;  (** Set when the last stage was rejected. *)
  errcode : string option;  (** The error code paired with {!field-error}. *)
}
(** The body of a 401 challenge. *)

val uiaa_response_jsont : uiaa_response Jsont.t
(** Reads and writes a challenge. *)

val parse_uiaa_response : string -> uiaa_response option
(** [parse_uiaa_response body] decodes a challenge from the body of a 401,
    normally the [body] of an {!Error.Http_error}. [None] if it is not a
    challenge. *)

val has_dummy_flow : uiaa_response -> bool
(** [has_dummy_flow r] is [true] when some flow of [r] consists of {!Dummy}
    alone, which {!dummy_auth} satisfies without asking the user anything. *)

(** {1 Answering a challenge} *)

(** How the password stage names the account.

    @see <https://spec.matrix.org/v1.11/client-server-api/#identifier-types>
      Identifier types *)
type user_identifier =
  | User of string  (** A localpart or a full [@user:server]. *)
  | ThirdParty of { medium : string; address : string }
      (** A validated third-party identifier, such as an email address. *)
  | Phone of { country : string; phone : string }
      (** [country] is an ISO 3166-1 alpha-2 code. *)

type threepid_creds = {
  sid : string;  (** From {!request_token_response.sid}. *)
  client_secret : string;
      (** The same opaque string passed to the requestToken call. *)
  id_server : string option;
  id_access_token : string option;
}
(** Proof that a third-party identifier was validated. *)

(** One completed stage, ready to be sent as the request's [auth] member.
    [session] is the one the challenge named, and omitting it starts a fresh
    attempt. *)
type auth_data =
  | Password_auth of {
      identifier : user_identifier;
      password : string;
      session : string option;
    }
  | Recaptcha_auth of { response : string; session : string option }
  | Email_identity_auth of {
      threepid_creds : threepid_creds;
      session : string option;
    }
  | Msisdn_auth of { threepid_creds : threepid_creds; session : string option }
  | Dummy_auth of { session : string option }
  | Token_auth of { token : string; session : string option }
  | OAuth_auth of { session : string option }
  | Terms_auth of { session : string option }

val password_auth :
  user:string -> password:string -> ?session:string -> unit -> auth_data
(** [password_auth ~user ~password ()] answers a {!Password} stage, naming the
    account by {!User}, so [user] may be a localpart or a full [@user:server].
*)

val dummy_auth : ?session:string -> unit -> auth_data
(** Answers a {!Dummy} stage, which asserts nothing. *)

val recaptcha_auth : response:string -> ?session:string -> unit -> auth_data
(** [recaptcha_auth ~response ()] answers a {!Recaptcha} stage with the token
    the widget produced. *)

val email_identity_auth :
  sid:string ->
  client_secret:string ->
  ?id_server:string ->
  ?id_access_token:string ->
  ?session:string ->
  unit ->
  auth_data
(** [email_identity_auth ~sid ~client_secret ()] answers an {!Email_identity}
    stage with the credentials of an address already validated through
    {!request_email_token}. *)

val msisdn_auth :
  sid:string ->
  client_secret:string ->
  ?id_server:string ->
  ?id_access_token:string ->
  ?session:string ->
  unit ->
  auth_data
(** [msisdn_auth ~sid ~client_secret ()] answers a {!Msisdn} stage with the
    credentials of a phone number already validated through
    {!request_msisdn_token}. *)

val token_auth : token:string -> ?session:string -> unit -> auth_data
(** Answers a {!Registration_token} stage. *)

val oauth_auth : ?session:string -> unit -> auth_data
(** [oauth_auth ?session ()] answers an {!OAuth} stage after the caller has
    completed the external OAuth reauthorization flow. *)

val terms_auth : ?session:string -> unit -> auth_data
(** Answers a {!Terms} stage, asserting that the user accepted the documents the
    challenge listed in {!uiaa_response.params}. *)

val user_identifier_to_json : user_identifier -> string
(** [user_identifier_to_json id] is [id] as a JSON object. It is built
    structurally and serialised, so a value carrying a quote or a backslash
    survives intact. *)

val auth_data_to_json : auth_data -> string
(** [auth_data_to_json data] is [data] as the JSON object an [auth] member
    takes. Passwords, tokens, session ids and third-party credentials appear as
    JSON string values and cannot introduce members of their own. *)

val add_auth_to_body : body:string -> auth:string -> (string, Error.t) result
(** [add_auth_to_body ~body ~auth] is [body] with an [auth] member holding the
    object [auth], which is what {!auth_data_to_json} returns.

    Both arguments are parsed and the object rebuilt rather than spliced as
    text, so a member value containing a brace or a comma is left alone, and an
    [auth] member already present in [body] is replaced rather than duplicated.
    A [body] that is not a JSON object, and an [auth] that does not parse, are
    {!Error.Json_error}. *)

(** {1 Driving a protected request} *)

(** How a protected request ended. *)
type 'a uiaa_result =
  | Uiaa_success of 'a  (** The request went through. *)
  | Uiaa_auth_required of uiaa_response
      (** A further stage is needed, or the callback declined to answer. *)
  | Uiaa_error of Error.t  (** The request failed for another reason. *)

val with_uiaa :
  make_request:(string option -> ('a, Error.t) result) ->
  auth_callback:(uiaa_response -> auth_data option) ->
  'a uiaa_result
(** [with_uiaa ~make_request ~auth_callback] is the result of issuing the
    request, and, on a 401 challenge, of issuing it once more with whatever
    [auth_callback] returns.

    [make_request] receives the [auth] object as a JSON string, or [None] for
    the first attempt, and is responsible for splicing it in, normally with
    {!add_auth_to_body}. Exactly one stage is answered. A challenge that
    survives the second attempt is {!Uiaa_auth_required} for the caller to loop
    on, as is a challenge the callback answers with [None]. *)

(** {1 Third-party identifier validation}

    Proving an email address or a phone number takes two calls. Ask the server
    to send a token to the address, then prove the token arrived.
    [client_secret] must be the same opaque string across both, and across any
    retry of the first. [send_attempt] must increase for the server to actually
    re-send. *)

type request_token_response = {
  sid : string;  (** Identifies this validation attempt. *)
  submit_url : string option;
      (** Where to submit the token when the server validates it itself. Absent
          when the identity server does. *)
}
(** The type for a validation attempt's reply. *)

val request_token_response_jsont : request_token_response Jsont.t
(** Reads and writes a validation attempt's reply. *)

(** What a validation is for, which decides the endpoint and whether it carries
    an access token. *)
type token_use =
  | Bind
      (** Bind the identifier to the logged-in account, at
          [/account/3pid/{medium}/requestToken]. Sends the access token. *)
  | Register
      (** Prove the identifier while registering, at
          [/register/{medium}/requestToken]. Unauthenticated. *)
  | Password
      (** Prove the identifier to reset a password, at
          [/account/password/{medium}/requestToken]. Unauthenticated. *)

val request_email_token :
  Client.t ->
  ?use:token_use ->
  email:string ->
  client_secret:string ->
  send_attempt:int ->
  ?next_link:string ->
  ?id_server:string ->
  ?id_access_token:string ->
  unit ->
  (request_token_response, Error.t) result
(** [request_email_token t ~email ~client_secret ~send_attempt ()] asks the
    server to send a validation token to [email] (Matrix 1.0).

    [use] says what the validation is for and defaults to {!Bind}. Repeating a
    call with the same [send_attempt] is the previous [sid] without a second
    message being sent. [next_link] is where the server sends the user's browser
    once the address is validated. [id_server] and [id_access_token] name an
    identity server to delegate the validation to. *)

val request_msisdn_token :
  Client.t ->
  ?use:token_use ->
  country:string ->
  phone_number:string ->
  client_secret:string ->
  send_attempt:int ->
  ?next_link:string ->
  ?id_server:string ->
  ?id_access_token:string ->
  unit ->
  (request_token_response, Error.t) result
(** [request_msisdn_token t ~country ~phone_number ~client_secret ~send_attempt
     ()] is {!request_email_token} for a phone number (Matrix 1.0). [country] is
    the ISO 3166-1 alpha-2 code [phone_number] belongs to. *)

val validate_email_token :
  Client.t ->
  sid:string ->
  client_secret:string ->
  token:string ->
  (unit, Error.t) result
(** [validate_email_token t ~sid ~client_secret ~token] submits the token the
    user received to [/_matrix/client/v3/account/3pid/email/validate].

    That path is not in the client-server API. A server that validates tokens
    itself names where to submit them in {!request_token_response.submit_url},
    which is what a client should use. The response body is ignored, so any
    success is [Ok ()]. *)
