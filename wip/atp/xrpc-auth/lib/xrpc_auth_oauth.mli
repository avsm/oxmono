(*---------------------------------------------------------------------------
   Copyright (c) 2026 Anil Madhavapeddy. All rights reserved.
   SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

(** AT Protocol OAuth for command-line clients.

    The client is a
    {{:https://atproto.com/specs/oauth#localhost-client-development}loopback
    client}. It needs no published client metadata. The user approves the
    request in a browser, which returns to a one-shot listener on
    [127.0.0.1]. Tokens are bound to a P-256 DPoP key that is created for
    each login and saved with the session.

    @see <https://atproto.com/specs/oauth> AT Protocol OAuth *)

(** {1 Scopes} *)

val default_scope : string
(** [default_scope] is ["atproto transition:generic"], the access an app
    password grants. *)

val loopback_client_id : redirect_uri:string -> scope:string -> string
(** [loopback_client_id ~redirect_uri ~scope] is the [http://localhost]
    client ID that declares [redirect_uri] and [scope]. *)

(** {1 Login} *)

(** How the browser returns the authorization code. *)
type redirect =
  | Loopback
      (** To a one-shot listener on [127.0.0.1], with a [http://localhost]
          client ID. The browser must run on the same host. *)
  | Paste of {
      client_id : string;
          (** HTTPS URL of a published client metadata document. *)
      redirect_uri : string;
          (** Page declared in that document that shows the code. *)
      read : unit -> string;
          (** Reads what the user pastes: the page's [code=...&state=...]
              line or the whole URL it was opened at. *)
    }
      (** To a page that shows the code for the user to paste into the
          client. The browser may run anywhere. The scope must be within the
          one the client metadata declares. *)

val hosted_client_id : string
(** [hosted_client_id] is the published metadata of the ocaml-atp CLI,
    whose scope is {!default_scope}. *)

val hosted_redirect_uri : string
(** [hosted_redirect_uri] is the paste page that {!hosted_client_id}
    declares. *)

val authorize :
  net:_ Eio.Net.t ->
  clock:_ Eio.Time.clock ->
  http:_ Fetch.t ->
  ?plc:string ->
  ?pds:string ->
  ?identifier:string ->
  ?timeout:float ->
  ?redirect:redirect ->
  scope:string ->
  open_url:(string -> unit) ->
  unit ->
  Xrpc_auth_session.t
(** [authorize ~net ~clock ~http ~scope ~open_url ()] runs an authorization
    code grant and returns the new session.

    [identifier] is a handle or DID. Its PDS, found through [plc] for a
    [did:plc], selects the authorization server, and the server is asked to
    sign in that account. Without [identifier], [pds] selects the server and
    the user picks the account. [open_url] is given the authorization URL to
    show the user. [redirect] is [Loopback] by default. A loopback grant fails
    if the browser does not return within [timeout] seconds, 300 by
    default.

    The returned account must be [identifier] when one was given. Otherwise
    its PDS must name the same authorization server.

    @raise Failure if any step is refused. *)

(** {1 Sessions} *)

type t
(** A live OAuth session that refreshes its tokens. *)

val v :
  clock:_ Eio.Time.clock ->
  http:_ Fetch.t ->
  ?on_update:(Xrpc_auth_session.t -> unit) ->
  Xrpc_auth_session.t ->
  t
(** [v ~clock ~http session] resumes [session]. [on_update] receives each
    refreshed session, which must be saved because a refresh token is
    single-use.

    @raise Invalid_argument if [session] is not an OAuth session. *)

val session : t -> Xrpc_auth_session.t
(** [session t] is the current session. *)

val http : t -> Fetch.plain
(** [http t] is the [http] given to {!v}, with each request to the session's
    PDS origin carrying the access token and a DPoP proof. The token is
    refreshed when it is near expiry. A request refused for a stale DPoP
    nonce is sent once more with the server's nonce, if its body can be
    replayed. *)

val revoke : t -> unit
(** [revoke t] asks the authorization server to revoke the refresh token.
    Failures are ignored. *)
