(*---------------------------------------------------------------------------
   Copyright (c) 2025 Anil Madhavapeddy. All rights reserved.
   SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

(** Credential manager with automatic JWT token refresh.

    This module provides high-level session management for AT Protocol,
    including login, logout, and automatic token refresh before expiry.

    {2 Token Refresh}

    AT Protocol access tokens typically expire after 2 hours. The credential
    manager automatically refreshes tokens before they expire (with a 5-minute
    buffer) to ensure seamless operation.

    Refresh is performed atomically using a mutex to prevent concurrent refresh
    attempts from multiple fibers.

    {2 Usage}

    {[
      let cred = Xrpc_cred.create ~sw ~env ~service:"https://bsky.social" () in

      (* Register callbacks *)
      Xrpc_cred.on_session_update cred (fun session ->
          save_session_to_disk session);
      Xrpc_cred.on_session_expired cred (fun () -> show_login_prompt ());

      (* Login *)
      let client =
        Xrpc_cred.login cred ~identifier:"alice.bsky.social"
          ~password:"app-password" ()
      in

      (* Use client - token refresh happens automatically *)
      let timeline =
        Xrpc_client.query client ~nsid:"app.bsky.feed.getTimeline" ~params:[]
          ~decoder:timeline_jsont
      in

      (* Or resume from saved session *)
      let client = Xrpc_cred.resume cred ~session:saved_session () in

      (* Logout when done *)
      Xrpc_cred.logout cred
    ]} *)

(** {1 Credential Manager} *)

type t
(** Credential manager state. *)

val create :
  sw:Eio.Switch.t ->
  env:
    < clock : _ Eio.Time.clock
    ; net : _ Eio.Net.t
    ; fs : Eio.Fs.dir_ty Eio.Path.t
    ; .. > ->
  service:string ->
  unit ->
  t
(** [create ~sw ~env ~service ()] creates a credential manager.

    @param sw Eio switch for resource management
    @param env Eio environment capabilities
    @param service Base URL of the PDS (e.g., ["https://bsky.social"]) *)

(** {1 Callbacks} *)

val on_session_update : t -> (Xrpc_types.session -> unit) -> unit
(** [on_session_update cred f] registers a callback for session updates.

    Called after:
    - Successful login
    - Successful token refresh

    Use this to persist sessions to disk. *)

val on_session_expired : t -> (unit -> unit) -> unit
(** [on_session_expired cred f] registers a callback for session expiration.

    Called when:
    - Token refresh fails
    - Logout completes

    Use this to prompt for re-authentication. *)

(** {1 Session Access} *)

val get_session : t -> Xrpc_types.session option
(** [get_session cred] returns the current session, if authenticated. *)

val get_service : t -> string
(** [get_service cred] returns the service URL. *)

(** {1 Authentication} *)

val login :
  t ->
  identifier:string ->
  password:string ->
  ?auth_factor_token:string ->
  unit ->
  Xrpc_client.t
(** [login cred ~identifier ~password ?auth_factor_token ()] authenticates and
    returns a client with automatic token refresh.

    @param identifier Handle or DID to login as
    @param password Account password or app password
    @param auth_factor_token Optional 2FA token if required

    @raise Eio.Io with [Xrpc_error.E] on authentication failure *)

val login_client :
  t ->
  Xrpc_client.t ->
  identifier:string ->
  password:string ->
  ?auth_factor_token:string ->
  unit ->
  Xrpc_client.t
(** [login_client cred client ~identifier ~password ?auth_factor_token ()]
    authenticates using an existing client.

    Returns a new client with auto-refresh enabled. The original client is
    updated with the session but doesn't have the refresh interceptor.

    @raise Eio.Io with [Xrpc_error.E] on authentication failure *)

val resume : t -> session:Xrpc_types.session -> unit -> Xrpc_client.t
(** [resume cred ~session ()] resumes from a stored session.

    Returns a client with automatic token refresh. If the access token is
    expired but refresh token is valid, a refresh is attempted immediately.

    @raise Eio.Io with [Xrpc_error.E] if the refresh token is also expired *)

val logout : t -> unit
(** [logout cred] logs out and clears the session.

    Calls [com.atproto.server.deleteSession] to invalidate server-side session.
    If the server call fails, the local session is still cleared. *)
