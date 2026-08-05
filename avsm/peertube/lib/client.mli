(*---------------------------------------------------------------------------
   Copyright (c) 2025 Anil Madhavapeddy. All rights reserved.
   SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

(** PeerTube client with authentication.

    This module provides a high-level client that handles authentication
    and session management for PeerTube API access. *)

(** {1 Client Type} *)

(** An authenticated PeerTube client. *)
type t

(** {1 Authentication} *)

(** [login_password ~sw ~env ?requests_config ?profile ~server_url ~username ~password ()]
    authenticates with username and password using OAuth2 password grant.

    The OAuth client credentials are automatically retrieved from the server.
    On success, the session is saved to disk. *)
val login_password :
  sw:Eio.Switch.t ->
  env:< clock : _ Eio.Time.clock; fs : Eio.Fs.dir_ty Eio.Path.t; net : _ Eio.Net.t; .. > ->
  ?requests_config:Requests.Cmd.config ->
  ?profile:string ->
  server_url:string ->
  username:string ->
  password:string ->
  unit ->
  t

(** [resume ~sw ~env ?requests_config ?profile ~session ()] creates a client
    from an existing session. *)
val resume :
  sw:Eio.Switch.t ->
  env:< clock : _ Eio.Time.clock; fs : Eio.Fs.dir_ty Eio.Path.t; net : _ Eio.Net.t; .. > ->
  ?requests_config:Requests.Cmd.config ->
  ?profile:string ->
  session:Session.t ->
  unit ->
  t

(** [logout t] removes the saved session. *)
val logout : t -> unit

(** {1 Accessors} *)

(** [client t] returns the underlying PeerTube API client. *)
val client : t -> Peer_tube.t

(** [session t] returns the session. *)
val session : t -> Session.t

(** [profile t] returns the profile name, if any. *)
val profile : t -> string option

(** [fs t] returns the filesystem. *)
val fs : t -> Eio.Fs.dir_ty Eio.Path.t
