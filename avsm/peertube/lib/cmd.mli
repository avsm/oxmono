(*---------------------------------------------------------------------------
   Copyright (c) 2025 Anil Madhavapeddy. All rights reserved.
   SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

(** Cmdliner commands for PeerTube CLI.

    This module provides the command-line interface for authentication
    and other common operations. *)

(** Application name. *)
val app_name : string

(** {1 Common Options} *)

(** Profile name argument for cmdliner. *)
val profile_arg : string option Cmdliner.Term.t

(** Server URL argument for cmdliner. *)
val server_url_arg : string Cmdliner.Term.t

(** Username argument for cmdliner. *)
val username_arg : string Cmdliner.Term.t

(** Password argument for cmdliner. *)
val password_arg : string option Cmdliner.Term.t

(** Requests config term. *)
val requests_config_term : Eio.Fs.dir_ty Eio.Path.t -> Requests.Cmd.config Cmdliner.Term.t

(** {1 Commands} *)

(** [auth_cmd env fs] returns the auth command group containing login, logout, status, and profiles. *)
val auth_cmd :
  < clock : _ Eio.Time.clock; fs : Eio.Fs.dir_ty Eio.Path.t; net : _ Eio.Net.t; .. > ->
  Eio.Fs.dir_ty Eio.Path.t ->
  int Cmdliner.Cmd.t

(** {1 Client Helper} *)

(** [with_client ~sw ~env ~fs ?requests_config ?profile f] loads a session and creates a client,
    then calls [f] with the client. Exits with an error if not logged in. *)
val with_client :
  sw:Eio.Switch.t ->
  env:< clock : _ Eio.Time.clock; fs : Eio.Fs.dir_ty Eio.Path.t; net : _ Eio.Net.t; .. > ->
  fs:Eio.Fs.dir_ty Eio.Path.t ->
  ?requests_config:Requests.Cmd.config ->
  ?profile:string ->
  (Client.t -> 'a) ->
  'a
