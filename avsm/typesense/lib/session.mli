(*---------------------------------------------------------------------------
   Copyright (c) 2025 Anil Madhavapeddy. All rights reserved.
   SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

(** Session management for Typesense CLI with profile support.

    This module provides session persistence for Typesense authentication
    using API keys. Sessions are stored in profile-specific directories
    under [~/.config/typesense/profiles/<profile>/session.json].

    {2 Directory Structure}

    {v
    ~/.config/typesense/
      config.json           # Stores current_profile setting
      profiles/
        default/
          session.json      # Session for "default" profile
        prod/
          session.json      # Session for "prod" profile
    v}

    {[
      (* Login with API key *)
      let session = Session.create
        ~server_url:"http://localhost:8108"
        ~api_key:"xyz"
        () in
      Session.save fs ~profile:"default" session
    ]} *)

(** {1 Types} *)

type t
(** Session data. *)

val jsont : t Jsont.t
(** JSON codec for sessions. *)

(** {1 Session Construction} *)

val create : server_url:string -> api_key:string -> unit -> t
(** [create ~server_url ~api_key ()] creates a new session with the current timestamp. *)

(** {1 Session Accessors} *)

val server_url : t -> string
(** [server_url t] returns the server URL. *)

val api_key : t -> string
(** [api_key t] returns the API key. *)

val created_at : t -> string
(** [created_at t] returns the creation timestamp (RFC 3339). *)

(** {1 Profile Management} *)

val default_profile : string
(** The default profile name (["default"]). *)

val get_current_profile : Eio.Fs.dir_ty Eio.Path.t -> string
(** [get_current_profile fs] returns the current profile name. Returns
    {!default_profile} if no profile has been set. *)

val set_current_profile : Eio.Fs.dir_ty Eio.Path.t -> string -> unit
(** [set_current_profile fs profile] sets the current profile. *)

val list_profiles : Eio.Fs.dir_ty Eio.Path.t -> string list
(** [list_profiles fs] returns all profiles that have sessions.
    Returns profile names sorted alphabetically. *)

(** {1 Directory Paths} *)

val base_config_dir : Eio.Fs.dir_ty Eio.Path.t -> Eio.Fs.dir_ty Eio.Path.t
(** [base_config_dir fs] returns the base config directory
    ([~/.config/typesense]), creating it if needed. *)

val config_dir :
  Eio.Fs.dir_ty Eio.Path.t ->
  ?profile:string ->
  unit ->
  Eio.Fs.dir_ty Eio.Path.t
(** [config_dir fs ?profile ()] returns the config directory for a
    profile, creating it if needed.
    @param profile Profile name (default: current profile) *)

(** {1 Session Persistence} *)

val save : Eio.Fs.dir_ty Eio.Path.t -> ?profile:string -> t -> unit
(** [save fs ?profile session] saves the session.
    @param profile Profile name (default: current profile) *)

val load : Eio.Fs.dir_ty Eio.Path.t -> ?profile:string -> unit -> t option
(** [load fs ?profile ()] loads a saved session.
    @param profile Profile name (default: current profile) *)

val clear : Eio.Fs.dir_ty Eio.Path.t -> ?profile:string -> unit -> unit
(** [clear fs ?profile ()] removes the saved session.
    @param profile Profile name (default: current profile) *)

(** {1 Session Utilities} *)

val pp : t Fmt.t
(** Pretty-print a session. *)
