(*---------------------------------------------------------------------------
   Copyright (c) 2025 Anil Madhavapeddy. All rights reserved.
   SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

(** Session management for Immich CLI with profile support.

    This module provides session persistence for Immich authentication,
    supporting both JWT tokens and API keys. Sessions are stored in
    profile-specific directories under [~/.config/immich/profiles/<profile>/session.json].

    {2 Directory Structure}

    {v
    ~/.config/immich/
      config.json           # Stores current_profile setting
      profiles/
        default/
          session.json      # Session for "default" profile
        home/
          session.json      # Session for "home" profile
        work/
          session.json      # Session for "work" profile
    v}

    {2 Authentication Methods}

    Immich supports two authentication methods:
    - {b JWT}: Obtained via email/password login. Tokens expire and need refresh.
    - {b API Key}: Created in Immich settings. Never expires, simpler for CLI use.

    {[
      (* Login with API key *)
      let session = Session.create
        ~server_url:"https://immich.example.com"
        ~auth:(Api_key { key = "xxx"; name = Some "cli" })
        () in
      Session.save fs ~profile:"home" session
    ]} *)

(** {1 Types} *)

type auth_method =
  | Jwt of { access_token : string; user_id : string; email : string }
  | Api_key of { key : string; name : string option }
(** Authentication method. API keys are preferred for CLI use as they
    don't expire. *)

type t
(** Session data. *)

val jsont : t Jsont.t
(** JSON codec for sessions. *)

(** {1 Session Construction} *)

val create : server_url:string -> auth:auth_method -> unit -> t
(** [create ~server_url ~auth ()] creates a new session with the current timestamp. *)

(** {1 Session Accessors} *)

val server_url : t -> string
(** [server_url t] returns the server URL. *)

val auth : t -> auth_method
(** [auth t] returns the authentication method. *)

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
    ([~/.config/immich]), creating it if needed. *)

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

val is_jwt_expired : ?leeway:int -> string -> bool
(** [is_jwt_expired ?leeway token] returns [true] if the JWT token is expired.
    @param leeway Extra time buffer in seconds (default: 60) *)

val is_expired : ?leeway:int -> t -> bool
(** [is_expired ?leeway session] returns [true] if the session is expired.
    API key sessions never expire.
    @param leeway Extra time buffer in seconds for JWT (default: 60) *)

val pp : t Fmt.t
(** Pretty-print a session. *)
