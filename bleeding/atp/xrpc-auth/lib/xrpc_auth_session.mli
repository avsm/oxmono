(*---------------------------------------------------------------------------
   Copyright (c) 2025 Anil Madhavapeddy. All rights reserved.
   SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

(** Generic session management for AT Protocol CLIs with profile support.

    This module provides session persistence that can be used by any AT Protocol
    CLI application. Sessions are stored in profile-specific directories under
    [~/.config/<app_name>/profiles/<profile>/session.json].

    {2 Directory Structure}

    {v
    ~/.config/<app_name>/
      config.json           # Stores current_profile setting
      profiles/
        default/
          session.json      # Session for "default" profile
        alice.bsky.social/
          session.json      # Session for "alice.bsky.social" profile
        bob.example.com/
          session.json      # Session for "bob.example.com" profile
    v}

    {2 Profile Usage}

    Profiles allow multiple accounts to be logged in simultaneously. The current
    profile is used by default when no profile is specified.

    {[
      (* Login as alice and save to a profile named after her handle *)
      let () =
        Xrpc_auth.Session.save fs ~app_name:"bsky" ~profile:"alice.bsky.social"
          session;
        (* Set alice as the current profile *)
        Xrpc_auth.Session.set_current_profile fs ~app_name:"bsky"
          "alice.bsky.social"

      (* Later, load the current profile's session *)
      let session = Xrpc_auth.Session.load fs ~app_name:"bsky" ()
    ]} *)

(** {1 Session Type} *)

type t = {
  access_jwt : string;
  refresh_jwt : string;
  did : string;
  handle : string;
  pds : string;
  created_at : string;
}
(** Saved session data. *)

val jsont : t Jsont.t
(** JSON codec for sessions. *)

(** {1 Profile Management} *)

val default_profile : string
(** The default profile name (["default"]). *)

val get_current_profile : Eio.Fs.dir_ty Eio.Path.t -> app_name:string -> string
(** [get_current_profile fs ~app_name] returns the current profile name. Returns
    {!default_profile} if no profile has been set. *)

val set_current_profile :
  Eio.Fs.dir_ty Eio.Path.t -> app_name:string -> string -> unit
(** [set_current_profile fs ~app_name profile] sets the current profile. *)

val list_profiles : Eio.Fs.dir_ty Eio.Path.t -> app_name:string -> string list
(** [list_profiles fs ~app_name] returns all profiles that have sessions.
    Returns profile names sorted alphabetically. *)

(** {1 Directory Paths} *)

val base_config_dir :
  Eio.Fs.dir_ty Eio.Path.t -> app_name:string -> Eio.Fs.dir_ty Eio.Path.t
(** [base_config_dir fs ~app_name] returns the base config directory for the app
    ([~/.config/<app_name>]), creating it if needed. *)

val config_dir :
  Eio.Fs.dir_ty Eio.Path.t ->
  app_name:string ->
  ?profile:string ->
  unit ->
  Eio.Fs.dir_ty Eio.Path.t
(** [config_dir fs ~app_name ?profile ()] returns the config directory for a
    profile, creating it if needed.
    @param profile Profile name (default: current profile) *)

(** {1 Session Persistence} *)

val save :
  Eio.Fs.dir_ty Eio.Path.t -> app_name:string -> ?profile:string -> t -> unit
(** [save fs ~app_name ?profile session] saves the session.
    @param profile Profile name (default: current profile) *)

val load :
  Eio.Fs.dir_ty Eio.Path.t ->
  app_name:string ->
  ?profile:string ->
  unit ->
  t option
(** [load fs ~app_name ?profile ()] loads a saved session.
    @param profile Profile name (default: current profile) *)

val clear :
  Eio.Fs.dir_ty Eio.Path.t -> app_name:string -> ?profile:string -> unit -> unit
(** [clear fs ~app_name ?profile ()] removes the saved session.
    @param profile Profile name (default: current profile) *)

(** {1 Session Utilities} *)

val is_expired : ?leeway:Ptime.span -> t -> bool
(** [is_expired ?leeway session] returns [true] if the access token is expired.
    @param leeway Extra time buffer (default: 60 seconds) *)

val pp : t Fmt.t
(** Pretty-print a session. *)

(** {1 Conversion} *)

val of_xrpc : pds:string -> Xrpc.Types.session -> t
(** [of_xrpc ~pds xrpc_session] converts an XRPC session to our session type. *)

val to_xrpc : t -> Xrpc.Types.session
(** [to_xrpc session] converts our session type to an XRPC session. *)
