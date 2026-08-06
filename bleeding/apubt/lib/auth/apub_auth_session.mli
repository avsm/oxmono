(*---------------------------------------------------------------------------
   Copyright (c) 2025 Anil Madhavapeddy. All rights reserved.
   SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

(** Session management for ActivityPub CLI with profile support.

    This module provides session persistence for ActivityPub actors. Sessions
    support two authentication methods:
    - HTTP signatures (key_id + private_key_pem) for ActivityPub federation
    - OAuth (oauth_* fields) for Mastodon REST API access

    Sessions are stored in profile-specific directories under
    [~/.config/<app_name>/profiles/<profile>/session.json].

    {2 Directory Structure}

    {v
    ~/.config/<app_name>/
      config.json           # Stores current_profile setting
      profiles/
        default/
          session.json      # Session for "default" profile
        alice@mastodon.social/
          session.json      # Session for "alice@mastodon.social" profile
    v}

    {2 Profile Usage}

    Profiles allow multiple ActivityPub actors to be configured simultaneously.
    The current profile is used by default when no profile is specified.

    {[
      (* Setup an actor with HTTP signatures *)
      let session =
        Apub_auth_session.create ~actor_uri:"https://example.com/users/alice"
          ~key_id:"https://example.com/users/alice#main-key" ~private_key_pem
      in
      Apub_auth_session.save fs ~app_name:"apub" ~profile:"alice@example.com"
        session

      (* Or login via OAuth *)
      let session =
        Apub_auth_session.create_oauth ~actor_uri:"https://mastodon.social/@alice"
          ~instance:"mastodon.social" ~access_token ~client_id ~client_secret
      in
      Apub_auth_session.save fs ~app_name:"apub" ~profile:"alice@mastodon.social"
        session
    ]} *)

(** {1 Session Type} *)

type t = {
  actor_uri : string;
  (* HTTP Signature auth (optional for OAuth-only sessions) *)
  key_id : string option;
  private_key_pem : string option;
  (* Mastodon OAuth (optional for signature-only sessions) *)
  oauth_instance : string option;
  oauth_access_token : string option;
  oauth_client_id : string option;
  oauth_client_secret : string option;
  created_at : string;
}
(** Saved session data containing actor credentials. A session can have:
    - Signature auth only: key_id + private_key_pem
    - OAuth only: oauth_* fields
    - Both: for hybrid authentication *)

val jsont : t Jsont.t
(** JSON codec for sessions. *)

(** {1 Session Creation} *)

val create : actor_uri:string -> key_id:string -> private_key_pem:string -> t
(** [create ~actor_uri ~key_id ~private_key_pem] creates a new signature-based
    session with the current timestamp. *)

val create_oauth :
  actor_uri:string ->
  instance:string ->
  access_token:string ->
  client_id:string ->
  client_secret:string ->
  t
(** [create_oauth ~actor_uri ~instance ~access_token ~client_id ~client_secret]
    creates a new OAuth-based session with the current timestamp. *)

val add_oauth :
  t ->
  instance:string ->
  access_token:string ->
  client_id:string ->
  client_secret:string ->
  t
(** [add_oauth session ~instance ~access_token ~client_id ~client_secret]
    adds OAuth credentials to an existing session for hybrid auth. *)

val has_signature : t -> bool
(** [has_signature session] returns true if the session has HTTP signature
    credentials (key_id and private_key_pem). *)

val has_oauth : t -> bool
(** [has_oauth session] returns true if the session has OAuth credentials
    (oauth_access_token and oauth_instance). *)

val profile_name_of_actor_uri : string -> string
(** [profile_name_of_actor_uri uri] extracts a profile name from an actor URI.
    For example, [https://example.com/users/alice] becomes [alice@example.com]. *)

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

val pp : t Fmt.t
(** Pretty-print a session (does not print the private key). *)
