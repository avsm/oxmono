(*---------------------------------------------------------------------------
   Copyright (c) 2025 Anil Madhavapeddy. All rights reserved.
   SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

(** Cmdliner helpers for AT Protocol authentication with profile support.

    This module provides reusable command-line argument definitions and command
    builders for authentication workflows. It supports multiple profiles for
    managing multiple accounts.

    {2 Profile Support}

    The auth commands support a [--profile] flag to work with specific profiles.
    By default, the user's handle is used as the profile name when logging in.

    {[
      # Login as alice (creates profile "alice.bsky.social")
      $ myapp auth login alice.bsky.social

      # Login as bob with explicit profile name
      $ myapp auth login bob.example.com --profile work

      # Switch between profiles
      $ myapp auth profile switch alice.bsky.social

      # List all profiles
      $ myapp auth profile list
    ]} *)

open Cmdliner

(** {1 Common Arguments} *)

val identifier_arg : string Term.t
(** Required positional argument for handle or DID. *)

val password_arg : string option Term.t
(** Optional [--password] argument. *)

val pds_arg : ?default:string -> unit -> string Term.t
(** Optional [--pds] argument with configurable default. *)

val profile_arg : string option Term.t
(** Optional [--profile] argument for selecting a specific profile. *)

val user_arg : string option Term.t
(** Optional [--user] argument for viewing other users' data. *)

(** {1 Command Builders} *)

val login_cmd : app_name:string -> ?default_pds:string -> unit -> unit Cmd.t
(** [login_cmd ~app_name ?default_pds ()] builds a login subcommand. Supports
    [--profile] to specify the profile name (default: handle). *)

val logout_cmd : app_name:string -> unit -> unit Cmd.t
(** [logout_cmd ~app_name ()] builds a logout subcommand. Supports [--profile]
    to logout a specific profile. *)

val status_cmd : app_name:string -> unit -> unit Cmd.t
(** [status_cmd ~app_name ()] builds a status subcommand. Supports [--profile]
    to show a specific profile's status. *)

val profile_cmd : app_name:string -> unit -> unit Cmd.t
(** [profile_cmd ~app_name ()] builds a profile command group with [list],
    [switch], and [current] subcommands. *)

val auth_cmd : app_name:string -> ?default_pds:string -> unit -> unit Cmd.t
(** [auth_cmd ~app_name ?default_pds ()] builds a complete auth command group
    with login, logout, status, and profile subcommands. *)

(** {1 Helper Functions} *)

val with_session :
  app_name:string ->
  ?profile:string ->
  (Eio.Fs.dir_ty Eio.Path.t -> Xrpc_auth_session.t -> 'a) ->
  < fs : Eio.Fs.dir_ty Eio.Path.t ; .. > ->
  'a
(** [with_session ~app_name ?profile f env] loads the session and calls
    [f fs session]. Prints an error and exits if not logged in.
    @param profile Profile to load (default: current profile) *)

val resolve_did : Xrpc_auth_client.t -> string option -> string
(** [resolve_did client user_opt] resolves an optional user argument to a DID.
    If [None], returns the logged-in user's DID. If [Some handle], resolves the
    handle to a DID. *)
