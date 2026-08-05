(*---------------------------------------------------------------------------
   Copyright (c) 2025 Anil Madhavapeddy. All rights reserved.
   SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

(** CLI commands for ActivityPub authentication management.

    Provides cmdliner commands for setting up actor credentials, managing
    profiles, and checking authentication status.

    {2 Command Structure}

    {v
    apub auth
      setup <actor-uri>     Import key for an actor
      logout                Clear saved credentials
      status                Show authentication status
      profile
        list                List all profiles
        switch <name>       Switch to profile
        current             Show current profile name
    v}

    {2 Usage}

    {[
      (* Add auth commands to your CLI *)
      let cmds =
        [
          Apub_auth_cmd.auth_cmd ~app_name:"apub" ();
          (* ... other commands ... *)
        ]
    ]} *)

(** {1 Command Groups} *)

val auth_cmd : app_name:string -> unit -> unit Cmdliner.Cmd.t
(** [auth_cmd ~app_name ()] creates the auth command group with all
    subcommands. *)

val setup_cmd : app_name:string -> unit -> unit Cmdliner.Cmd.t
(** [setup_cmd ~app_name ()] creates the setup command for importing keys. *)

val logout_cmd : app_name:string -> unit -> unit Cmdliner.Cmd.t
(** [logout_cmd ~app_name ()] creates the logout command. *)

val status_cmd : app_name:string -> unit -> unit Cmdliner.Cmd.t
(** [status_cmd ~app_name ()] creates the status command. *)

val profile_cmd : app_name:string -> unit -> unit Cmdliner.Cmd.t
(** [profile_cmd ~app_name ()] creates the profile command group. *)

(** {1 Helpers} *)

val with_session :
  app_name:string ->
  ?profile:string ->
  (Eio.Fs.dir_ty Eio.Path.t -> Apub_auth_session.t -> 'a) ->
  < fs : Eio.Fs.dir_ty Eio.Path.t ; .. > ->
  'a
(** [with_session ~app_name ?profile f env] loads the session and calls [f]
    with it, or exits with an error if no session is found. *)

(** {1 Cmdliner Arguments} *)

val profile_arg : string option Cmdliner.Term.t
(** Common profile argument for commands. *)
