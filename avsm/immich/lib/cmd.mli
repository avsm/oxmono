(*---------------------------------------------------------------------------
   Copyright (c) 2025 Anil Madhavapeddy. All rights reserved.
   SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

(** Cmdliner helpers for Immich CLI authentication.

    This module provides reusable command-line argument definitions and command
    builders for authentication workflows. It supports multiple profiles for
    managing multiple server connections.

    {2 Usage}

    {[
      (* In your main.ml *)
      Eio_main.run @@ fun env ->
      let fs = env#fs in
      Cmd.group (Cmd.info "immich")
        [ Cmd.auth_cmd env fs
        ; (* other commands *)
        ]
      |> Cmd.eval_exn
    ]}

    {2 Environment Variables}

    - [IMMICH_SERVER]: Default server URL
    - [IMMICH_API_KEY]: Default API key

    {2 Logging}

    Uses Logs_cli for verbosity control:
    - [-v] or [--verbose]: Info level logging
    - [-v -v] or [--verbosity=debug]: Debug level logging
    - [--verbose-http]: Enable verbose HTTP protocol logging *)

open Cmdliner

(** {1 Common Arguments} *)

val server_arg : string Term.t
(** Required positional argument for server URL (also reads [IMMICH_SERVER]). *)

val server_opt : string option Term.t
(** Optional [--server] argument (also reads [IMMICH_SERVER]). *)

val api_key_arg : string option Term.t
(** Optional [--api-key] argument (also reads [IMMICH_API_KEY]). *)

val email_arg : string option Term.t
(** Optional [--email] argument for password authentication. *)

val password_arg : string option Term.t
(** Optional [--password] argument. *)

val profile_arg : string option Term.t
(** Optional [--profile] argument for selecting a specific profile. *)

val key_name_arg : string option Term.t
(** Optional [--name] argument for naming API keys. *)

(** {1 Logging and Configuration} *)

val setup_logging : (Fmt.style_renderer option * Logs.level option) Term.t
(** Term that collects logging options ([-v], [--color], etc.).
    Use with [setup_logging_with_config] to apply logging after parsing. *)

val setup_logging_with_config :
  Fmt.style_renderer option ->
  Logs.level option ->
  Requests.Cmd.config ->
  unit
(** [setup_logging_with_config style_renderer level config] sets up logging
    with the given options. Extracts [--verbose-http] from the requests config.
    Call this at the start of command execution. *)

val requests_config_term : Eio.Fs.dir_ty Eio.Path.t -> Requests.Cmd.config Term.t
(** Term for HTTP request configuration (timeouts, retries, proxy, etc.). *)

(** {1 Commands}

    Commands take an [env] parameter from the outer [Eio_main.run] context,
    and an [fs] path for building cmdliner terms. *)

val auth_cmd :
  < fs : Eio.Fs.dir_ty Eio.Path.t
  ; clock : _ Eio.Time.clock
  ; net : _ Eio.Net.t
  ; .. > ->
  Eio.Fs.dir_ty Eio.Path.t ->
  unit Cmd.t
(** Complete auth command group combining login, logout, status, and profile. *)

(** {1 Helper Functions} *)

val with_session :
  ?profile:string ->
  (Eio.Fs.dir_ty Eio.Path.t -> Session.t -> 'a) ->
  < fs : Eio.Fs.dir_ty Eio.Path.t ; .. > ->
  'a
(** [with_session ?profile f env] loads the session and calls
    [f fs session]. Prints an error and exits if not logged in.
    @param profile Profile to load (default: current profile) *)

val with_client :
  ?requests_config:Requests.Cmd.config ->
  ?profile:string ->
  (Eio.Fs.dir_ty Eio.Path.t -> Client.t -> 'a) ->
  < fs : Eio.Fs.dir_ty Eio.Path.t
  ; clock : _ Eio.Time.clock
  ; net : _ Eio.Net.t
  ; .. > ->
  'a
(** [with_client ?requests_config ?profile f env] loads the session, creates a client, and calls
    [f fs client]. Prints an error and exits if not logged in.
    @param requests_config HTTP request configuration
    @param profile Profile to load (default: current profile) *)

(** {1 Profile Configuration for External Programs}

    These types and functions allow other programs to easily use Immich profiles
    that were set up with the immich CLI. This provides a single cmdliner term
    that bundles all the necessary configuration.

    {2 Example Usage}

    {[
      (* In your sortal/bin/main.ml *)
      open Cmdliner

      let my_cmd env fs =
        let doc = "My command using Immich" in
        let info = Cmd.info "my-command" ~doc in
        let action config =
          let open Immich_auth.Cmd.Profile_config in
          setup_logging config;
          Immich_auth.Cmd.with_client
            ~requests_config:(requests_config config)
            ?profile:(profile config)
            (fun _fs client ->
              let api = Immich_auth.Client.client client in
              (* Use the Immich API here *)
              ()
            ) env
        in
        Cmd.v info Term.(const action $ Immich_auth.Cmd.profile_config_term fs)
    ]} *)

(** Configuration for using an Immich profile. *)
module Profile_config : sig
  type t
  (** Bundled configuration for Immich profile access. *)

  val style_renderer : t -> Fmt.style_renderer option
  (** Terminal style renderer setting. *)

  val log_level : t -> Logs.level option
  (** Logging level. *)

  val requests_config : t -> Requests.Cmd.config
  (** HTTP request configuration. *)

  val profile : t -> string option
  (** Selected profile name, if any. *)

  val setup_logging : t -> unit
  (** [setup_logging config] initializes logging with the bundled settings.
      Call this at the start of your command execution. *)
end

val profile_config_term : Eio.Fs.dir_ty Eio.Path.t -> Profile_config.t Term.t
(** Cmdliner term that collects all configuration needed to use an Immich profile.

    Combines:
    - Logging options ([-v], [--color], etc.)
    - HTTP configuration (timeouts, retries, proxy, [--verbose-http])
    - Profile selection ([--profile])

    Use with {!with_client} to create an authenticated client. *)
