(*---------------------------------------------------------------------------
   Copyright (c) 2025 Anil Madhavapeddy. All rights reserved.
   SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

(** Authenticated Immich client with session persistence and profile support.

    This module provides a high-level client that wraps the generated
    {!Immich} module with authentication support. Sessions are stored in
    profile-specific directories.

    {2 Authentication Methods}

    {b API Key} (recommended for CLI):
    {[
      let t = Client.login_api_key ~sw ~env
        ~server_url:"https://immich.example.com"
        ~api_key:"your-api-key"
        ~key_name:(Some "cli")
        () in
      let albums = Immich.Album.get_all_albums (Client.client t) ()
    ]}

    {b Password} (for interactive use):
    {[
      let t = Client.login_password ~sw ~env
        ~server_url:"https://immich.example.com"
        ~email:"user@example.com"
        ~password:"secret"
        () in
      ...
    ]}

    {2 Resuming Sessions}

    {[
      match Session.load fs () with
      | Some session ->
          let t = Client.resume ~sw ~env ~session () in
          ...
      | None ->
          Fmt.epr "Not logged in@."
    ]} *)

type t
(** Authenticated client state. *)

(** {1 Authentication} *)

val login_api_key :
  sw:Eio.Switch.t ->
  env:< clock : _ Eio.Time.clock
      ; net : _ Eio.Net.t
      ; fs : Eio.Fs.dir_ty Eio.Path.t
      ; .. > ->
  ?requests_config:Requests.Cmd.config ->
  ?profile:string ->
  server_url:string ->
  api_key:string ->
  ?key_name:string ->
  unit ->
  t
(** [login_api_key ~sw ~env ?requests_config ?profile ~server_url ~api_key ?key_name ()]
    authenticates using an API key. The API key is sent in the [x-api-key]
    header for all requests.

    The [server_url] can be either the full API URL (e.g., [https://example.com/api])
    or the base server URL (e.g., [https://example.com]). If the base URL is
    provided, the client automatically discovers the API endpoint via
    [.well-known/immich].

    @param requests_config Optional Requests.Cmd.config for HTTP settings
    @param profile Profile name (default: "default")
    @param key_name Optional name for the API key (for display purposes) *)

val login_password :
  sw:Eio.Switch.t ->
  env:< clock : _ Eio.Time.clock
      ; net : _ Eio.Net.t
      ; fs : Eio.Fs.dir_ty Eio.Path.t
      ; .. > ->
  ?requests_config:Requests.Cmd.config ->
  ?profile:string ->
  server_url:string ->
  email:string ->
  password:string ->
  unit ->
  t
(** [login_password ~sw ~env ?requests_config ?profile ~server_url ~email ~password ()]
    authenticates using email and password. Returns a JWT access token.

    @param requests_config Optional Requests.Cmd.config for HTTP settings
    @param profile Profile name (default: email address) *)

val resume :
  sw:Eio.Switch.t ->
  env:< clock : _ Eio.Time.clock
      ; net : _ Eio.Net.t
      ; fs : Eio.Fs.dir_ty Eio.Path.t
      ; .. > ->
  ?requests_config:Requests.Cmd.config ->
  ?profile:string ->
  session:Session.t ->
  unit ->
  t
(** [resume ~sw ~env ?requests_config ?profile ~session ()] resumes from a saved session.

    @param requests_config Optional Requests.Cmd.config for HTTP settings
    @raise Failure if the JWT session is expired *)

val logout : t -> unit
(** [logout t] clears the session from disk. *)

(** {1 Client Access} *)

val client : t -> Immich.t
(** [client t] returns the underlying Immich client for API calls. *)

val session : t -> Session.t
(** [session t] returns the current session. *)

val profile : t -> string option
(** [profile t] returns the current profile name, if set. *)

val fs : t -> Eio.Fs.dir_ty Eio.Path.t
(** [fs t] returns the filesystem capability. *)

val is_valid : t -> bool
(** [is_valid t] returns [true] if the session is still valid by calling
    the validate endpoint. *)

(** {1 URL Resolution} *)

val resolve_api_url : session:Requests.t -> string -> string
(** [resolve_api_url ~session base_url] resolves the actual API URL.

    If [base_url] already ends with [/api], returns it unchanged.
    Otherwise, tries to fetch [<base_url>/.well-known/immich] to discover
    the API endpoint. Falls back to [<base_url>/api] if discovery fails.

    This function is called automatically by {!login_api_key} and
    {!login_password}, but is exposed for advanced use cases. *)
