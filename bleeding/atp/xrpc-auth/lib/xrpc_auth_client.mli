(*---------------------------------------------------------------------------
   Copyright (c) 2025 Anil Madhavapeddy. All rights reserved.
   SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

(** Authenticated XRPC client with session persistence and profile support.

    This module provides a high-level client that automatically saves sessions
    to disk and refreshes tokens when needed. Sessions are stored in
    profile-specific directories.

    {2 Profile Behavior}

    When logging in, if no profile is specified, the user's handle becomes the
    profile name. This allows multiple accounts to be logged in simultaneously,
    each with their own session file.

    {[
      (* Login as alice - session saved to profiles/alice.bsky.social/ *)
      let api = Xrpc_auth.Client.create ~sw ~env ~app_name:"bsky"
        ~pds:"https://bsky.social" () in
      Xrpc_auth.Client.login api
        ~identifier:"alice.bsky.social" ~password:"..."

      (* Or explicitly specify a profile *)
      let api = Xrpc_auth.Client.create ~sw ~env ~app_name:"bsky"
        ~profile:"work" ~pds:"https://bsky.social" () in
    ]} *)

type t
(** Authenticated client state. *)

val create :
  sw:Eio.Switch.t ->
  env:
    < clock : _ Eio.Time.clock
    ; net : _ Eio.Net.t
    ; fs : Eio.Fs.dir_ty Eio.Path.t
    ; .. > ->
  app_name:string ->
  ?profile:string ->
  pds:string ->
  unit ->
  t
(** [create ~sw ~env ~app_name ?profile ~pds ()] creates a new authenticated
    client.

    Sessions are automatically saved to the profile directory when they are
    created or refreshed.

    @param sw Eio switch for resource management
    @param env Eio environment capabilities
    @param app_name Application name for config directory
    @param profile Profile name (default: user's handle after login)
    @param pds Base URL of the PDS (e.g., ["https://bsky.social"]) *)

(** {1 Authentication} *)

val login : t -> identifier:string -> password:string -> unit
(** [login client ~identifier ~password] authenticates with the PDS. The session
    is automatically saved to the profile directory. *)

val resume : t -> session:Xrpc_auth_session.t -> unit
(** [resume client ~session] resumes from a saved session. Refreshes the access
    token if needed. *)

val logout : t -> unit
(** [logout client] logs out and clears the session from disk. *)

val get_session : t -> Xrpc_auth_session.t option
(** [get_session client] returns the current session, if authenticated. *)

val is_logged_in : t -> bool
(** [is_logged_in client] returns [true] if there's an active session. *)

(** {1 Client Access} *)

val get_client : t -> Xrpc.Client.t
(** [get_client client] returns the underlying XRPC client.
    @raise Failure if not logged in *)

val get_did : t -> string
(** [get_did client] returns the DID of the authenticated user.
    @raise Failure if not logged in *)

val get_pds : t -> string
(** [get_pds client] returns the PDS URL. *)

val get_app_name : t -> string
(** [get_app_name client] returns the application name. *)

val get_profile : t -> string option
(** [get_profile client] returns the current profile name, if set. *)

val get_fs : t -> Eio.Fs.dir_ty Eio.Path.t
(** [get_fs client] returns the filesystem capability. *)

(** {1 Auxiliary Clients} *)

val make_client : t -> service:string -> Xrpc.Client.t
(** [make_client t ~service] creates a new XRPC client for a different service.
    Useful for connecting to knot servers, spindles, etc. *)
