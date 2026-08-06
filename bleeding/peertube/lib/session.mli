(*---------------------------------------------------------------------------
   Copyright (c) 2025 Anil Madhavapeddy. All rights reserved.
   SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

(** PeerTube session management.

    This module handles session persistence and profile management
    for PeerTube authentication. *)

(** {1 Authentication Methods} *)

(** Authentication method for PeerTube. *)
type auth_method =
  | OAuth of {
      access_token : string;
      refresh_token : string option;
      client_id : string;
      client_secret : string;
    }

(** {1 Session Type} *)

(** A PeerTube session containing server URL and authentication info. *)
type t

(** JSON codec for sessions. *)
val jsont : t Jsont.t

(** {1 Profile Management} *)

(** Default profile name. *)
val default_profile : string

(** Application name (used for config directory). *)
val app_name : string

(** [get_current_profile fs] returns the name of the current profile. *)
val get_current_profile : Eio.Fs.dir_ty Eio.Path.t -> string

(** [set_current_profile fs profile] sets the current profile. *)
val set_current_profile : Eio.Fs.dir_ty Eio.Path.t -> string -> unit

(** [list_profiles fs] returns a list of all available profiles. *)
val list_profiles : Eio.Fs.dir_ty Eio.Path.t -> string list

(** {1 Session Operations} *)

(** [load fs ?profile ()] loads a session from disk.
    Returns [None] if no session exists. *)
val load : Eio.Fs.dir_ty Eio.Path.t -> ?profile:string -> unit -> t option

(** [save fs ?profile session] saves a session to disk. *)
val save : Eio.Fs.dir_ty Eio.Path.t -> ?profile:string -> t -> unit

(** [clear fs ?profile ()] removes the session file. *)
val clear : Eio.Fs.dir_ty Eio.Path.t -> ?profile:string -> unit -> unit

(** [create ~server_url ~auth ()] creates a new session. *)
val create : server_url:string -> auth:auth_method -> unit -> t

(** {1 Session Accessors} *)

(** [server_url t] returns the server URL. *)
val server_url : t -> string

(** [auth t] returns the authentication method. *)
val auth : t -> auth_method

(** [created_at t] returns the creation timestamp. *)
val created_at : t -> string

(** {1 Pretty Printing} *)

(** [pp ppf session] pretty-prints a session. *)
val pp : t Fmt.t
