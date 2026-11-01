(*---------------------------------------------------------------------------
   Copyright (c) 2025 Anil Madhavapeddy. All rights reserved.
   SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

(** Configuration handling for Tangled CLI using TOML and XDG paths. *)

type t = { handle : string; pds : string }
(** Configuration type containing handle and PDS URL. *)

val tomlt : t Tomlt.t
(** TOML codec for configuration (excludes password). *)

val pp : t Fmt.t
(** Pretty printer for configuration. *)

val load : Eio.Fs.dir_ty Eio.Path.t -> Xdge.t -> t option
(** [load fs xdg] loads configuration from the TOML config file. Returns [None]
    if no config file exists. *)

val save : Eio.Fs.dir_ty Eio.Path.t -> Xdge.t -> t -> unit
(** [save fs xdg config] saves configuration to the TOML config file. *)

val load_password : Eio.Fs.dir_ty Eio.Path.t -> Xdge.t -> string option
(** [load_password fs xdg] loads the password from the separate password file.
    Returns [None] if no password file exists. *)

val save_password : Eio.Fs.dir_ty Eio.Path.t -> Xdge.t -> string -> unit
(** [save_password fs xdg password] saves the password to a separate file with
    0o600 permissions for security. *)

val clear_password : Eio.Fs.dir_ty Eio.Path.t -> Xdge.t -> unit
(** [clear_password fs xdg] removes the password file. *)
