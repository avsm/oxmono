(*---------------------------------------------------------------------------
  Copyright (c) 2025 Anil Madhavapeddy <anil@recoil.org>. All rights reserved.
  SPDX-License-Identifier: ISC
 ---------------------------------------------------------------------------*)

(** Sortal configuration management with XDG paths

    Configuration is loaded from [~/.config/sortal/config.toml] by default,
    with support for environment variable overrides via [XDG_CONFIG_HOME].

    {1 Example config.toml}

    {v
    [sync]
    remote = "ssh://server/path/to/sortal.git"
    branch = "main"
    auto_commit = true
    commit_message = "sync"
    v} *)

(** {1 Types} *)

type t = {
  sync : Gitops.Sync.Config.t;
}
(** Complete sortal configuration. *)

(** {1 XDG Paths} *)

val xdg_config_home : unit -> string
(** Return the XDG config home directory. *)

val xdg_data_home : unit -> string
(** Return the XDG data home directory. *)

val config_dir : unit -> string
(** Return the sortal config directory ([~/.config/sortal]). *)

val config_file : unit -> string
(** Return the path to the config file ([~/.config/sortal/config.toml]). *)

val data_dir : unit -> string
(** Return the sortal data directory ([~/.local/share/sortal]). *)

(** {1 Loading} *)

val default : unit -> t
(** Return the default configuration. *)

val load : unit -> (t, string) result
(** Load configuration from the default config file.
    Returns default config if file doesn't exist. *)

val load_file : string -> (t, string) result
(** Load configuration from a specific file path. *)

val of_string : string -> (t, string) result
(** Parse configuration from a TOML string. *)

(** {1 Pretty Printing} *)

val pp : t Fmt.t
(** Pretty-print the configuration. *)

(** {1 Initialization} *)

val default_config_toml : unit -> string
(** Generate a default config.toml content with comments. *)

val write_default_config : ?force:bool -> unit -> (string, string) result
(** Write a default config file to the config directory.
    Returns [Ok path] on success, or [Error msg] if the file exists
    and [force] is not set, or if writing fails. *)
