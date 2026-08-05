(*---------------------------------------------------------------------------
  Copyright (c) 2025 Anil Madhavapeddy <anil@recoil.org>. All rights reserved.
  SPDX-License-Identifier: ISC
 ---------------------------------------------------------------------------*)

(** Bushel configuration management with XDG paths

    Configuration is loaded from [~/.config/bushel/config.toml] by default,
    with support for environment variable overrides via [XDG_CONFIG_HOME].

    {1 Example config.toml}

    {v
    [data]
    local_dir = "/path/to/bushel/data"

    [images]
    images_dir = "/path/to/images"
    images_output_dir = "/path/to/images-web"
    paper_thumbs = "papers"
    contact_faces = "faces"
    video_thumbs = "videos"

    [papers]
    pdfs_dir = "/path/to/paper-pdfs"

    [peertube]
    [[peertube.servers]]
    name = "crank"
    endpoint = "https://crank.recoil.org"

    [[peertube.servers]]
    name = "talks"
    endpoint = "https://talks.example.com"

    [zotero]
    translation_server = "http://localhost:1969"

    [sync]
    remote = "ssh://server/path/to/bushel.git"
    branch = "main"
    auto_commit = true
    commit_message = "sync"

    [images_sync]
    remote = "ssh://server/path/to/images.git"
    branch = "main"
    auto_commit = true
    commit_message = "images sync"
    v}
*)

(** {1 Types} *)

type peertube_server = {
  name : string;
  endpoint : string;
}
(** A PeerTube server configuration. *)

type t = {
  data_dir : string;
  images_dir : string;
  images_output_dir : string;
  paper_thumbs_subdir : string;
  contact_faces_subdir : string;
  video_thumbs_subdir : string;
  paper_pdfs_dir : string;
  peertube_servers : peertube_server list;
  zotero_translation_server : string;
  sync : Gitops.Sync.Config.t;
  images_sync : Gitops.Sync.Config.t;
}
(** Complete bushel configuration. *)

(** {1 XDG Paths} *)

val xdg_config_home : unit -> string
(** Return the XDG config home directory. *)

val config_dir : unit -> string
(** Return the bushel config directory ([~/.config/bushel]). *)

val config_file : unit -> string
(** Return the path to the config file ([~/.config/bushel/config.toml]). *)

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

(** {1 Path Helpers} *)

val expand_path : string -> string
(** Expand [~] in paths to the home directory. *)

val paper_thumbs_dir : t -> string
(** Full path to paper thumbnails directory. *)

val contact_faces_dir : t -> string
(** Full path to contact faces directory. *)

val video_thumbs_dir : t -> string
(** Full path to video thumbnails directory. *)

(** {1 API Keys} *)

val read_api_key : string -> (string, string) result
(** Read an API key from a file. *)

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
