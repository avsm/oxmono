(*---------------------------------------------------------------------------
  Copyright (c) 2025 Anil Madhavapeddy <anil@recoil.org>. All rights reserved.
  SPDX-License-Identifier: ISC
 ---------------------------------------------------------------------------*)

(** Bushel configuration and XDG paths. *)

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
(** A complete Bushel configuration. *)

(** {1 XDG Paths} *)

val xdg_config_home : unit -> string
(** [xdg_config_home ()] is the XDG configuration directory. *)

val config_dir : unit -> string
(** [config_dir ()] is the Bushel configuration directory. *)

val config_file : unit -> string
(** [config_file ()] is the default configuration file. *)

(** {1 Loading} *)

val default : unit -> t
(** [default ()] is the default configuration. *)

val load : unit -> (t, string) result
(** [load ()] is the default configuration file, or {!default} if absent. *)

val load_file : string -> (t, string) result
(** [load_file path] is the configuration in [path]. *)

val of_string : string -> (t, string) result
(** [of_string s] is the configuration encoded by TOML string [s]. *)

(** {1 Path Helpers} *)

val expand_path : string -> string
(** [expand_path path] is [path] with a leading [~] expanded. *)

val paper_thumbs_dir : t -> string
(** [paper_thumbs_dir config] is the paper thumbnail directory. *)

val contact_faces_dir : t -> string
(** [contact_faces_dir config] is the contact image directory. *)

val video_thumbs_dir : t -> string
(** [video_thumbs_dir config] is the video thumbnail directory. *)

(** {1 API Keys} *)

val read_api_key : string -> (string, string) result
(** [read_api_key path] is the trimmed API key in [path]. *)

(** {1 Pretty Printing} *)

val pp : t Fmt.t
(** [pp] prints a configuration. *)

(** {1 Initialization} *)

val default_config_toml : unit -> string
(** [default_config_toml ()] is a commented default configuration. *)

val write_default_config : ?force:bool -> unit -> (string, string) result
(** [write_default_config ()] writes the default configuration and returns its
    path. [force] permits replacing an existing file. *)
