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

type releases = {
  github_user : string option;
      (** The GitHub login whose releases count as the author's. *)
  github : string list;  (** GitHub repositories to scan, as [org/name]. *)
  tangled : string list;
      (** Tangled repositories to scan, as [handle/name]. *)
  registries : string list;
      (** The registries a release is attached to, as ecosyste.ms names them. *)
  projects : (string * string) list;
      (** Repository to the slug of the bushel project it serves. *)
}
(** The releases section of the configuration. *)

type http_embeddings = {
  url : string;
  model : string;
  dimensions : int;
  api_key_file : string option;
}
(** An embedding endpoint and the path to its optional API key. *)

type mlx_embeddings = {
  model_dir : string;
  dimensions : int;
  max_tokens : int;
  query_instruction : string;
}
(** A local MLX model and its input limits. *)

type embeddings = Http of http_embeddings | Mlx of mlx_embeddings
(** The shared embedding backend. A missing TOML backend selects HTTP. *)

val default_query_instruction : string
(** [default_query_instruction] is the instruction used for MLX search queries
    unless configured explicitly. Documents are embedded without it. *)

val validate_embeddings : embeddings -> (unit, string) result
(** [validate_embeddings settings] checks the backend configuration. HTTP
    requires a nonempty model, dimensions in [1..4096] and an HTTP(S) URL with
    a host and no credentials, query or fragment. An API key path requires
    HTTPS. MLX requires a nonempty model directory, dimensions in [32..4096]
    and [max_tokens] in [32..32768]. Its default token limit is [8192].
    The query instruction is at most [8192] bytes with no NUL. An empty
    instruction disables the query prefix. Key files and model directories
    need not exist yet. Their contents are checked at use. *)

val default_registries : string list
(** [default_registries] is the registries used when none are configured. *)

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
  releases : releases;
  embeddings : embeddings option;
  image_embeddings : mlx_embeddings option;
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

val config_codec : t Tomlt.t
(** [config_codec] encodes and decodes the public Bushel configuration. *)

val default : unit -> t
(** [default ()] is the default configuration. *)

val load : unit -> (t, string) result
(** [load ()] is the default configuration file, or {!default} if absent. *)

val load_file : string -> (t, string) result
(** [load_file path] is the configuration in [path]. Relative API key and
    model directory paths resolve against the directory of this config file,
    producing absolute paths. Other configured paths retain their existing
    semantics. File I/O is synchronous. *)

val of_string : string -> (t, string) result
(** [of_string s] is the configuration encoded by TOML string [s]. Relative
    API key and model directory paths remain relative because no config file
    path is supplied. *)

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

val configure_embeddings :
  ?file:string -> embeddings -> (string, string) result
(** [configure_embeddings settings] updates the embeddings section of [file],
    which defaults to {!config_file}, and returns the written path. It expands
    leading [~] in the config, API key and model directory paths. Relative key
    and model paths resolve against the current working directory and are
    stored as absolute paths. Existing unrelated and unknown TOML values are
    preserved. Standard section layouts retain unrelated comments.
    Missing parent directories and files are created with permissions
    [0700] and [0600]. Existing files are atomically replaced and keep their
    permissions. Non-regular config files are refused. No API key is read.
    Writers serialize within the process and use a persistent [.lock] sidecar.
    An overlapping update from another process returns an error without writing.
    File I/O and locking are synchronous. Eio callers should use a system thread. *)

val default_image_query_instruction : string
(** [default_image_query_instruction] is the default visual retrieval prompt. *)

val configure_image_embeddings :
  ?file:string -> mlx_embeddings -> (string, string) result
(** [configure_image_embeddings settings] atomically updates [image_embeddings]
    while preserving text embedding settings and unrelated configuration. Paths
    follow [configure_embeddings]. *)

val validate_image_embeddings : mlx_embeddings -> (unit, string) result
(** [validate_image_embeddings settings] checks local vision model settings.
    Dimensions must be in [64..4096]. Other limits match MLX text settings. *)
