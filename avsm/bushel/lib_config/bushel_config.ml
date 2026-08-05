(*---------------------------------------------------------------------------
  Copyright (c) 2025 Anil Madhavapeddy <anil@recoil.org>. All rights reserved.
  SPDX-License-Identifier: ISC
 ---------------------------------------------------------------------------*)

(** Bushel configuration management with XDG paths *)

(** {1 Types} *)

type peertube_server = {
  name : string;
  endpoint : string;
}

type t = {
  (* Data paths *)
  data_dir : string;

  (* Image configuration *)
  images_dir : string;
  images_output_dir : string;
  paper_thumbs_subdir : string;
  contact_faces_subdir : string;
  video_thumbs_subdir : string;

  (* Paper PDFs *)
  paper_pdfs_dir : string;

  (* PeerTube *)
  peertube_servers : peertube_server list;

  (* Zotero *)
  zotero_translation_server : string;

  (* Git sync *)
  sync : Gitops.Sync.Config.t;
  images_sync : Gitops.Sync.Config.t;
}

(** {1 XDG Paths} *)

let xdg_config_home () =
  match Sys.getenv_opt "XDG_CONFIG_HOME" with
  | Some dir -> dir
  | None ->
    match Sys.getenv_opt "HOME" with
    | Some home -> Filename.concat home ".config"
    | None -> ".config"

let config_dir () = Filename.concat (xdg_config_home ()) "bushel"
let config_file () = Filename.concat (config_dir ()) "config.toml"

(** {1 Default Configuration} *)

let default () =
  let home = Sys.getenv_opt "HOME" |> Option.value ~default:"." in
  {
    data_dir = Filename.concat home "bushel/data";
    images_dir = Filename.concat home "bushel/images";
    images_output_dir = Filename.concat home "bushel/images-web";
    paper_thumbs_subdir = "papers";
    contact_faces_subdir = "faces";
    video_thumbs_subdir = "videos";
    paper_pdfs_dir = Filename.concat home "bushel/pdfs";
    peertube_servers = [];
    zotero_translation_server = "http://localhost:1969";
    sync = Gitops.Sync.Config.default;
    images_sync = Gitops.Sync.Config.default;
  }

(** {1 Path Helpers} *)

let expand_path path =
  if String.length path > 0 && path.[0] = '~' then
    match Sys.getenv_opt "HOME" with
    | Some home -> home ^ String.sub path 1 (String.length path - 1)
    | None -> path
  else path

let paper_thumbs_dir t = Filename.concat t.images_dir t.paper_thumbs_subdir
let contact_faces_dir t = Filename.concat t.images_dir t.contact_faces_subdir
let video_thumbs_dir t = Filename.concat t.images_dir t.video_thumbs_subdir

(** {1 Tomlt Codecs} *)

let peertube_server_codec =
  let open Tomlt in
  let open Tomlt.Table in
  obj (fun name endpoint -> { name; endpoint })
  |> mem "name" string ~enc:(fun s -> s.name)
  |> mem "endpoint" string ~enc:(fun s -> s.endpoint)
  |> finish

let data_codec ~default =
  let open Tomlt in
  let open Tomlt.Table in
  obj (fun local_dir -> local_dir)
  |> mem "local_dir" string ~dec_absent:default.data_dir ~enc:Fun.id
  |> finish

let images_codec ~default =
  let open Tomlt in
  let open Tomlt.Table in
  obj (fun images_dir images_output_dir paper_thumbs contact_faces video_thumbs ->
    (images_dir, images_output_dir, paper_thumbs, contact_faces, video_thumbs))
  |> mem "images_dir" string ~dec_absent:default.images_dir
       ~enc:(fun (d,_,_,_,_) -> d)
  |> mem "images_output_dir" string ~dec_absent:default.images_output_dir
       ~enc:(fun (_,o,_,_,_) -> o)
  |> mem "paper_thumbs" string ~dec_absent:default.paper_thumbs_subdir
       ~enc:(fun (_,_,p,_,_) -> p)
  |> mem "contact_faces" string ~dec_absent:default.contact_faces_subdir
       ~enc:(fun (_,_,_,c,_) -> c)
  |> mem "video_thumbs" string ~dec_absent:default.video_thumbs_subdir
       ~enc:(fun (_,_,_,_,v) -> v)
  |> finish

let papers_codec ~default =
  let open Tomlt in
  let open Tomlt.Table in
  obj Fun.id
  |> mem "pdfs_dir" string ~dec_absent:default.paper_pdfs_dir ~enc:Fun.id
  |> finish

let peertube_codec =
  let open Tomlt in
  let open Tomlt.Table in
  obj Fun.id
  |> mem "servers" (list peertube_server_codec) ~dec_absent:[] ~enc:Fun.id
  |> finish

let zotero_codec ~default =
  let open Tomlt in
  let open Tomlt.Table in
  obj Fun.id
  |> mem "translation_server" string ~dec_absent:default.zotero_translation_server
       ~enc:Fun.id
  |> finish

let config_codec =
  let default = default () in
  let open Tomlt.Table in
  obj (fun data_dir images papers peertube zotero sync images_sync ->
    let (images_dir, images_output_dir, paper_thumbs_subdir,
         contact_faces_subdir, video_thumbs_subdir) = images in
    {
      data_dir = expand_path data_dir;
      images_dir = expand_path images_dir;
      images_output_dir = expand_path images_output_dir;
      paper_thumbs_subdir;
      contact_faces_subdir;
      video_thumbs_subdir;
      paper_pdfs_dir = expand_path papers;
      peertube_servers = peertube;
      zotero_translation_server = zotero;
      sync;
      images_sync;
    })
  |> mem "data" (data_codec ~default) ~dec_absent:default.data_dir
       ~enc:(fun c -> c.data_dir)
  |> mem "images" (images_codec ~default)
       ~dec_absent:(default.images_dir, default.images_output_dir,
                    default.paper_thumbs_subdir,
                    default.contact_faces_subdir, default.video_thumbs_subdir)
       ~enc:(fun c -> (c.images_dir, c.images_output_dir,
                       c.paper_thumbs_subdir, c.contact_faces_subdir,
                       c.video_thumbs_subdir))
  |> mem "papers" (papers_codec ~default) ~dec_absent:default.paper_pdfs_dir
       ~enc:(fun c -> c.paper_pdfs_dir)
  |> mem "peertube" peertube_codec ~dec_absent:[]
       ~enc:(fun c -> c.peertube_servers)
  |> mem "zotero" (zotero_codec ~default)
       ~dec_absent:default.zotero_translation_server
       ~enc:(fun c -> c.zotero_translation_server)
  |> mem "sync" Gitops.Sync.Config.codec
       ~dec_absent:Gitops.Sync.Config.default
       ~enc:(fun c -> c.sync)
  |> mem "images_sync" Gitops.Sync.Config.codec
       ~dec_absent:Gitops.Sync.Config.default
       ~enc:(fun c -> c.images_sync)
  |> finish

(** {1 Loading} *)

let of_string s =
  match Tomlt_bytesrw.decode_string config_codec s with
  | Ok config -> Ok config
  | Error e -> Error (Tomlt.Toml.Error.to_string e)

let load_file path =
  try
    let ic = open_in path in
    let content = really_input_string ic (in_channel_length ic) in
    close_in ic;
    of_string content
  with
  | Sys_error msg -> Error (Printf.sprintf "Failed to read config: %s" msg)

let load () =
  let path = config_file () in
  if Sys.file_exists path then
    load_file path
  else
    Ok (default ())

(** {1 API Key Loading} *)

let read_api_key path =
  let path = expand_path path in
  try
    let ic = open_in path in
    let key = input_line ic |> String.trim in
    close_in ic;
    Ok key
  with
  | Sys_error msg -> Error (Printf.sprintf "Failed to read API key from %s: %s" path msg)
  | End_of_file -> Error (Printf.sprintf "API key file %s is empty" path)

(** {1 Pretty Printing} *)

let pp ppf t =
  let open Fmt in
  pf ppf "@[<v>";
  pf ppf "%a:@," (styled `Bold string) "Bushel Configuration";
  pf ppf "  data_dir: %s@," t.data_dir;
  pf ppf "  @[<v 2>images:@,";
  pf ppf "images_dir: %s@," t.images_dir;
  pf ppf "images_output_dir: %s@," t.images_output_dir;
  pf ppf "@]";
  pf ppf "  paper_pdfs: %s@," t.paper_pdfs_dir;
  pf ppf "  peertube servers: %d@," (List.length t.peertube_servers);
  pf ppf "  zotero: %s@," t.zotero_translation_server;
  pf ppf "  sync remote: %s@," t.sync.Gitops.Sync.Config.remote;
  pf ppf "  images_sync remote: %s@," t.images_sync.Gitops.Sync.Config.remote;
  pf ppf "@]"

(** {1 Default Config Generation} *)

let default_config_toml () =
  let home = Sys.getenv_opt "HOME" |> Option.value ~default:"~" in
  Printf.sprintf {|# Bushel Configuration
# Generated by: bushel init

# Data directory containing your bushel entries
[data]
local_dir = "%s/bushel/data"

# Image configuration
# images_dir is a git-tracked repository of original images
# images_output_dir is where srcsetter writes processed variants (not git-tracked)
[images]
images_dir = "%s/bushel/images"
images_output_dir = "%s/bushel/images-web"

# Subdirectories within images_dir for generated thumbnails
paper_thumbs = "papers"
contact_faces = "faces"
video_thumbs = "videos"

# Paper PDFs directory (for thumbnail generation)
[papers]
pdfs_dir = "%s/bushel/pdfs"

# PeerTube servers for video thumbnails
# Add servers as [[peertube.servers]] entries
[peertube]
# Example:
# [[peertube.servers]]
# name = "tilvids"
# endpoint = "https://tilvids.com"
#
# [[peertube.servers]]
# name = "spectra"
# endpoint = "https://spectra.video"

# Zotero Translation Server for DOI resolution
# Run locally: docker run -p 1969:1969 zotero/translation-server
[zotero]
translation_server = "http://localhost:1969"

# Git sync configuration for bushel data
[sync]
remote = ""
branch = "main"
auto_commit = true
commit_message = "sync"

# Git sync configuration for images repository
[images_sync]
remote = ""
branch = "main"
auto_commit = true
commit_message = "images sync"
|} home home home home

let write_default_config ?(force=false) () =
  let dir = config_dir () in
  let path = config_file () in

  (* Check if config already exists *)
  if Sys.file_exists path && not force then
    Error (Printf.sprintf "Config file already exists: %s\nUse --force to overwrite." path)
  else begin
    (* Create directory if needed *)
    if not (Sys.file_exists dir) then begin
      Unix.mkdir dir 0o755
    end;

    (* Write config file *)
    let content = default_config_toml () in
    let oc = open_out path in
    output_string oc content;
    close_out oc;
    Ok path
  end
