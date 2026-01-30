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
  remote_host : string;
  remote_user : string;
  remote_source_dir : string;
  local_source_dir : string;
  local_output_dir : string;
  paper_thumbs_subdir : string;
  contact_faces_subdir : string;
  video_thumbs_subdir : string;

  (* Paper PDFs *)
  paper_pdfs_dir : string;

  (* Immich *)
  immich_endpoint : string;
  immich_api_key_file : string;

  (* PeerTube *)
  peertube_servers : peertube_server list;

  (* Typesense *)
  typesense_endpoint : string;
  typesense_api_key_file : string;
  openai_api_key_file : string;

  (* Zotero *)
  zotero_translation_server : string;
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
    remote_host = "localhost";
    remote_user = Sys.getenv_opt "USER" |> Option.value ~default:"user";
    remote_source_dir = "/var/www/images/originals";
    local_source_dir = Filename.concat home "bushel/images/originals";
    local_output_dir = Filename.concat home "bushel/images/web";
    paper_thumbs_subdir = "papers";
    contact_faces_subdir = "faces";
    video_thumbs_subdir = "videos";
    paper_pdfs_dir = Filename.concat home "bushel/pdfs";
    immich_endpoint = "http://localhost:2283";
    immich_api_key_file = Filename.concat (config_dir ()) "immich-key";
    peertube_servers = [];
    typesense_endpoint = "http://localhost:8108";
    typesense_api_key_file = Filename.concat (config_dir ()) "typesense-key";
    openai_api_key_file = Filename.concat (config_dir ()) "openai-key";
    zotero_translation_server = "http://localhost:1969";
  }

(** {1 Path Helpers} *)

let expand_path path =
  if String.length path > 0 && path.[0] = '~' then
    match Sys.getenv_opt "HOME" with
    | Some home -> home ^ String.sub path 1 (String.length path - 1)
    | None -> path
  else path

let paper_thumbs_dir t = Filename.concat t.local_source_dir t.paper_thumbs_subdir
let contact_faces_dir t = Filename.concat t.local_source_dir t.contact_faces_subdir
let video_thumbs_dir t = Filename.concat t.local_source_dir t.video_thumbs_subdir

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
  obj (fun remote_host remote_user remote_source_dir local_source_dir
           local_output_dir paper_thumbs contact_faces video_thumbs ->
    (remote_host, remote_user, remote_source_dir, local_source_dir,
     local_output_dir, paper_thumbs, contact_faces, video_thumbs))
  |> mem "remote_host" string ~dec_absent:default.remote_host
       ~enc:(fun (h,_,_,_,_,_,_,_) -> h)
  |> mem "remote_user" string ~dec_absent:default.remote_user
       ~enc:(fun (_,u,_,_,_,_,_,_) -> u)
  |> mem "remote_source_dir" string ~dec_absent:default.remote_source_dir
       ~enc:(fun (_,_,r,_,_,_,_,_) -> r)
  |> mem "local_source_dir" string ~dec_absent:default.local_source_dir
       ~enc:(fun (_,_,_,l,_,_,_,_) -> l)
  |> mem "local_output_dir" string ~dec_absent:default.local_output_dir
       ~enc:(fun (_,_,_,_,o,_,_,_) -> o)
  |> mem "paper_thumbs" string ~dec_absent:default.paper_thumbs_subdir
       ~enc:(fun (_,_,_,_,_,p,_,_) -> p)
  |> mem "contact_faces" string ~dec_absent:default.contact_faces_subdir
       ~enc:(fun (_,_,_,_,_,_,c,_) -> c)
  |> mem "video_thumbs" string ~dec_absent:default.video_thumbs_subdir
       ~enc:(fun (_,_,_,_,_,_,_,v) -> v)
  |> finish

let papers_codec ~default =
  let open Tomlt in
  let open Tomlt.Table in
  obj Fun.id
  |> mem "pdfs_dir" string ~dec_absent:default.paper_pdfs_dir ~enc:Fun.id
  |> finish

let immich_codec ~default =
  let open Tomlt in
  let open Tomlt.Table in
  obj (fun endpoint api_key_file -> (endpoint, api_key_file))
  |> mem "endpoint" string ~dec_absent:default.immich_endpoint
       ~enc:(fun (e, _) -> e)
  |> mem "api_key_file" string ~dec_absent:default.immich_api_key_file
       ~enc:(fun (_, k) -> k)
  |> finish

let peertube_codec =
  let open Tomlt in
  let open Tomlt.Table in
  obj Fun.id
  |> mem "servers" (list peertube_server_codec) ~dec_absent:[] ~enc:Fun.id
  |> finish

let typesense_codec ~default =
  let open Tomlt in
  let open Tomlt.Table in
  obj (fun endpoint api_key_file openai_key_file ->
    (endpoint, api_key_file, openai_key_file))
  |> mem "endpoint" string ~dec_absent:default.typesense_endpoint
       ~enc:(fun (e, _, _) -> e)
  |> mem "api_key_file" string ~dec_absent:default.typesense_api_key_file
       ~enc:(fun (_, k, _) -> k)
  |> mem "openai_key_file" string ~dec_absent:default.openai_api_key_file
       ~enc:(fun (_, _, o) -> o)
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
  obj (fun data_dir images papers immich peertube typesense zotero ->
    let (remote_host, remote_user, remote_source_dir, local_source_dir,
         local_output_dir, paper_thumbs_subdir, contact_faces_subdir,
         video_thumbs_subdir) = images in
    let (immich_endpoint, immich_api_key_file) = immich in
    let (typesense_endpoint, typesense_api_key_file, openai_api_key_file) = typesense in
    {
      data_dir = expand_path data_dir;
      remote_host;
      remote_user;
      remote_source_dir = expand_path remote_source_dir;
      local_source_dir = expand_path local_source_dir;
      local_output_dir = expand_path local_output_dir;
      paper_thumbs_subdir;
      contact_faces_subdir;
      video_thumbs_subdir;
      paper_pdfs_dir = expand_path papers;
      immich_endpoint;
      immich_api_key_file = expand_path immich_api_key_file;
      peertube_servers = peertube;
      typesense_endpoint;
      typesense_api_key_file = expand_path typesense_api_key_file;
      openai_api_key_file = expand_path openai_api_key_file;
      zotero_translation_server = zotero;
    })
  |> mem "data" (data_codec ~default) ~dec_absent:default.data_dir
       ~enc:(fun c -> c.data_dir)
  |> mem "images" (images_codec ~default)
       ~dec_absent:(default.remote_host, default.remote_user,
                    default.remote_source_dir, default.local_source_dir,
                    default.local_output_dir, default.paper_thumbs_subdir,
                    default.contact_faces_subdir, default.video_thumbs_subdir)
       ~enc:(fun c -> (c.remote_host, c.remote_user, c.remote_source_dir,
                       c.local_source_dir, c.local_output_dir,
                       c.paper_thumbs_subdir, c.contact_faces_subdir,
                       c.video_thumbs_subdir))
  |> mem "papers" (papers_codec ~default) ~dec_absent:default.paper_pdfs_dir
       ~enc:(fun c -> c.paper_pdfs_dir)
  |> mem "immich" (immich_codec ~default)
       ~dec_absent:(default.immich_endpoint, default.immich_api_key_file)
       ~enc:(fun c -> (c.immich_endpoint, c.immich_api_key_file))
  |> mem "peertube" peertube_codec ~dec_absent:[]
       ~enc:(fun c -> c.peertube_servers)
  |> mem "typesense" (typesense_codec ~default)
       ~dec_absent:(default.typesense_endpoint, default.typesense_api_key_file,
                    default.openai_api_key_file)
       ~enc:(fun c -> (c.typesense_endpoint, c.typesense_api_key_file,
                       c.openai_api_key_file))
  |> mem "zotero" (zotero_codec ~default)
       ~dec_absent:default.zotero_translation_server
       ~enc:(fun c -> c.zotero_translation_server)
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

let immich_api_key t = read_api_key t.immich_api_key_file
let typesense_api_key t = read_api_key t.typesense_api_key_file
let openai_api_key t = read_api_key t.openai_api_key_file

(** {1 Rsync Command} *)

let rsync_source t =
  Printf.sprintf "%s@%s:%s" t.remote_user t.remote_host t.remote_source_dir

let rsync_command t =
  Printf.sprintf "rsync -avz %s/ %s/" (rsync_source t) t.local_source_dir

(** {1 Pretty Printing} *)

let pp ppf t =
  let open Fmt in
  pf ppf "@[<v>";
  pf ppf "%a:@," (styled `Bold string) "Bushel Configuration";
  pf ppf "  data_dir: %s@," t.data_dir;
  pf ppf "  @[<v 2>images:@,";
  pf ppf "remote: %s@%s:%s@," t.remote_user t.remote_host t.remote_source_dir;
  pf ppf "local_source: %s@," t.local_source_dir;
  pf ppf "local_output: %s@," t.local_output_dir;
  pf ppf "@]";
  pf ppf "  paper_pdfs: %s@," t.paper_pdfs_dir;
  pf ppf "  immich: %s@," t.immich_endpoint;
  pf ppf "  peertube servers: %d@," (List.length t.peertube_servers);
  pf ppf "  typesense: %s@," t.typesense_endpoint;
  pf ppf "  zotero: %s@," t.zotero_translation_server;
  pf ppf "@]"

(** {1 Default Config Generation} *)

let default_config_toml () =
  let home = Sys.getenv_opt "HOME" |> Option.value ~default:"~" in
  let user = Sys.getenv_opt "USER" |> Option.value ~default:"user" in
  Printf.sprintf {|# Bushel Configuration
# Generated by: bushel init

# Data directory containing your bushel entries
[data]
local_dir = "%s/bushel/data"

# Image sync configuration
# Images are rsynced from a remote server and processed locally
[images]
# Remote server settings (for rsync)
remote_host = "example.com"
remote_user = "%s"
remote_source_dir = "/var/www/images/originals"

# Local directories
local_source_dir = "%s/bushel/images/originals"
local_output_dir = "%s/bushel/images/web"

# Subdirectories within local_output_dir for generated thumbnails
paper_thumbs = "papers"
contact_faces = "faces"
video_thumbs = "videos"

# Paper PDFs directory (for thumbnail generation)
[papers]
pdfs_dir = "%s/bushel/pdfs"

# Immich integration for contact face thumbnails
# Get your API key from Immich web UI -> Account Settings -> API Keys
[immich]
endpoint = "http://localhost:2283"
api_key_file = "%s/.config/bushel/immich-key"

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

# Typesense search integration
[typesense]
endpoint = "http://localhost:8108"
api_key_file = "%s/.config/bushel/typesense-key"
openai_key_file = "%s/.config/bushel/openai-key"

# Zotero Translation Server for DOI resolution
# Run locally: docker run -p 1969:1969 zotero/translation-server
[zotero]
translation_server = "http://localhost:1969"
|} home user home home home home home home

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
