(*---------------------------------------------------------------------------
  Copyright (c) 2025 Anil Madhavapeddy <anil@recoil.org>. All rights reserved.
  SPDX-License-Identifier: ISC
 ---------------------------------------------------------------------------*)

type peertube_server = {
  name : string;
  endpoint : string;
}

type releases = {
  github_user : string option;
  github : string list;
  tangled : string list;
  registries : string list;
  projects : (string * string) list;
}

type http_embeddings = {
  url : string;
  model : string;
  dimensions : int;
  api_key_file : string option;
}

type mlx_embeddings = {
  model_dir : string;
  dimensions : int;
  max_tokens : int;
  query_instruction : string;
}

type embeddings = Http of http_embeddings | Mlx of mlx_embeddings

let default_query_instruction =
  "Given a search query, retrieve relevant passages from personal notes, papers, \
   feeds, and contacts."

let default_image_query_instruction =
  "Represent the user's input."

let validate_http_embeddings (e : http_embeddings) =
  let invalid_url =
    Error
      "Embeddings URL must be HTTP(S) with a host and no credentials, query or \
       fragment"
  in
  if String.trim e.model = "" then
    Error "Embeddings model must be nonempty"
  else if e.dimensions < 1 || e.dimensions > 4096 then
    Error "Embeddings dimensions must be between 1 and 4096"
  else if
    Option.fold ~none:false
      ~some:(fun path -> String.trim path = "" || String.contains path '\000')
      e.api_key_file
  then Error "Embeddings API key file path must be nonempty and contain no NUL"
  else
    match Uriz.of_string e.url with
    | Null -> invalid_url
    | This uri ->
        let https = Uriz.scheme uri = This "https" in
        if
          (not (https || Uriz.scheme uri = This "http"))
          || not (Uriz.has_authority uri)
          || Uriz.host uri = Null || Uriz.host uri = This ""
          || Uriz.userinfo uri <> Null || Uriz.query uri <> Null
          || Uriz.fragment uri <> Null
          || (match Uriz.port uri with
             | Null -> false
             | This port -> port < 1 || port > 65535)
        then invalid_url
        else if e.api_key_file <> None && not https then
          Error "An embeddings API key file requires an HTTPS endpoint"
        else Ok ()

let validate_embeddings = function
  | Http e -> validate_http_embeddings e
  | Mlx e ->
      if String.trim e.model_dir = "" || String.contains e.model_dir '\000' then
        Error "MLX model directory must be nonempty and contain no NUL"
      else if e.dimensions < 32 || e.dimensions > 4096 then
        Error "MLX embeddings dimensions must be between 32 and 4096"
      else if e.max_tokens < 32 || e.max_tokens > 32768 then
        Error "MLX max_tokens must be between 32 and 32768"
      else if String.length e.query_instruction > 8192
              || String.contains e.query_instruction '\000' then
        Error "MLX query instruction must be at most 8192 bytes with no NUL"
      else Ok ()

let validate_image_embeddings (e : mlx_embeddings) =
  match validate_embeddings (Mlx e) with
  | Error _ as error -> error
  | Ok () when e.dimensions < 64 ->
      Error "Vision embedding dimensions must be between 64 and 4096"
  | Ok () -> Ok ()

let default_registries =
  [ "pypi.org"; "opam.ocaml.org"; "npmjs.org"; "crates.io" ]

let default_releases =
  {
    github_user = None;
    github = [];
    tangled = [];
    registries = default_registries;
    projects = [];
  }

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

let xdg_config_home () =
  match Sys.getenv_opt "XDG_CONFIG_HOME" with
  | Some dir -> dir
  | None ->
    match Sys.getenv_opt "HOME" with
    | Some home -> Filename.concat home ".config"
    | None -> ".config"

let config_dir () = Filename.concat (xdg_config_home ()) "bushel"
let config_file () = Filename.concat (config_dir ()) "config.toml"

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
    releases = default_releases;
    embeddings = None;
    image_embeddings = None;
    sync = Gitops.Sync.Config.default;
    images_sync = Gitops.Sync.Config.default;
  }

let expand_path path =
  if String.length path > 0 && path.[0] = '~' then
    match Sys.getenv_opt "HOME" with
    | Some home -> home ^ String.sub path 1 (String.length path - 1)
    | None -> path
  else path

let resolve_embedding_path ~directory path =
  let path = expand_path path in
  if Filename.is_relative path then Filename.concat directory path else path

let resolve_embedding_paths ~directory = function
  | Http e ->
      Http { e with api_key_file =
        Option.map (resolve_embedding_path ~directory) e.api_key_file }
  | Mlx e ->
      Mlx { e with model_dir = resolve_embedding_path ~directory e.model_dir }

let paper_thumbs_dir t = Filename.concat t.images_dir t.paper_thumbs_subdir
let contact_faces_dir t = Filename.concat t.images_dir t.contact_faces_subdir
let video_thumbs_dir t = Filename.concat t.images_dir t.video_thumbs_subdir

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

let projects_codec =
  let open Tomlt in
  let open Tomlt.Table in
  obj Fun.id
  |> keep_unknown (Mems.assoc string) ~enc:Fun.id
  |> finish

let releases_codec =
  let open Tomlt in
  let open Tomlt.Table in
  obj (fun github_user github tangled registries projects ->
      { github_user; github; tangled; registries; projects })
  |> opt_mem "github_user" string
       ~enc:(fun r -> r.github_user)
  |> mem "github" (list string) ~dec_absent:[] ~enc:(fun r -> r.github)
  |> mem "tangled" (list string) ~dec_absent:[] ~enc:(fun r -> r.tangled)
  |> mem "registries" (list string) ~dec_absent:default_registries
       ~enc:(fun r -> r.registries)
  |> mem "projects" projects_codec ~dec_absent:[]
       ~enc:(fun r -> r.projects)
  |> finish

let http_embeddings_codec =
  let open Tomlt in
  let open Tomlt.Table in
  obj (fun () url model dimensions api_key_file ->
      {
        url; model; dimensions;
        api_key_file = Option.map expand_path api_key_file;
      })
  |> mem "backend" (enum [ "http", () ]) ~dec_absent:()
       ~enc:(fun _ -> ()) ~enc_omit:(fun () -> true)
  |> mem "url" string ~enc:(fun (e : http_embeddings) -> e.url)
  |> mem "model" string ~enc:(fun (e : http_embeddings) -> e.model)
  |> mem "dimensions" int ~enc:(fun (e : http_embeddings) -> e.dimensions)
  |> opt_mem "api_key_file" string
       ~enc:(fun (e : http_embeddings) -> e.api_key_file)
  |> finish

let mlx_embeddings_codec_with ~instruction =
  let open Tomlt in
  let open Tomlt.Table in
  obj (fun () model_dir dimensions max_tokens query_instruction ->
      { model_dir = expand_path model_dir; dimensions; max_tokens;
        query_instruction })
  |> mem "backend" (enum [ "mlx", () ]) ~enc:(fun _ -> ())
  |> mem "model_dir" string ~enc:(fun (e : mlx_embeddings) -> e.model_dir)
  |> mem "dimensions" int ~enc:(fun (e : mlx_embeddings) -> e.dimensions)
  |> mem "max_tokens" int ~dec_absent:8192
       ~enc:(fun (e : mlx_embeddings) -> e.max_tokens)
  |> mem "query_instruction" string ~dec_absent:instruction
       ~enc:(fun (e : mlx_embeddings) -> e.query_instruction)
  |> finish

let mlx_embeddings_codec =
  mlx_embeddings_codec_with ~instruction:default_query_instruction

let image_embeddings_codec =
  mlx_embeddings_codec_with ~instruction:default_image_query_instruction

let embeddings_codec =
  Tomlt.map
    ~dec:(function Ok e -> Http e | Error e -> Mlx e)
    ~enc:(function Http e -> Ok e | Mlx e -> Error e)
    (Tomlt.result ~ok:http_embeddings_codec ~error:mlx_embeddings_codec)

let config_codec =
  let default = default () in
  let open Tomlt.Table in
  obj (fun data_dir images papers peertube zotero releases embeddings image_embeddings sync
           images_sync ->
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
      releases;
      embeddings;
      image_embeddings;
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
  |> mem "releases" releases_codec ~dec_absent:default_releases
       ~enc:(fun c -> c.releases)
  |> opt_mem "embeddings" embeddings_codec ~enc:(fun c -> c.embeddings)
  |> opt_mem "image_embeddings" image_embeddings_codec
       ~enc:(fun c -> c.image_embeddings)
  |> mem "sync" Gitops.Sync.Config.codec
       ~dec_absent:Gitops.Sync.Config.default
       ~enc:(fun c -> c.sync)
  |> mem "images_sync" Gitops.Sync.Config.codec
       ~dec_absent:Gitops.Sync.Config.default
       ~enc:(fun c -> c.images_sync)
  |> finish

let of_string s =
  match Tomlt_bytesrw.decode_string config_codec s with
  | Ok config ->
      let text =
        Option.fold ~none:(Ok ()) ~some:validate_embeddings config.embeddings
      in
      Result.bind text (fun () ->
          let images =
            Option.fold ~none:(Ok ()) ~some:validate_image_embeddings
              config.image_embeddings
          in
          Result.map (fun () -> config) images)
  | Error e -> Error (Tomlt.Toml.Error.to_string e)

let load_file path =
  try
    In_channel.with_open_bin path (fun ic ->
      match of_string (really_input_string ic (in_channel_length ic)) with
      | Error message -> Error message
      | Ok config ->
          let path =
            if Filename.is_relative path then
              Filename.concat (Sys.getcwd ()) path
            else path
          in
          let directory = Filename.dirname path in
          let embeddings =
            Option.map
              (resolve_embedding_paths ~directory)
              config.embeddings
          in
          let image_embeddings =
            Option.map (fun (e : mlx_embeddings) ->
              { e with model_dir = resolve_embedding_path ~directory e.model_dir })
              config.image_embeddings
          in
          Ok { config with embeddings; image_embeddings })
  with
  | Sys_error msg -> Error (Printf.sprintf "Failed to read config: %s" msg)

let load () =
  let path = config_file () in
  if Sys.file_exists path then
    load_file path
  else
    Ok (default ())

let replace_embeddings ~section_name source document settings =
  let module T = Tomlt.Toml in
  let previous =
    match T.find_opt section_name document with
    | Some (T.Table fields) -> fields
    | _ -> []
  in
  let owned =
    [ "backend"; "url"; "model"; "dimensions"; "api_key_file";
      "model_dir"; "max_tokens"; "query_instruction" ]
  in
  let fields =
    T.to_table (Tomlt.encode embeddings_codec settings)
    @ List.filter (fun (key, _) -> not (List.mem key owned)) previous
  in
  let section = T.Table fields in
  let updated = T.(document.%{[ section_name ]} <- section) in
  let replacement =
    Tomlt_bytesrw.to_string (T.Table [ section_name, section ])
  in
  let rec ordered = function
    | T.Table fields ->
        T.Table
          (List.map (fun (key, value) -> key, ordered value) fields
          |> List.sort (fun (a, _) (b, _) -> String.compare a b))
    | T.Array values -> T.Array (List.map ordered values)
    | value -> value
  in
  let expected = ordered updated in
  let equivalent candidate =
    match Tomlt_bytesrw.of_string candidate with
    | Ok parsed -> T.equal (ordered parsed) expected
    | Error _ -> false
  in
  let fallback () =
    let content = Tomlt_bytesrw.to_string updated in
    if not (equivalent content) then
      invalid_arg "Cannot preserve the existing TOML configuration";
    content
  in
  if not (T.mem section_name document) then
    let candidate = source ^ "\n" ^ replacement in
    if equivalent candidate then candidate else fallback ()
  else
    let lines = Array.of_list (String.split_on_char '\n' source) in
    let length = Array.length lines in
    let join first last =
      Array.sub lines first (last - first) |> Array.to_list
      |> String.concat "\n"
    in
    let header line =
      let line = String.trim line in
      String.starts_with ~prefix:("[" ^ section_name ^ "]") line
      && let rest =
           String.trim (String.sub line (String.length section_name + 2)
             (String.length line - String.length section_name - 2))
         in
         rest = "" || String.starts_with ~prefix:"#" rest
    in
    let table_header line =
      let line = String.trim line in
      String.starts_with ~prefix:"[" line
      && Result.is_ok (Tomlt_bytesrw.of_string (line ^ "\n"))
    in
    let rec try_section first =
      if first = length then fallback ()
      else if not (header lines.(first)) then try_section (first + 1)
      else
        let rec end_section i =
          if i = length || table_header lines.(i) then i
          else end_section (i + 1)
        in
        let rec keep_comments i =
          if i <= first + 1 then i
          else
            let line = String.trim lines.(i - 1) in
            if line = "" || String.starts_with ~prefix:"#" line then
              keep_comments (i - 1)
            else i
        in
        let last = keep_comments (end_section (first + 1)) in
        let prefix = if first = 0 then "" else join 0 first ^ "\n" in
        let candidate = prefix ^ replacement ^ join last length in
        (* Header-like text may occur inside a multiline string. Only keep a
           textual edit when parsing proves every unrelated value survived. *)
        if equivalent candidate then candidate else try_section (first + 1)
    in
    try_section 0

let rec private_parents path =
  try
    if (Unix.stat path).Unix.st_kind <> Unix.S_DIR then
      raise (Unix.Unix_error (Unix.ENOTDIR, "mkdir", path))
  with Unix.Unix_error (Unix.ENOENT, _, _) ->
    let parent = Filename.dirname path in
    if parent <> path then private_parents parent;
    (try Unix.mkdir path 0o700 with
    | Unix.Unix_error (Unix.EEXIST, _, _)
      when (Unix.stat path).Unix.st_kind = Unix.S_DIR -> ())

let atomic_config path content permissions =
  let dir = Filename.dirname path in
  private_parents dir;
  let temporary, channel =
    Filename.open_temp_file ~temp_dir:dir ~perms:0o600 ".bushel-config-" ".toml"
  in
  let renamed = ref false in
  Fun.protect
    ~finally:(fun () ->
      close_out_noerr channel;
      if not !renamed then (try Sys.remove temporary with Sys_error _ -> ()))
    (fun () ->
      output_string channel content;
      flush channel;
      Unix.fchmod (Unix.descr_of_out_channel channel) permissions;
      Unix.fsync (Unix.descr_of_out_channel channel);
      close_out channel;
      Unix.rename temporary path;
      renamed := true)

(* POSIX record locks are process-wide. Serialize local writers as well so one
   thread cannot release another's lock by closing the same lock file. *)
let config_writer = Mutex.create ()

let with_config_lock path f =
  Mutex.protect config_writer (fun () ->
      (match Unix.lstat path with
      | stat when stat.Unix.st_kind <> Unix.S_REG ->
          invalid_arg "Config path must name a regular file"
      | _ -> ()
      | exception Unix.Unix_error (Unix.ENOENT, _, _) -> ());
      private_parents (Filename.dirname path);
      let fd =
        Unix.openfile (path ^ ".lock")
          [ Unix.O_RDWR; Unix.O_CREAT; Unix.O_NONBLOCK; Unix.O_CLOEXEC ] 0o600
      in
      Fun.protect ~finally:(fun () -> Unix.close fd) (fun () ->
          if (Unix.fstat fd).Unix.st_kind <> Unix.S_REG then
            invalid_arg "Config lock must name a regular file";
          (try Unix.lockf fd Unix.F_TLOCK 0 with
          | Unix.Unix_error ((Unix.EACCES | Unix.EAGAIN), _, _) ->
              invalid_arg "Another process is updating this configuration");
          f ()))

let configure_embedding_section ~section_name ?file settings =
  match validate_embeddings settings with
  | Error message -> Error message
  | Ok () ->
      let path = expand_path (Option.value file ~default:(config_file ())) in
      try
        let settings =
          resolve_embedding_paths ~directory:(Sys.getcwd ()) settings
        in
        with_config_lock path (fun () ->
            let source, permissions =
              match Unix.lstat path with
              | stat when stat.Unix.st_kind = Unix.S_REG ->
                  (In_channel.with_open_bin path In_channel.input_all,
                   stat.Unix.st_perm land 0o777)
              | _ -> invalid_arg "Config path must name a regular file"
              | exception Unix.Unix_error (Unix.ENOENT, _, _) -> ("", 0o600)
            in
            match Tomlt_bytesrw.of_string source with
            | Error error -> Error (Tomlt.Toml.Error.to_string error)
            | Ok document ->
                let content =
                  replace_embeddings ~section_name source document settings
                in
                match of_string content with
                | Error message -> Error message
                | Ok _ ->
                    atomic_config path content permissions;
                    Ok path)
      with
      | Sys_error message | Invalid_argument message -> Error message
      | Unix.Unix_error (error, operation, _) ->
          Error (operation ^ ": " ^ Unix.error_message error)

let configure_embeddings ?file settings =
  configure_embedding_section ~section_name:"embeddings" ?file settings

let configure_image_embeddings ?file settings =
  match validate_image_embeddings settings with
  | Error _ as error -> error
  | Ok () -> configure_embedding_section ~section_name:"image_embeddings" ?file (Mlx settings)

let read_api_key path =
  let path = expand_path path in
  try
    In_channel.with_open_bin path (fun ic -> Ok (input_line ic |> String.trim))
  with
  | Sys_error msg -> Error (Printf.sprintf "Failed to read API key from %s: %s" path msg)
  | End_of_file -> Error (Printf.sprintf "API key file %s is empty" path)

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
  pf ppf "  release repositories: %d@,"
    (List.length t.releases.github + List.length t.releases.tangled);
  pf ppf "  sync remote: %s@," t.sync.Gitops.Sync.Config.remote;
  pf ppf "  images_sync remote: %s@," t.images_sync.Gitops.Sync.Config.remote;
  pf ppf "@]"

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

# Code releases to register with: bushel release discover
# github_user is the login whose GitHub releases count as yours.
# [releases]
# github_user = "avsm"
# github = ["mirage/ocaml-cohttp"]
# tangled = ["anil.recoil.org/dune-rpc-eio"]
# registries = ["pypi.org", "opam.ocaml.org"]
#
# [releases.projects]
# "ucam-eo/geotessera" = "tessera"

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

  if Sys.file_exists path && not force then
    Error (Printf.sprintf "Config file already exists: %s\nUse --force to overwrite." path)
  else begin
    if not (Sys.file_exists dir) then begin
      Unix.mkdir dir 0o755
    end;
    let content = default_config_toml () in
    Out_channel.with_open_bin path (fun oc -> output_string oc content);
    Ok path
  end
