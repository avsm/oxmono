(*---------------------------------------------------------------------------
  Copyright (c) 2026 Anil Madhavapeddy <anil@recoil.org>. All rights reserved.
  SPDX-License-Identifier: ISC
 ---------------------------------------------------------------------------*)

type service = {
  kind : string;
  profile : string;
  account : string;
  endpoint : string;
}

type t = {
  version : int;
  arod : Arod.Config.t;
  bushel : Bushel_config.t;
  sortal : Gitops.Sync.Config.t;
  sortal_dir : string;
  pdf_source : string option;
  services : service list;
}

let home () = Sys.getenv "HOME"
let expand = Bushel_config.expand_path
let portable p =
  let prefix = home () ^ "/" in
  if String.starts_with ~prefix p then
    "~/" ^ String.sub p (String.length prefix)
      (String.length p - String.length prefix)
  else p

let config_root () = Bushel_config.xdg_config_home ()
let config_path app = Filename.concat (config_root ()) app
let default_path () = Filename.concat (config_path "arod") "pull.toml"
let read path = In_channel.with_open_bin path In_channel.input_all

let defaults () =
  let sync name = { Gitops.Sync.Config.default with
    remote = "avsm@fork.recoil.org:git/" ^ name } in
  let arod = Arod.Config.{ default with
    paths = { data_dir = expand "~/bushel/data";
      images_dir = expand "~/bushel/images-web";
      papers_dir = expand "~/bushel/papers" };
    site = { base_url = "https://anil.recoil.org";
      name = "Anil Madhavapeddy";
      description = "Anil Madhavapeddy's research combines computer science "
        ^ "and environmental science at the University of Cambridge";
      author_handle = "avsm"; author_name = "Anil Madhavapeddy" };
    feeds = { title = "Anil Madhavapeddy's feed"; subtitle = None } } in
  let bushel = Bushel_config.{ (default ()) with
    data_dir = arod.paths.data_dir;
    images_dir = expand "~/bushel/images-git";
    images_output_dir = arod.paths.images_dir;
    paper_pdfs_dir = arod.paths.papers_dir;
    peertube_servers = [
      { name = "crank"; endpoint = "https://crank.recoil.org" };
      { name = "eeg"; endpoint = "https://watch.eeg.cl.cam.ac.uk" };
      { name = "ocaml"; endpoint = "https://watch.ocaml.org" } ];
    sync = sync "bushel"; images_sync = sync "bushel-images" } in
  { version = 1; arod; bushel; sortal = sync "sortal";
    sortal_dir = expand "~/bushel/sortal"; pdf_source = None;
    services = [
      { kind = "standard-site"; profile = "anil.recoil.org";
        account = "anil.recoil.org"; endpoint = "https://bsky.social" };
      { kind = "apub"; profile = "avsm@amok.recoil.org";
        account = "avsm@amok.recoil.org";
        endpoint = "https://amok.recoil.org" };
      { kind = "karakeep"; profile = "default"; account = "";
        endpoint = "https://hoard.recoil.org" } ] }

let service_codec =
  let open Tomlt in
  Table.(obj (fun kind profile account endpoint ->
      { kind; profile; account; endpoint })
    |> mem "kind" string ~enc:(fun s -> s.kind)
    |> mem "profile" string ~enc:(fun s -> s.profile)
    |> mem "account" string ~dec_absent:"" ~enc:(fun s -> s.account)
    |> mem "endpoint" string ~enc:(fun s -> s.endpoint)
    |> finish)

let codec =
  let open Tomlt in
  Table.(obj (fun version arod bushel sortal sortal_dir pdf_source services ->
      { version; arod; bushel; sortal; sortal_dir = expand sortal_dir;
        pdf_source; services })
    |> mem "version" int ~enc:(fun t -> t.version)
    |> mem "arod" Arod.Config.config_codec ~enc:(fun t -> t.arod)
    |> mem "bushel" Bushel_config.config_codec ~enc:(fun t -> t.bushel)
    |> mem "sortal" Gitops.Sync.Config.codec ~enc:(fun t -> t.sortal)
    |> mem "sortal_dir" string ~enc:(fun t -> t.sortal_dir)
    |> opt_mem "pdf_source" string ~enc:(fun t -> t.pdf_source)
    |> mem "services" (list service_codec) ~enc:(fun t -> t.services)
    |> finish)

let safe_component s =
  s <> "" && s <> "." && s <> ".." &&
  not (String.exists (fun c -> c = '/' || c = '\\' || Char.code c < 32) s)

let safe_location s =
  if s = "" || s.[0] = '-' ||
     String.exists (fun c -> Char.code c < 32 || c = '?' || c = '#') s
  then failwith "Invalid location or credential-bearing URL in setup";
  match String.index_opt s ':' with
  | Some i when String.length s > i + 2 && String.sub s i 3 = "://" ->
      let rest = String.sub s (i + 3) (String.length s - i - 3) in
      let authority = List.hd (String.split_on_char '/' rest) in
      (* SSH usernames are portable, HTTP userinfo can contain credentials. *)
      let ssh = String.starts_with ~prefix:"ssh://" s in
      if String.contains authority '@' && not ssh then
        failwith "Credentials in URLs cannot be exported";
      if ssh && String.contains authority ':' then
        (* Permit a host port, but never user:password@host. *)
        (match String.index_opt authority '@' with
         | Some at when String.index authority ':' < at ->
             failwith "Passwords in SSH URLs cannot be exported"
         | _ -> ())
  | _ -> ()

let validate t =
  if t.version <> 1 then failwith "Unsupported pull manifest version";
  if t.arod.server.stats_password <> None then
    failwith "stats_password belongs in local arod config, not the manifest";
  let b = t.bushel in
  List.iter (fun (cfg : Gitops.Sync.Config.t) ->
    let remote = cfg.remote in
    if String.contains remote ':' &&
       not (String.starts_with ~prefix:"ssh://" remote ||
            String.starts_with ~prefix:"https://" remote) then begin
      let i = String.index remote ':' in
      if String.starts_with ~prefix:"//"
           (String.sub remote (i + 1) (String.length remote - i - 1)) ||
         String.contains (String.sub remote 0 i) '/' ||
         String.contains remote ' ' ||
         String.contains remote '\\' ||
         String.starts_with ~prefix:"ext:" remote ||
         String.starts_with ~prefix:"file:" remote ||
         String.contains (String.sub remote (i + 1)
           (String.length remote - i - 1)) ':' then
        failwith "Unsupported Git transport"
    end)
    [b.sync; b.images_sync; t.sortal];
  List.iter safe_location [t.arod.site.base_url; b.sync.remote;
    b.images_sync.remote; t.sortal.remote; b.zotero_translation_server];
  List.iter (fun (s : Bushel_config.peertube_server) ->
    safe_location s.endpoint) b.peertube_servers;
  Option.iter safe_location t.pdf_source;
  List.iter (fun s ->
    if not (List.mem s.kind ["standard-site"; "apub"; "karakeep"])
    then failwith "Unsupported authentication adapter";
    if not (safe_component s.profile) then failwith "Invalid profile name";
    safe_location s.endpoint;
    if not (String.starts_with ~prefix:"https://" s.endpoint) then
      failwith "Authentication endpoints must use HTTPS";
    if s.kind <> "karakeep" &&
       (s.account = "" || s.account.[0] = '-' ||
        String.exists (fun c -> Char.code c <= 32) s.account)
    then failwith "Invalid service account") t.services;
  let paths = [b.data_dir; b.images_dir; b.images_output_dir;
    b.paper_pdfs_dir; t.sortal_dir] in
  List.iter (fun p ->
    if Filename.is_relative p then failwith "Setup paths must be absolute";
    if String.exists (fun c -> Char.code c < 32) p ||
       List.mem ".." (String.split_on_char '/' p) ||
       p = "/" || p = home () then failwith "Unsafe setup destination") paths;
  let paths = List.map (fun p ->
    try Unix.realpath p with _ -> p) paths in
  List.iteri (fun i p -> List.iteri (fun j q ->
    if i <> j && (p = q || String.starts_with ~prefix:(p ^ "/") q) then
      failwith "Repository, PDF and generated-image paths must be separate")
    paths) paths;
  if t.arod.paths.data_dir <> b.data_dir ||
     t.arod.paths.images_dir <> b.images_output_dir ||
     t.arod.paths.papers_dir <> b.paper_pdfs_dir then
    failwith "Arod and Bushel paths disagree. Reconcile them before pulling"

let portable_config t =
  let a = t.arod and b = t.bushel in
  { t with
    arod = { a with paths = {
      data_dir = portable a.paths.data_dir;
      images_dir = portable a.paths.images_dir;
      papers_dir = portable a.paths.papers_dir } };
    bushel = { b with data_dir = portable b.data_dir;
      images_dir = portable b.images_dir;
      images_output_dir = portable b.images_output_dir;
      paper_pdfs_dir = portable b.paper_pdfs_dir };
    sortal_dir = portable t.sortal_dir;
    pdf_source = Option.map portable t.pdf_source }

let encode t =
  validate t;
  Tomlt_bytesrw.encode_string codec (portable_config t)

let decode text =
  match Tomlt_bytesrw.decode_string codec text with
  | Error _ -> failwith "Invalid pull manifest TOML"
  | Ok t -> validate t; t

let load path = decode (read path)

let json_string_field field =
  Jsont.Object.map (fun s -> s)
  |> Jsont.Object.mem field Jsont.string ~enc:Fun.id
  |> Jsont.Object.finish

let json_field path field =
  match Jsont_bytesrw.decode_string (json_string_field field) (read path) with
  | Ok s -> s
  | Error _ -> failwith ("Missing public metadata field: " ^ field)

let toml_field path field =
  let codec = Tomlt.Table.(obj Fun.id
    |> mem field Tomlt.string ~enc:Fun.id |> finish) in
  match Tomlt_bytesrw.decode_string codec (read path) with
  | Ok s -> s
  | Error _ -> failwith ("Missing public metadata field: " ^ field)

(* Karakeep currently owns a legacy HOME-relative store. *)
let service_root kind =
  if kind = "karakeep" then Filename.concat (home ()) ".config/karakeep"
  else config_path kind

let session_path s =
  Filename.concat (service_root s.kind)
    ("profiles/" ^ s.profile ^ "/" ^
     if s.kind = "karakeep" then "credentials.toml" else "session.json")

let scan_service fallback =
  let root = service_root fallback.kind in
  let current = Filename.concat root
      (if fallback.kind = "karakeep" then "config.toml" else "config.json") in
  let profile = if Sys.file_exists current then
      (if fallback.kind = "karakeep" then toml_field else json_field)
        current "current_profile"
    else fallback.profile in
  if not (safe_component profile) then failwith "Invalid current profile";
  let s = { fallback with profile } in
  let path = session_path s in
  if not (Sys.file_exists path) then s else
  match s.kind with
  | "standard-site" -> { s with
      account = json_field path "handle"; endpoint = json_field path "pds" }
  | "apub" ->
      let instance = json_field path "oauth_instance" in
      let actor = json_field path "actor_uri" in
      let username = List.rev (String.split_on_char '/' actor) |> List.hd in
      let username = if String.starts_with ~prefix:"@" username then
          String.sub username 1 (String.length username - 1) else username in
      { s with account = username ^ "@" ^ instance;
        endpoint = "https://" ^ instance }
  | "karakeep" -> { s with endpoint = toml_field path "base_url" }
  | _ -> assert false

let repo_config dir (fallback : Gitops.Sync.Config.t) =
  if Sys.file_exists (Filename.concat dir ".git") then
    { fallback with remote = Process.git dir ["remote"; "get-url"; "origin"];
      branch = Process.git dir ["symbolic-ref"; "--short"; "HEAD"] }
  else fallback

let scan ~fs ?config () =
  let d = defaults () in
  let path = Option.value ~default:(Arod.Config.config_file ()) config in
  let arod = if Sys.file_exists path then
      try Arod.Config.of_file path with _ ->
        failwith "Cannot decode arod configuration"
    else d.arod in
  let arod = { arod with
    server = { arod.server with stats_password = None } } in
  let bushel = if Sys.file_exists (Bushel_config.config_file ()) then
      match Bushel_config.load () with Ok b -> b
      | Error _ -> failwith "Cannot decode Bushel configuration"
    else d.bushel in
  let xdg = Xdge.create ~create_dirs:false fs "sortal" in
  let dir = Eio.Path.native_exn (Xdge.data_dir xdg) in
  let sortal_dir = if Sys.file_exists dir then Unix.realpath dir
    else d.sortal_dir in
  let sortal = if Sys.file_exists (Sortal_config.config_file ()) then
      match Sortal_config.load () with Ok s -> s.sync
      | Error _ -> failwith "Cannot decode Sortal configuration"
    else d.sortal in
  let t = { d with arod;
    bushel = { bushel with
      sync = repo_config bushel.data_dir bushel.sync;
      images_sync = repo_config bushel.images_dir bushel.images_sync };
    sortal = repo_config sortal_dir sortal; sortal_dir;
    services = List.map scan_service d.services } in
  validate t;
  t
