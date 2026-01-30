(*---------------------------------------------------------------------------
  Copyright (c) 2025 Anil Madhavapeddy <anil@recoil.org>. All rights reserved.
  SPDX-License-Identifier: ISC
 ---------------------------------------------------------------------------*)

(** Configuration for the Arod webserver *)

type server = {
  host : string;
  port : int;
}

type paths = {
  data_dir : string;
  assets_dir : string;
  images_dir : string;
  static_dir : string;
}

type site = {
  base_url : string;
  name : string;
  description : string;
  author_handle : string;
  author_name : string;
  author_email : string option;
  author_orcid : string option;
}

type feeds = {
  title : string;
  subtitle : string option;
}

type well_known_entry = {
  key : string;
  value : string;
}

type t = {
  server : server;
  paths : paths;
  site : site;
  feeds : feeds;
  well_known : well_known_entry list;
}

(** Path expansion helper - expands ~ to home directory *)
let expand_path p =
  if String.length p > 0 && p.[0] = '~' then
    let home = Sys.getenv_opt "HOME" |> Option.value ~default:"/tmp" in
    home ^ String.sub p 1 (String.length p - 1)
  else p

(** Default configuration *)
let default =
  let home = Sys.getenv_opt "HOME" |> Option.value ~default:"/tmp" in
  {
    server = {
      host = "0.0.0.0";
      port = 8080;
    };
    paths = {
      data_dir = Filename.concat home "bushel";
      assets_dir = "./assets";
      images_dir = Filename.concat home "bushel/images/web";
      static_dir = "./static";
    };
    site = {
      base_url = "http://localhost:8080";
      name = "My Site";
      description = "A personal website powered by Bushel";
      author_handle = "me";
      author_name = "Site Author";
      author_email = None;
      author_orcid = None;
    };
    feeds = {
      title = "Site Feed";
      subtitle = None;
    };
    well_known = [];
  }

(** {1 TOML Codecs} *)

(** String codec with path expansion *)
let path_string =
  Tomlt.(map string ~dec:expand_path)

let server_codec =
  Tomlt.(Table.(
    obj (fun host port -> { host; port })
    |> mem "host" string ~dec_absent:default.server.host ~enc:(fun s -> s.host)
    |> mem "port" int ~dec_absent:default.server.port ~enc:(fun s -> s.port)
    |> finish
  ))

let paths_codec =
  Tomlt.(Table.(
    obj (fun data_dir assets_dir images_dir static_dir ->
      { data_dir; assets_dir; images_dir; static_dir })
    |> mem "data_dir" path_string ~dec_absent:default.paths.data_dir ~enc:(fun p -> p.data_dir)
    |> mem "assets_dir" path_string ~dec_absent:default.paths.assets_dir ~enc:(fun p -> p.assets_dir)
    |> mem "images_dir" path_string ~dec_absent:default.paths.images_dir ~enc:(fun p -> p.images_dir)
    |> mem "static_dir" path_string ~dec_absent:default.paths.static_dir ~enc:(fun p -> p.static_dir)
    |> finish
  ))

let site_codec =
  Tomlt.(Table.(
    obj (fun base_url name description author_handle author_name author_email author_orcid ->
      { base_url; name; description; author_handle; author_name; author_email; author_orcid })
    |> mem "base_url" string ~dec_absent:default.site.base_url ~enc:(fun s -> s.base_url)
    |> mem "name" string ~dec_absent:default.site.name ~enc:(fun s -> s.name)
    |> mem "description" string ~dec_absent:default.site.description ~enc:(fun s -> s.description)
    |> mem "author_handle" string ~dec_absent:default.site.author_handle ~enc:(fun s -> s.author_handle)
    |> mem "author_name" string ~dec_absent:default.site.author_name ~enc:(fun s -> s.author_name)
    |> opt_mem "author_email" string ~enc:(fun s -> s.author_email)
    |> opt_mem "author_orcid" string ~enc:(fun s -> s.author_orcid)
    |> finish
  ))

let feeds_codec =
  Tomlt.(Table.(
    obj (fun title subtitle -> { title; subtitle })
    |> mem "title" string ~dec_absent:default.feeds.title ~enc:(fun f -> f.title)
    |> opt_mem "subtitle" string ~enc:(fun f -> f.subtitle)
    |> finish
  ))

let well_known_entry_codec =
  Tomlt.(Table.(
    obj (fun key value -> { key; value })
    |> mem "key" string ~enc:(fun e -> e.key)
    |> mem "value" string ~enc:(fun e -> e.value)
    |> finish
  ))

(** Codec for well_known as a table of key-value pairs *)
let well_known_codec =
  Tomlt.(Table.(
    keep_unknown
      ~enc:(fun wk -> List.map (fun e -> (e.key, e.value)) wk)
      (Mems.assoc string)
      (obj (fun assoc -> List.map (fun (key, value) -> { key; value }) assoc))
    |> finish
  ))

let config_codec =
  Tomlt.(Table.(
    obj (fun server paths site feeds well_known ->
      { server; paths; site; feeds; well_known })
    |> mem "server" server_codec ~dec_absent:default.server ~enc:(fun c -> c.server)
    |> mem "paths" paths_codec ~dec_absent:default.paths ~enc:(fun c -> c.paths)
    |> mem "site" site_codec ~dec_absent:default.site ~enc:(fun c -> c.site)
    |> mem "feeds" feeds_codec ~dec_absent:default.feeds ~enc:(fun c -> c.feeds)
    |> mem "well_known" well_known_codec ~dec_absent:[] ~enc:(fun c -> c.well_known)
    |> finish
  ))

let of_toml_string s =
  match Tomlt_bytesrw.decode_string config_codec s with
  | Ok cfg -> cfg
  | Error e -> failwith (Tomlt.Error.to_string e)

let of_file path =
  let ic = open_in path in
  let content = really_input_string ic (in_channel_length ic) in
  close_in ic;
  of_toml_string content

let config_file () =
  let xdg_config = Sys.getenv_opt "XDG_CONFIG_HOME" in
  let home = Sys.getenv_opt "HOME" in
  match xdg_config, home with
  | Some xdg, _ -> Filename.concat xdg "arod/config.toml"
  | None, Some h -> Filename.concat h ".config/arod/config.toml"
  | None, None -> "./config.toml"

let load_or_default ?path () =
  let path = match path with
    | Some p -> p
    | None -> config_file ()
  in
  if Sys.file_exists path then
    of_file path
  else
    default

(** {1 Sample Config Generation} *)

let sample_config = {|# Arod Webserver Configuration

[server]
host = "0.0.0.0"
port = 8080

[paths]
# Bushel data directory (notes, papers, projects, etc.)
data_dir = "~/bushel"
# Static assets (CSS, JS, icons)
assets_dir = "./assets"
# Processed images from srcsetter
images_dir = "~/bushel/images/web"
# Static files (PDFs, etc.)
static_dir = "./static"

[site]
base_url = "https://example.com"
name = "My Site"
description = "A personal website powered by Bushel"
author_handle = "me"
author_name = "Your Name"
# author_email = "you@example.com"
# author_orcid = "0000-0000-0000-0000"

[feeds]
title = "Site Feed"
# subtitle = "Latest posts and updates"

# Optional: well-known endpoints for AT Protocol, etc.
# [well_known]
# "site.standard.publication" = "at://did:plc:example/app.bsky.feed.post/id"
|}

(** {1 Pretty Printing} *)

let pp ppf t =
  let open Fmt in
  pf ppf "@[<v>";
  pf ppf "Server:@,";
  pf ppf "  host: %s@," t.server.host;
  pf ppf "  port: %d@," t.server.port;
  pf ppf "@,Paths:@,";
  pf ppf "  data_dir: %s@," t.paths.data_dir;
  pf ppf "  assets_dir: %s@," t.paths.assets_dir;
  pf ppf "  images_dir: %s@," t.paths.images_dir;
  pf ppf "  static_dir: %s@," t.paths.static_dir;
  pf ppf "@,Site:@,";
  pf ppf "  base_url: %s@," t.site.base_url;
  pf ppf "  name: %s@," t.site.name;
  pf ppf "  author: %s (@%s)@," t.site.author_name t.site.author_handle;
  pf ppf "@]"
