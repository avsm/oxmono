(*---------------------------------------------------------------------------
  Copyright (c) 2025 Anil Madhavapeddy <anil@recoil.org>. All rights reserved.
  SPDX-License-Identifier: ISC
 ---------------------------------------------------------------------------*)

(** Arod webserver - a tiny_httpd based server for Bushel content *)

(** {1 Logging} *)

let src = Logs.Src.create "arod" ~doc:"Arod webserver"

module Log = (val Logs.src_log src : Logs.LOG)

(** {1 CLI} *)

open Cmdliner

let setup_logging style_renderer level =
  Fmt_tty.setup_std_outputs ?style_renderer ();
  Logs.set_level level;
  Logs.set_reporter (Logs_fmt.reporter ())

let logging_t =
  let open Cmdliner in
  Term.(const setup_logging $ Fmt_cli.style_renderer () $ Logs_cli.level ())

let config_file =
  let doc = "Path to config file (default: ~/.config/arod/config.toml)." in
  Arg.(value & opt (some file) None & info [ "c"; "config" ] ~docv:"FILE" ~doc)

let memo_ttl =
  let doc = "TTL in seconds for memoized routes (default: 300.0)." in
  Arg.(value & opt float 300.0 & info [ "memo-ttl" ] ~docv:"SECONDS" ~doc)

let serve_cmd =
  let run () config_file memo_ttl =
    let cfg = Arod.Config.load_or_default ?path:config_file () in
    Log.info (fun m -> m "Starting Arod server...");
    Log.info (fun m -> m "Config:@.%a" Arod.Config.pp cfg);
    Eio_main.run @@ fun env ->
    let fs = Eio.Stdenv.fs env in
    Log.info (fun m -> m "Loading entries from %s" cfg.paths.data_dir);
    let _entries = Arod.Model.init ~cfg fs in
    Log.info (fun m ->
        m "Loaded %d notes, %d papers, %d projects, %d ideas, %d videos, %d images"
          (List.length (Arod.Model.notes ()))
          (List.length (Arod.Model.papers ()))
          (List.length (Arod.Model.projects ()))
          (List.length (Arod.Model.ideas ()))
          (List.length (Arod.Model.videos ()))
          (List.length (Arod.Model.images ())));
    (* Create memoization cache *)
    let memo_cache = Arod.Memo.create ~ttl:memo_ttl () in
    (* Get all routes *)
    let routes = Arod.Handlers.all_routes cfg in
    (* Run the server *)
    match Arod_server.run ~config:cfg ~memo_cache routes with
    | Ok () -> 0
    | Error e ->
        Log.err (fun m -> m "Server error: %s" (Printexc.to_string e));
        1
  in
  let doc = "Start the Arod webserver." in
  let info = Cmd.info "serve" ~doc in
  Cmd.v info Term.(const run $ logging_t $ config_file $ memo_ttl)

let init_cmd =
  let run () =
    let path = Arod.Config.config_file () in
    let dir = Filename.dirname path in
    if not (Sys.file_exists dir) then Unix.mkdir dir 0o755;
    if Sys.file_exists path then begin
      Printf.eprintf "Config file already exists: %s\n" path;
      1
    end
    else begin
      let oc = open_out path in
      output_string oc Arod.Config.sample_config;
      close_out oc;
      Printf.printf "Created config file: %s\n" path;
      0
    end
  in
  let doc = "Initialize a default configuration file." in
  let info = Cmd.info "init" ~doc in
  Cmd.v info Term.(const run $ const ())

let config_cmd =
  let run () config_file =
    let cfg = Arod.Config.load_or_default ?path:config_file () in
    Fmt.pr "%a\n" Arod.Config.pp cfg;
    0
  in
  let doc = "Show current configuration." in
  let info = Cmd.info "config" ~doc in
  Cmd.v info Term.(const run $ logging_t $ config_file)

let main_cmd =
  let doc = "Arod - a webserver for Bushel content" in
  let man =
    [
      `S Manpage.s_description;
      `P
        "Arod is a tiny_httpd-based webserver that serves Bushel content \
         (notes, papers, projects, ideas, videos) as a website.";
      `S "CONFIGURATION";
      `P "Configuration is read from ~/.config/arod/config.toml";
      `P "Run $(b,arod init) to create a default config file.";
    ]
  in
  let info = Cmd.info "arod" ~version:"0.1.0" ~doc ~man in
  Cmd.group info [ serve_cmd; init_cmd; config_cmd ]

let () =
  match Cmd.eval_value main_cmd with
  | Ok (`Ok exit_code) -> exit exit_code
  | Ok `Help | Ok `Version -> exit 0
  | Error _ -> exit 1
