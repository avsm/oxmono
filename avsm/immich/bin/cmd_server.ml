(*---------------------------------------------------------------------------
   Copyright (c) 2025 Anil Madhavapeddy. All rights reserved.
   SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

open Cmdliner

(* Styled output helpers *)
let header_style = Fmt.(styled `Bold string)
let label_style = Fmt.(styled `Faint string)
let value_style = Fmt.(styled (`Fg `Cyan) string)
let version_style = Fmt.(styled (`Fg `Green) string)
let success_style = Fmt.(styled (`Fg `Green) string)
let error_style = Fmt.(styled (`Fg `Red) string)

let percent_style pct =
  if pct >= 90.0 then Fmt.(styled (`Fg `Red) (fun ppf -> Fmt.pf ppf "%.1f%%"))
  else if pct >= 70.0 then Fmt.(styled (`Fg `Yellow) (fun ppf -> Fmt.pf ppf "%.1f%%"))
  else Fmt.(styled (`Fg `Green) (fun ppf -> Fmt.pf ppf "%.1f%%"))

(* Ping command - can work with or without auth *)

let ping_action ~requests_config ~server ~profile env =
  Immich_auth.Error.wrap (fun () ->
    Eio.Switch.run @@ fun sw ->
    let fs = env#fs in
    let server_url =
      match server with
      | Some url -> url
      | None ->
          (* Try to get from session *)
          let profile = match profile with
            | Some p -> Some p
            | None -> Some (Immich_auth.Session.get_current_profile fs)
          in
          match Immich_auth.Session.load fs ?profile () with
          | Some session -> Immich_auth.Session.server_url session
          | None ->
              Fmt.epr "%a No server specified and not logged in.@." error_style "Error:";
              Fmt.epr "Use --server or login first.@.";
              raise (Immich_auth.Error.Exit_code 1)
    in
    (* Create session using requests config *)
    let session = Requests.Cmd.create requests_config env sw in
    (* Resolve the API URL from .well-known/immich if available *)
    let server_url = Immich_auth.Client.resolve_api_url ~session server_url in
    let client = Immich.create ~session ~sw env ~base_url:server_url in
    let resp = Immich.ServerPing.ping_server client () in
    Fmt.pr "%a %a@." success_style "Server:" value_style (Immich.ServerPing.Response.res resp)
  )

let ping_cmd env fs =
  let doc = "Ping the Immich server." in
  let info = Cmd.info "ping" ~doc in
  let ping' (style_renderer, level) requests_config server profile =
    Immich_auth.Cmd.setup_logging_with_config style_renderer level requests_config;
    ping_action ~requests_config ~server ~profile env
  in
  Cmd.v info Term.(const ping' $ Immich_auth.Cmd.setup_logging $ Immich_auth.Cmd.requests_config_term fs $ Immich_auth.Cmd.server_opt $ Immich_auth.Cmd.profile_arg)

(* Status command - requires auth *)

let status_action ~requests_config ~profile env =
  Immich_auth.Error.wrap (fun () ->
    Immich_auth.Cmd.with_client ~requests_config ?profile (fun _fs client ->
      let api = Immich_auth.Client.client client in
      (* Get server version *)
      let version = Immich.ServerVersion.get_server_version api () in
      Fmt.pr "%a %a@." header_style "Server Version:"
        version_style (Printf.sprintf "%d.%d.%d"
          (Immich.ServerVersion.ResponseDto.major version)
          (Immich.ServerVersion.ResponseDto.minor version)
          (Immich.ServerVersion.ResponseDto.patch version));
      (* Get server about *)
      let about = Immich.ServerAbout.get_about_info api () in
      let full_version = Immich.ServerAbout.ResponseDto.version about in
      if full_version <> "" then
        Fmt.pr "  %a %a@." label_style "Full version:" value_style full_version;
      (match Immich.ServerAbout.ResponseDto.build about with
       | Some b -> Fmt.pr "  %a %a@." label_style "Build:" value_style b
       | None -> ());
      (* Get storage info *)
      let storage = Immich.ServerStorage.get_storage api () in
      let pct = Immich.ServerStorage.ResponseDto.disk_usage_percentage storage in
      Fmt.pr "%a@." header_style "Storage:";
      Fmt.pr "  %a %a@." label_style "Disk size:" value_style (Immich.ServerStorage.ResponseDto.disk_size storage);
      Fmt.pr "  %a %a@." label_style "Disk used:" value_style (Immich.ServerStorage.ResponseDto.disk_use storage);
      Fmt.pr "  %a %a@." label_style "Disk available:" value_style (Immich.ServerStorage.ResponseDto.disk_available storage);
      Fmt.pr "  %a %a@." label_style "Usage:" (percent_style pct) pct
    ) env
  )

let status_cmd env fs =
  let doc = "Show server status." in
  let info = Cmd.info "status" ~doc in
  let status' (style_renderer, level) requests_config profile =
    Immich_auth.Cmd.setup_logging_with_config style_renderer level requests_config;
    status_action ~requests_config ~profile env
  in
  Cmd.v info Term.(const status' $ Immich_auth.Cmd.setup_logging $ Immich_auth.Cmd.requests_config_term fs $ Immich_auth.Cmd.profile_arg)

(* Server command group *)

let server_cmd env fs =
  let doc = "Server information commands." in
  let info = Cmd.info "server" ~doc in
  Cmd.group info
    [ ping_cmd env fs
    ; status_cmd env fs
    ]
