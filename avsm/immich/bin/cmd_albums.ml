(*---------------------------------------------------------------------------
   Copyright (c) 2025 Anil Madhavapeddy. All rights reserved.
   SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

open Cmdliner

(* Styled output helpers *)
let header_style = Fmt.(styled `Bold string)
let id_style = Fmt.(styled `Faint string)
let name_style = Fmt.(styled (`Fg `Cyan) string)
let count_style = Fmt.(styled (`Fg `Green) int)
let shared_style = Fmt.(styled (`Fg `Yellow) string)

(* List albums - using low-level API since get_all_albums returns array but typed as single *)

let list_action ~requests_config ~profile ~shared env =
  Immich_auth.Error.wrap (fun () ->
    Immich_auth.Cmd.with_client ~requests_config ?profile (fun _fs client ->
      let api = Immich_auth.Client.client client in
      let session = Immich.session api in
      let base_url = Immich.base_url api in
      let query = match shared with
        | None -> ""
        | Some true -> "?shared=true"
        | Some false -> "?shared=false"
      in
      let url = base_url ^ "/albums" ^ query in
      let response = Requests.get session url in
      if Requests.Response.ok response then begin
        let json = Requests.Response.json response in
        let albums = Openapi.Runtime.Json.decode_json_exn
          (Jsont.list Immich.Album.ResponseDto.jsont) json in
        if albums = [] then
          Fmt.pr "%a@." Fmt.(styled `Faint string) "No albums found."
        else begin
          Fmt.pr "%a@." header_style "Albums:";
          List.iter (fun album ->
            let is_shared = Immich.Album.ResponseDto.shared album in
            Fmt.pr "  %a  %a (%a assets)%a@."
              id_style (Immich.Album.ResponseDto.id album)
              name_style (Immich.Album.ResponseDto.album_name album)
              count_style (Immich.Album.ResponseDto.asset_count album)
              shared_style (if is_shared then " (shared)" else "")
          ) albums
        end
      end else begin
        raise (Openapi.Runtime.Api_error {
          operation = "get_all_albums";
          method_ = "GET";
          url;
          status = Requests.Response.status_code response;
          body = Requests.Response.text response;
          parsed_body = None;
        })
      end
    ) env
  )

let shared_arg =
  let doc = "Filter by shared status. Use --shared for shared albums, --no-shared for owned only." in
  Arg.(value & opt (some bool) None & info ["shared"] ~doc)

let list_cmd env fs =
  let doc = "List all albums." in
  let info = Cmd.info "list" ~doc in
  let list' (style_renderer, level) requests_config profile shared =
    Immich_auth.Cmd.setup_logging_with_config style_renderer level requests_config;
    list_action ~requests_config ~profile ~shared env
  in
  Cmd.v info Term.(const list' $ Immich_auth.Cmd.setup_logging $ Immich_auth.Cmd.requests_config_term fs $ Immich_auth.Cmd.profile_arg $ shared_arg)

(* Albums command group *)

let albums_cmd env fs =
  let doc = "Album commands." in
  let info = Cmd.info "albums" ~doc in
  Cmd.group info
    [ list_cmd env fs
    ]
