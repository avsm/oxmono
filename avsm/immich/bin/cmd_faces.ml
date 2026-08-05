(*---------------------------------------------------------------------------
   Copyright (c) 2025 Anil Madhavapeddy. All rights reserved.
   SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

open Cmdliner

(* Styled output helpers *)
let header_style = Fmt.(styled `Bold string)
let id_style = Fmt.(styled `Faint string)
let name_style = Fmt.(styled (`Fg `Cyan) string)
let hidden_style = Fmt.(styled (`Fg `Yellow) string)
let count_style = Fmt.(styled (`Fg `Green) int)
let success_style = Fmt.(styled (`Fg `Green) string)

(* Search for people by name *)

let search_action ~requests_config ~profile ~name ~with_hidden env =
  Immich_auth.Error.wrap (fun () ->
    Immich_auth.Cmd.with_client ~requests_config ?profile (fun _fs client ->
      let api = Immich_auth.Client.client client in
      let session = Immich.session api in
      let base_url = Immich.base_url api in
      (* Person.search_person returns ResponseDto (single) but should be a list *)
      (* Use low-level API to get proper list response *)
      let query = Printf.sprintf "?name=%s%s"
        (Uri.pct_encode name)
        (if with_hidden then "&withHidden=true" else "") in
      let url = base_url ^ "/people/search" ^ query in
      let response = Requests.get session url in
      if Requests.Response.ok response then begin
        let json = Requests.Response.json response in
        let people = Openapi.Runtime.Json.decode_json_exn
          (Jsont.list Immich.Person.ResponseDto.jsont) json in
        if people = [] then
          Fmt.pr "%a '%a'@."
            Fmt.(styled `Faint string) "No people found matching"
            name_style name
        else begin
          Fmt.pr "%a '%a':@."
            header_style "People matching"
            name_style name;
          List.iter (fun person ->
            let pname = Immich.Person.ResponseDto.name person in
            let id = Immich.Person.ResponseDto.id person in
            let is_hidden = Immich.Person.ResponseDto.is_hidden person in
            let display_name = if pname = "" then "<unnamed>" else pname in
            Fmt.pr "  %a  %a%a@."
              id_style id
              name_style display_name
              hidden_style (if is_hidden then " (hidden)" else "")
          ) people
        end
      end else begin
        (* Raise API error for proper handling *)
        raise (Openapi.Runtime.Api_error {
          operation = "search_people";
          method_ = "GET";
          url;
          status = Requests.Response.status_code response;
          body = Requests.Response.text response;
          parsed_body = None;
        })
      end
    ) env
  )

let name_arg =
  let doc = "Name to search for." in
  Arg.(required & pos 0 (some string) None & info [] ~docv:"NAME" ~doc)

let with_hidden_arg =
  let doc = "Include hidden people in results." in
  Arg.(value & flag & info ["with-hidden"] ~doc)

let search_cmd env fs =
  let doc = "Search for people by name." in
  let info = Cmd.info "search" ~doc in
  let search' (style_renderer, level) requests_config profile name with_hidden =
    Immich_auth.Cmd.setup_logging_with_config style_renderer level requests_config;
    search_action ~requests_config ~profile ~name ~with_hidden env
  in
  Cmd.v info Term.(const search' $ Immich_auth.Cmd.setup_logging $ Immich_auth.Cmd.requests_config_term fs $ Immich_auth.Cmd.profile_arg $ name_arg $ with_hidden_arg)

(* Get person thumbnail *)

let output_arg =
  let doc = "Output file path. Use '-' for stdout." in
  Arg.(value & opt string "-" & info ["output"; "o"] ~docv:"FILE" ~doc)

let person_id_arg =
  let doc = "Person ID." in
  Arg.(required & pos 0 (some string) None & info [] ~docv:"PERSON_ID" ~doc)

let thumbnail_action ~requests_config ~profile ~person_id ~output env =
  Immich_auth.Error.wrap (fun () ->
    Immich_auth.Cmd.with_client ~requests_config ?profile (fun _fs client ->
      let api = Immich_auth.Client.client client in
      let session = Immich.session api in
      let base_url = Immich.base_url api in
      let url = Printf.sprintf "%s/people/%s/thumbnail" base_url person_id in
      let response = Requests.get session url in
      if Requests.Response.ok response then begin
        let data = Requests.Response.text response in
        if output = "-" then
          print_string data
        else begin
          let oc = open_out_bin output in
          output_string oc data;
          close_out oc;
          Fmt.pr "%a %a@."
            success_style "Thumbnail saved to"
            Fmt.(styled `Bold string) output
        end
      end else begin
        raise (Openapi.Runtime.Api_error {
          operation = "get_person_thumbnail";
          method_ = "GET";
          url;
          status = Requests.Response.status_code response;
          body = Requests.Response.text response;
          parsed_body = None;
        })
      end
    ) env
  )

let thumbnail_cmd env fs =
  let doc = "Download a person's thumbnail image." in
  let info = Cmd.info "thumbnail" ~doc in
  let thumbnail' (style_renderer, level) requests_config profile person_id output =
    Immich_auth.Cmd.setup_logging_with_config style_renderer level requests_config;
    thumbnail_action ~requests_config ~profile ~person_id ~output env
  in
  Cmd.v info Term.(const thumbnail' $ Immich_auth.Cmd.setup_logging $ Immich_auth.Cmd.requests_config_term fs $ Immich_auth.Cmd.profile_arg $ person_id_arg $ output_arg)

(* List all people *)

let list_action ~requests_config ~profile ~with_hidden env =
  Immich_auth.Error.wrap (fun () ->
    Immich_auth.Cmd.with_client ~requests_config ?profile (fun _fs client ->
      let api = Immich_auth.Client.client client in
      let with_hidden_param = if with_hidden then Some "true" else None in
      let resp = Immich.People.get_all_people api ?with_hidden:with_hidden_param () in
      let people = Immich.People.ResponseDto.people resp in
      let total = Immich.People.ResponseDto.total resp in
      let hidden_count = Immich.People.ResponseDto.hidden resp in
      Fmt.pr "%a %a total, %a hidden@."
        header_style "People:"
        count_style total
        count_style hidden_count;
      if people = [] then
        Fmt.pr "%a@." Fmt.(styled `Faint string) "No people found."
      else begin
        List.iter (fun person ->
          let name = Immich.Person.ResponseDto.name person in
          let id = Immich.Person.ResponseDto.id person in
          let is_hidden = Immich.Person.ResponseDto.is_hidden person in
          let display_name = if name = "" then "<unnamed>" else name in
          Fmt.pr "  %a  %a%a@."
            id_style id
            name_style display_name
            hidden_style (if is_hidden then " (hidden)" else "")
        ) people
      end
    ) env
  )

let list_cmd env fs =
  let doc = "List all people." in
  let info = Cmd.info "list" ~doc in
  let list' (style_renderer, level) requests_config profile with_hidden =
    Immich_auth.Cmd.setup_logging_with_config style_renderer level requests_config;
    list_action ~requests_config ~profile ~with_hidden env
  in
  Cmd.v info Term.(const list' $ Immich_auth.Cmd.setup_logging $ Immich_auth.Cmd.requests_config_term fs $ Immich_auth.Cmd.profile_arg $ with_hidden_arg)

(* Faces command group *)

let faces_cmd env fs =
  let doc = "Face and people commands." in
  let info = Cmd.info "faces" ~doc in
  Cmd.group info
    [ list_cmd env fs
    ; search_cmd env fs
    ; thumbnail_cmd env fs
    ]
