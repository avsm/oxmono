(*---------------------------------------------------------------------------
   Copyright (c) 2025 Anil Madhavapeddy. All rights reserved.
   SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

type t = {
  client : Immich.t;
  session : Session.t;
  fs : Eio.Fs.dir_ty Eio.Path.t;
  profile : string option;
}

(* JSON type for .well-known/immich response *)
let well_known_jsont =
  let api_obj =
    Jsont.Object.map ~kind:"api" (fun endpoint -> endpoint)
    |> Jsont.Object.mem "endpoint" Jsont.string ~enc:Fun.id
    |> Jsont.Object.finish
  in
  Jsont.Object.map ~kind:"well-known" (fun api -> api)
  |> Jsont.Object.mem "api" api_obj ~enc:Fun.id
  |> Jsont.Object.finish

(** [resolve_api_url ~session base_url] resolves the actual API URL.

    If [base_url] already ends with [/api], returns it unchanged.
    Otherwise, tries to fetch [<base_url>/.well-known/immich] to discover
    the API endpoint. Falls back to [<base_url>/api] if discovery fails. *)
let resolve_api_url ~session base_url =
  (* Remove trailing slash if present *)
  let base_url =
    if String.ends_with ~suffix:"/" base_url then
      String.sub base_url 0 (String.length base_url - 1)
    else base_url
  in
  (* If already has /api suffix, use as-is *)
  if String.ends_with ~suffix:"/api" base_url then
    base_url
  else begin
    (* Try .well-known/immich discovery *)
    let well_known_url = base_url ^ "/.well-known/immich" in
    try
      let response = Requests.get session well_known_url in
      if Requests.Response.ok response then begin
        let json = Requests.Response.json response in
        let endpoint = Openapi.Runtime.Json.decode_json_exn well_known_jsont json in
        (* Construct full API URL *)
        if String.starts_with ~prefix:"/" endpoint then
          base_url ^ endpoint
        else
          base_url ^ "/" ^ endpoint
      end
      else
        (* Discovery failed, default to /api *)
        base_url ^ "/api"
    with _ ->
      (* Any error, default to /api *)
      base_url ^ "/api"
  end

let create_with_session ~sw ~env ?requests_config ?profile ~session () =
  let fs = env#fs in
  let server_url = Session.server_url session in
  (* Create a Requests session, optionally from cmdliner config *)
  let requests_session = match requests_config with
    | Some config -> Requests.Cmd.create config env sw
    | None -> Requests.create ~sw env
  in
  let requests_session =
    match Session.auth session with
    | Session.Jwt { access_token; _ } ->
        Requests.set_auth requests_session (Requests.Auth.bearer ~token:access_token)
    | Session.Api_key { key; _ } ->
        Requests.set_default_header requests_session "x-api-key" key
  in
  let client = Immich.create ~session:requests_session ~sw env ~base_url:server_url in
  { client; session; fs; profile }

let login_api_key ~sw ~env ?requests_config ?profile ~server_url ~api_key ?key_name () =
  let fs = env#fs in
  (* Create session with API key header *)
  let requests_session = match requests_config with
    | Some config -> Requests.Cmd.create config env sw
    | None -> Requests.create ~sw env
  in
  let requests_session = Requests.set_default_header requests_session "x-api-key" api_key in
  (* Resolve the API URL from .well-known/immich if available *)
  let server_url = resolve_api_url ~session:requests_session server_url in
  let client = Immich.create ~session:requests_session ~sw env ~base_url:server_url in
  (* Validate by calling the validate endpoint *)
  let resp = Immich.ValidateAccessToken.validate_access_token client () in
  if not (Immich.ValidateAccessToken.ResponseDto.auth_status resp) then
    failwith "API key validation failed";
  (* Create and save session *)
  let auth = Session.Api_key { key = api_key; name = key_name } in
  let session = Session.create ~server_url ~auth () in
  Session.save fs ?profile session;
  (* Set as current profile if first login or explicitly requested *)
  let profiles = Session.list_profiles fs in
  let profile_name = Option.value ~default:Session.default_profile profile in
  if profiles = [] || Option.is_some profile then
    Session.set_current_profile fs profile_name;
  { client; session; fs; profile }

let login_password ~sw ~env ?requests_config ?profile ~server_url ~email ~password () =
  let fs = env#fs in
  (* Create session without auth first *)
  let requests_session = match requests_config with
    | Some config -> Requests.Cmd.create config env sw
    | None -> Requests.create ~sw env
  in
  (* Resolve the API URL from .well-known/immich if available *)
  let server_url = resolve_api_url ~session:requests_session server_url in
  let client = Immich.create ~session:requests_session ~sw env ~base_url:server_url in
  (* Login using the API *)
  let body = Immich.LoginCredential.Dto.v ~email ~password () in
  let resp = Immich.Login.login client ~body () in
  let access_token = Immich.Login.ResponseDto.access_token resp in
  let user_id = Immich.Login.ResponseDto.user_id resp in
  (* Now create a new client with the auth token *)
  let requests_session = match requests_config with
    | Some config -> Requests.Cmd.create config env sw
    | None -> Requests.create ~sw env
  in
  let requests_session = Requests.set_auth requests_session (Requests.Auth.bearer ~token:access_token) in
  let client = Immich.create ~session:requests_session ~sw env ~base_url:server_url in
  (* Create and save session *)
  let auth = Session.Jwt { access_token; user_id; email } in
  let session = Session.create ~server_url ~auth () in
  Session.save fs ?profile session;
  (* Set as current profile if first login or explicitly requested *)
  let profiles = Session.list_profiles fs in
  let profile_name = Option.value ~default:email profile in
  if profiles = [] || Option.is_some profile then
    Session.set_current_profile fs profile_name;
  { client; session; fs; profile }

let resume ~sw ~env ?requests_config ?profile ~session () =
  (* Check if JWT is expired and refresh if needed *)
  let session =
    if Session.is_expired session then begin
      match Session.auth session with
      | Session.Api_key _ -> session  (* API keys don't expire *)
      | Session.Jwt _ ->
          (* JWT expired - for now just fail, user needs to re-login *)
          failwith "Session expired. Please login again."
    end
    else session
  in
  create_with_session ~sw ~env ?requests_config ?profile ~session ()

let logout t =
  Session.clear t.fs ?profile:t.profile ()

let client t = t.client
let session t = t.session
let profile t = t.profile
let fs t = t.fs

let is_valid t =
  try
    let resp = Immich.ValidateAccessToken.validate_access_token t.client () in
    Immich.ValidateAccessToken.ResponseDto.auth_status resp
  with _ -> false
