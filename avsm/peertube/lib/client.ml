(*---------------------------------------------------------------------------
   Copyright (c) 2025 Anil Madhavapeddy. All rights reserved.
   SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

type t = {
  client : Peer_tube.t;
  session : Session.t;
  fs : Eio.Fs.dir_ty Eio.Path.t;
  profile : string option;
}

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
    | Session.OAuth { access_token; _ } ->
        Requests.set_auth requests_session (Requests.Auth.bearer ~token:access_token)
  in
  let client = Peer_tube.create ~session:requests_session ~sw env ~base_url:server_url in
  { client; session; fs; profile }

(* OAuth token response codec *)
let oauth_token_jsont =
  Jsont.Object.map ~kind:"OAuthToken"
    (fun access_token refresh_token ->
      (access_token, refresh_token))
  |> Jsont.Object.mem "access_token" Jsont.string ~enc:fst
  |> Jsont.Object.opt_mem "refresh_token" Jsont.string ~enc:snd
  |> Jsont.Object.skip_unknown
  |> Jsont.Object.finish

let login_password ~sw ~env ?requests_config ?profile ~server_url ~username ~password () =
  let fs = env#fs in
  (* Create session without auth first *)
  let requests_session = match requests_config with
    | Some config -> Requests.Cmd.create config env sw
    | None -> Requests.create ~sw env
  in
  let client = Peer_tube.create ~session:requests_session ~sw env ~base_url:server_url in

  (* Step 1: Get OAuth client credentials *)
  let oauth_client = Peer_tube.OauthClient.get_oauth_client client () in
  let client_id = Option.get (Peer_tube.OauthClient.T.client_id oauth_client) in
  let client_secret = Option.get (Peer_tube.OauthClient.T.client_secret oauth_client) in

  (* Step 2: Get OAuth token using password grant *)
  let token_url = server_url ^ "/api/v1/users/token" in
  let body = Requests.Body.form [
    ("client_id", client_id);
    ("client_secret", client_secret);
    ("grant_type", "password");
    ("username", username);
    ("password", password);
  ] in
  let resp = Requests.post requests_session token_url ~body in
  if not (Requests.Response.ok resp) then
    failwith (Printf.sprintf "Login failed: %d" (Requests.Response.status_code resp));

  let json = Requests.Response.json resp in
  let (access_token, refresh_token) = Openapi.Runtime.Json.decode_json_exn oauth_token_jsont json in

  (* Now create a new client with the auth token *)
  let requests_session = match requests_config with
    | Some config -> Requests.Cmd.create config env sw
    | None -> Requests.create ~sw env
  in
  let requests_session = Requests.set_auth requests_session (Requests.Auth.bearer ~token:access_token) in
  let client = Peer_tube.create ~session:requests_session ~sw env ~base_url:server_url in

  (* Create and save session *)
  let auth = Session.OAuth { access_token; refresh_token; client_id; client_secret } in
  let session = Session.create ~server_url ~auth () in
  Session.save fs ?profile session;

  (* Set as current profile if first login or explicitly requested *)
  let profiles = Session.list_profiles fs in
  let profile_name = Option.value ~default:username profile in
  if profiles = [] || Option.is_some profile then
    Session.set_current_profile fs profile_name;
  { client; session; fs; profile }

let resume ~sw ~env ?requests_config ?profile ~session () =
  create_with_session ~sw ~env ?requests_config ?profile ~session ()

let logout t =
  Session.clear t.fs ?profile:t.profile ()

let client t = t.client
let session t = t.session
let profile t = t.profile
let fs t = t.fs
