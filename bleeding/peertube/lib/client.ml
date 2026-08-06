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

(* The access token is scoped to the instance it was issued by; a
   plain-http instance has to opt in to carrying it. *)
let base_client ~sw ~env ?http_config () =
  match http_config with
  | Some config -> Fetch_cmdliner.create config env sw
  | None -> Fetch_curl.std ~sw env

let with_bearer ~scope ~token t =
  Fetch.with_credentials ~scope:[ scope ]
    ~allow_insecure:(String.starts_with ~prefix:"http://" scope)
    Fetch.Credential.[ Bearer (fun () -> token) ] t

let create_with_session ~sw ~env ?http_config ?profile ~session () =
  let fs = env#fs in
  let server_url = Session.server_url session in
  let base = base_client ~sw ~env ?http_config () in
  let http =
    match Session.auth session with
    | Session.OAuth { access_token; _ } ->
        with_bearer ~scope:server_url ~token:access_token base
  in
  let client = Peer_tube.create ~session:http ~sw env ~base_url:server_url in
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

let login_password ~sw ~env ?http_config ?profile ~server_url ~username ~password () =
  let fs = env#fs in
  (* Create session without auth first *)
  let anon = base_client ~sw ~env ?http_config () in
  let client = Peer_tube.create ~session:anon ~sw env ~base_url:server_url in

  (* Step 1: Get OAuth client credentials *)
  let oauth_client = Peer_tube.OauthClient.get_oauth_client client () in
  let client_id = Option.get (Peer_tube.OauthClient.T.client_id oauth_client) in
  let client_secret = Option.get (Peer_tube.OauthClient.T.client_secret oauth_client) in

  (* Step 2: Get OAuth token using password grant *)
  let token_url = server_url ^ "/api/v1/users/token" in
  (* [Form.urlencoded] returns the Content-Type header along with the body,
     so both have to be threaded into the request. *)
  let form_headers, form_body =
    Fetch.Form.urlencoded [
      ("client_id", client_id);
      ("client_secret", client_secret);
      ("grant_type", "password");
      ("username", username);
      ("password", password);
    ]
  in
  let status, text =
    Fetch.with_response ~headers:form_headers ~body:form_body anon `POST token_url
    @@ fun resp -> (Fetch.status resp, Eio.Flow.read_all (Fetch.body resp))
  in
  if status < 200 || status >= 300 then
    failwith (Printf.sprintf "Login failed: %d" status);

  let (access_token, refresh_token) =
    Openapi.Runtime.Json.decode_exn oauth_token_jsont text
  in

  (* Now create a new client with the auth token *)
  let http =
    base_client ~sw ~env ?http_config ()
    |> with_bearer ~scope:server_url ~token:access_token
  in
  let client = Peer_tube.create ~session:http ~sw env ~base_url:server_url in

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

let resume ~sw ~env ?http_config ?profile ~session () =
  create_with_session ~sw ~env ?http_config ?profile ~session ()

let logout t =
  Session.clear t.fs ?profile:t.profile ()

let client t = t.client
let session t = t.session
let profile t = t.profile
let fs t = t.fs
