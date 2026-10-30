(*---------------------------------------------------------------------------
   Copyright (c) 2025 Anil Madhavapeddy. All rights reserved.
   SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

type t = {
  cred : Xrpc.Credential.t;
  mutable client : Xrpc.Client.t option;
  mutable oauth : Xrpc_auth_oauth.t option;
  clock : float Eio.Time.clock_ty Eio.Resource.t;
  fs : Eio.Fs.dir_ty Eio.Path.t;
  pds : string;
  app_name : string;
  profile : string option ref;
  make_client : service:string -> Xrpc.Client.t;
  http : Fetch.plain;
}

let create ~sw ~env ~app_name ?profile ~pds ?http () =
  Xrpc_auth_session.validate_name app_name;
  Option.iter Xrpc_auth_session.validate_name profile;
  let pds = Xrpc.Client.normalize_service pds in
  let fs = env#fs in
  let http =
    match http with
    | Some client -> Fetch.restrict client
    | None -> Fetch_curl.std ~sw env
  in
  let cred = Xrpc.Credential.create ~sw ~env ~service:pds ~http () in
  let profile_ref = ref profile in
  (* Set up callback to save session on updates *)
  Xrpc.Credential.on_session_update cred (fun xrpc_session ->
      let session = Xrpc_auth_session.of_xrpc ~pds xrpc_session in
      (* Use the handle as profile name if not specified *)
      let profile =
        match !profile_ref with Some p -> Some p | None -> Some session.handle
      in
      profile_ref := profile;
      Xrpc_auth_session.save fs ~app_name ?profile session);
  Xrpc.Credential.on_session_expired cred (fun () ->
    Xrpc_auth_session.clear fs ~app_name ?profile:!profile_ref ());
  let make_client ~service = Xrpc.Client.create ~sw ~env ~service ~http () in
  {
    cred;
    client = None;
    oauth = None;
    clock = (env#clock :> float Eio.Time.clock_ty Eio.Resource.t);
    fs;
    pds;
    app_name;
    profile = profile_ref;
    make_client;
    http;
  }

let login t ~identifier ~password =
  let first_login = Xrpc_auth_session.list_profiles t.fs ~app_name:t.app_name = [] in
  let client = Xrpc.Credential.login t.cred ~identifier ~password () in
  t.client <- Some client;
  if first_login then Option.iter
    (Xrpc_auth_session.set_current_profile t.fs ~app_name:t.app_name) !(t.profile)

let resume_oauth t session =
  let save session =
    Xrpc_auth_session.save t.fs ~app_name:t.app_name ?profile:!(t.profile)
      session
  in
  let oauth =
    Xrpc_auth_oauth.v ~clock:t.clock ~http:t.http ~on_update:save session
  in
  t.oauth <- Some oauth;
  t.client <-
    Some (Xrpc.Client.of_fetch ~service:t.pds (Xrpc_auth_oauth.http oauth))

let resume t ~session =
  if Xrpc.Client.normalize_service session.Xrpc_auth_session.pds <> t.pds then
    invalid_arg "Saved session PDS does not match configured PDS";
  if !(t.profile) = None then
    t.profile := Some (Xrpc_auth_session.get_current_profile t.fs ~app_name:t.app_name);
  match session.oauth with
  | Some _ -> resume_oauth t session
  | None ->
      let xrpc_session = Xrpc_auth_session.to_xrpc session in
      let client = Xrpc.Credential.resume t.cred ~session:xrpc_session () in
      t.client <- Some client

let login_oauth t ~net ?plc ?identifier ?scope ~open_url () =
  let scope = Option.value ~default:Xrpc_auth_oauth.default_scope scope in
  let session =
    Xrpc_auth_oauth.authorize ~net ~clock:t.clock ~http:t.http ?plc
      ~pds:t.pds ?identifier ~scope ~open_url ()
  in
  if Xrpc.Client.normalize_service session.pds <> t.pds then
    failwith
      (Printf.sprintf "Account is hosted at %s, not %s" session.pds t.pds);
  let app_name = t.app_name in
  let first_login = Xrpc_auth_session.list_profiles t.fs ~app_name = [] in
  if !(t.profile) = None then t.profile := Some session.handle;
  Xrpc_auth_session.save t.fs ~app_name ?profile:!(t.profile) session;
  if first_login then
    Option.iter
      (Xrpc_auth_session.set_current_profile t.fs ~app_name)
      !(t.profile);
  resume_oauth t session

let logout t =
  Fun.protect ~finally:(fun () ->
    t.client <- None;
    t.oauth <- None;
    Xrpc_auth_session.clear t.fs ~app_name:t.app_name ?profile:!(t.profile) ())
    (fun () -> match t.oauth with
      | Some oauth -> Xrpc_auth_oauth.revoke oauth
      | None -> Xrpc.Credential.logout t.cred)

let get_session t =
  match t.oauth with
  | Some oauth -> Some (Xrpc_auth_oauth.session oauth)
  | None ->
      Option.map
        (fun xrpc_session -> Xrpc_auth_session.of_xrpc ~pds:t.pds xrpc_session)
        (Xrpc.Credential.get_session t.cred)

let is_logged_in t =
  Option.is_some t.oauth || Option.is_some (Xrpc.Credential.get_session t.cred)

let get_client t =
  match t.client with Some c -> c | None -> failwith "Not logged in"

let get_did t =
  match t.oauth, Xrpc.Credential.get_session t.cred with
  | Some oauth, _ -> (Xrpc_auth_oauth.session oauth).did
  | None, Some session -> session.did
  | None, None -> failwith "Not logged in"

let get_pds t = t.pds
let get_app_name t = t.app_name
let get_profile t = !(t.profile)
let get_fs t = t.fs
let make_client t = t.make_client

let http t = t.http
