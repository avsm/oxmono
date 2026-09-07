(*---------------------------------------------------------------------------
   Copyright (c) 2025 Anil Madhavapeddy. All rights reserved.
   SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

type t = {
  cred : Xrpc.Credential.t;
  mutable client : Xrpc.Client.t option;
  fs : Eio.Fs.dir_ty Eio.Path.t;
  pds : string;
  app_name : string;
  profile : string option ref;
  make_client : service:string -> Xrpc.Client.t;
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
  { cred; client = None; fs; pds; app_name; profile = profile_ref; make_client }

let login t ~identifier ~password =
  let first_login = Xrpc_auth_session.list_profiles t.fs ~app_name:t.app_name = [] in
  let client = Xrpc.Credential.login t.cred ~identifier ~password () in
  t.client <- Some client;
  if first_login then Option.iter
    (Xrpc_auth_session.set_current_profile t.fs ~app_name:t.app_name) !(t.profile)

let resume t ~session =
  if Xrpc.Client.normalize_service session.Xrpc_auth_session.pds <> t.pds then
    invalid_arg "Saved session PDS does not match configured PDS";
  if !(t.profile) = None then
    t.profile := Some (Xrpc_auth_session.get_current_profile t.fs ~app_name:t.app_name);
  let xrpc_session = Xrpc_auth_session.to_xrpc session in
  let client = Xrpc.Credential.resume t.cred ~session:xrpc_session () in
  t.client <- Some client

let logout t =
  Fun.protect ~finally:(fun () ->
    t.client <- None;
    Xrpc_auth_session.clear t.fs ~app_name:t.app_name ?profile:!(t.profile) ())
    (fun () -> Xrpc.Credential.logout t.cred)

let get_session t =
  Option.map
    (fun xrpc_session -> Xrpc_auth_session.of_xrpc ~pds:t.pds xrpc_session)
    (Xrpc.Credential.get_session t.cred)

let is_logged_in t = Option.is_some (Xrpc.Credential.get_session t.cred)

let get_client t =
  match t.client with Some c -> c | None -> failwith "Not logged in"

let get_did t =
  match Xrpc.Credential.get_session t.cred with
  | Some session -> session.did
  | None -> failwith "Not logged in"

let get_pds t = t.pds
let get_app_name t = t.app_name
let get_profile t = !(t.profile)
let get_fs t = t.fs
let make_client t = t.make_client
