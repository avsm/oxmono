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
  mutable profile : string option;
  make_client : service:string -> Xrpc.Client.t;
}

let create ~sw ~env ~app_name ?profile ~pds () =
  let fs = env#fs in
  let cred = Xrpc.Credential.create ~sw ~env ~service:pds () in
  let client_ref = ref None in
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
  let make_client ~service = Xrpc.Client.create ~sw ~env ~service () in
  let t = { cred; client = None; fs; pds; app_name; profile; make_client } in
  client_ref := Some t;
  t

let login t ~identifier ~password =
  let client = Xrpc.Credential.login t.cred ~identifier ~password () in
  t.client <- Some client;
  (* Update profile to the handle if not already set *)
  match t.profile with
  | None -> (
      match Xrpc.Credential.get_session t.cred with
      | Some session -> t.profile <- Some session.handle
      | None -> ())
  | Some _ -> ()

let resume t ~session =
  let xrpc_session = Xrpc_auth_session.to_xrpc session in
  let client = Xrpc.Credential.resume t.cred ~session:xrpc_session () in
  t.client <- Some client;
  (* Use the session's handle as profile if not set *)
  if t.profile = None then t.profile <- Some session.handle

let logout t =
  Xrpc.Credential.logout t.cred;
  Xrpc_auth_session.clear t.fs ~app_name:t.app_name ?profile:t.profile ();
  t.client <- None

let get_session t =
  Option.map
    (fun xrpc_session -> Xrpc_auth_session.of_xrpc ~pds:t.pds xrpc_session)
    (Xrpc.Credential.get_session t.cred)

let is_logged_in t = Option.is_some t.client

let get_client t =
  match t.client with Some c -> c | None -> failwith "Not logged in"

let get_did t =
  match Xrpc.Credential.get_session t.cred with
  | Some session -> session.did
  | None -> failwith "Not logged in"

let get_pds t = t.pds
let get_app_name t = t.app_name
let get_profile t = t.profile
let get_fs t = t.fs
let make_client t = t.make_client
