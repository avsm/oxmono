(*---------------------------------------------------------------------------
   Copyright (c) 2025 Anil Madhavapeddy. All rights reserved.
   SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

type t = {
  service : string;
  make_client : ?on_request:(Xrpc_client.t -> unit) -> unit -> Xrpc_client.t;
  mutable session : Xrpc_types.session option;
  mutable on_session_update : (Xrpc_types.session -> unit) option;
  mutable on_session_expired : (unit -> unit) option;
  refresh_mutex : Eio.Mutex.t;
  now : unit -> Ptime.t;
}

let create ~sw ~env ~service ?http () =
  let http = match http with Some client -> Fetch.restrict client | None -> Fetch_curl.std ~sw env in
  let service = Xrpc_client.get_service (Xrpc_client.of_fetch ~service http) in
  let make_client ?on_request () = Xrpc_client.of_fetch ~service ?on_request http in
  let now () = match Ptime.of_float_s (Eio.Time.now env#clock) with
    | Some now -> now | None -> invalid_arg "XRPC clock outside Ptime range" in
  {
    service;
    now;
    make_client;
    session = None;
    on_session_update = None;
    on_session_expired = None;
    refresh_mutex = Eio.Mutex.create ();
  }

let on_session_update t callback = t.on_session_update <- Some callback
let on_session_expired t callback = t.on_session_expired <- Some callback
let get_session t = t.session
let get_service t = t.service

(* Update session and notify callback *)
let update_session t session =
  t.session <- Some session;
  Option.iter (fun callback -> callback session) t.on_session_update

(* Clear session and notify callback *)
let clear_session t =
  t.session <- None;
  Option.iter (fun callback -> callback ()) t.on_session_expired

(* Create raw client for auth operations (no interceptor) *)
let make_raw_client t = t.make_client ()

(* Refresh the session using refresh token *)
let refresh_session t =
  match t.session with
  | None ->
      raise
        (Xrpc_error.err
           (Xrpc_error.Xrpc_error
              {
                status = 401;
                error = "AuthRequired";
                message = Some "No session to refresh";
              }))
  | Some session -> (
      let client = make_raw_client t in
      (* Use refresh token for auth *)
      Xrpc_client.set_session client
        { session with access_jwt = session.refresh_jwt };
      try
        let new_session =
          Xrpc_client.procedure client ~nsid:"com.atproto.server.refreshSession"
            ~params:[] ~input:None ~input_data:None
            ~decoder:Xrpc_types.session_jsont
        in
        if new_session.did <> session.did then
          raise (Xrpc_error.err (Xrpc_error.Parse_error {
            reason = "Refresh changed account identity"; body_preview = None }));
        update_session t new_session;
        Some new_session
      with
      | Eio.Io (Xrpc_error.E (Xrpc_error.Xrpc_error { error; _ }), _)
        when error = "ExpiredToken" || error = "InvalidToken" ->
          clear_session t;
          None
      | exn -> raise exn)

(* 5 minute leeway for token refresh *)
let refresh_leeway = Ptime.Span.of_int_s 300

(* Check token expiry and refresh if needed *)
let check_and_refresh t ~did client =
  Eio.Mutex.use_rw t.refresh_mutex ~protect:true @@ fun () ->
  let require_session () = match t.session with
    | Some session when session.did = did -> session
    | _ -> Xrpc_client.clear_session client;
        raise (Xrpc_error.err Xrpc_error.Session_required) in
  let session = require_session () in
  let session =
    if Xrpc_jwt.is_expired ~now:(t.now ()) ~leeway:refresh_leeway session.access_jwt then
      match refresh_session t with
      | Some session when session.did = did -> session
      | _ -> Xrpc_client.clear_session client;
          raise (Xrpc_error.err Xrpc_error.Token_expired)
    else session in
  Xrpc_client.set_session client session

(* Perform login and return session *)
let perform_login client ~identifier ~password ?auth_factor_token () =
  let login_req = { Xrpc_types.identifier; password; auth_factor_token } in
  Xrpc_client.procedure client ~nsid:"com.atproto.server.createSession"
    ~params:[] ~input:(Some Xrpc_types.login_request_jsont)
    ~input_data:(Some login_req) ~decoder:Xrpc_types.session_jsont

(* Create authenticated client with auto-refresh interceptor *)
let make_authed_client t session =
  let authed_client =
    t.make_client ~on_request:(fun c -> check_and_refresh t ~did:session.Xrpc_types.did c) ()
  in
  Xrpc_client.set_session authed_client session;
  authed_client

let login t ~identifier ~password ?auth_factor_token () =
  Eio.Mutex.use_rw t.refresh_mutex ~protect:true @@ fun () ->
  let client = make_raw_client t in
  let session =
    perform_login client ~identifier ~password ?auth_factor_token ()
  in
  update_session t session;
  make_authed_client t session

let login_client t client ~identifier ~password ?auth_factor_token () =
  if Xrpc_client.get_service client <> t.service then
    invalid_arg "Login client service does not match credential manager";
  let session =
    perform_login client ~identifier ~password ?auth_factor_token ()
  in
  Eio.Mutex.use_rw t.refresh_mutex ~protect:true @@ fun () ->
  update_session t session;
  Xrpc_client.set_session client session;
  make_authed_client t session

let resume t ~session () =
  (match session.Xrpc_types.pds_uri with
   | Some service when Xrpc_client.normalize_service service <> t.service ->
       invalid_arg "Saved session PDS does not match credential manager"
   | _ -> ());
  Eio.Mutex.use_rw t.refresh_mutex ~protect:true @@ fun () ->
  update_session t session;
  make_authed_client t session

let logout t =
  Eio.Mutex.use_rw t.refresh_mutex ~protect:true @@ fun () ->
  Fun.protect ~finally:(fun () -> clear_session t) @@ fun () ->
  Option.iter (fun (session : Xrpc_types.session) ->
    let client = make_raw_client t in
    Xrpc_client.set_session client { session with access_jwt = session.refresh_jwt };
    try Xrpc_client.procedure_unit client
      ~nsid:"com.atproto.server.deleteSession" ~params:[] ~input:None ~input_data:None
    with Eio.Io _ -> ()) t.session
