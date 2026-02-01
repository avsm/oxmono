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
}

let create ~sw ~env ~service () =
  let make_client ?on_request () =
    Xrpc_client.create ~sw ~env ~service ?on_request ()
  in
  {
    service;
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
let check_and_refresh t client =
  match t.session with
  | None -> ()
  | Some session ->
      if Xrpc_jwt.is_expired ~leeway:refresh_leeway session.access_jwt then
        (* Token expired or about to expire, need to refresh *)
        Eio.Mutex.use_rw t.refresh_mutex ~protect:true (fun () ->
            (* Check again in case another fiber already refreshed *)
            match t.session with
            | None -> ()
            | Some current_session ->
                if
                  Xrpc_jwt.is_expired ~leeway:refresh_leeway
                    current_session.access_jwt
                then (
                  match refresh_session t with
                  | Some s -> Xrpc_client.set_session client s
                  | None ->
                      Xrpc_client.clear_session client;
                      raise (Xrpc_error.err Xrpc_error.Token_expired))
                else
                  (* Another fiber already refreshed, just update our client *)
                  Xrpc_client.set_session client current_session)

(* Perform login and return session *)
let perform_login client ~identifier ~password ?auth_factor_token () =
  let login_req = { Xrpc_types.identifier; password; auth_factor_token } in
  Xrpc_client.procedure client ~nsid:"com.atproto.server.createSession"
    ~params:[] ~input:(Some Xrpc_types.login_request_jsont)
    ~input_data:(Some login_req) ~decoder:Xrpc_types.session_jsont

(* Create authenticated client with auto-refresh interceptor *)
let make_authed_client t session =
  let authed_client =
    t.make_client ~on_request:(fun c -> check_and_refresh t c) ()
  in
  Xrpc_client.set_session authed_client session;
  authed_client

let login t ~identifier ~password ?auth_factor_token () =
  let client = make_raw_client t in
  let session =
    perform_login client ~identifier ~password ?auth_factor_token ()
  in
  update_session t session;
  make_authed_client t session

let login_client t client ~identifier ~password ?auth_factor_token () =
  let session =
    perform_login client ~identifier ~password ?auth_factor_token ()
  in
  update_session t session;
  Xrpc_client.set_session client session;
  make_authed_client t session

let resume t ~session () =
  update_session t session;
  make_authed_client t session

let logout t =
  Option.iter
    (fun session ->
      let client = make_raw_client t in
      Xrpc_client.set_session client session;
      try
        ignore
          (Xrpc_client.procedure client ~nsid:"com.atproto.server.deleteSession"
             ~params:[] ~input:None ~input_data:None
             ~decoder:Xrpc_types.empty_jsont)
      with _ ->
        (* Even if server fails, clear local session *)
        ())
    t.session;
  clear_session t
