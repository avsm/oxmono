(*---------------------------------------------------------------------------
   Copyright (c) 2026 Anil Madhavapeddy. All rights reserved.
   SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

module Session = Xrpc_auth_session
module P = Xrpc_auth_protocol

let default_scope = "atproto transition:generic"
let b64 = P.b64
let sha256 = P.sha256
let random = P.random
let parse = P.parse
let field = P.field
let required = P.required
let strip_slash = P.strip_slash
let encode_query = P.encode_query

type dpop = P.dpop

let proof = P.proof
let remember = P.remember
let nonce = P.nonce
let www_error = P.www_error

let origin url =
  match Fetch.Middleware.Url.of_string url with
  | Ok u -> Fetch.Middleware.Url.origin u
  | Error e -> invalid_arg e

let post_form ~clock ~http dpop url pairs =
  P.post_form ~clock ~http dpop url pairs

let get_json = P.get_json

type server = P.server = {
  issuer : string;
  authorization_endpoint : string;
  token_endpoint : string;
  par_endpoint : string;
  revocation_endpoint : string option;
}

let server_of_pds = P.server_of_pds

let loopback_client_id ~redirect_uri ~scope =
  "http://localhost?"
  ^ encode_query [ ("redirect_uri", redirect_uri); ("scope", scope) ]

(* Loopback redirect listener *)

let query_params target =
  match String.index_opt target '?' with
  | None -> (target, [])
  | Some i ->
      ( String.sub target 0 i,
        P.decode_query
          (String.sub target (i + 1) (String.length target - i - 1)) )

let respond flow status text =
  let page =
    "<!doctype html><meta charset=utf-8><title>Login</title><p>" ^ text
    ^ "</p>\n"
  in
  Eio.Flow.copy_string
    (Printf.sprintf
       "HTTP/1.1 %s\r\n\
        Content-Type: text/html; charset=utf-8\r\n\
        Content-Length: %d\r\n\
        Connection: close\r\n\
        \r\n\
        %s"
       status (String.length page) page)
    flow

(* Answers requests until one reaches /callback, whose parameters it returns.
   A browser may ask for other paths, such as a favicon, first. *)
let rec await_callback ~sw socket =
  let flow, _ = Eio.Net.accept ~sw socket in
  let result =
    Fun.protect ~finally:(fun () -> Eio.Flow.close flow) @@ fun () ->
    let reader = Eio.Buf_read.of_flow ~max_size:(64 * 1024) flow in
    let line = Eio.Buf_read.line reader in
    let rec skip () = if Eio.Buf_read.line reader <> "" then skip () in
    skip ();
    match String.split_on_char ' ' line with
    | [ "GET"; target; _ ] -> (
        match query_params target with
        | "/callback", params ->
            let text =
              if List.mem_assoc "error" params then
                "Login was refused. You can close this window."
              else "Login complete. You can close this window."
            in
            respond flow "200 OK" text;
            Some params
        | _ ->
            respond flow "404 Not Found" "Not found.";
            None)
    | _ ->
        respond flow "400 Bad Request" "Bad request.";
        None
  in
  match result with Some params -> params | None -> await_callback ~sw socket

let now_rfc3339 clock =
  Ptime.to_rfc3339 ~tz_offset_s:0
    (Option.get (Ptime.of_float_s (Eio.Time.now clock)))

let expiry clock fields =
  let seconds =
    match List.assoc_opt "expires_in" fields with
    | Some (`N n) when n > 0. -> n
    | _ -> 300.
  in
  Ptime.to_rfc3339 ~tz_offset_s:0
    (Option.get (Ptime.of_float_s (Eio.Time.now clock +. seconds)))

let check_token_type = P.check_token_type

let handle_of ~http ~pds did =
  let url =
    strip_slash pds ^ "/xrpc/com.atproto.repo.describeRepo?"
    ^ encode_query [ ("repo", did) ]
  in
  match field (parse (get_json ~http url)) "handle" with
  | Some handle -> handle
  | None -> did
  | exception (Failure _ | Eio.Io _) -> did

let pds_of = P.pds_of

type redirect =
  | Loopback
  | Paste of {
      client_id : string;
      redirect_uri : string;
      read : unit -> string;
    }

let hosted_client_id = "https://www.cl.cam.ac.uk/~avsm2/oauth-client.json"
let hosted_redirect_uri = "https://www.cl.cam.ac.uk/~avsm2/oauth-login.html"

(* Accepts the page's "code=...&state=..." line or the whole redirected URL. *)
let pasted text =
  let text = String.trim text in
  let query =
    match String.index_opt text '?' with
    | Some i -> String.sub text (i + 1) (String.length text - i - 1)
    | None -> text
  in
  let query =
    match String.index_opt query '#' with
    | Some i -> String.sub query 0 i
    | None -> query
  in
  snd (query_params ("?" ^ query))

let authorize ~net ~clock ~http ?plc ?pds ?identifier ?(timeout = 300.)
    ?(redirect = Loopback) ~scope ~open_url () =
  let http = Fetch.restrict http in
  let expected, pds =
    match (identifier, pds) with
    | Some id, _ ->
        let did, pds = pds_of ?plc ~http id in
        (Some did, pds)
    | None, Some pds -> (None, pds)
    | None, None -> invalid_arg "Give an account or a PDS to log in to"
  in
  let server = server_of_pds ~http pds in
  let dpop = P.dpop (P.Key.generate ()) in
  Eio.Switch.run @@ fun sw ->
  let client_id, redirect_uri, await =
    match redirect with
    | Paste p -> (p.client_id, p.redirect_uri, fun () -> pasted (p.read ()))
    | Loopback ->
        let socket =
          Eio.Net.listen ~sw ~backlog:4 net
            (`Tcp (Eio.Net.Ipaddr.V4.loopback, 0))
        in
        let port =
          match Eio.Net.listening_addr socket with
          | `Tcp (_, port) -> port
          | _ -> failwith "Loopback listener has no port"
        in
        let redirect_uri = Printf.sprintf "http://127.0.0.1:%d/callback" port in
        let await () =
          match
            Eio.Time.with_timeout clock timeout (fun () ->
                Ok (await_callback ~sw socket))
          with
          | Ok params -> params
          | Error `Timeout -> failwith "Timed out waiting for the browser"
        in
        (loopback_client_id ~redirect_uri ~scope, redirect_uri, await)
  in
  let verifier = random 32 in
  let state = random 16 in
  let par =
    post_form ~clock ~http dpop server.par_endpoint
      ([
         ("client_id", client_id);
         ("response_type", "code");
         ("redirect_uri", redirect_uri);
         ("scope", scope);
         ("state", state);
         ("code_challenge", b64 (sha256 verifier));
         ("code_challenge_method", "S256");
       ]
      @ Option.to_list (Option.map (fun id -> ("login_hint", id)) identifier))
  in
  let request_uri =
    required "Pushed authorization response" par "request_uri"
  in
  open_url
    (server.authorization_endpoint ^ "?"
    ^ encode_query [ ("client_id", client_id); ("request_uri", request_uri) ]);
  let params = await () in
  if List.assoc_opt "state" params <> Some state then
    failwith "Authorization response has the wrong state";
  (* A pasted line may omit the issuer. PKCE and the DPoP key still bind the
     code to this request. *)
  (match (List.assoc_opt "iss" params, redirect) with
  | Some iss, _ when strip_slash iss = server.issuer -> ()
  | None, Paste _ -> ()
  | _ -> failwith "Authorization response names the wrong issuer");
  (match List.assoc_opt "error" params with
  | Some error ->
      failwith
        (Printf.sprintf "Authorization refused: %s%s" error
           (match List.assoc_opt "error_description" params with
           | Some d -> ": " ^ d
           | None -> ""))
  | None -> ());
  let code =
    match List.assoc_opt "code" params with
    | Some code -> code
    | None -> failwith "Authorization response lacks a code"
  in
  let token =
    post_form ~clock ~http dpop server.token_endpoint
      [
        ("grant_type", "authorization_code");
        ("code", code);
        ("redirect_uri", redirect_uri);
        ("code_verifier", verifier);
        ("client_id", client_id);
      ]
  in
  check_token_type token;
  let what = "Token response" in
  let did = required what token "sub" in
  let pds =
    P.account_pds ?plc ~http ~issuer:server.issuer ~expected ~pds:(Some pds) did
  in
  {
    Session.access_jwt = required what token "access_token";
    refresh_jwt = required what token "refresh_token";
    did;
    handle = handle_of ~http ~pds did;
    pds = Xrpc.Client.normalize_service pds;
    created_at = now_rfc3339 clock;
    oauth =
      Some
        {
          issuer = server.issuer;
          token_endpoint = server.token_endpoint;
          revocation_endpoint = server.revocation_endpoint;
          client_id;
          dpop_key = P.Key.to_string (P.dpop_key dpop);
          scope = Option.value ~default:scope (field token "scope");
          expires_at = expiry clock token;
        };
  }

(* Sessions *)

type t = {
  clock : float Eio.Time.clock_ty Eio.Resource.t;
  base : Fetch.plain;
  dpop : dpop;
  mutable session : Session.t;
  on_update : Session.t -> unit;
  lock : Eio.Mutex.t;
}

let oauth_of session =
  match session.Session.oauth with
  | Some o -> o
  | None -> invalid_arg "Not an OAuth session"

let v ~clock ~http ?(on_update = ignore) session =
  let oauth = oauth_of session in
  {
    clock :> float Eio.Time.clock_ty Eio.Resource.t;
    base = Fetch.restrict http;
    dpop = P.dpop (P.Key.of_string oauth.dpop_key);
    session;
    on_update;
    lock = Eio.Mutex.create ();
  }

let session t = t.session

let refresh t ~stale =
  Eio.Mutex.use_rw ~protect:true t.lock @@ fun () ->
  (* Another fibre may have refreshed while this one waited. *)
  if t.session.access_jwt = stale then begin
    let oauth = oauth_of t.session in
    let token =
      try
        post_form ~clock:t.clock ~http:t.base t.dpop oauth.token_endpoint
          [
            ("grant_type", "refresh_token");
            ("refresh_token", t.session.refresh_jwt);
            ("client_id", oauth.client_id);
          ]
      with Failure message ->
        failwith ("OAuth session could not be renewed. Log in again. " ^ message)
    in
    check_token_type token;
    if field token "sub" <> Some t.session.did then
      failwith "Refreshed token names a different account";
    let what = "Refresh response" in
    t.session <-
      {
        t.session with
        access_jwt = required what token "access_token";
        refresh_jwt = required what token "refresh_token";
        oauth =
          Some
            {
              oauth with
              scope = Option.value ~default:oauth.scope (field token "scope");
              expires_at = expiry t.clock token;
            };
      };
    t.on_update t.session
  end

let fresh_token t =
  let token = t.session.access_jwt in
  let near_expiry =
    match Ptime.of_rfc3339 (oauth_of t.session).expires_at with
    | Ok (expires, _, _) ->
        Ptime.to_float_s expires -. Eio.Time.now t.clock < 60.
    | Error _ -> true
  in
  if near_expiry then refresh t ~stale:token;
  t.session.access_jwt

let middleware t : Fetch.Middleware.middleware =
 fun next ~sw (request : Fetch.Middleware.request) ->
  let url = Fetch.Middleware.Url.to_string request.url in
  if Fetch.Middleware.Url.origin request.url <> origin t.session.pds then
    next ~sw request
  else
    let send () =
      let token = fresh_token t in
      let proof =
        proof (P.dpop_key t.dpop) ~now:(Eio.Time.now t.clock)
          ~meth:(Http.Method.to_string request.meth)
          ~url ?nonce:(nonce t.dpop url) ~token ()
      in
      let headers =
        Http.Header.replace request.headers "Authorization" ("DPoP " ^ token)
      in
      let headers = Http.Header.replace headers "DPoP" proof in
      let response =
        next ~sw
          { request with headers; sensitive = "dpop" :: request.sensitive }
      in
      remember t.dpop url (Fetch.Middleware.headers response);
      (token, response)
    in
    let token, response = send () in
    let retry =
      Fetch.Middleware.status response = 401
      && Fetch.Middleware.body_replayable request.body
      &&
      match www_error (Fetch.Middleware.headers response) with
      | Some "use_dpop_nonce" -> true
      | Some "invalid_token" ->
          refresh t ~stale:token;
          true
      | _ -> false
    in
    if retry then begin
      Fetch.Middleware.close response;
      snd (send ())
    end
    else response

let http t = Fetch.Middleware.middleware (middleware t) t.base

let revoke t =
  match (oauth_of t.session).revocation_endpoint with
  | None -> ()
  | Some url -> (
      try
        ignore
          (post_form ~clock:t.clock ~http:t.base t.dpop url
             [
               ("token", t.session.refresh_jwt);
               ("client_id", (oauth_of t.session).client_id);
             ])
      with Failure _ | Eio.Io _ -> ())
