(*---------------------------------------------------------------------------
   Copyright (c) 2026 Anil Madhavapeddy. All rights reserved.
   SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

module Session = Xrpc_auth_session
module P256 = Mirage_crypto_ec.P256.Dsa

let default_scope = "atproto transition:generic"
let b64 = Jsonwt.base64url_encode
let sha256 s = Digestif.SHA256.(to_raw_string (digest_string s))

let ensure_rng () =
  match Mirage_crypto_rng.default_generator () with
  | _ -> ()
  | exception Mirage_crypto_rng.No_default_generator ->
      Mirage_crypto_rng_unix.use_default ()

let random n =
  ensure_rng ();
  b64 (Mirage_crypto_rng.generate n)

let json_string value =
  match Jsont_bytesrw.encode_string Jsont.json value with
  | Ok s -> s
  | Error e -> failwith e

let obj pairs =
  Jsont.Json.object'
    (List.map (fun (k, v) -> Jsont.Json.mem (k, Jsont.Meta.none) v) pairs)

let str = Jsont.Json.string

let parse body =
  match Jsont_bytesrw.decode_string Jsont.json body with
  | Ok (Jsont.Object (members, _)) ->
      List.filter_map
        (fun ((name, _), value) ->
          match value with
          | Jsont.String (s, _) -> Some (name, `S s)
          | Jsont.Number (n, _) -> Some (name, `N n)
          | Jsont.Array (items, _) ->
              Some
                ( name,
                  `L
                    (List.filter_map
                       (function Jsont.String (s, _) -> Some s | _ -> None)
                       items) )
          | _ -> None)
        members
  | _ -> []

let field fields name =
  match List.assoc_opt name fields with Some (`S s) -> Some s | _ -> None

let required what fields name =
  match field fields name with
  | Some s -> s
  | None -> failwith (Printf.sprintf "%s lacks %s" what name)

(* DPoP keys *)

type key = { priv : P256.priv; jwk : Jsont.json }

let public_jwk pub =
  let point = P256.pub_to_octets pub in
  obj
    [
      ("kty", str "EC");
      ("crv", str "P-256");
      ("x", str (b64 (String.sub point 1 32)));
      ("y", str (b64 (String.sub point 33 32)));
    ]

let generate_key () =
  ensure_rng ();
  let priv, pub = P256.generate () in
  { priv; jwk = public_jwk pub }

let key_to_string key =
  match key.jwk with
  | Jsont.Object (members, meta) ->
      json_string
        (Jsont.Object
           ( members
             @ [
                 Jsont.Json.mem ("d", Jsont.Meta.none)
                   (str (b64 (P256.priv_to_octets key.priv)));
               ],
             meta ))
  | _ -> assert false

let key_of_string text =
  let d =
    match field (parse text) "d" with
    | Some d -> d
    | None -> invalid_arg "DPoP key lacks a private scalar"
  in
  match Jsonwt.base64url_decode d with
  | Error _ -> invalid_arg "Malformed DPoP key"
  | Ok d -> (
      match P256.priv_of_octets d with
      | Ok priv -> { priv; jwk = public_jwk (P256.pub_of_priv priv) }
      | Error _ -> invalid_arg "Malformed DPoP key")

(* RFC 9449 section 4.2. [htu] excludes the query and fragment. *)
let proof key ~now ~meth ~url ?nonce ?token () =
  let htu =
    match String.index_opt url '?' with
    | Some i -> String.sub url 0 i
    | None -> (
        match String.index_opt url '#' with
        | Some i -> String.sub url 0 i
        | None -> url)
  in
  let header =
    obj [ ("alg", str "ES256"); ("typ", str "dpop+jwt"); ("jwk", key.jwk) ]
  in
  let opt name = Option.map (fun v -> (name, str v)) in
  let claims =
    obj
      ([
         ("jti", str (random 16));
         ("htm", str meth);
         ("htu", str htu);
         ("iat", Jsont.Json.int (int_of_float now));
       ]
      @ List.filter_map Fun.id
          [
            opt "nonce" nonce;
            opt "ath" (Option.map (fun t -> b64 (sha256 t)) token);
          ])
  in
  let input = b64 (json_string header) ^ "." ^ b64 (json_string claims) in
  ensure_rng ();
  let r, s = P256.sign ~mask:`Yes ~key:key.priv (sha256 input) in
  input ^ "." ^ b64 (r ^ s)

(* Nonces are per origin. A server may hand out a fresh one on any reply. *)
type dpop = { key : key; nonces : (string, string) Hashtbl.t }

let origin url =
  match Fetch.Middleware.Url.of_string url with
  | Ok u -> Fetch.Middleware.Url.origin u
  | Error e -> invalid_arg e

let remember dpop url headers =
  Option.iter
    (Hashtbl.replace dpop.nonces (origin url))
    (Http.Header.get headers "dpop-nonce")

let nonce dpop url = Hashtbl.find_opt dpop.nonces (origin url)

let www_error headers =
  match Http.Header.get headers "www-authenticate" with
  | None -> None
  | Some value ->
      let marker = "error=\"" in
      let rec find i =
        if i + String.length marker > String.length value then None
        else if String.sub value i (String.length marker) = marker then
          let start = i + String.length marker in
          match String.index_from_opt value start '"' with
          | Some stop -> Some (String.sub value start (stop - start))
          | None -> None
        else find (i + 1)
      in
      find 0

(* Authorization server requests *)

let read_body response =
  Fetch.decode ~limit:(1024 * 1024) Fetch.Media.octets response

(* Posts a form with a DPoP proof, retrying once with the nonce the server
   demands (RFC 9449 section 8). *)
let post_form ~clock ~http dpop url pairs =
  let send () =
    let headers, body = Fetch.Form.urlencoded pairs in
    let proof =
      proof dpop.key ~now:(Eio.Time.now clock) ~meth:"POST" ~url
        ?nonce:(nonce dpop url) ()
    in
    let headers =
      Fetch.Header.append headers [ Fetch.Header.raw "DPoP" proof ]
    in
    Fetch.with_response ~headers ~body ~redirects:0 ~sensitive:[ "dpop" ] http
      `POST url (fun response ->
        remember dpop url (Fetch.headers response);
        (Fetch.status response, read_body response))
  in
  let status, body = send () in
  let status, body =
    if status = 400 && field (parse body) "error" = Some "use_dpop_nonce" then
      send ()
    else (status, body)
  in
  let fields = parse body in
  if status >= 200 && status < 300 then fields
  else
    failwith
      (Printf.sprintf "%s refused the request (HTTP %d): %s%s" url status
         (Option.value ~default:"unknown error" (field fields "error"))
         (match field fields "error_description" with
         | Some d -> ": " ^ d
         | None -> ""))

let get_json ~http url =
  Fetch.with_response ~redirects:0 http `GET url (fun response ->
      let status = Fetch.status response in
      if status <> 200 then
        failwith (Printf.sprintf "%s answered HTTP %d" url status);
      read_body response)

type server = {
  issuer : string;
  authorization_endpoint : string;
  token_endpoint : string;
  par_endpoint : string;
  revocation_endpoint : string option;
}

let strip_slash s =
  if String.ends_with ~suffix:"/" s then String.sub s 0 (String.length s - 1)
  else s

let server_of_pds ~http pds =
  let pds = strip_slash pds in
  let resource =
    parse (get_json ~http (pds ^ "/.well-known/oauth-protected-resource"))
  in
  let issuer =
    match List.assoc_opt "authorization_servers" resource with
    | Some (`L (issuer :: _)) -> strip_slash issuer
    | _ -> failwith (pds ^ " names no authorization server")
  in
  let what = issuer ^ " metadata" in
  let meta =
    parse (get_json ~http (issuer ^ "/.well-known/oauth-authorization-server"))
  in
  if Option.map strip_slash (field meta "issuer") <> Some issuer then
    failwith (what ^ " names a different issuer");
  {
    issuer;
    authorization_endpoint = required what meta "authorization_endpoint";
    token_endpoint = required what meta "token_endpoint";
    par_endpoint = required what meta "pushed_authorization_request_endpoint";
    revocation_endpoint = field meta "revocation_endpoint";
  }

let pct_encode s =
  let b = Buffer.create (String.length s) in
  String.iter
    (fun c ->
      match c with
      | 'A' .. 'Z' | 'a' .. 'z' | '0' .. '9' | '-' | '.' | '_' | '~' ->
          Buffer.add_char b c
      | c -> Buffer.add_string b (Printf.sprintf "%%%02X" (Char.code c)))
    s;
  Buffer.contents b

let pct_decode s =
  let n = String.length s in
  let b = Buffer.create n in
  let rec go i =
    if i < n then
      match s.[i] with
      | '%' when i + 2 < n -> (
          match int_of_string_opt ("0x" ^ String.sub s (i + 1) 2) with
          | Some c ->
              Buffer.add_char b (Char.chr c);
              go (i + 3)
          | None ->
              Buffer.add_char b '%';
              go (i + 1))
      | '+' ->
          Buffer.add_char b ' ';
          go (i + 1)
      | c ->
          Buffer.add_char b c;
          go (i + 1)
  in
  go 0;
  Buffer.contents b

let encode_query pairs =
  String.concat "&" (List.map (fun (k, v) -> k ^ "=" ^ pct_encode v) pairs)

let loopback_client_id ~redirect_uri ~scope =
  "http://localhost?"
  ^ encode_query [ ("redirect_uri", redirect_uri); ("scope", scope) ]

(* Loopback redirect listener *)

let query_params target =
  match String.index_opt target '?' with
  | None -> (target, [])
  | Some i ->
      let path = String.sub target 0 i in
      let query = String.sub target (i + 1) (String.length target - i - 1) in
      ( path,
        List.filter_map
          (fun pair ->
            match String.index_opt pair '=' with
            | None -> None
            | Some j ->
                Some
                  ( pct_decode (String.sub pair 0 j),
                    pct_decode
                      (String.sub pair (j + 1) (String.length pair - j - 1)) ))
          (String.split_on_char '&' query) )

let respond flow status text =
  let page =
    "<!doctype html><meta charset=utf-8><title>Login</title><p>" ^ text
    ^ "</p>\n"
  in
  Eio.Flow.copy_string
    (Printf.sprintf
       "HTTP/1.1 %s\r\nContent-Type: text/html; charset=utf-8\r\n\
        Content-Length: %d\r\nConnection: close\r\n\r\n%s"
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

let now_rfc3339 clock = Ptime.to_rfc3339 ~tz_offset_s:0 (Option.get
  (Ptime.of_float_s (Eio.Time.now clock)))

let expiry clock fields =
  let seconds =
    match List.assoc_opt "expires_in" fields with
    | Some (`N n) when n > 0. -> n
    | _ -> 300.
  in
  Ptime.to_rfc3339 ~tz_offset_s:0
    (Option.get (Ptime.of_float_s (Eio.Time.now clock +. seconds)))

let check_token_type fields =
  match field fields "token_type" with
  | Some t when String.lowercase_ascii t = "dpop" -> ()
  | _ -> failwith "Authorization server did not issue a DPoP-bound token"

let handle_of ~http ~pds did =
  let url =
    strip_slash pds ^ "/xrpc/com.atproto.repo.describeRepo?"
    ^ encode_query [ ("repo", did) ]
  in
  match field (parse (get_json ~http url)) "handle" with
  | Some handle -> handle
  | None -> did
  | exception (Failure _ | Eio.Io _) -> did

let pds_of ?plc ~http identifier =
  let did =
    if String.starts_with ~prefix:"did:" identifier then identifier
    else
      match Xrpc.Identity.did_of_handle http identifier with
      | Ok did -> did
      | Error e -> failwith e
  in
  match Xrpc.Identity.pds_of_did ?plc http did with
  | Ok pds -> (did, pds)
  | Error e -> failwith e

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
  let dpop = { key = generate_key (); nonces = Hashtbl.create 2 } in
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
        let redirect_uri =
          Printf.sprintf "http://127.0.0.1:%d/callback" port
        in
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
    match expected with
    | Some expected ->
        if did <> expected then failwith "Signed in as a different account";
        pds
    | None ->
        (* The account's own PDS must trust this issuer, or any server could
           claim any DID. *)
        let _, pds = pds_of ?plc ~http did in
        if (server_of_pds ~http pds).issuer <> server.issuer then
          failwith "Account is not served by this authorization server";
        pds
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
          dpop_key = key_to_string dpop.key;
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
    clock = (clock :> float Eio.Time.clock_ty Eio.Resource.t);
    base = Fetch.restrict http;
    dpop = { key = key_of_string oauth.dpop_key; nonces = Hashtbl.create 2 };
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
        failwith
          ("OAuth session could not be renewed. Log in again. " ^ message)
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
        proof t.dpop.key ~now:(Eio.Time.now t.clock)
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
