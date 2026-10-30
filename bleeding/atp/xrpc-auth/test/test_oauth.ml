(* An authorization server and PDS in one mock. Every DPoP proof is verified
   against the key in its header, and that key must stay the same across the
   grant. *)

let check name value = if not value then failwith name
let as_ = "https://as.test"
let pds = "https://pds.test"
let plc = "https://plc.test"
let did = "did:plc:alice"

let b64d s =
  match Jsonwt.base64url_decode s with Ok s -> s | Error _ -> failwith "b64"

let json s =
  match Jsont_bytesrw.decode_string Jsont.json s with
  | Ok v -> v
  | Error e -> failwith e

let mem name = function
  | Jsont.Object (members, _) ->
      List.find_map
        (fun ((n, _), v) -> if n = name then Some v else None)
        members
  | _ -> None

let text name v =
  match mem name v with Some (Jsont.String (s, _)) -> Some s | _ -> None

let encode v =
  match Jsont_bytesrw.encode_string Jsont.json v with
  | Ok s -> s
  | Error e -> failwith e

let form body =
  let dec s =
    let b = Buffer.create (String.length s) in
    let i = ref 0 in
    while !i < String.length s do
      (match s.[!i] with
      | '%' ->
          Buffer.add_char b
            (Char.chr (int_of_string ("0x" ^ String.sub s (!i + 1) 2)));
          i := !i + 2
      | '+' -> Buffer.add_char b ' '
      | c -> Buffer.add_char b c);
      incr i
    done;
    Buffer.contents b
  in
  String.split_on_char '&' body
  |> List.filter_map (fun pair ->
         match String.index_opt pair '=' with
         | None -> None
         | Some i ->
             Some
               ( dec (String.sub pair 0 i),
                 dec (String.sub pair (i + 1) (String.length pair - i - 1)) ))

let sha256 s = Digestif.SHA256.(to_raw_string (digest_string s))

type state = {
  mutable jwk : string option;
  mutable as_nonce_sent : bool;
  mutable pds_nonce_sent : bool;
  mutable par : (string * string) list;
  mutable access : string;
  mutable refresh : string;
  mutable refreshes : int;
  mutable pds_calls : int;
}

let st =
  {
    jwk = None;
    as_nonce_sent = false;
    pds_nonce_sent = false;
    par = [];
    access = "";
    refresh = "";
    refreshes = 0;
    pds_calls = 0;
  }

(* Verifies a proof and returns its claims. *)
let verify_proof (req : Fetch.Middleware.request) =
  let proof =
    match Http.Header.get req.headers "dpop" with
    | Some p -> p
    | None -> failwith "request lacks DPoP"
  in
  check "DPoP is sensitive" (List.mem "dpop" req.sensitive);
  let header = json (b64d (List.hd (String.split_on_char '.' proof))) in
  check "typ" (text "typ" header = Some "dpop+jwt");
  let jwk = encode (Option.get (mem "jwk" header)) in
  check "no private key in proof" (mem "d" (json jwk) = None);
  (match st.jwk with
  | None -> st.jwk <- Some jwk
  | Some k -> check "same DPoP key" (k = jwk));
  let key =
    match Jsonwt.Jwk.of_json jwk with
    | Ok k -> Jsonwt.Jwk.with_alg Jsonwt.Algorithm.ES256 k
    | Error _ -> failwith "jwk"
  in
  let token =
    match Jsonwt.parse proof with Ok t -> t | Error _ -> failwith "parse"
  in
  check "signature"
    (Jsonwt.verify ~key ~allowed_algs:[ Jsonwt.Algorithm.ES256 ] token = Ok ());
  let claims = Jsonwt.claims token in
  let url = Fetch.Middleware.Url.to_string req.url in
  let htu =
    match String.index_opt url '?' with
    | Some i -> String.sub url 0 i
    | None -> url
  in
  check "htm"
    (Jsonwt.Claims.get_string "htm" claims
    = Some (Http.Method.to_string req.meth));
  check "htu" (Jsonwt.Claims.get_string "htu" claims = Some htu);
  check "jti" (Jsonwt.Claims.jti claims <> None);
  claims

let reply ?(status = 200) ?(headers = []) body req =
  Fetch_mock.respond ~status
    ~headers:
      (Http.Header.of_list (("content-type", "application/json") :: headers))
    body req

let body (req : Fetch.Middleware.request) =
  match req.body with Fetch.Middleware.String s -> s | _ -> ""

let as_nonce req k =
  let claims = verify_proof req in
  if Jsonwt.Claims.get_string "nonce" claims <> Some "as-n" then begin
    st.as_nonce_sent <- true;
    reply ~status:400 ~headers:[ ("dpop-nonce", "as-n") ]
      {|{"error":"use_dpop_nonce"}|} req
  end
  else k (form (body req))

let token_reply req =
  st.access <- "at" ^ string_of_int (st.refreshes + 1);
  st.refresh <- "rt" ^ string_of_int (st.refreshes + 1);
  reply
    (Printf.sprintf
       ({|{"access_token":"%s","refresh_token":"%s","token_type":"DPoP",|}
       ^^ {|"sub":"%s","scope":"atproto","expires_in":3600}|})
       st.access st.refresh did)
    req

let server (req : Fetch.Middleware.request) =
  let url = Fetch.Middleware.Url.to_string req.url in
  let path =
    match String.index_opt url '?' with
    | Some i -> String.sub url 0 i
    | None -> url
  in
  match path with
  | "https://plc.test/did:plc:alice" ->
      reply
        ({|{"id":"did:plc:alice","alsoKnownAs":["at://alice.test"],|}
        ^ {|"service":[{"id":"#atproto_pds",|}
        ^ {|"type":"AtprotoPersonalDataServer",|}
        ^ {|"serviceEndpoint":"https://pds.test"}]}|})
        req
  | "https://pds.test/.well-known/oauth-protected-resource" ->
      reply
        ({|{"resource":"https://pds.test",|}
        ^ {|"authorization_servers":["https://as.test"]}|})
        req
  | "https://as.test/.well-known/oauth-authorization-server" ->
      reply
        ({|{"issuer":"https://as.test",|}
        ^ {|"authorization_endpoint":"https://as.test/authorize",|}
        ^ {|"token_endpoint":"https://as.test/token",|}
        ^ {|"pushed_authorization_request_endpoint":"https://as.test/par",|}
        ^ {|"revocation_endpoint":"https://as.test/revoke"}|})
        req
  | "https://pds.test/xrpc/com.atproto.repo.describeRepo" ->
      reply {|{"handle":"alice.test"}|} req
  | "https://as.test/par" ->
      as_nonce req (fun params ->
          check "S256" (List.assoc "code_challenge_method" params = "S256");
          check "login hint" (List.assoc "login_hint" params = did);
          check "loopback client"
            (String.starts_with ~prefix:"http://localhost?redirect_uri="
               (List.assoc "client_id" params));
          st.par <- params;
          reply ~status:201 {|{"request_uri":"urn:req:1","expires_in":60}|} req)
  | "https://as.test/token" ->
      as_nonce req (fun params ->
          match List.assoc "grant_type" params with
          | "authorization_code" ->
              check "code" (List.assoc "code" params = "code-1");
              check "PKCE"
                (Jsonwt.base64url_encode
                   (sha256 (List.assoc "code_verifier" params))
                = List.assoc "code_challenge" st.par);
              check "redirect"
                (List.assoc "redirect_uri" params
                = List.assoc "redirect_uri" st.par);
              token_reply req
          | "refresh_token" ->
              check "refresh token"
                (List.assoc "refresh_token" params = st.refresh);
              st.refreshes <- st.refreshes + 1;
              token_reply req
          | _ -> failwith "grant")
  | "https://as.test/revoke" ->
      as_nonce req (fun params ->
          check "revoke" (List.assoc "token" params = st.refresh);
          st.refresh <- "";
          reply "{}" req)
  | "https://pds.test/xrpc/com.atproto.server.getSession" ->
      st.pds_calls <- st.pds_calls + 1;
      let claims = verify_proof req in
      check "authorization"
        (Http.Header.get req.headers "authorization"
        = Some ("DPoP " ^ st.access));
      check "ath"
        (Jsonwt.Claims.get_string "ath" claims
        = Some (Jsonwt.base64url_encode (sha256 st.access)));
      if Jsonwt.Claims.get_string "nonce" claims <> Some "pds-n" then begin
        st.pds_nonce_sent <- true;
        reply ~status:401
          ~headers:
            [
              ("dpop-nonce", "pds-n");
              ("www-authenticate", {|DPoP error="use_dpop_nonce"|});
            ]
          {|{"error":"use_dpop_nonce"}|} req
      end
      else reply (Printf.sprintf {|{"did":"%s","handle":"alice.test"}|} did) req
  | _ -> failwith ("unexpected request " ^ url)

let params_of url =
  match String.index_opt url '?' with
  | None -> []
  | Some i -> form (String.sub url (i + 1) (String.length url - i - 1))

(* The browser: follow the redirect to the loopback listener. *)
let browse ~sw ~net ?(iss = as_) ?(state_override = None) url =
  check "authorize URL"
    (String.starts_with ~prefix:"https://as.test/authorize?" url);
  check "request_uri" (List.assoc "request_uri" (params_of url) = "urn:req:1");
  let redirect = List.assoc "redirect_uri" st.par in
  let state =
    Option.value ~default:(List.assoc "state" st.par) state_override
  in
  let port =
    let rest = String.sub redirect 17 (String.length redirect - 17) in
    int_of_string (String.sub rest 0 (String.index rest '/'))
  in
  Eio.Fiber.fork ~sw (fun () ->
      let get target =
        Eio.Switch.run @@ fun sw ->
        let flow =
          Eio.Net.connect ~sw net (`Tcp (Eio.Net.Ipaddr.V4.loopback, port))
        in
        Eio.Flow.copy_string
          ("GET " ^ target ^ " HTTP/1.1\r\nHost: 127.0.0.1\r\n\r\n")
          flow;
        ignore (Eio.Buf_read.(parse_exn take_all) ~max_size:65536 flow)
      in
      get "/favicon.ico";
      get
        (Printf.sprintf "/callback?code=code-1&state=%s&iss=%s" state
           (String.concat "%3A" (String.split_on_char ':' iss))))

let raises expected f =
  st.jwk <- None;
  match f () with
  | _ -> false
  | exception Failure message -> message = expected

let () =
  Eio_main.run @@ fun env ->
  let http = Fetch_mock.client server in
  let net = env#net and clock = env#clock in
  let session =
    Eio.Switch.run @@ fun sw ->
    Xrpc_auth.Oauth.authorize ~net ~clock ~http ~plc ~identifier:did
      ~scope:"atproto" ~open_url:(browse ~sw ~net) ()
  in
  check "AS nonce used" st.as_nonce_sent;
  check "session did" (session.did = did);
  check "session handle" (session.handle = "alice.test");
  check "session pds" (session.pds = pds);
  check "access token" (session.access_jwt = "at1");
  let oauth = Option.get session.oauth in
  check "issuer" (oauth.issuer = as_);
  check "key is private" (mem "d" (json oauth.dpop_key) <> None);
  check "not expired" (not (Xrpc_auth.Session.is_expired session));
  (* A state mismatch is refused. *)
  check "wrong state refused"
    (raises "Authorization response has the wrong state" (fun () ->
         Eio.Switch.run @@ fun sw ->
         Xrpc_auth.Oauth.authorize ~net ~clock ~http ~plc ~identifier:did
           ~scope:"atproto"
           ~open_url:(browse ~sw ~net ~state_override:(Some "forged"))
           ()));
  check "wrong issuer refused"
    (raises "Authorization response names the wrong issuer" (fun () ->
         Eio.Switch.run @@ fun sw ->
         Xrpc_auth.Oauth.authorize ~net ~clock ~http ~plc ~identifier:did
           ~scope:"atproto"
           ~open_url:(browse ~sw ~net ~iss:"https://evil.test")
           ()));
  (* Rebuild the session the grant above produced; the failed grants
     replaced the mock's token state. *)
  st.jwk <- None;
  let session =
    Eio.Switch.run @@ fun sw ->
    Xrpc_auth.Oauth.authorize ~net ~clock ~http ~plc ~identifier:did
      ~scope:"atproto" ~open_url:(browse ~sw ~net) ()
  in
  (* Resource requests carry the token and retry for the PDS nonce. *)
  let saved = ref None in
  let live =
    Xrpc_auth.Oauth.v ~clock ~http ~on_update:(fun s -> saved := Some s) session
  in
  let client = Xrpc.Client.of_fetch ~service:pds (Xrpc_auth.Oauth.http live) in
  let get () =
    Xrpc.Client.query client ~nsid:"com.atproto.server.getSession" ~params:[]
      ~decoder:Jsont.json
  in
  ignore (get ());
  check "PDS nonce used" st.pds_nonce_sent;
  check "nonce retried once" (st.pds_calls = 2);
  ignore (get ());
  check "nonce remembered" (st.pds_calls = 3);
  (* An expired token is refreshed first, and the refresh is reported. *)
  let expired =
    {
      session with
      oauth =
        Some
          {
            (Option.get session.oauth) with
            expires_at = "2000-01-01T00:00:00Z";
          };
    }
  in
  let live =
    Xrpc_auth.Oauth.v ~clock ~http ~on_update:(fun s -> saved := Some s) expired
  in
  let client = Xrpc.Client.of_fetch ~service:pds (Xrpc_auth.Oauth.http live) in
  ignore
    (Xrpc.Client.query client ~nsid:"com.atproto.server.getSession" ~params:[]
       ~decoder:Jsont.json);
  check "refreshed" (st.refreshes = 1);
  (match !saved with
  | Some s -> check "saved rotated token" (s.refresh_jwt = "rt2")
  | None -> failwith "refresh not saved");
  check "session updated"
    ((Xrpc_auth.Oauth.session live).access_jwt = "at2");
  (* Requests to other origins carry nothing. *)
  let other =
    Fetch_mock.client (fun req ->
        check "no token elsewhere"
          (Http.Header.get req.headers "authorization" = None
          && Http.Header.get req.headers "dpop" = None);
        Fetch_mock.respond "{}" req)
  in
  let live = Xrpc_auth.Oauth.v ~clock ~http:other session in
  ignore (Fetch.read (Xrpc_auth.Oauth.http live) "https://elsewhere.test/");
  Xrpc_auth.Oauth.revoke
    (Xrpc_auth.Oauth.v ~clock ~http { session with refresh_jwt = st.refresh });
  check "revoked" (st.refresh = "");
  print_endline "oauth: ok"
