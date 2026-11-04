(* A web site's login against one mock that plays the PLC directory, the PDS,
   the handle's host and the authorization server. The server verifies every
   DPoP proof and client assertion it is sent, and refuses an assertion whose
   jti it has seen before. *)

module W = Xrpc_auth_web

let check name value = if not value then failwith name
let did = "did:plc:alice"
let issuer = "https://as.test"

let b64d s =
  match Jsonwt.base64url_decode s with Ok s -> s | Error _ -> failwith "b64"

let json s =
  match Jsont_bytesrw.decode_string Jsont.json s with
  | Ok v -> v
  | Error e -> failwith e

let encode v =
  match Jsont_bytesrw.encode_string Jsont.json v with
  | Ok s -> s
  | Error e -> failwith e

let mem name = function
  | Jsont.Object (members, _) ->
      List.find_map
        (fun ((n, _), v) -> if n = name then Some v else None)
        members
  | _ -> None

let text name v =
  match mem name v with Some (Jsont.String (s, _)) -> Some s | _ -> None

let form body = Xrpc_auth_protocol.decode_query body
let sha256 s = Digestif.SHA256.(to_raw_string (digest_string s))

(* What the mock answers, set by each test. *)
type script = {
  mutable token_type : string;
  mutable sub : string;
  mutable scope : string;
  mutable handle_names : string;
  mutable nonce_on_token : bool;
}

let script =
  {
    token_type = "DPoP";
    sub = did;
    scope = "atproto";
    handle_names = did;
    nonce_on_token = true;
  }

let reset () =
  script.token_type <- "DPoP";
  script.sub <- did;
  script.scope <- "atproto";
  script.handle_names <- did;
  script.nonce_on_token <- true

let par = ref []
let jtis = Hashtbl.create 16
let revoked = ref []
let nonces_demanded = ref 0
let client_public = ref (Jsont.Json.null ())

let verify_proof (req : Fetch.Middleware.request) =
  let proof =
    match Http.Header.get req.headers "dpop" with
    | Some p -> p
    | None -> failwith "request lacks DPoP"
  in
  let header = json (b64d (List.hd (String.split_on_char '.' proof))) in
  check "dpop typ" (text "typ" header = Some "dpop+jwt");
  let jwk = Option.get (mem "jwk" header) in
  check "no private key in a proof" (mem "d" jwk = None);
  let key =
    match Jsonwt.Jwk.of_json (encode jwk) with
    | Ok k -> Jsonwt.Jwk.with_alg Jsonwt.Algorithm.ES256 k
    | Error _ -> failwith "jwk"
  in
  let token =
    match Jsonwt.parse proof with Ok t -> t | Error _ -> failwith "parse"
  in
  check "dpop signature"
    (Jsonwt.verify ~key ~allowed_algs:[ Jsonwt.Algorithm.ES256 ] token = Ok ());
  Jsonwt.claims token

(* RFC 7523: signed by the key the JWKS publishes, about this client, for
   this server, short-lived and never replayed. *)
let verify_assertion params =
  check "assertion type"
    (List.assoc "client_assertion_type" params
    = "urn:ietf:params:oauth:client-assertion-type:jwt-bearer");
  check "no client secret" (not (List.mem_assoc "client_secret" params));
  let client_id = List.assoc "client_id" params in
  let assertion = List.assoc "client_assertion" params in
  let header = json (b64d (List.hd (String.split_on_char '.' assertion))) in
  check "assertion kid" (text "kid" header = Some "k1");
  check "assertion alg" (text "alg" header = Some "ES256");
  let published =
    match mem "keys" !client_public with
    | Some (Jsont.Array ([ k ], _)) -> k
    | _ -> failwith "jwks"
  in
  let key =
    match Jsonwt.Jwk.of_json (encode published) with
    | Ok k -> k
    | Error _ -> failwith "jwks key"
  in
  let token =
    match Jsonwt.parse assertion with Ok t -> t | Error _ -> failwith "parse"
  in
  check "assertion signature"
    (Jsonwt.verify ~key ~allowed_algs:[ Jsonwt.Algorithm.ES256 ] token = Ok ());
  let claims = Jsonwt.claims token in
  check "iss" (Jsonwt.Claims.iss claims = Some client_id);
  check "sub" (Jsonwt.Claims.sub claims = Some client_id);
  check "aud" (Jsonwt.Claims.aud claims = [ issuer ]);
  let iat = Option.get (Jsonwt.Claims.iat claims)
  and exp = Option.get (Jsonwt.Claims.exp claims) in
  check "short-lived" (Ptime.Span.to_float_s (Ptime.diff exp iat) <= 60.);
  let jti = Option.get (Jsonwt.Claims.jti claims) in
  check "fresh jti" (not (Hashtbl.mem jtis jti));
  Hashtbl.replace jtis jti ()

(* An atproto authorization server sends a DPoP nonce with every answer. *)
let reply ?(status = 200) ?(headers = []) body req =
  let headers =
    let url = Fetch.Middleware.Url.to_string req.Fetch.Middleware.url in
    if
      List.mem_assoc "dpop-nonce" headers
      || not (String.starts_with ~prefix:"https://as.test/" url)
    then headers
    else ("dpop-nonce", "as-n") :: headers
  in
  Fetch_mock.respond ~status
    ~headers:
      (Http.Header.of_list (("content-type", "application/json") :: headers))
    body req

let body (req : Fetch.Middleware.request) =
  match req.body with Fetch.Middleware.String s -> s | _ -> ""

(* The first request to each endpoint is told to use a nonce. *)
let with_nonce ~demand req k =
  let claims = verify_proof req in
  if demand && Jsonwt.Claims.get_string "nonce" claims <> Some "as-n" then begin
    incr nonces_demanded;
    reply ~status:400
      ~headers:[ ("dpop-nonce", "as-n") ]
      {|{"error":"use_dpop_nonce"}|} req
  end
  else
    let params = form (body req) in
    verify_assertion params;
    k params

let doc =
  {|{"id":"did:plc:alice","alsoKnownAs":["at://Alice.test"],"service":[{"id":"#atproto_pds","type":"AtprotoPersonalDataServer","serviceEndpoint":"https://pds.test"}]}|}

let server (req : Fetch.Middleware.request) =
  let url = Fetch.Middleware.Url.to_string req.url in
  let path =
    match String.index_opt url '?' with
    | Some i -> String.sub url 0 i
    | None -> url
  in
  match path with
  | "https://plc.test/did:plc:alice" -> reply doc req
  | "https://plc.test/did:plc:mallory" ->
      reply
        {|{"id":"did:plc:mallory","service":[{"id":"#atproto_pds","type":"AtprotoPersonalDataServer","serviceEndpoint":"https://pds.test"}]}|}
        req
  | "https://alice.test/.well-known/atproto-did" ->
      Fetch_mock.respond script.handle_names req
  | "https://pds.test/.well-known/oauth-protected-resource" ->
      reply
        {|{"resource":"https://pds.test","authorization_servers":["https://as.test"]}|}
        req
  | "https://as.test/.well-known/oauth-authorization-server" ->
      reply
        ({|{"issuer":"https://as.test","authorization_endpoint":"https://as.test/authorize",|}
       ^ {|"token_endpoint":"https://as.test/token","pushed_authorization_request_endpoint":"https://as.test/par",|}
       ^ {|"revocation_endpoint":"https://as.test/revoke"}|})
        req
  | "https://as.test/par" ->
      with_nonce ~demand:true req (fun params ->
          check "S256" (List.assoc "code_challenge_method" params = "S256");
          check "redirect"
            (List.assoc "redirect_uri" params = "https://site.test/callback");
          check "scope" (List.assoc "scope" params = "atproto");
          check "state is long" (String.length (List.assoc "state" params) >= 22);
          par := params;
          reply ~status:201 {|{"request_uri":"urn:req:1","expires_in":60}|} req)
  | "https://as.test/token" ->
      with_nonce ~demand:script.nonce_on_token req (fun params ->
          check "grant" (List.assoc "grant_type" params = "authorization_code");
          check "code" (List.assoc "code" params = "code-1");
          check "PKCE"
            (Jsonwt.base64url_encode
               (sha256 (List.assoc "code_verifier" params))
            = List.assoc "code_challenge" !par);
          check "same redirect"
            (List.assoc "redirect_uri" params = List.assoc "redirect_uri" !par);
          reply
            (Printf.sprintf
               {|{"access_token":"at","refresh_token":"rt","token_type":"%s","sub":"%s","scope":"%s","expires_in":3600}|}
               script.token_type script.sub script.scope)
            req)
  | "https://as.test/revoke" ->
      with_nonce ~demand:false req (fun params ->
          revoked := List.assoc "token" params :: !revoked;
          reply "{}" req)
  | "https://cloudflare-dns.com/dns-query" ->
      (* No TXT record, so handles resolve over HTTPS. *)
      reply {|{"Status":3}|} req
  | _ -> failwith ("unexpected request " ^ url)

let ok = function
  | Ok v -> v
  | Error e -> failwith ("unexpected error: " ^ W.error_to_string e)

let expect_error expected = function
  | Ok _ -> failwith "expected an error"
  | Error e ->
      if e <> expected then failwith ("wrong error: " ^ W.error_to_string e)

let raises_invalid f =
  match f () with _ -> false | exception Invalid_argument _ -> true

let () =
  Eio_main.run @@ fun env ->
  let clock = env#clock in
  let http = Fetch_mock.client server in
  let key = W.Key.generate () in
  let client =
    W.Client.v ~client_id:"https://site.test/oauth/client-metadata.json"
      ~redirect_uri:"https://site.test/callback"
      ~jwks_uri:"https://site.test/oauth/jwks.json" ~key ~kid:"k1"
      ~client_name:"Site" ()
  in
  (* Client URLs are HTTPS on one origin. *)
  check "http client id refused"
    (raises_invalid (fun () ->
         W.Client.v ~client_id:"http://site.test/m.json"
           ~redirect_uri:"http://site.test/cb" ~jwks_uri:"http://site.test/k"
           ~key ~kid:"k1" ()));
  check "foreign redirect refused"
    (raises_invalid (fun () ->
         W.Client.v ~client_id:"https://site.test/m.json"
           ~redirect_uri:"https://evil.test/cb" ~jwks_uri:"https://site.test/k"
           ~key ~kid:"k1" ()));
  check "scope without atproto refused"
    (raises_invalid (fun () ->
         W.Client.v ~client_id:"https://site.test/m.json"
           ~redirect_uri:"https://site.test/cb" ~jwks_uri:"https://site.test/k"
           ~key ~kid:"k1" ~scope:"transition:generic" ()));
  (* The metadata and the public key set. *)
  let meta = W.Client.metadata client in
  check "auth method"
    (text "token_endpoint_auth_method" meta = Some "private_key_jwt");
  check "signing alg"
    (text "token_endpoint_auth_signing_alg" meta = Some "ES256");
  check "application type" (text "application_type" meta = Some "web");
  check "dpop bound"
    (mem "dpop_bound_access_tokens" meta = Some (Jsont.Json.bool true));
  let jwks = W.Client.jwks client in
  client_public := jwks;
  check "jwks has no private scalar"
    (not (String.split_on_char '"' (encode jwks) |> List.mem "d"));
  (match mem "keys" jwks with
  | Some (Jsont.Array ([ k ], _)) ->
      check "kid" (text "kid" k = Some "k1");
      check "use" (text "use" k = Some "sig")
  | _ -> failwith "one key");
  check "stored key keeps d" (mem "d" (json (W.Key.to_string key)) <> None);
  let start ?ttl identifier =
    ok (W.start ~clock ~http ~plc:"https://plc.test" ?ttl client ~identifier)
  in
  let finish pending query =
    W.finish ~clock ~http ~plc:"https://plc.test" client pending ~query
  in
  let good pending =
    Printf.sprintf "code=code-1&state=%s&iss=%s"
      (Xrpc_auth_protocol.pct_encode (W.pending_state pending))
      (Xrpc_auth_protocol.pct_encode issuer)
  in
  (* The happy path, with nonces demanded on both requests. *)
  let pending, url = start "alice.test" in
  check "authorize url"
    (String.starts_with ~prefix:"https://as.test/authorize?client_id=" url);
  check "login hint" (List.assoc "login_hint" !par = "alice.test");
  check "nonces on PAR" (!nonces_demanded = 1);
  (* The pending login survives the site's store. *)
  let pending =
    match
      Jsont_bytesrw.decode_string W.pending_jsont
        (match Jsont_bytesrw.encode_string W.pending_jsont pending with
        | Ok s -> s
        | Error e -> failwith e)
    with
    | Ok p -> p
    | Error e -> failwith e
  in
  revoked := [];
  let id = ok (finish pending (good pending)) in
  check "nonce on token" (!nonces_demanded = 2);
  check "did" (id.did = did);
  check "verified handle" (id.handle = Some "alice.test");
  check "pds" (id.pds = "https://pds.test");
  check "scope" (id.scope = "atproto");
  check "tokens revoked" (!revoked = [ "rt" ]);
  (* Callback checks, each against a fresh login. *)
  let p, _ = start did in
  expect_error `Wrong_state
    (finish p
       (Printf.sprintf "code=code-1&state=forged&iss=%s"
          (Xrpc_auth_protocol.pct_encode issuer)));
  let p, _ = start did in
  expect_error (`Missing "iss")
    (finish p
       ("code=code-1&state=" ^ Xrpc_auth_protocol.pct_encode (W.pending_state p)));
  let p, _ = start did in
  expect_error `Wrong_issuer
    (finish p
       (Printf.sprintf "code=code-1&state=%s&iss=https%%3A%%2F%%2Fevil.test"
          (Xrpc_auth_protocol.pct_encode (W.pending_state p))));
  let p, _ = start did in
  expect_error
    (`Denied ("access_denied", Some "no"))
    (finish p (good p ^ "&error=access_denied&error_description=no"));
  let p, _ = start did in
  expect_error (`Duplicate "state")
    (finish p (good p ^ "&state=" ^ W.pending_state p));
  let p, _ = start ~ttl:(-1.) did in
  expect_error `Expired (finish p (good p));
  (* Token responses that are not acceptable, whose tokens are still
     revoked. *)
  let refused p =
    revoked := [];
    match finish p (good p) with
    | Error (`Refused _) -> check "revoked after refusal" (!revoked = [ "rt" ])
    | _ -> failwith "expected a refusal"
  in
  script.nonce_on_token <- false;
  script.sub <- "did:plc:mallory";
  refused (fst (start did));
  reset ();
  script.nonce_on_token <- false;
  script.token_type <- "Bearer";
  refused (fst (start did));
  reset ();
  script.nonce_on_token <- false;
  script.scope <- "transition:generic";
  refused (fst (start did));
  (* A handle whose host names another DID is not given. *)
  reset ();
  script.nonce_on_token <- false;
  script.handle_names <- "did:plc:mallory";
  let p, _ = start did in
  check "unverified handle omitted" ((ok (finish p (good p))).handle = None);
  (* Starting from a server, the account's PDS must name the same issuer. *)
  reset ();
  script.nonce_on_token <- false;
  let p, _ = start "https://pds.test" in
  check "no hint" (not (List.mem_assoc "login_hint" !par));
  check "server-first login" ((ok (finish p (good p))).did = did);
  (* Only HTTPS leaves. *)
  (match W.start ~clock ~http ~plc:"http://plc.test" client ~identifier:did with
  | Error (`Refused _) -> ()
  | _ -> failwith "plain HTTP refused");
  (* Addresses. *)
  let ip s =
    match Ipaddr.of_string s with
    | Ok (Ipaddr.V4 v4) -> Eio.Net.Ipaddr.of_raw (Ipaddr.V4.to_octets v4)
    | Ok (Ipaddr.V6 v6) -> Eio.Net.Ipaddr.of_raw (Ipaddr.V6.to_octets v6)
    | Error _ -> failwith s
  in
  List.iter
    (fun (s, public) ->
      if W.Public_net.is_public (ip s) <> public then failwith ("address " ^ s))
    [
      ("8.8.8.8", true);
      ("1.1.1.1", true);
      ("2606:4700::1111", true);
      ("0.0.0.0", false);
      ("10.1.2.3", false);
      ("100.64.0.1", false);
      ("100.128.0.1", true);
      ("127.0.0.1", false);
      ("169.254.169.254", false);
      ("172.16.0.1", false);
      ("172.32.0.1", true);
      ("192.168.1.1", false);
      ("198.18.0.1", false);
      ("224.0.0.1", false);
      ("255.255.255.255", false);
      ("::", false);
      ("::1", false);
      ("::ffff:127.0.0.1", false);
      ("::ffff:8.8.8.8", true);
      ("64:ff9b::a00:1", false);
      ("fc00::1", false);
      ("fd12::1", false);
      ("fe80::1", false);
      ("ff02::1", false);
      ("2001:db8::1", false);
      ("2002:7f00:1::1", false);
      ("2002:808:808::1", true);
      ("2001:0:4136:e378::1", false);
      ("64:ff9b:1::8.8.8.8", false);
      ("fec0::1", false);
      ("100::1", false);
    ];
  (* A host with no public address is refused before dialling. *)
  List.iter
    (fun host ->
      match
        Eio.Switch.run @@ fun sw ->
        W.Public_net.connect env#net ~sw ~host ~port:9
      with
      | _ -> failwith ("connected to " ^ host)
      | exception Failure _ -> ())
    [ "127.0.0.1"; "::1"; "localhost" ];
  print_endline "web oauth: ok"
