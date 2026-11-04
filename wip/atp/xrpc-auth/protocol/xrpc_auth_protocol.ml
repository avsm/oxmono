(*---------------------------------------------------------------------------
   Copyright (c) 2026 Anil Madhavapeddy. All rights reserved.
   SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

module P256 = Mirage_crypto_ec.P256.Dsa

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

let equal = Eqaf.equal

let json_string value =
  match Jsont_bytesrw.encode_string Jsont.json value with
  | Ok s -> s
  | Error e -> failwith e

let obj pairs =
  Jsont.Json.object'
    (List.map (fun (k, v) -> Jsont.Json.mem (k, Jsont.Meta.none) v) pairs)

let str = Jsont.Json.string

type fields = (string * [ `S of string | `N of float | `L of string list ]) list

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

let strip_slash s =
  if String.ends_with ~suffix:"/" s then String.sub s 0 (String.length s - 1)
  else s

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

let decode_query query =
  List.filter_map
    (fun pair ->
      match String.index_opt pair '=' with
      | None -> None
      | Some j ->
          Some
            ( pct_decode (String.sub pair 0 j),
              pct_decode (String.sub pair (j + 1) (String.length pair - j - 1))
            ))
    (String.split_on_char '&' query)

(* Keys *)

module Key = struct
  type t = { priv : P256.priv; pub : P256.pub }

  let generate () =
    ensure_rng ();
    let priv, pub = P256.generate () in
    { priv; pub }

  let coordinates pub =
    let point = P256.pub_to_octets pub in
    (b64 (String.sub point 1 32), b64 (String.sub point 33 32))

  let public_jwk ?kid k =
    let x, y = coordinates k.pub in
    obj
      ([ ("kty", str "EC"); ("crv", str "P-256"); ("x", str x); ("y", str y) ]
      @ match kid with Some kid -> [ ("kid", str kid) ] | None -> [])

  let to_string k =
    let x, y = coordinates k.pub in
    json_string
      (obj
         [
           ("kty", str "EC");
           ("crv", str "P-256");
           ("x", str x);
           ("y", str y);
           ("d", str (b64 (P256.priv_to_octets k.priv)));
         ])

  let of_string text =
    let d =
      match field (parse text) "d" with
      | Some d -> d
      | None -> invalid_arg "The key lacks a private scalar"
    in
    match Jsonwt.base64url_decode d with
    | Error _ -> invalid_arg "Malformed key"
    | Ok d -> (
        match P256.priv_of_octets d with
        | Ok priv -> { priv; pub = P256.pub_of_priv priv }
        | Error _ -> invalid_arg "Malformed key")

  let sign k input =
    ensure_rng ();
    let r, s = P256.sign ~mask:`Yes ~key:k.priv (sha256 input) in
    r ^ s
end

(* A JWS whose header is [header] and payload [claims], signed by [key]. *)
let jws key header claims =
  let input = b64 (json_string header) ^ "." ^ b64 (json_string claims) in
  input ^ "." ^ b64 (Key.sign key input)

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
    obj
      [
        ("alg", str "ES256");
        ("typ", str "dpop+jwt");
        ("jwk", Key.public_jwk key);
      ]
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
  jws key header claims

(* Nonces are per origin. A server may hand out a fresh one on any reply. *)
type dpop = { key : Key.t; nonces : (string, string) Hashtbl.t }

let dpop key = { key; nonces = Hashtbl.create 2 }
let dpop_key d = d.key

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

(* Client authentication *)

type client =
  | Public of string
  | Confidential of { id : string; key : Key.t; kid : string }

let client_id = function Public id -> id | Confidential c -> c.id

(* RFC 7523 section 3. A short lifetime and a random [jti] keep a stolen
   assertion from being replayed for long. *)
let client_assertion ~key ~kid ~client_id ~issuer ~now =
  let now = int_of_float now in
  jws key
    (obj [ ("alg", str "ES256"); ("typ", str "JWT"); ("kid", str kid) ])
    (obj
       [
         ("iss", str client_id);
         ("sub", str client_id);
         ("aud", str issuer);
         ("jti", str (random 16));
         ("iat", Jsont.Json.int now);
         ("exp", Jsont.Json.int (now + 60));
       ])

let assertion_type = "urn:ietf:params:oauth:client-assertion-type:jwt-bearer"

let authentication client ~issuer ~now =
  match client with
  | Public id -> [ ("client_id", id) ]
  | Confidential { id; key; kid } ->
      [
        ("client_id", id);
        ("client_assertion_type", assertion_type);
        ( "client_assertion",
          client_assertion ~key ~kid ~client_id:id ~issuer ~now );
      ]

(* Authorization server requests *)

let read_body response =
  Fetch.decode ~limit:(1024 * 1024) Fetch.Media.octets response

(* Posts a form with a DPoP proof, retrying once with the nonce the server
   demands (RFC 9449 section 8). The client's assertion is made afresh for
   each attempt, so that its [jti] is never sent twice. *)
let post_form ~clock ~http dpop ?client url pairs =
  let send () =
    let now = Eio.Time.now clock in
    let auth =
      match client with
      | Some (client, issuer) -> authentication client ~issuer ~now
      | None -> []
    in
    let headers, body = Fetch.Form.urlencoded (pairs @ auth) in
    let proof =
      proof dpop.key ~now ~meth:"POST" ~url ?nonce:(nonce dpop url) ()
    in
    let headers =
      Fetch.Header.append headers [ Fetch.Header.raw "DPoP" proof ]
    in
    Fetch.with_response ~headers ~body ~redirects:0 ~sensitive:[ "dpop" ] http
      `POST url (fun response ->
        let headers = Fetch.headers response in
        (* atproto makes server nonces mandatory, so an answer to a DPoP
           request without one is not from a conforming server. *)
        if Http.Header.get headers "dpop-nonce" = None then
          failwith (url ^ " sent no DPoP-Nonce");
        remember dpop url headers;
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

let server_jsont =
  Jsont.Object.map ~kind:"server"
    (fun
      issuer
      authorization_endpoint
      token_endpoint
      par_endpoint
      revocation_endpoint
    ->
      {
        issuer;
        authorization_endpoint;
        token_endpoint;
        par_endpoint;
        revocation_endpoint;
      })
  |> Jsont.Object.mem "issuer" Jsont.string ~enc:(fun s -> s.issuer)
  |> Jsont.Object.mem "authorization_endpoint" Jsont.string ~enc:(fun s ->
      s.authorization_endpoint)
  |> Jsont.Object.mem "token_endpoint" Jsont.string ~enc:(fun s ->
      s.token_endpoint)
  |> Jsont.Object.mem "par_endpoint" Jsont.string ~enc:(fun s -> s.par_endpoint)
  |> Jsont.Object.opt_mem "revocation_endpoint" Jsont.string ~enc:(fun s ->
      s.revocation_endpoint)
  |> Jsont.Object.finish

let server_of_pds ~http pds =
  let pds = strip_slash pds in
  let resource =
    parse (get_json ~http (pds ^ "/.well-known/oauth-protected-resource"))
  in
  let issuer =
    match List.assoc_opt "authorization_servers" resource with
    | Some (`L [ issuer ]) -> strip_slash issuer
    | Some (`L (_ :: _ :: _)) ->
        (* atproto allows exactly one, so the choice is never the client's. *)
        failwith (pds ^ " names more than one authorization server")
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

let check_token_type fields =
  match field fields "token_type" with
  | Some t when String.lowercase_ascii t = "dpop" -> ()
  | _ -> failwith "Authorization server did not issue a DPoP-bound token"

let account_pds ?plc ~http ~issuer ~expected ~pds sub =
  match (expected, pds) with
  | Some expected, Some pds ->
      if sub <> expected then failwith "Signed in as a different account";
      pds
  | Some _, None -> invalid_arg "An expected account needs its PDS"
  | None, _ ->
      let _, pds = pds_of ?plc ~http sub in
      if (server_of_pds ~http pds).issuer <> issuer then
        failwith "Account is not served by this authorization server";
      pds
