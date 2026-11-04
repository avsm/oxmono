(*---------------------------------------------------------------------------
   Copyright (c) 2026 Anil Madhavapeddy. All rights reserved.
   SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

module P = Xrpc_auth_protocol
module Key = P.Key

let obj pairs =
  Jsont.Json.object'
    (List.map (fun (k, v) -> Jsont.Json.mem (k, Jsont.Meta.none) v) pairs)

let str = Jsont.Json.string
let strs l = Jsont.Json.list (List.map str l)

let origin_of what url =
  if not (String.starts_with ~prefix:"https://" url) then
    invalid_arg (what ^ " must be an HTTPS URL");
  if String.contains url '#' then invalid_arg (what ^ " must have no fragment");
  match Fetch.Middleware.Url.of_string url with
  | Ok u -> Fetch.Middleware.Url.origin u
  | Error e -> invalid_arg (what ^ ": " ^ e)

let has_atproto scope = List.mem "atproto" (String.split_on_char ' ' scope)

module Client = struct
  type t = {
    id : string;
    redirect_uri : string;
    jwks_uri : string;
    key : Key.t;
    kid : string;
    scope : string;
    client_name : string option;
    client_uri : string option;
  }

  let v ~client_id ~redirect_uri ~jwks_uri ~key ~kid ?(scope = "atproto")
      ?client_name ?client_uri () =
    let origin = origin_of "The client ID" client_id in
    (* The redirect and the keys are the client's own, on its origin. *)
    if origin_of "The redirect URI" redirect_uri <> origin then
      invalid_arg "The redirect URI must be on the client ID's origin";
    if origin_of "The JWKS URI" jwks_uri <> origin then
      invalid_arg "The JWKS URI must be on the client ID's origin";
    if not (has_atproto scope) then invalid_arg "The scope must include atproto";
    if kid = "" then invalid_arg "The key ID is empty";
    {
      id = client_id;
      redirect_uri;
      jwks_uri;
      key;
      kid;
      scope;
      client_name;
      client_uri;
    }

  let id t = t.id
  let redirect_uri t = t.redirect_uri
  let scope t = t.scope

  let metadata t =
    let opt k = function Some v -> [ (k, str v) ] | None -> [] in
    obj
      ([ ("client_id", str t.id) ]
      @ opt "client_name" t.client_name
      @ opt "client_uri" t.client_uri
      @ [
          ("application_type", str "web");
          ("grant_types", strs [ "authorization_code"; "refresh_token" ]);
          ("response_types", strs [ "code" ]);
          ("redirect_uris", strs [ t.redirect_uri ]);
          ("scope", str t.scope);
          ("token_endpoint_auth_method", str "private_key_jwt");
          ("token_endpoint_auth_signing_alg", str "ES256");
          ("jwks_uri", str t.jwks_uri);
          ("dpop_bound_access_tokens", Jsont.Json.bool true);
        ])

  let jwks t =
    let key =
      match Key.public_jwk ~kid:t.kid t.key with
      | Jsont.Object (members, meta) ->
          Jsont.Object
            ( members
              @ [
                  Jsont.Json.mem ("use", Jsont.Meta.none) (str "sig");
                  Jsont.Json.mem ("alg", Jsont.Meta.none) (str "ES256");
                ],
              meta )
      | j -> j
    in
    obj [ ("keys", Jsont.Json.list [ key ]) ]

  let protocol t = P.Confidential { id = t.id; key = t.key; kid = t.kid }
end

type pending = {
  state : string;
  verifier : string;
  dpop_key : string;
  server : P.server;
  expected : string option;
  pds : string option;
  client_id : string;
  redirect_uri : string;
  scope : string;
  expires_at : float;
}

let pending_state p = p.state
let pending_expires_at p = p.expires_at

let pending_jsont =
  Jsont.Object.map ~kind:"pending login"
    (fun
      state
      verifier
      dpop_key
      server
      expected
      pds
      client_id
      redirect_uri
      scope
      expires_at
    ->
      {
        state;
        verifier;
        dpop_key;
        server;
        expected;
        pds;
        client_id;
        redirect_uri;
        scope;
        expires_at;
      })
  |> Jsont.Object.mem "state" Jsont.string ~enc:(fun p -> p.state)
  |> Jsont.Object.mem "verifier" Jsont.string ~enc:(fun p -> p.verifier)
  |> Jsont.Object.mem "dpop_key" Jsont.string ~enc:(fun p -> p.dpop_key)
  |> Jsont.Object.mem "server" P.server_jsont ~enc:(fun p -> p.server)
  |> Jsont.Object.opt_mem "expected" Jsont.string ~enc:(fun p -> p.expected)
  |> Jsont.Object.opt_mem "pds" Jsont.string ~enc:(fun p -> p.pds)
  |> Jsont.Object.mem "client_id" Jsont.string ~enc:(fun p -> p.client_id)
  |> Jsont.Object.mem "redirect_uri" Jsont.string ~enc:(fun p -> p.redirect_uri)
  |> Jsont.Object.mem "scope" Jsont.string ~enc:(fun p -> p.scope)
  |> Jsont.Object.mem "expires_at" Jsont.number ~enc:(fun p -> p.expires_at)
  |> Jsont.Object.finish

type identity = {
  did : string;
  handle : string option;
  pds : string;
  scope : string;
}

type error =
  [ `Expired
  | `Wrong_state
  | `Duplicate of string
  | `Missing of string
  | `Wrong_issuer
  | `Denied of string * string option
  | `Refused of string ]

let error_to_string = function
  | `Expired -> "The login took too long. Try again."
  | `Wrong_state -> "The login does not match the one this site started."
  | `Duplicate name -> "The login response repeats " ^ name
  | `Missing name -> "The login response lacks " ^ name
  | `Wrong_issuer -> "The login response names another authorization server."
  | `Denied (code, None) -> "The login was refused: " ^ code
  | `Denied (code, Some d) -> "The login was refused: " ^ code ^ ": " ^ d
  | `Refused message -> message

(* Every request leaves over HTTPS, whatever a document names. *)
let https_only http =
  Fetch.restrict http ~filter:(fun request ->
      if
        String.starts_with ~prefix:"https://"
          (Fetch.Middleware.Url.to_string request.Fetch.Middleware.url)
      then `Allow
      else `Reject "only HTTPS is allowed")

let guard f =
  match f () with
  | v -> v
  | exception (Eio.Cancel.Cancelled _ as e) -> raise e
  | exception Failure m -> Error (`Refused m)
  | exception Invalid_argument m -> Error (`Refused m)
  | exception (Eio.Io _ as e) -> Error (`Refused (Printexc.to_string e))

let start ~clock ~http ?plc ?(ttl = 600.) client ~identifier =
  guard @@ fun () ->
  let http = https_only http in
  let expected, pds, hint =
    if String.starts_with ~prefix:"https://" identifier then
      (None, identifier, None)
    else
      let did, pds = P.pds_of ?plc ~http identifier in
      (Some did, pds, Some identifier)
  in
  let server = P.server_of_pds ~http pds in
  (* The browser is sent to the authorization endpoint, so it must be a
     plain HTTPS address. *)
  if
    not
      (String.starts_with ~prefix:"https://" server.authorization_endpoint
      && String.for_all
           (fun c -> c > ' ' && c < '\127' && c <> '"' && c <> '<' && c <> '>')
           server.authorization_endpoint)
  then failwith "The authorization server's endpoint is not an HTTPS address";
  let dpop = P.dpop (Key.generate ()) in
  let verifier = P.random 32 and state = P.random 32 in
  let par =
    P.post_form ~clock ~http dpop
      ~client:(Client.protocol client, server.issuer)
      server.par_endpoint
      ([
         ("response_type", "code");
         ("redirect_uri", Client.redirect_uri client);
         ("scope", Client.scope client);
         ("state", state);
         ("code_challenge", P.b64 (P.sha256 verifier));
         ("code_challenge_method", "S256");
       ]
      @ Option.to_list (Option.map (fun id -> ("login_hint", id)) hint))
  in
  let request_uri =
    P.required "Pushed authorization response" par "request_uri"
  in
  let pending =
    {
      state;
      verifier;
      dpop_key = Key.to_string (P.dpop_key dpop);
      server;
      expected;
      pds = (if expected = None then None else Some pds);
      client_id = Client.id client;
      redirect_uri = Client.redirect_uri client;
      scope = Client.scope client;
      expires_at = Eio.Time.now clock +. ttl;
    }
  in
  Ok
    ( pending,
      server.authorization_endpoint ^ "?"
      ^ P.encode_query
          [ ("client_id", Client.id client); ("request_uri", request_uri) ] )

(* The callback's parameters, none of them repeated. *)
let callback query =
  let pairs = P.decode_query query in
  let rec check seen = function
    | [] -> Ok pairs
    | (k, _) :: rest ->
        if List.mem k seen then Error (`Duplicate k) else check (k :: seen) rest
  in
  check [] pairs

let ( let* ) = Result.bind

let member name = function
  | Jsont.Object (members, _) -> (
      match Jsont.Json.find_mem name members with
      | Some (_, v) -> Some v
      | None -> None)
  | _ -> None

(* A handle counts only when the DID document claims it and the handle's host
   names the DID back. *)
let verified_handle ?plc ~http did =
  match Xrpc.Identity.document ?plc http did with
  | Error _ -> None
  | Ok body -> (
      match Jsont_bytesrw.decode_string Jsont.json body with
      | Error _ -> None
      | Ok doc -> (
          if
            match member "id" doc with
            | Some (Jsont.String (id, _)) -> id <> did
            | _ -> true
          then None
          else
            let claimed =
              match member "alsoKnownAs" doc with
              | Some (Jsont.Array (items, _)) ->
                  List.find_map
                    (function
                      | Jsont.String (aka, _)
                        when String.starts_with ~prefix:"at://" aka ->
                          Some (String.sub aka 5 (String.length aka - 5))
                      | _ -> None)
                    items
              | _ -> None
            in
            match claimed with
            | None -> None
            | Some handle -> (
                match Xrpc.Identity.did_of_handle http handle with
                | Ok back when back = did ->
                    Some (String.lowercase_ascii handle)
                | _ -> None)))

let revoke ~clock ~http client dpop server token =
  match server.P.revocation_endpoint with
  | None -> ()
  | Some url -> (
      try
        ignore
          (P.post_form ~clock ~http dpop
             ~client:(Client.protocol client, server.issuer)
             url
             [ ("token", token) ])
      with
      | Eio.Cancel.Cancelled _ as e -> raise e
      | _ -> ())

let finish ~clock ~http ?plc client pending ~query =
  let http = https_only http in
  let* params = callback query in
  let param name = List.assoc_opt name params in
  if Eio.Time.now clock > pending.expires_at then Error `Expired
  else if
    pending.client_id <> Client.id client
    || pending.redirect_uri <> Client.redirect_uri client
  then Error (`Refused "The login was started by another client")
  else
    let* state =
      match param "state" with
      | Some s -> Ok s
      | None -> Error (`Missing "state")
    in
    if not (P.equal state pending.state) then Error `Wrong_state
    else
      (* RFC 9207: a web client always requires the issuer, which stops a
         code from one server being replayed as if from another. *)
      let* () =
        match param "iss" with
        | None -> Error (`Missing "iss")
        | Some iss when P.strip_slash iss = pending.server.issuer -> Ok ()
        | Some _ -> Error `Wrong_issuer
      in
      let* () =
        match param "error" with
        | Some code -> Error (`Denied (code, param "error_description"))
        | None -> Ok ()
      in
      let* code =
        match param "code" with
        | Some c -> Ok c
        | None -> Error (`Missing "code")
      in
      guard @@ fun () ->
      let dpop = P.dpop (Key.of_string pending.dpop_key) in
      let token =
        P.post_form ~clock ~http dpop
          ~client:(Client.protocol client, pending.server.issuer)
          pending.server.token_endpoint
          [
            ("grant_type", "authorization_code");
            ("code", code);
            ("redirect_uri", pending.redirect_uri);
            ("code_verifier", pending.verifier);
          ]
      in
      let what = "Token response" in
      (* The tokens go whatever happens next. *)
      let forget () =
        Option.iter
          (revoke ~clock ~http client dpop pending.server)
          (match P.field token "refresh_token" with
          | Some t -> Some t
          | None -> P.field token "access_token")
      in
      Fun.protect ~finally:forget @@ fun () ->
      P.check_token_type token;
      let scope = P.required what token "scope" in
      if not (has_atproto scope) then
        failwith "The token was not granted the atproto scope";
      let did = P.required what token "sub" in
      let pds =
        P.account_pds ?plc ~http ~issuer:pending.server.issuer
          ~expected:pending.expected ~pds:pending.pds did
      in
      Ok
        {
          did;
          handle = verified_handle ?plc ~http did;
          pds = P.strip_slash pds;
          scope;
        }

module Public_net = struct
  let v4_public (a : string) =
    let b i = Char.code a.[i] in
    let in_ prefix bits =
      let rec go i bits =
        if bits <= 0 then true
        else if bits >= 8 then b i = List.nth prefix i && go (i + 1) (bits - 8)
        else
          let mask = (0xff lsl (8 - bits)) land 0xff in
          b i land mask = List.nth prefix i land mask
      in
      go 0 bits
    in
    not
      (List.exists
         (fun (prefix, bits) -> in_ prefix bits)
         [
           ([ 0; 0; 0; 0 ], 8);
           ([ 10; 0; 0; 0 ], 8);
           ([ 100; 64; 0; 0 ], 10);
           ([ 127; 0; 0; 0 ], 8);
           ([ 169; 254; 0; 0 ], 16);
           ([ 172; 16; 0; 0 ], 12);
           ([ 192; 0; 0; 0 ], 24);
           ([ 192; 0; 2; 0 ], 24);
           ([ 192; 88; 99; 0 ], 24);
           ([ 192; 168; 0; 0 ], 16);
           ([ 198; 18; 0; 0 ], 15);
           ([ 198; 51; 100; 0 ], 24);
           ([ 203; 0; 113; 0 ], 24);
           ([ 224; 0; 0; 0 ], 4);
           ([ 240; 0; 0; 0 ], 4);
         ])

  let v6_public (a : string) =
    let b i = Char.code a.[i] in
    let zeros n = String.for_all (fun c -> c = '\000') (String.sub a 0 n) in
    let zeros_from i n =
      String.for_all (fun c -> c = '\000') (String.sub a i n)
    in
    let embedded () = v4_public (String.sub a 12 4) in
    if zeros 16 then false (* :: *)
    else if zeros 15 && b 15 = 1 then false (* ::1 *)
    else if zeros 10 && b 10 = 0xff && b 11 = 0xff then embedded ()
      (* ::ffff:0:0/96, IPv4-mapped *)
    else if zeros 12 then embedded () (* ::/96, IPv4-compatible *)
    else if b 0 = 0x00 && b 1 = 0x64 && b 2 = 0xff && b 3 = 0x9b && b 4 = 0
            && b 5 = 1 then false (* 64:ff9b:1::/48, local NAT64 *)
    else if b 0 = 0x00 && b 1 = 0x64 && b 2 = 0xff && b 3 = 0x9b then
      embedded () (* 64:ff9b::/96, NAT64 *)
    else if b 0 = 0x20 && b 1 = 0x02 then v4_public (String.sub a 2 4)
      (* 2002::/16, 6to4 names an IPv4 address *)
    else if b 0 = 0x20 && b 1 = 0x01 && b 2 = 0 && b 3 = 0 then false
      (* 2001::/32, Teredo *)
    else if b 0 = 0x01 && b 1 = 0 && zeros_from 2 6 then false
      (* 100::/64, discard only *)
    else if b 0 = 0xfe && b 1 land 0xc0 = 0xc0 then false (* fec0::/10 *)
    else if b 0 land 0xfe = 0xfc then false (* fc00::/7, unique local *)
    else if b 0 = 0xfe && b 1 land 0xc0 = 0x80 then false (* fe80::/10 *)
    else if b 0 = 0xff then false (* ff00::/8, multicast *)
    else if b 0 = 0x20 && b 1 = 0x01 && b 2 = 0x0d && b 3 = 0xb8 then
      false (* 2001:db8::/32, documentation *)
    else true

  let is_public ip =
    Eio.Net.Ipaddr.fold
      ~v4:(fun v4 -> v4_public (v4 : [> `V4 ] Eio.Net.Ipaddr.t :> string))
      ~v6:(fun v6 -> v6_public (v6 : [> `V6 ] Eio.Net.Ipaddr.t :> string))
      ip

  let connect net : Fetch_httpz.connect =
   fun ~sw ~host ~port ->
    let addrs =
      Eio.Net.getaddrinfo_stream ~service:(string_of_int port) net host
      |> List.filter (function
        | `Tcp (ip, _) -> is_public ip
        | `Unix _ -> false)
    in
    let rec go first = function
      | [] -> (
          match first with
          | Some e -> raise e
          | None -> failwith (host ^ " has no public address"))
      | addr :: rest -> (
          match Eio.Net.connect ~sw net addr with
          | sock -> (sock :> Fetch_httpz.conn)
          | exception (Eio.Cancel.Cancelled _ as e) -> raise e
          | exception e -> go (Some (Option.value first ~default:e)) rest)
    in
    go None addrs
end
