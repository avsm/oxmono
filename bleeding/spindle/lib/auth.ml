(* SPDX-License-Identifier: ISC *)
open Json

module Jwt = Jsonwt

exception Rejected

let reject () = raise Rejected
let check b = if not b then reject ()
let jwt_result = function Ok value -> value | Error _ -> reject ()
let required_number name claims =
  match Jwt.Claims.get_number name claims with
  | Some n -> n | None -> reject ()

let authenticate ~read ~plc ~actor ~audience ~meth ~now token =
  try
    let token = jwt_result (Jwt.parse ~max_size:8192 token) in
    let header = Jwt.header token in
    let claims = Jwt.claims token in
    check (header.alg = Jwt.Algorithm.ES256K);
    check (header.typ = Some "JWT");
    check (header.kid = None || header.kid = Some "#atproto");
    check (Jwt.Claims.iss claims = Some actor);
    check (Jwt.Claims.get_string "aud" claims = Some audience);
    check (Jwt.Claims.get_string "lxm" claims = Some meth);
    let exp = required_number "exp" claims in
    check (exp = Float.floor exp && exp > now && exp <= now +. 3600.);
    let iat = required_number "iat" claims in
    check (iat = Float.floor iat && iat <= now +. 30. && iat <= exp);
    (match Jwt.Claims.get_number "nbf" claims with None -> () | Some nbf ->
      check (nbf <= now));
    (* The configured PLC is the trust root. Refresh on each dispatch to
       observe key changes. *)
    let document = decode (read (plc ^ "/" ^ actor)) in
    check (get "id" document = actor);
    let keys = list (required "verificationMethod" document) in
    let key = List.find_opt (fun key -> get "id" key = actor ^ "#atproto")
        keys in
    let key = match key with Some key -> key | None -> reject () in
    check (get "controller" key = actor);
    let raw = match Multibase.decode (get "publicKeyMultibase" key) with
      | Ok (`Base58btc, raw) -> raw | _ -> reject () in
    (* multicodec secp256k1-pub = 0xe7, followed by a compressed SEC1 point. *)
    check (String.length raw = 35 && String.sub raw 0 2 = "\231\001");
    let key = jwt_result (Jwt.Jwk.secp256k1_pub (String.sub raw 2 33)) in
    jwt_result (Jwt.verify ~key ~allowed_algs:[Jwt.Algorithm.ES256K] token);
    actor
  with Invalid _ | Rejected -> raise Rejected
