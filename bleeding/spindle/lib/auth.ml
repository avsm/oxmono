(* SPDX-License-Identifier: ISC *)
open Json
module Jwt = Jsonwt

exception Rejected

let reject () = raise Rejected
let check b = if not b then reject ()
let jwt_result = function Ok value -> value | Error _ -> reject ()

let required_number name claims =
  match Jwt.Claims.get_number name claims with Some n -> n | None -> reject ()

let verification_key algorithm raw =
  check (String.length raw = 35);
  let point = String.sub raw 2 33 in
  match (algorithm, String.sub raw 0 2) with
  | Jwt.Algorithm.ES256K, "\231\001" -> jwt_result (Jwt.Jwk.secp256k1_pub point)
  | Jwt.Algorithm.ES256, "\128\036" ->
      let module EC = Mirage_crypto_ec.P256.Dsa in
      let pub =
        match EC.pub_of_octets point with
        | Ok pub -> EC.pub_to_octets pub
        | Error _ -> reject ()
      in
      Jwt.Jwk.p256_pub ~x:(String.sub pub 1 32) ~y:(String.sub pub 33 32)
  | _ -> reject ()

let authenticate ~resolve ~consume ~audience ~meth ~now token =
  try
    let token = jwt_result (Jwt.parse ~max_size:8192 token) in
    let header = Jwt.header token in
    let claims = Jwt.claims token in
    check (header.alg = Jwt.Algorithm.ES256K || header.alg = ES256);
    check (header.typ = Some "JWT");
    check (header.kid = None || header.kid = Some "#atproto");
    let actor =
      match Jwt.Claims.iss claims with
      | Some actor when Atp.Did.is_valid actor -> actor
      | _ -> reject ()
    in
    let aud = Jwt.Claims.get_string "aud" claims in
    check (aud = Some audience || aud = Some (audience ^ "#tangled_spindle"));
    check (Jwt.Claims.get_string "lxm" claims = Some meth);
    let exp = required_number "exp" claims in
    check (exp = Float.floor exp && exp > now && exp <= now +. 3600.);
    let iat = required_number "iat" claims in
    check (iat = Float.floor iat && iat <= now +. 30. && iat <= exp);
    check (exp -. iat <= 3600.);
    (match Jwt.Claims.get_number "nbf" claims with
    | None -> ()
    | Some nbf -> check (nbf <= now));
    let jti =
      match Jwt.Claims.jti claims with
      | Some jti when String.length jti > 0 && String.length jti <= 256 -> jti
      | _ -> reject ()
    in
    let document = decode (resolve actor) in
    check (get "id" document = actor);
    let keys = list (required "verificationMethod" document) in
    let key =
      List.find_opt
        (fun key ->
          let id = get "id" key in
          id = actor ^ "#atproto" || id = "#atproto")
        keys
    in
    let key = match key with Some key -> key | None -> reject () in
    check (get "controller" key = actor && get "type" key = "Multikey");
    let raw =
      match Multibase.decode (get "publicKeyMultibase" key) with
      | Ok (`Base58btc, raw) -> raw
      | _ -> reject ()
    in
    let key = verification_key header.alg raw in
    jwt_result
      (Jwt.verify ~key ~allowed_algs:[ Jwt.Algorithm.ES256K; ES256 ] token);
    (* Persist only after verification, so forged claims cannot consume a
       legitimate nonce. Consumption precedes every authenticated mutation. *)
    check (consume ~issuer:actor ~jti ~expires:exp);
    actor
  with Invalid _ | Invalid_argument _ | Rejected -> raise Rejected
