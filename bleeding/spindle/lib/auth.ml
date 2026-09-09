(* SPDX-License-Identifier: ISC *)
open Json

external verify : string -> string -> string -> bool
  = "spindle_verify_es256k" [@@noalloc]

exception Rejected

let reject () = raise Rejected
let check b = if not b then reject ()

let base64 s =
  check (String.for_all (function
    | 'A' .. 'Z' | 'a' .. 'z' | '0' .. '9' | '-' | '_' -> true
    | _ -> false) s);
  match Base64.decode ~pad:false ~alphabet:Base64.uri_safe_alphabet s with
  | Error _ -> reject ()
  | Ok raw ->
      check (Base64.encode_string ~pad:false
        ~alphabet:Base64.uri_safe_alphabet raw = s);
      raw

let authenticate ~read ~plc ~actor ~audience ~meth ~now token =
  try
    check (String.length token <= 8192);
    let header, payload, signature = match String.split_on_char '.' token with
      | [h; p; s] -> h, p, s | _ -> reject () in
    let h = decode (base64 header) in
    let p = decode (base64 payload) in
    check (get "alg" h = "ES256K");
    check (field "crit" h = None);
    check (field "b64" h = None);
    check (get "iss" p = actor && get "aud" p = audience);
    check (get "lxm" p = meth);
    let exp = number (required "exp" p) in
    check (exp = Float.floor exp && exp > now && exp <= now +. 3600.);
    (match field "iat" p with None -> () | Some value ->
      let iat = number value in
      check (iat = Float.floor iat && iat <= now +. 30. && iat <= exp));
    (match field "nbf" p with None -> () | Some value ->
      check (number value <= now));
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
    check (verify (String.sub raw 2 33) (header ^ "." ^ payload)
      (base64 signature));
    actor
  with Invalid _ | Rejected -> raise Rejected
