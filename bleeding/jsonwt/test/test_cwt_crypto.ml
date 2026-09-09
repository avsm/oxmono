(* SPDX-License-Identifier: ISC *)
(* Standalone domain ownership avoids a dependency on a runtime thread pool. *)
[@@@alert "-do_not_spawn_domains"]
module C = Jsonwt_cwt
module P = Cwt_pristine
let ok = function Ok x -> x | Error e -> failwith (C.error_to_string e)
let old_ok = function Ok x -> x | Error e -> failwith (P.error_to_string e)
let check name b = if not b then failwith name
let now = Ptime.epoch
let key = C.Cose_key.symmetric (String.make 64 'k')
    |> C.Cose_key.with_alg C.Algorithm.HMAC_256
let shared = ok (C.create ~key ~algorithm:C.Algorithm.HMAC_256
    ~claims:(C.Claims.build C.Claims.empty))
let portable = (fun () ->
  for _ = 1 to 200 do
    let parsed = match C.parse (C.encode shared) with
      | Ok t -> t | Error _ -> failwith "portable parsing" in
    check "portable CWT verification" (Result.is_ok
      (C.verify ~key ~allowed_algs:[C.Algorithm.HMAC_256] parsed))
  done : (unit -> unit) @ portable)
let differential () =
  List.iter (fun (algorithm, old_algorithm) ->
    for i = 1 to 500 do
      let secret = String.make (64 + i mod 200) (Char.chr (i mod 256)) in
      let key = C.Cose_key.symmetric secret |> C.Cose_key.with_alg algorithm in
      let old_key = P.Cose_key.symmetric secret in
      let claims = C.Claims.(empty |> set_iss "issuer" |> set_aud ["service"]
        |> set_exp (Option.get (Ptime.of_float_s (2000. +. float_of_int i)))
        |> build) in
      let old_claims = old_ok (P.Claims.of_cbor (C.Claims.to_cbor claims)) in
      let old = old_ok (P.create ~algorithm:old_algorithm
        ~claims:old_claims ~key:old_key) in
      check "CWT verifies pristine HMAC" (Result.is_ok
        (C.verify ~key ~allowed_algs:[algorithm]
          (ok (C.parse (P.encode old)))));
      let created = ok (C.create ~algorithm ~claims ~key) in
      check "pristine verifies CWT HMAC" (Result.is_ok
        (P.verify ~key:old_key ~allowed_algs:[old_algorithm]
          (old_ok (P.parse (C.encode created)))));
      check "canonical HMAC bytes preserved" (C.encode created = P.encode old)
    done)
    [C.Algorithm.HMAC_256_64, P.Algorithm.HMAC_256_64;
     C.Algorithm.HMAC_256, P.Algorithm.HMAC_256;
     C.Algorithm.HMAC_384, P.Algorithm.HMAC_384;
     C.Algorithm.HMAC_512, P.Algorithm.HMAC_512];
  print_endline "CWT HMAC differential: 2000 cases, both directions"
let ec algorithm width make (module D : Mirage_crypto_ec.Dsa) =
  let d = String.make (width - 1) '\000' ^ "\001" in
  let private_key = Result.get_ok (D.priv_of_octets d) in
  let public = D.pub_to_octets (D.pub_of_priv private_key) in
  let x = String.sub public 1 width in
  let y = String.sub public (1 + width) width in
  let key = make ~x ~y ~d in
  check "private/public disagreement rejected" (try
    ignore (make ~x ~y ~d:(String.make (width - 1) '\000' ^ "\002"));
    false with Invalid_argument _ -> true);
  let token = ok (C.create ~algorithm ~key
      ~claims:(C.Claims.build C.Claims.empty)) in
  let key = ok (C.Cose_key.of_cbor (C.Cose_key.to_cbor key)) in
  check "ECDSA signing and key roundtrip" (Result.is_ok
    (C.verify ~key ~allowed_algs:[algorithm] (ok (C.parse (C.encode token)))))
let signing () =
  Mirage_crypto_rng_unix.use_default ();
  ec C.Algorithm.ES256 32 C.Cose_key.p256_priv
    (module Mirage_crypto_ec.P256.Dsa);
  ec C.Algorithm.ES384 48 C.Cose_key.p384_priv
    (module Mirage_crypto_ec.P384.Dsa);
  ec C.Algorithm.ES512 66 C.Cose_key.p521_priv
    (module Mirage_crypto_ec.P521.Dsa);
  let d = String.make 32 '\001' in
  let private_key = Result.get_ok (Mirage_crypto_ec.Ed25519.priv_of_octets d) in
  let pub = Mirage_crypto_ec.Ed25519.pub_of_priv private_key
      |> Mirage_crypto_ec.Ed25519.pub_to_octets in
  let key = C.Cose_key.ed25519_priv ~pub ~priv:d in
  let token = ok (C.create ~key ~algorithm:C.Algorithm.EdDSA
      ~claims:(C.Claims.build C.Claims.empty)) in
  check "Ed25519 signing" (Result.is_ok
    (C.verify ~key:(C.Cose_key.ed25519_pub pub)
      ~allowed_algs:[C.Algorithm.EdDSA] token));
  print_endline "CWT signing: P-256, P-384, P-521 and Ed25519 passed"
let openssl () =
  let raw = In_channel.with_open_bin "cwt-fixtures.json" In_channel.input_all in
  let json = Result.get_ok (Jsont_bytesrw.decode_string Jsont.json raw) in
  let fixtures = match json with
    | Jsont.Array (xs, _) -> xs | _ -> assert false in
  let field name = function Jsont.Object (xs, _) ->
    snd (List.find (fun ((key, _), _) -> key = name) xs) | _ -> assert false in
  let bytes = function Jsont.String (s, _) ->
    Result.get_ok (Jsonwt.base64url_decode s) | _ -> assert false in
  List.iter (fun fixture ->
    let algorithm = match field "alg" fixture with
      | Jsont.Number (n, _) -> ok (C.Algorithm.of_cose_int (int_of_float n))
      | _ -> assert false in
    let key = ok (C.Cose_key.of_cbor (bytes (field "key" fixture))) in
    let raw = bytes (field "token" fixture) in
    let token = ok (C.parse raw) in
    let work = (fun () ->
      for _ = 1 to 25 do
        check "OpenSSL CWT signature" (Result.is_ok
          (C.verify_and_validate ~key ~now ~iss:"fixture" ~aud:"service"
            ~allowed_algs:[algorithm] token))
      done : (unit -> unit) @ portable) in
    let domains = List.init 4 (fun _ -> Domain.Safe.spawn work) in
    List.iter Domain.join domains;
    let bad = Bytes.of_string raw in
    let last = Bytes.length bad - 1 in
    Bytes.set bad last (Char.chr (Char.code (Bytes.get bad last) lxor 1));
    check "OpenSSL fixture tampering rejected" (Result.is_error
      (C.verify ~key ~allowed_algs:[algorithm]
        (ok (C.parse (Bytes.to_string bad)))))) fixtures;
  print_endline "CWT OpenSSL signatures: all four algorithms, four domains"
let () =
  differential (); signing (); openssl ();
  let domains = List.init 4 (fun _ -> Domain.Safe.spawn portable) in
  List.iter Domain.join domains;
  print_endline "CWT portable HMAC parsing and verification passed"
