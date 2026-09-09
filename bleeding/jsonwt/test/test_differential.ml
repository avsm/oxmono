(* These standalone tests own their domains and do not use a thread pool. *)
[@@@alert "-do_not_spawn_domains"]

module J = Jsonwt
module P = Jsonwt_pristine
let ok = function Ok x -> x | Error e -> failwith (J.error_to_string e)
let old_ok = function Ok x -> x | Error e -> failwith (P.error_to_string e)
let check name b = if not b then failwith name

let hmac () =
  List.iter (fun (alg, old_alg) ->
    for i = 1 to 500 do
      let secret = String.make (64 + i mod 200) (Char.chr (i mod 256)) in
      let key = J.Jwk.symmetric secret |> J.Jwk.with_alg alg in
      let old_key = P.Jwk.symmetric secret in
      let payload = Printf.sprintf
        {|{"iss":"issuer","aud":"service","exp":2000,"n":%d,"s":"%s"}|}
        i (String.make (i mod 150) 'x') in
      let old = old_ok (P.create ~header:(P.Header.make old_alg)
          ~claims:(old_ok (P.Claims.of_json payload)) ~key:old_key) in
      let parsed = ok (J.parse (P.encode old)) in
      check "preserve pristine compact serialization"
        (J.encode parsed = P.encode old);
      check "verify pristine HMAC"
        (Result.is_ok (J.verify ~key ~allowed_algs:[alg] parsed));
      check "preserve claims" (J.Claims.get_int "n" (J.claims parsed) = Some i);
      let created = ok (J.create ~header:(J.Header.make alg)
          ~claims:(ok (J.Claims.of_json payload)) ~key ()) in
      let old_parsed = old_ok (P.parse (J.encode created)) in
      check "pristine verifies new HMAC"
        (Result.is_ok (P.verify ~key:old_key
          ~allowed_algs:[old_alg] old_parsed));
      check "both validate the same claims"
        (Result.is_ok (J.validate ~now:Ptime.epoch
           ~iss:"issuer" ~aud:"service" parsed)
         = Result.is_ok (P.validate ~now:Ptime.epoch
           ~iss:"issuer" ~aud:"service" old))
    done) [J.Algorithm.HS256, P.Algorithm.HS256;
            J.Algorithm.HS384, P.Algorithm.HS384;
            J.Algorithm.HS512, P.Algorithm.HS512];
  Printf.printf "HMAC differential: 1500 cases, both signing directions\n"

let ec alg width constructor (module D : Mirage_crypto_ec.Dsa) =
  let d = String.make (width - 1) '\000' ^ "\001" in
  let private_key = Result.get_ok (D.priv_of_octets d) in
  let public = D.pub_to_octets (D.pub_of_priv private_key) in
  let key = constructor ~x:(String.sub public 1 width)
      ~y:(String.sub public (1 + width) width) ~d in
  check "ECDSA private/public disagreement rejected"
    (try
       ignore (constructor ~x:(String.sub public 1 width)
         ~y:(String.sub public (1 + width) width)
         ~d:(String.make (width - 1) '\000' ^ "\002"));
       false
     with Invalid_argument _ -> true);
  let claims = ok (J.Claims.of_json {|{"message":"signed by JSONWT"}|}) in
  let token = ok (J.create ~header:(J.Header.make alg) ~claims ~key ()) in
  check "ECDSA signing roundtrip"
    (Result.is_ok (J.verify ~key ~allowed_algs:[alg] token));
  let decoded = ok (J.parse (J.encode token)) in
  check "ECDSA parsed signing roundtrip"
    (Result.is_ok (J.verify ~key ~allowed_algs:[alg] decoded));
  check "private JWK roundtrip" (Result.is_ok (J.verify
    ~key:(ok (J.Jwk.of_json (J.Jwk.to_json key))) ~allowed_algs:[alg] decoded))

let signing () =
  Mirage_crypto_rng_unix.use_default ();
  ec J.Algorithm.ES256 32 J.Jwk.p256_priv (module Mirage_crypto_ec.P256.Dsa);
  ec J.Algorithm.ES384 48 J.Jwk.p384_priv (module Mirage_crypto_ec.P384.Dsa);
  ec J.Algorithm.ES512 66 J.Jwk.p521_priv (module Mirage_crypto_ec.P521.Dsa);
  let d = String.make 32 '\001' in
  let priv = Result.get_ok (Mirage_crypto_ec.Ed25519.priv_of_octets d) in
  let pub = Mirage_crypto_ec.Ed25519.pub_of_priv priv
      |> Mirage_crypto_ec.Ed25519.pub_to_octets in
  let key = J.Jwk.ed25519_priv ~pub ~priv:d in
  check "Ed25519 private/public disagreement rejected"
    (try ignore (J.Jwk.ed25519_priv ~pub ~priv:(String.make 32 '\002'));
       false
     with Invalid_argument _ -> true);
  let token = ok (J.create ~header:(J.Header.make J.Algorithm.EdDSA)
      ~claims:(J.Claims.build J.Claims.empty) ~key ()) in
  check "Ed25519 signing roundtrip"
    (Result.is_ok (J.verify ~key:(J.Jwk.ed25519_pub pub)
      ~allowed_algs:[J.Algorithm.EdDSA] token));
  Printf.printf "Signing: P-256, P-384, P-521 and Ed25519 passed\n"

(* Capturing these module-level values proves their immutable_data kinds,
   while the closure proves the imported parsing and verification modes. *)
let shared_key = J.Jwk.symmetric (String.make 64 'k')
    |> J.Jwk.with_alg J.Algorithm.HS256
let shared_token = ok (J.create ~header:(J.Header.make J.Algorithm.HS256)
    ~claims:(J.Claims.build J.Claims.empty) ~key:shared_key ())
let portable_verify = (fun () ->
  for _ = 1 to 100 do
    let parsed = match J.parse (J.encode shared_token) with
      | Ok t -> t | Error _ -> failwith "portable parse failed" in
    check "portable verification" (Result.is_ok
      (J.verify ~key:shared_key ~allowed_algs:[J.Algorithm.HS256] parsed))
  done : (unit -> unit) @ portable)

let allocation () =
  let raw = J.encode shared_token in
  let old = old_ok (P.parse raw) in
  let old_key = P.Jwk.symmetric (String.make 64 'k') in
  let measure f =
    let before = Gc.allocated_bytes () in
    for _ = 1 to 1000 do f () done;
    (Gc.allocated_bytes () -. before) /. 1000. in
  let old_bytes = measure (fun () ->
    ignore (P.verify ~key:old_key ~allowed_algs:[P.Algorithm.HS256] old)) in
  let new_bytes = measure (fun () ->
    ignore (J.verify ~key:shared_key
      ~allowed_algs:[J.Algorithm.HS256] shared_token)) in
  Printf.printf "OCaml heap bytes/HMAC verification: pristine %.0f, port %.0f\n"
    old_bytes new_bytes

let () =
  hmac ();
  signing ();
  let domains = List.init 4 (fun _ -> Domain.Safe.spawn portable_verify) in
  List.iter Domain.join domains;
  Printf.printf "Portable verification: four domains passed\n";
  allocation ()
