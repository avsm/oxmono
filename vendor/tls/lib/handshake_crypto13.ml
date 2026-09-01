open Core

let (cdiv @ portable) (x : int) (y : int) =
  if x > 0 && y > 0 then (x + y - 1) / y
  else if x < 0 && y < 0 then (x + y + 1) / y
  else x / y

let (left_pad_dh @ portable) group msg =
  let bytes = cdiv (Mirage_crypto_pk.Dh.modulus_size group) 8 in
  let padding = String.make (bytes - String.length msg) '\x00' in
  padding ^ msg

let (not_all_zero @ portable) r =
  let* str = r in
  try
    for i = 0 to String.length str - 1 do
      if String.unsafe_get str i != '\x00' then raise_notrace Not_found;
    done;
    Error (`Fatal (`Handshake (`BadDH "all zero")))
  with Not_found -> Ok str

let (dh_shared @ portable) secret share =
  (* RFC 8556, Section 7.4.1 - we need zero-padding on the left *)
  let map_ecdh_error =
    Result.map_error (fun e -> `Fatal (`Handshake (`BadECDH e)))
  in
  let open Mirage_crypto_ec in
  not_all_zero
    (match secret with
     | `Finite_field secret ->
       let group = secret.Mirage_crypto_pk.Dh.group in
       let bits = Mirage_crypto_pk.Dh.modulus_size group in
       let* () =
         (* truncated share, better reject this *)
         guard (String.length share = cdiv bits 8)
           (`Fatal (`Handshake (`BadDH "truncated")))
       in
       let* shared =
         Option.to_result
           ~none:(`Fatal (`Handshake (`BadDH "invalid FF")))
           (Mirage_crypto_pk.Dh.shared secret share)
       in
       Ok (left_pad_dh group shared)
     | `P256 priv -> map_ecdh_error (P256.Dh.key_exchange priv share)
     | `P384 priv -> map_ecdh_error (P384.Dh.key_exchange priv share)
     | `P521 priv -> map_ecdh_error (P521.Dh.key_exchange priv share)
     | `X25519 priv -> map_ecdh_error (X25519.key_exchange priv share))

let dh_gen_key group =
  (* RFC 8556, Section 4.2.8.1 - we need zero-padding on the left *)
  match Core.group_to_impl group with
  | `Finite_field mc_group ->
    let sec, shared = Mirage_crypto_pk.Dh.gen_key mc_group in
    `Finite_field sec, left_pad_dh mc_group shared
  | `P256 ->
    let secret, shared = Mirage_crypto_ec.P256.Dh.gen_key () in
    `P256 secret, shared
  | `P384 ->
    let secret, shared = Mirage_crypto_ec.P384.Dh.gen_key () in
    `P384 secret, shared
  | `P521 ->
    let secret, shared = Mirage_crypto_ec.P521.Dh.gen_key () in
    `P521 secret, shared
  | `X25519 ->
    let secret, shared = Mirage_crypto_ec.X25519.gen_key () in
    `X25519 secret, shared

let (dh_gen_key_with_rng @ portable) ~g group =
  match Core.group_to_impl group with
  | `Finite_field mc_group ->
    let sec, shared = Mirage_crypto_pk.Dh.gen_key_with ~g mc_group in
    `Finite_field sec, left_pad_dh mc_group shared
  | `P256 ->
    let secret, shared = Mirage_crypto_ec.P256.Dh.gen_key_with ~g () in
    `P256 secret, shared
  | `P384 ->
    let secret, shared = Mirage_crypto_ec.P384.Dh.gen_key_with ~g () in
    `P384 secret, shared
  | `P521 ->
    let secret, shared = Mirage_crypto_ec.P521.Dh.gen_key_with ~g () in
    `P521 secret, shared
  | `X25519 ->
    let secret, shared = Mirage_crypto_ec.X25519.gen_key_with ~g () in
    `X25519 secret, shared

let (trace @ portable) _tag _data = ()

let (pp_hash_k_n @ portable) ciphersuite =
  let open Ciphersuite in
  let pp = privprot13 ciphersuite
  and hash = hash13 ciphersuite
  in
  let k, n = kn_13 pp in
  (pp, hash, k, n)

let (hkdflabel @ portable) label context length =
  let lbl = "tls13 " ^ label in
  let len_llen = Bytes.create 3 in
  Bytes.set_uint16_be len_llen 0 length;
  Bytes.set_uint8 len_llen 2 (String.length lbl);
  let clen = String.make 1 (Char.unsafe_chr (String.length context)) in
  let lbl = String.concat ""
      [ Bytes.unsafe_to_string len_llen ;
        lbl ;
        clen ;
        context ]
  in
  trace "hkdflabel" lbl ;
  lbl

let (derive_secret_no_hash @ portable) hash prk ?length ?(ctx = "") label =
  let length = match length with
    | None -> Digestif.digest_size hash
    | Some x -> x
  in
  let info = hkdflabel label ctx length in
  trace "prk" prk ;
  let key = Hkdf.expand ~hash ~prk ~info length in
  trace ("derive_secret: " ^ label) key ;
  key

let (derive_secret @ portable) t label log =
  let ctx = Digestif.digest_string_raw t.State.hash log in
  trace "derive secret ctx" ctx ;
  derive_secret_no_hash t.State.hash t.State.secret ~ctx label

let (empty @ portable) cipher = {
  State.secret = "" ;
  cipher ;
  hash = Ciphersuite.hash13 cipher
}

let (derive @ portable) t secret_ikm =
  let salt =
    if String.equal t.State.secret "" then
      ""
    else
      derive_secret t "derived" ""
  in
  trace "derive: secret_ikm" secret_ikm ;
  trace "derive: salt" salt ;
  let secret = Hkdf.extract ~hash:t.State.hash ~salt secret_ikm in
  trace "derive (extracted secret)" secret ;
  { t with State.secret }

let (traffic_key @ portable) cipher prk =
  let _, hash, key_len, iv_len = pp_hash_k_n cipher in
  let key_info = hkdflabel "key" "" key_len in
  let key = Hkdf.expand ~hash ~prk ~info:key_info key_len in
  let iv_info = hkdflabel "iv" "" iv_len in
  let iv = Hkdf.expand ~hash ~prk ~info:iv_info iv_len in
  (key, iv)

let (ctx @ portable) t label secret =
  let secret, nonce = traffic_key t.State.cipher secret in
  trace (label ^ " secret") secret ;
  trace (label ^ " nonce") nonce ;
  let pp = Ciphersuite.privprot13 t.State.cipher in
  { State.sequence = 0L ; cipher_st = Crypto.Ciphers.get_aead_cipher ~secret ~nonce pp }

let (early_traffic @ portable) t log =
  let secret = derive_secret t "c e traffic" log in
  (secret, ctx t "client early traffic" secret)

let (hs_ctx @ portable) t log =
  Tracing.portable_cs ~tag:"hs ctx with sec" t.State.secret ;
  Tracing.portable_cs ~tag:"log is" log ;
  let server_handshake_traffic_secret = derive_secret t "s hs traffic" log
  and client_handshake_traffic_secret = derive_secret t "c hs traffic" log
  in
  (server_handshake_traffic_secret,
   ctx t "server handshake traffic" server_handshake_traffic_secret,
   client_handshake_traffic_secret,
   ctx t "client handshake traffic" client_handshake_traffic_secret)

let (app_ctx @ portable) t log =
  let server_application_traffic_secret = derive_secret t "s ap traffic" log
  and client_application_traffic_secret = derive_secret t "c ap traffic" log
  in
  (server_application_traffic_secret,
   ctx t "server application traffic" server_application_traffic_secret,
   client_application_traffic_secret,
   ctx t "client application traffic" client_application_traffic_secret)

let (app_secret_n_1 @ portable) t app_secret =
  let secret = derive_secret_no_hash t.State.hash app_secret "traffic upd" in
  secret, ctx t "traffic update" secret

let (exporter @ portable) t log = derive_secret t "exp master" log
let (resumption @ portable) t log = derive_secret t "res master" log

let (res_secret @ portable) hash secret nonce =
  derive_secret_no_hash hash secret ~ctx:nonce "resumption"

let (finished @ portable) hash secret data =
  let key = derive_secret_no_hash hash secret "finished" in
  let digest = Digestif.digest_string_raw hash data in
  Digestif.hmacv_string_raw hash ~key [ digest ]
