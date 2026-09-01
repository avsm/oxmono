open State

let (halve @ portable) secret =
  let size = String.length secret in
  let half = size - size / 2 in
  String.(sub secret 0 half, sub secret (size - half) half)

let rec (p_hash @ portable) hash key seed len =
  let rec expand a to_go =
    let res = Digestif.hmacv_string_raw hash ~key [ a; seed ] in
    let digest_size = Digestif.digest_size hash in
    if to_go > digest_size then
      res ^ expand (Digestif.hmacv_string_raw hash ~key [ a ])
        (to_go - digest_size)
    else String.sub res 0 to_go
  in
  expand (Digestif.hmacv_string_raw hash ~key [ seed ]) len

let (prf_hash @ portable) = function
  | `RSA_WITH_AES_256_GCM_SHA384
  | `DHE_RSA_WITH_AES_256_GCM_SHA384
  | `ECDHE_RSA_WITH_AES_256_GCM_SHA384
  | `ECDHE_RSA_WITH_AES_256_CBC_SHA384
  | `ECDHE_ECDSA_WITH_AES_256_CBC_SHA384
  | `ECDHE_ECDSA_WITH_AES_256_GCM_SHA384 -> `SHA384
  | _ -> `SHA256

let (xor_strings @ portable) left right =
  let length = min (String.length left) (String.length right) in
  let result = Bytes.create length in
  for index = 0 to length - 1 do
    Bytes.set_uint8 result index
      (String.get_uint8 left index lxor String.get_uint8 right index)
  done;
  Bytes.unsafe_to_string result

let (pseudo_random_function @ portable) version cipher len secret label seed =
  let labelled = label ^ seed in
  match version with
  | `TLS_1_1 | `TLS_1_0 ->
     let (s1, s2) = halve secret in
     let md5 = p_hash `MD5 s1 labelled len
     and sha = p_hash `SHA1 s2 labelled len in
     xor_strings md5 sha
  | `TLS_1_2 ->
     p_hash (prf_hash cipher) secret labelled len

let (key_block @ portable) version cipher len master_secret seed =
  pseudo_random_function version cipher len master_secret "key expansion" seed

let (hash @ portable) version cipher data =
  match version with
  | `TLS_1_0 | `TLS_1_1 ->
      Digestif.digest_string_raw `MD5 data
      ^ Digestif.digest_string_raw `SHA1 data
  | `TLS_1_2 -> Digestif.digest_string_raw (prf_hash cipher) data

let (finished @ portable) version cipher master_secret label ps =
  let data = String.concat "" ps in
  let seed = hash version cipher data in
  pseudo_random_function version cipher 12 master_secret label seed

let (divide_keyblock @ portable) key mac iv buf =
  let c_mac, rt0 = Core.split_str buf mac in
  let s_mac, rt1 = Core.split_str rt0 mac in
  let c_key, rt2 = Core.split_str rt1 key in
  let s_key, rt3 = Core.split_str rt2 key in
  let c_iv , s_iv = Core.split_str rt3 iv
  in
  (c_mac, s_mac, c_key, s_key, c_iv, s_iv)

let (derive_master_secret @ portable) version (session : session_data)
    premaster log =
  let prf = pseudo_random_function version session.ciphersuite 48 premaster in
  if session.extended_ms then
    let session_hash =
      let data = String.concat "" log in
      hash version session.ciphersuite data
    in
    prf "extended master secret" session_hash
  else
    prf "master secret" (session.common_session_data.client_random ^ session.common_session_data.server_random)

let initialise_crypto_ctx version (session : session_data) =
  let open Ciphersuite in
  let client_random = session.common_session_data.client_random
  and server_random = session.common_session_data.server_random
  and master = session.common_session_data.master_secret
  and cipher = session.ciphersuite
  in

  let pp = ciphersuite_privprot cipher in

  let c_mac, s_mac, c_key, s_key, c_iv, s_iv =
    let iv_l = match version with
      | `TLS_1_0 -> Some ()
      | _ -> None
    in
    let key_len, iv_len, mac_len = Ciphersuite.key_length iv_l pp in
    let kblen = 2 * key_len + 2 * mac_len + 2 * iv_len
    and rand = server_random ^ client_random
    in
    let keyblock = key_block version cipher kblen master rand in
    divide_keyblock key_len mac_len iv_len keyblock
  in

  let context cipher_k iv mac_k =
    let open Crypto.Ciphers in
    let cipher_st =
      let iv_mode = match version with
        | `TLS_1_0 -> Iv iv
        | _ -> Random_iv
      in
      get_cipher ~secret:cipher_k ~hmac_secret:mac_k ~iv_mode ~nonce:iv pp
    and sequence = 0L in
    { cipher_st ; sequence }
  in

  let c_context = context c_key c_iv c_mac
  and s_context = context s_key s_iv s_mac in

  (c_context, s_context)

let (initialise_crypto_ctx_client @ portable) version
    (session : session_data) =
  let open Ciphersuite in
  let client_random = session.common_session_data.client_random in
  let server_random = session.common_session_data.server_random in
  let master = session.common_session_data.master_secret in
  let cipher = session.ciphersuite in
  let protection = ciphersuite_privprot cipher in
  let aead =
    match protection with
    | `AEAD cipher -> cipher
    | `Block _ -> invalid_arg "portable TLS clients require AEAD cipher suites"
  in
  let key_length, iv_length, mac_length =
    Ciphersuite.key_length None protection
  in
  let keyblock_length =
    (2 * key_length) + (2 * mac_length) + (2 * iv_length)
  in
  let keyblock =
    key_block version cipher keyblock_length master
      (server_random ^ client_random)
  in
  let client_mac, server_mac, client_key, server_key, client_iv, server_iv =
    divide_keyblock key_length mac_length iv_length keyblock
  in
  let _ = client_mac, server_mac in
  let context key nonce =
    let cipher_st = Crypto.Ciphers.get_aead_cipher ~secret:key ~nonce aead in
    { cipher_st; sequence = 0L }
  in
  context client_key client_iv, context server_key server_iv
