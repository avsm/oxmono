let ( let* ) = Result.bind

type ecdsa = [
  | `P256 of Mirage_crypto_ec.P256.Dsa.pub
  | `P384 of Mirage_crypto_ec.P384.Dsa.pub
  | `P521 of Mirage_crypto_ec.P521.Dsa.pub
]

type t = [
  | ecdsa
  | `RSA of Mirage_crypto_pk.Rsa.pub
  | `ED25519 of Mirage_crypto_ec.Ed25519.pub
]

module Asn_oid = Asn.OID

module Asn = struct
  open Asn_grammars
  open Asn.S
  open Mirage_crypto_pk

  let rsa_public_key =
    let f (n, e) =
      let n = Z_extra.of_octets_be n
      and e = Z_extra.of_octets_be e in
      match Rsa.pub ~e ~n with
      | Ok p -> p
      | Error (`Msg m) -> parse_error "bad RSA public key %s" m
    and g ({ Rsa.n; e } : Rsa.pub) = (Z_extra.to_octets_be n, Z_extra.to_octets_be e) in
    map f g @@
    sequence2
      (required ~label:"modulus"        unsigned_integer)
      (required ~label:"publicExponent" unsigned_integer)

  let (rsa_public_of_octets, rsa_public_to_octets) =
    projections_of Asn.der rsa_public_key

  let rsa_pub_of_octets : _ @ portable = project_exn_decoder rsa_public_key
  let _, rsa_pub_to_octets = project_exn rsa_public_key

  let to_err : _ @ portable = function
    | Ok r -> r
    | Error _ -> parse_error "failed to decode public EC key"

  let reparse_pk =
    let open Mirage_crypto_ec in
    let open Algorithm in
    function
    | (RSA      , cs) -> `RSA (rsa_pub_of_octets cs)
    | (ED25519  , cs) -> `ED25519 (to_err (Ed25519.pub_of_octets cs))
    | (EC_pub `SECP256R1, cs) -> `P256 (to_err (P256.Dsa.pub_of_octets cs))
    | (EC_pub `SECP384R1, cs) -> `P384 (to_err (P384.Dsa.pub_of_octets cs))
    | (EC_pub `SECP521R1, cs) -> `P521 (to_err (P521.Dsa.pub_of_octets cs))
    | _ -> parse_error "unknown public key algorithm"

  let unparse_pk =
    let open Mirage_crypto_ec in
    let open Algorithm in
    function
    | `RSA pk    -> (RSA, rsa_pub_to_octets pk)
    | `ED25519 pk -> (ED25519, Ed25519.pub_to_octets pk)
    | `P256 pk -> (EC_pub `SECP256R1, P256.Dsa.pub_to_octets pk)
    | `P384 pk -> (EC_pub `SECP384R1, P384.Dsa.pub_to_octets pk)
    | `P521 pk -> (EC_pub `SECP521R1, P521.Dsa.pub_to_octets pk)

  let pk_info_der =
    map reparse_pk unparse_pk @@
    sequence2
      (required ~label:"algorithm" Algorithm.identifier)
      (required ~label:"subjectPK" bit_string_octets)

  let (pub_info_of_octets, pub_info_to_octets) =
    projections_of Asn.der pk_info_der
end

let id k =
  let data = match k with
    | `RSA p -> Asn.rsa_public_to_octets p
    | `ED25519 pk -> Mirage_crypto_ec.Ed25519.pub_to_octets pk
    | `P256 pk -> Mirage_crypto_ec.P256.Dsa.pub_to_octets pk
    | `P384 pk -> Mirage_crypto_ec.P384.Dsa.pub_to_octets pk
    | `P521 pk -> Mirage_crypto_ec.P521.Dsa.pub_to_octets pk
  in
  Digestif.(to_raw_string SHA1 (digest_string SHA1 data))

let fingerprint ?(hash = `SHA256) pub =
  let module Hash = (val (Digestif.module_of_hash' (hash :> Digestif.hash'))) in
  Hash.(to_raw_string (digest_string (Asn.pub_info_to_octets pub)))

let key_type = function
  | `RSA _ -> `RSA
  | `ED25519 _ -> `ED25519
  | `P256 _ -> `P256
  | `P384 _ -> `P384
  | `P521 _ -> `P521

let sig_alg = function
  | #ecdsa -> `ECDSA
  | `RSA _ -> `RSA
  | `ED25519 _ -> `ED25519

let pp ppf k =
  Fmt.string ppf (Key_type.to_string (key_type k));
  Fmt.sp ppf ();
  Ohex.pp ppf (fingerprint k)

let hashed : _ @ portable = fun hash data ->
  match data with
  | `Message msg -> Ok (Digestif.digest_string_raw hash msg)
  | `Digest d ->
    let n = String.length d in
    let m =
      match hash with
      | `MD5 -> 16
      | `SHA1 -> 20
      | `RMD160 -> 20
      | `SHA224 -> 28
      | `SHA256 -> 32
      | `SHA384 -> 48
      | `SHA512 -> 64
      | `BLAKE2B -> 64
      | `BLAKE2S -> 32
    in
    if n = m then Ok d else Error (`Msg "digested data of invalid size")

let trunc : _ @ portable = fun len data ->
  if String.length data > len then
    String.sub data 0 len
  else
    data

(* ECDSA signatures in certificates are a tiny, fixed DER grammar: a sequence
   of two positive integers. Keeping this decoder here avoids capturing the
   general ASN.1 codec graph in the portable verification closure. *)
let der_length : _ @ portable = fun data off limit ->
  if off >= limit then Error (`Msg "truncated DER length")
  else
    let first = String.get_uint8 data off in
    if first < 0x80 then Ok (first, off + 1)
    else
      let octets = first land 0x7f in
      if octets = 0 then Error (`Msg "indefinite DER length")
      else if octets > (Sys.int_size - 1) / 8 || off + 1 + octets > limit then
        Error (`Msg "invalid DER length")
      else if String.get_uint8 data (off + 1) = 0 then
        Error (`Msg "non-minimal DER length")
      else
        let rec collect value pos left =
          if left = 0 then Ok (value, pos)
          else
            let byte = String.get_uint8 data pos in
            if value > (max_int - byte) / 256 then
              Error (`Msg "DER length overflow")
            else collect ((value * 256) + byte) (pos + 1) (left - 1)
        in
        match collect 0 (off + 1) octets with
        | Error _ as error -> error
        | Ok (length, next) when length < 0x80 ->
            Error (`Msg "non-minimal DER length")
        | Ok pair -> Ok pair

let der_positive_integer : _ @ portable = fun data off limit ->
  if off >= limit || String.get_uint8 data off <> 0x02 then
    Error (`Msg "expected DER INTEGER")
  else
    match der_length data (off + 1) limit with
    | Error _ as error -> error
    | Ok (length, start) ->
        if length = 0 || length > limit - start then
          Error (`Msg "invalid DER INTEGER length")
        else
          let first = String.get_uint8 data start in
          if first land 0x80 <> 0 then Error (`Msg "negative ECDSA scalar")
          else
            let skip =
              if first = 0 then
                if length = 1 || String.get_uint8 data (start + 1) land 0x80 = 0
                then -1
                else 1
              else 0
            in
            if skip < 0 then Error (`Msg "non-minimal ECDSA scalar")
            else
              Ok
                ( String.sub data (start + skip) (length - skip),
                  start + length )

let ecdsa_signature_of_der : _ @ portable = fun data ->
  let limit = String.length data in
  if limit = 0 || String.get_uint8 data 0 <> 0x30 then
    Error (`Msg "expected DER SEQUENCE")
  else
    match der_length data 1 limit with
    | Error _ as error -> error
    | Ok (length, start) ->
        if length <> limit - start then Error (`Msg "invalid DER SEQUENCE length")
        else
          match der_positive_integer data start limit with
          | Error _ as error -> error
          | Ok (r, next) ->
              match der_positive_integer data next limit with
              | Error _ as error -> error
              | Ok (s, finish) when finish = limit -> Ok (r, s)
              | Ok _ -> Error (`Msg "trailing ECDSA signature data")

let verify : _ @ portable = fun hash ?scheme ~signature key data ->
  let open Mirage_crypto_ec in
  let ok_if_true p = if p then Ok () else Error (`Msg "bad signature") in
  let scheme = Key_type.opt_signature_scheme ?scheme (key_type key) in
  match key, scheme with
  | `RSA key, `RSA_PSS ->
    let* d = hashed hash data in
    begin match hash with
      | (`MD5 | `SHA1 | `SHA224 | `SHA256 | `SHA384 | `SHA512) as hash ->
        ok_if_true (Mirage_crypto_pk.Rsa.pss_verify_digest
                      ~hash ~key ~signature d)
      | _ -> Error (`Msg "unsupported RSA hash algorithm")
    end
  | `RSA key, `RSA_PKCS1 ->
    let* d = hashed hash data in
    begin match hash with
      | (`MD5 | `SHA1 | `SHA224 | `SHA256 | `SHA384 | `SHA512) as hash ->
        ok_if_true (Mirage_crypto_pk.Rsa.pkcs1_verify_digest
                      ~hash ~key ~signature d)
      | _ -> Error (`Msg "unsupported RSA hash algorithm")
    end
  | `ED25519 key, `ED25519 ->
    begin match data with
      | `Message msg -> ok_if_true (Ed25519.verify ~key signature ~msg)
      | `Digest _ -> Error (`Msg "Ed25519 only suitable with raw message")
    end
  | #ecdsa as key, `ECDSA ->
    let* d = hashed hash data in
    let* s = ecdsa_signature_of_der signature in
    ok_if_true
      (match key with
       | `P256 key -> P256.Dsa.verify ~key s (trunc P256.Dsa.byte_length d)
       | `P384 key -> P384.Dsa.verify ~key s (trunc P384.Dsa.byte_length d)
       | `P521 key -> P521.Dsa.verify ~key s (trunc P521.Dsa.byte_length d))
  | _ -> Error (`Msg "invalid key and signature scheme combination")

let encode_der = Asn.pub_info_to_octets

let decode_der cs = Asn_grammars.err_to_msg (Asn.pub_info_of_octets cs)

let decode_pem cs =
  let* data = Pem.parse cs in
  let pks = List.filter (fun (t, _) -> String.equal "PUBLIC KEY" t) data in
  let* keys = Pem.foldM (fun (_, k) -> decode_der k) pks in
  Pem.exactly_one ~what:"public key" keys

let encode_pem v =
  Pem.unparse ~tag:"PUBLIC KEY" (encode_der v)
