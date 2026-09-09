(*---------------------------------------------------------------------------
  Copyright (c) 2025 Anil Madhavapeddy <anil@recoil.org>. All rights reserved.
  SPDX-License-Identifier: ISC
 ---------------------------------------------------------------------------*)

(** CBOR Web Token (CWT) - RFC 8392 *)

(* Error handling *)

type error =
  | Invalid_cbor of string
  | Invalid_cose of string
  | Invalid_claims of string
  | Unsupported_algorithm of string
  | Algorithm_not_allowed of string
  | Signature_mismatch
  | Token_expired
  | Token_not_yet_valid
  | Invalid_issuer
  | Invalid_audience
  | Key_type_mismatch of string

let pp_error ppf = function
  | Invalid_cbor s -> Format.fprintf ppf "Invalid CBOR: %s" s
  | Invalid_cose s -> Format.fprintf ppf "Invalid COSE: %s" s
  | Invalid_claims s -> Format.fprintf ppf "Invalid claims: %s" s
  | Unsupported_algorithm s -> Format.fprintf ppf "Unsupported algorithm: %s" s
  | Algorithm_not_allowed s -> Format.fprintf ppf "Algorithm not allowed: %s" s
  | Signature_mismatch -> Format.fprintf ppf "Signature mismatch"
  | Token_expired -> Format.fprintf ppf "Token expired"
  | Token_not_yet_valid -> Format.fprintf ppf "Token not yet valid"
  | Invalid_issuer -> Format.fprintf ppf "Invalid issuer"
  | Invalid_audience -> Format.fprintf ppf "Invalid audience"
  | Key_type_mismatch s -> Format.fprintf ppf "Key type mismatch: %s" s

let error_to_string e = Format.asprintf "%a" pp_error e

(* Cbort codec helpers *)

let ( let* ) = Result.bind

module B = Cbort.Cbor

let decode_cbor bytes =
  Result.map_error (fun message -> Invalid_cbor message) (Cwt_cbor.decode bytes)

let encode_cbor = Cwt_cbor.encode
let int n = B.Int (Z.of_int n)

let number = function
  | B.Int n -> Some (Z.to_float n)
  | B.Float n -> Some n
  | _ -> None

let native_int = function
  | B.Int n when Z.fits_int n -> Ok (Z.to_int n)
  | _ -> Error (Invalid_cose "expected an in-range integer")

let pairs = function
  | B.Map pairs -> Ok pairs
  | _ -> Error (Invalid_cose "expected a CBOR map")

let get label pairs = List.assoc_opt (int label) pairs

let optional_bytes label pairs =
  match get label pairs with
  | None -> Ok None
  | Some (B.Bytes s) -> Ok (Some s)
  | Some _ -> Error (Invalid_cose "expected a byte string")

let required_bytes label pairs =
  let* value = optional_bytes label pairs in
  match value with
  | Some s -> Ok s
  | None -> Error (Invalid_cose "missing key parameter")

let required_int label pairs =
  match get label pairs with
  | Some value -> native_int value
  | None -> Error (Invalid_cose "missing integer parameter")

(* COSE Algorithms - RFC 9053 *)

module Algorithm = struct
  type t =
    | ES256
    | ES384
    | ES512
    | EdDSA
    | HMAC_256_64
    | HMAC_256
    | HMAC_384
    | HMAC_512

  let to_cose_int = function
    | ES256 -> -7
    | ES384 -> -35
    | ES512 -> -36
    | EdDSA -> -8
    | HMAC_256_64 -> 4
    | HMAC_256 -> 5
    | HMAC_384 -> 6
    | HMAC_512 -> 7

  let of_cose_int = function
    | -7 -> Ok ES256
    | -35 -> Ok ES384
    | -36 -> Ok ES512
    | -8 -> Ok EdDSA
    | 4 -> Ok HMAC_256_64
    | 5 -> Ok HMAC_256
    | 6 -> Ok HMAC_384
    | 7 -> Ok HMAC_512
    | n -> Error (Unsupported_algorithm (Printf.sprintf "COSE algorithm %d" n))

  let to_string = function
    | ES256 -> "ES256"
    | ES384 -> "ES384"
    | ES512 -> "ES512"
    | EdDSA -> "EdDSA"
    | HMAC_256_64 -> "HMAC 256/64"
    | HMAC_256 -> "HMAC 256/256"
    | HMAC_384 -> "HMAC 384/384"
    | HMAC_512 -> "HMAC 512/512"

  let all =
    [ ES256; ES384; ES512; EdDSA; HMAC_256_64; HMAC_256; HMAC_384; HMAC_512 ]
end

(* COSE Key - RFC 9052 Section 7 *)

module Cose_key = struct
  type kty = Okp | Ec2 | Symmetric

  (* COSE key labels *)
  let label_kty = 1
  let label_kid = 2
  let label_alg = 3
  let label_crv = -1
  let label_x = -2
  let label_y = -3
  let label_d = -4
  let label_k = -1 (* for symmetric *)

  (* COSE key type values *)
  let kty_okp = 1
  let kty_ec2 = 2
  let kty_symmetric = 4

  (* COSE curve values *)
  let crv_p256 = 1
  let crv_p384 = 2
  let crv_p521 = 3
  let crv_ed25519 = 6

  type key_data =
    | Symmetric_key of { k : string }
    | Ed25519_pub of { x : string }
    | Ed25519_priv of { x : string; d : string }
    | P256_pub of { x : string; y : string }
    | P256_priv of { x : string; y : string; d : string }
    | P384_pub of { x : string; y : string }
    | P384_priv of { x : string; y : string; d : string }
    | P521_pub of { x : string; y : string }
    | P521_priv of { x : string; y : string; d : string }

  type t = {
    key_data : key_data;
    kid : string option;
    alg : Algorithm.t option;
    key_ops : int list option;
  }

  let symmetric k =
    { key_data = Symmetric_key { k }; kid = None; alg = None; key_ops = None }

  let ed25519_pub x =
    ignore (Jsonwt.Jwk.ed25519_pub x);
    {
      key_data = Ed25519_pub { x };
      kid = None;
      alg = Some Algorithm.EdDSA;
      key_ops = None;
    }

  let ed25519_priv ~pub ~priv =
    ignore (Jsonwt.Jwk.ed25519_priv ~pub ~priv);
    {
      key_data = Ed25519_priv { x = pub; d = priv };
      kid = None;
      alg = Some Algorithm.EdDSA;
      key_ops = None;
    }

  let p256_pub ~x ~y =
    ignore (Jsonwt.Jwk.p256_pub ~x ~y);
    {
      key_data = P256_pub { x; y };
      kid = None;
      alg = Some Algorithm.ES256;
      key_ops = None;
    }

  let p256_priv ~x ~y ~d =
    ignore (Jsonwt.Jwk.p256_priv ~x ~y ~d);
    {
      key_data = P256_priv { x; y; d };
      kid = None;
      alg = Some Algorithm.ES256;
      key_ops = None;
    }

  let p384_pub ~x ~y =
    ignore (Jsonwt.Jwk.p384_pub ~x ~y);
    {
      key_data = P384_pub { x; y };
      kid = None;
      alg = Some Algorithm.ES384;
      key_ops = None;
    }

  let p384_priv ~x ~y ~d =
    ignore (Jsonwt.Jwk.p384_priv ~x ~y ~d);
    {
      key_data = P384_priv { x; y; d };
      kid = None;
      alg = Some Algorithm.ES384;
      key_ops = None;
    }

  let p521_pub ~x ~y =
    ignore (Jsonwt.Jwk.p521_pub ~x ~y);
    {
      key_data = P521_pub { x; y };
      kid = None;
      alg = Some Algorithm.ES512;
      key_ops = None;
    }

  let p521_priv ~x ~y ~d =
    ignore (Jsonwt.Jwk.p521_priv ~x ~y ~d);
    {
      key_data = P521_priv { x; y; d };
      kid = None;
      alg = Some Algorithm.ES512;
      key_ops = None;
    }

  let kty t =
    match t.key_data with
    | Symmetric_key _ -> Symmetric
    | Ed25519_pub _ | Ed25519_priv _ -> Okp
    | P256_pub _ | P256_priv _ | P384_pub _ | P384_priv _ | P521_pub _
    | P521_priv _ ->
        Ec2

  let kid t = t.kid
  let alg t = t.alg
  let with_kid id t = { t with kid = Some id }

  let compatible alg t =
    match (alg, t.key_data) with
    | (Algorithm.HMAC_256_64 | HMAC_256 | HMAC_384 | HMAC_512), Symmetric_key _
      ->
        true
    | Algorithm.EdDSA, (Ed25519_pub _ | Ed25519_priv _) -> true
    | Algorithm.ES256, (P256_pub _ | P256_priv _) -> true
    | Algorithm.ES384, (P384_pub _ | P384_priv _) -> true
    | Algorithm.ES512, (P521_pub _ | P521_priv _) -> true
    | _ -> false

  let with_alg a t =
    if not (compatible a t) then invalid_arg "CWT key algorithm";
    { t with alg = Some a }

  (* COSE key_ops values from RFC 9052, section 7.1. *)
  let sign = 1
  let verify = 2
  let mac_create = 9
  let mac_verify = 10

  let permits operation t =
    match t.key_ops with None -> true | Some ops -> List.mem operation ops

  (* Helper to build CBOR map pairs *)
  let int_key k = Cbort.Cbor.Int (Z.of_int k)

  (* CBOR encoding/decoding for COSE keys *)
  let of_cbor bytes =
    let* cbor = decode_cbor bytes in
    let* pairs = pairs cbor in
    let* kind = required_int label_kty pairs in
    let* kid = optional_bytes label_kid pairs in
    let* () =
      if get 5 pairs = None then Ok ()
      else Error (Invalid_cose "Base IV is unsupported by these algorithms")
    in
    let* alg =
      match get label_alg pairs with
      | None -> Ok None
      | Some value ->
          let* n = native_int value in
          Result.map Option.some (Algorithm.of_cose_int n)
    in
    let* key_ops =
      match get 4 pairs with
      | None -> Ok None
      | Some (B.Array []) -> Error (Invalid_cose "key_ops must be nonempty")
      | Some (B.Array values) ->
          let rec loop acc = function
            | [] -> Ok (Some (List.rev acc))
            | value :: rest ->
                let* n = native_int value in
                if
                  List.mem n acc
                  || not (List.mem n [ sign; verify; mac_create; mac_verify ])
                then Error (Invalid_cose "invalid key_ops")
                else loop (n :: acc) rest
          in
          loop [] values
      | Some _ -> Error (Invalid_cose "key_ops must be an array")
    in
    try
      let* key =
        if kind = kty_symmetric then
          let* k = required_bytes label_k pairs in
          Ok (symmetric k)
        else
          let* curve = required_int label_crv pairs in
          let* x = required_bytes label_x pairs in
          let* d = optional_bytes label_d pairs in
          if kind = kty_okp && curve = crv_ed25519 then
            Ok
              (match d with
              | None -> ed25519_pub x
              | Some priv -> ed25519_priv ~pub:x ~priv)
          else if kind = kty_ec2 then
            let* y = required_bytes label_y pairs in
            match (curve, d) with
            | 1, None -> Ok (p256_pub ~x ~y)
            | 1, Some d -> Ok (p256_priv ~x ~y ~d)
            | 2, None -> Ok (p384_pub ~x ~y)
            | 2, Some d -> Ok (p384_priv ~x ~y ~d)
            | 3, None -> Ok (p521_pub ~x ~y)
            | 3, Some d -> Ok (p521_priv ~x ~y ~d)
            | _ -> Error (Invalid_cose "unsupported curve")
          else Error (Invalid_cose "unsupported key type or curve")
      in
      let key = match alg with None -> key | Some a -> with_alg a key in
      let permitted =
        match key.key_data with
        | Symmetric_key _ -> [ mac_create; mac_verify ]
        | _ -> [ sign; verify ]
      in
      if
        not
          (Option.fold ~none:true
             ~some:(List.for_all (fun op -> List.mem op permitted))
             key_ops)
      then Error (Invalid_cose "key_ops does not match key type")
      else Ok { key with kid; key_ops }
    with Invalid_argument message -> Error (Key_type_mismatch message)

  let to_cbor t =
    let pairs = ref [] in
    let add k v = pairs := (int_key k, v) :: !pairs in
    let add_bytes k s = add k (Cbort.Cbor.Bytes s) in
    let add_int k i = add k (Cbort.Cbor.Int (Z.of_int i)) in

    (* kty - always present *)
    (match t.key_data with
    | Symmetric_key _ -> add_int label_kty kty_symmetric
    | Ed25519_pub _ | Ed25519_priv _ -> add_int label_kty kty_okp
    | _ -> add_int label_kty kty_ec2);

    (* kid (optional) *)
    Option.iter (fun kid -> add_bytes label_kid kid) t.kid;

    (* alg (optional) *)
    Option.iter
      (fun alg -> add_int label_alg (Algorithm.to_cose_int alg)) t.alg;

    Option.iter
      (fun ops -> add 4 (B.Array (List.map (fun n -> B.Int (Z.of_int n)) ops)))
      t.key_ops;

    (* Key-type specific parameters *)
    (match t.key_data with
    | Symmetric_key { k } -> add_bytes label_k k
    | Ed25519_pub { x } ->
        add_int label_crv crv_ed25519;
        add_bytes label_x x
    | Ed25519_priv { x; d } ->
        add_int label_crv crv_ed25519;
        add_bytes label_x x;
        add_bytes label_d d
    | P256_pub { x; y } ->
        add_int label_crv crv_p256;
        add_bytes label_x x;
        add_bytes label_y y
    | P256_priv { x; y; d } ->
        add_int label_crv crv_p256;
        add_bytes label_x x;
        add_bytes label_y y;
        add_bytes label_d d
    | P384_pub { x; y } ->
        add_int label_crv crv_p384;
        add_bytes label_x x;
        add_bytes label_y y
    | P384_priv { x; y; d } ->
        add_int label_crv crv_p384;
        add_bytes label_x x;
        add_bytes label_y y;
        add_bytes label_d d
    | P521_pub { x; y } ->
        add_int label_crv crv_p521;
        add_bytes label_x x;
        add_bytes label_y y
    | P521_priv { x; y; d } ->
        add_int label_crv crv_p521;
        add_bytes label_x x;
        add_bytes label_y y;
        add_bytes label_d d);

    encode_cbor (Cbort.Cbor.Map (List.rev !pairs))
end

(* CWT Claims - RFC 8392 Section 3 *)

module Claims = struct
  type t = (B.t * B.t) list
  type builder = t

  let get_int_key key claims = get key claims
  let get_string_key key claims = List.assoc_opt (B.Text key) claims

  let string key claims =
    match get key claims with Some (B.Text s) -> Some s | _ -> None

  let iss claims = string 1 claims
  let sub claims = string 2 claims

  let aud claims =
    match get 3 claims with
    | None -> []
    | Some (B.Text s) -> [ s ]
    | Some (B.Array xs) ->
        List.map (function B.Text s -> s | _ -> assert false) xs
    | _ -> assert false

  let numeric key claims = Option.bind (get key claims) number
  let date key claims = Option.bind (numeric key claims) Ptime.of_float_s
  let exp claims = date 4 claims
  let nbf claims = date 5 claims
  let iat claims = date 6 claims

  let cti claims =
    match get 7 claims with Some (B.Bytes s) -> Some s | _ -> None

  let uri s =
    if not (String.contains s ':') then Ok ()
    else
      match Uriz.of_string__local s with
      | This u when Uriz.scheme u <> Null -> Ok ()
      | _ -> Error (Invalid_claims "invalid StringOrURI")

  let validate claims =
    let rec loop = function
      | [] -> Ok claims
      | (key, value) :: rest ->
          let* () =
            match (key, value) with
            | B.Int key, value
              when Z.compare key Z.one >= 0 && Z.compare key (Z.of_int 7) <= 0
              -> (
                match (Z.to_int key, value) with
                | (1 | 2), B.Text s -> uri s
                | 3, B.Text s -> uri s
                | 3, B.Array values ->
                    let rec audiences = function
                      | [] -> Ok ()
                      | B.Text s :: rest ->
                          let* () = uri s in
                          audiences rest
                      | _ -> Error (Invalid_claims "invalid audience")
                    in
                    audiences values
                | (4 | 5 | 6), value -> (
                    match number value with
                    | Some n
                      when Float.is_finite n && Ptime.of_float_s n <> None ->
                        Ok ()
                    | _ -> Error (Invalid_claims "invalid NumericDate"))
                | 7, B.Bytes _ -> Ok ()
                | _ -> Error (Invalid_claims "invalid registered claim"))
            | B.Int n, _ when not (Z.fits_int n) ->
                Error
                  (Invalid_claims "claim label outside native integer range")
            | B.Int _, _ | B.Text _, _ -> Ok ()
            | _ -> Error (Invalid_claims "invalid claim label")
          in
          loop rest
    in
    loop claims

  let of_cbor bytes =
    let* cbor = decode_cbor bytes in
    let* claims = pairs cbor in
    validate claims

  let to_cbor claims = encode_cbor (B.Map claims)
  let empty = []

  let set key value claims =
    List.filter (fun (k, _) -> k <> key) claims @ [ (key, value) ]

  let set_int_key key value claims = set (int key) value claims
  let set_string_key key value claims = set (B.Text key) value claims
  let set_iss value claims = set_int_key 1 (B.Text value) claims
  let set_sub value claims = set_int_key 2 (B.Text value) claims

  let set_aud values claims =
    let value =
      match values with
      | [ s ] -> B.Text s
      | xs -> B.Array (List.map (fun s -> B.Text s) xs)
    in
    set_int_key 3 value claims

  let set_date key value claims =
    let n = Ptime.to_float_s value in
    let value = if n = Float.floor n then B.Int (Z.of_float n) else B.Float n in
    set_int_key key value claims

  let set_exp value claims = set_date 4 value claims
  let set_nbf value claims = set_date 5 value claims
  let set_iat value claims = set_date 6 value claims
  let set_cti value claims = set_int_key 7 (B.Bytes value) claims

  let build claims =
    match of_cbor (to_cbor claims) with
    | Ok claims -> claims
    | Error error -> invalid_arg (error_to_string error)
end

type kind = Sign1 | Mac0

let kind_of_algorithm = function
  | Algorithm.HMAC_256_64 | HMAC_256 | HMAC_384 | HMAC_512 -> Mac0
  | _ -> Sign1

let context = function Sign1 -> "Signature1" | Mac0 -> "MAC0"
let cose_sign1_tag = 18
let cose_mac0_tag = 17
let header_alg = 1

let unsupported_headers =
  [
    (2, "crit");
    (7, "counter signature");
    (9, "counter signature 0");
    (11, "counter signature V2");
    (12, "counter signature 0 V2");
  ]

let validate_headers headers =
  let* () =
    match get 3 headers with
    | None | Some (B.Text _) -> Ok ()
    | Some (B.Int n) when Z.sign n >= 0 -> Ok ()
    | Some _ -> Error (Invalid_cose "invalid content type")
  in
  if get 5 headers <> None || get 6 headers <> None then
    Error (Invalid_cose "IV headers are unsupported by these algorithms")
  else Ok ()

type t = {
  claims : Claims.t;
  algorithm : Algorithm.t;
  kid : string option;
  kind : kind;
  protected_header : string;
  payload : string;
  signature : string;
  raw : string;
}

let claims t = t.claims
let algorithm t = Some t.algorithm
let kid t = t.kid
let raw t = t.raw

let parse ?(max_size = 8192) bytes =
  let* cbor =
    Result.map_error (fun s -> Invalid_cbor s) (Cwt_cbor.decode ~max_size bytes)
  in
  let* cbor =
    match cbor with
    | B.Tag (61, (B.Tag ((17 | 18), _) as value)) -> Ok value
    | B.Tag (61, _) -> Error (Invalid_cose "CWT tag requires a COSE tag")
    | value -> Ok value
  in
  let* tagged_kind, body =
    match cbor with
    | B.Tag (17, body) -> Ok (Some Mac0, body)
    | B.Tag (18, body) -> Ok (Some Sign1, body)
    | B.Array _ -> Ok (None, cbor)
    | _ -> Error (Invalid_cose "expected COSE_Sign1 or COSE_Mac0")
  in
  match body with
  | B.Array
      [
        B.Bytes protected_header;
        B.Map unprotected;
        B.Bytes payload;
        B.Bytes signature;
      ] ->
      let* protected =
        if protected_header = "" then Ok []
        else
          let* cbor = decode_cbor protected_header in
          pairs cbor
      in
      let* () =
        if List.exists (fun (k, _) -> List.mem_assoc k unprotected) protected
        then Error (Invalid_cose "header label in both buckets")
        else if
          List.exists
            (fun (label, _) ->
              get label protected <> None || get label unprotected <> None)
            unsupported_headers
        then
          Error
            (Invalid_cose "critical or countersignature headers unsupported")
        else Ok ()
      in
      let* () = validate_headers protected in
      let* () = validate_headers unprotected in
      let* alg = required_int header_alg protected in
      let* algorithm = Algorithm.of_cose_int alg in
      let kind = kind_of_algorithm algorithm in
      let* () =
        if tagged_kind <> None && tagged_kind <> Some kind then
          Error (Invalid_cose "COSE tag and algorithm disagree")
        else Ok ()
      in
      let* protected_kid = optional_bytes 4 protected in
      let* unprotected_kid = optional_bytes 4 unprotected in
      let kid =
        match protected_kid with
        | Some _ -> protected_kid
        | None -> unprotected_kid
      in
      let* claims = Claims.of_cbor payload in
      Ok
        {
          claims;
          algorithm;
          kid;
          kind;
          protected_header;
          payload;
          signature;
          raw = bytes;
        }
  | _ -> Error (Invalid_cose "expected four correctly typed COSE fields")

(* Cryptographic operations *)

external hmac : int -> string -> string -> int -> string
  @@ portable = "jsonwt_hmac"

let hmac_sign alg key payload =
  let len = String.length payload in
  match alg with
  | Algorithm.HMAC_256_64 -> Ok (String.sub (hmac 256 key payload len) 0 8)
  | Algorithm.HMAC_256 -> Ok (hmac 256 key payload len)
  | Algorithm.HMAC_384 -> Ok (hmac 384 key payload len)
  | Algorithm.HMAC_512 -> Ok (hmac 512 key payload len)
  | _ -> Error (Key_type_mismatch "not an HMAC algorithm")

let hmac_verify alg key payload expected_mac =
  match hmac_sign alg key payload with
  | Error _ -> false
  | Ok computed -> Eqaf.equal computed expected_mac

let p256_sign ~priv payload =
  match Mirage_crypto_ec.P256.Dsa.priv_of_octets priv with
  | Error _ -> Error (Key_type_mismatch "Invalid P-256 private key")
  | Ok priv ->
      let hash = Digestif.SHA256.(digest_string payload |> to_raw_string) in
      let r, s = Mirage_crypto_ec.P256.Dsa.sign ~key:priv hash in
      let pad32 s =
        let len = String.length s in
        if len >= 32 then String.sub s (len - 32) 32
        else String.make (32 - len) '\x00' ^ s
      in
      Ok (pad32 r ^ pad32 s)

let p384_sign ~priv payload =
  match Mirage_crypto_ec.P384.Dsa.priv_of_octets priv with
  | Error _ -> Error (Key_type_mismatch "Invalid P-384 private key")
  | Ok priv ->
      let hash = Digestif.SHA384.(digest_string payload |> to_raw_string) in
      let r, s = Mirage_crypto_ec.P384.Dsa.sign ~key:priv hash in
      let pad48 s =
        let len = String.length s in
        if len >= 48 then String.sub s (len - 48) 48
        else String.make (48 - len) '\x00' ^ s
      in
      Ok (pad48 r ^ pad48 s)

let p521_sign ~priv payload =
  match Mirage_crypto_ec.P521.Dsa.priv_of_octets priv with
  | Error _ -> Error (Key_type_mismatch "Invalid P-521 private key")
  | Ok priv ->
      let hash = Digestif.SHA512.(digest_string payload |> to_raw_string) in
      let r, s = Mirage_crypto_ec.P521.Dsa.sign ~key:priv hash in
      let pad66 s =
        let len = String.length s in
        if len >= 66 then String.sub s (len - 66) 66
        else String.make (66 - len) '\x00' ^ s
      in
      Ok (pad66 r ^ pad66 s)

let ed25519_sign ~priv payload =
  match Mirage_crypto_ec.Ed25519.priv_of_octets priv with
  | Error _ -> Error (Key_type_mismatch "Invalid Ed25519 private key")
  | Ok priv -> Ok (Mirage_crypto_ec.Ed25519.sign ~key:priv payload)

(** Build Sig_structure or MAC_structure for COSE operations *)
let build_sig_structure ~context_string ~protected_header ~payload =
  let open Cbort.Cbor in
  Array
    [
      Text context_string;
      Bytes protected_header;
      Bytes "";
      (* external_aad = empty *)
      Bytes payload;
    ]
  |> encode_cbor

(** Expected signature/MAC length for each algorithm *)
let expected_sig_length = function
  | Algorithm.ES256 -> 64 (* 32 + 32 *)
  | Algorithm.ES384 -> 96 (* 48 + 48 *)
  | Algorithm.ES512 -> 132 (* 66 + 66 *)
  | Algorithm.EdDSA -> 64
  | Algorithm.HMAC_256_64 -> 8
  | Algorithm.HMAC_256 -> 32
  | Algorithm.HMAC_384 -> 48
  | Algorithm.HMAC_512 -> 64

let key_policy ~key ~operation algorithm =
  if
    (not (Cose_key.compatible algorithm key))
    || key.Cose_key.alg <> Some algorithm
    || not (Cose_key.permits operation key)
  then Error (Key_type_mismatch "key algorithm or operation is not permitted")
  else
    match key.Cose_key.key_data with
    | Cose_key.Symmetric_key { k } ->
        let minimum =
          match algorithm with
          | Algorithm.HMAC_256_64 | HMAC_256 -> 32
          | HMAC_384 -> 48
          | HMAC_512 -> 64
          | _ -> assert false
        in
        if String.length k < minimum then
          Error (Key_type_mismatch "HMAC key is too short")
        else Ok ()
    | _ -> Ok ()

let verify ~key ~allowed_algs t =
  let alg = t.algorithm in
  let* () =
    if List.mem alg allowed_algs then Ok ()
    else Error (Algorithm_not_allowed (Algorithm.to_string alg))
  in
  let operation =
    match t.kind with Mac0 -> Cose_key.mac_verify | Sign1 -> Cose_key.verify
  in
  let* () = key_policy ~key ~operation alg in
  if String.length t.signature <> expected_sig_length alg then
    Error Signature_mismatch
  else
    let sig_structure =
      build_sig_structure ~context_string:(context t.kind)
        ~protected_header:t.protected_header ~payload:t.payload
    in
    let verify_result =
      match (alg, key.Cose_key.key_data) with
      | ( ( Algorithm.HMAC_256_64 | Algorithm.HMAC_256 | Algorithm.HMAC_384
          | Algorithm.HMAC_512 ),
          Cose_key.Symmetric_key { k } ) ->
          if hmac_verify alg k sig_structure t.signature then Ok ()
          else Error Signature_mismatch
      | ( Algorithm.EdDSA,
          (Cose_key.Ed25519_pub { x } | Cose_key.Ed25519_priv { x; _ }) ) -> (
          match Mirage_crypto_ec.Ed25519.pub_of_octets x with
          | Ok pub ->
              if
                Mirage_crypto_ec.Ed25519.verify ~key:pub t.signature
                  ~msg:sig_structure
              then Ok ()
              else Error Signature_mismatch
          | Error _ -> Error (Key_type_mismatch "Invalid Ed25519 public key"))
      | ( Algorithm.ES256,
          (Cose_key.P256_pub { x; y } | Cose_key.P256_priv { x; y; _ }) ) -> (
          match Mirage_crypto_ec.P256.Dsa.pub_of_octets ("\x04" ^ x ^ y) with
          | Ok pub ->
              let hash =
                Digestif.SHA256.(digest_string sig_structure |> to_raw_string)
              in
              let r = String.sub t.signature 0 32 in
              let s = String.sub t.signature 32 32 in
              if Mirage_crypto_ec.P256.Dsa.verify ~key:pub (r, s) hash then
                Ok ()
              else Error Signature_mismatch
          | Error _ -> Error (Key_type_mismatch "Invalid P-256 public key"))
      | ( Algorithm.ES384,
          (Cose_key.P384_pub { x; y } | Cose_key.P384_priv { x; y; _ }) ) -> (
          match Mirage_crypto_ec.P384.Dsa.pub_of_octets ("\x04" ^ x ^ y) with
          | Ok pub ->
              let hash =
                Digestif.SHA384.(digest_string sig_structure |> to_raw_string)
              in
              let r = String.sub t.signature 0 48 in
              let s = String.sub t.signature 48 48 in
              if Mirage_crypto_ec.P384.Dsa.verify ~key:pub (r, s) hash then
                Ok ()
              else Error Signature_mismatch
          | Error _ -> Error (Key_type_mismatch "Invalid P-384 public key"))
      | ( Algorithm.ES512,
          (Cose_key.P521_pub { x; y } | Cose_key.P521_priv { x; y; _ }) ) -> (
          match Mirage_crypto_ec.P521.Dsa.pub_of_octets ("\x04" ^ x ^ y) with
          | Ok pub ->
              let hash =
                Digestif.SHA512.(digest_string sig_structure |> to_raw_string)
              in
              let r = String.sub t.signature 0 66 in
              let s = String.sub t.signature 66 66 in
              if Mirage_crypto_ec.P521.Dsa.verify ~key:pub (r, s) hash then
                Ok ()
              else Error Signature_mismatch
          | Error _ -> Error (Key_type_mismatch "Invalid P-521 public key"))
      | _ ->
          Error
            (Key_type_mismatch
               (Printf.sprintf "Key type doesn't match algorithm %s"
                  (Algorithm.to_string alg)))
    in
    verify_result

let leeway_seconds leeway =
  if Ptime.Span.compare leeway Ptime.Span.zero < 0 then
    invalid_arg "CWT leeway must be nonnegative";
  Ptime.Span.to_float_s leeway

let validate ~now ?iss ?aud ?(leeway = Ptime.Span.zero) t =
  let now = Ptime.to_float_s now and leeway = leeway_seconds leeway in
  let* () =
    match Claims.numeric 4 t.claims with
    | Some exp when now -. leeway >= exp -> Error Token_expired
    | _ -> Ok ()
  in
  let* () =
    match Claims.numeric 5 t.claims with
    | Some nbf when now +. leeway < nbf -> Error Token_not_yet_valid
    | _ -> Ok ()
  in
  let* () =
    match iss with
    | Some iss when Claims.iss t.claims <> Some iss -> Error Invalid_issuer
    | _ -> Ok ()
  in
  match aud with
  | Some aud when not (List.mem aud (Claims.aud t.claims)) ->
      Error Invalid_audience
  | _ -> Ok ()

let verify_and_validate ~key ~now ~allowed_algs ?iss ?aud ?leeway t =
  let* () = verify ~key ~allowed_algs t in
  validate ~now ?iss ?aud ?leeway t

(** Encode protected header as CBOR map *)
let encode_protected_header algorithm kid =
  let open Cbort.Cbor in
  Map
    ([
       ( Int (Z.of_int header_alg),
         Int (Z.of_int (Algorithm.to_cose_int algorithm)) );
     ]
    @ match kid with None -> [] | Some kid -> [ (int 4, Bytes kid) ])
  |> encode_cbor

(** Encode COSE_Sign1 or COSE_Mac0 structure *)
let encode_cose_message ~cose_tag ~protected_header ~payload ~signature =
  Cbort.Cbor.Tag
    ( cose_tag,
      Cbort.Cbor.Array
        [
          Cbort.Cbor.Bytes protected_header;
          Cbort.Cbor.Map [];
          (* unprotected header - empty *)
          Cbort.Cbor.Bytes payload;
          Cbort.Cbor.Bytes signature;
        ] )
  |> encode_cbor

let create ~algorithm ~claims ~key =
  try
    let kind = kind_of_algorithm algorithm in
    let operation =
      match kind with Mac0 -> Cose_key.mac_create | Sign1 -> Cose_key.sign
    in
    let* () = key_policy ~key ~operation algorithm in
    (* Encode protected header *)
    let protected_header = encode_protected_header algorithm key.Cose_key.kid in

    (* Build Sig_structure or MAC_structure *)
    let context_string = context kind in
    let payload = Claims.to_cbor claims in
    let to_be_signed =
      build_sig_structure ~context_string ~protected_header ~payload
    in

    (* Sign or MAC *)
    let signature_result =
      match (algorithm, key.Cose_key.key_data) with
      | ( ( Algorithm.HMAC_256_64 | Algorithm.HMAC_256 | Algorithm.HMAC_384
          | Algorithm.HMAC_512 ),
          Cose_key.Symmetric_key { k } ) ->
          hmac_sign algorithm k to_be_signed
      | Algorithm.EdDSA, Cose_key.Ed25519_priv { d; _ } ->
          ed25519_sign ~priv:d to_be_signed
      | Algorithm.ES256, Cose_key.P256_priv { d; _ } ->
          p256_sign ~priv:d to_be_signed
      | Algorithm.ES384, Cose_key.P384_priv { d; _ } ->
          p384_sign ~priv:d to_be_signed
      | Algorithm.ES512, Cose_key.P521_priv { d; _ } ->
          p521_sign ~priv:d to_be_signed
      | _ -> Error (Key_type_mismatch "Key type doesn't match algorithm")
    in

    match signature_result with
    | Error e -> Error e
    | Ok signature ->
        (* Encode COSE_Sign1 or COSE_Mac0 structure *)
        let cose_tag =
          match kind with Mac0 -> cose_mac0_tag | Sign1 -> cose_sign1_tag
        in
        let raw =
          encode_cose_message ~cose_tag ~protected_header ~payload ~signature
        in
        parse ~max_size:(String.length raw) raw
  with Invalid_argument message -> Error (Invalid_cose message)

let encode t = t.raw

let is_expired ~now ?(leeway = Ptime.Span.zero) t =
  let leeway = leeway_seconds leeway in
  match Claims.numeric 4 t.claims with
  | None -> false
  | Some exp -> Ptime.to_float_s now -. leeway >= exp

let time_to_expiry ~now t =
  match Claims.exp t.claims with
  | None -> None
  | Some exp ->
      let diff = Ptime.diff exp now in
      if Ptime.Span.compare diff Ptime.Span.zero <= 0 then None else Some diff
