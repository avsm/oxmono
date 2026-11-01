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

let cbort_error_to_error e = Invalid_cbor (Cbort.Error.to_string e)

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
  }

  let symmetric k = { key_data = Symmetric_key { k }; kid = None; alg = None }

  let ed25519_pub x =
    { key_data = Ed25519_pub { x }; kid = None; alg = Some Algorithm.EdDSA }

  let ed25519_priv ~pub ~priv =
    {
      key_data = Ed25519_priv { x = pub; d = priv };
      kid = None;
      alg = Some Algorithm.EdDSA;
    }

  let p256_pub ~x ~y =
    { key_data = P256_pub { x; y }; kid = None; alg = Some Algorithm.ES256 }

  let p256_priv ~x ~y ~d =
    { key_data = P256_priv { x; y; d }; kid = None; alg = Some Algorithm.ES256 }

  let p384_pub ~x ~y =
    { key_data = P384_pub { x; y }; kid = None; alg = Some Algorithm.ES384 }

  let p384_priv ~x ~y ~d =
    { key_data = P384_priv { x; y; d }; kid = None; alg = Some Algorithm.ES384 }

  let p521_pub ~x ~y =
    { key_data = P521_pub { x; y }; kid = None; alg = Some Algorithm.ES512 }

  let p521_priv ~x ~y ~d =
    { key_data = P521_priv { x; y; d }; kid = None; alg = Some Algorithm.ES512 }

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
  let with_alg a t = { t with alg = Some a }

  (* Helper to build CBOR map pairs *)
  let int_key k = Cbort.Cbor.Int (Z.of_int k)

  (* CBOR encoding/decoding for COSE keys *)
  let of_cbor bytes =
    match Cbort.decode_string Cbort.any bytes with
    | Error e -> Error (cbort_error_to_error e)
    | Ok cbor ->
        let find_int key = Cbort.Cbor.find (int_key key) cbor in
        let find_bytes key =
          match find_int key with
          | Some (Cbort.Cbor.Bytes s) -> Some s
          | _ -> None
        in
        (* kid can be Text or Bytes per RFC 9052 *)
        let find_kid key =
          match find_int key with
          | Some (Cbort.Cbor.Bytes s) -> Some s
          | Some (Cbort.Cbor.Text s) -> Some s
          | _ -> None
        in
        let get_int_value = function
          | Some (Cbort.Cbor.Int z) -> Some (Z.to_int z)
          | _ -> None
        in
        let kty_val = get_int_value (find_int label_kty) in
        let crv_val = get_int_value (find_int label_crv) in
        let kid = find_kid label_kid in
        let alg =
          match get_int_value (find_int label_alg) with
          | None -> None
          | Some n -> (
              match Algorithm.of_cose_int n with
              | Ok a -> Some a
              | Error _ -> None)
        in
        let x = find_bytes label_x in
        let y = find_bytes label_y in
        let d = find_bytes label_d in
        let k = find_bytes label_k in
        let key_data =
          match (kty_val, crv_val, x, y, d, k) with
          | Some 4, _, _, _, _, Some k -> Ok (Symmetric_key { k })
          | Some 1, Some 6, Some x, _, None, _ -> Ok (Ed25519_pub { x })
          | Some 1, Some 6, Some x, _, Some d, _ -> Ok (Ed25519_priv { x; d })
          | Some 2, Some 1, Some x, Some y, None, _ -> Ok (P256_pub { x; y })
          | Some 2, Some 1, Some x, Some y, Some d, _ ->
              Ok (P256_priv { x; y; d })
          | Some 2, Some 2, Some x, Some y, None, _ -> Ok (P384_pub { x; y })
          | Some 2, Some 2, Some x, Some y, Some d, _ ->
              Ok (P384_priv { x; y; d })
          | Some 2, Some 3, Some x, Some y, None, _ -> Ok (P521_pub { x; y })
          | Some 2, Some 3, Some x, Some y, Some d, _ ->
              Ok (P521_priv { x; y; d })
          | _ ->
              Error (Invalid_cose "unsupported or invalid COSE key structure")
        in
        Result.map (fun key_data -> { key_data; kid; alg }) key_data

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
    Option.iter (fun alg -> add_int label_alg (Algorithm.to_cose_int alg)) t.alg;

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

    Cbort.encode_string Cbort.any (Cbort.Cbor.Map (List.rev !pairs))
end

(* CWT Claims - RFC 8392 Section 3 *)

module Claims = struct
  (* Claim keys (integers per RFC 8392) *)
  let key_iss = 1
  let key_sub = 2
  let key_aud = 3
  let key_exp = 4
  let key_nbf = 5
  let key_iat = 6
  let key_cti = 7

  type claim_key = Int_key of int | String_key of string

  type t = {
    iss : string option;
    sub : string option;
    aud : string list;
    exp : Ptime.t option;
    nbf : Ptime.t option;
    iat : Ptime.t option;
    cti : string option;
    custom : (claim_key * Cbort.Cbor.t) list;
  }

  let iss t = t.iss
  let sub t = t.sub
  let aud t = t.aud
  let exp t = t.exp
  let nbf t = t.nbf
  let iat t = t.iat
  let cti t = t.cti

  let get_int_key key t =
    List.find_map
      (function Int_key k, v when k = key -> Some v | _ -> None)
      t.custom

  let get_string_key key t =
    List.find_map
      (function String_key k, v when k = key -> Some v | _ -> None)
      t.custom

  type builder = t

  let empty =
    {
      iss = None;
      sub = None;
      aud = [];
      exp = None;
      nbf = None;
      iat = None;
      cti = None;
      custom = [];
    }

  let set_iss v t = { t with iss = Some v }
  let set_sub v t = { t with sub = Some v }
  let set_aud v t = { t with aud = v }
  let set_exp v t = { t with exp = Some v }
  let set_nbf v t = { t with nbf = Some v }
  let set_iat v t = { t with iat = Some v }
  let set_cti v t = { t with cti = Some v }

  let set_int_key key value t =
    { t with custom = (Int_key key, value) :: t.custom }

  let set_string_key key value t =
    { t with custom = (String_key key, value) :: t.custom }

  let build t = t

  (* Standard claim keys *)
  let standard_keys =
    [ key_iss; key_sub; key_aud; key_exp; key_nbf; key_iat; key_cti ]

  (* Helper to convert claim_key to CBOR *)
  let claim_key_to_cbor = function
    | Int_key i -> Cbort.Cbor.Int (Z.of_int i)
    | String_key s -> Cbort.Cbor.Text s

  (* Helper to find value by integer key in CBOR map *)
  let find_int_key key pairs =
    let target = Cbort.Cbor.Int (Z.of_int key) in
    List.find_map
      (fun (k, v) -> if Cbort.Cbor.equal k target then Some v else None)
      pairs

  (* Helper to extract string from CBOR *)
  let cbor_to_string = function Cbort.Cbor.Text s -> Some s | _ -> None

  (* Helper to extract bytes from CBOR *)
  let cbor_to_bytes = function Cbort.Cbor.Bytes s -> Some s | _ -> None

  (* Helper to extract ptime from CBOR integer *)
  let cbor_to_ptime = function
    | Cbort.Cbor.Int z -> Ptime.of_float_s (Z.to_float z)
    | _ -> None

  (* Helper to extract audience (string or array of strings) *)
  let cbor_to_aud = function
    | Cbort.Cbor.Text s -> Some [ s ]
    | Cbort.Cbor.Array items ->
        let strings = List.filter_map cbor_to_string items in
        if List.length strings = List.length items then Some strings else None
    | _ -> None

  (* Decode claims from CBOR map pairs *)
  let decode_from_pairs pairs =
    let iss = Option.bind (find_int_key key_iss pairs) cbor_to_string in
    let sub = Option.bind (find_int_key key_sub pairs) cbor_to_string in
    let aud =
      Option.value ~default:[]
        (Option.bind (find_int_key key_aud pairs) cbor_to_aud)
    in
    let exp = Option.bind (find_int_key key_exp pairs) cbor_to_ptime in
    let nbf = Option.bind (find_int_key key_nbf pairs) cbor_to_ptime in
    let iat = Option.bind (find_int_key key_iat pairs) cbor_to_ptime in
    let cti = Option.bind (find_int_key key_cti pairs) cbor_to_bytes in
    (* Collect custom claims (non-standard keys) *)
    let custom =
      List.filter_map
        (fun (k, v) ->
          match k with
          | Cbort.Cbor.Int z ->
              let i = Z.to_int z in
              if List.mem i standard_keys then None else Some (Int_key i, v)
          | Cbort.Cbor.Text s -> Some (String_key s, v)
          | _ -> None)
        pairs
    in
    { iss; sub; aud; exp; nbf; iat; cti; custom }

  (* Encode claims to CBOR map pairs *)
  let encode_to_pairs t =
    let open Cbort.Cbor in
    let pairs = ref [] in
    let add_int k v = pairs := (Int (Z.of_int k), v) :: !pairs in
    (* Standard claims *)
    Option.iter (fun v -> add_int key_iss (Text v)) t.iss;
    Option.iter (fun v -> add_int key_sub (Text v)) t.sub;
    (match t.aud with
    | [] -> ()
    | [ s ] -> add_int key_aud (Text s)
    | lst -> add_int key_aud (Array (List.map (fun s -> Text s) lst)));
    Option.iter
      (fun v -> add_int key_exp (Int (Z.of_float (Ptime.to_float_s v))))
      t.exp;
    Option.iter
      (fun v -> add_int key_nbf (Int (Z.of_float (Ptime.to_float_s v))))
      t.nbf;
    Option.iter
      (fun v -> add_int key_iat (Int (Z.of_float (Ptime.to_float_s v))))
      t.iat;
    Option.iter (fun v -> add_int key_cti (Bytes v)) t.cti;
    (* Custom claims *)
    List.iter
      (fun (k, v) -> pairs := (claim_key_to_cbor k, v) :: !pairs)
      t.custom;
    List.rev !pairs

  let claims_not_map_error = "claims must be a CBOR map"

  (** Full codec for claims including custom claims *)
  let codec : t Cbort.t =
    Cbort.conv
      (fun cbor ->
        match cbor with
        | Cbort.Cbor.Map pairs -> Ok (decode_from_pairs pairs)
        | _ -> Error claims_not_map_error)
      (fun t -> Cbort.Cbor.Map (encode_to_pairs t))
      Cbort.any

  let of_cbor bytes =
    match Cbort.decode_string codec bytes with
    | Ok t -> Ok t
    | Error e ->
        (* Distinguish CBOR parse errors from claims structure errors *)
        let msg = Cbort.Error.to_string e in
        if msg = claims_not_map_error then Error (Invalid_claims msg)
        else Error (Invalid_cbor msg)

  let to_cbor t = Cbort.encode_string codec t
end

(* CWT Token *)

(* COSE tags *)
let cose_sign1_tag = 18
let cose_mac0_tag = 17

(* COSE header labels *)
let header_alg = 1
let header_kid = 4

type t = {
  claims : Claims.t;
  algorithm : Algorithm.t option;
  kid : string option;
  protected_header : string; (* CBOR-encoded protected header *)
  signature : string; (* Signature or MAC tag *)
  raw : string; (* Original CBOR bytes *)
}

let claims t = t.claims
let algorithm t = t.algorithm
let kid t = t.kid
let raw t = t.raw

(** Extract kid from header - can be Text or Bytes per RFC 9052 *)
let extract_kid_from_header pairs =
  let kid_key = Cbort.Cbor.Int (Z.of_int header_kid) in
  List.find_map
    (fun (k, v) ->
      if Cbort.Cbor.equal k kid_key then
        match v with
        | Cbort.Cbor.Bytes s -> Some s
        | Cbort.Cbor.Text s -> Some s
        | _ -> None
      else None)
    pairs

(** Decode protected header to extract algorithm and kid *)
let decode_protected_header bytes =
  match Cbort.decode_string Cbort.any bytes with
  | Error _ -> (None, None)
  | Ok (Cbort.Cbor.Map pairs) ->
      let alg_key = Cbort.Cbor.Int (Z.of_int header_alg) in
      let alg_int =
        List.find_map
          (fun (k, v) ->
            if Cbort.Cbor.equal k alg_key then
              match v with Cbort.Cbor.Int z -> Some (Z.to_int z) | _ -> None
            else None)
          pairs
      in
      let algorithm =
        Option.bind alg_int (fun n ->
            match Algorithm.of_cose_int n with
            | Ok alg -> Some alg
            | Error _ -> None)
      in
      let kid = extract_kid_from_header pairs in
      (algorithm, kid)
  | Ok _ -> (None, None)

(** Extract kid from unprotected header if present *)
let decode_unprotected_header cbor =
  match cbor with
  | Cbort.Cbor.Map pairs -> extract_kid_from_header pairs
  | _ -> None

let parse bytes =
  match Cbort.decode_string Cbort.any bytes with
  | Error e -> Error (cbort_error_to_error e)
  | Ok cbor -> (
      (* Handle optional COSE tag and extract the array *)
      let cose_array =
        match cbor with
        | Cbort.Cbor.Tag (18, arr) -> Some arr (* COSE_Sign1 *)
        | Cbort.Cbor.Tag (17, arr) -> Some arr (* COSE_Mac0 *)
        | Cbort.Cbor.Array _ as arr -> Some arr (* Untagged *)
        | _ -> None
      in
      match cose_array with
      | None ->
          Error (Invalid_cose "expected COSE_Sign1 or COSE_Mac0 structure")
      | Some
          (Cbort.Cbor.Array
             [ protected_bstr; unprotected; payload_bstr; sig_bstr ]) -> (
          (* Extract byte strings *)
          let protected_header =
            match protected_bstr with Cbort.Cbor.Bytes s -> Some s | _ -> None
          in
          let signature =
            match sig_bstr with Cbort.Cbor.Bytes s -> Some s | _ -> None
          in
          match (protected_header, signature) with
          | Some protected_header, Some signature -> (
              (* Decode protected header for algorithm and kid *)
              let algorithm, protected_kid =
                decode_protected_header protected_header
              in
              (* Decode unprotected header for kid - prefer unprotected over protected *)
              let unprotected_kid = decode_unprotected_header unprotected in
              let kid =
                match unprotected_kid with
                | Some _ -> unprotected_kid
                | None -> protected_kid
              in
              (* Decode claims from payload - handle detached payloads *)
              match payload_bstr with
              | Cbort.Cbor.Null ->
                  (* Detached payload: not currently supported *)
                  Error (Invalid_cose "detached payloads are not supported")
              | Cbort.Cbor.Bytes payload -> (
                  match Claims.of_cbor payload with
                  | Error e -> Error e
                  | Ok claims ->
                      Ok
                        {
                          claims;
                          algorithm;
                          kid;
                          protected_header;
                          signature;
                          raw = bytes;
                        })
              | _ ->
                  Error (Invalid_cose "payload must be a byte string or null"))
          | _ -> Error (Invalid_cose "invalid COSE structure fields"))
      | Some (Cbort.Cbor.Array _) ->
          Error (Invalid_cose "COSE structure must have exactly 4 elements")
      | Some _ -> Error (Invalid_cose "expected COSE array structure"))

(* Cryptographic operations *)

let hmac_sign alg key payload =
  let module Hash = Digestif in
  match alg with
  | Algorithm.HMAC_256_64 ->
      let mac = Hash.SHA256.hmac_string ~key payload in
      Ok (String.sub (Hash.SHA256.to_raw_string mac) 0 8)
  | Algorithm.HMAC_256 ->
      let mac = Hash.SHA256.hmac_string ~key payload in
      Ok (Hash.SHA256.to_raw_string mac)
  | Algorithm.HMAC_384 ->
      let mac = Hash.SHA384.hmac_string ~key payload in
      Ok (Hash.SHA384.to_raw_string mac)
  | Algorithm.HMAC_512 ->
      let mac = Hash.SHA512.hmac_string ~key payload in
      Ok (Hash.SHA512.to_raw_string mac)
  | _ -> Error (Key_type_mismatch "Not an HMAC algorithm")

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
  |> Cbort.encode_string Cbort.any

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

let verify ~key ?allowed_algs t =
  (* Check algorithm is allowed *)
  let alg =
    match t.algorithm with
    | None -> Error (Invalid_cose "No algorithm in protected header")
    | Some a -> Ok a
  in
  match alg with
  | Error e -> Error e
  | Ok alg ->
      let allowed =
        match allowed_algs with None -> Algorithm.all | Some l -> l
      in
      if not (List.mem alg allowed) then
        Error (Algorithm_not_allowed (Algorithm.to_string alg))
      else
        (* Validate signature length before attempting to parse it *)
        let expected_len = expected_sig_length alg in
        let actual_len = String.length t.signature in
        if actual_len <> expected_len then
          Error
            (Invalid_cose
               (Printf.sprintf "signature length mismatch: expected %d, got %d"
                  expected_len actual_len))
        else
          (* Build Sig_structure or MAC_structure for verification *)
          let context_string =
            match alg with
            | Algorithm.HMAC_256_64 | Algorithm.HMAC_256 | Algorithm.HMAC_384
            | Algorithm.HMAC_512 ->
                "MAC0"
            | _ -> "Signature1"
          in
          let payload = Claims.to_cbor t.claims in
          let sig_structure =
            build_sig_structure ~context_string
              ~protected_header:t.protected_header ~payload
          in
          (* Verify based on algorithm - returns Result to distinguish key mismatch from sig failure *)
          let verify_result =
            match (alg, key.Cose_key.key_data) with
            | ( ( Algorithm.HMAC_256_64 | Algorithm.HMAC_256
                | Algorithm.HMAC_384 | Algorithm.HMAC_512 ),
                Cose_key.Symmetric_key { k } ) ->
                if hmac_verify alg k sig_structure t.signature then Ok ()
                else Error Signature_mismatch
            | ( Algorithm.EdDSA,
                (Cose_key.Ed25519_pub { x } | Cose_key.Ed25519_priv { x; _ }) )
              -> (
                match Mirage_crypto_ec.Ed25519.pub_of_octets x with
                | Ok pub ->
                    if
                      Mirage_crypto_ec.Ed25519.verify ~key:pub t.signature
                        ~msg:sig_structure
                    then Ok ()
                    else Error Signature_mismatch
                | Error _ ->
                    Error (Key_type_mismatch "Invalid Ed25519 public key"))
            | ( Algorithm.ES256,
                (Cose_key.P256_pub { x; y } | Cose_key.P256_priv { x; y; _ }) )
              -> (
                match
                  Mirage_crypto_ec.P256.Dsa.pub_of_octets ("\x04" ^ x ^ y)
                with
                | Ok pub ->
                    let hash =
                      Digestif.SHA256.(
                        digest_string sig_structure |> to_raw_string)
                    in
                    let r = String.sub t.signature 0 32 in
                    let s = String.sub t.signature 32 32 in
                    if Mirage_crypto_ec.P256.Dsa.verify ~key:pub (r, s) hash
                    then Ok ()
                    else Error Signature_mismatch
                | Error _ ->
                    Error (Key_type_mismatch "Invalid P-256 public key"))
            | ( Algorithm.ES384,
                (Cose_key.P384_pub { x; y } | Cose_key.P384_priv { x; y; _ }) )
              -> (
                match
                  Mirage_crypto_ec.P384.Dsa.pub_of_octets ("\x04" ^ x ^ y)
                with
                | Ok pub ->
                    let hash =
                      Digestif.SHA384.(
                        digest_string sig_structure |> to_raw_string)
                    in
                    let r = String.sub t.signature 0 48 in
                    let s = String.sub t.signature 48 48 in
                    if Mirage_crypto_ec.P384.Dsa.verify ~key:pub (r, s) hash
                    then Ok ()
                    else Error Signature_mismatch
                | Error _ ->
                    Error (Key_type_mismatch "Invalid P-384 public key"))
            | ( Algorithm.ES512,
                (Cose_key.P521_pub { x; y } | Cose_key.P521_priv { x; y; _ }) )
              -> (
                match
                  Mirage_crypto_ec.P521.Dsa.pub_of_octets ("\x04" ^ x ^ y)
                with
                | Ok pub ->
                    let hash =
                      Digestif.SHA512.(
                        digest_string sig_structure |> to_raw_string)
                    in
                    let r = String.sub t.signature 0 66 in
                    let s = String.sub t.signature 66 66 in
                    if Mirage_crypto_ec.P521.Dsa.verify ~key:pub (r, s) hash
                    then Ok ()
                    else Error Signature_mismatch
                | Error _ ->
                    Error (Key_type_mismatch "Invalid P-521 public key"))
            | _ ->
                Error
                  (Key_type_mismatch
                     (Printf.sprintf "Key type doesn't match algorithm %s"
                        (Algorithm.to_string alg)))
          in
          verify_result

let validate ~now ?iss ?aud ?leeway t =
  let leeway = Option.value leeway ~default:Ptime.Span.zero in
  (* Check exp *)
  let check_exp () =
    match t.claims.exp with
    | Some exp -> (
        match Ptime.add_span exp leeway with
        | Some exp' when Ptime.is_later now ~than:exp' -> Error Token_expired
        | _ -> Ok ())
    | None -> Ok ()
  in
  (* Check nbf *)
  let check_nbf () =
    match t.claims.nbf with
    | Some nbf -> (
        match Ptime.sub_span nbf leeway with
        | Some nbf' when Ptime.is_earlier now ~than:nbf' ->
            Error Token_not_yet_valid
        | _ -> Ok ())
    | None -> Ok ()
  in
  (* Check iss *)
  let check_iss () =
    match iss with
    | Some expected_iss -> (
        match t.claims.iss with
        | Some actual_iss when actual_iss = expected_iss -> Ok ()
        | _ -> Error Invalid_issuer)
    | None -> Ok ()
  in
  (* Check aud *)
  let check_aud () =
    match aud with
    | Some expected_aud ->
        if List.mem expected_aud t.claims.aud then Ok ()
        else Error Invalid_audience
    | None -> Ok ()
  in
  match check_exp () with
  | Error _ as e -> e
  | Ok () -> (
      match check_nbf () with
      | Error _ as e -> e
      | Ok () -> (
          match check_iss () with Error _ as e -> e | Ok () -> check_aud ()))

let verify_and_validate ~key ~now ?allowed_algs ?iss ?aud ?leeway t =
  match verify ~key ?allowed_algs t with
  | Error _ as e -> e
  | Ok () -> validate ~now ?iss ?aud ?leeway t

(** Encode protected header as CBOR map *)
let encode_protected_header algorithm =
  let open Cbort.Cbor in
  Map
    [
      ( Int (Z.of_int header_alg),
        Int (Z.of_int (Algorithm.to_cose_int algorithm)) );
    ]
  |> Cbort.encode_string Cbort.any

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
  |> Cbort.encode_string Cbort.any

let create ~algorithm ~claims ~key =
  (* Encode protected header *)
  let protected_header = encode_protected_header algorithm in

  (* Build Sig_structure or MAC_structure *)
  let context_string =
    match algorithm with
    | Algorithm.HMAC_256_64 | Algorithm.HMAC_256 | Algorithm.HMAC_384
    | Algorithm.HMAC_512 ->
        "MAC0"
    | _ -> "Signature1"
  in
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
        match algorithm with
        | Algorithm.HMAC_256_64 | Algorithm.HMAC_256 | Algorithm.HMAC_384
        | Algorithm.HMAC_512 ->
            cose_mac0_tag
        | _ -> cose_sign1_tag
      in
      let raw =
        encode_cose_message ~cose_tag ~protected_header ~payload ~signature
      in
      Ok
        {
          claims;
          algorithm = Some algorithm;
          kid = key.Cose_key.kid;
          protected_header;
          signature;
          raw;
        }

let encode t = t.raw

let is_expired ~now ?leeway t =
  match t.claims.exp with
  | None -> false
  | Some exp -> (
      let leeway = Option.value leeway ~default:Ptime.Span.zero in
      match Ptime.add_span exp leeway with
      | Some exp' -> Ptime.is_later now ~than:exp'
      | None -> true)

let time_to_expiry ~now t =
  match t.claims.exp with
  | None -> None
  | Some exp ->
      let diff = Ptime.diff exp now in
      if Ptime.Span.compare diff Ptime.Span.zero <= 0 then None else Some diff
