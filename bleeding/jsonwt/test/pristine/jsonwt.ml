(** JSON Web Token (JWT) - RFC 7519 *)

(* Error types *)
type error =
  | Invalid_json of string
  | Invalid_base64url of string
  | Invalid_structure of string
  | Invalid_header of string
  | Invalid_claims of string
  | Invalid_uri of string
  | Duplicate_claim of string
  | Unsupported_algorithm of string
  | Algorithm_not_allowed of string
  | Signature_mismatch
  | Token_expired
  | Token_not_yet_valid
  | Invalid_issuer
  | Invalid_audience
  | Key_type_mismatch of string
  | Unsecured_not_allowed
  | Nesting_too_deep

let pp_error fmt = function
  | Invalid_json s -> Format.fprintf fmt "Invalid JSON: %s" s
  | Invalid_base64url s -> Format.fprintf fmt "Invalid base64url: %s" s
  | Invalid_structure s -> Format.fprintf fmt "Invalid structure: %s" s
  | Invalid_header s -> Format.fprintf fmt "Invalid header: %s" s
  | Invalid_claims s -> Format.fprintf fmt "Invalid claims: %s" s
  | Invalid_uri s -> Format.fprintf fmt "Invalid URI: %s" s
  | Duplicate_claim s -> Format.fprintf fmt "Duplicate claim: %s" s
  | Unsupported_algorithm s -> Format.fprintf fmt "Unsupported algorithm: %s" s
  | Algorithm_not_allowed s -> Format.fprintf fmt "Algorithm not allowed: %s" s
  | Signature_mismatch -> Format.fprintf fmt "Signature mismatch"
  | Token_expired -> Format.fprintf fmt "Token expired"
  | Token_not_yet_valid -> Format.fprintf fmt "Token not yet valid"
  | Invalid_issuer -> Format.fprintf fmt "Invalid issuer"
  | Invalid_audience -> Format.fprintf fmt "Invalid audience"
  | Key_type_mismatch s -> Format.fprintf fmt "Key type mismatch: %s" s
  | Unsecured_not_allowed -> Format.fprintf fmt "Unsecured JWT not allowed"
  | Nesting_too_deep -> Format.fprintf fmt "Nested JWT too deep"

let error_to_string e = Format.asprintf "%a" pp_error e

(* Base64url encoding/decoding per RFC 7515 Appendix C *)
let base64url_encode s =
  Base64.encode_string ~pad:false ~alphabet:Base64.uri_safe_alphabet s

let base64url_decode s =
  (* Add padding if needed *)
  let len = String.length s in
  let pad_len = (4 - (len mod 4)) mod 4 in
  let padded = s ^ String.make pad_len '=' in
  match Base64.decode ~alphabet:Base64.uri_safe_alphabet padded with
  | Ok v -> Ok v
  | Error (`Msg m) -> Error (Invalid_base64url m)

(* StringOrURI validation per RFC 7519 Section 2 *)
let validate_string_or_uri s =
  if String.contains s ':' then
    (* Must be a valid URI - basic check for scheme *)
    match String.index_opt s ':' with
    | Some i when i > 0 ->
        let scheme = String.sub s 0 i in
        (* Check scheme is alphanumeric with +.- allowed after first char *)
        let valid_scheme =
          String.length scheme > 0
          && (match scheme.[0] with
            | 'a' .. 'z' | 'A' .. 'Z' -> true
            | _ -> false)
          && String.for_all
               (fun c ->
                 match c with
                 | 'a' .. 'z' | 'A' .. 'Z' | '0' .. '9' | '+' | '-' | '.' ->
                     true
                 | _ -> false)
               scheme
        in
        if valid_scheme then Ok s
        else Error (Invalid_uri (Printf.sprintf "Invalid URI scheme in: %s" s))
    | _ -> Error (Invalid_uri (Printf.sprintf "Invalid URI: %s" s))
  else Ok s

(* Algorithm module *)
module Algorithm = struct
  type t =
    | None
    | HS256
    | HS384
    | HS512
    | RS256
    | RS384
    | RS512
    | ES256
    | ES384
    | ES512
    | EdDSA

  let to_string = function
    | None -> "none"
    | HS256 -> "HS256"
    | HS384 -> "HS384"
    | HS512 -> "HS512"
    | RS256 -> "RS256"
    | RS384 -> "RS384"
    | RS512 -> "RS512"
    | ES256 -> "ES256"
    | ES384 -> "ES384"
    | ES512 -> "ES512"
    | EdDSA -> "EdDSA"

  let of_string = function
    | "none" -> Ok None
    | "HS256" -> Ok HS256
    | "HS384" -> Ok HS384
    | "HS512" -> Ok HS512
    | "RS256" -> Ok RS256
    | "RS384" -> Ok RS384
    | "RS512" -> Ok RS512
    | "ES256" -> Ok ES256
    | "ES384" -> Ok ES384
    | "ES512" -> Ok ES512
    | "EdDSA" -> Ok EdDSA
    | s -> Error (Unsupported_algorithm s)

  let all =
    [ HS256; HS384; HS512; RS256; RS384; RS512; ES256; ES384; ES512; EdDSA ]

  let all_with_none = None :: all
end

(* JWK module *)
module Jwk = struct
  type kty = Oct | Rsa | Ec | Okp
  type crv = P256 | P384 | P521 | Ed25519

  type key_data =
    | Symmetric of { k : string }
    | Ed25519_pub of { x : string }
    | Ed25519_priv of { x : string; d : string }
    | P256_pub of { x : string; y : string }
    | P256_priv of { x : string; y : string; d : string }
    | P384_pub of { x : string; y : string }
    | P384_priv of { x : string; y : string; d : string }
    | P521_pub of { x : string; y : string }
    | P521_priv of { x : string; y : string; d : string }
    | Rsa_pub of { n : string; e : string }
    | Rsa_priv of {
        n : string;
        e : string;
        d : string;
        p : string;
        q : string;
        dp : string;
        dq : string;
        qi : string;
      }

  type t = {
    key_data : key_data;
    kid : string option;
    alg : Algorithm.t option;
  }

  let symmetric k = { key_data = Symmetric { k }; kid = None; alg = None }

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

  let rsa_pub ~n ~e =
    { key_data = Rsa_pub { n; e }; kid = None; alg = Some Algorithm.RS256 }

  let rsa_priv ~n ~e ~d ~p ~q ~dp ~dq ~qi =
    {
      key_data = Rsa_priv { n; e; d; p; q; dp; dq; qi };
      kid = None;
      alg = Some Algorithm.RS256;
    }

  let kty t =
    match t.key_data with
    | Symmetric _ -> Oct
    | Ed25519_pub _ | Ed25519_priv _ -> Okp
    | P256_pub _ | P256_priv _ | P384_pub _ | P384_priv _ | P521_pub _
    | P521_priv _ ->
        Ec
    | Rsa_pub _ | Rsa_priv _ -> Rsa

  let kid t = t.kid
  let alg t = t.alg
  let with_kid id t = { t with kid = Some id }
  let with_alg a t = { t with alg = Some a }

  (* Helper to extract string from Jsont.json object members *)
  let get_json_string members name =
    List.find_map
      (fun ((n, _), v) ->
        if n = name then
          match v with Jsont.String (s, _) -> Some s | _ -> None
        else None)
      members

  let get_json_string_req members name =
    match get_json_string members name with
    | Some s -> Ok s
    | None ->
        Error (Invalid_json (Printf.sprintf "missing required field: %s" name))

  let of_json s =
    (* Parse the JSON to determine key type first *)
    match Jsont_bytesrw.decode_string Jsont.json s with
    | Error e -> Error (Invalid_json e)
    | Ok (Jsont.Null _) -> Error (Invalid_json "null is not a valid JWK")
    | Ok (Jsont.Object (members, _)) -> (
        let ( let* ) = Result.bind in
        let* kty_s = get_json_string_req members "kty" in
        let kid = get_json_string members "kid" in
        let alg_opt =
          match get_json_string members "alg" with
          | None -> Ok None
          | Some s -> (
              match Algorithm.of_string s with
              | Ok a -> Ok (Some a)
              | Error _ -> Ok None (* ignore unknown alg in JWK *))
        in
        let* alg = alg_opt in
        match kty_s with
        | "oct" ->
            let* k_b64 = get_json_string_req members "k" in
            let* k = base64url_decode k_b64 in
            Ok { key_data = Symmetric { k }; kid; alg }
        | "OKP" -> (
            let* crv = get_json_string_req members "crv" in
            if crv <> "Ed25519" then
              Error (Invalid_json (Printf.sprintf "unsupported curve: %s" crv))
            else
              let* x_b64 = get_json_string_req members "x" in
              let* x = base64url_decode x_b64 in
              match get_json_string members "d" with
              | None -> Ok { key_data = Ed25519_pub { x }; kid; alg }
              | Some d_b64 ->
                  let* d = base64url_decode d_b64 in
                  Ok { key_data = Ed25519_priv { x; d }; kid; alg })
        | "EC" -> (
            let* crv = get_json_string_req members "crv" in
            let* x_b64 = get_json_string_req members "x" in
            let* y_b64 = get_json_string_req members "y" in
            let* x = base64url_decode x_b64 in
            let* y = base64url_decode y_b64 in
            let has_d = Option.is_some (get_json_string members "d") in
            let get_d () =
              let* d_b64 = get_json_string_req members "d" in
              base64url_decode d_b64
            in
            match crv with
            | "P-256" ->
                if has_d then
                  let* d = get_d () in
                  Ok { key_data = P256_priv { x; y; d }; kid; alg }
                else Ok { key_data = P256_pub { x; y }; kid; alg }
            | "P-384" ->
                if has_d then
                  let* d = get_d () in
                  Ok { key_data = P384_priv { x; y; d }; kid; alg }
                else Ok { key_data = P384_pub { x; y }; kid; alg }
            | "P-521" ->
                if has_d then
                  let* d = get_d () in
                  Ok { key_data = P521_priv { x; y; d }; kid; alg }
                else Ok { key_data = P521_pub { x; y }; kid; alg }
            | _ ->
                Error
                  (Invalid_json (Printf.sprintf "unsupported curve: %s" crv)))
        | "RSA" -> (
            let* n_b64 = get_json_string_req members "n" in
            let* e_b64 = get_json_string_req members "e" in
            let* n = base64url_decode n_b64 in
            let* e = base64url_decode e_b64 in
            match get_json_string members "d" with
            | None -> Ok { key_data = Rsa_pub { n; e }; kid; alg }
            | Some d_b64 ->
                let* d = base64url_decode d_b64 in
                let* p_b64 = get_json_string_req members "p" in
                let* q_b64 = get_json_string_req members "q" in
                let* dp_b64 = get_json_string_req members "dp" in
                let* dq_b64 = get_json_string_req members "dq" in
                let* qi_b64 = get_json_string_req members "qi" in
                let* p = base64url_decode p_b64 in
                let* q = base64url_decode q_b64 in
                let* dp = base64url_decode dp_b64 in
                let* dq = base64url_decode dq_b64 in
                let* qi = base64url_decode qi_b64 in
                Ok
                  {
                    key_data = Rsa_priv { n; e; d; p; q; dp; dq; qi };
                    kid;
                    alg;
                  })
        | _ -> Error (Invalid_json (Printf.sprintf "unsupported kty: %s" kty_s))
        )
    | Ok _ -> Error (Invalid_json "JWK must be a JSON object")

  (* Helper to create JSON members *)
  let meta = Jsont.Meta.none
  let json_string s = Jsont.String (s, meta)
  let json_mem name value = ((name, meta), value)

  let to_json t =
    let add_opt name v_opt acc =
      match v_opt with
      | None -> acc
      | Some v -> json_mem name (json_string v) :: acc
    in
    let members = [] in
    let members = add_opt "kid" t.kid members in
    let members =
      add_opt "alg" (Option.map Algorithm.to_string t.alg) members
    in
    let members =
      match t.key_data with
      | Symmetric { k } ->
          json_mem "kty" (json_string "oct")
          :: json_mem "k" (json_string (base64url_encode k))
          :: members
      | Ed25519_pub { x } ->
          json_mem "kty" (json_string "OKP")
          :: json_mem "crv" (json_string "Ed25519")
          :: json_mem "x" (json_string (base64url_encode x))
          :: members
      | Ed25519_priv { x; d } ->
          json_mem "kty" (json_string "OKP")
          :: json_mem "crv" (json_string "Ed25519")
          :: json_mem "x" (json_string (base64url_encode x))
          :: json_mem "d" (json_string (base64url_encode d))
          :: members
      | P256_pub { x; y } ->
          json_mem "kty" (json_string "EC")
          :: json_mem "crv" (json_string "P-256")
          :: json_mem "x" (json_string (base64url_encode x))
          :: json_mem "y" (json_string (base64url_encode y))
          :: members
      | P256_priv { x; y; d } ->
          json_mem "kty" (json_string "EC")
          :: json_mem "crv" (json_string "P-256")
          :: json_mem "x" (json_string (base64url_encode x))
          :: json_mem "y" (json_string (base64url_encode y))
          :: json_mem "d" (json_string (base64url_encode d))
          :: members
      | P384_pub { x; y } ->
          json_mem "kty" (json_string "EC")
          :: json_mem "crv" (json_string "P-384")
          :: json_mem "x" (json_string (base64url_encode x))
          :: json_mem "y" (json_string (base64url_encode y))
          :: members
      | P384_priv { x; y; d } ->
          json_mem "kty" (json_string "EC")
          :: json_mem "crv" (json_string "P-384")
          :: json_mem "x" (json_string (base64url_encode x))
          :: json_mem "y" (json_string (base64url_encode y))
          :: json_mem "d" (json_string (base64url_encode d))
          :: members
      | P521_pub { x; y } ->
          json_mem "kty" (json_string "EC")
          :: json_mem "crv" (json_string "P-521")
          :: json_mem "x" (json_string (base64url_encode x))
          :: json_mem "y" (json_string (base64url_encode y))
          :: members
      | P521_priv { x; y; d } ->
          json_mem "kty" (json_string "EC")
          :: json_mem "crv" (json_string "P-521")
          :: json_mem "x" (json_string (base64url_encode x))
          :: json_mem "y" (json_string (base64url_encode y))
          :: json_mem "d" (json_string (base64url_encode d))
          :: members
      | Rsa_pub { n; e } ->
          json_mem "kty" (json_string "RSA")
          :: json_mem "n" (json_string (base64url_encode n))
          :: json_mem "e" (json_string (base64url_encode e))
          :: members
      | Rsa_priv { n; e; d; p; q; dp; dq; qi } ->
          json_mem "kty" (json_string "RSA")
          :: json_mem "n" (json_string (base64url_encode n))
          :: json_mem "e" (json_string (base64url_encode e))
          :: json_mem "d" (json_string (base64url_encode d))
          :: json_mem "p" (json_string (base64url_encode p))
          :: json_mem "q" (json_string (base64url_encode q))
          :: json_mem "dp" (json_string (base64url_encode dp))
          :: json_mem "dq" (json_string (base64url_encode dq))
          :: json_mem "qi" (json_string (base64url_encode qi))
          :: members
    in
    match
      Jsont_bytesrw.encode_string Jsont.json (Jsont.Object (members, meta))
    with
    | Ok s -> s
    | Error _ -> "{}" (* Should not happen *)
end

(* Header module *)
module Header = struct
  type t = {
    alg : Algorithm.t;
    typ : string option;
    kid : string option;
    cty : string option;
  }

  let make ?typ ?kid ?cty alg = { alg; typ; kid; cty }

  let is_nested t =
    match t.cty with
    | Some s -> String.uppercase_ascii s = "JWT"
    | None -> false

  (* Helper to extract string from Jsont.json object members *)
  let get_json_string members name =
    List.find_map
      (fun ((n, _), v) ->
        if n = name then
          match v with Jsont.String (s, _) -> Some s | _ -> None
        else None)
      members

  let of_json s =
    match Jsont_bytesrw.decode_string Jsont.json s with
    | Error e -> Error (Invalid_json e)
    | Ok (Jsont.Null _) -> Error (Invalid_header "null is not a valid header")
    | Ok (Jsont.Object (members, _)) -> (
        let ( let* ) = Result.bind in
        let alg_s = get_json_string members "alg" in
        match alg_s with
        | None -> Error (Invalid_header "missing required 'alg' field")
        | Some alg_str ->
            let* alg = Algorithm.of_string alg_str in
            let typ = get_json_string members "typ" in
            let kid = get_json_string members "kid" in
            let cty = get_json_string members "cty" in
            Ok { alg; typ; kid; cty })
    | Ok _ -> Error (Invalid_header "header must be a JSON object")

  let meta = Jsont.Meta.none
  let json_string s = Jsont.String (s, meta)
  let json_mem name value = ((name, meta), value)

  let to_json h =
    let members =
      [ json_mem "alg" (json_string (Algorithm.to_string h.alg)) ]
    in
    let add_opt name v_opt acc =
      match v_opt with
      | None -> acc
      | Some v -> json_mem name (json_string v) :: acc
    in
    let members = add_opt "typ" h.typ members in
    let members = add_opt "kid" h.kid members in
    let members = add_opt "cty" h.cty members in
    match
      Jsont_bytesrw.encode_string Jsont.json
        (Jsont.Object (List.rev members, meta))
    with
    | Ok s -> s
    | Error _ -> "{}"
end

(* Claims module *)
module Claims = struct
  type t = {
    iss : string option;
    sub : string option;
    aud : string list;
    exp : Ptime.t option;
    nbf : Ptime.t option;
    iat : Ptime.t option;
    jti : string option;
    custom : (string * Jsont.json) list;
  }

  let iss t = t.iss
  let sub t = t.sub
  let aud t = t.aud
  let exp t = t.exp
  let nbf t = t.nbf
  let iat t = t.iat
  let jti t = t.jti
  let get name t = List.assoc_opt name t.custom

  let get_string name t =
    match get name t with Some (Jsont.String (s, _)) -> Some s | _ -> None

  let get_int name t =
    match get name t with
    | Some (Jsont.Number (n, _)) -> (
        try Some (int_of_float n) with _ -> None)
    | _ -> None

  let get_bool name t =
    match get name t with Some (Jsont.Bool (b, _)) -> Some b | _ -> None

  let meta = Jsont.Meta.none
  let json_string s = Jsont.String (s, meta)
  let json_number n = Jsont.Number (n, meta)
  let json_bool b = Jsont.Bool (b, meta)
  let json_mem name value = ((name, meta), value)

  type builder = t

  let empty =
    {
      iss = None;
      sub = None;
      aud = [];
      exp = None;
      nbf = None;
      iat = None;
      jti = None;
      custom = [];
    }

  let set_iss v t = { t with iss = Some v }
  let set_sub v t = { t with sub = Some v }
  let set_aud v t = { t with aud = v }
  let set_exp v t = { t with exp = Some v }
  let set_nbf v t = { t with nbf = Some v }
  let set_iat v t = { t with iat = Some v }
  let set_jti v t = { t with jti = Some v }
  let set name value t = { t with custom = (name, value) :: t.custom }
  let set_string name value t = set name (json_string value) t
  let set_int name value t = set name (json_number (float_of_int value)) t
  let set_bool name value t = set name (json_bool value) t
  let build t = t

  let ptime_of_numeric_date n =
    let span = Ptime.Span.of_float_s n in
    Option.bind span (fun s -> Ptime.of_span s)

  let numeric_date_of_ptime t = Ptime.to_span t |> Ptime.Span.to_float_s

  (* Helper to extract values from Jsont.json object members *)
  let get_json_string members name =
    List.find_map
      (fun ((n, _), v) ->
        if n = name then
          match v with Jsont.String (s, _) -> Some s | _ -> None
        else None)
      members

  let get_json_number members name =
    List.find_map
      (fun ((n, _), v) ->
        if n = name then
          match v with Jsont.Number (n, _) -> Some n | _ -> None
        else None)
      members

  let get_json_aud members =
    List.find_map
      (fun ((n, _), v) ->
        if n = "aud" then
          match v with
          | Jsont.String (s, _) -> Some [ s ]
          | Jsont.Array (arr, _) ->
              Some
                (List.filter_map
                   (function Jsont.String (s, _) -> Some s | _ -> None)
                   arr)
          | _ -> None
        else None)
      members
    |> Option.value ~default:[]

  let of_json ?(strict = true) s =
    match Jsont_bytesrw.decode_string Jsont.json s with
    | Error e -> Error (Invalid_json e)
    | Ok (Jsont.Null _) ->
        Error (Invalid_claims "null is not a valid claims set")
    | Ok (Jsont.Object (members, _)) ->
        let ( let* ) = Result.bind in
        (* Check for duplicates in strict mode *)
        let* () =
          if strict then
            let names = List.map (fun ((n, _), _) -> n) members in
            let rec check_dups = function
              | [] -> Ok ()
              | n :: rest ->
                  if List.mem n rest then Error (Duplicate_claim n)
                  else check_dups rest
            in
            check_dups names
          else Ok ()
        in
        (* Validate StringOrURI for iss and sub *)
        let* iss =
          match get_json_string members "iss" with
          | None -> Ok None
          | Some s ->
              let* _ = validate_string_or_uri s in
              Ok (Some s)
        in
        let* sub =
          match get_json_string members "sub" with
          | None -> Ok None
          | Some s ->
              let* _ = validate_string_or_uri s in
              Ok (Some s)
        in
        let exp =
          Option.bind (get_json_number members "exp") ptime_of_numeric_date
        in
        let nbf =
          Option.bind (get_json_number members "nbf") ptime_of_numeric_date
        in
        let iat =
          Option.bind (get_json_number members "iat") ptime_of_numeric_date
        in
        let jti = get_json_string members "jti" in
        let aud = get_json_aud members in
        (* Collect custom claims (everything not registered) *)
        let registered = [ "iss"; "sub"; "aud"; "exp"; "nbf"; "iat"; "jti" ] in
        let custom =
          List.filter_map
            (fun ((n, _), v) ->
              if List.mem n registered then None else Some (n, v))
            members
        in
        Ok { iss; sub; aud; exp; nbf; iat; jti; custom }
    | Ok _ -> Error (Invalid_claims "claims must be a JSON object")

  let to_json t =
    let members = [] in
    let add_string name v_opt acc =
      match v_opt with
      | None -> acc
      | Some v -> json_mem name (json_string v) :: acc
    in
    let add_time name v_opt acc =
      match v_opt with
      | None -> acc
      | Some v -> json_mem name (json_number (numeric_date_of_ptime v)) :: acc
    in
    let members = add_string "iss" t.iss members in
    let members = add_string "sub" t.sub members in
    let members =
      match t.aud with
      | [] -> members
      | [ single ] -> json_mem "aud" (json_string single) :: members
      | many ->
          let arr = List.map json_string many in
          json_mem "aud" (Jsont.Array (arr, meta)) :: members
    in
    let members = add_time "exp" t.exp members in
    let members = add_time "nbf" t.nbf members in
    let members = add_time "iat" t.iat members in
    let members = add_string "jti" t.jti members in
    let members =
      List.fold_left
        (fun acc (name, value) -> json_mem name value :: acc)
        members t.custom
    in
    match
      Jsont_bytesrw.encode_string Jsont.json
        (Jsont.Object (List.rev members, meta))
    with
    | Ok s -> s
    | Error _ -> "{}"
end

(* JWT type *)
type t = {
  header : Header.t;
  claims : Claims.t;
  signature : string;
  raw : string;
}

let header t = t.header
let claims t = t.claims
let signature t = t.signature
let raw t = t.raw
let is_nested t = Header.is_nested t.header

(* Parsing *)
let parse ?(strict = true) token =
  let ( let* ) = Result.bind in
  (* RFC 7519 Section 7.2 step 1: verify at least one period *)
  if not (String.contains token '.') then
    Error (Invalid_structure "JWT must contain at least one period character")
  else
    match String.split_on_char '.' token with
    | [ header_b64; payload_b64; sig_b64 ] ->
        (* JWS compact serialization: 3 parts *)
        let* header_json = base64url_decode header_b64 in
        let* payload_json = base64url_decode payload_b64 in
        let* signature = base64url_decode sig_b64 in
        let* header = Header.of_json header_json in
        let* claims = Claims.of_json ~strict payload_json in
        Ok { header; claims; signature; raw = token }
    | parts when List.length parts = 5 ->
        (* JWE compact serialization - not yet supported *)
        Error (Invalid_structure "JWE (encrypted JWT) not yet supported")
    | _ ->
        Error (Invalid_structure "JWT must have 3 parts (JWS) or 5 parts (JWE)")

let parse_unsafe = parse ~strict:false

let parse_nested ?(strict = true) ?(max_depth = 5) token =
  let ( let* ) = Result.bind in
  let rec loop depth acc tok =
    if depth > max_depth then Error Nesting_too_deep
    else
      let* jwt = parse ~strict tok in
      let acc = jwt :: acc in
      if is_nested jwt then
        (* The payload is another JWT - decode and parse it *)
        match String.split_on_char '.' tok with
        | [ _; payload_b64; _ ] ->
            let* inner_token = base64url_decode payload_b64 in
            loop (depth + 1) acc inner_token
        | _ -> Ok (List.rev acc)
      else Ok (List.rev acc)
  in
  loop 1 [] token

(* Signature operations *)
module Sign = struct
  let hmac_sha256 ~key data =
    let key = Cstruct.of_string key in
    let data = Cstruct.of_string data in
    Digestif.SHA256.hmac_string ~key:(Cstruct.to_string key)
      (Cstruct.to_string data)
    |> Digestif.SHA256.to_raw_string

  let hmac_sha384 ~key data =
    let key = Cstruct.of_string key in
    let data = Cstruct.of_string data in
    Digestif.SHA384.hmac_string ~key:(Cstruct.to_string key)
      (Cstruct.to_string data)
    |> Digestif.SHA384.to_raw_string

  let hmac_sha512 ~key data =
    let key = Cstruct.of_string key in
    let data = Cstruct.of_string data in
    Digestif.SHA512.hmac_string ~key:(Cstruct.to_string key)
      (Cstruct.to_string data)
    |> Digestif.SHA512.to_raw_string

  (* EdDSA signing using mirage-crypto-ec *)
  let ed25519_sign ~priv data =
    match Mirage_crypto_ec.Ed25519.priv_of_octets priv with
    | Error _ -> Error (Key_type_mismatch "Invalid Ed25519 private key")
    | Ok priv ->
        let sig_ = Mirage_crypto_ec.Ed25519.sign ~key:priv data in
        Ok sig_

  let ed25519_verify ~pub ~signature data =
    match Mirage_crypto_ec.Ed25519.pub_of_octets pub with
    | Error _ -> Error (Key_type_mismatch "Invalid Ed25519 public key")
    | Ok pub ->
        let valid =
          Mirage_crypto_ec.Ed25519.verify ~key:pub signature ~msg:data
        in
        if valid then Ok () else Error Signature_mismatch

  (* P-256 ECDSA *)
  let p256_sign ~priv data =
    match Mirage_crypto_ec.P256.Dsa.priv_of_octets priv with
    | Error _ -> Error (Key_type_mismatch "Invalid P-256 private key")
    | Ok priv ->
        let hash =
          Digestif.SHA256.digest_string data |> Digestif.SHA256.to_raw_string
        in
        let r, s = Mirage_crypto_ec.P256.Dsa.sign ~key:priv hash in
        (* JWS uses raw R||S format, each 32 bytes for P-256 *)
        (* Pad to 32 bytes each *)
        let pad32 s =
          let len = String.length s in
          if len >= 32 then String.sub s (len - 32) 32
          else String.make (32 - len) '\x00' ^ s
        in
        Ok (pad32 r ^ pad32 s)

  let p256_verify ~pub ~signature data =
    if String.length signature <> 64 then Error Signature_mismatch
    else
      let r = String.sub signature 0 32 in
      let s = String.sub signature 32 32 in
      match Mirage_crypto_ec.P256.Dsa.pub_of_octets pub with
      | Error _ -> Error (Key_type_mismatch "Invalid P-256 public key")
      | Ok pub ->
          let hash =
            Digestif.SHA256.digest_string data |> Digestif.SHA256.to_raw_string
          in
          let valid = Mirage_crypto_ec.P256.Dsa.verify ~key:pub (r, s) hash in
          if valid then Ok () else Error Signature_mismatch

  (* P-384 ECDSA *)
  let p384_sign ~priv data =
    match Mirage_crypto_ec.P384.Dsa.priv_of_octets priv with
    | Error _ -> Error (Key_type_mismatch "Invalid P-384 private key")
    | Ok priv ->
        let hash =
          Digestif.SHA384.digest_string data |> Digestif.SHA384.to_raw_string
        in
        let r, s = Mirage_crypto_ec.P384.Dsa.sign ~key:priv hash in
        let pad48 s =
          let len = String.length s in
          if len >= 48 then String.sub s (len - 48) 48
          else String.make (48 - len) '\x00' ^ s
        in
        Ok (pad48 r ^ pad48 s)

  let p384_verify ~pub ~signature data =
    if String.length signature <> 96 then Error Signature_mismatch
    else
      let r = String.sub signature 0 48 in
      let s = String.sub signature 48 48 in
      match Mirage_crypto_ec.P384.Dsa.pub_of_octets pub with
      | Error _ -> Error (Key_type_mismatch "Invalid P-384 public key")
      | Ok pub ->
          let hash =
            Digestif.SHA384.digest_string data |> Digestif.SHA384.to_raw_string
          in
          let valid = Mirage_crypto_ec.P384.Dsa.verify ~key:pub (r, s) hash in
          if valid then Ok () else Error Signature_mismatch

  (* P-521 ECDSA *)
  let p521_sign ~priv data =
    match Mirage_crypto_ec.P521.Dsa.priv_of_octets priv with
    | Error _ -> Error (Key_type_mismatch "Invalid P-521 private key")
    | Ok priv ->
        let hash =
          Digestif.SHA512.digest_string data |> Digestif.SHA512.to_raw_string
        in
        let r, s = Mirage_crypto_ec.P521.Dsa.sign ~key:priv hash in
        let pad66 s =
          let len = String.length s in
          if len >= 66 then String.sub s (len - 66) 66
          else String.make (66 - len) '\x00' ^ s
        in
        Ok (pad66 r ^ pad66 s)

  let p521_verify ~pub ~signature data =
    if String.length signature <> 132 then Error Signature_mismatch
    else
      let r = String.sub signature 0 66 in
      let s = String.sub signature 66 66 in
      match Mirage_crypto_ec.P521.Dsa.pub_of_octets pub with
      | Error _ -> Error (Key_type_mismatch "Invalid P-521 public key")
      | Ok pub ->
          let hash =
            Digestif.SHA512.digest_string data |> Digestif.SHA512.to_raw_string
          in
          let valid = Mirage_crypto_ec.P521.Dsa.verify ~key:pub (r, s) hash in
          if valid then Ok () else Error Signature_mismatch

  (* RSA PKCS#1 v1.5 - stub implementations *)
  (* TODO: Implement proper RSA signing/verification with JWK key parsing *)
  let _rsa_sign _hash_type ~priv:_ _data =
    Error (Key_type_mismatch "RSA signing not yet implemented")

  let _rsa_verify _hash_type ~pub:_ ~signature:_ _data =
    Error (Key_type_mismatch "RSA verification not yet implemented")
end

(* Get signing input from token *)
let signing_input token =
  match String.rindex_opt token '.' with
  | None -> token
  | Some i -> String.sub token 0 i

(* Verification *)
let verify ~key ?(allow_none = false) ?(allowed_algs = Algorithm.all) t =
  let ( let* ) = Result.bind in
  let alg = t.header.alg in
  let alg_str = Algorithm.to_string alg in
  (* Check if algorithm is allowed *)
  let* () =
    if alg = Algorithm.None then
      (* For alg:none, only allow_none flag matters *)
      if allow_none then Ok () else Error Unsecured_not_allowed
    else if List.mem alg allowed_algs then Ok ()
    else Error (Algorithm_not_allowed alg_str)
  in
  let input = signing_input t.raw in
  match (alg, key.Jwk.key_data) with
  | Algorithm.None, _ ->
      (* Unsecured JWT - signature must be empty *)
      if t.signature = "" then Ok () else Error Signature_mismatch
  | Algorithm.HS256, Jwk.Symmetric { k } ->
      let expected = Sign.hmac_sha256 ~key:k input in
      if Eqaf.equal expected t.signature then Ok ()
      else Error Signature_mismatch
  | Algorithm.HS384, Jwk.Symmetric { k } ->
      let expected = Sign.hmac_sha384 ~key:k input in
      if Eqaf.equal expected t.signature then Ok ()
      else Error Signature_mismatch
  | Algorithm.HS512, Jwk.Symmetric { k } ->
      let expected = Sign.hmac_sha512 ~key:k input in
      if Eqaf.equal expected t.signature then Ok ()
      else Error Signature_mismatch
  | Algorithm.EdDSA, Jwk.Ed25519_pub { x } ->
      Sign.ed25519_verify ~pub:x ~signature:t.signature input
  | Algorithm.EdDSA, Jwk.Ed25519_priv { x; d = _ } ->
      Sign.ed25519_verify ~pub:x ~signature:t.signature input
  | Algorithm.ES256, Jwk.P256_pub { x; y } ->
      let pub = x ^ y in
      (* Uncompressed point *)
      Sign.p256_verify ~pub ~signature:t.signature input
  | Algorithm.ES256, Jwk.P256_priv { x; y; d = _ } ->
      let pub = x ^ y in
      Sign.p256_verify ~pub ~signature:t.signature input
  | Algorithm.ES384, Jwk.P384_pub { x; y } ->
      let pub = x ^ y in
      Sign.p384_verify ~pub ~signature:t.signature input
  | Algorithm.ES384, Jwk.P384_priv { x; y; d = _ } ->
      let pub = x ^ y in
      Sign.p384_verify ~pub ~signature:t.signature input
  | Algorithm.ES512, Jwk.P521_pub { x; y } ->
      let pub = x ^ y in
      Sign.p521_verify ~pub ~signature:t.signature input
  | Algorithm.ES512, Jwk.P521_priv { x; y; d = _ } ->
      let pub = x ^ y in
      Sign.p521_verify ~pub ~signature:t.signature input
  | Algorithm.RS256, Jwk.Rsa_pub _ ->
      Error (Key_type_mismatch "RSA verification not yet implemented")
  | Algorithm.RS384, Jwk.Rsa_pub _ ->
      Error (Key_type_mismatch "RSA verification not yet implemented")
  | Algorithm.RS512, Jwk.Rsa_pub _ ->
      Error (Key_type_mismatch "RSA verification not yet implemented")
  | alg, _ ->
      Error
        (Key_type_mismatch
           (Printf.sprintf "Key type doesn't match algorithm %s"
              (Algorithm.to_string alg)))

(* Claims validation *)
let validate ~now ?iss ?aud ?(leeway = Ptime.Span.zero) t =
  let ( let* ) = Result.bind in
  let claims = t.claims in
  (* Check exp claim *)
  let* () =
    match Claims.exp claims with
    | None -> Ok ()
    | Some exp_time ->
        let exp_with_leeway =
          Ptime.add_span exp_time leeway |> Option.value ~default:exp_time
        in
        if Ptime.is_later now ~than:exp_with_leeway then Error Token_expired
        else Ok ()
  in
  (* Check nbf claim *)
  let* () =
    match Claims.nbf claims with
    | None -> Ok ()
    | Some nbf_time ->
        let nbf_with_leeway =
          Ptime.sub_span nbf_time leeway |> Option.value ~default:nbf_time
        in
        if Ptime.is_earlier now ~than:nbf_with_leeway then
          Error Token_not_yet_valid
        else Ok ()
  in
  (* Check iss claim *)
  let* () =
    match iss with
    | None -> Ok ()
    | Some expected_iss -> (
        match Claims.iss claims with
        | None -> Error Invalid_issuer
        | Some actual_iss ->
            if String.equal expected_iss actual_iss then Ok ()
            else Error Invalid_issuer)
  in
  (* Check aud claim *)
  let* () =
    match aud with
    | None -> Ok ()
    | Some expected_aud ->
        let actual_aud = Claims.aud claims in
        if List.mem expected_aud actual_aud then Ok ()
        else Error Invalid_audience
  in
  Ok ()

let verify_and_validate ~key ~now ?allow_none ?allowed_algs ?iss ?aud ?leeway t
    =
  let ( let* ) = Result.bind in
  let* () = verify ~key ?allow_none ?allowed_algs t in
  validate ~now ?iss ?aud ?leeway t

(* Creation *)
let create ~header ~claims ~key =
  let ( let* ) = Result.bind in
  let header_json = Header.to_json header in
  let claims_json = Claims.to_json claims in
  let header_b64 = base64url_encode header_json in
  let payload_b64 = base64url_encode claims_json in
  let signing_input = header_b64 ^ "." ^ payload_b64 in
  let* signature =
    match (header.Header.alg, key.Jwk.key_data) with
    | Algorithm.None, _ -> Ok ""
    | Algorithm.HS256, Jwk.Symmetric { k } ->
        Ok (Sign.hmac_sha256 ~key:k signing_input)
    | Algorithm.HS384, Jwk.Symmetric { k } ->
        Ok (Sign.hmac_sha384 ~key:k signing_input)
    | Algorithm.HS512, Jwk.Symmetric { k } ->
        Ok (Sign.hmac_sha512 ~key:k signing_input)
    | Algorithm.EdDSA, Jwk.Ed25519_priv { x = _; d } ->
        Sign.ed25519_sign ~priv:d signing_input
    | Algorithm.ES256, Jwk.P256_priv { x = _; y = _; d } ->
        Sign.p256_sign ~priv:d signing_input
    | Algorithm.ES384, Jwk.P384_priv { x = _; y = _; d } ->
        Sign.p384_sign ~priv:d signing_input
    | Algorithm.ES512, Jwk.P521_priv { x = _; y = _; d } ->
        Sign.p521_sign ~priv:d signing_input
    | alg, _ ->
        Error
          (Key_type_mismatch
             (Printf.sprintf "Cannot sign with algorithm %s and given key"
                (Algorithm.to_string alg)))
  in
  let sig_b64 = base64url_encode signature in
  let raw = signing_input ^ "." ^ sig_b64 in
  Ok { header; claims; signature; raw }

let encode t = t.raw

(* Utilities *)
let is_expired ~now ?(leeway = Ptime.Span.zero) t =
  match Claims.exp t.claims with
  | None -> false
  | Some exp_time ->
      let exp_with_leeway =
        Ptime.add_span exp_time leeway |> Option.value ~default:exp_time
      in
      Ptime.is_later now ~than:exp_with_leeway

let time_to_expiry ~now t =
  match Claims.exp t.claims with
  | None -> None
  | Some exp_time ->
      let diff = Ptime.diff exp_time now in
      if Ptime.Span.compare diff Ptime.Span.zero <= 0 then None else Some diff

module Cwt = Cwt
(** CBOR Web Token (CWT) support *)
