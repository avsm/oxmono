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

let error_to_string e = Format.asprintf "%a" pp_error e

(* Base64url encoding/decoding per RFC 7515 Appendix C *)
let base64url_encode s =
  Base64.encode_string ~pad:false ~alphabet:Base64.uri_safe_alphabet s

let base64url_decode s =
  let alphabet = function
    | 'A' .. 'Z' | 'a' .. 'z' | '0' .. '9' | '-' | '_' -> true
    | _ -> false in
  if not (String.for_all alphabet s) then
    Error (Invalid_base64url "expected unpadded base64url")
  else match Base64.decode ~pad:false ~alphabet:Base64.uri_safe_alphabet s with
    | Ok raw when base64url_encode raw = s -> Ok raw
    | Ok _ -> Error (Invalid_base64url "non-canonical trailing bits")
    | Error (`Msg m) -> Error (Invalid_base64url m)

let validate_string_or_uri s =
  if not (String.is_valid_utf_8 s) then Error (Invalid_uri "invalid UTF-8")
  else if not (String.contains s ':') then Ok s
  else match Uriz.of_string__local s with
    | This uri when Uriz.scheme uri <> Null -> Ok s
    | This _ | Null -> Error (Invalid_uri s)

module Json = struct
  exception Invalid of error
  let fail e = raise (Invalid e)
  let ( let* ) = Result.bind
  let meta = Jsont.Meta.none
  let string s = Jsont.String (s, meta)
  let mem name v = (name, meta), v
  let get name members =
    List.find_map (fun ((key, _), value) ->
      if name = key then Some value else None) members

  let[@zero_alloc] within_depth s =
    let mutable depth = 0 in
    let mutable quoted = false in
    let mutable escaped = false in
    let mutable valid = true in
    for i = 0 to String.length s - 1 do
      let c = String.unsafe_get s i in
      if quoted then (
        if escaped then escaped <- false
        else if c = '\\' then escaped <- true
        else if c = '"' then quoted <- false)
      else if c = '"' then quoted <- true
      else if c = '{' || c = '[' then (
        depth <- depth + 1;
        if depth > 32 then valid <- false)
      else if c = '}' || c = ']' then depth <- depth - 1
    done;
    valid

  let rec validate depth = function
    | _ when depth > 32 -> fail (Invalid_json "JSON nesting exceeds 32")
    | Jsont.Object (members, _) ->
        let names = List.map (fun ((name, _), _) -> name) members in
        let sorted = List.sort String.compare names in
        let rec unique = function
          | a :: (b :: _ as rest) ->
              if a = b then fail (Duplicate_claim a);
              unique rest
          | _ -> () in
        unique sorted;
        List.iter (fun (_, value) -> validate (depth + 1) value) members
    | Jsont.Array (values, _) -> List.iter (validate (depth + 1)) values
    | Jsont.Number (n, _) when not (Float.is_finite n) ->
        fail (Invalid_json "non-finite JSON number")
    | Jsont.String (s, _) when not (String.is_valid_utf_8 s) ->
        fail (Invalid_json "invalid UTF-8")
    | _ -> ()

  let object_of_string s =
    if String.length s > 65536 || not (within_depth s) then
      Error (Invalid_json "JSON size or nesting limit")
    else match Jsont_bytesrw.decode_string Jsont.json s with
      | Error e -> Error (Invalid_json e)
      | Ok (Jsont.Object (members, _) as json) ->
          (try validate 0 json; Ok members with Invalid e -> Error e)
      | Ok _ -> Error (Invalid_json "expected a JSON object")

  let encode members =
    let json = Jsont.Object (members, meta) in
    (try validate 0 json with Invalid e -> invalid_arg (error_to_string e));
    match Jsont_bytesrw.encode_string Jsont.json json with
    | Ok s when String.length s <= 65536 -> s
    | Ok _ -> invalid_arg "JSONWT JSON size limit"
    | Error e -> invalid_arg e

  let optional_string members name =
    match get name members with
    | None -> Ok None
    | Some (Jsont.String (s, _)) -> Ok (Some s)
    | Some _ -> Error (Invalid_json (name ^ " must be a string"))

  let required_string members name =
    let* value = optional_string members name in
    match value with
    | Some s -> Ok s
    | None -> Error (Invalid_json (name ^ " is required"))

  let string_array = function
    | Jsont.Array (values, _) ->
        let rec loop acc = function
          | [] -> Ok (List.rev acc)
          | Jsont.String (s, _) :: rest -> loop (s :: acc) rest
          | _ -> Error (Invalid_json "expected an array of strings") in
        loop [] values
    | _ -> Error (Invalid_json "expected an array of strings")
end

external secp256k1_point : string -> string option @@ portable
  = "jsonwt_secp256k1_point"
external verify_es256k : string -> string -> int -> string -> bool @@ portable
  = "jsonwt_verify_es256k" [@@noalloc]

(* Algorithm module *)
external hmac : int -> string -> string -> int -> string @@ portable
  = "jsonwt_hmac"

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
    | ES256K
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
    | ES256K -> "ES256K"
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
    | "ES256K" -> Ok ES256K
    | "ES256" -> Ok ES256
    | "ES384" -> Ok ES384
    | "ES512" -> Ok ES512
    | "EdDSA" -> Ok EdDSA
    | s -> Error (Unsupported_algorithm s)

  let all =
    [ HS256; HS384; HS512; ES256; ES256K; ES384; ES512; EdDSA ]

  let all_with_none = None :: all
end

module Jwk = struct
  type kty = Oct | Ec | Okp
  type crv = P256 | P384 | P521 | Secp256k1 | Ed25519
  type key_data =
    | Symmetric of string
    | Ed of { public : string; private_ : string option }
    | Ec_key of { curve : crv; public : string; private_ : string option }
  type t = {
    key_data : key_data;
    kid : string option;
    alg : Algorithm.t option;
    key_ops : string list option;
  }
  let make ?alg key_data = { key_data; kid = None; alg; key_ops = None }
  let symmetric k = make (Symmetric k)
  let kid t = t.kid
  let alg t = t.alg
  let with_kid kid t =
    if not (String.is_valid_utf_8 kid) then invalid_arg "JSONWT key ID";
    { t with kid = Some kid }
  let kty t = match t.key_data with
    | Symmetric _ -> Oct | Ed _ -> Okp | Ec_key _ -> Ec
  let algorithm = function
    | P256 -> Algorithm.ES256 | P384 -> Algorithm.ES384
    | P521 -> Algorithm.ES512 | Secp256k1 -> Algorithm.ES256K
    | Ed25519 -> Algorithm.EdDSA
  let width = function
    | P256 | Secp256k1 | Ed25519 -> 32 | P384 -> 48 | P521 -> 66
  let compatible alg = function
    | Symmetric _ -> List.mem alg
        [Algorithm.HS256; Algorithm.HS384; Algorithm.HS512]
    | Ed _ -> alg = Algorithm.EdDSA
    | Ec_key { curve; _ } -> alg = algorithm curve
  let with_alg alg t =
    if not (compatible alg t.key_data) then invalid_arg "JSONWT key algorithm";
    { t with alg = Some alg }
  let permits operation t = match t.key_ops with
    | None -> true | Some ops -> List.mem operation ops

  let ed25519_pub public =
    if String.length public <> 32 ||
       Result.is_error (Mirage_crypto_ec.Ed25519.pub_of_octets public)
    then invalid_arg "JSONWT Ed25519 public key";
    make ~alg:Algorithm.EdDSA (Ed { public; private_ = None })
  let ed25519_priv ~pub ~priv =
    let t = ed25519_pub pub in
    match Mirage_crypto_ec.Ed25519.priv_of_octets priv with
    | Error _ -> invalid_arg "JSONWT Ed25519 private key"
    | Ok key ->
        let actual = Mirage_crypto_ec.Ed25519.pub_of_priv key
            |> Mirage_crypto_ec.Ed25519.pub_to_octets in
        if String.length priv <> 32 || pub <> actual then
          invalid_arg "JSONWT Ed25519 key pair mismatch";
        { t with key_data = Ed { public = pub; private_ = Some priv } }

  let ec curve ~x ~y private_ =
    let size = width curve in
    if String.length x <> size || String.length y <> size then
      invalid_arg "JSONWT EC coordinate length";
    let public = "\004" ^ x ^ y in
    let valid = match curve with
      | P256 -> Result.is_ok (Mirage_crypto_ec.P256.Dsa.pub_of_octets public)
      | P384 -> Result.is_ok (Mirage_crypto_ec.P384.Dsa.pub_of_octets public)
      | P521 -> Result.is_ok (Mirage_crypto_ec.P521.Dsa.pub_of_octets public)
      | Secp256k1 -> secp256k1_point public <> None
      | Ed25519 -> false in
    if not valid then invalid_arg "JSONWT EC public key";
    (match private_ with
     | None -> ()
     | Some d ->
         if String.length d <> size then invalid_arg "JSONWT EC scalar length";
         let actual = match curve with
           | P256 -> Result.map (fun k ->
               Mirage_crypto_ec.P256.Dsa.pub_of_priv k
               |> Mirage_crypto_ec.P256.Dsa.pub_to_octets)
               (Mirage_crypto_ec.P256.Dsa.priv_of_octets d)
           | P384 -> Result.map (fun k ->
               Mirage_crypto_ec.P384.Dsa.pub_of_priv k
               |> Mirage_crypto_ec.P384.Dsa.pub_to_octets)
               (Mirage_crypto_ec.P384.Dsa.priv_of_octets d)
           | P521 -> Result.map (fun k ->
               Mirage_crypto_ec.P521.Dsa.pub_of_priv k
               |> Mirage_crypto_ec.P521.Dsa.pub_to_octets)
               (Mirage_crypto_ec.P521.Dsa.priv_of_octets d)
           | Secp256k1 | Ed25519 -> invalid_arg "JSONWT unsupported private key"
         in
         if actual <> Ok public then invalid_arg "JSONWT EC key pair mismatch");
    make ~alg:(algorithm curve) (Ec_key { curve; public; private_ })
  let p256_pub ~x ~y = ec P256 ~x ~y None
  let p256_priv ~x ~y ~d = ec P256 ~x ~y (Some d)
  let p384_pub ~x ~y = ec P384 ~x ~y None
  let p384_priv ~x ~y ~d = ec P384 ~x ~y (Some d)
  let p521_pub ~x ~y = ec P521 ~x ~y None
  let p521_priv ~x ~y ~d = ec P521 ~x ~y (Some d)
  let secp256k1_pub public =
    match secp256k1_point public with
    | None -> Error (Key_type_mismatch "invalid secp256k1 public key")
    | Some public ->
        Ok (make ~alg:Algorithm.ES256K
          (Ec_key { curve = Secp256k1; public; private_ = None }))

  let of_json s =
    let ( let* ) = Result.bind in
    let* members = Json.object_of_string s in
    let* () =
      if List.exists (fun name -> Json.get name members <> None)
          ["x5u"; "x5c"; "x5t"; "x5t#S256"] then
        Error (Invalid_json "certificate-bound JWKs are unsupported")
      else Ok () in
    let* kty = Json.required_string members "kty" in
    let* kid = Json.optional_string members "kid" in
    let* alg_name = Json.optional_string members "alg" in
    let* alg = match alg_name with
      | None -> Ok None
      | Some s -> Result.map Option.some (Algorithm.of_string s) in
    let* use = Json.optional_string members "use" in
    let* key_ops = match Json.get "key_ops" members with
      | None -> Ok None
      | Some v -> Result.map Option.some (Json.string_array v) in
    let* () = if use <> None && use <> Some "sig" then
        Error (Invalid_json "JWK use must be sig")
      else match key_ops with
        | Some ops when List.length ops <>
            List.length (List.sort_uniq String.compare ops) ||
            not (List.for_all (fun op -> op = "sign" || op = "verify") ops) ->
            Error (Invalid_json "invalid JWK key_ops")
        | _ -> Ok () in
    let octets name =
      let* value = Json.required_string members name in
      base64url_decode value in
    let optional_octets name =
      let* value = Json.optional_string members name in
      match value with None -> Ok None
      | Some v -> Result.map Option.some (base64url_decode v) in
    try
      let* t = match kty with
        | "oct" -> let* k = octets "k" in Ok (symmetric k)
        | "OKP" ->
            let* curve = Json.required_string members "crv" in
            if curve <> "Ed25519" then Error (Invalid_json "unsupported OKP")
            else
              let* pub = octets "x" in
              let* priv = optional_octets "d" in
              Ok (match priv with None -> ed25519_pub pub
                | Some priv -> ed25519_priv ~pub ~priv)
        | "EC" ->
            let* curve = Json.required_string members "crv" in
            let* curve = match curve with
              | "P-256" -> Ok P256 | "P-384" -> Ok P384 | "P-521" -> Ok P521
              | "secp256k1" -> Ok Secp256k1
              | _ -> Error (Invalid_json "unsupported EC curve") in
            let* x = octets "x" in
            let* y = octets "y" in
            let* d = optional_octets "d" in
            Ok (ec curve ~x ~y d)
        | _ -> Error (Invalid_json "unsupported JWK key type") in
      let t = match alg with None -> t | Some alg -> with_alg alg t in
      Ok { t with kid; key_ops }
    with Invalid_argument e -> Error (Key_type_mismatch e)

  let to_json t =
    let str name value = Json.mem name (Json.string value) in
    let octets name value = str name (base64url_encode value) in
    let members = match t.key_data with
      | Symmetric k -> [str "kty" "oct"; octets "k" k]
      | Ed { public; private_ } ->
          [str "kty" "OKP"; str "crv" "Ed25519"; octets "x" public] @
          (match private_ with None -> [] | Some d -> [octets "d" d])
      | Ec_key { curve; public; private_ } ->
          let name = match curve with
            | P256 -> "P-256" | P384 -> "P-384" | P521 -> "P-521"
            | Secp256k1 -> "secp256k1" | Ed25519 -> assert false in
          let size = width curve in
          [str "kty" "EC"; str "crv" name;
           octets "x" (String.sub public 1 size);
           octets "y" (String.sub public (size + 1) size)] @
          (match private_ with None -> [] | Some d -> [octets "d" d]) in
    let members = members @
      (match t.kid with None -> [] | Some kid -> [str "kid" kid]) @
      (match t.alg with None -> [] | Some alg ->
        [str "alg" (Algorithm.to_string alg)]) @
      (match t.key_ops with None -> [] | Some ops ->
        [Json.mem "key_ops" (Jsont.Json.list (List.map Json.string ops))]) in
    Json.encode members
end

module Header = struct
  type t = {
    alg : Algorithm.t;
    typ : string option;
    kid : string option;
    cty : string option;
  }
  let make ?typ ?kid ?cty alg = { alg; typ; kid; cty }
  let is_nested t =
    match t.cty with Some s -> String.uppercase_ascii s = "JWT" | None -> false

  let of_json s =
    let ( let* ) = Result.bind in
    let* members = Json.object_of_string s in
    if Json.get "crit" members <> None || Json.get "b64" members <> None then
      Error (Invalid_header "JOSE extensions are unsupported")
    else
      let* name = Json.required_string members "alg" in
      let* alg = Algorithm.of_string name in
      let* typ = Json.optional_string members "typ" in
      let* kid = Json.optional_string members "kid" in
      let* cty = Json.optional_string members "cty" in
      let h = { alg; typ; kid; cty } in
      if is_nested h then Error (Invalid_header "nested JWTs are unsupported")
      else Ok h

  let to_json h =
    let add name value members = match value with
      | None -> members | Some s -> Json.mem name (Json.string s) :: members in
    let members = [Json.mem "alg" (Json.string (Algorithm.to_string h.alg))] in
    Json.encode (List.rev (members |> add "typ" h.typ |> add "kid" h.kid
      |> add "cty" h.cty))
end

module Claims = struct
  type t = ((string * Jsont.Meta.t) * Jsont.json) list
  type builder = t

  let get name t = Json.get name t
  let get_string name t = match get name t with
    | Some (Jsont.String (s, _)) -> Some s | _ -> None
  let get_number name t = match get name t with
    | Some (Jsont.Number (n, _)) -> Some n | _ -> None
  let get_int name t = match get_number name t with
    | Some n when Float.is_finite n && n = Float.floor n &&
                  n >= float_of_int min_int && n < -. float_of_int min_int ->
        Some (int_of_float n)
    | _ -> None
  let get_bool name t = match get name t with
    | Some (Jsont.Bool (b, _)) -> Some b | _ -> None
  let iss t = get_string "iss" t
  let sub t = get_string "sub" t
  let jti t = get_string "jti" t
  let aud t = match get "aud" t with
    | None -> []
    | Some (Jsont.String (s, _)) -> [s]
    | Some value -> Result.get_ok (Json.string_array value)
  let date name t = Option.bind (get_number name t) Ptime.of_float_s
  let exp t = date "exp" t
  let nbf t = date "nbf" t
  let iat t = date "iat" t

  let validate members =
    let ( let* ) = Result.bind in
    let rec strings = function
      | [] -> Ok ()
      | name :: rest ->
          let* value = Json.optional_string members name in
          let* () = match value with
            | Some s when name <> "jti" ->
                Result.map (fun _ -> ()) (validate_string_or_uri s)
            | _ -> Ok () in
          strings rest in
    let* () = strings ["iss"; "sub"; "jti"] in
    let* audiences = match get "aud" members with
      | None -> Ok []
      | Some (Jsont.String (s, _)) -> Ok [s]
      | Some value -> Json.string_array value in
    let rec uris = function
      | [] -> Ok ()
      | s :: rest -> let* _ = validate_string_or_uri s in uris rest in
    let* () = uris audiences in
    let rec dates = function
      | [] -> Ok members
      | name :: rest ->
          match get name members with
          | None -> dates rest
          | Some (Jsont.Number (n, _))
              when Float.is_finite n && Ptime.of_float_s n <> None -> dates rest
          | Some _ -> Error (Invalid_claims (name ^ " is not a NumericDate")) in
    dates ["exp"; "nbf"; "iat"]

  let of_json s =
    let ( let* ) = Result.bind in
    let* members = Json.object_of_string s in
    validate members
  let to_json = Json.encode
  let empty = []
  let set name value t =
    List.filter (fun ((key, _), _) -> name <> key) t @ [Json.mem name value]
  let set_string name value t = set name (Json.string value) t
  let set_int name value t = set name (Jsont.Json.int value) t
  let set_bool name value t = set name (Jsont.Json.bool value) t
  let set_iss value t = set_string "iss" value t
  let set_sub value t = set_string "sub" value t
  let set_jti value t = set_string "jti" value t
  let set_aud values t =
    let value = match values with
      | [s] -> Json.string s
      | ss -> Jsont.Json.list (List.map Json.string ss) in
    set "aud" value t
  let set_date name value t =
    set name (Jsont.Json.number (Ptime.to_float_s value)) t
  let set_exp value t = set_date "exp" value t
  let set_nbf value t = set_date "nbf" value t
  let set_iat value t = set_date "iat" value t
  let build t =
    match of_json (to_json t) with
    | Ok t -> t | Error e -> invalid_arg (error_to_string e)
end

(* JWT type *)
type t = {
  header : Header.t;
  claims : Claims.t;
  signature : string;
  raw : string;
  signing_length : int;
}

let header t = t.header
let claims t = t.claims
let signature t = t.signature
let raw t = t.raw

(* Retain offsets into the original token instead of splitting it into a
   list and copying its signing input again during every verification. *)
let[@zero_alloc] separators token =
  let mutable first = -1 in
  let mutable second = -1 in
  let mutable count = 0 in
  for i = 0 to String.length token - 1 do
    if String.unsafe_get token i = '.' then begin
      count <- count + 1;
      if count = 1 then first <- i else if count = 2 then second <- i
    end
  done;
  #(first, second, count)

let parse ?(max_size = 8192) token =
  let ( let* ) = Result.bind in
  if max_size < 0 || String.length token > max_size then
    Error (Invalid_structure "JWT size limit")
  else
    let #(first, second, count) = separators token in
    if count <> 2 || first = 0 || second = first + 1 then
      Error (Invalid_structure "expected three compact JWS components")
    else
      let* h = base64url_decode (String.sub token 0 first) in
      let* header = Header.of_json h in
      let* p = base64url_decode
          (String.sub token (first + 1) (second - first - 1)) in
      let* claims = Claims.of_json p in
      let* signature = base64url_decode
          (String.sub token (second + 1) (String.length token - second - 1)) in
      Ok { header; claims; signature; raw = token; signing_length = second }

(* Signature operations *)
module Sign = struct
  let hmac_sha256 ~key ~len data = hmac 256 key data len

  let hmac_sha384 ~key ~len data = hmac 384 key data len

  let hmac_sha512 ~key ~len data = hmac 512 key data len

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

  let p256_verify ~pub ~signature ~len data =
    if String.length signature <> 64 then Error Signature_mismatch
    else
      let r = String.sub signature 0 32 in
      let s = String.sub signature 32 32 in
      match Mirage_crypto_ec.P256.Dsa.pub_of_octets pub with
      | Error _ -> Error (Key_type_mismatch "Invalid P-256 public key")
      | Ok pub ->
          let hash =
            Digestif.SHA256.digest_string ~off:0 ~len data
            |> Digestif.SHA256.to_raw_string
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

  let p384_verify ~pub ~signature ~len data =
    if String.length signature <> 96 then Error Signature_mismatch
    else
      let r = String.sub signature 0 48 in
      let s = String.sub signature 48 48 in
      match Mirage_crypto_ec.P384.Dsa.pub_of_octets pub with
      | Error _ -> Error (Key_type_mismatch "Invalid P-384 public key")
      | Ok pub ->
          let hash =
            Digestif.SHA384.digest_string ~off:0 ~len data
            |> Digestif.SHA384.to_raw_string
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

  let p521_verify ~pub ~signature ~len data =
    if String.length signature <> 132 then Error Signature_mismatch
    else
      let r = String.sub signature 0 66 in
      let s = String.sub signature 66 66 in
      match Mirage_crypto_ec.P521.Dsa.pub_of_octets pub with
      | Error _ -> Error (Key_type_mismatch "Invalid P-521 public key")
      | Ok pub ->
          let hash =
            Digestif.SHA512.digest_string ~off:0 ~len data
            |> Digestif.SHA512.to_raw_string
          in
          let valid = Mirage_crypto_ec.P521.Dsa.verify ~key:pub (r, s) hash in
          if valid then Ok () else Error Signature_mismatch

end

let policy ~key ~allow_none ~allowed_algs ~operation alg =
  let name = Algorithm.to_string alg in
  if alg = Algorithm.None && not allow_none then Error Unsecured_not_allowed
  else if not (List.mem alg allowed_algs) then
    Error (Algorithm_not_allowed name)
  else if alg = Algorithm.None then Ok ()
  else if not (List.mem alg Algorithm.all) then
    Error (Unsupported_algorithm name)
  else if not (Jwk.compatible alg key.Jwk.key_data) ||
          (key.Jwk.alg <> Some alg) ||
          not (Jwk.permits operation key) then
    Error (Key_type_mismatch "key algorithm or operation is not permitted")
  else match key.Jwk.key_data with
    | Jwk.Symmetric k ->
        let minimum = match alg with
          | Algorithm.HS256 -> 32 | Algorithm.HS384 -> 48
          | Algorithm.HS512 -> 64 | _ -> assert false in
        if String.length k < minimum then
          Error (Key_type_mismatch "HMAC key is shorter than the hash output")
        else Ok ()
    | _ -> Ok ()

let verify ~key ?(allow_none = false) ~allowed_algs t =
  let ( let* ) = Result.bind in
  let* () = policy ~key ~allow_none ~allowed_algs ~operation:"verify"
      t.header.alg in
  let input = t.raw and len = t.signing_length in
  match t.header.alg, key.Jwk.key_data with
  | Algorithm.None, _ ->
      if t.signature = "" then Ok () else Error Signature_mismatch
  | Algorithm.HS256, Jwk.Symmetric k ->
      if Eqaf.equal (Sign.hmac_sha256 ~key:k ~len input) t.signature then Ok ()
      else Error Signature_mismatch
  | Algorithm.HS384, Jwk.Symmetric k ->
      if Eqaf.equal (Sign.hmac_sha384 ~key:k ~len input) t.signature then Ok ()
      else Error Signature_mismatch
  | Algorithm.HS512, Jwk.Symmetric k ->
      if Eqaf.equal (Sign.hmac_sha512 ~key:k ~len input) t.signature then Ok ()
      else Error Signature_mismatch
  | Algorithm.EdDSA, Jwk.Ed { public; _ } ->
      Sign.ed25519_verify ~pub:public ~signature:t.signature
        (String.sub input 0 len)
  | Algorithm.ES256, Jwk.Ec_key { public; _ } ->
      Sign.p256_verify ~pub:public ~signature:t.signature ~len input
  | Algorithm.ES384, Jwk.Ec_key { public; _ } ->
      Sign.p384_verify ~pub:public ~signature:t.signature ~len input
  | Algorithm.ES512, Jwk.Ec_key { public; _ } ->
      Sign.p521_verify ~pub:public ~signature:t.signature ~len input
  | Algorithm.ES256K, Jwk.Ec_key { public; _ } ->
      if verify_es256k public input len t.signature then Ok ()
      else Error Signature_mismatch
  | _ -> Error (Key_type_mismatch "unsupported algorithm/key combination")

let leeway_seconds leeway =
  if Ptime.Span.compare leeway Ptime.Span.zero < 0 then
    invalid_arg "JSONWT leeway must be nonnegative";
  Ptime.Span.to_float_s leeway

let validate ~now ?iss ?aud ?(leeway = Ptime.Span.zero) t =
  let now = Ptime.to_float_s now in
  let leeway = leeway_seconds leeway in
  let claims = t.claims in
  let ( let* ) = Result.bind in
  let* () = match Claims.get_number "exp" claims with
    | Some exp when now -. leeway >= exp -> Error Token_expired
    | _ -> Ok () in
  let* () = match Claims.get_number "nbf" claims with
    | Some nbf when now +. leeway < nbf -> Error Token_not_yet_valid
    | _ -> Ok () in
  let* () = match iss with
    | Some issuer when Claims.iss claims <> Some issuer -> Error Invalid_issuer
    | _ -> Ok () in
  match aud with
  | Some audience when not (List.mem audience (Claims.aud claims)) ->
      Error Invalid_audience
  | _ -> Ok ()

let verify_and_validate ~key ~now ?allow_none ~allowed_algs ?iss ?aud
    ?leeway t =
  let ( let* ) = Result.bind in
  let* () = verify ~key ?allow_none ~allowed_algs t in
  validate ~now ?iss ?aud ?leeway t

let create ?(allow_none = false) ~header ~claims ~key () =
  let ( let* ) = Result.bind in
  let* () = policy ~key ~allow_none ~allowed_algs:[header.Header.alg]
      ~operation:"sign" header.Header.alg in
  try
    let* header = Header.of_json (Header.to_json header) in
    let input = base64url_encode (Header.to_json header) ^ "." ^
      base64url_encode (Claims.to_json claims) in
    let len = String.length input in
    let* signature = match header.Header.alg, key.Jwk.key_data with
      | Algorithm.None, _ -> Ok ""
      | Algorithm.HS256, Jwk.Symmetric k ->
          Ok (Sign.hmac_sha256 ~key:k ~len input)
      | Algorithm.HS384, Jwk.Symmetric k ->
          Ok (Sign.hmac_sha384 ~key:k ~len input)
      | Algorithm.HS512, Jwk.Symmetric k ->
          Ok (Sign.hmac_sha512 ~key:k ~len input)
      | Algorithm.EdDSA, Jwk.Ed { private_ = Some priv; _ } ->
          Sign.ed25519_sign ~priv input
      | Algorithm.ES256, Jwk.Ec_key { private_ = Some priv; _ } ->
          Sign.p256_sign ~priv input
      | Algorithm.ES384, Jwk.Ec_key { private_ = Some priv; _ } ->
          Sign.p384_sign ~priv input
      | Algorithm.ES512, Jwk.Ec_key { private_ = Some priv; _ } ->
          Sign.p521_sign ~priv input
      | _ -> Error (Key_type_mismatch "no supported signing key") in
    let raw = input ^ "." ^ base64url_encode signature in
    parse ~max_size:(String.length raw) raw
  with Invalid_argument e -> Error (Invalid_structure e)

let encode t = t.raw

let is_expired ~now ?(leeway = Ptime.Span.zero) t =
  let leeway = leeway_seconds leeway in
  match Claims.get_number "exp" t.claims with
  | None -> false
  | Some exp -> Ptime.to_float_s now -. leeway >= exp

let time_to_expiry ~now t =
  match Claims.exp t.claims with
  | None -> None
  | Some exp ->
      let diff = Ptime.diff exp now in
      if Ptime.Span.compare diff Ptime.Span.zero <= 0 then None else Some diff
