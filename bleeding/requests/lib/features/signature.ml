(*---------------------------------------------------------------------------
  Copyright (c) 2025 Anil Madhavapeddy <anil@recoil.org>. All rights reserved.
  SPDX-License-Identifier: ISC
 ---------------------------------------------------------------------------*)

(** RFC 9421 HTTP Message Signatures *)

let src = Logs.Src.create "requests.signature" ~doc:"HTTP Message Signatures"
module Log = (val Logs.src_log src : Logs.LOG)

(* Result monad syntax for cleaner error handling *)
let ( let* ) = Result.bind

(* Helper to get current time from an Eio clock as Ptime.t *)
let now_ptime clock =
  let now = Eio.Time.now clock in
  match Ptime.of_float_s now with
  | Some t -> t
  | None ->
      (* Fallback: should not happen with valid system time *)
      Log.warn (fun m -> m "Failed to convert Eio clock time to Ptime, using epoch");
      Ptime.epoch

(* ========================================================================= *)
(* Algorithms                                                                *)
(* ========================================================================= *)

module Algorithm = struct
  type t = [
    | `Rsa_pss_sha512
    | `Rsa_v1_5_sha256
    | `Hmac_sha256
    | `Ecdsa_p256_sha256
    | `Ecdsa_p384_sha384
    | `Ed25519
  ]

  let to_string : t -> string = function
    | `Rsa_pss_sha512 -> "rsa-pss-sha512"
    | `Rsa_v1_5_sha256 -> "rsa-v1_5-sha256"
    | `Hmac_sha256 -> "hmac-sha256"
    | `Ecdsa_p256_sha256 -> "ecdsa-p256-sha256"
    | `Ecdsa_p384_sha384 -> "ecdsa-p384-sha384"
    | `Ed25519 -> "ed25519"

  let of_string : string -> t option = function
    | "rsa-pss-sha512" -> Some `Rsa_pss_sha512
    | "rsa-v1_5-sha256" -> Some `Rsa_v1_5_sha256
    | "hmac-sha256" -> Some `Hmac_sha256
    | "ecdsa-p256-sha256" -> Some `Ecdsa_p256_sha256
    | "ecdsa-p384-sha384" -> Some `Ecdsa_p384_sha384
    | "ed25519" -> Some `Ed25519
    | _ -> None

  let of_string_exn s =
    match of_string s with
    | Some a -> a
    | None -> invalid_arg ("Unknown algorithm: " ^ s)
end

(* ========================================================================= *)
(* Components                                                                *)
(* ========================================================================= *)

module Component = struct
  type derived = [
    | `Method
    | `Authority
    | `Path
    | `Query
    | `Query_param of string
    | `Target_uri
    | `Status
    | `Request_target
  ]

  type param = [
    | `Sf
    | `Key of string
    | `Bs
    | `Tr
    | `Req
  ]

  type t = [
    | `Derived of derived * param list
    | `Field of string * param list
  ]

  (* Constructors *)
  let method_ : t = `Derived (`Method, [])
  let authority : t = `Derived (`Authority, [])
  let path : t = `Derived (`Path, [])
  let query : t = `Derived (`Query, [])
  let query_param name : t = `Derived (`Query_param name, [])
  let target_uri : t = `Derived (`Target_uri, [])
  let status : t = `Derived (`Status, [])
  let request_target : t = `Derived (`Request_target, [])

  let field name : t = `Field (String.lowercase_ascii name, [])
  let field_sf name : t = `Field (String.lowercase_ascii name, [`Sf])
  let field_bs name : t = `Field (String.lowercase_ascii name, [`Bs])
  let field_key name ~key : t = `Field (String.lowercase_ascii name, [`Key key])
  let field_req name : t = `Field (String.lowercase_ascii name, [`Req])

  let content_type = field "content-type"
  let content_length = field "content-length"
  let content_digest = field "content-digest"
  let date = field "date"
  let host = field "host"

  let derived_to_string : derived -> string = function
    | `Method -> "@method"
    | `Authority -> "@authority"
    | `Path -> "@path"
    | `Query -> "@query"
    | `Query_param _ -> "@query-param"
    | `Target_uri -> "@target-uri"
    | `Status -> "@status"
    | `Request_target -> "@request-target"

  let param_to_sf : param -> string * Sf.item = function
    | `Sf -> ("sf", Sf.Boolean true)
    | `Key k -> ("key", Sf.String k)
    | `Bs -> ("bs", Sf.Boolean true)
    | `Tr -> ("tr", Sf.Boolean true)
    | `Req -> ("req", Sf.Boolean true)

  let param_to_string : param -> string = function
    | `Sf -> ";sf"
    | `Key k -> ";key=\"" ^ k ^ "\""
    | `Bs -> ";bs"
    | `Tr -> ";tr"
    | `Req -> ";req"

  let to_identifier : t -> string = function
    | `Derived (d, params) ->
        let base = derived_to_string d in
        let name_param = match d with
          | `Query_param name -> ";name=\"" ^ name ^ "\""
          | _ -> ""
        in
        "\"" ^ base ^ "\"" ^ name_param ^ String.concat "" (List.map param_to_string params)
    | `Field (name, params) ->
        "\"" ^ name ^ "\"" ^ String.concat "" (List.map param_to_string params)

  let to_sf_item : t -> Sf.item * Sf.parameters = function
    | `Derived (d, params) ->
        let base = derived_to_string d in
        let sf_params = List.map param_to_sf params in
        let sf_params = match d with
          | `Query_param name -> ("name", Sf.String name) :: sf_params
          | _ -> sf_params
        in
        (Sf.String base, sf_params)
    | `Field (name, params) ->
        let sf_params = List.map param_to_sf params in
        (Sf.String name, sf_params)

  let of_identifier str =
    (* Parse component identifier like "@method" or "content-type;sf" *)
    let parse_params rest =
      let rec loop acc s =
        if String.length s = 0 then Ok (List.rev acc)
        else if s.[0] = ';' then
          let s = String.sub s 1 (String.length s - 1) in
          if String.length s >= 2 && String.sub s 0 2 = "sf" then
            loop (`Sf :: acc) (String.sub s 2 (String.length s - 2))
          else if String.length s >= 2 && String.sub s 0 2 = "bs" then
            loop (`Bs :: acc) (String.sub s 2 (String.length s - 2))
          else if String.length s >= 2 && String.sub s 0 2 = "tr" then
            loop (`Tr :: acc) (String.sub s 2 (String.length s - 2))
          else if String.length s >= 3 && String.sub s 0 3 = "req" then
            loop (`Req :: acc) (String.sub s 3 (String.length s - 3))
          else if String.length s >= 4 && String.sub s 0 4 = "key=" then
            let s = String.sub s 4 (String.length s - 4) in
            if String.length s > 0 && s.[0] = '"' then
              match String.index_from_opt s 1 '"' with
              | Some idx ->
                  let key = String.sub s 1 (idx - 1) in
                  loop (`Key key :: acc) (String.sub s (idx + 1) (String.length s - idx - 1))
              | None -> Error "Unterminated key parameter"
            else Error "Expected quoted key value"
          else if String.length s >= 5 && String.sub s 0 5 = "name=" then
            (* Skip name parameter, handled separately for query-param *)
            let s = String.sub s 5 (String.length s - 5) in
            if String.length s > 0 && s.[0] = '"' then
              match String.index_from_opt s 1 '"' with
              | Some idx ->
                  loop acc (String.sub s (idx + 1) (String.length s - idx - 1))
              | None -> Error "Unterminated name parameter"
            else Error "Expected quoted name value"
          else Error ("Unknown parameter: " ^ s)
        else Error ("Unexpected character: " ^ s)
      in
      loop [] rest
    in
    (* Extract name parameter for query-param *)
    let extract_name s =
      match String.index_opt s ';' with
      | None -> (s, None)
      | Some idx ->
          let rest = String.sub s idx (String.length s - idx) in
          if String.length rest >= 6 &&
             String.sub rest 0 6 = ";name=" then
            let after = String.sub rest 6 (String.length rest - 6) in
            if String.length after > 0 && after.[0] = '"' then
              match String.index_from_opt after 1 '"' with
              | Some end_idx ->
                  let name = String.sub after 1 (end_idx - 1) in
                  let remaining = String.sub s 0 idx ^
                    String.sub after (end_idx + 1) (String.length after - end_idx - 1) in
                  (remaining, Some name)
              | None -> (s, None)
            else (s, None)
          else (s, None)
    in
    let (ident, query_name) = extract_name str in
    match String.index_opt ident ';' with
    | None ->
        if String.length ident > 0 && ident.[0] = '@' then
          match ident with
          | "@method" -> Ok method_
          | "@authority" -> Ok authority
          | "@path" -> Ok path
          | "@query" -> Ok query
          | "@query-param" ->
              (match query_name with
              | Some n -> Ok (query_param n)
              | None -> Error "@query-param requires name parameter")
          | "@target-uri" -> Ok target_uri
          | "@status" -> Ok status
          | "@request-target" -> Ok request_target
          | _ -> Error ("Unknown derived component: " ^ ident)
        else Ok (field ident)
    | Some idx ->
        let name = String.sub ident 0 idx in
        let params_str = String.sub ident idx (String.length ident - idx) in
        match parse_params params_str with
        | Error e -> Error e
        | Ok params ->
            if String.length name > 0 && name.[0] = '@' then
              match name with
              | "@method" -> Ok (`Derived (`Method, params))
              | "@authority" -> Ok (`Derived (`Authority, params))
              | "@path" -> Ok (`Derived (`Path, params))
              | "@query" -> Ok (`Derived (`Query, params))
              | "@query-param" ->
                  (match query_name with
                  | Some n -> Ok (`Derived (`Query_param n, params))
                  | None -> Error "@query-param requires name parameter")
              | "@target-uri" -> Ok (`Derived (`Target_uri, params))
              | "@status" -> Ok (`Derived (`Status, params))
              | "@request-target" -> Ok (`Derived (`Request_target, params))
              | _ -> Error ("Unknown derived component: " ^ name)
            else Ok (`Field (name, params))
end

(* ========================================================================= *)
(* Parameters                                                                *)
(* ========================================================================= *)

module Params = struct
  type t = {
    created : Ptime.t option;
    expires : Ptime.t option;
    nonce : string option;
    alg : Algorithm.t option;
    keyid : string option;
    tag : string option;
  }

  let empty = {
    created = None;
    expires = None;
    nonce = None;
    alg = None;
    keyid = None;
    tag = None;
  }

  let created time t = { t with created = Some time }
  let expires time t = { t with expires = Some time }
  let nonce value t = { t with nonce = Some value }
  let alg algorithm t = { t with alg = Some algorithm }
  let keyid id t = { t with keyid = Some id }
  let tag value t = { t with tag = Some value }

  let get_created t = t.created
  let get_expires t = t.expires
  let get_nonce t = t.nonce
  let get_alg t = t.alg
  let get_keyid t = t.keyid
  let get_tag t = t.tag

  let ptime_to_unix t =
    Int64.of_float (Ptime.to_float_s t)

  let to_sf_params t =
    List.filter_map Fun.id [
      Option.map (fun c -> ("created", Sf.Integer (ptime_to_unix c))) t.created;
      Option.map (fun e -> ("expires", Sf.Integer (ptime_to_unix e))) t.expires;
      Option.map (fun n -> ("nonce", Sf.String n)) t.nonce;
      Option.map (fun a -> ("alg", Sf.String (Algorithm.to_string a))) t.alg;
      Option.map (fun k -> ("keyid", Sf.String k)) t.keyid;
      Option.map (fun tag -> ("tag", Sf.String tag)) t.tag;
    ]

  let of_sf_params params =
    let get_int k =
      match List.assoc_opt k params with
      | Some (Sf.Integer i) -> Some (Ptime.of_float_s (Int64.to_float i))
      | _ -> None
    in
    let get_string k =
      match List.assoc_opt k params with
      | Some (Sf.String s) -> Some s
      | Some (Sf.Token s) -> Some s
      | _ -> None
    in
    {
      created = Option.join (get_int "created");
      expires = Option.join (get_int "expires");
      nonce = get_string "nonce";
      alg = Option.bind (get_string "alg") Algorithm.of_string;
      keyid = get_string "keyid";
      tag = get_string "tag";
    }
end

(* ========================================================================= *)
(* Key Material                                                              *)
(* ========================================================================= *)

module Key = struct
  type private_key =
    | Symmetric_priv of string
    | Ed25519_priv of string
    | P256_priv of Mirage_crypto_ec.P256.Dsa.priv
    | P384_priv of Mirage_crypto_ec.P384.Dsa.priv
    | Rsa_priv of Mirage_crypto_pk.Rsa.priv

  type public_key =
    | Symmetric_pub of string
    | Ed25519_pub of string
    | P256_pub of Mirage_crypto_ec.P256.Dsa.pub
    | P384_pub of Mirage_crypto_ec.P384.Dsa.pub
    | Rsa_pub of Mirage_crypto_pk.Rsa.pub

  type t = {
    priv : private_key option;
    pub : public_key option;
  }

  let symmetric secret = {
    priv = Some (Symmetric_priv secret);
    pub = Some (Symmetric_pub secret);
  }

  let ed25519 ~priv ~pub = {
    priv = Some (Ed25519_priv priv);
    pub = Some (Ed25519_pub pub);
  }

  let ed25519_priv priv = {
    priv = Some (Ed25519_priv priv);
    pub = None;
  }

  let ed25519_pub pub = {
    priv = None;
    pub = Some (Ed25519_pub pub);
  }

  let p256 ~priv = {
    priv = Some (P256_priv priv);
    pub = Some (P256_pub (Mirage_crypto_ec.P256.Dsa.pub_of_priv priv));
  }

  let p256_pub pub = {
    priv = None;
    pub = Some (P256_pub pub);
  }

  let p384 ~priv = {
    priv = Some (P384_priv priv);
    pub = Some (P384_pub (Mirage_crypto_ec.P384.Dsa.pub_of_priv priv));
  }

  let p384_pub pub = {
    priv = None;
    pub = Some (P384_pub pub);
  }

  let rsa ~priv = {
    priv = Some (Rsa_priv priv);
    pub = Some (Rsa_pub (Mirage_crypto_pk.Rsa.pub_of_priv priv));
  }

  let rsa_pub pub = {
    priv = None;
    pub = Some (Rsa_pub pub);
  }

  let can_sign t = Option.is_some t.priv
  let can_verify t = Option.is_some t.pub

  let algorithm t : Algorithm.t option =
    match t.priv, t.pub with
    | Some (Symmetric_priv _), _ | _, Some (Symmetric_pub _) -> Some `Hmac_sha256
    | Some (Ed25519_priv _), _ | _, Some (Ed25519_pub _) -> Some `Ed25519
    | Some (P256_priv _), _ | _, Some (P256_pub _) -> Some `Ecdsa_p256_sha256
    | Some (P384_priv _), _ | _, Some (P384_pub _) -> Some `Ecdsa_p384_sha384
    | Some (Rsa_priv _), _ | _, Some (Rsa_pub _) -> Some `Rsa_pss_sha512
    | None, None -> None
end

(* ========================================================================= *)
(* Context                                                                   *)
(* ========================================================================= *)

type request_ctx = {
  method_ : Method.t;
  uri : Uri.t;
  headers : Headers.t;
}

type response_ctx = {
  status : int;
  headers : Headers.t;
  request : request_ctx option;
}

module Context = struct
  type t = [
    | `Request of request_ctx
    | `Response of response_ctx
  ]

  let request ~method_ ~uri ~headers : t =
    `Request { method_; uri; headers }

  let response ~status ~headers ?request () : t =
    let req_ctx = match request with
      | Some (`Request r) -> Some r
      | Some (`Response _) -> None
      | None -> None
    in
    `Response { status; headers; request = req_ctx }
end

(* ========================================================================= *)
(* Content-Digest                                                            *)
(* ========================================================================= *)

module Content_digest = struct
  type algorithm = [ `Sha256 | `Sha512 ]

  let compute ~algorithm ~body =
    match algorithm with
    | `Sha256 ->
        let hash = Digestif.SHA256.digest_string body in
        let b64 = Base64.encode_string (Digestif.SHA256.to_raw_string hash) in
        "sha-256=:" ^ b64 ^ ":"
    | `Sha512 ->
        let hash = Digestif.SHA512.digest_string body in
        let b64 = Base64.encode_string (Digestif.SHA512.to_raw_string hash) in
        "sha-512=:" ^ b64 ^ ":"

  let add ~algorithm ~body headers =
    let value = compute ~algorithm ~body in
    Headers.set `Content_digest value headers

  let verify ~header ~body =
    (* Extract base64 value from :base64: format *)
    let extract_b64 rest =
      if String.length rest >= 2 && rest.[0] = ':' then
        let end_idx = String.rindex rest ':' in
        if end_idx > 0 then Some (String.sub rest 1 (end_idx - 1))
        else None
      else None
    in
    (* Verify a single digest entry *)
    let verify_one part =
      let part = String.trim part in
      let try_algorithm ~prefix ~hash_fn ~name =
        if String.length part > String.length prefix &&
           String.sub part 0 (String.length prefix) = prefix then
          let rest = String.sub part (String.length prefix)
            (String.length part - String.length prefix) in
          match extract_b64 rest with
          | None -> Some (Error "Invalid Content-Digest format")
          | Some b64 ->
              match Base64.decode b64 with
              | Error _ -> Some (Error "Invalid base64 in Content-Digest")
              | Ok expected ->
                  let actual = hash_fn body in
                  if Eqaf.equal expected actual then Some (Ok ())
                  else Some (Error ("Content-Digest mismatch (" ^ name ^ ")"))
        else None
      in
      match try_algorithm ~prefix:"sha-256="
              ~hash_fn:Digestif.SHA256.(fun s -> to_raw_string (digest_string s))
              ~name:"sha-256" with
      | Some r -> r
      | None ->
          match try_algorithm ~prefix:"sha-512="
                  ~hash_fn:Digestif.SHA512.(fun s -> to_raw_string (digest_string s))
                  ~name:"sha-512" with
          | Some r -> r
          | None -> Error ("Unknown Content-Digest algorithm: " ^ part)
    in
    let parts = String.split_on_char ',' header in
    let results = List.map verify_one parts in
    if List.exists Result.is_ok results then Ok ()
    else match results with
      | [] -> Error "Empty Content-Digest header"
      | err :: _ -> err
end

(* ========================================================================= *)
(* Signature Base Construction                                               *)
(* ========================================================================= *)

let resolve_component (ctx : Context.t) (component : Component.t) =
  let get_headers : Context.t -> Headers.t = function
    | `Request r -> r.headers
    | `Response r -> r.headers
  in
  let get_request_headers : Context.t -> Headers.t option = function
    | `Request r -> Some r.headers
    | `Response (r : response_ctx) -> Option.map (fun (req : request_ctx) -> req.headers) r.request
  in
  match component with
  | `Derived (d, params) ->
      let has_req = List.mem `Req params in
      let target_ctx : Context.t = if has_req then
        match ctx with
        | `Response { request = Some req; _ } -> `Request req
        | _ -> ctx
      else ctx
      in
      (match d with
      | `Method ->
          (match target_ctx with
          | `Request r -> Ok (Method.to_string r.method_)
          | `Response _ -> Error "Cannot resolve @method on response")
      | `Authority ->
          (match target_ctx with
          | `Request r ->
              let host = Uri.host r.uri |> Option.value ~default:"" in
              let port = Uri.port r.uri in
              let authority = match port with
                | Some p when p <> 80 && p <> 443 -> host ^ ":" ^ string_of_int p
                | _ -> host
              in
              Ok (String.lowercase_ascii authority)
          | `Response _ -> Error "Cannot resolve @authority on response")
      | `Path ->
          (match target_ctx with
          | `Request r ->
              let path = Uri.path r.uri in
              Ok (if path = "" then "/" else path)
          | `Response _ -> Error "Cannot resolve @path on response")
      | `Query ->
          (match target_ctx with
          | `Request r ->
              let query = Uri.query r.uri in
              let encoded = Uri.encoded_of_query query in
              Ok ("?" ^ encoded)
          | `Response _ -> Error "Cannot resolve @query on response")
      | `Query_param name ->
          (match target_ctx with
          | `Request r ->
              (match Uri.get_query_param r.uri name with
              | Some v -> Ok v
              | None -> Error ("Missing query parameter: " ^ name))
          | `Response _ -> Error "Cannot resolve @query-param on response")
      | `Target_uri ->
          (match target_ctx with
          | `Request r -> Ok (Uri.to_string r.uri)
          | `Response _ -> Error "Cannot resolve @target-uri on response")
      | `Status ->
          (match ctx with
          | `Response r -> Ok (string_of_int r.status)
          | `Request _ -> Error "Cannot resolve @status on request")
      | `Request_target ->
          (match target_ctx with
          | `Request r ->
              let path = Uri.path r.uri in
              let path = if path = "" then "/" else path in
              let query = Uri.query r.uri in
              if query = [] then Ok path
              else Ok (path ^ "?" ^ Uri.encoded_of_query query)
          | `Response _ -> Error "Cannot resolve @request-target on response"))
  | `Field (name, params) ->
      let has_req = List.mem `Req params in
      let headers = if has_req then get_request_headers ctx else Some (get_headers ctx) in
      match headers with
      | None -> Error ("Cannot resolve request-bound field: " ^ name)
      | Some hdrs ->
          let name_typed = Header_name.of_string name in
          match Headers.get name_typed hdrs with
          | None -> Error ("Missing header: " ^ name)
          | Some value ->
              (* Apply bs (byte sequence) parameter if present *)
              if List.mem `Bs params then
                Ok (":" ^ Base64.encode_string value ^ ":")
              else
                Ok value

let build_signature_base ~(components : Component.t list) ~(params : Params.t) (ctx : Context.t) =
  let component_lines = List.map (fun c ->
    match resolve_component ctx c with
    | Error e -> Error e
    | Ok value ->
        let id = Component.to_identifier c in
        Ok (id ^ ": " ^ value)
  ) components in
  (* Check for errors *)
  let errors = List.filter_map (function Error e -> Some e | Ok _ -> None) component_lines in
  if errors <> [] then Error (List.hd errors)
  else
    let lines = List.filter_map (function Ok l -> Some l | Error _ -> None) component_lines in
    (* Build @signature-params line *)
    let component_items = List.map Component.to_sf_item components in
    let sf_params = Params.to_sf_params params in
    let sig_params_il : Sf.inner_list = (component_items, sf_params) in
    let sig_params_str = Sf.inner_list_to_string sig_params_il in
    let sig_params_line = "\"@signature-params\": " ^ sig_params_str in
    let base = String.concat "\n" (lines @ [sig_params_line]) in
    Log.debug (fun m -> m "Signature base:\n%s" base);
    Ok base

(* ========================================================================= *)
(* Cryptographic Operations                                                  *)
(* ========================================================================= *)

(* RSA-PSS requires a functor instantiation *)
module Rsa_pss_sha512 = Mirage_crypto_pk.Rsa.PSS(Digestif.SHA512)

let sign_bytes ~(alg : Algorithm.t) ~(key : Key.t) (data : string) =
  match alg, key.priv with
  | `Hmac_sha256, Some (Key.Symmetric_priv secret) ->
      let mac = Digestif.SHA256.hmac_string ~key:secret data in
      Ok (Digestif.SHA256.to_raw_string mac)

  | `Ed25519, Some (Key.Ed25519_priv priv) ->
      (match Mirage_crypto_ec.Ed25519.priv_of_octets priv with
      | Error _ -> Error "Invalid Ed25519 private key"
      | Ok priv_key ->
          let sig_ = Mirage_crypto_ec.Ed25519.sign ~key:priv_key data in
          Ok sig_)

  | `Ecdsa_p256_sha256, Some (Key.P256_priv priv) ->
      let hash = Digestif.SHA256.(to_raw_string (digest_string data)) in
      let (r, s) = Mirage_crypto_ec.P256.Dsa.sign ~key:priv hash in
      (* Concatenate r and s for raw signature format *)
      Ok (r ^ s)

  | `Ecdsa_p384_sha384, Some (Key.P384_priv priv) ->
      let hash = Digestif.SHA384.(to_raw_string (digest_string data)) in
      let (r, s) = Mirage_crypto_ec.P384.Dsa.sign ~key:priv hash in
      Ok (r ^ s)

  | `Rsa_pss_sha512, Some (Key.Rsa_priv priv) ->
      let hash = Digestif.SHA512.(to_raw_string (digest_string data)) in
      let sig_ = Rsa_pss_sha512.sign ~key:priv (`Digest hash) in
      Ok sig_

  | `Rsa_v1_5_sha256, Some (Key.Rsa_priv priv) ->
      let hash = Digestif.SHA256.(to_raw_string (digest_string data)) in
      let sig_ = Mirage_crypto_pk.Rsa.PKCS1.sign ~hash:`SHA256 ~key:priv (`Digest hash) in
      Ok sig_

  | alg, None -> Error ("Missing private key for " ^ Algorithm.to_string alg)
  | _, _ -> Error "Key type mismatch for algorithm"

let verify_bytes ~(alg : Algorithm.t) ~(key : Key.t) ~(signature : string) (data : string) =
  match alg, key.pub with
  | `Hmac_sha256, Some (Key.Symmetric_pub secret) ->
      let expected = Digestif.SHA256.hmac_string ~key:secret data in
      let expected_str = Digestif.SHA256.to_raw_string expected in
      if Eqaf.equal signature expected_str then Ok ()
      else Error "HMAC signature mismatch"

  | `Ed25519, Some (Key.Ed25519_pub pub) ->
      (match Mirage_crypto_ec.Ed25519.pub_of_octets pub with
      | Error _ -> Error "Invalid Ed25519 public key"
      | Ok pub_key ->
          let valid = Mirage_crypto_ec.Ed25519.verify ~key:pub_key signature ~msg:data in
          if valid then Ok () else Error "Ed25519 signature verification failed")

  | `Ecdsa_p256_sha256, Some (Key.P256_pub pub) ->
      let hash = Digestif.SHA256.(to_raw_string (digest_string data)) in
      (* Split signature into r and s (each 32 bytes for P-256) *)
      if String.length signature <> 64 then Error "Invalid P-256 signature length"
      else
        let r = String.sub signature 0 32 in
        let s = String.sub signature 32 32 in
        let valid = Mirage_crypto_ec.P256.Dsa.verify ~key:pub (r, s) hash in
        if valid then Ok () else Error "ECDSA P-256 signature verification failed"

  | `Ecdsa_p384_sha384, Some (Key.P384_pub pub) ->
      let hash = Digestif.SHA384.(to_raw_string (digest_string data)) in
      (* Split signature into r and s (each 48 bytes for P-384) *)
      if String.length signature <> 96 then Error "Invalid P-384 signature length"
      else
        let r = String.sub signature 0 48 in
        let s = String.sub signature 48 48 in
        let valid = Mirage_crypto_ec.P384.Dsa.verify ~key:pub (r, s) hash in
        if valid then Ok () else Error "ECDSA P-384 signature verification failed"

  | `Rsa_pss_sha512, Some (Key.Rsa_pub pub) ->
      let hash = Digestif.SHA512.(to_raw_string (digest_string data)) in
      let valid = Rsa_pss_sha512.verify ~key:pub ~signature (`Digest hash) in
      if valid then Ok () else Error "RSA-PSS signature verification failed"

  | `Rsa_v1_5_sha256, Some (Key.Rsa_pub pub) ->
      let hash = Digestif.SHA256.(to_raw_string (digest_string data)) in
      let hashp = function `SHA256 -> true | _ -> false in
      let valid = Mirage_crypto_pk.Rsa.PKCS1.verify ~hashp ~key:pub
        ~signature (`Digest hash) in
      if valid then Ok () else Error "RSA-PKCS1 signature verification failed"

  | alg, None -> Error ("Missing public key for " ^ Algorithm.to_string alg)
  | _, _ -> Error "Key type mismatch for algorithm"

(* ========================================================================= *)
(* Configuration                                                             *)
(* ========================================================================= *)

type config = {
  key : Key.t;
  keyid : string option;
  components : Component.t list;
  tag : string option;
  include_created : bool;
  label : string;
}

let default_components = [
  Component.method_;
  Component.authority;
  Component.path;
]

let config ~key ?keyid ?(components = default_components) ?tag
    ?(include_created = true) ?(label = "sig1") () =
  { key; keyid; components; tag; include_created; label }

(* ========================================================================= *)
(* Signing                                                                   *)
(* ========================================================================= *)

type sign_error = [
  | `Key_algorithm_mismatch of string
  | `Missing_private_key
  | `Component_resolution_error of string
  | `Crypto_error of string
]

let sign_error_to_string : sign_error -> string = function
  | `Key_algorithm_mismatch s -> "Key/algorithm mismatch: " ^ s
  | `Missing_private_key -> "Missing private key"
  | `Component_resolution_error s -> "Component resolution error: " ^ s
  | `Crypto_error s -> "Cryptographic error: " ^ s

let sign ~clock ~(config : config) ~(context : Context.t) ~(headers : Headers.t) =
  if not (Key.can_sign config.key) then Error `Missing_private_key
  else
    let alg = Key.algorithm config.key |> Option.value ~default:`Ed25519 in
    let params =
      Params.empty
      |> (fun p -> if config.include_created then Params.created (now_ptime clock) p else p)
      |> Option.fold ~none:Fun.id ~some:Params.keyid config.keyid
      |> Option.fold ~none:Fun.id ~some:Params.tag config.tag
      |> Params.alg alg
    in
    let* base = Result.map_error (fun e -> `Component_resolution_error e)
        (build_signature_base ~components:config.components ~params context) in
    let* sig_bytes = Result.map_error (fun e -> `Crypto_error e)
        (sign_bytes ~alg ~key:config.key base) in
    (* Build headers *)
    let component_items = List.map Component.to_sf_item config.components in
    let sf_params = Params.to_sf_params params in
    let sig_input_il : Sf.inner_list = (component_items, sf_params) in
    let sig_input_header = Sf.dictionary_to_string [(config.label, Sf.Inner_list sig_input_il)] in
    let sig_header = Sf.dictionary_to_string [(config.label, Sf.Item (Sf.Byte_seq sig_bytes, []))] in
    Log.debug (fun m -> m "Signature-Input: %s" sig_input_header);
    Log.debug (fun m -> m "Signature: %s" sig_header);
    Ok (headers
        |> Headers.set `Signature_input sig_input_header
        |> Headers.set `Signature sig_header)

let sign_with_digest ~clock ~(config : config) ~(context : Context.t) ~(headers : Headers.t)
    ~(body : string) ~(digest_algorithm : Content_digest.algorithm) =
  (* Add Content-Digest header *)
  let headers = Content_digest.add ~algorithm:digest_algorithm ~body headers in
  (* Add content-digest to components if not already present *)
  let has_content_digest = List.exists (function
    | `Field ("content-digest", _) -> true
    | _ -> false
  ) config.components in
  let components = if has_content_digest then config.components
    else config.components @ [Component.content_digest]
  in
  let config = { config with components } in
  sign ~clock ~config ~context ~headers

(* ========================================================================= *)
(* Verification                                                              *)
(* ========================================================================= *)

type verify_error = [
  | `Missing_signature_header
  | `Missing_signature_input_header
  | `Invalid_signature_input of string
  | `Signature_label_not_found of string
  | `Key_algorithm_mismatch of string
  | `Missing_public_key
  | `Component_resolution_error of string
  | `Signature_mismatch
  | `Signature_expired
  | `Required_component_missing of string
  | `Crypto_error of string
]

let verify_error_to_string : verify_error -> string = function
  | `Missing_signature_header -> "Missing Signature header"
  | `Missing_signature_input_header -> "Missing Signature-Input header"
  | `Invalid_signature_input s -> "Invalid Signature-Input: " ^ s
  | `Signature_label_not_found s -> "Signature label not found: " ^ s
  | `Key_algorithm_mismatch s -> "Key/algorithm mismatch: " ^ s
  | `Missing_public_key -> "Missing public key"
  | `Component_resolution_error s -> "Component resolution error: " ^ s
  | `Signature_mismatch -> "Signature verification failed"
  | `Signature_expired -> "Signature has expired"
  | `Required_component_missing s -> "Required component missing from signature: " ^ s
  | `Crypto_error s -> "Cryptographic error: " ^ s

type verify_result = {
  label : string;
  keyid : string option;
  created : Ptime.t option;
  expires : Ptime.t option;
  verified_components : Component.t list;
}

let verify ~clock ~(key : Key.t) ?label ?max_age ?required_components
    ~(context : Context.t) ~(headers : Headers.t) () =
  (* Helper to require a value or return an error *)
  let require opt err = match opt with Some x -> Ok x | None -> Error err in
  (* Get current time from Eio clock *)
  let now = now_ptime clock in
  (* Parse components from SF items *)
  let parse_components items =
    List.filter_map (fun (item, item_params) ->
      match item with
      | Sf.String s ->
          let params_str = String.concat "" (List.map (fun (k, v) ->
            match v with
            | Sf.Boolean true -> ";" ^ k
            | Sf.String s -> ";" ^ k ^ "=\"" ^ s ^ "\""
            | _ -> ";" ^ k ^ "=" ^ Sf.item_to_string v
          ) item_params) in
          Result.to_option (Component.of_identifier (s ^ params_str))
      | _ -> None
    ) items
  in
  (* Check required components are present *)
  let check_required components =
    match required_components with
    | None -> Ok ()
    | Some req ->
        let missing = List.find_opt (fun rc ->
          not (List.exists (fun c ->
            Component.to_identifier c = Component.to_identifier rc
          ) components)
        ) req in
        match missing with
        | Some m -> Error (`Required_component_missing (Component.to_identifier m))
        | None -> Ok ()
  in
  (* Check signature hasn't expired (RFC 9421 Section 2.2.5) *)
  let check_expiration sig_params =
    match sig_params.Params.expires with
    | Some exp when Ptime.is_earlier exp ~than:now ->
        Log.debug (fun m -> m "Signature expired: expires=%a now=%a"
          Ptime.pp exp Ptime.pp now);
        Error `Signature_expired
    | _ -> Ok ()
  in
  (* Check signature isn't too old based on created timestamp *)
  let check_max_age sig_params =
    match max_age, sig_params.Params.created with
    | Some age, Some created ->
        (match Ptime.add_span created age with
        | Some limit when Ptime.is_earlier limit ~than:now ->
            Log.debug (fun m -> m "Signature too old: created=%a max_age=%a now=%a"
              Ptime.pp created Ptime.Span.pp age Ptime.pp now);
            Error `Signature_expired
        | _ -> Ok ())
    | _ -> Ok ()
  in
  (* Check signature was created in the past (not in the future) *)
  let check_not_future sig_params =
    match sig_params.Params.created with
    | Some created when Ptime.is_later created ~than:now ->
        (* Allow small clock skew of 60 seconds *)
        let skew = Ptime.Span.of_int_s 60 in
        (match Ptime.add_span now skew with
        | Some limit when Ptime.is_later created ~than:limit ->
            Log.warn (fun m -> m "Signature created in the future: created=%a now=%a"
              Ptime.pp created Ptime.pp now);
            Error `Signature_expired  (* Reuse error type for future signatures *)
        | _ -> Ok ())
    | _ -> Ok ()
  in
  (* Main verification flow using let* *)
  if not (Key.can_verify key) then Error `Missing_public_key
  else
    let* sig_header = require (Headers.get `Signature headers) `Missing_signature_header in
    let* sig_input_header = require (Headers.get `Signature_input headers) `Missing_signature_input_header in
    let* sig_dict = Result.map_error (fun e -> `Invalid_signature_input ("Signature: " ^ e))
        (Sf.parse_dictionary sig_header) in
    let* sig_input_dict = Result.map_error (fun e -> `Invalid_signature_input ("Signature-Input: " ^ e))
        (Sf.parse_dictionary sig_input_header) in
    let target_label = label |> Option.value ~default:(
      match sig_dict with (l, _) :: _ -> l | [] -> "sig1"
    ) in
    let* sig_member = require (List.assoc_opt target_label sig_dict)
        (`Signature_label_not_found target_label) in
    let* sig_bytes = match sig_member with
      | Sf.Item (Sf.Byte_seq s, _) -> Ok s
      | _ -> Error (`Invalid_signature_input "Expected byte sequence")
    in
    let* input_member = require (List.assoc_opt target_label sig_input_dict)
        (`Signature_label_not_found target_label) in
    let* (items, params) = match input_member with
      | Sf.Inner_list il -> Ok il
      | _ -> Error (`Invalid_signature_input "Expected inner list")
    in
    let components = parse_components items in
    let sig_params = Params.of_sf_params params in
    let alg = match sig_params.Params.alg, Key.algorithm key with
      | Some a, _ -> a
      | None, Some a -> a
      | None, None -> `Ed25519
    in
    let* () = check_required components in
    let* () = check_not_future sig_params in
    let* () = check_expiration sig_params in
    let* () = check_max_age sig_params in
    let* base = Result.map_error (fun e -> `Component_resolution_error e)
        (build_signature_base ~components ~params:sig_params context) in
    let* () = Result.map_error (fun e -> `Crypto_error e)
        (verify_bytes ~alg ~key ~signature:sig_bytes base) in
    Ok {
      label = target_label;
      keyid = sig_params.Params.keyid;
      created = sig_params.Params.created;
      expires = sig_params.Params.expires;
      verified_components = components;
    }

let verify_all ~clock ~key_resolver ?max_age ~(context : Context.t) ~(headers : Headers.t) () =
  match Headers.get `Signature headers, Headers.get `Signature_input headers with
  | None, _ -> Error `Missing_signature_header
  | _, None -> Error `Missing_signature_input_header
  | Some sig_header, Some sig_input_header ->
      match Sf.parse_dictionary sig_header, Sf.parse_dictionary sig_input_header with
      | Error e, _ -> Error (`Invalid_signature_input e)
      | _, Error e -> Error (`Invalid_signature_input e)
      | Ok sig_dict, Ok sig_input_dict ->
          let results = List.filter_map (fun (label, _) ->
            (* Get keyid from signature-input *)
            let keyid = match List.assoc_opt label sig_input_dict with
              | Some (Sf.Inner_list (_, params)) ->
                  (match List.assoc_opt "keyid" params with
                  | Some (Sf.String k) -> Some k
                  | _ -> None)
              | _ -> None
            in
            match keyid with
            | None -> None
            | Some kid ->
                match key_resolver kid with
                | None -> None
                | Some key ->
                    match verify ~clock ~key ~label ?max_age ~context ~headers () with
                    | Ok r -> Some (Ok r)
                    | Error e -> Some (Error e)
          ) sig_dict in
          let successes = List.filter_map (function Ok r -> Some r | _ -> None) results in
          let errors = List.filter_map (function Error e -> Some e | _ -> None) results in
          if successes <> [] then Ok successes
          else match errors with
            | e :: _ -> Error e
            | [] -> Error (`Signature_label_not_found "no signatures found")
