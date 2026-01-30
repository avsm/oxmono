(*---------------------------------------------------------------------------
  Copyright (c) 2025 Anil Madhavapeddy <anil@recoil.org>. All rights reserved.
  SPDX-License-Identifier: ISC
 ---------------------------------------------------------------------------*)

let src = Logs.Src.create "requests.auth" ~doc:"HTTP Authentication"
module Log = (val Logs.src_log src : Logs.LOG)

type t =
  | No_auth
  | Basic of { username : string; password : string }
  | Bearer of { token : string }
  | Bearer_form of { token : string }
    (** RFC 6750 Section 2.2: Bearer token in form-encoded body *)
  | Digest of { username : string; password : string }
  | Signature of Signature.config
    (** RFC 9421: HTTP Message Signatures *)
  | Custom of (Headers.t -> Headers.t)

(** Digest authentication challenge parsed from WWW-Authenticate header *)
type digest_challenge = {
  realm : string;
  nonce : string;
  qop : string option;
  algorithm : string;  (** MD5, SHA-256, etc. *)
  opaque : string option;
  stale : bool;
  userhash : bool;  (** RFC 7616: If true, hash the username *)
}

let none = No_auth

let basic ~username ~password = Basic { username; password }

let bearer ~token = Bearer { token }

let digest ~username ~password = Digest { username; password }

let signature config = Signature config

let custom f = Custom f

(** Check if a URL uses HTTPS scheme *)
let is_https url =
  let uri = Uri.of_string url in
  match Uri.scheme uri with
  | Some "https" -> true
  | _ -> false

(** Get the authentication type name for error messages *)
let auth_type_name = function
  | No_auth -> "None"
  | Basic _ -> "Basic"
  | Bearer _ -> "Bearer"
  | Bearer_form _ -> "Bearer (form)"
  | Digest _ -> "Digest"
  | Signature _ -> "Signature"
  | Custom _ -> "Custom"

(** Check if auth type requires HTTPS (per RFC 7617/6750).
    Basic, Bearer, and Digest send credentials that can be intercepted.
    Signature does not strictly require HTTPS as it provides its own integrity. *)
let requires_https = function
  | Basic _ | Bearer _ | Bearer_form _ | Digest _ -> true
  | No_auth | Signature _ | Custom _ -> false

(** Validate that sensitive authentication is used over HTTPS.
    Per RFC 7617 Section 4 (Basic) and RFC 6750 Section 5.1 (Bearer):
    These authentication methods MUST be used over TLS to prevent credential leakage.

    @param allow_insecure_auth If true, skip the check (for testing environments)
    @param url The request URL
    @param auth The authentication configuration
    @raise Error.Insecure_auth if auth requires HTTPS but URL is HTTP *)
let validate_secure_transport ?(allow_insecure_auth = false) ~url auth =
  if allow_insecure_auth then
    Log.warn (fun m -> m "allow_insecure_auth=true: skipping HTTPS check for %s auth"
      (auth_type_name auth))
  else if requires_https auth && not (is_https url) then begin
    Log.err (fun m -> m "%s authentication rejected over HTTP (use HTTPS or allow_insecure_auth=true)"
      (auth_type_name auth));
    raise (Error.err (Error.Insecure_auth {
      url;
      auth_type = auth_type_name auth
    }))
  end

let apply auth headers =
  match auth with
  | No_auth -> headers
  | Basic { username; password } ->
      Log.debug (fun m -> m "Applying basic authentication for user: %s" username);
      Headers.basic ~username ~password headers
  | Bearer { token } ->
      Log.debug (fun m -> m "Applying bearer token authentication");
      Headers.bearer token headers
  | Bearer_form { token = _ } ->
      Log.debug (fun m -> m "Bearer form auth - token goes in body, not headers");
      (* Bearer form auth puts token in body, not headers.
         Use get_bearer_form_body to get the body content. *)
      headers
  | Digest { username; password = _ } ->
      Log.debug (fun m -> m "Digest auth configured for user: %s (requires server challenge)" username);
      (* Digest auth requires server challenge first, handled elsewhere *)
      headers
  | Signature _ ->
      Log.debug (fun m -> m "Signature auth configured (requires request context)");
      (* Signature auth requires request context (method, URI) to compute.
         Handled separately in request flow via apply_signature. *)
      headers
  | Custom f ->
      Log.debug (fun m -> m "Applying custom authentication handler");
      f headers

(** Apply authentication with HTTPS validation.
    This is the secure version that checks transport security before applying auth.

    @param allow_insecure_auth If true, allow auth over HTTP (not recommended)
    @param url The request URL (used for security check)
    @param auth The authentication to apply
    @param headers The headers to modify *)
let apply_secure ?(allow_insecure_auth = false) ~url auth headers =
  validate_secure_transport ~allow_insecure_auth ~url auth;
  apply auth headers

(** {1 Digest Authentication Implementation} *)

(** Parse WWW-Authenticate header for Digest challenge *)
let parse_www_authenticate header =
  Log.debug (fun m -> m "Parsing WWW-Authenticate: %s" header);
  let header = String.trim header in
  if not (String.length header >= 7 &&
          String.lowercase_ascii (String.sub header 0 7) = "digest ") then begin
    Log.debug (fun m -> m "Not a Digest challenge");
    None
  end
  else
    let params = String.sub header 7 (String.length header - 7) in
    (* Parse key=value or key="value" pairs, separated by commas *)
    let pairs =
      let rec parse_pairs acc str =
        let str = String.trim str in
        if str = "" then List.rev acc
        else
          match String.index_opt str '=' with
          | None -> List.rev acc
          | Some eq_idx ->
              let key = String.trim (String.sub str 0 eq_idx) in
              let rest = String.sub str (eq_idx + 1) (String.length str - eq_idx - 1) in
              let rest = String.trim rest in
              let value, remaining =
                if String.length rest > 0 && rest.[0] = '"' then
                  (* Quoted value *)
                  match String.index_from_opt rest 1 '"' with
                  | Some end_quote ->
                      let v = String.sub rest 1 (end_quote - 1) in
                      let rem = String.sub rest (end_quote + 1) (String.length rest - end_quote - 1) in
                      let rem = String.trim rem in
                      let rem = if String.length rem > 0 && rem.[0] = ',' then
                        String.sub rem 1 (String.length rem - 1)
                      else rem in
                      (v, rem)
                  | None -> (rest, "")
                else
                  (* Unquoted value *)
                  match String.index_opt rest ',' with
                  | Some comma ->
                      let v = String.trim (String.sub rest 0 comma) in
                      let rem = String.sub rest (comma + 1) (String.length rest - comma - 1) in
                      (v, rem)
                  | None -> (String.trim rest, "")
              in
              parse_pairs ((String.lowercase_ascii key, value) :: acc) remaining
      in
      parse_pairs [] params
    in
    (* Extract required fields *)
    match List.assoc_opt "realm" pairs, List.assoc_opt "nonce" pairs with
    | Some realm, Some nonce ->
        let challenge = {
          realm;
          nonce;
          qop = List.assoc_opt "qop" pairs;
          algorithm = List.assoc_opt "algorithm" pairs |> Option.value ~default:"MD5";
          opaque = List.assoc_opt "opaque" pairs;
          stale = List.assoc_opt "stale" pairs = (Some "true");
          userhash = List.assoc_opt "userhash" pairs = (Some "true");
        } in
        Log.debug (fun m -> m "Parsed Digest challenge: realm=%s nonce=%s algorithm=%s userhash=%b"
          challenge.realm challenge.nonce challenge.algorithm challenge.userhash);
        Option.some challenge
    | _ ->
        Log.warn (fun m -> m "Digest challenge missing required fields (realm/nonce)");
        Option.none

(** Hash function based on algorithm.
    Supports MD5 (default), SHA-256, and SHA-512 per RFC 7616.
    @raise Error.Authentication_failed if an unsupported algorithm is requested *)
let hash_string ~algorithm s =
  match String.uppercase_ascii algorithm with
  | "MD5" | "MD5-SESS" ->
      Digestif.MD5.(to_hex (digest_string s))
  | "SHA-256" | "SHA256" | "SHA-256-SESS" ->
      Digestif.SHA256.(to_hex (digest_string s))
  | "SHA-512" | "SHA512" ->
      Digestif.SHA512.(to_hex (digest_string s))
  | "SHA-512-256" | "SHA512-256" ->
      (* SHA-512/256 requires specific initialization vectors that differ from
         standard SHA-512. Truncating SHA-512 output is cryptographically incorrect.
         This algorithm is rarely used; recommend SHA-256 instead. *)
      Log.err (fun m -> m "SHA-512-256 algorithm not supported (requires special IVs)");
      raise (Error.err (Error.Authentication_failed {
        url = "";
        reason = "Digest algorithm SHA-512-256 is not supported. Server should offer SHA-256 or MD5."
      }))
  | other ->
      (* RFC 7616: Unknown algorithms should be rejected to prevent security downgrades.
         Silent fallback to MD5 could mask server misconfigurations. *)
      Log.err (fun m -> m "Unknown digest algorithm '%s'" other);
      raise (Error.err (Error.Authentication_failed {
        url = "";
        reason = Printf.sprintf "Unknown digest algorithm '%s'. Supported: MD5, SHA-256, SHA-512." other
      }))

(** Generate a random client nonce *)
let generate_cnonce () =
  let bytes = Mirage_crypto_rng.generate 8 in
  (* Convert bytes to hex string *)
  let hex_of_char c =
    let n = Char.code c in
    Printf.sprintf "%02x" n
  in
  String.concat "" (List.init (String.length bytes) (fun i -> hex_of_char bytes.[i]))

(** Check if algorithm is a -sess variant *)
let is_sess_algorithm algorithm =
  let alg = String.uppercase_ascii algorithm in
  String.ends_with ~suffix:"-SESS" alg

(** Compute digest response according to RFC 7616.

    @param body Optional request body for auth-int qop (body hash included in HA2) *)
let compute_digest_response ~username ~password ~method_ ~uri ~challenge ~nc ~cnonce ?body () =
  let algorithm = challenge.algorithm in
  (* HA1 calculation differs for -sess algorithms (RFC 7616 Section 3.4.2) *)
  let ha1_base = hash_string ~algorithm
    (Printf.sprintf "%s:%s:%s" username challenge.realm password) in
  let ha1 =
    if is_sess_algorithm algorithm then
      (* For -sess: HA1 = hash(hash(username:realm:password):nonce:cnonce) *)
      hash_string ~algorithm (Printf.sprintf "%s:%s:%s" ha1_base challenge.nonce cnonce)
    else
      ha1_base
  in
  (* Determine which qop to use *)
  let selected_qop = match challenge.qop with
    | Some qop ->
      let qop_parts = String.split_on_char ',' qop |> List.map String.trim in
      (* Prefer auth-int if body is provided and available, else auth *)
      if List.mem "auth-int" qop_parts && Option.is_some body then
        Some "auth-int"
      else if List.mem "auth" qop_parts then
        Some "auth"
      else if qop_parts <> [] then
        Some (List.hd qop_parts)
      else
        None
    | None -> None
  in
  (* HA2 depends on qop *)
  let ha2 = match selected_qop, body with
    | Some "auth-int", Some body_content ->
      (* HA2 = hash(method:uri:hash(body)) for auth-int *)
      let body_hash = hash_string ~algorithm body_content in
      hash_string ~algorithm (Printf.sprintf "%s:%s:%s" method_ uri body_hash)
    | _ ->
      (* HA2 = hash(method:uri) for auth or no qop *)
      hash_string ~algorithm (Printf.sprintf "%s:%s" method_ uri)
  in
  (* Response depends on qop *)
  let response, actual_qop = match selected_qop with
    | Some qop ->
        (* qop present: hash(HA1:nonce:nc:cnonce:qop:HA2) *)
        let resp = hash_string ~algorithm
          (Printf.sprintf "%s:%s:%s:%s:%s:%s"
             ha1 challenge.nonce nc cnonce qop ha2) in
        (resp, Some qop)
    | None ->
        (* No qop: hash(HA1:nonce:HA2) *)
        let resp = hash_string ~algorithm
          (Printf.sprintf "%s:%s:%s" ha1 challenge.nonce ha2) in
        (resp, None)
  in
  Log.debug (fun m -> m "Computed digest response for user %s (qop=%s)"
    username (Option.value ~default:"none" actual_qop));
  (response, actual_qop)

(** Build the Authorization header value for Digest auth.
    @param actual_qop The qop that was actually used (auth or auth-int) *)
let build_digest_header ~username ~uri ~challenge ~nc ~cnonce ~response ~actual_qop =
  (* RFC 7616 Section 3.4.4: userhash support *)
  let username_value, userhash_param =
    if challenge.userhash then
      let hashed = hash_string ~algorithm:challenge.algorithm
        (Printf.sprintf "%s:%s" username challenge.realm) in
      (hashed, Some "userhash=true")
    else
      (username, None)
  in
  let parts = [
    Printf.sprintf "username=\"%s\"" username_value;
    Printf.sprintf "realm=\"%s\"" challenge.realm;
    Printf.sprintf "nonce=\"%s\"" challenge.nonce;
    Printf.sprintf "uri=\"%s\"" uri;
    Printf.sprintf "algorithm=%s" challenge.algorithm;
    Printf.sprintf "response=\"%s\"" response;
  ] in
  let parts = match userhash_param with
    | Some p -> parts @ [p]
    | None -> parts
  in
  let parts = match actual_qop with
    | Some qop -> parts @ [
        Printf.sprintf "qop=%s" qop;
        Printf.sprintf "nc=%s" nc;
        Printf.sprintf "cnonce=\"%s\"" cnonce;
      ]
    | None -> parts
  in
  let parts = match challenge.opaque with
    | Some o -> parts @ [Printf.sprintf "opaque=\"%s\"" o]
    | None -> parts
  in
  "Digest " ^ String.concat ", " parts

(** {1 Nonce Count Tracking}

    Per RFC 7616, the nonce count (nc) must be incremented for each request
    using the same server nonce to prevent replay attacks. *)

module Nonce_counter = struct
  (** Mutable nonce count tracker, keyed by server nonce *)
  type t = (string, int) Hashtbl.t

  let create () : t = Hashtbl.create 16

  (** Get and increment the nonce count for a given server nonce.
      Returns the count formatted as 8 hex digits (e.g., "00000001"). *)
  let next (t : t) ~nonce =
    let count = match Hashtbl.find_opt t nonce with
      | Some c -> c + 1
      | None -> 1
    in
    Hashtbl.replace t nonce count;
    Printf.sprintf "%08x" count

  (** Clear all tracked nonces (e.g., on session reset) *)
  let clear (t : t) = Hashtbl.clear t
end

(** Apply Digest authentication given a challenge.
    @param nonce_counter Optional nonce counter for replay protection.
           If provided, the nonce count is tracked and incremented per-nonce.
           If not provided, defaults to "00000001" (single-request mode).
    @param body Optional request body for auth-int qop support. *)
let apply_digest ?nonce_counter ?body ~username ~password ~method_ ~uri ~challenge headers =
  let nc = match nonce_counter with
    | Some counter -> Nonce_counter.next counter ~nonce:challenge.nonce
    | None -> "00000001"
  in
  let cnonce = generate_cnonce () in
  let response, actual_qop = compute_digest_response
    ~username ~password ~method_ ~uri ~challenge ~nc ~cnonce ?body () in
  let auth_header = build_digest_header
    ~username ~uri ~challenge ~nc ~cnonce ~response ~actual_qop in
  Log.debug (fun m -> m "Applied Digest authentication for user %s (nc=%s qop=%s)"
    username nc (Option.value ~default:"none" actual_qop));
  Headers.set `Authorization auth_header headers

(** Check if auth type is Digest *)
let is_digest = function
  | Digest _ -> true
  | _ -> false

(** Get Digest credentials if configured *)
let get_digest_credentials = function
  | Digest { username; password } -> Some (username, password)
  | _ -> None

(** {1 Bearer Form Authentication}

    Per RFC 6750 Section 2.2: Bearer token can be sent as a form-encoded
    body parameter "access_token". This is less preferred than the
    Authorization header but may be required by some APIs. *)

let bearer_form ~token = Bearer_form { token }

let is_bearer_form = function
  | Bearer_form _ -> true
  | _ -> false

let get_bearer_form_body = function
  | Bearer_form { token } -> Some (Printf.sprintf "access_token=%s" token)
  | _ -> None

(** Check if stale=true in digest challenge, indicating password is still valid.
    Per RFC 7616: If stale=true, the client should retry with same credentials
    using the new nonce. If stale=false or not present, credentials are wrong. *)
let digest_is_stale challenge = challenge.stale

(** {1 HTTP Message Signatures (RFC 9421)} *)

let is_signature = function
  | Signature _ -> true
  | _ -> false

let get_signature_config = function
  | Signature config -> Some config
  | _ -> None

(** Apply HTTP Message Signature to headers given request context.
    This computes and adds the Signature-Input and Signature headers.

    @param clock Eio clock for timestamp generation
    @param method_ The HTTP method
    @param uri The request URI
    @param headers The headers to sign (and add signature to)
    @param auth The authentication configuration (must be [Signature])
    @return Updated headers with signature, or original headers if not Signature auth *)
let apply_signature ~clock ~method_ ~uri ~headers auth =
  match auth with
  | Signature config ->
      let context = Signature.Context.request ~method_ ~uri ~headers in
      (match Signature.sign ~clock ~config ~context ~headers with
      | Ok signed_headers ->
          Log.debug (fun m -> m "Applied HTTP message signature");
          signed_headers
      | Error e ->
          Log.err (fun m -> m "Failed to apply HTTP message signature: %s"
            (Signature.sign_error_to_string e));
          headers)
  | _ -> headers

