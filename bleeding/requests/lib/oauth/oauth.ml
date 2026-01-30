(*---------------------------------------------------------------------------
  Copyright (c) 2025 Anil Madhavapeddy <anil@recoil.org>. All rights reserved.
  SPDX-License-Identifier: ISC
 ---------------------------------------------------------------------------*)

(** RFC 6749 OAuth 2.0 Authorization Framework. *)

let src = Logs.Src.create "requests.oauth" ~doc:"OAuth 2.0"
module Log = (val Logs.src_log src : Logs.LOG)

(** {1 Client Configuration} *)

type config = {
  client_id : string;
  client_secret : string option;
  token_endpoint : string;
  authorization_endpoint : string option;
  redirect_uri : string option;
  scopes : string list;
}

let make_config ~client_id ?client_secret ~token_endpoint
    ?authorization_endpoint ?redirect_uri ?(scopes = []) () =
  { client_id; client_secret; token_endpoint;
    authorization_endpoint; redirect_uri; scopes }

(** {1 Token Types} *)

type token = {
  access_token : string;
  token_type : string;
  expires_at : Ptime.t option;
  refresh_token : string option;
  scope : string option;
}

let get_access_token t = t.access_token
let get_refresh_token t = t.refresh_token

let now () = Ptime_clock.now ()

let is_expired token =
  match token.expires_at with
  | None -> false
  | Some expires_at -> not (Ptime.is_later expires_at ~than:(now ()))

let expires_within span token =
  match token.expires_at with
  | None -> false
  | Some expires_at ->
      match Ptime.sub_span expires_at span with
      | None -> true  (* Overflow, assume expiring *)
      | Some threshold -> not (Ptime.is_later threshold ~than:(now ()))

(** {1 Error Types} *)

type error_code =
  | Invalid_request
  | Invalid_client
  | Invalid_grant
  | Unauthorized_client
  | Unsupported_grant_type
  | Invalid_scope
  | Unknown_error of string

type error = {
  code : error_code;
  description : string option;
  uri : string option;
}

let error_code_of_string = function
  | "invalid_request" -> Invalid_request
  | "invalid_client" -> Invalid_client
  | "invalid_grant" -> Invalid_grant
  | "unauthorized_client" -> Unauthorized_client
  | "unsupported_grant_type" -> Unsupported_grant_type
  | "invalid_scope" -> Invalid_scope
  | s -> Unknown_error s

let error_code_to_string = function
  | Invalid_request -> "invalid_request"
  | Invalid_client -> "invalid_client"
  | Invalid_grant -> "invalid_grant"
  | Unauthorized_client -> "unauthorized_client"
  | Unsupported_grant_type -> "unsupported_grant_type"
  | Invalid_scope -> "invalid_scope"
  | Unknown_error s -> s

let pp_error ppf err =
  Format.fprintf ppf "OAuth error: %s" (error_code_to_string err.code);
  Option.iter (fun desc -> Format.fprintf ppf " - %s" desc) err.description;
  Option.iter (fun uri -> Format.fprintf ppf " (see: %s)" uri) err.uri

(** {1 PKCE Support} *)

type pkce_method = Plain | S256

type pkce = {
  verifier : string;
  challenge : string;
  method_ : pkce_method;
}

let pkce_method_to_string = function
  | Plain -> "plain"
  | S256 -> "S256"

(** URL-safe base64 encoding without padding per RFC 7636 Appendix A *)
let base64_url_encode_no_padding s =
  Base64.encode_exn s
  |> String.map (function '+' -> '-' | '/' -> '_' | c -> c)
  |> String.to_seq
  |> Seq.filter (fun c -> c <> '=')
  |> String.of_seq

let generate_verifier () =
  Mirage_crypto_rng.generate 32 |> base64_url_encode_no_padding

let compute_challenge ~method_ verifier =
  match method_ with
  | Plain -> verifier
  | S256 ->
      Digestif.SHA256.digest_string verifier
      |> Digestif.SHA256.to_raw_string
      |> base64_url_encode_no_padding

let generate_pkce ?(method_ = S256) () =
  let verifier = generate_verifier () in
  let challenge = compute_challenge ~method_ verifier in
  { verifier; challenge; method_ }

(** {1 State Parameter} *)

let generate_state () =
  Mirage_crypto_rng.generate 16 |> base64_url_encode_no_padding

let validate_state ~expected ~received =
  Eqaf.equal expected received

(** {1 Authorization URL} *)

let add_opt_param key opt params =
  Option.fold ~none:params ~some:(fun v -> (key, v) :: params) opt

let authorization_url ~config ~state ?pkce ?(extra_params = []) () =
  match config.authorization_endpoint with
  | None ->
      invalid_arg "authorization_endpoint is required for authorization URL"
  | Some endpoint ->
      let params =
        [ ("response_type", "code");
          ("client_id", config.client_id);
          ("state", state) ]
        |> add_opt_param "redirect_uri" config.redirect_uri
        |> (fun params ->
             match config.scopes with
             | [] -> params
             | scopes -> ("scope", String.concat " " scopes) :: params)
        |> (fun params ->
             match pkce with
             | None -> params
             | Some p ->
                 ("code_challenge", p.challenge)
                 :: ("code_challenge_method", pkce_method_to_string p.method_)
                 :: params)
        |> List.rev_append extra_params
      in
      let uri = Uri.of_string endpoint in
      let params_list = List.map (fun (k, v) -> (k, [v])) params in
      Uri.to_string (Uri.add_query_params uri params_list)

(** {1 JSON Codecs} *)

let string_option = Jsont.(some string)
let int_option = Jsont.(some int)

(** Token response JSON codec *)
let token_response_jsont =
  let make access_token token_type expires_in refresh_token scope =
    let received_at = now () in
    let expires_at =
      Option.bind expires_in (fun secs ->
        Ptime.add_span received_at (Ptime.Span.of_int_s secs))
    in
    { access_token; token_type; expires_at; refresh_token; scope }
  in
  Jsont.Object.map ~kind:"token_response" make
  |> Jsont.Object.mem "access_token" Jsont.string ~enc:(fun t -> t.access_token)
  |> Jsont.Object.mem "token_type" Jsont.string ~enc:(fun t -> t.token_type)
  |> Jsont.Object.mem "expires_in" int_option ~dec_absent:None
       ~enc:(fun _ -> None) (* Don't encode expires_in, it's derived *)
  |> Jsont.Object.mem "refresh_token" string_option ~dec_absent:None
       ~enc:(fun t -> t.refresh_token)
  |> Jsont.Object.mem "scope" string_option ~dec_absent:None
       ~enc:(fun t -> t.scope)
  |> Jsont.Object.finish

let error_code_jsont =
  Jsont.string
  |> Jsont.map ~dec:error_code_of_string ~enc:error_code_to_string

let error_jsont =
  let make code description uri = { code; description; uri } in
  Jsont.Object.map ~kind:"oauth_error" make
  |> Jsont.Object.mem "error" error_code_jsont ~enc:(fun e -> e.code)
  |> Jsont.Object.mem "error_description" string_option ~dec_absent:None
       ~enc:(fun e -> e.description)
  |> Jsont.Object.mem "error_uri" string_option ~dec_absent:None
       ~enc:(fun e -> e.uri)
  |> Jsont.Object.finish

(** {1 Token Operations} *)

let parse_token_response ~status ~body =
  Log.debug (fun m -> m "Token response status=%d" status);
  if status >= 200 && status < 300 then
    match Jsont_bytesrw.decode_string' token_response_jsont body with
    | Ok token ->
        Log.info (fun m -> m "Received access token (type=%s)" token.token_type);
        Ok token
    | Error e ->
        Log.err (fun m -> m "Failed to parse token response: %s"
          (Jsont.Error.to_string e));
        Error {
          code = Invalid_request;
          description = Some ("Failed to parse token response: " ^
                              Jsont.Error.to_string e);
          uri = None;
        }
  else
    match Jsont_bytesrw.decode_string' error_jsont body with
    | Ok err ->
        Log.warn (fun m -> m "OAuth error: %a" pp_error err);
        Error err
    | Error e ->
        Log.err (fun m -> m "Failed to parse error response: %s (status=%d)"
          (Jsont.Error.to_string e) status);
        Error {
          code = Unknown_error "parse_error";
          description = Some (Printf.sprintf "HTTP %d: %s" status body);
          uri = None;
        }

let add_client_auth config headers body_params =
  match config.client_secret with
  | Some secret ->
      let headers = Requests.Headers.basic
        ~username:config.client_id
        ~password:secret
        headers
      in
      (headers, body_params)
  | None ->
      let body_params = ("client_id", config.client_id) :: body_params in
      (headers, body_params)

let token_request session config ~grant_type ~params =
  let headers = Requests.Headers.empty in
  let body_params = ("grant_type", grant_type) :: params in
  let body_params =
    match config.scopes with
    | [] -> body_params
    | scopes -> ("scope", String.concat " " scopes) :: body_params
  in
  let headers, body_params = add_client_auth config headers body_params in
  Log.debug (fun m -> m "Token request to %s: grant_type=%s"
    config.token_endpoint grant_type);
  let body = Requests.Body.form body_params in
  let response = Requests.post session ~headers ~body config.token_endpoint in
  let status = Requests.Response.status_code response in
  let body = Requests.Response.text response in
  parse_token_response ~status ~body

let client_credentials session config =
  Log.info (fun m -> m "Performing client credentials grant");
  token_request session config ~grant_type:"client_credentials" ~params:[]

let password_grant session config ~username ~password =
  Log.info (fun m -> m "Performing password credentials grant for user: %s" username);
  let params = [("username", username); ("password", password)] in
  token_request session config ~grant_type:"password" ~params

let exchange_code session config ~code ?pkce_verifier () =
  Log.info (fun m -> m "Exchanging authorization code for tokens");
  let params =
    [("code", code)]
    |> add_opt_param "redirect_uri" config.redirect_uri
    |> add_opt_param "code_verifier" pkce_verifier
  in
  token_request session config ~grant_type:"authorization_code" ~params

let refresh session config ~refresh_token =
  Log.info (fun m -> m "Refreshing access token");
  let params = [("refresh_token", refresh_token)] in
  token_request session config ~grant_type:"refresh_token" ~params

(** {1 Managed Token State} *)

type t = {
  session : Requests.t;
  config : config;
  mutable token : token;
  mutex : Eio.Mutex.t;
  on_refresh : (token -> unit) option;
}

let default_leeway = Ptime.Span.of_int_s 30

let create session config token ?on_refresh () =
  { session; config; token; mutex = Eio.Mutex.create (); on_refresh }

let force_refresh t =
  Eio.Mutex.use_rw ~protect:true t.mutex (fun () ->
    match t.token.refresh_token with
    | None ->
        Log.warn (fun m -> m "Cannot refresh: no refresh token available");
        Error {
          code = Invalid_grant;
          description = Some "No refresh token available";
          uri = None;
        }
    | Some refresh_token ->
        match refresh t.session t.config ~refresh_token with
        | Ok new_token ->
            (* Preserve refresh token if new response doesn't include one *)
            let new_token =
              if new_token.refresh_token = None then
                { new_token with refresh_token = Some refresh_token }
              else
                new_token
            in
            t.token <- new_token;
            Option.iter (fun f -> f new_token) t.on_refresh;
            Ok new_token
        | Error e ->
            Log.err (fun m -> m "Token refresh failed: %a" pp_error e);
            Error e)

let check_and_refresh t =
  if expires_within default_leeway t.token then begin
    Log.debug (fun m -> m "Token needs refresh, refreshing...");
    match force_refresh t with
    | Ok _ -> ()
    | Error e -> Log.warn (fun m -> m "Auto-refresh failed: %a" pp_error e)
  end

let get_token t =
  check_and_refresh t;
  t.token

let get_access_token_managed t =
  (get_token t).access_token

let with_client_credentials session config ?on_refresh () =
  client_credentials session config
  |> Result.map (fun token -> create session config token ?on_refresh ())
