(*---------------------------------------------------------------------------
   Copyright (c) 2025 Anil Madhavapeddy. All rights reserved.
   SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

(** Mastodon OAuth implementation for CLI authentication *)

(** OAuth scopes for ActivityPub operations *)
let scopes =
  "read:accounts read:statuses write:statuses read:follows write:follows \
   read:favourites write:favourites"

(** Client app name shown during authorization *)
let client_name = "apub CLI"

(** Redirect URI for out-of-band CLI authorization *)
let redirect_uri = "urn:ietf:wg:oauth:2.0:oob"

(** App registration response *)
type app = {
  client_id : string;
  client_secret : string;
  vapid_key : string option;
}

let app_jsont =
  Jsont.Object.map ~kind:"MastodonApp"
    (fun client_id client_secret vapid_key ->
      { client_id; client_secret; vapid_key })
  |> Jsont.Object.mem "client_id" Jsont.string ~enc:(fun a -> a.client_id)
  |> Jsont.Object.mem "client_secret" Jsont.string ~enc:(fun a -> a.client_secret)
  |> Jsont.Object.opt_mem "vapid_key" Jsont.string ~enc:(fun a -> a.vapid_key)
  |> Jsont.Object.finish

(** Token response *)
type token = {
  access_token : string;
  token_type : string;
  scope : string;
  created_at : int;
}

let token_jsont =
  Jsont.Object.map ~kind:"MastodonToken"
    (fun access_token token_type scope created_at ->
      { access_token; token_type; scope; created_at })
  |> Jsont.Object.mem "access_token" Jsont.string ~enc:(fun t -> t.access_token)
  |> Jsont.Object.mem "token_type" Jsont.string ~enc:(fun t -> t.token_type)
  |> Jsont.Object.mem "scope" Jsont.string ~enc:(fun t -> t.scope)
  |> Jsont.Object.mem "created_at" Jsont.int ~enc:(fun t -> t.created_at)
  |> Jsont.Object.finish

(** Account (verify_credentials response) *)
type account = {
  id : string;
  username : string;
  acct : string;
  display_name : string option;
  url : string;
}

let account_jsont =
  Jsont.Object.map ~kind:"MastodonAccount"
    (fun id username acct display_name url ->
      { id; username; acct; display_name; url })
  |> Jsont.Object.mem "id" Jsont.string ~enc:(fun a -> a.id)
  |> Jsont.Object.mem "username" Jsont.string ~enc:(fun a -> a.username)
  |> Jsont.Object.mem "acct" Jsont.string ~enc:(fun a -> a.acct)
  |> Jsont.Object.opt_mem "display_name" Jsont.string ~enc:(fun a -> a.display_name)
  |> Jsont.Object.mem "url" Jsont.string ~enc:(fun a -> a.url)
  |> Jsont.Object.finish

(** PKCE (Proof Key for Code Exchange) *)
module Pkce = struct
  (** Generate a random code verifier (43-128 chars, URL-safe base64) *)
  let generate_verifier () =
    (* Generate 32 random bytes (will produce 43 base64 chars) *)
    let bytes = Mirage_crypto_rng.generate 32 in
    Base64.encode_string ~pad:false ~alphabet:Base64.uri_safe_alphabet bytes

  (** Generate code challenge from verifier using SHA-256 *)
  let challenge_of_verifier verifier =
    let hash = Digestif.SHA256.digest_string verifier in
    Base64.encode_string ~pad:false ~alphabet:Base64.uri_safe_alphabet
      (Digestif.SHA256.to_raw_string hash)

  (** Generate a PKCE pair: (verifier, challenge) *)
  let generate () =
    let verifier = generate_verifier () in
    let challenge = challenge_of_verifier verifier in
    (verifier, challenge)
end

(** Extract instance domain from account handle (user@instance.social) *)
let instance_of_account account =
  match String.split_on_char '@' account with
  | [_user; instance] -> Some instance
  | _ -> None

(** Register a new OAuth app with the instance *)
let register_app requests ~instance =
  let url = Printf.sprintf "https://%s/api/v1/apps" instance in
  let params = [
    ("client_name", client_name);
    ("redirect_uris", redirect_uri);
    ("scopes", scopes);
    ("website", "https://github.com/avsm/apub");
  ] in
  let body = Requests.Body.form params in
  let resp = Requests.post requests ~body url in
  let status = Requests.Response.status_code resp in
  if status >= 200 && status < 300 then
    Ok (Requests.Response.jsonv app_jsont resp)
  else
    let body = Requests.Response.text resp in
    Error (Printf.sprintf "Failed to register app (HTTP %d): %s" status body)

(** Build the authorization URL for the user to visit *)
let authorization_url ~instance ~client_id ~code_challenge =
  let base = Printf.sprintf "https://%s/oauth/authorize" instance in
  let params = [
    ("response_type", "code");
    ("client_id", client_id);
    ("redirect_uri", redirect_uri);
    ("scope", scopes);
    ("code_challenge", code_challenge);
    ("code_challenge_method", "S256");
  ] in
  let query = String.concat "&" (List.map (fun (k, v) ->
    k ^ "=" ^ Uri.pct_encode v
  ) params) in
  base ^ "?" ^ query

(** Exchange authorization code for access token *)
let exchange_code requests ~instance ~client_id ~client_secret ~code ~code_verifier =
  let url = Printf.sprintf "https://%s/oauth/token" instance in
  let params = [
    ("grant_type", "authorization_code");
    ("code", code);
    ("client_id", client_id);
    ("client_secret", client_secret);
    ("redirect_uri", redirect_uri);
    ("code_verifier", code_verifier);
  ] in
  let body = Requests.Body.form params in
  let resp = Requests.post requests ~body url in
  let status = Requests.Response.status_code resp in
  if status >= 200 && status < 300 then
    Ok (Requests.Response.jsonv token_jsont resp)
  else
    let body = Requests.Response.text resp in
    Error (Printf.sprintf "Failed to exchange code (HTTP %d): %s" status body)

(** Verify credentials and get account info *)
let verify_credentials requests ~instance ~access_token =
  let url = Printf.sprintf "https://%s/api/v1/accounts/verify_credentials" instance in
  let headers =
    Requests.Headers.empty
    |> Requests.Headers.bearer access_token
  in
  let resp = Requests.get requests ~headers url in
  let status = Requests.Response.status_code resp in
  if status >= 200 && status < 300 then
    Ok (Requests.Response.jsonv account_jsont resp)
  else
    let body = Requests.Response.text resp in
    Error (Printf.sprintf "Failed to verify credentials (HTTP %d): %s" status body)

(** Get the ActivityPub actor URI from a Mastodon account URL *)
let actor_uri_of_account_url url =
  (* Mastodon account URLs are typically the same as actor URIs *)
  url
