(*---------------------------------------------------------------------------
   Copyright (c) 2025 Anil Madhavapeddy. All rights reserved.
   SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

(** Mastodon REST API client using OAuth bearer tokens *)

(** Status visibility options *)
type visibility = Public | Unlisted | Private | Direct

let string_of_visibility = function
  | Public -> "public"
  | Unlisted -> "unlisted"
  | Private -> "private"
  | Direct -> "direct"

(** Status response *)
type status = {
  id : string;
  uri : string;
  url : string option;
  content : string;
  created_at : string;
  visibility : string;
}

let status_jsont =
  Jsont.Object.map ~kind:"MastodonStatus"
    (fun id uri url content created_at visibility ->
      { id; uri; url; content; created_at; visibility })
  |> Jsont.Object.mem "id" Jsont.string ~enc:(fun s -> s.id)
  |> Jsont.Object.mem "uri" Jsont.string ~enc:(fun s -> s.uri)
  |> Jsont.Object.mem "url" (Jsont.option Jsont.string) ~dec_absent:(fun () -> None) ~enc:(fun s -> s.url)
  |> Jsont.Object.mem "content" Jsont.string ~enc:(fun s -> s.content)
  |> Jsont.Object.mem "created_at" Jsont.string ~enc:(fun s -> s.created_at)
  |> Jsont.Object.mem "visibility" Jsont.string ~enc:(fun s -> s.visibility)
  |> Jsont.Object.finish

(** Relationship response (for follow/unfollow) *)
type relationship = {
  id : string;
  following : bool;
  followed_by : bool;
  blocking : bool;
  muting : bool;
  requested : bool;
}

let relationship_jsont =
  Jsont.Object.map ~kind:"MastodonRelationship"
    (fun id following followed_by blocking muting requested ->
      { id; following; followed_by; blocking; muting; requested })
  |> Jsont.Object.mem "id" Jsont.string ~enc:(fun r -> r.id)
  |> Jsont.Object.mem "following" Jsont.bool ~enc:(fun r -> r.following)
  |> Jsont.Object.mem "followed_by" Jsont.bool ~enc:(fun r -> r.followed_by)
  |> Jsont.Object.mem "blocking" Jsont.bool ~enc:(fun r -> r.blocking)
  |> Jsont.Object.mem "muting" Jsont.bool ~enc:(fun r -> r.muting)
  |> Jsont.Object.mem "requested" Jsont.bool ~enc:(fun r -> r.requested)
  |> Jsont.Object.finish

(** Bearer tokens are supplied only to their instance, including after redirects. *)
let authed fetch ~instance ~token =
  Fetch.with_credentials
    ~scope:[ Printf.sprintf "https://%s/" instance ]
    Fetch.Credential.[ Bearer (fun () -> token) ]
    fetch

(** Check response and return error if not successful *)
let check_response resp =
  let status = Fetch.status resp in
  if status >= 200 && status < 300 then
    Ok ()
  else
    let body = Apub_mastodon_oauth.response_body resp in
    Error (Printf.sprintf "HTTP %d: %s" status body)

(** Decode through the shared bounded Fetch codec. *)
let decode_body = Apub_mastodon_oauth.decode_body

(** Post a new status *)
let post_status fetch ~instance ~token ~content
    ?(visibility = Public) ?in_reply_to_id ?sensitive ?spoiler_text () =
  let url = Printf.sprintf "https://%s/api/v1/statuses" instance in
  let fetch = authed fetch ~instance ~token in
  let params = [
    ("status", content);
    ("visibility", string_of_visibility visibility);
  ] in
  let params = match in_reply_to_id with
    | Some id -> ("in_reply_to_id", id) :: params
    | None -> params
  in
  let params = match sensitive with
    | Some true -> ("sensitive", "true") :: params
    | _ -> params
  in
  let params = match spoiler_text with
    | Some text -> ("spoiler_text", text) :: params
    | None -> params
  in
  let headers, body = Fetch.Form.urlencoded params in
  Apub_mastodon_oauth.with_response ~headers ~body fetch `POST url @@ fun resp ->
  match check_response resp with
  | Error e -> Error e
  | Ok () -> decode_body status_jsont resp

(** Internal: a bodyless POST decoding [jsont] from a successful response *)
let post_action fetch ~instance ~token jsont url =
  let fetch = authed fetch ~instance ~token in
  Apub_mastodon_oauth.with_response fetch `POST url @@ fun resp ->
  match check_response resp with
  | Error e -> Error e
  | Ok () -> decode_body jsont resp

(** Internal: a GET decoding [jsont] from a successful response *)
let get_action fetch ~instance ~token jsont url =
  let fetch = authed fetch ~instance ~token in
  Apub_mastodon_oauth.with_response fetch `GET url @@ fun resp ->
  match check_response resp with
  | Error e -> Error e
  | Ok () -> decode_body jsont resp

(** Favourite (like) a status *)
let favourite fetch ~instance ~token ~status_id =
  Printf.sprintf "https://%s/api/v1/statuses/%s/favourite" instance status_id
  |> post_action fetch ~instance ~token status_jsont

(** Unfavourite a status *)
let unfavourite fetch ~instance ~token ~status_id =
  Printf.sprintf "https://%s/api/v1/statuses/%s/unfavourite" instance status_id
  |> post_action fetch ~instance ~token status_jsont

(** Reblog (boost) a status *)
let reblog fetch ~instance ~token ~status_id =
  Printf.sprintf "https://%s/api/v1/statuses/%s/reblog" instance status_id
  |> post_action fetch ~instance ~token status_jsont

(** Unreblog a status *)
let unreblog fetch ~instance ~token ~status_id =
  Printf.sprintf "https://%s/api/v1/statuses/%s/unreblog" instance status_id
  |> post_action fetch ~instance ~token status_jsont

(** Follow an account by ID *)
let follow fetch ~instance ~token ~account_id =
  Printf.sprintf "https://%s/api/v1/accounts/%s/follow" instance account_id
  |> post_action fetch ~instance ~token relationship_jsont

(** Unfollow an account by ID *)
let unfollow fetch ~instance ~token ~account_id =
  Printf.sprintf "https://%s/api/v1/accounts/%s/unfollow" instance account_id
  |> post_action fetch ~instance ~token relationship_jsont

(** Look up an account by webfinger address (user@domain) *)
let lookup_account fetch ~instance ~token ~acct =
  Printf.sprintf "https://%s/api/v1/accounts/lookup?acct=%s" instance
    (Uri.pct_encode acct)
  |> get_action fetch ~instance ~token Apub_mastodon_oauth.account_jsont

(** Search for accounts *)
let search_accounts fetch ~instance ~token ~query ?(limit = 10) () =
  Printf.sprintf "https://%s/api/v1/accounts/search?q=%s&limit=%d" instance
    (Uri.pct_encode query) limit
  |> get_action fetch ~instance ~token
       (Jsont.list Apub_mastodon_oauth.account_jsont)

(** Get a status by ID *)
let get_status fetch ~instance ~token ~status_id =
  Printf.sprintf "https://%s/api/v1/statuses/%s" instance status_id
  |> get_action fetch ~instance ~token status_jsont

(** Delete a status *)
let delete_status fetch ~instance ~token ~status_id =
  let url = Printf.sprintf "https://%s/api/v1/statuses/%s" instance status_id in
  let fetch = authed fetch ~instance ~token in
  Apub_mastodon_oauth.with_response fetch `DELETE url @@ fun resp -> check_response resp

(** Resolve on the authenticated instance: remote URL IDs are never local IDs. *)
let resolve fetch ~instance ~token ~kind codec query =
  let response_codec = Jsont.Object.map Fun.id
    |> Jsont.Object.mem kind (Jsont.list codec) ~enc:Fun.id
    |> Jsont.Object.finish in
  let url = Uri.of_string (Printf.sprintf "https://%s/api/v2/search" instance)
    |> fun u -> Uri.with_query' u ["q", query; "resolve", "true"; "type", kind; "limit", "2"]
    |> Uri.to_string in
  match get_action fetch ~instance ~token response_codec url with
  | Error error -> Error error
  | Ok [value] -> Ok value
  | Ok [] -> Error ("Could not resolve " ^ query ^ "; OAuth login must include read:search")
  | Ok _ -> Error ("Ambiguous search result for " ^ query)

let resolve_status fetch ~instance ~token ~url =
  match Fetch.Middleware.Url.of_string url with
  | Error _ -> Error "Expected an absolute HTTP(S) status URL"
  | Ok _ -> resolve fetch ~instance ~token ~kind:"statuses" status_jsont url

let resolve_account fetch ~instance ~token ~account =
  resolve fetch ~instance ~token ~kind:"accounts" Apub_mastodon_oauth.account_jsont account

let post_status_reply fetch ~instance ~token ~content ~visibility ?reply_to
    ?sensitive ?spoiler_text () =
  let reply = match reply_to with
    | None -> Ok None
    | Some url -> Result.map (fun (status : status) -> Some status.id)
        (resolve_status fetch ~instance ~token ~url) in
  Result.bind reply (fun in_reply_to_id -> post_status fetch ~instance ~token
    ~content ~visibility ?in_reply_to_id ?sensitive ?spoiler_text ())
