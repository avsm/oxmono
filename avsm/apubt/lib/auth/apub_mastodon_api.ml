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
  |> Jsont.Object.opt_mem "url" Jsont.string ~enc:(fun s -> s.url)
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

(** Helper to create authenticated headers *)
let auth_headers token =
  Requests.Headers.empty
  |> Requests.Headers.bearer token

(** Check response and return error if not successful *)
let check_response resp =
  let status = Requests.Response.status_code resp in
  if status >= 200 && status < 300 then
    Ok ()
  else
    let body = Requests.Response.text resp in
    Error (Printf.sprintf "HTTP %d: %s" status body)

(** Post a new status *)
let post_status requests ~instance ~token ~content
    ?(visibility = Public) ?in_reply_to_id ?sensitive ?spoiler_text () =
  let url = Printf.sprintf "https://%s/api/v1/statuses" instance in
  let headers = auth_headers token in
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
  let body = Requests.Body.form params in
  let resp = Requests.post requests ~headers ~body url in
  match check_response resp with
  | Error e -> Error e
  | Ok () -> Ok (Requests.Response.jsonv status_jsont resp)

(** Favourite (like) a status *)
let favourite requests ~instance ~token ~status_id =
  let url = Printf.sprintf "https://%s/api/v1/statuses/%s/favourite" instance status_id in
  let headers = auth_headers token in
  let resp = Requests.post requests ~headers url in
  match check_response resp with
  | Error e -> Error e
  | Ok () -> Ok (Requests.Response.jsonv status_jsont resp)

(** Unfavourite a status *)
let unfavourite requests ~instance ~token ~status_id =
  let url = Printf.sprintf "https://%s/api/v1/statuses/%s/unfavourite" instance status_id in
  let headers = auth_headers token in
  let resp = Requests.post requests ~headers url in
  match check_response resp with
  | Error e -> Error e
  | Ok () -> Ok (Requests.Response.jsonv status_jsont resp)

(** Reblog (boost) a status *)
let reblog requests ~instance ~token ~status_id =
  let url = Printf.sprintf "https://%s/api/v1/statuses/%s/reblog" instance status_id in
  let headers = auth_headers token in
  let resp = Requests.post requests ~headers url in
  match check_response resp with
  | Error e -> Error e
  | Ok () -> Ok (Requests.Response.jsonv status_jsont resp)

(** Unreblog a status *)
let unreblog requests ~instance ~token ~status_id =
  let url = Printf.sprintf "https://%s/api/v1/statuses/%s/unreblog" instance status_id in
  let headers = auth_headers token in
  let resp = Requests.post requests ~headers url in
  match check_response resp with
  | Error e -> Error e
  | Ok () -> Ok (Requests.Response.jsonv status_jsont resp)

(** Follow an account by ID *)
let follow requests ~instance ~token ~account_id =
  let url = Printf.sprintf "https://%s/api/v1/accounts/%s/follow" instance account_id in
  let headers = auth_headers token in
  let resp = Requests.post requests ~headers url in
  match check_response resp with
  | Error e -> Error e
  | Ok () -> Ok (Requests.Response.jsonv relationship_jsont resp)

(** Unfollow an account by ID *)
let unfollow requests ~instance ~token ~account_id =
  let url = Printf.sprintf "https://%s/api/v1/accounts/%s/unfollow" instance account_id in
  let headers = auth_headers token in
  let resp = Requests.post requests ~headers url in
  match check_response resp with
  | Error e -> Error e
  | Ok () -> Ok (Requests.Response.jsonv relationship_jsont resp)

(** Look up an account by webfinger address (user@domain) *)
let lookup_account requests ~instance ~token ~acct =
  let url = Printf.sprintf "https://%s/api/v1/accounts/lookup?acct=%s"
    instance (Uri.pct_encode acct) in
  let headers = auth_headers token in
  let resp = Requests.get requests ~headers url in
  match check_response resp with
  | Error e -> Error e
  | Ok () -> Ok (Requests.Response.jsonv Apub_mastodon_oauth.account_jsont resp)

(** Search for accounts *)
let search_accounts requests ~instance ~token ~query ?(limit = 10) () =
  let url = Printf.sprintf "https://%s/api/v1/accounts/search?q=%s&limit=%d"
    instance (Uri.pct_encode query) limit in
  let headers = auth_headers token in
  let resp = Requests.get requests ~headers url in
  match check_response resp with
  | Error e -> Error e
  | Ok () -> Ok (Requests.Response.jsonv (Jsont.list Apub_mastodon_oauth.account_jsont) resp)

(** Get a status by ID *)
let get_status requests ~instance ~token ~status_id =
  let url = Printf.sprintf "https://%s/api/v1/statuses/%s" instance status_id in
  let headers = auth_headers token in
  let resp = Requests.get requests ~headers url in
  match check_response resp with
  | Error e -> Error e
  | Ok () -> Ok (Requests.Response.jsonv status_jsont resp)

(** Delete a status *)
let delete_status requests ~instance ~token ~status_id =
  let url = Printf.sprintf "https://%s/api/v1/statuses/%s" instance status_id in
  let headers = auth_headers token in
  let resp = Requests.delete requests ~headers url in
  check_response resp

(** Extract status ID from a Mastodon URL like https://instance/users/name/statuses/123
    or https://instance/@name/123 *)
let status_id_of_url url =
  let uri = Uri.of_string url in
  let path = Uri.path uri in
  (* Try different URL formats *)
  let parts = String.split_on_char '/' path in
  let parts = List.filter (fun s -> s <> "") parts in
  match List.rev parts with
  | id :: _ when String.for_all (fun c -> c >= '0' && c <= '9') id -> Some id
  | _ -> None
