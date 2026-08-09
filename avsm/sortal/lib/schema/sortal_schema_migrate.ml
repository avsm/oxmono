(*---------------------------------------------------------------------------
  Copyright (c) 2025 Anil Madhavapeddy <anil@recoil.org>. All rights reserved.
  SPDX-License-Identifier: ISC
 ---------------------------------------------------------------------------*)

module V1 = Sortal_schema_contact_v1
module V2 = Sortal_schema_contact_v2
module A = Sortal_schema_account
module P = Sortal_schema_platform

let ( let* ) = Result.bind

let strip_www h =
  match String.starts_with ~prefix:"www." h with
  | true -> String.sub h 4 (String.length h - 4)
  | false -> h

(* The part of a URL before any query string or fragment. Both
   [host_of_url] and [url_tail] extract a substring by position, and a
   query or fragment glued onto the end of that substring would corrupt
   it (a query with no path, such as [https://host?q], would otherwise
   leak into the host; a trailing [?q] or [#frag] on the last path
   segment would otherwise leak into a handle), so both call this first. *)
let strip_query_and_fragment url =
  let cut_at c s =
    match String.index_opt s c with Some i -> String.sub s 0 i | None -> s
  in
  cut_at '#' (cut_at '?' url)

(* The host of an [http(s)://host/...] URL, or [None] for a string with no
   scheme, such as a bare handle recorded as a service's [url]. *)
let host_of_url url =
  let url = strip_query_and_fragment url in
  match String.index_opt url ':' with
  | None -> None
  | Some _ -> (
      match String.split_on_char '/' url with
      | _scheme :: "" :: host :: _ -> Some (strip_www host)
      | _ -> None)

(* [true] if [h] is [scholar.google.<suffix>] where [<suffix>] is one or
   two letters-only labels of 2 to 3 characters, such as [com], [ca], [at]
   or [co.uk]. Matched on label structure, not a string prefix: a prefix
   test would also accept a crafted host such as
   [scholar.google.com.evil.example], which has "scholar.google." as a
   literal prefix but is not a Google host at all. *)
let is_scholar_google_host h =
  let plausible_suffix_label s =
    let n = String.length s in
    (n = 2 || n = 3) && String.for_all (fun c -> c >= 'a' && c <= 'z') s
  in
  match String.split_on_char '.' h with
  | "scholar" :: "google" :: suffix -> (
      match suffix with
      | [ tld ] -> plausible_suffix_label tld
      | [ sld; tld ] -> plausible_suffix_label sld && plausible_suffix_label tld
      | _ -> false)
  | _ -> false

let platform_of_host h =
  (* A github.io host is a personal page, not a GitHub account. *)
  if String.ends_with ~suffix:"github.io" h then None
  else if is_scholar_google_host h then
    (* A Scholar profile id is global, not per-domain, so every regional
       host (scholar.google.ca, scholar.google.at, ...) is the same
       platform. *)
    Some (P.Simple P.Scholar)
  else
    match h with
    | "github.com" -> Some (P.Simple P.Github)
    | "gitlab.com" -> Some (P.Simple P.Gitlab)
    | "codeberg.org" -> Some (P.Simple P.Codeberg)
    | "orcid.org" -> Some (P.Simple P.Orcid)
    | "twitter.com" | "x.com" -> Some (P.Simple P.Twitter)
    | "linkedin.com" | "uk.linkedin.com" -> Some (P.Simple P.LinkedIn)
    | "threads.com" | "threads.net" -> Some (P.Simple P.Threads)
    | "instagram.com" -> Some (P.Simple P.Instagram)
    | "flickr.com" -> Some (P.Simple P.Flickr)
    | _ -> None

(* The last non-empty path segment, which is the handle for every platform
   whose URL is a bare path. Scholar is the exception and is handled by its
   caller. *)
let url_tail url =
  let segments =
    strip_query_and_fragment url
    |> String.split_on_char '/'
    |> List.filter (fun s -> s <> "")
  in
  match List.rev segments with tail :: _ -> Some tail | [] -> None

(* A Google Scholar profile id, drawn from [A-Za-z0-9_-]. No length check:
   all six ids in the live store are twelve characters, but that is not a
   guarantee Google makes. *)
let is_scholar_id_char c =
  (c >= 'a' && c <= 'z')
  || (c >= 'A' && c <= 'Z')
  || (c >= '0' && c <= '9')
  || c = '_' || c = '-'

let valid_scholar_id v = v <> "" && String.for_all is_scholar_id_char v

(* A Scholar profile URL carries its id in the [user] query parameter,
   never in the path, so [url_tail] cannot be reused here. The query string
   can hold other parameters in either order, such as the trailing [&hl=en]
   most Scholar URLs in the store carry, so this parses [key=value] pairs
   properly rather than taking everything after the first [=]: that would
   fold a following [&hl=en] into the id, and would misfire if [hl] came
   first. The extracted value is then checked against the id's character
   set. That one check, rather than a special case for each, is what
   rejects an empty [user=], a value with a stray [#fragment] glued onto
   it (the [&]-split alone does not separate a fragment), and a
   percent-encoded value: none of those are a plausible id, decoded or
   not. *)
let scholar_id url =
  match String.index_opt url '?' with
  | None -> None
  | Some qi ->
      let query = String.sub url (qi + 1) (String.length url - qi - 1) in
      String.split_on_char '&' query
      |> List.find_map (fun kv ->
             match String.index_opt kv '=' with
             | Some i when String.sub kv 0 i = "user" ->
                 let v = String.sub kv (i + 1) (String.length kv - i - 1) in
                 if valid_scholar_id v then Some v else None
             | _ -> None)

let handle_of_simple p url =
  match p with P.Scholar -> scholar_id url | _ -> url_tail url

let account_of_simple p handle = Ok (A.Simple (p, handle))

let account_of_federated p handle =
  (* A fediverse handle is sometimes written with a leading sigil, as
     [@user@host]. Strip it before splitting, or the user part keeps it. *)
  let handle =
    if String.length handle > 0 && handle.[0] = '@' then
      String.sub handle 1 (String.length handle - 1)
    else handle
  in
  match String.rindex_opt handle '@' with
  | Some i when i > 0 && i < String.length handle - 1 ->
      let user = String.sub handle 0 i in
      let host = String.sub handle (i + 1) (String.length handle - i - 1) in
      Ok (A.Federated (p, user, host))
  | _ ->
      Error
        (Printf.sprintf "%s account %S is not in user@host form"
           (P.key (P.Federated p))
           handle)

(* A service's handle, preferring the recorded one, then the URL tail, then
   the URL itself when it is a bare handle rather than a URL. *)
let service_handle (s : V1.service) =
  match s.handle with
  | Some h -> Some h
  | None ->
      if String.starts_with ~prefix:"http" s.url then url_tail s.url
      else Some s.url

(* A Matrix, Zulip or Discourse service records the account's handle and its
   host separately, as [handle] and the host of [url], rather than together
   as [user@host]. Join them here so [account_of_federated] has one form to
   parse. A handle that already contains an [@] is left alone. *)
let with_host_of (s : V1.service) h =
  if String.contains h '@' then h
  else
    match host_of_url s.url with Some host -> h ^ "@" ^ host | None -> h

let account_of_service (s : V1.service) =
  let need_handle () =
    match service_handle s with
    | Some h -> Ok h
    | None -> Error (Printf.sprintf "service %S has no usable handle" s.url)
  in
  match s.kind with
  | None -> Error (Printf.sprintf "service %S has no kind" s.url)
  | Some V1.Github ->
      let* h = need_handle () in
      account_of_simple P.Github h
  | Some V1.Twitter ->
      let* h = need_handle () in
      account_of_simple P.Twitter h
  | Some V1.LinkedIn ->
      let* h = need_handle () in
      account_of_simple P.LinkedIn h
  | Some V1.Git -> (
      match Option.bind (host_of_url s.url) platform_of_host with
      | Some (P.Simple p) ->
          let* h = need_handle () in
          account_of_simple p h
      | _ -> Error (Printf.sprintf "git service %S has no known host" s.url))
  | Some V1.Photo -> (
      match Option.bind (host_of_url s.url) platform_of_host with
      | Some (P.Simple p) ->
          let* h = need_handle () in
          account_of_simple p h
      | _ -> Error (Printf.sprintf "photo service %S has no known host" s.url))
  | Some (V1.ActivityPub V1.Mastodon) ->
      let* h = need_handle () in
      account_of_federated P.Mastodon h
  | Some (V1.ActivityPub V1.Pixelfed) ->
      let* h = need_handle () in
      account_of_federated P.Pixelfed h
  | Some (V1.ActivityPub V1.PeerTube) ->
      let* h = need_handle () in
      account_of_federated P.PeerTube h
  | Some (V1.ActivityPub (V1.Other_activitypub v)) ->
      Error (Printf.sprintf "unknown ActivityPub variant %S" v)
  | Some (V1.Custom "matrix") ->
      let* h = need_handle () in
      account_of_federated P.Matrix (with_host_of s h)
  | Some (V1.Custom "zulip") ->
      let* h = need_handle () in
      account_of_federated P.Zulip (with_host_of s h)
  | Some (V1.Custom "discourse") ->
      let* h = need_handle () in
      account_of_federated P.Discourse (with_host_of s h)
  | Some (V1.Custom "threads") ->
      let* h = need_handle () in
      account_of_simple P.Threads h
  | Some (V1.Custom "bluesky") ->
      let* h = need_handle () in
      Ok (A.Atproto { A.handle = h; did = None; apps = [ A.Bluesky ] })
  | Some (V1.Custom other) ->
      Error (Printf.sprintf "unknown service kind %S" other)

let app_of_v1 = function
  | V1.ATBluesky -> Ok A.Bluesky
  | V1.ATTangled -> Ok A.Tangled
  | V1.ATCustom "standard-site" -> Ok A.Standard_site
  | V1.ATCustom other ->
      Error (Printf.sprintf "unknown AT Protocol app %S" other)

let account_of_atproto (a : V1.atproto) =
  let* apps =
    List.fold_left
      (fun acc (s : V1.atproto_service) ->
        let* acc = acc in
        let* app = app_of_v1 s.atp_type in
        Ok (acc @ [ app ]))
      (Ok []) a.atp_services
  in
  Ok (A.Atproto { A.handle = a.atp_handle; did = a.atp_did; apps })

(* V2 affiliations have no field for an organization's work [email], unlike
   every other organization field, which either carries over or has nowhere
   sensible to go but is not identifying. Losing an address silently is the
   one drop this migration refuses to make: [Error] rather than a dropped
   field, so an operator decides where it belongs instead of losing it. *)
let affiliation_of_org ~contact (o : V1.organization) =
  match o.email with
  | Some _ ->
      Error
        (Printf.sprintf
           "contact %S: organization %S has an email, which V2 \
            affiliations have no field for"
           contact o.name)
  | None ->
      let bound f = Option.bind o.range f in
      Ok
        {
          V2.org = o.name;
          department = o.department;
          title = o.title;
          url = o.url;
          address = o.address;
          from = bound (fun (r : Sortal_schema_temporal.range) -> r.from);
          until = bound (fun (r : Sortal_schema_temporal.range) -> r.until);
        }

let collect f xs =
  List.fold_left
    (fun acc x ->
      let* acc = acc in
      let* y = f x in
      Ok (acc @ [ y ]))
    (Ok []) xs

let classify_url url =
  match Option.bind (host_of_url url) platform_of_host with
  | Some (P.Simple p) -> (
      match handle_of_simple p url with
      | Some h -> `Account (A.Simple (p, h))
      | None -> `Link)
  | _ -> `Link

(* An account promoted out of [urls], or [Right u] to keep [u] as a link
   when its host names no platform, or is not a URL at all. *)
let account_of_url (u : V1.url_entry) =
  match classify_url u.url with
  | `Account a -> Either.Left a
  | `Link -> Either.Right u

let v1_to_v2 (c : V1.t) =
  let* service_accounts = collect account_of_service (V1.services c) in
  let* atproto_accounts =
    match V1.atproto c with
    | None -> Ok []
    | Some a ->
        let* acc = account_of_atproto a in
        Ok [ acc ]
  in
  let orcid_accounts =
    match V1.orcid c with None -> [] | Some o -> [ A.Simple (P.Orcid, o) ]
  in
  let promoted, plain = List.partition_map account_of_url (V1.urls c) in
  let links =
    List.map
      (fun (u : V1.url_entry) -> { V2.url = u.url; label = u.label })
      plain
  in
  let accounts =
    service_accounts @ orcid_accounts @ atproto_accounts @ promoted
  in
  (* Drop an exact duplicate, keeping the first. This guards against the
     same handle arriving from two sources, such as an ORCID recorded both
     in the [orcid] field and as a URL, though no contact in the store
     currently does so. Two different handles on one platform are kept,
     because the schema permits them. *)
  let accounts =
    List.fold_left
      (fun acc a ->
        let seen =
          List.exists
            (fun b -> A.platform b = A.platform a && A.handle b = A.handle a)
            acc
        in
        if seen then acc else acc @ [ a ])
      [] accounts
  in
  (* [Sortal_schema_account.json_t] decodes accounts in ascending platform-key
     order, because it groups them through a [Map.Make(String)] on the way
     in. A value built here in a different order, such as the order
     [services] happened to list them, would not survive an encode and a
     decode unchanged: the round trip would differ only in account order,
     which is not a loss of data but does mean this function's output is
     not idempotent under that codec. Sorting here, stably so that two
     accounts on the same platform keep their relative order, matches what
     a decode always produces. *)
  let accounts =
    List.stable_sort
      (fun a b -> String.compare (P.key (A.platform a)) (P.key (A.platform b)))
      accounts
  in
  let kind =
    match V1.kind c with
    | V1.Person -> V2.Person
    | V1.Organization -> V2.Organization
  in
  (* [thumbnail] is a local file and [icon] a remote URL; prefer the local
     copy when both are present, as they are for every contact in the live
     store that sets either. *)
  let photo =
    match V1.thumbnail c with Some t -> Some t | None -> V1.icon c
  in
  let* affiliations =
    collect (affiliation_of_org ~contact:(V1.handle c)) (V1.organizations c)
  in
  Ok
    (V2.make ~handle:(V1.handle c) ~names:(V1.names c) ~kind
       ~emails:(List.map (fun (e : V1.email) -> e.address) (V1.emails c))
       ~accounts ~links ~affiliations ?photo
       ~feeds:(Option.value ~default:[] (V1.feeds c))
       ())
