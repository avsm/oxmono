(*---------------------------------------------------------------------------
  Copyright (c) 2025 Anil Madhavapeddy <anil@recoil.org>. All rights reserved.
  SPDX-License-Identifier: ISC
 ---------------------------------------------------------------------------*)

module Account = Sortal_schema_account
module Platform = Sortal_schema_platform
module Date = Sortal_schema_date
module Feed = Sortal_schema_feed
module Smap = Stdlib.Map.Make (String)

let version = 2

type kind = Person | Organization

type link = { url : string; label : string option }

type affiliation = {
  org : string;
  department : string option;
  title : string option;
  url : string option;
  address : string option;
  from : Date.t option;
  until : Date.t option;
}

type t = {
  kind : kind;
  handle : string;
  names : string list;
  emails : string list;
  accounts : Account.t list;
  links : link list;
  affiliations : affiliation list;
  photo : string option;
  feeds : Feed.t list;
  vcard : (string * string) list;
}

let make ~handle ~names ?(kind = Person) ?(emails = []) ?(accounts = [])
    ?(links = []) ?(affiliations = []) ?photo ?(feeds = []) ?(vcard = []) ()
    =
  { kind; handle; names; emails; accounts; links; affiliations; photo;
    feeds; vcard }

let kind t = t.kind
let handle t = t.handle
let names t = t.names
let name t = match t.names with n :: _ -> n | [] -> t.handle
let emails t = t.emails
let accounts t = t.accounts
let links t = t.links
let affiliations t = t.affiliations
let photo t = t.photo
let feeds t = t.feeds
let vcard t = t.vcard

let accounts_on t p =
  List.filter (fun a -> Account.platform a = p) t.accounts

let account_on t p = List.nth_opt (accounts_on t p) 0
let handle_on t p = Option.map Account.handle (account_on t p)
let url_on t p = Option.map Account.url (account_on t p)

let atproto t =
  List.find_map
    (function Account.Atproto a -> Some a | _ -> None)
    t.accounts

let atproto_handle t =
  Option.map (fun (a : Account.atproto) -> a.handle) (atproto t)

let atproto_did t =
  Option.bind (atproto t) (fun (a : Account.atproto) -> a.did)

let set_atproto_did t did =
  let replace = function
    | Account.Atproto a -> Account.Atproto { a with did = Some did }
    | other -> other
  in
  { t with accounts = List.map replace t.accounts }

(* Decoding does not preserve file order (see [Sortal_schema_account.json_t]),
   so the "best" account is picked by sorting on platform key rather than by
   taking the head of [t.accounts], which would be file order for a value
   built by [make] and decode order for a decoded one. *)
let best_url t =
  match t.links with
  | { url; _ } :: _ -> Some url
  | [] -> (
      match t.accounts with
      | [] -> None
      | accounts ->
          let key a = Platform.key (Account.platform a) in
          let sorted =
            List.sort (fun a b -> String.compare (key a) (key b)) accounts
          in
          Some (Account.url (List.hd sorted)))

let current_affiliation t =
  List.find_opt (fun a -> a.until = None) t.affiliations

let add_feed t feed = { t with feeds = t.feeds @ [ feed ] }

let remove_feed t url =
  { t with feeds = List.filter (fun f -> Feed.url f <> url) t.feeds }

let check t =
  if t.names = [] then Error "a contact needs at least one name"
  else
    List.fold_left
      (fun acc a -> match acc with Error _ -> acc | Ok () -> Account.check a)
      (Ok ()) t.accounts

let compare a b = String.compare a.handle b.handle

let kind_to_string = function
  | Person -> "person"
  | Organization -> "organization"

let kind_of_string = function
  | "person" -> Some Person
  | "organization" -> Some Organization
  | _ -> None

let kind_json =
  let dec meta s =
    match kind_of_string s with
    | Some k -> k
    | None -> Jsont.Error.msgf meta "Kind: unknown contact kind: %S" s
  in
  Jsont.Base.string (Jsont.Base.map ~kind:"Kind" ~dec ~enc:kind_to_string ())

(* A link with no label encodes as a bare string, which is what 318 of the
   319 links in the live database are. *)
(* [l.url] and [l.label] would otherwise disambiguate to [affiliation],
   which also has [url] and is declared later in this file, so [l] is
   annotated explicitly rather than relying on inference to pick [link]. *)
let link_json =
  let obj =
    let open Jsont.Object in
    map ~kind:"Link" (fun url label -> { url; label })
    |> mem "url" Jsont.string ~enc:(fun (l : link) -> l.url)
    |> opt_mem "label" Jsont.string ~enc:(fun (l : link) -> l.label)
    |> finish
  in
  Jsont.any ~kind:"Link"
    ~dec_string:
      (Jsont.map ~dec:(fun url -> { url; label = None }) Jsont.string)
    ~dec_object:obj
    ~enc:(fun (l : link) ->
      match l.label with
      | None -> Jsont.map ~enc:(fun (l : link) -> l.url) Jsont.string
      | Some _ -> obj)
    ()

let affiliation_json =
  let open Jsont.Object in
  map ~kind:"Affiliation"
    (fun org department title url address from until ->
      { org; department; title; url; address; from; until })
  |> mem "org" Jsont.string ~enc:(fun (a : affiliation) -> a.org)
  |> opt_mem "department" Jsont.string ~enc:(fun (a : affiliation) -> a.department)
  |> opt_mem "title" Jsont.string ~enc:(fun (a : affiliation) -> a.title)
  |> opt_mem "url" Jsont.string ~enc:(fun (a : affiliation) -> a.url)
  |> opt_mem "address" Jsont.string ~enc:(fun (a : affiliation) -> a.address)
  |> opt_mem "from" Date.json_t ~enc:(fun (a : affiliation) -> a.from)
  |> opt_mem "until" Date.json_t ~enc:(fun (a : affiliation) -> a.until)
  |> finish

let vcard_json =
  Jsont.map ~kind:"VCard"
    ~dec:(fun m -> Smap.bindings m)
    ~enc:(fun l -> List.fold_left (fun m (k, v) -> Smap.add k v m) Smap.empty l)
    (Jsont.Object.as_string_map Jsont.string)

(* Omit a collection that is empty rather than writing it as null, which is
   what V1 does in 173 of the 460 live files. *)
let list_mem name codec get =
  Jsont.Object.mem name codec ~dec_absent:[] ~enc:get
    ~enc_omit:(fun v -> v = [])

let json_t =
  let open Jsont.Object in
  map ~kind:"ContactV2"
    (fun v kind handle names emails accounts links affiliations photo feeds
         vcard ->
      if v <> version then
        Jsont.Error.msgf Jsont.Meta.none
          "ContactV2: expected schema version %d, got %d" version v;
      { kind; handle; names; emails; accounts; links; affiliations; photo;
        feeds; vcard })
  |> mem "version" Jsont.int ~enc:(fun _ -> version)
  |> mem "kind" kind_json ~enc:(fun c -> c.kind)
  |> mem "handle" Jsont.string ~enc:(fun c -> c.handle)
  |> mem "names" (Jsont.list Jsont.string) ~enc:(fun c -> c.names)
  |> list_mem "emails" (Jsont.list Jsont.string) (fun c -> c.emails)
  |> list_mem "accounts" Account.json_t (fun c -> c.accounts)
  |> list_mem "links" (Jsont.list link_json) (fun c -> c.links)
  |> list_mem "affiliations" (Jsont.list affiliation_json)
       (fun c -> c.affiliations)
  |> opt_mem "photo" Jsont.string ~enc:(fun c -> c.photo)
  |> list_mem "feeds" (Jsont.list Feed.json_t) (fun c -> c.feeds)
  |> list_mem "vcard" vcard_json (fun c -> c.vcard)
  |> finish

let pp ppf t =
  Fmt.pf ppf "@[<v>%a (%a)@,%a@]"
    Fmt.(styled `Bold string) (name t)
    Fmt.(styled `Faint string) t.handle
    Fmt.(list ~sep:cut string)
    (List.map
       (fun a -> Platform.key (Account.platform a) ^ ": " ^ Account.handle a)
       t.accounts)
