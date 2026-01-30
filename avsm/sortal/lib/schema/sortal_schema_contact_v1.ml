(*---------------------------------------------------------------------------
  Copyright (c) 2025 Anil Madhavapeddy <anil@recoil.org>. All rights reserved.
  SPDX-License-Identifier: ISC
 ---------------------------------------------------------------------------*)

let version = 1

type contact_kind = Person | Organization | Group | Role

type activitypub_variant =
  | Mastodon
  | Pixelfed
  | PeerTube
  | Other_activitypub of string

type service_kind =
  | ActivityPub of activitypub_variant
  | Bluesky
  | Github
  | Git
  | Twitter
  | Photo
  | Custom of string

type service = {
  url: string;
  kind: service_kind option;
  handle: string option;
  label: string option;
  range: Sortal_schema_temporal.range option;
  primary: bool;
}

type email_type = Work | Personal | Other

type email = {
  address: string;
  type_: email_type option;
  range: Sortal_schema_temporal.range option;
  note: string option;
}

type organization = {
  name: string;
  title: string option;
  department: string option;
  range: Sortal_schema_temporal.range option;
  email: string option;
  url: string option;
}

type url_entry = {
  url: string;
  label: string option;
  range: Sortal_schema_temporal.range option;
}

type t = {
  version: int;
  kind: contact_kind;
  handle: string;
  names: string list;
  emails: email list;
  organizations: organization list;
  urls: url_entry list;
  services: service list;
  icon: string option;
  thumbnail: string option;
  orcid: string option;
  feeds: Sortal_schema_feed.t list option;
}

(* Helpers *)
let make_email ?type_ ?from ?until ?note address =
  let range = match from, until with
    | None, None -> None
    | _, _ -> Some (Sortal_schema_temporal.make ?from ?until ())
  in
  { address; type_; range; note }

let email_of_string address =
  { address; type_ = Some Personal; range = None; note = None }

let make_org ?title ?department ?from ?until ?email ?url name =
  let range = match from, until with
    | None, None -> None
    | _, _ -> Some (Sortal_schema_temporal.make ?from ?until ())
  in
  { name; title; department; range; email; url }

let make_url ?label ?from ?until url =
  let range = match from, until with
    | None, None -> None
    | _, _ -> Some (Sortal_schema_temporal.make ?from ?until ())
  in
  { url; label; range }

let url_of_string url =
  { url; label = None; range = None }

let make_service ?kind ?handle ?label ?from ?until ?(primary = false) url =
  let range = match from, until with
    | None, None -> None
    | _, _ -> Some (Sortal_schema_temporal.make ?from ?until ())
  in
  { url; kind; handle; label; range; primary }

let service_of_url url =
  { url; kind = None; handle = None; label = None; range = None; primary = false }

let make ~handle ~names ?(kind = Person) ?(emails = []) ?(organizations = [])
    ?(urls = []) ?(services = []) ?icon ?thumbnail ?orcid ?feeds () =
  { version; kind; handle; names; emails; organizations; urls; services;
    icon; thumbnail; orcid; feeds }

(* Accessors *)
let version_of t = t.version
let kind t = t.kind
let handle t = t.handle
let names t = t.names
let name t = List.hd t.names
let primary_name = name
let emails t = t.emails
let organizations t = t.organizations
let urls t = t.urls
let services t = t.services
let icon t = t.icon
let thumbnail t = t.thumbnail
let orcid t = t.orcid
let feeds t = t.feeds

(* Service convenience accessors *)
let github t =
  List.find_opt (fun (s : service) ->
    match s.kind with Some Github -> true | _ -> false
  ) t.services

let github_handle t =
  match github t with
  | Some s -> s.handle
  | None -> None

let twitter t =
  List.find_opt (fun (s : service) ->
    match s.kind with Some Twitter -> true | _ -> false
  ) t.services

let twitter_handle t =
  match twitter t with
  | Some s -> s.handle
  | None -> None

let mastodon t =
  List.find_opt (fun (s : service) ->
    match s.kind with Some (ActivityPub Mastodon) -> true | _ -> false
  ) t.services

let mastodon_handle t =
  match mastodon t with
  | Some s -> s.handle
  | None -> None

let bluesky t =
  List.find_opt (fun (s : service) ->
    match s.kind with Some Bluesky -> true | _ -> false
  ) t.services

let bluesky_handle t =
  match bluesky t with
  | Some s -> s.handle
  | None -> None

(* Temporal queries *)
let emails_at t ~date =
  Sortal_schema_temporal.at_date ~get:(fun (e : email) -> e.range) ~date t.emails

let email_at t ~date =
  match emails_at t ~date with
  | e :: _ -> Some e.address
  | [] -> None

let current_email t =
  match Sortal_schema_temporal.current ~get:(fun (e : email) -> e.range) t.emails with
  | Some e -> Some e.address
  | None -> None

let organization_at t ~date =
  match Sortal_schema_temporal.at_date ~get:(fun (o : organization) -> o.range) ~date t.organizations with
  | o :: _ -> Some o
  | [] -> None

let current_organization t =
  Sortal_schema_temporal.current ~get:(fun (o : organization) -> o.range) t.organizations

let url_at t ~date =
  match Sortal_schema_temporal.at_date ~get:(fun (u : url_entry) -> u.range) ~date t.urls with
  | u :: _ -> Some u.url
  | [] -> None

let current_url t =
  match Sortal_schema_temporal.current ~get:(fun (u : url_entry) -> u.range) t.urls with
  | Some u -> Some u.url
  | None -> None

let all_email_addresses t =
  List.map (fun e -> e.address) t.emails

(* Service queries *)
let services_of_kind t (kind : service_kind) =
  List.filter (fun (s : service) ->
    match (s.kind : service_kind option) with
    | Some k when k = kind -> true
    | _ -> false
  ) t.services

let services_at t ~date =
  Sortal_schema_temporal.at_date ~get:(fun (s : service) -> s.range) ~date t.services

let current_services t =
  List.filter (fun (s : service) -> Sortal_schema_temporal.is_current s.range) t.services

let primary_service t (kind : service_kind) =
  List.find_opt (fun (s : service) ->
    match (s.kind : service_kind option) with
    | Some k when k = kind && s.primary -> true
    | _ -> false
  ) t.services

let best_url t =
  current_url t
  |> Option.fold ~none:(
    match current_services t with
    | s :: _ -> Some s.url
    | [] -> current_email t |> Option.map (fun e -> "mailto:" ^ e)
  ) ~some:Option.some

(* Modification *)
let add_feed t feed =
  { t with feeds = Some (feed :: Option.value t.feeds ~default:[]) }

let remove_feed t url =
  { t with feeds = Option.map (List.filter (fun f -> Sortal_schema_feed.url f <> url)) t.feeds }

(* Comparison *)
let compare a b = String.compare a.handle b.handle

(* Type conversions *)
let contact_kind_to_string = function
  | Person -> "person"
  | Organization -> "organization"
  | Group -> "group"
  | Role -> "role"

let contact_kind_of_string = function
  | "person" -> Some Person
  | "organization" -> Some Organization
  | "group" -> Some Group
  | "role" -> Some Role
  | _ -> None

let activitypub_variant_to_string = function
  | Mastodon -> "mastodon"
  | Pixelfed -> "pixelfed"
  | PeerTube -> "peertube"
  | Other_activitypub s -> s

let activitypub_variant_of_string s =
  match String.lowercase_ascii s with
  | "mastodon" -> Mastodon
  | "pixelfed" -> Pixelfed
  | "peertube" -> PeerTube
  | _ -> Other_activitypub s

let service_kind_to_string = function
  | ActivityPub v -> "activitypub:" ^ activitypub_variant_to_string v
  | Bluesky -> "bluesky"
  | Github -> "github"
  | Git -> "git"
  | Twitter -> "twitter"
  | Photo -> "photo"
  | Custom s -> s

let service_kind_of_string s =
  match String.lowercase_ascii s with
  | "bluesky" -> Some Bluesky
  | "github" -> Some Github
  | "git" -> Some Git
  | "twitter" -> Some Twitter
  | "photo" -> Some Photo
  | "" | "custom" -> None
  | s when String.length s > 11 && String.sub s 0 11 = "activitypub" ->
    (* Handle activitypub:variant format *)
    let rest = String.sub s 11 (String.length s - 11) in
    let variant = if rest = "" then Mastodon
      else if String.length rest > 1 && rest.[0] = ':' then
        activitypub_variant_of_string (String.sub rest 1 (String.length rest - 1))
      else Mastodon
    in
    Some (ActivityPub variant)
  | "mastodon" -> Some (ActivityPub Mastodon)
  | "pixelfed" -> Some (ActivityPub Pixelfed)
  | "peertube" -> Some (ActivityPub PeerTube)
  | _ -> Some (Custom s)

let email_type_to_string = function
  | Work -> "work"
  | Personal -> "personal"
  | Other -> "other"

let email_type_of_string = function
  | "work" -> Some Work
  | "personal" -> Some Personal
  | "other" -> Some Other
  | _ -> None

(* JSON encoding *)

(* Helper: case-insensitive enum decoder *)
let case_insensitive_enum ~kind:kind_name cases =
  let open Jsont in
  let lowercase_cases = List.map (fun (s, v) -> (String.lowercase_ascii s, v)) cases in
  let dec s =
    match List.assoc_opt (String.lowercase_ascii s) lowercase_cases with
    | Some v -> v
    | None -> failwith ("unknown " ^ kind_name ^ ": " ^ s)
  in
  let enc v =
    match List.find_opt (fun (_, v') -> v = v') cases with
    | Some (s, _) -> s
    | None -> failwith ("invalid " ^ kind_name)
  in
  let t = map ~kind:kind_name ~dec ~enc string in
  t

let contact_kind_json =
  case_insensitive_enum ~kind:"ContactKind" [
    "person", Person;
    "organization", Organization;
    "group", Group;
    "role", Role;
  ]

let service_json : service Jsont.t =
  let open Jsont in
  let open Jsont.Object in
  let mem_opt f v ~enc = mem f v ~dec_absent:None ~enc_omit:Option.is_none ~enc in
  (* Convert string option to/from service_kind option *)
  let dec_kind_opt kind_str =
    match kind_str with
    | None -> None
    | Some s -> service_kind_of_string s
  in
  let enc_kind_opt = Option.map service_kind_to_string in
  let make url kind_str handle label range primary : service =
    let kind = dec_kind_opt kind_str in
    { url; kind; handle; label; range; primary }
  in
  map ~kind:"Service" make
  |> mem "url" string ~enc:(fun (s : service) -> s.url)
  |> mem_opt "kind" (some string) ~enc:(fun (s : service) -> enc_kind_opt s.kind)
  |> mem_opt "handle" (some string) ~enc:(fun (s : service) -> s.handle)
  |> mem_opt "label" (some string) ~enc:(fun (s : service) -> s.label)
  |> mem_opt "range" (some Sortal_schema_temporal.json_t) ~enc:(fun (s : service) -> s.range)
  |> mem "primary" bool ~dec_absent:false ~enc:(fun (s : service) -> s.primary)
  |> finish

let email_type_json =
  case_insensitive_enum ~kind:"EmailType" [
    "work", Work;
    "personal", Personal;
    "other", Other;
  ]

let email_json : email Jsont.t =
  let open Jsont in
  let open Jsont.Object in
  let mem_opt f v ~enc = mem f v ~dec_absent:None ~enc_omit:Option.is_none ~enc in
  let make address type_ range note : email = { address; type_; range; note } in
  map ~kind:"Email" make
  |> mem "address" string ~enc:(fun (e : email) -> e.address)
  |> mem_opt "type" (some email_type_json) ~enc:(fun (e : email) -> e.type_)
  |> mem_opt "range" (some Sortal_schema_temporal.json_t) ~enc:(fun (e : email) -> e.range)
  |> mem_opt "note" (some string) ~enc:(fun (e : email) -> e.note)
  |> finish

let organization_json : organization Jsont.t =
  let open Jsont in
  let open Jsont.Object in
  let mem_opt f v ~enc = mem f v ~dec_absent:None ~enc_omit:Option.is_none ~enc in
  let make name title department range email url : organization =
    { name; title; department; range; email; url }
  in
  map ~kind:"Organization" make
  |> mem "name" string ~enc:(fun (o : organization) -> o.name)
  |> mem_opt "title" (some string) ~enc:(fun (o : organization) -> o.title)
  |> mem_opt "department" (some string) ~enc:(fun (o : organization) -> o.department)
  |> mem_opt "range" (some Sortal_schema_temporal.json_t) ~enc:(fun (o : organization) -> o.range)
  |> mem_opt "email" (some string) ~enc:(fun (o : organization) -> o.email)
  |> mem_opt "url" (some string) ~enc:(fun (o : organization) -> o.url)
  |> finish

let url_entry_json : url_entry Jsont.t =
  let open Jsont in
  let open Jsont.Object in
  let mem_opt f v ~enc = mem f v ~dec_absent:None ~enc_omit:Option.is_none ~enc in
  let make url label range : url_entry = { url; label; range } in
  map ~kind:"URL" make
  |> mem "url" string ~enc:(fun (u : url_entry) -> u.url)
  |> mem_opt "label" (some string) ~enc:(fun (u : url_entry) -> u.label)
  |> mem_opt "range" (some Sortal_schema_temporal.json_t) ~enc:(fun (u : url_entry) -> u.range)
  |> finish

let json_t =
  let open Jsont in
  let open Jsont.Object in
  let mem_opt f v ~enc = mem f v ~dec_absent:None ~enc_omit:Option.is_none ~enc in
  let make version kind handle names emails organizations urls services
           icon thumbnail orcid feeds =
    if version <> 1 then
      failwith (Printf.sprintf "Unsupported contact schema version: %d" version);
    { version; kind; handle; names; emails; organizations; urls; services;
      icon; thumbnail; orcid; feeds }
  in
  map ~kind:"Contact" make
  |> mem "version" int ~enc:(fun _ -> 1)
  |> mem "kind" contact_kind_json ~dec_absent:Person ~enc:(fun c -> c.kind)
  |> mem "handle" string ~enc:(fun c -> c.handle)
  |> mem "names" (list string) ~dec_absent:[] ~enc:(fun c -> c.names)
  |> mem "emails" (list email_json) ~dec_absent:[] ~enc:(fun c -> c.emails)
  |> mem "organizations" (list organization_json) ~dec_absent:[] ~enc:(fun c -> c.organizations)
  |> mem "urls" (list url_entry_json) ~dec_absent:[] ~enc:(fun c -> c.urls)
  |> mem "services" (list service_json) ~dec_absent:[] ~enc:(fun c -> c.services)
  |> mem_opt "icon" (some string) ~enc:(fun c -> c.icon)
  |> mem_opt "thumbnail" (some string) ~enc:(fun c -> c.thumbnail)
  |> mem_opt "orcid" (some string) ~enc:(fun c -> c.orcid)
  |> mem_opt "feeds" (some (list Sortal_schema_feed.json_t)) ~enc:(fun c -> c.feeds)
  |> finish

(* Pretty printing *)
let pp ppf t =
  let open Fmt in
  let label = styled (`Fg `Cyan) string in
  let url_style = styled (`Fg `Blue) in
  let date_style = styled (`Fg `Green) in
  let field lbl fmt_v = Option.iter (fun v -> pf ppf "%a: %a@," label lbl fmt_v v) in

  let pp_range ppf = function
    | None -> ()
    | Some { Sortal_schema_temporal.from; until } ->
      match from, until with
      | Some f, Some u ->
          let fs = Sortal_schema_temporal.format_date f in
          let us = Sortal_schema_temporal.format_date u in
          pf ppf " %a" (date_style string) (Printf.sprintf "[%s to %s]" fs us)
      | Some f, None ->
          let fs = Sortal_schema_temporal.format_date f in
          pf ppf " %a" (date_style string) (Printf.sprintf "[from %s]" fs)
      | None, Some u ->
          let us = Sortal_schema_temporal.format_date u in
          pf ppf " %a" (date_style string) (Printf.sprintf "[until %s]" us)
      | None, None -> ()
  in

  pf ppf "@[<v>";
  pf ppf "%a: %a@," label "Handle" (styled `Bold (fun ppf s -> pf ppf "@%s" s)) t.handle;

  (* Show kind if not a person *)
  (match t.kind with
   | Person -> ()
   | k -> pf ppf "%a: %a@," label "Kind" (styled (`Fg `Magenta) string) (contact_kind_to_string k));

  pf ppf "%a: %a@," label "Name" (styled `Bold string) (name t);

  if List.length (names t) > 1 then
    pf ppf "%a: @[<h>%a@]@," label "Aliases"
      (list ~sep:comma string) (List.tl (names t));

  (* Emails with temporal info *)
  if emails t <> [] then begin
    pf ppf "%a:@," label "Emails";
    List.iter (fun e ->
      pf ppf "  %a%s%s%a%a@,"
        (styled (`Fg `Yellow) string) e.address
        (match e.type_ with Some Work -> " (work)" | Some Personal -> " (personal)" | Some Other -> " (other)" | None -> "")
        (match e.note with Some n -> " - " ^ n | None -> "")
        pp_range e.range
        (fun ppf current -> if current then pf ppf " %a" (styled (`Fg `Magenta) string) "[current]" else ())
        (Sortal_schema_temporal.is_current e.range)
    ) (emails t)
  end;

  (* Organizations with temporal info *)
  if organizations t <> [] then begin
    pf ppf "%a:@," label "Organizations";
    List.iter (fun o ->
      pf ppf "  %a" (styled `Bold string) o.name;
      Option.iter (fun title -> pf ppf " - %s" title) o.title;
      Option.iter (fun dept -> pf ppf " (%s)" dept) o.department;
      pf ppf "%a" pp_range o.range;
      if Sortal_schema_temporal.is_current o.range then
        pf ppf " %a" (styled (`Fg `Magenta) string) "[current]";
      pf ppf "@,";
      Option.iter (fun email -> pf ppf "    Email: %a@," (styled (`Fg `Yellow) string) email) o.email;
      Option.iter (fun url -> pf ppf "    URL: %a@," (url_style string) url) o.url;
    ) (organizations t)
  end;

  (* URLs *)
  if urls t <> [] then begin
    pf ppf "%a:@," label "URLs";
    List.iter (fun u ->
      pf ppf "  %a" (url_style string) u.url;
      Option.iter (fun lbl -> pf ppf " (%s)" lbl) u.label;
      pf ppf "%a" pp_range u.range;
      if Sortal_schema_temporal.is_current u.range then
        pf ppf " %a" (styled (`Fg `Magenta) string) "[current]";
      pf ppf "@,"
    ) (urls t)
  end;

  (* Services *)
  if services t <> [] then begin
    pf ppf "%a:@," label "Services";
    List.iter (fun (s : service) ->
      pf ppf "  %a" (url_style string) s.url;
      Option.iter (fun k -> pf ppf " (%s)" (service_kind_to_string k)) s.kind;
      Option.iter (fun h -> pf ppf " [@%s]" h) s.handle;
      Option.iter (fun lbl -> pf ppf " - %s" lbl) s.label;
      pf ppf "%a" pp_range s.range;
      if s.primary then pf ppf " %a" (styled (`Fg `Yellow) string) "[primary]";
      if Sortal_schema_temporal.is_current s.range then
        pf ppf " %a" (styled (`Fg `Magenta) string) "[current]";
      pf ppf "@,"
    ) (services t)
  end;

  field "ORCID" (url_style (fun ppf o -> pf ppf "https://orcid.org/%s" o)) t.orcid;

  field "Icon" (url_style string) t.icon;
  field "Thumbnail" (styled (`Fg `White) string) t.thumbnail;

  Option.iter (function
    | [] -> ()
    | feeds ->
      pf ppf "%a:@," label "Feeds";
      List.iter (fun feed -> pf ppf "  - %a@," Sortal_schema_feed.pp feed) feeds
  ) t.feeds;

  pf ppf "@]"
