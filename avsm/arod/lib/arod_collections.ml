(*---------------------------------------------------------------------------
  Copyright (c) 2025 Anil Madhavapeddy <anil@recoil.org>. All rights reserved.
  SPDX-License-Identifier: ISC
 ---------------------------------------------------------------------------*)

module Entry = Bushel.Entry
module Paper = Bushel.Paper
module Contact = Sortal_schema.Contact
module FeedEntry = Sortal_feed.Entry
module Idea = Bushel.Idea
module Ia = Stdlib_stable.Iarray

type feed_item = {
  contact : Sortal_schema.Contact.t;
  entry : Sortal_feed.Entry.t;
  mentions : Bushel.Entry.entry list;
}

type link_group = {
  ent : Entry.entry;
  links : Bushel.Link_graph.external_link list;
}

type timeline_item = Feed_item of feed_item * (int * int * int)

let timeline_date (Feed_item (_, d)) = d

type month_section = {
  year : int;
  month : int;
  collaborators : Contact.t list;
  items : timeline_item list;
}

let find_contact_by_handle entries handle =
  List.find_opt
    (fun contact -> Sortal_schema.Contact.handle contact = handle)
    (Bushel.Entry.contacts entries)

(** [host_and_path url] is the host and path of [url], or [("", "/")]. *)
let host_and_path url =
  match Uriz.of_string url with
  | Null -> ("", "/")
  | This u ->
    ( (match Uriz.host u with Null -> "" | This h -> h),
      match Uriz.path u with "" -> "/" | p -> p )

(** Domains that do not identify a single contact. *)
let shared_platforms = [
  "github.com"; "gitlab.com"; "codeberg.org"; "tangled.org"; "bitbucket.org";
  "twitter.com"; "x.com"; "bsky.app"; "bsky.social";
  "mastodon.social"; "mastodon.online"; "hachyderm.io";
  "linkedin.com"; "youtube.com"; "youtu.be";
  "arxiv.org"; "doi.org"; "orcid.org";
  "reddit.com"; "news.ycombinator.com";
  "threads.com";
  "medium.com"; "substack.com"; "wordpress.com";
  "en.wikipedia.org"; "wikipedia.org";
  "scholar.google.com"; "google.com";
  "stackoverflow.com"; "stackexchange.com";
  "researchgate.net"; "academia.edu";
  "amazon.com"; "goodreads.com";
]

let strip_www s =
  if String.length s > 4 && String.starts_with ~prefix:"www." s then
    String.sub s 4 (String.length s - 4) else s

(** [build_contact_by_domain contacts] indexes contact URLs by domain and path. *)
let build_contact_by_domain contacts =
  let tbl : (string, (string * Contact.t) list) Hashtbl.t = Hashtbl.create 64 in
  let is_shared h = List.mem h shared_platforms in
  let add_url c url_str =
    match host_and_path url_str with
    | "", _ -> ()
    | h, path ->
      let bare = strip_www (String.lowercase_ascii h) in
      if not (is_shared bare) then begin
        let cur = Option.value ~default:[] (Hashtbl.find_opt tbl bare) in
        if not (List.exists (fun (p, _) -> p = path) cur) then
          Hashtbl.replace tbl bare ((path, c) :: cur)
      end
  in
  List.iter (fun c ->
    List.iter (fun (l : Contact.link) -> add_url c l.url) (Contact.links c);
    List.iter (fun a -> add_url c (Contact.Account.url a)) (Contact.accounts c)
  ) contacts;
  Bushel.Smap.of_list (Hashtbl.fold (fun k v acc -> (k, v) :: acc) tbl [])

(** [find_contact_for_url index host path] is the best matching contact. *)
let find_contact_for_url contact_by_domain bare_host url_path =
  match Bushel.Smap.find_opt bare_host contact_by_domain with
  | None -> None
  | Some entries ->
    List.fold_left (fun best (prefix, contact) ->
      if not (String.starts_with ~prefix url_path) then best
      else match best with
        | Some (old, _) when String.length old >= String.length prefix -> best
        | _ -> Some (prefix, contact)
    ) None entries
    |> Option.map snd

(** [compute_groups ~entries] is the external-link groups in reverse date order. *)
let compute_groups ~entries =
  let all_links = Entry.all_external_links entries in
  let by_source : (string, Bushel.Link_graph.external_link list) Hashtbl.t =
    Hashtbl.create 128 in
  List.iter (fun (link : Bushel.Link_graph.external_link) ->
    let cur = Option.value ~default:[] (Hashtbl.find_opt by_source link.source) in
    if List.exists (fun (l : Bushel.Link_graph.external_link) -> l.url = link.url) cur then ()
    else Hashtbl.replace by_source link.source (link :: cur)
  ) all_links;
  let groups = Hashtbl.fold (fun slug links acc ->
    match Entry.lookup entries slug with
    | Some ent -> { ent; links } :: acc
    | None -> acc
  ) by_source [] in
  List.sort (fun a b ->
    compare (Entry.date b.ent) (Entry.date a.ent)
  ) groups

(** [month_collaborators ~entries bushel_entries feed_items] is the month's collaborator
    list ordered by appearance count. *)
let month_collaborators ~entries bushel_entries feed_items =
  let counts : (string, int) Hashtbl.t = Hashtbl.create 16 in
  let contact_map : (string, Contact.t) Hashtbl.t = Hashtbl.create 16 in
  let bump contact =
    let h = Contact.handle contact in
    Hashtbl.replace contact_map h contact;
    let cur = Option.value ~default:0 (Hashtbl.find_opt counts h) in
    Hashtbl.replace counts h (cur + 1)
  in
  List.iter (fun ent ->
    let tags = Bushel.Entry.tags_of_ent ent in
    List.iter (function
      | `Contact handle ->
        (match find_contact_by_handle entries handle with
         | Some c -> bump c
         | None -> ())
      | _ -> ()
    ) tags
  ) bushel_entries;
  List.iter (fun ent ->
    match ent with
    | `Paper paper ->
      List.iter (fun author_name ->
        match Entry.lookup_by_name entries author_name with
        | Some c -> bump c
        | None -> ()
      ) (Paper.authors paper)
    | _ -> ()
  ) bushel_entries;
  List.iter (fun (item : feed_item) ->
    bump item.contact
  ) feed_items;
  let contacts_with_counts =
    Hashtbl.fold (fun h count acc ->
      match Hashtbl.find_opt contact_map h with
      | Some c -> (c, count) :: acc
      | None -> acc
    ) counts []
  in
  let sorted = List.sort (fun (_, a) (_, b) -> compare b a) contacts_with_counts in
  List.map fst sorted

let compute_month_sections ~entries ~feed_items =
  let all_entries = Entry.all_entries entries in
  let all_feed_items = feed_items in

  let bushel_by_month : (int * int, Entry.entry list) Hashtbl.t = Hashtbl.create 64 in
  List.iter (fun ent ->
    let (y, m, _d) = Entry.date ent in
    let key = (y, m) in
    let cur = Option.value ~default:[] (Hashtbl.find_opt bushel_by_month key) in
    Hashtbl.replace bushel_by_month key (ent :: cur)
  ) all_entries;

  let feed_by_month : (int * int, feed_item list) Hashtbl.t = Hashtbl.create 64 in
  List.iter (fun (item : feed_item) ->
    match item.entry.FeedEntry.date with
    | Some d ->
      let (y, m, _d), _ = Ptime.to_date_time d in
      let key = (y, m) in
      let cur = Option.value ~default:[] (Hashtbl.find_opt feed_by_month key) in
      Hashtbl.replace feed_by_month key (item :: cur)
    | None -> ()
  ) all_feed_items;

  let months =
    Hashtbl.fold (fun k _ acc -> k :: acc) feed_by_month []
    |> List.sort (fun (y1, m1) (y2, m2) ->
      let c = compare y2 y1 in if c <> 0 then c else compare m2 m1)
  in

  List.map (fun (y, m) ->
    let bushel_ents =
      Hashtbl.find_opt bushel_by_month (y, m)
      |> Option.value ~default:[] |> List.rev
    in
    let feed_items =
      Hashtbl.find_opt feed_by_month (y, m)
      |> Option.value ~default:[] |> List.rev
    in
    let collaborators = month_collaborators ~entries bushel_ents feed_items in
    let timeline =
      List.map (fun (item : feed_item) ->
        let d = match item.entry.FeedEntry.date with
          | Some pt -> let (y, m, d), _ = Ptime.to_date_time pt in (y, m, d)
          | None -> (y, m, 1)
        in
        Feed_item (item, d)
      ) feed_items
      |> List.sort (fun a b -> compare (timeline_date b) (timeline_date a))
    in
    { year = y; month = m; collaborators; items = timeline }
  ) months

(** [build_idea_index entries] maps students to their ideas. *)
let build_idea_index entries =
  let tbl : (string, (string * string) list) Hashtbl.t = Hashtbl.create 64 in
  List.iter (fun idea ->
    List.iter (fun handle ->
      let cur = Option.value ~default:[] (Hashtbl.find_opt tbl handle) in
      let pair = (Idea.slug idea, Idea.title idea) in
      if not (List.mem pair cur) then
        Hashtbl.replace tbl handle (pair :: cur)
    ) (Idea.student_handles idea)
  ) (Entry.ideas entries);
  Bushel.Smap.of_list (Hashtbl.fold (fun k v acc -> (k, v) :: acc) tbl [])

type contact_index = (string * Contact.t) list Bushel.Smap.t

type t = {
  links : link_group iarray;
  months : month_section iarray;
  contact_domains : (string * Contact.t) list Bushel.Smap.t;
  student_ideas : (string * string) list Bushel.Smap.t;
}

let create ~entries ~feed_items = {
  links = Ia.of_list (compute_groups ~entries);
  months = Ia.of_list (compute_month_sections ~entries ~feed_items);
  contact_domains = build_contact_by_domain (Entry.contacts entries);
  student_ideas = build_idea_index entries;
}

let links t = t.links
let months t = t.months
let contact_domains t = t.contact_domains
let student_ideas t = t.student_ideas
let length = Ia.length
let to_list = Ia.to_list

let page items ~offset ~limit =
  let length = Ia.length items in
  let offset = max 0 (min offset length) in
  let count = max 0 (min limit (length - offset)) in
  let rec collect i acc =
    if i < offset then acc
    else collect (i - 1) (Ia.get items i :: acc)
  in
  collect (offset + count - 1) []
