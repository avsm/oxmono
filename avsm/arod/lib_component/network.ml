(*---------------------------------------------------------------------------
  Copyright (c) 2025 Anil Madhavapeddy <anil@recoil.org>. All rights reserved.
  SPDX-License-Identifier: ISC
 ---------------------------------------------------------------------------*)

(** Network page component.

    Unified chronological timeline grouped by month. Each month section
    shows collaborators (overlapping 36px avatar circles) then interleaves
    bushel entries and external feed entries chronologically. Paginated
    via infinite scrolling. *)

open Htmlit

module Entry = Bushel.Entry
module Paper = Bushel.Paper
module Contact = Sortal_schema.Contact
module Feed = Sortal_schema.Feed
module FeedEntry = Sortal_feed.Entry
module Idea = Bushel.Idea
module I = Arod.Icons

(** {1 Forward Links} *)

(** Normalise a URL for matching: strip www. prefix from host, remove trailing slash. *)
let normalise_url url =
  match Uri.of_string url |> Uri.host with
  | Some host ->
    let host' = Common.strip_www host in
    let u = Uri.of_string url in
    let u = Uri.with_host u (Some host') in
    let path = Uri.path u in
    let path = if String.length path > 1 && path.[String.length path - 1] = '/'
      then String.sub path 0 (String.length path - 1) else path in
    Uri.to_string (Uri.with_path u path)
  | None -> url

(** Build a reverse index from normalised external URL → source slugs, so we
    can find local entries that link TO a given feed entry URL. *)
let build_forward_index () =
  let tbl : (string, string list) Hashtbl.t = Hashtbl.create 256 in
  List.iter (fun (link : Bushel.Link_graph.external_link) ->
    let key = normalise_url link.url in
    let cur = try Hashtbl.find tbl key with Not_found -> [] in
    if not (List.mem link.source cur) then
      Hashtbl.replace tbl key (link.source :: cur)
  ) (Bushel.Link_graph.all_external_links ());
  tbl

(** {1 Timeline Item} *)

type timeline_item =
  | Bushel of Entry.entry * (int * int * int)
  | Feed_item of Arod.Ctx.feed_item * (int * int * int)

let timeline_date = function
  | Bushel (_, d) -> d
  | Feed_item (_, d) -> d

(** {1 Month Section Data} *)

type month_section = {
  year : int;
  month : int;
  collaborators : Contact.t list;
  items : timeline_item list;
}

(** {1 Collaborator Computation} *)

(** Compute collaborators for a given month, sorted by appearance count
    (most frequent first). Sources:
    1. @Contact tags on bushel entries
    2. Paper co-authors matched to known contacts
    3. Contacts whose feed entries appear that month *)
let month_collaborators ~ctx bushel_entries feed_items =
  let counts : (string, int) Hashtbl.t = Hashtbl.create 16 in
  let contact_map : (string, Contact.t) Hashtbl.t = Hashtbl.create 16 in
  let bump contact =
    let h = Contact.handle contact in
    Hashtbl.replace contact_map h contact;
    let cur = try Hashtbl.find counts h with Not_found -> 0 in
    Hashtbl.replace counts h (cur + 1)
  in
  (* 1. @Contact tags on bushel entries *)
  List.iter (fun ent ->
    let tags = Arod.Ctx.tags_of_ent ctx ent in
    List.iter (function
      | `Contact handle ->
        (match Arod.Ctx.lookup_by_handle ctx handle with
         | Some c -> bump c
         | None -> ())
      | _ -> ()
    ) tags
  ) bushel_entries;
  (* 2. Paper co-authors *)
  List.iter (fun ent ->
    match ent with
    | `Paper paper ->
      List.iter (fun author_name ->
        match Arod.Ctx.lookup_by_name ctx author_name with
        | Some c -> bump c
        | None -> ()
      ) (Paper.authors paper)
    | _ -> ()
  ) bushel_entries;
  (* 3. Contacts from feed items *)
  List.iter (fun (item : Arod.Ctx.feed_item) ->
    bump item.contact
  ) feed_items;
  (* Sort by count descending *)
  let contacts_with_counts =
    Hashtbl.fold (fun h count acc ->
      match Hashtbl.find_opt contact_map h with
      | Some c -> (c, count) :: acc
      | None -> acc
    ) counts []
  in
  let sorted = List.sort (fun (_, a) (_, b) -> compare b a) contacts_with_counts in
  List.map fst sorted

(** {1 Rendering} *)

(** Render a collaborator avatar (36px overlapping circle). *)
let render_avatar ~entries contact =
  let name = Contact.name contact in
  let thumb = Entry.contact_thumbnail entries contact in
  match thumb with
  | Some src ->
    El.a ~at:[At.href (match Contact.best_url contact with Some u -> u | None -> "#");
              At.class' "network-avatar-wrap";
              At.v "title" name]
      [El.img ~at:[At.src src; At.v "alt" name;
                    At.class' "network-avatar"] ()]
  | None ->
    let initials = Common.contact_initials name in
    El.a ~at:[At.href (match Contact.best_url contact with Some u -> u | None -> "#");
              At.class' "network-avatar-wrap";
              At.v "title" name]
      [El.span ~at:[At.class' "network-avatar-initials"]
         [El.txt initials]]

(** Render a feed entry row in the network timeline. *)
let render_feed_item ~entries ~forward_index ~idea_index (item : Arod.Ctx.feed_item) ((_y, _m, day) : int * int * int) =
  let fe = item.entry in
  let contact = item.contact in
  let name = Contact.name contact in
  let thumb = Entry.contact_thumbnail entries contact in
  (* Avatar *)
  let avatar_el = match thumb with
    | Some src ->
      El.img ~at:[At.src src; At.v "alt" name;
                  At.class' "network-feed-avatar"] ()
    | None ->
      El.span ~at:[At.class' "network-avatar-initials network-feed-avatar"]
        [El.txt (Common.contact_initials name)]
  in
  (* Title *)
  let title_el = Common.feed_entry_title_el fe in
  (* Badge — hidden on mobile *)
  let badge_el = El.span ~at:[At.class' "hidden md:inline"]
    [Common.feed_type_badge fe.FeedEntry.source_type] in
  (* Contact name on the right *)
  let name_el = match Contact.best_url contact with
    | Some u ->
      El.a ~at:[At.href u; At.class' "network-feed-name no-underline"]
        [El.txt name]
    | None ->
      El.span ~at:[At.class' "network-feed-name"]
        [El.txt name]
  in
  (* Summary — inline, flows after author *)
  let summary_el =
    match Common.feed_entry_summary ~max_len:150 fe with
    | Some text ->
      El.span ~at:[At.class' "network-feed-summary"]
        [El.txt (" \xe2\x80\x94 " ^ text)]
    | None -> El.void
  in
  (* Mentions: local entries that this feed entry references (backlinks) *)
  let mention_els = match item.mentions with
    | [] -> El.void
    | mentions ->
      El.div ~at:[At.class' "feed-item-mentions pl-0"]
        (List.map (fun entry ->
          let type_icon = Sidebar.entry_type_icon ~opacity:"opacity-60" ~size:10 entry in
          El.a ~at:[At.href (Entry.site_url entry);
                    At.class' "link-backlink-chip no-underline"]
            [El.unsafe_raw type_icon;
             El.txt (Entry.title entry)]
        ) mentions)
  in
  (* Forward links: local entries that link TO this feed entry *)
  let forward_els =
    match fe.FeedEntry.url with
    | Some u ->
      let url_str = normalise_url (Uri.to_string u) in
      let slugs = try Hashtbl.find forward_index url_str with Not_found -> [] in
      let forward_entries = List.filter_map (fun slug ->
        Entry.lookup entries slug
      ) slugs in
      (match forward_entries with
       | [] -> El.void
       | fwds ->
         El.div ~at:[At.class' "feed-item-mentions pl-0"]
           (List.map (fun entry ->
             let fwd_icon = I.outline ~cl:"opacity-60" ~size:10 I.external_link_o in
             El.a ~at:[At.href (Entry.site_url entry);
                       At.class' "link-backlink-chip no-underline"]
               [El.unsafe_raw fwd_icon;
                El.txt (Entry.title entry)]
           ) fwds))
    | None -> El.void
  in
  (* Idea backlinks: if the contact is a student on any idea, link to it *)
  let idea_els =
    let handle = Contact.handle contact in
    let ideas = try Hashtbl.find idea_index handle with Not_found -> [] in
    match ideas with
    | [] -> El.void
    | ideas ->
      El.div ~at:[At.class' "feed-item-mentions pl-0"]
        (List.map (fun (idea_slug, idea_title) ->
          let idea_icon = I.outline ~cl:"opacity-60" ~size:10 I.bulb_o in
          El.a ~at:[At.href ("/ideas/" ^ idea_slug);
                    At.class' "link-backlink-chip no-underline"]
            [El.unsafe_raw idea_icon;
             El.txt idea_title]
        ) ideas)
  in
  El.div ~at:[At.class' "network-feed-item h-entry px-0.5 py-1 md:px-2 md:py-1";
              At.v "data-month-id" (Printf.sprintf "%04d-%02d" _y _m);
              At.v "data-day" (string_of_int day)] [
    avatar_el;
    El.span ~at:[At.class' "network-feed-headline"] [
      title_el; El.txt " "; badge_el; El.txt " "; name_el;
      summary_el];
    mention_els;
    forward_els;
    idea_els]

(** Render a single month section (feed items only, bushel entries skipped). *)
let render_month ~entries ~forward_index ~idea_index section =
  let people_els = List.map (render_avatar ~entries) section.collaborators in
  let item_els = List.filter_map (fun item ->
    match item with
    | Bushel _ -> None
    | Feed_item (fi, d) -> Some (render_feed_item ~entries ~forward_index ~idea_index fi d)
  ) section.items in
  El.div ~at:[At.class' "network-month"] [
    El.div ~at:[At.class' "network-month-header"] [
      El.h2 ~at:[At.class' "network-month-title"]
        [El.txt (Printf.sprintf "%s %d" (Common.month_name_full section.month) section.year)];
      El.div ~at:[At.class' "network-month-people"] people_els];
    El.div ~at:[At.class' "network-month-body"] item_els]

(** {1 Month Section Computation} *)

let compute_month_sections ~ctx =
  let all_entries = Arod.Ctx.all_entries ctx in
  let all_feed_items = Arod.Ctx.feed_items ctx in

  (* Group bushel entries by (year, month) *)
  let bushel_by_month : (int * int, Entry.entry list) Hashtbl.t = Hashtbl.create 64 in
  List.iter (fun ent ->
    let (y, m, _d) = Entry.date ent in
    let key = (y, m) in
    let cur = try Hashtbl.find bushel_by_month key with Not_found -> [] in
    Hashtbl.replace bushel_by_month key (ent :: cur)
  ) all_entries;

  (* Group feed items by (year, month) *)
  let feed_by_month : (int * int, Arod.Ctx.feed_item list) Hashtbl.t = Hashtbl.create 64 in
  List.iter (fun (item : Arod.Ctx.feed_item) ->
    match item.entry.FeedEntry.date with
    | Some d ->
      let (y, m, _d), _ = Ptime.to_date_time d in
      let key = (y, m) in
      let cur = try Hashtbl.find feed_by_month key with Not_found -> [] in
      Hashtbl.replace feed_by_month key (item :: cur)
    | None -> ()
  ) all_feed_items;

  (* Only include months that have feed items *)
  let all_months = Hashtbl.create 64 in
  Hashtbl.iter (fun k _ -> Hashtbl.replace all_months k true) feed_by_month;
  let months =
    Hashtbl.fold (fun k _ acc -> k :: acc) all_months []
    |> List.sort (fun (y1, m1) (y2, m2) ->
      let c = compare y2 y1 in if c <> 0 then c else compare m2 m1)
  in

  (* Build month sections *)
  List.map (fun (y, m) ->
    let bushel_ents = try List.rev (Hashtbl.find bushel_by_month (y, m)) with Not_found -> [] in
    let feed_items = try List.rev (Hashtbl.find feed_by_month (y, m)) with Not_found -> [] in
    let collaborators = month_collaborators ~ctx bushel_ents feed_items in
    let timeline =
      let b = List.map (fun ent -> Bushel (ent, Entry.date ent)) bushel_ents in
      let f = List.map (fun (item : Arod.Ctx.feed_item) ->
        let d = match item.entry.FeedEntry.date with
          | Some pt -> let (y, m, d), _ = Ptime.to_date_time pt in (y, m, d)
          | None -> (y, m, 1)
        in
        Feed_item (item, d)
      ) feed_items in
      List.sort (fun a b ->
        compare (timeline_date b) (timeline_date a)
      ) (b @ f)
    in
    { year = y; month = m; collaborators; items = timeline }
  ) months

(** Build a reverse index from contact handle → list of (idea_slug, idea_title)
    for all ideas where the contact is a student. *)
let build_idea_index ~ctx =
  let tbl : (string, (string * string) list) Hashtbl.t = Hashtbl.create 64 in
  List.iter (fun idea ->
    List.iter (fun handle ->
      let cur = try Hashtbl.find tbl handle with Not_found -> [] in
      let pair = (Idea.slug idea, Idea.title idea) in
      if not (List.mem pair cur) then
        Hashtbl.replace tbl handle (pair :: cur)
    ) (Idea.student_handles idea)
  ) (Arod.Ctx.ideas ctx);
  tbl

(** Render a slice of month sections as an HTML string for the pagination API. *)
let render_months_html ~ctx sections =
  let entries = Arod.Ctx.entries ctx in
  let forward_index = build_forward_index () in
  let idea_index = build_idea_index ~ctx in
  let els = List.map (render_month ~entries ~forward_index ~idea_index) sections in
  El.to_string ~doctype:false (El.div els)

(** Return all computed month sections for use by the pagination API. *)
let all_months ~ctx = compute_month_sections ~ctx

(** {1 Network Page} *)

let page_size = 6

let network_page ~ctx =
  let entries = Arod.Ctx.entries ctx in
  let all_feed_items = Arod.Ctx.feed_items ctx in
  let all_contacts = Arod.Ctx.contacts ctx in

  let sections = compute_month_sections ~ctx in

  (* Stats *)
  let total_feed = List.length all_feed_items in
  let contacts_with_feeds = Common.contacts_with_feeds all_contacts in
  let total_contacts = List.length contacts_with_feeds in
  let total_months = List.length sections in

  (* Build calendar data: { "YYYY-MM": [day1, day2, ...], ... } *)
  let month_days : (string, int list) Hashtbl.t = Hashtbl.create 64 in
  List.iter (fun section ->
    let key = Printf.sprintf "%04d-%02d" section.year section.month in
    let days = List.filter_map (fun item ->
      match item with
      | Feed_item (_, (_y, _m, d)) -> Some d
      | Bushel _ -> None
    ) section.items in
    let days = List.sort_uniq compare days in
    Hashtbl.replace month_days key days
  ) sections;
  let calendar_months =
    Hashtbl.fold (fun k _ acc -> k :: acc) month_days []
    |> List.sort (fun a b -> compare b a)
  in
  let calendar_json =
    let entries_json = List.map (fun key ->
      let days = Hashtbl.find month_days key in
      let day_strs = List.map string_of_int days in
      Printf.sprintf {|"%s":[%s]|} key (String.concat "," day_strs)
    ) calendar_months in
    "{" ^ String.concat "," entries_json ^ "}"
  in
  let first_month = match calendar_months with
    | m :: _ -> m | [] -> ""
  in

  (* Render only first page of month sections *)
  let visible_sections =
    if List.length sections > page_size then Common.take page_size sections
    else sections
  in
  let forward_index = build_forward_index () in
  let idea_index = build_idea_index ~ctx in
  let month_els = List.map (render_month ~entries ~forward_index ~idea_index) visible_sections in

  let intro =
    El.p ~at:[At.class' "text-sm text-gray-600 dark:text-gray-400 mb-6"] [
      El.txt "I track a number of online blogs and connect relevant ones to things I am working on. You can grab my blogroll ";
      El.a ~at:[At.href "/network/blogroll.opml";
                At.class' "text-accent hover:underline"] [
        El.txt "OPML here"];
      El.txt ", or just browse it below. If you have your own blog that I've missed, do ";
      El.a ~at:[At.href "mailto:anil@recoil.org";
                At.class' "text-accent hover:underline"] [
        El.txt "let me know"];
      El.txt "!"]
  in

  let article =
    El.div ~at:[
      At.v "data-pagination" "true";
      At.v "data-collection-type" "network";
      At.v "data-total-count" (string_of_int total_months);
      At.v "data-current-count" (string_of_int (List.length visible_sections));
      At.v "data-types" ""] [
      intro;
      El.div ~at:[At.class' "network-timeline h-feed"] month_els]
  in

  (* Sidebar — calendar *)
  let calendar_box =
    Common.meta_box ~id:"network-calendar"
      ~body_cls:"sidebar-meta-body notes-calendar"
      ~data_attrs:["data-calendar-months", calendar_json;
                   "data-current-month", first_month]
      ~header:[El.txt (Printf.sprintf " %d posts \xC2\xB7 %d contacts"
                 total_feed total_contacts)]
      [El.div ~at:[At.class' "cal-header"] [];
       El.div ~at:[At.class' "heatmap-strip"] [];
       El.div ~at:[At.class' "cal-divider"] [];
       El.div ~at:[At.class' "cal-grid"] []]
  in

  (* Blogroll — split by contact kind *)
  let blogroll_contacts = Common.contacts_with_feeds all_contacts in
  let render_blogroll_row (contact, feeds) =
    let name = Contact.name contact in
    let thumb = Entry.contact_thumbnail entries contact in
    let img_el = match thumb with
      | Some src ->
        El.img ~at:[At.src src; At.v "alt" name;
                    At.class' "network-blogroll-avatar"] ()
      | None ->
        El.span ~at:[At.class' "network-blogroll-initials"]
          [El.txt (Common.contact_initials name)]
    in
    let name_el = match Contact.best_url contact with
      | Some u -> El.a ~at:[At.href u; At.class' "sidebar-meta-link"] [El.txt name]
      | None -> El.txt name
    in
    let feed_badges = List.map (fun feed ->
      let ft = Feed.feed_type feed in
      let icon = match ft with
        | Feed.Atom | Feed.Rss | Feed.Manual -> I.brand ~size:8 I.rss_brand
        | Feed.Json -> I.brand ~size:8 I.jsonfeed_brand
      in
      El.a ~at:[At.href (Feed.url feed); At.class' "feed-type-badge shrink-0 inline-flex items-center text-secondary opacity-50";
                At.v "title" (Feed.url feed)]
        [El.unsafe_raw icon]
    ) feeds in
    El.div ~at:[At.class' "sidebar-meta-line feed-blogroll-row"] [
      El.span ~at:[At.class' "sidebar-meta-icon"] [img_el];
      El.span ~at:[At.class' "sidebar-meta-val text-dim"] [name_el];
      El.span ~at:[At.class' "feed-blogroll-badges"] feed_badges]
  in
  let people, orgs = List.partition (fun (contact, _) ->
    match Contact.kind contact with
    | Contact.Person -> true
    | Contact.Organization -> false
  ) blogroll_contacts in
  (* Sort people by most recent feed entry date *)
  let all_fi = Arod.Ctx.feed_items ctx in
  let latest_date_for handle =
    let items = List.filter (fun (fi : Arod.Ctx.feed_item) ->
      Contact.handle fi.contact = handle
    ) all_fi in
    List.fold_left (fun best (fi : Arod.Ctx.feed_item) ->
      match fi.entry.Sortal_feed.Entry.date with
      | Some d -> (match best with Some b when Ptime.compare b d >= 0 -> best | _ -> Some d)
      | None -> best
    ) None items
  in
  let people_sorted = List.sort (fun (a, _) (b, _) ->
    let da = latest_date_for (Contact.handle a) in
    let db = latest_date_for (Contact.handle b) in
    match da, db with
    | Some a, Some b -> Ptime.compare b a
    | Some _, None -> -1
    | None, Some _ -> 1
    | None, None -> String.compare (Contact.name a) (Contact.name b)
  ) people in
  let max_people = 5 in
  let total_people = List.length people_sorted in
  let people_blogroll = match people_sorted with
    | [] -> El.void
    | _ ->
      let shown = List.filteri (fun i _ -> i < max_people) people_sorted in
      let expand_btn =
        if total_people > max_people then
          El.button ~at:[At.class' "sidebar-meta-expand";
                         At.v "data-modal-target" "people-modal-overlay"]
            [El.txt (Printf.sprintf "+ %d more" (total_people - max_people))]
        else El.void
      in
      Common.meta_box
        ~header:[El.txt " people ";
                 El.a ~at:[At.href "/network/blogroll.opml";
                           At.class' "text-xs opacity-60 hover:opacity-100";
                           At.v "title" "Download OPML"] [El.txt "[opml]"]]
        (List.map render_blogroll_row shown @ [expand_btn])
  in
  let people_modal =
    if total_people > max_people then
      let all_rows = List.map render_blogroll_row people_sorted in
      El.div ~at:[At.id "people-modal-overlay";
                  At.class' "links-modal-overlay"] [
        El.div ~at:[At.class' "links-modal"] [
          El.div ~at:[At.class' "links-modal-header"] [
            El.span [El.txt (Printf.sprintf "People (%d)" total_people)];
            El.button ~at:[At.class' "links-modal-close-btn"]
              [El.txt "\xC3\x97"]];
          El.div ~at:[At.class' "links-modal-body"] all_rows]]
    else El.void
  in
  let org_blogroll = match orgs with
    | [] -> El.void
    | _ ->
      Common.meta_box
        ~header:[El.txt " organisations "]
        (List.map render_blogroll_row orgs)
  in

  let sidebar =
    El.aside ~at:[At.class' "hidden lg:block lg:w-72 shrink-0"]
      [El.div ~at:[At.class' "sticky top-20"]
        [calendar_box; people_blogroll; org_blogroll];
       people_modal]
  in
  (article, sidebar)
