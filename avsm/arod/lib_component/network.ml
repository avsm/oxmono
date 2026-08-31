(*---------------------------------------------------------------------------
  Copyright (c) 2025 Anil Madhavapeddy <anil@recoil.org>. All rights reserved.
  SPDX-License-Identifier: ISC
 ---------------------------------------------------------------------------*)

(** Network page components. *)

open Htmlit

module Entry = Bushel.Entry
module Paper = Bushel.Paper
module Contact = Sortal_schema.Contact
module Feed = Sortal_schema.Feed
module FeedEntry = Sortal_feed.Entry
module Idea = Bushel.Idea
module I = Arod.Icons

type timeline_item = Feed_item of Arod.Ctx.feed_item * (int * int * int)

let timeline_date (Feed_item (_, d)) = d

type month_section = {
  year : int;
  month : int;
  collaborators : Contact.t list;
  items : timeline_item list;
}

(** [month_collaborators ~ctx entries feed_items] is the month's collaborator
    list ordered by appearance count. *)
let month_collaborators ~ctx bushel_entries feed_items =
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
        (match Arod.Ctx.lookup_by_handle ctx handle with
         | Some c -> bump c
         | None -> ())
      | _ -> ()
    ) tags
  ) bushel_entries;
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
  List.iter (fun (item : Arod.Ctx.feed_item) ->
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

(** [render_avatar ~entries contact] is the avatar for [contact]. *)
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

(** [render_feed_item ~ctx ~entries ~idea_index item date] is a timeline row. *)
let render_feed_item ~ctx ~entries ~idea_index (item : Arod.Ctx.feed_item) ((_y, _m, day) : int * int * int) =
  let fe = item.entry in
  let contact = item.contact in
  let name = Contact.name contact in
  let thumb = Entry.contact_thumbnail entries contact in
  let avatar_el = match thumb with
    | Some src ->
      El.img ~at:[At.src src; At.v "alt" name;
                  At.class' "network-feed-avatar"] ()
    | None ->
      El.span ~at:[At.class' "network-avatar-initials network-feed-avatar"]
        [El.txt (Common.contact_initials name)]
  in
  let title_el = Common.feed_entry_title_el fe in
  let badge_el = El.span ~at:[At.class' "hidden md:inline"]
    [Common.feed_type_badge fe.FeedEntry.source_type] in
  let name_el = match Contact.best_url contact with
    | Some u ->
      El.a ~at:[At.href u; At.class' "network-feed-name no-underline"]
        [El.txt name]
    | None ->
      El.span ~at:[At.class' "network-feed-name"]
        [El.txt name]
  in
  let summary_el =
    match Common.feed_entry_summary ~max_len:150 fe with
    | Some text ->
      El.span ~at:[At.class' "network-feed-summary"]
        [El.txt (" \xe2\x80\x94 " ^ text)]
    | None -> El.void
  in
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
  let forward_els =
    match fe.FeedEntry.url with
    | Some u ->
      let slugs = Arod.Ctx.forward_slugs ctx (Uriz.to_string u) in
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

(** [render_month ~ctx ~entries ~idea_index section] is a timeline month. *)
let render_month ~ctx ~entries ~idea_index section =
  let people_els = List.map (render_avatar ~entries) section.collaborators in
  let item_els = List.map (fun (Feed_item (fi, d)) ->
    render_feed_item ~ctx ~entries ~idea_index fi d
  ) section.items in
  El.div ~at:[At.class' "network-month"] [
    El.div ~at:[At.class' "network-month-header"] [
      El.h2 ~at:[At.class' "network-month-title"]
        [El.txt (Printf.sprintf "%s %d" (Common.month_name_full section.month) section.year)];
      El.div ~at:[At.class' "network-month-people"] people_els];
    El.div ~at:[At.class' "network-month-body"] item_els]

let compute_month_sections ~ctx =
  let all_entries = Arod.Ctx.all_entries ctx in
  let all_feed_items = Arod.Ctx.feed_items ctx in

  let bushel_by_month : (int * int, Entry.entry list) Hashtbl.t = Hashtbl.create 64 in
  List.iter (fun ent ->
    let (y, m, _d) = Entry.date ent in
    let key = (y, m) in
    let cur = Option.value ~default:[] (Hashtbl.find_opt bushel_by_month key) in
    Hashtbl.replace bushel_by_month key (ent :: cur)
  ) all_entries;

  let feed_by_month : (int * int, Arod.Ctx.feed_item list) Hashtbl.t = Hashtbl.create 64 in
  List.iter (fun (item : Arod.Ctx.feed_item) ->
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
    let collaborators = month_collaborators ~ctx bushel_ents feed_items in
    let timeline =
      List.map (fun (item : Arod.Ctx.feed_item) ->
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

(** [build_idea_index ~ctx] maps students to their ideas. *)
let build_idea_index ~ctx =
  let tbl : (string, (string * string) list) Hashtbl.t = Hashtbl.create 64 in
  List.iter (fun idea ->
    List.iter (fun handle ->
      let cur = Option.value ~default:[] (Hashtbl.find_opt tbl handle) in
      let pair = (Idea.slug idea, Idea.title idea) in
      if not (List.mem pair cur) then
        Hashtbl.replace tbl handle (pair :: cur)
    ) (Idea.student_handles idea)
  ) (Arod.Ctx.ideas ctx);
  tbl

(** [render_months_html ~ctx sections] is the pagination fragment for [sections]. *)
let render_months_html ~ctx sections =
  let entries = Arod.Ctx.entries ctx in
  let idea_index = build_idea_index ~ctx in
  let els = List.map (render_month ~ctx ~entries ~idea_index) sections in
  El.to_string ~doctype:false (El.div els)

(** [all_months ~ctx] is the network timeline grouped by month. *)
let all_months ~ctx = compute_month_sections ~ctx

let page_size = 6

(** [network_page ~ctx] is the network timeline and its sidebar. *)
let network_page ~ctx =
  let entries = Arod.Ctx.entries ctx in
  let all_feed_items = Arod.Ctx.feed_items ctx in
  let all_contacts = Arod.Ctx.contacts ctx in

  let sections = compute_month_sections ~ctx in

  let total_feed = List.length all_feed_items in
  let contacts_with_feeds = Common.contacts_with_feeds all_contacts in
  let total_contacts = List.length contacts_with_feeds in
  let total_months = List.length sections in

  let month_days : (string, int list) Hashtbl.t = Hashtbl.create 64 in
  List.iter (fun section ->
    let key = Printf.sprintf "%04d-%02d" section.year section.month in
    let days = List.map (fun item ->
      match item with Feed_item (_, (_, _, d)) -> d
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

  let visible_sections =
    if List.length sections > page_size then Common.take page_size sections
    else sections
  in
  let idea_index = build_idea_index ~ctx in
  let month_els = List.map (render_month ~ctx ~entries ~idea_index) visible_sections in

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

  let calendar_box =
    Common.meta_box
      ~body_cls:"sidebar-meta-body notes-calendar"
      ~data_attrs:["data-calendar-months", calendar_json;
                   "data-current-month", first_month;
                   "data-cal-track", ".network-feed-item";
                   "data-cal-noun", "day";
                   "data-cal-empty", "no posts"]
      ~header:[El.txt (Printf.sprintf " %d posts \xC2\xB7 %d contacts"
                 total_feed total_contacts)]
      [El.div ~at:[At.class' "cal-header"] [];
       El.div ~at:[At.class' "heatmap-strip"] [];
       El.div ~at:[At.class' "cal-divider"] [];
       El.div ~at:[At.class' "cal-grid"] []]
  in

  let blogroll_contacts = contacts_with_feeds in
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
  let latest_dates = Hashtbl.create 64 in
  List.iter (fun (fi : Arod.Ctx.feed_item) ->
    match fi.entry.Sortal_feed.Entry.date with
    | None -> ()
    | Some date ->
      let handle = Contact.handle fi.contact in
      match Hashtbl.find_opt latest_dates handle with
      | Some old when Ptime.compare old date >= 0 -> ()
      | _ -> Hashtbl.replace latest_dates handle date
  ) all_feed_items;
  let latest_date_for handle = Hashtbl.find_opt latest_dates handle in
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
