(*---------------------------------------------------------------------------
  Copyright (c) 2025 Anil Madhavapeddy <anil@recoil.org>. All rights reserved.
  SPDX-License-Identifier: ISC
 ---------------------------------------------------------------------------*)

(** Feeds page component.

    Shows aggregated feed entries from contacts ordered by date,
    forming a planet-style feed reader. Also includes a blogroll
    sidebar of contacts with registered feeds. *)

open Htmlit

module Contact = Sortal_schema.Contact
module Feed = Sortal_schema.Feed
module FeedEntry = Sortal_feed.Entry
module I = Arod.Icons

(** {1 Feeds List Page} *)

let feeds_list ~ctx =
  let all_items = Arod.Ctx.feed_items ctx in
  let entries = Arod.Ctx.entries ctx in
  let all_contacts = Arod.Ctx.contacts ctx in

  (* Contacts with feeds for sidebar blogroll *)
  let contacts_with_feeds = Common.contacts_with_feeds all_contacts in

  (* Stats *)
  let total_items = List.length all_items in
  let total_contacts = List.length contacts_with_feeds in
  let total_feeds = List.fold_left (fun acc (_, feeds) ->
    acc + List.length feeds
  ) 0 contacts_with_feeds in

  (* Render feed entry rows — show all items, newest first *)
  let item_rows = List.map (fun (item : Arod.Ctx.feed_item) ->
    let fe = item.entry in
    let contact = item.contact in
    let name = Contact.name contact in

    (* Title *)
    let title_el = Common.feed_entry_title_el ~cls:"note-compact-title flex-1 min-w-0 font-medium !text-text !no-underline" fe in

    (* Date *)
    let date_el = match fe.FeedEntry.date with
      | Some d ->
        let (y, m, day), _ = Ptime.to_date_time d in
        El.span ~at:[At.class' "note-compact-meta shrink-0 text-[0.82rem] text-secondary whitespace-nowrap tabular-nums"]
          [El.txt (Printf.sprintf "%d %s %d" day (Common.month_name m) y)]
      | None -> El.void
    in

    (* Feed type badge *)
    let badge_el = Common.feed_type_badge fe.FeedEntry.source_type in

    (* Summary *)
    let summary_el =
      match Common.feed_entry_summary ~max_len:200 fe with
      | Some text ->
        El.div ~at:[At.class' "note-compact-synopsis text-[0.85rem] text-secondary leading-[1.4] mt-[0.1rem]"]
          [El.txt text]
      | None -> El.void
    in

    (* Contact name *)
    let contact_el = match Contact.best_url contact with
      | Some u ->
        El.a ~at:[At.href u; At.class' "link-backlink-chip no-underline"]
          [El.txt name]
      | None ->
        El.span ~at:[At.class' "link-backlink-chip"]
          [El.txt name]
    in

    (* Mentions of local bushel entries *)
    let mention_els = match item.mentions with
      | [] -> El.void
      | mentions ->
        El.div ~at:[At.class' "feed-item-mentions pl-0 md:pl-3"]
          (List.map (fun entry ->
            let type_icon = Sidebar.entry_type_icon ~size:10 entry in
            El.a ~at:[At.href (Bushel.Entry.site_url entry);
                      At.class' "link-backlink-chip no-underline"]
              [El.unsafe_raw type_icon;
               El.txt (Sidebar.truncate_str 30 (Bushel.Entry.title entry))]
          ) mentions)
    in

    El.div ~at:[At.class' "feed-item h-entry px-0 py-1 md:px-2"] [
      El.div ~at:[At.class' "note-compact-row"] [
        title_el;
        badge_el;
        date_el];
      summary_el;
      El.div ~at:[At.class' "feed-item-source pl-0 md:pl-3"] [contact_el];
      mention_els]
  ) all_items in

  let article =
    El.div ~at:[At.class' "h-feed"] [
      El.div ~at:[At.class' "feed-list"] item_rows]
  in

  (* Sidebar *)
  let stats_box =
    El.div ~at:[At.class' "sidebar-meta-box mb-3"] [
      El.div ~at:[At.class' "sidebar-meta-header"] [
        El.span ~at:[At.class' "sidebar-meta-prompt"] [El.txt ">_"];
        El.txt " feeds"];
      El.div ~at:[At.class' "sidebar-meta-body"] [
        Sidebar.meta_line
          ~icon:(I.brand ~cl:"opacity-50" ~size:12 I.rss_brand)
          (El.txt (Printf.sprintf "%d entries" total_items));
        Sidebar.meta_line
          ~icon:(I.outline ~cl:"opacity-50" ~size:12 I.user_o)
          (El.txt (Printf.sprintf "%d contacts" total_contacts));
        Sidebar.meta_line
          ~icon:(I.brand ~cl:"opacity-50" ~size:12 I.rss_brand)
          (El.txt (Printf.sprintf "%d feeds" total_feeds))]]
  in

  (* Blogroll: contacts with their feeds *)
  let blogroll =
    El.div ~at:[At.class' "sidebar-meta-box mb-3"] [
      El.div ~at:[At.class' "sidebar-meta-header"] [
        El.span ~at:[At.class' "sidebar-meta-prompt"] [El.txt ">_"];
        El.txt " blogroll"];
      El.div ~at:[At.class' "sidebar-meta-body"]
        (List.map (fun (contact, feeds) ->
          let name = Contact.name contact in
          let thumb = Bushel.Entry.contact_thumbnail entries contact in
          let img_el = match thumb with
            | Some src ->
              El.img ~at:[At.src src; At.v "alt" name;
                          At.class' "feed-blogroll-avatar"] ()
            | None ->
              El.span ~at:[At.class' "feed-blogroll-avatar-initials"]
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
        ) contacts_with_feeds)]
  in

  let sidebar =
    El.aside ~at:[At.class' "hidden lg:block lg:w-72 shrink-0"]
      [El.div ~at:[At.class' "sticky top-20"] [stats_box; blogroll]]
  in
  (article, sidebar)
