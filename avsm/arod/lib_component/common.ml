(*---------------------------------------------------------------------------
  Copyright (c) 2025 Anil Madhavapeddy <anil@recoil.org>. All rights reserved.
  SPDX-License-Identifier: ISC
 ---------------------------------------------------------------------------*)

(** Common utilities shared across Arod components.

    Centralises date formatting, list helpers, feed rendering,
    and other functions that were previously duplicated across
    multiple component files. *)

open Htmlit

module I = Arod.Icons
module Contact = Sortal_schema.Contact
module Feed = Sortal_schema.Feed
module FeedEntry = Sortal_feed.Entry

(** {1 Date Formatting} *)

let month_name = function
  | 1 -> "Jan" | 2 -> "Feb" | 3 -> "Mar" | 4 -> "Apr"
  | 5 -> "May" | 6 -> "Jun" | 7 -> "Jul" | 8 -> "Aug"
  | 9 -> "Sep" | 10 -> "Oct" | 11 -> "Nov" | 12 -> "Dec"
  | _ -> ""

let month_name_full = function
  | 1 -> "January" | 2 -> "February" | 3 -> "March" | 4 -> "April"
  | 5 -> "May" | 6 -> "June" | 7 -> "July" | 8 -> "August"
  | 9 -> "September" | 10 -> "October" | 11 -> "November" | 12 -> "December"
  | _ -> ""

let ptime_date_short (y, m, _d) =
  Printf.sprintf "%s %4d" (month_name m) y

let ptime_date_full (y, m, d) =
  Printf.sprintf "%d %s %4d" d (month_name_full m) y

(** {1 List Utilities} *)

let take n l =
  let[@tail_mod_cons] rec aux n l =
    match n, l with
    | 0, _ | _, [] -> []
    | n, x :: rest -> x :: aux (n - 1) rest
  in
  if n < 0 then invalid_arg "take"; aux n l

let map_and fn l =
  let ll = List.length l in
  List.mapi (fun i v ->
    match i with
    | 0 -> fn v
    | _ when i + 1 = ll -> " and " ^ (fn v)
    | _ -> ", " ^ (fn v)
  ) l |> String.concat ""

(** {1 String Utilities} *)

let strip_www h =
  if String.length h > 4 && String.sub h 0 4 = "www."
  then String.sub h 4 (String.length h - 4) else h

let contact_initials name =
  match String.split_on_char ' ' name with
  | f :: l :: _ when String.length f > 0 && String.length l > 0 ->
    String.make 1 (Char.uppercase_ascii f.[0])
    ^ String.make 1 (Char.uppercase_ascii l.[0])
  | f :: _ when String.length f > 0 ->
    String.make 1 (Char.uppercase_ascii f.[0])
  | _ -> "?"

(** {1 Paper Helpers} *)

let venue_of_paper paper =
  let bibty = String.lowercase_ascii (Bushel.Paper.bibtype paper) in
  match bibty with
  | "inproceedings" | "abstract" -> Bushel.Paper.booktitle paper
  | "article" | "journal" -> Bushel.Paper.journal paper
  | "book" -> Bushel.Paper.publisher paper
  | "techreport" -> Bushel.Paper.institution paper
  | _ -> Bushel.Paper.publisher paper

(** {1 Idea Helpers} *)

let idea_level_to_string = function
  | Bushel.Idea.Any -> "Any" | PartII -> "Part II" | MPhil -> "MPhil"
  | PhD -> "PhD" | Postdoc -> "Postdoc"

(** {1 Feed Helpers} *)

let feed_entry_title_str fe =
  Option.value ~default:"(untitled)" fe.FeedEntry.title

let feed_entry_title_el ?(cls="project-activity-title") fe =
  let title_str = feed_entry_title_str fe in
  match fe.FeedEntry.url with
  | Some u ->
    El.a ~at:[At.href (Uri.to_string u);
              At.class' (cls ^ " no-underline p-name u-url");
              At.v "rel" "noopener"]
      [El.txt title_str]
  | None ->
    El.span ~at:[At.class' (cls ^ " p-name")]
      [El.txt title_str]

let feed_entry_raw_text fe =
  match fe.FeedEntry.summary with
  | Some s when String.length s > 0 -> Some s
  | _ ->
    match fe.FeedEntry.content with
    | Some c when String.length c > 0 -> Some c
    | _ -> None

let feed_entry_summary ~max_len fe =
  Option.bind (feed_entry_raw_text fe) (Arod.Text.plain_summary ~max_len)

let feed_type_badge ft =
  let icon = match (ft : Feed.feed_type) with
    | Atom | Rss | Manual -> I.brand ~size:10 I.rss_brand
    | Json -> I.brand ~size:10 I.jsonfeed_brand
  in
  El.span ~at:[At.class' "feed-type-badge shrink-0 inline-flex items-center text-secondary opacity-50"]
    [El.unsafe_raw icon]

let contacts_with_feeds contacts =
  let with_feeds = List.filter_map (fun contact ->
    match Contact.feeds contact with
    | Some feeds when feeds <> [] -> Some (contact, feeds)
    | _ -> None
  ) contacts in
  List.sort (fun (a, _) (b, _) ->
    String.compare (Contact.name a) (Contact.name b)
  ) with_feeds

(** {1 Component Combinators} *)

let hidden_author_hcard ~ctx =
  let cfg = Arod.Ctx.config ctx in
  let author_name = Arod.Ctx.author_name ctx in
  let photo_el = match Arod.Ctx.author ctx with
    | Some author ->
      (match Bushel.Entry.contact_thumbnail (Arod.Ctx.entries ctx) author with
       | Some src -> [El.img ~at:[At.class' "u-photo"; At.src src;
                                   At.v "alt" author_name] ()]
       | None -> [])
    | None -> []
  in
  El.span ~at:[At.class' "p-author h-card"; At.v "style" "display:none"]
    ([El.a ~at:[At.class' "p-name u-url"; At.href cfg.site.base_url]
        [El.txt author_name]] @ photo_el)

let detail_tags ?date ?doi tags =
  let tag_els = List.map (fun raw ->
    El.a ~at:[At.class' "paper-detail-tag p-category"; At.v "data-tag" raw;
              At.href ("#tag=" ^ raw)]
      [El.txt ("#" ^ raw)]
  ) tags in
  let meta_items =
    let doi_el = match doi with
      | Some doi_str ->
        [El.a ~at:[At.href ("https://doi.org/" ^ doi_str); At.v "title" doi_str]
          [El.txt "[cite]"]]
      | None -> []
    in
    let date_el = match date with
      | Some (y, m, d) ->
        let datetime = Printf.sprintf "%04d-%02d-%02d" y m d in
        let label = Printf.sprintf "%d %s %d" d (month_name m) y in
        [El.time ~at:[At.v "datetime" datetime]
          [El.txt label]]
      | None -> []
    in
    let items = doi_el @ date_el in
    let sep = El.span ~at:[At.class' "detail-meta-sep"] [El.txt "\xC2\xB7"] in
    match items with
    | [] -> []
    | first :: rest ->
      first :: List.concat_map (fun el -> [sep; el]) rest
  in
  let meta_el = match meta_items with
    | [] -> []
    | items -> [El.span ~at:[At.class' "detail-meta"] items]
  in
  match tag_els, meta_el with
  | [], [] -> El.void
  | _ ->
    El.div ~at:[At.class' "paper-detail-tags"] (tag_els @ meta_el)

let page_title ?(cls="page-title text-xl font-semibold mb-3 p-name") title =
  El.h1 ~at:[At.class' cls] [El.txt title]

let meta_box ?id ?(cls="sidebar-meta-box mb-3") ?(body_cls="sidebar-meta-body")
    ?(data_attrs=[]) ~header body_els =
  let id_at = match id with Some i -> [At.id i] | None -> [] in
  let data_at = List.map (fun (k, v) -> At.v k v) data_attrs in
  El.div ~at:(id_at @ [At.class' cls] @ data_at) [
    El.div ~at:[At.class' "sidebar-meta-header"]
      (El.span ~at:[At.class' "sidebar-meta-prompt"] [El.txt ">_"] :: header);
    El.div ~at:[At.class' body_cls] body_els]

let hidden_dt_published (y, m, d) =
  let iso = Printf.sprintf "%04d-%02d-%02d" y m d in
  El.time ~at:[At.class' "dt-published"; At.v "datetime" iso;
               At.v "style" "display:none"] [El.txt iso]

(** {1 Shared Card Helpers} *)

let card_header ?(title_cls="") ~prompt ~title ~href meta =
  let cls = match title_cls with
    | "" -> "proj-card-title no-underline"
    | s -> "proj-card-title no-underline " ^ s
  in
  El.div ~at:[At.class' "proj-card-header"] [
    El.span ~at:[At.class' "proj-card-prompt"] [El.txt prompt];
    El.a ~at:[At.href href; At.class' cls] [El.txt title];
    meta]

let card_entry_row ~icon ~href ~title =
  El.div ~at:[At.class' "project-entry-row"] [
    El.span ~at:[At.class' "project-entry-icon"]
      [El.unsafe_raw icon];
    El.a ~at:[At.href href;
              At.class' "project-entry-link"]
      [El.txt title]]

let card_tags tags =
  if tags = [] then El.void
  else
    El.div ~at:[At.class' "proj-card-tags"]
      (List.map (fun t ->
        El.a ~at:[At.class' "proj-card-tag"; At.v "data-tag" t;
                  At.href ("#tag=" ^ t)] [El.txt t]
      ) tags)

(** {1 Body Truncation} *)

let truncate_body_parts ent =
  let body = Bushel.Entry.body ent in
  let first, last = Bushel.Util.first_and_last_hunks body in
  let remaining_words = Bushel.Util.count_words last in
  let total_words = Bushel.Util.count_words first + remaining_words in
  let is_note = match ent with `Note _ -> true | _ -> false in
  let is_truncated = remaining_words > 1 in
  let word_count_info =
    if is_truncated || (is_note && total_words > 0) then
      Some (total_words, is_truncated)
    else None
  in
  let footnote_lines = Bushel.Util.find_footnote_lines last in
  let footnotes_text =
    if footnote_lines = [] then ""
    else "\n\n" ^ String.concat "\n" footnote_lines
  in
  (first ^ footnotes_text, word_count_info)

let truncated_body ~ctx ent =
  let markdown_content, word_count_info = truncate_body_parts ent in
  let body_html = El.unsafe_raw (fst (Arod.Md.to_html ~ctx markdown_content)) in
  let read_more_el = match word_count_info with
    | Some (total, true) ->
      let url = Bushel.Entry.site_url ent in
      El.a ~at:[At.href url; At.class' "project-read-more"]
        [El.unsafe_raw (I.outline ~size:14 I.arrow_right_sm_o);
         El.txt (Printf.sprintf " Read more (%d words)" total)]
    | _ -> El.void
  in
  (El.div [body_html; read_more_el], word_count_info)
