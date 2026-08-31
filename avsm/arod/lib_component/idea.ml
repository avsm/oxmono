(*---------------------------------------------------------------------------
  Copyright (c) 2025 Anil Madhavapeddy <anil@recoil.org>. All rights reserved.
  SPDX-License-Identifier: ISC
 ---------------------------------------------------------------------------*)

(** Idea components. *)

open Htmlit

module Idea = Bushel.Idea
module Contact = Sortal_schema.Contact
module I = Arod.Icons

(** [heading ~ctx ent] is the linked heading of [ent]. *)
let heading ~ctx:_ ent =
  El.h2 ~at:[At.class' "text-xl font-semibold mb-2"] [
    El.a ~at:[At.href (Bushel.Entry.site_url ent)] [
      El.txt (Bushel.Entry.title ent)];
    El.span ~at:[At.class' "text-sm text-secondary"] [
      El.txt " / ";
      El.txt (Common.ptime_date_short (Bushel.Entry.date ent))]]

let status_class = function
  | Idea.Available -> "font-medium text-st-avail"
  | Discussion -> "font-medium text-st-discuss"
  | Ongoing -> "font-medium text-st-ongoing"
  | Completed -> "font-medium text-st-done"
  | Expired -> "font-medium text-st-expired"

(** [status_badge status] is the labelled badge for [status]. *)
let status_badge status =
  let label = Idea.status_to_string status in
  El.span ~at:[At.class' (status_class status)] [El.txt label]

let status_to_long_string s = function
  | Idea.Available ->
    Printf.sprintf "is <span class=\"font-medium text-st-avail\">available</span> for being worked on"
  | Discussion ->
    Printf.sprintf "is <span class=\"font-medium text-st-discuss\">under discussion</span> with a student but not yet confirmed"
  | Ongoing ->
    Printf.sprintf "is currently <span class=\"font-medium text-st-ongoing\">being worked on</span> by %s" s
  | Completed ->
    Printf.sprintf "has been <span class=\"font-medium text-st-done\">completed</span> by %s" s
  | Expired ->
    Printf.sprintf "has <span class=\"font-medium text-st-expired\">expired</span>"

let level_to_long_string = function
  | Idea.Any -> " as an internship project"
  | PartII -> " as a Cambridge Computer Science Part II project"
  | MPhil -> " as a Cambridge Computer Science Part III or MPhil project"
  | PhD -> " as a Cambridge Computer Science PhD topic"
  | Postdoc -> " as a postdoctoral project"

let sups_for i =
  let v = match Idea.status i with
    | Idea.Completed -> "was" | Ongoing -> "is" | _ -> "may be"
  in
  let sups = List.filter (fun x -> x <> "avsm") i.Idea.supervisor_handles in
  match sups with
  | [] -> ""
  | s -> " It " ^ v ^ " co-supervised with " ^ (Common.map_and (Printf.sprintf "[@%s]") s) ^ "."

let render_contacts ~ctx contacts =
  match contacts with
  | [] -> El.void
  | cs ->
    let contact_links = List.filter_map (fun handle ->
      match Arod.Ctx.lookup_by_handle ctx handle with
      | Some contact ->
        let name = Contact.name contact in
        (match Contact.best_url contact with
         | Some url -> Some (El.a ~at:[At.href url] [El.txt name])
         | None -> Some (El.txt name))
      | None -> Some (El.txt ("@" ^ handle))
    ) cs in
    let rec intersperse_and = function
      | [] -> [] | [x] -> [x] | [x; y] -> [x; El.txt " and "; y]
      | x :: xs -> x :: El.txt ", " :: intersperse_and xs
    in
    let children = intersperse_and contact_links in
    El.span children

(** [status_dot status] is the coloured marker for [status]. *)
let status_dot status =
  let cls = "idea-dot " ^ status_class status in
  El.span ~at:[At.class' cls]
    [El.unsafe_raw (I.filled ~size:8 I.circle_f)]

(** [brief ~ctx i] is a summary of [i] with its status and level. *)
let brief ~ctx i =
  let studs = Common.map_and (Printf.sprintf "[@%s]") (Idea.student_handles i) in
  let r = Printf.sprintf "This is an idea proposed in %d%s, and %s.%s"
    (Idea.year i) (level_to_long_string (Idea.level i))
    (status_to_long_string studs (Idea.status i)) (sups_for i)
  in
  let body_html, word_count_info = Common.truncated_body ~ctx (`Idea i) in
  (El.div [
    heading ~ctx (`Idea i);
    El.div ~at:[At.class' "mb-4"] [El.unsafe_raw (fst (Arod.Md.to_html ~ctx r)); body_html]
  ], word_count_info)

(** [full_page ~ctx i] is the article page for [i]. *)
let full_page ~ctx i =
  let level_str = Common.idea_level_to_string (Idea.level i) in
  let sups = List.filter (fun x -> x <> "avsm") i.Idea.supervisor_handles in
  let meta_row =
    El.p ~at:[At.class' "text-sm text-secondary mb-2 lg:hidden"]
      [status_badge (Idea.status i);
       El.txt (Printf.sprintf " \xC2\xB7 %s \xC2\xB7 %d" level_str (Idea.year i));
       (match sups with
        | [] -> El.void
        | _ -> El.span [El.txt " \xC2\xB7 "; render_contacts ~ctx sups])]
  in
  let title_el =
    Common.page_title ~cls:"page-title text-xl font-semibold tracking-tight mb-3 p-name"
      (Idea.title i)
  in
  let back =
    El.p ~at:[At.class' "idea-back"] [
      El.a ~at:[At.href ("/ideas#" ^ Idea.project i);
                At.class' "idea-back-link"] [
        El.unsafe_raw (I.outline ~size:12 I.arrow_left_o);
        El.txt "All research ideas"]]
  in
  let header_el =
    El.header ~at:[At.id "intro"; At.class' "mb-6"]
      [back; meta_row; title_el]
  in
  let body = Idea.body i in
  let body_html, sidenotes = Arod.Md.to_html ~ctx body in
  let headings = Arod.Md.extract_headings body in
  let article_el =
    El.article ~at:[At.class' "space-y-4 e-content"] [El.unsafe_raw body_html]
  in
  let activity_el =
    let items = List.concat_map (fun handle ->
      Arod.Ctx.feed_items_for_contact ctx handle
    ) i.Idea.student_handles in
    match items with
    | [] -> El.void
    | items ->
      let rows = List.map (fun (item : Arod.Ctx.feed_item) ->
        let fe = item.entry in
        let title_el = Common.feed_entry_title_el fe in
        let date_str = match fe.Sortal_feed.Entry.date with
          | Some d ->
            let (y, m, _d), _ = Ptime.to_date_time d in
            Common.ptime_date_short (y, m, 0)
          | None -> ""
        in
        let summary_el =
          match Common.feed_entry_summary ~max_len:150 fe with
          | Some text ->
            El.div ~at:[At.class' "project-activity-detail"]
              [El.txt text]
          | None -> El.void
        in
        let name = Contact.name item.contact in
        El.div ~at:[At.class' "project-activity-row"] [
          El.span ~at:[At.class' "project-activity-icon"]
            [El.unsafe_raw (I.brand ~size:12 I.rss_brand)];
          El.div ~at:[At.class' "project-activity-content"] [
            El.div ~at:[At.class' "project-activity-header"] [
              title_el;
              El.span ~at:[At.class' "project-activity-date"]
                [El.txt date_str]];
            El.div ~at:[At.class' "project-activity-detail"]
              [El.txt name];
            summary_el]]
      ) items in
      El.div ~at:[At.class' "related-stream not-prose mt-6"] [
        El.h3 ~at:[At.class' "text-sm font-semibold text-secondary uppercase tracking-wide mb-2"]
          [El.txt "Activity"];
        El.div ~at:[At.class' "project-activity-list"] rows]
  in
  let hidden_author = Common.hidden_author_hcard ~ctx in
  let published_dt = Common.hidden_dt_published (Bushel.Entry.date (`Idea i)) in
  (El.div ~at:[At.class' "h-entry"] [header_el; hidden_author; published_dt; article_el; activity_el], sidenotes, headings)

(** [compact ~ctx i] is a compact list card for [i]. *)
let compact ~ctx idea =
  let year = Idea.year idea in
  let status = Idea.status idea in
  let level_str = match Idea.level idea with
    | Idea.Any -> "Internship" | PartII -> "Part II" | MPhil -> "MPhil"
    | PhD -> "PhD" | Postdoc -> "Postdoc"
  in
  let meta_parts =
    [string_of_int year] @
    (if level_str <> "" then [level_str] else [])
  in
  let meta_text = String.concat " \xC2\xB7 " meta_parts in
  let url = "/ideas/" ^ idea.Idea.slug in
  let status_str = Idea.status_to_string status in
  let people = match status, idea.Idea.student_handles with
    | Ongoing, (_ :: _ as hs) -> [El.txt " with "; render_contacts ~ctx hs]
    | Completed, (_ :: _ as hs) -> [El.txt " by "; render_contacts ~ctx hs]
    | _ -> []
  in
  let sups = List.filter (fun x -> x <> "avsm") idea.Idea.supervisor_handles in
  let cosup = match sups with
    | [] -> []
    | _ -> [El.txt ", with "; render_contacts ~ctx sups]
  in
  let synopsis = (El.txt status_str :: people) @ cosup in
  El.div ~at:[At.class' "note-compact hover:bg-surface idea-item h-entry px-1 py-1 md:px-2 md:py-1";
              At.v "data-filter-item" (Idea.status_to_string status);
              At.v "data-year" (string_of_int year)] [
    El.div ~at:[At.class' "note-compact-row"] [
      status_dot status;
      El.a ~at:[At.href url; At.class' "note-compact-title flex-1 min-w-0 font-medium !text-text !no-underline p-name u-url"]
        [El.txt (Idea.title idea)];
      El.span ~at:[At.class' "note-compact-meta shrink-0 text-[0.82rem] text-secondary whitespace-nowrap tabular-nums"]
        [El.txt meta_text]];
    El.div ~at:[At.class' "note-compact-synopsis text-[0.85rem] text-secondary leading-[1.4] mt-[0.1rem] p-summary"]
      synopsis]

(** [is_open i] is [true] if a student can take on [i]. *)
let is_open i =
  match Idea.status i with
  | Idea.Available | Idea.Discussion -> true
  | Ongoing | Completed | Expired -> false

(** [is_live i] is [true] if [i] is available, discussed or ongoing. *)
let is_live i =
  match Idea.status i with
  | Idea.Available | Idea.Discussion | Ongoing -> true
  | Completed | Expired -> false

(** [level_key l] is the filter token for [l]. *)
let level_key = function
  | Idea.Any -> "Any" | PartII -> "PartII" | MPhil -> "MPhil"
  | PhD -> "PhD" | Postdoc -> "Postdoc"

(** [level_label l] is how a level reads on a card and on a filter row. *)
let level_label = function
  | Idea.Any -> "Internship" | PartII -> "Part II" | MPhil -> "MPhil"
  | PhD -> "PhD" | Postdoc -> "Postdoc"

(** [level_note l] is the explanatory text for [l]. *)
let level_note = function
  | Idea.Any -> "an internship, which suits any level of study"
  | PartII -> "Computer Science undergraduate final year"
  | MPhil -> "Part III or a one year MPhil"
  | PhD -> "a doctoral topic, over three or four years"
  | Postdoc -> "postdoctoral research"

(** [level_phrase l] opens the sentence under a card title. *)
let level_phrase = function
  | Idea.Any -> "An internship project"
  | PartII -> "A Part II project"
  | MPhil -> "An MPhil or Part III project"
  | PhD -> "A PhD topic"
  | Postdoc -> "A postdoctoral project"

(** [level_past_phrase l] is the past-work phrase for [l]. *)
let level_past_phrase = function
  | Idea.Any -> "An internship project"
  | PartII -> "A Part II project"
  | MPhil -> "An MPhil or Part III project"
  | PhD -> "A PhD"
  | Postdoc -> "A postdoctoral project"

(** [status_key s] is the CSS token for [s]. *)
let status_key = function
  | Idea.Available -> "avail" | Discussion -> "discuss" | Ongoing -> "ongoing"
  | Completed -> "done" | Expired -> "expired"

(** [status_label s] is the display label for [s]. *)
let status_label = function
  | Idea.Available -> "open"
  | Discussion -> "under discussion"
  | Ongoing -> "under way"
  | Completed -> "completed"
  | Expired -> "expired"

(** [all_statuses] is the display order of idea statuses. *)
let all_statuses =
  [Idea.Available; Idea.Discussion; Idea.Ongoing; Idea.Completed; Idea.Expired]

(** [statuses_present is] is the statuses present in [is], in display order. *)
let statuses_present is =
  List.filter (fun s -> List.exists (fun i -> Idea.status i = s) is)
    all_statuses

(** [tally ~slots is] is the count for each status in [slots]. *)
let tally ~slots is =
  List.map (fun s ->
    (s, List.length (List.filter (fun i -> Idea.status i = s) is))) slots

(** [status_cls i] is the card classes of [i] up to its status colour. *)
let status_cls i = "idea-st-" ^ status_key (Idea.status i)

(** [filter_at i] is the client-side filter attributes of [i]. *)
let filter_at i =
  [At.v "data-idea-item" ""; At.v "data-level" (level_key (Idea.level i));
   At.v "data-status" (status_key (Idea.status i))]

(** [summary_text ~ctx ~max_len i] is the plain-text opening of [i]. *)
let summary_text ~ctx ~max_len i =
  let first, _ = Bushel.Util.first_and_last_hunks (Idea.body i) in
  Arod.Text.plain_summary ~max_len (Arod.Md.to_plain_html ~ctx first)

(** [card ~ctx i] is the full list card for live idea [i]. *)
let card ~ctx i =
  let url = "/ideas/" ^ Idea.slug i in
  let sups = List.filter (fun x -> x <> "avsm") i.Idea.supervisor_handles in
  let meta =
    let year = Idea.year i in
    let opening =
      let lvl = level_past_phrase (Idea.level i) in
      match Idea.status i, Idea.student_handles i with
      | Idea.Ongoing, (_ :: _ as hs) ->
        [El.txt (lvl ^ ", under way with "); render_contacts ~ctx hs;
         El.txt (Printf.sprintf " since %d" year)]
      | Ongoing, [] ->
        [El.txt (Printf.sprintf "%s, under way since %d" lvl year)]
      | _ ->
        [El.txt (Printf.sprintf "%s, proposed in %d"
                   (level_phrase (Idea.level i)) year)]
    in
    let cosup =
      match sups with
      | [] -> [El.txt "."]
      | _ -> [El.txt ", co-supervised with "; render_contacts ~ctx sups;
              El.txt "."]
    in
    let status =
      match Idea.status i with
      | Idea.Discussion ->
        [El.txt " ";
         El.span ~at:[At.class' "idea-card-discuss"]
           [El.txt "Already under discussion with a student."]]
      | _ -> []
    in
    El.p ~at:[At.class' "idea-card-meta"] (opening @ cosup @ status)
  in
  let summary =
    match summary_text ~ctx ~max_len:240 i with
    | None -> El.void
    | Some text ->
      El.p ~at:[At.class' "idea-card-summary p-summary"] [El.txt text]
  in
  El.article
    ~at:(At.class' ("idea-card h-entry " ^ status_cls i) :: filter_at i) [
    El.div ~at:[At.class' "idea-card-body"] [
      El.a ~at:[At.href url; At.class' "idea-card-title p-name u-url"]
        [El.txt (Idea.title i)];
      meta; summary]]

(** [past_card ~ctx i] is the compact list row for past idea [i]. *)
let past_card ~ctx i =
  let status = Idea.status i in
  let year = Idea.year i in
  let lvl = level_past_phrase (Idea.level i) in
  let line =
    let say fmt = El.txt (Printf.sprintf fmt lvl year) in
    match status, Idea.student_handles i with
    | Idea.Completed, (_ :: _ as hs) ->
      [El.txt (lvl ^ ", completed by "); render_contacts ~ctx hs;
       El.txt (Printf.sprintf " in %d" year)]
    | Completed, [] -> [say "%s, completed in %d"]
    | Ongoing, (_ :: _ as hs) ->
      [El.txt (lvl ^ ", under way with "); render_contacts ~ctx hs;
       El.txt (Printf.sprintf " since %d" year)]
    | Ongoing, [] -> [say "%s, under way since %d"]
    | Expired, _ -> [say "%s, offered in %d and no longer open"]
    | (Available | Discussion), _ -> [say "%s, offered in %d"]
  in
  El.div
    ~at:(At.class' ("idea-past-card h-entry " ^ status_cls i) :: filter_at i) [
    El.span ~at:[At.class' "idea-past-line"] [
      El.a ~at:[At.href ("/ideas/" ^ Idea.slug i);
                At.class' "idea-past-title p-name u-url"]
        [El.txt (Idea.title i)]];
    El.span ~at:[At.class' "idea-past-meta"] line;
    El.span ~at:[At.class' "idea-past-open"; At.v "aria-hidden" "true"]
      [El.unsafe_raw (I.outline ~size:12 I.arrow_up_right_o)]]

(** [group_head ~ctx proj ~n_open ~n_going ~n_past] is the heading for
    [proj]'s idea group. *)
let group_head ~ctx proj ~n_open ~n_going ~n_past =
  let tail = El.span ~at:[At.class' "idea-group-tail"] [
    (if n_open = 0 then El.void
     else El.span ~at:[At.class' "idea-group-count idea-st-avail"]
            [El.txt (Printf.sprintf "%d open" n_open)]);
    (if n_going = 0 then El.void
     else El.span ~at:[At.class' "idea-group-count idea-st-ongoing"]
            [El.txt (Printf.sprintf "%d under way" n_going)]);
    (if n_past = 0 then El.void
     else El.span ~at:[At.class' "idea-group-count idea-group-count-past"]
            [El.txt (Printf.sprintf "%d previous" n_past)])]
  in
  let art =
    match Bushel.Entry.thumbnail (Arod.Ctx.entries ctx) (`Project proj) with
    | None -> El.void
    | Some src ->
      El.div ~at:[At.class' "idea-group-art"; At.v "aria-hidden" "true"]
        [El.img ~at:[At.src src; At.alt ""; At.v "loading" "lazy"] ()]
  in
  El.div ~at:[At.class' "idea-group-head"] [
    art;
    El.a ~at:[At.href ("#" ^ proj.Bushel.Project.slug);
              At.class' "idea-group-prompt";
              At.v "aria-label" ("Link to " ^ proj.Bushel.Project.title)]
      [El.txt ">_"];
    El.a ~at:[At.href ("/projects/" ^ proj.Bushel.Project.slug);
              At.class' "idea-group-title"]
      [El.txt proj.Bushel.Project.title];
    tail;
    (match String.trim (Bushel.Project.ideas proj) with
     | "" -> El.void
     | t -> El.p ~at:[At.class' "idea-group-note"] [El.txt t])]

(** [toc_row ~slots ~widest proj is] is the contents row for [proj]. *)
let toc_row ~slots ~widest proj is =
  let counts = tally ~slots is in
  let slug = proj.Bushel.Project.slug in
  let total = List.length is in
  let spoken =
    String.concat ", "
      (List.filter_map (fun (s, n) ->
         if n = 0 then None
         else Some (Printf.sprintf "%d %s" n (status_label s))) counts)
  in
  let bands =
    List.filter_map (fun (s, n) ->
      if n = 0 then None
      else
        let pct = 100.0 *. float_of_int n /. float_of_int widest in
        Some (
          El.span ~at:[At.class' ("idea-toc-seg idea-st-" ^ status_key s);
                       At.v "style" (Printf.sprintf "width:%.3f%%" pct);
                       At.v "title"
                         (Printf.sprintf "%d %s" n (status_label s))] [])
    ) counts
  in
  El.a ~at:[At.href ("#" ^ slug); At.class' "idea-toc-row";
            At.v "data-toc" slug;
            At.v "aria-label" (proj.Bushel.Project.title ^ ": " ^ spoken)] [
    El.span ~at:[At.class' "idea-toc-name"] [El.txt proj.Bushel.Project.title];
    El.span ~at:[At.class' "idea-toc-bar"; At.v "aria-hidden" "true"] bands;
    El.span ~at:[At.class' "idea-toc-total"; At.v "aria-hidden" "true"]
      [El.txt (string_of_int total)]]

(** [level_box ~count l] is the filter checkbox for level [l]. *)
let level_box ~count l =
  El.label ~at:[At.class' "idea-box"; At.v "title" (level_note l)] [
    El.input ~at:[At.type' "checkbox"; At.class' "idea-box-in";
                  At.v "data-level" (level_key l)] ();
    El.span ~at:[At.class' "idea-box-name"] [El.txt (level_label l)];
    El.span ~at:[At.class' "idea-box-n"] [El.txt (string_of_int count)]]

(** [status_box ~count s] is the filter checkbox for status [s]. *)
let status_box ~count s =
  El.label ~at:[At.class' ("idea-box idea-st-" ^ status_key s)] [
    El.input ~at:[At.type' "checkbox"; At.class' "idea-box-in";
                  At.v "data-status" (status_key s)] ();
    El.span ~at:[At.class' "idea-box-name"] [El.txt (status_label s)];
    El.span ~at:[At.class' "idea-box-n"] [El.txt (string_of_int count)]]

(** [ideas_list ~ctx] is the idea index grouped by project. *)
let ideas_list ~ctx =
  let all_ideas = Arod.Ctx.ideas ctx in
  let projects = Arod.Ctx.projects ctx |> List.sort Bushel.Project.compare in
  let by_project =
    List.filter_map (fun proj ->
      let slug = proj.Bushel.Project.slug in
      match List.filter (fun i -> Idea.project i = slug) all_ideas with
      | [] -> None
      | is ->
        let live, past = List.partition is_live is in
        let takeable, going = List.partition is_open live in
        let live =
          List.sort Idea.compare takeable @ List.sort Idea.compare going
        in
        Some (proj, live, List.sort Idea.compare past, is)
    ) projects
  in
  let by_project =
    let opens live = List.length (List.filter is_open live) in
    let going live = List.length live - opens live in
    let by f a b = compare (f b) (f a) in
    List.stable_sort (fun (_, l1, p1, _) (_, l2, p2, _) ->
      match by opens l1 l2 with
      | 0 ->
        (match by going l1 l2 with
         | 0 -> compare (List.length p2) (List.length p1)
         | c -> c)
      | c -> c) by_project
  in
  let groups =
    List.map (fun (proj, live, past, _) ->
      El.section ~at:[At.class' "idea-group";
                      At.id proj.Bushel.Project.slug;
                      At.v "data-idea-slug" proj.Bushel.Project.slug;
                      At.v "data-idea-group" proj.Bushel.Project.title] (
        group_head ~ctx proj
          ~n_open:(List.length (List.filter is_open live))
          ~n_going:(List.length (List.filter (fun i -> not (is_open i)) live))
          ~n_past:(List.length past)
        :: List.map (card ~ctx) live
        @ List.map (past_card ~ctx) past)
    ) by_project
  in
  let slots = statuses_present all_ideas in
  let widest =
    List.fold_left (fun n (_, _, _, is) -> max n (List.length is)) 1 by_project
  in
  let toc_rows =
    List.map (fun (proj, _, _, is) -> toc_row ~slots ~widest proj is)
      by_project
  in
  let level_boxes =
    List.filter_map (fun l ->
      let at_level i = Idea.level i = l in
      match List.length (List.filter at_level all_ideas) with
      | 0 -> None
      | count -> Some (level_box ~count l)
    ) [Idea.PartII; Idea.MPhil; Idea.PhD; Idea.Postdoc; Idea.Any]
  in
  let status_boxes =
    List.map (fun s ->
      let count = List.length (List.filter (fun i -> Idea.status i = s)
                                 all_ideas) in
      status_box ~count s) slots
  in
  let band =
    El.aside ~at:[At.class' "idea-band not-prose"] [
      El.div ~at:[At.class' "idea-facet"] (
        El.span ~at:[At.class' "idea-facet-label"] [El.txt "Status"]
        :: status_boxes);
      El.div ~at:[At.class' "idea-facet"] (
        (El.span ~at:[At.class' "idea-facet-label"] [El.txt "Level"]
         :: level_boxes)
        @ [El.button ~at:[At.class' "idea-clear"; At.type' "button";
                          At.id "idea-clear"; At.v "hidden" ""]
             [El.txt "clear"]]);
      El.div ~at:[At.class' "idea-band-part"] [
        El.h2 ~at:[At.class' "idea-band-label"] [El.txt "By project"];
        El.div ~at:[At.class' "idea-toc"] toc_rows]]
  in
  let intro =
    El.div ~at:[At.class' "idea-intro mb-4"] [
      El.p [
        El.txt "These are research ideas that include new, ongoing and completed projects. They are \
                only open to Cambridge students for now, with the occasional \
                exception for summer interns."];
      El.p [
        El.txt "I get a vast number of LLM-driven applications and cannot \
                reply to every one. Your chances are ";
        El.em [El.txt "much"];
        El.txt " higher if you read some of the ideas here and send a \
                short, specific enquiry about something concrete you would \
                like to do. Original ideas are welcome too, but try to \
                relate them to one of the projects here if you can."]]
  in
  let empty =
    El.p ~at:[At.class' "idea-empty"; At.id "idea-empty"; At.v "hidden" ""]
      [El.txt "Nothing matches that. Try a broader filter, or look through \
               the ideas offered previously."]
  in
  El.article ~at:[At.class' "h-feed"; At.v "data-idea-index" ""] [
    intro; band;
    El.div ~at:[At.class' "idea-grid not-prose"] groups;
    empty]

(** [for_feed ~ctx i] is the feed rendering of [i]. *)
let for_feed ~ctx i =
  let studs = Common.map_and (Printf.sprintf "[@%s]") (Idea.student_handles i) in
  let r = Printf.sprintf "This is an idea proposed %s, and %s.%s"
    (level_to_long_string (Idea.level i))
    (status_to_long_string studs (Idea.status i)) (sups_for i)
  in
  let body_html, word_count_info = Common.truncated_body ~ctx (`Idea i) in
  (El.div [El.unsafe_raw (fst (Arod.Md.to_html ~ctx r)); body_html], word_count_info)
