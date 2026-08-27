(*---------------------------------------------------------------------------
  Copyright (c) 2025 Anil Madhavapeddy <anil@recoil.org>. All rights reserved.
  SPDX-License-Identifier: ISC
 ---------------------------------------------------------------------------*)

(** Idea component rendering using htmlit. *)

open Htmlit

module Idea = Bushel.Idea
module Contact = Sortal_schema.Contact
module I = Arod.Icons

(** {1 Helpers} *)

(** Render a heading for an entry. *)
let heading ~ctx:_ ent =
  El.h2 ~at:[At.class' "text-xl font-semibold mb-2"] [
    El.a ~at:[At.href (Bushel.Entry.site_url ent)] [
      El.txt (Bushel.Entry.title ent)];
    El.span ~at:[At.class' "text-sm text-secondary"] [
      El.txt " / ";
      El.txt (Common.ptime_date_short (Bushel.Entry.date ent))]]

(** {1 Status and Level Descriptions} *)

let status_class = function
  | Idea.Available -> "font-medium text-st-avail"
  | Discussion -> "font-medium text-st-discuss"
  | Ongoing -> "font-medium text-st-ongoing"
  | Completed -> "font-medium text-st-done"
  | Expired -> "font-medium text-st-expired"

(** Colored status indicator span. *)
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
  | Idea.Any -> " as a good starter project"
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

(** {1 Contact Rendering} *)

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

(** Small filled status dot icon with the appropriate status colour class. *)
let status_dot status =
  let cls = "idea-dot " ^ status_class status in
  El.span ~at:[At.class' cls]
    [El.unsafe_raw (I.filled ~size:8 I.circle_f)]

(** {1 Main Rendering Functions} *)

(** Brief idea with status/level info. *)
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

(** Full idea page with structured header and article. *)
let full_page ~ctx i =
  let level_str = Common.idea_level_to_string (Idea.level i) in
  let sups = List.filter (fun x -> x <> "avsm") i.Idea.supervisor_handles in
  (* Mobile-only meta row *)
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
  let header_el =
    El.header ~at:[At.id "intro"; At.class' "mb-6"]
      [meta_row; title_el]
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

(** Compact idea card for list view. *)
let compact ~ctx idea =
  let year = Idea.year idea in
  let status = Idea.status idea in
  let level_str = match Idea.level idea with
    | Idea.Any -> "" | PartII -> "Part II" | MPhil -> "MPhil"
    | PhD -> "PhD" | Postdoc -> "Postdoc"
  in
  let meta_parts =
    [string_of_int year] @
    (if level_str <> "" then [level_str] else [])
  in
  let meta_text = String.concat " \xC2\xB7 " meta_parts in
  let url = "/ideas/" ^ idea.Idea.slug in
  let status_str = Idea.status_to_string status in
  let resolve_handle h =
    match Arod.Ctx.lookup_by_handle ctx h with
    | Some c -> Contact.name c
    | None -> "@" ^ h
  in
  let people_text = match status with
    | Ongoing ->
      (match idea.Idea.student_handles with
       | [] -> "" | handles -> " with " ^ Common.map_and resolve_handle handles)
    | Completed ->
      (match idea.Idea.student_handles with
       | [] -> "" | handles -> " by " ^ Common.map_and resolve_handle handles)
    | _ -> ""
  in
  let sups = List.filter (fun x -> x <> "avsm") idea.Idea.supervisor_handles in
  let cosup_text = match sups with
    | [] -> ""
    | _ -> ", with " ^ Common.map_and resolve_handle sups
  in
  let synopsis_text = status_str ^ people_text ^ cosup_text in
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
      [El.txt synopsis_text]]

(** {1 Ideas Index}

    The index is written for a student looking for something to work on. Ideas
    are grouped under the project that owns them, and the group heading is the
    only link to the project page, which is what keeps this page from restating
    the project index. An idea still open for takers is a card with a picture,
    and one already taken or finished is a collapsed line under the same
    heading, so the history of a project stays in view without competing with
    what is on offer. *)

(** [is_open i] is whether [i] can still be taken on by a student. *)
let is_open i =
  match Idea.status i with
  | Idea.Available | Idea.Discussion -> true
  | Ongoing | Completed | Expired -> false

(** [level_key l] is the token the filter script matches on. It is the
    constructor name rather than the reader-facing label, so rewording the
    label cannot change what the filter selects. *)
let level_key = function
  | Idea.Any -> "Any" | PartII -> "PartII" | MPhil -> "MPhil"
  | PhD -> "PhD" | Postdoc -> "Postdoc"

(** [level_label l] is how a level reads on a card and on a filter row. *)
let level_label = function
  | Idea.Any -> "Any level" | PartII -> "Part II" | MPhil -> "MPhil"
  | PhD -> "PhD" | Postdoc -> "Postdoc"

(** [level_note l] tells a student whether [l] is the row they want, which the
    bare label does not for a reader outside Cambridge. *)
let level_note = function
  | Idea.Any -> "suits any level, and makes a good starter"
  | PartII -> "Computer Science undergraduate final year"
  | MPhil -> "Part III or a one year MPhil"
  | PhD -> "a doctoral topic, over three or four years"
  | Postdoc -> "postdoctoral research"

(** [level_phrase l] opens the sentence under a card title. *)
let level_phrase = function
  | Idea.Any -> "Suits any level"
  | PartII -> "A Part II project"
  | MPhil -> "An MPhil or Part III project"
  | PhD -> "A PhD topic"
  | Postdoc -> "A postdoctoral project"

(** [status_key s] names [s] in a class. A card and a folded line take their
    border colour from it, so a border and the status dot beside it always
    name the same state. *)
let status_key = function
  | Idea.Available -> "avail" | Discussion -> "discuss" | Ongoing -> "ongoing"
  | Completed -> "done" | Expired -> "expired"

(** [status_cls i] is the card classes of [i] up to its status colour. *)
let status_cls i = "idea-st-" ^ status_key (Idea.status i)

(** [filter_at i] is the attributes the filter script reads. An open card and a
    past line carry the same keys so one pass filters both. *)
let filter_at i =
  [At.v "data-idea-item" ""; At.v "data-level" (level_key (Idea.level i))]

(** [resolve_names ~ctx handles] is the display names of [handles], joined for
    reading. *)
let resolve_names ~ctx handles =
  Common.map_and (fun h ->
    match Arod.Ctx.lookup_by_handle ctx h with
    | Some c -> Contact.name c
    | None -> "@" ^ h) handles

(** [summary_text ~ctx i ~max_len] is the opening of the body of [i] as plain
    text, so a card or a panel cannot turn into a wall of links. *)
let summary_text ~ctx ~max_len i =
  let first, _ = Bushel.Util.first_and_last_hunks (Idea.body i) in
  Arod.Text.plain_summary ~max_len (Arod.Md.to_plain_html ~ctx first)

(** [card ~ctx i] is an idea open for takers. Its picture is washed into the
    card behind the words rather than set beside them, so a card carries the
    look of the work without a thumbnail column stealing the width the title
    needs. *)
let card ~ctx i =
  let url = "/ideas/" ^ Idea.slug i in
  let sups = List.filter (fun x -> x <> "avsm") i.Idea.supervisor_handles in
  let art =
    match Bushel.Entry.thumbnail (Arod.Ctx.entries ctx) (`Idea i) with
    | None -> El.void
    | Some src ->
      El.div ~at:[At.class' "idea-card-art"; At.v "aria-hidden" "true"]
        [El.img ~at:[At.src src; At.alt ""; At.v "loading" "lazy"] ()]
  in
  let meta =
    let opening =
      Printf.sprintf "%s, proposed in %d"
        (level_phrase (Idea.level i)) (Idea.year i)
    in
    let cosup =
      match sups with
      | [] -> [El.txt "."]
      | _ -> [El.txt ", co-supervised with "; render_contacts ~ctx sups;
              El.txt "."]
    in
    (* An idea already under discussion is still listed here, since it is not
       yet confirmed, but it must not read as freely available. *)
    let status =
      match Idea.status i with
      | Idea.Discussion ->
        [El.txt " ";
         El.span ~at:[At.class' "idea-card-discuss"]
           [El.txt "Already under discussion with a student."]]
      | _ -> []
    in
    El.p ~at:[At.class' "idea-card-meta"]
      ((El.txt opening :: cosup) @ status)
  in
  let summary =
    match summary_text ~ctx ~max_len:190 i with
    | None -> El.void
    | Some text ->
      El.p ~at:[At.class' "idea-card-summary p-summary"] [El.txt text]
  in
  El.article
    ~at:(At.class' ("idea-card h-entry " ^ status_cls i) :: filter_at i) [
    art;
    El.div ~at:[At.class' "idea-card-main"] [
      El.a ~at:[At.href url; At.class' "idea-card-title p-name u-url"]
        [El.txt (Idea.title i)];
      meta; summary]]

(** [past_card ~ctx i] is an idea no longer on offer, folded to one line. The
    line says who took it, since that is what a student reading the history of
    a project wants from it. *)
let past_card ~ctx i =
  let status = Idea.status i in
  let year = Idea.year i in
  let line =
    match status, Idea.student_handles i with
    | Idea.Completed, (_ :: _ as hs) ->
      Printf.sprintf "Completed by %s in %d" (resolve_names ~ctx hs) year
    | Completed, [] -> Printf.sprintf "Completed in %d" year
    | Ongoing, (_ :: _ as hs) ->
      Printf.sprintf "Under way with %s since %d" (resolve_names ~ctx hs) year
    | Ongoing, [] -> Printf.sprintf "Under way since %d" year
    | Expired, _ -> Printf.sprintf "Offered in %d, no longer open" year
    | (Available | Discussion), _ -> Printf.sprintf "Offered in %d" year
  in
  let sups = List.filter (fun x -> x <> "avsm") i.Idea.supervisor_handles in
  let detail =
    (match summary_text ~ctx ~max_len:260 i with
     | None -> []
     | Some text -> [El.p ~at:[At.class' "idea-past-text"] [El.txt text]])
    @ (match sups with
       | [] -> []
       | _ -> [El.p ~at:[At.class' "idea-past-sups"]
                 [El.txt "Co-supervised with "; render_contacts ~ctx sups;
                  El.txt "."]])
  in
  let detail =
    match detail with
    | [] ->
      (* Nothing to reveal, so the panel carries the link rather than
         opening on an empty box. *)
      [El.a ~at:[At.href ("/ideas/" ^ Idea.slug i);
                 At.class' "idea-past-text"]
         [El.txt "Read the full idea"]]
    | ds -> ds
  in
  (* The link sits on the folded line rather than in the panel, so reaching
     the idea itself never costs an extra click. The filter script stops the
     click here from reaching the summary, which would open the panel as
     well as follow the link. *)
  let open_link =
    El.a ~at:[At.href ("/ideas/" ^ Idea.slug i);
              At.class' "idea-past-open u-url";
              At.v "title" ("Read " ^ Idea.title i)]
      [El.unsafe_raw (I.outline ~size:12 I.arrow_up_right_o)]
  in
  El.details
    ~at:(At.class' ("idea-past-card h-entry " ^ status_cls i) :: filter_at i) [
    El.summary ~at:[At.class' "idea-past-head"] [
      El.span ~at:[At.class' "idea-past-line"] [
        status_dot status;
        El.span ~at:[At.class' "idea-past-title p-name"]
          [El.txt (Idea.title i)];
        open_link];
      El.span ~at:[At.class' "idea-past-meta"] [El.txt line]];
    El.div ~at:[At.class' "idea-past-detail"] detail]

(** [group_head proj ~n_open ~n_past] heads a project block with a link to the
    project page, which is where the wider context for its ideas lives. The
    chevron opens every collapsed line in the block at once. *)
let group_head proj ~n_open ~n_past =
  let expand =
    if n_past = 0 then El.void
    else
      El.button ~at:[At.class' "idea-expand"; At.type' "button";
                     At.v "data-expand-all" "";
                     At.v "aria-expanded" "false";
                     At.v "title" "show every idea offered previously"] [
        El.span ~at:[At.class' "idea-expand-count"]
          [El.txt (Printf.sprintf "%d previous" n_past)];
        El.span ~at:[At.class' "idea-expand-icon"]
          [El.unsafe_raw (I.outline ~size:12 I.chevron_down_o)]]
  in
  El.div ~at:[At.class' "idea-group-head"] [
    El.span ~at:[At.class' "idea-group-prompt"] [El.txt ">_"];
    El.a ~at:[At.href ("/projects/" ^ proj.Bushel.Project.slug);
              At.class' "idea-group-title"]
      [El.txt proj.Bushel.Project.title];
    (* A project with nothing on offer shows no open count, so a bare zero
       does not read as the more prominent of the two numbers. *)
    (if n_open = 0 then El.void
     else El.span ~at:[At.class' "idea-group-count"]
            [El.txt (Printf.sprintf "%d open" n_open)]);
    expand]

(** [level_row ~count l] is a filter row for one academic level. Levels are
    rows rather than a wrapped strip of chips because a student picks exactly
    one, and a row has space for the note saying which one that is. *)
let level_row ~count l =
  El.button ~at:[At.class' "idea-level"; At.type' "button";
                 At.v "data-level" (level_key l)] [
    El.span ~at:[At.class' "idea-level-name"] [El.txt (level_label l)];
    El.span ~at:[At.class' "idea-level-note"] [El.txt (level_note l)];
    El.span ~at:[At.class' "idea-level-count"] [El.txt (string_of_int count)]]

(** Ideas grouped by project, open ones as cards and the rest as collapsed
    lines beneath them. Returns the article alone, since the page is laid out
    full width. *)
let ideas_list ~ctx =
  let all_ideas = Arod.Ctx.ideas ctx in
  let projects = Arod.Ctx.projects ctx |> List.sort Bushel.Project.compare in
  let open_ideas = List.filter is_open all_ideas in
  let n_open = List.length open_ideas in
  let groups =
    List.filter_map (fun proj ->
      let slug = proj.Bushel.Project.slug in
      match List.filter (fun i -> Idea.project i = slug) all_ideas with
      | [] -> None
      | is ->
        let opened, past = List.partition is_open is in
        let opened = List.sort Idea.compare opened in
        let past = List.sort Idea.compare past in
        Some (
          El.section ~at:[At.class' "idea-group";
                          At.v "data-idea-group" proj.Bushel.Project.title] (
            group_head proj ~n_open:(List.length opened)
              ~n_past:(List.length past)
            :: List.map (card ~ctx) opened
            @ List.map (past_card ~ctx) past))
    ) projects
  in
  let level_rows =
    List.filter_map (fun l ->
      let at_level i = Idea.level i = l in
      match List.length (List.filter at_level open_ideas) with
      | 0 -> None
      | count -> Some (level_row ~count l)
    ) [Idea.PartII; Idea.MPhil; Idea.PhD; Idea.Postdoc; Idea.Any]
  in
  let band =
    El.div ~at:[At.class' "idea-band not-prose"] [
      El.div ~at:[At.class' "idea-band-top"] [
        El.span ~at:[At.class' "idea-band-icon"]
          [El.unsafe_raw (I.outline ~size:14 I.search_o)];
        El.input ~at:[At.type' "search"; At.id "idea-search";
                      At.class' "idea-search";
                      At.placeholder "filter ideas by keyword";
                      At.autocomplete "off"] ();
        El.span ~at:[At.class' "idea-band-status"] [
          El.span ~at:[At.id "idea-count"]
            [El.txt (Printf.sprintf "%d open for takers" n_open)];
          El.button ~at:[At.class' "idea-clear"; At.type' "button";
                         At.id "idea-clear"; At.v "hidden" ""]
            [El.txt "clear"]]];
      El.div ~at:[At.class' "idea-levels"] level_rows]
  in
  let intro =
    El.p ~at:[At.class' "mb-4"] [
      El.txt "These are research ideas looking for a student. Each one says \
              the level it suits and sits under the project that owns it, so \
              follow the project link for the wider context. Pick a level or \
              type a keyword to narrow the list, then write to me to discuss \
              an idea. You are welcome to propose your own along the lines of \
              an existing project. Ideas offered previously stay under their \
              project as folded lines, which the chevron beside a project \
              opens."]
  in
  let empty =
    El.p ~at:[At.class' "idea-empty"; At.id "idea-empty"; At.v "hidden" ""]
      [El.txt "Nothing matches that. Try a broader filter, or look through \
               the ideas offered previously."]
  in
  El.article ~at:[At.class' "h-feed"; At.v "data-idea-index" "";
                  At.v "data-idea-open" (string_of_int n_open)] [
    intro; band;
    El.div ~at:[At.class' "idea-grid not-prose"] groups;
    empty]

(** Idea for feeds. *)
let for_feed ~ctx i =
  let studs = Common.map_and (Printf.sprintf "[@%s]") (Idea.student_handles i) in
  let r = Printf.sprintf "This is an idea proposed %s, and %s.%s"
    (level_to_long_string (Idea.level i))
    (status_to_long_string studs (Idea.status i)) (sups_for i)
  in
  let body_html, word_count_info = Common.truncated_body ~ctx (`Idea i) in
  (El.div [El.unsafe_raw (fst (Arod.Md.to_html ~ctx r)); body_html], word_count_info)
