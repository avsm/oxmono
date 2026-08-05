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

(** Render an idea as a compact list row for project listings.
    Uses a coloured dot icon as the bullet; text stays neutral. *)
let to_html_no_sidenotes ~ctx idea =
  let idea_url = "/ideas/" ^ idea.Idea.slug in
  let sups = List.filter (fun x -> x <> "avsm") idea.Idea.supervisor_handles in
  let lev = match idea.Idea.level with
    | Idea.Any -> "" | PartII -> "Part II" | MPhil -> "MPhil"
    | PhD -> "PhD" | Postdoc -> "Postdoc"
  in
  let status_text = match idea.Idea.status with
    | Available -> "Available"
    | Discussion -> "Discussion"
    | Ongoing -> "Ongoing"
    | Completed -> "Completed"
    | Expired -> "Expired"
  in
  let detail_parts =
    [status_text] @
    (if lev <> "" then [lev] else []) @
    (match idea.Idea.status with
     | Ongoing | Completed when idea.Idea.student_handles <> [] -> []
     | _ -> [])
  in
  let detail_text = String.concat " \xC2\xB7 " detail_parts in
  let people_el = match idea.Idea.status with
    | Ongoing ->
      (match idea.Idea.student_handles with
       | [] -> El.void
       | _ -> El.span [El.txt " with "; render_contacts ~ctx idea.Idea.student_handles])
    | Completed ->
      (match idea.Idea.student_handles with
       | [] -> El.void
       | _ -> El.span [El.txt " by "; render_contacts ~ctx idea.Idea.student_handles;
                        El.txt (Printf.sprintf " (%d)" idea.Idea.year)])
    | _ -> El.void
  in
  let cosup_el = match sups with
    | [] -> El.void
    | _ -> El.span ~at:[At.class' "text-secondary"] [
        El.txt " + "; render_contacts ~ctx sups]
  in
  El.div ~at:[At.class' "idea-row"] [
    status_dot (Idea.status idea);
    El.div ~at:[At.class' "idea-row-content"] [
      El.a ~at:[At.href idea_url; At.class' "idea-row-title"]
        [El.txt (Idea.title idea)];
      El.span ~at:[At.class' "idea-row-meta"]
        [El.txt detail_text; people_el; cosup_el]]]

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

(** Combined status filter + stats sidebar box. *)
let status_filter_box ~total ~counts =
  let checkbox ~id ~label_text ~checked:is_checked ~status_name ~cls ~count =
    El.label ~at:[At.class' "idea-filter-row"] [
      El.input ~at:([At.type' "checkbox"; At.id id;
        At.class' "status-checkbox";
        At.v "data-status" status_name]
        @ (if is_checked then [At.checked] else [])) ();
      El.span ~at:[At.class' ("idea-dot " ^ cls)]
        [El.unsafe_raw (I.filled ~size:7 I.circle_f)];
      El.span ~at:[At.class' "idea-filter-label"] [El.txt label_text];
      El.span ~at:[At.class' "idea-stat-count"] [El.txt (string_of_int count)]]
  in
  let (n_avail, n_discuss, n_ongoing, n_done, n_expired) = counts in
  Common.meta_box
    ~header:[El.txt (Printf.sprintf " filter: %d ideas" total)]
    [checkbox ~id:"filter-available" ~label_text:"Available"
       ~checked:true ~status_name:"Available" ~cls:"idea-available" ~count:n_avail;
     checkbox ~id:"filter-discussion" ~label_text:"Discussion"
       ~checked:true ~status_name:"Discussion" ~cls:"idea-discussion" ~count:n_discuss;
     checkbox ~id:"filter-ongoing" ~label_text:"Ongoing"
       ~checked:true ~status_name:"Ongoing" ~cls:"idea-ongoing" ~count:n_ongoing;
     checkbox ~id:"filter-completed" ~label_text:"Completed"
       ~checked:true ~status_name:"Completed" ~cls:"idea-completed" ~count:n_done;
     checkbox ~id:"filter-expired" ~label_text:"Expired"
       ~checked:false ~status_name:"Expired" ~cls:"idea-expired" ~count:n_expired]

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
              At.v "data-status" (Idea.status_to_string status);
              At.v "data-year" (string_of_int year)] [
    El.div ~at:[At.class' "note-compact-row"] [
      status_dot status;
      El.a ~at:[At.href url; At.class' "note-compact-title flex-1 min-w-0 font-medium !text-text !no-underline p-name u-url"]
        [El.txt (Idea.title idea)];
      El.span ~at:[At.class' "note-compact-meta shrink-0 text-[0.82rem] text-secondary whitespace-nowrap tabular-nums"]
        [El.txt meta_text]];
    El.div ~at:[At.class' "note-compact-synopsis text-[0.85rem] text-secondary leading-[1.4] mt-[0.1rem] p-summary"]
      [El.txt synopsis_text]]

(** Ideas grouped by project with status filter and year heatmap.
    Returns [(article, sidebar)]. *)
let ideas_list ~ctx =
  let all_ideas = Arod.Ctx.ideas ctx in
  let all_projects =
    Arod.Ctx.projects ctx
    |> List.sort Bushel.Project.compare
  in
  let total = List.length all_ideas in
  let ideas_by_project = Hashtbl.create 32 in
  List.iter (fun i ->
    let proj_slug = Idea.project i in
    let existing =
      try Hashtbl.find ideas_by_project proj_slug with Not_found -> []
    in
    Hashtbl.replace ideas_by_project proj_slug (i :: existing)
  ) all_ideas;
  Hashtbl.iter (fun proj_slug ideas ->
    Hashtbl.replace ideas_by_project proj_slug (List.sort Idea.compare ideas)
  ) ideas_by_project;
  (* Collect projects that have ideas *)
  let projects_with_ideas = List.filter_map (fun proj ->
    let proj_slug = proj.Bushel.Project.slug in
    match Hashtbl.find_opt ideas_by_project proj_slug with
    | None -> None
    | Some ideas -> Some (proj, ideas)
  ) all_projects in
  (* Build project sections *)
  let project_sections = List.map (fun (proj, ideas) ->
    let proj_slug = proj.Bushel.Project.slug in
    let cards = List.map (compact ~ctx) ideas in
    let thumbnail_md =
      Printf.sprintf "![%%lc](:project-%s \"%s\")"
        proj_slug proj.Bushel.Project.title
    in
    let thumbnail_html = El.unsafe_raw (fst (Arod.Md.to_html ~ctx thumbnail_md)) in
    let body = Bushel.Project.body proj in
    let first, _ = Bushel.Util.first_and_last_hunks body in
    let summary_html = El.unsafe_raw (Arod.Md.to_plain_html ~ctx first) in
    let date_range = match proj.Bushel.Project.finish with
      | Some y -> Printf.sprintf "%d\u{2013}%d" proj.Bushel.Project.start y
      | None -> Printf.sprintf "%d\u{2013}now" proj.Bushel.Project.start
    in
    El.div ~at:[At.id ("proj-" ^ proj_slug); At.class' "idea-project-section mb-8"] [
      El.div ~at:[At.class' "proj-card-header sticky top-0 z-10"] [
        El.span ~at:[At.class' "proj-card-prompt"] [El.txt ">_"];
        El.a ~at:[At.href ("/projects/" ^ proj_slug);
                  At.class' "proj-card-title no-underline"]
          [El.txt proj.Bushel.Project.title];
        El.span ~at:[At.class' "proj-card-date"] [El.txt date_range]];
      El.div ~at:[At.class' "idea-project-brief not-prose"] [
        El.div ~at:[At.class' "idea-project-thumb"] [thumbnail_html];
        summary_html];
      El.div ~at:[At.class' "note-month-list"] cards]
  ) projects_with_ideas in
  (* Sidebar: status filter *)
  let count_status s = List.length (List.filter (fun i -> Idea.status i = s) all_ideas) in
  let counts = (count_status Idea.Available, count_status Idea.Discussion,
                count_status Idea.Ongoing, count_status Idea.Completed,
                count_status Idea.Expired) in
  let filter_box = status_filter_box ~total ~counts in
  (* Sidebar: project jump list with per-project status bars *)
  let max_ideas =
    List.fold_left (fun acc (_, ideas) -> max acc (List.length ideas))
      1 projects_with_ideas
  in
  let proj_jump_items = List.map (fun (proj, ideas) ->
    let proj_slug = proj.Bushel.Project.slug in
    let n = List.length ideas in
    let cs s = List.length (List.filter (fun i -> Idea.status i = s) ideas) in
    let n_a = cs Idea.Available and n_d = cs Idea.Discussion
    and n_o = cs Idea.Ongoing and n_c = cs Idea.Completed
    and n_e = cs Idea.Expired in
    let bar_seg cls count =
      if count = 0 then El.void
      else El.span ~at:[At.class' cls;
                        At.v "style" (Printf.sprintf "flex:%d" count);
                        At.v "title" (Printf.sprintf "%d" count)] []
    in
    let bar_pct = n * 100 / max_ideas in
    let bar =
      El.span ~at:[At.class' "idea-status-bar idea-proj-bar";
                    At.v "style" (Printf.sprintf "width:%d%%" bar_pct)] [
        bar_seg "bar-available" n_a;
        bar_seg "bar-discussion" n_d;
        bar_seg "bar-ongoing" n_o;
        bar_seg "bar-completed" n_c;
        bar_seg "bar-expired" n_e]
    in
    El.a ~at:[At.href ("#proj-" ^ proj_slug);
              At.class' "idea-jump-link no-underline"]
      [El.span ~at:[At.class' "idea-jump-title"]
         [El.txt proj.Bushel.Project.title];
       El.span ~at:[At.class' "idea-jump-count"]
         [El.txt (string_of_int n)];
       bar]
  ) projects_with_ideas in
  let proj_jump_box =
    Common.meta_box ~body_cls:"sidebar-meta-body idea-jump-list"
      ~header:[El.txt " projects"] proj_jump_items
  in
  let intro = El.p ~at:[At.class' "text-sm text-gray-600 dark:text-gray-400 mb-6"] [
    El.txt "These are research ideas for students at various levels \
         (Part II, MPhil, PhD, and postdoctoral). Browse through the ideas \
         below to find projects that interest you. You're also welcome to \
         propose your own research ideas that align with our ongoing projects."]
  in
  let article = El.article ~at:[At.class' "h-feed"] [
    intro;
    El.div project_sections]
  in
  let sidebar =
    El.aside ~at:[At.class' "hidden lg:block lg:w-72 shrink-0"]
      [El.div ~at:[At.class' "sticky top-16"]
         [filter_box; proj_jump_box]]
  in
  (article, sidebar)

(** Idea for feeds. *)
let for_feed ~ctx i =
  let studs = Common.map_and (Printf.sprintf "[@%s]") (Idea.student_handles i) in
  let r = Printf.sprintf "This is an idea proposed %s, and %s.%s"
    (level_to_long_string (Idea.level i))
    (status_to_long_string studs (Idea.status i)) (sups_for i)
  in
  let body_html, word_count_info = Common.truncated_body ~ctx (`Idea i) in
  (El.div [El.unsafe_raw (fst (Arod.Md.to_html ~ctx r)); body_html], word_count_info)
