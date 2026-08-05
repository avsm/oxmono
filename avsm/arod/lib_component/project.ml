(*---------------------------------------------------------------------------
  Copyright (c) 2025 Anil Madhavapeddy <anil@recoil.org>. All rights reserved.
  SPDX-License-Identifier: ISC
 ---------------------------------------------------------------------------*)

(** Project component rendering using htmlit. *)

open Htmlit

module Project = Bushel.Project
module B_paper = Bushel.Paper
module I = Arod.Icons

module StringSet = Set.Make(String)

(** {1 Main Rendering Functions} *)

(** Project card with recent papers/notes. *)
let card ~ctx proj =
  let all_entries = Arod.Ctx.all_entries ctx in
  let project_slug = proj.Project.slug in
  let recent_papers =
    List.filter (fun e ->
      match e with
      | `Paper paper -> List.mem project_slug (B_paper.project_slugs paper)
      | _ -> false
    ) all_entries
    |> List.sort (fun a b -> compare (Bushel.Entry.date b) (Bushel.Entry.date a))
    |> (fun l -> Common.take 3 l)
  in
  let backlink_slugs = Bushel.Link_graph.get_backlinks_for_slug project_slug in
  let backlink_set =
    List.fold_left (fun acc slug -> StringSet.add slug acc) StringSet.empty backlink_slugs
  in
  let recent_notes =
    List.filter (fun e ->
      match e with
      | `Note _ -> StringSet.mem (Bushel.Entry.slug e) backlink_set
      | _ -> false
    ) all_entries
    |> List.sort (fun a b -> compare (Bushel.Entry.date b) (Bushel.Entry.date a))
    |> (fun l -> Common.take 3 l)
  in
  let entry_row icon_svg ent =
    Common.card_entry_row
      ~icon:(I.outline ~size:12 icon_svg)
      ~href:(Bushel.Entry.site_url ent)
      ~title:(Bushel.Entry.title ent)
  in
  let paper_items = List.map (entry_row I.paper_o) recent_papers in
  let note_items = List.map (entry_row I.writing_o) recent_notes in
  let recent_items_display =
    if paper_items = [] && note_items = [] then El.void
    else
      El.div ~at:[At.class' "project-entries not-prose"] (paper_items @ note_items)
  in
  let body_html, _wc = Common.truncated_body ~ctx (`Project proj) in
  El.div ~at:[At.class' "mb-6 border rounded-lg p-4"] [
    El.h3 ~at:[At.class' "text-lg font-semibold mb-2"] [
      El.a ~at:[At.href ("/projects/" ^ proj.Project.slug)] [El.txt proj.Project.title]];
    El.div ~at:[At.class' "mb-2"] [body_html];
    recent_items_display]

(** Full project with activity stream and references. *)
let full ~ctx proj =
  let project_slug = proj.Project.slug in
  let backlink_slugs = Bushel.Link_graph.get_backlinks_for_slug project_slug in
  let outbound_slugs = Bushel.Link_graph.get_outbound_for_slug project_slug in
  let backlink_set =
    List.fold_left (fun acc slug -> StringSet.add slug acc) StringSet.empty backlink_slugs
  in
  let all_entries = Arod.Ctx.all_entries ctx in
  let entries = Arod.Ctx.entries ctx in
  (* Papers explicitly tagged with this project *)
  let project_papers =
    List.filter (fun e ->
      match e with
      | `Paper paper -> List.mem project_slug (B_paper.project_slugs paper)
      | _ -> false
    ) all_entries
    |> List.sort (fun a b -> compare (Bushel.Entry.date b) (Bushel.Entry.date a))
  in
  (* Ideas belonging to this project *)
  let project_ideas =
    List.filter (fun e ->
      match e with
      | `Idea i -> Bushel.Idea.project i = project_slug
      | _ -> false
    ) all_entries
    |> List.sort (fun a b -> compare (Bushel.Entry.date b) (Bushel.Entry.date a))
  in
  (* Backlinked entries (notes, videos, etc.) *)
  let backlinked_entries =
    List.filter (fun e ->
      match e with
      | `Paper _ -> false  (* papers shown separately *)
      | `Idea _ -> false   (* ideas shown separately *)
      | _ -> StringSet.mem (Bushel.Entry.slug e) backlink_set
    ) all_entries
    |> List.sort (fun a b -> compare (Bushel.Entry.date b) (Bushel.Entry.date a))
  in
  (* Outbound entries not already covered *)
  let covered = Hashtbl.create 32 in
  List.iter (fun e -> Hashtbl.replace covered (Bushel.Entry.slug e) ()) project_papers;
  List.iter (fun e -> Hashtbl.replace covered (Bushel.Entry.slug e) ()) project_ideas;
  List.iter (fun e -> Hashtbl.replace covered (Bushel.Entry.slug e) ()) backlinked_entries;
  Hashtbl.replace covered project_slug ();
  let outbound_entries =
    List.filter_map (fun s ->
      if Hashtbl.mem covered s then None
      else match Bushel.Entry.lookup entries s with
      | Some ent -> Hashtbl.replace covered s (); Some ent
      | None -> None
    ) outbound_slugs
    |> List.sort (fun a b -> compare (Bushel.Entry.date b) (Bushel.Entry.date a))
  in
  (* Feed backlinks — network entries annotated as related to this project *)
  let feed_bls = Arod.Ctx.feed_backlinks_for_slug ctx project_slug in
  let outbound_feed = Arod.Ctx.feed_items_for_outbound ctx project_slug in
  let feed_seen = Hashtbl.create 16 in
  let all_feed_bls = List.filter (fun (bl : Arod.Ctx.feed_backlink) ->
    let u = match bl.feed_entry.Sortal_feed.Entry.url with
      | Some u -> Uri.to_string u | None -> "" in
    if u = "" || Hashtbl.mem feed_seen u then false
    else (Hashtbl.add feed_seen u (); true)
  ) (feed_bls @ outbound_feed) in
  (* Unified activity stream — entries + feed backlinks sorted by date *)
  let entry_items =
    List.map (fun ent ->
      Sidebar.Entry_item (ent, Bushel.Entry.date ent))
      (project_papers @ project_ideas @ backlinked_entries @ outbound_entries)
  in
  let feed_items = List.map (fun (bl : Arod.Ctx.feed_backlink) ->
    let d = match bl.feed_entry.Sortal_feed.Entry.date with
      | Some pt -> let (y, m, d), _ = Ptime.to_date_time pt in (y, m, d)
      | None -> (0, 0, 0)
    in
    Sidebar.Feed_item (bl, d)
  ) all_feed_bls in
  let all_items = List.sort (fun a b ->
    let da = match a with Sidebar.Entry_item (_, d) -> d | Sidebar.Feed_item (_, d) -> d in
    let db = match b with Sidebar.Entry_item (_, d) -> d | Sidebar.Feed_item (_, d) -> d in
    compare db da
  ) (entry_items @ feed_items) in
  let activity_section = match all_items with
    | [] -> El.void
    | items ->
      let rows = List.map (fun item ->
        match item with
        | Sidebar.Entry_item (ent, _) -> Sidebar.activity_row ~ctx ent
        | Sidebar.Feed_item (bl, _) -> Sidebar.feed_backlink_row bl
      ) items in
      El.div ~at:[At.class' "mt-6"] [
        El.h2 ~at:[At.class' "text-lg font-semibold mb-3"] [El.txt "Activity"];
        El.div ~at:[At.class' "project-activity-list not-prose"] rows]
  in
  (* Ideas summary section — compact cards like the ideas index page *)
  let ideas_section =
    let idea_values = List.filter_map (fun e ->
      match e with `Idea i -> Some i | _ -> None
    ) project_ideas in
    match idea_values with
    | [] -> El.void
    | ideas ->
      let cards = List.map (Idea.compact ~ctx) ideas in
      El.div ~at:[At.class' "mt-6"] [
        El.h2 ~at:[At.class' "text-lg font-semibold mb-3"] [El.txt "Ideas"];
        El.div ~at:[At.class' "note-month-list not-prose"] cards]
  in
  let body_html, sidenotes = Arod.Md.to_html ~ctx (Project.body proj) in
  let logo_el =
    let entries = Arod.Ctx.entries ctx in
    match Bushel.Entry.thumbnail entries (`Project proj) with
    | Some src ->
      El.img ~at:[At.class' "proj-detail-logo";
                  At.src src; At.v "alt" (Project.title proj)] ()
    | None -> El.void
  in
  (El.div ~at:[At.class' "mb-4 h-entry"] [
    El.h1 ~at:[At.class' "page-title text-xl font-semibold mb-3 p-name"] [El.txt (Project.title proj)];
    El.div ~at:[At.class' "e-content"] [logo_el; El.unsafe_raw body_html];
    ideas_section;
    activity_section], sidenotes)

(** Masonry-style two-column project grid with terminal-inspired cards. *)
let projects_list ~ctx =
  let all_projects =
    Arod.Ctx.projects ctx |> List.sort Project.compare
  in
  let all_entries = Arod.Ctx.all_entries ctx in
  let project_card proj =
    let project_slug = proj.Project.slug in
    let start_year = proj.Project.start in
    let date_range = match proj.Project.finish with
      | Some y -> Printf.sprintf "%d\u{2013}%d" start_year y
      | None -> Printf.sprintf "%d\u{2013}now" start_year
    in
    let recent_papers =
      List.filter (fun e ->
        match e with
        | `Paper paper -> List.mem project_slug (B_paper.project_slugs paper)
        | _ -> false
      ) all_entries
      |> List.sort (fun a b -> compare (Bushel.Entry.date b) (Bushel.Entry.date a))
      |> (fun l -> Common.take 3 l)
    in
    let backlink_slugs = Bushel.Link_graph.get_backlinks_for_slug project_slug in
    let backlink_set =
      List.fold_left (fun acc slug ->
        StringSet.add slug acc) StringSet.empty backlink_slugs
    in
    let recent_notes =
      List.filter (fun e ->
        match e with
        | `Note _ -> StringSet.mem (Bushel.Entry.slug e) backlink_set
        | _ -> false
      ) all_entries
      |> List.sort (fun a b -> compare (Bushel.Entry.date b) (Bushel.Entry.date a))
      |> (fun l -> Common.take 3 l)
    in
    let recent_ideas =
      List.filter (fun e ->
        match e with
        | `Idea i -> Bushel.Idea.project i = project_slug
        | _ -> false
      ) all_entries
      |> List.sort (fun a b -> compare (Bushel.Entry.date b) (Bushel.Entry.date a))
      |> (fun l -> Common.take 3 l)
    in
    let all_recent =
      (List.map (fun e -> (I.paper_o, e)) recent_papers) @
      (List.map (fun e -> (I.writing_o, e)) recent_notes) @
      (List.map (fun e -> (I.bulb_o, e)) recent_ideas)
    in
    let all_recent =
      List.sort (fun (_, a) (_, b) ->
        compare (Bushel.Entry.date b) (Bushel.Entry.date a)) all_recent
      |> (fun l -> Common.take 5 l)
    in
    let recent_items =
      if all_recent = [] then El.void
      else
        El.div ~at:[At.class' "proj-card-recent"] (
          (El.div ~at:[At.class' "proj-card-section-label"]
            [El.txt "recent"]) ::
          List.map (fun (icon, ent) ->
            Common.card_entry_row
              ~icon:(I.outline ~size:11 icon)
              ~href:(Bushel.Entry.site_url ent)
              ~title:(Bushel.Entry.title ent)) all_recent)
    in
    (* Thumbnail *)
    let thumbnail_md =
      Printf.sprintf "![%%lc](:project-%s \"%s\")"
        proj.Project.slug proj.Project.title
    in
    let thumbnail_html = El.unsafe_raw (fst (Arod.Md.to_html ~ctx thumbnail_md)) in
    (* Summary — first paragraph *)
    let body = Project.body proj in
    let first, _ = Bushel.Util.first_and_last_hunks body in
    let summary_html = El.unsafe_raw (Arod.Md.to_plain_html ~ctx first) in
    (* Tags *)
    let tags_el = Common.card_tags (Project.tags proj) in
    El.div ~at:[At.class' "proj-card not-prose"] [
      (* Header *)
      Common.card_header ~prompt:">_" ~title:proj.Project.title
        ~href:("/projects/" ^ project_slug)
        (El.span ~at:[At.class' "proj-card-date"] [El.txt date_range]);
      (* Body *)
      El.div ~at:[At.class' "proj-card-body"] [
        El.div ~at:[At.class' "proj-card-thumb"] [thumbnail_html];
        El.div ~at:[At.class' "proj-card-summary"] [summary_html];
        tags_el];
      (* Recent items *)
      recent_items]
  in
  let cards = List.map project_card all_projects in
  let intro = El.p ~at:[At.class' "mb-6"] [
    El.txt "I work on a number of research projects and open source efforts, which you can find here. We often discuss these on our ";
    El.a ~at:[At.href "https://eeg.zulipchat.com"] [El.txt "EEG Zulip"];
    El.txt " which is open for registration, so feel free to sign up and get involved."]
  in
  let article = El.article [
    intro;
    El.div ~at:[At.class' "proj-grid"] cards]
  in
  article

(** Project for feeds. *)
let for_feed ~ctx proj =
  Common.truncated_body ~ctx (`Project proj)
