(*---------------------------------------------------------------------------
  Copyright (c) 2025 Anil Madhavapeddy <anil@recoil.org>. All rights reserved.
  SPDX-License-Identifier: ISC
 ---------------------------------------------------------------------------*)

(** Markdown export for content negotiation.

    Produces markdown representations of pages for AI agents and tools.
    Uses Bushel's original markdown content with links resolved to absolute
    URLs via {!Bushel.Md.to_markdown}. *)

module Paper_component = Paper
module Project_component = Project
module Idea_component = Idea
module Video_component = Video
module Note_component = Note
module Links_component = Links
module Entry = Bushel.Entry
module Paper = Bushel.Paper
module Contact = Sortal_schema.Contact
module Feed = Sortal_schema.Feed
module FeedEntry = Sortal_feed.Entry

(** {1 Helpers} *)

let date_str (y, m, d) =
  Printf.sprintf "%04d-%02d-%02d" y m d

(** [one_line s] is [s] with each run of white space made one space. A title
    with a line break would otherwise end a markdown heading or link early. *)
let one_line s =
  String.map (function '\n' | '\t' | '\r' -> ' ' | c -> c) s
  |> String.split_on_char ' '
  |> List.filter (fun w -> w <> "")
  |> String.concat " "

(** [link_text s] is [s] as the text of a markdown link: on one line, with its
    square brackets escaped. *)
let link_text s =
  let b = Buffer.create (String.length s) in
  String.iter (function
    | ('[' | ']') as c -> Buffer.add_char b '\\'; Buffer.add_char b c
    | c -> Buffer.add_char b c) (one_line s);
  Buffer.contents b

let entry_url ~ctx ent =
  Arod.Ctx.base_url ctx ^ Entry.site_url ent

let render_body ~ctx body =
  let base_url = Arod.Ctx.base_url ctx in
  let entries = Arod.Ctx.entries ctx in
  Bushel.Md.to_markdown ~base_url ~image_base:"/images" ~entries body

let tags_line ~ctx ent =
  match Bushel.Entry.tags_of_ent ent with
  | [] -> ""
  | tags ->
    let strs = List.map Bushel.Tags.to_string tags in
    "Tags: " ^ String.concat ", " strs ^ "\n"

let license_line =
  "License: CC BY 4.0 <https://creativecommons.org/licenses/by/4.0/>\n"

let footer ~ctx ent =
  let url = entry_url ~ctx ent in
  let type_str = Entry.to_type_string ent in
  Printf.sprintf "\n---\nCanonical: %s\nType: %s\n%s%s"
    url type_str license_line (tags_line ~ctx ent)

let social_links (s : Bushel.Types.social) =
  let add label urls acc =
    List.fold_left (fun a u -> Printf.sprintf "- %s: <%s>" label u :: a) acc urls
  in
  let lines = [] in
  let lines = add "Bluesky" s.bluesky lines in
  let lines = add "Hacker News" s.hn lines in
  let lines = add "Instagram" s.instagram lines in
  let lines = add "LinkedIn" s.linkedin lines in
  let lines = add "Lobsters" s.lobsters lines in
  let lines = add "Mastodon" s.mastodon lines in
  let lines = add "Twitter" s.twitter lines in
  let lines = List.rev lines in
  match lines with
  | [] -> ""
  | _ -> "\nDiscussion:\n" ^ String.concat "\n" lines ^ "\n"

let resolve_slug ~ctx slug =
  let entries = Arod.Ctx.entries ctx in
  match Entry.lookup entries slug with
  | Some ent -> Some (Entry.title ent, entry_url ~ctx ent, Entry.to_type_string ent, Entry.date ent)
  | None -> None

let related_entries ~ctx ent =
  let slug = Entry.slug ent in
  let backlink_slugs = Arod.Ctx.backlinks ctx slug in
  let outbound_slugs = Arod.Ctx.outbound ctx slug in
  let feed_bls = Arod.Ctx.feed_backlinks_for_slug ctx slug in
  let feed_items = List.map (fun (bl : Arod.Ctx.feed_backlink) ->
    let name = Contact.name bl.contact in
    let title = match bl.feed_entry.FeedEntry.title with Some t -> t | None -> name in
    let url = match bl.feed_entry.FeedEntry.url with
      | Some u -> Uriz.to_string u | None -> "" in
    let date_str_s = match bl.feed_entry.FeedEntry.date with
      | Some d -> let (y, m, dd), _ = Ptime.to_date_time d in
        Printf.sprintf "%04d-%02d-%02d" y m dd
      | None -> "" in
    (title, url, "feed", date_str_s)
  ) feed_bls in
  let seen = Hashtbl.create 32 in
  let all_slugs = backlink_slugs @ outbound_slugs in
  let resolved = List.filter_map (fun s ->
    if Hashtbl.mem seen s then None
    else begin
      Hashtbl.replace seen s ();
      match resolve_slug ~ctx s with
      | Some (title, url, typ, date) ->
        Some (title, url, typ, date_str date)
      | None -> None
    end
  ) all_slugs in
  let all_items = resolved @ feed_items in
  let all_items = List.sort (fun (_, _, _, d1) (_, _, _, d2) ->
    String.compare d2 d1
  ) all_items in
  match all_items with
  | [] -> ""
  | items ->
    let lines = List.map (fun (title, url, typ, d) ->
      if url <> "" then
        Printf.sprintf "- [%s](%s) (%s, %s)" (link_text title) url typ d
      else Printf.sprintf "- %s (%s, %s)" title typ d
    ) items in
    "\n## Related\n\n" ^ String.concat "\n" lines ^ "\n"

let infobox_md ~ctx ent =
  let buf = Buffer.create 256 in
  let add s = Buffer.add_string buf s in
  let add_opt label = function
    | Some v when v <> "" -> add (Printf.sprintf "%s: %s\n" label v)
    | _ -> () in
  let add_social_opt = function
    | Some s -> add (social_links s)
    | None -> () in
  let entries = Arod.Ctx.entries ctx in
  let resolve_to_title slug =
    match Entry.lookup entries slug with
    | Some e -> Entry.title e
    | None -> slug in
  (match ent with
  | `Note n ->
    add_opt "Synopsis" (Bushel.Note.synopsis n);
    let wc = Bushel.Note.words n in
    if wc > 0 then add (Printf.sprintf "Words: %d\n" wc);
    add_opt "Category" (Bushel.Note.category n);
    add_opt "DOI" (Bushel.Note.doi n);
    add_social_opt (Bushel.Note.social n)
  | `Paper paper ->
    let cls = Bushel.Paper.classification paper in
    add (Printf.sprintf "Classification: %s\n" (Bushel.Paper.string_of_classification cls));
    let venue = Common.venue_of_paper paper in
    if venue <> "" then add (Printf.sprintf "Venue: %s\n" venue);
    add_opt "Volume" (Bushel.Paper.volume paper);
    add_opt "Issue" (Bushel.Paper.number paper);
    add_opt "URL" (Bushel.Paper.url paper);
    let proj_slugs = Bushel.Paper.project_slugs paper in
    if proj_slugs <> [] then begin
      let names = List.map resolve_to_title proj_slugs in
      add (Printf.sprintf "Projects: %s\n" (String.concat ", " names))
    end;
    add_social_opt (Bushel.Paper.social paper)
  | `Idea idea ->
    add (Printf.sprintf "Status: %s\n" (Bushel.Idea.status_to_string (Bushel.Idea.status idea)));
    add (Printf.sprintf "Level: %s\n" (Bushel.Idea.level_to_string (Bushel.Idea.level idea)));
    add (Printf.sprintf "Year: %d\n" (Bushel.Idea.year idea));
    let proj = Bushel.Idea.project idea in
    if proj <> "" then add (Printf.sprintf "Project: %s\n" (resolve_to_title proj));
    let sups = Bushel.Idea.supervisors idea in
    if sups <> [] then
      add (Printf.sprintf "Supervisors: %s\n"
        (String.concat ", " (List.map Contact.name sups)));
    let studs = Bushel.Idea.students idea in
    if studs <> [] then
      add (Printf.sprintf "Students: %s\n"
        (String.concat ", " (List.map Contact.name studs)));
    add_opt "URL" (Bushel.Idea.url idea);
    add_social_opt (Bushel.Idea.social idea)
  | `Project proj ->
    let range = match Bushel.Project.finish proj with
      | Some y -> Printf.sprintf "%d–%d" (Bushel.Project.start proj) y
      | None -> Printf.sprintf "%d–present" (Bushel.Project.start proj)
    in
    add (Printf.sprintf "Period: %s\n" range);
    add_social_opt (Bushel.Project.social proj)
  | `Video v ->
    add (Printf.sprintf "Type: %s\n" (if Bushel.Video.talk v then "Talk" else "Video"));
    let url = Bushel.Video.url v in
    if url <> "" then add (Printf.sprintf "URL: %s\n" url);
    (match Bushel.Video.project v with
    | Some slug -> add (Printf.sprintf "Project: %s\n" (resolve_to_title slug))
    | None -> ());
    (match Bushel.Video.paper v with
    | Some slug -> add (Printf.sprintf "Paper: %s\n" (resolve_to_title slug))
    | None -> ());
    add_social_opt (Bushel.Video.social v));
  Buffer.contents buf

(** {1 Entry to Markdown} *)

let entry_to_markdown ~ctx ent =
  let title = Entry.title ent in
  let d = Entry.date ent in
  let type_str = Entry.to_type_string ent in
  let header =
    Printf.sprintf "# %s\n\n*%s — %s*\n\n" (one_line title) (date_str d) type_str
  in
  let body_md = match ent with
    | `Paper p ->
      let abs = Paper.abstract p in
      let authors = String.concat ", " (Paper.authors p) in
      let doi_line = match Paper.doi p with
        | Some doi -> Printf.sprintf "DOI: %s\n" doi
        | None -> ""
      in
      Printf.sprintf "Authors: %s\n\n%s%s" authors
        (if abs <> "" then render_body ~ctx abs ^ "\n\n" else "")
        doi_line
    | `Video v ->
      let desc = Bushel.Video.description v in
      if desc <> "" then render_body ~ctx desc else ""
    | _ ->
      let body = Entry.body ent in
      if body <> "" then render_body ~ctx body else ""
  in
  let infobox = infobox_md ~ctx ent in
  let related = related_entries ~ctx ent in
  header ^ body_md ^ infobox ^ related ^ footer ~ctx ent

(** {1 List Page Helpers} *)

let list_header ~ctx ~title ~description ~path =
  let base = Arod.Ctx.base_url ctx in
  let footer = Printf.sprintf "\n---\nCanonical: %s%s\nFeeds: [Atom](%s/news.xml), [JSON](%s/feed.json)\n%s"
    base path base base license_line
  in
  (Printf.sprintf "# %s\n\n%s\n\n" title description, footer)

(* [entry_bullet_link ~ctx ent] is a link to [ent] in markdown. *)
let entry_bullet_link ~ctx ent =
  Printf.sprintf "- [%s](%s)" (link_text (Entry.title ent))
    (entry_url ~ctx ent)

let entry_bullet ~ctx ent =
  let title = Entry.title ent in
  let url = entry_url ~ctx ent in
  let d = date_str (Entry.date ent) in
  Printf.sprintf "- [%s](%s) (%s)" (link_text title) url d

(** {1 List Pages} *)

(* [paper_md ~ctx paper] mirrors the compact HTML card: title, month,
   classification, authors, publisher and the resource links. *)
let paper_md ~ctx paper =
  let ent = `Paper paper in
  let title = Paper.title paper in
  let url = entry_url ~ctx ent in
  let (y, m, _) = Entry.date ent in
  let cls = Paper.string_of_classification (Paper.classification paper) in
  let rec join = function
    | [] -> ""
    | [ a ] -> a
    | [ a; b ] -> a ^ " and " ^ b
    | a :: rest -> a ^ ", " ^ join rest
  in
  let authors = match Paper.authors paper with
    | [] -> ""
    | l -> join l ^ ". "
  in
  let link l u = Printf.sprintf "[%s](%s)" l u in
  let publisher = Paper_component.publisher_with ~link paper in
  let base = Arod.Ctx.base_url ctx in
  let slug = Paper.slug paper in
  let doi = Option.map (fun d -> link "DOI" ("https://doi.org/" ^ d))
      (Paper.doi paper) in
  let bib = Some (link "BIB" (Printf.sprintf "%s/papers/%s.bib" base slug)) in
  let pdf = Option.map (fun _ ->
      link "PDF" (Printf.sprintf "%s/papers/%s.pdf" base slug))
      (Paper_component.pdf_path ~ctx paper) in
  let ext = Option.map (fun u ->
      let host = Paper_component.host_without_www u in
      Printf.sprintf "%s (%s)" (link "URL" u) host)
      (Paper.url paper) in
  let links = List.filter_map Fun.id [ doi; bib; pdf; ext ] in
  Printf.sprintf "- [%s](%s) (%s %d, %s)\n  %s%s.\n  %s"
    title url (Common.month_name m) y cls authors publisher
    (String.concat ", " links)

let papers_list_md ~ctx =
  let papers = List.sort Bushel.Paper.compare (Arod.Ctx.papers ctx) in
  let count c =
    List.length
      (List.filter (fun p -> Bushel.Paper.classification p = c) papers)
  in
  let description =
    Printf.sprintf
      "Academic papers, most recent first. %d papers: %d full, %d short, \
       %d preprint."
      (List.length papers) (count Bushel.Paper.Full) (count Short)
      (count Preprint)
  in
  let header, footer =
    list_header ~ctx ~title:"Papers" ~description ~path:"/papers"
  in
  let by_year = List.fold_left (fun acc paper ->
    let y = Bushel.Paper.year paper in
    match acc with
    | (y', ps) :: rest when y' = y -> (y, paper :: ps) :: rest
    | _ -> (y, [ paper ]) :: acc
  ) [] papers |> List.rev in
  let sections = List.map (fun (y, ps) ->
    let items = List.map (paper_md ~ctx) (List.rev ps) in
    Printf.sprintf "## %d\n\n%s" y (String.concat "\n" items)
  ) by_year in
  header ^ String.concat "\n\n" sections ^ "\n" ^ footer

(* [notes_list_md ~ctx] mirrors the HTML timeline. It reads the same months and
   rows, so a month holds its notes, weeknotes and code releases newest first
   and the weeks with nothing in them, and the featured notes of the sidebar
   follow. *)
let notes_list_md ~ctx =
  let months = Note_component.timeline ~ctx in
  let popularity = Note_component.tag_popularity ctx in
  let header, footer =
    list_header ~ctx ~title:"Notes" ~description:"Notes and blog posts."
      ~path:"/notes"
  in
  let words n =
    match Bushel.Note.words n with
    | 0 -> ""
    | w ->
      Printf.sprintf ", %s word%s" (Note_component.format_number w)
        (if w = 1 then "" else "s")
  in
  let synopsis n =
    match Bushel.Note.synopsis n with
    | Some text when text <> "" -> "\n  " ^ text
    | _ -> ""
  in
  let tags n =
    match Note_component.ranked_tags ~popularity n with
    | [] -> ""
    | tags -> "\n  Tags: " ^ String.concat ", " (List.map fst tags)
  in
  let links n =
    match Note_component.heading_links n with
    | [] -> ""
    | links ->
      "\n  Links: "
      ^ String.concat ", "
          (List.map (fun (label, url) -> Printf.sprintf "[%s](%s)" label url)
             links)
  in
  let row = function
    | Note_component.Journal n ->
      Printf.sprintf "%s (%s%s)%s%s%s"
        (entry_bullet_link ~ctx (`Note n))
        (date_str (Entry.date (`Note n))) (words n) (synopsis n) (tags n)
        (links n)
    | Note_component.Weeknote n ->
      let ((y, _, _) as date) = Entry.date (`Note n) in
      let _, week = Bushel.Note.week_number n in
      Printf.sprintf "- [%s](%s) (Week %d, %s %d%s)%s%s%s"
        (link_text
           (Note_component.strip_weeknote_prefix (Entry.title (`Note n))))
        (entry_url ~ctx (`Note n)) week (Note_component.week_range date) y
        (words n) (synopsis n) (tags n) (links n)
    | Note_component.Releases (t, rs) ->
      let r = List.hd rs in
      let earlier = List.tl rs in
      let registries =
        match r.Bushel.Release.registries with
        | [] -> ""
        | regs ->
          "\n  Registries: "
          ^ String.concat ", "
              (List.map (fun reg ->
                 Printf.sprintf "[%s](%s)" reg.Bushel.Release.name
                   (Bushel.Release.metadata_url reg r)) regs)
      in
      Printf.sprintf "- [%s %s](%s) (%s, code release)\n  %s%s%s"
        (Note_component.release_name t) r.Bushel.Release.version
        r.Bushel.Release.url (date_str r.Bushel.Release.date)
        r.Bushel.Release.summary
        (match earlier with
         | [] -> ""
         | _ ->
           Printf.sprintf "\n  %d earlier: %s" (List.length earlier)
             (String.concat ", "
                (List.rev_map (fun (e : Bushel.Release.release) -> e.version)
                   earlier)))
        registries
    | Note_component.Quiet n ->
      Printf.sprintf "- *%s*"
        (if n = 1 then "1 quiet week" else Printf.sprintf "%d quiet weeks" n)
  in
  let sections =
    List.map (fun (year, month, rows) ->
      Printf.sprintf "## %s %d\n\n%s" (Common.month_name_full month) year
        (String.concat "\n" (List.map row rows))) months
  in
  let featured =
    let journal =
      List.filter (fun n -> not (Bushel.Note.weeknote n)) (Arod.Ctx.notes ctx)
    in
    match Note_component.featured_notes journal with
    | [] -> ""
    | notes ->
      "\n\n## Featured\n\n"
      ^ String.concat "\n"
          (List.map (fun n ->
             let doi =
               match Bushel.Note.doi n with
               | Some d -> Printf.sprintf ", [DOI](https://doi.org/%s)" d
               | None -> ""
             in
             Printf.sprintf "%s (%s%s)" (entry_bullet_link ~ctx (`Note n))
               (date_str (Entry.date (`Note n))) doi) notes)
  in
  header ^ String.concat "\n\n" sections ^ featured ^ "\n" ^ footer

(* [join_and items] is [items] as the HTML sentences give them: "a", "a and b",
   "a, b and c". *)
let rec join_and = function
  | [] -> ""
  | [ a ] -> a
  | [ a; b ] -> a ^ " and " ^ b
  | a :: rest -> a ^ ", " ^ join_and rest

(* [parts_md ~ctx parts] is a sentence of an idea card, with the people it names
   linked as the HTML links them. *)
let parts_md ~ctx parts =
  let who handle =
    match Arod.Ctx.lookup_by_handle ctx handle with
    | Some contact ->
      let name = Contact.name contact in
      (match Contact.best_url contact with
       | Some url -> Printf.sprintf "[%s](%s)" name url
       | None -> name)
    | None -> "@" ^ handle
  in
  String.concat "" (List.map (function
    | Idea_component.Say text -> text
    | Idea_component.Who handles -> join_and (List.map who handles)) parts)

(* [ideas_list_md ~ctx] mirrors the HTML index: its introduction, the counts of
   the status and level filters, the contents by project, and then each project
   in the order of the page with its live ideas, open ones first, and after
   them its past ideas. *)
let ideas_list_md ~ctx =
  let ideas = Arod.Ctx.ideas ctx in
  let groups = Idea_component.list_groups ~ctx in
  let before, emphasis, after = Idea_component.list_intro_second in
  let intro =
    Printf.sprintf "%s\n\n%s*%s*%s" Idea_component.list_intro_first before
      emphasis after
  in
  let header, footer =
    list_header ~ctx ~title:"Research Ideas" ~description:intro ~path:"/ideas"
  in
  let facets =
    let status =
      String.concat ", "
        (List.map (fun (s, n) ->
           Printf.sprintf "%d %s" n (Idea_component.status_label s))
           (Idea_component.status_counts ideas))
    in
    let level =
      String.concat ", "
        (List.map (fun (l, n) ->
           Printf.sprintf "%d %s" n (Idea_component.level_label l))
           (Idea_component.level_counts ideas))
    in
    Printf.sprintf "Status: %s\nLevel: %s\n\n" status level
  in
  let slots = Idea_component.statuses_present ideas in
  let contents =
    "## By project\n\n"
    ^ String.concat "\n"
        (List.map (fun (proj, _, _, is) ->
           Printf.sprintf "- %s: %d (%s)" proj.Bushel.Project.title
             (List.length is) (Idea_component.spoken_counts ~slots is)) groups)
    ^ "\n\n"
  in
  let live_item idea =
    let parts, discuss = Idea_component.card_meta_parts idea in
    let meta =
      parts_md ~ctx parts ^ (if discuss then " " ^ Idea_component.discussion_note else "")
    in
    let summary =
      match Idea_component.summary_text ~ctx ~max_len:240 idea with
      | Some text -> "\n  " ^ text
      | None -> ""
    in
    Printf.sprintf "%s (%s)\n  %s%s" (entry_bullet_link ~ctx (`Idea idea))
      (Idea_component.status_label (Bushel.Idea.status idea)) meta summary
  in
  let past_item idea =
    Printf.sprintf "%s (%s)\n  %s" (entry_bullet_link ~ctx (`Idea idea))
      (Idea_component.status_label (Bushel.Idea.status idea))
      (parts_md ~ctx (Idea_component.past_line_parts idea))
  in
  let section (proj, live, past, _) =
    let open_n = List.length (List.filter Idea_component.is_open live) in
    let going_n = List.length live - open_n in
    let counts =
      List.filter_map Fun.id
        [ (if open_n = 0 then None else Some (Printf.sprintf "%d open" open_n));
          (if going_n = 0 then None
           else Some (Printf.sprintf "%d under way" going_n));
          (if past = [] then None
           else Some (Printf.sprintf "%d previous" (List.length past))) ]
    in
    let note =
      match String.trim (Bushel.Project.ideas proj) with
      | "" -> ""
      | t -> t ^ "\n\n"
    in
    Printf.sprintf "## [%s](%s)\n\n%s\n\n%s%s"
      (link_text proj.Bushel.Project.title)
      (entry_url ~ctx (`Project proj))
      (String.concat ", " counts) note
      (String.concat "\n" (List.map live_item live @ List.map past_item past))
  in
  header ^ facets ^ contents
  ^ String.concat "\n\n" (List.map section groups)
  ^ "\n" ^ footer

(* [indented text] is [text] with each of its lines indented to sit inside a
   list item. A blank line stays empty. *)
let indented text =
  String.split_on_char '\n' (String.trim text)
  |> List.map (fun l -> if l = "" then l else "  " ^ l)
  |> String.concat "\n"

(* [projects_list_md ~ctx] mirrors the HTML list: the same introduction, the
   projects in the order the cards use, and for each the years, the opening of
   its body, its tags and its recent papers, notes and ideas. *)
let projects_list_md ~ctx =
  let projects = List.sort Bushel.Project.compare (Arod.Ctx.projects ctx) in
  let intro =
    let label, url = Project_component.list_intro_link in
    Printf.sprintf "%s[%s](%s)%s" Project_component.list_intro_before label url
      Project_component.list_intro_after
  in
  let header, footer =
    list_header ~ctx ~title:"Projects" ~description:intro ~path:"/projects"
  in
  let item proj =
    let bullet =
      Printf.sprintf "%s (%s)" (entry_bullet_link ~ctx (`Project proj))
        (Project_component.list_date_range proj)
    in
    let summary =
      match String.trim (Project_component.list_summary proj) with
      | "" -> ""
      | text -> "\n" ^ indented (render_body ~ctx text)
    in
    let tags =
      match Bushel.Project.tags proj with
      | [] -> ""
      | tags -> "\n  Tags: " ^ String.concat ", " tags
    in
    let recent =
      match Project_component.list_recent ~ctx proj with
      | [] -> ""
      | recent ->
        "\n  Recent:\n"
        ^ String.concat "\n"
            (List.map (fun (_, ent) ->
               Printf.sprintf "  - [%s](%s) (%s, %s)" (link_text (Entry.title ent))
                 (entry_url ~ctx ent) (Entry.to_type_string ent)
                 (date_str (Entry.date ent))) recent)
    in
    bullet ^ summary ^ tags ^ recent
  in
  header ^ String.concat "\n" (List.map item projects) ^ "\n" ^ footer

(* [videos_list_md ~ctx] mirrors the HTML talks page: the talks, newest first,
   and for each its month, the video, the opening of its description, its tags
   and the references that the card shows under it. *)
let videos_list_md ~ctx =
  let talks = Video_component.list_talks ~ctx in
  let header, footer =
    list_header ~ctx ~title:"Talks"
      ~description:"Conference talks and presentations." ~path:"/videos"
  in
  let item vid =
    let (y, m, _) = Bushel.Video.date vid in
    let bullet =
      Printf.sprintf "%s (%s %d)" (entry_bullet_link ~ctx (`Video vid))
        (Common.month_name m) y
    in
    let watch =
      match Bushel.Video.url vid with
      | "" -> ""
      | url -> Printf.sprintf "\n  Watch: <%s>" url
    in
    let desc =
      match String.trim (Video_component.card_desc vid) with
      | "" -> ""
      | text -> "\n" ^ indented (render_body ~ctx text)
    in
    let tags =
      match Bushel.Video.tags vid with
      | [] -> ""
      | tags -> "\n  Tags: " ^ String.concat ", " tags
    in
    let refs =
      match Video_component.card_refs ~ctx vid with
      | [] -> ""
      | refs ->
        "\n  References:\n"
        ^ String.concat "\n"
            (List.map (fun (r : Video_component.reference) ->
               Printf.sprintf "  - [%s](%s%s) (%s)" (link_text r.title)
                 (Arod.Ctx.base_url ctx) r.href r.kind) refs)
    in
    bullet ^ watch ^ desc ^ tags ^ refs
  in
  header ^ String.concat "\n" (List.map item talks) ^ "\n" ^ footer

(* [links_list_md ~ctx] mirrors the HTML links page: its introduction, the
   counts of its sidebar, and the groups of links under the entries that make
   them, newest entry first, each link described as the page describes it. The
   HTML loads the groups a page at a time, and this lists them all. *)
let links_list_md ~ctx =
  let groups = Links_component.compute_groups ~ctx in
  let stats = Links_component.stats ~ctx groups in
  let describe = Links_component.displayer ~ctx in
  let base = Arod.Ctx.base_url ctx in
  let intro =
    let label, url = Links_component.list_intro_link in
    Printf.sprintf "%s[%s](%s)%s" Links_component.list_intro_before label url
      Links_component.list_intro_after
  in
  let filters =
    String.concat ", "
      (List.filter_map (fun (kind, label) ->
         match Hashtbl.find_opt stats.Links_component.filter_counts kind with
         | Some n when n > 0 -> Some (Printf.sprintf "%s %d" label n)
         | _ -> None) Links_component.filter_categories)
  in
  let top_domains =
    List.filteri (fun i _ -> i < 50) stats.Links_component.domain_counts
    |> List.map (fun (d, n) -> Printf.sprintf "- %s: %d" d n)
    |> String.concat "\n"
  in
  let summary =
    Printf.sprintf "%d links, %d domains.\n\nFilter: %s\n\n## Top domains\n\n%s\n\n"
      stats.Links_component.total_urls stats.Links_component.total_domains
      filters top_domains
  in
  let header = Printf.sprintf "# Links\n\n%s\n\n%s" intro summary in
  let sections =
    List.map (fun (g : Links_component.link_group) ->
      let (y, m, _) = Entry.date g.ent in
      let lines =
        List.map (fun (link : Bushel.Link_graph.external_link) ->
          let d = describe link.url in
          let label =
            match d.Links_component.secondary with
            | Some sec -> d.Links_component.label ^ " " ^ sec
            | None -> d.Links_component.label
          in
          let hint =
            if Links_component.show_domain_hint d then
              " (" ^ link.domain ^ ")"
            else ""
          in
          Printf.sprintf "  - [%s](%s)%s" (link_text label) link.url hint)
          g.links
      in
      Printf.sprintf "- **[%s](%s)** (%s, %s %d)\n%s" (link_text (Entry.title g.ent))
        (entry_url ~ctx g.ent) (Entry.to_type_string g.ent)
        (Common.month_name m) y (String.concat "\n" lines)) groups
  in
  let footer = Printf.sprintf "\n---\nCanonical: %s/links\n%s" base license_line in
  header ^ "## Links by entry\n\n" ^ String.concat "\n" sections ^ "\n" ^ footer

let network_md ~ctx =
  let all_contacts = Arod.Ctx.contacts ctx in
  let entries = Arod.Ctx.entries ctx in
  let contacts_with_feeds = List.filter_map (fun contact ->
    match Contact.feeds contact with
    | feeds when feeds <> [] -> Some (contact, feeds)
    | _ -> None
  ) all_contacts in
  let contacts_with_feeds = List.sort (fun (a, _) (b, _) ->
    String.compare (Contact.name a) (Contact.name b)
  ) contacts_with_feeds in
  let base = Arod.Ctx.base_url ctx in
  let header = "# Network\n\nUnified timeline of activity and contact feeds.\n\n" in
  (* Blogroll *)
  let blogroll_items = List.map (fun (contact, feeds) ->
    let name = Contact.name contact in
    let feed_links = List.map (fun feed ->
      let ft = match Feed.feed_type feed with
        | Feed.Atom -> "Atom" | Feed.Rss -> "RSS" | Feed.Json -> "JSON"
        | Feed.Manual -> "Manual"
      in
      Printf.sprintf "[%s](%s)" ft (Feed.url feed)
    ) feeds in
    Printf.sprintf "- **%s**: %s" name (String.concat ", " feed_links)
  ) contacts_with_feeds in
  let blogroll_section = "## Blogroll\n\n" ^ String.concat "\n" blogroll_items ^ "\n\n" in
  (* Feed timeline *)
  let feed_items = Arod.Ctx.feed_items ctx in
  let feed_lines = List.map (fun (item : Arod.Ctx.feed_item) ->
    let fe = item.entry in
    let name = Contact.name item.contact in
    let title = match fe.FeedEntry.title with Some t -> t | None -> "(untitled)" in
    let url_str = match fe.FeedEntry.url with
      | Some u -> Uriz.to_string u | None -> "" in
    let date_line = match fe.FeedEntry.date with
      | Some d ->
        let (y, m, d), _ = Ptime.to_date_time d in
        Printf.sprintf " (%04d-%02d-%02d)" y m d
      | None -> ""
    in
    let mention_strs = List.map (fun ent ->
      Printf.sprintf "[%s](%s%s)" (link_text (Entry.title ent)) base
        (Entry.site_url ent)
    ) item.mentions in
    let forward_strs = match fe.FeedEntry.url with
      | Some u ->
        let slugs = Arod.Ctx.forward_slugs ctx (Uriz.to_string u) in
        List.filter_map (fun slug ->
          match Entry.lookup entries slug with
          | Some ent ->
            Some (Printf.sprintf "[%s](%s%s)" (link_text (Entry.title ent)) base
        (Entry.site_url ent))
          | None -> None
        ) slugs
      | None -> []
    in
    let links_line =
      let all_refs = mention_strs @ forward_strs in
      match all_refs with
      | [] -> ""
      | refs -> "\n  Linked: " ^ String.concat ", " refs
    in
    if url_str <> "" then
      Printf.sprintf "- **%s**: [%s](%s)%s%s" name (link_text title) url_str
        date_line links_line
    else
      Printf.sprintf "- **%s**: %s%s%s" name title date_line links_line
  ) feed_items in
  let feed_section = "## Timeline\n\n" ^ String.concat "\n" feed_lines ^ "\n" in
  let footer = Printf.sprintf "\n---\nCanonical: %s/network\n%s" base license_line in
  header ^ blogroll_section ^ feed_section ^ footer

let index_md ~ctx =
  match Arod.Ctx.lookup ctx "index" with
  | None -> ""
  | Some ent ->
    let title = Entry.title ent in
    let body = Entry.body ent in
    let base = Arod.Ctx.base_url ctx in
    let body_md = if body <> "" then render_body ~ctx body else "" in
    Printf.sprintf "# %s\n\n%s\n\n---\nCanonical: %s\n%s" title body_md base
      license_line
