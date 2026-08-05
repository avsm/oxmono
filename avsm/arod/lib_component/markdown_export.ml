(*---------------------------------------------------------------------------
  Copyright (c) 2025 Anil Madhavapeddy <anil@recoil.org>. All rights reserved.
  SPDX-License-Identifier: ISC
 ---------------------------------------------------------------------------*)

(** Markdown export for content negotiation.

    Produces markdown representations of pages for AI agents and tools.
    Uses Bushel's original markdown content with links resolved to absolute
    URLs via {!Bushel.Md.to_markdown}. *)

module Entry = Bushel.Entry
module Paper = Bushel.Paper
module Contact = Sortal_schema.Contact
module Feed = Sortal_schema.Feed
module FeedEntry = Sortal_feed.Entry

(** {1 Helpers} *)

let date_str (y, m, d) =
  Printf.sprintf "%04d-%02d-%02d" y m d

let entry_url ~ctx ent =
  Arod.Ctx.base_url ctx ^ Entry.site_url ent

let render_body ~ctx body =
  let base_url = Arod.Ctx.base_url ctx in
  let entries = Arod.Ctx.entries ctx in
  Bushel.Md.to_markdown ~base_url ~image_base:"/images" ~entries body

let tags_line ~ctx ent =
  match Arod.Ctx.tags_of_ent ctx ent with
  | [] -> ""
  | tags ->
    let strs = List.map Bushel.Tags.to_string tags in
    "Tags: " ^ String.concat ", " strs ^ "\n"

let footer ~ctx ent =
  let url = entry_url ~ctx ent in
  let type_str = Entry.to_type_string ent in
  Printf.sprintf "\n---\nCanonical: %s\nType: %s\n%s"
    url type_str (tags_line ~ctx ent)

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
  let backlink_slugs = Bushel.Link_graph.get_backlinks_for_slug slug in
  let outbound_slugs = Bushel.Link_graph.get_outbound_for_slug slug in
  let feed_bls = Arod.Ctx.feed_backlinks_for_slug ctx slug in
  let feed_items = List.map (fun (bl : Arod.Ctx.feed_backlink) ->
    let name = Contact.name bl.contact in
    let title = match bl.feed_entry.FeedEntry.title with Some t -> t | None -> name in
    let url = match bl.feed_entry.FeedEntry.url with
      | Some u -> Uri.to_string u | None -> "" in
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
      if url <> "" then Printf.sprintf "- [%s](%s) (%s, %s)" title url typ d
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
  let header = Printf.sprintf "# %s\n\n*%s — %s*\n\n" title (date_str d) type_str in
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
  let footer = Printf.sprintf "\n---\nCanonical: %s%s\nFeeds: [Atom](%s/news.xml), [JSON](%s/feed.json)\n"
    base path base base
  in
  (Printf.sprintf "# %s\n\n%s\n\n" title description, footer)

let entry_bullet ~ctx ent =
  let title = Entry.title ent in
  let url = entry_url ~ctx ent in
  let d = date_str (Entry.date ent) in
  Printf.sprintf "- [%s](%s) (%s)" title url d

(** {1 List Pages} *)

let papers_list_md ~ctx =
  let papers = Arod.Ctx.papers ctx in
  let header, footer = list_header ~ctx ~title:"Papers" ~description:"Academic papers." ~path:"/papers" in
  let items = List.map (fun paper ->
    let ent = `Paper paper in
    let title = Paper.title paper in
    let url = entry_url ~ctx ent in
    let (y, _, _) = Entry.date ent in
    let authors = String.concat ", " (Paper.authors paper) in
    Printf.sprintf "- [%s](%s) (%d) — %s" title url y authors
  ) papers in
  header ^ String.concat "\n" items ^ "\n" ^ footer

let notes_list_md ~ctx =
  let notes = Arod.Ctx.notes ctx in
  let header, footer = list_header ~ctx ~title:"Notes" ~description:"Notes and blog posts." ~path:"/notes" in
  let items = List.map (fun note ->
    let bullet = entry_bullet ~ctx (`Note note) in
    match Bushel.Note.synopsis note with
    | Some syn when syn <> "" -> bullet ^ "\n  " ^ syn
    | _ -> bullet
  ) notes in
  header ^ String.concat "\n" items ^ "\n" ^ footer

let ideas_list_md ~ctx =
  let ideas = Arod.Ctx.ideas ctx in
  let entries = Arod.Ctx.entries ctx in
  let header, footer = list_header ~ctx ~title:"Research Ideas" ~description:"Research ideas." ~path:"/ideas" in
  let by_project : (string, Bushel.Idea.t list) Hashtbl.t = Hashtbl.create 16 in
  let order = ref [] in
  List.iter (fun idea ->
    let proj = Bushel.Idea.project idea in
    let cur = try Hashtbl.find by_project proj with Not_found -> [] in
    if cur = [] then order := proj :: !order;
    Hashtbl.replace by_project proj (cur @ [idea])
  ) ideas;
  let groups = List.rev !order in
  let sections = List.map (fun proj_slug ->
    let ideas = Hashtbl.find by_project proj_slug in
    let proj_title = match Entry.lookup entries proj_slug with
      | Some ent -> Entry.title ent
      | None -> if proj_slug <> "" then proj_slug else "Other"
    in
    let items = List.map (fun idea ->
      let bullet = entry_bullet ~ctx (`Idea idea) in
      let body = Bushel.Idea.body idea in
      if body <> "" then
        let first_line = match String.split_on_char '\n' body with
          | l :: _ -> String.trim l | [] -> "" in
        if first_line <> "" then bullet ^ "\n  " ^ first_line
        else bullet
      else bullet
    ) ideas in
    Printf.sprintf "### %s\n\n%s" proj_title (String.concat "\n" items)
  ) groups in
  header ^ String.concat "\n\n" sections ^ "\n" ^ footer

let projects_list_md ~ctx =
  let projects = Arod.Ctx.projects ctx in
  let header, footer = list_header ~ctx ~title:"Projects" ~description:"Research projects." ~path:"/projects" in
  let items = List.map (fun proj ->
    let bullet = entry_bullet ~ctx (`Project proj) in
    let body = Bushel.Project.body proj in
    if body <> "" then
      let first_line = match String.split_on_char '\n' body with
        | l :: _ -> String.trim l | [] -> "" in
      if first_line <> "" then bullet ^ "\n  " ^ first_line
      else bullet
    else bullet
  ) projects in
  header ^ String.concat "\n" items ^ "\n" ^ footer

let videos_list_md ~ctx =
  let videos = Arod.Ctx.videos ctx in
  let header, footer = list_header ~ctx ~title:"Talks" ~description:"Conference talks and presentations." ~path:"/videos" in
  let items = List.map (fun vid ->
    entry_bullet ~ctx (`Video vid)
  ) videos in
  header ^ String.concat "\n" items ^ "\n" ^ footer

let links_list_md ~ctx =
  let entries = Arod.Ctx.entries ctx in
  let all_links = Bushel.Link_graph.all_external_links () in
  let by_source : (string, Bushel.Link_graph.external_link list) Hashtbl.t =
    Hashtbl.create 128 in
  List.iter (fun (link : Bushel.Link_graph.external_link) ->
    let cur = try Hashtbl.find by_source link.source with Not_found -> [] in
    if List.exists (fun (l : Bushel.Link_graph.external_link) -> l.url = link.url) cur then ()
    else Hashtbl.replace by_source link.source (link :: cur)
  ) all_links;
  let groups = Hashtbl.fold (fun slug links acc ->
    match Entry.lookup entries slug with
    | Some ent -> (ent, links) :: acc
    | None -> acc
  ) by_source [] in
  let groups = List.sort (fun (a, _) (b, _) ->
    compare (Entry.date b) (Entry.date a)
  ) groups in
  let base = Arod.Ctx.base_url ctx in
  let header = "# Links\n\nOutbound links grouped by source entry.\n\n" in
  let sections = List.map (fun (ent, links) ->
    let title = Entry.title ent in
    let url = entry_url ~ctx ent in
    let link_lines = List.map (fun (link : Bushel.Link_graph.external_link) ->
      let label = match Arod.Ctx.link_for_url ctx link.url with
        | Some l ->
          let meta = match l.karakeep with Some k -> k.metadata | None -> [] in
          (match List.assoc_opt "title" meta with Some t -> t | None -> link.domain)
        | None -> link.domain
      in
      Printf.sprintf "  - [%s](%s)" label link.url
    ) links in
    Printf.sprintf "- **[%s](%s)**\n%s" title url (String.concat "\n" link_lines)
  ) groups in
  let footer = Printf.sprintf "\n---\nCanonical: %s/links\n" base in
  header ^ String.concat "\n" sections ^ "\n" ^ footer

let network_md ~ctx =
  let all_contacts = Arod.Ctx.contacts ctx in
  let entries = Arod.Ctx.entries ctx in
  let contacts_with_feeds = List.filter_map (fun contact ->
    match Contact.feeds contact with
    | Some feeds when feeds <> [] -> Some (contact, feeds)
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
  let forward_index = Network.build_forward_index () in
  let feed_items = Arod.Ctx.feed_items ctx in
  let feed_lines = List.map (fun (item : Arod.Ctx.feed_item) ->
    let fe = item.entry in
    let name = Contact.name item.contact in
    let title = match fe.FeedEntry.title with Some t -> t | None -> "(untitled)" in
    let url_str = match fe.FeedEntry.url with
      | Some u -> Uri.to_string u | None -> "" in
    let date_line = match fe.FeedEntry.date with
      | Some d ->
        let (y, m, d), _ = Ptime.to_date_time d in
        Printf.sprintf " (%04d-%02d-%02d)" y m d
      | None -> ""
    in
    let mention_strs = List.map (fun ent ->
      Printf.sprintf "[%s](%s%s)" (Entry.title ent) base (Entry.site_url ent)
    ) item.mentions in
    let forward_strs = match fe.FeedEntry.url with
      | Some u ->
        let key = Network.normalise_url (Uri.to_string u) in
        let slugs = try Hashtbl.find forward_index key with Not_found -> [] in
        List.filter_map (fun slug ->
          match Entry.lookup entries slug with
          | Some ent ->
            Some (Printf.sprintf "[%s](%s%s)" (Entry.title ent) base (Entry.site_url ent))
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
      Printf.sprintf "- **%s**: [%s](%s)%s%s" name title url_str date_line links_line
    else
      Printf.sprintf "- **%s**: %s%s%s" name title date_line links_line
  ) feed_items in
  let feed_section = "## Timeline\n\n" ^ String.concat "\n" feed_lines ^ "\n" in
  let footer = Printf.sprintf "\n---\nCanonical: %s/network\n" base in
  header ^ blogroll_section ^ feed_section ^ footer

let index_md ~ctx =
  match Arod.Ctx.lookup ctx "index" with
  | None -> ""
  | Some ent ->
    let title = Entry.title ent in
    let body = Entry.body ent in
    let base = Arod.Ctx.base_url ctx in
    let body_md = if body <> "" then render_body ~ctx body else "" in
    Printf.sprintf "# %s\n\n%s\n\n---\nCanonical: %s\n" title body_md base

let wiki_md ~ctx =
  let all = Arod.Ctx.all_entries ctx in
  let all = List.sort (fun a b -> compare (Entry.date b) (Entry.date a)) all in
  let base = Arod.Ctx.base_url ctx in
  let header = "# All Entries\n\n" in
  let items = List.map (fun ent ->
    let type_str = Entry.to_type_string ent in
    let title = Entry.title ent in
    let url = entry_url ~ctx ent in
    let d = date_str (Entry.date ent) in
    Printf.sprintf "- [%s](%s) (%s, %s)" title url type_str d
  ) all in
  let footer = Printf.sprintf "\n---\nCanonical: %s/wiki\n" base in
  header ^ String.concat "\n" items ^ "\n" ^ footer

let news_md ~ctx =
  let notes = Arod.Ctx.notes ctx in
  let base = Arod.Ctx.base_url ctx in
  let header = "# News\n\n" in
  let items = List.map (fun note ->
    entry_bullet ~ctx (`Note note)
  ) notes in
  let footer = Printf.sprintf "\n---\nCanonical: %s/news\n" base in
  header ^ String.concat "\n" items ^ "\n" ^ footer
