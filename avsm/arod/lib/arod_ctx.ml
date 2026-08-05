(*---------------------------------------------------------------------------
  Copyright (c) 2025 Anil Madhavapeddy <anil@recoil.org>. All rights reserved.
  SPDX-License-Identifier: ISC
 ---------------------------------------------------------------------------*)

let src = Logs.Src.create "arod.ctx" ~doc:"Arod context"
module Log = (val Logs.src_log src : Logs.LOG)

(** Context record for Arod - replaces global state *)

type feed_item = {
  contact : Sortal_schema.Contact.t;
  entry : Sortal_feed.Entry.t;
  mentions : Bushel.Entry.entry list;
}

(** A record of a feed entry that mentions a bushel entry. *)
type feed_backlink = {
  contact : Sortal_schema.Contact.t;
  feed_entry : Sortal_feed.Entry.t;
}

type t = {
  config : Arod_config.t;
  entries : Bushel.Entry.t;
  feed_items : feed_item list;
  feed_backlinks : (string, feed_backlink list) Hashtbl.t;
  feed_by_url : (string, feed_backlink list) Hashtbl.t;
  links_by_url : (string, Bushel.Link.t) Hashtbl.t;
}

(** Normalise a URL for matching: strip www. prefix from host, remove trailing slash. *)
let normalise_url url =
  match Uri.of_string url |> Uri.host with
  | Some host ->
    let host' =
      if String.length host > 4 && String.sub host 0 4 = "www."
      then String.sub host 4 (String.length host - 4) else host
    in
    let u = Uri.of_string url in
    let u = Uri.with_host u (Some host') in
    let path = Uri.path u in
    let path = if String.length path > 1 && path.[String.length path - 1] = '/'
      then String.sub path 0 (String.length path - 1) else path in
    Uri.to_string (Uri.with_path u path)
  | None -> url

(** {1 Feed Link Scanning}

    Extract URLs from HTML content/summary of feed entries and resolve
    them to bushel slugs by stripping the site base_url and parsing
    the /type/slug path pattern.

    {2 Bushel Migration Notes}

    This feed-link scanning is prototyped here in Arod but belongs in Bushel:

    - [extract_urls_from_html] and [resolve_url_to_entry] are generic and
      should move to a new [Bushel_feed_links] module (or extend
      [Bushel_link_graph]).

    - The URL-to-slug reverse mapping ([resolve_url_to_entry]) needs the
      site base_url and knows about the /type/slug URL scheme from
      [Bushel_entry.site_url]. It should live next to [site_url] in
      [Bushel_entry] as [entry_of_site_url ~base_url ~entries url].

    - [scan_feed_entry_mentions] belongs in a [Bushel_feed_links] module
      that takes a [Bushel.Entry.t] and a list of feed entries, returning
      [(feed_entry * Bushel.Entry.entry list) list] plus a reverse map
      [(string * feed_backlink list) Hashtbl.t].

    - The link graph ([Bushel_link_graph]) could gain a new [feed_backlinks]
      hashtable (slug -> feed_backlink list) alongside the existing
      [backlinks] (slug -> StringSet of source slugs). A new
      [add_feed_backlink ~target_slug ~source_feed_url ~source_title ~contact]
      would register these. The existing [get_backlinks_for_slug] would not
      change; a separate [get_feed_backlinks_for_slug] would query the new
      table.

    - Feed entry scanning could run during [Bushel_loader.load] if feed
      data is available, or as a post-load step called by the application
      (like Arod does now). *)

(** Extract all href="..." URLs from an HTML string. *)
let extract_urls_from_html html =
  let urls = ref [] in
  let len = String.length html in
  let rec scan i =
    if i >= len - 6 then ()
    else
      (* Look for href=QUOTE patterns *)
      if i + 5 < len
         && html.[i] = 'h' && html.[i+1] = 'r' && html.[i+2] = 'e'
         && html.[i+3] = 'f' && html.[i+4] = '=' then begin
        let q = html.[i+5] in
        if q = '"' || q = '\'' then begin
          let start = i + 6 in
          let rec find_end j =
            if j >= len then j
            else if html.[j] = q then j
            else find_end (j + 1)
          in
          let stop = find_end start in
          if stop > start then
            urls := String.sub html start (stop - start) :: !urls;
          scan (stop + 1)
        end else
          scan (i + 1)
      end else
        scan (i + 1)
  in
  scan 0;
  !urls

(** Try to resolve a URL to a bushel slug.
    Strips base_url prefix, then parses /type/slug from the remaining path.
    Returns Some entry if the slug exists, None otherwise. *)
let resolve_url_to_entry ~base_url ~entries url =
  let path =
    if String.length url > String.length base_url
       && String.sub url 0 (String.length base_url) = base_url then
      String.sub url (String.length base_url)
        (String.length url - String.length base_url)
    else if String.length url > 0 && url.[0] = '/' then
      url
    else
      ""
  in
  (* Parse /type/slug — valid types are papers, notes, projects, ideas, videos *)
  match String.split_on_char '/' path with
  | "" :: typ :: slug :: _ when
      typ = "papers" || typ = "notes" || typ = "projects"
      || typ = "ideas" || typ = "videos" ->
    (* Strip any trailing fragment/query from slug *)
    let slug = match String.split_on_char '#' slug with s :: _ -> s | [] -> slug in
    let slug = match String.split_on_char '?' slug with s :: _ -> s | [] -> slug in
    if String.length slug > 0 then
      Bushel.Entry.lookup entries slug
    else
      None
  | _ -> None

(** Scan a feed entry's HTML content for mentions of bushel entries. *)
let scan_feed_entry_mentions ~base_url ~entries (fe : Sortal_feed.Entry.t) =
  let html_parts =
    (match fe.content with Some c -> [c] | None -> [])
    @ (match fe.summary with Some s -> [s] | None -> [])
  in
  let all_urls = List.concat_map extract_urls_from_html html_parts in
  let seen = Hashtbl.create 8 in
  List.filter_map (fun url ->
    match resolve_url_to_entry ~base_url ~entries url with
    | Some entry ->
      let slug = Bushel.Entry.slug entry in
      if Hashtbl.mem seen slug then None
      else (Hashtbl.add seen slug (); Some entry)
    | None -> None
  ) all_urls

let load_feed_items ~author_handle ~base_url ~entries fs contacts =
  let xdg = Xdge.create fs "sortal" in
  let feed_store = Sortal_feed.Store.create_from_xdg xdg in
  let feed_backlinks = Hashtbl.create 64 in
  let items = List.concat_map (fun contact ->
    let handle = Sortal_schema.Contact.handle contact in
    if handle = author_handle then [] else
    match Sortal_schema.Contact.feeds contact with
    | Some feeds when feeds <> [] ->
      (try
         let feed_entries = Sortal_feed.Store.all_entries feed_store ~handle feeds in
         (* Load annotations for each feed *)
         let ann_by_feed = List.map (fun feed ->
           (Sortal_schema.Feed.url feed,
            Sortal_feed.Annotations.load
              (Sortal_feed.Store.annotations_file feed_store handle feed))
         ) feeds in
         List.map (fun fe ->
           let auto_mentions = scan_feed_entry_mentions ~base_url ~entries fe in
           let ann_mentions = match fe.Sortal_feed.Entry.url with
             | Some u ->
               let url_str = Uri.to_string u in
               List.concat_map (fun (_feed_url, ann) ->
                 List.filter_map (fun slug -> Bushel.Entry.lookup entries slug)
                   (Sortal_feed.Annotations.slugs_for_url ann url_str)
               ) ann_by_feed
             | None -> []
           in
           (* Deduplicate by slug *)
           let seen = Hashtbl.create 8 in
           let mentions = List.filter (fun entry ->
             let s = Bushel.Entry.slug entry in
             if Hashtbl.mem seen s then false
             else (Hashtbl.add seen s (); true)
           ) (ann_mentions @ auto_mentions) in
           (* Register feed backlinks for each mentioned slug *)
           List.iter (fun entry ->
             let slug = Bushel.Entry.slug entry in
             let bl = { contact; feed_entry = fe } in
             let cur = try Hashtbl.find feed_backlinks slug with Not_found -> [] in
             Hashtbl.replace feed_backlinks slug (bl :: cur)
           ) mentions;
           { contact; entry = fe; mentions }
         ) feed_entries
       with exn ->
         Log.warn (fun m -> m "Failed to load feed items for %s: %s"
           handle (Printexc.to_string exn));
         [])
    | _ -> []
  ) contacts in
  let items = List.sort (fun a b ->
    Sortal_feed.Entry.compare_by_date a.entry b.entry
  ) items in
  (* Build reverse index: normalised feed entry URL -> feed_backlink list *)
  let feed_by_url = Hashtbl.create 256 in
  List.iter (fun (item : feed_item) ->
    match item.entry.Sortal_feed.Entry.url with
    | Some u ->
      let key = normalise_url (Uri.to_string u) in
      let bl = { contact = item.contact; feed_entry = item.entry } in
      let cur = try Hashtbl.find feed_by_url key with Not_found -> [] in
      Hashtbl.replace feed_by_url key (bl :: cur)
    | None -> ()
  ) items;
  (items, feed_backlinks, feed_by_url)

let create ~config fs =
  let image_output_dir = config.Arod_config.paths.images_dir in
  let data_dir = config.paths.data_dir in
  let entries = Bushel_eio.Bushel_loader.load ~image_output_dir fs data_dir in
  let contacts = Bushel.Entry.contacts entries in
  let author_handle = config.site.author_handle in
  let base_url = config.site.base_url in
  let feed_items, feed_backlinks, feed_by_url = load_feed_items ~author_handle ~base_url ~entries fs contacts in
  let links_by_url =
    let links_file = Filename.concat data_dir "links.yml" in
    let tbl = Hashtbl.create 256 in
    (try
       let links = Bushel.Link.load_links_file links_file in
       List.iter (fun (l : Bushel.Link.t) ->
         Hashtbl.replace tbl l.url l
       ) links
     with _ -> ());
    tbl
  in
  { config; entries; feed_items; feed_backlinks; feed_by_url; links_by_url }

(** {1 Config Accessors} *)

let config t = t.config
let base_url t = t.config.site.base_url
let site_name t = t.config.site.name
let site_description t = t.config.site.description

let author t =
  let contacts = Bushel.Entry.contacts t.entries in
  List.find_opt (fun c ->
    Sortal_schema.Contact.handle c = t.config.site.author_handle
  ) contacts

let author_exn t =
  match author t with
  | Some c -> c
  | None -> failwith "Author not found"

let author_name t =
  match author t with
  | Some c -> Sortal_schema.Contact.name c
  | None -> t.config.site.author_name

(** {1 Entry Lookup} *)

let lookup t slug = Bushel.Entry.lookup t.entries slug
let lookup_exn t slug = Bushel.Entry.lookup_exn t.entries slug
let lookup_image t slug = Bushel.Entry.lookup_image t.entries slug
let lookup_by_name t name = Bushel.Entry.lookup_by_name t.entries name

let lookup_by_handle t handle =
  let contacts = Bushel.Entry.contacts t.entries in
  List.find_opt (fun c -> Sortal_schema.Contact.handle c = handle) contacts

(** {1 Entry Lists} *)

let entries t = t.entries
let papers t = Bushel.Entry.papers t.entries
let notes t = Bushel.Entry.notes t.entries
let ideas t = Bushel.Entry.ideas t.entries
let projects t = Bushel.Entry.projects t.entries
let videos t = Bushel.Entry.videos t.entries
let contacts t = Bushel.Entry.contacts t.entries
let images t = Bushel.Entry.images t.entries
let all_entries t = Bushel.Entry.all_entries t.entries

(** {1 Feed Items} *)

let feed_items t = t.feed_items

let feed_items_for_contact t handle =
  List.filter (fun (item : feed_item) ->
    Sortal_schema.Contact.handle item.contact = handle
  ) t.feed_items

let feed_backlinks_for_slug t slug =
  try Hashtbl.find t.feed_backlinks slug with Not_found -> []

let feed_items_for_outbound t slug =
  let ext_urls = Bushel.Link_graph.get_external_links_for_slug slug in
  let seen = Hashtbl.create 16 in
  List.concat_map (fun url ->
    let key = normalise_url url in
    match Hashtbl.find_opt t.feed_by_url key with
    | Some bls ->
      List.filter (fun (bl : feed_backlink) ->
        let fe_url = match bl.feed_entry.Sortal_feed.Entry.url with
          | Some u -> Uri.to_string u | None -> "" in
        if Hashtbl.mem seen fe_url then false
        else (Hashtbl.add seen fe_url (); true)
      ) bls
    | None -> []
  ) ext_urls

(** {1 Tags} *)

let tags_of_ent t ent = Bushel.Entry.tags_of_ent t.entries ent

(** {1 Links} *)

let link_for_url t url = Hashtbl.find_opt t.links_by_url url

let all_links t =
  Hashtbl.to_seq_values t.links_by_url |> List.of_seq

(** {1 Entry Filtering} *)

type entry_type = [ `Paper | `Note | `Video | `Idea | `Project ]

let entry_matches_type types ent =
  if types = [] then true
  else List.exists (fun typ ->
    match typ, ent with
    | `Paper, `Paper _ -> true | `Note, `Note _ -> true
    | `Video, `Video _ -> true | `Idea, `Idea _ -> true
    | `Project, `Project _ -> true | _ -> false
  ) types

let get_entries t ~types =
  let filterent = entry_matches_type types in
  let select ent =
    let only_talks = function
      | `Video { Bushel.Video.talk; _ } -> talk
      | _ -> true
    in
    let not_index_page = function
      | `Note { Bushel.Note.index_page; _ } -> not index_page
      | _ -> true
    in
    only_talks ent && not_index_page ent
  in
  all_entries t
  |> List.filter (fun ent -> select ent && filterent ent)
  |> List.sort Bushel.Entry.compare
  |> List.rev

let perma_entries t =
  all_entries t
  |> List.filter (function `Note n -> Bushel.Note.perma n | _ -> false)
  |> List.sort Bushel.Entry.compare
  |> List.rev
