(*---------------------------------------------------------------------------
  Copyright (c) 2025 Anil Madhavapeddy <anil@recoil.org>. All rights reserved.
  SPDX-License-Identifier: ISC
 ---------------------------------------------------------------------------*)

let src = Logs.Src.create "arod.ctx" ~doc:"Arod context"
module Log = (val Logs.src_log src : Logs.LOG)

type feed_item = {
  contact : Sortal_schema.Contact.t;
  entry : Sortal_feed.Entry.t;
  mentions : Bushel.Entry.entry list;
}

type feed_backlink = {
  contact : Sortal_schema.Contact.t;
  feed_entry : Sortal_feed.Entry.t;
}

type t = {
  config : Arod_config.t;
  entries : Bushel.Entry.t;
  feed_items : feed_item list;
  feed_backlinks : feed_backlink list Bushel.Smap.t;
  feed_by_url : feed_backlink list Bushel.Smap.t;
  forward_slugs : string list Bushel.Smap.t;
  outbound_feed : feed_backlink list Bushel.Smap.t;
  links_by_url : Bushel.Link.t Bushel.Smap.t;
  note_references :
    (string * string * Bushel.Md.reference_source) list Bushel.Smap.t;
}

let find_contact_by_handle entries handle =
  List.find_opt
    (fun contact -> Sortal_schema.Contact.handle contact = handle)
    (Bushel.Entry.contacts entries)

let normalise_url url =
  let uri = Uri.of_string url in
  match Uri.host uri with
  | Some host ->
    let host' =
      if String.starts_with ~prefix:"www." host
      then String.sub host 4 (String.length host - 4) else host
    in
    let u = Uri.with_host uri (Some host') in
    let path = Uri.path u in
    let path = if String.length path > 1 && String.ends_with ~suffix:"/" path
      then String.sub path 0 (String.length path - 1) else path in
    Uri.to_string (Uri.with_path u path)
  | None -> url

type annotation_index = (string, string list) Hashtbl.t

let annotation_index ann =
  let idx = Hashtbl.create (Hashtbl.length ann) in
  Hashtbl.iter
    (fun k (a : Sortal_feed.Annotations.entry_annotation) ->
       let key = normalise_url k in
       let cur = try Hashtbl.find idx key with Not_found -> [] in
       Hashtbl.replace idx key (a.slugs @ cur))
    ann;
  idx

let annotation_slugs idx url =
  try Hashtbl.find idx (normalise_url url) with Not_found -> []

let extract_urls_from_html html =
  let urls = ref [] in
  let len = String.length html in
  let rec scan i =
    if i >= len - 6 then ()
    else
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

let resolve_url_to_entry ~base_url ~entries url =
  let path =
    if String.length url > String.length base_url
       && String.starts_with ~prefix:base_url url then
      String.sub url (String.length base_url)
        (String.length url - String.length base_url)
    else if String.length url > 0 && url.[0] = '/' then
      url
    else
      ""
  in
  match String.split_on_char '/' path with
  | "" :: typ :: slug :: _ when
      typ = "papers" || typ = "notes" || typ = "projects"
      || typ = "ideas" || typ = "videos" ->
    let slug = match String.split_on_char '#' slug with s :: _ -> s | [] -> slug in
    let slug = match String.split_on_char '?' slug with s :: _ -> s | [] -> slug in
    if String.length slug > 0 then
      Bushel.Entry.lookup entries slug
    else
      None
  | _ -> None

let dedupe_by key values =
  let seen = Hashtbl.create (List.length values) in
  List.filter
    (fun value ->
      let key = key value in
      if Hashtbl.mem seen key then false else (Hashtbl.add seen key (); true))
    values

let scan_feed_entry_mentions ~base_url ~entries (fe : Sortal_feed.Entry.t) =
  let html_parts =
    (match fe.content with Some c -> [c] | None -> [])
    @ (match fe.summary with Some s -> [s] | None -> [])
  in
  let all_urls = List.concat_map extract_urls_from_html html_parts in
  List.filter_map (resolve_url_to_entry ~base_url ~entries) all_urls
  |> dedupe_by Bushel.Entry.slug

let load_feed_items ~author_handle ~base_url ~entries fs contacts =
  let xdg = Xdge.create fs "sortal" in
  let feed_store = Sortal_feed.Store.create_from_xdg xdg in
  let feed_backlinks = Hashtbl.create 64 in
  let items = List.concat_map (fun contact ->
    let handle = Sortal_schema.Contact.handle contact in
    if handle = author_handle then [] else
    match Sortal_schema.Contact.feeds contact with
    | feeds when feeds <> [] ->
      (try
         let feed_entries = Sortal_feed.Store.all_entries feed_store ~handle feeds in
         let ann_by_feed = List.map (fun feed ->
           annotation_index
             (Sortal_feed.Annotations.load
                (Sortal_feed.Store.annotations_file feed_store handle feed))
         ) feeds in
         List.map (fun fe ->
           let auto_mentions = scan_feed_entry_mentions ~base_url ~entries fe in
           let ann_mentions = match fe.Sortal_feed.Entry.url with
             | Some u ->
               let url_str = Uriz.to_string u in
               List.concat_map (fun idx ->
                 List.filter_map (fun slug -> Bushel.Entry.lookup entries slug)
                   (annotation_slugs idx url_str)
               ) ann_by_feed
             | None -> []
           in
           let mentions =
             dedupe_by Bushel.Entry.slug (ann_mentions @ auto_mentions)
           in
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
  let feed_by_url = Hashtbl.create 256 in
  List.iter (fun (item : feed_item) ->
    match item.entry.Sortal_feed.Entry.url with
    | Some u ->
      let key = normalise_url (Uriz.to_string u) in
      let bl = { contact = item.contact; feed_entry = item.entry } in
      let cur = try Hashtbl.find feed_by_url key with Not_found -> [] in
      Hashtbl.replace feed_by_url key (bl :: cur)
    | None -> ()
  ) items;
  let freeze tbl =
    Bushel.Smap.of_list (Hashtbl.fold (fun k v acc -> (k, v) :: acc) tbl [])
  in
  (items, freeze feed_backlinks, freeze feed_by_url)

let build_forward_slugs entries feed_items =
  let idx = Hashtbl.create 256 in
  List.iter
    (fun (l : Bushel.Link_graph.external_link) ->
       let key = normalise_url l.url in
       let cur = try Hashtbl.find idx key with Not_found -> [] in
       if not (List.mem l.source cur) then Hashtbl.replace idx key (l.source :: cur))
    (Bushel.Entry.all_external_links entries);
  Bushel.Smap.of_list
    (List.filter_map
       (fun (item : feed_item) ->
          match item.entry.Sortal_feed.Entry.url with
          | None -> None
          | Some u ->
            let raw = Uriz.to_string u in
            (match Hashtbl.find_opt idx (normalise_url raw) with
             | None | Some [] -> None
             | Some slugs -> Some (raw, slugs)))
       feed_items)

let build_outbound_feed entries feed_by_url =
  let by_source = Hashtbl.create 256 in
  List.iter
    (fun (l : Bushel.Link_graph.external_link) ->
       let cur = try Hashtbl.find by_source l.source with Not_found -> [] in
       Hashtbl.replace by_source l.source (l.url :: cur))
    (Bushel.Entry.all_external_links entries);
  let backlinks_of urls =
    let seen = Hashtbl.create 16 in
    List.concat_map
      (fun url ->
         match Bushel.Smap.find_opt (normalise_url url) feed_by_url with
         | None -> []
         | Some bls ->
           List.filter
             (fun (bl : feed_backlink) ->
                let fe_url =
                  match bl.feed_entry.Sortal_feed.Entry.url with
                  | Some u -> Uriz.to_string u
                  | None -> ""
                in
                if Hashtbl.mem seen fe_url then false
                else (Hashtbl.add seen fe_url (); true))
             bls)
      (List.sort_uniq String.compare urls)
  in
  Bushel.Smap.of_list
    (Hashtbl.fold
       (fun source urls acc ->
          match backlinks_of urls with [] -> acc | bls -> (source, bls) :: acc)
       by_source [])

let build_note_references ~config entries =
  match find_contact_by_handle entries config.Arod_config.site.author_handle with
  | None -> Bushel.Smap.empty
  | Some author ->
    Bushel.Smap.of_list
      (List.filter_map
         (fun note ->
            match Bushel.Md.note_references entries author note with
            | [] -> None
            | refs -> Some (Bushel.Note.slug note, refs))
         (Bushel.Entry.notes entries))

let create ~config fs =
  let image_output_dir = config.Arod_config.paths.images_dir in
  let data_dir = config.paths.data_dir in
  let entries = Bushel_eio.Bushel_loader.load ~image_output_dir fs data_dir in
  let contacts = Bushel.Entry.contacts entries in
  let author_handle = config.site.author_handle in
  let base_url = config.site.base_url in
  let feed_items, feed_backlinks, feed_by_url = load_feed_items ~author_handle ~base_url ~entries fs contacts in
  let forward_slugs = build_forward_slugs entries feed_items in
  let outbound_feed = build_outbound_feed entries feed_by_url in
  let links_by_url =
    let links_file = Filename.concat data_dir "links.yml" in
    let links =
      try Bushel.Link.load_links_file links_file with _ -> []
    in
    Bushel.Smap.of_list (List.map (fun (l : Bushel.Link.t) -> (l.url, l)) links)
  in
  {
    config;
    entries;
    feed_items;
    feed_backlinks;
    feed_by_url;
    forward_slugs;
    outbound_feed;
    links_by_url;
    note_references = build_note_references ~config entries;
  }

let of_entries ~config entries =
  {
    config;
    entries;
    feed_items = [];
    feed_backlinks = Bushel.Smap.empty;
    feed_by_url = Bushel.Smap.empty;
    forward_slugs = Bushel.Smap.empty;
    outbound_feed = Bushel.Smap.empty;
    links_by_url = Bushel.Smap.empty;
    note_references = build_note_references ~config entries;
  }

let config t = t.config
let base_url t = t.config.site.base_url

let author t =
  find_contact_by_handle t.entries t.config.site.author_handle

let author_exn t =
  match author t with
  | Some c -> c
  | None ->
    failwith
      (Printf.sprintf
         "Author handle %S not found among %d contacts in the sortal store"
         t.config.site.author_handle
         (List.length (Bushel.Entry.contacts t.entries)))

let author_name t =
  match author t with
  | Some c -> Sortal_schema.Contact.name c
  | None -> t.config.site.author_name

let lookup t slug = Bushel.Entry.lookup t.entries slug
let lookup_exn t slug = Bushel.Entry.lookup_exn t.entries slug
let lookup_image t slug = Bushel.Entry.lookup_image t.entries slug
let lookup_by_name t name = Bushel.Entry.lookup_by_name t.entries name

let lookup_by_handle t handle =
  find_contact_by_handle t.entries handle

let entries t = t.entries
let papers t = Bushel.Entry.papers t.entries
let notes t = Bushel.Entry.notes t.entries
let ideas t = Bushel.Entry.ideas t.entries
let projects t = Bushel.Entry.projects t.entries
let videos t = Bushel.Entry.videos t.entries
let contacts t = Bushel.Entry.contacts t.entries
let images t = Bushel.Entry.images t.entries
let all_entries t = Bushel.Entry.all_entries t.entries

let backlinks t slug = Bushel.Entry.backlinks t.entries slug
let outbound t slug = Bushel.Entry.outbound t.entries slug
let all_external_links t = Bushel.Entry.all_external_links t.entries

let find_list map key =
  Option.value (Bushel.Smap.find_opt key map) ~default:[]

let note_references t slug =
  find_list t.note_references slug

let feed_items t = t.feed_items

let feed_items_for_contact t handle =
  List.filter (fun (item : feed_item) ->
    Sortal_schema.Contact.handle item.contact = handle
  ) t.feed_items

let feed_backlinks_for_slug t slug =
  find_list t.feed_backlinks slug

let feed_items_for_outbound t slug =
  find_list t.outbound_feed slug

let forward_slugs t url =
  find_list t.forward_slugs url

let link_for_url t url = Bushel.Smap.find_opt url t.links_by_url

let all_links t = List.map snd (Bushel.Smap.bindings t.links_by_url)

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
