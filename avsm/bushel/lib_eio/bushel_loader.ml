(*---------------------------------------------------------------------------
  Copyright (c) 2025 Anil Madhavapeddy <anil@recoil.org>. All rights reserved.
  SPDX-License-Identifier: ISC
 ---------------------------------------------------------------------------*)

(** Eio-based directory scanner and file loader for Bushel entries *)

let src = Logs.Src.create "bushel.loader" ~doc:"Bushel loader"
module Log = (val Logs.src_log src : Logs.LOG)

(** Load images from srcsetter index.json *)
let load_images fs ~output_dir =
  let index_path = Filename.concat output_dir "index.json" in
  let path = Eio.Path.(fs / index_path) in
  try
    let content = Eio.Path.load path in
    match Srcsetter.list_of_json content with
    | Ok images ->
      Log.info (fun m -> m "Loaded %d images from %s" (List.length images) index_path);
      images
    | Error e ->
      Log.warn (fun m -> m "Failed to parse %s: %s" index_path e);
      []
  with
  | Eio.Io (Eio.Fs.E (Eio.Fs.Not_found _), _) ->
    Log.info (fun m -> m "No image index found at %s" index_path);
    []

(** List markdown files in a directory *)
let list_md_files fs dir =
  let path = Eio.Path.(fs / dir) in
  try
    Eio.Path.read_dir path
    |> List.filter (fun f -> Filename.check_suffix f ".md")
    |> List.map (fun f -> Filename.concat dir f)
  with
  | Eio.Io (Eio.Fs.E (Eio.Fs.Not_found _), _) ->
    Log.warn (fun m -> m "Directory not found: %s" dir);
    []

(** Load and map files from a directory *)
let map_category fs base subdir parse_fn =
  let dir = Filename.concat base subdir in
  Log.debug (fun m -> m "Loading %s" subdir);
  let files = list_md_files fs dir in
  List.filter_map (fun path ->
    match Frontmatter_eio.of_file fs path with
    | Ok fm ->
      (match parse_fn fm with
       | Ok entry -> Some entry
       | Error e ->
         Log.err (fun m -> m "Error parsing %s: %s" path e);
         None)
    | Error e ->
      Log.err (fun m -> m "Error reading %s: %s" path e);
      None
  ) files

(** Load contacts from Sortal XDG store *)
let load_contacts fs _base =
  let store = Sortal.Store.create fs "sortal" in
  Sortal.Store.list store

(** Load projects from projects/ *)
let load_projects fs base =
  map_category fs base "projects" Bushel.Project.of_frontmatter

(** Load notes from notes/ and news/ *)
let load_notes fs base =
  let notes_dir = map_category fs base "notes" Bushel.Note.of_frontmatter in
  let news_dir = map_category fs base "news" Bushel.Note.of_frontmatter in
  notes_dir @ news_dir

(** Load ideas from ideas/ *)
let load_ideas fs base =
  map_category fs base "ideas" Bushel.Idea.of_frontmatter

(** Load videos from videos/ *)
let load_videos fs base =
  map_category fs base "videos" Bushel.Video.of_frontmatter

(** Load papers from papers/ (nested directory structure) *)
let load_papers fs base =
  let papers_dir = Filename.concat base "papers" in
  Log.debug (fun m -> m "Loading papers from %s" papers_dir);
  let path = Eio.Path.(fs / papers_dir) in
  let slug_dirs =
    try
      Eio.Path.read_dir path
      |> List.filter (fun slug ->
           try
             let stat = Eio.Path.stat ~follow:true Eio.Path.(fs / papers_dir / slug) in
             stat.kind = `Directory
           with _ -> false)
    with _ -> []
  in
  let papers = List.concat_map (fun slug ->
    let slug_path = Filename.concat papers_dir slug in
    let ver_files =
      try
        Eio.Path.(read_dir (fs / slug_path))
        |> List.filter (fun f -> Filename.check_suffix f ".md")
      with _ -> []
    in
    List.filter_map (fun ver_file ->
      let ver = Filename.chop_extension ver_file in
      let file_path = Filename.concat slug_path ver_file in
      match Frontmatter_eio.of_file fs file_path with
      | Ok fm ->
        (match Bushel.Paper.of_frontmatter ~slug ~ver fm with
         | Ok paper -> Some paper
         | Error e ->
           Log.err (fun m -> m "Error parsing paper %s/%s: %s" slug ver e);
           None)
      | Error e ->
        Log.err (fun m -> m "Error reading paper %s/%s: %s" slug ver e);
        None
    ) ver_files
  ) slug_dirs in
  Bushel.Paper.tv papers

(** Load all entries from a base directory *)
let rec load ?image_output_dir fs base =
  Log.info (fun m -> m "Loading bushel data from %s" base);
  let contacts = load_contacts fs base in
  Log.info (fun m -> m "Loaded %d contacts" (List.length contacts));
  let projects = load_projects fs base in
  Log.info (fun m -> m "Loaded %d projects" (List.length projects));
  let notes = load_notes fs base in
  Log.info (fun m -> m "Loaded %d notes" (List.length notes));
  let ideas = load_ideas fs base in
  Log.info (fun m -> m "Loaded %d ideas" (List.length ideas));
  let videos = load_videos fs base in
  Log.info (fun m -> m "Loaded %d videos" (List.length videos));
  let papers = load_papers fs base in
  Log.info (fun m -> m "Loaded %d papers" (List.length papers));
  let images = match image_output_dir with
    | Some output_dir -> load_images fs ~output_dir
    | None -> []
  in
  Log.info (fun m -> m "Loaded %d images" (List.length images));
  let doi_entries =
    let doi_path = Filename.concat base "doi.yml" in
    try
      let content = Eio.Path.load Eio.Path.(fs / doi_path) in
      let entries = Bushel.Doi_entry.of_yaml_string content in
      Log.info (fun m -> m "Loaded %d DOI entries from %s" (List.length entries) doi_path);
      entries
    with
    | Eio.Io (Eio.Fs.E (Eio.Fs.Not_found _), _) ->
      Log.info (fun m -> m "No DOI cache found at %s" doi_path);
      []
  in
  let data_dir = base in
  let entries = Bushel.Entry.v ~papers ~notes ~projects ~ideas ~videos ~contacts ~images ~doi_entries ~data_dir () in
  Log.info (fun m -> m "Building link graph");
  let graph = build_link_graph entries in
  Bushel.Link_graph.set_graph graph;
  Log.info (fun m -> m "Load complete: %a" Bushel.Link_graph.pp graph);
  entries

(** Build link graph from entries *)
and build_link_graph entries =
  let graph = Bushel.Link_graph.empty () in

  let add_internal_link source target target_type =
    let link = { Bushel.Link_graph.source; target; target_type } in
    graph.internal_links <- link :: graph.internal_links;
    Bushel.Link_graph.add_to_set_hashtbl graph.outbound source target;
    Bushel.Link_graph.add_to_set_hashtbl graph.backlinks target source
  in

  let add_external_link source url =
    let domain = Bushel.Util.extract_domain url in
    let link = { Bushel.Link_graph.source; domain; url } in
    graph.external_links <- link :: graph.external_links;
    Bushel.Link_graph.add_to_set_hashtbl graph.external_by_entry source url;
    Bushel.Link_graph.add_to_set_hashtbl graph.external_by_domain domain source
  in

  (* Process each entry *)
  List.iter (fun entry ->
    let source_slug = Bushel.Entry.slug entry in
    let md_content = Bushel.Entry.body entry in
    let all_links = Bushel.Md.extract_all_links md_content in

    List.iter (fun link ->
      if Bushel.Md.is_bushel_slug link then
        let target_slug = Bushel.Md.strip_handle link in
        (match Bushel.Entry.lookup entries target_slug with
         | Some target_entry ->
           let target_type = Bushel.Link_graph.entry_type_of_entry target_entry in
           add_internal_link source_slug target_slug target_type
         | None -> ())
      else if Bushel.Md.is_contact_slug link then
        let handle = Bushel.Md.strip_handle link in
        let contacts = Bushel.Entry.contacts entries in
        (match List.find_opt (fun c -> Sortal_schema.Contact.handle c = handle) contacts with
         | Some c ->
           add_internal_link source_slug (Sortal_schema.Contact.handle c) `Contact
         | None -> ())
      else if Bushel.Md.is_tag_slug link || Bushel.Md.is_type_filter_slug link then
        ()  (* Skip tag links *)
      else if String.starts_with ~prefix:"http://" link ||
              String.starts_with ~prefix:"https://" link then
        add_external_link source_slug link
    ) all_links
  ) (Bushel.Entry.all_entries entries);

  (* Process slug_ent references from notes *)
  List.iter (fun note ->
    match Bushel.Note.slug_ent note with
    | Some target_slug ->
      let source_slug = Bushel.Note.slug note in
      (match Bushel.Entry.lookup entries target_slug with
       | Some target_entry ->
         let target_type = Bushel.Link_graph.entry_type_of_entry target_entry in
         add_internal_link source_slug target_slug target_type
       | None -> ())
    | None -> ()
  ) (Bushel.Entry.notes entries);

  (* Process project references from papers *)
  List.iter (fun paper ->
    let source_slug = Bushel.Paper.slug paper in
    List.iter (fun project_slug ->
      match Bushel.Entry.lookup entries project_slug with
      | Some (`Project _) ->
        add_internal_link source_slug project_slug `Project
      | _ -> ()
    ) (Bushel.Paper.project_slugs paper)
  ) (Bushel.Entry.papers entries);

  (* Deduplicate links *)
  let module LinkSet = Set.Make(struct
    type t = Bushel.Link_graph.internal_link
    let compare (a : t) (b : t) =
      match String.compare a.source b.source with
      | 0 -> String.compare a.target b.target
      | n -> n
  end) in

  let module ExtLinkSet = Set.Make(struct
    type t = Bushel.Link_graph.external_link
    let compare (a : t) (b : t) =
      match String.compare a.source b.source with
      | 0 -> String.compare a.url b.url
      | n -> n
  end) in

  graph.internal_links <- LinkSet.elements (LinkSet.of_list graph.internal_links);
  graph.external_links <- ExtLinkSet.elements (ExtLinkSet.of_list graph.external_links);

  graph
