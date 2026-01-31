(*---------------------------------------------------------------------------
  Copyright (c) 2025 Anil Madhavapeddy <anil@recoil.org>. All rights reserved.
  SPDX-License-Identifier: ISC
 ---------------------------------------------------------------------------*)

(** Pure route handlers for arod *)

open Arod_entries
module R = Httpz_server.Route

(** {1 Response Helpers} *)

let to_page el = Htmlit.El.to_string ~doctype:true el

(** {1 File Serving} *)

let mime_type_of_path path =
  if String.ends_with ~suffix:".pdf" path then "application/pdf"
  else if String.ends_with ~suffix:".html" path then "text/html"
  else if String.ends_with ~suffix:".css" path then "text/css"
  else if String.ends_with ~suffix:".js" path then "text/javascript"
  else if String.ends_with ~suffix:".svg" path then "image/svg+xml"
  else if String.ends_with ~suffix:".png" path then "image/png"
  else if String.ends_with ~suffix:".jpg" path then "image/jpeg"
  else if String.ends_with ~suffix:".jpeg" path then "image/jpeg"
  else if String.ends_with ~suffix:".webp" path then "image/webp"
  else if String.ends_with ~suffix:".xml" path then "application/xml"
  else if String.ends_with ~suffix:".wasm" path then "application/wasm"
  else if String.ends_with ~suffix:".ico" path then "image/x-icon"
  else if String.ends_with ~suffix:".woff" path then "font/woff"
  else if String.ends_with ~suffix:".woff2" path then "font/woff2"
  else if String.ends_with ~suffix:".bib" path then "application/x-bibtex"
  else "application/octet-stream"

let static_file ~dir path _ctx respond =
  let clean_path =
    let parts = String.split_on_char '/' path in
    let safe_parts = List.filter (fun s -> s <> ".." && s <> ".") parts in
    String.concat "/" safe_parts
  in
  let file_path = Filename.concat dir clean_path in
  try
    if Sys.file_exists file_path && not (Sys.is_directory file_path) then begin
      let ic = open_in_bin file_path in
      let len = in_channel_length ic in
      let content = really_input_string ic len in
      close_in ic;
      let mime = mime_type_of_path file_path in
      R.respond_string respond ~status:Httpz.Res.Success ~headers:[(Httpz.Header_name.Content_type, mime)] content
    end
    else R.not_found respond
  with _ -> R.not_found respond

(** {1 Entry Handlers} *)

let entries_handler ~types _ctx respond =
  R.html respond (to_page (view_entries ~types))

let feed_handler ~types _ctx respond =
  R.html respond (to_page (view_news ~types))

(** {1 Content Handlers} *)

let index _ctx respond =
  match Arod_model.lookup "index" with
  | None -> R.not_found respond
  | Some ent -> R.html respond (to_page (view_one ent))

let papers = entries_handler ~types:[ `Paper ]

let paper cfg slug _ctx respond =
  match slug with
  | slug when String.ends_with ~suffix:".pdf" slug ->
      static_file ~dir:cfg.Arod_config.paths.static_dir ("papers/" ^ slug) _ctx respond
  | slug when String.ends_with ~suffix:".bib" slug ->
      let paper_slug = Filename.chop_extension slug in
      ( match Arod_model.lookup paper_slug with
      | Some (`Paper p) -> R.plain respond (Arod_model.Paper.bib p)
      | _ -> R.not_found respond )
  | _ -> (
      match Arod_model.lookup slug with
      | None -> R.not_found respond
      | Some ent -> R.html respond (to_page (view_one ent)) )

let notes = feed_handler ~types:[ `Note ]

let note slug _ctx respond =
  match Arod_model.lookup slug with
  | None -> R.not_found respond
  | Some ent -> R.html respond (to_page (view_one ent))

let ideas _ctx respond = R.html respond (to_page (Arod_ideas.view_ideas_by_project ()))

let idea slug _ctx respond =
  match Arod_model.lookup slug with
  | None -> R.not_found respond
  | Some ent -> R.html respond (to_page (view_one ent))

let projects _ctx respond = R.html respond (to_page (Arod_projects.view_projects_timeline ()))

let project slug _ctx respond =
  match Arod_model.lookup slug with
  | None -> R.not_found respond
  | Some ent -> R.html respond (to_page (view_one ent))

let videos = feed_handler ~types:[ `Video ]

let video slug _ctx respond =
  match Arod_model.lookup slug with
  | None -> R.not_found respond
  | Some ent -> R.html respond (to_page (view_one ent))

let content slug _ctx respond =
  match Arod_model.lookup slug with
  | None -> R.not_found respond
  | Some ent -> R.html respond (to_page (view_one ent))

(** {1 Legacy Handlers} *)

let news_redirect slug _ctx respond =
  R.redirect respond ~status:Httpz.Res.Moved_permanently ~location:("/notes/" ^ slug)

let wiki = entries_handler ~types:[ `Paper; `Note; `Video; `Idea; `Project ]

let news = feed_handler ~types:[ `Note ]

(** {1 Feed Handlers} *)

let atom_feed cfg ctx respond =
  let feed = get_entries ~types:[] in
  let path = R.path ctx in
  let s = Arod_feed.feed_string cfg path feed in
  R.atom respond s

let json_feed cfg _ctx respond =
  let feed = get_entries ~types:[] in
  let s = Arod_jsonfeed.feed_string cfg "/feed.json" feed in
  R.json respond s

let perma_atom cfg _ctx respond =
  let feed = perma_feed_of_req () in
  let s = Arod_feed.feed_string cfg "/perma.xml" feed in
  R.atom respond s

let perma_json cfg _ctx respond =
  let feed = perma_feed_of_req () in
  let s = Arod_jsonfeed.feed_string cfg "/perma.json" feed in
  R.json respond s

(** {1 Utility Handlers} *)

let sitemap cfg _ctx respond =
  let all_feed =
    Arod_model.all_entries ()
    |> List.sort Arod_model.Entry.compare
    |> List.rev
  in
  let url_of_entry ent =
    let lastmod = Arod_model.Entry.date ent in
    let loc = cfg.Arod_config.site.base_url ^ Arod_model.Entry.site_url ent in
    Sitemap.v ~lastmod loc
  in
  let sitemap_xml = List.map url_of_entry all_feed |> Sitemap.output in
  R.xml respond sitemap_xml

let bushel_graph _ctx respond = R.html respond (to_page (Arod_page.bushel_graph ()))

let bushel_graph_data _ctx respond =
  let entries = Arod_model.get_entries () in
  match Bushel.Link_graph.get_graph () with
  | None -> R.json respond {|{"error": "Link graph not initialized"}|}
  | Some graph ->
      let json = Bushel.Link_graph.to_json graph entries in
      R.json respond (Ezjsonm.value_to_string json)

let pagination_api ctx respond =
  try
    let collection_type =
      match R.query_param ctx "collection" with
      | Some t -> t
      | None -> failwith "Missing collection parameter"
    in
    let offset =
      match R.query_param ctx "offset" with
      | Some o -> int_of_string o
      | None -> 0
    in
    let limit =
      match R.query_param ctx "limit" with
      | Some l -> int_of_string l
      | None -> 25
    in
    let type_strings = R.query_params ctx "type" in
    let types = List.filter_map entry_type_of_string type_strings in
    let all_items = get_entries ~types in
    let total = List.length all_items in
    let slice =
      all_items
      |> (fun l -> List.filteri (fun i _ -> i >= offset) l)
      |> (fun l -> List.filteri (fun i _ -> i < limit) l)
    in
    let has_more = offset + List.length slice < total in
    let render_fn = match collection_type with
      | "feed" -> render_feeds_html
      | "entries" -> render_entries_html
      | _ -> failwith "Invalid collection type"
    in
    let html = (render_fn slice, total, has_more) in
    let rendered_html, total, has_more = html in
    let json =
      `O
        [
          ("html", `String rendered_html);
          ("total", `Float (float_of_int total));
          ("offset", `Float (float_of_int offset));
          ("limit", `Float (float_of_int limit));
          ("has_more", `Bool has_more);
        ]
    in
    R.json respond (Ezjsonm.to_string json)
  with e ->
    let error_json = `O [ ("error", `String (Printexc.to_string e)) ] in
    R.json respond (Ezjsonm.to_string error_json)

let well_known cfg key _ctx respond =
  match
    List.find_opt (fun e -> e.Arod_config.key = key) cfg.Arod_config.well_known
  with
  | Some entry -> R.plain respond entry.value
  | None -> R.not_found respond

let robots_txt cfg ctx respond =
  static_file ~dir:cfg.Arod_config.paths.assets_dir "robots.txt" ctx respond

(** {1 Route Collection} *)

let all_routes cfg =
  let open R in
  of_list
    [
      (* Index routes *)
      get_ [] index;
      get_ [ "about" ] index;
      (* Atom feeds *)
      get_ [ "wiki.xml" ] (atom_feed cfg);
      get_ [ "news.xml" ] (atom_feed cfg);
      get_ [ "feeds"; "atom.xml" ] (atom_feed cfg);
      get_ [ "notes"; "atom.xml" ] (atom_feed cfg);
      get_ [ "perma.xml" ] (perma_atom cfg);
      (* JSON feeds *)
      get_ [ "feed.json" ] (json_feed cfg);
      get_ [ "feeds"; "feed.json" ] (json_feed cfg);
      get_ [ "notes"; "feed.json" ] (json_feed cfg);
      get_ [ "perma.json" ] (perma_json cfg);
      (* Sitemap *)
      get_ [ "sitemap.xml" ] (sitemap cfg);
      (* Papers *)
      get ("papers" / seg root) (fun (slug, ()) -> paper cfg slug);
      get_ [ "papers" ] papers;
      (* Ideas *)
      get ("ideas" / seg root) (fun (slug, ()) -> idea slug);
      get_ [ "ideas" ] ideas;
      (* Notes *)
      get ("notes" / seg root) (fun (slug, ()) -> note slug);
      get_ [ "notes" ] notes;
      (* Videos/Talks *)
      get ("videos" / seg root) (fun (slug, ()) -> video slug);
      get_ [ "talks" ] videos;
      get_ [ "videos" ] videos;
      (* Projects *)
      get ("projects" / seg root) (fun (slug, ()) -> project slug);
      get_ [ "projects" ] projects;
      (* Legacy news redirect *)
      get ("news" / seg root) (fun (slug, ()) -> news_redirect slug);
      (* Wiki/News legacy *)
      get_ [ "wiki" ] wiki;
      get_ [ "news" ] news;
      (* Pagination API *)
      get_ [ "api"; "entries" ] pagination_api;
      (* Bushel link graph *)
      get_ [ "bushel" ] bushel_graph;
      get_ [ "bushel"; "graph.json" ] bushel_graph_data;
      (* Well-known endpoints *)
      get (".well-known" / seg root) (fun (key, ()) -> well_known cfg key);
      (* Robots.txt *)
      get_ [ "robots.txt" ] (robots_txt cfg);
      (* Static files *)
      get ("assets" / tail) (fun path -> static_file ~dir:cfg.paths.assets_dir (String.concat "/" path));
      get ("images" / tail) (fun path -> static_file ~dir:cfg.paths.images_dir (String.concat "/" path));
      get ("static" / tail) (fun path -> static_file ~dir:cfg.paths.static_dir (String.concat "/" path));
    ]
