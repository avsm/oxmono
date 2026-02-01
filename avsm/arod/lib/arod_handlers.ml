(*---------------------------------------------------------------------------
  Copyright (c) 2025 Anil Madhavapeddy <anil@recoil.org>. All rights reserved.
  SPDX-License-Identifier: ISC
 ---------------------------------------------------------------------------*)

(** Pure route handlers for arod using context-based state *)

module R = Httpz_server.Route
module Entry = Bushel.Entry
module Paper = Bushel.Paper

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

(** {1 Cached Handler Wrapper} *)

let cached ~cache ~key f respond =
  match Arod_cache.get cache key with
  | Some html -> R.html respond html
  | None ->
    let html = f () in
    Arod_cache.set cache key html;
    R.html respond html

(** {1 Cached Content Handlers} *)

let index ~ctx ~cache _rctx respond =
  let key = "/" in
  cached ~cache ~key (fun () ->
    match Arod_ctx.lookup ctx "index" with
    | None -> ""
    | Some ent -> to_page (Arod_render.view_one ~ctx ent)
  ) respond

let papers_list ~ctx ~cache _rctx respond =
  let key = "/papers" in
  cached ~cache ~key (fun () ->
    to_page (Arod_render.view_entries ~ctx ~types:[`Paper])
  ) respond

let paper ~ctx ~cache slug _rctx respond =
  let cfg = Arod_ctx.config ctx in
  match slug with
  | slug when String.ends_with ~suffix:".pdf" slug ->
    static_file ~dir:cfg.paths.static_dir ("papers/" ^ slug) _rctx respond
  | slug when String.ends_with ~suffix:".bib" slug ->
    let paper_slug = Filename.chop_extension slug in
    (match Arod_ctx.lookup ctx paper_slug with
     | Some (`Paper p) -> R.plain respond (Paper.bib p)
     | _ -> R.not_found respond)
  | _ ->
    let key = "/papers/" ^ slug in
    cached ~cache ~key (fun () ->
      match Arod_ctx.lookup ctx slug with
      | None -> ""
      | Some ent -> to_page (Arod_render.view_one ~ctx ent)
    ) respond

let notes_list ~ctx ~cache _rctx respond =
  let key = "/notes" in
  cached ~cache ~key (fun () ->
    to_page (Arod_render.view_news ~ctx ~types:[`Note])
  ) respond

let note ~ctx ~cache slug _rctx respond =
  let key = "/notes/" ^ slug in
  cached ~cache ~key (fun () ->
    match Arod_ctx.lookup ctx slug with
    | None -> ""
    | Some ent -> to_page (Arod_render.view_one ~ctx ent)
  ) respond

let ideas_list ~ctx ~cache _rctx respond =
  let key = "/ideas" in
  cached ~cache ~key (fun () ->
    to_page (Arod_render.view_ideas_by_project ~ctx)
  ) respond

let idea ~ctx ~cache slug _rctx respond =
  let key = "/ideas/" ^ slug in
  cached ~cache ~key (fun () ->
    match Arod_ctx.lookup ctx slug with
    | None -> ""
    | Some ent -> to_page (Arod_render.view_one ~ctx ent)
  ) respond

let projects_list ~ctx ~cache _rctx respond =
  let key = "/projects" in
  cached ~cache ~key (fun () ->
    to_page (Arod_render.view_projects_timeline ~ctx)
  ) respond

let project ~ctx ~cache slug _rctx respond =
  let key = "/projects/" ^ slug in
  cached ~cache ~key (fun () ->
    match Arod_ctx.lookup ctx slug with
    | None -> ""
    | Some ent -> to_page (Arod_render.view_one ~ctx ent)
  ) respond

let videos_list ~ctx ~cache _rctx respond =
  let key = "/videos" in
  cached ~cache ~key (fun () ->
    to_page (Arod_render.view_news ~ctx ~types:[`Video])
  ) respond

let video ~ctx ~cache slug _rctx respond =
  let key = "/videos/" ^ slug in
  cached ~cache ~key (fun () ->
    match Arod_ctx.lookup ctx slug with
    | None -> ""
    | Some ent -> to_page (Arod_render.view_one ~ctx ent)
  ) respond

let content ~ctx ~cache slug _rctx respond =
  let key = "/content/" ^ slug in
  cached ~cache ~key (fun () ->
    match Arod_ctx.lookup ctx slug with
    | None -> ""
    | Some ent -> to_page (Arod_render.view_one ~ctx ent)
  ) respond

(** {1 Legacy Handlers} *)

let news_redirect slug _rctx respond =
  R.redirect respond ~status:Httpz.Res.Moved_permanently ~location:("/notes/" ^ slug)

let wiki ~ctx ~cache _rctx respond =
  let key = "/wiki" in
  cached ~cache ~key (fun () ->
    to_page (Arod_render.view_entries ~ctx ~types:[`Paper; `Note; `Video; `Idea; `Project])
  ) respond

let news ~ctx ~cache _rctx respond =
  let key = "/news" in
  cached ~cache ~key (fun () ->
    to_page (Arod_render.view_news ~ctx ~types:[`Note])
  ) respond

(** {1 Feed Handlers} *)

let atom_feed ~ctx ~cache rctx respond =
  let path = R.path rctx in
  let key = "feed:" ^ path in
  cached ~cache ~key (fun () ->
    let cfg = Arod_ctx.config ctx in
    let feed = Arod_render.get_entries ~ctx ~types:[] in
    Arod_feed.feed_string ~ctx cfg path feed
  ) respond

let json_feed ~ctx ~cache _rctx respond =
  let key = "feed:/feed.json" in
  match Arod_cache.get cache key with
  | Some json -> R.json respond json
  | None ->
    let cfg = Arod_ctx.config ctx in
    let feed = Arod_render.get_entries ~ctx ~types:[] in
    let json = Arod_jsonfeed.feed_string ~ctx cfg "/feed.json" feed in
    Arod_cache.set cache key json;
    R.json respond json

let perma_atom ~ctx ~cache _rctx respond =
  let key = "feed:/perma.xml" in
  match Arod_cache.get cache key with
  | Some xml -> R.atom respond xml
  | None ->
    let cfg = Arod_ctx.config ctx in
    let feed = Arod_render.perma_entries ~ctx in
    let xml = Arod_feed.feed_string ~ctx cfg "/perma.xml" feed in
    Arod_cache.set cache key xml;
    R.atom respond xml

let perma_json ~ctx ~cache _rctx respond =
  let key = "feed:/perma.json" in
  match Arod_cache.get cache key with
  | Some json -> R.json respond json
  | None ->
    let cfg = Arod_ctx.config ctx in
    let feed = Arod_render.perma_entries ~ctx in
    let json = Arod_jsonfeed.feed_string ~ctx cfg "/perma.json" feed in
    Arod_cache.set cache key json;
    R.json respond json

(** {1 Utility Handlers (Dynamic - not cached)} *)

let sitemap ~ctx _rctx respond =
  let cfg = Arod_ctx.config ctx in
  let all_feed =
    Arod_ctx.all_entries ctx
    |> List.sort Entry.compare
    |> List.rev
  in
  let url_of_entry ent =
    let lastmod = Entry.date ent in
    let loc = cfg.site.base_url ^ Entry.site_url ent in
    Sitemap.v ~lastmod loc
  in
  let sitemap_xml = List.map url_of_entry all_feed |> Sitemap.output in
  R.xml respond sitemap_xml

let bushel_graph ~ctx ~cache _rctx respond =
  let key = "/bushel" in
  cached ~cache ~key (fun () ->
    to_page (Arod_page.bushel_graph ~ctx ())
  ) respond

let bushel_graph_data ~ctx _rctx respond =
  let entries = Arod_ctx.entries ctx in
  match Bushel.Link_graph.get_graph () with
  | None -> R.json respond {|{"error": "Link graph not initialized"}|}
  | Some graph ->
    let json = Bushel.Link_graph.to_json graph entries in
    R.json respond (Ezjsonm.value_to_string json)

let pagination_api ~ctx rctx respond =
  try
    let collection_type =
      match R.query_param rctx "collection" with
      | Some t -> t
      | None -> failwith "Missing collection parameter"
    in
    let offset =
      match R.query_param rctx "offset" with
      | Some o -> int_of_string o
      | None -> 0
    in
    let limit =
      match R.query_param rctx "limit" with
      | Some l -> int_of_string l
      | None -> 25
    in
    let type_strings = R.query_params rctx "type" in
    let types = List.filter_map Arod_render.entry_type_of_string type_strings in
    let all_items = Arod_render.get_entries ~ctx ~types in
    let total = List.length all_items in
    let slice =
      all_items
      |> (fun l -> List.filteri (fun i _ -> i >= offset) l)
      |> (fun l -> List.filteri (fun i _ -> i < limit) l)
    in
    let has_more = offset + List.length slice < total in
    let render_fn = match collection_type with
      | "feed" -> Arod_render.render_feeds_html ~ctx
      | "entries" -> Arod_render.render_entries_html ~ctx
      | _ -> failwith "Invalid collection type"
    in
    let rendered_html = render_fn slice in
    let json =
      `O [
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

let well_known ~ctx key _rctx respond =
  let cfg = Arod_ctx.config ctx in
  match List.find_opt (fun e -> e.Arod_config.key = key) cfg.well_known with
  | Some entry -> R.plain respond entry.value
  | None -> R.not_found respond

let robots_txt ~ctx rctx respond =
  let cfg = Arod_ctx.config ctx in
  static_file ~dir:cfg.paths.assets_dir "robots.txt" rctx respond

(** {1 Route Collection} *)

let all_routes ~ctx ~cache =
  let cfg = Arod_ctx.config ctx in
  let open R in
  of_list [
    (* Index routes *)
    get_ [] (index ~ctx ~cache);
    get_ [ "about" ] (index ~ctx ~cache);
    (* Atom feeds *)
    get_ [ "wiki.xml" ] (atom_feed ~ctx ~cache);
    get_ [ "news.xml" ] (atom_feed ~ctx ~cache);
    get_ [ "feeds"; "atom.xml" ] (atom_feed ~ctx ~cache);
    get_ [ "notes"; "atom.xml" ] (atom_feed ~ctx ~cache);
    get_ [ "perma.xml" ] (perma_atom ~ctx ~cache);
    (* JSON feeds *)
    get_ [ "feed.json" ] (json_feed ~ctx ~cache);
    get_ [ "feeds"; "feed.json" ] (json_feed ~ctx ~cache);
    get_ [ "notes"; "feed.json" ] (json_feed ~ctx ~cache);
    get_ [ "perma.json" ] (perma_json ~ctx ~cache);
    (* Sitemap *)
    get_ [ "sitemap.xml" ] (sitemap ~ctx);
    (* Papers *)
    get ("papers" / seg root) (fun (slug, ()) -> paper ~ctx ~cache slug);
    get_ [ "papers" ] (papers_list ~ctx ~cache);
    (* Ideas *)
    get ("ideas" / seg root) (fun (slug, ()) -> idea ~ctx ~cache slug);
    get_ [ "ideas" ] (ideas_list ~ctx ~cache);
    (* Notes *)
    get ("notes" / seg root) (fun (slug, ()) -> note ~ctx ~cache slug);
    get_ [ "notes" ] (notes_list ~ctx ~cache);
    (* Videos/Talks *)
    get ("videos" / seg root) (fun (slug, ()) -> video ~ctx ~cache slug);
    get_ [ "talks" ] (videos_list ~ctx ~cache);
    get_ [ "videos" ] (videos_list ~ctx ~cache);
    (* Projects *)
    get ("projects" / seg root) (fun (slug, ()) -> project ~ctx ~cache slug);
    get_ [ "projects" ] (projects_list ~ctx ~cache);
    (* Legacy news redirect *)
    get ("news" / seg root) (fun (slug, ()) -> news_redirect slug);
    (* Wiki/News legacy *)
    get_ [ "wiki" ] (wiki ~ctx ~cache);
    get_ [ "news" ] (news ~ctx ~cache);
    (* Pagination API - dynamic, not cached *)
    get_ [ "api"; "entries" ] (pagination_api ~ctx);
    (* Bushel link graph *)
    get_ [ "bushel" ] (bushel_graph ~ctx ~cache);
    get_ [ "bushel"; "graph.json" ] (bushel_graph_data ~ctx);
    (* Well-known endpoints *)
    get (".well-known" / seg root) (fun (key, ()) -> well_known ~ctx key);
    (* Robots.txt *)
    get_ [ "robots.txt" ] (robots_txt ~ctx);
    (* Static files - not cached *)
    get ("assets" / tail) (fun path -> static_file ~dir:cfg.paths.assets_dir (String.concat "/" path));
    get ("images" / tail) (fun path -> static_file ~dir:cfg.paths.images_dir (String.concat "/" path));
    get ("static" / tail) (fun path -> static_file ~dir:cfg.paths.static_dir (String.concat "/" path));
  ]
