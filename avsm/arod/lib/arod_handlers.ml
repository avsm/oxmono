(*---------------------------------------------------------------------------
  Copyright (c) 2025 Anil Madhavapeddy <anil@recoil.org>. All rights reserved.
  SPDX-License-Identifier: ISC
 ---------------------------------------------------------------------------*)

(** Pure route handlers for arod *)

open Arod_entries
module R = Httpz.Route

(** {1 Query Information} *)

type query_info = Arod_entries.query_info = {
  tags : Arod_model.Tags.t list;
  min : int;
  show_all : bool;
}

let query_info_of_ctx ctx : query_info =
  let tags =
    R.query_params ctx "t"
    |> List.map Arod_model.Tags.of_string
  in
  let min =
    match R.query_param ctx "min" with
    | None -> 25
    | Some v -> ( try int_of_string v with _ -> 25 )
  in
  let show_all =
    match R.query_param ctx "all" with
    | None -> false
    | Some _ -> true
  in
  { tags; min; show_all }

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

let entries_handler ~extra_tags ~types ctx respond =
  let q = query_info_of_ctx ctx in
  let all_tags =
    Arod_model.concat_tags q.tags (List.map Arod_model.Tags.of_string extra_tags)
  in
  R.html respond
    (to_page
       (view_entries ~show_all:q.show_all ~tags:all_tags ~min:q.min ~types
          (entries_of_req ~extra_tags ~types { tags = q.tags; min = q.min; show_all = q.show_all })))

let feed_handler ~types ctx respond =
  let q = query_info_of_ctx ctx in
  R.html respond
    (to_page
       (view_news ~show_all:q.show_all ~tags:q.tags ~min:q.min ~types
          (feed_of_req ~types { tags = q.tags; min = q.min; show_all = q.show_all })))

let feed_handler_with_tags ~extra_tags ~types ctx respond =
  let q = query_info_of_ctx ctx in
  let tags =
    Arod_model.concat_tags q.tags (List.map Arod_model.Tags.of_string extra_tags)
  in
  let q' = { tags; min = q.min; show_all = q.show_all } in
  R.html respond
    (to_page
       (view_news ~show_all:q'.show_all ~tags:q'.tags ~min:q'.min ~types
          (feed_of_req ~types q')))

(** {1 Content Handlers} *)

let index ctx respond =
  let q = query_info_of_ctx ctx in
  match Arod_model.lookup "index" with
  | None -> R.not_found respond
  | Some ent ->
      R.html respond (to_page (view_one { tags = q.tags; min = q.min; show_all = q.show_all } ent))

let papers = entries_handler ~extra_tags:[] ~types:[ `Paper ]

let paper cfg slug ctx respond =
  let q = query_info_of_ctx ctx in
  match slug with
  | slug when String.ends_with ~suffix:".pdf" slug ->
      static_file ~dir:cfg.Arod_config.paths.static_dir ("papers/" ^ slug) ctx respond
  | slug when String.ends_with ~suffix:".bib" slug ->
      let paper_slug = Filename.chop_extension slug in
      ( match Arod_model.lookup paper_slug with
      | Some (`Paper p) -> R.plain respond (Arod_model.Paper.bib p)
      | _ -> R.not_found respond )
  | _ -> (
      match Arod_model.lookup slug with
      | None -> R.not_found respond
      | Some ent ->
          R.html respond (to_page (view_one { tags = q.tags; min = q.min; show_all = q.show_all } ent)) )

let notes = feed_handler_with_tags ~extra_tags:[] ~types:[ `Note ]

let note slug ctx respond =
  let q = query_info_of_ctx ctx in
  match Arod_model.lookup slug with
  | None -> R.not_found respond
  | Some ent ->
      R.html respond (to_page (view_one { tags = q.tags; min = q.min; show_all = q.show_all } ent))

let ideas _ctx respond = R.html respond (to_page (Arod_ideas.view_ideas_by_project ()))

let idea slug ctx respond =
  let q = query_info_of_ctx ctx in
  match Arod_model.lookup slug with
  | None -> R.not_found respond
  | Some ent ->
      R.html respond (to_page (view_one { tags = q.tags; min = q.min; show_all = q.show_all } ent))

let projects _ctx respond = R.html respond (to_page (Arod_projects.view_projects_timeline ()))

let project slug ctx respond =
  let q = query_info_of_ctx ctx in
  match Arod_model.lookup slug with
  | None -> R.not_found respond
  | Some ent ->
      R.html respond (to_page (view_one { tags = q.tags; min = q.min; show_all = q.show_all } ent))

let videos = feed_handler_with_tags ~extra_tags:[] ~types:[ `Video ]

let video slug ctx respond =
  let q = query_info_of_ctx ctx in
  match Arod_model.lookup slug with
  | None -> R.not_found respond
  | Some ent ->
      R.html respond (to_page (view_one { tags = q.tags; min = q.min; show_all = q.show_all } ent))

let content slug ctx respond =
  let q = query_info_of_ctx ctx in
  match Arod_model.lookup slug with
  | None -> R.not_found respond
  | Some ent ->
      R.html respond (to_page (view_one { tags = q.tags; min = q.min; show_all = q.show_all } ent))

(** {1 Legacy Handlers} *)

let news_redirect slug _ctx respond =
  R.redirect respond ~status:Httpz.Res.Moved_permanently ~location:("/notes/" ^ slug)

let wiki = entries_handler ~extra_tags:[] ~types:[ `Paper; `Note; `Video; `Idea; `Project ]

let news = feed_handler ~types:[ `Note ]

(** {1 Feed Handlers} *)

let atom_uri ctx =
  let path = R.path ctx in
  let query = R.query ctx in
  if query = [] then path
  else
    let query_string =
      String.concat "&" (List.map (fun (k, v) -> k ^ "=" ^ v) query)
    in
    path ^ "?" ^ query_string

let atom_feed cfg ctx respond =
  let q = query_info_of_ctx ctx in
  let feed = feed_of_req ~types:[] { tags = q.tags; min = q.min; show_all = q.show_all } in
  let ur = atom_uri ctx in
  let s = Arod_feed.feed_string cfg ur feed in
  R.atom respond s

let json_feed cfg ctx respond =
  let q = query_info_of_ctx ctx in
  let feed = feed_of_req ~types:[] { tags = q.tags; min = q.min; show_all = q.show_all } in
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
    let q = query_info_of_ctx ctx in
    let q' = { tags = q.tags; min = q.min; show_all = q.show_all } in
    let html =
      match collection_type with
      | "feed" ->
          let all_feed = feed_of_req ~types q' in
          let total = List.length all_feed in
          let feed_slice =
            all_feed
            |> (fun l -> List.filteri (fun i _ -> i >= offset) l)
            |> (fun l -> List.filteri (fun i _ -> i < limit) l)
          in
          let has_more = offset + List.length feed_slice < total in
          (render_feeds_html feed_slice, total, has_more)
      | "entries" ->
          let all_ents = entries_of_req ~extra_tags:[] ~types q' in
          let total = List.length all_ents in
          let ents_slice =
            all_ents
            |> (fun l -> List.filteri (fun i _ -> i >= offset) l)
            |> (fun l -> List.filteri (fun i _ -> i < limit) l)
          in
          let has_more = offset + List.length ents_slice < total in
          (render_entries_html ents_slice, total, has_more)
      | _ -> failwith "Invalid collection type"
    in
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
      get_ [ "about"; "" ] index;
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
      get ("papers" **> seg) (paper cfg);
      get ("papers" **> seg <** "") (paper cfg);
      get_ [ "papers" ] papers;
      get_ [ "papers"; "" ] papers;
      (* Ideas *)
      get ("ideas" **> seg) idea;
      get ("ideas" **> seg <** "") idea;
      get_ [ "ideas" ] ideas;
      get_ [ "ideas"; "" ] ideas;
      (* Notes *)
      get ("notes" **> seg) note;
      get ("notes" **> seg <** "") note;
      get_ [ "notes" ] notes;
      get_ [ "notes"; "" ] notes;
      (* Videos/Talks *)
      get ("videos" **> seg) video;
      get ("videos" **> seg <** "") video;
      get_ [ "talks" ] videos;
      get_ [ "talks"; "" ] videos;
      get_ [ "videos" ] videos;
      get_ [ "videos"; "" ] videos;
      (* Projects *)
      get ("projects" **> seg) project;
      get ("projects" **> seg <** "") project;
      get_ [ "projects" ] projects;
      get_ [ "projects"; "" ] projects;
      (* Legacy news redirect *)
      get ("news" **> seg) news_redirect;
      (* Wiki/News legacy *)
      get_ [ "wiki" ] wiki;
      get_ [ "news" ] news;
      (* Pagination API *)
      get_ [ "api"; "entries" ] pagination_api;
      (* Bushel link graph *)
      get_ [ "bushel" ] bushel_graph;
      get_ [ "bushel"; "" ] bushel_graph;
      get_ [ "bushel"; "graph.json" ] bushel_graph_data;
      (* Well-known endpoints *)
      get (".well-known" **> seg) (well_known cfg);
      (* Robots.txt *)
      get_ [ "robots.txt" ] (robots_txt cfg);
      (* Static files *)
      get ("assets" **> tail) (fun path -> static_file ~dir:cfg.paths.assets_dir path);
      get ("images" **> tail) (fun path -> static_file ~dir:cfg.paths.images_dir path);
      get ("static" **> tail) (fun path -> static_file ~dir:cfg.paths.static_dir path);
    ]
