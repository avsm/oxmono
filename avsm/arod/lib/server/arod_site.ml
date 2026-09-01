(*---------------------------------------------------------------------------
  Copyright (c) 2025 Anil Madhavapeddy <anil@recoil.org>. All rights reserved.
  SPDX-License-Identifier: ISC
 ---------------------------------------------------------------------------*)

open Proffer
open Proffer.Route
module H = Arod_handlers

(* Captured values cross into application callbacks. *)
let segment = conv ~name:"segment" (fun s -> Some s)

(* Routes match in order, so a literal segment is listed before a capture at
   the same depth. That is why the feed and index.html routes under /notes and
   /papers come before the entry routes that would otherwise swallow them.

   A capture arrives decoded, so a Location built from one is put back through
   [H.encode_segment]. *)
let routes =
  [
    (* Front page *)
    get root H.index;
    get (s "about") H.index;
    (* Markdown renderings of the list pages *)
    get (s "index.md") H.index_markdown;
    get (s "papers.md") H.papers_markdown;
    get (s "notes.md") H.notes_markdown;
    get (s "ideas.md") H.ideas_markdown;
    get (s "projects.md") H.projects_markdown;
    get (s "videos.md") H.videos_markdown;
    get (s "links.md") H.links_markdown;
    get (s "network.md") H.network_markdown;
    (* Feeds *)
    get (s "news.xml") (H.atom_feed "/news.xml");
    get (s "notes" / s "atom.xml") (H.atom_feed "/notes/atom.xml");
    get (s "perma.xml") H.perma_atom;
    get (s "feed.json") H.json_feed;
    get (s "feeds" / s "feed.json") H.json_feed;
    get (s "notes" / s "feed.json") H.json_feed;
    get (s "perma.json") H.perma_json;
    moved (s "atom.xml") "/news.xml";
    moved (s "feed.xml") "/news.xml";
    moved (s "rss.xml") "/news.xml";
    moved (s "wiki.xml") "/news.xml";
    moved (s "feeds" / s "atom.xml") "/news.xml";
    (* A stale index.html URL redirects to its canonical page *)
    moved (s "index.html") "/";
    moved (s "papers" / s "index.html") "/papers";
    moved (s "notes" / s "index.html") "/notes";
    moved (s "ideas" / s "index.html") "/ideas";
    moved (s "projects" / s "index.html") "/projects";
    moved (s "videos" / s "index.html") "/videos";
    get
      (s "papers" / segment / s "index.html")
      (fun slug _env _req respond ->
        Resp.redirect respond ~permanent:true
          ("/papers/" ^ H.encode_segment slug));
    get
      (s "notes" / segment / s "index.html")
      (fun slug _env _req respond ->
        Resp.redirect respond ~permanent:true
          ("/notes/" ^ H.encode_segment slug));
    get
      (s "ideas" / segment / s "index.html")
      (fun slug _env _req respond ->
        Resp.redirect respond ~permanent:true
          ("/ideas/" ^ H.encode_segment slug));
    get
      (s "projects" / segment / s "index.html")
      (fun slug _env _req respond ->
        Resp.redirect respond ~permanent:true
          ("/projects/" ^ H.encode_segment slug));
    get
      (s "videos" / segment / s "index.html")
      (fun slug _env _req respond ->
        Resp.redirect respond ~permanent:true
          ("/videos/" ^ H.encode_segment slug));
    (* Collections *)
    get (s "papers") H.papers_list;
    get (s "papers" / segment) H.paper;
    get (s "notes") H.notes_list;
    get (s "notes" / segment) H.note;
    get (s "ideas") H.ideas_list;
    get (s "ideas" / segment) H.idea;
    get (s "projects") H.projects_list;
    get (s "projects" / segment) H.project;
    get (s "talks") H.videos_list;
    get (s "videos") H.videos_list;
    get (s "videos" / segment) H.video;
    get (s "links") H.links_list;
    get (s "network") H.network_page;
    get (s "network" / s "blogroll.opml") H.blogroll_opml;
    (* Pages that moved, and the tag links older markdown still writes *)
    moved (s "feeds") "/network";
    moved (s "wiki") "/notes";
    moved (s "news") "/notes";
    get (s "news" / segment) (fun slug _env _req respond ->
        Resp.redirect respond ~permanent:true
          ("/notes/" ^ H.encode_segment slug));
    get (s "tags" / segment) (fun tag _env _req respond ->
        Resp.redirect respond ("/#tag=" ^ H.encode_segment tag));
    (* Machine-readable pages *)
    get (s "sitemap.xml") H.sitemap;
    get (s "robots.txt") H.robots_txt;
    get (s "llms.txt") H.llms_txt;
    get (s ".well-known" / segment) H.well_known;
    get (s "search") H.search_page;
    (* JSON APIs *)
    get (s "api" / s "entries") H.pagination_api;
    get (s "api" / s "search") H.search_api;
    (* Assets *)
    get (s "favicon.svg") (H.embedded_file "favicon.svg");
    get (s "favicon.ico") (H.embedded_file "favicon.ico");
    get (s "favicon.png") (H.embedded_file "favicon-32x32.png");
    get (s "favicon-32x32.png") (H.embedded_file "favicon-32x32.png");
    get (s "favicon-16x16.png") (H.embedded_file "favicon-16x16.png");
    get (s "apple-touch-icon.png")
      (H.embedded_file "apple-touch-icon.png");
    get (s "site.webmanifest") (H.embedded_file "site.webmanifest");
    get (s "tw.css") (H.embedded_file_immutable "tw.css");
    get (s "js" / segment) H.js_file;
    get (s "images" / rest) H.image_file;
    (* Stats dashboard, hidden, not cached and not in the sitemap *)
    get (s "action") H.stats_dashboard;
    get (s "action" / s "api" / s "overview") H.stats_overview;
    get (s "action" / s "api" / s "traffic") H.stats_traffic;
    get (s "action" / s "api" / s "recent") H.stats_recent;
  ]

(* Sniffing a response as another type is what turns a served upload into a
   script, and a full referrer leaks the reader's path to every site linked
   from a note. Neither header constrains the site itself. *)
let security_headers =
  [
    ("X-Content-Type-Options", "nosniff");
    ("Referrer-Policy", "strict-origin-when-cross-origin");
  ]

let build (cfg : Arod.Config.t) =
  (* The gate answers everything under /action, so an unauthenticated caller
     cannot tell which paths there name a route. A configuration with no
     password leaves the dashboard open, as it always has. *)
  let check =
    match cfg.server.stats_password with
    | None -> fun (_ : string option) -> true
    | Some password -> fun auth ->
        let auth =
          match auth with
          | None -> None
          | Some header -> Some (Req.globalize header)
        in
        H.stats_auth ~password auth
  in
  Site.of_routes routes
  |> Site.with_auth ~scope:[ [ "action" ] ] ~realm:"stats" ~check
  |> Site.with_headers security_headers
