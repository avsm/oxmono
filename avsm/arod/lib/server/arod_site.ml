(*---------------------------------------------------------------------------
  Copyright (c) 2025 Anil Madhavapeddy <anil@recoil.org>. All rights reserved.
  SPDX-License-Identifier: ISC
 ---------------------------------------------------------------------------*)

open Proffer
open Proffer.Route
module H = Arod_handlers

(* Routes match in order, so a literal segment is listed before a capture at
   the same depth. That is why the feed and index.html routes under /notes and
   /papers come before the entry routes that would otherwise swallow them.

   A capture arrives decoded, so a Location built from one is put back through
   [H.encode_segment]. *)
let routes =
  [
    (* Front page *)
    get nil H.index;
    get (s "about" /? nil) H.index;
    (* Markdown renderings of the list pages *)
    get (s "index.md" /? nil) H.index_markdown;
    get (s "papers.md" /? nil) H.papers_markdown;
    get (s "notes.md" /? nil) H.notes_markdown;
    get (s "ideas.md" /? nil) H.ideas_markdown;
    get (s "projects.md" /? nil) H.projects_markdown;
    get (s "videos.md" /? nil) H.videos_markdown;
    get (s "links.md" /? nil) H.links_markdown;
    get (s "network.md" /? nil) H.network_markdown;
    (* Feeds *)
    get (s "news.xml" /? nil) (H.atom_feed "/news.xml");
    get (s "notes" / s "atom.xml" /? nil) (H.atom_feed "/notes/atom.xml");
    get (s "perma.xml" /? nil) H.perma_atom;
    get (s "feed.json" /? nil) H.json_feed;
    get (s "feeds" / s "feed.json" /? nil) H.json_feed;
    get (s "notes" / s "feed.json" /? nil) H.json_feed;
    get (s "perma.json" /? nil) H.perma_json;
    moved (s "atom.xml" /? nil) "/news.xml";
    moved (s "feed.xml" /? nil) "/news.xml";
    moved (s "rss.xml" /? nil) "/news.xml";
    moved (s "wiki.xml" /? nil) "/news.xml";
    moved (s "feeds" / s "atom.xml" /? nil) "/news.xml";
    (* A stale index.html URL redirects to its canonical page *)
    moved (s "index.html" /? nil) "/";
    moved (s "papers" / s "index.html" /? nil) "/papers";
    moved (s "notes" / s "index.html" /? nil) "/notes";
    moved (s "ideas" / s "index.html" /? nil) "/ideas";
    moved (s "projects" / s "index.html" /? nil) "/projects";
    moved (s "videos" / s "index.html" /? nil) "/videos";
    get
      (s "papers" / str / s "index.html" /? nil)
      (fun slug _env _req respond ->
        Resp.redirect respond ~permanent:true
          ("/papers/" ^ H.encode_segment slug));
    get
      (s "notes" / str / s "index.html" /? nil)
      (fun slug _env _req respond ->
        Resp.redirect respond ~permanent:true
          ("/notes/" ^ H.encode_segment slug));
    get
      (s "ideas" / str / s "index.html" /? nil)
      (fun slug _env _req respond ->
        Resp.redirect respond ~permanent:true
          ("/ideas/" ^ H.encode_segment slug));
    get
      (s "projects" / str / s "index.html" /? nil)
      (fun slug _env _req respond ->
        Resp.redirect respond ~permanent:true
          ("/projects/" ^ H.encode_segment slug));
    get
      (s "videos" / str / s "index.html" /? nil)
      (fun slug _env _req respond ->
        Resp.redirect respond ~permanent:true
          ("/videos/" ^ H.encode_segment slug));
    (* Collections *)
    get (s "papers" /? nil) H.papers_list;
    get (s "papers" / str /? nil) H.paper;
    get (s "notes" /? nil) H.notes_list;
    get (s "notes" / str /? nil) H.note;
    get (s "ideas" /? nil) H.ideas_list;
    get (s "ideas" / str /? nil) H.idea;
    get (s "projects" /? nil) H.projects_list;
    get (s "projects" / str /? nil) H.project;
    get (s "talks" /? nil) H.videos_list;
    get (s "videos" /? nil) H.videos_list;
    get (s "videos" / str /? nil) H.video;
    get (s "links" /? nil) H.links_list;
    get (s "network" /? nil) H.network_page;
    get (s "network" / s "blogroll.opml" /? nil) H.blogroll_opml;
    (* Pages that moved, and the tag links older markdown still writes *)
    moved (s "feeds" /? nil) "/network";
    moved (s "wiki" /? nil) "/notes";
    moved (s "news" /? nil) "/notes";
    get (s "news" / str /? nil) (fun slug _env _req respond ->
        Resp.redirect respond ~permanent:true
          ("/notes/" ^ H.encode_segment slug));
    get (s "tags" / str /? nil) (fun tag _env _req respond ->
        Resp.redirect respond ("/#tag=" ^ H.encode_segment tag));
    (* Machine-readable pages *)
    get (s "sitemap.xml" /? nil) H.sitemap;
    get (s "robots.txt" /? nil) H.robots_txt;
    get (s ".well-known" / str /? nil) H.well_known;
    (* JSON APIs *)
    get (s "api" / s "entries" /? nil) H.pagination_api;
    get (s "api" / s "search" /? nil) H.search_api;
    (* Assets *)
    get (s "favicon.svg" /? nil) (H.embedded_file "favicon.svg");
    get (s "favicon.ico" /? nil) (H.embedded_file "favicon.ico");
    get (s "favicon.png" /? nil) (H.embedded_file "favicon-32x32.png");
    get (s "favicon-32x32.png" /? nil) (H.embedded_file "favicon-32x32.png");
    get (s "favicon-16x16.png" /? nil) (H.embedded_file "favicon-16x16.png");
    get (s "apple-touch-icon.png" /? nil)
      (H.embedded_file "apple-touch-icon.png");
    get (s "site.webmanifest" /? nil) (H.embedded_file "site.webmanifest");
    get (s "tw.css" /? nil) (H.embedded_file_immutable "tw.css");
    get (s "js" / str /? nil) H.js_file;
    get (s "images" /* rest) H.image_file;
    (* Stats dashboard, hidden, not cached and not in the sitemap *)
    get (s "action" /? nil) H.stats_dashboard;
    get (s "action" / s "api" / s "overview" /? nil) H.stats_overview;
    get (s "action" / s "api" / s "traffic" /? nil) H.stats_traffic;
    get (s "action" / s "api" / s "recent" /? nil) H.stats_recent;
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
    | Some password -> fun auth -> H.stats_auth ~password auth
  in
  Site.of_routes routes
  |> Site.with_auth ~scope:[ [ "action" ] ] ~realm:"stats" ~check
  |> Site.with_headers security_headers
  |> Compiled.compile
