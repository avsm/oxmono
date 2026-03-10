(*---------------------------------------------------------------------------
  Copyright (c) 2025 Anil Madhavapeddy <anil@recoil.org>. All rights reserved.
  SPDX-License-Identifier: ISC
 ---------------------------------------------------------------------------*)

(** Pure route handlers for arod using context-based state *)

module R = Httpz_server.Route
module Entry = Bushel.Entry
module Paper = Bushel.Paper
module C = Arod_component

(** {1 Response Helpers} *)

(* Response helpers that work with local_ respond - call respond with all args at once *)
let[@inline] send_html (local_ respond) s =
  respond ~status:Httpz.Res.Success
    ~headers:[(Httpz.Header_name.Content_type, "text/html; charset=utf-8")]
    (R.String s)

let[@inline] send_html_empty (local_ respond) =
  respond ~status:Httpz.Res.Success
    ~headers:[(Httpz.Header_name.Content_type, "text/html; charset=utf-8")]
    R.Empty

let[@inline] send_atom (local_ respond) s =
  respond ~status:Httpz.Res.Success
    ~headers:[(Httpz.Header_name.Content_type, "application/atom+xml; charset=utf-8")]
    (R.String s)

let[@inline] send_atom_empty (local_ respond) =
  respond ~status:Httpz.Res.Success
    ~headers:[(Httpz.Header_name.Content_type, "application/atom+xml; charset=utf-8")]
    R.Empty

let[@inline] send_json (local_ respond) s =
  respond ~status:Httpz.Res.Success
    ~headers:[(Httpz.Header_name.Content_type, "application/json; charset=utf-8")]
    (R.String s)

let[@inline] send_json_empty (local_ respond) =
  respond ~status:Httpz.Res.Success
    ~headers:[(Httpz.Header_name.Content_type, "application/json; charset=utf-8")]
    R.Empty

let[@inline] send_file (local_ respond) ~mime s =
  respond ~status:Httpz.Res.Success
    ~headers:[(Httpz.Header_name.Content_type, mime)]
    (R.String s)

let[@inline] send_file_empty (local_ respond) ~mime =
  respond ~status:Httpz.Res.Success
    ~headers:[(Httpz.Header_name.Content_type, mime)]
    R.Empty

let[@inline] not_found (local_ respond) =
  respond ~status:Httpz.Res.Not_found ~headers:[] (R.String "Not Found")

(** {1 HTTP Basic Auth} *)

let[@inline] send_auth_challenge (local_ respond) =
  respond ~status:Httpz.Res.Unauthorized
    ~headers:[(Httpz.Header_name.Www_authenticate, "Basic realm=\"stats\"")]
    (R.String "Unauthorized")

let check_stats_auth (cfg : Arod.Config.t) auth =
  match cfg.server.stats_password with
  | None -> true
  | Some password ->
    match auth with
    | None -> false
    | Some header ->
      let prefix = "Basic " in
      if not (String.starts_with ~prefix header) then false
      else
        let encoded = String.sub header (String.length prefix)
            (String.length header - String.length prefix) in
        match Base64.decode encoded with
        | Error _ -> false
        | Ok decoded ->
          (* Basic auth format: "user:password" — we only check the password *)
          match String.index_opt decoded ':' with
          | None -> false
          | Some i ->
            let pw = String.sub decoded (i + 1) (String.length decoded - i - 1) in
            String.equal pw password

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
  else if String.ends_with ~suffix:".webmanifest" path then "application/manifest+json"
  else "application/octet-stream"

let static_file ~dir path rctx (local_ respond) =
  match Eio.Path.load Eio.Path.(dir / path) with
  | content ->
    let mime = mime_type_of_path path in
    if R.is_head rctx then send_file_empty respond ~mime
    else send_file respond ~mime content
  | exception _ -> not_found respond

(** {1 Embedded Asset Serving} *)

let embedded_file path rctx (local_ respond) =
  match Arod_assets.read path with
  | Some content ->
    let mime = mime_type_of_path path in
    if R.is_head rctx then
      send_file_empty respond ~mime
    else
      send_file respond ~mime content
  | None -> not_found respond

(** {1 Cached Handler Wrapper} *)

let cached ~cache ~key rctx f (local_ respond) =
  if R.is_head rctx then
    send_html_empty respond
  else
    match Arod.Cache.get cache key with
    | Some html ->
      respond ~status:Httpz.Res.Success
        ~headers:[(Httpz.Header_name.Content_type, "text/html; charset=utf-8");
                   (Httpz.Header_name.X_cache, "hit")]
        (R.String html)
    | None ->
      let html = f () in
      Arod.Cache.set cache key html;
      respond ~status:Httpz.Res.Success
        ~headers:[(Httpz.Header_name.Content_type, "text/html; charset=utf-8");
                   (Httpz.Header_name.X_cache, "miss")]
        (R.String html)

(** {1 Content Negotiation} *)

let wants_markdown = function
  | Some accept ->
    let parts = String.split_on_char ',' accept in
    List.exists (fun s ->
      let t = String.trim s in
      t = "text/markdown" || String.starts_with ~prefix:"text/markdown;" t
    ) parts
  | None -> false

let[@inline] send_markdown (local_ respond) s =
  respond ~status:Httpz.Res.Success
    ~headers:[
      (Httpz.Header_name.Content_type, "text/markdown; charset=utf-8");
      (Httpz.Header_name.Vary, "Accept")]
    (R.String s)

let[@inline] send_html_vary (local_ respond) s =
  respond ~status:Httpz.Res.Success
    ~headers:[
      (Httpz.Header_name.Content_type, "text/html; charset=utf-8");
      (Httpz.Header_name.Vary, "Accept")]
    (R.String s)

let negotiated ~cache ~key rctx accept ~html_fn ~md_fn (local_ respond) =
  if R.is_head rctx then send_html_empty respond
  else if wants_markdown accept then
    let md_key = key ^ ":md" in
    (match Arod.Cache.get cache md_key with
     | Some md ->
       respond ~status:Httpz.Res.Success
         ~headers:[(Httpz.Header_name.Content_type, "text/markdown; charset=utf-8");
                    (Httpz.Header_name.Vary, "Accept");
                    (Httpz.Header_name.X_cache, "hit")]
         (R.String md)
     | None ->
       let md = md_fn () in
       Arod.Cache.set cache md_key md;
       respond ~status:Httpz.Res.Success
         ~headers:[(Httpz.Header_name.Content_type, "text/markdown; charset=utf-8");
                    (Httpz.Header_name.Vary, "Accept");
                    (Httpz.Header_name.X_cache, "miss")]
         (R.String md))
  else
    (match Arod.Cache.get cache key with
     | Some html ->
       respond ~status:Httpz.Res.Success
         ~headers:[(Httpz.Header_name.Content_type, "text/html; charset=utf-8");
                    (Httpz.Header_name.Vary, "Accept");
                    (Httpz.Header_name.X_cache, "hit")]
         (R.String html)
     | None ->
       let html = html_fn () in
       Arod.Cache.set cache key html;
       respond ~status:Httpz.Res.Success
         ~headers:[(Httpz.Header_name.Content_type, "text/html; charset=utf-8");
                    (Httpz.Header_name.Vary, "Accept");
                    (Httpz.Header_name.X_cache, "miss")]
         (R.String html))

(** {1 Cached Content Handlers} *)

let index ~ctx ~cache accept rctx (local_ respond) =
  let key = "/" in
  negotiated ~cache ~key rctx accept
    ~html_fn:(fun () ->
      match Arod.Ctx.lookup ctx "index" with
      | None -> ""
      | Some ent ->
        let article = C.Entry.full_body ~ctx ent in
        let socials = C.Sidebar.socials_box ~ctx in
        let sidebar =
          Htmlit.El.aside
            ~at:[Htmlit.At.class' "hidden lg:block lg:w-72 shrink-0"]
            [socials]
        in
        let cfg = Arod.Ctx.config ctx in
        let base_url = cfg.site.base_url in
        let jsonld = [
          Arod.Jsonld.profile_page_jsonld ~ctx;
          Arod.Jsonld.breadcrumb_jsonld ~base_url [("Home", "/")];
        ] in
        C.Layout.page ~ctx ~title:(Bushel.Entry.title ent) ~description:"" ~url:"/" ~current_page:"About" ~jsonld ~page_scripts:[] ~article ~sidebar ~mobile_footer:socials ())
    ~md_fn:(fun () -> C.Markdown_export.index_md ~ctx)
  respond

let papers_list ~ctx ~cache accept rctx (local_ respond) =
  let key = "/papers" in
  negotiated ~cache ~key rctx accept
    ~html_fn:(fun () ->
      let article, sidebar = C.Paper.papers_list ~ctx in
      let cfg = Arod.Ctx.config ctx in
      let base_url = cfg.site.base_url in
      let count = List.length (Arod.Ctx.papers ctx) in
      let jsonld = [
        Arod.Jsonld.collection_page_jsonld ~base_url ~url:"/papers" ~title:"Papers" ~description:"Academic papers" ~count ();
        Arod.Jsonld.breadcrumb_jsonld ~base_url [("Home", "/"); ("Papers", "/papers")];
      ] in
      C.Layout.page ~ctx ~title:"Papers" ~description:"Academic papers" ~url:"/papers" ~current_page:"Papers" ~jsonld ~page_scripts:[Papers_calendar; Classification_filter; Tag_cloud_filter; Pagination; Toc] ~article ~sidebar ())
    ~md_fn:(fun () -> C.Markdown_export.papers_list_md ~ctx)
  respond

let paper ~ctx ~cache ~papers_dir slug accept rctx (local_ respond) =
  let cfg = Arod.Ctx.config ctx in
  match slug with
  | slug when String.ends_with ~suffix:".pdf" slug ->
    static_file ~dir:papers_dir slug rctx respond
  | slug when String.ends_with ~suffix:".bib" slug ->
    let paper_slug = Filename.chop_extension slug in
    begin match Arod.Ctx.lookup ctx paper_slug with
     | Some (`Paper p) -> R.plain_gen rctx respond (fun () -> Paper.bib p)
     | _ -> not_found respond
    end
  | slug when String.ends_with ~suffix:".md" slug ->
    let real_slug = Filename.chop_extension slug in
    begin match Arod.Ctx.lookup ctx real_slug with
     | Some ent -> send_markdown respond (C.Markdown_export.entry_to_markdown ~ctx ent)
     | None -> not_found respond
    end
  | _ ->
    let key = "/papers/" ^ slug in
    negotiated ~cache ~key rctx accept
      ~html_fn:(fun () ->
        match Arod.Ctx.lookup ctx slug with
        | None -> ""
        | Some (`Paper p) ->
          let paper_el, sidenotes = C.Paper.full ~ctx p in
          let related = C.Sidebar.related_stream ~ctx (Paper.slug p) in
          let article = Htmlit.El.div [paper_el; C.Paper.extra ~ctx p; related] in
          let sidebar = C.Sidebar.for_entry ~ctx ~sidenotes (`Paper p) in
          let entries = Arod.Ctx.entries ctx in
          let description = let a = Paper.abstract p in if a <> "" then a else Paper.title p in
          let image = match Bushel.Entry.thumbnail entries (`Paper p) with
            | Some t -> Some (cfg.site.base_url ^ t) | None -> None in
          let published = Paper.date p in
          let tags = List.map Bushel.Tags.to_raw_string (Arod.Ctx.tags_of_ent ctx (`Paper p)) in
          let journal =
            let bibty = String.lowercase_ascii (Paper.bibtype p) in
            match bibty with
            | "article" | "journal" -> Some (Paper.journal p)
            | "inproceedings" | "abstract" -> Some (Paper.booktitle p)
            | _ -> None
          in
          let citation = C.Layout.{
            citation_title = Paper.title p;
            citation_authors = Paper.authors p;
            citation_date = C.Layout.ptime_to_citation_date published;
            citation_doi = Paper.doi p;
            citation_pdf_url = (let pdf_file = Paper.slug p ^ ".pdf" in
              let pdf_path = Filename.concat cfg.paths.papers_dir pdf_file in
              if Sys.file_exists pdf_path then Some (cfg.site.base_url ^ "/papers/" ^ pdf_file) else None);
            citation_journal = journal;
          } in
          let base_url = cfg.site.base_url in
          let jsonld = [
            Arod.Jsonld.scholarly_article_jsonld
              ~base_url ~url:("/papers/" ^ slug)
              ~title:(Paper.title p) ~description
              ~authors:(Paper.authors p) ~date:published
              ?doi:(Paper.doi p) ?image
              ?journal:(let bibty = String.lowercase_ascii (Paper.bibtype p) in
                        match bibty with
                        | "article" | "journal" -> Some (Paper.journal p)
                        | "inproceedings" | "abstract" -> Some (Paper.booktitle p)
                        | _ -> None)
              ~tags ();
            Arod.Jsonld.breadcrumb_jsonld ~base_url
              [("Home", "/"); ("Papers", "/papers"); (Paper.title p, "/papers/" ^ slug)];
          ] in
          C.Layout.page ~ctx ~title:(Paper.title p) ~description
            ~url:("/papers/" ^ slug) ?image ~og_type:"article"
            ~published ~tags ~citation ~jsonld ~page_scripts:[Toc; Lightbox; Links_modal] ~article ~sidebar ()
        | Some ent ->
          let article = C.Entry.full_body ~ctx ent in
          C.Layout.page ~ctx ~title:(Bushel.Entry.title ent) ~description:"" ~page_scripts:[Toc; Lightbox] ~article ())
      ~md_fn:(fun () ->
        match Arod.Ctx.lookup ctx slug with
        | None -> ""
        | Some ent -> C.Markdown_export.entry_to_markdown ~ctx ent)
    respond

let notes_list ~ctx ~cache accept rctx (local_ respond) =
  let key = "/notes" in
  negotiated ~cache ~key rctx accept
    ~html_fn:(fun () ->
      let article, sidebar = C.Note.notes_list ~ctx in
      let cfg = Arod.Ctx.config ctx in
      let base_url = cfg.site.base_url in
      let count = List.length (Arod.Ctx.notes ctx) in
      let jsonld = [
        Arod.Jsonld.collection_page_jsonld ~base_url ~url:"/notes" ~title:"Notes" ~description:"Notes and blog posts" ~count ();
        Arod.Jsonld.breadcrumb_jsonld ~base_url [("Home", "/"); ("Notes", "/notes")];
      ] in
      C.Layout.page ~ctx ~title:"Notes" ~description:"Notes and blog posts" ~url:"/notes" ~current_page:"Notes" ~jsonld ~page_scripts:[Notes_calendar; Tag_cloud_filter; Pagination; Toc] ~article ~sidebar ())
    ~md_fn:(fun () -> C.Markdown_export.notes_list_md ~ctx)
  respond

let note ~ctx ~cache slug accept rctx (local_ respond) =
  if String.ends_with ~suffix:".md" slug then
    let real_slug = Filename.chop_extension slug in
    match Arod.Ctx.lookup ctx real_slug with
    | Some ent -> send_markdown respond (C.Markdown_export.entry_to_markdown ~ctx ent)
    | None -> not_found respond
  else
  let key = "/notes/" ^ slug in
  negotiated ~cache ~key rctx accept
    ~html_fn:(fun () ->
      match Arod.Ctx.lookup ctx slug with
      | None -> ""
      | Some (`Note n) ->
        let article_el, sidenotes, headings = C.Note.full_page ~ctx n in
        let refs = C.Note.references ~ctx n in
        let related = C.Sidebar.related_stream ~ctx (Bushel.Note.slug n) in
        let full_article = Htmlit.El.div [article_el; refs; related] in
        let sidebar = C.Sidebar.for_entry ~ctx ~sidenotes (`Note n) in
        let cfg = Arod.Ctx.config ctx in
        let entries = Arod.Ctx.entries ctx in
        let description = Option.value ~default:"" (Bushel.Note.synopsis n) in
        let image = match Bushel.Entry.thumbnail entries (`Note n) with
          | Some t -> Some (cfg.site.base_url ^ t) | None -> None in
        let published = Bushel.Entry.date (`Note n) in
        let modified = n.Bushel.Note.updated in
        let tags = List.map Bushel.Tags.to_raw_string (Arod.Ctx.tags_of_ent ctx (`Note n)) in
        let citation = match Bushel.Note.doi n with
          | Some doi -> Some C.Layout.{
              citation_title = Bushel.Note.title n;
              citation_authors = [Arod.Ctx.author_name ctx];
              citation_date = C.Layout.ptime_to_citation_date published;
              citation_doi = Some doi;
              citation_pdf_url = None;
              citation_journal = None;
            }
          | None -> None in
        let base_url = cfg.site.base_url in
        let author_name = Arod.Ctx.author_name ctx in
        let jsonld = [
          Arod.Jsonld.article_jsonld
            ~base_url ~url:("/notes/" ^ slug)
            ~title:(Bushel.Note.title n) ~description ~author_name
            ~date:published ?modified ?image ~tags ();
          Arod.Jsonld.breadcrumb_jsonld ~base_url
            [("Home", "/"); ("Notes", "/notes"); (Bushel.Note.title n, "/notes/" ^ slug)];
        ] in
        let standardsite = Bushel.Note.standardsite n in
        C.Layout.page ~ctx ~title:(Bushel.Note.title n) ~description
          ~url:("/notes/" ^ slug) ?image ~og_type:"article"
          ~published ?modified ~tags ?citation ~jsonld ?standardsite
          ~page_scripts:[Toc; Lightbox; Links_modal]
          ~toc_sections:headings ~article:full_article ~sidebar ()
      | Some ent ->
        let article = C.Entry.full_body ~ctx ent in
        C.Layout.page ~ctx ~title:(Bushel.Entry.title ent) ~description:"" ~page_scripts:[Toc; Lightbox] ~article ())
    ~md_fn:(fun () ->
      match Arod.Ctx.lookup ctx slug with
      | None -> ""
      | Some ent -> C.Markdown_export.entry_to_markdown ~ctx ent)
  respond

let ideas_list ~ctx ~cache accept rctx (local_ respond) =
  let key = "/ideas" in
  negotiated ~cache ~key rctx accept
    ~html_fn:(fun () ->
      let article, sidebar = C.Idea.ideas_list ~ctx in
      let cfg = Arod.Ctx.config ctx in
      let base_url = cfg.site.base_url in
      let count = List.length (Arod.Ctx.ideas ctx) in
      let jsonld = [
        Arod.Jsonld.collection_page_jsonld ~base_url ~url:"/ideas" ~title:"Research Ideas" ~description:"Research ideas by year" ~count ();
        Arod.Jsonld.breadcrumb_jsonld ~base_url [("Home", "/"); ("Ideas", "/ideas")];
      ] in
      C.Layout.page ~ctx ~title:"Research Ideas" ~description:"Research ideas by year" ~url:"/ideas" ~current_page:"Ideas" ~jsonld ~page_scripts:[Ideas_calendar; Status_filter; Toc] ~article ~sidebar ())
    ~md_fn:(fun () -> C.Markdown_export.ideas_list_md ~ctx)
  respond

let idea ~ctx ~cache slug accept rctx (local_ respond) =
  if String.ends_with ~suffix:".md" slug then
    let real_slug = Filename.chop_extension slug in
    match Arod.Ctx.lookup ctx real_slug with
    | Some ent -> send_markdown respond (C.Markdown_export.entry_to_markdown ~ctx ent)
    | None -> not_found respond
  else
  let key = "/ideas/" ^ slug in
  negotiated ~cache ~key rctx accept
    ~html_fn:(fun () ->
      match Arod.Ctx.lookup ctx slug with
      | None -> ""
      | Some (`Idea i) ->
        let article_el, sidenotes, headings = C.Idea.full_page ~ctx i in
        let related = C.Sidebar.related_stream ~ctx i.Bushel.Idea.slug in
        let full_article = Htmlit.El.div [article_el; related] in
        let sidebar = C.Sidebar.for_entry ~ctx ~sidenotes (`Idea i) in
        let description = Option.value ~default:(Bushel.Idea.title i) (Bushel.Entry.synopsis (`Idea i)) in
        let published = Bushel.Entry.date (`Idea i) in
        let cfg = Arod.Ctx.config ctx in
        let entries = Arod.Ctx.entries ctx in
        let base_url = cfg.site.base_url in
        let image = match Bushel.Entry.thumbnail entries (`Idea i) with
          | Some t -> Some (base_url ^ t) | None -> None in
        let author_name = Arod.Ctx.author_name ctx in
        let tags = Bushel.Idea.tags i in
        let jsonld = [
          Arod.Jsonld.article_jsonld
            ~base_url ~url:("/ideas/" ^ slug)
            ~title:(Bushel.Idea.title i) ~description ~author_name
            ~date:published ~tags ();
          Arod.Jsonld.breadcrumb_jsonld ~base_url
            [("Home", "/"); ("Ideas", "/ideas"); (Bushel.Idea.title i, "/ideas/" ^ slug)];
        ] in
        C.Layout.page ~ctx ~title:(Bushel.Idea.title i) ~description
          ~url:("/ideas/" ^ slug) ?image ~og_type:"article" ~published ~jsonld
          ~page_scripts:[Toc; Links_modal]
          ~toc_sections:headings ~article:full_article ~sidebar ()
      | Some ent ->
        let article = C.Entry.full_body ~ctx ent in
        C.Layout.page ~ctx ~title:(Bushel.Entry.title ent) ~description:"" ~page_scripts:[Toc] ~article ())
    ~md_fn:(fun () ->
      match Arod.Ctx.lookup ctx slug with
      | None -> ""
      | Some ent -> C.Markdown_export.entry_to_markdown ~ctx ent)
  respond

let projects_list ~ctx ~cache accept rctx (local_ respond) =
  let key = "/projects" in
  negotiated ~cache ~key rctx accept
    ~html_fn:(fun () ->
      let article = C.Project.projects_list ~ctx in
      let cfg = Arod.Ctx.config ctx in
      let base_url = cfg.site.base_url in
      let count = List.length (Arod.Ctx.projects ctx) in
      let jsonld = [
        Arod.Jsonld.collection_page_jsonld ~base_url ~url:"/projects" ~title:"Projects" ~description:"Research projects" ~count ();
        Arod.Jsonld.breadcrumb_jsonld ~base_url [("Home", "/"); ("Projects", "/projects")];
      ] in
      C.Layout.wide_page ~ctx ~title:"Projects" ~description:"Research projects" ~url:"/projects" ~current_page:"Projects" ~jsonld ~page_scripts:[Masonry] ~article ())
    ~md_fn:(fun () -> C.Markdown_export.projects_list_md ~ctx)
  respond

let project ~ctx ~cache slug accept rctx (local_ respond) =
  if String.ends_with ~suffix:".md" slug then
    let real_slug = Filename.chop_extension slug in
    match Arod.Ctx.lookup ctx real_slug with
    | Some ent -> send_markdown respond (C.Markdown_export.entry_to_markdown ~ctx ent)
    | None -> not_found respond
  else
  let key = "/projects/" ^ slug in
  negotiated ~cache ~key rctx accept
    ~html_fn:(fun () ->
      match Arod.Ctx.lookup ctx slug with
      | None -> ""
      | Some (`Project p) ->
        let article, sidenotes = C.Project.full ~ctx p in
        let sidebar = C.Sidebar.for_entry ~ctx ~sidenotes (`Project p) in
        let description = Option.value ~default:(Bushel.Project.title p) (Bushel.Entry.synopsis (`Project p)) in
        let published = Bushel.Entry.date (`Project p) in
        let cfg = Arod.Ctx.config ctx in
        let base_url = cfg.site.base_url in
        let image = match Bushel.Entry.thumbnail (Arod.Ctx.entries ctx) (`Project p) with
          | Some t -> Some (base_url ^ t) | None -> None in
        let jsonld = [
          Arod.Jsonld.project_jsonld
            ~base_url ~url:("/projects/" ^ slug)
            ~title:(Bushel.Project.title p) ~description
            ~date_start:p.Bushel.Project.start
            ?date_end:p.Bushel.Project.finish
            ~tags:(Bushel.Project.tags p) ();
          Arod.Jsonld.breadcrumb_jsonld ~base_url
            [("Home", "/"); ("Projects", "/projects"); (Bushel.Project.title p, "/projects/" ^ slug)];
        ] in
        C.Layout.page ~ctx ~title:(Bushel.Project.title p) ~description
          ~url:("/projects/" ^ slug) ?image ~og_type:"article" ~published ~jsonld
          ~page_scripts:[Lightbox; Links_modal] ~article ~sidebar ()
      | Some ent ->
        let article = C.Entry.full_body ~ctx ent in
        C.Layout.page ~ctx ~title:(Bushel.Entry.title ent) ~description:"" ~page_scripts:[Lightbox] ~article ())
    ~md_fn:(fun () ->
      match Arod.Ctx.lookup ctx slug with
      | None -> ""
      | Some ent -> C.Markdown_export.entry_to_markdown ~ctx ent)
  respond

let videos_list ~ctx ~cache accept rctx (local_ respond) =
  let key = "/videos" in
  negotiated ~cache ~key rctx accept
    ~html_fn:(fun () ->
      let article = C.Video.videos_list ~ctx in
      let cfg = Arod.Ctx.config ctx in
      let base_url = cfg.site.base_url in
      let count = List.length (Arod.Ctx.videos ctx) in
      let jsonld = [
        Arod.Jsonld.collection_page_jsonld ~base_url ~url:"/videos" ~title:"Talks" ~description:"Conference talks and presentations" ~count ();
        Arod.Jsonld.breadcrumb_jsonld ~base_url [("Home", "/"); ("Talks", "/videos")];
      ] in
      C.Layout.wide_page ~ctx ~title:"Talks" ~description:"Conference talks and presentations" ~url:"/videos" ~current_page:"Talks" ~jsonld ~page_scripts:[Masonry; Pagination] ~article ())
    ~md_fn:(fun () -> C.Markdown_export.videos_list_md ~ctx)
  respond

let video ~ctx ~cache slug accept rctx (local_ respond) =
  if String.ends_with ~suffix:".md" slug then
    let real_slug = Filename.chop_extension slug in
    match Arod.Ctx.lookup ctx real_slug with
    | Some ent -> send_markdown respond (C.Markdown_export.entry_to_markdown ~ctx ent)
    | None -> not_found respond
  else
  let key = "/videos/" ^ slug in
  negotiated ~cache ~key rctx accept
    ~html_fn:(fun () ->
      match Arod.Ctx.lookup ctx slug with
      | None -> ""
      | Some (`Video v) ->
        let article_el, sidebar = C.Video.full_page ~ctx v in
        let related = C.Sidebar.related_stream ~ctx (Bushel.Video.slug v) in
        let article = Htmlit.El.div [article_el; related] in
        let description = Bushel.Video.description v in
        let published = Bushel.Entry.date (`Video v) in
        let datetime = Bushel.Entry.datetime (`Video v) in
        let cfg = Arod.Ctx.config ctx in
        let base_url = cfg.site.base_url in
        let image = match Bushel.Entry.thumbnail (Arod.Ctx.entries ctx) (`Video v) with
          | Some t -> Some (base_url ^ t) | None -> None in
        let jsonld = [
          Arod.Jsonld.video_jsonld
            ~base_url ~url:("/videos/" ^ slug)
            ~title:(Bushel.Video.title v) ~description
            ~datetime ?image
            ~embed_url:(Bushel.Video.url v)
            ~is_talk:(Bushel.Video.talk v) ();
          Arod.Jsonld.breadcrumb_jsonld ~base_url
            [("Home", "/"); ("Talks", "/videos"); (Bushel.Video.title v, "/videos/" ^ slug)];
        ] in
        C.Layout.page ~ctx ~title:(Bushel.Video.title v) ~description
          ~url:("/videos/" ^ slug) ?image ~og_type:"article" ~published ~jsonld
          ~page_scripts:[Lightbox; Links_modal] ~article ~sidebar ()
      | Some ent ->
        let article = C.Entry.full_body ~ctx ent in
        C.Layout.page ~ctx ~title:(Bushel.Entry.title ent) ~description:"" ~page_scripts:[Lightbox] ~article ())
    ~md_fn:(fun () ->
      match Arod.Ctx.lookup ctx slug with
      | None -> ""
      | Some ent -> C.Markdown_export.entry_to_markdown ~ctx ent)
  respond

let content ~ctx ~cache slug accept rctx (local_ respond) =
  if String.ends_with ~suffix:".md" slug then
    let real_slug = Filename.chop_extension slug in
    match Arod.Ctx.lookup ctx real_slug with
    | Some ent -> send_markdown respond (C.Markdown_export.entry_to_markdown ~ctx ent)
    | None -> not_found respond
  else
  let key = "/content/" ^ slug in
  negotiated ~cache ~key rctx accept
    ~html_fn:(fun () ->
      match Arod.Ctx.lookup ctx slug with
      | None -> ""
      | Some ent ->
        let article = C.Entry.full_body ~ctx ent in
        C.Layout.page ~ctx ~title:(Bushel.Entry.title ent) ~description:"" ~url:("/content/" ^ slug) ~page_scripts:[Toc; Lightbox] ~article ())
    ~md_fn:(fun () ->
      match Arod.Ctx.lookup ctx slug with
      | None -> ""
      | Some ent -> C.Markdown_export.entry_to_markdown ~ctx ent)
  respond

(** {1 Legacy Handlers} *)

let news_redirect slug _rctx (local_ respond) =
  R.redirect respond ~status:Httpz.Res.Moved_permanently ~location:("/notes/" ^ slug)

let links_list ~ctx ~cache accept rctx (local_ respond) =
  let key = "/links" in
  negotiated ~cache ~key rctx accept
    ~html_fn:(fun () ->
      let article, sidebar = C.Links.links_list ~ctx in
      C.Layout.page ~ctx ~title:"Links" ~description:"Outbound links" ~url:"/links" ~current_page:"Links" ~page_scripts:[Links_calendar; Link_filter; Links_modal; Pagination; Toc] ~article ~sidebar ())
    ~md_fn:(fun () -> C.Markdown_export.links_list_md ~ctx)
  respond

let network_page ~ctx ~cache accept rctx (local_ respond) =
  let key = "/network" in
  negotiated ~cache ~key rctx accept
    ~html_fn:(fun () ->
      let article, sidebar = C.Network.network_page ~ctx in
      C.Layout.page ~ctx ~title:"Network" ~description:"Network activity" ~url:"/network" ~current_page:"Network" ~page_scripts:[Network_calendar; Links_modal; Pagination; Toc] ~article ~sidebar ())
    ~md_fn:(fun () -> C.Markdown_export.network_md ~ctx)
  respond

(** {1 Feed Handlers} *)

(* Cached wrapper for atom feeds - uses atom content type *)
let cached_atom ~cache ~key rctx f (local_ respond) =
  if R.is_head rctx then
    send_atom_empty respond
  else
    match Arod.Cache.get cache key with
    | Some xml ->
      respond ~status:Httpz.Res.Success
        ~headers:[(Httpz.Header_name.Content_type, "application/atom+xml; charset=utf-8");
                   (Httpz.Header_name.X_cache, "hit")]
        (R.String xml)
    | None ->
      let xml = f () in
      Arod.Cache.set cache key xml;
      respond ~status:Httpz.Res.Success
        ~headers:[(Httpz.Header_name.Content_type, "application/atom+xml; charset=utf-8");
                   (Httpz.Header_name.X_cache, "miss")]
        (R.String xml)

(* Cached wrapper for json feeds *)
let cached_json ~cache ~key rctx f (local_ respond) =
  if R.is_head rctx then
    send_json_empty respond
  else
    match Arod.Cache.get cache key with
    | Some json ->
      respond ~status:Httpz.Res.Success
        ~headers:[(Httpz.Header_name.Content_type, "application/json; charset=utf-8");
                   (Httpz.Header_name.X_cache, "hit")]
        (R.String json)
    | None ->
      let json = f () in
      Arod.Cache.set cache key json;
      respond ~status:Httpz.Res.Success
        ~headers:[(Httpz.Header_name.Content_type, "application/json; charset=utf-8");
                   (Httpz.Header_name.X_cache, "miss")]
        (R.String json)

let atom_feed ~ctx ~cache rctx (local_ respond) =
  let path = R.path rctx in
  let key = "feed:" ^ path in
  cached_atom ~cache ~key rctx (fun () ->
    let cfg = Arod.Ctx.config ctx in
    let feed = Arod.Ctx.get_entries ctx ~types:[] in
    Arod.Feed.feed_string ~ctx cfg path feed
  ) respond

let json_feed ~ctx ~cache rctx (local_ respond) =
  let key = "feed:/feed.json" in
  cached_json ~cache ~key rctx (fun () ->
    let cfg = Arod.Ctx.config ctx in
    let feed = Arod.Ctx.get_entries ctx ~types:[] in
    Arod.Jsonfeed.feed_string ~ctx cfg "/feed.json" feed
  ) respond

let perma_atom ~ctx ~cache rctx (local_ respond) =
  let key = "feed:/perma.xml" in
  cached_atom ~cache ~key rctx (fun () ->
    let cfg = Arod.Ctx.config ctx in
    let feed = Arod.Ctx.perma_entries ctx in
    Arod.Feed.feed_string ~ctx cfg "/perma.xml" feed
  ) respond

let perma_json ~ctx ~cache rctx (local_ respond) =
  let key = "feed:/perma.json" in
  cached_json ~cache ~key rctx (fun () ->
    let cfg = Arod.Ctx.config ctx in
    let feed = Arod.Ctx.perma_entries ctx in
    Arod.Jsonfeed.feed_string ~ctx cfg "/perma.json" feed
  ) respond

(** {1 Utility Handlers (Dynamic - not cached)} *)

let sitemap ~ctx rctx (local_ respond) =
  R.xml_gen rctx respond (fun () ->
    let cfg = Arod.Ctx.config ctx in
    let all_feed =
      Arod.Ctx.all_entries ctx
      |> List.sort Entry.compare
      |> List.rev
    in
    let url_of_entry ent =
      let lastmod = Entry.date ent in
      let loc = cfg.site.base_url ^ Entry.site_url ent in
      Sitemap.v ~lastmod loc
    in
    List.map url_of_entry all_feed |> Sitemap.output
  )

let blogroll_opml ~ctx rctx (local_ respond) =
  let module Contact = Sortal_schema.Contact in
  let module Feed = Sortal_schema.Feed in
  let contacts = Arod.Ctx.contacts ctx in
  let contacts_with_feeds = List.filter_map (fun contact ->
    match Contact.feeds contact with
    | Some feeds when feeds <> [] -> Some (contact, feeds)
    | _ -> None
  ) contacts in
  let contacts_with_feeds = List.sort (fun (a, _) (b, _) ->
    String.compare (Contact.name a) (Contact.name b)
  ) contacts_with_feeds in
  let outlines = List.map (fun (contact, feeds) ->
    let name = Contact.name contact in
    let html_url = Option.map Uri.of_string (Contact.best_url contact) in
    let sub_outlines = List.map (fun feed ->
      let feed_type_str = match Feed.feed_type feed with
        | Feed.Atom -> "rss" | Feed.Rss -> "rss" | Feed.Json -> "rss"
        | Feed.Manual -> "rss"
      in
      Syndic.Opml1.outline ~typ:feed_type_str
        ~xml_url:(Uri.of_string (Feed.url feed))
        ?html_url
        (Option.value ~default:name (Feed.name feed))
    ) feeds in
    Syndic.Opml1.outline ?html_url ~outlines:sub_outlines name
  ) contacts_with_feeds in
  let head = Syndic.Opml1.head
    ~date_modified:(Ptime_clock.now ())
    ~owner_name:"Anil Madhavapeddy"
    ~owner_email:"anil@recoil.org"
    "Blogroll"
  in
  let opml : Syndic.Opml1.t = { version = "1.0"; head; body = outlines } in
  let buf = Buffer.create 4096 in
  Syndic.Opml1.output opml (`Buffer buf);
  send_file respond ~mime:"text/x-opml+xml; charset=utf-8" (Buffer.contents buf)

let slice_list offset limit l =
  List.filteri (fun i _ -> i >= offset && i < offset + limit) l

let error_json msg = Ezjsonm.to_string (`O [ ("error", `String msg) ])

let pagination_api ~ctx rctx (local_ respond) =
  R.json_gen rctx respond (fun () ->
    match R.query_param rctx "collection" with
    | None -> error_json "Missing collection parameter"
    | Some collection_type ->
    let offset =
      match R.query_param rctx "offset" with
      | Some o -> (match int_of_string_opt o with Some n -> max 0 n | None -> 0)
      | None -> 0
    in
    let limit =
      match R.query_param rctx "limit" with
      | Some l -> (match int_of_string_opt l with Some n -> min 100 (max 1 n) | None -> 25)
      | None -> 25
    in
    match collection_type with
    | "links" ->
      let all = C.Links.all_groups ~ctx in
      let total = List.length all in
      let offset = min offset (max 0 (total - 1)) in
      let slice = slice_list offset limit all in
      let count = max 0 (min limit (total - offset)) in
      let has_more = offset + count < total in
      let json =
        `O [
          ("html", `String (C.Links.render_groups_html ~ctx slice));
          ("total", `Float (float_of_int total));
          ("offset", `Float (float_of_int offset));
          ("limit", `Float (float_of_int limit));
          ("count", `Float (float_of_int count));
          ("has_more", `Bool has_more);
        ]
      in
      Ezjsonm.to_string json
    | "network" ->
      let all = C.Network.all_months ~ctx in
      let total = List.length all in
      let offset = min offset (max 0 (total - 1)) in
      let slice = slice_list offset limit all in
      let count = max 0 (min limit (total - offset)) in
      let has_more = offset + count < total in
      let json =
        `O [
          ("html", `String (C.Network.render_months_html ~ctx slice));
          ("total", `Float (float_of_int total));
          ("offset", `Float (float_of_int offset));
          ("limit", `Float (float_of_int limit));
          ("count", `Float (float_of_int count));
          ("has_more", `Bool has_more);
        ]
      in
      Ezjsonm.to_string json
    | ("feed" | "entries") as collection_type ->
      let type_strings = R.query_params rctx "type" in
      let types = List.filter_map C.List_view.entry_type_of_string type_strings in
      let all_items = C.List_view.get_entries ~ctx ~types in
      let total = List.length all_items in
      let offset = min offset (max 0 (total - 1)) in
      let slice = slice_list offset limit all_items in
      let render_fn = match collection_type with
        | "feed" -> C.List_view.render_feeds_html ~ctx
        | _ -> C.List_view.render_entries_html ~ctx
      in
      let count = max 0 (min limit (total - offset)) in
      let has_more = offset + count < total in
      let json =
        `O [
          ("html", `String (render_fn slice));
          ("total", `Float (float_of_int total));
          ("offset", `Float (float_of_int offset));
          ("limit", `Float (float_of_int limit));
          ("count", `Float (float_of_int count));
          ("has_more", `Bool has_more);
        ]
      in
      Ezjsonm.to_string json
    | _ -> error_json "Invalid collection type"
  )

let well_known ~ctx key rctx (local_ respond) =
  let cfg = Arod.Ctx.config ctx in
  match List.find_opt (fun e -> e.Arod.Config.key = key) cfg.well_known with
  | Some entry -> R.plain_gen rctx respond (fun () -> entry.value)
  | None -> not_found respond

let robots_txt ~ctx rctx (local_ respond) =
  let cfg = Arod.Ctx.config ctx in
  R.plain_gen rctx respond (fun () ->
    Printf.sprintf "User-agent: *\nAllow: /\n\nSitemap: %s/sitemap.xml\n"
      cfg.site.base_url)

(** {1 Search API} *)

let search_api ~ctx ~search rctx (local_ respond) =
  R.json_gen rctx respond (fun () ->
    let q = match R.query_param rctx "q" with Some q -> q | None -> "" in
    let limit = match R.query_param rctx "limit" with
      | Some l -> (match int_of_string_opt l with Some n -> min 100 (max 1 n) | None -> 20)
      | None -> 20 in
    Logs.info (fun m -> m "Search API: q=%S limit=%d" q limit);
    if q = "" then {|{"results":[]}|}
    else
      let entries = Arod.Ctx.entries ctx in
      let results = Arod_search.search search ~limit q in
      Logs.info (fun m -> m "Search API: %d results for %S" (List.length results) q);
      let json_results = List.map (fun (r : Arod_search.result) ->
        let parent_entries = List.filter_map (fun slug ->
          match Arod.Ctx.lookup ctx slug with
          | Some ent ->
            Some (`O [
              ("slug", `String slug);
              ("title", `String (Bushel.Entry.title ent));
              ("url", `String (Bushel.Entry.site_url ent));
              ("kind", `String (Bushel.Entry.to_type_string ent));
            ])
          | None -> None
        ) r.parent_slugs in
        let thumbnail = match r.kind with
          | "link" ->
            (match Arod.Ctx.link_for_url ctx r.url with
             | Some link ->
               let meta = match link.karakeep with Some k -> k.metadata | None -> [] in
               (match List.assoc_opt "favicon" meta with
                | Some f when f <> "" -> Some f
                | _ -> None)
             | None -> None)
          | _ ->
            (match Arod.Ctx.lookup ctx r.slug with
             | Some ent -> Bushel.Entry.thumbnail entries ent
             | None -> None)
        in
        `O ([ ("slug", `String r.slug); ("kind", `String r.kind);
              ("url", `String r.url); ("title", `String r.title);
              ("snippet", `String r.snippet); ("date", `String r.date) ]
             @ (if r.tags <> [] then
                  [("tags", `A (List.map (fun t -> `String t) r.tags))]
                else [])
             @ (match thumbnail with Some t -> [("thumbnail", `String t)] | None -> [])
             @ (if parent_entries <> [] then
                  [("parents", `A parent_entries)]
                else []))
      ) results in
      Ezjsonm.to_string (`O [("results", `A json_results)])
  )

(** {1 Route Collection} *)

let all_routes ~ctx ~cache ~search ~log ~fs =
  let cfg = Arod.Ctx.config ctx in
  let images_dir = Eio.Path.(fs / cfg.paths.images_dir) in
  let papers_dir = Eio.Path.(fs / cfg.paths.papers_dir) in
  let open R in
  let lits segs = List.fold_right lit segs root in
  of_list [
    (* Index routes — content-negotiated *)
    get_h1 root Accept (fun () -> index ~ctx ~cache);
    get_h1 (lits ["about"]) Accept (fun () -> index ~ctx ~cache);
    (* .md suffix routes for index pages *)
    get_ [ "index.md" ] (fun _rctx (local_ respond) ->
      send_markdown respond (C.Markdown_export.index_md ~ctx));
    get_ [ "papers.md" ] (fun _rctx (local_ respond) ->
      send_markdown respond (C.Markdown_export.papers_list_md ~ctx));
    get_ [ "notes.md" ] (fun _rctx (local_ respond) ->
      send_markdown respond (C.Markdown_export.notes_list_md ~ctx));
    get_ [ "ideas.md" ] (fun _rctx (local_ respond) ->
      send_markdown respond (C.Markdown_export.ideas_list_md ~ctx));
    get_ [ "projects.md" ] (fun _rctx (local_ respond) ->
      send_markdown respond (C.Markdown_export.projects_list_md ~ctx));
    get_ [ "videos.md" ] (fun _rctx (local_ respond) ->
      send_markdown respond (C.Markdown_export.videos_list_md ~ctx));
    get_ [ "links.md" ] (fun _rctx (local_ respond) ->
      send_markdown respond (C.Markdown_export.links_list_md ~ctx));
    get_ [ "network.md" ] (fun _rctx (local_ respond) ->
      send_markdown respond (C.Markdown_export.network_md ~ctx));
    (* Atom feeds *)
    get_ [ "news.xml" ] (atom_feed ~ctx ~cache);
    get_ [ "notes"; "atom.xml" ] (atom_feed ~ctx ~cache);
    (* Legacy feed redirects *)
    get_ [ "atom.xml" ] (fun _rctx (local_ respond) ->
      R.redirect respond ~status:Httpz.Res.Moved_permanently ~location:"/news.xml");
    get_ [ "feed.xml" ] (fun _rctx (local_ respond) ->
      R.redirect respond ~status:Httpz.Res.Moved_permanently ~location:"/news.xml");
    get_ [ "rss.xml" ] (fun _rctx (local_ respond) ->
      R.redirect respond ~status:Httpz.Res.Moved_permanently ~location:"/news.xml");
    get_ [ "wiki.xml" ] (fun _rctx (local_ respond) ->
      R.redirect respond ~status:Httpz.Res.Moved_permanently ~location:"/news.xml");
    get_ [ "feeds"; "atom.xml" ] (fun _rctx (local_ respond) ->
      R.redirect respond ~status:Httpz.Res.Moved_permanently ~location:"/news.xml");
    get_ [ "perma.xml" ] (perma_atom ~ctx ~cache);
    (* JSON feeds *)
    get_ [ "feed.json" ] (json_feed ~ctx ~cache);
    get_ [ "feeds"; "feed.json" ] (json_feed ~ctx ~cache);
    get_ [ "notes"; "feed.json" ] (json_feed ~ctx ~cache);
    get_ [ "perma.json" ] (perma_json ~ctx ~cache);
    (* Sitemap *)
    get_ [ "sitemap.xml" ] (sitemap ~ctx);
    (* Papers — content-negotiated *)
    get_h1 ("papers" / seg root) Accept (fun (slug, ()) -> paper ~ctx ~cache ~papers_dir slug);
    get_h1 (lits ["papers"]) Accept (fun () -> papers_list ~ctx ~cache);
    (* Ideas — content-negotiated *)
    get_h1 ("ideas" / seg root) Accept (fun (slug, ()) -> idea ~ctx ~cache slug);
    get_h1 (lits ["ideas"]) Accept (fun () -> ideas_list ~ctx ~cache);
    (* Notes — content-negotiated *)
    get_h1 ("notes" / seg root) Accept (fun (slug, ()) -> note ~ctx ~cache slug);
    get_h1 (lits ["notes"]) Accept (fun () -> notes_list ~ctx ~cache);
    (* Videos/Talks — content-negotiated *)
    get_h1 ("videos" / seg root) Accept (fun (slug, ()) -> video ~ctx ~cache slug);
    get_h1 (lits ["talks"]) Accept (fun () -> videos_list ~ctx ~cache);
    get_h1 (lits ["videos"]) Accept (fun () -> videos_list ~ctx ~cache);
    (* Projects — content-negotiated *)
    get_h1 ("projects" / seg root) Accept (fun (slug, ()) -> project ~ctx ~cache slug);
    get_h1 (lits ["projects"]) Accept (fun () -> projects_list ~ctx ~cache);
    (* Tag search redirect — handles /tags/foo from ##tag markdown links *)
    get ("tags" / seg root) (fun (tag, ()) _rctx (local_ respond) ->
      R.redirect respond ~status:Httpz.Res.Found ~location:("/#tag=" ^ tag));
    (* Legacy news redirect *)
    get ("news" / seg root) (fun (slug, ()) -> news_redirect slug);
    (* Links and Feeds — content-negotiated *)
    get_h1 (lits ["links"]) Accept (fun () -> links_list ~ctx ~cache);
    get_h1 (lits ["network"]) Accept (fun () -> network_page ~ctx ~cache);
    get_ [ "network"; "blogroll.opml" ] (blogroll_opml ~ctx);
    get_ ["feeds"] (fun _rctx (local_ respond) ->
      R.redirect respond ~status:Httpz.Res.Moved_permanently ~location:"/network");
    (* Wiki/News legacy — redirects *)
    get_ ["wiki"] (fun _rctx (local_ respond) ->
      R.redirect respond ~status:Httpz.Res.Moved_permanently ~location:"/notes");
    get_ ["news"] (fun _rctx (local_ respond) ->
      R.redirect respond ~status:Httpz.Res.Moved_permanently ~location:"/notes");
    (* Pagination API - dynamic, not cached *)
    get_ [ "api"; "entries" ] (pagination_api ~ctx);
    (* Search API - dynamic, not cached *)
    get_ [ "api"; "search" ] (search_api ~ctx ~search);
    (* Well-known endpoints *)
    get (".well-known" / seg root) (fun (key, ()) -> well_known ~ctx key);
    (* Robots.txt *)
    get_ [ "robots.txt" ] (robots_txt ~ctx);
    (* Embedded favicon/asset routes *)
    get_ [ "favicon.svg" ] (fun rctx respond -> embedded_file "favicon.svg" rctx respond);
    get_ [ "favicon.ico" ] (fun rctx respond -> embedded_file "favicon.ico" rctx respond);
    get_ [ "favicon.png" ] (fun rctx respond -> embedded_file "favicon-32x32.png" rctx respond);
    get_ [ "favicon-32x32.png" ] (fun rctx respond -> embedded_file "favicon-32x32.png" rctx respond);
    get_ [ "favicon-16x16.png" ] (fun rctx respond -> embedded_file "favicon-16x16.png" rctx respond);
    get_ [ "apple-touch-icon.png" ] (fun rctx respond -> embedded_file "apple-touch-icon.png" rctx respond);
    get_ [ "site.webmanifest" ] (fun rctx respond -> embedded_file "site.webmanifest" rctx respond);
    (* Stats dashboard — hidden, not cached, not in sitemap, HTTP Basic auth *)
    get_h1 (lits ["action"]) Authorization (fun () auth rctx (local_ respond) ->
      if not (check_stats_auth cfg auth) then send_auth_challenge respond
      else
        let db = Arod_log.db log in
        let html = Arod_handlers_stats.render_dashboard db in
        if R.is_head rctx then send_html_empty respond
        else send_html respond html);
    get_h1 (lits ["action"; "api"; "overview"]) Authorization (fun () auth rctx (local_ respond) ->
      if not (check_stats_auth cfg auth) then send_auth_challenge respond
      else
        let db = Arod_log.db log in
        R.json_gen rctx respond (fun () -> Arod_handlers_stats.overview_json db));
    get_h1 (lits ["action"; "api"; "traffic"]) Authorization (fun () auth rctx (local_ respond) ->
      if not (check_stats_auth cfg auth) then send_auth_challenge respond
      else
        let db = Arod_log.db log in
        R.json_gen rctx respond (fun () -> Arod_handlers_stats.traffic_json db));
    get_h1 (lits ["action"; "api"; "recent"]) Authorization (fun () auth rctx (local_ respond) ->
      if not (check_stats_auth cfg auth) then send_auth_challenge respond
      else
        let db = Arod_log.db log in
        R.json_gen rctx respond (fun () -> Arod_handlers_stats.recent_json db));
    (* Redirect /collection/slug/index.html to canonical /collection/slug *)
    get_ [ "index.html" ] (fun _rctx (local_ respond) ->
      R.redirect respond ~status:Httpz.Res.Moved_permanently ~location:"/");
    get ("papers" / lit "index.html" root) (fun () _rctx (local_ respond) ->
      R.redirect respond ~status:Httpz.Res.Moved_permanently ~location:"/papers");
    get ("papers" / seg (lit "index.html" root)) (fun (slug, ()) _rctx (local_ respond) ->
      R.redirect respond ~status:Httpz.Res.Moved_permanently ~location:("/papers/" ^ slug));
    get ("notes" / lit "index.html" root) (fun () _rctx (local_ respond) ->
      R.redirect respond ~status:Httpz.Res.Moved_permanently ~location:"/notes");
    get ("notes" / seg (lit "index.html" root)) (fun (slug, ()) _rctx (local_ respond) ->
      R.redirect respond ~status:Httpz.Res.Moved_permanently ~location:("/notes/" ^ slug));
    get ("ideas" / lit "index.html" root) (fun () _rctx (local_ respond) ->
      R.redirect respond ~status:Httpz.Res.Moved_permanently ~location:"/ideas");
    get ("ideas" / seg (lit "index.html" root)) (fun (slug, ()) _rctx (local_ respond) ->
      R.redirect respond ~status:Httpz.Res.Moved_permanently ~location:("/ideas/" ^ slug));
    get ("projects" / lit "index.html" root) (fun () _rctx (local_ respond) ->
      R.redirect respond ~status:Httpz.Res.Moved_permanently ~location:"/projects");
    get ("projects" / seg (lit "index.html" root)) (fun (slug, ()) _rctx (local_ respond) ->
      R.redirect respond ~status:Httpz.Res.Moved_permanently ~location:("/projects/" ^ slug));
    get ("videos" / lit "index.html" root) (fun () _rctx (local_ respond) ->
      R.redirect respond ~status:Httpz.Res.Moved_permanently ~location:"/videos");
    get ("videos" / seg (lit "index.html" root)) (fun (slug, ()) _rctx (local_ respond) ->
      R.redirect respond ~status:Httpz.Res.Moved_permanently ~location:("/videos/" ^ slug));
    (* Static files - not cached *)
    get ("images" / tail) (fun path -> static_file ~dir:images_dir (String.concat "/" path));
  ]
