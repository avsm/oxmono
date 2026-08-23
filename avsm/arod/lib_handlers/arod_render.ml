(*---------------------------------------------------------------------------
  Copyright (c) 2025 Anil Madhavapeddy <anil@recoil.org>. All rights reserved.
  SPDX-License-Identifier: ISC
 ---------------------------------------------------------------------------*)

(** Rendering, separated from serving. *)

module Entry = Bushel.Entry
module Paper = Bushel.Paper
module C = Arod_component

type flavour = [ `Html | `Markdown ]

type listing =
  [ `Index
  | `Papers
  | `Notes
  | `Ideas
  | `Projects
  | `Videos
  | `Links
  | `Network ]

type entry_kind = [ `Paper | `Note | `Idea | `Project | `Video ]
type feed = [ `Atom of string | `Json | `Perma_atom | `Perma_json ]
type report = [ `Dashboard | `Overview | `Traffic | `Recent ]

(** {1 Index} *)

let index_html ~ctx =
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
    C.Layout.page ~ctx ~title:(Bushel.Entry.title ent) ~description:"" ~url:"/" ~current_page:"About" ~jsonld ~page_scripts:[] ~article ~sidebar ~mobile_footer:socials ()

(** {1 List pages} *)

let papers_list_html ~ctx =
  let article, sidebar = C.Paper.papers_list ~ctx in
  let cfg = Arod.Ctx.config ctx in
  let base_url = cfg.site.base_url in
  let count = List.length (Arod.Ctx.papers ctx) in
  let jsonld = [
    Arod.Jsonld.collection_page_jsonld ~base_url ~url:"/papers" ~title:"Papers" ~description:"Academic papers" ~count ();
    Arod.Jsonld.breadcrumb_jsonld ~base_url [("Home", "/"); ("Papers", "/papers")];
  ] in
  C.Layout.page ~ctx ~title:"Papers" ~description:"Academic papers" ~url:"/papers" ~current_page:"Papers" ~jsonld ~page_scripts:[Calendar; Checkbox_filter; Tag_cloud_filter; Pagination; Toc] ~article ~sidebar ()

let notes_list_html ~ctx =
  let article, sidebar = C.Note.notes_list ~ctx in
  let cfg = Arod.Ctx.config ctx in
  let base_url = cfg.site.base_url in
  let count = List.length (Arod.Ctx.notes ctx) in
  let jsonld = [
    Arod.Jsonld.collection_page_jsonld ~base_url ~url:"/notes" ~title:"Notes" ~description:"Notes and blog posts" ~count ();
    Arod.Jsonld.breadcrumb_jsonld ~base_url [("Home", "/"); ("Notes", "/notes")];
  ] in
  C.Layout.page ~ctx ~title:"Notes" ~description:"Notes and blog posts" ~url:"/notes" ~current_page:"Notes" ~jsonld ~page_scripts:[Pagination; Toc] ~main_cls:"max-w-4xl" ~article ~sidebar ()

let ideas_list_html ~ctx =
  let article, sidebar = C.Idea.ideas_list ~ctx in
  let cfg = Arod.Ctx.config ctx in
  let base_url = cfg.site.base_url in
  let count = List.length (Arod.Ctx.ideas ctx) in
  let jsonld = [
    Arod.Jsonld.collection_page_jsonld ~base_url ~url:"/ideas" ~title:"Research Ideas" ~description:"Research ideas by year" ~count ();
    Arod.Jsonld.breadcrumb_jsonld ~base_url [("Home", "/"); ("Ideas", "/ideas")];
  ] in
  C.Layout.page ~ctx ~title:"Research Ideas" ~description:"Research ideas by year" ~url:"/ideas" ~current_page:"Ideas" ~jsonld ~page_scripts:[Checkbox_filter; Toc] ~article ~sidebar ()

let projects_list_html ~ctx =
  let article = C.Project.projects_list ~ctx in
  let cfg = Arod.Ctx.config ctx in
  let base_url = cfg.site.base_url in
  let count = List.length (Arod.Ctx.projects ctx) in
  let jsonld = [
    Arod.Jsonld.collection_page_jsonld ~base_url ~url:"/projects" ~title:"Projects" ~description:"Research projects" ~count ();
    Arod.Jsonld.breadcrumb_jsonld ~base_url [("Home", "/"); ("Projects", "/projects")];
  ] in
  C.Layout.wide_page ~ctx ~title:"Projects" ~description:"Research projects" ~url:"/projects" ~current_page:"Projects" ~jsonld ~article ()

let videos_list_html ~ctx =
  let article = C.Video.videos_list ~ctx in
  let cfg = Arod.Ctx.config ctx in
  let base_url = cfg.site.base_url in
  let count = List.length (Arod.Ctx.videos ctx) in
  let jsonld = [
    Arod.Jsonld.collection_page_jsonld ~base_url ~url:"/videos" ~title:"Talks" ~description:"Conference talks and presentations" ~count ();
    Arod.Jsonld.breadcrumb_jsonld ~base_url [("Home", "/"); ("Talks", "/videos")];
  ] in
  C.Layout.wide_page ~ctx ~title:"Talks" ~description:"Conference talks and presentations" ~url:"/videos" ~current_page:"Talks" ~jsonld ~page_scripts:[Pagination] ~article ()

let links_list_html ~ctx =
  let article, sidebar = C.Links.links_list ~ctx in
  C.Layout.page ~ctx ~title:"Links" ~description:"Outbound links" ~url:"/links" ~current_page:"Links" ~page_scripts:[Calendar; Checkbox_filter; Links_modal; Pagination; Toc] ~article ~sidebar ()

let network_html ~ctx =
  let article, sidebar = C.Network.network_page ~ctx in
  C.Layout.page ~ctx ~title:"Network" ~description:"Network activity" ~url:"/network" ~current_page:"Network" ~page_scripts:[Calendar; Links_modal; Pagination; Toc] ~article ~sidebar ()

let listing ~ctx (which : listing) (flavour : flavour) =
  match (which, flavour) with
  | `Index, `Html -> index_html ~ctx
  | `Index, `Markdown -> C.Markdown_export.index_md ~ctx
  | `Papers, `Html -> papers_list_html ~ctx
  | `Papers, `Markdown -> C.Markdown_export.papers_list_md ~ctx
  | `Notes, `Html -> notes_list_html ~ctx
  | `Notes, `Markdown -> C.Markdown_export.notes_list_md ~ctx
  | `Ideas, `Html -> ideas_list_html ~ctx
  | `Ideas, `Markdown -> C.Markdown_export.ideas_list_md ~ctx
  | `Projects, `Html -> projects_list_html ~ctx
  | `Projects, `Markdown -> C.Markdown_export.projects_list_md ~ctx
  | `Videos, `Html -> videos_list_html ~ctx
  | `Videos, `Markdown -> C.Markdown_export.videos_list_md ~ctx
  | `Links, `Html -> links_list_html ~ctx
  | `Links, `Markdown -> C.Markdown_export.links_list_md ~ctx
  | `Network, `Html -> network_html ~ctx
  | `Network, `Markdown -> C.Markdown_export.network_md ~ctx

(** {1 Entry pages} *)

(* An entry whose type does not match the collection it was asked for still
   renders, as a bare page. A slug that names nothing renders as the empty
   string, which is what the route table has always answered with. *)
let generic_html ~ctx ~page_scripts ent =
  let article = C.Entry.full_body ~ctx ent in
  C.Layout.page ~ctx ~title:(Bushel.Entry.title ent) ~description:""
    ~page_scripts ~article ()

let paper_html ~ctx slug p =
  let cfg = Arod.Ctx.config ctx in
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
      ?journal:journal
      ~tags ();
    Arod.Jsonld.breadcrumb_jsonld ~base_url
      [("Home", "/"); ("Papers", "/papers"); (Paper.title p, "/papers/" ^ slug)];
  ] in
  C.Layout.page ~ctx ~title:(Paper.title p) ~description
    ~url:("/papers/" ^ slug) ?image ~og_type:"article"
    ~published ~tags ~citation ~jsonld ~page_scripts:[Toc; Lightbox; Links_modal] ~article ~sidebar ()

let note_html ~ctx slug n =
  let article_el, sidenotes, headings = C.Note.full_page ~ctx n in
  let refs = C.Note.references ~ctx n in
  let related = C.Sidebar.related_stream ~ctx (Bushel.Note.slug n) in
  let full_article = Htmlit.El.div [article_el; refs; related] in
  let sidebar = C.Sidebar.for_entry ~ctx ~sidenotes ~toc:headings (`Note n) in
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
    ~article:full_article ~sidebar ()

let idea_html ~ctx slug i =
  let article_el, sidenotes, headings = C.Idea.full_page ~ctx i in
  let related = C.Sidebar.related_stream ~ctx i.Bushel.Idea.slug in
  let full_article = Htmlit.El.div [article_el; related] in
  let sidebar = C.Sidebar.for_entry ~ctx ~sidenotes ~toc:headings (`Idea i) in
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
    ~article:full_article ~sidebar ()

let project_html ~ctx slug p =
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

let video_html ~ctx slug v =
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

let entry_html ~ctx (kind : entry_kind) slug =
  match (kind, Arod.Ctx.lookup ctx slug) with
  | _, None -> ""
  | `Paper, Some (`Paper p) -> paper_html ~ctx slug p
  | `Note, Some (`Note n) -> note_html ~ctx slug n
  | `Idea, Some (`Idea i) -> idea_html ~ctx slug i
  | `Project, Some (`Project p) -> project_html ~ctx slug p
  | `Video, Some (`Video v) -> video_html ~ctx slug v
  | `Paper, Some ent -> generic_html ~ctx ~page_scripts:[Toc; Lightbox] ent
  | `Note, Some ent -> generic_html ~ctx ~page_scripts:[Toc; Lightbox] ent
  | `Idea, Some ent -> generic_html ~ctx ~page_scripts:[Toc] ent
  | (`Project | `Video), Some ent ->
    generic_html ~ctx ~page_scripts:[Lightbox] ent

let entry_markdown ~ctx slug =
  match Arod.Ctx.lookup ctx slug with
  | Some ent -> Some (C.Markdown_export.entry_to_markdown ~ctx ent)
  | None -> None

let entry ~ctx kind slug (flavour : flavour) =
  match flavour with
  | `Html -> entry_html ~ctx kind slug
  | `Markdown -> Option.value ~default:"" (entry_markdown ~ctx slug)

let paper_bib ~ctx slug =
  match Arod.Ctx.lookup ctx slug with
  | Some (`Paper p) -> Some (Paper.bib p)
  | _ -> None

(** {1 Feeds} *)

let feed ~ctx (which : feed) =
  let cfg = Arod.Ctx.config ctx in
  match which with
  | `Atom path ->
    Arod.Feed.feed_string ~ctx cfg path (Arod.Ctx.get_entries ctx ~types:[])
  | `Json ->
    Arod.Jsonfeed.feed_string ~ctx cfg "/feed.json"
      (Arod.Ctx.get_entries ctx ~types:[])
  | `Perma_atom ->
    Arod.Feed.feed_string ~ctx cfg "/perma.xml" (Arod.Ctx.perma_entries ctx)
  | `Perma_json ->
    Arod.Jsonfeed.feed_string ~ctx cfg "/perma.json"
      (Arod.Ctx.perma_entries ctx)

(** {1 Machine-readable pages} *)

let sitemap ~ctx =
  let cfg = Arod.Ctx.config ctx in
  let all_feed =
    Arod.Ctx.all_entries ctx |> List.sort Entry.compare |> List.rev
  in
  let url_of_entry ent =
    let lastmod = Entry.date ent in
    let loc = cfg.site.base_url ^ Entry.site_url ent in
    Sitemap.v ~lastmod loc
  in
  List.map url_of_entry all_feed |> Sitemap.output

let blogroll ~ctx =
  let module Contact = Sortal_schema.Contact in
  let module Feed = Sortal_schema.Feed in
  let contacts = Arod.Ctx.contacts ctx in
  let contacts_with_feeds = List.filter_map (fun contact ->
    match Contact.feeds contact with
    | feeds when feeds <> [] -> Some (contact, feeds)
    | _ -> None
  ) contacts in
  let contacts_with_feeds = List.sort (fun (a, _) (b, _) ->
    String.compare (Contact.name a) (Contact.name b)
  ) contacts_with_feeds in
  let outlines = List.map (fun (contact, feeds) ->
    let name = Contact.name contact in
    let html_url =
      Option.map Syndic.XML.uri_of_string (Contact.best_url contact)
    in
    let sub_outlines = List.map (fun feed ->
      let feed_type_str = match Feed.feed_type feed with
        | Feed.Atom -> "rss" | Feed.Rss -> "rss" | Feed.Json -> "rss"
        | Feed.Manual -> "rss"
      in
      Syndic.Opml1.outline ~typ:feed_type_str
        ~xml_url:(Syndic.XML.uri_of_string (Feed.url feed))
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
  Buffer.contents buf

(** {1 JSON APIs} *)

let slice_list offset limit l =
  List.filteri (fun i _ -> i >= offset && i < offset + limit) l

let error_codec =
  Jsont.Object.map ~kind:"error" Fun.id
  |> Jsont.Object.mem "error" Jsont.string ~enc:Fun.id
  |> Jsont.Object.finish

let error_json msg = Arod_json.stream error_codec msg

type page = {
  html : string;
  total : int;
  offset : int;
  limit : int;
  count : int;
  has_more : bool;
}

let page_codec =
  Jsont.Object.map ~kind:"page"
    (fun html total offset limit count has_more ->
      { html; total; offset; limit; count; has_more })
  |> Jsont.Object.mem "html" Jsont.string ~enc:(fun p -> p.html)
  |> Jsont.Object.mem "total" Jsont.int ~enc:(fun p -> p.total)
  |> Jsont.Object.mem "offset" Jsont.int ~enc:(fun p -> p.offset)
  |> Jsont.Object.mem "limit" Jsont.int ~enc:(fun p -> p.limit)
  |> Jsont.Object.mem "count" Jsont.int ~enc:(fun p -> p.count)
  |> Jsont.Object.mem "has_more" Jsont.bool ~enc:(fun p -> p.has_more)
  |> Jsont.Object.finish

(* The record is built now and encoded later, when the backend runs the
   writer. The page HTML is the expensive half and is already a string by the
   time it lands in [html], but the JSON around it never becomes one. *)
let page_json ~html ~total ~offset ~limit ~count ~has_more =
  Arod_json.stream page_codec { html; total; offset; limit; count; has_more }

let pagination ~ctx ~collection ~offset ~limit ~types =
  let paginate all render =
    let total = List.length all in
    let offset = min offset (max 0 (total - 1)) in
    let slice = slice_list offset limit all in
    let count = max 0 (min limit (total - offset)) in
    page_json ~html:(render slice) ~total ~offset ~limit ~count
      ~has_more:(offset + count < total)
  in
  match collection with
  | None -> error_json "Missing collection parameter"
  | Some "links" ->
    paginate (C.Links.all_groups ~ctx) (C.Links.render_groups_html ~ctx)
  | Some "network" ->
    paginate (C.Network.all_months ~ctx) (C.Network.render_months_html ~ctx)
  | Some (("feed" | "entries") as collection_type) ->
    let types = List.filter_map C.List_view.entry_type_of_string types in
    let render =
      match collection_type with
      | "feed" -> C.List_view.render_feeds_html ~ctx
      | _ -> C.List_view.render_entries_html ~ctx
    in
    paginate (C.List_view.get_entries ~ctx ~types) render
  | Some _ -> error_json "Invalid collection type"

(* A hit and the entries it hangs under share four member names, so each shape
   is its own module rather than two record types competing for them. *)
module Search_parent = struct
  type t = { slug : string; title : string; url : string; kind : string }

  let codec =
    Jsont.Object.map ~kind:"parent" (fun slug title url kind ->
      { slug; title; url; kind })
    |> Jsont.Object.mem "slug" Jsont.string ~enc:(fun p -> p.slug)
    |> Jsont.Object.mem "title" Jsont.string ~enc:(fun p -> p.title)
    |> Jsont.Object.mem "url" Jsont.string ~enc:(fun p -> p.url)
    |> Jsont.Object.mem "kind" Jsont.string ~enc:(fun p -> p.kind)
    |> Jsont.Object.finish
end

module Search_hit = struct
  type t = {
    slug : string;
    kind : string;
    url : string;
    title : string;
    snippet : string;
    date : string;
    tags : string list;
    thumbnail : string option;
    parents : Search_parent.t list;
  }

  let codec =
    Jsont.Object.map ~kind:"hit"
      (fun slug kind url title snippet date tags thumbnail parents ->
        { slug; kind; url; title; snippet; date; tags; thumbnail; parents })
    |> Jsont.Object.mem "slug" Jsont.string ~enc:(fun h -> h.slug)
    |> Jsont.Object.mem "kind" Jsont.string ~enc:(fun h -> h.kind)
    |> Jsont.Object.mem "url" Jsont.string ~enc:(fun h -> h.url)
    |> Jsont.Object.mem "title" Jsont.string ~enc:(fun h -> h.title)
    |> Jsont.Object.mem "snippet" Jsont.string ~enc:(fun h -> h.snippet)
    |> Jsont.Object.mem "date" Jsont.string ~enc:(fun h -> h.date)
    |> Jsont.Object.mem "tags" (Jsont.list Jsont.string)
         ~enc:(fun h -> h.tags) ~enc_omit:(fun tags -> tags = [])
    |> Jsont.Object.opt_mem "thumbnail" Jsont.string
         ~enc:(fun h -> h.thumbnail)
    |> Jsont.Object.mem "parents" (Jsont.list Search_parent.codec)
         ~enc:(fun h -> h.parents) ~enc_omit:(fun parents -> parents = [])
    |> Jsont.Object.finish
end

module Search_goto = struct
  type t = { label : string; url : string; detail : string; kind : string }

  let codec =
    Jsont.Object.map ~kind:"goto" (fun label url detail kind ->
      { label; url; detail; kind })
    |> Jsont.Object.mem "label" Jsont.string ~enc:(fun g -> g.label)
    |> Jsont.Object.mem "url" Jsont.string ~enc:(fun g -> g.url)
    |> Jsont.Object.mem "detail" Jsont.string ~enc:(fun g -> g.detail)
    |> Jsont.Object.mem "kind" Jsont.string ~enc:(fun g -> g.kind)
    |> Jsont.Object.finish
end

(* A facet is a name and a count. The member naming the thing counted
   differs per facet, so one codec is built per member name. *)
let count_codec ~kind name =
  Jsont.Object.map ~kind (fun k n -> (k, n))
  |> Jsont.Object.mem name Jsont.string ~enc:fst
  |> Jsont.Object.mem "count" Jsont.int ~enc:snd
  |> Jsont.Object.finish

let year_codec =
  Jsont.Object.map ~kind:"year" (fun y n -> (y, n))
  |> Jsont.Object.mem "year" Jsont.int ~enc:fst
  |> Jsont.Object.mem "count" Jsont.int ~enc:snd
  |> Jsont.Object.finish

module Search_response = struct
  type t = {
    goto : Search_goto.t list;
    work : Search_hit.t list;
    work_total : int;
    links : Search_hit.t list;
    links_total : int;
    kinds : (string * int) list;
    years : (int * int) list;
    tags : (string * int) list;
  }

  let codec =
    Jsont.Object.map ~kind:"results"
      (fun goto work work_total links links_total kinds years tags ->
        { goto; work; work_total; links; links_total; kinds; years; tags })
    |> Jsont.Object.mem "goto" (Jsont.list Search_goto.codec)
         ~enc:(fun r -> r.goto)
    |> Jsont.Object.mem "work" (Jsont.list Search_hit.codec)
         ~enc:(fun r -> r.work)
    |> Jsont.Object.mem "work_total" Jsont.int ~enc:(fun r -> r.work_total)
    |> Jsont.Object.mem "links" (Jsont.list Search_hit.codec)
         ~enc:(fun r -> r.links)
    |> Jsont.Object.mem "links_total" Jsont.int
         ~enc:(fun r -> r.links_total)
    |> Jsont.Object.mem "kinds" (Jsont.list (count_codec ~kind:"kind" "kind"))
         ~enc:(fun r -> r.kinds)
    |> Jsont.Object.mem "years" (Jsont.list year_codec) ~enc:(fun r -> r.years)
    |> Jsont.Object.mem "tags" (Jsont.list (count_codec ~kind:"tag" "tag"))
         ~enc:(fun r -> r.tags)
    |> Jsont.Object.finish
end

let search_hit ~ctx (r : Arod_search.hit) =
  let entries = Arod.Ctx.entries ctx in
  let parents = List.filter_map (fun slug ->
    match Arod.Ctx.lookup ctx slug with
    | Some ent ->
      Some {
        Search_parent.slug;
        title = Bushel.Entry.title ent;
        url = Bushel.Entry.site_url ent;
        kind = Bushel.Entry.to_type_string ent;
      }
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
  { Search_hit.slug = r.slug; kind = r.kind; url = r.url; title = r.title;
    snippet = r.snippet; date = r.date; tags = r.tags; thumbnail; parents }

let goto_kind_string = function
  | `Section -> "section" | `Project -> "project" | `Tag -> "tag"

let search ~ctx (r : Arod_search.results) =
  let goto = List.map (fun (g : Arod_search.goto) ->
    { Search_goto.label = g.label; url = g.url; detail = g.detail;
      kind = goto_kind_string g.goto_kind }) r.goto in
  Arod_json.stream Search_response.codec
    { Search_response.goto; work = List.map (search_hit ~ctx) r.work;
      work_total = r.work_total; links = List.map (search_hit ~ctx) r.links;
      links_total = r.links_total; kinds = r.kinds; years = r.years;
      tags = r.tags }

(** {1 Stats dashboard} *)

let report ~db (which : report) ~range =
  let range = Arod_handlers_stats.range_of_string range in
  match which with
  | `Dashboard -> Arod_handlers_stats.render_dashboard db range
  | `Overview -> Arod_handlers_stats.overview_json db range
  | `Traffic -> Arod_handlers_stats.traffic_json db range
  | `Recent -> Arod_handlers_stats.recent_json db
