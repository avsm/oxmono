(*---------------------------------------------------------------------------
  Copyright (c) 2025 Anil Madhavapeddy <anil@recoil.org>. All rights reserved.
  SPDX-License-Identifier: ISC
 ---------------------------------------------------------------------------*)

(** Page shell layout component for the Arod website.

    Provides the overall page structure: head meta tags, header navigation,
    content grid with optional sidebar, footer, and scripts. *)

open Htmlit

(** {1 Footer} *)

let footer_el ~ctx ?url () =
  let module Contact = Sortal_schema.Contact in
  let open Arod.Icons in
  let icon_link ~icon ~title url =
    El.a ~at:[At.href url; At.v "title" title;
             At.rel "me";
             At.class' "opacity-50 hover:opacity-100 transition-opacity"]
      [El.unsafe_raw icon]
  in
  let photo_icon a =
    match Contact.Account.platform a with
    | Simple Instagram ->
      Some (icon_link ~icon:(brand ~size:14 instagram_brand)
        ~title:"Instagram" (Contact.Account.url a))
    | Simple Flickr ->
      Some (icon_link ~icon:(brand ~size:14 flickr_brand)
        ~title:"Flickr" (Contact.Account.url a))
    | _ -> None
  in
  let social_icons = match Arod.Ctx.author ctx with
    | None -> []
    | Some author_contact ->
      let photo_icons = List.filter_map photo_icon
        (List.concat_map (Contact.accounts_on author_contact)
           [ Simple Instagram; Simple Flickr ]) in
      let main_icons = List.filter_map Fun.id [
        (match Contact.handle_on author_contact (Simple Github) with
         | Some g -> Some (icon_link ~icon:(brand ~size:14 github_brand)
             ~title:"GitHub" ("https://github.com/" ^ g))
         | None -> None);
        (match Contact.atproto_handle author_contact with
         | Some b -> Some (icon_link ~icon:(brand ~size:14 bluesky_brand)
             ~title:"Bluesky" ("https://bsky.app/profile/" ^ b))
         | None -> None);
        (match Contact.account_on author_contact (Federated Mastodon) with
         | Some svc -> Some (icon_link ~icon:(brand ~size:14 mastodon_brand)
             ~title:"Mastodon" (Contact.Account.url svc))
         | None -> None);
        (match Contact.account_on author_contact (Federated PeerTube) with
         | Some svc -> Some (icon_link ~icon:(brand ~size:14 peertube_brand)
             ~title:"PeerTube" (Contact.Account.url svc))
         | None -> None);
        (match Contact.account_on author_contact (Simple Threads) with
         | Some svc -> Some (icon_link ~icon:(brand ~size:14 threads_brand)
             ~title:"Threads" (Contact.Account.url svc))
         | None -> None);
        (match Contact.account_on author_contact (Simple LinkedIn) with
         | Some svc -> Some (icon_link ~icon:(brand ~size:14 linkedin_brand)
             ~title:"LinkedIn" (Contact.Account.url svc))
         | None -> None);
        (match Contact.handle_on author_contact (Simple Twitter) with
         | Some t -> Some (icon_link ~icon:(brand ~size:14 x_brand)
             ~title:"X" ("https://twitter.com/" ^ t))
         | None -> None);
        (match Contact.handle_on author_contact (Simple Orcid) with
         | Some o -> Some (icon_link ~icon:(brand ~size:14 orcid_brand)
             ~title:"ORCID" ("https://orcid.org/" ^ o))
         | None -> None);
        Some (icon_link ~icon:(brand ~size:14 rss_brand)
          ~title:"RSS Feed" "/news.xml");
      ] in
      main_icons @ photo_icons
  in
  let md_url = match url with
    | Some "/" -> Some "/index.md"
    | Some u -> Some (u ^ ".md")
    | None -> None
  in
  let md_link = match md_url with
    | Some href ->
      [El.a ~at:[At.href href;
                 At.v "title" "View as Markdown";
                 At.class' "opacity-50 hover:opacity-100 transition-opacity text-xs font-mono"]
         [El.txt "{} md"]]
    | None -> []
  in
  El.footer ~at:[At.class' "max-w-6xl mx-auto px-2 md:px-6 py-3 border-t border-gray-200"]
    [ El.div ~at:[At.class' "flex items-center justify-center md:justify-between text-xs text-secondary"]
        [ El.p ~at:[At.class' "hidden md:block"] [El.txt {|© 1998–2026 Anil Madhavapeddy.|}];
          El.div ~at:[At.class' "flex items-center gap-3"]
            (social_icons @ md_link)
        ]
    ]

(** {1 Asset Versions}

    Static assets are served with far-future cache headers, so their URLs
    carry a content hash that changes whenever the content does. *)

let js_version =
  let all = String.concat "" (List.map snd Scripts.by_name) in
  String.sub (Digest.to_hex (Digest.string all)) 0 8

let tw_version =
  let css = Option.value ~default:"" (Arod_assets.read "tw.css") in
  String.sub (Digest.to_hex (Digest.string css)) 0 8

(** {1 Head Elements} *)

let meta_tag ~name ~content =
  El.meta ~at:[ At.name name; At.content content ] ()

let og_tag ~property ~content =
  El.meta ~at:[ At.v "property" property; At.content content ] ()

(** Citation metadata for Google Scholar. *)
type citation = {
  citation_title : string;
  citation_authors : string list;
  citation_date : string; (** YYYY/MM/DD format *)
  citation_doi : string option;
  citation_pdf_url : string option;
  citation_journal : string option;
}

let ptime_to_iso (y, m, d) =
  Printf.sprintf "%04d-%02d-%02d" y m d

let ptime_to_citation_date (y, m, d) =
  Printf.sprintf "%04d/%02d/%02d" y m d

let head_elements ~ctx ~config ~title ~description ?url ?image ?(jsonld=[]) ?standardsite
    ?(og_type="website") ?published ?modified ?(tags=[]) ?citation () =
  let module Contact = Sortal_schema.Contact in
  let site = config.Arod.Config.site in
  let base_url = site.base_url in
  let page_url = match url with Some u -> base_url ^ u | None -> base_url in
  let head_els =
    [ (* Basic meta *)
      meta_tag ~name:"description" ~content:description;
      meta_tag ~name:"author" ~content:site.author_name;
      El.meta ~at:[ At.name "theme-color"; At.content "#fffffc"; At.id "meta-theme-color" ] ();

      (* Canonical URL *)
      El.link ~at:[ At.rel "canonical"; At.href page_url ] ();

      (* Open Graph *)
      og_tag ~property:"og:type" ~content:og_type;
      og_tag ~property:"og:title" ~content:title;
      og_tag ~property:"og:description" ~content:description;
      og_tag ~property:"og:site_name" ~content:site.name;
      og_tag ~property:"og:url" ~content:page_url;

      (* Twitter Card *)
      meta_tag ~name:"twitter:card"
        ~content:(if image <> None then "summary_large_image" else "summary");
      meta_tag ~name:"twitter:title" ~content:title;
      meta_tag ~name:"twitter:description" ~content:description;

      (* Feeds *)
      El.link ~at:[ At.rel "alternate"; At.v "type" "application/atom+xml";
                 At.v "title" (site.name ^ " (Atom)");
                 At.href "/news.xml" ] ();
      El.link ~at:[ At.rel "alternate"; At.v "type" "application/feed+json";
                 At.v "title" (site.name ^ " (JSON Feed)");
                 At.href "/feed.json" ] ();

      (* Favicon *)
      El.link ~at:[ At.rel "icon"; At.v "type" "image/svg+xml";
                 At.href "/favicon.svg" ] ();
      El.link ~at:[ At.rel "icon"; At.v "type" "image/png";
                 At.href "/favicon.png" ] ();
      El.link ~at:[ At.rel "apple-touch-icon";
                 At.href "/apple-touch-icon.png" ] ();

      (* rel=me verification — dynamic from author contact *)

      (* rel=author, blogroll, license *)
      El.link ~at:[ At.rel "author"; At.href "/about" ] ();
      El.link ~at:[ At.rel "blogroll"; At.v "type" "text/x-opml";
                 At.v "title" "Blogroll"; At.href "/network/blogroll.opml" ] ();
      El.link ~at:[ At.rel "license";
                 At.href "https://creativecommons.org/licenses/by/4.0/" ] ();

      (* Theme init — must run before stylesheets apply to prevent FOUC *)
      El.script [El.unsafe_raw Theme.theme_init_js];

      (* Tailwind stylesheet, prebuilt by tailwind/regen.sh and committed *)
      El.link ~at:[ At.rel "stylesheet";
                 At.href ("/tw.css?v=" ^ tw_version) ] ();

      (* Highlight.js — both themes, JS toggles which one is active.
         Deferred so it never blocks parsing; the site bundle that calls
         hljs.highlightAll() is also deferred and comes later in the
         document, which guarantees it runs after this. *)
      El.link ~at:[ At.rel "stylesheet"; At.id "hljs-light";
                 At.href "https://cdnjs.cloudflare.com/ajax/libs/highlight.js/11.9.0/styles/github.min.css" ] ();
      El.link ~at:[ At.rel "stylesheet"; At.id "hljs-dark"; At.disabled;
                 At.href "https://cdnjs.cloudflare.com/ajax/libs/highlight.js/11.9.0/styles/github-dark.min.css" ] ();
      El.script ~at:[ At.defer;
                 At.src "https://cdnjs.cloudflare.com/ajax/libs/highlight.js/11.9.0/highlight.min.js" ] [];

      (* Custom CSS *)
      El.style [El.unsafe_raw Theme.custom_css];
    ]
  in
  (* Dynamic rel=me links from author contact *)
  let head_els =
    match Arod.Ctx.author ctx with
    | None -> head_els
    | Some author_contact ->
      let me_links = List.filter_map Fun.id [
        (match Contact.handle_on author_contact (Simple Github) with
         | Some g -> Some (El.link ~at:[ At.rel "me";
             At.href ("https://github.com/" ^ g) ] ())
         | None -> None);
        (match Contact.atproto_handle author_contact with
         | Some b -> Some (El.link ~at:[ At.rel "me";
             At.href ("https://bsky.app/profile/" ^ b) ] ())
         | None -> None);
        (match Contact.account_on author_contact (Federated Mastodon) with
         | Some svc -> Some (El.link ~at:[ At.rel "me";
             At.href (Contact.Account.url svc) ] ())
         | None -> None);
        (match Contact.account_on author_contact (Simple LinkedIn) with
         | Some svc -> Some (El.link ~at:[ At.rel "me";
             At.href (Contact.Account.url svc) ] ())
         | None -> None);
        (match Contact.handle_on author_contact (Simple Twitter) with
         | Some t -> Some (El.link ~at:[ At.rel "me";
             At.href ("https://twitter.com/" ^ t) ] ())
         | None -> None);
      ] in
      head_els @ me_links
  in
  (* Markdown alternate link *)
  let head_els =
    let md_href = match url with
      | Some "/" -> Some "/index.md"
      | Some u -> Some (u ^ ".md")
      | None -> None
    in
    match md_href with
    | Some href ->
      head_els @ [
        El.link ~at:[ At.rel "alternate"; At.v "type" "text/markdown";
                   At.v "title" (title ^ " (Markdown)");
                   At.href href ] ()
      ]
    | None -> head_els
  in
  (* OG image — use page image, or fall back to author photo *)
  let head_els =
    let img = match image with
      | Some _ -> image
      | None ->
        match Arod.Ctx.author ctx with
        | Some author ->
          (match Bushel.Entry.contact_thumbnail (Arod.Ctx.entries ctx) author with
           | Some t -> Some (base_url ^ t)
           | None -> None)
        | None -> None
    in
    match img with
    | Some img_url ->
      head_els @ [ og_tag ~property:"og:image" ~content:img_url;
                   meta_tag ~name:"twitter:image" ~content:img_url ]
    | None -> head_els
  in
  (* Article OG tags when og_type = "article" *)
  let head_els =
    if og_type = "article" then
      let article_els = List.filter_map Fun.id [
        Option.map (fun date ->
          og_tag ~property:"article:published_time" ~content:(ptime_to_iso date)) published;
        Option.map (fun date ->
          og_tag ~property:"article:modified_time" ~content:(ptime_to_iso date)) modified;
      ] in
      let article_els =
        article_els
        @ [og_tag ~property:"article:author" ~content:site.author_name]
        @ List.map (fun tag -> og_tag ~property:"article:tag" ~content:tag) tags
      in
      head_els @ article_els
    else head_els
  in
  (* Google Scholar citation tags *)
  let head_els = match citation with
    | Some c ->
      let cite_els =
        [ meta_tag ~name:"citation_title" ~content:c.citation_title ]
        @ List.map (fun a ->
            meta_tag ~name:"citation_author" ~content:a
          ) c.citation_authors
        @ [ meta_tag ~name:"citation_publication_date" ~content:c.citation_date ]
      in
      let cite_els = cite_els @ List.filter_map Fun.id [
        Option.map (fun doi -> meta_tag ~name:"citation_doi" ~content:doi) c.citation_doi;
        Option.map (fun pdf -> meta_tag ~name:"citation_pdf_url" ~content:pdf) c.citation_pdf_url;
        Option.map (fun j -> meta_tag ~name:"citation_journal_title" ~content:j) c.citation_journal;
      ] in
      head_els @ cite_els
    | None -> head_els
  in
  (* JSON-LD — always include WebSite, plus any page-specific blocks *)
  let head_els =
    let site = config.Arod.Config.site in
    let website_ld = Arod.Jsonld.website_jsonld
      ~base_url:site.base_url ~site_name:site.name
      ~description:site.description in
    let all_ld = website_ld :: jsonld in
    head_els @ List.map (fun ld ->
      El.script ~at:[ At.v "type" "application/ld+json" ] [ El.unsafe_raw ld ]
    ) all_ld
  in
  (* Optional standardsite link *)
  let head_els = match standardsite with
    | Some ss_url ->
      head_els @ [
        El.link ~at:[ At.rel "site.standard.document"; At.href ss_url ] ()
      ]
    | None -> head_els
  in
  head_els

(** {1 Script Elements} *)

type page_script =
  | Toc | Pagination | Lightbox | Links_modal
  | Checkbox_filter | Calendar
  | Tag_cloud_filter | Idea_filter | Search

let script_file_of = function
  | Toc -> "toc.js"
  | Pagination -> "pagination.js"
  | Lightbox -> "lightbox.js"
  | Links_modal -> "links-modal.js"
  | Checkbox_filter -> "filter.js"
  | Calendar -> "calendar.js"
  | Tag_cloud_filter -> "tag-filter.js"
  | Idea_filter -> "idea-filter.js"
  | Search -> "search.js"

(* Deferred external scripts. Placing them in the head lets the browser
   fetch early, and defer preserves document order while running only
   after the DOM is parsed. *)
let build_scripts page_scripts =
  let script_el name =
    El.script ~at:[ At.defer; At.src ("/js/" ^ name ^ "?v=" ^ js_version) ] []
  in
  script_el "site.js"
  :: List.map (fun s -> script_el (script_file_of s)) page_scripts

(** {1 Content Grid} *)

let content_grid ?(main_cls="max-w-2xl") ~article ?sidebar () =
  El.div
    ~at:[At.class' "max-w-6xl mx-auto px-2 md:px-6 py-8 flex flex-col lg:flex-row gap-6 lg:gap-10"]
    ([ El.main
         ~at:[At.class' ("prose text-body flex-1 " ^ main_cls)]
         [ article ] ]
     @ Option.to_list sidebar)

(** {1 Page Assembly} *)

let page ~ctx ~title ~description ?url ?image ?(jsonld=[]) ?standardsite ?current_page
    ?og_type ?published ?modified ?tags ?citation ?(page_scripts=[]) ?main_cls ~article ?sidebar ?mobile_footer () =
  let config = Arod.Ctx.config ctx in
  let full_title = title ^ " | " ^ config.Arod.Config.site.name in
  let og_type = Option.value ~default:"website" og_type in
  let tags = Option.value ~default:[] tags in
  let head_els =
    head_elements ~ctx ~config ~title ~description ?url ?image ~jsonld ?standardsite
      ~og_type ?published ?modified ~tags ?citation ()
  in
  let mobile_footer_el = match mobile_footer with
    | Some el ->
      El.div ~at:[At.class' "lg:hidden max-w-sm mx-auto px-2 md:px-6 pb-6"]
        [el]
    | None -> El.void
  in
  let body_content =
    [ Nav.header ?current_page ctx;
      content_grid ?main_cls ~article ?sidebar ();
      mobile_footer_el;
      footer_el ~ctx ?url () ]
  in
  let head_el =
    El.head
      ([ El.meta ~at:[At.charset "utf-8"] ();
         El.meta ~at:[At.name "viewport"; At.content "width=device-width, initial-scale=1.0"] ();
         El.title [El.txt full_title] ]
       @ head_els
       @ build_scripts page_scripts)
  in
  let body_el =
    El.body ~at:[At.class' "bg-bg text-text font-sans"] body_content
  in
  El.to_string ~doctype:true
    (El.html ~at:[At.lang "en"] [head_el; body_el])

let wide_page ~ctx ~title ~description ?url ?current_page ?(jsonld=[]) ?(page_scripts=[]) ~article () =
  let config = Arod.Ctx.config ctx in
  let full_title = title ^ " | " ^ config.Arod.Config.site.name in
  let head_els = head_elements ~ctx ~config ~title ~description ?url ~jsonld () in
  let body_content =
    [ Nav.header ?current_page ctx;
      El.div ~at:[At.class' "max-w-screen-xl mx-auto px-2 md:px-6 py-8"]
        [El.main ~at:[At.class' "prose text-body"] [article]];
      footer_el ~ctx ?url () ]
  in
  let head_el =
    El.head
      ([ El.meta ~at:[At.charset "utf-8"] ();
         El.meta ~at:[At.name "viewport"; At.content "width=device-width, initial-scale=1.0"] ();
         El.title [El.txt full_title] ]
       @ head_els
       @ build_scripts page_scripts)
  in
  let body_el =
    El.body ~at:[At.class' "bg-bg text-text font-sans"] body_content
  in
  El.to_string ~doctype:true
    (El.html ~at:[At.lang "en"] [head_el; body_el])

