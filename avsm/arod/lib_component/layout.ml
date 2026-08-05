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
  let photo_icon (svc : Contact.service) =
    let host = match Uri.host (Uri.of_string svc.url) with
      | Some h -> String.lowercase_ascii h | None -> "" in
    let bare = Common.strip_www host in
    if String.length bare >= 13 && String.sub bare 0 13 = "instagram.com" then
      Some (icon_link ~icon:(brand ~size:14 instagram_brand)
        ~title:"Instagram" svc.url)
    else if String.length bare >= 10 && String.sub bare 0 10 = "flickr.com" then
      Some (icon_link ~icon:(brand ~size:14 flickr_brand)
        ~title:"Flickr" svc.url)
    else None
  in
  let social_icons = match Arod.Ctx.author ctx with
    | None -> []
    | Some author_contact ->
      let photo_icons = List.filter_map photo_icon
        (Contact.services_of_kind author_contact Photo) in
      let main_icons = List.filter_map Fun.id [
        (match Contact.github_handle author_contact with
         | Some g -> Some (icon_link ~icon:(brand ~size:14 github_brand)
             ~title:"GitHub" ("https://github.com/" ^ g))
         | None -> None);
        (let bsky_svc = List.find_opt (fun (s : Contact.atproto_service) ->
           s.atp_type = ATBluesky) (Contact.atproto_services author_contact) in
         match bsky_svc with
         | Some svc -> Some (icon_link ~icon:(brand ~size:14 bluesky_brand)
             ~title:"Bluesky" svc.atp_url)
         | None ->
           match Contact.bluesky_handle author_contact with
           | Some b -> Some (icon_link ~icon:(brand ~size:14 bluesky_brand)
               ~title:"Bluesky" ("https://bsky.app/profile/" ^ b))
           | None -> None);
        (match Contact.mastodon author_contact with
         | Some svc when svc.Contact.url <> "" ->
           Some (icon_link ~icon:(brand ~size:14 mastodon_brand)
             ~title:"Mastodon" svc.Contact.url)
         | _ -> None);
        (match Contact.peertube author_contact with
         | Some svc when svc.Contact.url <> "" ->
           Some (icon_link ~icon:(brand ~size:14 peertube_brand)
             ~title:"PeerTube" svc.Contact.url)
         | _ -> None);
        (match Contact.threads author_contact with
         | Some svc when svc.Contact.url <> "" ->
           Some (icon_link ~icon:(brand ~size:14 threads_brand)
             ~title:"Threads" svc.Contact.url)
         | _ -> None);
        (match Contact.linkedin author_contact with
         | Some svc -> Some (icon_link ~icon:(brand ~size:14 linkedin_brand)
             ~title:"LinkedIn" svc.Contact.url)
         | None -> None);
        (match Contact.twitter_handle author_contact with
         | Some t -> Some (icon_link ~icon:(brand ~size:14 x_brand)
             ~title:"X" ("https://twitter.com/" ^ t))
         | None -> None);
        (match Contact.orcid author_contact with
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

      (* Theme init — must be before Tailwind CDN to prevent FOUC *)
      El.script [El.unsafe_raw Theme.theme_init_js];

      (* Tailwind CDN *)
      El.script ~at:[ At.src "https://cdn.tailwindcss.com?plugins=typography" ] [];
      El.script [El.unsafe_raw Theme.tailwind_config];

      (* Highlight.js — both themes, JS toggles which one is active *)
      El.link ~at:[ At.rel "stylesheet"; At.id "hljs-light";
                 At.href "https://cdnjs.cloudflare.com/ajax/libs/highlight.js/11.9.0/styles/github.min.css" ] ();
      El.link ~at:[ At.rel "stylesheet"; At.id "hljs-dark"; At.disabled;
                 At.href "https://cdnjs.cloudflare.com/ajax/libs/highlight.js/11.9.0/styles/github-dark.min.css" ] ();
      El.script ~at:[ At.src "https://cdnjs.cloudflare.com/ajax/libs/highlight.js/11.9.0/highlight.min.js" ] [];

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
        (match Contact.github_handle author_contact with
         | Some g -> Some (El.link ~at:[ At.rel "me";
             At.href ("https://github.com/" ^ g) ] ())
         | None -> None);
        (let bsky_svc = List.find_opt (fun (s : Contact.atproto_service) ->
           s.atp_type = ATBluesky) (Contact.atproto_services author_contact) in
         match bsky_svc with
         | Some svc -> Some (El.link ~at:[ At.rel "me";
             At.href svc.atp_url ] ())
         | None ->
           match Contact.bluesky_handle author_contact with
           | Some b -> Some (El.link ~at:[ At.rel "me";
               At.href ("https://bsky.app/profile/" ^ b) ] ())
           | None -> None);
        (match Contact.mastodon author_contact with
         | Some svc when svc.Contact.url <> "" ->
           Some (El.link ~at:[ At.rel "me"; At.href svc.Contact.url ] ())
         | _ -> None);
        (match Contact.linkedin author_contact with
         | Some svc -> Some (El.link ~at:[ At.rel "me";
             At.href svc.Contact.url ] ())
         | None -> None);
        (match Contact.twitter_handle author_contact with
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
  | Status_filter | Classification_filter | Link_filter
  | Papers_calendar | Notes_calendar | Links_calendar
  | Network_calendar | Ideas_calendar
  | Tag_cloud_filter | Masonry

let script_of = function
  | Toc -> Scripts.toc_js
  | Pagination -> Scripts.pagination_js
  | Lightbox -> Scripts.lightbox_js
  | Links_modal -> Scripts.links_modal_js
  | Status_filter -> Scripts.status_filter_js
  | Classification_filter -> Scripts.classification_filter_js
  | Link_filter -> Scripts.link_filter_js
  | Papers_calendar -> Scripts.papers_calendar_js
  | Notes_calendar -> Scripts.notes_calendar_js
  | Links_calendar -> Scripts.links_calendar_js
  | Network_calendar -> Scripts.network_calendar_js
  | Ideas_calendar -> Scripts.ideas_calendar_js
  | Tag_cloud_filter -> Scripts.tag_cloud_filter_js
  | Masonry -> Scripts.masonry_js

let global_scripts =
  [ El.script [ El.unsafe_raw Scripts.sidenotes_js ];
    El.script [ El.unsafe_raw Scripts.search_js ];
    El.script [ El.unsafe_raw Scripts.hljs_init ];
    El.script [ El.unsafe_raw Scripts.theme_toggle_js ];
    El.script [ El.unsafe_raw Scripts.feed_dropdown_js ];
    El.script [ El.unsafe_raw Scripts.mobile_menu_js ] ]

let build_scripts page_scripts =
  let page_els = List.map (fun s ->
    El.script [ El.unsafe_raw (script_of s) ]
  ) page_scripts in
  global_scripts @ page_els

(** {1 Content Grid} *)

let content_grid ~article ?sidebar () =
  El.div
    ~at:[At.class' "max-w-6xl mx-auto px-2 md:px-6 py-8 flex flex-col lg:flex-row gap-6 lg:gap-10"]
    ([ El.main
         ~at:[At.class' "prose text-body flex-1 max-w-2xl"]
         [ article ] ]
     @ Option.to_list sidebar)

(** {1 Page Assembly} *)

let page ~ctx ~title ~description ?url ?image ?(jsonld=[]) ?standardsite ?current_page ?toc_sections
    ?og_type ?published ?modified ?tags ?citation ?(page_scripts=[]) ~article ?sidebar ?mobile_footer () =
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
    [ Nav.header ?current_page ?toc_sections ctx;
      content_grid ~article ?sidebar ();
      mobile_footer_el;
      footer_el ~ctx ?url () ]
    @ build_scripts page_scripts
  in
  let head_el =
    El.head
      ([ El.meta ~at:[At.charset "utf-8"] ();
         El.meta ~at:[At.name "viewport"; At.content "width=device-width, initial-scale=1.0"] ();
         El.title [El.txt full_title] ]
       @ head_els)
  in
  let body_el =
    El.body ~at:[At.class' "bg-bg text-text font-sans"] body_content
  in
  El.to_string ~doctype:true
    (El.html ~at:[At.lang "en"] [head_el; body_el])

let simple_page ~ctx ~title ~description ?url ?current_page ?(page_scripts=[]) ~content () =
  page ~ctx ~title ~description ?url ?current_page ~page_scripts ~article:content ()

let wide_page ~ctx ~title ~description ?url ?current_page ?(jsonld=[]) ?(page_scripts=[]) ~article () =
  let config = Arod.Ctx.config ctx in
  let full_title = title ^ " | " ^ config.Arod.Config.site.name in
  let head_els = head_elements ~ctx ~config ~title ~description ?url ~jsonld () in
  let body_content =
    [ Nav.header ?current_page ctx;
      El.div ~at:[At.class' "max-w-screen-xl mx-auto px-2 md:px-6 py-8"]
        [El.main ~at:[At.class' "prose text-body"] [article]];
      footer_el ~ctx ?url () ]
    @ build_scripts page_scripts
  in
  let head_el =
    El.head
      ([ El.meta ~at:[At.charset "utf-8"] ();
         El.meta ~at:[At.name "viewport"; At.content "width=device-width, initial-scale=1.0"] ();
         El.title [El.txt full_title] ]
       @ head_els)
  in
  let body_el =
    El.body ~at:[At.class' "bg-bg text-text font-sans"] body_content
  in
  El.to_string ~doctype:true
    (El.html ~at:[At.lang "en"] [head_el; body_el])

