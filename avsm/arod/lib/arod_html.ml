(*---------------------------------------------------------------------------
  Copyright (c) 2025 Anil Madhavapeddy <anil@recoil.org>. All rights reserved.
  SPDX-License-Identifier: ISC
 ---------------------------------------------------------------------------*)

(** Htmlit-based HTML generation for Arod *)

open Htmlit

(** {1 Attribute Helpers} *)

let class_ c = At.class' c
let id i = At.id i
let href h = At.href h
let alt a = At.alt a
let src s = At.src s
let title t = At.title t
let name n = At.name n
let content c = At.content c
let loading l = At.v "loading" l
let sizes s = At.v "sizes" s
let srcset s = At.v "srcset" s
let frameborder f = At.v "frameborder" f
let allowfullscreen = At.v "allowfullscreen" ""
let sandbox s = At.v "sandbox" s
let width w = At.v "width" w
let height h = At.v "height" h
let rel r = At.rel r
let property p = At.v "property" p
let http_equiv h = At.v "http-equiv" h
let type_ t = At.type' t
let lang l = At.lang l

(** {1 SVG Icons} *)

let svg_icon_paper =
  El.unsafe_raw {|<svg xmlns="http://www.w3.org/2000/svg" viewBox="0 0 32 32" width="18" height="18" fill="none" stroke="currentcolor" stroke-linecap="round" stroke-linejoin="round" stroke-width="2"><path d="M16 7 C16 7 9 1 2 6 L2 28 C9 23 16 28 16 28 16 28 23 23 30 28 L30 6 C23 1 16 7 16 7 Z M16 7 L16 28" /></svg>|}

let svg_icon_project =
  El.unsafe_raw {|<svg xmlns="http://www.w3.org/2000/svg" viewBox="0 0 32 32" width="18" height="18" fill="none" stroke="currentcolor" stroke-linecap="round" stroke-linejoin="round" stroke-width="2"><path d="M30 8 L2 8 2 26 30 26 Z M20 8 C20 8 20 4 16 4 12 4 12 8 12 8 M8 26 L8 8 M24 26 L24 8" /></svg>|}

let svg_icon_note =
  El.unsafe_raw {|<svg xmlns="http://www.w3.org/2000/svg" viewBox="0 0 32 32" width="18" height="18" fill="none" stroke="currentcolor" stroke-linecap="round" stroke-linejoin="round" stroke-width="2"><path d="M27 15 L27 30 2 30 2 5 17 5 M30 6 L26 2 9 19 7 25 13 23 Z M22 6 L26 10 Z M9 19 L13 23 Z" /></svg>|}

let svg_icon_video =
  El.unsafe_raw {|<svg xmlns="http://www.w3.org/2000/svg" viewBox="0 0 32 32" width="18" height="18" fill="none" stroke="currentcolor" stroke-linecap="round" stroke-linejoin="round" stroke-width="2"><path d="M22 13 L30 8 30 24 22 19 Z M2 8 L2 24 22 24 22 8 Z" /></svg>|}

let svg_icon_idea =
  El.unsafe_raw {|<svg xmlns="http://www.w3.org/2000/svg" viewBox="0 0 32 32" width="18" height="18" fill="none" stroke="currentcolor" stroke-linecap="round" stroke-linejoin="round" stroke-width="2"><path d="M18 13 L26 2 8 13 14 19 6 30 24 19 Z" /></svg>|}

let svg_icon_search =
  El.unsafe_raw {|<svg xmlns="http://www.w3.org/2000/svg" viewBox="0 0 24 24" width="18" height="18" fill="none" stroke="currentColor" stroke-width="2" stroke-linecap="round" stroke-linejoin="round"><circle cx="11" cy="11" r="8"/><path d="m21 21-4.35-4.35"/></svg>|}

(** {1 Date Formatting} *)

let int_to_date_suffix ~r n =
  let suffix =
    if n mod 10 = 1 && n mod 100 <> 11 then "st"
    else if n mod 10 = 2 && n mod 100 <> 12 then "nd"
    else if n mod 10 = 3 && n mod 100 <> 13 then "rd"
    else "th"
  in
  let x = string_of_int n in
  let x = if r && String.length x = 1 then " " ^ x else x in
  x ^ suffix

let month_name = function
  | 1 -> "Jan" | 2 -> "Feb" | 3 -> "Mar" | 4 -> "Apr"
  | 5 -> "May" | 6 -> "Jun" | 7 -> "Jul" | 8 -> "Aug"
  | 9 -> "Sep" | 10 -> "Oct" | 11 -> "Nov" | 12 -> "Dec"
  | _ -> ""

let ptime_date ?(r=false) ?(with_d=false) (y, m, d) =
  let ms = month_name m in
  match with_d with
  | false -> Printf.sprintf "%s %4d" ms y
  | true -> Printf.sprintf "%s %s %4d" (int_to_date_suffix ~r d) ms y

(** {1 Image Rendering} *)

let img ?cl ?(alt_text="") ?(title_text="") img_ent =
  let origin_url = Printf.sprintf "/images/%s.webp"
    (Filename.chop_extension (Arod_model.Img.origin img_ent)) in

  let srcsets =
    let variants = Arod_model.Img.variants img_ent in
    String.concat ","
      (Arod_model.Img.MS.fold (fun f (w, _h) acc ->
        Printf.sprintf "/images/%s %dw" f w :: acc
      ) variants [])
  in

  let base_attrs = [
    loading "lazy";
    src origin_url;
    srcset srcsets;
    sizes "(max-width: 768px) 100vw, 33vw"
  ] in

  let attrs = match cl with
    | Some c -> class_ c :: base_attrs
    | None -> base_attrs
  in

  match alt_text with
  | "%r" ->
    El.figure ~at:[class_ "image-right"] [
      El.img ~at:(At.alt title_text :: At.title title_text :: attrs) ();
      El.figcaption [El.txt title_text]
    ]
  | "%c" ->
    El.figure ~at:[class_ "image-center"] [
      El.img ~at:(At.alt title_text :: At.title title_text :: attrs) ();
      El.figcaption [El.txt title_text]
    ]
  | "%lc" ->
    El.figure ~at:[class_ "image-left-float"] [
      El.img ~at:(At.alt title_text :: At.title title_text :: attrs) ();
      El.figcaption [El.txt title_text]
    ]
  | "%rc" ->
    El.figure ~at:[class_ "image-right-float"] [
      El.img ~at:(At.alt title_text :: At.title title_text :: attrs) ();
      El.figcaption [El.txt title_text]
    ]
  | _ ->
    El.img ~at:(At.alt alt_text :: At.title title_text :: attrs) ()

(** {1 Entry Rendering} *)

let full_body ent =
  El.unsafe_raw (Arod_model.md_to_html (Arod_model.Entry.body ent))

(** {1 Video Embedding} *)

let embed_video ~video_title ~url =
  El.div ~at:[class_ "video-center"] [
    El.iframe ~at:[
      title video_title;
      width "100%";
      height "315px";
      src url;
      frameborder "0";
      allowfullscreen;
      sandbox "allow-same-origin allow-scripts allow-popups allow-forms"
    ] []
  ]

(** {1 Page Layout} *)

let page ?(image="/assets/imagetitle-default.jpg") ?(jsonld="") ~page_title ~description ~page_content () =
  let cfg = Arod_model.get_config () in
  let title_text = if page_title = "" then cfg.site.name else page_title in

  let head_els = [
    El.meta ~at:[http_equiv "X-UA-Compatible"; content "ie=edge"] ();
    El.meta ~at:[name "description"; content description] ();
    El.meta ~at:[property "og:image"; content image] ();
    El.meta ~at:[property "og:site_name"; content cfg.site.name] ();
    El.meta ~at:[property "og:type"; content "object"] ();
    El.meta ~at:[property "og:title"; content title_text] ();
    El.meta ~at:[property "og:description"; content description] ();
    El.meta ~at:[name "twitter:card"; content "summary_large_image"] ();
    El.meta ~at:[name "twitter:title"; content title_text] ();
    El.meta ~at:[name "twitter:description"; content description] ();
    El.meta ~at:[name "twitter:image"; content image] ();
    El.meta ~at:[name "theme-color"; content "#fff"] ();
    El.meta ~at:[name "color-scheme"; content "white"] ();
    El.link ~at:[rel "apple-touch-icon"; sizes "180x180"; href "/assets/apple-touch-icon.png"] ();
    El.link ~at:[rel "icon"; type_ "image/png"; sizes "32x32"; href "/assets/favicon-32x32.png"] ();
    El.link ~at:[rel "icon"; type_ "image/png"; sizes "16x16"; href "/assets/favicon-16x16.png"] ();
    El.link ~at:[rel "alternate"; type_ "application/atom+xml"; At.title "Atom Feed"; href "/news.xml"] ();
    El.link ~at:[rel "alternate"; type_ "application/feed+json"; At.title "JSON Feed"; href "/feed.json"] ();
    El.link ~at:[rel "stylesheet"; href "/assets/site.css"] ();
    El.link ~at:[rel "stylesheet"; href "/assets/highlight.min.css"] ();
    El.unsafe_raw jsonld;
    El.script ~at:[src "/assets/highlight.min.js"] [];
    El.script [El.txt "hljs.highlightAll();"]
  ] in

  let header_el = El.header ~at:[class_ "site-header"] [
    El.div ~at:[class_ "header-content"] [
      El.h1 ~at:[class_ "site-name"] [
        El.a ~at:[href "/"] [El.txt cfg.site.name]
      ];
      El.nav ~at:[class_ "main-nav"] [
        El.a ~at:[class_ "nav-link"; href "/papers"] [svg_icon_paper; El.txt "Papers"];
        El.a ~at:[class_ "nav-link"; href "/projects"] [svg_icon_project; El.txt "Projects"];
        El.a ~at:[class_ "nav-link"; href "/notes"] [svg_icon_note; El.txt "Notes"];
        El.a ~at:[class_ "nav-link"; href "/videos"] [svg_icon_video; El.txt "Talks"];
        El.a ~at:[class_ "nav-link"; href "/ideas"] [svg_icon_idea; El.txt "Ideas"];
      ];
      El.div ~at:[class_ "header-right"] [
        El.div ~at:[class_ "search-container"] [
          El.button ~at:[class_ "search-toggle"; At.v "aria-label" "Search"; id "search-toggle-btn"] [
            svg_icon_search;
            El.span ~at:[class_ "search-label"] [El.txt "Search"]
          ]
        ]
      ]
    ]
  ] in

  let footer_el = El.footer [
    El.txt (Printf.sprintf "Powered by Bushel | %s" cfg.site.name)
  ] in

  let body_el = El.body ~at:[class_ "light"] [
    header_el;
    El.div ~at:[class_ "content-grid"] [page_content];
    footer_el;
    El.script ~at:[src "/assets/site.js"] [];
  ] in

  El.page ~lang:"en" ~title:title_text ~more_head:(El.splice head_els) body_el

(** {1 Output Helpers} *)

let to_string el = El.to_string ~doctype:false el
let to_page el = El.to_string ~doctype:true el
