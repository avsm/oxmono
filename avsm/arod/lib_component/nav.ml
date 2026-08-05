(*---------------------------------------------------------------------------
  Copyright (c) 2025 Anil Madhavapeddy <anil@recoil.org>. All rights reserved.
  SPDX-License-Identifier: ISC
 ---------------------------------------------------------------------------*)

(** Navigation header component for the Arod site.

    Sticky header with navigation links, search button, and TOC breadcrumb
    row matching the Tailwind CSS reference design. *)

open Htmlit

(** {1 Icons} *)

module I = Arod.Icons

let search_icon =
  El.unsafe_raw (I.outline ~cl:"w-4 h-4" ~size:16 I.search_o)

let theme_toggle_btn =
  El.button
    ~at:[
      At.id "theme-toggle-btn";
      At.v "aria-label" "Toggle theme";
      At.class' "shrink-0 p-1.5 rounded-md text-secondary hover:text-link hover:bg-surface transition-all";
    ]
    [ (* Three icons: only one shown at a time via JS *)
      El.unsafe_raw (I.outline ~cl:"w-4 h-4 theme-icon-system" ~size:16 I.device_desktop_o);
      El.unsafe_raw (I.outline ~cl:"w-4 h-4 theme-icon-light hidden" ~size:16 I.sun_o);
      El.unsafe_raw (I.outline ~cl:"w-4 h-4 theme-icon-dark hidden" ~size:16 I.moon_o);
    ]

let nav_icon_for label =
  let paths = match label with
    | "Papers" -> Some I.paper_o
    | "Projects" -> Some I.folder_o
    | "Notes" -> Some I.note_o
    | "Talks" -> Some I.presentation_o
    | "Ideas" -> Some I.bulb_o
    | "Links" -> Some I.link_o
    | "Network" -> Some I.broadcast_tower_o
    | "About" -> Some I.home_o
    | _ -> None
  in
  match paths with
  | Some p -> Some (El.unsafe_raw (I.outline ~size:16 p))
  | None -> None

let filter_icon_for collection =
  let paths = match collection with
    | "papers" | "paper" -> I.paper_o
    | "notes" | "note" -> I.note_o
    | "videos" | "video" -> I.video_o
    | "projects" | "project" -> I.folder_o
    | "ideas" | "idea" -> I.bulb_o
    | "links" | "link" -> I.link_o
    | _ -> I.tag_o
  in
  El.unsafe_raw (I.outline ~size:14 paths)

(** {1 Flow Line SVG}

    Decorative gradient line connecting nav items, visible only on desktop. *)

let flow_svg =
  El.unsafe_raw {|<svg class="hidden md:block absolute top-1/2 left-0 w-full h-8 -translate-y-1/2 pointer-events-none" preserveAspectRatio="none">
  <defs>
    <linearGradient id="flow-grad" x1="0%" y1="0%" x2="100%" y2="0%">
      <stop offset="0%" stop-color="#d1d5db" stop-opacity="0"/>
      <stop offset="15%" stop-color="#d1d5db"/>
      <stop offset="50%" stop-color="#bbf7d0"/>
      <stop offset="85%" stop-color="#d1d5db"/>
      <stop offset="100%" stop-color="#d1d5db" stop-opacity="0"/>
    </linearGradient>
  </defs>
  <line x1="0" y1="50%" x2="100%" y2="50%" stroke="url(#flow-grad)" stroke-width="1"/>
</svg>|}

(** {1 Nav Items} *)

type nav_link = {
  label : string;
  href : string;
  id : string option;
}

type nav_item = Link of nav_link | Divider

let nav_items =
  [
    Link { label = "About"; href = "/"; id = None };
    Divider;
    Link { label = "Projects"; href = "/projects"; id = None };
    Link { label = "Ideas"; href = "/ideas"; id = None };
    Divider;
    Link { label = "Papers"; href = "/papers"; id = None };
    Link { label = "Notes"; href = "/notes"; id = Some "nav-notes" };
    Link { label = "Talks"; href = "/videos"; id = None };
    Divider;
    Link { label = "Network"; href = "/network"; id = None };
    Link { label = "Links"; href = "/links"; id = None };
  ]

(** Desktop nav divider — thin vertical line. *)
let nav_divider () =
  El.li ~at:[At.class' "nav-group-divider flex items-center px-0.5";
             At.v "aria-hidden" "true"]
    [El.span ~at:[At.v "style" "width:1px;height:16px;background:var(--color-secondary);opacity:0.4"] []]

(** Desktop nav link with caret indicator. *)
let render_nav_link ~current_page item =
  let is_current =
    match current_page with
    | Some page ->
      String.lowercase_ascii page = String.lowercase_ascii item.label
    | None -> false
  in
  let base_class =
    "inline-flex items-center gap-1 px-1.5 sm:px-2 py-1 rounded-md text-secondary hover:text-link hover:bg-surface no-underline transition-all"
  in
  let cls = if is_current then base_class ^ " text-link font-semibold bg-surface" else base_class in
  let at =
    [ At.href item.href;
      At.class' cls ]
    @ (match item.id with Some id -> [ At.id id ] | None -> [])
    @ (if is_current then [ At.v "aria-current" "page" ] else [])
  in
  let icon_el = match nav_icon_for item.label with
    | Some i -> [El.span ~at:[At.class' "hidden lg:inline-flex"] [i]]
    | None -> []
  in
  let text_children = [ El.txt item.label ] in
  let caret =
    if is_current then
      [ El.span ~at:[At.class' "absolute -bottom-2 left-1/2 -translate-x-1/2 nav-caret"]
          [ El.unsafe_raw {|<svg width="8" height="4" viewBox="0 0 8 4" fill="currentColor"><path d="M0 0l4 4 4-4z"/></svg>|} ] ]
    else []
  in
  El.li ~at:[At.class' "relative"]
    ([ El.a ~at (icon_el @ text_children) ] @ caret)

(** Render a nav item (link or divider) for desktop. *)
let nav_item_el ~current_page = function
  | Link item -> render_nav_link ~current_page item
  | Divider -> nav_divider ()

(** Mobile nav divider — subtle horizontal rule. *)
let mobile_nav_divider () =
  El.div ~at:[At.class' "mx-4 my-1 border-t border-border-color opacity-40"] []

(** Mobile nav link — vertical list with icon + label. *)
let render_mobile_nav_link ~current_page item =
  let is_current =
    match current_page with
    | Some page ->
      String.lowercase_ascii page = String.lowercase_ascii item.label
    | None -> false
  in
  let base_class =
    "mobile-nav-link flex items-center gap-3 px-4 py-2.5 rounded-md text-secondary hover:text-link hover:bg-surface no-underline transition-all"
  in
  let cls = if is_current then base_class ^ " text-link font-semibold bg-surface" else base_class in
  let at =
    [ At.href item.href; At.class' cls ]
    @ (if is_current then [ At.v "aria-current" "page" ] else [])
  in
  let icon_el = match nav_icon_for item.label with
    | Some i -> [i]
    | None -> []
  in
  El.a ~at (icon_el @ [ El.txt item.label ])

(** Render a nav item (link or divider) for mobile. *)
let mobile_nav_item_el ~current_page = function
  | Link item -> render_mobile_nav_link ~current_page item
  | Divider -> mobile_nav_divider ()

(** {1 TOC Row} *)

let toc_row ~sections =
  match sections with
  | [] -> El.void
  | _ ->
  El.div
    ~at:[ At.id "toc-row";
          At.class' "hidden md:flex items-center gap-0 mt-1.5 opacity-0 max-h-0 overflow-hidden transition-all duration-300 scrollbar-hide" ]
    ([ El.a
        ~at:[
          At.id "toc-root";
          At.href "#intro";
          At.v "title" "Top";
          At.class' "text-xs text-secondary hover:text-link no-underline transition-colors shrink-0";
        ]
        [ El.unsafe_raw (I.outline ~size:12 I.arrow_up_o) ];
    ]
    @ List.concat
        (List.mapi
           (fun i (id, short_label) ->
             [
               El.span ~at:[At.class' "text-gray-300 select-none px-0.5 text-xs"]
                 [ El.txt "/" ];
               El.a
                 ~at:[
                   At.href ("#" ^ id);
                   At.class' "toc-link no-underline text-xs px-0.5 py-0 rounded-md text-secondary hover:text-link transition-all whitespace-nowrap overflow-hidden text-ellipsis inline-block max-w-40";
                   At.v "data-index" (string_of_int i);
                 ]
                 [ El.txt short_label ];
             ])
           sections))

(** {1 Search Modal} *)

let search_filter_pill ~active kind label =
  let icon = match kind with
    | "" -> []
    | k -> [filter_icon_for k]
  in
  El.button
    ~at:[
      At.class' ("search-filter-pill" ^ (if active then " active" else "")
        ^ " inline-flex items-center gap-1 text-sm transition-colors");
      At.v "data-kind" kind;
    ]
    (icon @ [ El.txt label ])

let search_modal =
  let kbd cls txt =
    El.unsafe_raw (Printf.sprintf
      {|<kbd class="px-1 py-0.5 bg-surface border border-border-color rounded text-xs %s">%s</kbd>|}
      cls txt)
  in
  El.div
    ~at:[
      At.id "search-modal-overlay";
      At.class' "search-modal-overlay items-center justify-center p-4";
    ]
    [
      El.div
        ~at:[At.class' "search-modal bg-bg border border-border-color rounded-xl w-full max-w-2xl overflow-hidden";
             At.v "role" "search"]
        [
          (* Search input row *)
          El.div
            ~at:[At.class' "flex items-center gap-2 px-4 py-3 border-b border-border-color"]
            [
              search_icon;
              El.span ~at:[At.class' "text-accent font-mono text-sm font-semibold shrink-0"]
                [ El.txt ">_" ];
              El.input
                ~at:[
                  At.id "search-input";
                  At.type' "text";
                  At.v "placeholder" "Search papers, notes, projects...";
                  At.autocomplete "off";
                  At.class' "shrink w-full bg-transparent text-sm text-text border-none outline-none placeholder-secondary";
                ] ();
              kbd "shrink-0" "esc";
            ];
          (* Kind filter pills — toggle individually *)
          El.div
            ~at:[At.id "search-filters";
                 At.class' "flex items-center gap-1.5 px-4 py-1.5 border-b border-border-color overflow-x-auto scrollbar-hide"]
            [
              search_filter_pill ~active:false "paper" "Papers";
              search_filter_pill ~active:false "note" "Notes";
              search_filter_pill ~active:false "project" "Projects";
              search_filter_pill ~active:false "idea" "Ideas";
              search_filter_pill ~active:false "video" "Talks";
              search_filter_pill ~active:false "link" "Links";
            ];
          (* Search results area *)
          El.div
            ~at:[ At.id "search-results";
                  At.class' "search-results-area overflow-y-auto" ]
            [
              El.div ~at:[At.class' "search-empty-state"]
                [ El.unsafe_raw (I.outline ~cl:"text-border-color" ~size:32 I.search_o);
                  El.span ~at:[At.class' "text-sm text-secondary mt-2"]
                    [ El.txt "Type to search across all content" ] ];
            ];
          (* Footer *)
          El.div
            ~at:[At.class' "flex items-center justify-between px-4 py-1.5 border-t border-border-color text-secondary"]
            [
              El.span ~at:[At.id "search-count"; At.class' "text-xs font-mono"] [];
              El.div ~at:[At.class' "flex items-center gap-2.5 text-xs"]
                [
                  El.span [ kbd "" "\xE2\x86\x91\xE2\x86\x93"; El.txt " nav" ];
                  El.span [ kbd "" "\xE2\x86\xB5"; El.txt " open" ];
                  El.span [ kbd "" "esc"; El.txt " close" ];
                ];
            ];
        ];
    ]

(** {1 Header} *)

let header ?(current_page : string option) ?(toc_sections=[]) ctx =
  let config = Arod.Ctx.config ctx in
  let site_name = config.Arod.Config.site.name in

  (* Mobile menu panel — hidden by default, toggled via JS *)
  let mobile_menu =
    El.div
      ~at:[ At.id "mobile-menu";
            At.class' "mobile-menu" ]
      [
        El.div ~at:[At.class' "mobile-menu-backdrop"] [];
        El.div ~at:[At.class' "mobile-menu-panel"]
          [
            (* Close button row *)
            El.div ~at:[At.class' "flex items-center justify-between px-4 py-3 border-b border-gray-200"]
              [
                El.span ~at:[At.class' "text-sm font-semibold text-text"]
                  [ El.span ~at:[At.class' "nav-prompt"] [ El.txt ">_ " ];
                    El.txt site_name ];
                El.button
                  ~at:[ At.id "mobile-menu-close";
                        At.v "aria-label" "Close menu";
                        At.class' "p-2 rounded-md text-secondary hover:text-link transition-all" ]
                  [ El.unsafe_raw (I.outline ~cl:"w-5 h-5" ~size:20 I.x_o) ];
              ];
            (* Nav links *)
            El.nav ~at:[At.class' "flex flex-col py-2 px-2 text-sm"]
              (List.map (mobile_nav_item_el ~current_page) nav_items);
          ];
      ]
  in

  El.header
    ~at:[ At.id "header";
          At.class' "sticky top-0 z-50 nav-bg nav-border overflow-x-hidden" ]
    [
      El.div ~at:[At.class' "max-w-6xl mx-auto px-6 py-1.5"]
        [
          El.nav ~at:[At.class' "relative"]
            [
              (* Flow line SVG - desktop only *)
              flow_svg;

              (* Main nav row *)
              El.div ~at:[At.class' "flex items-center gap-2 sm:gap-3"]
                [
                  (* Hamburger button — mobile only *)
                  El.button
                    ~at:[
                      At.id "mobile-menu-btn";
                      At.v "aria-label" "Open menu";
                      At.class' "md:hidden shrink-0 p-2 rounded-md text-secondary hover:text-link hover:bg-surface transition-all";
                    ]
                    [ El.unsafe_raw (I.outline ~cl:"w-5 h-5" ~size:20 I.menu_o) ];

                  (* Site name / link *)
                  El.a
                    ~at:[
                      At.href "/";
                      At.class' "shrink-0 text-lg font-semibold no-underline text-text hover:text-link transition-colors whitespace-nowrap";
                    ]
                    [
                      El.span ~at:[At.class' "nav-prompt"] [ El.txt ">_ " ];
                      El.span ~at:[At.class' "hidden lg:inline"]
                        [ El.txt site_name ];
                      El.span ~at:[At.class' "lg:hidden"]
                        [ El.txt "@avsm" ];
                    ];

                  (* Separator *)
                  El.span ~at:[At.class' "hidden md:block text-gray-300 select-none"]
                    [ El.txt "/" ];

                  (* Nav items — hidden on mobile, shown on md+ *)
                  El.ul
                    ~at:[ At.class' "hidden md:flex items-center gap-0.5 text-sm" ]
                    (List.map (nav_item_el ~current_page) nav_items);

                  (* Search button *)
                  El.button
                    ~at:[
                      At.id "search-toggle-btn";
                      At.v "aria-label" "Search";
                      At.class' "shrink-0 ml-auto p-1.5 rounded-md text-secondary hover:text-link hover:bg-surface transition-all";
                    ]
                    [ search_icon ];

                  (* Feed format dropdown *)
                  El.div ~at:[At.class' "feed-dropdown-wrap shrink-0"]
                    [
                      El.button
                        ~at:[
                          At.id "feed-dropdown-btn";
                          At.v "aria-label" "Subscribe to feeds";
                          At.class' "p-1.5 rounded-md text-secondary hover:text-link hover:bg-surface transition-all";
                        ]
                        [ El.unsafe_raw (I.brand ~size:16 I.rss_brand) ];
                      El.div ~at:[At.id "feed-dropdown"; At.class' "feed-dropdown"]
                        [
                          El.div ~at:[At.class' "feed-dropdown-header"]
                            [ El.txt "Subscribe" ];
                          El.a ~at:[At.href "/news.xml"; At.class' "feed-dropdown-item"]
                            [ El.unsafe_raw (I.brand ~size:12 I.rss_brand);
                              El.span [El.txt "Atom"]; El.span ~at:[At.class' "feed-dropdown-desc"] [El.txt "full"] ];
                          El.a ~at:[At.href "/feeds/feed.json"; At.class' "feed-dropdown-item"]
                            [ El.unsafe_raw (I.brand ~size:12 I.jsonfeed_brand);
                              El.span [El.txt "JSON Feed"]; El.span ~at:[At.class' "feed-dropdown-desc"] [El.txt "full"] ];
                          El.div ~at:[At.class' "feed-dropdown-divider"] [];
                          El.a ~at:[At.href "/perma.xml"; At.class' "feed-dropdown-item"]
                            [ El.unsafe_raw (I.brand ~size:12 I.rss_brand);
                              El.span [El.txt "Atom"]; El.span ~at:[At.class' "feed-dropdown-desc"] [El.txt "perma"] ];
                          El.a ~at:[At.href "/perma.json"; At.class' "feed-dropdown-item"]
                            [ El.unsafe_raw (I.brand ~size:12 I.jsonfeed_brand);
                              El.span [El.txt "JSON Feed"]; El.span ~at:[At.class' "feed-dropdown-desc"] [El.txt "perma"] ];
                        ];
                    ];

                  (* Theme toggle *)
                  theme_toggle_btn;
                ];

              (* TOC row - populated per-page *)
              toc_row ~sections:toc_sections;
            ];
        ];
      mobile_menu;
      search_modal;
    ]
