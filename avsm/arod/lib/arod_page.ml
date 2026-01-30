(*---------------------------------------------------------------------------
  Copyright (c) 2025 Anil Madhavapeddy <anil@recoil.org>. All rights reserved.
  SPDX-License-Identifier: ISC
 ---------------------------------------------------------------------------*)

(** Page layout for Arod webserver *)

open Htmlit

(** SVG icons for navigation *)
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

(** Search modal HTML *)
let search_modal =
  El.div ~at:[At.class' "search-modal"; At.id "search-modal"] [
    El.div ~at:[At.class' "search-modal-content"] [
      El.div ~at:[At.class' "search-modal-header"] [
        El.unsafe_raw {|<svg xmlns="http://www.w3.org/2000/svg" viewBox="0 0 24 24" width="20" height="20" fill="none" stroke="currentColor" stroke-width="2" stroke-linecap="round" stroke-linejoin="round"><circle cx="11" cy="11" r="8"/><path d="m21 21-4.35-4.35"/></svg>|};
        El.input ~at:[
          At.class' "search-modal-input";
          At.id "search-modal-input";
          At.type' "text";
          At.v "placeholder" "Search papers, notes, videos, projects...";
          At.v "autocomplete" "off"
        ] ();
        El.button ~at:[At.class' "search-modal-close"; At.id "search-modal-close"] [
          El.txt "×"
        ]
      ];
      El.div ~at:[At.class' "search-filters"; At.id "search-filters"] [
        El.div ~at:[At.class' "search-filters-controls"] [
          El.button ~at:[At.class' "search-filter-toggle"; At.id "filter-toggle-all"] [El.txt "All"];
          El.button ~at:[At.class' "search-filter-toggle"; At.id "filter-toggle-none"] [El.txt "None"]
        ];
        El.button ~at:[At.class' "search-filter active"; At.v "data-collection" "papers"] [El.txt "Papers"];
        El.button ~at:[At.class' "search-filter active"; At.v "data-collection" "notes"] [El.txt "Notes"];
        El.button ~at:[At.class' "search-filter active"; At.v "data-collection" "videos"] [El.txt "Videos"];
        El.button ~at:[At.class' "search-filter active"; At.v "data-collection" "projects"] [El.txt "Projects"];
        El.button ~at:[At.class' "search-filter active"; At.v "data-collection" "ideas"] [El.txt "Ideas"]
      ];
      El.div ~at:[At.class' "search-modal-body"; At.id "search-modal-body"] [
        El.div ~at:[At.class' "search-loading"] [
          El.txt "Loading recent items..."
        ]
      ];
      El.div ~at:[At.class' "search-modal-footer"] [
        El.div ~at:[At.class' "search-status"; At.id "search-status"] [
          El.span ~at:[At.class' "search-status-indicator"] [];
          El.span ~at:[At.class' "search-status-text"] [El.txt "Ready"]
        ];
        El.div ~at:[At.class' "search-keyboard-hint"] [
          El.span [
            El.unsafe_raw {|<kbd>↑</kbd> <kbd>↓</kbd>|};
            El.txt " navigate"
          ];
          El.span [
            El.unsafe_raw {|<kbd>↵</kbd>|};
            El.txt " select"
          ];
          El.span [
            El.unsafe_raw {|<kbd>ESC</kbd>|};
            El.txt " close"
          ]
        ]
      ]
    ]
  ]

let page ?(image="/assets/imagetitle-default.jpg") ?(jsonld="") ?standardsite ?(page_footer=El.splice []) ~title ~description ~page_content:content () =
  let cfg = Arod_model.get_config () in
  let page_title = if title = "" then cfg.site.name else title in

  let head_els = [
    El.meta ~at:[At.v "http-equiv" "X-UA-Compatible"; At.content "ie=edge"] ();
    El.meta ~at:[At.name "description"; At.content description] ();
    El.meta ~at:[At.v "property" "og:image"; At.content image] ();
    El.meta ~at:[At.v "property" "og:site_name"; At.content cfg.site.name] ();
    El.meta ~at:[At.v "property" "og:type"; At.content "object"] ();
    El.meta ~at:[At.v "property" "og:title"; At.content page_title] ();
    El.meta ~at:[At.v "property" "og:description"; At.content description] ();
    El.meta ~at:[At.name "twitter:card"; At.content "summary_large_image"] ();
    El.meta ~at:[At.name "twitter:title"; At.content page_title] ();
    El.meta ~at:[At.name "twitter:description"; At.content description] ();
    El.meta ~at:[At.name "twitter:image"; At.content image] ();
    El.meta ~at:[At.name "theme-color"; At.content "#fff"] ();
    El.meta ~at:[At.name "color-scheme"; At.content "white"] ();
    El.link ~at:[At.rel "apple-touch-icon"; At.v "sizes" "180x180"; At.href "/assets/apple-touch-icon.png"] ();
    El.link ~at:[At.rel "icon"; At.type' "image/png"; At.v "sizes" "32x32"; At.href "/assets/favicon-32x32.png"] ();
    El.link ~at:[At.rel "icon"; At.type' "image/png"; At.v "sizes" "16x16"; At.href "/assets/favicon-16x16.png"] ();
    El.link ~at:[At.rel "alternate"; At.type' "application/atom+xml"; At.title "Atom Feed"; At.href "/news.xml"] ();
    El.link ~at:[At.rel "alternate"; At.type' "application/atom+xml"; At.title "Perma Feed (Significant Articles)"; At.href "/perma.xml"] ();
    El.link ~at:[At.rel "alternate"; At.type' "application/feed+json"; At.title "JSON Feed"; At.href "/feed.json"] ();
    El.link ~at:[At.rel "alternate"; At.type' "application/feed+json"; At.title "Perma JSON Feed (Significant Articles)"; At.href "/perma.json"] ();
    El.link ~at:[At.rel "stylesheet"; At.href "/assets/site.css"] ();
    El.link ~at:[At.rel "stylesheet"; At.href "/assets/highlight.min.css"] ();
    El.unsafe_raw jsonld;
    El.script ~at:[At.src "/assets/highlight.min.js"] [];
    El.script [El.txt "hljs.highlightAll();"]
  ] in

  (* Add standardsite link if present *)
  let head_els = match standardsite with
    | Some url -> head_els @ [El.link ~at:[At.rel "site.standard.document"; At.href url] ()]
    | None -> head_els
  in

  let header_el = El.header ~at:[At.class' "site-header"] [
    El.div ~at:[At.class' "header-content"] [
      (* Site name on the left *)
      El.h1 ~at:[At.class' "site-name"] [
        El.a ~at:[At.href "/"] [El.txt cfg.site.name]
      ];
      (* Navigation links *)
      El.nav ~at:[At.class' "main-nav"] [
        El.a ~at:[At.class' "nav-link"; At.href "/papers"] [
          svg_icon_paper;
          El.txt "Papers"
        ];
        El.a ~at:[At.class' "nav-link"; At.href "/projects"] [
          svg_icon_project;
          El.txt "Projects"
        ];
        El.a ~at:[At.class' "nav-link"; At.href "/notes"] [
          svg_icon_note;
          El.txt "Notes"
        ];
        El.a ~at:[At.class' "nav-link"; At.href "/videos"] [
          svg_icon_video;
          El.txt "Talks"
        ];
        El.a ~at:[At.class' "nav-link"; At.href "/ideas"] [
          svg_icon_idea;
          El.txt "Ideas"
        ]
      ];
      (* Right side: Search *)
      El.div ~at:[At.class' "header-right"] [
        El.div ~at:[At.class' "search-container"] [
          El.button ~at:[
            At.class' "search-toggle";
            At.v "aria-label" "Search";
            At.id "search-toggle-btn"
          ] [
            svg_icon_search;
            El.span ~at:[At.class' "search-label"] [El.txt "Search"]
          ]
        ]
      ]
    ]
  ] in

  let footer_el = El.footer [page_footer] in

  let body_el = El.body ~at:[At.class' "light"] [
    header_el;
    El.div ~at:[At.class' "content-grid"] [content];
    footer_el;
    search_modal;
    El.script ~at:[At.src "/assets/site.js"] []
  ] in

  El.page ~lang:"en" ~title:page_title ~more_head:(El.splice head_els) body_el

let bushel_graph () =
  let title = "Bushel Link Graph" in
  let description = "Interactive force-directed graph visualization of links and backlinks in the Bushel dataset" in

  let graph_html = El.div [
    El.h1 [El.txt "Bushel Link Graph"];

    El.div ~at:[At.id "controls"; At.style "margin: 20px 0; padding: 15px; background: #f5f5f5; border-radius: 5px;"] [
      El.div ~at:[At.style "margin-bottom: 10px;"] [
        El.strong [El.txt "Filter by type: "];
        El.label ~at:[At.style "margin: 0 10px;"] [
          El.input ~at:[At.type' "checkbox"; At.id "filter-paper"; At.checked; At.class' "type-filter"] ();
          El.txt " Papers"
        ];
        El.label ~at:[At.style "margin: 0 10px;"] [
          El.input ~at:[At.type' "checkbox"; At.id "filter-project"; At.checked; At.class' "type-filter"] ();
          El.txt " Projects"
        ];
        El.label ~at:[At.style "margin: 0 10px;"] [
          El.input ~at:[At.type' "checkbox"; At.id "filter-note"; At.checked; At.class' "type-filter"] ();
          El.txt " Notes"
        ];
        El.label ~at:[At.style "margin: 0 10px;"] [
          El.input ~at:[At.type' "checkbox"; At.id "filter-idea"; At.checked; At.class' "type-filter"] ();
          El.txt " Ideas"
        ];
        El.label ~at:[At.style "margin: 0 10px;"] [
          El.input ~at:[At.type' "checkbox"; At.id "filter-video"; At.checked; At.class' "type-filter"] ();
          El.txt " Videos"
        ];
        El.label ~at:[At.style "margin: 0 10px;"] [
          El.input ~at:[At.type' "checkbox"; At.id "filter-contact"; At.checked; At.class' "type-filter"] ();
          El.txt " Contacts"
        ];
        El.label ~at:[At.style "margin: 0 10px;"] [
          El.input ~at:[At.type' "checkbox"; At.id "filter-domain"; At.checked; At.class' "type-filter"] ();
          El.txt " Domains"
        ]
      ];
      El.div ~at:[At.style "margin-bottom: 10px;"] [
        El.strong [El.txt "Link type: "];
        El.label ~at:[At.style "margin: 0 10px;"] [
          El.input ~at:[At.type' "checkbox"; At.id "filter-internal"; At.checked; At.class' "link-filter"] ();
          El.txt " Internal"
        ];
        El.label ~at:[At.style "margin: 0 10px;"] [
          El.input ~at:[At.type' "checkbox"; At.id "filter-external"; At.checked; At.class' "link-filter"] ();
          El.txt " External"
        ]
      ];
      El.div [
        El.button ~at:[At.id "reset-filters"; At.style "padding: 5px 15px; cursor: pointer;"] [El.txt "Reset Filters"]
      ]
    ];

    El.div ~at:[At.id "graph"; At.style "width: 100%; height: 800px; border: 1px solid #ddd;"] [];

    El.script ~at:[At.src "https://d3js.org/d3.v7.min.js"] [];

    El.script [El.unsafe_raw {|
fetch('/bushel/graph.json')
  .then(response => response.json())
  .then(data => { initGraph(data); })
  .catch(error => {
    console.error('Error loading graph data:', error);
    document.getElementById('graph').innerHTML = '<p style="color: red;">Error loading graph data</p>';
  });

function initGraph(graphData) {
  const width = document.getElementById('graph').offsetWidth;
  const height = 800;
  const colors = {
    'paper': '#4285f4', 'project': '#ea4335', 'note': '#fbbc04',
    'idea': '#34a853', 'video': '#ff6d00', 'contact': '#9c27b0', 'domain': '#607d8b'
  };
  const svg = d3.select('#graph').append('svg').attr('width', width).attr('height', height);
  const g = svg.append('g');
  svg.call(d3.zoom().scaleExtent([0.1, 4]).on('zoom', (event) => g.attr('transform', event.transform)));
  const simulation = d3.forceSimulation(graphData.nodes)
    .force('link', d3.forceLink(graphData.links).id(d => d.id).distance(d => d.type === 'external' ? 150 : 100))
    .force('charge', d3.forceManyBody().strength(-300))
    .force('center', d3.forceCenter(width / 2, height / 2))
    .force('collision', d3.forceCollide().radius(30));
  const link = g.append('g').selectAll('line').data(graphData.links).join('line')
    .attr('class', d => 'link link-' + d.type)
    .attr('stroke', d => d.type === 'internal' ? '#999' : '#ccc')
    .attr('stroke-opacity', 0.6).attr('stroke-width', 1);
  const node = g.append('g').selectAll('g').data(graphData.nodes).join('g')
    .attr('class', d => 'node node-' + d.type).style('cursor', 'pointer')
    .call(d3.drag().on('start', dragstarted).on('drag', dragged).on('end', dragended));
  node.append('circle').attr('r', d => d.group === 'domain' ? 8 : 10)
    .attr('fill', d => colors[d.type] || '#999').attr('stroke', '#fff').attr('stroke-width', 2);
  node.append('text').text(d => d.group === 'domain' ? d.title : d.id)
    .attr('x', 12).attr('y', 4).attr('font-size', '10px').attr('fill', '#333');
  node.append('title').text(d => d.title + '\nType: ' + d.type);
  simulation.on('tick', () => {
    link.attr('x1', d => d.source.x).attr('y1', d => d.source.y).attr('x2', d => d.target.x).attr('y2', d => d.target.y);
    node.attr('transform', d => 'translate(' + d.x + ',' + d.y + ')');
  });
  function dragstarted(event) { if (!event.active) simulation.alphaTarget(0.3).restart(); event.subject.fx = event.subject.x; event.subject.fy = event.subject.y; }
  function dragged(event) { event.subject.fx = event.x; event.subject.fy = event.y; }
  function dragended(event) { if (!event.active) simulation.alphaTarget(0); event.subject.fx = null; event.subject.fy = null; }
  function updateFilters() {
    const activeTypes = new Set();
    document.querySelectorAll('.type-filter').forEach(cb => { if (cb.checked) activeTypes.add(cb.id.replace('filter-', '')); });
    const activeLinks = new Set();
    document.querySelectorAll('.link-filter').forEach(cb => { if (cb.checked) activeLinks.add(cb.id.replace('filter-', '')); });
    node.style('display', d => activeTypes.has(d.type) ? null : 'none');
    link.style('display', d => (activeTypes.has(d.source.type) && activeTypes.has(d.target.type) && activeLinks.has(d.type)) ? null : 'none');
    simulation.alpha(0.3).restart();
  }
  document.querySelectorAll('.type-filter, .link-filter').forEach(cb => cb.addEventListener('change', updateFilters));
  document.getElementById('reset-filters').addEventListener('click', () => {
    document.querySelectorAll('.type-filter, .link-filter').forEach(cb => cb.checked = true);
    updateFilters();
  });
}
|}]
  ] in

  page ~title ~description ~page_content:graph_html ~page_footer:Arod_footer.footer ()
