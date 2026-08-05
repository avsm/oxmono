(*---------------------------------------------------------------------------
  Copyright (c) 2025 Anil Madhavapeddy <anil@recoil.org>. All rights reserved.
  SPDX-License-Identifier: ISC
 ---------------------------------------------------------------------------*)

(** Bushel Web UI - dense debug-style knowledge base browser *)

module R = Httpz_server.Route
module H = Tw_html
module Entry = Bushel.Entry
module Link_graph = Bushel.Link_graph

(** {1 Helpers} *)

let format_date (year, month, day) =
  Printf.sprintf "%04d-%02d-%02d" year month day

let format_ptime t =
  Ptime.to_rfc3339 t

let type_string = Entry.to_type_string

let type_url = function
  | `Paper _ -> "/papers"
  | `Project _ -> "/projects"
  | `Idea _ -> "/ideas"
  | `Video _ -> "/videos"
  | `Note _ -> "/notes"

let entry_url entry =
  Printf.sprintf "%s/%s" (type_url entry) (Entry.slug entry)

let opt_str = function
  | Some s when s <> "" -> s
  | _ -> "\xe2\x80\x94"

let opt_str_raw = function
  | Some s -> s
  | None -> ""

let bool_str b = if b then "true" else "false"

(** {1 Styles} *)

(* Reusable style fragments for the dense UI *)
let mono = Tw.[font_mono; text_xs]

(** {1 Layout} *)

let tab_item ~active_tab label href =
  let is_active = active_tab = label in
  H.a
    ~at:[H.At.href href]
    ~tw:Tw.[
      px 2; py 1; text_xs; font_mono; no_underline;
      (if is_active then text_white else text gray 400);
      (if is_active then bg gray 700 else bg_transparent);
      hover [text_white; bg gray 600];
    ]
    [H.txt label]

let navbar ~active_tab =
  H.nav ~tw:Tw.[bg gray 900; border_b; border_color gray 700] [
    H.div ~tw:Tw.[max_w_7xl; mx_auto; px 2; flex; items_center; gap 1] [
      H.a ~at:[H.At.href "/"]
        ~tw:Tw.[font_mono; font_bold; text_xs; text green 400; py 1; px 2; no_underline]
        [H.txt "bushel"];
      H.span ~tw:Tw.[text gray 600; text_xs] [H.txt "|"];
      tab_item ~active_tab "notes" "/notes";
      tab_item ~active_tab "papers" "/papers";
      tab_item ~active_tab "projects" "/projects";
      tab_item ~active_tab "ideas" "/ideas";
      tab_item ~active_tab "videos" "/videos";
    ];
  ]

let layout ~title:page_title ~active_tab content =
  let body_content = [
    navbar ~active_tab;
    H.main ~tw:Tw.[max_w_7xl; mx_auto; px 2; py 2] content;
  ] in
  let head_extra = [
    H.raw "<style>img{max-height:48px;width:auto}ul.kv li::marker{content:'\b7  ';color:#d1d5db}</style>";
  ] in
  H.page ~title:page_title ~tw_css:"/tw.css" head_extra body_content

(** {1 Common UI components} *)

let kv_row k v =
  H.li ~tw:mono [
    H.span ~tw:Tw.[text gray 500] [H.txt (k ^ ": ")];
    H.span ~tw:Tw.[text gray 900] [H.txt v];
  ]

let kv_row_html k children =
  H.li ~tw:mono [
    H.span ~tw:Tw.[text gray 500] [H.txt (k ^ ": ")];
    H.span ~tw:Tw.[text gray 900] children;
  ]

let section_row label =
  H.li ~tw:Tw.[text_xs; font_bold; font_mono; text gray 400; uppercase;
                tracking_wide; pt 2; list_none] [H.txt label]

let kv_table rows =
  H.ul ~at:[H.At.v "class" "kv"] ~tw:Tw.[pl 4; list_inside; space_y 0] rows

let slug_link slug href =
  H.a ~at:[H.At.href href]
    ~tw:Tw.[text_xs; font_mono; text blue 500; no_underline; hover [underline]]
    [H.txt slug]

let entry_link entry =
  slug_link (Entry.slug entry) (entry_url entry)

let entry_link_with_title entry =
  H.a ~at:[H.At.href (entry_url entry)]
    ~tw:Tw.[text_xs; font_mono; text blue 500; no_underline; hover [underline]]
    [H.txt (Printf.sprintf "%s (%s)" (Entry.slug entry) (Entry.title entry))]

let tag_pill tag_str =
  H.span ~tw:Tw.[text_xs; font_mono; px 1; mr 1; bg gray 100; text gray 700; border; border_color gray 200]
    [H.txt tag_str]

let tag_list tags =
  if tags = [] then [H.txt "\xe2\x80\x94"]
  else List.map tag_pill tags

let link_list ~entries slugs =
  if slugs = [] then [H.txt "\xe2\x80\x94"]
  else List.map (fun slug ->
    match Entry.lookup entries slug with
    | Some entry ->
      H.div [entry_link_with_title entry]
    | None ->
      H.div [H.span ~tw:Tw.[text_xs; font_mono; text gray 400] [H.txt slug]]
  ) slugs

let external_link_list urls =
  if urls = [] then [H.txt "\xe2\x80\x94"]
  else List.map (fun url ->
    H.div [
      H.a ~at:[H.At.href url]
        ~tw:Tw.[text_xs; font_mono; text blue 500; no_underline; hover [underline]; break_all]
        [H.txt url]
    ]
  ) urls

(** {1 Link graph section} *)

let link_graph_section ~entries slug =
  let backlinks = Link_graph.get_backlinks_for_slug slug in
  let outbound = Link_graph.get_outbound_for_slug slug in
  let external_links = Link_graph.get_external_links_for_slug slug in
  kv_table [
    section_row "LINK GRAPH";
    kv_row_html "backlinks" (link_list ~entries backlinks);
    kv_row_html "outbound" (link_list ~entries outbound);
    kv_row_html "external" (external_link_list external_links);
    kv_row "backlink_count" (string_of_int (List.length backlinks));
    kv_row "outbound_count" (string_of_int (List.length outbound));
    kv_row "external_count" (string_of_int (List.length external_links));
  ]

(** {1 Tags section} *)

let tags_section ~entries entry =
  let tags = Entry.tags_of_ent entries entry in
  let tag_strs = List.filter_map (fun tag ->
    match tag with
    | `Text s -> Some s
    | `Year y -> Some (string_of_int y)
    | `Slug s -> Some (":" ^ s)
    | _ -> None
  ) tags in
  kv_table [
    section_row "TAGS";
    kv_row_html "tags" (tag_list tag_strs);
    kv_row "tag_count" (string_of_int (List.length tag_strs));
  ]

(** {1 Type-specific detail tables} *)

let note_metadata (n : Bushel.Note.t) =
  let words = Bushel.Note.words n in
  kv_table [
    section_row "NOTE METADATA";
    kv_row "slug" (Bushel.Note.slug n);
    kv_row "title" (Bushel.Note.title n);
    kv_row "date" (format_date (Bushel.Note.date n));
    kv_row "datetime" (format_ptime (Bushel.Note.datetime n));
    kv_row "draft" (bool_str (Bushel.Note.draft n));
    kv_row "perma" (bool_str (Bushel.Note.perma n));
    kv_row "index_page" (bool_str n.index_page);
    kv_row "doi" (opt_str (Bushel.Note.doi n));
    kv_row "synopsis" (opt_str (Bushel.Note.synopsis n));
    kv_row "titleimage" (opt_str (Bushel.Note.titleimage n));
    kv_row "slug_ent" (opt_str n.slug_ent);
    kv_row "source" (opt_str (Bushel.Note.source n));
    kv_row "url" (opt_str (Bushel.Note.url n));
    kv_row "author" (opt_str (Bushel.Note.author n));
    kv_row "category" (opt_str (Bushel.Note.category n));
    kv_row "standardsite" (opt_str (Bushel.Note.standardsite n));
    kv_row_html "tags" (tag_list (Bushel.Note.tags n));
    kv_row "word_count" (string_of_int words);
    (match Bushel.Note.sidebar n with
     | Some s -> kv_row "sidebar" (Printf.sprintf "(%d chars)" (String.length s))
     | None -> kv_row "sidebar" "\xe2\x80\x94");
    (match Bushel.Note.link n with
     | `Ext (label, url) -> kv_row "via" (Printf.sprintf "%s -> %s" label url)
     | `Local slug -> kv_row "via" (Printf.sprintf "local:%s" slug)
     | `None -> kv_row "via" "\xe2\x80\x94");
  ]

let paper_metadata (p : Bushel.Paper.t) =
  kv_table [
    section_row "PAPER METADATA";
    kv_row "slug" (Bushel.Paper.slug p);
    kv_row "title" (Bushel.Paper.title p);
    kv_row "authors" (String.concat "; " (Bushel.Paper.authors p));
    kv_row "year" (string_of_int (Bushel.Paper.year p));
    kv_row "date" (format_date (Bushel.Paper.date p));
    kv_row "bibtype" (Bushel.Paper.bibtype p);
    kv_row "classification" (match Bushel.Paper.classification p with
      | Bushel.Paper.Full -> "full"
      | Bushel.Paper.Short -> "short"
      | Bushel.Paper.Preprint -> "preprint");
    kv_row "selected" (bool_str (Bushel.Paper.selected p));
    kv_row "publisher" (let s = Bushel.Paper.publisher p in if s = "" then "\xe2\x80\x94" else s);
    kv_row "journal" (let s = Bushel.Paper.journal p in if s = "" then "\xe2\x80\x94" else s);
    kv_row "booktitle" (let s = Bushel.Paper.booktitle p in if s = "" then "\xe2\x80\x94" else s);
    kv_row "institution" (let s = Bushel.Paper.institution p in if s = "" then "\xe2\x80\x94" else s);
    kv_row "pages" (let s = Bushel.Paper.pages p in if s = "" then "\xe2\x80\x94" else s);
    kv_row "volume" (opt_str (Bushel.Paper.volume p));
    kv_row "number" (opt_str (Bushel.Paper.number p));
    kv_row "doi" (opt_str (Bushel.Paper.doi p));
    kv_row "url" (opt_str (Bushel.Paper.url p));
    kv_row "isbn" (let s = Bushel.Paper.isbn p in if s = "" then "\xe2\x80\x94" else s);
    kv_row "editor" (let s = Bushel.Paper.editor p in if s = "" then "\xe2\x80\x94" else s);
    kv_row "video" (opt_str (Bushel.Paper.video p));
    kv_row "note" (opt_str (Bushel.Paper.note p));
    kv_row_html "tags" (tag_list (Bushel.Paper.tags p));
    kv_row_html "projects" (tag_list (Bushel.Paper.project_slugs p));
    kv_row_html "slides" (tag_list (Bushel.Paper.slides p));
  ]

let project_metadata (p : Bushel.Project.t) =
  let finish_str = match Bushel.Project.finish p with
    | Some y -> string_of_int y
    | None -> "ongoing"
  in
  kv_table [
    section_row "PROJECT METADATA";
    kv_row "slug" (Bushel.Project.slug p);
    kv_row "title" (Bushel.Project.title p);
    kv_row "start" (string_of_int (Bushel.Project.start p));
    kv_row "finish" finish_str;
    kv_row_html "tags" (tag_list (Bushel.Project.tags p));
    kv_row "ideas" (let s = Bushel.Project.ideas p in if s = "" then "\xe2\x80\x94" else s);
  ]

let idea_metadata (i : Bushel.Idea.t) =
  kv_table [
    section_row "IDEA METADATA";
    kv_row "slug" (Bushel.Idea.slug i);
    kv_row "title" (Bushel.Idea.title i);
    kv_row "level" (Bushel.Idea.level_to_string (Bushel.Idea.level i));
    kv_row "status" (Bushel.Idea.status_to_string (Bushel.Idea.status i));
    kv_row "project" (Bushel.Idea.project i);
    kv_row "year" (string_of_int (Bushel.Idea.year i));
    kv_row "month" (string_of_int (Bushel.Idea.month i));
    kv_row_html "supervisors" (let sups = Bushel.Idea.supervisors i in
      if sups = [] then [H.txt "\xe2\x80\x94"]
      else List.map (fun c ->
        let h = Sortal_schema.Contact.handle c in
        let n = Sortal_schema.Contact.name c in
        tag_pill (Printf.sprintf "%s (%s)" h n)
      ) sups);
    kv_row_html "students" (let studs = Bushel.Idea.students i in
      if studs = [] then [H.txt "\xe2\x80\x94"]
      else List.map (fun c ->
        let h = Sortal_schema.Contact.handle c in
        let n = Sortal_schema.Contact.name c in
        tag_pill (Printf.sprintf "%s (%s)" h n)
      ) studs);
    kv_row "url" (opt_str (Bushel.Idea.url i));
    kv_row_html "tags" (tag_list (Bushel.Idea.tags i));
    kv_row "reading" (let s = Bushel.Idea.reading i in if s = "" then "\xe2\x80\x94" else s);
  ]

let video_metadata (v : Bushel.Video.t) =
  kv_table [
    section_row "VIDEO METADATA";
    kv_row "slug" (Bushel.Video.slug v);
    kv_row "uuid" (Bushel.Video.uuid v);
    kv_row "title" (Bushel.Video.title v);
    kv_row "date" (format_date (Bushel.Video.date v));
    kv_row "datetime" (format_ptime (Bushel.Video.datetime v));
    kv_row "url" (let s = Bushel.Video.url v in if s = "" then "\xe2\x80\x94" else s);
    kv_row "talk" (bool_str (Bushel.Video.talk v));
    kv_row "paper" (opt_str (Bushel.Video.paper v));
    kv_row "project" (opt_str (Bushel.Video.project v));
    kv_row_html "tags" (tag_list (Bushel.Video.tags v));
  ]

(** {1 List Views} *)

let list_header columns =
  H.thead [
    H.tr ~tw:Tw.[bg gray 50; border_b; border_color gray 200] (
      List.map (fun col ->
        H.th ~tw:Tw.[text_left; text_xs; font_mono; font_bold; text gray 500;
                      uppercase; tracking_wide; px 2; py 1]
          [H.txt col]
      ) columns
    )
  ]

let cell ?(tw_extra=[]) children =
  H.td ~tw:(Tw.[text_xs; font_mono; px 2; py 1; align_top; border_b; border_color gray 100] @ tw_extra) children

let cell_text ?(tw_extra=[]) s = cell ~tw_extra [H.txt s]

let cell_link slug href =
  cell [slug_link slug href]

let notes_page entries =
  let notes = Entry.notes entries
    |> List.sort Bushel.Note.compare
    |> List.filter (fun n -> not (Bushel.Note.draft n)) in
  let rows = List.map (fun n ->
    let entry = `Note n in
    let tags = Bushel.Note.tags n in
    H.tr ~tw:Tw.[hover [bg gray 50]] [
      cell_link (Bushel.Note.slug n) (entry_url entry);
      cell_text (Bushel.Note.title n);
      cell_text (format_date (Bushel.Note.date n));
      cell [H.div ~tw:Tw.[flex; flex_wrap; gap 1] (tag_list tags)];
      cell_text ~tw_extra:Tw.[text gray 500] (opt_str (Bushel.Note.synopsis n));
      cell_text ~tw_extra:Tw.[text_right; text gray 400] (string_of_int (Bushel.Note.words n));
      cell_text ~tw_extra:Tw.[text_right; text gray 400] (bool_str (Bushel.Note.perma n));
    ]
  ) notes in
  let content = [
    H.div ~tw:Tw.[flex; items_center; gap 2; py 1] [
      H.span ~tw:(mono @ Tw.[font_bold; text gray 700]) [H.txt "notes"];
      H.span ~tw:(mono @ Tw.[text gray 400])
        [H.txt (Printf.sprintf "(%d entries)" (List.length notes))];
    ];
    H.table ~tw:Tw.[w_full; border_collapse; table_auto] [
      list_header ["slug"; "title"; "date"; "tags"; "synopsis"; "words"; "perma"];
      H.tbody rows;
    ];
  ] in
  layout ~title:"notes" ~active_tab:"notes" content

let papers_page entries =
  let papers = Entry.papers entries
    |> List.sort Bushel.Paper.compare in
  let rows = List.map (fun p ->
    let entry = `Paper p in
    let cls = match Bushel.Paper.classification p with
      | Bushel.Paper.Full -> "full"
      | Bushel.Paper.Short -> "short"
      | Bushel.Paper.Preprint -> "preprint"
    in
    H.tr ~tw:Tw.[hover [bg gray 50]] [
      cell_link (Bushel.Paper.slug p) (entry_url entry);
      cell_text (Bushel.Paper.title p);
      cell_text (String.concat "; " (Bushel.Paper.authors p));
      cell_text (string_of_int (Bushel.Paper.year p));
      cell_text (let j = Bushel.Paper.journal p in
                 if j <> "" then j else Bushel.Paper.booktitle p);
      cell_text cls;
      cell_text ~tw_extra:Tw.[text_right] (bool_str (Bushel.Paper.selected p));
    ]
  ) papers in
  let content = [
    H.div ~tw:Tw.[flex; items_center; gap 2; py 1] [
      H.span ~tw:(mono @ Tw.[font_bold; text gray 700]) [H.txt "papers"];
      H.span ~tw:(mono @ Tw.[text gray 400])
        [H.txt (Printf.sprintf "(%d entries)" (List.length papers))];
    ];
    H.table ~tw:Tw.[w_full; border_collapse; table_auto] [
      list_header ["slug"; "title"; "authors"; "year"; "venue"; "class"; "sel"];
      H.tbody rows;
    ];
  ] in
  layout ~title:"papers" ~active_tab:"papers" content

let projects_page entries =
  let projects = Entry.projects entries
    |> List.sort Bushel.Project.compare in
  let rows = List.map (fun p ->
    let entry = `Project p in
    let finish_str = match Bushel.Project.finish p with
      | Some y -> string_of_int y
      | None -> "ongoing"
    in
    let tags = Bushel.Project.tags p in
    H.tr ~tw:Tw.[hover [bg gray 50]] [
      cell_link (Bushel.Project.slug p) (entry_url entry);
      cell_text (Bushel.Project.title p);
      cell_text (string_of_int (Bushel.Project.start p));
      cell_text finish_str;
      cell [H.div ~tw:Tw.[flex; flex_wrap; gap 1] (tag_list tags)];
    ]
  ) projects in
  let content = [
    H.div ~tw:Tw.[flex; items_center; gap 2; py 1] [
      H.span ~tw:(mono @ Tw.[font_bold; text gray 700]) [H.txt "projects"];
      H.span ~tw:(mono @ Tw.[text gray 400])
        [H.txt (Printf.sprintf "(%d entries)" (List.length projects))];
    ];
    H.table ~tw:Tw.[w_full; border_collapse; table_auto] [
      list_header ["slug"; "title"; "start"; "finish"; "tags"];
      H.tbody rows;
    ];
  ] in
  layout ~title:"projects" ~active_tab:"projects" content

let ideas_page entries =
  let ideas = Entry.ideas entries
    |> List.sort Bushel.Idea.compare in
  let rows = List.map (fun i ->
    let entry = `Idea i in
    H.tr ~tw:Tw.[hover [bg gray 50]] [
      cell_link (Bushel.Idea.slug i) (entry_url entry);
      cell_text (Bushel.Idea.title i);
      cell_text (Bushel.Idea.level_to_string (Bushel.Idea.level i));
      cell_text (Bushel.Idea.status_to_string (Bushel.Idea.status i));
      cell_text (Bushel.Idea.project i);
      cell_text (Printf.sprintf "%04d-%02d" (Bushel.Idea.year i) (Bushel.Idea.month i));
    ]
  ) ideas in
  let content = [
    H.div ~tw:Tw.[flex; items_center; gap 2; py 1] [
      H.span ~tw:(mono @ Tw.[font_bold; text gray 700]) [H.txt "ideas"];
      H.span ~tw:(mono @ Tw.[text gray 400])
        [H.txt (Printf.sprintf "(%d entries)" (List.length ideas))];
    ];
    H.table ~tw:Tw.[w_full; border_collapse; table_auto] [
      list_header ["slug"; "title"; "level"; "status"; "project"; "date"];
      H.tbody rows;
    ];
  ] in
  layout ~title:"ideas" ~active_tab:"ideas" content

let videos_page entries =
  let videos = Entry.videos entries
    |> List.sort Bushel.Video.compare in
  let rows = List.map (fun v ->
    let entry = `Video v in
    let url_str = Bushel.Video.url v in
    H.tr ~tw:Tw.[hover [bg gray 50]] [
      cell_link (Bushel.Video.slug v) (entry_url entry);
      cell_text (Bushel.Video.title v);
      cell_text (format_date (Bushel.Video.date v));
      cell_text (bool_str (Bushel.Video.talk v));
      cell_text (opt_str_raw (Bushel.Video.paper v));
      cell [if url_str <> "" then
              H.a ~at:[H.At.href url_str]
                ~tw:Tw.[text_xs; font_mono; text blue 500; no_underline; hover [underline]]
                [H.txt (String.sub url_str 0 (min 40 (String.length url_str)) ^ (if String.length url_str > 40 then "..." else ""))]
            else H.txt "\xe2\x80\x94"];
    ]
  ) videos in
  let content = [
    H.div ~tw:Tw.[flex; items_center; gap 2; py 1] [
      H.span ~tw:(mono @ Tw.[font_bold; text gray 700]) [H.txt "videos"];
      H.span ~tw:(mono @ Tw.[text gray 400])
        [H.txt (Printf.sprintf "(%d entries)" (List.length videos))];
    ];
    H.table ~tw:Tw.[w_full; border_collapse; table_auto] [
      list_header ["slug"; "title"; "date"; "talk"; "paper"; "url"];
      H.tbody rows;
    ];
  ] in
  layout ~title:"videos" ~active_tab:"videos" content

(** {1 Detail Views} *)

let render_markdown ~entries body =
  let md = Bushel.Md.to_markdown ~base_url:"" ~image_base:"/images" ~entries body in
  let doc = Cmarkit.Doc.of_string ~strict:false md in
  Cmarkit_html.of_doc ~safe:false doc

let detail_page ~entries entry ~view =
  let active_tab = match entry with
    | `Note _ -> "notes"
    | `Paper _ -> "papers"
    | `Project _ -> "projects"
    | `Idea _ -> "ideas"
    | `Video _ -> "videos"
  in
  let title = Entry.title entry in
  let slug = Entry.slug entry in
  let toggle_url = match view with
    | `Rendered -> Printf.sprintf "%s?view=source" (entry_url entry)
    | `Source -> entry_url entry
  in
  let toggle_label = match view with
    | `Rendered -> "view=source"
    | `Source -> "view=rendered"
  in
  (* Type-specific metadata table *)
  let type_meta = match entry with
    | `Note n -> note_metadata n
    | `Paper p -> paper_metadata p
    | `Project p -> project_metadata p
    | `Idea i -> idea_metadata i
    | `Video v -> video_metadata v
  in
  (* Body content *)
  let body_content = match view with
    | `Rendered ->
      let body = Entry.body entry in
      if body = "" then
        H.div ~tw:(mono @ Tw.[text gray 400; p 2]) [H.txt "(no body content)"]
      else
        let html = render_markdown ~entries body in
        H.div ~tw:Tw.[prose; prose_sm; max_w_none; font_mono; text_xs] [H.raw html]
    | `Source ->
      let body = Entry.body entry in
      if body = "" then
        H.div ~tw:(mono @ Tw.[text gray 400; p 2]) [H.txt "(no body content)"]
      else
        H.pre ~tw:Tw.[bg gray 50; p 2; text_xs; font_mono; overflow_auto;
                       border; border_color gray 200] [
          H.code [H.txt body]
        ]
  in
  (* Paper-specific: abstract and bibtex *)
  let extra_content = match entry with
    | `Paper p ->
      let abstract = Bushel.Paper.abstract p in
      let bib = Bushel.Paper.bib p in
      [
        (if abstract <> "" then
           H.div ~tw:Tw.[mt 2] [
             kv_table [
               section_row "ABSTRACT";
               H.li ~tw:Tw.[text_xs; font_mono; text gray 800; list_none]
                 [H.txt abstract];
             ]
           ]
         else H.empty);
        (if bib <> "" then
           H.div ~tw:Tw.[mt 2] [
             kv_table [
               section_row "BIBTEX";
               H.li ~tw:Tw.[list_none] [
                 H.pre ~tw:Tw.[bg gray 50; p 2; text_xs; font_mono; overflow_auto;
                                border; border_color gray 100; m 0] [
                   H.code [H.txt bib]
                 ]
               ];
             ]
           ]
         else H.empty);
      ]
    | `Idea i ->
      let reading = Bushel.Idea.reading i in
      if reading <> "" then [
        H.div ~tw:Tw.[mt 2] [
          kv_table [
            section_row "READING";
            H.li ~tw:Tw.[text_xs; font_mono; text gray 800; list_none]
              [H.raw (render_markdown ~entries reading)];
          ]
        ]
      ]
      else []
    | `Note n ->
      (match Bushel.Note.sidebar n with
       | Some sidebar_text when sidebar_text <> "" -> [
           H.div ~tw:Tw.[mt 2] [
             kv_table [
               section_row "SIDEBAR";
               H.li ~tw:Tw.[text_xs; font_mono; text gray 800; list_none]
                 [H.raw (render_markdown ~entries sidebar_text)];
             ]
           ]
         ]
       | _ -> [])
    | _ -> []
  in
  (* Images *)
  let images_section =
    let thumb = Entry.thumbnail entries entry in
    let thumb_slug = Entry.thumbnail_slug entries entry in
    (* Collect image slugs referenced in the body via :slug syntax *)
    let body = Entry.body entry in
    let body_image_slugs =
      let re = Re.(compile (seq [char ':'; group (rep1 (alt [wordc; char '-']))])) in
      let matches = Re.all re body in
      List.filter_map (fun g ->
        let s = Re.Group.get g 1 in
        match Entry.lookup_image entries s with
        | Some _ -> Some s
        | None -> None
      ) matches
    in
    (* Deduplicate and build image list *)
    let all_image_slugs =
      (match thumb_slug with Some s -> [s] | None -> []) @
      List.filter (fun s -> Some s <> thumb_slug) body_image_slugs
    in
    let seen = Hashtbl.create 16 in
    let unique_slugs = List.filter (fun s ->
      if Hashtbl.mem seen s then false
      else (Hashtbl.add seen s (); true)
    ) all_image_slugs in
    let image_rows = List.filter_map (fun img_slug ->
      match Entry.lookup_image entries img_slug with
      | Some img ->
        let url = Entry.smallest_webp_variant img in
        let (w, h) = Srcsetter.dims img in
        Some (kv_row_html img_slug [
          H.div ~tw:Tw.[flex; items_center; gap 2] [
            H.img ~at:[H.At.src url; H.At.alt img_slug; H.At.height 48]
              ~tw:Tw.[] ();
            H.span ~tw:Tw.[text_xs; font_mono; text gray 400]
              [H.txt (Printf.sprintf "%dx%d" w h)];
          ];
        ])
      | None -> None
    ) unique_slugs in
    if image_rows = [] then
      kv_table [
        section_row "IMAGES";
        kv_row "thumbnail" (match thumb with Some url -> url | None -> "\xe2\x80\x94");
      ]
    else
      kv_table (
        section_row "IMAGES" ::
        kv_row "thumbnail" (match thumb with Some url -> url | None -> "\xe2\x80\x94") ::
        image_rows
      )
  in
  (* Related notes *)
  let related_notes = Entry.notes_for_slug entries slug in
  let related_section =
    if related_notes = [] then H.empty
    else
      H.div ~tw:Tw.[mt 2] [
        kv_table (
          section_row "RELATED NOTES" ::
          List.map (fun n ->
            let ne = `Note n in
            kv_row_html (Bushel.Note.slug n) [
              slug_link (Bushel.Note.title n) (entry_url ne)
            ]
          ) related_notes
        )
      ]
  in
  let content = [
    (* Header bar *)
    H.div ~tw:Tw.[flex; items_center; justify_between; py 1; border_b; border_color gray 200] [
      H.div ~tw:Tw.[flex; items_center; gap 1] [
        H.a ~at:[H.At.href (type_url entry)]
          ~tw:(mono @ Tw.[text blue 500; no_underline; hover [underline]])
          [H.txt active_tab];
        H.span ~tw:(mono @ Tw.[text gray 400]) [H.txt "/"];
        H.span ~tw:(mono @ Tw.[text gray 700; font_bold]) [H.txt slug];
        H.span ~tw:(mono @ Tw.[text gray 300]) [H.txt "\xe2\x80\x94"];
        H.span ~tw:(mono @ Tw.[text gray 600]) [H.txt title];
      ];
      H.a ~at:[H.At.href toggle_url]
        ~tw:(mono @ Tw.[text blue 500; no_underline; hover [underline]])
        [H.txt toggle_label];
    ];
    (* Metadata *)
    H.div ~tw:Tw.[mt 2] [type_meta];
    (* Tags *)
    H.div ~tw:Tw.[mt 2] [tags_section ~entries entry];
    (* Link graph *)
    H.div ~tw:Tw.[mt 2] [link_graph_section ~entries slug];
    (* Images *)
    H.div ~tw:Tw.[mt 2] [images_section];
    (* Related notes *)
    related_section;
    (* Body *)
    H.div ~tw:Tw.[mt 2] [
      kv_table [section_row (match view with `Rendered -> "BODY (RENDERED)" | `Source -> "BODY (SOURCE)")];
    ];
    H.div ~tw:Tw.[mt 1; border; border_color gray 200; p 2] [body_content];
  ] @ extra_content in
  layout ~title:(Printf.sprintf "%s | %s" slug active_tab) ~active_tab content

(** {1 CSS Generation} *)

let reference_page entries =
  let all = Entry.all_entries entries in
  let sample_entry = match all with
    | e :: _ -> Some e
    | [] -> None
  in
  (* Build a page that exercises all Tw utilities used in the UI *)
  layout ~title:"Reference" ~active_tab:"notes" [
    navbar ~active_tab:"notes";
    navbar ~active_tab:"papers";
    (* List table styles *)
    H.table ~tw:Tw.[w_full; border_collapse; table_auto] [
      list_header ["a"; "b"; "c"];
      H.tbody [
        H.tr ~tw:Tw.[hover [bg gray 50]] [
          cell_text "x";
          cell_text ~tw_extra:Tw.[text_right; text gray 400] "y";
          cell_text ~tw_extra:Tw.[text gray 500] "z";
        ];
      ];
    ];
    (* KV list styles *)
    kv_table [
      section_row "SECTION";
      kv_row "key" "val";
      kv_row_html "key2" [H.txt "val2"];
    ];
    (* Extra content list items *)
    H.li ~tw:Tw.[text_xs; font_mono; text gray 800; list_none] [H.txt "content"];
    H.li ~tw:Tw.[list_none] [H.txt "block"];
    (* Tag pills *)
    H.div ~tw:Tw.[flex; flex_wrap; gap 1] (tag_list ["a"; "b"]);
    tag_pill "x";
    (* Links *)
    slug_link "slug" "/url";
    H.a ~tw:Tw.[text_xs; font_mono; text blue 500; no_underline; hover [underline]; break_all]
      [H.txt "ext"];
    (* Entry links *)
    (match sample_entry with
     | Some e -> entry_link e
     | None -> H.empty);
    (match sample_entry with
     | Some e -> entry_link_with_title e
     | None -> H.empty);
    (* Mono styles *)
    H.span ~tw:(mono @ Tw.[font_bold; text gray 700]) [H.txt "header"];
    H.span ~tw:(mono @ Tw.[text gray 400]) [H.txt "count"];
    H.span ~tw:(mono @ Tw.[text gray 300]) [H.txt "sep"];
    H.span ~tw:(mono @ Tw.[text gray 600]) [H.txt "sub"];
    H.span ~tw:(mono @ Tw.[text gray 700; font_bold]) [H.txt "slug"];
    H.span ~tw:(mono @ Tw.[text blue 500; no_underline; hover [underline]]) [H.txt "l"];
    H.span ~tw:Tw.[text gray 500] [H.txt "label"];
    H.span ~tw:Tw.[text gray 900] [H.txt "value"];
    (* Body styles *)
    H.div ~tw:Tw.[prose; prose_sm; max_w_none; font_mono; text_xs] [H.txt "body"];
    H.pre ~tw:Tw.[bg gray 50; p 2; text_xs; font_mono; overflow_auto;
                   border; border_color gray 200] [
      H.code [H.txt "source"]
    ];
    H.pre ~tw:Tw.[bg gray 50; p 2; text_xs; font_mono; overflow_auto;
                   border; border_color gray 100; m 0] [
      H.code [H.txt "bib"]
    ];
    (* Detail page styles *)
    H.div ~tw:Tw.[flex; items_center; justify_between; py 1; border_b; border_color gray 200] [H.txt "x"];
    H.div ~tw:Tw.[mt 1; border; border_color gray 200; p 2] [H.txt "body"];
    H.div ~tw:Tw.[mt 2] [H.txt "mt2"];
    H.div ~tw:(mono @ Tw.[text gray 400; p 2]) [H.txt "empty"];
    H.div ~tw:Tw.[flex; items_center; gap 2] [H.txt "img row"];
    (* Tab styles *)
    H.a ~tw:Tw.[px 2; py 1; text_xs; font_mono; no_underline;
                 text_white; bg gray 700; hover [text_white; bg gray 600]]
      [H.txt "active"];
    H.a ~tw:Tw.[px 2; py 1; text_xs; font_mono; no_underline;
                 text gray 400; bg_transparent; hover [text_white; bg gray 600]]
      [H.txt "inactive"];
    (* Navbar *)
    H.span ~tw:Tw.[text gray 600; text_xs] [H.txt "|"];
    H.a ~tw:Tw.[font_mono; font_bold; text_xs; text green 400; py 1; px 2; no_underline]
      [H.txt "brand"];
  ]

let generate_css entries =
  let page = reference_page entries in
  let (_css_name, css) = H.css page in
  Tw.Css.to_string ~minify:true css

(** {1 Response Helpers} *)

let[@inline] send_css (local_ respond) s =
  respond ~status:Httpz.Res.Success
    ~headers:[(Httpz.Header_name.Content_type, "text/css; charset=utf-8")]
    (R.String s)

let mime_of_path path =
  if String.ends_with ~suffix:".webp" path then "image/webp"
  else if String.ends_with ~suffix:".png" path then "image/png"
  else if String.ends_with ~suffix:".jpg" path || String.ends_with ~suffix:".jpeg" path then "image/jpeg"
  else if String.ends_with ~suffix:".svg" path then "image/svg+xml"
  else if String.ends_with ~suffix:".gif" path then "image/gif"
  else "application/octet-stream"

let[@inline] send_file (local_ respond) ~mime s =
  respond ~status:Httpz.Res.Success
    ~headers:[(Httpz.Header_name.Content_type, mime)]
    (R.String s)

let[@inline] send_not_found (local_ respond) =
  respond ~status:Httpz.Res.Not_found ~headers:[] (R.String "Not Found")

let static_file ~dir path _ctx (local_ respond) =
  let parts = String.split_on_char '/' path in
  let safe_parts = List.filter (fun s -> s <> ".." && s <> ".") parts in
  let clean_path = String.concat "/" safe_parts in
  let file_path = Filename.concat dir clean_path in
  try
    if Sys.file_exists file_path && not (Sys.is_directory file_path) then begin
      let mime = mime_of_path file_path in
      let ic = open_in_bin file_path in
      let len = in_channel_length ic in
      let content = really_input_string ic len in
      close_in ic;
      send_file respond ~mime content
    end
    else send_not_found respond
  with _ -> send_not_found respond

(** {1 Route Handlers} *)

let routes ?image_dir entries =
  let css_content = generate_css entries in
  let open R in
  let image_routes = match image_dir with
    | Some dir -> [get ("images" / tail) (fun path ->
        static_file ~dir (String.concat "/" path))]
    | None -> []
  in
  of_list (image_routes @ [
    (* Root redirect *)
    get_ [] (fun _ctx (local_ respond) ->
      redirect respond ~status:Httpz.Res.Moved_permanently ~location:"/notes");

    (* CSS *)
    get_ ["tw.css"] (fun _ctx (local_ respond) ->
      send_css respond css_content);

    (* List pages *)
    get_ ["notes"] (fun ctx (local_ respond) ->
      html_gen ctx respond (fun () ->
        let page = notes_page entries in
        H.html page));

    get_ ["papers"] (fun ctx (local_ respond) ->
      html_gen ctx respond (fun () ->
        let page = papers_page entries in
        H.html page));

    get_ ["projects"] (fun ctx (local_ respond) ->
      html_gen ctx respond (fun () ->
        let page = projects_page entries in
        H.html page));

    get_ ["ideas"] (fun ctx (local_ respond) ->
      html_gen ctx respond (fun () ->
        let page = ideas_page entries in
        H.html page));

    get_ ["videos"] (fun ctx (local_ respond) ->
      html_gen ctx respond (fun () ->
        let page = videos_page entries in
        H.html page));

    (* Detail pages *)
    get ("notes" / seg root) (fun (slug, ()) ctx (local_ respond) ->
      match Entry.lookup entries slug with
      | Some ((`Note _) as entry) ->
        let view = match query_param ctx "view" with
          | Some "source" -> `Source
          | _ -> `Rendered
        in
        html_gen ctx respond (fun () ->
          let page = detail_page ~entries entry ~view in
          H.html page)
      | _ -> not_found respond);

    get ("papers" / seg root) (fun (slug, ()) ctx (local_ respond) ->
      match Entry.lookup entries slug with
      | Some ((`Paper _) as entry) ->
        let view = match query_param ctx "view" with
          | Some "source" -> `Source
          | _ -> `Rendered
        in
        html_gen ctx respond (fun () ->
          let page = detail_page ~entries entry ~view in
          H.html page)
      | _ -> not_found respond);

    get ("projects" / seg root) (fun (slug, ()) ctx (local_ respond) ->
      match Entry.lookup entries slug with
      | Some ((`Project _) as entry) ->
        let view = match query_param ctx "view" with
          | Some "source" -> `Source
          | _ -> `Rendered
        in
        html_gen ctx respond (fun () ->
          let page = detail_page ~entries entry ~view in
          H.html page)
      | _ -> not_found respond);

    get ("ideas" / seg root) (fun (slug, ()) ctx (local_ respond) ->
      match Entry.lookup entries slug with
      | Some ((`Idea _) as entry) ->
        let view = match query_param ctx "view" with
          | Some "source" -> `Source
          | _ -> `Rendered
        in
        html_gen ctx respond (fun () ->
          let page = detail_page ~entries entry ~view in
          H.html page)
      | _ -> not_found respond);

    get ("videos" / seg root) (fun (slug, ()) ctx (local_ respond) ->
      match Entry.lookup entries slug with
      | Some ((`Video _) as entry) ->
        let view = match query_param ctx "view" with
          | Some "source" -> `Source
          | _ -> `Rendered
        in
        html_gen ctx respond (fun () ->
          let page = detail_page ~entries entry ~view in
          H.html page)
      | _ -> not_found respond);
  ])
