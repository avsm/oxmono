(*---------------------------------------------------------------------------
  Copyright (c) 2025 Anil Madhavapeddy <anil@recoil.org>. All rights reserved.
  SPDX-License-Identifier: ISC
 ---------------------------------------------------------------------------*)

(** Entry type filtering and rendering for Arod webserver *)

open Htmlit

(** Entry type filter *)
type entry_type = [ `Paper | `Note | `Video | `Idea | `Project ]

let entry_type_to_string = function
  | `Paper -> "paper"
  | `Note -> "note"
  | `Video -> "video"
  | `Idea -> "idea"
  | `Project -> "project"

let entry_type_of_string = function
  | "paper" -> Some `Paper
  | "note" -> Some `Note
  | "video" -> Some `Video
  | "idea" -> Some `Idea
  | "project" -> Some `Project
  | _ -> None

(** Helper functions for common attributes *)
let class_ c = At.class' c
let href h = At.href h

let render_entry (ent:Arod_model.Entry.entry) =
  let (t, _word_count_info) = match ent with
    | `Paper p -> Arod_papers.paper_for_entry p
    | `Note n -> Arod_notes.one_note_brief n
    | `Video v -> Arod_videos.one_video v
    | `Idea i -> Arod_ideas.one_idea_brief i
    | `Project p -> Arod_projects.one_project_brief p
  in
  El.splice [t; Arod_view.tags_meta ent]

let render_entry_for_feed ent =
  match ent with
  | `Paper p -> fst (Arod_papers.paper_for_feed p)
  | `Note n -> fst (Arod_notes.note_for_feed n)
  | `Video v -> fst (Arod_videos.video_for_feed v)
  | `Idea i -> fst (Arod_ideas.idea_for_feed i)
  | `Project p -> fst (Arod_projects.project_for_feed p)

let render_feed ent =
  let (entry_html, _word_count_info) = match ent with
    | `Paper p -> Arod_papers.paper_for_feed p
    | `Note n -> Arod_notes.note_for_feed n
    | `Video v -> Arod_videos.video_for_feed v
    | `Idea i -> Arod_ideas.idea_for_feed i
    | `Project p -> Arod_projects.project_for_feed p
  in
  El.splice [
    Arod_view.entry_href ent;
    entry_html;
    Arod_view.tags_meta ent
  ]

let render_backlinks_content ent =
  let slug = Arod_model.Entry.slug ent in
  let entry_type = match ent with
    | `Paper _ -> "paper"
    | `Note _ -> "note"
    | `Idea _ -> "idea"
    | `Project _ -> "project"
    | `Video _ -> "video"
  in
  let entries = Arod_model.get_entries () in
  let backlink_slugs = Bushel.Link_graph.get_backlinks_for_slug slug in
  if backlink_slugs = [] then
    None
  else
    let backlink_items = List.filter_map (fun backlink_slug ->
      match Arod_model.Entry.lookup entries backlink_slug with
      | Some entry ->
        let title = Arod_model.Entry.title entry in
        let url = Arod_model.Entry.site_url entry in
        Some (El.li [El.a ~at:[At.href url] [El.txt title]])
      | None -> None
    ) backlink_slugs in
    if backlink_items = [] then
      None
    else
      Some (El.splice [
        El.span ~at:[At.class' "sidenote-number"] [El.txt "↑"];
        El.span ~at:[At.class' "sidenote-icon"] [El.txt ""];
        El.txt (Printf.sprintf "The following entries link to this %s: " entry_type);
        El.ul backlink_items
      ])

let render_one_entry ent =
  match ent with
  | `Paper p -> Arod_papers.one_paper_full p, Arod_papers.one_paper_extra p
  | `Idea i -> Arod_ideas.one_idea_full i, El.splice []
  | `Note n -> Arod_notes.one_note_full n, El.splice []
  | `Video v -> Arod_videos.one_video_full v, El.splice []
  | `Project p -> Arod_projects.one_project_full p, El.splice []

type query_info = {
  tags: Arod_model.Tags.t list;
  min: int;
  show_all: bool;
}

let sort_of_ent ent =
  match ent with
  | `Paper p -> (match Arod_model.Paper.bibtype p with
    | "inproceedings" -> "conference paper"
    | "article" | "journal" -> "journal paper"
    | "misc" -> "preprint"
    | "techreport" -> "technical report"
    | _ -> "paper"), ""
  | `Note {Arod_model.Note.updated=Some _;date=u; _} ->
    "note", Printf.sprintf " (originally on %s)" (Arod_view.ptime_date ~with_d:true u)
  | `Note _ -> "note", ""
  | `Project _ -> "project", ""
  | `Idea _ -> "research idea", ""
  | `Video _ -> "video", ""

let footer = Arod_footer.footer

let take n l =
  let[@tail_mod_cons] rec aux n l =
    match n, l with
    | 0, _ | _, [] -> []
    | n, x::l -> x::aux (n - 1) l
  in
  if n < 0 then invalid_arg "List.take";
  aux n l

let feed_title_link ent =
  El.a ~at:[href (Arod_model.Entry.site_url ent)] [El.txt (Arod_model.Entry.title ent)]

let tags_heading tags =
  Arod_view.map_and Arod_model.Tags.to_raw_string tags

let view_news ~show_all ~tags ~min:_ ~types feed =
  let feed' =
    match show_all, List.length feed with
    | false, n when n > 25 -> take 25 feed
    | false, _ -> feed
    | true, _ -> feed
  in
  let title = "News " ^ (match tags with [] -> "" | tags -> " about " ^ (tags_heading tags)) in
  let description = Printf.sprintf "Showing %d news item(s)" (List.length feed') in
  let main_content =
    let rec intersperse_hr = function
      | [] -> []
      | [x] -> [render_feed x]
      | x::xs -> render_feed x :: El.hr () :: intersperse_hr xs
    in
    intersperse_hr feed' in
  let page_footer = El.splice [footer] in
  let pagination_attrs =
    let tags_str = String.concat "," (List.map Arod_model.Tags.to_raw_string tags) in
    let types_str = String.concat "," (List.map entry_type_to_string types) in
    [
      At.v "data-pagination" "true";
      At.v "data-collection-type" "feed";
      At.v "data-total-count" (string_of_int (List.length feed));
      At.v "data-current-count" (string_of_int (List.length feed'));
      At.v "data-tags" tags_str;
      At.v "data-types" types_str;
    ]
  in
  let page_content =
    El.splice [
      El.article ~at:pagination_attrs main_content;
      El.aside []
    ]
  in
  Arod_page.page ~title ~page_content ~page_footer ~description ()

let render_entries_html ents =
  let rendered = List.map render_entry ents in
  let rec add_separators = function
    | [] -> []
    | [x] -> [x]
    | x :: xs -> x :: El.hr () :: add_separators xs
  in
  let html_elements = El.hr () :: add_separators rendered in
  El.to_string ~doctype:false (El.splice html_elements)

let render_feeds_html feeds =
  let rec intersperse_hr = function
    | [] -> []
    | [x] -> [render_feed x]
    | x::xs -> render_feed x :: El.hr () :: intersperse_hr xs
  in
  let html_elements = El.hr () :: intersperse_hr feeds in
  El.to_string ~doctype:false (El.splice html_elements)

let view_entries ~show_all ~tags ~min:_ ~types ents =
  let ents' =
    match show_all, List.length ents with
    | false, n when n > 25 -> take 25 ents
    | false, _ -> ents
    | true, _ -> ents
  in
  let title = String.capitalize_ascii (tags_heading tags ^ (if tags <> [] then " " else "")) in
  let description = Printf.sprintf "Showing %d item(s)" (List.length ents') in
  let main_content =
    let rendered = List.map render_entry ents' in
    let rec add_separators = function
      | [] -> []
      | [x] -> [x]
      | x :: xs -> x :: El.hr () :: add_separators xs
    in
    add_separators rendered
  in
  let page_footer = El.splice [footer] in
  let pagination_attrs =
    let tags_str = String.concat "," (List.map Arod_model.Tags.to_raw_string tags) in
    let types_str = String.concat "," (List.map entry_type_to_string types) in
    [
      At.v "data-pagination" "true";
      At.v "data-collection-type" "entries";
      At.v "data-total-count" (string_of_int (List.length ents));
      At.v "data-current-count" (string_of_int (List.length ents'));
      At.v "data-tags" tags_str;
      At.v "data-types" types_str;
    ]
  in
  let page_content =
    El.splice [
      El.article ~at:pagination_attrs main_content;
      El.aside []
    ]
  in
  Arod_page.page ~title ~page_content ~page_footer ~description ()

let breadcrumbs cfg l = ("Home", cfg.Arod_config.site.base_url ^ "/") :: l

let view_one _q ent =
  let cfg = Arod_model.get_config () in
  let entries = Arod_model.get_entries () in
  let title = Arod_model.Entry.title ent in
  let description = match Arod_model.Entry.synopsis ent with Some v -> v | None -> "" in
  let eh, extra = render_one_entry ent in
  let is_index = Arod_model.Entry.is_index_entry ent in
  let standardsite = match ent with
    | `Note n -> Arod_model.Note.standardsite n
    | _ -> None
  in
  let backlinks_content =
    if is_index then None
    else render_backlinks_content ent
  in
  let related_container =
    match ent with
    | `Project _ -> El.splice []
    | _ when is_index -> El.splice []
    | `Note _ ->
      let tags = Arod_model.Entry.tags_of_ent entries ent in
      let tag_strings = List.map Arod_model.Tags.to_raw_string tags |> String.concat " " in
      El.div ~at:[
        class_ "related-items";
        At.v "data-entry-title" title;
        At.v "data-entry-id" (Arod_model.Entry.slug ent);
        At.v "data-entry-tags" tag_strings
      ] []
    | _ ->
      let tags = Arod_model.Entry.tags_of_ent entries ent in
      let tag_strings = List.map Arod_model.Tags.to_raw_string tags |> String.concat " " in
      El.splice [
        El.hr ();
        El.div ~at:[
          class_ "related-items";
          At.v "data-entry-title" title;
          At.v "data-entry-id" (Arod_model.Entry.slug ent);
          At.v "data-entry-tags" tag_strings
        ] []
      ]
  in
  let bs = Arod_richdata.(breadcrumbs @@ breadcrumb_of_ent cfg ent) in
  let jsonld = bs ^ (Arod_richdata.json_of_entry cfg ent) in
  let image = match Arod_model.Entry.thumbnail entries ent with
    | Some thumb -> cfg.site.base_url ^ thumb
    | None -> cfg.site.base_url ^ "/assets/imagetitle-default.jpg"
  in
  let page_footer, page_content =
    if is_index then
      let page_footer = footer in
      let page_content = El.splice [
        El.article [eh];
        El.aside []
      ] in
      page_footer, page_content
    else
      let page_footer = footer in
      let references_html = match ent with
        | `Note n -> El.splice [El.hr (); Arod_view.note_references_html n]
        | _ -> El.splice []
      in
      let page_content = El.splice [
        El.article [
          eh;
          Arod_view.tags_meta ?backlinks_content ent;
          references_html;
          related_container;
          extra
        ];
        El.aside []
      ] in
      page_footer, page_content
  in
  Arod_page.page ~image ~title ~jsonld ?standardsite ~page_content ~page_footer ~description ()

let filter_fn query_tags item_tags =
  let item_sets, item_text = List.partition (function `Set _ -> true | _ -> false) item_tags in
  let query_sets, query_text = List.partition (function `Set _ -> true | _ -> false) query_tags in
  let test_set seta setb =
    match setb with
    | [] -> true
    | setb -> List.exists (fun tag -> List.mem tag seta) setb
  in
  (test_set item_sets query_sets) &&
  (test_set item_text query_text)

let entry_matches_type types ent =
  if types = [] then true
  else
    List.exists (fun typ ->
      match typ, ent with
      | `Paper, `Paper _ -> true
      | `Note, `Note _ -> true
      | `Video, `Video _ -> true
      | `Idea, `Idea _ -> true
      | `Project, `Project _ -> true
      | _ -> false
    ) types

let feed_of_req ~types q =
  let entries = Arod_model.get_entries () in
  let filterent = entry_matches_type types in
  let select ent =
    let only_talks = function
      | `Video { Arod_model.Video.talk; _ } -> talk
      | _ -> true
    in
    let not_index_page = function
      | `Note { Arod_model.Note.index_page; _ } -> not index_page
      | _ -> true
    in
    only_talks ent && not_index_page ent
  in
  let all_entries = Arod_model.all_entries () in
  match q.tags with
  | [] ->
    all_entries
    |> List.filter (fun ent -> select ent && filterent ent)
    |> List.sort Arod_model.Entry.compare
    |> List.rev
  | t ->
    all_entries
    |> List.filter (fun ent ->
      select ent && filterent ent && filter_fn t (Arod_model.Entry.tags_of_ent entries ent))
    |> List.sort Arod_model.Entry.compare
    |> List.rev

let perma_feed_of_req () =
  let filterent ent =
    match ent with
    | `Note n -> Arod_model.Note.perma n
    | _ -> false
  in
  let all_entries = Arod_model.all_entries () in
  all_entries
  |> List.filter filterent
  |> List.sort Arod_model.Entry.compare
  |> List.rev

let entries_of_req ~extra_tags ~types q =
  let entries = Arod_model.get_entries () in
  let tags = Arod_model.concat_tags q.tags (List.map Arod_model.Tags.of_string extra_tags) in
  let q = { q with tags } in
  let filterent = entry_matches_type types in
  let select ent =
    let only_talks = function
      | `Video { Arod_model.Video.talk; _ } -> talk
      | _ -> true
    in
    let not_index_page = function
      | `Note { Arod_model.Note.index_page; _ } -> not index_page
      | _ -> true
    in
    only_talks ent && not_index_page ent
  in
  let all_entries = Arod_model.all_entries () in
  match q.tags with
  | [] ->
    all_entries
    |> List.filter (fun ent -> select ent && filterent ent)
    |> List.sort Arod_model.Entry.compare
    |> List.rev
  | ts ->
    all_entries
    |> List.filter (fun ent ->
      select ent && filterent ent && filter_fn ts (Arod_model.Entry.tags_of_ent entries ent))
    |> List.sort Arod_model.Entry.compare
    |> List.rev
