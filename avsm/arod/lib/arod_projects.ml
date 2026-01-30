(*---------------------------------------------------------------------------
  Copyright (c) 2025 Anil Madhavapeddy <anil@recoil.org>. All rights reserved.
  SPDX-License-Identifier: ISC
 ---------------------------------------------------------------------------*)

(** Project rendering for Arod webserver *)

open Htmlit
open Printf

module MP = Arod_model.Project
module StringSet = Set.Make(String)

let class_ c = At.class' c

let ideas_for_project entries project =
  List.filter (fun i -> Arod_model.Idea.project i = project.MP.slug)
    (Arod_model.Entry.ideas entries)

let project_for_feed p =
  let (body_html, word_count_info) = Arod_view.truncated_body (`Project p) in
  (El.div [body_html], word_count_info)

let one_project_brief p =
  let entries = Arod_model.get_entries () in
  let idea_items = ideas_for_project entries p
    |> List.sort Arod_model.Idea.compare
    |> List.map (fun i ->
      El.li [Arod_ideas.idea_to_html_no_sidenotes i]
    ) in
  let (body_html, word_count_info) = Arod_view.truncated_body (`Project p) in
  (El.splice [
    Arod_view.entry_href (`Project p);
    body_html;
    El.ul idea_items
  ], word_count_info)

let one_project_full p =
  let entries = Arod_model.get_entries () in
  let project_slug = p.MP.slug in

  let backlink_slugs = Bushel.Link_graph.get_backlinks_for_slug project_slug in
  let backlink_set = List.fold_left (fun acc slug ->
    StringSet.add slug acc
  ) StringSet.empty backlink_slugs in

  let all_entries = Arod_model.all_entries () in

  let project_papers = List.filter (fun e ->
    match e with
    | `Paper paper -> List.mem project_slug (Arod_model.Paper.project_slugs paper)
    | _ -> false
  ) all_entries |> List.sort (fun a b ->
    compare (Arod_model.Entry.date b) (Arod_model.Entry.date a)
  ) in

  let recent_activity = List.filter (fun e ->
    match e with
    | `Paper _ -> false
    | _ -> StringSet.mem (Arod_model.Entry.slug e) backlink_set
  ) all_entries |> List.sort (fun a b ->
    compare (Arod_model.Entry.date b) (Arod_model.Entry.date a)
  ) in

  let activity_section =
    if recent_activity = [] then El.splice []
    else
      let activity_items = List.map (fun ent ->
        let icon_name = Arod_view.ent_to_icon ent in
        let date_str = Arod_view.ptime_date ~with_d:false (Arod_model.Entry.date ent) in

        let lookup_title slug =
          match Arod_model.Entry.lookup entries slug with
          | Some ent -> Some (Arod_model.Entry.title ent)
          | None -> None
        in

        let description = match ent with
          | `Paper paper -> Bushel.Description.paper_description paper ~date_str
          | `Note n -> Bushel.Description.note_description n ~date_str ~lookup_fn:lookup_title
          | `Idea i -> Bushel.Description.idea_description i ~date_str
          | `Video v -> Bushel.Description.video_description v ~date_str ~lookup_fn:lookup_title
          | `Project pr -> Bushel.Description.project_description pr
        in

        El.li [
          El.img ~at:[
            At.alt "icon";
            At.class' "inline-icon";
            At.src (sprintf "/assets/%s" icon_name)
          ] ();
          El.a ~at:[At.href (Arod_model.Entry.site_url ent)] [
            El.txt (Arod_model.Entry.title ent)
          ];
          El.txt " – ";
          El.span ~at:[At.class' "activity-description"] [El.txt description]
        ]
      ) recent_activity in
      El.splice [
        El.h1 [El.txt "Activity"];
        El.ul ~at:[At.class' "activity-list"] activity_items
      ]
  in

  let references_section =
    if project_papers = [] then El.splice []
    else
      let paper_items = List.map (fun ent ->
        match ent with
        | `Paper paper -> Arod_papers.paper_for_entry paper |> fst
        | _ -> El.splice []
      ) project_papers in
      El.splice [
        El.h1 [El.txt "References"];
        El.splice paper_items
      ]
  in

  let title = MP.title p in

  El.div ~at:[class_ "project"] [
    El.h1 [El.txt title];
    El.p [Arod_view.full_body (`Project p)];
    activity_section;
    references_section
  ]

let view_projects_timeline () =
  let entries = Arod_model.get_entries () in
  let all_projects = Arod_model.Entry.projects entries
    |> List.sort MP.compare
    |> List.rev in

  if all_projects = [] then
    El.div [El.txt "No projects found"]
  else
    let current_year = let (y, _, _), _ = Ptime.to_date_time (Ptime_clock.now ()) in y in

    let project_cards = List.map (fun p ->
      let start_year = p.MP.start in
      let end_year = match p.MP.finish with Some y -> y | None -> current_year in
      let duration = end_year - start_year in

      let all_entries = Arod_model.all_entries () in
      let project_slug = p.MP.slug in

      let recent_papers = List.filter (fun e ->
        match e with
        | `Paper paper -> List.mem project_slug (Arod_model.Paper.project_slugs paper)
        | _ -> false
      ) all_entries |> List.sort (fun a b ->
        compare (Arod_model.Entry.date b) (Arod_model.Entry.date a)
      ) |> (fun l -> if List.length l > 3 then List.filteri (fun i _ -> i < 3) l else l) in

      let backlink_slugs = Bushel.Link_graph.get_backlinks_for_slug project_slug in
      let backlink_set = List.fold_left (fun acc slug ->
        StringSet.add slug acc
      ) StringSet.empty backlink_slugs in

      let recent_notes = List.filter (fun e ->
        match e with
        | `Note _ -> StringSet.mem (Arod_model.Entry.slug e) backlink_set
        | _ -> false
      ) all_entries |> List.sort (fun a b ->
        compare (Arod_model.Entry.date b) (Arod_model.Entry.date a)
      ) |> (fun l -> if List.length l > 3 then List.filteri (fun i _ -> i < 3) l else l) in

      let recent_items_display =
        let paper_items = List.map (fun ent ->
          El.li [
            El.a ~at:[At.href (Arod_model.Entry.site_url ent)] [
              El.txt (Arod_model.Entry.title ent)
            ]
          ]
        ) recent_papers in
        let note_items = List.map (fun ent ->
          El.li [
            El.a ~at:[At.href (Arod_model.Entry.site_url ent)] [
              El.txt (Arod_model.Entry.title ent)
            ]
          ]
        ) recent_notes in

        if paper_items = [] && note_items = [] then El.splice []
        else
          El.div ~at:[At.class' "project-recent-items"] [
            (if paper_items <> [] then
              El.div ~at:[At.class' "project-recent-column"] [
                El.h4 [El.txt "Recent papers"];
                El.ul paper_items
              ]
            else El.splice []);
            (if note_items <> [] then
              El.div ~at:[At.class' "project-recent-column"] [
                El.h4 [El.txt "Recent notes"];
                El.ul note_items
              ]
            else El.splice [])
          ]
      in

      let thumbnail_md = sprintf "![%%lc](:project-%s \"%s\")" p.MP.slug p.MP.title in
      let thumbnail_html = El.unsafe_raw (Arod_view.md_to_html thumbnail_md) in

      let date_range = match p.MP.finish with
        | Some y -> sprintf "%d–%d" start_year y
        | None -> sprintf "%d–present" start_year
      in

      let duration_height = max 40 (duration * 8) in

      El.div ~at:[At.class' "timeline-project"] [
        El.div ~at:[At.class' "timeline-marker-wrapper"] [
          El.div ~at:[At.class' "timeline-dot"] [];
          El.div ~at:[
            At.class' "timeline-duration";
            At.v "style" (sprintf "height: %dpx" duration_height)
          ] [];
          El.span ~at:[At.class' "timeline-year"] [El.txt (string_of_int start_year)]
        ];
        El.div ~at:[At.class' "project-card"] [
          El.div ~at:[At.class' "project-header"] [
            El.h3 [
              El.a ~at:[At.href ("/projects/" ^ p.MP.slug)] [
                El.txt p.MP.title
              ]
            ];
            El.span ~at:[At.class' "project-dates"] [El.txt date_range]
          ];
          thumbnail_html;
          El.div ~at:[At.class' "project-body"] [
            Arod_view.truncated_body (`Project p) |> fst
          ];
          recent_items_display
        ]
      ]
    ) all_projects in

    let title = "Projects" in
    let description = "Research projects timeline" in

    let intro = El.p [El.txt "Research projects and relevant publications, ideas and notes."] in

    let page_footer = Arod_footer.footer in

    let page_content = El.splice [
      El.article [
        El.h1 [El.txt title];
        intro;
        El.div ~at:[At.class' "projects-timeline"] project_cards
      ];
      El.aside []
    ] in

    Arod_page.page ~title ~page_content ~page_footer ~description ()
