(*---------------------------------------------------------------------------
  Copyright (c) 2025 Anil Madhavapeddy <anil@recoil.org>. All rights reserved.
  SPDX-License-Identifier: ISC
 ---------------------------------------------------------------------------*)

(** Idea rendering for Arod webserver *)

open Htmlit
open Printf

module MI = Arod_model.Idea

let class_ c = At.class' c

let color_of_status =
  let open MI in
  function
  | Available -> "#ddffdd"
  | Discussion -> "#efee99"
  | Ongoing -> "#ffeebb"
  | Completed -> "#f0f0fe"
  | Expired -> "#cccccc"

let status_to_long_string s =
  let open MI in
  function
  | Available -> sprintf {|is <span class="idea-available">available</span> for being worked on|}
  | Discussion -> sprintf {|is <span class="idea-discussion">under discussion</span> with a student but not yet confirmed|}
  | Ongoing -> sprintf {|is currently <span class="idea-ongoing">being worked on</span> by %s|} s
  | Completed -> sprintf {|has been <span class="idea-completed">completed</span> by %s|} s
  | Expired -> sprintf {|has <span class="idea-expired">expired</span>|}

let level_to_long_string =
  let open MI in
  function
  | Any -> " as a good starter project"
  | PartII -> " as a Cambridge Computer Science Part II project"
  | MPhil -> " as a Cambridge Computer Science Part III or MPhil project"
  | PhD -> " as a Cambridge Computer Science PhD topic"
  | Postdoc -> " as a postdoctoral project"

let idea_to_html_no_sidenotes idea =
  let open MI in
  let idea_url = "/ideas/" ^ idea.slug in

  let render_contacts contacts =
    match contacts with
    | [] -> El.splice []
    | cs ->
      let contact_links = List.filter_map (fun handle ->
        match Arod_model.lookup_by_handle handle with
        | Some contact ->
          let name = Sortal_schema.Contact.name contact in
          (match Sortal_schema.Contact.best_url contact with
           | Some url -> Some (El.a ~at:[At.href url] [El.txt name])
           | None -> Some (El.txt name))
        | None ->
          Some (El.txt ("@" ^ handle))
      ) cs in
      let rec intersperse_and = function
        | [] -> []
        | [x] -> [x]
        | [x; y] -> [x; El.txt " and "; y]
        | x :: xs -> x :: El.txt ", " :: intersperse_and xs
      in
      El.splice (intersperse_and contact_links)
  in

  let sups = List.filter (fun x -> x <> "avsm") idea.supervisors in
  let sups_el = match sups with
    | [] -> El.splice []
    | _ -> El.splice [El.txt " and cosupervised with "; render_contacts sups]
  in

  let studs_el = match idea.students with
    | [] -> El.splice []
    | _ -> El.splice [render_contacts idea.students]
  in

  let lev = match idea.level with
    | Any -> ""
    | PartII -> " (Part II)"
    | MPhil -> " (MPhil)"
    | PhD -> " (PhD)"
    | Postdoc -> ""
  in

  let status_and_info = match idea.status with
    | Available -> El.splice [
        El.a ~at:[At.href idea_url] [El.txt (MI.title idea)];
        El.txt " ";
        El.br ();
        El.span ~at:[At.class' "idea-available"] [El.txt ("Available" ^ lev)];
        El.txt " ";
        sups_el
      ]
    | Discussion -> El.splice [
        El.a ~at:[At.href idea_url] [El.txt (MI.title idea)];
        El.txt " ";
        El.br ();
        El.span ~at:[At.class' "idea-discussion"] [El.txt ("Under discussion" ^ lev)];
        El.txt " ";
        sups_el
      ]
    | Ongoing -> El.splice [
        El.a ~at:[At.href idea_url] [El.txt (MI.title idea)];
        El.txt " ";
        El.br ();
        El.span ~at:[At.class' "idea-ongoing"] [El.txt ("Currently ongoing" ^ lev)];
        El.txt " with ";
        studs_el;
        El.txt " ";
        sups_el
      ]
    | Completed -> El.splice [
        El.a ~at:[At.href idea_url] [El.txt (MI.title idea)];
        El.txt " ";
        El.br ();
        El.span ~at:[At.class' "idea-completed"] [El.txt ("Completed" ^ lev)];
        El.txt " by ";
        studs_el;
        El.txt " ";
        sups_el;
        El.txt (" in " ^ string_of_int idea.year)
      ]
    | Expired -> El.splice [
        El.a ~at:[At.href idea_url] [El.txt (MI.title idea)];
        El.txt " ";
        El.br ();
        El.span ~at:[At.class' "idea-expired"] [El.txt ("Expired" ^ lev)];
        El.txt " ";
        sups_el
      ]
  in
  status_and_info

let sups_for i =
  let v = match MI.status i with
    | Completed -> "was"
    | Ongoing -> "is"
    | _ -> "may be" in
  let sups = List.filter (fun x -> x <> "avsm") i.supervisors in
  match sups with
  | [] -> ""
  | s -> " It " ^ v ^ " co-supervised with " ^ (Arod_view.map_and (sprintf "[@%s]") s) ^ "."

let one_idea_full i =
  let studs = Arod_view.map_and (sprintf "[@%s]") (MI.students i) in
  let r = Printf.sprintf "# %s\n\nThis is an idea proposed in %d%s, and %s.%s\n\n%s"
    (MI.title i) (MI.year i) (level_to_long_string @@ MI.level i) (status_to_long_string studs (MI.status i)) (sups_for i) (MI.body i)
  in
  El.div ~at:[class_ "idea"] [
    El.unsafe_raw (Arod_view.md_to_html r)
  ]

let idea_for_feed i =
  let studs = Arod_view.map_and (sprintf "[@%s]") (MI.students i) in
  let r = Printf.sprintf "This is an idea proposed %s, and %s.%s"
    (level_to_long_string @@ MI.level i) (status_to_long_string studs (MI.status i)) (sups_for i)
  in
  let (body_html, word_count_info) = Arod_view.truncated_body (`Idea i) in
  (El.splice [
    El.unsafe_raw (Arod_view.md_to_html r);
    body_html
  ], word_count_info)

let one_idea_brief i =
  let studs = Arod_view.map_and (sprintf "[@%s]") (MI.students i) in
  let r = Printf.sprintf "This is an idea proposed in %d%s, and %s.%s"
      (MI.year i) (level_to_long_string @@ MI.level i) (status_to_long_string studs (MI.status i)) (sups_for i)
  in
  let (body_html, word_count_info) = Arod_view.truncated_body (`Idea i) in
  (El.splice [
    Arod_view.entry_href (`Idea i);
    El.div ~at:[class_ "idea"] [
      El.unsafe_raw (Arod_view.md_to_html r);
      body_html
    ]
  ], word_count_info)

let view_ideas_by_project () =
  let entries = Arod_model.get_entries () in
  let all_ideas = Arod_model.Entry.ideas entries in
  let all_projects = Arod_model.Entry.projects entries
    |> List.sort Arod_model.Project.compare |> List.rev in

  let ideas_by_project = Hashtbl.create 32 in
  List.iter (fun i ->
    let proj_slug = MI.project i in
    let existing = try Hashtbl.find ideas_by_project proj_slug with Not_found -> [] in
    Hashtbl.replace ideas_by_project proj_slug (i :: existing)
  ) all_ideas;

  Hashtbl.iter (fun proj_slug ideas ->
    Hashtbl.replace ideas_by_project proj_slug (List.sort MI.compare ideas)
  ) ideas_by_project;

  let project_sections = List.filter_map (fun p ->
    let proj_slug = p.Arod_model.Project.slug in
    match Hashtbl.find_opt ideas_by_project proj_slug with
    | None -> None
    | Some ideas ->
      let idea_items = List.map (fun i ->
        El.li ~at:[At.class' "idea-item"; At.v "data-status" (MI.status_to_string (MI.status i))] [
          idea_to_html_no_sidenotes i
        ]
      ) ideas in
      let thumbnail_md = Printf.sprintf "![%%lc](:project-%s \"%s\")" proj_slug p.Arod_model.Project.title in
      let thumbnail_html = El.unsafe_raw (Arod_view.md_to_html thumbnail_md) in
      Some (El.div ~at:[At.class' "project-section"] [
        El.h2 [
          El.a ~at:[At.href ("/projects/" ^ proj_slug)] [El.txt p.Arod_model.Project.title]
        ];
        thumbnail_html;
        El.p [Arod_view.truncated_body (`Project p) |> fst];
        El.ul ~at:[At.class' "ideas-list"] idea_items
      ])
  ) all_projects in

  let status_filter = El.div ~at:[At.class' "status-filter"] [
    El.h3 [El.txt "Filter by status:"];
    El.label [
      El.input ~at:[At.type' "checkbox"; At.id "filter-available"; At.checked; At.class' "status-checkbox"; At.v "data-status" "Available"] ();
      El.span ~at:[At.class' "status-label idea-available"] [El.txt "Available"]
    ];
    El.label [
      El.input ~at:[At.type' "checkbox"; At.id "filter-discussion"; At.checked; At.class' "status-checkbox"; At.v "data-status" "Discussion"] ();
      El.span ~at:[At.class' "status-label idea-discussion"] [El.txt "Discussion"]
    ];
    El.label [
      El.input ~at:[At.type' "checkbox"; At.id "filter-ongoing"; At.checked; At.class' "status-checkbox"; At.v "data-status" "Ongoing"] ();
      El.span ~at:[At.class' "status-label idea-ongoing"] [El.txt "Ongoing"]
    ];
    El.label [
      El.input ~at:[At.type' "checkbox"; At.id "filter-completed"; At.checked; At.class' "status-checkbox"; At.v "data-status" "Completed"] ();
      El.span ~at:[At.class' "status-label idea-completed"] [El.txt "Completed"]
    ];
    El.label [
      El.input ~at:[At.type' "checkbox"; At.id "filter-expired"; At.class' "status-checkbox"; At.v "data-status" "Expired"] ();
      El.span ~at:[At.class' "status-label idea-expired"] [El.txt "Expired"]
    ]
  ] in

  let title = "Research Ideas" in
  let description = "Research ideas grouped by project" in

  let intro = El.p [El.txt "These are research ideas for students at various levels (Part II, MPhil, PhD, and postdoctoral). Browse through the ideas below to find projects that interest you. You're also welcome to propose your own research ideas that align with our ongoing projects."] in

  let page_footer = Arod_footer.footer in
  let page_content = El.splice [
    El.article [
      El.h1 [El.txt title];
      intro;
      El.splice project_sections
    ];
    El.aside [
      status_filter
    ]
  ] in
  Arod_page.page ~title ~page_content ~page_footer ~description ()
