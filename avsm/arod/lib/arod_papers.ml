(*---------------------------------------------------------------------------
  Copyright (c) 2025 Anil Madhavapeddy <anil@recoil.org>. All rights reserved.
  SPDX-License-Identifier: ISC
 ---------------------------------------------------------------------------*)

(** Paper rendering for Arod webserver *)

open Htmlit
open Printf

module MP = Arod_model.Paper
module MC = Arod_model.Contact

(** Author name with text-wrap:nowrap *)
let author_name name =
  El.span ~at:[At.style "text-wrap:nowrap"] [El.txt name]

(** Render one author - link to their best URL if available *)
let one_author author_name_str =
  match Arod_model.lookup_by_name author_name_str with
  | None ->
    El.span ~at:[At.class' "author"] [author_name author_name_str]
  | Some contact ->
    let name = MC.name contact in
    match MC.best_url contact with
    | None ->
      El.span ~at:[At.class' "author"] [author_name name]
    | Some url ->
      El.a ~at:[At.href url] [author_name name]

(** Render all authors with proper comma and "and" formatting *)
let authors p =
  let author_names = MP.authors p in
  let author_els = List.map one_author author_names in
  match author_els with
  | [] -> El.splice []
  | [a] -> a
  | els ->
    let rec make_list = function
      | [] -> []
      | [x] -> [El.txt " and "; x]
      | x :: xs -> x :: El.txt ", " :: make_list xs
    in
    El.splice (make_list els)

(** Generate publication info based on bibtype *)
let paper_publisher p =
  let bibty = MP.bibtype p in
  let ourl l = function
    | None -> l
    | Some u -> sprintf {|<a href="%s">%s</a>|} u l
  in
  let string_of_vol_issue p =
    match (MP.volume p), (MP.number p) with
    | Some v, Some n -> sprintf " (vol %s issue %s)" v n
    | Some v, None -> sprintf " (vol %s)" v
    | None, Some n -> sprintf " (issue %s)" n
    | _ -> ""
  in
  let result = match String.lowercase_ascii bibty with
    | "misc" ->
      sprintf {|Working paper at %s|} (ourl (MP.publisher p) (MP.url p))
    | "inproceedings" ->
      sprintf {|Paper in the %s|} (ourl (MP.booktitle p) (MP.url p))
    | "proceedings" ->
      sprintf {|%s|} (ourl (MP.title p) (MP.url p))
    | "abstract" ->
      sprintf {|Abstract in the %s|} (ourl (MP.booktitle p) (MP.url p))
    | "article" | "journal" ->
      sprintf {|Journal paper in %s%s|} (ourl (MP.journal p) (MP.url p)) (string_of_vol_issue p)
    | "book" ->
      sprintf {|Book published by %s|} (ourl (MP.publisher p) (MP.url p))
    | "techreport" ->
      sprintf {|Technical report%s at %s|}
        (match MP.number p with None -> "" | Some n -> " (" ^ n ^ ")")
        (ourl (MP.institution p) (MP.url p))
    | _ -> sprintf {|Publication in %s|} (ourl (MP.publisher p) (MP.url p))
  in
  El.unsafe_raw result

(** Extract host without www prefix *)
let host_without_www u =
  match Uri.host (Uri.of_string u) with
  | None -> ""
  | Some h ->
    if String.starts_with ~prefix:"www." h then
      String.sub h 4 (String.length h - 4)
    else h

(** Render the links bar (URL, DOI, BIB, PDF) *)
let paper_bar_for_feed ?(nopdf=false) p =
  let cfg = Arod_model.get_config () in
  let pdf =
    let pdf_path = Filename.concat cfg.paths.static_dir (sprintf "papers/%s.pdf" (MP.slug p)) in
    if Sys.file_exists pdf_path && not nopdf then
      Some (El.a ~at:[At.href (sprintf "/papers/%s.pdf" (MP.slug p))] [
        El.span ~at:[At.class' "nobreak"] [
          El.txt "PDF";
          El.img ~at:[At.class' "inline-icon"; At.alt "pdf"; At.src "/assets/pdf.svg"] ()
        ]
      ])
    else None
  in
  let bib =
    if nopdf then None
    else Some (El.a ~at:[At.href (sprintf "/papers/%s.bib" (MP.slug p))] [El.txt "BIB"])
  in
  let url =
    match MP.url p with
    | None -> None
    | Some u ->
      Some (El.splice [
        El.a ~at:[At.href u] [El.txt "URL"];
        El.txt " ";
        El.unsafe_raw (sprintf {|<i style="color: #666666">(%s)</i>|} (host_without_www u))
      ])
  in
  let doi =
    match MP.doi p with
    | None -> None
    | Some d ->
      Some (El.a ~at:[At.href ("https://doi.org/" ^ d)] [El.txt "DOI"])
  in
  let bits = [url; doi; bib; pdf] |> List.filter_map Fun.id in
  El.splice ~sep:(El.unsafe_raw " &nbsp; ") bits

(** Render paper for feed/listing (blockquote style) *)
let paper_for_feed p =
  let title_el = El.p ~at:[At.class' "paper-title"] [
    El.a ~at:[At.href (Arod_model.Entry.site_url (`Paper p))] [El.txt (MP.title p)]
  ] in
  (El.blockquote ~at:[At.class' "paper noquote"] [
    El.div ~at:[At.class' "paper-info"] [
      title_el;
      El.p [authors p; El.txt "."];
      El.p [paper_publisher p; El.txt "."];
      El.p [paper_bar_for_feed p]
    ]
  ], None)

(** Render paper for entry listing *)
let paper_for_entry ?nopdf p =
  (El.div ~at:[At.class' "paper"] [
    El.div ~at:[At.class' "paper-info"] [
      El.p ~at:[At.class' "paper-title"] [
        El.a ~at:[At.href (Arod_model.Entry.site_url (`Paper p))] [El.txt (MP.title p)]
      ];
      El.p [authors p; El.txt "."];
      El.p [paper_publisher p; El.txt "."];
      El.p [paper_bar_for_feed ?nopdf p]
    ]
  ], None)

(** Render older versions section for a paper *)
let one_paper_extra p =
  let entries = Arod_model.get_entries () in
  let all = Arod_model.Entry.old_papers entries
    |> List.filter (fun op -> MP.slug op = MP.slug p)
  in
  match all with
  | [] -> El.splice []
  | all ->
    let older_versions = List.map (fun op ->
      let (paper_html, _) = paper_for_entry ~nopdf:true op in
      El.splice [
        El.hr ();
        El.p [
          El.txt ("This is " ^ op.Arod_model.Paper.ver ^ " of the publication from " ^
                  Arod_view.ptime_date ~with_d:false (MP.date op) ^ ".")
        ];
        El.blockquote ~at:[At.class' "noquote"] [
          paper_html
        ];
        Arod_view.tags_meta (`Paper op)
      ]
    ) all in
    El.splice [
      El.h1 [El.txt "Older versions"];
      El.p [
        El.txt "There are earlier revisions of this paper available below for historical reasons. ";
        El.txt "Please cite the latest version of the paper above instead of these."
      ];
      El.splice older_versions
    ]

(** Render full paper page *)
let one_paper_full p =
  let img_el =
    match Arod_model.lookup_image (MP.slug p) with
    | Some img ->
      El.p [
        El.a ~at:[At.href (Option.value ~default:"#" (MP.best_url p))] [
          Arod_view.img ~cl:"image-center" img
        ]
      ]
    | None -> El.splice []
  in
  let abstract_html =
    let abstract = MP.abstract p in
    if abstract <> "" then
      El.p [El.unsafe_raw (Arod_view.md_to_html abstract)]
    else
      El.splice []
  in
  El.div ~at:[At.class' "paper"] [
    El.div ~at:[At.class' "paper-info"] [
      El.h2 [El.txt (MP.title p)];
      El.p [authors p; El.txt "."];
      El.p [paper_publisher p; El.txt "."];
      El.p [paper_bar_for_feed p]
    ];
    img_el;
    abstract_html
  ]
