(*---------------------------------------------------------------------------
  Copyright (c) 2025 Anil Madhavapeddy <anil@recoil.org>. All rights reserved.
  SPDX-License-Identifier: ISC
 ---------------------------------------------------------------------------*)

(** Generate descriptive text for Bushel entries *)

(** Format a date as "Month Year" *)
let format_date (year, month, _day) =
  Printf.sprintf "%s %d" (Bushel_types.month_name month) year

(** Generate a descriptive sentence for a paper *)
let paper_description (p : Bushel_paper.t) ~date_str =
  let venue = match String.lowercase_ascii (Bushel_paper.bibtype p) with
    | "inproceedings" -> Bushel_paper.booktitle p
    | "article" -> Bushel_paper.journal p
    | "book" ->
      let pub = Bushel_paper.publisher p in
      if pub = "" then "Book" else "Book by " ^ pub
    | "techreport" ->
      let inst = Bushel_paper.institution p in
      if inst = "" then "Technical report" else "Technical report at " ^ inst
    | "misc" ->
      let pub = Bushel_paper.publisher p in
      if pub = "" then "Working paper" else "Working paper at " ^ pub
    | _ -> "Publication"
  in
  Printf.sprintf "Paper in %s (%s)" venue date_str

(** Generate a descriptive sentence for a note *)
let note_description (n : Bushel_note.t) ~date_str ~lookup_fn =
  match Bushel_note.slug_ent n with
  | Some slug_ent ->
    (match lookup_fn slug_ent with
     | Some related_title ->
       Printf.sprintf "Note about %s (%s)" related_title date_str
     | None -> Printf.sprintf "Research note (%s)" date_str)
  | None -> Printf.sprintf "Research note (%s)" date_str

(** Generate a descriptive sentence for an idea *)
let idea_description (i : Bushel_idea.t) ~date_str =
  let status_str = String.lowercase_ascii (Bushel_idea.status_to_string (Bushel_idea.status i)) in
  let level_str = Bushel_idea.level_to_string (Bushel_idea.level i) in
  Printf.sprintf "Research idea (%s, %s level, %s)" status_str level_str date_str

(** Generate a descriptive sentence for a video *)
let video_description (v : Bushel_video.t) ~date_str ~lookup_fn =
  let video_type = if Bushel_video.talk v then "Talk video" else "Video" in
  let context = match Bushel_video.paper v with
    | Some paper_slug ->
      (match lookup_fn paper_slug with
       | Some title -> Printf.sprintf " about %s" title
       | None -> "")
    | None ->
      (match Bushel_video.project v with
       | Some project_slug ->
         (match lookup_fn project_slug with
          | Some title -> Printf.sprintf " about %s" title
          | None -> "")
       | None -> "")
  in
  Printf.sprintf "%s%s (%s)" video_type context date_str

(** Generate a descriptive sentence for a project *)
let project_description (pr : Bushel_project.t) =
  let end_str = match Bushel_project.finish pr with
    | Some year -> string_of_int year
    | None -> "present"
  in
  Printf.sprintf "Project (%d–%s)" (Bushel_project.start pr) end_str

(** Generate description for any entry type *)
let entry_description entries entry =
  let lookup_fn slug =
    match Bushel_entry.lookup entries slug with
    | Some e -> Some (Bushel_entry.title e)
    | None -> None
  in
  let date = Bushel_entry.date entry in
  let date_str = format_date date in
  match entry with
  | `Paper p -> paper_description p ~date_str
  | `Note n -> note_description n ~date_str ~lookup_fn
  | `Idea i -> idea_description i ~date_str
  | `Video v -> video_description v ~date_str ~lookup_fn
  | `Project p -> project_description p
