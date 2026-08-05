(*---------------------------------------------------------------------------
  Copyright (c) 2025 Anil Madhavapeddy <anil@recoil.org>. All rights reserved.
  SPDX-License-Identifier: ISC
 ---------------------------------------------------------------------------*)

(** Lint checks for Bushel knowledge base entries *)

type severity = Warning | Error

type issue = {
  severity : severity;
  slug : string;
  category : string;
  message : string;
}

type result = {
  issues : issue list;
  entries_checked : int;
}

(** {1 Known frontmatter fields per entry type} *)

let note_fields =
  [ "title"; "date"; "slug"; "tags"; "draft"; "updated"; "index_page";
    "perma"; "weeknote"; "doi"; "synopsis"; "titleimage"; "slug_ent";
    "source"; "url"; "author"; "category"; "standardsite"; "social";
    "via"; "via-url"; "sidebar" ]

let paper_fields =
  [ "title"; "author"; "year"; "month"; "bibtype"; "publisher";
    "booktitle"; "journal"; "institution"; "pages"; "volume"; "number";
    "doi"; "url"; "video"; "isbn"; "editor"; "bib"; "tags"; "projects";
    "slides"; "selected"; "classification"; "note"; "social"; "keywords" ]

let idea_fields =
  [ "title"; "date"; "level"; "project"; "status"; "supervisors";
    "students"; "tags"; "reading"; "url"; "social" ]

let video_fields =
  [ "title"; "published_date"; "uuid"; "url"; "talk"; "tags"; "paper";
    "project"; "social" ]

let project_fields =
  [ "title"; "date"; "finish"; "tags"; "ideas"; "social" ]

(** {1 Slug reference checks} *)

let check_slug_references entries =
  let issues = ref [] in
  let add sev slug cat msg =
    issues := { severity = sev; slug; category = cat; message = msg } :: !issues
  in
  (* Note slug_ent references *)
  List.iter (fun note ->
    match Bushel_note.slug_ent note with
    | Some target ->
      (match Bushel_entry.lookup entries target with
       | None ->
         add Error (Bushel_note.slug note) "broken-ref"
           (Printf.sprintf "slug_ent references unknown entry: %s" target)
       | Some _ -> ())
    | None -> ()
  ) (Bushel_entry.notes entries);
  (* Paper project references *)
  List.iter (fun paper ->
    List.iter (fun project_slug ->
      match Bushel_entry.lookup entries project_slug with
      | None ->
        add Error (Bushel_paper.slug paper) "broken-ref"
          (Printf.sprintf "projects references unknown entry: %s" project_slug)
      | Some _ -> ()
    ) (Bushel_paper.project_slugs paper)
  ) (Bushel_entry.papers entries);
  (* Video paper/project references *)
  List.iter (fun video ->
    (match Bushel_video.paper video with
     | Some paper_slug ->
       (match Bushel_entry.lookup entries paper_slug with
        | None ->
          add Error (Bushel_video.slug video) "broken-ref"
            (Printf.sprintf "paper references unknown entry: %s" paper_slug)
        | Some _ -> ())
     | None -> ());
    (match Bushel_video.project video with
     | Some project_slug ->
       (match Bushel_entry.lookup entries project_slug with
        | None ->
          add Error (Bushel_video.slug video) "broken-ref"
            (Printf.sprintf "project references unknown entry: %s" project_slug)
        | Some _ -> ())
     | None -> ())
  ) (Bushel_entry.videos entries);
  (* Idea project references *)
  List.iter (fun idea ->
    let proj = Bushel_idea.project idea in
    if proj <> "" then begin
      let proj_slug = if String.starts_with ~prefix:":" proj then
        String.sub proj 1 (String.length proj - 1)
      else proj in
      match Bushel_entry.lookup entries proj_slug with
      | None ->
        add Error (Bushel_idea.slug idea) "broken-ref"
          (Printf.sprintf "project references unknown entry: %s" proj)
      | Some _ -> ()
    end;
    (* Supervisor/student handle references *)
    let contacts = Bushel_entry.contacts entries in
    List.iter (fun handle ->
      if not (List.exists (fun c ->
        Sortal_schema.Contact.handle c = handle
      ) contacts) then
        add Warning (Bushel_idea.slug idea) "broken-ref"
          (Printf.sprintf "supervisor handle not found: %s" handle)
    ) (Bushel_idea.supervisor_handles idea);
    List.iter (fun handle ->
      if not (List.exists (fun c ->
        Sortal_schema.Contact.handle c = handle
      ) contacts) then
        add Warning (Bushel_idea.slug idea) "broken-ref"
          (Printf.sprintf "student handle not found: %s" handle)
    ) (Bushel_idea.student_handles idea)
  ) (Bushel_entry.ideas entries);
  List.rev !issues

(** {1 Markdown reference checks} *)

let check_markdown_references entries =
  let issues = ref [] in
  List.iter (fun entry ->
    let slug = Bushel_entry.slug entry in
    let body = Bushel_entry.body entry in
    if body <> "" then begin
      let (broken_slugs, broken_contacts) =
        Bushel_md.validate_references entries body
      in
      List.iter (fun s ->
        issues := { severity = Error; slug; category = "broken-ref";
                    message = Printf.sprintf "broken slug reference in body: %s" s } :: !issues
      ) broken_slugs;
      List.iter (fun c ->
        issues := { severity = Error; slug; category = "broken-ref";
                    message = Printf.sprintf "broken contact reference in body: %s" c } :: !issues
      ) broken_contacts
    end
  ) (Bushel_entry.all_entries entries);
  List.rev !issues

(** {1 Missing content checks} *)

let check_missing_content entries =
  let issues = ref [] in
  (* Notes without synopsis (non-draft) *)
  List.iter (fun note ->
    if not (Bushel_note.draft note) then
      match Bushel_note.synopsis note with
      | None | Some "" ->
        issues := { severity = Warning;
                    slug = Bushel_note.slug note;
                    category = "missing-content";
                    message = "note has no synopsis" } :: !issues
      | Some _ -> ()
  ) (Bushel_entry.notes entries);
  (* Papers without abstract *)
  List.iter (fun paper ->
    let abstract = Bushel_paper.abstract paper in
    if abstract = "" || String.trim abstract = "" then
      issues := { severity = Warning;
                  slug = Bushel_paper.slug paper;
                  category = "missing-content";
                  message = "paper has no abstract" } :: !issues;
    (* Papers without doi *)
    (match Bushel_paper.doi paper with
     | None | Some "" ->
       issues := { severity = Warning;
                   slug = Bushel_paper.slug paper;
                   category = "missing-content";
                   message = "paper has no DOI" } :: !issues
     | Some _ -> ())
  ) (Bushel_entry.papers entries);
  (* Ideas without body *)
  List.iter (fun idea ->
    let body = Bushel_idea.body idea in
    if body = "" || String.trim body = "" then
      issues := { severity = Warning;
                  slug = Bushel_idea.slug idea;
                  category = "missing-content";
                  message = "idea has no body" } :: !issues
  ) (Bushel_entry.ideas entries);
  List.rev !issues

(** {1 Unknown field checks} *)

let check_unknown_fields triples =
  let issues = ref [] in
  List.iter (fun (slug, yaml_keys, known) ->
    List.iter (fun key ->
      if not (List.mem key known) then
        issues := { severity = Warning; slug; category = "unknown-field";
                    message = Printf.sprintf "unknown frontmatter field: %s" key } :: !issues
    ) yaml_keys
  ) triples;
  List.rev !issues

(** {1 Main entry point} *)

let run entries =
  let slug_issues = check_slug_references entries in
  let md_issues = check_markdown_references entries in
  let content_issues = check_missing_content entries in
  slug_issues @ md_issues @ content_issues
