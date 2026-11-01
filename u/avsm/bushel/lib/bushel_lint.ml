(*---------------------------------------------------------------------------
  Copyright (c) 2025 Anil Madhavapeddy <anil@recoil.org>. All rights reserved.
  SPDX-License-Identifier: ISC
 ---------------------------------------------------------------------------*)

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

let note_fields =
  [ "title"; "date"; "slug"; "tags"; "draft"; "updated"; "index_page";
    "perma"; "weeknote"; "featured"; "doi"; "synopsis"; "titleimage";
    "slug_ent"; "source"; "url"; "author"; "category"; "standardsite";
    "social"; "via"; "via-url"; "sidebar" ]

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

let add_issue issues severity slug category message =
  issues := { severity; slug; category; message } :: !issues

let check_entry_reference ?display entries issues ~slug ~field target =
  if Option.is_none (Bushel_entry.lookup entries target) then
    let display = Option.value display ~default:target in
    add_issue issues Error slug "broken-ref"
      (Printf.sprintf "%s references unknown entry: %s" field display)

let check_slug_references entries =
  let issues = ref [] in
  List.iter (fun note ->
    match Bushel_note.slug_ent note with
    | Some target ->
      check_entry_reference entries issues ~slug:(Bushel_note.slug note)
        ~field:"slug_ent" target
    | None -> ()
  ) (Bushel_entry.notes entries);
  List.iter (fun paper ->
    List.iter (fun project_slug ->
      check_entry_reference entries issues ~slug:(Bushel_paper.slug paper)
        ~field:"projects" project_slug
    ) (Bushel_paper.project_slugs paper)
  ) (Bushel_entry.papers entries);
  List.iter (fun video ->
    let slug = Bushel_video.slug video in
    Option.iter
      (check_entry_reference entries issues ~slug ~field:"paper")
      (Bushel_video.paper video);
    Option.iter
      (check_entry_reference entries issues ~slug ~field:"project")
      (Bushel_video.project video)
  ) (Bushel_entry.videos entries);
  let contacts = Bushel_entry.contacts entries in
  let check_contact slug kind handle =
    if not (List.exists
      (fun c -> Sortal_schema.Contact.handle c = handle) contacts)
    then
      add_issue issues Warning slug "broken-ref"
        (Printf.sprintf "%s handle not found: %s" kind handle)
  in
  List.iter (fun idea ->
    let slug = Bushel_idea.slug idea in
    let proj = Bushel_idea.project idea in
    if proj <> "" then begin
      let proj_slug = if String.starts_with ~prefix:":" proj then
        String.sub proj 1 (String.length proj - 1)
      else proj in
      check_entry_reference ~display:proj entries issues ~slug ~field:"project"
        proj_slug
    end;
    List.iter (check_contact slug "supervisor")
      (Bushel_idea.supervisor_handles idea);
    List.iter (check_contact slug "student")
      (Bushel_idea.student_handles idea)
  ) (Bushel_entry.ideas entries);
  List.rev !issues

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
        add_issue issues Error slug "broken-ref"
          (Printf.sprintf "broken slug reference in body: %s" s)
      ) broken_slugs;
      List.iter (fun c ->
        add_issue issues Error slug "broken-ref"
          (Printf.sprintf "broken contact reference in body: %s" c)
      ) broken_contacts
    end
  ) (Bushel_entry.all_entries entries);
  List.rev !issues

let check_missing_content entries =
  let issues = ref [] in
  List.iter (fun note ->
    if not (Bushel_note.draft note) then
      match Bushel_note.synopsis note with
      | None | Some "" ->
        add_issue issues Warning (Bushel_note.slug note) "missing-content"
          "note has no synopsis"
      | Some _ -> ()
  ) (Bushel_entry.notes entries);
  List.iter (fun paper ->
    let slug = Bushel_paper.slug paper in
    let abstract = Bushel_paper.abstract paper in
    if abstract = "" || String.trim abstract = "" then
      add_issue issues Warning slug "missing-content" "paper has no abstract";
    (match Bushel_paper.doi paper with
     | None | Some "" ->
       add_issue issues Warning slug "missing-content" "paper has no DOI"
     | Some _ -> ())
  ) (Bushel_entry.papers entries);
  List.iter (fun idea ->
    let body = Bushel_idea.body idea in
    if body = "" || String.trim body = "" then
      add_issue issues Warning (Bushel_idea.slug idea) "missing-content"
        "idea has no body"
  ) (Bushel_entry.ideas entries);
  List.rev !issues

let check_unknown_fields triples =
  let issues = ref [] in
  List.iter (fun (slug, yaml_keys, known) ->
    List.iter (fun key ->
      if not (List.mem key known) then
        add_issue issues Warning slug "unknown-field"
          (Printf.sprintf "unknown frontmatter field: %s" key)
    ) yaml_keys
  ) triples;
  List.rev !issues

let run entries =
  let slug_issues = check_slug_references entries in
  let md_issues = check_markdown_references entries in
  let content_issues = check_missing_content entries in
  slug_issues @ md_issues @ content_issues
