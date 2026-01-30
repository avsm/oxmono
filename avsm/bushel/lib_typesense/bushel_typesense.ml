(*---------------------------------------------------------------------------
  Copyright (c) 2025 Anil Madhavapeddy <anil@recoil.org>. All rights reserved.
  SPDX-License-Identifier: ISC
 ---------------------------------------------------------------------------*)

(** Typesense search integration for Bushel entries *)

let src = Logs.Src.create "bushel.typesense" ~doc:"Bushel Typesense sync"
module Log = (val Logs.src_log src : Logs.LOG)

(** {1 Schema Definitions using Typesense library} *)

let field ~name ~type_ ?facet ?optional () =
  Typesense.Field.T.v ~name ~type_
    ?facet ?optional ()

let notes_schema =
  Typesense.CollectionSchema.T.v
    ~name:"notes"
    ~default_sorting_field:"date_timestamp"
    ~fields:[
      field ~name:"id" ~type_:"string" ();
      field ~name:"title" ~type_:"string" ();
      field ~name:"content" ~type_:"string" ();
      field ~name:"date" ~type_:"string" ();
      field ~name:"date_timestamp" ~type_:"int64" ();
      field ~name:"tags" ~type_:"string[]" ~facet:true ();
      field ~name:"body" ~type_:"string" ~optional:true ();
      field ~name:"draft" ~type_:"bool" ();
      field ~name:"synopsis" ~type_:"string[]" ~optional:true ();
      field ~name:"thumbnail_url" ~type_:"string" ~optional:true ();
      field ~name:"type" ~type_:"string" ~facet:true ~optional:true ();
      field ~name:"status" ~type_:"string" ~facet:true ~optional:true ();
      field ~name:"related_papers" ~type_:"string[]" ~optional:true ();
      field ~name:"related_projects" ~type_:"string[]" ~optional:true ();
      field ~name:"related_contacts" ~type_:"string[]" ~optional:true ();
      field ~name:"attachments" ~type_:"string[]" ~optional:true ();
      field ~name:"source" ~type_:"string" ~facet:true ~optional:true ();
      field ~name:"url" ~type_:"string" ~optional:true ();
      field ~name:"author" ~type_:"string" ~optional:true ();
      field ~name:"category" ~type_:"string" ~facet:true ~optional:true ();
      field ~name:"slug_ent" ~type_:"string" ~optional:true ();
      field ~name:"words" ~type_:"int32" ~optional:true ();
    ] ()

let papers_schema =
  Typesense.CollectionSchema.T.v
    ~name:"papers"
    ~default_sorting_field:"date_timestamp"
    ~fields:[
      field ~name:"id" ~type_:"string" ();
      field ~name:"title" ~type_:"string" ();
      field ~name:"authors" ~type_:"string[]" ();
      field ~name:"abstract" ~type_:"string" ();
      field ~name:"date" ~type_:"string" ();
      field ~name:"date_timestamp" ~type_:"int64" ();
      field ~name:"tags" ~type_:"string[]" ~facet:true ();
      field ~name:"doi" ~type_:"string[]" ~optional:true ();
      field ~name:"arxiv_id" ~type_:"string" ~optional:true ();
      field ~name:"pdf_url" ~type_:"string[]" ~optional:true ();
      field ~name:"thumbnail_url" ~type_:"string" ~optional:true ();
      field ~name:"journal" ~type_:"string[]" ~optional:true ();
      field ~name:"related_projects" ~type_:"string[]" ~optional:true ();
      field ~name:"related_talks" ~type_:"string[]" ~optional:true ();
    ] ()

let projects_schema =
  Typesense.CollectionSchema.T.v
    ~name:"projects"
    ~default_sorting_field:"date_timestamp"
    ~fields:[
      field ~name:"id" ~type_:"string" ();
      field ~name:"title" ~type_:"string" ();
      field ~name:"description" ~type_:"string" ();
      field ~name:"start_year" ~type_:"int32" ();
      field ~name:"finish_year" ~type_:"int32" ~optional:true ();
      field ~name:"date" ~type_:"string" ();
      field ~name:"date_timestamp" ~type_:"int64" ();
      field ~name:"tags" ~type_:"string[]" ~facet:true ();
      field ~name:"repository_url" ~type_:"string" ~optional:true ();
      field ~name:"homepage_url" ~type_:"string" ~optional:true ();
      field ~name:"languages" ~type_:"string[]" ~facet:true ~optional:true ();
      field ~name:"license" ~type_:"string" ~facet:true ~optional:true ();
      field ~name:"status" ~type_:"string" ~facet:true ~optional:true ();
      field ~name:"related_papers" ~type_:"string[]" ~optional:true ();
      field ~name:"related_talks" ~type_:"string[]" ~optional:true ();
      field ~name:"body" ~type_:"string" ~optional:true ();
      field ~name:"ideas" ~type_:"string" ~optional:true ();
    ] ()

let ideas_schema =
  Typesense.CollectionSchema.T.v
    ~name:"ideas"
    ~default_sorting_field:"date_timestamp"
    ~fields:[
      field ~name:"id" ~type_:"string" ();
      field ~name:"title" ~type_:"string" ();
      field ~name:"description" ~type_:"string" ();
      field ~name:"year" ~type_:"int32" ();
      field ~name:"date" ~type_:"string" ();
      field ~name:"date_timestamp" ~type_:"int64" ();
      field ~name:"tags" ~type_:"string[]" ~facet:true ();
      field ~name:"level" ~type_:"string" ~facet:true ();
      field ~name:"status" ~type_:"string" ~facet:true ();
      field ~name:"project" ~type_:"string" ~facet:true ();
      field ~name:"supervisors" ~type_:"string[]" ~optional:true ();
      field ~name:"body" ~type_:"string" ~optional:true ();
      field ~name:"students" ~type_:"string[]" ~optional:true ();
      field ~name:"reading" ~type_:"string" ~optional:true ();
      field ~name:"url" ~type_:"string" ~optional:true ();
    ] ()

let videos_schema =
  Typesense.CollectionSchema.T.v
    ~name:"videos"
    ~default_sorting_field:"date_timestamp"
    ~fields:[
      field ~name:"id" ~type_:"string" ();
      field ~name:"title" ~type_:"string" ();
      field ~name:"description" ~type_:"string" ();
      field ~name:"published_date" ~type_:"string" ();
      field ~name:"date" ~type_:"string" ();
      field ~name:"date_timestamp" ~type_:"int64" ();
      field ~name:"tags" ~type_:"string[]" ~facet:true ();
      field ~name:"url" ~type_:"string" ();
      field ~name:"uuid" ~type_:"string" ();
      field ~name:"is_talk" ~type_:"bool" ();
      field ~name:"paper" ~type_:"string[]" ~optional:true ();
      field ~name:"project" ~type_:"string[]" ~optional:true ();
      field ~name:"video_url" ~type_:"string" ~optional:true ();
      field ~name:"embed_url" ~type_:"string" ~optional:true ();
      field ~name:"duration" ~type_:"int32" ~optional:true ();
      field ~name:"channel" ~type_:"string" ~facet:true ~optional:true ();
      field ~name:"platform" ~type_:"string" ~facet:true ~optional:true ();
      field ~name:"views" ~type_:"int32" ~optional:true ();
      field ~name:"related_papers" ~type_:"string[]" ~optional:true ();
      field ~name:"related_talks" ~type_:"string[]" ~optional:true ();
    ] ()

let contacts_schema =
  Typesense.CollectionSchema.T.v
    ~name:"contacts"
    ~fields:[
      field ~name:"id" ~type_:"string" ();
      field ~name:"handle" ~type_:"string" ();
      field ~name:"name" ~type_:"string" ();
      field ~name:"names" ~type_:"string[]" ~optional:true ();
      field ~name:"email" ~type_:"string[]" ~optional:true ();
      field ~name:"icon" ~type_:"string[]" ~optional:true ();
      field ~name:"github" ~type_:"string[]" ~optional:true ();
      field ~name:"twitter" ~type_:"string[]" ~optional:true ();
      field ~name:"bluesky" ~type_:"string[]" ~optional:true ();
      field ~name:"mastodon" ~type_:"string[]" ~optional:true ();
      field ~name:"orcid" ~type_:"string[]" ~optional:true ();
      field ~name:"url" ~type_:"string[]" ~optional:true ();
      field ~name:"atom" ~type_:"string[]" ~optional:true ();
    ] ()

(** All collection schemas *)
let all_schemas = [
  notes_schema;
  papers_schema;
  projects_schema;
  ideas_schema;
  videos_schema;
  contacts_schema;
]

(** {1 Document Conversion} *)

module J = Jsont.Json

let ptime_to_timestamp t =
  let span = Ptime.to_span t in
  Int64.of_float (Ptime.Span.to_float_s span)

let date_to_timestamp (y, m, d) =
  match Ptime.of_date (y, m, d) with
  | Some t -> ptime_to_timestamp t
  | None -> 0L

let mem k v = J.mem (J.name k) v
let str s = J.string s
let num f = J.number f
let int64_ i = J.number (Int64.to_float i)
let int_ i = J.number (Float.of_int i)
let bool_ b = J.bool b
let str_list l = J.list (List.map str l)

let obj fields = J.object' (List.filter_map Fun.id fields)

let opt_mem k = function
  | Some v -> Some (mem k v)
  | None -> None

let note_to_document (n : Bushel.Note.t) =
  let date = Bushel.Note.date n in
  let (y, m, d) = date in
  obj [
    Some (mem "id" (str (Bushel.Note.slug n)));
    Some (mem "title" (str (Bushel.Note.title n)));
    Some (mem "content" (str (Bushel.Note.body n)));
    Some (mem "date" (str (Printf.sprintf "%04d-%02d-%02d" y m d)));
    Some (mem "date_timestamp" (int64_ (date_to_timestamp date)));
    Some (mem "tags" (str_list (Bushel.Note.tags n)));
    Some (mem "draft" (bool_ (Bushel.Note.draft n)));
    Some (mem "words" (int_ (Bushel.Note.words n)));
    opt_mem "synopsis" (Option.map (fun s -> str_list [s]) (Bushel.Note.synopsis n));
    opt_mem "source" (Option.map str (Bushel.Note.source n));
    opt_mem "url" (Option.map str (Bushel.Note.url n));
    opt_mem "author" (Option.map str (Bushel.Note.author n));
    opt_mem "category" (Option.map str (Bushel.Note.category n));
    opt_mem "slug_ent" (Option.map str (Bushel.Note.slug_ent n));
  ]

let paper_to_document (p : Bushel.Paper.t) =
  let date = Bushel.Paper.date p in
  let (y, m, d) = date in
  obj [
    Some (mem "id" (str (Bushel.Paper.slug p)));
    Some (mem "title" (str (Bushel.Paper.title p)));
    Some (mem "authors" (str_list (Bushel.Paper.authors p)));
    Some (mem "abstract" (str (Bushel.Paper.abstract p)));
    Some (mem "date" (str (Printf.sprintf "%04d-%02d-%02d" y m d)));
    Some (mem "date_timestamp" (int64_ (date_to_timestamp date)));
    Some (mem "tags" (str_list (Bushel.Paper.tags p)));
    opt_mem "doi" (Option.map (fun d -> str_list [d]) (Bushel.Paper.doi p));
    opt_mem "pdf_url" (Option.map (fun u -> str_list [u]) (Bushel.Paper.url p));
    (let j = Bushel.Paper.journal p in if j <> "" then Some (mem "journal" (str_list [j])) else None);
  ]

let project_to_document (p : Bushel.Project.t) =
  let date = (Bushel.Project.start p, 1, 1) in
  let (y, m, d) = date in
  obj [
    Some (mem "id" (str (Bushel.Project.slug p)));
    Some (mem "title" (str (Bushel.Project.title p)));
    Some (mem "description" (str (Bushel.Project.body p)));
    Some (mem "start_year" (int_ (Bushel.Project.start p)));
    Some (mem "date" (str (Printf.sprintf "%04d-%02d-%02d" y m d)));
    Some (mem "date_timestamp" (int64_ (date_to_timestamp date)));
    Some (mem "tags" (str_list (Bushel.Project.tags p)));
    Some (mem "body" (str (Bushel.Project.body p)));
    Some (mem "ideas" (str (Bushel.Project.ideas p)));
    opt_mem "finish_year" (Option.map int_ (Bushel.Project.finish p));
  ]

let idea_to_document (i : Bushel.Idea.t) =
  let date = (Bushel.Idea.year i, Bushel.Idea.month i, 1) in
  let (y, m, d) = date in
  obj [
    Some (mem "id" (str (Bushel.Idea.slug i)));
    Some (mem "title" (str (Bushel.Idea.title i)));
    Some (mem "description" (str (Bushel.Idea.body i)));
    Some (mem "year" (int_ (Bushel.Idea.year i)));
    Some (mem "date" (str (Printf.sprintf "%04d-%02d-%02d" y m d)));
    Some (mem "date_timestamp" (int64_ (date_to_timestamp date)));
    Some (mem "tags" (str_list (Bushel.Idea.tags i)));
    Some (mem "level" (str (Bushel.Idea.level_to_string (Bushel.Idea.level i))));
    Some (mem "status" (str (Bushel.Idea.status_to_string (Bushel.Idea.status i))));
    Some (mem "project" (str (Bushel.Idea.project i)));
    Some (mem "supervisors" (str_list (Bushel.Idea.supervisors i)));
    Some (mem "students" (str_list (Bushel.Idea.students i)));
    Some (mem "body" (str (Bushel.Idea.body i)));
    Some (mem "reading" (str (Bushel.Idea.reading i)));
  ]

let video_to_document (v : Bushel.Video.t) =
  let date = Bushel.Video.date v in
  let (y, m, d) = date in
  obj [
    Some (mem "id" (str (Bushel.Video.uuid v)));
    Some (mem "title" (str (Bushel.Video.title v)));
    Some (mem "description" (str (Bushel.Video.description v)));
    Some (mem "published_date" (str (Ptime.to_rfc3339 (Bushel.Video.datetime v))));
    Some (mem "date" (str (Printf.sprintf "%04d-%02d-%02d" y m d)));
    Some (mem "date_timestamp" (int64_ (date_to_timestamp date)));
    Some (mem "tags" (str_list (Bushel.Video.tags v)));
    Some (mem "url" (str (Bushel.Video.url v)));
    Some (mem "uuid" (str (Bushel.Video.uuid v)));
    Some (mem "is_talk" (bool_ (Bushel.Video.talk v)));
    opt_mem "paper" (Option.map (fun p -> str_list [p]) (Bushel.Video.paper v));
    opt_mem "project" (Option.map (fun p -> str_list [p]) (Bushel.Video.project v));
  ]

let contact_to_document (c : Sortal_schema.Contact.t) =
  (* Extract atom feed URLs from Sortal feeds *)
  let atom_urls = match Sortal_schema.Contact.feeds c with
    | Some feeds ->
      List.filter_map (fun f ->
        if Sortal_schema.Feed.feed_type f = Sortal_schema.Feed.Atom
        then Some (Sortal_schema.Feed.url f)
        else None
      ) feeds
    | None -> []
  in
  obj [
    Some (mem "id" (str (Sortal_schema.Contact.handle c)));
    Some (mem "handle" (str (Sortal_schema.Contact.handle c)));
    Some (mem "name" (str (Sortal_schema.Contact.name c)));
    Some (mem "names" (str_list (Sortal_schema.Contact.names c)));
    opt_mem "email" (Option.map (fun e -> str_list [e]) (Sortal_schema.Contact.current_email c));
    opt_mem "github" (Option.map (fun g -> str_list [g]) (Sortal_schema.Contact.github_handle c));
    opt_mem "twitter" (Option.map (fun t -> str_list [t]) (Sortal_schema.Contact.twitter_handle c));
    opt_mem "bluesky" (Option.map (fun b -> str_list [b]) (Sortal_schema.Contact.bluesky_handle c));
    opt_mem "mastodon" (Option.map (fun m -> str_list [m]) (Sortal_schema.Contact.mastodon_handle c));
    opt_mem "orcid" (Option.map (fun o -> str_list [o]) (Sortal_schema.Contact.orcid c));
    opt_mem "url" (Option.map (fun u -> str_list [u]) (Sortal_schema.Contact.current_url c));
    (if atom_urls = [] then None else Some (mem "atom" (str_list atom_urls)));
  ]

(** {1 Document ID Extraction} *)

let get_doc_id (doc : Jsont.json) : string option =
  match doc with
  | Jsont.Object (fields, _) ->
    List.find_map (fun ((name, _), v) ->
      if name = "id" then
        match v with Jsont.String (s, _) -> Some s | _ -> None
      else None
    ) fields
  | _ -> None

(** {1 Sync State} *)

type sync_stats = {
  mutable created : int;
  mutable updated : int;
  mutable deleted : int;
  mutable unchanged : int;
  mutable errors : int;
}

let empty_stats () = {
  created = 0;
  updated = 0;
  deleted = 0;
  unchanged = 0;
  errors = 0;
}

(** {1 Collection Sync} *)

type collection_sync_result = {
  collection : string;
  stats : sync_stats;
  details : string list;
}

(** Ensure a collection exists, creating it if necessary *)
let ensure_collection (client : Typesense_auth.Client.t) (schema : Typesense.CollectionSchema.T.t) =
  let name = Typesense.CollectionSchema.T.name schema in
  let ts = Typesense_auth.Client.client client in
  try
    let _ = Typesense.Collection.get_collection ~collection_name:name ts () in
    Log.debug (fun m -> m "Collection %s already exists" name);
    `Exists
  with _ ->
    Log.info (fun m -> m "Creating collection %s" name);
    let _ = Typesense.Collection.create_collection ~body:schema ts () in
    `Created

(** Get existing document IDs from a collection *)
let get_existing_ids (client : Typesense_auth.Client.t) ~collection : string list =
  try
    let params = Typesense_auth.Client.export_params ~include_fields:["id"] () in
    let docs = Typesense_auth.Client.export client ~collection ~params () in
    List.filter_map get_doc_id docs
  with _ ->
    Log.warn (fun m -> m "Failed to export existing documents from %s" collection);
    []

(** Sync a single collection incrementally *)
let sync_collection ~dry_run (client : Typesense_auth.Client.t)
    ~collection ~(schema : Typesense.CollectionSchema.T.t)
    ~(documents : Jsont.json list) : collection_sync_result =
  let stats = empty_stats () in
  let details = ref [] in
  let name = Typesense.CollectionSchema.T.name schema in

  (* Get IDs of new documents *)
  let new_ids = List.filter_map get_doc_id documents in
  let new_id_set = List.fold_left (fun s id ->
    Hashtbl.replace s id (); s
  ) (Hashtbl.create (List.length new_ids)) new_ids in

  if dry_run then begin
    (* In dry-run mode, just report what would happen *)
    begin try
      let existing_ids = get_existing_ids client ~collection in
      let existing_set = List.fold_left (fun s id ->
        Hashtbl.replace s id (); s
      ) (Hashtbl.create (List.length existing_ids)) existing_ids in

      (* Count creates (in new but not existing) *)
      List.iter (fun id ->
        if not (Hashtbl.mem existing_set id) then begin
          stats.created <- stats.created + 1;
          if stats.created <= 5 then
            details := (Printf.sprintf "Would create: %s" id) :: !details
        end else
          stats.unchanged <- stats.unchanged + 1
      ) new_ids;

      (* Count deletes (in existing but not new) *)
      List.iter (fun id ->
        if not (Hashtbl.mem new_id_set id) then begin
          stats.deleted <- stats.deleted + 1;
          if stats.deleted <= 5 then
            details := (Printf.sprintf "Would delete: %s" id) :: !details
        end
      ) existing_ids;

      if stats.created > 5 then
        details := (Printf.sprintf "...and %d more creates" (stats.created - 5)) :: !details;
      if stats.deleted > 5 then
        details := (Printf.sprintf "...and %d more deletes" (stats.deleted - 5)) :: !details
    with _ ->
      (* Collection doesn't exist yet *)
      stats.created <- List.length documents;
      details := [Printf.sprintf "Would create collection with %d documents" stats.created]
    end
  end else begin
    (* Actual sync *)
    (* Ensure collection exists *)
    let _ = ensure_collection client schema in

    (* Get existing document IDs *)
    let existing_ids = get_existing_ids client ~collection in
    let existing_set = List.fold_left (fun s id ->
      Hashtbl.replace s id (); s
    ) (Hashtbl.create (List.length existing_ids)) existing_ids in

    (* Upsert all new documents *)
    if documents <> [] then begin
      Log.info (fun m -> m "Upserting %d documents to %s" (List.length documents) name);
      let results = Typesense_auth.Client.import client ~collection ~action:Upsert documents in
      List.iter (fun (r : Typesense_auth.Client.import_result) ->
        if r.success then begin
          (* Check if this was a create or update *)
          match r.document with
          | Some doc_str ->
            (try
              match Jsont_bytesrw.decode_string Jsont.json doc_str with
              | Ok doc ->
                (match get_doc_id doc with
                | Some id when not (Hashtbl.mem existing_set id) ->
                  stats.created <- stats.created + 1
                | _ ->
                  stats.updated <- stats.updated + 1)
              | Error _ -> stats.updated <- stats.updated + 1
            with _ -> stats.updated <- stats.updated + 1)
          | None ->
            stats.updated <- stats.updated + 1
        end else begin
          stats.errors <- stats.errors + 1;
          match r.error with
          | Some e -> details := e :: !details
          | None -> ()
        end
      ) results
    end;

    (* Delete documents that no longer exist *)
    let to_delete = List.filter (fun id ->
      not (Hashtbl.mem new_id_set id)
    ) existing_ids in

    if to_delete <> [] then begin
      Log.info (fun m -> m "Deleting %d documents from %s" (List.length to_delete) name);
      let ts = Typesense_auth.Client.client client in
      List.iter (fun id ->
        try
          let _ = Typesense.Client.delete_document
            ~collection_name:collection ~document_id:id ts () in
          stats.deleted <- stats.deleted + 1
        with e ->
          stats.errors <- stats.errors + 1;
          details := (Printf.sprintf "Failed to delete %s: %s" id (Printexc.to_string e)) :: !details
      ) to_delete
    end
  end;

  { collection; stats; details = List.rev !details }

(** {1 Full Sync} *)

type sync_result = {
  collections : collection_sync_result list;
  total_created : int;
  total_updated : int;
  total_deleted : int;
  total_errors : int;
}

let sync ~dry_run ~(client : Typesense_auth.Client.t) ~(entries : Bushel.Entry.t) : sync_result =
  Log.info (fun m -> m "%s Typesense collections..."
    (if dry_run then "Checking" else "Syncing"));

  (* Prepare documents for each collection *)
  let notes_docs = List.map note_to_document (Bushel.Entry.notes entries) in
  let papers_docs = List.map paper_to_document (Bushel.Entry.papers entries) in
  let projects_docs = List.map project_to_document (Bushel.Entry.projects entries) in
  let ideas_docs = List.map idea_to_document (Bushel.Entry.ideas entries) in
  let videos_docs = List.map video_to_document (Bushel.Entry.videos entries) in
  let contacts_docs = List.map contact_to_document (Bushel.Entry.contacts entries) in

  (* Sync each collection *)
  let collections = [
    sync_collection ~dry_run client ~collection:"notes" ~schema:notes_schema ~documents:notes_docs;
    sync_collection ~dry_run client ~collection:"papers" ~schema:papers_schema ~documents:papers_docs;
    sync_collection ~dry_run client ~collection:"projects" ~schema:projects_schema ~documents:projects_docs;
    sync_collection ~dry_run client ~collection:"ideas" ~schema:ideas_schema ~documents:ideas_docs;
    sync_collection ~dry_run client ~collection:"videos" ~schema:videos_schema ~documents:videos_docs;
    sync_collection ~dry_run client ~collection:"contacts" ~schema:contacts_schema ~documents:contacts_docs;
  ] in

  (* Calculate totals *)
  let total_created = List.fold_left (fun acc r -> acc + r.stats.created) 0 collections in
  let total_updated = List.fold_left (fun acc r -> acc + r.stats.updated) 0 collections in
  let total_deleted = List.fold_left (fun acc r -> acc + r.stats.deleted) 0 collections in
  let total_errors = List.fold_left (fun acc r -> acc + r.stats.errors) 0 collections in

  Log.info (fun m -> m "Sync complete: %d created, %d updated, %d deleted, %d errors"
    total_created total_updated total_deleted total_errors);

  { collections; total_created; total_updated; total_deleted; total_errors }
