(*---------------------------------------------------------------------------
  Copyright (c) 2025 Anil Madhavapeddy <anil@recoil.org>. All rights reserved.
  SPDX-License-Identifier: ISC
 ---------------------------------------------------------------------------*)

(** Bushel synchronization pipeline. *)

(** Zotero metadata resolution. *)
module Zotero = Bushel_zotero

(** PeerTube metadata and thumbnails. *)
module Peertube = Bushel_peertube

(** Fetch-backed HTTP client. *)
module Http = Bushel_http

(** Karakeep synchronization. *)
module Karakeep = Bushel_karakeep

let src = Logs.Src.create "bushel.sync" ~doc:"Bushel sync pipeline"
module Log = (val Logs.src_log src : Logs.LOG)

type step =
  | Git         (** Pulls the data repository. *)
  | Images      (** Pulls the image repository. *)
  | Srcsetter   (** Processes images with Srcsetter. *)
  | Thumbs      (** Generates paper thumbnails. *)
  | Faces       (** Copies contact portraits from Sortal. *)
  | Videos      (** Fetches PeerTube thumbnails. *)
  | Links       (** Synchronizes links with Karakeep. *)
  | Dois        (** Resolves link metadata with Zotero. *)

let string_of_step = function
  | Git -> "git"
  | Images -> "images"
  | Srcsetter -> "srcsetter"
  | Thumbs -> "thumbs"
  | Faces -> "faces"
  | Videos -> "videos"
  | Links -> "links"
  | Dois -> "dois"

let step_of_string = function
  | "git" -> Some Git
  | "images" -> Some Images
  | "srcsetter" -> Some Srcsetter
  | "thumbs" -> Some Thumbs
  | "faces" -> Some Faces
  | "videos" -> Some Videos
  | "links" -> Some Links
  | "dois" -> Some Dois
  | _ -> None

let all_steps = [Git; Images; Thumbs; Faces; Videos; Srcsetter; Links; Dois]

type step_result = {
  step : step;
  success : bool;
  message : string;
  details : string list;
}

let pp_result ppf r =
  let status = if r.success then "OK" else "FAILED" in
  Fmt.pf ppf "[%s] %s: %s" status (string_of_step r.step) r.message;
  if r.details <> [] then begin
    Fmt.pf ppf "@,";
    List.iter (fun d -> Fmt.pf ppf "  - %s@," d) r.details
  end

let count_outcomes classify values =
  List.fold_left
    (fun (ok, skipped, error) value ->
      match classify value with
      | `Ok -> (ok + 1, skipped, error)
      | `Skipped -> (ok, skipped + 1, error)
      | `Error -> (ok, skipped, error + 1))
    (0, 0, 0) values

let sync_images ~dry_run ~env config =
  Log.info (fun m -> m "%s images..." (if dry_run then "Checking" else "Pulling"));
  let sync_config = config.Bushel_config.images_sync in
  if sync_config.Gitops.Sync.Config.remote = "" then begin
    Log.warn (fun m -> m "No images sync remote configured, skipping");
    { step = Images; success = true;
      message = "Skipped (no remote configured)";
      details = [] }
  end else begin
    try
      let git = Gitops.v ~dry_run env in
      let repo = Eio.Path.(env#fs / config.Bushel_config.images_dir) in
      let pulled = Gitops.Sync.pull git ~config:sync_config ~repo in
      { step = Images; success = true;
        message = (if pulled then "Pulled image changes from remote"
                   else "Images already up to date");
        details = [] }
    with e ->
      { step = Images; success = false;
        message = Printf.sprintf "Image pull failed: %s" (Printexc.to_string e);
        details = [] }
  end

let run_srcsetter ~dry_run ~fs ~proc_mgr config =
  Log.info (fun m -> m "Running srcsetter...");
  let src_dir = config.Bushel_config.images_dir in
  let dst_dir = config.Bushel_config.images_output_dir in

  if dry_run then begin
    { step = Srcsetter; success = true;
      message = "Would run srcsetter";
      details = [Printf.sprintf "srcsetter %s %s" src_dir dst_dir] }
  end else begin
    let src_path = Eio.Path.(fs / src_dir) in
    let dst_path = Eio.Path.(fs / dst_dir) in
    Eio.Path.mkdirs ~exists_ok:true ~perm:0o755 dst_path;

    try
      let entries = Srcsetter_cmd.run
        ~proc_mgr
        ~src_dir:src_path
        ~dst_dir:dst_path
        ~preserve:true
        ()
      in
      { step = Srcsetter; success = true;
        message = Printf.sprintf "Srcsetter completed: %d images processed"
          (List.length entries);
        details = [] }
    with e ->
      { step = Srcsetter; success = false;
        message = Printf.sprintf "Srcsetter failed: %s" (Printexc.to_string e);
        details = [] }
  end

let generate_paper_thumbnails ~dry_run ~fs ~proc_mgr config =
  Log.info (fun m -> m "Generating paper thumbnails...");
  let pdfs_dir = config.Bushel_config.paper_pdfs_dir in
  let output_dir = Filename.concat config.Bushel_config.images_dir "papers" in

  if not (Sys.file_exists pdfs_dir) then begin
    Log.warn (fun m -> m "PDFs directory does not exist: %s" pdfs_dir);
    { step = Thumbs; success = true;
      message = "No PDFs directory";
      details = [] }
  end else begin
    let pdfs = Sys.readdir pdfs_dir |> Array.to_list
               |> List.filter (fun f -> Filename.check_suffix f ".pdf") in

    if dry_run then begin
      let would_run = List.filter_map (fun pdf_file ->
        let slug = Filename.chop_extension pdf_file in
        let pdf_path = Filename.concat pdfs_dir pdf_file in
        let output_path = Filename.concat output_dir (slug ^ ".png") in
        if Sys.file_exists output_path then None
        else begin
          let args = [
            "magick"; "-density"; "600"; "-quality"; "100";
            pdf_path ^ "[0]"; "-gravity"; "North";
            "-crop"; "100%x50%+0+0"; "-resize"; "2048x"; output_path
          ] in
          Some (String.concat " " args)
        end
      ) pdfs in
      let skipped = List.length pdfs - List.length would_run in
      { step = Thumbs; success = true;
        message = Printf.sprintf "Would generate %d thumbnails (%d already exist)"
          (List.length would_run) skipped;
        details = would_run }
    end else begin
      let output_path = Eio.Path.(fs / output_dir) in
      Eio.Path.mkdirs ~exists_ok:true ~perm:0o755 output_path;

      let results = List.map (fun pdf_file ->
        let slug = Filename.chop_extension pdf_file in
        let pdf_path = Filename.concat pdfs_dir pdf_file in
        let output_path = Filename.concat output_dir (slug ^ ".png") in

        if Sys.file_exists output_path then begin
          Log.debug (fun m -> m "Skipping %s: thumbnail exists" slug);
          `Skipped slug
        end else begin
          Log.info (fun m -> m "Generating thumbnail for %s" slug);
          try
            let args = [
              "magick";
              "-density"; "600";
              "-quality"; "100";
              pdf_path ^ "[0]";
              "-gravity"; "North";
              "-crop"; "100%x50%+0+0";
              "-resize"; "2048x";
              output_path
            ] in
            Eio.Process.run proc_mgr args;
            `Ok slug
          with e ->
            Log.err (fun m -> m "Failed to generate thumbnail for %s: %s"
              slug (Printexc.to_string e));
            `Error slug
        end
      ) pdfs in

      let ok_count, skipped_count, error_count =
        count_outcomes
          (function `Ok _ -> `Ok | `Skipped _ -> `Skipped | `Error _ -> `Error)
          results
      in

      { step = Thumbs; success = error_count = 0;
        message = Printf.sprintf "%d generated, %d skipped, %d errors"
          ok_count skipped_count error_count;
        details = List.filter_map (fun r -> match r with `Error s -> Some s | _ -> None) results }
    end
  end

let sync_faces ~dry_run ~fs config entries =
  Log.info (fun m -> m "Syncing contact faces from Sortal...");
  let output_dir = Filename.concat config.Bushel_config.images_dir "faces" in
  let contacts = Bushel.Entry.contacts entries in

  let sortal_store = Sortal.Store.create fs "sortal" in

  let contacts_with_thumbs = List.filter_map (fun c ->
    match Sortal.Store.thumbnail_path sortal_store c with
    | Some path -> Some (c, path)
    | None -> None
  ) contacts in

  if dry_run then begin
    let would_copy = List.filter (fun (c, src_path) ->
      let handle = Sortal_schema.Contact.handle c in
      let ext = Filename.extension (Eio.Path.native_exn src_path) in
      let output_path = Filename.concat output_dir (handle ^ ext) in
      not (Sys.file_exists output_path)
    ) contacts_with_thumbs in
    let skipped = List.length contacts_with_thumbs - List.length would_copy in
    let no_thumb = List.length contacts - List.length contacts_with_thumbs in
    { step = Faces; success = true;
      message = Printf.sprintf "Would copy %d faces from Sortal (%d already exist, %d without thumbnails)"
        (List.length would_copy) skipped no_thumb;
      details = List.map (fun (c, src_path) ->
        let handle = Sortal_schema.Contact.handle c in
        let ext = Filename.extension (Eio.Path.native_exn src_path) in
        Printf.sprintf "cp %s %s/%s%s" (Eio.Path.native_exn src_path) output_dir handle ext
      ) (List.filteri (fun i _ -> i < 5) would_copy) @
      (if List.length would_copy > 5 then ["...and more"] else []) }
  end else begin
    let output_path = Eio.Path.(fs / output_dir) in
    Eio.Path.mkdirs ~exists_ok:true ~perm:0o755 output_path;

    let results = List.map (fun (c, src_path) ->
      let handle = Sortal_schema.Contact.handle c in
      let ext = Filename.extension (Eio.Path.native_exn src_path) in
      let dst_path = Filename.concat output_dir (handle ^ ext) in

      if Sys.file_exists dst_path then begin
        Log.debug (fun m -> m "Skipping %s: already exists" handle);
        (handle, `Skipped)
      end else begin
        Log.info (fun m -> m "Copying face for %s" handle);
        try
          let content = Eio.Path.load src_path in
          Out_channel.with_open_bin dst_path (fun oc -> output_string oc content);
          (handle, `Ok)
        with e ->
          Log.err (fun m -> m "Failed to copy face for %s: %s" handle (Printexc.to_string e));
          (handle, `Error (Printexc.to_string e))
      end
    ) contacts_with_thumbs in

    let ok_count, skipped_count, error_count =
      count_outcomes
        (fun (_, result) -> match result with
          | `Ok -> `Ok | `Skipped -> `Skipped | `Error _ -> `Error)
        results
    in
    let no_thumb = List.length contacts - List.length contacts_with_thumbs in

    { step = Faces; success = error_count = 0;
      message = Printf.sprintf "%d copied, %d skipped, %d errors, %d without thumbnails"
        ok_count skipped_count error_count no_thumb;
      details = List.filter_map (fun (h, r) ->
        match r with `Error e -> Some (h ^ ": " ^ e) | _ -> None
      ) results }
  end

let sync_video_thumbnails ~dry_run ~http config entries =
  Log.info (fun m -> m "Syncing video thumbnails from PeerTube...");
  let output_dir = Bushel_config.video_thumbs_dir config in
  let videos_yml = Filename.concat config.data_dir "videos.yml" in

  let index = Bushel_peertube.VideoIndex.load_file videos_yml in
  let videos = Bushel.Entry.videos entries in
  let count = List.length videos in

  if count = 0 then begin
    Log.info (fun m -> m "No videos found");
    { step = Videos; success = true;
      message = "No videos found";
      details = [] }
  end else if dry_run then begin
    let would_fetch = List.filter (fun video ->
      let uuid = Bushel.Video.uuid video in
      let output_path = Filename.concat output_dir (uuid ^ ".jpg") in
      not (Sys.file_exists output_path)
    ) videos in
    let skipped = count - List.length would_fetch in
    { step = Videos; success = true;
      message = Printf.sprintf "Would fetch %d video thumbnails from PeerTube (%d already exist)"
        (List.length would_fetch) skipped;
      details = List.map (fun video ->
        let uuid = Bushel.Video.uuid video in
        let url = Bushel.Video.url video in
        if url <> "" then
          Printf.sprintf "%s (from URL: %s)" uuid url
        else
          Printf.sprintf "%s (will search servers)" uuid
      ) (List.filteri (fun i _ -> i < 5) would_fetch) @
      (if List.length would_fetch > 5 then ["...and more"] else []) }
  end else begin
    let results = Bushel_peertube.fetch_thumbnails
      ~http
      ~servers:config.peertube_servers
      ~output_dir
      ~videos
      ~index in

    Bushel_peertube.VideoIndex.save_file videos_yml index;

    let ok_count, skipped_count, error_count =
      count_outcomes
        (fun (_, result) -> match result with
          | Bushel_peertube.Ok _ -> `Ok
          | Bushel_peertube.Skipped _ -> `Skipped
          | Bushel_peertube.Error _ -> `Error)
        results
    in

    { step = Videos; success = true;
      message = Printf.sprintf "%d fetched, %d skipped, %d errors"
        ok_count skipped_count error_count;
      details = List.filter_map (fun (uuid, r) ->
        match r with Bushel_peertube.Error e -> Some (uuid ^ ": " ^ e) | _ -> None
      ) results }
  end

let sync_git ~dry_run ~env ~data_dir config =
  Log.info (fun m -> m "%s git data..." (if dry_run then "Checking" else "Pulling"));
  let sync_config = config.Bushel_config.sync in
  if sync_config.Gitops.Sync.Config.remote = "" then begin
    Log.warn (fun m -> m "No sync remote configured, skipping git pull");
    { step = Git; success = true;
      message = "Skipped (no remote configured)";
      details = [] }
  end else begin
    try
      let git = Gitops.v ~dry_run env in
      let repo = Eio.Path.(env#fs / data_dir) in
      let pulled = Gitops.Sync.pull git ~config:sync_config ~repo in
      { step = Git; success = true;
        message = (if pulled then "Pulled changes from remote"
                   else "Already up to date");
        details = [] }
    with e ->
      { step = Git; success = false;
        message = Printf.sprintf "Git pull failed: %s" (Printexc.to_string e);
        details = [] }
  end

type link_generation = { generated : int; added : int; total : int }

(** [generate_links ~dry_run ~data_dir ~entries] is the link-generation
    summary for [entries]. *)
let generate_links ~dry_run ~data_dir ~entries =
  let external_links = Bushel.Entry.all_external_links entries in
  if external_links = [] then begin
    Log.info (fun m -> m "No external links found in entries");
    { generated = 0; added = 0; total = 0 }
  end
  else begin
    let by_url : (string, Bushel.Link_graph.external_link list) Hashtbl.t =
      Hashtbl.create 256
    in
    let open Bushel.Link_graph in
    List.iter (fun (link : external_link) ->
      let cur = Option.value ~default:[] (Hashtbl.find_opt by_url link.url) in
      if not (List.exists (fun (l : external_link) ->
        l.source = link.source) cur) then
        Hashtbl.replace by_url link.url (link :: cur)
    ) external_links;
    let urls = Hashtbl.fold (fun url links acc -> (url, links) :: acc) by_url [] in
    let today () =
      let t = Unix.gmtime (Unix.gettimeofday ()) in
      t.tm_year + 1900, t.tm_mon + 1, t.tm_mday
    in
    let new_links = List.map (fun (url, sources) ->
      let date = match sources with
        | link :: _ ->
          (match Bushel.Entry.lookup entries link.source with
           | Some entry -> Bushel.Entry.date entry
           | None -> today ())
        | [] -> today ()
      in
      let slugs = List.map (fun (l : external_link) -> l.source) sources in
      let link : Bushel.Link.t = {
        url; date; description = "";
        karakeep = None;
        bushel = Some { slugs; tags = [] };
      } in
      link
    ) urls in
    let links_file = Filename.concat data_dir "links.yml" in
    let existing = Bushel.Link.load_links_file links_file in
    let merged = Bushel.Link.merge_links existing new_links in
    let result = {
      generated = List.length new_links;
      added = List.length merged - List.length existing;
      total = List.length merged;
    } in
    if dry_run then
      Log.info (fun m -> m "Would generate %d links (%d new, %d total)"
        result.generated result.added result.total)
    else begin
      Bushel.Link.save_links_file links_file merged;
      Log.info (fun m -> m "Generated %d links (%d new, %d total)"
        result.generated result.added result.total)
    end;
    result
  end

let sync_links ~dry_run ~sw ~env ~data_dir ~entries =
  let fs = Eio.Stdenv.fs env in
  match Karakeep_auth.Session.load fs () with
  | None ->
    Log.warn (fun m -> m "No karakeep credentials found, skipping links sync");
    { step = Links; success = true;
      message = "Skipped (no karakeep credentials)";
      details = [] }
  | Some session ->
    ignore (generate_links ~dry_run ~data_dir ~entries);
    Log.info (fun m -> m "Syncing links with Karakeep...");
    try
      let client = Karakeep_auth.Client.resume ~sw ~env ~session () in
      let api = Karakeep_auth.Client.client client in
      let (success, message, details) =
        Bushel_karakeep.sync_links ~dry_run ~api ~data_dir
      in
      { step = Links; success; message; details }
    with e ->
      { step = Links; success = false;
        message = Printf.sprintf "Links sync failed: %s" (Printexc.to_string e);
        details = [] }

let sync_dois ~dry_run ~retry_errors ~http ~data_dir ~config =
  let doi_file = Filename.concat data_dir "doi.yml" in
  let links_file = Filename.concat data_dir "links.yml" in
  let existing_dois = Bushel.Doi_entry.load_file doi_file in
  let existing_dois =
    if retry_errors then begin
      let failed = List.filter Bushel.Doi_entry.is_failed existing_dois in
      if failed <> [] then
        Log.info (fun m -> m "Retrying %d previously failed DOI entries"
          (List.length failed));
      Bushel.Doi_entry.remove_failed existing_dois
    end else existing_dois
  in
  let links = Bushel.Link.load_links_file links_file in

  let extract_doi url =
    match Astring.String.cut ~sep:"doi.org/" url with
    | Some (_, doi) -> Some doi
    | None -> None
  in

  let doi_links = List.filter_map (fun l ->
    let url = Bushel.Link.url l in
    match extract_doi url with
    | None -> None
    | Some doi ->
      if Bushel.Doi_entry.find_by_doi existing_dois doi <> None then None
      else Some (`Doi (doi, url))
  ) links in

  let academic_links = List.filter_map (fun l ->
    let url = Bushel.Link.url l in
    if not (Bushel.Link.is_academic_url url) then None
    else if Bushel.Link.is_doi_url url then None
    else if Bushel.Doi_entry.find_by_url existing_dois url <> None then None
    else Some (`Url url)
  ) links in

  let all_new = doi_links @ academic_links in
  Log.info (fun m -> m "Found %d DOI links and %d academic URLs to resolve"
    (List.length doi_links) (List.length academic_links));

  if all_new = [] then
    { step = Dois; success = true;
      message = Printf.sprintf "No new DOIs (checked %d links)" (List.length links);
      details = [] }
  else if dry_run then
    { step = Dois; success = true;
      message = Printf.sprintf "Would resolve %d DOIs + %d academic URLs"
        (List.length doi_links) (List.length academic_links);
      details = List.map (function
        | `Doi (doi, _) -> "doi: " ^ doi
        | `Url url -> "url: " ^ url
      ) all_new }
  else begin
    let server_url = config.Bushel_config.zotero_translation_server in
    let now =
      let t = Unix.gmtime (Unix.gettimeofday ()) in
      Printf.sprintf "%04d-%02d-%02d" (t.tm_year + 1900) (t.tm_mon + 1) t.tm_mday
    in
    let resolve_one item =
      let (source_url, result) = match item with
        | `Doi (doi, source_url) ->
          (source_url, Bushel_zotero.resolve ~http ~server_url ~slug:doi doi)
        | `Url source_url ->
          (source_url, Bushel_zotero.resolve_from_url ~http ~server_url
            ~slug:"auto" source_url)
      in
      match result with
      | Ok meta ->
        let doi = match meta.doi with
          | Some d -> d
          | None -> source_url
        in
        Log.info (fun m -> m "Resolved: %s -> %s" source_url meta.title);
        Some Bushel.Doi_entry.{
          doi;
          title = meta.title;
          authors = meta.authors;
          year = meta.year;
          bibtype = meta.bibtype;
          publisher = meta.publisher;
          resolved_at = now;
          source_urls = [source_url];
          status = Resolved;
          ignore = false;
        }
      | Error e ->
        let doi = (match item with `Doi (d, _) -> d | `Url u -> u) in
        Log.warn (fun m -> m "Failed to resolve %s: %s" source_url e);
        Some Bushel.Doi_entry.{
          doi;
          title = "";
          authors = [];
          year = 0;
          bibtype = "";
          publisher = "";
          resolved_at = now;
          source_urls = [source_url];
          status = Failed e;
          ignore = false;
        }
    in
    let resolved = List.filter_map resolve_one all_new in
    let merged = Bushel.Doi_entry.merge_entries existing_dois resolved in
    Bushel.Doi_entry.save_file doi_file merged;
    let ok_count = List.length (List.filter (fun e ->
      e.Bushel.Doi_entry.status = Resolved) resolved) in
    let fail_count = List.length resolved - ok_count in
    { step = Dois; success = true;
      message = Printf.sprintf "Resolved %d entries (%d failed)" ok_count fail_count;
      details = List.map (fun e ->
        let status = match e.Bushel.Doi_entry.status with
          | Resolved -> e.Bushel.Doi_entry.title
          | Failed err -> "FAILED: " ^ err
        in
        Printf.sprintf "%s: %s" e.doi status
      ) resolved }
  end

(** [run ~dry_run ~retry_errors ~sw ~env ~data_dir ~config ~steps ~entries]
    is the result of running [steps] in order. *)
let run ~dry_run ~retry_errors ~sw ~env ~data_dir ~config ~steps ~entries =
  let proc_mgr = Eio.Stdenv.process_mgr env in
  let fs = Eio.Stdenv.fs env in
  let http = Bushel_http.create ~sw env in

  let results = List.map (fun step ->
    Log.info (fun m -> m "%s step: %s"
      (if dry_run then "Dry-run" else "Running")
      (string_of_step step));
    match step with
    | Git -> sync_git ~dry_run ~env ~data_dir config
    | Images -> sync_images ~dry_run ~env config
    | Srcsetter -> run_srcsetter ~dry_run ~fs ~proc_mgr config
    | Thumbs -> generate_paper_thumbnails ~dry_run ~fs ~proc_mgr config
    | Faces -> sync_faces ~dry_run ~fs config entries
    | Videos -> sync_video_thumbnails ~dry_run ~http config entries
    | Links -> sync_links ~dry_run ~sw ~env ~data_dir ~entries
    | Dois -> sync_dois ~dry_run ~retry_errors ~http ~data_dir ~config
  ) steps in

  let success_count = List.length (List.filter (fun r -> r.success) results) in
  let total = List.length results in
  Log.info (fun m -> m "%s complete: %d/%d steps succeeded"
    (if dry_run then "Dry-run" else "Sync")
    success_count total);

  results
