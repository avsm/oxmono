(*---------------------------------------------------------------------------
  Copyright (c) 2025 Anil Madhavapeddy <anil@recoil.org>. All rights reserved.
  SPDX-License-Identifier: ISC
 ---------------------------------------------------------------------------*)

(** Bushel sync orchestration

    {1 Re-exported Modules}

    - {!Zotero} - DOI resolution via Zotero Translation Server
    - {!Immich} - Contact face thumbnails from Immich
    - {!Peertube} - Video thumbnails from PeerTube
    - {!Http} - Simple HTTP client using curl
*)

(** DOI resolution via Zotero Translation Server *)
module Zotero = Bushel_zotero

(** Contact face thumbnails from Immich *)
module Immich = Bushel_immich

(** Video metadata and thumbnails from PeerTube *)
module Peertube = Bushel_peertube

(** HTTP client using the requests library *)
module Http = Bushel_http

let src = Logs.Src.create "bushel.sync" ~doc:"Bushel sync pipeline"
module Log = (val Logs.src_log src : Logs.LOG)

(** {1 Sync Steps} *)

type step =
  | Images      (** Rsync images from remote *)
  | Srcsetter   (** Run srcsetter on images *)
  | Thumbs      (** Generate paper thumbnails from PDFs *)
  | Faces       (** Fetch contact faces from Immich *)
  | Videos      (** Fetch video thumbnails from PeerTube *)
  | Typesense   (** Upload to Typesense *)

let string_of_step = function
  | Images -> "images"
  | Srcsetter -> "srcsetter"
  | Thumbs -> "thumbs"
  | Faces -> "faces"
  | Videos -> "videos"
  | Typesense -> "typesense"

let step_of_string = function
  | "images" -> Some Images
  | "srcsetter" -> Some Srcsetter
  | "thumbs" -> Some Thumbs
  | "faces" -> Some Faces
  | "videos" -> Some Videos
  | "typesense" -> Some Typesense
  | _ -> None

let all_steps = [Images; Thumbs; Faces; Videos; Srcsetter]
let all_steps_with_remote = all_steps @ [Typesense]

(** {1 Step Results} *)

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

(** {1 Rsync Images} *)

let sync_images ~dry_run ~fs ~proc_mgr config =
  Log.info (fun m -> m "Syncing images from remote...");
  let local_dir = config.Bushel_config.local_source_dir in
  let args = ["rsync"; "-avz";
              Bushel_config.rsync_source config ^ "/";
              local_dir ^ "/"] in
  let cmd = String.concat " " args in

  if dry_run then begin
    { step = Images; success = true;
      message = "Would run rsync";
      details = [cmd] }
  end else begin
    Log.debug (fun m -> m "Running: %s" cmd);

    (* Ensure local directory exists (recursive) *)
    let local_path = Eio.Path.(fs / local_dir) in
    Eio.Path.mkdirs ~exists_ok:true ~perm:0o755 local_path;

    try
      Eio.Process.run proc_mgr args;
      { step = Images; success = true;
        message = "Images synced from remote";
        details = [] }
    with e ->
      { step = Images; success = false;
        message = Printf.sprintf "Rsync failed: %s" (Printexc.to_string e);
        details = [] }
  end

(** {1 Srcsetter} *)

let run_srcsetter ~dry_run ~fs ~proc_mgr config =
  Log.info (fun m -> m "Running srcsetter...");
  let src_dir = config.Bushel_config.local_source_dir in
  let dst_dir = config.Bushel_config.local_output_dir in

  if dry_run then begin
    { step = Srcsetter; success = true;
      message = "Would run srcsetter";
      details = [Printf.sprintf "srcsetter %s %s" src_dir dst_dir] }
  end else begin
    (* Ensure output directory exists (recursive) *)
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

(** {1 Paper Thumbnails} *)

let generate_paper_thumbnails ~dry_run ~fs ~proc_mgr config =
  Log.info (fun m -> m "Generating paper thumbnails...");
  let pdfs_dir = config.Bushel_config.paper_pdfs_dir in
  (* Output to local_source_dir/papers/ so srcsetter processes them *)
  let output_dir = Filename.concat config.Bushel_config.local_source_dir "papers" in

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
        (* Output as PNG - srcsetter will convert to webp *)
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
      (* Ensure output directory exists (recursive) *)
      let output_path = Eio.Path.(fs / output_dir) in
      Eio.Path.mkdirs ~exists_ok:true ~perm:0o755 output_path;

      let results = List.map (fun pdf_file ->
        let slug = Filename.chop_extension pdf_file in
        let pdf_path = Filename.concat pdfs_dir pdf_file in
        (* Output as PNG - srcsetter will convert to webp *)
        let output_path = Filename.concat output_dir (slug ^ ".png") in

        if Sys.file_exists output_path then begin
          Log.debug (fun m -> m "Skipping %s: thumbnail exists" slug);
          `Skipped slug
        end else begin
          Log.info (fun m -> m "Generating thumbnail for %s" slug);
          try
            (* ImageMagick command: render PDF at 600 DPI, crop top 50%, resize to 2048px *)
            let args = [
              "magick";
              "-density"; "600";
              "-quality"; "100";
              pdf_path ^ "[0]";  (* First page only *)
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

      let ok_count = List.fold_left (fun acc r -> match r with `Ok _ -> acc + 1 | _ -> acc) 0 results in
      let skipped_count = List.fold_left (fun acc r -> match r with `Skipped _ -> acc + 1 | _ -> acc) 0 results in
      let error_count = List.fold_left (fun acc r -> match r with `Error _ -> acc + 1 | _ -> acc) 0 results in

      { step = Thumbs; success = error_count = 0;
        message = Printf.sprintf "%d generated, %d skipped, %d errors"
          ok_count skipped_count error_count;
        details = List.filter_map (fun r -> match r with `Error s -> Some s | _ -> None) results }
    end
  end

(** {1 Contact Faces} *)

let sync_faces ~dry_run ~fs config entries =
  Log.info (fun m -> m "Syncing contact faces from Sortal...");
  (* Output to local_source_dir/faces/ so srcsetter processes them *)
  let output_dir = Filename.concat config.Bushel_config.local_source_dir "faces" in
  let contacts = Bushel.Entry.contacts entries in

  (* Load sortal store to get thumbnail paths *)
  let sortal_store = Sortal.Store.create fs "sortal" in

  (* Find contacts with PNG thumbnails that need copying *)
  let contacts_with_thumbs = List.filter_map (fun c ->
    match Sortal.Store.png_thumbnail_path sortal_store c with
    | Some path -> Some (c, path)
    | None -> None
  ) contacts in

  if dry_run then begin
    let would_copy = List.filter (fun (c, _src_path) ->
      let handle = Sortal_schema.Contact.handle c in
      let output_path = Filename.concat output_dir (handle ^ ".png") in
      not (Sys.file_exists output_path)
    ) contacts_with_thumbs in
    let skipped = List.length contacts_with_thumbs - List.length would_copy in
    let no_thumb = List.length contacts - List.length contacts_with_thumbs in
    { step = Faces; success = true;
      message = Printf.sprintf "Would copy %d faces from Sortal (%d already exist, %d without thumbnails)"
        (List.length would_copy) skipped no_thumb;
      details = List.map (fun (c, src_path) ->
        let handle = Sortal_schema.Contact.handle c in
        Printf.sprintf "cp %s %s/%s.png" (Eio.Path.native_exn src_path) output_dir handle
      ) (List.filteri (fun i _ -> i < 5) would_copy) @
      (if List.length would_copy > 5 then ["...and more"] else []) }
  end else begin
    (* Ensure output directory exists *)
    let output_path = Eio.Path.(fs / output_dir) in
    Eio.Path.mkdirs ~exists_ok:true ~perm:0o755 output_path;

    let results = List.map (fun (c, src_path) ->
      let handle = Sortal_schema.Contact.handle c in
      let dst_path = Filename.concat output_dir (handle ^ ".png") in

      if Sys.file_exists dst_path then begin
        Log.debug (fun m -> m "Skipping %s: already exists" handle);
        (handle, `Skipped)
      end else begin
        Log.info (fun m -> m "Copying face for %s" handle);
        try
          let content = Eio.Path.load src_path in
          let oc = open_out_bin dst_path in
          output_string oc content;
          close_out oc;
          (handle, `Ok)
        with e ->
          Log.err (fun m -> m "Failed to copy face for %s: %s" handle (Printexc.to_string e));
          (handle, `Error (Printexc.to_string e))
      end
    ) contacts_with_thumbs in

    let ok_count = List.length (List.filter (fun (_, r) -> r = `Ok) results) in
    let skipped_count = List.length (List.filter (fun (_, r) -> r = `Skipped) results) in
    let error_count = List.length (List.filter (fun (_, r) -> match r with `Error _ -> true | _ -> false) results) in
    let no_thumb = List.length contacts - List.length contacts_with_thumbs in

    { step = Faces; success = error_count = 0;
      message = Printf.sprintf "%d copied, %d skipped, %d errors, %d without thumbnails"
        ok_count skipped_count error_count no_thumb;
      details = List.filter_map (fun (h, r) ->
        match r with `Error e -> Some (h ^ ": " ^ e) | _ -> None
      ) results }
  end

(** {1 Video Thumbnails} *)

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

    (* Save updated index (may have discovered new server mappings) *)
    Bushel_peertube.VideoIndex.save_file videos_yml index;

    let ok_count = List.length (List.filter (fun (_, r) ->
      match r with Bushel_peertube.Ok _ -> true | _ -> false) results) in
    let skipped_count = List.length (List.filter (fun (_, r) ->
      match r with Bushel_peertube.Skipped _ -> true | _ -> false) results) in
    let error_count = List.length (List.filter (fun (_, r) ->
      match r with Bushel_peertube.Error _ -> true | _ -> false) results) in

    { step = Videos; success = true;
      message = Printf.sprintf "%d fetched, %d skipped, %d errors"
        ok_count skipped_count error_count;
      details = List.filter_map (fun (uuid, r) ->
        match r with Bushel_peertube.Error e -> Some (uuid ^ ": " ^ e) | _ -> None
      ) results }
  end

(** {1 Typesense Upload} *)

let upload_typesense ~dry_run ~sw ~env config entries =
  Log.info (fun m -> m "%s Typesense..." (if dry_run then "Checking" else "Syncing"));

  match Bushel_config.typesense_api_key config with
  | Error e ->
    { step = Typesense; success = false;
      message = "Missing Typesense API key";
      details = [e] }
  | Ok api_key ->
    try
      (* Create Typesense client *)
      let client = Typesense_auth.Client.login ~sw ~env
        ~server_url:config.typesense_endpoint
        ~api_key
        () in

      (* Run incremental sync *)
      let result = Bushel_typesense.sync ~dry_run ~client ~entries in

      (* Format details from each collection *)
      let details = List.concat_map (fun (r : Bushel_typesense.collection_sync_result) ->
        let stats = r.stats in
        let summary = Printf.sprintf "%s: %d created, %d updated, %d deleted"
          r.collection stats.created stats.updated stats.deleted in
        summary :: r.details
      ) result.collections in

      { step = Typesense; success = result.total_errors = 0;
        message = Printf.sprintf "%s: %d created, %d updated, %d deleted, %d errors"
          (if dry_run then "Would sync" else "Synced")
          result.total_created result.total_updated result.total_deleted result.total_errors;
        details }
    with e ->
      { step = Typesense; success = false;
        message = Printf.sprintf "Typesense sync failed: %s" (Printexc.to_string e);
        details = [] }

(** {1 Run Pipeline} *)

let run ~dry_run ~sw ~env ~config ~steps ~entries =
  let proc_mgr = Eio.Stdenv.process_mgr env in
  let fs = Eio.Stdenv.fs env in
  (* Create HTTP session for network requests *)
  let http = Bushel_http.create ~sw env in

  let results = List.map (fun step ->
    Log.info (fun m -> m "%s step: %s"
      (if dry_run then "Dry-run" else "Running")
      (string_of_step step));
    match step with
    | Images -> sync_images ~dry_run ~fs ~proc_mgr config
    | Srcsetter -> run_srcsetter ~dry_run ~fs ~proc_mgr config
    | Thumbs -> generate_paper_thumbnails ~dry_run ~fs ~proc_mgr config
    | Faces -> sync_faces ~dry_run ~fs config entries
    | Videos -> sync_video_thumbnails ~dry_run ~http config entries
    | Typesense -> upload_typesense ~dry_run ~sw ~env config entries
  ) steps in

  (* Summary *)
  let success_count = List.length (List.filter (fun r -> r.success) results) in
  let total = List.length results in
  Log.info (fun m -> m "%s complete: %d/%d steps succeeded"
    (if dry_run then "Dry-run" else "Sync")
    success_count total);

  results
