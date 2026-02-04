(*---------------------------------------------------------------------------
  Copyright (c) 2025 Anil Madhavapeddy <anil@recoil.org>. All rights reserved.
  SPDX-License-Identifier: ISC
 ---------------------------------------------------------------------------*)

(** Bushel CLI - knowledge base management tool *)

open Cmdliner

(** Simple table formatting *)
module Table = struct
  type row = string list
  type t = { headers : string list; rows : row list }

  let make ~headers rows = { headers; rows }

  let column_widths t =
    let num_cols = List.length t.headers in
    let widths = Array.make num_cols 0 in
    (* Headers *)
    List.iteri (fun i h -> widths.(i) <- String.length h) t.headers;
    (* Rows *)
    List.iter (fun row ->
      List.iteri (fun i cell ->
        if i < num_cols then
          widths.(i) <- max widths.(i) (String.length cell)
      ) row
    ) t.rows;
    Array.to_list widths

  let pad s width =
    let len = String.length s in
    if len >= width then s
    else s ^ String.make (width - len) ' '

  let print t =
    let widths = column_widths t in
    let print_row row =
      List.iter2 (fun cell width ->
        Printf.printf "%s  " (pad cell width)
      ) row widths;
      print_newline ()
    in
    (* Print header *)
    print_row t.headers;
    (* Print separator *)
    List.iter (fun w -> Printf.printf "%s  " (String.make w '-')) widths;
    print_newline ();
    (* Print rows *)
    List.iter print_row t.rows
end

(** Truncate string to max length with ellipsis *)
let truncate max_len s =
  if String.length s <= max_len then s
  else String.sub s 0 (max_len - 3) ^ "..."

(** Format date tuple *)
let format_date (year, month, day) =
  Printf.sprintf "%04d-%02d-%02d" year month day

(** Entry type to string *)
let type_string = function
  | `Paper _ -> "paper"
  | `Project _ -> "project"
  | `Idea _ -> "idea"
  | `Video _ -> "video"
  | `Note _ -> "note"

(** {1 Common Options} *)

let data_dir =
  let doc = "Path to the bushel data repository." in
  let env = Cmd.Env.info "BUSHEL_DATA" in
  Arg.(value & opt (some string) None & info ["d"; "data-dir"] ~env ~docv:"DIR" ~doc)

let config_file =
  let doc = "Path to config file (default: ~/.config/bushel/config.toml)." in
  Arg.(value & opt (some string) None & info ["c"; "config"] ~docv:"FILE" ~doc)

(** Setup logging *)
let setup_log style_renderer level =
  Fmt_tty.setup_std_outputs ?style_renderer ();
  Logs.set_level level;
  Logs.set_reporter (Logs_fmt.reporter ())

let logging_t =
  Term.(const setup_log $ Fmt_cli.style_renderer () $ Logs_cli.level ())

(** Load config *)
let load_config config_file =
  match config_file with
  | Some path -> Bushel_config.load_file path
  | None -> Bushel_config.load ()

(** Get data directory from config or CLI *)
let get_data_dir config data_dir_opt =
  match data_dir_opt with
  | Some d -> d
  | None -> config.Bushel_config.data_dir

(** Load entries using Eio *)
let with_entries ?image_output_dir data_dir f =
  Eio_main.run @@ fun env ->
  let fs = Eio.Stdenv.fs env in
  let entries = Bushel_eio.Bushel_loader.load ?image_output_dir fs data_dir in
  f env entries

(** {1 List Command} *)

let list_cmd =
  let type_filter =
    let doc = "Filter by entry type (paper, project, idea, video, note)." in
    Arg.(value & opt (some string) None & info ["t"; "type"] ~docv:"TYPE" ~doc)
  in
  let limit =
    let doc = "Maximum number of entries to show." in
    Arg.(value & opt (some int) None & info ["n"; "limit"] ~docv:"N" ~doc)
  in
  let sort_by =
    let doc = "Sort by field (date, title, type). Default: date." in
    Arg.(value & opt string "date" & info ["s"; "sort"] ~docv:"FIELD" ~doc)
  in
  let run () config_file data_dir type_filter limit sort_by =
    match load_config config_file with
    | Error e -> Printf.eprintf "Config error: %s\n" e; 1
    | Ok config ->
      let data_dir = get_data_dir config data_dir in
      with_entries data_dir @@ fun _env entries ->
      let all = Bushel.Entry.all_entries entries in
      (* Filter by type *)
      let filtered = match type_filter with
        | None -> all
        | Some t ->
          List.filter (fun e ->
            String.lowercase_ascii (type_string e) = String.lowercase_ascii t
          ) all
      in
      (* Sort *)
      let sorted = match sort_by with
        | "title" ->
          List.sort (fun a b ->
            String.compare (Bushel.Entry.title a) (Bushel.Entry.title b)
          ) filtered
        | "type" ->
          List.sort (fun a b ->
            let cmp = String.compare (type_string a) (type_string b) in
            if cmp <> 0 then cmp
            else Bushel.Entry.compare a b
          ) filtered
        | _ -> (* date, default *)
          List.sort (fun a b -> Bushel.Entry.compare b a) filtered (* newest first *)
      in
      (* Limit *)
      let limited = match limit with
        | None -> sorted
        | Some n -> List.filteri (fun i _ -> i < n) sorted
      in
      (* Build table *)
      let rows = List.map (fun e ->
        let thumb = match Bushel.Entry.thumbnail_slug entries e with
          | Some s -> s
          | None -> "-"
        in
        [ type_string e
        ; Bushel.Entry.slug e
        ; truncate 50 (Bushel.Entry.title e)
        ; format_date (Bushel.Entry.date e)
        ; thumb
        ]
      ) limited in
      let table = Table.make
        ~headers:["TYPE"; "SLUG"; "TITLE"; "DATE"; "THUMBNAIL"]
        rows
      in
      Table.print table;
      Printf.printf "\nTotal: %d entries\n" (List.length limited);
      0
  in
  let doc = "List all entries in the knowledge base." in
  let info = Cmd.info "list" ~doc in
  Cmd.v info Term.(const run $ logging_t $ config_file $ data_dir $ type_filter $ limit $ sort_by)

(** {1 Stats Command} *)

let stats_cmd =
  let run () config_file data_dir =
    match load_config config_file with
    | Error e -> Printf.eprintf "Config error: %s\n" e; 1
    | Ok config ->
      let data_dir = get_data_dir config data_dir in
      Eio_main.run @@ fun env ->
      let fs = Eio.Stdenv.fs env in
      let entries = Bushel_eio.Bushel_loader.load fs data_dir in
      let papers = List.length (Bushel.Entry.papers entries) in
      let notes = List.length (Bushel.Entry.notes entries) in
      let projects = List.length (Bushel.Entry.projects entries) in
      let ideas = List.length (Bushel.Entry.ideas entries) in
      let videos = List.length (Bushel.Entry.videos entries) in
      let contacts = List.length (Bushel.Entry.contacts entries) in
      let images = List.length (Bushel_eio.Bushel_loader.load_images fs
        ~output_dir:config.Bushel_config.local_output_dir) in
      Printf.printf "Bushel Statistics\n";
      Printf.printf "=================\n";
      Printf.printf "Papers:   %4d\n" papers;
      Printf.printf "Notes:    %4d\n" notes;
      Printf.printf "Projects: %4d\n" projects;
      Printf.printf "Ideas:    %4d\n" ideas;
      Printf.printf "Videos:   %4d\n" videos;
      Printf.printf "Contacts: %4d\n" contacts;
      Printf.printf "Images:   %4d\n" images;
      Printf.printf "-----------------\n";
      Printf.printf "Total:    %4d\n" (papers + notes + projects + ideas + videos);
      0
  in
  let doc = "Show statistics about the knowledge base." in
  let info = Cmd.info "stats" ~doc in
  Cmd.v info Term.(const run $ logging_t $ config_file $ data_dir)

(** {1 Show Command} *)

let show_cmd =
  let slug_arg =
    let doc = "The slug of the entry to show." in
    Arg.(required & pos 0 (some string) None & info [] ~docv:"SLUG" ~doc)
  in
  let run () config_file data_dir slug =
    match load_config config_file with
    | Error e -> Printf.eprintf "Config error: %s\n" e; 1
    | Ok config ->
      let data_dir = get_data_dir config data_dir in
      with_entries data_dir @@ fun _env entries ->
      match Bushel.Entry.lookup entries slug with
      | None ->
        Printf.eprintf "Entry not found: %s\n" slug;
        1
      | Some entry ->
        Printf.printf "Type:  %s\n" (type_string entry);
        Printf.printf "Slug:  %s\n" (Bushel.Entry.slug entry);
        Printf.printf "Title: %s\n" (Bushel.Entry.title entry);
        Printf.printf "Date:  %s\n" (format_date (Bushel.Entry.date entry));
        Printf.printf "URL:   %s\n" (Bushel.Entry.site_url entry);
        (match Bushel.Entry.thumbnail_slug entries entry with
         | Some s -> Printf.printf "Thumbnail: %s\n" s
         | None -> Printf.printf "Thumbnail: -\n");
        (match Bushel.Entry.synopsis entry with
         | Some s -> Printf.printf "Synopsis: %s\n" s
         | None -> ());
        Printf.printf "\n--- Body ---\n%s\n" (Bushel.Entry.body entry);
        0
  in
  let doc = "Show details of a specific entry." in
  let info = Cmd.info "show" ~doc in
  Cmd.v info Term.(const run $ logging_t $ config_file $ data_dir $ slug_arg)

(** {1 Render Command} *)

let render_cmd =
  let slug_arg =
    let doc = "The slug of the entry to render." in
    Arg.(required & pos 0 (some string) None & info [] ~docv:"SLUG" ~doc)
  in
  let base_url =
    let doc = "Base URL prefix for entry links (default: empty)." in
    Arg.(value & opt string "" & info ["base-url"] ~docv:"URL" ~doc)
  in
  let image_base =
    let doc = "Base path for images (default: /images)." in
    Arg.(value & opt string "/images" & info ["image-base"] ~docv:"PATH" ~doc)
  in
  let run () config_file data_dir slug base_url image_base =
    match load_config config_file with
    | Error e -> Printf.eprintf "Config error: %s\n" e; 1
    | Ok config ->
      let data_dir = get_data_dir config data_dir in
      let image_output_dir = config.Bushel_config.local_output_dir in
      with_entries ~image_output_dir data_dir @@ fun _env entries ->
      match Bushel.Entry.lookup entries slug with
      | None ->
        Printf.eprintf "Entry not found: %s\n" slug;
        1
      | Some entry ->
        let body = Bushel.Entry.body entry in
        let rendered = Bushel.Md.to_markdown ~base_url ~image_base ~entries body in
        print_string rendered;
        0
  in
  let doc = "Render an entry's markdown with resolved Bushel links." in
  let man = [
    `S Manpage.s_description;
    `P "Converts Bushel-flavored markdown to standard markdown by resolving:";
    `P "- $(b,:slug) links to [Title](URL)";
    `P "- $(b,@@handle) to [Name](contact_url)";
    `P "- $(b,##tag) to [tag](/tags/tag)";
    `P "- $(b,![:image-slug]) to ![caption](/images/slug.webp)";
  ] in
  let info = Cmd.info "render" ~doc ~man in
  Cmd.v info Term.(const run $ logging_t $ config_file $ data_dir $ slug_arg $ base_url $ image_base)

(** {1 Sync Command} *)

let sync_cmd =
  let remote =
    let doc = "Also upload to Typesense (remote sync)." in
    Arg.(value & flag & info ["remote"] ~doc)
  in
  let dry_run =
    let doc = "Show what commands would be run without executing them." in
    Arg.(value & flag & info ["dry-run"; "n"] ~doc)
  in
  let only =
    let doc = "Only run specific step (images, srcsetter, thumbs, faces, videos, typesense)." in
    Arg.(value & opt (some string) None & info ["only"] ~docv:"STEP" ~doc)
  in
  let run () config_file data_dir remote dry_run only =
    match load_config config_file with
    | Error e -> Printf.eprintf "Config error: %s\n" e; 1
    | Ok config ->
      let data_dir = get_data_dir config data_dir in
      (* Determine which steps to run *)
      let steps = match only with
        | Some step_name ->
          (match Bushel_sync.step_of_string step_name with
           | Some step -> [step]
           | None ->
             Printf.eprintf "Unknown step: %s\n" step_name;
             Printf.eprintf "Valid steps: images, srcsetter, thumbs, faces, videos, typesense\n";
             exit 1)
        | None ->
          if remote then Bushel_sync.all_steps_with_remote
          else Bushel_sync.all_steps
      in

      Eio_main.run @@ fun env ->
      Eio.Switch.run @@ fun sw ->
      let fs = Eio.Stdenv.fs env in
      let entries = Bushel_eio.Bushel_loader.load fs data_dir in

      Printf.printf "%s sync pipeline...\n" (if dry_run then "Dry-run" else "Running");
      List.iter (fun step ->
        Printf.printf "  - %s\n" (Bushel_sync.string_of_step step)
      ) steps;
      Printf.printf "\n";

      let results = Bushel_sync.run ~dry_run ~sw ~env ~config ~steps ~entries in

      Printf.printf "\nResults:\n";
      List.iter (fun r ->
        let status = if r.Bushel_sync.success then "OK" else "FAIL" in
        Printf.printf "  [%s] %s: %s\n"
          status
          (Bushel_sync.string_of_step r.step)
          r.message;
        (* In dry-run mode, show the details (commands) *)
        if dry_run && r.Bushel_sync.details <> [] then begin
          List.iter (fun d -> Printf.printf "      %s\n" d) r.Bushel_sync.details
        end
      ) results;

      let failures = List.filter (fun r -> not r.Bushel_sync.success) results in
      if failures = [] then 0 else 1
  in
  let doc = "Sync images, thumbnails, and optionally upload to Typesense." in
  let man = [
    `S Manpage.s_description;
    `P "The sync command runs a pipeline to synchronize images and thumbnails:";
    `P "1. $(b,images) - Rsync images from remote server";
    `P "2. $(b,thumbs) - Generate paper thumbnails from PDFs";
    `P "3. $(b,faces) - Fetch contact face thumbnails from Sortal";
    `P "4. $(b,videos) - Fetch video thumbnails from PeerTube";
    `P "5. $(b,srcsetter) - Convert all images to WebP srcset variants";
    `P "6. $(b,typesense) - Upload to Typesense (with --remote)";
    `P "Use $(b,--dry-run) to see what commands would be run without executing them.";
  ] in
  let info = Cmd.info "sync" ~doc ~man in
  Cmd.v info Term.(const run $ logging_t $ config_file $ data_dir $ remote $ dry_run $ only)

(** {1 Paper Add Command} *)

let paper_add_cmd =
  let doi_arg =
    let doc = "The DOI to resolve." in
    Arg.(required & pos 0 (some string) None & info [] ~docv:"DOI" ~doc)
  in
  let slug =
    let doc = "Slug for the paper (e.g., 2024-venue-name)." in
    Arg.(required & opt (some string) None & info ["slug"] ~docv:"SLUG" ~doc)
  in
  let version =
    let doc = "Paper version (e.g., v1, v2). Auto-increments if not specified." in
    Arg.(value & opt (some string) None & info ["ver"] ~docv:"VER" ~doc)
  in
  let run () config_file data_dir doi slug version =
    match load_config config_file with
    | Error e -> Printf.eprintf "Config error: %s\n" e; 1
    | Ok config ->
      let data_dir = get_data_dir config data_dir in

      Eio_main.run @@ fun env ->
      Eio.Switch.run @@ fun sw ->
      let fs = Eio.Stdenv.fs env in
      let http = Bushel_sync.Http.create ~sw env in
      let entries = Bushel_eio.Bushel_loader.load fs data_dir in

      (* Determine version *)
      let papers_dir = Filename.concat data_dir ("papers/" ^ slug) in
      let version = match version with
        | Some v -> v
        | None ->
          (* Auto-increment: find highest existing version *)
          if Sys.file_exists papers_dir then begin
            let files = Sys.readdir papers_dir |> Array.to_list in
            let versions = List.filter_map (fun f ->
              if Filename.check_suffix f ".md" then
                Some (Filename.chop_extension f)
              else None
            ) files in
            let max_ver = List.fold_left (fun acc v ->
              try
                let n = Scanf.sscanf v "v%d" Fun.id in
                max acc n
              with _ -> acc
            ) 0 versions in
            Printf.sprintf "v%d" (max_ver + 1)
          end else "v1"
      in

      Printf.printf "Resolving DOI: %s\n" doi;
      Printf.printf "Slug: %s, Version: %s\n" slug version;

      match Bushel_sync.Zotero.resolve ~http
              ~server_url:config.zotero_translation_server
              ~slug doi with
      | Error e ->
        Printf.eprintf "Error resolving DOI: %s\n" e;
        1
      | Ok metadata ->
        Printf.printf "Title: %s\n" metadata.title;
        Printf.printf "Authors: %s\n" (String.concat ", " metadata.authors);
        Printf.printf "Year: %d\n" metadata.year;

        (* Check for existing versions and merge *)
        let metadata =
          let existing_papers = Bushel.Entry.papers entries in
          match Bushel.Paper.lookup existing_papers slug with
          | Some existing ->
            Printf.printf "Merging with existing paper data...\n";
            Bushel_sync.Zotero.merge_with_existing ~existing metadata
          | None -> metadata
        in

        (* Generate file content *)
        let content = Bushel_sync.Zotero.to_yaml_frontmatter ~slug ~ver:version metadata in

        (* Create directory if needed *)
        if not (Sys.file_exists papers_dir) then
          Unix.mkdir papers_dir 0o755;

        (* Write file *)
        let filepath = Filename.concat papers_dir (version ^ ".md") in
        let oc = open_out filepath in
        output_string oc content;
        close_out oc;

        Printf.printf "Created: %s\n" filepath;
        0
  in
  let doc = "Add a paper from DOI, merging with existing versions." in
  let man = [
    `S Manpage.s_description;
    `P "Resolves a DOI using the Zotero Translation Server and creates a paper entry.";
    `P "If older versions of the paper exist, preserves abstract, tags, projects, \
        selected flag, and slides from the existing paper.";
  ] in
  let info = Cmd.info "paper" ~doc ~man in
  Cmd.v info Term.(const run $ logging_t $ config_file $ data_dir $ doi_arg $ slug $ version)

(** {1 Video Fetch Command} *)

(** Helper to create a video markdown file *)
let create_video_file ~videos_dir ~index ~server (video : Bushel_sync.Peertube.video) =
  let video_path = Filename.concat videos_dir (video.uuid ^ ".md") in
  if Sys.file_exists video_path then begin
    Printf.printf "  Skipping %s (exists)\n" video.uuid;
    false
  end else begin
    Printf.printf "  Creating %s: %s\n" video.uuid video.name;
    let content = Printf.sprintf {|---
title: %s
published_date: %s
uuid: %s
url: %s
talk: false
tags: []
---

%s
|}
      video.name
      (Ptime.to_rfc3339 video.published_at)
      video.uuid
      video.url
      (Option.value ~default:"" video.description)
    in
    let oc = open_out video_path in
    output_string oc content;
    close_out oc;
    Bushel_sync.Peertube.VideoIndex.add index ~uuid:video.uuid ~server;
    true
  end

let video_fetch_cmd =
  let url_arg =
    let doc = "PeerTube video URL to fetch (e.g., https://example.com/w/UUID)." in
    Arg.(value & pos 0 (some string) None & info [] ~docv:"URL" ~doc)
  in
  let server =
    let doc = "PeerTube server name from config (for channel mode)." in
    Arg.(value & opt (some string) None & info ["server"; "s"] ~docv:"NAME" ~doc)
  in
  let channel =
    let doc = "Channel name to fetch videos from (for channel mode)." in
    Arg.(value & opt (some string) None & info ["channel"] ~docv:"CHANNEL" ~doc)
  in
  let run () config_file data_dir url_arg server channel =
    match load_config config_file with
    | Error e -> Printf.eprintf "Config error: %s\n" e; 1
    | Ok config ->
      let data_dir = get_data_dir config data_dir in
      let index_path = Filename.concat data_dir "videos.yml" in
      let index = Bushel_sync.Peertube.VideoIndex.load_file index_path in
      let videos_dir = Filename.concat data_dir "videos" in
      if not (Sys.file_exists videos_dir) then
        Unix.mkdir videos_dir 0o755;

      match url_arg, server, channel with
      (* Single video mode: fetch by URL *)
      | Some url, _, _ ->
        (match Bushel_sync.Peertube.find_server_for_url config.peertube_servers url with
         | None ->
           Printf.eprintf "No configured server matches URL: %s\n" url;
           Printf.eprintf "Configured servers:\n";
           List.iter (fun (s : Bushel_config.peertube_server) ->
             Printf.eprintf "  - %s (%s)\n" s.name s.endpoint
           ) config.peertube_servers;
           1
         | Some matched_server ->
           match Bushel_sync.Peertube.uuid_of_url url with
           | None ->
             Printf.eprintf "Could not extract video UUID from URL: %s\n" url;
             1
           | Some uuid ->
             Printf.printf "Fetching video %s from %s...\n" uuid matched_server.name;
             Eio_main.run @@ fun env ->
             Eio.Switch.run @@ fun sw ->
             let http = Bushel_sync.Http.create ~sw env in
             match Bushel_sync.Peertube.fetch_video_details ~http
                     ~endpoint:matched_server.endpoint uuid with
             | Error e ->
               Printf.eprintf "Error fetching video: %s\n" e;
               1
             | Ok video ->
               let created = create_video_file ~videos_dir ~index
                   ~server:matched_server.name video in
               Bushel_sync.Peertube.VideoIndex.save_file index_path index;
               if created then
                 Printf.printf "\nCreated video entry: %s\n" video.name
               else
                 Printf.printf "\nVideo already exists: %s\n" video.name;
               0)

      (* Channel mode: fetch all videos from channel *)
      | None, Some server, Some channel ->
        let endpoint = List.find_map (fun (s : Bushel_config.peertube_server) ->
          if s.name = server then Some s.endpoint else None
        ) config.peertube_servers in
        (match endpoint with
         | None ->
           Printf.eprintf "Unknown server: %s\n" server;
           Printf.eprintf "Available servers:\n";
           List.iter (fun (s : Bushel_config.peertube_server) ->
             Printf.eprintf "  - %s (%s)\n" s.name s.endpoint
           ) config.peertube_servers;
           1
         | Some endpoint ->
           Eio_main.run @@ fun env ->
           Eio.Switch.run @@ fun sw ->
           let http = Bushel_sync.Http.create ~sw env in
           Printf.printf "Fetching videos from %s channel %s...\n" server channel;
           let videos = Bushel_sync.Peertube.fetch_all_channel_videos
             ~http ~endpoint ~channel () in
           Printf.printf "Found %d videos\n" (List.length videos);
           let new_count = List.fold_left (fun count video ->
             if create_video_file ~videos_dir ~index ~server video
             then count + 1 else count
           ) 0 videos in
           Bushel_sync.Peertube.VideoIndex.save_file index_path index;
           Printf.printf "\nCreated %d new video entries\n" new_count;
           Printf.printf "Updated index: %s\n" index_path;
           0)

      (* Missing arguments *)
      | None, None, _ | None, _, None ->
        Printf.eprintf "Usage: bushel video <URL>\n";
        Printf.eprintf "   or: bushel video --server NAME --channel CHANNEL\n";
        1
  in
  let doc = "Fetch videos from PeerTube." in
  let man = [
    `S Manpage.s_description;
    `P "Fetch video metadata from a PeerTube instance.";
    `P "Single video mode: $(b,bushel video <URL>)";
    `P "  Fetches a single video by URL. The server is auto-detected from config.";
    `P "Channel mode: $(b,bushel video --server NAME --channel CHANNEL)";
    `P "  Fetches all videos from a channel on the named server.";
  ] in
  let info = Cmd.info "video" ~doc ~man in
  Cmd.v info Term.(const run $ logging_t $ config_file $ data_dir $ url_arg $ server $ channel)

(** {1 Images Command} *)

let images_cmd =
  let limit =
    let doc = "Maximum number of images to show." in
    Arg.(value & opt (some int) None & info ["n"; "limit"] ~docv:"N" ~doc)
  in
  let sort_by =
    let doc = "Sort by field (slug, width, height, variants). Default: slug." in
    Arg.(value & opt string "slug" & info ["s"; "sort"] ~docv:"FIELD" ~doc)
  in
  let run () config_file limit sort_by =
    match load_config config_file with
    | Error e -> Printf.eprintf "Config error: %s\n" e; 1
    | Ok config ->
      Eio_main.run @@ fun env ->
      let fs = Eio.Stdenv.fs env in
      let output_dir = config.Bushel_config.local_output_dir in
      let images = Bushel_eio.Bushel_loader.load_images fs ~output_dir in
      if images = [] then begin
        Printf.printf "No images found.\n";
        Printf.printf "Run 'bushel sync' to generate image index.\n";
        0
      end else begin
        (* Sort *)
        let sorted = match sort_by with
          | "width" ->
            List.sort (fun a b ->
              let (wa, _) = Srcsetter.dims a in
              let (wb, _) = Srcsetter.dims b in
              compare wb wa  (* largest first *)
            ) images
          | "height" ->
            List.sort (fun a b ->
              let (_, ha) = Srcsetter.dims a in
              let (_, hb) = Srcsetter.dims b in
              compare hb ha  (* largest first *)
            ) images
          | "variants" ->
            List.sort (fun a b ->
              let va = Srcsetter.MS.cardinal (Srcsetter.variants a) in
              let vb = Srcsetter.MS.cardinal (Srcsetter.variants b) in
              compare vb va  (* most variants first *)
            ) images
          | _ -> (* slug, default *)
            List.sort (fun a b ->
              String.compare (Srcsetter.slug a) (Srcsetter.slug b)
            ) images
        in
        (* Limit *)
        let limited = match limit with
          | None -> sorted
          | Some n -> List.filteri (fun i _ -> i < n) sorted
        in
        (* Build table *)
        let rows = List.map (fun img ->
          let (w, h) = Srcsetter.dims img in
          let num_variants = Srcsetter.MS.cardinal (Srcsetter.variants img) in
          [ Srcsetter.slug img
          ; Printf.sprintf "%dx%d" w h
          ; string_of_int num_variants
          ; Srcsetter.origin img
          ]
        ) limited in
        let table = Table.make
          ~headers:["SLUG"; "DIMS"; "VARIANTS"; "ORIGIN"]
          rows
        in
        Table.print table;
        Printf.printf "\nTotal: %d images\n" (List.length limited);
        0
      end
  in
  let doc = "List images from the srcsetter index." in
  let man = [
    `S Manpage.s_description;
    `P "Lists images that have been processed by srcsetter.";
    `P "Images are stored separately from other entries and are referenced \
        by slug in markdown content using the :slug syntax.";
    `P "Run $(b,bushel sync) to process images and generate the index.";
  ] in
  let info = Cmd.info "images" ~doc ~man in
  Cmd.v info Term.(const run $ logging_t $ config_file $ limit $ sort_by)

(** {1 Config Command} *)

let config_cmd =
  let run () config_file =
    match load_config config_file with
    | Error e -> Printf.eprintf "Config error: %s\n" e; 1
    | Ok config ->
      Printf.printf "Config file: %s\n" (Bushel_config.config_file ());
      Printf.printf "\n";
      Fmt.pr "%a\n" Bushel_config.pp config;
      0
  in
  let doc = "Show current configuration." in
  let info = Cmd.info "config" ~doc in
  Cmd.v info Term.(const run $ logging_t $ config_file)

(** {1 Init Command} *)

let init_cmd =
  let force =
    let doc = "Overwrite existing config file." in
    Arg.(value & flag & info ["force"; "f"] ~doc)
  in
  let run () force =
    match Bushel_config.write_default_config ~force () with
    | Error e ->
      Printf.eprintf "%s\n" e;
      1
    | Ok path ->
      Printf.printf "Created config file: %s\n" path;
      Printf.printf "\nEdit this file to configure:\n";
      Printf.printf "  - Remote server for image sync\n";
      Printf.printf "  - Local data and image directories\n";
      Printf.printf "  - Immich endpoint and API key\n";
      Printf.printf "  - PeerTube servers\n";
      Printf.printf "  - Typesense and OpenAI API keys\n";
      Printf.printf "  - Zotero Translation Server URL\n";
      0
  in
  let doc = "Initialize a default configuration file." in
  let man = [
    `S Manpage.s_description;
    `P "Creates a default config.toml file at ~/.config/bushel/config.toml";
    `P "The generated file includes comments explaining each option.";
    `P "Use --force to overwrite an existing config file.";
  ] in
  let info = Cmd.info "init" ~doc ~man in
  Cmd.v info Term.(const run $ logging_t $ force)

(** {1 Git Sync Command} *)

let git_sync_cmd =
  let run () config_file data_dir dry_run remote_override =
    match load_config config_file with
    | Error e -> Printf.eprintf "Config error: %s\n" e; 1
    | Ok config ->
      let data_dir = get_data_dir config data_dir in

      (* Check if sync is configured *)
      let sync_config = match remote_override with
        | Some r -> { config.Bushel_config.sync with Gitops.Sync.Config.remote = r }
        | None -> config.Bushel_config.sync
      in

      if sync_config.Gitops.Sync.Config.remote = "" then begin
        Printf.eprintf "Error: No sync remote configured.\n";
        Printf.eprintf "Add to ~/.config/bushel/config.toml:\n";
        Printf.eprintf "  [sync]\n";
        Printf.eprintf "  remote = \"ssh://server/path/to/repo.git\"\n";
        Printf.eprintf "\nOr use --remote URL\n";
        1
      end else begin
        Eio_main.run @@ fun env ->
        let git = Gitops.v ~dry_run env in
        let repo = Eio.Path.(env#fs / data_dir) in

        Printf.printf "%s bushel data with %s\n"
          (if dry_run then "Would sync" else "Syncing")
          sync_config.Gitops.Sync.Config.remote;

        let result = Gitops.Sync.run git ~config:sync_config ~repo in

        if result.pulled then
          Printf.printf "Pulled changes from remote\n";
        if result.pushed then
          Printf.printf "Pushed changes to remote\n";
        if not result.pulled && not result.pushed then
          Printf.printf "Already in sync\n";
        0
      end
  in
  let doc = "Sync bushel data with remote git repository." in
  let man = [
    `S Manpage.s_description;
    `P "Synchronizes your bushel data directory with a remote git repository.";
    `P "Configure the remote in ~/.config/bushel/config.toml:";
    `Pre "  [sync]\n  remote = \"ssh://server/path/to/bushel.git\"";
    `P "The sync process:";
    `P "1. Fetches from the remote repository";
    `P "2. Merges any remote changes";
    `P "3. Commits local changes (if auto_commit is enabled)";
    `P "4. Pushes to the remote";
    `P "Use $(b,--dry-run) to preview what would happen.";
  ] in
  let info = Cmd.info "git-sync" ~doc ~man in
  Cmd.v info Term.(const run
    $ logging_t
    $ config_file
    $ data_dir
    $ Gitops.Sync.Cmd.dry_run_term
    $ Gitops.Sync.Cmd.remote_term)

(** {1 References Command} *)

let references_cmd =
  let slug_arg =
    Arg.(required & pos 0 (some string) None &
         info [] ~docv:"SLUG" ~doc:"Note slug to extract references from")
  in
  let format_arg =
    Arg.(value & opt string "text" &
         info ["f"; "format"] ~docv:"FORMAT" ~doc:"Output: text, json, yaml")
  in
  let author_arg =
    Arg.(value & opt string "avsm" &
         info ["a"; "author"] ~docv:"HANDLE" ~doc:"Default author handle")
  in
  let run () config_file data_dir slug format author_handle =
    match load_config config_file with
    | Error e -> Printf.eprintf "Config error: %s\n" e; 1
    | Ok config ->
      let data_dir = get_data_dir config data_dir in
      with_entries data_dir @@ fun _env entries ->
      match Bushel.Entry.lookup entries slug with
      | None -> Printf.eprintf "Entry not found: %s\n" slug; 1
      | Some (`Note note) ->
        let contacts = Bushel.Entry.contacts entries in
        (match List.find_opt (fun c ->
          Sortal_schema.Contact.handle c = author_handle
        ) contacts with
        | None -> Printf.eprintf "Author not found: %s\n" author_handle; 1
        | Some author ->
          let refs = Bushel.Reference.of_note ~entries ~default_author:author note in
          (match format with
           | "json" ->
             print_endline (Jsont_bytesrw.encode_string Bushel.Reference.list_jsont refs
                           |> Result.value ~default:"[]")
           | "yaml" -> print_string (Bushel.Reference.to_yaml_string refs)
           | _ ->
             if refs = [] then Printf.printf "No references for %s\n" slug
             else List.iteri (fun i r ->
               Printf.printf "%d. %s\n   DOI: https://doi.org/%s\n   Source: %s\n\n"
                 (i+1) r.Bushel.Reference.citation r.doi
                 (Bushel.Reference.source_to_string r.source)
             ) refs);
          0)
      | Some _ -> Printf.eprintf "%s is not a note\n" slug; 1
  in
  Cmd.v (Cmd.info "references" ~doc:"Extract references from a note")
    Term.(const run $ logging_t $ config_file $ data_dir $ slug_arg $ format_arg $ author_arg)

(** {1 Main Command Group} *)

let main_cmd =
  let doc = "Bushel knowledge base CLI" in
  let man = [
    `S Manpage.s_description;
    `P "Bushel is a CLI tool for managing and querying a knowledge base \
        containing papers, notes, projects, ideas, and videos.";
    `S Manpage.s_commands;
    `P "Use $(b,bushel COMMAND --help) for help on a specific command.";
    `S "CONFIGURATION";
    `P "Configuration is read from ~/.config/bushel/config.toml";
    `P "See $(b,bushel config) for current settings.";
  ] in
  let info = Cmd.info "bushel" ~version:"0.2.0" ~doc ~man in
  Cmd.group info [
    init_cmd;
    list_cmd;
    images_cmd;
    stats_cmd;
    show_cmd;
    render_cmd;
    references_cmd;
    sync_cmd;
    git_sync_cmd;
    paper_add_cmd;
    video_fetch_cmd;
    config_cmd;
  ]

let () =
  match Cmd.eval_value main_cmd with
  | Ok (`Ok exit_code) -> exit exit_code
  | Ok (`Help | `Version) -> exit 0
  | Error _ -> exit 1
