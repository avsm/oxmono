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
        ~output_dir:config.Bushel_config.images_output_dir) in
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
        let pp = match entry with
          | `Note n -> Bushel.Note.pp Fmt.stdout n
          | `Paper p -> Bushel.Paper.pp Fmt.stdout p
          | `Video v -> Bushel.Video.pp Fmt.stdout v
          | `Idea i -> Bushel.Idea.pp Fmt.stdout i
          | `Project p -> Bushel.Project.pp Fmt.stdout p
        in
        ignore pp;
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
      let image_output_dir = config.Bushel_config.images_output_dir in
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

(** {1 Pull Command} *)

let pull_cmd =
  let dry_run =
    let doc = "Show what commands would be run without executing them." in
    Arg.(value & flag & info ["dry-run"; "n"] ~doc)
  in
  let retry_errors =
    let doc = "Retry DOI and academic URL resolutions that previously failed." in
    Arg.(value & flag & info ["retry-errors"] ~doc)
  in
  let only =
    let doc = "Only run specific step (git, images, srcsetter, thumbs, faces, videos, links)." in
    Arg.(value & opt (some string) None & info ["only"] ~docv:"STEP" ~doc)
  in
  let run () config_file data_dir dry_run retry_errors only =
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
             Printf.eprintf "Valid steps: git, images, srcsetter, thumbs, faces, videos, links\n";
             exit 1)
        | None -> Bushel_sync.all_steps
      in

      Eio_main.run @@ fun env ->
      Eio.Switch.run @@ fun sw ->
      let fs = Eio.Stdenv.fs env in
      let entries = Bushel_eio.Bushel_loader.load fs data_dir in

      Printf.printf "%s pull pipeline...\n" (if dry_run then "Dry-run" else "Running");
      List.iter (fun step ->
        Printf.printf "  - %s\n" (Bushel_sync.string_of_step step)
      ) steps;
      Printf.printf "\n";

      let results = Bushel_sync.run ~dry_run ~retry_errors ~sw ~env ~data_dir ~config ~steps ~entries in

      Printf.printf "\nResults:\n";
      List.iter (fun r ->
        let status = if r.Bushel_sync.success then "OK" else "FAIL" in
        Printf.printf "  [%s] %s: %s\n"
          status
          (Bushel_sync.string_of_step r.step)
          r.message;
        if dry_run && r.Bushel_sync.details <> [] then begin
          List.iter (fun d -> Printf.printf "      %s\n" d) r.Bushel_sync.details
        end
      ) results;

      let failures = List.filter (fun r -> not r.Bushel_sync.success) results in
      if failures = [] then 0 else 1
  in
  let doc = "Pull remote data and run local processing." in
  let man = [
    `S Manpage.s_description;
    `P "The pull command fetches remote data and runs local processing steps:";
    `P "1. $(b,git) - Pull from remote git repository";
    `P "2. $(b,images) - Pull images from remote git repository";
    `P "3. $(b,thumbs) - Generate paper thumbnails from PDFs";
    `P "4. $(b,faces) - Fetch contact face thumbnails from Sortal";
    `P "5. $(b,videos) - Fetch video thumbnails from PeerTube";
    `P "6. $(b,srcsetter) - Convert all images to WebP srcset variants";
    `P "7. $(b,links) - Sync links with Karakeep bookmark service";
    `P "8. $(b,dois) - Resolve DOIs from links.yml via Zotero Translation Server";
    `P "Use $(b,--dry-run) to see what commands would be run without executing them.";
    `P "Use $(b,--retry-errors) to retry DOI resolutions that previously failed \
        (e.g., due to Zotero server being unavailable).";
  ] in
  let info = Cmd.info "pull" ~doc ~man in
  Cmd.v info Term.(const run $ logging_t $ config_file $ data_dir $ dry_run $ retry_errors $ only)

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
let create_video_file ~videos_dir ~index ~server ~endpoint (video : Bushel_sync.Peertube.video) =
  let video_path = Filename.concat videos_dir (video.uuid ^ ".md") in
  if Sys.file_exists video_path then begin
    Printf.printf "  Skipping %s (exists)\n" video.uuid;
    false
  end else begin
    Printf.printf "  Creating %s: %s\n  -> %s\n" video.uuid video.name video_path;
    let url = if video.url <> "" then video.url
      else Printf.sprintf "%s/videos/watch/%s" endpoint video.uuid in
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
      url
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
                   ~server:matched_server.name ~endpoint:matched_server.endpoint video in
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
             if create_video_file ~videos_dir ~index ~server ~endpoint video
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
      let output_dir = config.Bushel_config.images_output_dir in
      let images = Bushel_eio.Bushel_loader.load_images fs ~output_dir in
      if images = [] then begin
        Printf.printf "No images found.\n";
        Printf.printf "Run 'bushel pull' to process images and generate the index.\n";
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
    `P "Run $(b,bushel pull) to process images and generate the index.";
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
      Printf.printf "  - Data and image directories\n";
      Printf.printf "  - Git sync remotes for data and images\n";
      Printf.printf "  - PeerTube servers\n";
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

(** {1 Push Command} *)

let push_cmd =
  let dry_run =
    let doc = "Show what commands would be run without executing them." in
    Arg.(value & flag & info ["dry-run"; "n"] ~doc)
  in
  let remote_override =
    let doc = "Override sync remote URL." in
    Arg.(value & opt (some string) None & info ["remote"] ~docv:"URL" ~doc)
  in
  let message =
    let doc = "Override commit message." in
    Arg.(value & opt (some string) None & info ["m"; "message"] ~docv:"MSG" ~doc)
  in
  let run () config_file data_dir dry_run remote_override message =
    match load_config config_file with
    | Error e -> Printf.eprintf "Config error: %s\n" e; 1
    | Ok config ->
      let data_dir = get_data_dir config data_dir in

      let sync_config = match remote_override with
        | Some r -> { config.Bushel_config.sync with Gitops.Sync.Config.remote = r }
        | None -> config.Bushel_config.sync
      in
      let images_sync_config = config.Bushel_config.images_sync in

      if sync_config.Gitops.Sync.Config.remote = "" &&
         images_sync_config.Gitops.Sync.Config.remote = "" then begin
        Printf.eprintf "Error: No sync remote configured.\n";
        Printf.eprintf "Add to ~/.config/bushel/config.toml:\n";
        Printf.eprintf "  [sync]\n";
        Printf.eprintf "  remote = \"ssh://server/path/to/repo.git\"\n";
        Printf.eprintf "\nOr use --remote URL\n";
        1
      end else begin
        Eio_main.run @@ fun env ->
        let git = Gitops.v ~dry_run env in

        (* Push data repo *)
        if sync_config.Gitops.Sync.Config.remote <> "" then begin
          let repo = Eio.Path.(env#fs / data_dir) in
          Printf.printf "%s data to %s\n"
            (if dry_run then "Would push" else "Pushing")
            sync_config.Gitops.Sync.Config.remote;
          let pushed = Gitops.Sync.push git ~config:sync_config ?msg:message ~repo () in
          if pushed then
            Printf.printf "  Pushed data changes to remote\n"
          else
            Printf.printf "  Data already up to date\n"
        end;

        (* Push images repo *)
        if images_sync_config.Gitops.Sync.Config.remote <> "" then begin
          let images_repo = Eio.Path.(env#fs / config.Bushel_config.images_dir) in
          Printf.printf "%s images to %s\n"
            (if dry_run then "Would push" else "Pushing")
            images_sync_config.Gitops.Sync.Config.remote;
          let pushed = Gitops.Sync.push git ~config:images_sync_config ?msg:message
            ~repo:images_repo () in
          if pushed then
            Printf.printf "  Pushed image changes to remote\n"
          else
            Printf.printf "  Images already up to date\n"
        end;
        0
      end
  in
  let doc = "Commit and push bushel data and images to remote git repositories." in
  let man = [
    `S Manpage.s_description;
    `P "Commits local changes and pushes to remote git repositories.";
    `P "Pushes both the data repository and the images repository (if configured).";
    `P "Configure remotes in ~/.config/bushel/config.toml:";
    `Pre "  [sync]\n  remote = \"ssh://server/path/to/bushel.git\"\n\n  \
          [images_sync]\n  remote = \"ssh://server/path/to/images.git\"";
    `P "Use $(b,--dry-run) to preview what would happen.";
    `P "Use $(b,-m MSG) to override the commit message.";
  ] in
  let info = Cmd.info "push" ~doc ~man in
  Cmd.v info Term.(const run $ logging_t $ config_file $ data_dir $ dry_run $ remote_override $ message)

(** {1 Status Command} *)

let print_repo_status git ~label ~path =
  if not (Gitops.is_repo git ~repo:path) then
    Printf.printf "%s: %s (not a git repository)\n\n" label (Eio.Path.native_exn path)
  else begin
    let branch = match Gitops.current_branch git ~repo:path with
      | Some b -> b
      | None -> "(detached)"
    in
    let porcelain = Gitops.status_porcelain git ~repo:path in
    let remote = Gitops.remote_url git ~repo:path ~remote:"origin" in
    let head = Gitops.rev_parse_opt git ~repo:path "HEAD" in

    Printf.printf "%s: %s\n" label (Eio.Path.native_exn path);
    Printf.printf "  Branch: %s" branch;
    (match remote with
     | Some url -> Printf.printf "  Remote: %s" url
     | None -> Printf.printf "  Remote: (none)");
    (match head with
     | Some h -> Printf.printf "  HEAD: %s\n" (String.sub h 0 (min 8 (String.length h)))
     | None -> Printf.printf "  HEAD: (no commits)\n");

    if porcelain = "" then
      Printf.printf "  Working tree clean\n"
    else begin
      Printf.printf "  Changes:\n";
      let lines = String.split_on_char '\n' porcelain in
      List.iter (fun line ->
        if line <> "" then Printf.printf "    %s\n" line
      ) lines
    end;
    Printf.printf "\n"
  end

let status_cmd =
  let run () config_file data_dir =
    match load_config config_file with
    | Error e -> Printf.eprintf "Config error: %s\n" e; 1
    | Ok config ->
      let data_dir = get_data_dir config data_dir in

      Eio_main.run @@ fun env ->
      let git = Gitops.v ~dry_run:false env in
      let data_repo = Eio.Path.(env#fs / data_dir) in
      let images_repo = Eio.Path.(env#fs / config.Bushel_config.images_dir) in

      print_repo_status git ~label:"Data" ~path:data_repo;
      print_repo_status git ~label:"Images" ~path:images_repo;
      0
  in
  let doc = "Show git status of bushel repositories." in
  let man = [
    `S Manpage.s_description;
    `P "Shows the git status of the bushel data and images directories, \
        including branch, remote, and any uncommitted changes.";
  ] in
  let info = Cmd.info "status" ~doc ~man in
  Cmd.v info Term.(const run $ logging_t $ config_file $ data_dir)

(** {1 Commit Command} *)

let commit_cmd =
  let message =
    let doc = "Commit message." in
    Arg.(value & opt (some string) None & info ["m"; "message"] ~docv:"MSG" ~doc)
  in
  let run () config_file data_dir message =
    match load_config config_file with
    | Error e -> Printf.eprintf "Config error: %s\n" e; 1
    | Ok config ->
      let data_dir = get_data_dir config data_dir in

      Eio_main.run @@ fun env ->
      let git = Gitops.v ~dry_run:false env in

      let commit_repo ~label ~path ~default_msg =
        if not (Gitops.is_repo git ~repo:path) then
          Printf.printf "%s: not a git repository, skipping\n" label
        else begin
          match Gitops.status git ~repo:path with
          | `Clean ->
            Printf.printf "%s: working tree clean\n" label
          | `Dirty ->
            let msg = match message with Some m -> m | None -> default_msg in
            Gitops.add_all git ~repo:path;
            Gitops.commit git ~repo:path ~msg;
            Printf.printf "%s: committed (%s)\n" label msg
        end
      in

      let data_repo = Eio.Path.(env#fs / data_dir) in
      let images_repo = Eio.Path.(env#fs / config.Bushel_config.images_dir) in
      commit_repo ~label:"Data" ~path:data_repo
        ~default_msg:config.Bushel_config.sync.Gitops.Sync.Config.commit_message;
      commit_repo ~label:"Images" ~path:images_repo
        ~default_msg:config.Bushel_config.images_sync.Gitops.Sync.Config.commit_message;
      0
  in
  let doc = "Commit outstanding changes in bushel repositories." in
  let man = [
    `S Manpage.s_description;
    `P "Stages all changes and commits them in both the data and images \
        git repositories.";
    `P "Use $(b,-m MSG) to specify a commit message. If not provided, uses \
        the commit_message from the respective config section.";
  ] in
  let info = Cmd.info "commit" ~doc ~man in
  Cmd.v info Term.(const run $ logging_t $ config_file $ data_dir $ message)

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

(** {1 Serve Command} *)

let serve_cmd =
  let port =
    let doc = "Port to listen on." in
    Arg.(value & opt int 8080 & info ["p"; "port"] ~docv:"PORT" ~doc)
  in
  let run () config_file data_dir port =
    match load_config config_file with
    | Error e -> Printf.eprintf "Config error: %s\n" e; 1
    | Ok config ->
      let data_dir = get_data_dir config data_dir in
      let image_output_dir = config.Bushel_config.images_output_dir in
      Eio_main.run @@ fun env ->
      let fs = Eio.Stdenv.fs env in
      let net = Eio.Stdenv.net env in
      let entries = Bushel_eio.Bushel_loader.load ~image_output_dir fs data_dir in
      let routes = Bushel_web.routes ~image_dir:image_output_dir entries in
      Eio.Switch.run @@ fun sw ->
      let addr = `Tcp (Eio.Net.Ipaddr.V4.any, port) in
      let socket = Eio.Net.listen net ~sw ~backlog:128 ~reuse_addr:true addr in
      Printf.printf "Bushel web UI at http://localhost:%d\n%!" port;
      let on_request (local_ info : Httpz_eio.request_info) =
        let meth_s = Httpz.Method.to_string info.meth in
        let len = String.length info.path in
        let dst = Bytes.create len in
        for i = 0 to len - 1 do Bytes.unsafe_set dst i (String.unsafe_get info.path i) done;
        let path_s = Bytes.unsafe_to_string dst in
        let status_s = Httpz.Res.status_to_string info.status in
        Logs.info (fun m -> m "%s %s -> %s" meth_s path_s status_s)
      in
      let on_error exn =
        Logs.err (fun m -> m "Connection error: %s" (Printexc.to_string exn))
      in
      Eio.Net.run_server socket ~on_error (fun flow addr ->
        Httpz_eio.handle_client ~routes ~on_request ~on_error flow addr)
  in
  let doc = "Browse the knowledge base in a web browser." in
  let man = [
    `S Manpage.s_description;
    `P "Starts an HTTP server to browse the knowledge base.";
    `P "Navigate to http://localhost:PORT to view notes, papers, projects, \
        ideas, and videos with crosslinks.";
  ] in
  Cmd.v (Cmd.info "serve" ~doc ~man)
    Term.(const run $ logging_t $ config_file $ data_dir $ port)

(** {1 Links Commands} *)

let links_list_cmd =
  let run () config_file data_dir =
    match load_config config_file with
    | Error e -> Printf.eprintf "Config error: %s\n" e; 1
    | Ok config ->
      let data_dir = get_data_dir config data_dir in
      let links_file = Filename.concat data_dir "links.yml" in
      let links = Bushel.Link.load_links_file links_file in
      if links = [] then begin
        Printf.printf "No links found in %s\n" links_file;
        0
      end else begin
        Printf.printf "%d links:\n\n" (List.length links);
        List.iter (fun (link : Bushel.Link.t) ->
          let (y, m, d) = Bushel.Link.date link in
          Printf.printf "  %04d-%02d-%02d  %s\n" y m d (Bushel.Link.url link);
          let desc = Bushel.Link.description link in
          if desc <> "" then Printf.printf "             %s\n" desc;
          (match link.karakeep with
           | Some kd -> Printf.printf "             karakeep: %s\n" kd.id
           | None -> ())
        ) links;
        0
      end
  in
  let doc = "List all tracked links." in
  let info = Cmd.info "list" ~doc in
  Cmd.v info Term.(const run $ logging_t $ config_file $ data_dir)

let links_add_cmd =
  let url_arg =
    let doc = "URL to add." in
    Arg.(required & pos 0 (some string) None & info [] ~docv:"URL" ~doc)
  in
  let description =
    let doc = "Description for the link." in
    Arg.(value & opt (some string) None & info ["d"; "description"] ~docv:"TEXT" ~doc)
  in
  let tags =
    let doc = "Tag to add (can be repeated)." in
    Arg.(value & opt_all string [] & info ["t"; "tag"] ~docv:"TAG" ~doc)
  in
  let run () config_file data_dir url description tags =
    match load_config config_file with
    | Error e -> Printf.eprintf "Config error: %s\n" e; 1
    | Ok config ->
      let data_dir = get_data_dir config data_dir in
      let links_file = Filename.concat data_dir "links.yml" in
      let existing = Bushel.Link.load_links_file links_file in
      let today =
        let t = Unix.gmtime (Unix.gettimeofday ()) in
        (t.tm_year + 1900, t.tm_mon + 1, t.tm_mday)
      in
      let new_link : Bushel.Link.t = {
        url;
        date = today;
        description = Option.value ~default:"" description;
        karakeep = None;
        bushel = (if tags = [] then None
                  else Some { slugs = []; tags });
      } in
      let merged = Bushel.Link.merge_links existing [new_link] in
      Bushel.Link.save_links_file links_file merged;
      Printf.printf "Added link: %s\n" url;
      0
  in
  let doc = "Add a new link to track." in
  let info = Cmd.info "add" ~doc in
  Cmd.v info Term.(const run $ logging_t $ config_file $ data_dir
    $ url_arg $ description $ tags)

let links_generate_cmd =
  let run () config_file data_dir =
    match load_config config_file with
    | Error e -> Printf.eprintf "Config error: %s\n" e; 1
    | Ok config ->
      let data_dir = get_data_dir config data_dir in
      with_entries data_dir @@ fun _env entries ->
      let external_links = Bushel.Link_graph.all_external_links () in
      if external_links = [] then begin
        Printf.printf "No external links found in entries.\n";
        0
      end else begin
        (* Deduplicate by URL, collecting source slugs *)
        let by_url : (string, Bushel.Link_graph.external_link list) Hashtbl.t =
          Hashtbl.create 256
        in
        let open Bushel.Link_graph in
        List.iter (fun (link : external_link) ->
          let cur = try Hashtbl.find by_url link.url with Not_found -> [] in
          if not (List.exists (fun (l : external_link) ->
            l.source = link.source) cur) then
            Hashtbl.replace by_url link.url (link :: cur)
        ) external_links;
        let urls = Hashtbl.fold (fun url links acc -> (url, links) :: acc) by_url [] in
        (* Build Link.t values from external links *)
        let new_links = List.map (fun (url, sources) ->
          (* Use the date from the first source entry *)
          let date = match sources with
            | link :: _ ->
              (match Bushel.Entry.lookup entries link.source with
               | Some entry -> Bushel.Entry.date entry
               | None -> let t = Unix.gmtime (Unix.gettimeofday ()) in
                 (t.tm_year + 1900, t.tm_mon + 1, t.tm_mday))
            | [] -> let t = Unix.gmtime (Unix.gettimeofday ()) in
              (t.tm_year + 1900, t.tm_mon + 1, t.tm_mday)
          in
          let slugs = List.map (fun (l : external_link) ->
            l.source) sources in
          let link : Bushel.Link.t = {
            url;
            date;
            description = "";
            karakeep = None;
            bushel = Some { slugs; tags = [] };
          } in
          link
        ) urls in
        (* Merge with existing links.yml *)
        let links_file = Filename.concat data_dir "links.yml" in
        let existing = Bushel.Link.load_links_file links_file in
        let merged = Bushel.Link.merge_links existing new_links in
        Bushel.Link.save_links_file links_file merged;
        Printf.printf "Generated %d links (%d new, %d total) in %s\n"
          (List.length new_links)
          (List.length merged - List.length existing)
          (List.length merged)
          links_file;
        0
      end
  in
  let doc = "Generate links.yml from external URLs in entry content." in
  let man = [
    `S Manpage.s_description;
    `P "Scans all bushel entries for external HTTP/HTTPS links and writes \
        them to links.yml. Merges with any existing links, preserving \
        karakeep data and bushel tags.";
    `P "Each link records which entry slugs reference it.";
    `P "After generating, run $(b,bushel pull --only links) to sync with Karakeep.";
  ] in
  let info = Cmd.info "generate" ~doc ~man in
  Cmd.v info Term.(const run $ logging_t $ config_file $ data_dir)

let links_cmd =
  let doc = "Link management commands." in
  let info = Cmd.info "links" ~doc in
  Cmd.group info [links_list_cmd; links_add_cmd; links_generate_cmd]

(** {1 Lint Command} *)

let yaml_keys_of_frontmatter fm =
  match Frontmatter.yaml fm with
  | `O fields -> List.map fst fields
  | _ -> []

let known_for_dir = function
  | "notes" | "news" | "weeklies" -> Some Bushel.Lint.note_fields
  | "papers" -> Some Bushel.Lint.paper_fields
  | "ideas" -> Some Bushel.Lint.idea_fields
  | "projects" -> Some Bushel.Lint.project_fields
  | "videos" -> Some Bushel.Lint.video_fields
  | _ -> None

let scan_flat_dir fs data_dir subdir known_fields acc =
  let dir = Filename.concat data_dir subdir in
  let path = Eio.Path.(fs / dir) in
  let files =
    try
      Eio.Path.read_dir path
      |> List.filter (fun f -> Filename.check_suffix f ".md")
    with _ -> []
  in
  List.fold_left (fun acc fname ->
    let file_path = Filename.concat dir fname in
    try
      let content = Eio.Path.load Eio.Path.(fs / file_path) in
      match Frontmatter.of_string ~fname:file_path content with
      | Ok fm ->
        let keys = yaml_keys_of_frontmatter fm in
        let slug = Filename.chop_extension fname in
        (slug, keys, known_fields) :: acc
      | Error _ -> acc
    with _ -> acc
  ) acc files

let scan_papers_dir fs data_dir acc =
  let papers_dir = Filename.concat data_dir "papers" in
  let path = Eio.Path.(fs / papers_dir) in
  let slug_dirs =
    try
      Eio.Path.read_dir path
      |> List.filter (fun slug ->
           try
             let stat = Eio.Path.stat ~follow:true Eio.Path.(fs / papers_dir / slug) in
             stat.kind = `Directory
           with _ -> false)
    with _ -> []
  in
  List.fold_left (fun acc slug ->
    let slug_path = Filename.concat papers_dir slug in
    let ver_files =
      try
        Eio.Path.(read_dir (fs / slug_path))
        |> List.filter (fun f -> Filename.check_suffix f ".md")
      with _ -> []
    in
    List.fold_left (fun acc ver_file ->
      let file_path = Filename.concat slug_path ver_file in
      try
        let content = Eio.Path.load Eio.Path.(fs / file_path) in
        match Frontmatter.of_string ~fname:file_path content with
        | Ok fm ->
          let keys = yaml_keys_of_frontmatter fm in
          (slug, keys, Bushel.Lint.paper_fields) :: acc
        | Error _ -> acc
      with _ -> acc
    ) acc ver_files
  ) acc slug_dirs

let scan_frontmatter_fields fs data_dir =
  let acc = [] in
  let flat_dirs = ["notes"; "news"; "weeklies"; "ideas"; "projects"; "videos"] in
  let acc = List.fold_left (fun acc subdir ->
    match known_for_dir subdir with
    | Some known -> scan_flat_dir fs data_dir subdir known acc
    | None -> acc
  ) acc flat_dirs in
  let acc = scan_papers_dir fs data_dir acc in
  Bushel.Lint.check_unknown_fields acc

let lint_cmd =
  let run () config_file data_dir =
    match load_config config_file with
    | Error e -> Printf.eprintf "Config error: %s\n" e; 1
    | Ok config ->
      let data_dir = get_data_dir config data_dir in
      with_entries data_dir @@ fun env entries ->
      let fs = Eio.Stdenv.fs env in
      (* Run entry-based checks *)
      let issues = Bushel.Lint.run entries in
      (* Scan disk for unknown field checks *)
      let field_issues = scan_frontmatter_fields fs data_dir in
      let all_issues = issues @ field_issues in
      if all_issues = [] then
        (Printf.printf "No issues found.\n"; 0)
      else begin
        let errors = List.filter (fun i ->
          i.Bushel.Lint.severity = Error) all_issues in
        let warnings = List.filter (fun i ->
          i.Bushel.Lint.severity = Warning) all_issues in
        List.iter (fun i ->
          Printf.printf "error: [%s] %s: %s\n"
            i.Bushel.Lint.category i.Bushel.Lint.slug i.Bushel.Lint.message
        ) errors;
        List.iter (fun i ->
          Printf.printf "warning: [%s] %s: %s\n"
            i.Bushel.Lint.category i.Bushel.Lint.slug i.Bushel.Lint.message
        ) warnings;
        Printf.printf "\n%d error(s), %d warning(s)\n"
          (List.length errors) (List.length warnings);
        if errors <> [] then 1 else 0
      end
  in
  let doc = "Lint the knowledge base for broken references and issues." in
  let info = Cmd.info "lint" ~doc in
  Cmd.v info Term.(const run $ logging_t $ config_file $ data_dir)

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
    lint_cmd;
    links_cmd;
    images_cmd;
    stats_cmd;
    show_cmd;
    render_cmd;
    references_cmd;
    serve_cmd;
    pull_cmd;
    push_cmd;
    status_cmd;
    commit_cmd;
    paper_add_cmd;
    video_fetch_cmd;
    config_cmd;
  ]

let () =
  match Cmd.eval_value main_cmd with
  | Ok (`Ok exit_code) -> exit exit_code
  | Ok (`Help | `Version) -> exit 0
  | Error _ -> exit 1
