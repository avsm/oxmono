(*---------------------------------------------------------------------------
  Copyright (c) 2025 Anil Madhavapeddy <anil@recoil.org>. All rights reserved.
  SPDX-License-Identifier: ISC
 ---------------------------------------------------------------------------*)

(** Arod webserver - an httpz-based server for Bushel content *)

(** {1 Logging} *)

let src = Logs.Src.create "arod" ~doc:"Arod webserver"

module Log = (val Logs.src_log src : Logs.LOG)

(** {1 CLI} *)

open Cmdliner

let setup_logging style_renderer level =
  Fmt_tty.setup_std_outputs ?style_renderer ();
  Logs.set_level level;
  Logs.set_reporter (Logs_fmt.reporter ())

let logging_t =
  let open Cmdliner in
  Term.(const setup_logging $ Fmt_cli.style_renderer () $ Logs_cli.level ())

let config_file =
  let doc = "Path to config file (default: ~/.config/arod/config.toml)." in
  Arg.(value & opt (some file) None & info [ "c"; "config" ] ~docv:"FILE" ~doc)

let serve_cmd =
  let run () config_file =
    let cfg = Arod.Config.load_or_default ?path:config_file () in
    Log.info (fun m -> m "Starting Arod server...");
    Log.info (fun m -> m "Config:@.%a" Arod.Config.pp cfg);
    Eio_main.run @@ fun env ->
    let fs = Eio.Stdenv.fs env in
    let net = Eio.Stdenv.net env in
    Log.info (fun m -> m "Loading entries from %s" cfg.paths.data_dir);
    (* Create context (loads Bushel entries) *)
    let ctx = Arod.Ctx.create ~config:cfg fs in
    Log.info (fun m ->
        m "Loaded %d notes, %d papers, %d projects, %d ideas, %d videos, %d images, %d feed items"
          (List.length (Arod.Ctx.notes ctx))
          (List.length (Arod.Ctx.papers ctx))
          (List.length (Arod.Ctx.projects ctx))
          (List.length (Arod.Ctx.ideas ctx))
          (List.length (Arod.Ctx.videos ctx))
          (List.length (Arod.Ctx.images ctx))
          (List.length (Arod.Ctx.feed_items ctx)));
    (* Create cache with 5 minute TTL *)
    let cache = Arod.Cache.create ~ttl:300.0 in
    (* Run inside switch so search DB and log DB stay open for server lifetime *)
    Eio.Switch.run @@ fun sw ->
    (* Build in-memory search index on startup *)
    let search = Arod_search.create_memory ~sw () in
    Arod_search.rebuild search ctx;
    Log.info (fun m -> m "Search index built (%d entries)"
      (List.length (Arod.Ctx.all_entries ctx) + List.length (Arod.Ctx.all_links ctx)));
    (* Open access log database *)
    let xdg = Xdge.create fs "arod" in
    let log_path = Eio.Path.(Xdge.data_dir xdg / "access.db") in
    let log = Arod_log.create ~sw log_path in
    Log.info (fun m -> m "Access log: %a" Eio.Path.pp log_path);
    (* Get all routes with ctx, cache and search *)
    let routes = Arod_handlers.all_routes ~ctx ~cache ~search ~log ~fs in
    (* Start finger server alongside HTTP if configured *)
    (match cfg.server.finger_port with
     | Some finger_port ->
       Eio.Fiber.both
         (fun () -> Arod_server.run ~sw ~net ~config:cfg ~log routes)
         (fun () ->
           let handler = Arod_finger.handler ~ctx in
           Finger.Server.run ~sw ~net ~port:finger_port ~handler ())
     | None ->
       Arod_server.run ~sw ~net ~config:cfg ~log routes);
    0
  in
  let doc = "Start the Arod webserver." in
  let info = Cmd.info "serve" ~doc in
  Cmd.v info Term.(const run $ logging_t $ config_file)

let init_cmd =
  let run () =
    let path = Arod.Config.config_file () in
    let dir = Filename.dirname path in
    if not (Sys.file_exists dir) then Unix.mkdir dir 0o755;
    if Sys.file_exists path then begin
      Printf.eprintf "Config file already exists: %s\n" path;
      1
    end
    else begin
      let oc = open_out path in
      output_string oc Arod.Config.sample_config;
      close_out oc;
      Printf.printf "Created config file: %s\n" path;
      0
    end
  in
  let doc = "Initialize a default configuration file." in
  let info = Cmd.info "init" ~doc in
  Cmd.v info Term.(const run $ const ())

let config_cmd =
  let run () config_file =
    let cfg = Arod.Config.load_or_default ?path:config_file () in
    Fmt.pr "%a\n" Arod.Config.pp cfg;
    0
  in
  let doc = "Show current configuration." in
  let info = Cmd.info "config" ~doc in
  Cmd.v info Term.(const run $ logging_t $ config_file)

let search_cmd =
  let run () _config_file limit query_words =
    let input = String.concat " " query_words in
    if input = "" then begin
      Printf.eprintf "Usage: arod search [kind:TYPE] QUERY...\n\n";
      Printf.eprintf "Search syntax:\n";
      Printf.eprintf "  words            search for words in title, body, tags\n";
      Printf.eprintf "  \"exact phrase\"   match exact phrase\n";
      Printf.eprintf "  prefix*          prefix matching\n";
      Printf.eprintf "  kind:TYPE        restrict to type (%s)\n"
        (String.concat ", " Arod_search.kinds);
      Printf.eprintf "\nExamples:\n";
      Printf.eprintf "  arod search ocaml runtime\n";
      Printf.eprintf "  arod search kind:paper \"memory safety\"\n";
      Printf.eprintf "  arod search kind:note unikernel*\n";
      1
    end else begin
      Eio_main.run @@ fun env ->
      let fs = Eio.Stdenv.fs env in
      Eio.Switch.run @@ fun sw ->
      let xdg = Xdge.create fs "arod" in
      let db_path = Eio.Path.(Xdge.cache_dir xdg / "search.db") in
      let search = Arod_search.open_readonly ~sw db_path in
      let results = Arod_search.search search ?limit input in
      if results = [] then begin
        Printf.printf "No results.\n";
        0
      end else begin
        List.iter (fun r ->
          Fmt.pr "%a@.@." Arod_search.pp_result r
        ) results;
        Printf.printf "(%d result%s)\n"
          (List.length results)
          (if List.length results = 1 then "" else "s");
        0
      end
    end
  in
  let limit =
    let doc = "Maximum number of results." in
    Arg.(value & opt (some int) None & info [ "n"; "limit" ] ~docv:"N" ~doc)
  in
  let query_words =
    Arg.(value & pos_all string [] & info [] ~docv:"QUERY")
  in
  let doc = "Search the full-text index." in
  let man = [
    `S Manpage.s_description;
    `P "Search across all indexed content (notes, papers, projects, \
        ideas, videos, links) using the FTS5 full-text search engine.";
    `P "Use $(b,kind:TYPE) to restrict results to a specific type. \
        Valid types are: paper, note, project, idea, video, link.";
    `S Manpage.s_examples;
    `P "Search for OCaml-related content:";
    `Pre "  arod search ocaml";
    `P "Search only papers:";
    `Pre "  arod search kind:paper garbage collection";
    `P "Prefix matching:";
    `Pre "  arod search unikernel*";
  ] in
  let info = Cmd.info "search" ~doc ~man in
  Cmd.v info Term.(const run $ logging_t $ config_file $ limit $ query_words)

let index_cmd =
  let run () config_file =
    let cfg = Arod.Config.load_or_default ?path:config_file () in
    Eio_main.run @@ fun env ->
    let fs = Eio.Stdenv.fs env in
    let ctx = Arod.Ctx.create ~config:cfg fs in
    Eio.Switch.run @@ fun sw ->
    let xdg = Xdge.create fs "arod" in
    let db_path = Eio.Path.(Xdge.cache_dir xdg / "search.db") in
    let search = Arod_search.create ~sw db_path in
    Arod_search.rebuild search ctx;
    Log.info (fun m -> m "Search index built at %a" Eio.Path.pp db_path);
    0
  in
  let doc = "Build the full-text search index." in
  let info = Cmd.info "index" ~doc in
  Cmd.v info Term.(const run $ logging_t $ config_file)

let annotate_cmd =
  let run () config_file entry_url slug =
    let cfg = Arod.Config.load_or_default ?path:config_file () in
    (* Validate bushel slug exists *)
    Eio_main.run @@ fun env ->
    let fs = Eio.Stdenv.fs env in
    let ctx = Arod.Ctx.create ~config:cfg fs in
    (match Arod.Ctx.lookup ctx slug with
     | None ->
       Printf.eprintf "Error: Bushel slug '%s' not found.\n" slug;
       1
     | Some _ ->
       (* Find the feed entry URL across all contacts/feeds *)
       let xdg = Xdge.create fs "sortal" in
       let feed_store = Sortal_feed.Store.create_from_xdg xdg in
       let contacts = Arod.Ctx.contacts ctx in
       let normalize_url s =
         let s = if String.length s > 0 && s.[String.length s - 1] = '/' then
           String.sub s 0 (String.length s - 1)
         else s in
         (* Normalize www. prefix: strip it for comparison *)
         let s = match String.split_on_char '/' s with
           | proto :: "" :: host :: rest when String.length host > 4
             && String.sub host 0 4 = "www." ->
             String.concat "/" (proto :: "" :: String.sub host 4 (String.length host - 4) :: rest)
           | _ -> s
         in
         String.lowercase_ascii s
       in
       let norm_entry_url = normalize_url entry_url in
       let found = ref false in
       List.iter (fun contact ->
         if not !found then
           let handle = Sortal_schema.Contact.handle contact in
           match Sortal_schema.Contact.feeds contact with
           | Some feeds when feeds <> [] ->
             List.iter (fun feed ->
               if not !found then
                 let entries = Sortal_feed.Store.entries_of_feed feed_store ~handle feed in
                 List.iter (fun (fe : Sortal_feed.Entry.t) ->
                   if not !found then
                     match fe.url with
                     | Some u when normalize_url (Uri.to_string u) = norm_entry_url ->
                       let feed_url = Uri.to_string u in
                       let ann_path = Sortal_feed.Store.annotations_file feed_store handle feed in
                       let ann = Sortal_feed.Annotations.load ann_path in
                       Sortal_feed.Annotations.add_slug ann ~url:feed_url ~slug;
                       Sortal_feed.Annotations.save ann_path ann;
                       Printf.printf "Associated %s with %s (contact: %s)\n"
                         feed_url slug (Sortal_schema.Contact.name contact);
                       found := true
                     | _ -> ()
                 ) entries
             ) feeds
           | _ -> ()
       ) contacts;
       if not !found then begin
         Printf.eprintf "Error: URL '%s' not found in any feed.\n" entry_url;
         1
       end else
         0)
  in
  let entry_url =
    let doc = "URL of the feed entry to annotate." in
    Arg.(required & pos 0 (some string) None & info [] ~docv:"URL" ~doc)
  in
  let slug =
    let doc = "Bushel slug to associate with the feed entry." in
    Arg.(required & pos 1 (some string) None & info [] ~docv:"SLUG" ~doc)
  in
  let doc = "Associate a feed entry with a bushel slug." in
  let man = [
    `S Manpage.s_description;
    `P "Creates a persistent annotation linking a feed entry URL to a \
        bushel slug. The annotation survives feed syncs and causes the \
        entry to show a mention chip on the network page.";
    `S Manpage.s_examples;
    `P "Associate a blog post with a paper:";
    `Pre "  arod annotate https://example.com/post my-paper-slug";
  ] in
  let info = Cmd.info "annotate" ~doc ~man in
  Cmd.v info Term.(const run $ logging_t $ config_file $ entry_url $ slug)

(** {1 StandardSite Publishing} *)

module Document = Atp_lexicon_standard_site.Site.Standard.Document
module Publication = Atp_lexicon_standard_site.Site.Standard.Publication

let now_rfc3339 () = Ptime.to_rfc3339 ~tz_offset_s:0 ~frac_s:3 (Ptime_clock.now ())

let date_to_rfc3339 (y, m, d) =
  match Ptime.of_date (y, m, d) with
  | Some t -> Ptime.to_rfc3339 ~tz_offset_s:0 ~frac_s:3 t
  | None -> Printf.sprintf "%04d-%02d-%02dT00:00:00.000Z" y m d

let with_api env f =
  Eio.Switch.run @@ fun sw ->
  let fs = env#fs in
  match Xrpc_auth.Session.load fs ~app_name:"standard-site" () with
  | None ->
    Printf.eprintf "Not logged in. Run 'standard-site auth login' first.\n";
    exit 1
  | Some session ->
    let api =
      Standard_site.Api.create ~sw ~env ~app_name:"standard-site"
        ~pds:session.pds ()
    in
    Standard_site.Api.resume api ~session;
    f api

let resolve_site_uri ~did site =
  if String.starts_with ~prefix:"at://" site then site
  else Printf.sprintf "at://%s/site.standard.publication/%s" did site

(** Find the publication whose URL matches the config's base_url. *)
let auto_detect_site api ~base_url =
  let did = Standard_site.Api.get_did api in
  let pubs = Standard_site.Api.list_publications api ~did () in
  (* Normalize URL for comparison: strip trailing slash, lowercase *)
  let norm s =
    let s = String.lowercase_ascii s in
    if String.length s > 0 && s.[String.length s - 1] = '/' then
      String.sub s 0 (String.length s - 1)
    else s
  in
  let target = norm base_url in
  List.find_opt (fun (_rkey, (pub : Publication.main)) ->
    norm pub.url = target
  ) pubs
  |> Option.map (fun (rkey, _pub) ->
    Printf.sprintf "at://%s/site.standard.publication/%s" did rkey)

(** Find an existing document by path. *)
let find_doc_by_path api ~did ~path =
  let docs = Standard_site.Api.list_documents api ~did () in
  List.find_opt (fun (_rkey, (doc : Document.main)) ->
    doc.path = Some path
  ) docs


(** Publish a single note to StandardSite. Returns 0 on success, non-zero on error.
    If [bsky_post] is not provided, falls back to the note's social:bluesky field. *)
let publish_note ~ctx ~(cfg : Arod.Config.t) ~fs ~api ~did ~site ~dry_run ?bsky_post n =
  let ent = `Note n in
  let bsky_post = match bsky_post with
    | Some _ -> bsky_post
    | None ->
      match Bushel.Note.social n with
      | Some { bluesky = url :: _; _ } -> Some url
      | _ -> None
  in
  let slug = Bushel.Note.slug n in
  let title = Bushel.Entry.title ent in
  let path = Bushel.Entry.site_url ent in
  let published_at = date_to_rfc3339 n.Bushel.Note.date in
  let updated_at = Option.map date_to_rfc3339 n.Bushel.Note.updated in
  let tags =
    Arod.Ctx.tags_of_ent ctx ent
    |> List.filter_map (fun tag ->
      match tag with
      | `Text t -> Some t
      | `Year _ -> None
      | `Slug _ | `Contact _ | `Set _ -> None)
  in
  let description = Bushel.Entry.synopsis ent in
  let text_content = None in
  let thumb_slug =
    Bushel.Entry.thumbnail_slug (Arod.Ctx.entries ctx) ent
  in
  if dry_run then begin
    Printf.printf "Would publish to StandardSite:\n";
    Printf.printf "  Title: %s\n" title;
    Printf.printf "  Path: %s\n" path;
    Printf.printf "  Site: %s\n" site;
    (match description with
     | Some d -> Printf.printf "  Description: %s\n" d
     | None -> Printf.printf "  Description: (none)\n");
    (match tags with
     | [] -> Printf.printf "  Tags: (none)\n"
     | ts -> Printf.printf "  Tags: %s\n" (String.concat ", " ts));
    Printf.printf "  Published: %s\n" published_at;
    (match updated_at with
     | Some u -> Printf.printf "  Updated: %s\n" u
     | None -> ());
    (match text_content with
     | Some tc ->
       let preview = if String.length tc > 200 then
         String.sub tc 0 200 ^ "..."
       else tc in
       Printf.printf "  Text: %s\n" preview
     | None -> ());
    (match bsky_post with
     | Some url -> Printf.printf "  Bluesky: %s\n" url
     | None -> ());
    (match thumb_slug with
     | Some s -> Printf.printf "  Cover image: %s\n" s
     | None -> Printf.printf "  Cover image: (none)\n");
    0
  end else begin
    let bsky_post_ref = Option.map (Standard_site.Api.resolve_bsky_post api) bsky_post in
    let cover_image = match thumb_slug with
      | None -> None
      | Some slug ->
        match Arod.Ctx.lookup_image ctx slug with
        | None -> None
        | Some img ->
          (* Pick a variant <= 640px wide, or the base image if smaller *)
          let max_width = 640 in
          let base_w, _ = Srcsetter.dims img in
          let img_file =
            if base_w <= max_width then Srcsetter.name img
            else
              let variants = Srcsetter.MS.bindings img.Srcsetter.variants in
              let suitable = List.filter (fun (_f, (w, _h)) -> w <= max_width) variants in
              match List.sort (fun (_f1, (w1, _)) (_f2, (w2, _)) -> compare w2 w1) suitable with
              | (f, _) :: _ -> f
              | [] -> Srcsetter.name img
          in
          let path = Eio.Path.(fs / Filename.concat cfg.paths.images_dir img_file) in
          (try
            let blob = Eio.Path.load path in
            let ext = Filename.extension img_file |> String.lowercase_ascii in
            let content_type = match ext with
              | ".webp" -> "image/webp" | ".jpg" | ".jpeg" -> "image/jpeg"
              | ".png" -> "image/png" | _ -> "application/octet-stream"
            in
            Some (Standard_site.Api.upload_blob api ~blob ~content_type)
          with _ ->
            Printf.eprintf "Warning: Could not upload cover image for %s\n" slug;
            None)
    in
    let tags_opt = match tags with [] -> None | ts -> Some ts in
    let update_yaml ~did ~rkey =
      let at_uri = Printf.sprintf "at://%s/site.standard.document/%s" did rkey in
      match Bushel.Note.source_file n with
      | Some file_path ->
        (match Frontmatter_eio.of_file fs file_path with
        | Ok fm ->
          let fm = Frontmatter.set_field "standardsite" (`String at_uri) fm in
          Frontmatter_eio.save_file fs file_path fm;
          Printf.printf "Updated %s with standardsite field.\n" file_path
        | Error e ->
          Printf.eprintf "Warning: Could not read frontmatter in %s: %s\n" file_path e)
      | None ->
        Printf.eprintf "Warning: Could not find source file for slug '%s'\n" slug
    in
    match find_doc_by_path api ~did ~path with
    | Some (rkey, _existing) ->
      Standard_site.Api.update_document api ~rkey ~site ~title
        ~published_at ~path
        ?updated_at ?description ?text_content ?tags:tags_opt ?bsky_post_ref
        ?cover_image ();
      Printf.printf "Updated document: %s\n" title;
      Printf.printf "AT URI: at://%s/site.standard.document/%s\n" did rkey;
      if Bushel.Note.standardsite n = None then
        update_yaml ~did ~rkey;
      0
    | None ->
      let rkey =
        Standard_site.Api.create_document api ~site ~title
          ~published_at ~path
          ?description ?text_content ?tags:tags_opt ?bsky_post_ref
          ?cover_image ()
      in
      Printf.printf "Created document: %s\n" title;
      Printf.printf "AT URI: at://%s/site.standard.document/%s\n" did rkey;
      update_yaml ~did ~rkey;
      0
  end

let publish_cmd =
  let run () config_file slug site_opt dry_run bsky_post =
    let cfg = Arod.Config.load_or_default ?path:config_file () in
    Eio_main.run @@ fun env ->
    let fs = Eio.Stdenv.fs env in
    let ctx = Arod.Ctx.create ~config:cfg fs in
    match Arod.Ctx.lookup ctx slug with
    | None ->
      Printf.eprintf "Error: Bushel slug '%s' not found.\n" slug;
      1
    | Some (`Note n) ->
      with_api env @@ fun api ->
      let did = Standard_site.Api.get_did api in
      let site = match site_opt with
        | Some s -> resolve_site_uri ~did s
        | None ->
          match auto_detect_site api ~base_url:cfg.site.base_url with
          | Some uri -> uri
          | None ->
            Printf.eprintf "Error: No publication found matching base_url '%s'.\n" cfg.site.base_url;
            Printf.eprintf "Use --site to specify the publication rkey or AT-URI.\n";
            exit 1
      in
      publish_note ~ctx ~cfg ~fs ~api ~did ~site ~dry_run ?bsky_post n
    | Some _ ->
      Printf.eprintf "Error: '%s' is not a note. Only notes can be published.\n" slug;
      1
  in
  let slug =
    let doc = "Bushel note slug to publish." in
    Arg.(required & pos 0 (some string) None & info [] ~docv:"SLUG" ~doc)
  in
  let site_opt =
    let doc = "Publication rkey or AT-URI. Auto-detected from config base_url if omitted." in
    Arg.(value & opt (some string) None & info [ "s"; "site" ] ~docv:"SITE" ~doc)
  in
  let dry_run =
    let doc = "Preview what would be published without making API calls." in
    Arg.(value & flag & info [ "n"; "dry-run" ] ~doc)
  in
  let bsky_post =
    let doc = "Bluesky post URL to link. Defaults to the note's $(b,social:bluesky) field if set." in
    Arg.(value & opt (some string) None & info [ "bsky-post" ] ~docv:"URL" ~doc)
  in
  let doc = "Publish a Bushel note to StandardSite." in
  let man = [
    `S Manpage.s_description;
    `P "Publishes a Bushel note as a StandardSite document on the AT Protocol \
        network. Title, path, description, tags, and publication date are \
        automatically derived from the note metadata. If a document \
        already exists at the same path, it is automatically updated. \
        After creating a new document, the note's YAML frontmatter is \
        automatically updated with the $(b,standardsite) record URI.";
    `P "Only notes can be published (not papers, projects, ideas, or videos).";
    `P "Requires authentication via $(b,standard-site auth login).";
    `S Manpage.s_examples;
    `P "Preview what would be published:";
    `Pre "  arod publish --dry-run my-note-slug";
    `P "Publish a note:";
    `Pre "  arod publish my-note-slug";
    `P "Publish with a specific site and Bluesky link:";
    `Pre "  arod publish -s my-pub-rkey --bsky-post https://bsky.app/... my-note-slug";
    `P "Update an existing document (auto-detected by path):";
    `Pre "  arod publish my-note-slug";
  ] in
  let info = Cmd.info "publish" ~doc ~man in
  Cmd.v info Term.(const run $ logging_t $ config_file $ slug $ site_opt
                   $ dry_run $ bsky_post)

let publish_bulk_cmd =
  let run () config_file slug count dry_run =
    let cfg = Arod.Config.load_or_default ?path:config_file () in
    Eio_main.run @@ fun env ->
    let fs = Eio.Stdenv.fs env in
    let ctx = Arod.Ctx.create ~config:cfg fs in
    (* Get all notes sorted newest-first *)
    let all_notes =
      Arod.Ctx.get_entries ctx ~types:[`Note]
      |> List.filter_map (function `Note n -> Some n | _ -> None)
    in
    (* Find the starting slug's position *)
    let rec find_index i = function
      | [] -> None
      | n :: _ when Bushel.Note.slug n = slug -> Some i
      | _ :: rest -> find_index (i + 1) rest
    in
    match find_index 0 all_notes with
    | None ->
      Printf.eprintf "Error: Bushel slug '%s' not found.\n" slug;
      1
    | Some idx ->
      (* Take the N notes after this position (older notes, since list is newest-first) *)
      let start = idx + 1 in
      let to_publish =
        List.filteri (fun i _ -> i >= start && i < start + count) all_notes
      in
      if to_publish = [] then begin
        Printf.printf "No notes found before '%s' to publish.\n" slug;
        0
      end else begin
        Printf.printf "Publishing %d notes before '%s':\n\n" (List.length to_publish) slug;
        with_api env @@ fun api ->
        let did = Standard_site.Api.get_did api in
        let site = match auto_detect_site api ~base_url:cfg.site.base_url with
          | Some uri -> uri
          | None ->
            Printf.eprintf "Error: No publication found matching base_url '%s'.\n" cfg.site.base_url;
            exit 1
        in
        let total = List.length to_publish in
        let errors = ref 0 in
        List.iteri (fun i n ->
          if i > 0 && not dry_run then begin
            Printf.printf "\nWaiting 5s...\n%!";
            Unix.sleepf 5.0
          end;
          Printf.printf "[%d/%d] %s\n%!" (i + 1) total (Bushel.Note.slug n);
          let rc = publish_note ~ctx ~cfg ~fs ~api ~did ~site ~dry_run n in
          if rc <> 0 then incr errors
        ) to_publish;
        if !errors > 0 then begin
          Printf.eprintf "\n%d of %d notes had errors.\n" !errors total;
          1
        end else begin
          Printf.printf "\nAll %d notes published successfully.\n" total;
          0
        end
      end
  in
  let slug =
    let doc = "Starting note slug. Notes older than this one will be published." in
    Arg.(required & pos 0 (some string) None & info [] ~docv:"SLUG" ~doc)
  in
  let count =
    let doc = "Number of notes to publish (default: 50)." in
    Arg.(value & opt int 50 & info [ "count" ] ~docv:"N" ~doc)
  in
  let dry_run =
    let doc = "Preview what would be published without making API calls." in
    Arg.(value & flag & info [ "n"; "dry-run" ] ~doc)
  in
  let doc = "Bulk-publish notes to StandardSite." in
  let man = [
    `S Manpage.s_description;
    `P "Publishes multiple Bushel notes to StandardSite in chronological order, \
        starting from the note $(i,before) the given slug and working backwards. \
        Pauses 5 seconds between each publish to avoid overloading the network.";
    `P "If a note has a $(b,social:bluesky) field, it is automatically used as \
        the Bluesky post link.";
    `P "Requires authentication via $(b,standard-site auth login).";
    `S Manpage.s_examples;
    `P "Preview the 50 notes before a given slug:";
    `Pre "  arod publish-bulk --dry-run my-note-slug";
    `P "Publish the 10 notes before a given slug:";
    `Pre "  arod publish-bulk --count 10 my-note-slug";
  ] in
  let info = Cmd.info "publish-bulk" ~doc ~man in
  Cmd.v info Term.(const run $ logging_t $ config_file $ slug $ count $ dry_run)

let standardsite_cmd =
  let pp_document ppf (rkey, (d : Document.main)) =
    Fmt.pf ppf "@[<v>%s@,  Title: %s@,  Site: %s%a%a@,  Published: %s" rkey
      d.title d.site
      Fmt.(option (fmt "@,  Path: %s")) d.path
      Fmt.(option (fmt "@,  Description: %s")) d.description
      d.published_at;
    (match d.tags with
     | Some tags ->
       Fmt.pf ppf "@,  Tags: %a" Fmt.(list ~sep:(any ", ") string) tags
     | None -> ());
    (match d.bsky_post_ref with
     | Some (r : Atp_lexicon_standard_site.Com.Atproto.Repo.StrongRef.main) ->
       Fmt.pf ppf "@,  Bluesky: %s" r.uri
     | None -> ());
    Fmt.pf ppf "@]"
  in
  let pp_document_detail ppf (rkey, (d : Document.main)) =
    Fmt.pf ppf "@[<v>Document: %s@,@," rkey;
    Fmt.pf ppf "Title: %s@," d.title;
    Fmt.pf ppf "Site: %s@," d.site;
    Fmt.(option (fmt "Path: %s@,")) ppf d.path;
    Fmt.(option (fmt "Description: %s@,")) ppf d.description;
    Fmt.pf ppf "Published: %s@," d.published_at;
    Fmt.(option (fmt "Updated: %s@,")) ppf d.updated_at;
    (match d.tags with
     | Some tags ->
       Fmt.pf ppf "Tags: %a@," Fmt.(list ~sep:(any ", ") string) tags
     | None -> ());
    (match d.bsky_post_ref with
     | Some (r : Atp_lexicon_standard_site.Com.Atproto.Repo.StrongRef.main) ->
       Fmt.pf ppf "Bluesky: %s@," r.uri
     | None -> ());
    (match d.text_content with
     | Some tc ->
       let preview = if String.length tc > 500 then
         String.sub tc 0 500 ^ "..."
       else tc in
       Fmt.pf ppf "@,Text: %s@," preview
     | None -> ());
    Fmt.pf ppf "@]"
  in
  let list_cmd =
    let run () _config_file user =
      Eio_main.run @@ fun env ->
      with_api env @@ fun api ->
      let did = match user with
        | Some u ->
          if String.starts_with ~prefix:"did:" u then u
          else Standard_site.Api.resolve_handle api u
        | None -> Standard_site.Api.get_did api
      in
      let docs = Standard_site.Api.list_documents api ~did () in
      if docs = [] then begin
        Printf.printf "No documents found.\n";
        0
      end else begin
        Printf.printf "Documents:\n\n";
        List.iter (fun d -> Fmt.pr "%a@.@." pp_document d) docs;
        0
      end
    in
    let user =
      let doc = "User DID or handle to list documents for (default: logged-in user)." in
      Arg.(value & opt (some string) None & info [ "u"; "user" ] ~docv:"USER" ~doc)
    in
    let doc = "List StandardSite documents." in
    let info = Cmd.info "list" ~doc in
    Cmd.v info Term.(const run $ logging_t $ config_file $ user)
  in
  let show_cmd =
    let run () _config_file rkey user =
      Eio_main.run @@ fun env ->
      with_api env @@ fun api ->
      let did = match user with
        | Some u ->
          if String.starts_with ~prefix:"did:" u then u
          else Standard_site.Api.resolve_handle api u
        | None -> Standard_site.Api.get_did api
      in
      match Standard_site.Api.get_document api ~did ~rkey with
      | Some doc ->
        Fmt.pr "%a@." pp_document_detail (rkey, doc);
        0
      | None ->
        Printf.eprintf "Document not found: %s\n" rkey;
        1
    in
    let rkey =
      let doc = "Document record key." in
      Arg.(required & pos 0 (some string) None & info [] ~docv:"RKEY" ~doc)
    in
    let user =
      let doc = "User DID or handle (default: logged-in user)." in
      Arg.(value & opt (some string) None & info [ "u"; "user" ] ~docv:"USER" ~doc)
    in
    let doc = "Show a StandardSite document." in
    let info = Cmd.info "show" ~doc in
    Cmd.v info Term.(const run $ logging_t $ config_file $ rkey $ user)
  in
  let doc = "Manage StandardSite documents." in
  let info = Cmd.info "standardsite" ~doc in
  Cmd.group info [list_cmd; show_cmd]

let stats_cmd =
  let run () hours =
    let range = match hours with
      | Some h -> Arod_stats.Last_hours h
      | None -> Arod_stats.All
    in
    Eio_main.run @@ fun env ->
    let fs = Eio.Stdenv.fs env in
    Eio.Switch.run @@ fun sw ->
    let xdg = Xdge.create fs "arod" in
    let log_path = Eio.Path.(Xdge.data_dir xdg / "access.db") in
    Arod_stats.report ~sw log_path range;
    0
  in
  let hours =
    let doc = "Only show stats for the last $(docv) hours." in
    Arg.(value & opt (some int) None & info [ "hours" ] ~docv:"N" ~doc)
  in
  let doc = "Show access log statistics." in
  let man = [
    `S Manpage.s_description;
    `P "Analyzes the SQLite access log database and displays comprehensive \
        web statistics including latency percentiles, cache hit rates, \
        top paths, status code breakdowns, bandwidth, user agents, and more.";
    `S Manpage.s_examples;
    `P "Show all-time statistics:";
    `Pre "  arod stats";
    `P "Show statistics for the last 24 hours:";
    `Pre "  arod stats --hours 24";
  ] in
  let info = Cmd.info "stats" ~doc ~man in
  Cmd.v info Term.(const run $ logging_t $ hours)

let main_cmd =
  let doc = "Arod - a webserver for Bushel content" in
  let man =
    [
      `S Manpage.s_description;
      `P
        "Arod is an httpz-based webserver that serves Bushel content \
         (notes, papers, projects, ideas, videos) as a website.";
      `S "CONFIGURATION";
      `P "Configuration is read from ~/.config/arod/config.toml";
      `P "Run $(b,arod init) to create a default config file.";
    ]
  in
  let info = Cmd.info "arod" ~version:"0.1.0" ~doc ~man in
  Cmd.group info [ serve_cmd; init_cmd; config_cmd; index_cmd; search_cmd;
                   annotate_cmd; publish_cmd; publish_bulk_cmd;
                   standardsite_cmd; stats_cmd ]

let () =
  match Cmd.eval_value main_cmd with
  | Ok (`Ok exit_code) -> exit exit_code
  | Ok `Help | Ok `Version -> exit 0
  | Error _ -> exit 1
