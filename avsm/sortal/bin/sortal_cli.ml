(*---------------------------------------------------------------------------
  Copyright (c) 2025 Anil Madhavapeddy <anil@recoil.org>. All rights reserved.
  SPDX-License-Identifier: ISC
 ---------------------------------------------------------------------------*)

open Cmdliner

(* Main command *)
let () =
  Random.self_init ();
  Fmt.set_style_renderer Fmt.stdout `Ansi_tty;
  Fmt.set_style_renderer Fmt.stderr `Ansi_tty;

  let exit_code = Eio_main.run @@ fun env ->

  let xdg_term = Xdge.Cmd.term "sortal" env#fs ~dirs:[`Data] () in

  let info = Cmd.info "sortal"
    ~version:"0.1.0"
    ~doc:"Contact metadata management"
    ~man:[
      `S Manpage.s_description;
      `P "Sortal manages contact metadata including URLs, emails, ORCID identifiers, \
          and social media handles. Data is stored in XDG-compliant locations.";
      `S Manpage.s_commands;
      `P "Use $(b,sortal COMMAND --help) for detailed help on each command.";
    ]
  in

  let make_term info main_term =
    let term =
      let open Term.Syntax in
      let+ (xdg, _) = xdg_term
      and+ main = main_term
      and+ log_level = Logs_cli.level () in
      Logs.set_reporter (Logs_fmt.reporter ~app:Fmt.stdout ~dst:Fmt.stderr ());
      Logs.set_level log_level;
      main xdg
    in
    Cmd.v info term
  in

  let list_cmd = make_term Sortal.Cmd.list_info (Term.const Sortal.Cmd.list_cmd) in
  let show_cmd = make_term Sortal.Cmd.show_info Term.(const Sortal.Cmd.show_cmd $ Sortal.Cmd.handle_arg) in
  let thumbnail_cmd = make_term Sortal.Cmd.thumbnail_info Term.(const Sortal.Cmd.thumbnail_cmd $ Sortal.Cmd.handle_arg) in
  let search_cmd = make_term Sortal.Cmd.search_info Term.(const Sortal.Cmd.search_cmd $ Sortal.Cmd.query_arg) in
  let stats_cmd = make_term Sortal.Cmd.stats_info Term.(const (fun () -> Sortal.Cmd.stats_cmd ()) $ const ()) in
  let sync_cmd =
    let force_arg =
      Arg.(value & flag & info ["force"] ~doc:"Force re-fetch all thumbnails, overwriting existing ones")
    in
    let term =
      let open Term.Syntax in
      let+ (xdg, _) = xdg_term
      and+ log_level = Logs_cli.level ()
      and+ force = force_arg in
      Logs.set_reporter (Logs_fmt.reporter ~app:Fmt.stdout ~dst:Fmt.stderr ());
      Logs.set_level log_level;
      Sortal.Cmd.sync_cmd ~force () xdg env
    in
    Cmd.v Sortal.Cmd.sync_info term
  in

  (* Helper: load config, resolve remote, provide git+repo context *)
  let with_git_remote xdg env ~dry_run ~remote_override f =
    match Sortal_config.load () with
    | Error e -> Printf.eprintf "Config error: %s\n" e; 1
    | Ok config ->
      let data_dir = Xdge.data_dir xdg |> Eio.Path.native_exn in
      let sync_config = match remote_override with
        | Some r -> { config.Sortal_config.sync with Gitops.Sync.Config.remote = r }
        | None -> config.Sortal_config.sync
      in
      if sync_config.Gitops.Sync.Config.remote = "" then begin
        Printf.eprintf "Error: No sync remote configured.\n";
        Printf.eprintf "Add to ~/.config/sortal/config.toml:\n";
        Printf.eprintf "  [sync]\n";
        Printf.eprintf "  remote = \"ssh://server/path/to/repo.git\"\n";
        Printf.eprintf "\nOr use --remote URL\n"; 1
      end else
        let git = Gitops.v ~dry_run env in
        let repo = Eio.Path.(env#fs / data_dir) in
        f git repo sync_config
  in

  (* Git command group *)
  let git_init_sub =
    let term =
      let open Term.Syntax in
      let+ (xdg, _) = xdg_term
      and+ log_level = Logs_cli.level () in
      Logs.set_reporter (Logs_fmt.reporter ~app:Fmt.stdout ~dst:Fmt.stderr ());
      Logs.set_level log_level;
      Sortal.Cmd.git_init_cmd xdg env
    in
    let info = Cmd.info "init" ~doc:"Initialize git repository for contact versioning" in
    Cmd.v info term
  in

  let git_pull_sub =
    let term =
      let open Term.Syntax in
      let+ (xdg, _) = xdg_term
      and+ dry_run = Gitops.Sync.Cmd.dry_run_term
      and+ remote_override = Gitops.Sync.Cmd.remote_term
      and+ log_level = Logs_cli.level () in
      Logs.set_reporter (Logs_fmt.reporter ~app:Fmt.stdout ~dst:Fmt.stderr ());
      Logs.set_level log_level;
      with_git_remote xdg env ~dry_run ~remote_override (fun git repo _sync_config ->
        Gitops.fetch git ~repo ~remote:"origin";
        match Gitops.current_branch git ~repo with
        | None ->
          Printf.eprintf "Error: not on any branch (detached HEAD)\n"; 1
        | Some branch ->
          let local_head = Gitops.rev_parse_opt git ~repo "HEAD" in
          let remote_ref = "origin/" ^ branch in
          let remote_head = Gitops.rev_parse_opt git ~repo remote_ref in
          if local_head = remote_head then begin
            Printf.printf "Already up to date.\n"; 0
          end else begin
            Gitops.merge git ~repo ~ref_:remote_ref;
            Printf.printf "Merged changes from %s\n" remote_ref; 0
          end)
    in
    let doc = "Fetch and merge changes from remote." in
    let man = [
      `S Manpage.s_description;
      `P "Fetches from the remote and merges any new changes into the local branch.";
      `P "Use $(b,--dry-run) to preview what would happen.";
    ] in
    let info = Cmd.info "pull" ~doc ~man in
    Cmd.v info term
  in

  let git_commit_sub =
    let term =
      let open Term.Syntax in
      let+ (xdg, _) = xdg_term
      and+ msg = Arg.(value & opt string "sync" & info ["m"; "message"] ~docv:"MSG"
        ~doc:"Commit message.")
      and+ log_level = Logs_cli.level () in
      Logs.set_reporter (Logs_fmt.reporter ~app:Fmt.stdout ~dst:Fmt.stderr ());
      Logs.set_level log_level;
      let data_dir = Xdge.data_dir xdg |> Eio.Path.native_exn in
      let git = Gitops.v ~dry_run:false env in
      let repo = Eio.Path.(env#fs / data_dir) in
      match Gitops.status git ~repo with
      | `Clean ->
        Printf.printf "Nothing to commit, working tree clean\n"; 0
      | `Dirty ->
        Gitops.add_all git ~repo;
        Gitops.commit git ~repo ~msg;
        Printf.printf "Committed: %s\n" msg; 0
    in
    let doc = "Stage and commit all changes." in
    let man = [
      `S Manpage.s_description;
      `P "Stages all changes and creates a commit.";
      `P "Use $(b,-m MSG) to set the commit message (default: \"sync\").";
    ] in
    let info = Cmd.info "commit" ~doc ~man in
    Cmd.v info term
  in

  let git_push_sub =
    let term =
      let open Term.Syntax in
      let+ (xdg, _) = xdg_term
      and+ dry_run = Gitops.Sync.Cmd.dry_run_term
      and+ remote_override = Gitops.Sync.Cmd.remote_term
      and+ log_level = Logs_cli.level () in
      Logs.set_reporter (Logs_fmt.reporter ~app:Fmt.stdout ~dst:Fmt.stderr ());
      Logs.set_level log_level;
      with_git_remote xdg env ~dry_run ~remote_override (fun git repo _sync_config ->
        Gitops.push git ~repo ~remote:"origin";
        Printf.printf "Pushed to remote\n";
        0)
    in
    let doc = "Push commits to remote." in
    let man = [
      `S Manpage.s_description;
      `P "Pushes committed changes to the remote repository.";
      `P "Use $(b,--dry-run) to preview what would happen.";
    ] in
    let info = Cmd.info "push" ~doc ~man in
    Cmd.v info term
  in

  let git_status_sub =
    let term =
      let open Term.Syntax in
      let+ (xdg, _) = xdg_term
      and+ log_level = Logs_cli.level () in
      Logs.set_reporter (Logs_fmt.reporter ~app:Fmt.stdout ~dst:Fmt.stderr ());
      Logs.set_level log_level;
      let data_dir = Xdge.data_dir xdg |> Eio.Path.native_exn in
      let git = Gitops.v ~dry_run:false env in
      let repo = Eio.Path.(env#fs / data_dir) in
      if not (Gitops.is_repo git ~repo) then begin
        Printf.printf "Not a git repository. Run 'sortal git init' first.\n"; 1
      end else begin
        let branch = Gitops.current_branch git ~repo in
        let remote = Gitops.remote_url git ~repo ~remote:"origin" in
        let head = Gitops.rev_parse_opt git ~repo "HEAD" in
        let files = Gitops.status_files git ~repo in
        Printf.printf "Branch: %s\n"
          (match branch with Some b -> b | None -> "(detached)");
        (match remote with
         | Some url -> Printf.printf "Remote: %s\n" url
         | None -> Printf.printf "Remote: (none configured)\n");
        (match head with
         | Some h -> Printf.printf "HEAD:   %s\n" (String.sub h 0 (min 8 (String.length h)))
         | None -> Printf.printf "HEAD:   (no commits)\n");
        if files = [] then
          Printf.printf "\nClean working tree\n"
        else begin
          Printf.printf "\nChanged files:\n";
          List.iter (fun line -> Printf.printf "  %s\n" line) files
        end;
        0
      end
    in
    let info = Cmd.info "status" ~doc:"Show git repository status." in
    Cmd.v info term
  in

  let git_group =
    let info = Cmd.info "git" ~doc:"Git repository management"
      ~man:[
        `S Manpage.s_description;
        `P "Manage the git repository backing your sortal data.";
        `P "Use $(b,sortal git init) to initialize the repository.";
        `P "Use $(b,sortal git status) to view repository status.";
        `P "Use $(b,sortal git commit) to stage and commit changes.";
        `P "Use $(b,sortal git pull) to fetch and merge remote changes.";
        `P "Use $(b,sortal git push) to push commits to remote.";
      ]
    in
    Cmd.group info [git_init_sub; git_status_sub; git_commit_sub; git_pull_sub; git_push_sub]
  in

  (* Contact management commands - need special handling for env *)
  let add_cmd =
    let term =
      let open Term.Syntax in
      let+ (xdg, _) = xdg_term
      and+ handle = Sortal.Cmd.add_handle_arg
      and+ names = Sortal.Cmd.add_names_arg
      and+ kind = Sortal.Cmd.add_kind_arg
      and+ email = Sortal.Cmd.add_email_arg
      and+ github = Sortal.Cmd.add_github_arg
      and+ url = Sortal.Cmd.add_url_arg
      and+ orcid = Sortal.Cmd.add_orcid_arg
      and+ log_level = Logs_cli.level () in
      Logs.set_reporter (Logs_fmt.reporter ~app:Fmt.stdout ~dst:Fmt.stderr ());
      Logs.set_level log_level;
      Sortal.Cmd.add_cmd handle names kind email github url orcid xdg env
    in
    Cmd.v Sortal.Cmd.add_info term
  in

  let delete_cmd =
    let term =
      let open Term.Syntax in
      let+ (xdg, _) = xdg_term
      and+ handle = Sortal.Cmd.handle_arg
      and+ log_level = Logs_cli.level () in
      Logs.set_reporter (Logs_fmt.reporter ~app:Fmt.stdout ~dst:Fmt.stderr ());
      Logs.set_level log_level;
      Sortal.Cmd.delete_cmd handle xdg env
    in
    Cmd.v Sortal.Cmd.delete_info term
  in

  (* Entry management commands *)
  let add_email_cmd =
    let term =
      let open Term.Syntax in
      let+ (xdg, _) = xdg_term
      and+ handle = Sortal.Cmd.handle_arg
      and+ address = Sortal.Cmd.email_address_arg
      and+ type_ = Sortal.Cmd.email_type_arg
      and+ from = Sortal.Cmd.date_arg "from"
      and+ until = Sortal.Cmd.date_arg "until"
      and+ note = Sortal.Cmd.note_arg
      and+ log_level = Logs_cli.level () in
      Logs.set_reporter (Logs_fmt.reporter ~app:Fmt.stdout ~dst:Fmt.stderr ());
      Logs.set_level log_level;
      Sortal.Cmd.add_email_cmd handle address type_ from until note xdg env
    in
    Cmd.v Sortal.Cmd.add_email_info term
  in

  let remove_email_cmd =
    let term =
      let open Term.Syntax in
      let+ (xdg, _) = xdg_term
      and+ handle = Sortal.Cmd.handle_arg
      and+ address = Sortal.Cmd.email_address_arg
      and+ log_level = Logs_cli.level () in
      Logs.set_reporter (Logs_fmt.reporter ~app:Fmt.stdout ~dst:Fmt.stderr ());
      Logs.set_level log_level;
      Sortal.Cmd.remove_email_cmd handle address xdg env
    in
    Cmd.v Sortal.Cmd.remove_email_info term
  in

  let add_service_cmd =
    let term =
      let open Term.Syntax in
      let+ (xdg, _) = xdg_term
      and+ handle = Sortal.Cmd.handle_arg
      and+ url = Sortal.Cmd.service_url_arg
      and+ kind = Sortal.Cmd.service_kind_arg
      and+ service_handle = Sortal.Cmd.service_handle_arg
      and+ label = Sortal.Cmd.label_arg
      and+ log_level = Logs_cli.level () in
      Logs.set_reporter (Logs_fmt.reporter ~app:Fmt.stdout ~dst:Fmt.stderr ());
      Logs.set_level log_level;
      Sortal.Cmd.add_service_cmd handle url kind service_handle label xdg env
    in
    Cmd.v Sortal.Cmd.add_service_info term
  in

  let remove_service_cmd =
    let term =
      let open Term.Syntax in
      let+ (xdg, _) = xdg_term
      and+ handle = Sortal.Cmd.handle_arg
      and+ url = Sortal.Cmd.service_url_arg
      and+ log_level = Logs_cli.level () in
      Logs.set_reporter (Logs_fmt.reporter ~app:Fmt.stdout ~dst:Fmt.stderr ());
      Logs.set_level log_level;
      Sortal.Cmd.remove_service_cmd handle url xdg env
    in
    Cmd.v Sortal.Cmd.remove_service_info term
  in

  let add_org_cmd =
    let term =
      let open Term.Syntax in
      let+ (xdg, _) = xdg_term
      and+ handle = Sortal.Cmd.handle_arg
      and+ org_name = Sortal.Cmd.org_name_arg
      and+ title = Sortal.Cmd.org_title_arg
      and+ department = Sortal.Cmd.org_department_arg
      and+ from = Sortal.Cmd.date_arg "from"
      and+ until = Sortal.Cmd.date_arg "until"
      and+ org_email = Sortal.Cmd.org_email_arg
      and+ org_url = Sortal.Cmd.org_url_arg
      and+ log_level = Logs_cli.level () in
      Logs.set_reporter (Logs_fmt.reporter ~app:Fmt.stdout ~dst:Fmt.stderr ());
      Logs.set_level log_level;
      Sortal.Cmd.add_org_cmd handle org_name title department from until org_email org_url xdg env
    in
    Cmd.v Sortal.Cmd.add_org_info term
  in

  let remove_org_cmd =
    let term =
      let open Term.Syntax in
      let+ (xdg, _) = xdg_term
      and+ handle = Sortal.Cmd.handle_arg
      and+ org_name = Sortal.Cmd.org_name_arg
      and+ log_level = Logs_cli.level () in
      Logs.set_reporter (Logs_fmt.reporter ~app:Fmt.stdout ~dst:Fmt.stderr ());
      Logs.set_level log_level;
      Sortal.Cmd.remove_org_cmd handle org_name xdg env
    in
    Cmd.v Sortal.Cmd.remove_org_info term
  in

  let add_url_cmd =
    let term =
      let open Term.Syntax in
      let+ (xdg, _) = xdg_term
      and+ handle = Sortal.Cmd.handle_arg
      and+ url = Sortal.Cmd.url_value_arg
      and+ label = Sortal.Cmd.label_arg
      and+ log_level = Logs_cli.level () in
      Logs.set_reporter (Logs_fmt.reporter ~app:Fmt.stdout ~dst:Fmt.stderr ());
      Logs.set_level log_level;
      Sortal.Cmd.add_url_cmd handle url label xdg env
    in
    Cmd.v Sortal.Cmd.add_url_info term
  in

  let remove_url_cmd =
    let term =
      let open Term.Syntax in
      let+ (xdg, _) = xdg_term
      and+ handle = Sortal.Cmd.handle_arg
      and+ url = Sortal.Cmd.url_value_arg
      and+ log_level = Logs_cli.level () in
      Logs.set_reporter (Logs_fmt.reporter ~app:Fmt.stdout ~dst:Fmt.stderr ());
      Logs.set_level log_level;
      Sortal.Cmd.remove_url_cmd handle url xdg env
    in
    Cmd.v Sortal.Cmd.remove_url_info term
  in

  let add_atproto_cmd =
    let term =
      let open Term.Syntax in
      let+ (xdg, _) = xdg_term
      and+ handle = Sortal.Cmd.handle_arg
      and+ atproto_handle = Sortal.Cmd.atproto_handle_arg
      and+ svc_type = Sortal.Cmd.atproto_opt_service_type
      and+ svc_url = Sortal.Cmd.atproto_opt_service_url
      and+ log_level = Logs_cli.level () in
      Logs.set_reporter (Logs_fmt.reporter ~app:Fmt.stdout ~dst:Fmt.stderr ());
      Logs.set_level log_level;
      Sortal.Cmd.add_atproto_cmd handle atproto_handle svc_type svc_url xdg env
    in
    Cmd.v Sortal.Cmd.add_atproto_info term
  in

  let add_atproto_service_cmd =
    let term =
      let open Term.Syntax in
      let+ (xdg, _) = xdg_term
      and+ handle = Sortal.Cmd.handle_arg
      and+ svc_type = Sortal.Cmd.atproto_svc_type_arg
      and+ svc_url = Sortal.Cmd.atproto_svc_url_arg
      and+ log_level = Logs_cli.level () in
      Logs.set_reporter (Logs_fmt.reporter ~app:Fmt.stdout ~dst:Fmt.stderr ());
      Logs.set_level log_level;
      Sortal.Cmd.add_atproto_service_cmd handle svc_type svc_url xdg env
    in
    Cmd.v Sortal.Cmd.add_atproto_service_info term
  in

  (* Config command *)
  let config_cmd =
    let term =
      let open Term.Syntax in
      let+ _ = xdg_term
      and+ log_level = Logs_cli.level () in
      Logs.set_reporter (Logs_fmt.reporter ~app:Fmt.stdout ~dst:Fmt.stderr ());
      Logs.set_level log_level;
      match Sortal_config.load () with
      | Error e -> Printf.eprintf "Config error: %s\n" e; 1
      | Ok config ->
        Printf.printf "Config file: %s\n" (Sortal_config.config_file ());
        Printf.printf "\n";
        Fmt.pr "%a\n" Sortal_config.pp config;
        0
    in
    let info = Cmd.info "config" ~doc:"Show current configuration." in
    Cmd.v info term
  in

  (* Init config command *)
  let init_config_cmd =
    let force =
      let doc = "Overwrite existing config file." in
      Arg.(value & flag & info ["force"; "f"] ~doc)
    in
    let term =
      let open Term.Syntax in
      let+ _ = xdg_term
      and+ force = force
      and+ log_level = Logs_cli.level () in
      Logs.set_reporter (Logs_fmt.reporter ~app:Fmt.stdout ~dst:Fmt.stderr ());
      Logs.set_level log_level;
      match Sortal_config.write_default_config ~force () with
      | Error e ->
        Printf.eprintf "%s\n" e;
        1
      | Ok path ->
        Printf.printf "Created config file: %s\n" path;
        Printf.printf "\nEdit this file to configure:\n";
        Printf.printf "  - Git sync remote URL\n";
        Printf.printf "  - Branch name and commit message\n";
        0
    in
    let doc = "Initialize a default configuration file." in
    let man = [
      `S Manpage.s_description;
      `P "Creates a default config.toml file at ~/.config/sortal/config.toml";
      `P "The generated file includes comments explaining each option.";
      `P "Use --force to overwrite an existing config file.";
    ] in
    let info = Cmd.info "init" ~doc ~man in
    Cmd.v info term
  in

  (* Feed commands *)
  let opt_handle_arg =
    Arg.(value & pos 0 (some string) None & info [] ~docv:"HANDLE"
      ~doc:"Contact handle (syncs/lists all contacts with feeds if omitted)")
  in

  let contacts_with_feeds store =
    Sortal.Store.list store |> List.filter_map (fun contact ->
      match Sortal.Contact.feeds contact with
      | Some (_ :: _ as feeds) -> Some (Sortal.Contact.handle contact, feeds)
      | _ -> None
    )
  in

  let feed_sync_cmd =
    let force_arg =
      Arg.(value & flag & info ["force"] ~doc:"Force re-download, ignoring cached etag/last-modified")
    in
    let term =
      let open Term.Syntax in
      let+ (xdg, _) = xdg_term
      and+ handle_opt = opt_handle_arg
      and+ force = force_arg
      and+ log_level = Logs_cli.level () in
      Logs.set_reporter (Logs_fmt.reporter ~app:Fmt.stdout ~dst:Fmt.stderr ());
      Logs.set_level log_level;
      let store = Sortal.Store.create_from_xdg xdg in
      let targets = match handle_opt with
        | Some handle ->
          (match Sortal.Store.lookup store handle with
           | None -> Logs.err (fun m -> m "Contact not found: %s" handle); []
           | Some contact ->
             match Sortal.Contact.feeds contact with
             | None | Some [] ->
               Logs.err (fun m -> m "No feeds configured for @%s" handle); []
             | Some feeds -> [(handle, feeds)])
        | None ->
          let all = contacts_with_feeds store in
          if all = [] then
            Logs.err (fun m -> m "No contacts with feeds configured");
          all
      in
      if targets = [] then 1
      else
        let result = ref 0 in
        (try
          Eio.Switch.run @@ fun sw ->
          let session = Requests.create ~sw env in
          let feed_store = Sortal_feed.Store.create_from_xdg xdg in
          let failed = ref false in
          List.iter (fun (handle, feeds) ->
            match Sortal_feed.Sync.sync_all ~session ~store:feed_store ~handle ~force feeds with
            | Error msg ->
              Logs.err (fun m -> m "Feed sync failed for @%s: %s" handle msg);
              failed := true
            | Ok results ->
              List.iter (fun (r : Sortal_feed.Sync.sync_result) ->
                let name = Option.value ~default:"(unnamed)" r.feed_name in
                Logs.app (fun m -> m "  @%s %s: %d new, %d total" handle name r.new_entries r.total_entries)
              ) results
          ) targets;
          result := (if !failed then 1 else 0)
        with Eio.Cancel.Cancelled _ -> ());
        !result
    in
    let info = Cmd.info "sync" ~doc:"Sync feeds for a contact (or all contacts)" in
    Cmd.v info term
  in

  let feed_list_cmd =
    let term =
      let open Term.Syntax in
      let+ (xdg, _) = xdg_term
      and+ handle_opt = opt_handle_arg
      and+ log_level = Logs_cli.level () in
      Logs.set_reporter (Logs_fmt.reporter ~app:Fmt.stdout ~dst:Fmt.stderr ());
      Logs.set_level log_level;
      let store = Sortal.Store.create_from_xdg xdg in
      let targets = match handle_opt with
        | Some handle ->
          (match Sortal.Store.lookup store handle with
           | None -> Logs.err (fun m -> m "Contact not found: %s" handle); []
           | Some contact ->
             match Sortal.Contact.feeds contact with
             | None | Some [] ->
               Logs.err (fun m -> m "No feeds configured for @%s" handle); []
             | Some feeds -> [(handle, feeds)])
        | None ->
          let all = contacts_with_feeds store in
          if all = [] then
            Logs.err (fun m -> m "No contacts with feeds configured");
          all
      in
      if targets = [] then 1
      else begin
        let feed_store = Sortal_feed.Store.create_from_xdg xdg in
        List.iter (fun (handle, feeds) ->
          let entries = Sortal_feed.Store.all_entries feed_store ~handle feeds in
          if entries <> [] then begin
            Logs.app (fun m -> m "@%s (%d entries):" handle (List.length entries));
            List.iter (fun entry ->
              Fmt.pr "  %a@." Sortal_feed.Entry.pp entry
            ) entries
          end else begin
            Logs.app (fun m -> m "@%s (no entries synced):" handle);
            List.iter (fun feed ->
              Fmt.pr "  %a@." Sortal_schema.Feed.pp feed
            ) feeds
          end
        ) targets;
        0
      end
    in
    let info = Cmd.info "list" ~doc:"List feed entries for a contact (or all contacts)" in
    Cmd.v info term
  in

  let feed_show_cmd =
    let term =
      let open Term.Syntax in
      let+ (xdg, _) = xdg_term
      and+ handle = Sortal.Cmd.handle_arg
      and+ entry_id = Arg.(required & pos 1 (some string) None & info [] ~docv:"ID"
        ~doc:"Entry ID to display")
      and+ log_level = Logs_cli.level () in
      Logs.set_reporter (Logs_fmt.reporter ~app:Fmt.stdout ~dst:Fmt.stderr ());
      Logs.set_level log_level;
      let store = Sortal.Store.create_from_xdg xdg in
      match Sortal.Store.lookup store handle with
      | None -> Logs.err (fun m -> m "Contact not found: %s" handle); 1
      | Some contact ->
        match Sortal.Contact.feeds contact with
        | None | Some [] ->
          Logs.err (fun m -> m "No feeds configured for @%s" handle); 1
        | Some feeds ->
          let feed_store = Sortal_feed.Store.create_from_xdg xdg in
          let entries = Sortal_feed.Store.all_entries feed_store ~handle feeds in
          match List.find_opt (fun (e : Sortal_feed.Entry.t) -> e.id = entry_id) entries with
          | None ->
            Logs.err (fun m -> m "Entry not found: %s" entry_id); 1
          | Some entry ->
            Fmt.pr "%a@." Sortal_feed.Entry.pp_full entry;
            0
    in
    let info = Cmd.info "show" ~doc:"Show a specific feed entry" in
    Cmd.v info term
  in

  let contacts_with_manual_feeds store =
    Sortal.Store.list store |> List.filter_map (fun contact ->
      match Sortal.Contact.feeds contact with
      | Some feeds ->
        let manual = List.filter (fun f ->
          Sortal_schema.Feed.feed_type f = Manual) feeds
        in
        if manual <> [] then Some (Sortal.Contact.handle contact, manual)
        else None
      | _ -> None
    )
  in

  let feed_discover_cmd =
    let term =
      let open Term.Syntax in
      let+ (xdg, _) = xdg_term
      and+ handle_opt = opt_handle_arg
      and+ log_level = Logs_cli.level () in
      Logs.set_reporter (Logs_fmt.reporter ~app:Fmt.stdout ~dst:Fmt.stderr ());
      Logs.set_level log_level;
      let store = Sortal.Store.create_from_xdg xdg in
      let targets = match handle_opt with
        | Some handle ->
          (match Sortal.Store.lookup store handle with
           | None -> Logs.err (fun m -> m "Contact not found: %s" handle); []
           | Some contact ->
             match Sortal.Contact.feeds contact with
             | None | Some [] ->
               Logs.err (fun m -> m "No feeds configured for @%s" handle); []
             | Some feeds ->
               let manual = List.filter (fun f ->
                 Sortal_schema.Feed.feed_type f = Manual) feeds
               in
               if manual = [] then begin
                 Logs.err (fun m -> m "No manual feeds configured for @%s" handle); []
               end else [(handle, manual)])
        | None ->
          let all = contacts_with_manual_feeds store in
          if all = [] then
            Logs.err (fun m -> m "No contacts with manual feeds configured");
          all
      in
      if targets = [] then 1
      else begin
        let feed_store = Sortal_feed.Store.create_from_xdg xdg in
        let failed = ref false in
        Eio.Switch.run @@ fun sw ->
        let process_mgr = Eio.Stdenv.process_mgr env in
        let clock = Eio.Stdenv.clock env in
        List.iter (fun (handle, feeds) ->
          (* Load contact YAML for context *)
          let data_dir = Sortal.Store.data_dir store in
          let yaml_path = Eio.Path.(data_dir / (handle ^ ".yaml")) in
          let contact_yaml =
            try Eio.Path.load yaml_path
            with _ -> Printf.sprintf "handle: %s" handle
          in
          List.iter (fun feed ->
            let name = Option.value ~default:"(unnamed)"
                (Sortal_schema.Feed.name feed) in
            Logs.app (fun m -> m "Discovering @%s %s ..." handle name);
            match Sortal_discover.discover ~sw ~process_mgr ~clock
                    ~store:feed_store ~handle ~contact_yaml feed with
            | Ok r ->
              Logs.app (fun m -> m "  @%s %s: %d new, %d total"
                          handle name r.new_entries r.total_entries)
            | Error msg ->
              Logs.err (fun m -> m "  @%s %s: %s" handle name msg);
              failed := true
          ) feeds
        ) targets;
        if !failed then 1 else 0
      end
    in
    let doc = "Discover feed entries for contacts with manual feeds." in
    let man = [
      `S Manpage.s_description;
      `P "Uses Claude to inspect websites for contacts with manual feeds \
          and discover new content entries (blog posts, articles, etc.).";
      `P "Manual feeds are configured with $(b,feed_type: manual) in the \
          contact's feed list. The $(b,url) field specifies the page to \
          inspect and an optional $(b,hint) guides the discovery.";
      `P "Use $(b,sortal feed discover HANDLE) to discover for one contact, \
          or omit the handle to discover for all contacts with manual feeds.";
    ] in
    let info = Cmd.info "discover" ~doc ~man in
    Cmd.v info term
  in

  let feed_group =
    let info = Cmd.info "feed" ~doc:"Feed content management"
      ~man:[
        `S Manpage.s_description;
        `P "Fetch, store, and display feed content for contacts.";
        `P "Use $(b,sortal feed sync) to sync all contacts with feeds.";
        `P "Use $(b,sortal feed sync HANDLE) to sync a specific contact.";
        `P "Use $(b,sortal feed list) to view all feed entries.";
        `P "Use $(b,sortal feed show HANDLE ID) to view a specific entry.";
        `P "Use $(b,sortal feed discover) to discover entries for manual feeds.";
      ]
    in
    Cmd.group info [feed_sync_cmd; feed_list_cmd; feed_show_cmd; feed_discover_cmd]
  in

  let default_term =
    let open Term.Syntax in
    let+ _ = xdg_term
    and+ _ = Logs_cli.level () in
    `Help (`Pager, None)
  in
  let default_term = Term.ret default_term in

  let cmd = Cmd.group info ~default:default_term [
    list_cmd;
    show_cmd;
    thumbnail_cmd;
    search_cmd;
    stats_cmd;
    sync_cmd;
    git_group;
    init_config_cmd;
    config_cmd;
    add_cmd;
    delete_cmd;
    add_email_cmd;
    remove_email_cmd;
    add_service_cmd;
    remove_service_cmd;
    add_org_cmd;
    remove_org_cmd;
    add_url_cmd;
    remove_url_cmd;
    add_atproto_cmd;
    add_atproto_service_cmd;
    feed_group;
  ] in

  Cmd.eval' cmd
  in
  exit exit_code
