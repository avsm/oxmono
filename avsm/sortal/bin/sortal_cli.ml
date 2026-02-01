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

  Eio_main.run @@ fun env ->

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
  let sync_cmd = make_term Sortal.Cmd.sync_info Term.(const (fun () -> Sortal.Cmd.sync_cmd ()) $ const ()) in

  (* Git init command needs special handling to pass env *)
  let git_init_cmd =
    let term =
      let open Term.Syntax in
      let+ (xdg, _) = xdg_term
      and+ log_level = Logs_cli.level () in
      Logs.set_reporter (Logs_fmt.reporter ~app:Fmt.stdout ~dst:Fmt.stderr ());
      Logs.set_level log_level;
      Sortal.Cmd.git_init_cmd xdg env
    in
    Cmd.v Sortal.Cmd.git_init_info term
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

  (* Git sync command *)
  let git_sync_cmd =
    let term =
      let open Term.Syntax in
      let+ (xdg, _) = xdg_term
      and+ dry_run = Gitops.Sync.Cmd.dry_run_term
      and+ remote_override = Gitops.Sync.Cmd.remote_term
      and+ log_level = Logs_cli.level () in
      Logs.set_reporter (Logs_fmt.reporter ~app:Fmt.stdout ~dst:Fmt.stderr ());
      Logs.set_level log_level;
      match Sortal_config.load () with
      | Error e -> Printf.eprintf "Config error: %s\n" e; 1
      | Ok config ->
        let data_dir = Xdge.data_dir xdg |> Eio.Path.native_exn in

        (* Check if sync is configured *)
        let sync_config = match remote_override with
          | Some r -> { config.Sortal_config.sync with Gitops.Sync.Config.remote = r }
          | None -> config.Sortal_config.sync
        in

        if sync_config.Gitops.Sync.Config.remote = "" then begin
          Printf.eprintf "Error: No sync remote configured.\n";
          Printf.eprintf "Add to ~/.config/sortal/config.toml:\n";
          Printf.eprintf "  [sync]\n";
          Printf.eprintf "  remote = \"ssh://server/path/to/repo.git\"\n";
          Printf.eprintf "\nOr use --remote URL\n";
          1
        end else begin
          let git = Gitops.v ~dry_run env in
          let repo = Eio.Path.(env#fs / data_dir) in

          Printf.printf "%s sortal data with %s\n"
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
    let doc = "Sync sortal data with remote git repository." in
    let man = [
      `S Manpage.s_description;
      `P "Synchronizes your sortal data directory with a remote git repository.";
      `P "Configure the remote in ~/.config/sortal/config.toml:";
      `Pre "  [sync]\n  remote = \"ssh://server/path/to/sortal.git\"";
      `P "The sync process:";
      `P "1. Fetches from the remote repository";
      `P "2. Merges any remote changes";
      `P "3. Commits local changes (if auto_commit is enabled)";
      `P "4. Pushes to the remote";
      `P "Use $(b,--dry-run) to preview what would happen.";
    ] in
    let info = Cmd.info "git-sync" ~doc ~man in
    Cmd.v info term
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
    git_init_cmd;
    git_sync_cmd;
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
  ] in

  exit (Cmd.eval' cmd)
