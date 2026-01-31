(*---------------------------------------------------------------------------
  Copyright (c) 2025 Anil Madhavapeddy <anil@recoil.org>. All rights reserved.
  SPDX-License-Identifier: ISC
 ---------------------------------------------------------------------------*)

(** oxmono CLI - package source management for the OxCaml monorepo *)

open Cmdliner

module Log = (val Logs.src_log (Logs.Src.create "oxmono") : Logs.LOG)

(** Setup logging *)
let setup_log style_renderer level =
  Fmt_tty.setup_std_outputs ?style_renderer ();
  Logs.set_level level;
  Logs.set_reporter (Logs_fmt.reporter ())

let logging_t =
  Term.(const setup_log $ Fmt_cli.style_renderer () $ Logs_cli.level ())

(** Common options *)
let repo_paths =
  let doc = "Path to an opam repository. Can be specified multiple times." in
  Arg.(value & opt_all string [] & info ["repo"] ~docv:"PATH" ~doc)

(** {1 Add Command} *)

let add_cmd =
  let package_name =
    let doc = "Name of the package to add." in
    Arg.(required & pos 0 (some string) None & info [] ~docv:"PACKAGE" ~doc)
  in
  let run () package_name repo_paths =
    if repo_paths = [] then begin
      Log.err (fun m -> m "No repository paths specified. Use --repo to specify opam repositories.");
      1
    end else
      Eio_main.run @@ fun env ->
      let fs = Eio.Stdenv.fs env in
      (* Load existing sources *)
      match Oxmono.Sources_file.load ~fs Oxmono.Sources_file.sources_filename with
      | Error e ->
        Log.err (fun m -> m "Failed to load sources.yaml: %s" e);
        1
      | Ok sources ->
        (* Check if package already exists *)
        if Oxmono.Source.find_package package_name sources <> None then begin
          Log.err (fun m -> m "Package %s already exists in sources.yaml" package_name);
          1
        end else
          (* Lookup package in repos *)
          match Oxmono.Opam_repo.lookup_package ~fs repo_paths package_name with
          | Error e ->
            Log.err (fun m -> m "%s" e);
            1
          | Ok (version, source) ->
            let sources = Oxmono.Source.add_package package_name source sources in
            Oxmono.Sources_file.save ~fs Oxmono.Sources_file.sources_filename sources;
            Log.app (fun m -> m "Added %s.%s to sources.yaml" package_name version);
            0
  in
  let doc = "Add a package from opam repositories to sources.yaml" in
  let info = Cmd.info "add" ~doc in
  Cmd.v info Term.(const run $ logging_t $ package_name $ repo_paths)

(** {1 Sync Command} *)

let sync_cmd =
  let run () =
    Eio_main.run @@ fun env ->
    let fs = Eio.Stdenv.fs env in
    let cwd = Eio.Stdenv.cwd env in
    (* Load sources *)
    match Oxmono.Sources_file.load ~fs Oxmono.Sources_file.sources_filename with
    | Error e ->
      Log.err (fun m -> m "Failed to load sources.yaml: %s" e);
      1
    | Ok sources ->
      let packages = Oxmono.Source.packages sources in
      if packages = [] then begin
        Log.app (fun m -> m "No packages in sources.yaml");
        0
      end else begin
        (* Ensure worktree exists *)
        match Oxmono.Worktree.ensure_worktree ~env ~cwd with
        | Error e ->
          Log.err (fun m -> m "Failed to setup worktree: %s" (Oxmono.Process.error_to_string ["worktree"] e));
          1
        | Ok _wt_path ->
          let errors = ref 0 in
          List.iter (fun (name, source) ->
            Log.app (fun m -> m "Syncing %s..." name);
            let source_dir = Eio.Path.(fs / "sources" / name) in
            let result = match source with
              | Oxmono.Source.Git { url; commit } ->
                Oxmono.Git.clone_or_update ~env ~url ~target:source_dir ~commit
              | Oxmono.Source.Archive { url; checksum } ->
                Oxmono.Archive.fetch_and_extract ~env ~url ~checksum ~target:source_dir
            in
            match result with
            | Error e ->
              Log.err (fun m -> m "Failed to sync %s: %s" name (Oxmono.Process.error_to_string [name] e));
              incr errors
            | Ok () ->
              (* Copy to worktree *)
              match Oxmono.Worktree.copy_to_worktree ~env ~cwd ~package_name:name ~source_dir with
              | Error e ->
                Log.err (fun m -> m "Failed to copy %s to worktree: %s" name (Oxmono.Process.error_to_string [name] e));
                incr errors
              | Ok () ->
                Log.app (fun m -> m "Synced %s" name)
          ) packages;
          if !errors > 0 then 1 else 0
      end
  in
  let doc = "Sync all packages from sources.yaml" in
  let info = Cmd.info "sync" ~doc in
  Cmd.v info Term.(const run $ logging_t)

(** {1 Diff Command} *)

let diff_cmd =
  let package_name =
    let doc = "Name of the package to diff." in
    Arg.(required & pos 0 (some string) None & info [] ~docv:"PACKAGE" ~doc)
  in
  let run () package_name =
    Eio_main.run @@ fun env ->
    let fs = Eio.Stdenv.fs env in
    let cwd = Eio.Stdenv.cwd env in
    let source_dir = Eio.Path.(fs / "sources" / package_name) in
    if not (Eio.Path.is_directory source_dir) then begin
      Log.err (fun m -> m "Source directory sources/%s does not exist" package_name);
      1
    end else begin
      let root = Eio.Path.native_exn cwd in
      let wt_path = Oxmono.Worktree.worktree_path ~root in
      let pristine_dir = Filename.concat wt_path package_name in
      if not (Sys.file_exists pristine_dir && Sys.is_directory pristine_dir) then begin
        Log.err (fun m -> m "Pristine directory %s does not exist. Run 'oxmono sync' first." pristine_dir);
        1
      end else begin
        let source_path = Eio.Path.native_exn source_dir in
        (* Run diff *)
        match Oxmono.Process.run ~env ["diff"; "-ruN"; pristine_dir; source_path] with
        | Ok () ->
          Log.app (fun m -> m "No differences for %s" package_name);
          0
        | Error (`Exit_code 1) ->
          (* diff returns 1 when there are differences - that's expected *)
          0
        | Error e ->
          Log.err (fun m -> m "Diff failed: %s" (Oxmono.Process.error_to_string ["diff"] e));
          1
      end
    end
  in
  let doc = "Show differences between local sources and pristine upstream" in
  let info = Cmd.info "diff" ~doc in
  Cmd.v info Term.(const run $ logging_t $ package_name)

(** {1 List Command} *)

let list_cmd =
  let dir_path =
    let doc = "Directory to scan for untracked packages." in
    Arg.(value & opt string "opam" & info ["dir"] ~docv:"DIR" ~doc)
  in
  let run () dir_path =
    Eio_main.run @@ fun env ->
    let fs = Eio.Stdenv.fs env in
    (* Load sources *)
    let sources = match Oxmono.Sources_file.load ~fs Oxmono.Sources_file.sources_filename with
      | Error e ->
        Log.warn (fun m -> m "Failed to load sources.yaml: %s" e);
        Oxmono.Source.empty
      | Ok sources -> sources
    in
    let tracked = Oxmono.Source.packages sources |> List.map fst in
    (* List subdirectories in the target directory *)
    let target_dir = Eio.Path.(fs / dir_path) in
    if not (Eio.Path.is_directory target_dir) then begin
      Log.err (fun m -> m "Directory %s does not exist" dir_path);
      1
    end else begin
      let subdirs = Eio.Path.read_dir target_dir
        |> List.filter (fun name ->
          let subpath = Eio.Path.(target_dir / name) in
          Eio.Path.is_directory subpath)
        |> List.sort String.compare
      in
      let untracked = List.filter (fun name -> not (List.mem name tracked)) subdirs in
      if untracked = [] then begin
        Log.app (fun m -> m "All packages in %s/ are tracked in sources.yaml" dir_path);
        0
      end else begin
        Log.app (fun m -> m "Untracked packages in %s/:" dir_path);
        List.iter (fun name -> Log.app (fun m -> m "  %s" name)) untracked;
        0
      end
    end
  in
  let doc = "List packages in a directory not tracked in sources.yaml" in
  let info = Cmd.info "list" ~doc in
  Cmd.v info Term.(const run $ logging_t $ dir_path)

(** {1 Main} *)

let main_cmd =
  let doc = "Package source management for the OxCaml monorepo" in
  let info = Cmd.info "oxmono" ~version:"0.1.0" ~doc in
  Cmd.group info [add_cmd; sync_cmd; diff_cmd; list_cmd]

let () = exit (Cmd.eval' main_cmd)
