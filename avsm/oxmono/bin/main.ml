(*---------------------------------------------------------------------------
  Copyright (c) 2025 Anil Madhavapeddy <anil@recoil.org>. All rights reserved.
  SPDX-License-Identifier: ISC
 ---------------------------------------------------------------------------*)

(** oxmono CLI - package source management for the OxCaml monorepo *)

open Cmdliner

module Log = (val Logs.src_log (Logs.Src.create "oxmono") : Logs.LOG)

(** Result syntax for cleaner error handling *)
module Result_syntax = struct
  let ( let* ) = Result.bind
  let ( let+ ) r f = Result.map f r
end

(** Setup logging *)
let setup_log style_renderer level =
  Fmt_tty.setup_std_outputs ?style_renderer ();
  Logs.set_level level;
  Logs.set_reporter (Logs_fmt.reporter ())

let logging_t =
  Term.(const setup_log $ Fmt_cli.style_renderer () $ Logs_cli.level ())

(** Run a result computation, logging errors and returning exit code *)
let run_result r =
  match r with
  | Ok code -> code
  | Error msg ->
    Log.err (fun m -> m "%s" msg);
    1

(** Map process errors to string *)
let process_err ctx e = Oxmono.Process.error_to_string ctx e

(** Strip surrounding quotes from a string *)
let unquote s =
  let len = String.length s in
  if len >= 2 && s.[0] = '"' && s.[len - 1] = '"' then
    String.sub s 1 (len - 2)
  else s

(** {1 Add Command} *)

(** Parse opam url.src field to extract source info.
    Git URLs look like: git+https://github.com/foo/bar.git#commit
    Archive URLs are just: https://example.com/foo.tar.gz *)
let parse_opam_url url checksum =
  if String.length url > 4 && String.sub url 0 4 = "git+" then
    let url = String.sub url 4 (String.length url - 4) in
    match String.index_opt url '#' with
    | Some i ->
      let base_url = String.sub url 0 i in
      let commit = String.sub url (i + 1) (String.length url - i - 1) in
      Ok (Oxmono.Source.Git { url = base_url; commit })
    | None ->
      Error "Git URL missing commit hash"
  else if checksum <> "" then
    Ok (Oxmono.Source.Archive { url; checksum })
  else
    Error "Archive URL missing checksum"

let add_cmd =
  let package_name =
    let doc = "Name of the package to add from the current opam switch." in
    Arg.(required & pos 0 (some string) None & info [] ~docv:"PACKAGE" ~doc)
  in
  let run () package_name =
    Eio_main.run @@ fun env ->
    let open Result_syntax in
    let fs = Eio.Stdenv.fs env in
    let cwd = Eio.Stdenv.cwd env in
    run_result @@
    let* sources =
      Oxmono.Sources_file.load ~fs Oxmono.Sources_file.sources_filename
      |> Result.map_error (Printf.sprintf "Failed to load sources.yaml: %s")
    in
    let* () =
      if Oxmono.Source.find_package package_name sources <> None then
        Error (Printf.sprintf "Package %s already exists in sources.yaml" package_name)
      else Ok ()
    in
    let* url_raw =
      Oxmono.Process.run_with_output ~env ["opam"; "show"; "-f"; "url.src:"; package_name]
      |> Result.map_error (fun e -> Printf.sprintf "Failed to get package info: %s" (process_err ["opam"; "show"] e))
    in
    let url = url_raw |> String.trim |> unquote in
    let* () =
      if url = "" then Error (Printf.sprintf "Package %s has no source URL" package_name)
      else Ok ()
    in
    let checksum =
      Oxmono.Process.run_with_output ~env ["opam"; "show"; "-f"; "url.checksum:"; package_name]
      |> Result.value ~default:""
      |> String.trim |> unquote
    in
    let* source =
      parse_opam_url url checksum
      |> Result.map_error (Printf.sprintf "Failed to parse source URL: %s")
    in
    let* wt_path =
      Oxmono.Worktree.ensure_worktree ~env ~cwd
      |> Result.map_error (fun e -> Printf.sprintf "Failed to setup worktree: %s" (process_err ["worktree"] e))
    in
    let target_dir = Filename.concat wt_path package_name in
    Log.app (fun m -> m "Downloading %s..." package_name);
    let* () =
      Oxmono.Process.run ~env ["opam"; "source"; package_name; Printf.sprintf "--dir=%s" target_dir]
      |> Result.map_error (fun e -> Printf.sprintf "Failed to download source: %s" (process_err ["opam"; "source"] e))
    in
    let sources = Oxmono.Source.add_package package_name source sources in
    Oxmono.Sources_file.save ~fs Oxmono.Sources_file.sources_filename sources;
    let+ () =
      Oxmono.Worktree.commit_package ~env ~cwd ~package_name
      |> Result.map_error (fun e -> Printf.sprintf "Failed to commit: %s" (process_err ["commit"] e))
    in
    Log.app (fun m -> m "Added %s to sources.yaml and sources/" package_name);
    0
  in
  let doc = "Add a package from the current opam switch to sources.yaml" in
  let info = Cmd.info "add" ~doc in
  Cmd.v info Term.(const run $ logging_t $ package_name)

(** {1 Sync Command} *)

let sync_one_package ~env ~fs ~cwd ~wt_path (name, source) =
  let open Result_syntax in
  Log.app (fun m -> m "Syncing %s..." name);
  let target_dir = Eio.Path.(fs / wt_path / name) in
  let* () = match source with
    | Oxmono.Source.Git { url; commit } ->
      Oxmono.Git.clone_or_update ~env ~url ~target:target_dir ~commit
    | Oxmono.Source.Archive { url; checksum } ->
      Oxmono.Archive.fetch_and_extract ~env ~url ~checksum ~target:target_dir
  in
  Oxmono.Worktree.commit_package ~env ~cwd ~package_name:name

let sync_cmd =
  let run () =
    Eio_main.run @@ fun env ->
    let open Result_syntax in
    let fs = Eio.Stdenv.fs env in
    let cwd = Eio.Stdenv.cwd env in
    run_result @@
    let* sources =
      Oxmono.Sources_file.load ~fs Oxmono.Sources_file.sources_filename
      |> Result.map_error (Printf.sprintf "Failed to load sources.yaml: %s")
    in
    let packages = Oxmono.Source.packages sources in
    if packages = [] then begin
      Log.app (fun m -> m "No packages in sources.yaml");
      Ok 0
    end else
      let* wt_path =
        Oxmono.Worktree.ensure_worktree ~env ~cwd
        |> Result.map_error (fun e -> Printf.sprintf "Failed to setup worktree: %s" (process_err ["worktree"] e))
      in
      let errors =
        List.fold_left (fun errors pkg ->
          match sync_one_package ~env ~fs ~cwd ~wt_path pkg with
          | Ok () ->
            Log.app (fun m -> m "Synced %s" (fst pkg));
            errors
          | Error e ->
            Log.err (fun m -> m "Failed to sync %s: %s" (fst pkg) (process_err [fst pkg] e));
            errors + 1
        ) 0 packages
      in
      Ok (if errors > 0 then 1 else 0)
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
    let open Result_syntax in
    let cwd = Eio.Stdenv.cwd env in
    let root = Eio.Path.native_exn cwd in
    let wt_path = Oxmono.Worktree.worktree_path ~root in
    let pristine_dir = Filename.concat wt_path package_name in
    let modified_dir = Filename.concat root (Filename.concat "opam" package_name) in
    run_result @@
    let* () =
      if not (Sys.file_exists pristine_dir && Sys.is_directory pristine_dir) then
        Error (Printf.sprintf "Pristine directory %s does not exist. Run 'oxmono sync' first." pristine_dir)
      else Ok ()
    in
    let* () =
      if not (Sys.file_exists modified_dir && Sys.is_directory modified_dir) then
        Error (Printf.sprintf "Modified directory opam/%s does not exist" package_name)
      else Ok ()
    in
    match Oxmono.Process.run ~env ["diff"; "-ruN"; pristine_dir; modified_dir] with
    | Ok () ->
      Log.app (fun m -> m "No differences for %s" package_name);
      Ok 0
    | Error (`Exit_code 1) ->
      Ok 0  (* diff returns 1 when there are differences *)
    | Error e ->
      Error (Printf.sprintf "Diff failed: %s" (process_err ["diff"] e))
  in
  let doc = "Show differences between opam/<pkg> and pristine sources/<pkg>" in
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
    let sources =
      Oxmono.Sources_file.load ~fs Oxmono.Sources_file.sources_filename
      |> Result.fold ~ok:Fun.id ~error:(fun e ->
           Log.warn (fun m -> m "Failed to load sources.yaml: %s" e);
           Oxmono.Source.empty)
    in
    let tracked = Oxmono.Source.packages sources |> List.map fst in
    let target_dir = Eio.Path.(fs / dir_path) in
    run_result @@
    let open Result_syntax in
    let* () =
      if not (Eio.Path.is_directory target_dir) then
        Error (Printf.sprintf "Directory %s does not exist" dir_path)
      else Ok ()
    in
    let subdirs =
      Eio.Path.read_dir target_dir
      |> List.filter (fun name -> Eio.Path.is_directory Eio.Path.(target_dir / name))
      |> List.sort String.compare
    in
    let untracked = List.filter (fun name -> not (List.mem name tracked)) subdirs in
    if untracked = [] then
      Log.app (fun m -> m "All packages in %s/ are tracked in sources.yaml" dir_path)
    else begin
      Log.app (fun m -> m "Untracked packages in %s/:" dir_path);
      List.iter (fun name -> Log.app (fun m -> m "  %s" name)) untracked
    end;
    Ok 0
  in
  let doc = "List packages in a directory not tracked in sources.yaml" in
  let info = Cmd.info "list" ~doc in
  Cmd.v info Term.(const run $ logging_t $ dir_path)

(** {1 Changes Command} *)

(** JSON Schema for package change records *)
let changes_schema =
  let open Jsont in
  let meta = Meta.none in
  let json_object fields = Object (fields, meta) in
  let json_string s = String (s, meta) in
  let json_array items = Array (items, meta) in
  let json_field name value = ((name, meta), value) in
  json_object [
    json_field "type" (json_string "object");
    json_field "properties" (json_object [
      json_field "git_commit" (json_object [
        json_field "type" (json_string "string");
        json_field "description" (json_string "Git commit hash this diff was generated against")
      ]);
      json_field "change_type" (json_object [
        json_field "type" (json_string "string");
        json_field "enum" (json_array [
          json_string "unchanged";
          json_string "dune-port";
          json_string "oxcaml";
          json_string "new-feature";
          json_string "bugfix";
          json_string "compatibility";
          json_string "build-fix";
          json_string "mixed"
        ]);
        json_field "description" (json_string "Type of changes: unchanged (no meaningful diff), dune-port (adding dune build support), oxcaml (uses oxcaml features like unboxed types), new-feature (adds new features not in upstream), bugfix (bug fixes not yet in upstream), compatibility (compatibility fixes for monorepo), build-fix (build system fixes beyond dune), mixed (multiple types of changes)")
      ]);
      json_field "summary" (json_object [
        json_field "type" (json_string "string");
        json_field "description" (json_string "Single sentence summary of the changes")
      ]);
      json_field "details" (json_object [
        json_field "type" (json_string "string");
        json_field "description" (json_string "Longer markdown description with bullet points detailing specific changes")
      ])
    ]);
    json_field "required" (json_array [
      json_string "git_commit";
      json_string "change_type";
      json_string "summary";
      json_string "details"
    ])
  ]

(** Get current git HEAD commit *)
let get_git_head ~env =
  Oxmono.Process.run_with_output ~env ["git"; "rev-parse"; "HEAD"]
  |> Result.map String.trim

(** Get diff output between pristine and modified package directories *)
let get_diff_output ~env ~pristine_dir ~modified_dir =
  let result =
    Oxmono.Process.run_with_output ~env ["diff"; "-ruN"; pristine_dir; modified_dir]
  in
  match result with
  | Ok output -> Ok output
  | Error (`Exit_code 1) ->
    (* diff returns 1 when there are differences, but we still get output *)
    Oxmono.Process.run_with_output ~env ["diff"; "-ruN"; pristine_dir; modified_dir]
    |> Result.map_error (fun e -> process_err ["diff"] e)
  | Error e -> Error (process_err ["diff"] e)

(** Permission callback that allows all tool use with logging *)
let auto_allow_callback ctx =
  Log.debug (fun m -> m "Allowing tool: %s" ctx.Claude.Permissions.tool_name);
  Claude.Permissions.Decision.allow ()

(** Extract string field from JSON object *)
let json_get_string json key =
  match json with
  | Jsont.Object (fields, _) ->
    List.find_map (fun ((k, _), v) ->
      if k = key then
        match v with
        | Jsont.String (s, _) -> Some s
        | _ -> None
      else None
    ) fields
  | _ -> None

(** Load existing changes file and extract git_commit *)
let load_changes_commit ~fs changes_file =
  if not (Sys.file_exists changes_file) then
    None
  else
    let content = Eio.Path.load Eio.Path.(fs / changes_file) in
    match Jsont_bytesrw.decode_string Jsont.json content with
    | Ok json -> json_get_string json "git_commit"
    | Error _ -> None

(** Check if there have been commits to a directory since a given commit *)
let has_commits_since ~env ~commit ~dir =
  (* git log <commit>..HEAD -- <dir> returns commits affecting dir since commit *)
  let cmd = ["git"; "log"; "--oneline"; Printf.sprintf "%s..HEAD" commit; "--"; dir] in
  match Oxmono.Process.run_with_output ~env cmd with
  | Ok output -> String.trim output <> ""
  | Error _ -> true  (* On error, assume there are changes *)

(** Result type for single package analysis *)
type analyze_result =
  | Skipped
  | Analyzed of { change_type: string; summary: string }
  | Failed of string

(** Analyze a single package and write changes file.
    Returns the result of the analysis. *)
let analyze_package ~env ~fs ~root ~wt_path ~changes_dir ~git_commit ~force package_name =
  let pristine_dir = Filename.concat wt_path package_name in
  let modified_dir = Filename.concat root (Filename.concat "opam" package_name) in
  let opam_subdir = Filename.concat "opam" package_name in
  let changes_file = Filename.concat changes_dir (package_name ^ ".json") in
  (* Check directories exist *)
  if not (Sys.file_exists pristine_dir && Sys.is_directory pristine_dir) then
    Failed (Printf.sprintf "Pristine directory for %s does not exist. Run 'oxmono sync' first." package_name)
  else if not (Sys.file_exists modified_dir && Sys.is_directory modified_dir) then
    Failed (Printf.sprintf "Modified directory opam/%s does not exist" package_name)
  else
    (* Check if we need to re-analyze *)
    let should_analyze =
      if force then true
      else
        match load_changes_commit ~fs changes_file with
        | None -> true
        | Some prev_commit ->
          if has_commits_since ~env ~commit:prev_commit ~dir:opam_subdir then begin
            Log.app (fun m -> m "[%s] Changes detected since %s" package_name
              (String.sub prev_commit 0 (min 7 (String.length prev_commit))));
            true
          end else begin
            Log.debug (fun m -> m "[%s] No changes since %s, skipping"
              package_name (String.sub prev_commit 0 (min 7 (String.length prev_commit))));
            false
          end
    in
    if not should_analyze then
      Skipped
    else begin
      (* Build prompt for Claude - tell it to diff the directories itself *)
      let prompt =
        Printf.sprintf {|Analyze the differences between the upstream pristine version and the local modified version of the OCaml package "%s".

Directory paths:
- Pristine upstream: %s
- Modified local: %s

To see the differences, run: diff -ruN %s %s

Please categorize and summarize the changes. The change_type should be one of:
- "unchanged": No meaningful differences (empty diff)
- "dune-port": Main change is adding dune build support
- "oxcaml": Uses OxCaml features like unboxed types, stack allocation, or data-race-free parallelism
- "new-feature": Adds new features not in upstream
- "bugfix": Bug fixes not yet in upstream
- "compatibility": Compatibility fixes for the monorepo
- "build-fix": Build system fixes beyond just dune
- "mixed": Multiple types of changes

Provide:
- git_commit: "%s"
- change_type: The most appropriate category from above
- summary: A single sentence summary (e.g., "Add dune support" or "Use OxCaml unboxed types for performance")
- details: Longer markdown with bullet points detailing specific changes

Respond with ONLY the JSON object matching the schema.|} package_name pristine_dir modified_dir pristine_dir modified_dir git_commit
      in
      Log.app (fun m -> m "[%s] Analyzing with Claude..." package_name);
      (* Configure Claude with structured output *)
      let output_format = Claude.Proto.Structured_output.of_json_schema changes_schema in
      let options =
        Claude.Options.default
        |> Claude.Options.with_output_format output_format
        |> Claude.Options.with_permission_callback auto_allow_callback
        |> Claude.Options.with_max_turns 5
      in
      (* Run Claude *)
      let process_mgr = Eio.Stdenv.process_mgr env in
      let clock = Eio.Stdenv.clock env in
      let structured_output = ref None in
      Eio.Switch.run (fun sw ->
        let client = Claude.Client.create ~sw ~process_mgr ~clock ~options () in
        Claude.Client.query client prompt;
        let responses = Claude.Client.receive_all client in
        List.iter (function
          | Claude.Response.Complete result ->
            structured_output := Claude.Response.Complete.structured_output result
          | Claude.Response.Error err ->
            Log.err (fun m -> m "[%s] Claude error: %s" package_name (Claude.Response.Error.message err))
          | _ -> ()
        ) responses
      );
      (* Process result *)
      match !structured_output with
      | None ->
        Failed (Printf.sprintf "[%s] Claude did not return structured output" package_name)
      | Some json ->
        (* Pretty print JSON to file *)
        let json_str =
          match Jsont_bytesrw.encode_string ~format:Jsont.Indent Jsont.json json with
          | Ok s -> s
          | Error err -> err
        in
        (* Write to file *)
        Eio.Path.save ~create:(`Or_truncate 0o644)
          Eio.Path.(fs / changes_file)
          json_str;
        let change_type = json_get_string json "change_type" |> Option.value ~default:"unknown" in
        let summary = json_get_string json "summary" |> Option.value ~default:"" in
        Log.app (fun m -> m "[%s] Done: %s - %s" package_name change_type summary);
        Analyzed { change_type; summary }
    end

let changes_cmd =
  let package_name =
    let doc = "Name of the package to analyze. If not specified, analyzes all packages." in
    Arg.(value & pos 0 (some string) None & info [] ~docv:"PACKAGE" ~doc)
  in
  let force =
    let doc = "Force re-analysis even if no commits since last analysis." in
    Arg.(value & flag & info ["f"; "force"] ~doc)
  in
  let run () package_name_opt force =
    Eio_main.run @@ fun env ->
    let open Result_syntax in
    let fs = Eio.Stdenv.fs env in
    let cwd = Eio.Stdenv.cwd env in
    let root = Eio.Path.native_exn cwd in
    let wt_path = Oxmono.Worktree.worktree_path ~root in
    let changes_dir = Filename.concat root "changes" in
    run_result @@
    (* Create changes directory if it doesn't exist *)
    let () =
      if not (Sys.file_exists changes_dir) then
        Sys.mkdir changes_dir 0o755
    in
    (* Get git commit once for all packages *)
    let* git_commit =
      get_git_head ~env
      |> Result.map_error (fun e -> Printf.sprintf "Failed to get git HEAD: %s" (process_err ["git"] e))
    in
    (* Get list of packages to analyze *)
    let* packages =
      match package_name_opt with
      | Some name -> Ok [name]
      | None ->
        (* Get all packages from sources.yaml *)
        let* sources =
          Oxmono.Sources_file.load ~fs Oxmono.Sources_file.sources_filename
          |> Result.map_error (Printf.sprintf "Failed to load sources.yaml: %s")
        in
        let pkgs = Oxmono.Source.packages sources |> List.map fst in
        if pkgs = [] then
          Error "No packages found in sources.yaml"
        else begin
          Log.app (fun m -> m "Analyzing %d packages (max 20 concurrent)..." (List.length pkgs));
          Ok pkgs
        end
    in
    (* Analyze packages concurrently *)
    let results = ref [] in
    let analyze pkg =
      let result = analyze_package ~env ~fs ~root ~wt_path ~changes_dir ~git_commit ~force pkg in
      results := (pkg, result) :: !results
    in
    Eio.Fiber.List.iter ~max_fibers:20 analyze packages;
    (* Summarize results *)
    let analyzed = List.filter (fun (_, r) -> match r with Analyzed _ -> true | _ -> false) !results in
    let skipped = List.filter (fun (_, r) -> match r with Skipped -> true | _ -> false) !results in
    let failed = List.filter (fun (_, r) -> match r with Failed _ -> true | _ -> false) !results in
    if List.length packages > 1 then begin
      Log.app (fun m -> m "");
      Log.app (fun m -> m "Summary: %d analyzed, %d skipped, %d failed"
        (List.length analyzed) (List.length skipped) (List.length failed));
      List.iter (fun (pkg, r) ->
        match r with
        | Failed msg -> Log.err (fun m -> m "  %s: %s" pkg msg)
        | _ -> ()
      ) failed
    end;
    Ok (if List.length failed > 0 then 1 else 0)
  in
  let doc = "Analyze and record changes between opam/<pkg> and sources/<pkg>" in
  let info = Cmd.info "changes" ~doc in
  Cmd.v info Term.(const run $ logging_t $ package_name $ force)

(** {1 Main} *)

let main_cmd =
  let doc = "Package source management for the OxCaml monorepo" in
  let info = Cmd.info "oxmono" ~version:"0.1.0" ~doc in
  Cmd.group info [add_cmd; sync_cmd; diff_cmd; list_cmd; changes_cmd]

let () = exit (Cmd.eval' main_cmd)
