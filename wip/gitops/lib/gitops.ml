(*---------------------------------------------------------------------------
  Copyright (c) 2025 Anil Madhavapeddy <anil@recoil.org>. All rights reserved.
  SPDX-License-Identifier: ISC
 ---------------------------------------------------------------------------*)

(** Git operations library with Eio and dry-run support *)

(** {1 Error Types} *)

type git_error =
  | Exit_code of int
  | Signaled of int
  | Not_a_repo
  | No_remote of string
  | Merge_conflict
  | Nothing_to_commit
  | Push_rejected
  | Command_not_found
  | Output_limit of int

type Eio.Exn.err += Git of git_error

let () =
  Eio.Exn.register_pp (fun f -> function
    | Git (Exit_code n) -> Fmt.pf f "Git command failed (exit code %d)" n; true
    | Git (Signaled n) -> Fmt.pf f "Git command killed by signal %d" n; true
    | Git Not_a_repo -> Fmt.pf f "Not a git repository"; true
    | Git (No_remote r) -> Fmt.pf f "No remote named %S" r; true
    | Git Merge_conflict -> Fmt.pf f "Merge conflict"; true
    | Git Nothing_to_commit -> Fmt.pf f "Nothing to commit"; true
    | Git Push_rejected -> Fmt.pf f "Push rejected"; true
    | Git Command_not_found -> Fmt.pf f "Git command not found"; true
    | Git (Output_limit limit) ->
        Fmt.pf f "Git command output exceeds %d bytes" limit; true
    | _ -> false)

(** {1 Context} *)

type t = {
  env : Eio_unix.Stdenv.base;
  dry_run : bool;
}

let v ~dry_run env =
  { env; dry_run }

let dry_run t = t.dry_run

(** {1 Internal Execution} *)

let src = Logs.Src.create "gitops" ~doc:"Git operations"
module Log = (val Logs.src_log src : Logs.LOG)

let command ~repo args =
  "git" :: "-C" :: Eio.Path.native_exn repo :: args

let process_error = function
  | `Exited 0 -> None
  | `Exited n -> Some (Exit_code n)
  | `Signaled n -> Some (Signaled n)

let run_git_raw t ~repo args =
  try
    Eioext.Process.run ~process_mgr:t.env#process_mgr (command ~repo args);
    Ok ()
  with
  | Eio.Io (Eio.Process.E (Eio.Process.Child_error status), _) ->
      Error (Option.get (process_error status))
  | Eio.Io (Eio.Process.E (Eio.Process.Executable_not_found _), _) ->
      Error Command_not_found

let run_git_output t ~repo args =
  try
    let result =
      Eioext.Process.capture ~process_mgr:t.env#process_mgr (command ~repo args)
    in
    match process_error result.status with
    | None -> Ok result.stdout
    | Some error -> Error error
  with
  | Eioext.Process.Output_limit limit -> Error (Output_limit limit)
  | Eio.Io (Eio.Process.E (Eio.Process.Executable_not_found _), _) ->
      Error Command_not_found

let raise_git_error ~context err =
  let exn = Eio.Exn.create (Git err) in
  let bt = Printexc.get_callstack 10 in
  Eio.Exn.reraise_with_context exn bt "%s" context

let run_git t ~repo ~context args =
  match run_git_raw t ~repo args with
  | Ok () -> ()
  | Error err -> raise_git_error ~context err

let run_git_for_output t ~repo ~context args =
  match run_git_output t ~repo args with
  | Ok output -> output
  | Error err -> raise_git_error ~context err

(** {1 Query Operations} *)

(** These always execute, even in dry-run mode, since control flow may depend on results *)

let is_repo t ~repo =
  let git_dir = Eio.Path.(repo / ".git") in
  match Eio.Path.stat ~follow:false git_dir with
  | _ -> true
  | exception Eio.Io (Eio.Fs.E (Eio.Fs.Not_found _), _) -> false

let rev_parse t ~repo ref_ =
  run_git_for_output t ~repo ~context:(Printf.sprintf "rev-parse %s" ref_)
    ["rev-parse"; ref_] |> String.trim

let rev_parse_opt t ~repo ref_ =
  match run_git_output t ~repo ["rev-parse"; "--verify"; "--quiet"; ref_] with
  | Ok output -> Some (String.trim output)
  | Error (Exit_code 1) -> None
  | Error error -> raise_git_error ~context:"resolving revision" error

let status_porcelain t ~repo =
  run_git_for_output t ~repo ~context:"reading status" ["status"; "--porcelain"]

let status t ~repo =
  if status_porcelain t ~repo = "" then `Clean else `Dirty

let status_files t ~repo =
  String.split_on_char '\n' (status_porcelain t ~repo)
  |> List.filter (fun s -> s <> "")

let remote_url t ~repo ~remote =
  let names =
    run_git_for_output t ~repo ~context:"listing remotes" ["remote"]
    |> String.split_on_char '\n'
  in
  if List.mem remote names then
    Some (run_git_for_output t ~repo ~context:"reading remote URL"
            ["remote"; "get-url"; remote] |> String.trim)
  else None

let branch_exists t ~repo branch =
  Option.is_some (rev_parse_opt t ~repo ("refs/heads/" ^ branch))

let current_branch t ~repo =
  match run_git_output t ~repo ["symbolic-ref"; "--quiet"; "--short"; "HEAD"] with
  | Ok branch -> Some (String.trim branch)
  | Error (Exit_code 1) -> None
  | Error error -> raise_git_error ~context:"reading current branch" error

(** {1 Mutating Operations} *)

(** These log in dry-run mode instead of executing *)

let log_dry_run args =
  Log.info (fun m -> m "Would run: git %s" (String.concat " " args))

let init t ~repo =
  let args = ["init"] in
  if t.dry_run then log_dry_run args
  else run_git t ~repo ~context:"initializing repository" args

let fetch t ~repo ~remote =
  let args = ["fetch"; remote] in
  if t.dry_run then log_dry_run args
  else run_git t ~repo ~context:(Printf.sprintf "fetching from %s" remote) args

let pull t ~repo ~remote =
  let args = ["pull"; remote] in
  if t.dry_run then log_dry_run args
  else run_git t ~repo ~context:(Printf.sprintf "pulling from %s" remote) args

let merge t ~repo ~ref_ =
  let args = ["merge"; ref_] in
  if t.dry_run then log_dry_run args
  else run_git t ~repo ~context:(Printf.sprintf "merging %s" ref_) args

let add t ~repo ~paths =
  let args = "add" :: paths in
  if t.dry_run then log_dry_run args
  else run_git t ~repo ~context:"staging files" args

let add_all t ~repo =
  let args = ["add"; "-A"] in
  if t.dry_run then log_dry_run args
  else run_git t ~repo ~context:"staging all changes" args

let commit t ~repo ~msg =
  let args = ["commit"; "-m"; msg] in
  if t.dry_run then log_dry_run args
  else
    match run_git_output t ~repo ["diff"; "--cached"; "--quiet"; "--exit-code"] with
    | Ok _ -> ()
    | Error (Exit_code 1) -> run_git t ~repo ~context:"committing changes" args
    | Error error -> raise_git_error ~context:"inspecting staged changes" error

let push t ~repo ~remote =
  let args = ["push"; remote] in
  if t.dry_run then log_dry_run args
  else run_git t ~repo ~context:(Printf.sprintf "pushing to %s" remote) args

let push_set_upstream t ~repo ~remote ~branch =
  let args = ["push"; "-u"; remote; branch] in
  if t.dry_run then log_dry_run args
  else run_git t ~repo ~context:(Printf.sprintf "pushing to %s (set upstream)" remote) args

let remote_add t ~repo ~name ~url =
  let args = ["remote"; "add"; name; url] in
  if t.dry_run then log_dry_run args
  else run_git t ~repo ~context:(Printf.sprintf "adding remote %s" name) args

let remote_set_url t ~repo ~name ~url =
  let args = ["remote"; "set-url"; name; url] in
  if t.dry_run then log_dry_run args
  else run_git t ~repo ~context:(Printf.sprintf "setting URL for remote %s" name) args

let clone t ~url ~target =
  let target_str = Eio.Path.native_exn target in
  let args = ["clone"; url; target_str] in
  if t.dry_run then
    Log.info (fun m -> m "Would run: git %s" (String.concat " " args))
  else
    try Eioext.Process.run ~process_mgr:t.env#process_mgr ("git" :: args)
    with
    | Eio.Io (Eio.Process.E (Eio.Process.Child_error status), _) ->
        raise_git_error ~context:"cloning repository"
          (Option.get (process_error status))
    | Eio.Io (Eio.Process.E (Eio.Process.Executable_not_found _), _) ->
        raise_git_error ~context:"cloning repository" Command_not_found

(** {1 Sync} *)

module Sync = struct
  (** {2 Configuration} *)

  module Config = struct
    type t = {
      remote : string;
      branch : string;
      auto_commit : bool;
      commit_message : string;
    }

    let default = {
      remote = "";
      branch = "main";
      auto_commit = true;
      commit_message = "sync";
    }

    let codec =
      let open Tomlt in
      let open Tomlt.Table in
      obj (fun remote branch auto_commit commit_message ->
        { remote; branch; auto_commit; commit_message })
      |> mem "remote" string ~dec_absent:default.remote ~enc:(fun t -> t.remote)
      |> mem "branch" string ~dec_absent:default.branch ~enc:(fun t -> t.branch)
      |> mem "auto_commit" bool ~dec_absent:default.auto_commit ~enc:(fun t -> t.auto_commit)
      |> mem "commit_message" string ~dec_absent:default.commit_message ~enc:(fun t -> t.commit_message)
      |> finish

    let pp ppf t =
      Fmt.pf ppf "@[<v>remote: %s@,branch: %s@,auto_commit: %b@,commit_message: %s@]"
        t.remote t.branch t.auto_commit t.commit_message
  end

  (** {2 Sync Result} *)

  type result = {
    pulled : bool;
    pushed : bool;
  }

  let pp_result ppf r =
    Fmt.pf ppf "pulled=%b pushed=%b" r.pulled r.pushed

  (** {2 Helpers} *)

  let ensure_repo t ~config ~repo =
    let open Config in
    (* Ensure directory exists *)
    let dir_exists =
      match Eio.Path.stat ~follow:false repo with
      | _ -> true
      | exception Eio.Io (Eio.Fs.E (Eio.Fs.Not_found _), _) -> false
    in
    if not dir_exists then begin
      Log.info (fun m -> m "Creating directory %s" (Eio.Path.native_exn repo));
      if not t.dry_run then
        Eio.Path.mkdirs ~exists_ok:true ~perm:0o755 repo
    end;

    (* Ensure repo exists *)
    if not (is_repo t ~repo) then begin
      Log.info (fun m -> m "Initializing git repository");
      init t ~repo
    end;

    (* Ensure remote is configured *)
    begin match (if t.dry_run && not (is_repo t ~repo) then None
                 else remote_url t ~repo ~remote:"origin") with
    | None ->
        Log.info (fun m -> m "Adding remote origin -> %s" config.remote);
        remote_add t ~repo ~name:"origin" ~url:config.remote
    | Some url when url <> config.remote ->
        Log.warn (fun m -> m "Updating remote URL: %s -> %s" url config.remote);
        remote_set_url t ~repo ~name:"origin" ~url:config.remote
    | Some _ -> ()
    end

  (** {2 Pull} *)

  let pull t ~config ~repo =
    let open Config in
    Log.info (fun m -> m "Pulling %s from %s"
      (Eio.Path.native_exn repo) config.remote);

    ensure_repo t ~config ~repo;

    (* Fetch from remote *)
    Log.info (fun m -> m "Fetching from origin");
    fetch t ~repo ~remote:"origin";

    (* Check if we need to merge *)
    let remote_ref = Printf.sprintf "origin/%s" config.branch in
    let local_head = if t.dry_run && not (is_repo t ~repo) then None
      else rev_parse_opt t ~repo "HEAD" in
    let remote_head = if t.dry_run && not (is_repo t ~repo) then None
      else rev_parse_opt t ~repo remote_ref in

    match local_head, remote_head with
    | Some local, Some remote when local <> remote ->
        Log.info (fun m -> m "Merging %s" remote_ref);
        merge t ~repo ~ref_:remote_ref;
        true
    | None, Some _ ->
        Log.info (fun m -> m "Merging %s" remote_ref);
        merge t ~repo ~ref_:remote_ref;
        true
    | _, None ->
        Log.debug (fun m -> m "No remote branch yet");
        false
    | Some local, Some remote when local = remote ->
        Log.debug (fun m -> m "Already up to date");
        false
    | _ -> false

  (** {2 Push} *)

  (* Alias to avoid shadowing by Sync.push *)
  let git_push = push
  let git_push_set_upstream = push_set_upstream

  let push t ~config ?msg ~repo () =
    let open Config in
    Log.info (fun m -> m "Pushing %s to %s"
      (Eio.Path.native_exn repo) config.remote);

    ensure_repo t ~config ~repo;

    (* Auto-commit local changes *)
    if config.auto_commit then begin
      match (if t.dry_run && not (is_repo t ~repo) then `Clean
             else status t ~repo) with
      | `Dirty ->
          let commit_msg = match msg with Some m -> m | None -> config.commit_message in
          Log.info (fun m -> m "Committing local changes");
          add_all t ~repo;
          commit t ~repo ~msg:commit_msg
      | `Clean ->
          Log.debug (fun m -> m "Working tree clean")
    end;

    (* Fetch to update remote tracking refs *)
    fetch t ~repo ~remote:"origin";

    (* Push if needed *)
    let remote_ref = Printf.sprintf "origin/%s" config.branch in
    let current_head = if t.dry_run && not (is_repo t ~repo) then None
      else rev_parse_opt t ~repo "HEAD" in
    let remote_head = if t.dry_run && not (is_repo t ~repo) then None
      else rev_parse_opt t ~repo remote_ref in

    match current_head, remote_head with
    | Some current, Some remote when current <> remote ->
        Log.info (fun m -> m "Pushing to origin");
        git_push t ~repo ~remote:"origin";
        true
    | Some _, None ->
        Log.info (fun m -> m "Pushing to origin (first push)");
        git_push_set_upstream t ~repo ~remote:"origin" ~branch:config.branch;
        true
    | _ ->
        Log.debug (fun m -> m "Nothing to push");
        false

  (** {2 Run Sync} *)

  let run t ~config ~repo =
    Log.info (fun m -> m "Syncing %s with %s"
      (Eio.Path.native_exn repo) config.Config.remote);
    let pulled = pull t ~config ~repo in
    let pushed = push t ~config ~repo () in
    { pulled; pushed }

  (** {2 Cmdliner Integration} *)

  module Cmd = struct
    open Cmdliner

    let dry_run_term =
      let doc = "Show what would be done without making changes." in
      Arg.(value & flag & info ["dry-run"; "n"] ~doc)

    let verbose_term =
      let doc = "Enable verbose logging of git operations." in
      Arg.(value & flag & info ["verbose"; "v"] ~doc)

    let setup_term =
      let setup dry_run verbose =
        Fmt_tty.setup_std_outputs ();
        let level = if verbose then Some Logs.Debug else Some Logs.Info in
        Logs.set_level level;
        Logs.set_reporter (Logs_fmt.reporter ());
        dry_run
      in
      Term.(const setup $ dry_run_term $ verbose_term)

    let remote_term =
      let doc = "Override sync remote URL." in
      Arg.(value & opt (some string) None & info ["remote"] ~docv:"URL" ~doc)

    let sync_info =
      let doc = "Sync data with remote git repository." in
      let man = [
        `S Manpage.s_description;
        `P "Synchronizes the local data directory with a remote git repository.";
        `P "The sync process:";
        `P "1. Fetches from the remote";
        `P "2. Merges any remote changes";
        `P "3. Commits local changes (if auto_commit is enabled)";
        `P "4. Pushes to the remote";
        `P "Use $(b,--dry-run) to see what would be done without making changes.";
      ] in
      Cmdliner.Cmd.info "sync" ~doc ~man
  end
end
