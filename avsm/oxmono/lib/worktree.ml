(*---------------------------------------------------------------------------
  Copyright (c) 2025 Anil Madhavapeddy <anil@recoil.org>. All rights reserved.
  SPDX-License-Identifier: ISC
 ---------------------------------------------------------------------------*)

(** Orphan branch worktree management for pristine copies *)

module Log = (val Logs.src_log (Logs.Src.create "oxmono.worktree") : Logs.LOG)

let sources_branch = "sources"
let worktree_name = "oxmono-src"

let worktree_path ~root =
  (* Worktree is at ../oxmono-src relative to the monorepo root *)
  Filename.concat (Filename.dirname root) worktree_name

let ensure_sources_branch ~env ~cwd =
  if Git.branch_exists ~env ~cwd ~branch:sources_branch then begin
    Log.debug (fun m -> m "Branch %s already exists" sources_branch);
    Ok ()
  end else begin
    Log.info (fun m -> m "Creating orphan branch %s" sources_branch);
    (* Save current branch *)
    match Process.run_with_output ~env ~cwd ["git"; "rev-parse"; "--abbrev-ref"; "HEAD"] with
    | Error e -> Error e
    | Ok current_branch ->
      let current_branch = String.trim current_branch in
      (* Create orphan branch *)
      match Git.checkout_orphan ~env ~cwd ~branch:sources_branch with
      | Error e -> Error e
      | Ok () ->
        (* Reset index and create initial empty commit *)
        match Process.run ~env ~cwd ["git"; "rm"; "-rf"; "--cached"; "."] with
        | Error (`Exit_code 128) -> (* No files to remove, that's ok *)
          (match Git.commit_allow_empty ~env ~cwd ~message:"Initialize sources branch" with
          | Error e -> Error e
          | Ok () ->
            (* Return to original branch *)
            Git.checkout ~env ~cwd ~ref_:current_branch)
        | Error e -> Error e
        | Ok () ->
          match Git.commit_allow_empty ~env ~cwd ~message:"Initialize sources branch" with
          | Error e -> Error e
          | Ok () ->
            (* Return to original branch *)
            Git.checkout ~env ~cwd ~ref_:current_branch
  end

let ensure_worktree ~env ~cwd =
  let root = Eio.Path.native_exn cwd in
  let wt_path = worktree_path ~root in
  if Sys.file_exists wt_path && Sys.is_directory wt_path then begin
    Log.debug (fun m -> m "Worktree already exists at %s" wt_path);
    Ok wt_path
  end else begin
    Log.info (fun m -> m "Creating worktree at %s for branch %s" wt_path sources_branch);
    match ensure_sources_branch ~env ~cwd with
    | Error e -> Error e
    | Ok () ->
      match Process.run ~env ~cwd ["git"; "worktree"; "add"; wt_path; sources_branch] with
      | Error e -> Error e
      | Ok () -> Ok wt_path
  end

let copy_to_worktree ~env ~cwd ~package_name ~source_dir =
  match ensure_worktree ~env ~cwd with
  | Error e -> Error e
  | Ok wt_path ->
    let source_path = Eio.Path.native_exn source_dir in
    let dest_path = Filename.concat wt_path package_name in
    Log.info (fun m -> m "Copying %s to worktree at %s" source_path dest_path);
    (* Remove existing destination if present *)
    let _ = Process.run ~env ["rm"; "-rf"; dest_path] in
    (* Copy source to worktree *)
    match Process.run ~env ["cp"; "-r"; source_path; dest_path] with
    | Error e -> Error e
    | Ok () ->
      (* Commit changes in worktree *)
      let wt_cwd = Eio.Path.(Eio.Stdenv.fs env / wt_path) in
      match Git.add ~env ~cwd:wt_cwd [package_name] with
      | Error e -> Error e
      | Ok () ->
        let message = Printf.sprintf "Update %s from upstream" package_name in
        match Git.commit ~env ~cwd:wt_cwd ~message with
        | Error (`Exit_code 1) ->
          (* No changes to commit, that's ok *)
          Log.info (fun m -> m "No changes to commit for %s" package_name);
          Ok ()
        | Error e -> Error e
        | Ok () -> Ok ()
