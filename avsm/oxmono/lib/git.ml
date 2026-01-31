(*---------------------------------------------------------------------------
  Copyright (c) 2025 Anil Madhavapeddy <anil@recoil.org>. All rights reserved.
  SPDX-License-Identifier: ISC
 ---------------------------------------------------------------------------*)

(** Git operations for cloning and updating sources *)

module Log = (val Logs.src_log (Logs.Src.create "oxmono.git") : Logs.LOG)

let clone ~env ~url ~target =
  let target_path = Eio.Path.native_exn target in
  Log.info (fun m -> m "Cloning %s to %s" url target_path);
  Process.run ~env ["git"; "clone"; url; target_path]

let fetch ~env ~cwd =
  Log.info (fun m -> m "Fetching in %s" (Eio.Path.native_exn cwd));
  Process.run ~env ~cwd ["git"; "fetch"; "--all"]

let checkout ~env ~cwd ~ref_ =
  Log.info (fun m -> m "Checking out %s in %s" ref_ (Eio.Path.native_exn cwd));
  Process.run ~env ~cwd ["git"; "checkout"; ref_]

let clone_or_update ~env ~url ~target ~commit =
  if Eio.Path.is_directory target then begin
    Log.info (fun m -> m "Repository exists at %s, updating" (Eio.Path.native_exn target));
    match fetch ~env ~cwd:target with
    | Error e -> Error e
    | Ok () -> checkout ~env ~cwd:target ~ref_:commit
  end else begin
    match clone ~env ~url ~target with
    | Error e -> Error e
    | Ok () -> checkout ~env ~cwd:target ~ref_:commit
  end

let init ~env ~cwd =
  Process.run ~env ~cwd ["git"; "init"]

let add ~env ~cwd paths =
  Process.run ~env ~cwd (["git"; "add"] @ paths)

let commit ~env ~cwd ~message =
  Process.run ~env ~cwd ["git"; "commit"; "-m"; message]

let commit_allow_empty ~env ~cwd ~message =
  Process.run ~env ~cwd ["git"; "commit"; "--allow-empty"; "-m"; message]

let branch_exists ~env ~cwd ~branch =
  match Process.run ~env ~cwd ["git"; "rev-parse"; "--verify"; branch] with
  | Ok () -> true
  | Error _ -> false

let checkout_orphan ~env ~cwd ~branch =
  Process.run ~env ~cwd ["git"; "checkout"; "--orphan"; branch]

let checkout_new ~env ~cwd ~branch =
  Process.run ~env ~cwd ["git"; "checkout"; "-b"; branch]
