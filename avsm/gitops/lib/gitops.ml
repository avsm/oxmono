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

let run_git_raw t ~repo args =
  let repo_str = Eio.Path.native_exn repo in
  let cmd = "git" :: "-C" :: repo_str :: args in
  Log.debug (fun m -> m "Running: %s" (String.concat " " cmd));
  Eio.Switch.run @@ fun sw ->
  let mgr = t.env#process_mgr in
  try
    let proc = Eio.Process.spawn ~sw mgr cmd in
    match Eio.Process.await proc with
    | `Exited 0 -> Ok ()
    | `Exited n -> Error (Exit_code n)
    | `Signaled n -> Error (Signaled n)
  with
  | Eio.Io _ as exn -> raise exn
  | exn ->
      let msg = Printexc.to_string exn in
      if String.length msg >= 9 && String.sub msg 0 9 = "not found" then
        Error Command_not_found
      else if String.length msg >= 7 && String.sub msg 0 7 = "No such" then
        Error Command_not_found
      else
        raise exn

let run_git_output t ~repo args =
  let repo_str = Eio.Path.native_exn repo in
  let cmd = "git" :: "-C" :: repo_str :: args in
  Log.debug (fun m -> m "Running: %s" (String.concat " " cmd));
  Eio.Switch.run @@ fun sw ->
  let mgr = t.env#process_mgr in
  try
    let stdout_r, stdout_w = Eio.Process.pipe ~sw mgr in
    let proc = Eio.Process.spawn ~sw ~stdout:stdout_w mgr cmd in
    Eio.Flow.close stdout_w;
    let output = Eio.Buf_read.of_flow ~max_size:max_int stdout_r
                 |> Eio.Buf_read.take_all in
    match Eio.Process.await proc with
    | `Exited 0 -> Ok (String.trim output)
    | `Exited n -> Error (Exit_code n)
    | `Signaled n -> Error (Signaled n)
  with
  | Eio.Io _ as exn -> raise exn
  | exn ->
      let msg = Printexc.to_string exn in
      if String.length msg >= 9 && String.sub msg 0 9 = "not found" then
        Error Command_not_found
      else if String.length msg >= 7 && String.sub msg 0 7 = "No such" then
        Error Command_not_found
      else
        raise exn

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
    ["rev-parse"; ref_]

let rev_parse_opt t ~repo ref_ =
  match run_git_output t ~repo ["rev-parse"; "--verify"; "--quiet"; ref_] with
  | Ok output -> Some (String.trim output)
  | Error _ -> None

let status t ~repo =
  match run_git_output t ~repo ["status"; "--porcelain"] with
  | Ok "" -> `Clean
  | Ok _ -> `Dirty
  | Error _ -> `Dirty  (* Conservative: assume dirty on error *)

let remote_url t ~repo ~remote =
  match run_git_output t ~repo ["remote"; "get-url"; remote] with
  | Ok url -> Some (String.trim url)
  | Error _ -> None

let branch_exists t ~repo branch =
  match run_git_output t ~repo ["rev-parse"; "--verify"; "--quiet"; "refs/heads/" ^ branch] with
  | Ok _ -> true
  | Error _ -> false

let current_branch t ~repo =
  match run_git_output t ~repo ["rev-parse"; "--abbrev-ref"; "HEAD"] with
  | Ok branch -> Some (String.trim branch)
  | Error _ -> None
