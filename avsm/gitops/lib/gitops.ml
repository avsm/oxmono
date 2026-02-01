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
