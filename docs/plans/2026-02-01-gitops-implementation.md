# Gitops Library Implementation Plan

> **For Claude:** REQUIRED SUB-SKILL: Use superpowers:executing-plans to implement this plan task-by-task.

**Goal:** Create `avsm/gitops` library for git-based data sync with dry-run support, then integrate with bushel.

**Architecture:** Context-based API where `Gitops.t` carries Eio resources and dry-run flag. All operations use `Eio.Process` for execution with `Eio.Exn.reraise_with_context` for error handling. Query operations always execute; mutating operations log in dry-run mode.

**Tech Stack:** OCaml 5, Eio, Tomlt, Cmdliner, Logs

---

### Task 1: Create gitops library skeleton

**Files:**
- Create: `avsm/gitops/dune-project`
- Create: `avsm/gitops/gitops.opam`
- Create: `avsm/gitops/lib/dune`
- Create: `avsm/gitops/lib/gitops.ml`

**Step 1: Create dune-project**

```ocaml
(lang dune 3.21)
(name gitops)

(generate_opam_files true)

(license ISC)
(authors "Anil Madhavapeddy <anil@recoil.org>")
(maintainers "Anil Madhavapeddy <anil@recoil.org>")
(source (github avsm/oxmono))

(package
 (name gitops)
 (synopsis "Git operations library with Eio and dry-run support")
 (description
  "Gitops provides a context-based API for git operations using Eio.
   Features include dry-run mode for previewing changes, structured
   error handling with Eio.Io context, and reusable sync configuration
   with Cmdliner integration.")
 (depends
  (ocaml (>= 5.2))
  (eio (>= 1.2))
  (eio_main (>= 1.2))
  (tomlt (>= 0.1))
  (cmdliner (>= 1.3))
  (logs (>= 0.7))
  (fmt (>= 0.9))
  (odoc :with-doc)))
```

**Step 2: Create lib/dune**

```dune
(library
 (name gitops)
 (public_name gitops)
 (libraries eio eio_main tomlt cmdliner logs fmt))
```

**Step 3: Create lib/gitops.ml (initial stub)**

```ocaml
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
  proc_mgr : Eio.Process.mgr_ty Eio.Resource.t;
  fs : Eio.Fs.dir_ty Eio.Path.t;
  dry_run : bool;
}

let v ~dry_run (env : Eio_unix.Stdenv.base) =
  { proc_mgr = env#process_mgr; fs = env#fs; dry_run }

let dry_run t = t.dry_run

(** {1 Stub Operations} *)

(* TODO: Implement in subsequent tasks *)
```

**Step 4: Verify it builds**

Run: `dune build avsm/gitops`
Expected: Build succeeds

**Step 5: Commit**

```bash
git add avsm/gitops
git commit -m "feat(gitops): add library skeleton with error types

Initial structure for gitops library:
- dune-project with dependencies
- Error types registered with Eio.Exn
- Context type with dry_run flag"
```

---

### Task 2: Implement core git execution

**Files:**
- Modify: `avsm/gitops/lib/gitops.ml`

**Step 1: Add internal run_git function**

Add after the context type:

```ocaml
(** {1 Internal Execution} *)

let src = Logs.Src.create "gitops" ~doc:"Git operations"
module Log = (val Logs.src_log src : Logs.LOG)

let run_git_raw t ~repo args =
  let repo_str = Eio.Path.native_exn repo in
  let cmd = "git" :: "-C" :: repo_str :: args in
  Log.debug (fun m -> m "Running: %s" (String.concat " " cmd));
  try
    let proc = Eio.Process.spawn t.proc_mgr cmd in
    match Eio.Process.await proc with
    | `Exited 0 -> Ok ()
    | `Exited n -> Error (Exit_code n)
    | `Signaled n -> Error (Signaled n)
  with
  | Eio.Io _ as exn -> raise exn
  | exn ->
      let msg = Printexc.to_string exn in
      if String.length msg > 0 &&
         (String.sub msg 0 (min 9 (String.length msg)) = "not found" ||
          String.sub msg 0 (min 7 (String.length msg)) = "No such") then
        Error Command_not_found
      else
        raise exn

let run_git_output t ~repo args =
  let repo_str = Eio.Path.native_exn repo in
  let cmd = "git" :: "-C" :: repo_str :: args in
  Log.debug (fun m -> m "Running: %s" (String.concat " " cmd));
  try
    let buf = Buffer.create 256 in
    let proc = Eio.Process.spawn t.proc_mgr cmd
        ~stdout:(Eio.Flow.buffer_sink buf) in
    match Eio.Process.await proc with
    | `Exited 0 -> Ok (Buffer.contents buf |> String.trim)
    | `Exited n -> Error (Exit_code n)
    | `Signaled n -> Error (Signaled n)
  with
  | Eio.Io _ as exn -> raise exn
  | exn ->
      let msg = Printexc.to_string exn in
      if String.length msg > 0 &&
         (String.sub msg 0 (min 9 (String.length msg)) = "not found" ||
          String.sub msg 0 (min 7 (String.length msg)) = "No such") then
        Error Command_not_found
      else
        raise exn

let raise_git_error ~context err =
  let exn = Eio.Io (Git err, Eio.Exn.empty_context) in
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
```

**Step 2: Verify it builds**

Run: `dune build avsm/gitops`
Expected: Build succeeds

**Step 3: Commit**

```bash
git add avsm/gitops/lib/gitops.ml
git commit -m "feat(gitops): add internal git execution with error handling

- run_git_raw: execute git command, return Result
- run_git_output: execute and capture stdout
- run_git: execute with Eio.Exn context on error
- Logs integration for debug output"
```

---

### Task 3: Implement query operations

**Files:**
- Modify: `avsm/gitops/lib/gitops.ml`

**Step 1: Add query operations**

Add after the internal execution section:

```ocaml
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
```

**Step 2: Verify it builds**

Run: `dune build avsm/gitops`
Expected: Build succeeds

**Step 3: Commit**

```bash
git add avsm/gitops/lib/gitops.ml
git commit -m "feat(gitops): add query operations

Query operations that always execute (even in dry-run):
- is_repo: check if path is a git repository
- rev_parse: resolve ref to commit hash
- rev_parse_opt: optional version
- status: check if working tree is clean/dirty
- remote_url: get URL for named remote
- branch_exists: check if branch exists
- current_branch: get current branch name"
```

---

### Task 4: Implement mutating operations

**Files:**
- Modify: `avsm/gitops/lib/gitops.ml`

**Step 1: Add mutating operations**

Add after query operations:

```ocaml
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
  else begin
    match run_git_raw t ~repo args with
    | Ok () -> ()
    | Error (Exit_code 1) ->
        (* Exit code 1 often means nothing to commit *)
        Log.debug (fun m -> m "Nothing to commit")
    | Error err ->
        raise_git_error ~context:"committing changes" err
  end

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
  else begin
    let cmd = "git" :: args in
    Log.debug (fun m -> m "Running: %s" (String.concat " " cmd));
    let proc = Eio.Process.spawn t.proc_mgr cmd in
    match Eio.Process.await proc with
    | `Exited 0 -> ()
    | `Exited n -> raise_git_error ~context:(Printf.sprintf "cloning %s" url) (Exit_code n)
    | `Signaled n -> raise_git_error ~context:(Printf.sprintf "cloning %s" url) (Signaled n)
  end
```

**Step 2: Verify it builds**

Run: `dune build avsm/gitops`
Expected: Build succeeds

**Step 3: Commit**

```bash
git add avsm/gitops/lib/gitops.ml
git commit -m "feat(gitops): add mutating operations with dry-run support

Mutating operations that log in dry-run mode:
- init: initialize repository
- fetch: fetch from remote
- pull: pull from remote
- merge: merge ref
- add/add_all: stage files
- commit: create commit
- push/push_set_upstream: push to remote
- remote_add/remote_set_url: manage remotes
- clone: clone repository"
```

---

### Task 5: Implement sync config

**Files:**
- Modify: `avsm/gitops/lib/gitops.ml`

**Step 1: Add Sync module with config**

Add at end of file:

```ocaml
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
```

**Step 2: Add sync run function**

Continue in the Sync module:

```ocaml
  (** {2 Run Sync} *)

  let run t ~config ~repo =
    let open Config in
    Log.info (fun m -> m "Syncing %s with %s"
      (Eio.Path.native_exn repo) config.remote);

    (* Ensure repo exists *)
    if not (is_repo t ~repo) then begin
      Log.info (fun m -> m "Initializing git repository");
      init t ~repo
    end;

    (* Ensure remote is configured *)
    begin match remote_url t ~repo ~remote:"origin" with
    | None ->
        Log.info (fun m -> m "Adding remote origin -> %s" config.remote);
        remote_add t ~repo ~name:"origin" ~url:config.remote
    | Some url when url <> config.remote ->
        Log.warn (fun m -> m "Updating remote URL: %s -> %s" url config.remote);
        remote_set_url t ~repo ~name:"origin" ~url:config.remote
    | Some _ -> ()
    end;

    (* Fetch from remote *)
    Log.info (fun m -> m "Fetching from origin");
    (try fetch t ~repo ~remote:"origin" with
     | Eio.Io (Git (Exit_code 128), _) ->
         (* Remote might not exist yet, that's OK for first push *)
         Log.debug (fun m -> m "Fetch failed (remote may not exist yet)"));

    (* Check if we need to pull *)
    let remote_ref = Printf.sprintf "origin/%s" config.branch in
    let local_head = rev_parse_opt t ~repo "HEAD" in
    let remote_head = rev_parse_opt t ~repo remote_ref in

    let pulled = match local_head, remote_head with
      | Some local, Some remote when local <> remote ->
          Log.info (fun m -> m "Merging %s" remote_ref);
          merge t ~repo ~ref_:remote_ref;
          true
      | None, Some _ ->
          (* No local commits, remote exists - this shouldn't happen normally *)
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
    in

    (* Auto-commit local changes *)
    if config.auto_commit then begin
      match status t ~repo with
      | `Dirty ->
          Log.info (fun m -> m "Committing local changes");
          add_all t ~repo;
          commit t ~repo ~msg:config.commit_message
      | `Clean ->
          Log.debug (fun m -> m "Working tree clean")
    end;

    (* Push *)
    let current_head = rev_parse_opt t ~repo "HEAD" in
    let pushed = match current_head, remote_head with
      | Some current, Some remote when current <> remote ->
          Log.info (fun m -> m "Pushing to origin");
          push t ~repo ~remote:"origin";
          true
      | Some _, None ->
          Log.info (fun m -> m "Pushing to origin (first push)");
          push_set_upstream t ~repo ~remote:"origin" ~branch:config.branch;
          true
      | _ ->
          Log.debug (fun m -> m "Nothing to push");
          false
    in

    { pulled; pushed }
end
```

**Step 3: Verify it builds**

Run: `dune build avsm/gitops`
Expected: Build succeeds

**Step 4: Commit**

```bash
git add avsm/gitops/lib/gitops.ml
git commit -m "feat(gitops): add Sync module with config and run

Sync module provides:
- Config type with TOML codec for [sync] section
- run function: fetch -> merge -> auto-commit -> push
- Handles first-time setup (init, add remote)
- Returns result indicating what changed"
```

---

### Task 6: Add Cmdliner integration

**Files:**
- Modify: `avsm/gitops/lib/gitops.ml`

**Step 1: Add Cmd submodule to Sync**

Add inside Sync module, after the run function:

```ocaml
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
```

**Step 2: Verify it builds**

Run: `dune build avsm/gitops`
Expected: Build succeeds

**Step 3: Commit**

```bash
git add avsm/gitops/lib/gitops.ml
git commit -m "feat(gitops): add Cmdliner integration for sync command

Sync.Cmd module provides reusable Cmdliner terms:
- dry_run_term: --dry-run/-n flag
- verbose_term: --verbose/-v flag
- setup_term: combined setup (logging + returns dry_run)
- remote_term: --remote URL override
- sync_info: command info with manpage"
```

---

### Task 7: Add .mli interface file

**Files:**
- Create: `avsm/gitops/lib/gitops.mli`

**Step 1: Create interface file**

```ocaml
(*---------------------------------------------------------------------------
  Copyright (c) 2025 Anil Madhavapeddy <anil@recoil.org>. All rights reserved.
  SPDX-License-Identifier: ISC
 ---------------------------------------------------------------------------*)

(** Git operations library with Eio and dry-run support.

    Gitops provides a context-based API for git operations. The context
    carries Eio resources and a dry-run flag. In dry-run mode, mutating
    operations log what they would do instead of executing.

    {2 Basic Usage}

    {[
      Eio_main.run @@ fun env ->
      let git = Gitops.v ~dry_run:false env in
      let repo = Eio.Path.(env#fs / "/path/to/repo") in
      Gitops.fetch git ~repo ~remote:"origin";
      let head = Gitops.rev_parse git ~repo "HEAD" in
      Printf.printf "HEAD: %s\n" head
    ]}

    {2 Sync Usage}

    {[
      let config = { Gitops.Sync.Config.default with
        remote = "ssh://server/repo.git" } in
      let result = Gitops.Sync.run git ~config ~repo in
      if result.pulled then print_endline "Pulled changes"
    ]} *)

(** {1 Error Types} *)

type git_error =
  | Exit_code of int      (** Git exited with non-zero code *)
  | Signaled of int       (** Git killed by signal *)
  | Not_a_repo            (** Path is not a git repository *)
  | No_remote of string   (** Named remote does not exist *)
  | Merge_conflict        (** Merge conflict occurred *)
  | Nothing_to_commit     (** No changes to commit *)
  | Push_rejected         (** Push was rejected by remote *)
  | Command_not_found     (** Git executable not found *)

type Eio.Exn.err += Git of git_error
(** Eio exception for git errors. Raised with context via
    {!Eio.Exn.reraise_with_context}. *)

(** {1 Context} *)

type t
(** Git operations context carrying Eio resources and dry-run flag. *)

val v : dry_run:bool -> Eio_unix.Stdenv.base -> t
(** [v ~dry_run env] creates a git context from an Eio environment.
    If [dry_run] is true, mutating operations will log instead of executing. *)

val dry_run : t -> bool
(** [dry_run t] returns whether this context is in dry-run mode. *)

(** {1 Query Operations}

    These operations always execute, even in dry-run mode, since
    subsequent control flow may depend on their results. *)

val is_repo : t -> repo:Eio.Fs.dir_ty Eio.Path.t -> bool
(** [is_repo t ~repo] returns [true] if [repo] is a git repository. *)

val rev_parse : t -> repo:Eio.Fs.dir_ty Eio.Path.t -> string -> string
(** [rev_parse t ~repo ref] resolves [ref] to a commit hash.
    @raise Eio.Io on failure *)

val rev_parse_opt : t -> repo:Eio.Fs.dir_ty Eio.Path.t -> string -> string option
(** [rev_parse_opt t ~repo ref] resolves [ref] to a commit hash, or [None]. *)

val status : t -> repo:Eio.Fs.dir_ty Eio.Path.t -> [`Clean | `Dirty]
(** [status t ~repo] checks if the working tree has uncommitted changes. *)

val remote_url : t -> repo:Eio.Fs.dir_ty Eio.Path.t -> remote:string -> string option
(** [remote_url t ~repo ~remote] returns the URL for [remote], or [None]. *)

val branch_exists : t -> repo:Eio.Fs.dir_ty Eio.Path.t -> string -> bool
(** [branch_exists t ~repo branch] checks if [branch] exists. *)

val current_branch : t -> repo:Eio.Fs.dir_ty Eio.Path.t -> string option
(** [current_branch t ~repo] returns the current branch name, or [None] if detached. *)

(** {1 Mutating Operations}

    These operations log what they would do in dry-run mode instead of executing. *)

val init : t -> repo:Eio.Fs.dir_ty Eio.Path.t -> unit
(** [init t ~repo] initializes a git repository at [repo]. *)

val fetch : t -> repo:Eio.Fs.dir_ty Eio.Path.t -> remote:string -> unit
(** [fetch t ~repo ~remote] fetches from [remote]. *)

val pull : t -> repo:Eio.Fs.dir_ty Eio.Path.t -> remote:string -> unit
(** [pull t ~repo ~remote] pulls from [remote]. *)

val merge : t -> repo:Eio.Fs.dir_ty Eio.Path.t -> ref_:string -> unit
(** [merge t ~repo ~ref_] merges [ref_] into the current branch. *)

val add : t -> repo:Eio.Fs.dir_ty Eio.Path.t -> paths:string list -> unit
(** [add t ~repo ~paths] stages [paths] for commit. *)

val add_all : t -> repo:Eio.Fs.dir_ty Eio.Path.t -> unit
(** [add_all t ~repo] stages all changes (git add -A). *)

val commit : t -> repo:Eio.Fs.dir_ty Eio.Path.t -> msg:string -> unit
(** [commit t ~repo ~msg] creates a commit with [msg]. *)

val push : t -> repo:Eio.Fs.dir_ty Eio.Path.t -> remote:string -> unit
(** [push t ~repo ~remote] pushes to [remote]. *)

val push_set_upstream : t -> repo:Eio.Fs.dir_ty Eio.Path.t -> remote:string -> branch:string -> unit
(** [push_set_upstream t ~repo ~remote ~branch] pushes and sets upstream. *)

val remote_add : t -> repo:Eio.Fs.dir_ty Eio.Path.t -> name:string -> url:string -> unit
(** [remote_add t ~repo ~name ~url] adds a new remote. *)

val remote_set_url : t -> repo:Eio.Fs.dir_ty Eio.Path.t -> name:string -> url:string -> unit
(** [remote_set_url t ~repo ~name ~url] changes the URL of an existing remote. *)

val clone : t -> url:string -> target:Eio.Fs.dir_ty Eio.Path.t -> unit
(** [clone t ~url ~target] clones a repository. *)

(** {1 Sync} *)

module Sync : sig
  (** High-level sync operation: fetch -> merge -> auto-commit -> push *)

  (** {2 Configuration} *)

  module Config : sig
    type t = {
      remote : string;          (** Remote URL (git-over-ssh) *)
      branch : string;          (** Branch name (default: "main") *)
      auto_commit : bool;       (** Commit local changes before push (default: true) *)
      commit_message : string;  (** Commit message (default: "sync") *)
    }

    val default : t
    (** Default configuration with empty remote. *)

    val codec : t Tomlt.Table.obj_codec
    (** TOML codec for embedding in [sync] config section. *)

    val pp : t Fmt.t
    (** Pretty-printer for configuration. *)
  end

  (** {2 Result} *)

  type result = {
    pulled : bool;  (** True if changes were pulled from remote *)
    pushed : bool;  (** True if changes were pushed to remote *)
  }

  val pp_result : result Fmt.t
  (** Pretty-printer for sync result. *)

  (** {2 Run} *)

  val run : t -> config:Config.t -> repo:Eio.Fs.dir_ty Eio.Path.t -> result
  (** [run t ~config ~repo] performs a full sync operation:
      1. Initialize repo if needed
      2. Configure remote if needed
      3. Fetch and merge remote changes
      4. Auto-commit local changes (if enabled)
      5. Push to remote *)

  (** {2 Cmdliner Integration} *)

  module Cmd : sig
    open Cmdliner

    val dry_run_term : bool Term.t
    (** [--dry-run] / [-n] flag term. *)

    val verbose_term : bool Term.t
    (** [--verbose] / [-v] flag term. *)

    val setup_term : bool Term.t
    (** Combined setup term that configures logging and returns dry_run flag. *)

    val remote_term : string option Term.t
    (** [--remote URL] override term. *)

    val sync_info : Cmd.info
    (** Command info for "sync" subcommand with manpage. *)
  end
end
```

**Step 2: Verify it builds**

Run: `dune build avsm/gitops`
Expected: Build succeeds

**Step 3: Commit**

```bash
git add avsm/gitops/lib/gitops.mli
git commit -m "feat(gitops): add public interface with documentation

Complete .mli with:
- Error type documentation
- Context API
- Query operations (always execute)
- Mutating operations (log in dry-run)
- Sync module with Config, result, and Cmdliner integration
- Usage examples in module docstring"
```

---

### Task 8: Integrate gitops with bushel config

**Files:**
- Modify: `avsm/bushel/lib_config/bushel_config.ml`
- Modify: `avsm/bushel/lib_config/dune`

**Step 1: Add gitops dependency to bushel.config**

In `avsm/bushel/lib_config/dune`, add gitops to libraries:

```dune
(library
 (name bushel_config)
 (public_name bushel.config)
 (libraries tomlt tomlt.bytesrw unix gitops))
```

**Step 2: Add sync field to bushel config type**

In `avsm/bushel/lib_config/bushel_config.ml`, modify the type:

```ocaml
type t = {
  (* Data paths *)
  data_dir : string;

  (* ... existing fields ... *)

  (* Sync configuration *)
  sync : Gitops.Sync.Config.t;
}
```

**Step 3: Update default function**

```ocaml
let default () =
  let home = Sys.getenv_opt "HOME" |> Option.value ~default:"." in
  {
    data_dir = Filename.concat home "bushel/data";
    (* ... existing defaults ... *)
    sync = Gitops.Sync.Config.default;
  }
```

**Step 4: Update config_codec**

Add the sync field to the codec:

```ocaml
let config_codec =
  let default = default () in
  let open Tomlt.Table in
  obj (fun data_dir images papers immich peertube typesense zotero sync ->
    (* ... existing field extraction ... *)
    {
      (* ... existing fields ... *)
      sync;
    })
  |> (* ... existing mems ... *)
  |> mem "sync" Gitops.Sync.Config.codec
       ~dec_absent:Gitops.Sync.Config.default
       ~enc:(fun c -> c.sync)
  |> finish
```

**Step 5: Update default_config_toml**

Add sync section to the generated config template:

```ocaml
let default_config_toml () =
  (* ... existing content ... *)
  Printf.sprintf {|# Bushel Configuration
# ... existing sections ...

# Git sync configuration
# Sync your bushel data to a remote git repository
[sync]
remote = ""
branch = "main"
auto_commit = true
commit_message = "sync"
|} (* ... existing format args ... *)
```

**Step 6: Verify it builds**

Run: `dune build avsm/bushel`
Expected: Build succeeds

**Step 7: Commit**

```bash
git add avsm/bushel/lib_config/dune avsm/bushel/lib_config/bushel_config.ml
git commit -m "feat(bushel): add [sync] config section using gitops

- Add gitops dependency to bushel.config
- Add sync field to config type
- Add [sync] section to TOML codec
- Update default config template"
```

---

### Task 9: Add bushel sync command using gitops

**Files:**
- Modify: `avsm/bushel/bin/main.ml`
- Modify: `avsm/bushel/bin/dune`

**Step 1: Add gitops dependency to bushel CLI**

In `avsm/bushel/bin/dune`:

```dune
(executable
 (name main)
 (public_name bushel)
 (libraries bushel bushel.config bushel.eio bushel.sync
            cmdliner fmt fmt.cli fmt.tty logs logs.cli logs.fmt
            eio_main ptime srcsetter gitops))
```

**Step 2: Add new git-sync command to main.ml**

Add after the existing sync_cmd:

```ocaml
(** {1 Git Sync Command} *)

let git_sync_cmd =
  let run () config_file data_dir dry_run remote_override =
    match load_config config_file with
    | Error e -> Printf.eprintf "Config error: %s\n" e; 1
    | Ok config ->
      let data_dir = get_data_dir config data_dir in

      (* Check if sync is configured *)
      let sync_config = match remote_override with
        | Some r -> { config.Bushel_config.sync with Gitops.Sync.Config.remote = r }
        | None -> config.Bushel_config.sync
      in

      if sync_config.Gitops.Sync.Config.remote = "" then begin
        Printf.eprintf "Error: No sync remote configured.\n";
        Printf.eprintf "Add to ~/.config/bushel/config.toml:\n";
        Printf.eprintf "  [sync]\n";
        Printf.eprintf "  remote = \"ssh://server/path/to/repo.git\"\n";
        Printf.eprintf "\nOr use --remote URL\n";
        1
      end else begin
        Eio_main.run @@ fun env ->
        let git = Gitops.v ~dry_run env in
        let repo = Eio.Path.(env#fs / data_dir) in

        Printf.printf "%s bushel data with %s\n"
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
  let doc = "Sync bushel data with remote git repository." in
  let man = [
    `S Manpage.s_description;
    `P "Synchronizes your bushel data directory with a remote git repository.";
    `P "Configure the remote in ~/.config/bushel/config.toml:";
    `Pre "  [sync]\n  remote = \"ssh://server/path/to/bushel.git\"";
    `P "The sync process:";
    `P "1. Fetches from the remote repository";
    `P "2. Merges any remote changes";
    `P "3. Commits local changes (if auto_commit is enabled)";
    `P "4. Pushes to the remote";
    `P "Use $(b,--dry-run) to preview what would happen.";
  ] in
  let info = Cmd.info "git-sync" ~doc ~man in
  Cmd.v info Term.(const run
    $ logging_t
    $ config_file
    $ data_dir
    $ Gitops.Sync.Cmd.dry_run_term
    $ Gitops.Sync.Cmd.remote_term)
```

**Step 3: Add git-sync to command group**

Update main_cmd:

```ocaml
let main_cmd =
  (* ... existing info ... *)
  Cmd.group info [
    init_cmd;
    list_cmd;
    images_cmd;
    stats_cmd;
    show_cmd;
    render_cmd;
    sync_cmd;
    git_sync_cmd;  (* NEW *)
    paper_add_cmd;
    video_fetch_cmd;
    config_cmd;
  ]
```

**Step 4: Verify it builds**

Run: `dune build avsm/bushel`
Expected: Build succeeds

**Step 5: Test the command**

Run: `dune exec -- bushel git-sync --help`
Expected: Shows help text for git-sync command

**Step 6: Commit**

```bash
git add avsm/bushel/bin/dune avsm/bushel/bin/main.ml
git commit -m "feat(bushel): add git-sync command using gitops library

New 'bushel git-sync' command:
- Uses Gitops.Sync for git operations
- Supports --dry-run and --remote flags
- Reads [sync] config section
- Shows helpful error if remote not configured"
```

---

### Task 10: Final verification and documentation update

**Files:**
- Modify: `docs/plans/2026-02-01-gitops-sync-design.md`

**Step 1: Run full build**

Run: `dune build @all`
Expected: Build succeeds

**Step 2: Test dry-run mode**

Run: `dune exec -- bushel git-sync --dry-run --remote ssh://test/bushel.git -v`
Expected: Shows "Would run: git ..." messages

**Step 3: Update design doc with completion status**

Add at the top of the design document:

```markdown
**Status:** Implemented

**Implementation:** See `avsm/gitops/` for the library and `bushel git-sync` command.
```

**Step 4: Final commit**

```bash
git add docs/plans/2026-02-01-gitops-sync-design.md
git commit -m "docs: mark gitops design as implemented

Library complete with:
- Core git operations
- Dry-run support
- Sync module with TOML config
- Cmdliner integration
- Bushel integration"
```

---

## Summary

| Task | Description | Files |
|------|-------------|-------|
| 1 | Library skeleton | dune-project, lib/dune, lib/gitops.ml |
| 2 | Core git execution | lib/gitops.ml |
| 3 | Query operations | lib/gitops.ml |
| 4 | Mutating operations | lib/gitops.ml |
| 5 | Sync config | lib/gitops.ml |
| 6 | Cmdliner integration | lib/gitops.ml |
| 7 | Interface file | lib/gitops.mli |
| 8 | Bushel config integration | bushel/lib_config/* |
| 9 | Bushel git-sync command | bushel/bin/* |
| 10 | Final verification | docs/plans/* |
