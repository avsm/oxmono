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

val status_files : t -> repo:Eio.Fs.dir_ty Eio.Path.t -> string list
(** [status_files t ~repo] returns porcelain status lines for changed files. *)

val status_porcelain : t -> repo:Eio.Fs.dir_ty Eio.Path.t -> string
(** [status_porcelain t ~repo] returns the raw [git status --porcelain] output. *)

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

    val codec : t Tomlt.t
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

  (** {2 Operations} *)

  val pull : t -> config:Config.t -> repo:Eio.Fs.dir_ty Eio.Path.t -> bool
  (** [pull t ~config ~repo] fetches from remote and merges if needed.
      Returns [true] if changes were pulled. *)

  val push : t -> config:Config.t -> ?msg:string -> repo:Eio.Fs.dir_ty Eio.Path.t -> unit -> bool
  (** [push t ~config ?msg ~repo ()] auto-commits (if enabled) and pushes to remote.
      [msg] overrides the configured commit message.
      Returns [true] if changes were pushed. *)

  val run : t -> config:Config.t -> repo:Eio.Fs.dir_ty Eio.Path.t -> result
  (** [run t ~config ~repo] performs a full sync: [pull] then [push]. *)

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
