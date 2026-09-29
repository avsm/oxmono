(*---------------------------------------------------------------------------
   Copyright (c) 2026 Anil Madhavapeddy. All rights reserved.
   SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

(** Choosing and locating a model the DS4 engine runs.

    Models are distributed as one or more GGUF files under named download
    targets, each with short aliases. *)

type t = {
  name : string;  (** the target's name *)
  aliases : string list;  (** short forms accepted on the command line *)
  repo : string;  (** the Hugging Face repository the files come from *)
  files : string list;  (** the GGUF files that make up the model *)
  parts : string list;
      (** the files published in the repository, in order, whose concatenation
          is the one file in [files], or empty when [files] are published as
          they are *)
  descr : string;  (** a one-line description *)
  deprecated : bool;  (** superseded by a newer build of the same model *)
}
(** A download target.

    A target is named for the build it carries. The short names are aliases on
    the newest build, so [q4] follows the current one, while an older build
    stays reachable under its own version. *)

val all : t list
(** [all] is every known target. *)

val preferred : string list
(** [preferred] are the targets tried in order when no model is named, best
    first. Only downloaded targets are considered, so a machine that can hold
    only the smallest still gets a sensible default. *)

val find : string -> t option
(** [find s] is the target whose name or alias is [s]. *)

val of_path : string -> t option
(** [of_path p] is the target that the file [p] belongs to, matched on its name,
    or [None] for a model obtained some other way. *)

val dir : Xdge.t -> string
(** [dir xdg] is the directory models are kept in. *)

val present : dir:string -> t -> bool
(** [present ~dir t] is true when every file of [t] exists under [dir]. *)

val download :
  fs:_ Eio.Path.t ->
  proc:_ Eio.Process.mgr ->
  dir:string ->
  ?token:string ->
  t ->
  unit
(** [download ~fs ~proc ~dir ~token t] fetches the files of [t] from Hugging
    Face into [dir], creating [dir] if needed, and reports its progress on
    standard error.

    It runs [hf] and falls back to [uvx hf] when that fails, so one of the two
    must be on the PATH. [token] defaults to [HF_TOKEN] and then to the local
    Hugging Face login. A target published in parts is joined into its one file
    and the parts removed. An interrupted join resumes rather than fetching the
    first part again. It raises [Failure] when a join produces the wrong length,
    and leaves the parts for a retry. *)

val others : dir:string -> string list
(** [others ~dir] are the GGUF files under [dir] that belong to no target, in
    alphabetical order. *)

val resolve :
  ?env:(string -> string option) ->
  dir:string ->
  string option ->
  (string, string) result
(** [resolve ~dir override] is the path of the model to load, or an error
    explaining how to supply one.

    [override] is usually the [--model] argument. A target name or alias
    resolves to that target's file under [dir], and fails if it is not
    downloaded or is split across several files. Anything else is taken as a
    path. When [override] is absent, [DS4_MODEL] is used, then the best
    downloaded target from {!preferred}, then the first GGUF under [dir] in
    alphabetical order.

    [env] reads environment variables and defaults to {!Sys.getenv_opt}. *)

val arg : string option Cmdliner.Term.t
(** [arg] is the [--model] argument, giving a target name, an alias, or a path.
*)

val target_arg : t Cmdliner.Term.t
(** [target_arg] is a required positional argument naming a target, for commands
    that act on one. *)
