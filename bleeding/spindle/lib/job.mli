(** Workflows and event selection. *)

type kind = Push | Pull_request | Manual

type context = {
  repo : string;
  actor : string;
  commit : string;
  kind : kind;
  ref_ : string option;
  changed_files : string list;
  default_ref : bool;
  request : Jsont.json;
}

type step =
  | Metadata
  | Command of string list
      (** Commands run directly, without a shell, in a detached checkout. *)
  | Shell of { name : string; command : string; env : (string * string) list }
      (** A repository workflow's step, run by [bash -c] with [env] added. *)

type t = private {
  name : string;
  steps : step list;
  accepts : context -> bool;
  env : (string * string) list;  (** Added to every step's environment. *)
  clone : bool;  (** Whether to check out the commit first. *)
  definition : Jsont.json option;
      (** The [org.tangled.ci.defs#workflow] of a repository workflow. *)
}

val routine : context -> bool
(** [routine c] holds for every event except a pull request's [closed] and
    [merged] actions, which the reference spindle runs only on request. *)

val v : ?accepts:(context -> bool) -> string -> step list -> t
(** [v ~accepts name steps] defines a workflow in trusted OCaml. [accepts]
    selects events using repository, trigger and ref, and defaults to
    {!routine}. Knots do not report changed paths, so [changed_files] is empty
    for their pushes. *)

val workflow :
  env:(string * string) list ->
  clone:bool ->
  definition:Jsont.json ->
  string ->
  step list ->
  t
(** [workflow ~env ~clone ~definition name steps] is a workflow read from a
    repository's [.tangled/workflows], already selected for its event. *)

val inspect : t
