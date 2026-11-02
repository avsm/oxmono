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

type step = {
  name : string;
  command : string;  (** The step's command, as text for a shell. *)
  env : (string * string) list;  (** Added to the step's environment. *)
}
(** A step is a description handed to the {!Executor}. The spindle never runs it
    itself. *)

type t = private {
  name : string;
  steps : step list;
  accepts : context -> bool;
  env : (string * string) list;  (** Added to every step's environment. *)
  clone : bool;  (** Whether the commit should be checked out first. *)
  definition : Jsont.json option;
      (** The [org.tangled.ci.defs#workflow] of a repository workflow. *)
  yaml : string option;  (** The text of a repository workflow's file. *)
}

val step : ?env:(string * string) list -> string -> string -> step
(** [step ~env name command] is a step called [name] that runs [command]. *)

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
  yaml:string ->
  string ->
  step list ->
  t
(** [workflow ~env ~clone ~definition ~yaml name steps] is a workflow read from
    the file [yaml] in a repository's [.tangled/workflows], already selected for
    its event. *)

val inspect : t
(** [inspect] lists the files of the checked-out commit. *)
