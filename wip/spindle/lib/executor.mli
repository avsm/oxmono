(** Where workflows run.

    The spindle never runs a workflow's commands itself. It hands each workflow
    to an executor with everything needed to run it elsewhere, and records what
    the executor reports as the workflow's log. *)

type request = {
  pipeline : string;  (** The pipeline's ID. *)
  workflow : string;  (** The workflow's name. *)
  repo : string;  (** The repository's DID. *)
  source : string;  (** Where to clone the repository from. *)
  commit : string;  (** The commit to check out. *)
  clone : bool;  (** Whether to check out [commit] before the steps. *)
  metadata : Jsont.json;
      (** The trigger, the actor and the event that started the pipeline. *)
  definition : Jsont.json option;
      (** The [org.tangled.ci.defs#workflow] of a repository workflow. *)
  yaml : string option;  (** The text of a repository workflow's file. *)
  env : (string * string) list;  (** The workflow's own environment. *)
  variables : (string * string) list;  (** The CI variables. *)
  secrets : (string * string) list;
      (** The repository's secrets. Empty unless the pipeline is trusted. *)
  steps : Job.step list;
}
(** A step's environment is [env], then [variables], then [secrets], then the
    step's own [env], with later bindings winning. *)

type context = {
  step : string -> (unit -> unit) -> unit;
      (** [step name f] runs [f] as the next step of the log, called [name]. The
          step fails if [f] raises. *)
  log : [ `Stdout | `Stderr ] -> string -> unit;
      (** [log stream text] adds [text] to the current step's log, with every
          secret replaced by [***]. It raises [Invalid_argument] outside a
          step. *)
}

type t = context -> request -> unit
(** An executor runs a workflow and returns when it has succeeded. It fails the
    workflow by raising. Cancellation and the time limit cancel its fiber. *)

val print : ?delay:float -> _ Eio.Time.clock -> t
(** [print ~delay clock] logs the request, with each secret's value replaced by
    a placeholder, and logs each step without running it. It also prints the
    request to standard output. It then sleeps [delay] seconds, 2 by default,
    and succeeds. *)
