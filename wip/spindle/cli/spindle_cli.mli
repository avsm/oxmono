(** The [spindle] command line, for building custom spindles.

    A custom spindle supplies its own workflows and executor, and may add
    command-line options of its own. Every standard option, such as
    [--hostname], [--owner] and [--jetstream], is kept. *)

type 'a make =
  Eio_unix.Stdenv.base ->
  sw:Eio.Switch.t ->
  'a ->
  Spindle.Job.t list * Spindle.Executor.t
(** [make env ~sw extra] is the spindle's OCaml workflows and executor, given
    the value of its own options [extra]. Resources it opens may use [sw], which
    lasts as long as the spindle serves. *)

val main :
  ?name:string ->
  ?doc:string ->
  ?workflow_timeout:float ->
  ?repository_workflows:bool ->
  'a Cmdliner.Term.t ->
  'a make ->
  unit
(** [main ~name ~doc extra make] parses the command line, with [extra] as
    additional options, and serves until stopped. [name] defaults to
    ["spindle"], and [workflow_timeout] is the default of [--workflow-timeout],
    60 seconds unless given. [repository_workflows], [true] by default, runs
    each repository's [.tangled/workflows] as well as the spindle's own. It
    exits the process. *)
