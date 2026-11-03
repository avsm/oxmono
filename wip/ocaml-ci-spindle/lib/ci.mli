(** A spindle executor that tests OCaml projects as OCaml-CI does.

    Its [ocaml-ci] workflow finds the repository's opam files, solves their
    dependencies for each compiler and spawns a workflow for every solution.
    Each of those runs as an OBuilder job on an OCluster pool, with each
    OBuilder step shown as a step of its log. Nothing runs on the spindle's host
    apart from Git fetches of the opam repositories. *)

type config = {
  scheduler : Ocluster_client.scheduler;
      (** The OCluster scheduler, kept for the spindle's life. *)
  pool : string;  (** The pool builds run on, such as [linux-x86_64]. *)
  platform : Solve.platform;  (** The pool's opam variables. *)
  opam_repository : Opam_repo.t;
  oxcaml_repository : Opam_repo.t option;
      (** Adds an OxCaml build, solved against this overlay. *)
  compilers : string list;
      (** OCaml minor versions to build with, or [["auto"]]. See
          {!Variant.choose}. *)
}

val jobs : Spindle.Job.t list
(** [jobs] is the [ocaml-ci] workflow, which runs for every event. *)

val executor : config -> Spindle.Executor.t
(** [executor config] plans builds in the [ocaml-ci] workflow and runs the
    workflows it spawns on OCluster. *)
