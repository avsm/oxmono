(** A spindle executor that tests OCaml projects as OCaml-CI does.

    Its [ocaml-ci] workflow finds the repository's opam files, solves their
    dependencies for each compiler and spawns a workflow for every solution. The
    newest compiler's build spawns the builds for the other [platforms] once it
    passes. Each of those runs as an OBuilder job on an OCluster pool, with each
    OBuilder step shown as a step of its log. Nothing runs on the spindle's host
    apart from Git fetches of the opam repositories. *)

type config = {
  scheduler : Ocluster_client.scheduler;
      (** The OCluster scheduler, kept for the spindle's life. *)
  clock : Eio.Time.Mono.ty Eio.Resource.t;
      (** Bounds how long cancelling a job may take. *)
  opam_repository : Opam_repo.t;
  oxcaml_repository : Opam_repo.t option;
      (** Adds an OxCaml build, solved against this overlay. *)
  compilers : string list;
      (** OCaml minor versions to build with on Debian 13 x86_64, or [["auto"]].
          See {!Variant.choose}. *)
  platforms : Variant.platform list;
      (** Platforms that build the newest of [compilers] once its x86_64 build
          passes. *)
  diagnose : Diagnose.t option;
      (** A model that a [diagnose] workflow asks about failed builds. [None]
          sends nothing to any model. *)
}

val jobs : Spindle.Job.t list
(** [jobs] is the [ocaml-ci] workflow, which runs for every event. *)

val executor : config -> Spindle.Executor.t
(** [executor config] is a new executor, which plans builds in the [ocaml-ci]
    workflow and runs the workflows it spawns on OCluster. It fails a
    repository's own workflow, so the spindle should not offer them. *)
