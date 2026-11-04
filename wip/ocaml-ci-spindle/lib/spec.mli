(** OBuilder specs for building and testing opam projects, as OCaml-CI writes
    them for Linux. *)

val v :
  variant:Variant.t ->
  opam_repository:string ->
  ?oxcaml_repository:string * string ->
  opam_files:string list ->
  packages:string list ->
  unit ->
  string
(** [v ~variant ~opam_repository ~opam_files ~packages ()] is the spec, as an
    S-expression, that resets [~/opam-repository] to commit [opam_repository],
    pins the project's [opam_files], installs the solved [packages] and runs
    [dune build @install @check @runtest] on the source. An OxCaml [variant]
    first adds [oxcaml_repository], a URL and commit, and creates its switch. *)

val day10 :
  variant:Variant.t ->
  opam_repository:string * string ->
  opam_files:string list ->
  Ocluster_client.day10
(** [day10 ~variant ~opam_repository:(url, commit) ~opam_files] is the
    [day10 build] of the project for [variant] against the opam repository [url]
    at [commit]: its packages, those of [opam_files], with their tests, running
    [dune build @install @check @runtest], as {!v}'s spec does. *)

val day10_jsont : Ocluster_client.day10 Jsont.t
(** [day10_jsont] is how a day10 build is written as a workflow's step. *)
