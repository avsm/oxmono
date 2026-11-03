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
