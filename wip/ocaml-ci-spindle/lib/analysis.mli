(** Finding an opam project. *)

type t = { opam_files : (string * string) list }
(** The project's opam files at the root of the repository, with their text. *)

val find : Spindle.Executor.files -> t option
(** [find files] is the project in [files], or [None] when it has no opam files
    at its root. *)

val packages : t -> (string * string) list
(** [packages t] is each package's name and opam file text. *)
