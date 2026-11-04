(** Opam dependency solving against local opam repositories.

    The context is adapted from Ox's, which was adapted from oi. *)

type platform = {
  os : string;
  os_family : string;
  os_distribution : string;
  os_version : string;
  arch : string;
  opam_version : string;
}
(** The opam variables of the machines a build runs on. *)

type request = {
  platform : platform;
  repositories : string list;
      (** The [packages] directories to read, earlier ones taking precedence, as
          an overlay over opam-repository does. *)
  pins : (string * string) list;
      (** The project's packages, each a name and the text of its opam file.
          They are solved at version [dev], with their test dependencies. *)
  compiler : string * string;
      (** The package that fixes the compiler and its exact version, such as
          [("ocaml", "5.4.0")] or [("oxcaml-compiler", "5.2.0minus39")]. *)
  requires : string list;
      (** Other packages the solution must hold, at any version. *)
}

val solve : request -> (string list, string) result
(** [solve request] is the packages of a solution, as [name.version], or an
    explanation of why there is none. The pinned packages are included. *)

val direct : (string * string) list -> string list
(** [direct pins] is the names of the packages that the project's [pins], given
    as in {!request}, depend on directly, with their test dependencies, apart
    from the project's own. *)

val explain : request -> string -> string
(** [explain r diagnostics] is a short reason that [r] has no solution, from the
    solver's [diagnostics]: a project package's bound on the compiler that its
    version does not meet, such as [tdmrep requires ocaml >= 5.1], or else the
    first reason the solver gives for at most three packages it could not
    choose. *)

val versions : string -> string -> string list
(** [versions packages name] is the released versions of package [name] in the
    [packages] directory, without pre-releases, newest first. *)
