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

val debian_12_x86_64 : platform

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
}

val solve : request -> (string list, string) result
(** [solve request] is the packages of a solution, as [name.version], or an
    explanation of why there is none. The pinned packages are included. *)

val versions : string -> string -> string list
(** [versions packages name] is the released versions of package [name] in the
    [packages] directory, without pre-releases, newest first. *)
