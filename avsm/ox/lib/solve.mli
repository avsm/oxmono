type package = {
  id : OpamPackage.t;
  opam : OpamFile.OPAM.t;
  directory : string;
}
(** In-process resolution of opam repository metadata. *)

type t = { packages : package list; platform : Osrel.t }

val platform_value : Osrel.t -> string -> OpamVariable.variable_contents option
(** [platform_value platform name] resolves a platform variable. *)

val dependencies : t -> package -> package list
(** [dependencies solution package] returns selected build dependencies,
    including installed optional dependencies and excluding post dependencies. *)

val run : platform:Osrel.t -> repos:Repository.t list -> string list -> t
(** [run ~platform ~repos roots] resolves package atoms and orders the result by
    build dependencies. Earlier repositories take precedence. *)
