type built = {
  hash : string;
  prefix : string;
  closure : string list;
  installed : string list;
}
(** Package builds and restoration through the local day10 layer store. *)

type t = {
  d10 : D10.Config.t;
  proc : Support.proc;
  identity : string;
  jobs : int;
  refresh : bool;
}

val unique : string list -> string list
val prefix : t -> string -> string
val marker : string -> string

val materialise : t -> string list -> string -> unit
(** [materialise builder hashes destination] replaces [destination] with the
    ordered layer union, rebasing dune-package files and detaching hardlinks. *)

val restore : t -> built -> unit
(** [restore builder built] reconstructs a missing package prefix from layers. *)

val supplied : t -> Toolchain.t -> built
(** [supplied builder compiler] imports an explicitly supplied compiler. *)

val run : t -> solution:Solve.t -> deps:built list -> Solve.package -> built
(** [run builder ~solution ~deps package] restores or builds a package at its
    permanent prefix and captures a day10 layer. A failed build publishes no
    completion receipt. Cache mutations require the caller's process lock. *)
