type built
(** Package builds and restoration through the local day10 layer store. *)

type t = {
  d10 : D10.Config.t;
  proc : Support.proc;
  identity : string;
  jobs : int;
  refresh : bool;
}

val layers : built list -> string list
(** [layers built] returns their combined dependency layers in build order. *)

val run : t -> solution:Solve.t -> deps:built list -> Solve.package -> built
(** [run builder ~solution ~deps package] restores or builds a package at its
    permanent prefix and captures a day10 layer. A failed build publishes no
    completion receipt. Cache mutations require the caller's process lock. *)

val test : t -> solution:Solve.t -> deps:built list -> Solve.package -> unit
(** [test builder ~solution ~deps package] runs build, test and install actions
    in a fresh writable prefix. Test success is never reused from the cache. *)
