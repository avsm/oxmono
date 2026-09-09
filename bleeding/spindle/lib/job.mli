(** Trusted OCaml job definitions. *)

type step = Metadata | Command of string list
(** [Metadata] prints the dispatch metadata. [Command argv] executes [argv]
    directly in the checked-out repository, without a shell. *)

type t = { name : string; steps : step list }

val inspect : t
(** [inspect] prints metadata and lists the checked-out directory. *)
