(** Counted assertions shared by the Proffer test executables. *)

val check : string -> bool -> unit
(** [check name b] counts one check and exits with status 1 after printing
    [name] when [b] is [false]. *)

val report : string -> unit
(** [report name] prints the number of checks run by the executable [name]. *)
