(** Reading OBuilder job logs. *)

val strip_ansi : string -> string
(** [strip_ansi s] is [s] without its terminal escape sequences. *)

val operation : string -> string option
(** [operation line] is the OBuilder operation that [line] announces, such as
    [(run (shell "make"))], or [None] for a line of output. OBuilder prints
    [(from IMAGE)], then [DIR: OP] before each later operation. *)

val step_name : string -> string
(** [step_name op] is a short name for operation [op]: a [run] operation's shell
    command, or else the operation, at most 72 bytes. *)
