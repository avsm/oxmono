(** Wall time and allocation of one benchmark workload. *)

val run : string -> (unit -> 'a) -> 'a
(** [run name f] is [f ()]. It prints on standard output one line naming
    [name] with the wall time of [f ()] in seconds, the bytes it allocated
    as {!Gc.allocated_bytes} counts them and the words it allocated on the
    minor heap. *)
