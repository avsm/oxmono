val with_context : string -> (unit -> 'a) @ local -> 'a
(** Add a non-sensitive operation label to low-level Eio I/O failures. *)
