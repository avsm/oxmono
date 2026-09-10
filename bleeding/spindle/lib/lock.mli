(** Exception-safe serialization for transactional state. *)
val protect : Eio.Mutex.t -> (unit -> 'a) -> 'a
(** [protect mutex f] runs [f] with cancellation masked while it owns [mutex].
    Exceptions propagate without poisoning the lock. The caller must restore a
    consistent state before raising, for example by rolling back SQLite. *)
