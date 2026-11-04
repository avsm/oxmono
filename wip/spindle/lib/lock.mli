(** Exception-safe serialization for transactional state. *)

val protect : ?cancellable:bool -> Eio.Mutex.t -> (unit -> 'a) -> 'a
(** [protect ~cancellable mutex f] runs [f] while it owns [mutex], with
    cancellation masked unless [cancellable], [false] by default. A cancellable
    section may wait on a condition of [mutex] and is woken by cancellation.
    Exceptions propagate without poisoning the lock. The caller must restore a
    consistent state before raising, for example by rolling back SQLite. *)
