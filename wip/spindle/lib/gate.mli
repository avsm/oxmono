(** Giving up a held resource while a fiber waits for something else. *)

type t

val v : release:(unit -> unit) -> acquire:(unit -> unit) -> t
(** [v ~release ~acquire] gives up a resource with [release] and takes it back
    with [acquire]. *)

val run : t -> (unit -> 'a) -> 'a
(** [run t f] is [f ()], with the resource given up while it runs. The first of
    several overlapping calls releases it and the last to end acquires it. One
    that ends by cancellation may leave it given up, so the caller tracks
    whether it holds it, as [acquire] and [release] can record. *)
