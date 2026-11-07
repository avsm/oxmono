(** Scoped POSIX signal handlers that are safe with multiple domains.

    Handlers are installed through {!Sys.Safe}, so a behaviour is shared by
    every domain and must be portable. A handler runs on whichever domain
    receives the signal, so it should do no more than set an atomic or wake a
    scheduler. *)

val with_handlers :
  (int * Sys.signal_behavior) list @ portable -> (unit -> 'a) -> 'a
(** [with_handlers handlers f] is [f ()] with each signal in [handlers] bound to
    its behaviour. The previous behaviours are restored when [f] returns or
    raises. *)

val with_wait : sw:Eio.Switch.t -> int list -> ((unit -> unit) -> 'a) -> 'a
(** [with_wait ~sw signals f] is [f wait]. For the extent of [f], every
    signal in [signals] wakes the fibers blocked in [wait]. [wait ()] blocks
    the calling fiber until a signal has arrived since [wait] last returned.
    Arrivals are coalesced, so several signals before a call return once.
    The previous behaviours are restored when [f] returns or raises, and the
    pipe that carries the wakeups is closed when [sw] is released. *)
