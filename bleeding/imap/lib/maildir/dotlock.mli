(** Scoped Dovecot-compatible metadata dotlocks. *)

exception Busy of string

val with_lock : string -> ((unit -> unit) -> 'a) -> 'a
(** [with_lock path f] is [f refresh] under an exclusive dotlock at [path].
    [refresh ()] checks ownership and refreshes the timestamp. It is valid only
    during the callback. Return, exceptions and cancellation release the owned
    lock after checking its inode identity. Refresh writes through the owned
    descriptor and checks the path before and after. These checks do not make
    pathname unlink atomic against an uncooperative concurrent replacement.
    Existing locks raise [Busy], including stale locks. Recovery must occur
    offline while all users of the Maildir are stopped. The callback must join any fibers that use [refresh]. *)
