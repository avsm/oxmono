(** Scoped exclusive dotlocks for Maildir metadata. *)

exception Busy of string
(** [Busy path] is raised when a file already exists at the lock [path]. *)

exception Lost of string
(** [Lost path] is raised when the lock at [path] is no longer held, because
    another party removed or replaced it or the callback has returned. *)

val with_lock : _ Eio.Path.t -> ((unit -> unit) -> 'a) -> 'a
(** [with_lock path f] is [f refresh] under an exclusive dotlock created at
    [path]. The lock body is the process ID and host name separated by a
    space.

    The callback must join every fiber that uses [refresh] before it returns.

    [refresh ()] checks that the lock is still held and rewrites its first
    byte to advance its modification time. After [f] returns, the lock is
    checked once more without being rewritten. A lost lock raises [Lost] in
    both cases.

    Return, exceptions and cancellation release the lock. Release unlinks
    [path] only while it still names the created file. The check and the
    unlink are not atomic against a concurrent replacement by a party that
    ignores the lock. An exception from [f] is never replaced by a release
    failure.

    An existing file at [path] raises [Busy], including a stale lock. Remove
    a stale lock offline while every user of the Maildir is stopped. *)
