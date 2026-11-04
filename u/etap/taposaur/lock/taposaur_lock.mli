(** Advisory directory locks with diagnostic owner metadata.

    {1 NAME}
    Taposaur_lock - one exclusive, non-blocking lock per writable directory.

    {1 SYNOPSIS}
    {[
      Taposaur_lock.with_lock ~component:"taposaur.jobs" (dir ^ "/.lock")
        (fun () -> publish dir)
    ]}

    {1 DESCRIPTION}
    Every writer of a paper directory, staging directory or index shard
    takes the same lock file before changing it. The authority is an
    exclusive, non-blocking [flock] on the open file. After acquiring it,
    the holder writes a small JSON owner record into the file for operators
    and audit. A stale record left by a killed process does not keep the
    lock: the kernel releases the [flock] when that process dies, and the
    next writer overwrites the record.

    The lock is coherent across every process sharing the lock file's
    filesystem: processes on one host for a local disk, and every host
    mounting a CephFS tree. It does not coordinate writers over NFS and
    it is not a Duraml lease. Archive writers hold it
    only across local filesystem updates, never across HTTP transfers or
    conversions. The staging writer is the documented exception: a staged
    paper belongs to one acquisition, so {!Taposaur_store.with_paper}
    holds its lock for the whole transfer. Acquisition never waits for
    another holder, but its system calls block the calling thread, so
    callers on an Eio scheduler whose lock files live on a network
    filesystem should acquire from a system thread. *)

type t
(** A held lock. Releasing it closes the file. *)

type owner = {
  pid : int;  (** Process identifier at acquisition. *)
  host : string;  (** Hostname at acquisition. *)
  component : string;  (** Caller-supplied component name. *)
  at : string;  (** RFC 3339 UTC acquisition time. *)
}
(** The diagnostic record written into the lock file. It is evidence,
    not authority: a matching live process may no longer hold the lock
    and a dead process may have left it behind. *)

val owner_jsont : owner Jsont.t
(** [owner_jsont] encodes and decodes the owner record. *)

exception Busy of owner option
(** Another process or handle holds the lock. The payload is the owner
    record readable at the time, if any. It may be stale. *)

val acquire : ?component:string -> string -> t
(** [acquire ~component path] creates [path] with mode 0600 if absent,
    takes a non-blocking exclusive [flock] and then writes the owner
    record. [component] defaults to ["taposaur"]. Raises {!Busy} when the
    lock is held, including by another handle in this process,
    [Unix.Unix_error] for other failures and [Failure] when [path] is a
    symbolic link or not a regular file, so a planted link can never
    redirect the truncating open onto another file. *)

val release : t -> unit
(** [release t] truncates the owner record and closes the file, which
    releases the [flock]. Releasing twice is harmless. *)

val with_lock : ?component:string -> string -> (unit -> 'a) -> 'a
(** [with_lock path f] runs [f ()] while holding the lock and releases it
    afterwards, including on exceptions and cancellation. *)

val holder : string -> owner option
(** [holder path] reads the diagnostic owner record without locking. It is
    [None] when the file is absent, empty or unreadable. A record does not
    prove that the lock is held. Use it only for reporting. *)

val try_lock : Unix.file_descr -> bool
(** [try_lock fd] takes a non-blocking exclusive [flock] on [fd] and returns
    [false] when another holder exists. Raises [Unix.Unix_error] otherwise.
    Prefer {!acquire}, which also records the owner. *)
