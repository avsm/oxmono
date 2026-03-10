(** In-memory WebDAV lock manager.

    Implements the locking model from
    {{:https://datatracker.ietf.org/doc/html/rfc4918#section-6}RFC 4918 Section 6–7}
    with exclusive write locks stored in memory.

    Locks do not survive server restarts. This is sufficient for
    macOS Finder and other WebDAV clients that use locks for
    safe-save workflows.

    {2 Lock Semantics}

    - Exclusive write locks only (no shared locks)
    - Depth 0 locks protect a single resource
    - Depth infinity locks protect a resource and all descendants
    - Expired locks are garbage-collected on each operation
    - Write operations (PUT, DELETE, MKCOL, PROPPATCH) on locked
      resources require the correct lock token

    @see <https://datatracker.ietf.org/doc/html/rfc4918#section-6> RFC 4918 Section 6 — lock model
    @see <https://datatracker.ietf.org/doc/html/rfc4918#section-7> RFC 4918 Section 7 — write locks *)

(** {1 Types} *)

type lock_info = {
  token : string;
      (** Opaque lock token URI (Section 6.5). *)
  path : string;
      (** Locked resource path. *)
  depth : [ `Zero | `Infinity ];
      (** Lock depth. [`Infinity] covers all descendants. *)
  scope : [ `Exclusive ];
      (** Lock scope — only exclusive supported. *)
  owner : string option;
      (** Optional owner identification from the LOCK request body. *)
  timeout_s : int;
      (** Lock timeout in seconds. *)
  created_at : float;
      (** Unix timestamp when the lock was created. *)
}
(** Information about an active lock. *)

type t
(** The lock manager. Not thread-safe — use one per connection fiber
    or protect with a mutex for concurrent access. *)

(** {1 Creation} *)

val create : unit -> t
(** [create ()] creates an empty lock manager. *)

(** {1 Lock Operations} *)

val lock :
  t -> path:string -> depth:[ `Zero | `Infinity ] ->
  owner:string option -> timeout_s:int ->
  (lock_info, [> `Locked of lock_info list]) result
(** [lock t ~path ~depth ~owner ~timeout_s] attempts to create a lock.

    Returns [Ok info] on success, or [Error (`Locked conflicts)] if
    an existing lock conflicts.

    @see <https://datatracker.ietf.org/doc/html/rfc4918#section-9.10> RFC 4918 Section 9.10 — LOCK *)

val unlock : t -> token:string -> bool
(** [unlock t ~token] removes the lock with the given token.
    Returns [true] if the lock existed.

    @see <https://datatracker.ietf.org/doc/html/rfc4918#section-9.11> RFC 4918 Section 9.11 — UNLOCK *)

val refresh : t -> token:string -> timeout_s:int -> lock_info option
(** [refresh t ~token ~timeout_s] extends a lock's timeout.
    Returns [Some info] if the lock exists, [None] otherwise.

    @see <https://datatracker.ietf.org/doc/html/rfc4918#section-9.10.2> RFC 4918 Section 9.10.2 *)

(** {1 Lock Queries} *)

val find : t -> token:string -> lock_info option
(** [find t ~token] returns the lock info for a token, if it exists. *)

val active_locks : t -> path:string -> lock_info list
(** [active_locks t ~path] returns locks rooted at exactly [path]. *)

val locks_covering : t -> path:string -> lock_info list
(** [locks_covering t ~path] returns all locks that cover [path],
    including ancestor locks with depth infinity. *)

val check_write :
  t -> path:string -> lock_token:string option ->
  (unit, lock_info) result
(** [check_write t ~path ~lock_token] checks if a write to [path] is
    permitted. Returns [Ok ()] if the path is unlocked or the correct
    token is provided. Returns [Error lock] if blocked.

    @see <https://datatracker.ietf.org/doc/html/rfc4918#section-7> RFC 4918 Section 7 — write lock interactions *)

(** {1 Property Helpers} *)

val lockdiscovery_xml : t -> path:string -> Webdavz_xml.tree list
(** [lockdiscovery_xml t ~path] returns the XML children for the
    [DAV:lockdiscovery] property, listing all active locks on [path].

    @see <https://datatracker.ietf.org/doc/html/rfc4918#section-15.8> RFC 4918 Section 15.8 *)
