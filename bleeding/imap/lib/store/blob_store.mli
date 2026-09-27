(** Content-addressed message files and snapshot references. *)

type t = Database.t

type blob = private { sha256 : string; length : int64 }
exception Digest_mismatch

val put : t -> source:_ Eio.Flow.source -> length:int64 ->
  ?expected_sha256:string -> unit -> blob
(** Read exactly [length] octets, hash them with SHA-256, write a unique
    temporary file, sync it, rename to the content-addressed name and sync
    the containing directory. [expected_sha256], if set, must match or
    [Digest_mismatch] is raised.
    Requires [blob_dir]. Does not consume bytes beyond [length]. A failed
    operation never creates a DB reference, but may leave an orphan file. *)

val verify : t -> blob -> bool
(** Rehash the complete file and check its length. Missing or non-regular
    files return [false]; other I/O failures propagate. Requires [blob_dir]. *)

val open_in : t -> sw:Eio.Switch.t -> blob -> Eio.File.ro_ty Eio.Resource.t
(** Open exact blob bytes for reading. Call [verify] if corruption detection
    is required; opening alone does not rehash the file. *)

val attach : t -> scope:Imap.Mirror.scope ->
  uidvalidity:Imap.Proto.Uidvalidity.t -> uid:Imap.Proto.Uid.t ->
  blob -> unit
(** Verify and atomically reference [blob] from an existing message in the
    current mailbox epoch. An unknown UID or mismatched scope/epoch raises
    [Invalid_argument]. Replacing a reference is atomic. *)

val find : t -> scope:Imap.Mirror.scope ->
  uidvalidity:Imap.Proto.Uidvalidity.t -> uid:Imap.Proto.Uid.t ->
  blob option

val missing_page : t -> scope:Imap.Mirror.scope ->
  cursor:Imap.Mirror.cursor -> ?after_uid:Imap.Proto.Uid.t ->
  limit:int -> unit ->
  [ `Uids of Imap.Proto.Uid.t list | `Stale_revision ]
(** Indexed UID page from the current published snapshot whose messages
    have no blob reference. The cursor's revision, UIDVALIDITY and full
    scope are checked in the same read transaction. [limit] is 1..10,000.
    A new mailbox yields an empty page. Continue strictly after the last
    returned UID; a concurrent blob attachment can shrink later pages. *)

val referenced_page : t -> scope:Imap.Mirror.scope ->
  cursor:Imap.Mirror.cursor -> ?after_uid:Imap.Proto.Uid.t ->
  limit:int -> unit ->
  [ `Refs of (Imap.Proto.Uid.t * blob) list | `Stale_revision ]
(** Indexed UID page of blob references still present in the published
    snapshot. Checks the cursor revision, epoch and full scope in one read
    transaction. [limit] is 1..10,000. Page strictly after the last UID. *)

val detach_if_matches : t -> scope:Imap.Mirror.scope ->
  cursor:Imap.Mirror.cursor -> uid:Imap.Proto.Uid.t -> blob ->
  [ `Detached | `Unchanged | `Stale_revision ]
(** Remove a corrupt or missing cache reference only if the published
    cursor and exact reference still match. Does not unlink blob files.
    A changed reference returns [Unchanged]; a new snapshot revision or
    epoch returns [Stale_revision]. *)

val iter_orphan_candidates : t -> (string -> unit) -> unit
(** [iter_orphan_candidates t f] visits unreferenced final blobs and temporary
    files in unspecified order. It keeps at most 256 directory names in memory
    and checks references using indexed database lookups. The callback runs
    without a database lock; exceptions and cancellation close the directory.
    All blob writers, including other processes, must remain quiescent until
    iteration finishes. The callback must not create files or references. *)

val reap_orphans_iter : t -> removed:(string -> unit) -> unit
(** [reap_orphans_iter t ~removed] removes orphan candidates with bounded
    inventory memory. [removed name] runs after unlinking each candidate.
    The directory is synced on return, exception or cancellation if any unlink
    was attempted. Callbacks precede this sync and do not prove durability.
    The same writer-quiescence requirement as [iter_orphan_candidates] applies. *)

val orphan_candidates : t -> string list
(** Names of final blobs unreferenced by snapshots or pending journals, and temporary files. Call only while
    no writer is active; this is a non-destructive recovery inventory.
    A file can become referenced immediately after this call.
    This convenience wrapper collects and sorts all names in memory. *)

val reap_orphans : t -> string list
(** Remove orphan candidates and sync the directory, returning removed
    names. Call at startup while all blob writers are quiescent, including
    writers in other processes. Never call concurrently with [put]/[attach].
    A crash during reaping leaves candidates for the next startup.
    This convenience wrapper collects and sorts all removed names in memory. *)
