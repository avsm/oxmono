(** Policy-gated deletion of an established IMAP/Maildir occurrence pair.

    The caller must hold [Imap_maildir.with_writer_lock] across the remote
    scan, local inventory, and this call. A missing side is actionable only
    when a complete published inventory proves absence. The survivor must
    still have its paired byte digest, length, and last-common flags. No
    mailbox-wide EXPUNGE or retry of an uncertain remote mutation occurs. *)

type error =
  | Client of Imap_eio.Error.t
  | Missing_pair
  | Stale_pair
  | Stale_inventory
  | Identity_changed
  | Unsupported of string
  | Pending_operation of string
  | Diverged of string

val pp_error : Format.formatter -> error -> unit

type outcome =
  | Unchanged
  | Held of Imap.Sync_policy.deletion_hold
  | Deleted of Imap_store.Sync.pair

val expunge_preflight :
  before_flags:Mail_flag.Imap_flag.t list -> before_modseq:int64 ->
  (Mail_flag.Imap_flag.t list * int64 option) option -> bool
(** Require the target to retain exactly the expected [\\Deleted] flags and
    a non-regressing MODSEQ after conditional STORE, immediately before
    issuing a targeted UID EXPUNGE. *)

val reconcile_pair :
  ?min_absence_scans:int ->
  client:Imap_eio.Client.t -> store:Imap_store.t ->
  maildir:Imap_maildir.t -> mailbox:string ->
  cursor:Imap.Mirror.cursor ->
  local_inventory:Imap_maildir.paged_inventory ->
  pair:Imap_store.Sync.pair -> policy:Imap.Sync_policy.deletion_policy ->
  next_id:(unit -> string) -> spool_dir:_ Eio.Path.t -> unit ->
  (outcome, error) result
(** [Preserve] only reports a hold. [Propagate] removes an unchanged local
    survivor when the remote UID is absent, or an unchanged remote survivor
    when the local occurrence is absent. [Propagate_remote] and
    [Propagate_local] enable only the corresponding direction. A local
    [Retention] tombstone always holds remote deletion. A local delete is
    held until [min_absence_scans] later complete remote scan generations
    have passed since the missing side's first durable absence tombstone.
    Legacy local tombstones without a generation stay held if this setting
    is positive. A saved content or identity conflict holds either deletion
    direction.
    The local delete is journaled before [Maildir.remove]. A remote delete
    requires UIDPLUS and CONDSTORE, uses
    conditional UID STORE to add [\\Deleted], then UID EXPUNGE for exactly the
    paired UID. Before expunging, it fetches the target again and requires the
    expected flags and MODSEQ progression. It verifies UID absence before
    atomically committing the
    tombstone and journal. An uncertain result stays pending. [spool_dir] is
    used for bounded-memory remote body verification. *)

val recover_operation :
  store:Imap_store.t -> maildir:Imap_maildir.t ->
  cursor:Imap.Mirror.cursor ->
  local_inventory:Imap_maildir.paged_inventory ->
  operation:Imap_store.Sync.operation -> unit ->
  (outcome, error) result
(** Reconcile a pending deletion using complete newly published inventories.
    A [Prepared] operation is rejected because no send began. A [Sent] or
    [Ambiguous] deletion is committed only when the exact target is absent;
    otherwise it remains pending and is never replayed. This must run before
    new copies or flag changes. *)

val repair_local_delete :
  client:Imap_eio.Client.t -> store:Imap_store.t ->
  maildir:Imap_maildir.t -> scope:Imap.Mirror.scope -> mailbox:string ->
  id:string -> evidence:string -> unit -> (outcome, error) result
(** Explicit operator repair of a [Sent] or [Ambiguous] local unlink whose
    exact Maildir occurrence is still present. Acquires the writer lease and
    verifies the saved pair revision and identity, an existing complete
    remote absence tombstone, the current complete inventory, live read-only
    UID absence, and local bytes, length, and flags before unlinking and
    committing the journal. A saved OBJECTID+ binding is checked and pinned
    before the live UID check. It never retries a remote mutation. The caller
    must provide printable operator evidence; [Writer_lock_busy] may escape. *)

val reject_unchanged_remote_delete :
  client:Imap_eio.Client.t -> store:Imap_store.t ->
  maildir:Imap_maildir.t -> scope:Imap.Mirror.scope -> mailbox:string ->
  id:string -> evidence:string -> spool_dir:_ Eio.Path.t -> unit ->
  (unit, error) result
(** Explicitly reject a sent/ambiguous remote DELETE whose original UID
    remains present with the paired bytes, flags and stable MODSEQ. Requires
    current complete published UID membership, local absence, saved pair
    revision and exact journal identity, and a matching OBJECTID+ mailbox
    binding when one is saved. Holds the Maildir writer lease throughout.
    It sends no STORE or EXPUNGE; a changed or already-expunged UID remains
    pending. [Writer_lock_busy] may escape. *)

val finish_marked_remote_delete :
  client:Imap_eio.Client.t -> store:Imap_store.t ->
  maildir:Imap_maildir.t -> scope:Imap.Mirror.scope -> mailbox:string ->
  id:string -> evidence:string -> spool_dir:_ Eio.Path.t -> unit ->
  (outcome, error) result
(** Explicit operator completion of a sent/ambiguous remote DELETE when
    the exact saved UID remains present with paired bytes, original flags
    plus [\\Deleted], and stable MODSEQ. Verifies the current complete
    published inventory, local absence and OBJECTID+ mailbox binding under
    the Maildir writer lease. Persists operator evidence and [Ambiguous]
    state before sending only targeted UID EXPUNGE. A lost result remains
    pending for complete-inventory recovery, never automatic replay. The
    unavoidable concurrent remote-edit window between final FETCH and
    EXPUNGE remains; [Writer_lock_busy] may escape. *)
