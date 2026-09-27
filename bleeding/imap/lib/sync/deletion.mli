(** Policy-gated deletion of an established IMAP/Maildir occurrence pair.

    [reconcile_pair] and [recover_operation] require the caller to hold
    [Maildir.with_writer_lock] across the remote scan, the local
    inventory and the call. The three operator repairs take the lease
    themselves, so the caller must not hold it. A missing side is actionable
    only when a complete published inventory proves absence. The survivor
    must still have its paired byte digest, length, and last-common flags.
    No mailbox-wide EXPUNGE or retry of an uncertain remote mutation occurs.
    Journal writes and spool files are handled outside the mailbox
    selection. A pair in another scope is [Stale_pair] throughout. *)

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
  | Deleted of Imap_store.Journal.pair

val expunge_preflight :
  before_flags:Mail_flag.Imap_flag.t list -> before_modseq:int64 ->
  (Mail_flag.Imap_flag.t list * int64 option) option -> bool
(** [expunge_preflight ~before_flags ~before_modseq after] is [true] when
    [after] holds exactly [before_flags] plus [\\Deleted] and a MODSEQ
    above [before_modseq], or equal to it when [before_flags] already held
    [\\Deleted]. It is checked after the conditional STORE, immediately
    before a targeted UID EXPUNGE. *)

val reconcile_pair :
  ?min_absence_scans:int ->
  client:Imap_eio.Client.t -> store:Imap_store.t ->
  maildir:Maildir.t -> mailbox:string ->
  cursor:Imap.Mirror.cursor ->
  local_inventory:Maildir.paged_inventory ->
  pair:Imap_store.Journal.pair -> policy:Imap.Sync_policy.deletion_policy ->
  next_id:(unit -> string) -> spool_dir:_ Eio.Path.t -> unit ->
  (outcome, error) result
(** [Preserve] only reports a hold. [Propagate] removes an unchanged local
    survivor when the remote UID is absent, or an unchanged remote survivor
    when the local occurrence is absent. [Propagate_remote] and
    [Propagate_local] enable only the corresponding direction. A local
    [Retention] tombstone always holds remote deletion. Either direction is
    held until [min_absence_scans] (default 0) later complete scan
    generations have passed since the missing side's first durable absence
    tombstone. Legacy tombstones without a generation stay held if this
    setting is positive, and a negative value raises [Invalid_argument]. A saved content or identity conflict holds either
    direction, and a legacy pair without a content digest and length is held
    as [Missing_content_evidence].

    The local delete is journaled before [Maildir.remove]. A survivor
    whose bytes, flags or file changed is held as [Survivor_changed]. A
    remote delete requires UIDPLUS, CONDSTORE, a [spool_dir] directory,
    [\\Deleted] in PERMANENTFLAGS and a nonzero MODSEQ on the target, and
    otherwise returns [Unsupported]. It verifies the remote body through
    [spool_dir] with bounded memory, uses conditional UID STORE to add
    [\\Deleted], then UID EXPUNGE for exactly the paired UID. Before
    expunging it fetches the target again and requires {!expunge_preflight}.
    It verifies UID absence before atomically committing the tombstone and
    journal. A concurrent flag change, including MODIFIED on the conditional
    STORE, rejects the operation and is held as [Survivor_changed]. A
    concurrent expunge of the target is [Stale_inventory]. A STORE refused
    before dispatch rejects the operation. An uncertain result stays pending
    with its cause recorded. *)

val recover_operation :
  store:Imap_store.t -> maildir:Maildir.t ->
  cursor:Imap.Mirror.cursor ->
  local_inventory:Maildir.paged_inventory ->
  operation:Imap_store.Journal.operation -> unit ->
  (outcome, error) result
(** [recover_operation ~store ~maildir ~cursor ~local_inventory ~operation ()]
    reconciles a pending deletion using complete newly published
    inventories. A [Prepared] operation is rejected because no send began. A
    [Sent], [Ambiguous] or [Observed] deletion is committed only when both
    sides are absent and the pair carries the absence tombstone for the side
    the operation did not delete. Otherwise it remains pending and is never
    replayed. This must run before new copies or flag changes. *)

val repair_local_delete :
  client:Imap_eio.Client.t -> store:Imap_store.t ->
  maildir:Maildir.t -> scope:Imap.Mirror.scope -> mailbox:string ->
  id:string -> evidence:string -> unit -> (outcome, error) result
(** Explicit operator repair of a [Sent] or [Ambiguous] local unlink whose
    exact Maildir occurrence is still present. Acquires the writer lease and
    verifies the saved pair revision and identity, an existing complete
    remote absence tombstone, the current complete inventory, live read-only
    UID absence, and local bytes, length, and flags before unlinking and
    committing the journal. A saved OBJECTID+ binding is checked and pinned
    before the live UID check. It never retries a remote mutation. The caller
    must provide printable operator evidence. [Maildir.Writer_lock_busy]
    propagates when the lease is held. *)

val reject_unchanged_remote_delete :
  client:Imap_eio.Client.t -> store:Imap_store.t ->
  maildir:Maildir.t -> scope:Imap.Mirror.scope -> mailbox:string ->
  id:string -> evidence:string -> spool_dir:_ Eio.Path.t -> unit ->
  (unit, error) result
(** Explicitly reject a sent/ambiguous remote DELETE whose original UID
    remains present with the paired bytes, flags and stable MODSEQ. Requires
    current complete published UID membership, local absence, saved pair
    revision and exact journal identity, and a matching OBJECTID+ mailbox
    binding when one is saved. Holds the Maildir writer lease throughout.
    It sends no STORE or EXPUNGE, and a changed or already-expunged UID
    remains pending. [Maildir.Writer_lock_busy] propagates when the
    lease is held. *)

val finish_marked_remote_delete :
  client:Imap_eio.Client.t -> store:Imap_store.t ->
  maildir:Maildir.t -> scope:Imap.Mirror.scope -> mailbox:string ->
  id:string -> evidence:string -> spool_dir:_ Eio.Path.t -> unit ->
  (outcome, error) result
(** Explicit operator completion of a sent/ambiguous remote DELETE when
    the exact saved UID remains present with paired bytes, original flags
    plus [\\Deleted], and stable MODSEQ. Verifies the current complete
    published inventory, local absence and OBJECTID+ mailbox binding under
    the Maildir writer lease. Persists operator evidence and [Ambiguous]
    state before sending only targeted UID EXPUNGE. A lost result remains
    pending for complete-inventory recovery, never automatic replay. The
    unavoidable concurrent remote-edit window between the final FETCH and
    EXPUNGE remains. [Maildir.Writer_lock_busy] propagates when the
    lease is held. *)
