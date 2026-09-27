type t = Database.t

(** Durable identities and mutation evidence for a bidirectional driver.
    No method performs IMAP or Maildir I/O. In particular, pending mutations
    are never replayed automatically after a crash. *)
type tombstone_reason = Inventory_absence | Expunge_receipt
  | Local_absence | Explicit_delete | Retention
type tombstone = {
  reason : tombstone_reason;
  evidence : string;
  generation : int64 option;
}
type pair = {
  id : string;
  scope : Imap.Mirror.scope;
  remote_uidvalidity : Imap.Proto.Uidvalidity.t option;
  remote_uid : Imap.Proto.Uid.t option;
  local_id : string option;
  content_sha256 : string option;
  content_length : int64 option;
  internal_date : Imap.Internal_date.t option;
  common_flags : Mail_flag.Imap_flag.t list;
  remote_tombstone : tombstone option;
  local_tombstone : tombstone option;
  revision : int64;
}
val put_pair : t -> expected_revision:int64 option -> pair ->
  [ `Committed of pair | `Stale_revision ]
(** Create with [None] and revision 0, or CAS-update with [Some revision].
    The ID, scope, once-bound occurrence identities, and once-bound content
    digest/length/date are immutable. Legacy pairs without content evidence may
    retain [None], but deletion propagation must hold for them.
    A remote [Inventory_absence] tombstone requires the current complete
    published inventory reference and generation, and absence of that UID.
    A tombstone cannot be cleared. It can be replaced only by one whose
    reason is the same or more permanent, in the order absence, then
    [Expunge_receipt] or [Retention], then [Explicit_delete]. A changed
    scope, identity or tombstone raises [Invalid_argument].
    The pair, flags and tombstones commit atomically. *)
val find_pair : t -> id:string -> pair option
val note_presence : t -> pair:pair -> side:[ `Remote | `Local ] ->
  generation:int64 -> [ `Recorded | `Stale_revision ]
(** Record that a complete published scan saw the paired side present.
    The caller must verify local presence in its complete Maildir inventory;
    remote presence is checked against the published SQLite snapshot.
    Pair revision and published generation are checked transactionally.
    A pair without an occurrence on [side] raises [Invalid_argument]. *)
val last_presence_generation : t -> pair_id:string ->
  side:[ `Remote | `Local ] -> int64 option
(** The latest published generation at which {!note_presence} recorded
    this paired side present, or [None] if it never did. A read-only
    pre-v13 database returns [None]. *)
val reactivate_local : t -> pair:pair -> generation:int64 ->
  [ `Reactivated of pair | `Stale_revision ]
(** Clear a [Local_absence] tombstone after the caller verifies the same
    Maildir occurrence's saved body digest, length and INTERNALDATE in a
    complete local inventory. Requires a matching durable local presence
    witness and current published generation; pair revision is CAS-checked.
    No other tombstone reason can be cleared. *)
val find_remote : t -> scope:Imap.Mirror.scope ->
  uidvalidity:Imap.Proto.Uidvalidity.t -> uid:Imap.Proto.Uid.t -> pair option
val find_local : t -> scope:Imap.Mirror.scope -> local_id:string -> pair option
val pairs : t -> scope:Imap.Mirror.scope -> pair list
val pairs_page : t -> scope:Imap.Mirror.scope -> ?after:string ->
  limit:int -> unit -> pair list
(** Stable ID order, strictly after [after]. [limit] must be 1..10,000.
    Continue with the last returned ID until a page is short. *)

type conflict_kind = Flag_conflict | Identity_conflict | Content_conflict
  | Delete_conflict | Policy_conflict | Deletion_hold
type conflict = {
  id : string; pair_id : string; kind : conflict_kind;
  evidence : string; pair_revision : int64; resolved : bool;
}
val record_conflict : t -> conflict -> unit
(** Requires the named pair at [pair_revision]. Duplicate IDs fail. *)
val ensure_open_conflict : t -> pair:pair -> kind:conflict_kind ->
  id:string -> evidence:string -> [ `Open of conflict | `Stale_revision ]
(** Idempotently create or update the one open conflict of this kind for
    [pair]. The pair revision is CAS-checked; a repeated hold keeps its ID. *)
val resolve_open_conflicts : t -> pair:pair -> kind:conflict_kind ->
  [ `Resolved of int | `Stale_revision ]
(** Resolve open conflicts of this kind only if the pair revision still
    matches. Use only after independently proving the condition is gone. *)
val has_open_conflict : t -> pair:pair -> kind:conflict_kind -> bool
(** Read-only direct lookup for a specific pair and conflict kind. *)
val resolve_conflict : t -> id:string -> unit
val open_conflicts : t -> scope:Imap.Mirror.scope -> conflict list
val open_conflicts_page : t -> scope:Imap.Mirror.scope -> ?after:string ->
  limit:int -> unit -> conflict list
(** Stable conflict-ID order, strictly after [after]. [limit] must be
    1..10,000. Continue with the last returned ID until a page is short. *)

type operation_kind = Append | Local_append | Copy | Move | Flags
  | Delete | Local_delete
type operation_state = Prepared | Sent | Ambiguous | Observed
  | Committed | Rejected
type operation = {
  id : string;
  pair_id : string option;
  local_id : string option;
  scope : Imap.Mirror.scope;
  kind : operation_kind;
  state : operation_state;
  source_uidvalidity : Imap.Proto.Uidvalidity.t option;
  source_uid : Imap.Proto.Uid.t option;
  destination : Imap.Mirror.scope option;
  destination_uidvalidity : Imap.Proto.Uidvalidity.t option;
  blob_sha256 : string option;
  blob_length : int64 option;
  desired_flags : Mail_flag.Imap_flag.t list option;
  receipt : string option;
  receipt_uidvalidity : Imap.Proto.Uidvalidity.t option;
  receipt_uid : Imap.Proto.Uid.t option;
}
val prepare_operation : ?local_flags:Mail_flag.Imap_flag.t list ->
  ?local_source_mtime:float ->
  ?source_internal_date:Imap.Internal_date.t ->
  t -> operation -> unit
(** Persist immutable source/destination identity and desired change before
    dispatch. [local_id] reserves a stable Maildir occurrence name before a
    remote-to-local write. For an existing [pair_id], atomically capture its
    current revision as a durable commit precondition. The initial state
    must be [Prepared]. For FLAGS, [local_flags] atomically saves the
    Maildir preimage, including an empty list. Older operations without
    this evidence cannot safely finish a one-sided remote write.
    For an APPEND with a [local_id], [local_source_mtime] atomically saves
    the scanned Maildir file timestamp used as a source preimage. For a
    local append,
    [source_internal_date] saves the remote date before Maildir publication
    so crash recovery can reject an altered Maildir timestamp.
    A paired operation must match the stored local occurrence and any
    supplied remote UID and UIDVALIDITY; contradictions raise
    [Invalid_argument] before journaling. *)
val operation_source_mtime : t -> id:string -> float option
(** The immutable source timestamp captured when an APPEND was prepared.
    Older pending operations and read-only v8 databases return [None]. *)
val operation_source_date : t -> id:string -> Imap.Internal_date.t option
(** The immutable remote INTERNALDATE captured for a local append. Older
    pending operations and read-only databases before v11 return [None]. *)
val local_flags_preimage : t -> id:string ->
  Mail_flag.Imap_flag.t list option
(** Read the immutable local preimage saved with a FLAGS operation. *)
val operation_pair_revision : t -> id:string -> int64 option
(** The pair revision captured when the operation was prepared. Missing
    preconditions on legacy pending operations return [None]. *)
val mark_sent : t -> id:string -> unit
val mark_ambiguous : ?reason:string -> t -> id:string -> unit
(** Persist an uncertain outcome. [reason], when supplied, is bounded and
    visible in read-only operation inspection; it is superseded by a later
    verified receipt. Never use this state for a mutation proven unsent. *)
val reject_operation : t -> id:string -> receipt:string -> unit
val reject_prepared_operation : t -> id:string -> receipt:string -> unit
(** Atomically reject only an operation that is still [Prepared]. A
    concurrently dispatched mutation cannot be classified as unsent. *)
val observe_operation : t -> id:string -> receipt:string ->
  destination_uidvalidity:Imap.Proto.Uidvalidity.t option ->
  destination_uid:Imap.Proto.Uid.t option -> unit
val commit_operation : t -> id:string -> unit
(** Only an observed unpaired operation can become committed this way.
    A paired operation must use [commit_operation_with_pair] so its common
    state advances atomically. A [Sent] or [Ambiguous] operation must be
    reconciled before commit. *)
val commit_operation_with_pair : t -> id:string ->
  expected_pair_revision:int64 option -> pair ->
  [ `Committed of pair | `Stale_revision ]
(** Atomically CAS-publish the paired last-common state and mark an observed
    operation committed. [None] creates a pair for an unpaired operation.
    A stale pair leaves the operation observed for reconciliation. For
    operations against an existing pair, the supplied revision must also
    match the revision captured by [prepare_operation]. Legacy v5 pending
    operations lack that evidence and cannot auto-commit. A committed FLAGS
    operation also resolves that pair's open flag conflicts in the same
    transaction. The published pair must match the operation's local ID,
    remote source identity (or destination receipt for APPEND/COPY/MOVE),
    supplied content and desired flags. Remote creation also checks the
    destination scope and any expected UIDVALIDITY. Deletion requires the
    corresponding tombstone; other operations require live occurrences.
    Contradictory evidence, or a paired operation with
    [expected_pair_revision = None], raises [Invalid_argument] without
    committing. *)
val settle_flag_operation : t -> id:string -> pair ->
  flags:Mail_flag.Imap_flag.t list -> evidence:string ->
  [ `Settled of pair | `Stale_revision | `Invalid_operation ]
(** Operator repair after independent verification that remote and local
    flags agree. Atomically replace the paired common flag baseline, reject
    the old uncertain FLAGS intent with bounded evidence, and resolve its
    open flag conflict. Requires the saved pair revision and occurrence
    identities to match, an active sent/ambiguous/observed FLAGS operation,
    and no other active operation for the pair. This performs no network or
    Maildir write; the caller must verify both endpoints under its writer
    lease before calling it. A stale [pair], or one whose revision differs
    from the revision saved by {!prepare_operation}, yields
    [`Stale_revision]. An operation of another kind, state or identity, or
    other active work on the pair, yields [`Invalid_operation]. *)
val reject_unchanged_delete_operation : t -> id:string -> pair ->
  evidence:string ->
  [ `Rejected | `Stale_revision | `Invalid_operation ]
(** Atomically reject a sent/ambiguous paired remote DELETE after the
    caller independently verifies that the exact remote UID remains with
    its saved bytes and last-common flags. Requires the original pair
    revision and occurrence identities, a local-absence tombstone, and no
    other active operation for the pair. The operation's saved digest and
    length must equal the pair's, and its flag preimage, when present, must
    equal the pair's common flags as a set. Does not mutate either
    endpoint. A later deletion attempt requires a new journal operation.
    Outcomes follow {!settle_flag_operation}. *)
val attest_targeted_expunge : t -> id:string -> pair ->
  evidence:string ->
  [ `Attested | `Stale_revision | `Invalid_operation ]
(** Persist explicit operator authorization for a targeted UID EXPUNGE of
    a sent/ambiguous paired DELETE. Atomically checks the saved pair
    revision, exact operation identity, local-absence tombstone and absence
    of other active work for the pair. Leaves the operation [Ambiguous]
    before network dispatch so crash recovery never replays the command.
    The caller must verify the same remote UID, original bytes, expected
    [\\Deleted] flags and stable MODSEQ immediately before invoking this.
    Evidence checks and outcomes follow
    {!reject_unchanged_delete_operation}. Evidence that would grow the
    operation receipt beyond 4096 bytes raises [Invalid_argument]. *)
val find_operation : t -> id:string -> operation option
val active_operations : t -> scope:Imap.Mirror.scope -> operation list
(** Prepared, Sent, Ambiguous and Observed operations survive restart. *)
val active_operations_page : t -> scope:Imap.Mirror.scope ->
  ?after:string -> limit:int -> unit -> operation list
(** Active operations in ascending ID order, strictly after [after].
    [limit] must be 1..10,000. Continue with the last returned ID until a
    page is short. Terminal operations are excluded; a state change between
    calls may remove an operation from subsequent pages. *)
val active_operation_for_pair : t -> pair_id:string -> operation option
(** The lowest-ID active operation for the pair, if any. Pair IDs are
    globally unique. Use this to hold a pair while any mutation is pending
    without loading the entire mailbox journal. *)
