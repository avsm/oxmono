(** Durable mailbox snapshots and operation intents backed by SQLite.

    A store serializes access through one Eio mutex. It sets WAL and
    [synchronous=FULL], and publishes a cursor and full UID snapshot in one
    [BEGIN IMMEDIATE] transaction. SQLite durability also depends on the
    filesystem and its flush guarantees. Do not share a database file on an
    unsupported network filesystem. *)

type t

exception Scope_mismatch
(** Raised by {!load} and {!load_cursor} when the stored cursor for the
    scope's endpoint, account and mailbox key names a different raw name,
    encoding or mailbox ID than the requested scope. *)

val open_path : sw:Eio.Switch.t -> ?blob_dir:_ Eio.Path.t -> _ Eio.Path.t -> t
(** Opens or creates a versioned database. [blob_dir], when supplied, must
    already exist on a native filesystem and be controlled by this process.
    Raises on incompatible schema. *)

val open_readonly : sw:Eio.Switch.t -> _ Eio.Path.t -> t
(** Opens an existing version-8 through version-13 database with SQLite's [READONLY] flag and
    validates its schema. This never creates, migrates, or changes the
    database, and no blob directory is opened. Missing or incompatible
    databases raise. Reading a live WAL database may require an existing
    readable [-wal] and [-shm] pair, or a writable containing directory so
    SQLite can create [-shm]; for a strict no-file-write inspection, inspect
    a checkpointed database or a snapshot that includes those sidecars. *)

type mailbox = {
  cursor : Imap.Mirror.cursor;
  snapshot : Imap.Mirror.snapshot option;
}

val load : t -> scope:Imap.Mirror.scope -> mailbox
(** [load t ~scope] is the stored cursor and snapshot for [scope]. A missing
    mailbox yields [Mirror.initial scope] and no snapshot. A mismatched
    stored scope raises {!Scope_mismatch}. A corrupt row raises
    [Failure]. *)

type object_identity = { account_id:string; mailbox_id:string }

val object_identity : t -> scope:Imap.Mirror.scope -> object_identity option
(** A verified OBJECTID+ binding, or [None] for an unbound mailbox or a
    read-only pre-v12 database. The stored raw name and encoding must match
    the requested scope. *)

val observe_object_identity : t -> scope:Imap.Mirror.scope ->
  object_identity -> [ `Bound | `Matched | `Conflict ]
(** Atomically bind the first verified account/mailbox identity. A different
    identity for the logical scope, or one already bound to another logical
    scope on the same endpoint/account, returns [Conflict] without changing
    state. Invalid draft identifiers raise [Invalid_argument]. *)

val load_cursor : t -> scope:Imap.Mirror.scope -> Imap.Mirror.cursor
(** [load_cursor t ~scope] is the cursor of {!load} without the snapshot.
    It raises as {!load} does. *)

val snapshot_page : t -> scope:Imap.Mirror.scope ->
  cursor:Imap.Mirror.cursor -> ?after_uid:Imap.Proto.Uid.t ->
  limit:int -> unit -> [ `Rows of Imap.Mirror.row list | `Stale_revision ]
(** Page the current published epoch by UID. The caller's cursor revision,
    UIDVALIDITY and full scope must still match inside the read transaction,
    or the result is [`Stale_revision]. [limit] is 1..10,000. A new mailbox
    yields an empty page. A [cursor] for another scope or a [limit] out of
    range raises [Invalid_argument]. *)

val snapshot_contains_uid : t -> scope:Imap.Mirror.scope ->
  cursor:Imap.Mirror.cursor -> uid:Imap.Proto.Uid.t ->
  [ `Present of bool | `Stale_revision ]
(** Indexed membership check against the same published revision, epoch
    and full scope, or [`Stale_revision]. A complete published inventory is
    required before treating absence as deletion evidence. A [cursor] for
    another scope raises [Invalid_argument]. *)

type staged_receipt = {
  cursor : Imap.Mirror.cursor;
  row_count : int64;
}

val begin_stage : t -> cursor:Imap.Mirror.cursor ->
  action:Imap.Mirror.action -> unit
(** Create a uniquely named, durable scan stage. Stages surviving a crash are
    inert until explicitly discarded. *)

val seed_stage_from_published : t -> cursor:Imap.Mirror.cursor ->
  action:Imap.Mirror.action -> [ `Seeded | `Stale_revision ]
(** Copy the current epoch's published rows and flags into a new scan stage
    using SQLite statements, without materializing them in OCaml. The caller
    must cover the entire UID range with changed-row FETCH windows and then
    prove complete membership with SEARCH before publication. A stale cursor
    leaves the stage unseeded. *)

val stage_rows : ?preserve_newer:bool -> t -> stage_id:string ->
  first:int64 -> last:int64 ->
  Imap.Mirror.row list -> unit
(** Commit one contiguous FETCH window. The next window must start at the
    previous window's end plus one. Each call is atomic. With
    [preserve_newer=true], a row whose MODSEQ is older than its seeded stage
    counterpart is ignored, including its flags. *)

val stage_membership : t -> stage_id:string -> first:int64 -> last:int64 ->
  int64 list -> unit
(** Commit one contiguous SEARCH window. Every reported UID must have a
    staged FETCH row; otherwise the transaction fails without advancing
    coverage. SEARCH windows cannot overtake FETCH coverage. *)

val publish_stage : t -> cursor:Imap.Mirror.cursor ->
  action:Imap.Mirror.action ->
  explicit_highestmodseq:Imap.Proto.Modseq.t option ->
  nomodseq:bool ->
  [ `Committed of staged_receipt | `Stale_revision ]
(** Requires full FETCH and SEARCH coverage to the fixed upper UID. In one
    transaction, CAS-checks the cursor, replaces the current epoch's rows
    with SEARCH-confirmed stage rows, advances the cursor, and deletes the
    stage. No complete OCaml snapshot is materialized. *)

val discard_stage : t -> stage_id:string -> unit
val abandoned_stages : t -> string list
(** Inspect and explicitly remove incomplete stages, e.g. after restart.
    A stage is never automatically resumed or published. *)

val publish : t -> Imap.Mirror.transition -> [ `Committed | `Stale_revision ]
(** Compare-and-swap on the cursor revision, replacing the snapshot and
    cursor atomically. Old UIDVALIDITY epochs are retained for diagnosis.
    A stale write leaves the database untouched. *)

type intent_kind =
  | Append of {
      message_id : string;
      content_digest : string;
      spool_ref : string;
      pre_send_uid_frontier : int64 option;
      expected_length : int64 option;
      expected_flags : Mail_flag.Imap_flag.t list option;
      expected_internal_date : string option;
    }
  | Other of string

type intent_state = Prepared | Sent | Ambiguous | Confirmed | Rejected

type intent = {
  id : string;
  scope : Imap.Mirror.scope;
  kind : intent_kind;
  state : intent_state;
  uidvalidity : Imap.Proto.Uidvalidity.t option;
  uid : Imap.Proto.Uid.t option;
}

val prepare_intent : t -> intent -> unit
(** The caller supplies a globally unique ID. [Prepared] is committed before
    the network command is sent. Reusing an ID fails. APPEND reconciliation
    metadata is immutable once prepared; [None] fields mark legacy unknown
    values, while [Some []] flags mean known empty flags. The frontier is the
    last published UID bound before send, not proof of server state at send.
    New APPEND intents require a 64-character lowercase SHA-256 digest and,
    when supplied, a valid unquoted IMAP date-time. A [uid] requires a
    [uidvalidity]. Invalid metadata raises [Invalid_argument] without
    inserting an intent. Existing legacy metadata remains readable for
    inspection and explicit recovery. A legacy row with no stored message
    ID, digest or spool reference reads that field as the empty string. *)

val set_intent_state : t -> id:string -> intent_state -> unit
(** Legal transitions are Prepared -> Sent/Ambiguous/Rejected and
    Sent -> Ambiguous/Confirmed/Rejected and Ambiguous -> Confirmed/Rejected.
    A missing ID or illegal transition raises [Invalid_argument]. *)

val confirm_intent : t -> id:string ->
  uidvalidity:Imap.Proto.Uidvalidity.t option ->
  uid:Imap.Proto.Uid.t option -> unit
(** Resolve a sent or ambiguous operation and record an optional UIDPLUS
    [APPENDUID] receipt in the same transaction. [uid] requires
    [uidvalidity]. [uidvalidity = None] keeps the stored UIDVALIDITY. *)

val pending_intents : t -> scope:Imap.Mirror.scope -> intent list
(** Returns Prepared, Sent and Ambiguous operations for reconciliation.
    Sending an APPEND after restart requires app-specific duplicate detection;
    the journal does not itself claim exactly-once delivery. *)

val find_intent : t -> id:string -> intent option
(** Retrieve a pending or resolved intent, including a persisted UIDPLUS
    receipt recorded by [confirm_intent]. *)

module Sync : sig
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
end

module Blob : sig
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
end
