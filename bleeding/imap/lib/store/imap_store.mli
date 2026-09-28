@@ portable

(** Durable IMAP mailbox snapshots, sync journal and message blobs.

    A store is one SQLite database and, when one is given, a directory of
    message blobs. One Eio mutex per store serializes its operations, and
    every write commits in one transaction, so a call that raises leaves
    the database unchanged. {!open_path} runs the database in WAL mode with
    [synchronous=FULL] and refuses one where either cannot be set, so a
    committed write survives a crash as far as the filesystem honours
    flushes. A database on a network filesystem that SQLite does not
    support is unsafe. A call waits up to 5 seconds for a lock another
    process holds before it raises [Sqlite3.SqliteError].

    Publication is compare-and-swap. A write that depends on a published
    cursor or a pair names the revision it read and is [`Stale_revision],
    with nothing changed, once that revision has moved on. A snapshot is
    published whole in one transaction. Operations are journaled before
    their commands are sent and are never replayed. After a crash they stay
    pending until the caller reconciles them.

    A scope names a mailbox by endpoint, account and mailbox key, which
    key the stored rows, and also carries its raw name, encoding and
    mailbox ID. A check against the full scope compares all six.

    Unless a value says otherwise, a SQLite failure raises
    [Sqlite3.SqliteError], a stored row that cannot be decoded raises
    [Failure], and a blob I/O failure raises [Eio.Io].

    Every type other than {!t} is immutable data, so a portable closure
    may capture the records a store returns. It may also capture a store
    and call its functions, except {!open_path} and the {!Blob} functions
    that read or write the blob directory. *)

(** {1 Stores} *)

type t : value mod portable contended
(** The type for open stores. A store crosses portability and contention,
    since its mutex, which Eio makes safe to share between domains, guards
    every use of its SQLite connection. Its blob directory is used only by
    the nonportable functions, so it stays in the domain that opened the
    store. *)

exception Scope_mismatch
(** Raised by {!load_cursor} when the stored cursor for the scope's
    endpoint, account and mailbox key names another raw name, encoding or
    mailbox ID. *)

val open_path : sw:Eio.Switch.t -> ?blob_dir:_ Eio.Path.t -> _ Eio.Path.t ->
  t @@ nonportable
(** [open_path ~sw ~blob_dir path] opens the database at [path] for reading
    and writing, creating it and its schema when absent, and is the store,
    which [sw] owns. An existing database must hold exactly the schema of
    this library, recorded as SQLite [user_version] 1. [blob_dir] is
    omitted by default, and then the {!Blob} operations that touch files
    raise [Invalid_argument]. When given it is an existing directory on a
    native filesystem.

    @raise Invalid_argument if [blob_dir] is not an existing directory or
    has no native path.

    @raise Eio.Io if [path] cannot be opened.

    @raise Failure if the database has another [user_version], if a table
    or index differs from the schema, is missing or is not part of it, if
    the database has tables but no [user_version], or if WAL mode,
    [synchronous=FULL] or foreign keys cannot be enabled. *)

val open_readonly : sw:Eio.Switch.t -> _ Eio.Path.t -> t
(** [open_readonly ~sw path] opens the existing database at [path] with
    SQLite's read-only flag, validates its schema, and is the store, which
    [sw] owns. It never creates or changes the database, and opens no blob
    directory. Reading a live WAL database needs readable [-wal] and [-shm]
    files, or a writable containing directory where SQLite can create
    [-shm]. An inspection that must write no file reads a checkpointed
    database or a copy that includes those files.

    @raise Eio.Io if [path] cannot be opened.

    @raise Failure if the database has another [user_version] or its schema
    differs from that of this library, as for {!open_path}. *)

(** {1 Mailbox identity} *)

type object_identity = { account_id:string; mailbox_id:string }
(** The type for OBJECTID+ identities, the server's account and mailbox
    identifiers of a mailbox. *)

val object_identity : t -> scope:Imap.Mirror.scope ->
  [ `Bound of object_identity | `Unbound | `Conflict ]
(** [object_identity t ~scope] is the OBJECTID+ identity bound to [scope].
    It is [`Unbound] when none is bound, and [`Conflict] when the binding
    was made under another raw name or encoding. *)

val observe_object_identity : t -> scope:Imap.Mirror.scope ->
  object_identity -> [ `Bound | `Matched | `Conflict ]
(** [observe_object_identity t ~scope identity] binds [identity] to
    [scope] when [scope] has no binding, and is [`Bound]. It is [`Matched]
    when [scope] is already bound to [identity]. It is [`Conflict], with
    nothing changed, when [scope] is bound to another identity or under
    another raw name or encoding, or when another mailbox key of the same
    endpoint and account holds [identity].

    @raise Invalid_argument if an identifier of [identity] is empty, longer
    than 255 bytes, or holds a byte other than an ASCII letter, a digit,
    [_] or [-]. *)

(** {1 Cursors and snapshots} *)

val load_cursor : t -> scope:Imap.Mirror.scope -> Imap.Mirror.cursor
(** [load_cursor t ~scope] is the published cursor of [scope], or
    [Imap.Mirror.initial scope] when nothing is published.

    @raise Scope_mismatch if the stored cursor names another raw name,
    encoding or mailbox ID than [scope]. *)

val snapshot_page : t -> scope:Imap.Mirror.scope ->
  cursor:Imap.Mirror.cursor -> ?after_uid:Imap.Uid.t ->
  limit:int -> unit -> [ `Rows of Imap.Mirror.row list | `Stale_revision ]
(** [snapshot_page t ~scope ~cursor ~after_uid ~limit ()] is at most
    [limit] rows of the published snapshot of [scope] with UIDs above
    [after_uid], in ascending UID order. [after_uid] is omitted by default,
    which starts at the lowest UID. The page is read in one transaction
    that checks that [cursor] is still current, with the stored revision,
    UIDVALIDITY and full scope, and is [`Stale_revision] otherwise. A
    cursor without a UIDVALIDITY yields an empty page.

    @raise Invalid_argument if [cursor] is for another scope or [limit] is
    outside 1 to 10,000. *)

val snapshot_contains_uid : t -> scope:Imap.Mirror.scope ->
  cursor:Imap.Mirror.cursor -> uid:Imap.Uid.t ->
  [ `Present of bool | `Stale_revision ]
(** [snapshot_contains_uid t ~scope ~cursor ~uid] is [`Present b], where
    [b] holds when [uid] is in the published snapshot of [scope], with
    [cursor] checked as in {!snapshot_page}. A cursor without a
    UIDVALIDITY reports every UID absent, so absence is deletion evidence
    only under a published cursor.

    @raise Invalid_argument if [cursor] is for another scope. *)

(** {1 Scan stages and publication}

    A scan fills a stage with FETCH windows, then confirms membership with
    SEARCH windows, and {!publish_stage} replaces the published snapshot
    with the confirmed rows. Each window must continue the coverage of the
    previous one. *)

type staged_receipt = {
  cursor : Imap.Mirror.cursor;
  row_count : int64;
}
(** The type for publication receipts. [cursor] is the new published
    cursor and [row_count] the number of rows published. *)

val begin_stage : t -> cursor:Imap.Mirror.cursor ->
  action:Imap.Mirror.action -> unit
(** [begin_stage t ~cursor ~action] durably creates the empty stage
    [action.id] for the scan [action] planned from [cursor]. A stage that
    survives a crash stays inert until {!discard_stage} removes it.

    @raise Invalid_argument if [action] was planned for another scope,
    revision or generation than [cursor].

    @raise Sqlite3.SqliteError if a stage [action.id] exists. *)

val seed_stage_from_published : t -> cursor:Imap.Mirror.cursor ->
  action:Imap.Mirror.action -> [ `Seeded | `Stale_revision ]
(** [seed_stage_from_published t ~cursor ~action] copies the published rows
    and flags of [cursor]'s epoch, up to the upper UID of [action], into the
    stage of [action] without reading them into memory, and is [`Seeded].
    The stage gains no coverage, so the caller still covers the whole UID
    range with FETCH windows of the changed rows and proves complete
    membership with SEARCH windows before publication. When the stored
    revision or UIDVALIDITY differs from [cursor], the result is
    [`Stale_revision] and the stage stays empty.

    @raise Invalid_argument if [action] does not match [cursor] or its
    UIDVALIDITY, or if the stage is unknown, was begun for another cursor
    or action, or already has coverage. *)

val stage_rows : ?preserve_newer:bool -> t -> stage_id:string ->
  first:int64 -> last:int64 ->
  Imap.Mirror.row list -> unit
(** [stage_rows ~preserve_newer t ~stage_id ~first ~last rows] records
    [rows] as the FETCH result for the UIDs from [first] to [last] in the
    stage [stage_id], in one transaction, and extends its FETCH coverage to
    [last]. [first] is 1 for the first window and one above the previous
    window's [last] after that, and [last] is at most the stage's upper
    UID. A row replaces the staged row of its UID, flags included.
    [preserve_newer] defaults to [false]. When [true], a row whose MODSEQ
    is below that of the staged row for its UID is ignored, flags
    included.

    @raise Invalid_argument if the stage is unknown, if the window is empty
    or does not continue the coverage, if a row lies outside the window, or
    if [preserve_newer] holds and a row or its staged counterpart lacks a
    MODSEQ. *)

val stage_membership : t -> stage_id:string -> first:int64 -> last:int64 ->
  Imap.Uid.t list -> unit
(** [stage_membership t ~stage_id ~first ~last uids] records [uids] as the
    SEARCH result for the UIDs from [first] to [last] in the stage
    [stage_id], in one transaction, and extends its SEARCH coverage to
    [last]. [first] continues the previous SEARCH window as in
    {!stage_rows}, and FETCH coverage must already reach [last]. Only rows
    that a SEARCH window confirmed are published.

    @raise Invalid_argument if the stage is unknown, if the window is
    empty, does not continue the coverage or passes the FETCH coverage, or
    if a UID of [uids] is outside the window, repeated or without a staged
    row. Nothing is recorded then. *)

val publish_stage : t -> cursor:Imap.Mirror.cursor ->
  action:Imap.Mirror.action ->
  explicit_highestmodseq:Imap.Modseq.t option ->
  nomodseq:bool ->
  [ `Committed of staged_receipt | `Stale_revision ]
(** [publish_stage t ~cursor ~action ~explicit_highestmodseq ~nomodseq]
    publishes the stage of [action] in one transaction and is the receipt.
    The stage needs FETCH and SEARCH coverage up to the upper UID of
    [action]. When the stored revision differs from [cursor], the result
    is [`Stale_revision] and nothing changes, whatever the coverage of the
    stage. Otherwise the rows of the epoch of [action] become the staged
    rows a SEARCH window confirmed, blob references to UIDs no longer
    present are dropped, the
    cursor advances to the next revision and generation with [action.id]
    as its inventory reference, and the stage is deleted. Other epochs
    keep their rows until {!forget_epochs}.

    The new cursor is in baseline mode when [nomodseq] holds and in the
    mode of [action] otherwise. Its MODSEQ anchor is
    [explicit_highestmodseq] in CONDSTORE mode and [None] in baseline
    mode, so a CONDSTORE publication without an explicit HIGHESTMODSEQ
    anchors [None].

    @raise Invalid_argument if [action] does not match [cursor], if the
    stage is unknown, was begun for another cursor or action, or lacks full
    coverage, or if the new anchor is below the previous anchor of
    [action]. *)

val discard_stage : t -> stage_id:string -> unit
(** [discard_stage t ~stage_id] deletes the stage [stage_id] with its rows.
    An unknown [stage_id] is ignored. *)

val abandoned_stages : t -> string list
(** [abandoned_stages t] is the ID of every stage neither published nor
    discarded, in ascending order. A stage is never resumed or published
    automatically. *)

val forget_epochs : t -> scope:Imap.Mirror.scope ->
  cursor:Imap.Mirror.cursor -> [ `Dropped of int | `Stale_revision ]
(** [forget_epochs t ~scope ~cursor] deletes the snapshot rows and blob
    references of every UIDVALIDITY epoch of [scope] other than that of
    [cursor], and is [`Dropped n] for the [n] epochs removed. When the
    stored revision or UIDVALIDITY differs from [cursor] it changes
    nothing and is [`Stale_revision]. Blobs referenced only by a
    dropped epoch become orphan candidates.

    @raise Invalid_argument if [cursor] is for another scope. *)

(** {1 Sync journal} *)

module Journal : sig
  (** Pairs, conflicts and operations of a bidirectional sync.

      A pair links a remote message and a local Maildir occurrence and
      holds their last common state. A conflict records why a pair does not
      converge. An operation journals one mutation before it is sent. No
      value of this module performs IMAP or Maildir I/O, and a pending
      operation is never replayed after a crash.

      A read by scope raises [Failure] when a stored record for the key of
      the scope names another raw name, encoding or mailbox ID. *)

  (** {2 Pairs} *)

  type tombstone_reason = Inventory_absence | Expunge_receipt
    | Local_absence | Explicit_delete | Retention
  (** The type for reasons a side of a pair is gone. [Inventory_absence] is
      a remote UID missing from a complete published inventory, and
      [Expunge_receipt] a remote expunge the client carried out.
      [Local_absence] is an occurrence missing from a complete Maildir
      inventory, and [Retention] one that local retention removed, not a
      user. [Explicit_delete] is a deliberate deletion of that side, such
      as the sync's own unlink of a local occurrence. *)

  type tombstone = {
    reason : tombstone_reason;
    evidence : string;
    generation : int64 option;
  }
  (** The type for tombstones. [evidence] is nonempty and names what proves
      the absence, such as an inventory reference or an operation ID.
      [generation] is the published scan generation at which the absence
      was first recorded, when known. *)

  type pair = {
    id : string;
    scope : Imap.Mirror.scope;
    remote_uidvalidity : Imap.Uidvalidity.t option;
    remote_uid : Imap.Uid.t option;
    local_id : string option;
    content_sha256 : string option;
    content_length : int64 option;
    internal_date : Imap.Internal_date.t option;
    common_flags : Mail_flag.Imap_flag.t list;
    remote_tombstone : tombstone option;
    local_tombstone : tombstone option;
    revision : int64;
  }
  (** The type for pairs. [remote_uidvalidity] and [remote_uid] are given
      together, and at least one side is bound. [content_sha256],
      [content_length] and [internal_date] are the common content evidence,
      [None] when it was not recorded. [common_flags] is the last flag
      state both sides agreed on. [revision] is the compare-and-swap
      revision. *)

  val put_pair : t -> expected_revision:int64 option -> pair ->
    [ `Committed of pair | `Stale_revision ]
  (** [put_pair t ~expected_revision pair] writes [pair] with its flags and
      tombstones in one transaction, and is the committed pair at the next
      revision. [expected_revision = None] creates [pair], which needs
      revision 0 and an unused ID. [Some r] updates the stored pair, which
      must be at revision [r], as must [pair]. Any other case is
      [`Stale_revision] with nothing changed.

      The ID and scope never change, and neither does an occurrence
      identity or content evidence once bound. A pair without content
      evidence may keep [None], and the caller then holds deletion
      propagation for it. A tombstone is never cleared, and is replaced
      only by one with the same or a more permanent reason, in the order
      absence, then [Expunge_receipt] or [Retention], then
      [Explicit_delete]. A new remote [Inventory_absence] tombstone needs
      the published inventory reference as its evidence and the published
      generation as its generation, and its UID must be absent from the
      published snapshot.

      @raise Invalid_argument if [pair] breaks one of these rules or is
      malformed. A malformed pair has an empty ID or local ID, a negative
      revision or length, a UID without a UIDVALIDITY or no bound side, a
      tombstone on an unbound side or with a reason of the other side,
      empty tombstone evidence or a negative generation, a digest without
      a length or not of 64 lowercase hexadecimal digits, or [\Recent] or
      a repeated flag in [common_flags]. *)

  val find_pair : t -> id:string -> pair option
  (** [find_pair t ~id] is the pair [id], or [None]. *)

  val note_presence : t -> pair:pair -> side:[ `Remote | `Local ] ->
    generation:int64 -> [ `Recorded | `Stale_revision ]
  (** [note_presence t ~pair ~side ~generation] records that the complete
      scan published at [generation] saw the [side] occurrence of [pair],
      and is [`Recorded]. The record keeps the highest generation noted.
      For [`Remote] the store checks the UID against the published
      snapshot. For [`Local] the caller has verified presence in a complete
      Maildir inventory. The result is [`Stale_revision] when the stored
      pair differs from [pair] or a later publication has replaced
      [generation].

      @raise Invalid_argument if [generation] is negative, if [pair] has no
      occurrence on [side], if no complete inventory is published or
      [generation] is ahead of it, or if for [`Remote] the published epoch
      is not the pair's UIDVALIDITY or lacks its UID. *)

  val last_presence_generation : t -> pair_id:string ->
    side:[ `Remote | `Local ] -> int64 option
  (** [last_presence_generation t ~pair_id ~side] is the highest
      generation at which {!note_presence} recorded the [side] occurrence
      of [pair_id], or [None] if it never did. *)

  val reactivate_local : t -> pair:pair -> generation:int64 ->
    [ `Reactivated of pair | `Stale_revision ]
  (** [reactivate_local t ~pair ~generation] clears the [Local_absence]
      tombstone of [pair] and is the pair at its next revision. The caller
      first verifies the same Maildir occurrence, with the saved digest,
      length and INTERNALDATE, in a complete local inventory, and records
      it with {!note_presence} at [generation]. No other tombstone can be
      cleared. The result is [`Stale_revision] when the stored pair
      differs from [pair].

      @raise Invalid_argument unless the local tombstone is
      [Local_absence], the local presence is recorded at [generation] and
      not below the tombstone's generation, and [generation] is the
      published generation. *)

  val find_remote : t -> scope:Imap.Mirror.scope ->
    uidvalidity:Imap.Uidvalidity.t -> uid:Imap.Uid.t -> pair option
  (** [find_remote t ~scope ~uidvalidity ~uid] is the pair of [scope] bound
      to the remote UID [uid] of epoch [uidvalidity], or [None]. *)

  val find_local :
    t -> scope:Imap.Mirror.scope -> local_id:string -> pair option
  (** [find_local t ~scope ~local_id] is the pair of [scope] bound to the
      local occurrence [local_id], or [None]. *)

  val pairs_page : t -> scope:Imap.Mirror.scope -> ?after:string ->
    limit:int -> unit -> pair list
  (** [pairs_page t ~scope ~after ~limit ()] is at most [limit] pairs of
      [scope] with IDs above [after], in ascending ID order. [after] is
      omitted by default, which starts at the lowest ID. The next page
      starts after the last ID returned, and a page shorter than [limit] is
      the last.

      @raise Invalid_argument if [limit] is outside 1 to 10,000. *)

  (** {2 Conflicts} *)

  type conflict_kind = Flag_conflict | Identity_conflict | Content_conflict
    | Delete_conflict | Policy_conflict | Deletion_hold
  (** The type for conflict kinds. [Deletion_hold] records a deletion that
      the deletion policy holds. *)

  type conflict = {
    id : string; pair_id : string; kind : conflict_kind;
    evidence : string; pair_revision : int64; resolved : bool;
  }
  (** The type for conflicts. [evidence] is nonempty. [pair_revision] is
      the pair revision the conflict was recorded against. *)

  val record_conflict : t -> conflict -> unit
  (** [record_conflict t c] records the open conflict [c].

      @raise Invalid_argument if [c] has an empty ID or evidence, a
      negative pair revision or [resolved] set, or if its pair is missing
      or not at [c.pair_revision].

      @raise Sqlite3.SqliteError if a conflict [c.id] exists. *)

  val ensure_open_conflict : t -> pair:pair -> kind:conflict_kind ->
    id:string -> evidence:string -> [ `Open of conflict | `Stale_revision ]
  (** [ensure_open_conflict t ~pair ~kind ~id ~evidence] is the one open
      conflict of [kind] for [pair] with [evidence]. It creates the conflict
      as [id], or updates the evidence and pair revision of an open one,
      which keeps its ID. The result is [`Stale_revision] when the stored
      pair differs from [pair].

      @raise Invalid_argument if [id] or [evidence] is empty. *)

  val resolve_open_conflicts : t -> pair:pair -> kind:conflict_kind ->
    [ `Resolved of int | `Stale_revision ]
  (** [resolve_open_conflicts t ~pair ~kind] resolves every open conflict
      of [kind] for [pair] and is [`Resolved n] for the [n] resolved. The
      result is [`Stale_revision] when the stored pair differs from [pair].
      A caller resolves only after proving independently that the
      condition is gone. *)

  val has_open_conflict : t -> pair:pair -> kind:conflict_kind -> bool
  (** [has_open_conflict t ~pair ~kind] holds when the pair with the ID of
      [pair] has an open conflict of [kind]. *)

  val resolve_conflict : t -> id:string -> unit
  (** [resolve_conflict t ~id] resolves the open conflict [id].

      @raise Invalid_argument if no open conflict is [id]. *)

  val open_conflicts_page : t -> scope:Imap.Mirror.scope -> ?after:string ->
    limit:int -> unit -> conflict list
  (** [open_conflicts_page t ~scope ~after ~limit ()] is at most [limit]
      open conflicts of [scope] with IDs above [after], in ascending ID
      order. [after] is omitted by default, which starts at the lowest ID.
      The next page starts after the last ID returned, and a page shorter
      than [limit] is the last.

      @raise Invalid_argument if [limit] is outside 1 to 10,000. *)

  (** {2 Operations} *)

  type operation_kind = Append | Local_append | Copy | Move | Flags
    | Delete | Local_delete
  (** The type for journaled mutation kinds. [Local_append] writes a
      remote message into the Maildir and [Local_delete] removes a local
      occurrence. *)

  type operation_state = Prepared | Sent | Ambiguous | Observed
    | Committed | Rejected
  (** The type for operation states. [Prepared] is journaled before
      dispatch and [Sent] after it. [Ambiguous] has an unknown outcome.
      [Observed] has a verified receipt not yet committed to a pair.
      [Committed] and [Rejected] are final, and the others are active. *)

  type append = {
    message_id : string;
    spool_ref : string;
    pre_send_frontier : int64;
  }
  (** The type for the recovery metadata of an APPEND. [message_id] names
      the message for the application's duplicate detection and
      [spool_ref] where its bytes stay available. [pre_send_frontier] is
      the published UID frontier of the destination when the APPEND was
      prepared, 0 for a mailbox with nothing published, and bounds the
      UIDs an ambiguous APPEND can have produced. It is not proof of the
      server state at the send. *)

  type operation = {
    id : string;
    pair_id : string option;
    local_id : string option;
    scope : Imap.Mirror.scope;
    kind : operation_kind;
    state : operation_state;
    source_uidvalidity : Imap.Uidvalidity.t option;
    source_uid : Imap.Uid.t option;
    destination : Imap.Mirror.scope option;
    destination_uidvalidity : Imap.Uidvalidity.t option;
    blob_sha256 : string option;
    blob_length : int64 option;
    desired_flags : Mail_flag.Imap_flag.t list option;
    internal_date : Imap.Internal_date.t option;
    append : append option;
    receipt : string option;
    receipt_uidvalidity : Imap.Uidvalidity.t option;
    receipt_uid : Imap.Uid.t option;
  }
  (** The type for operations. [source_uidvalidity] and [source_uid] name
      the remote message acted on. [destination] and
      [destination_uidvalidity] name the target mailbox of an APPEND, COPY
      or MOVE, and for an APPEND the epoch it was sent under.
      [blob_sha256] and [blob_length] name the content. [desired_flags] is
      the target flag state of a FLAGS operation or an APPEND and the flag
      preimage of a deletion. [internal_date] is the INTERNALDATE an APPEND
      sends, or the remote INTERNALDATE a local append saves before its
      Maildir write so that recovery can reject an altered Maildir
      timestamp. [append] holds the recovery metadata of an APPEND and is
      [None] for every other kind. [receipt], [receipt_uidvalidity] and
      [receipt_uid] record the outcome. *)

  val prepare_operation : ?local_flags:Mail_flag.Imap_flag.t list ->
    ?local_source_mtime:float -> t -> operation -> unit
  (** [prepare_operation ~local_flags ~local_source_mtime t op] journals
      [op] in the [Prepared] state before dispatch. Its source,
      destination and desired change never change afterwards.
      [op.local_id] reserves a stable Maildir occurrence name before a
      remote-to-local write. For [op.pair_id] the pair's
      current revision is saved as the commit precondition, and [op] must
      match the pair's scope, local occurrence and any given remote UID and
      UIDVALIDITY. A FLAGS operation or a deletion that names content must
      match the pair's, and a deletion's flag preimage, when given, must
      equal the pair's common flags. An APPEND needs [op.append], with a
      nonempty message ID and spool reference and a frontier from 0 to
      4,294,967,295, and only an APPEND or a local append may carry
      [op.internal_date].

      [local_flags] is omitted by default. For a paired FLAGS operation it
      saves the Maildir flag preimage, possibly empty. An operation without
      it cannot finish a one-sided remote write. [local_source_mtime] is
      omitted by default. For an APPEND with a [local_id] it saves the
      scanned Maildir file time as the source preimage.

      @raise Invalid_argument if [op] is not [Prepared], carries a receipt,
      is incomplete for its kind or contradicts its pair, or if an optional
      argument does not fit the kind of [op] or is not finite. Nothing is
      journaled then.

      @raise Sqlite3.SqliteError if an operation [op.id] exists. *)

  val operation_source_mtime : t -> id:string -> float option
  (** [operation_source_mtime t ~id] is the Maildir file time saved when
      the APPEND [id] was prepared, or [None] when none was saved. *)

  val local_flags_preimage : t -> id:string ->
    Mail_flag.Imap_flag.t list option
  (** [local_flags_preimage t ~id] is the Maildir flag preimage saved with
      the FLAGS operation [id], or [None] when none was saved. *)

  val operation_pair_revision : t -> id:string -> int64 option
  (** [operation_pair_revision t ~id] is the pair revision saved when [id]
      was prepared, or [None] for an unpaired operation. *)

  val mark_sent : t -> id:string -> unit
  (** [mark_sent t ~id] moves the prepared operation [id] to [Sent].

      @raise Invalid_argument if [id] is unknown or not [Prepared]. *)

  val mark_ambiguous : ?reason:string -> t -> id:string -> unit
  (** [mark_ambiguous ~reason t ~id] moves the prepared or sent operation
      [id] to [Ambiguous], whose outcome is unknown. It is never used for a
      mutation proven unsent. [reason] is omitted by default. When given it
      is kept as the receipt, shown in read-only inspection, until a
      verified receipt replaces it.

      @raise Invalid_argument if [reason] is empty or longer than 4,096
      bytes, or if [id] is unknown or neither [Prepared] nor [Sent]. *)

  val reject_operation : t -> id:string -> receipt:string -> unit
  (** [reject_operation t ~id ~receipt] moves the active operation [id] to
      [Rejected] with [receipt] as its evidence.

      @raise Invalid_argument if [receipt] is empty, or if [id] is unknown
      or not active. *)

  val reject_prepared_operation : t -> id:string -> receipt:string -> unit
  (** [reject_prepared_operation t ~id ~receipt] is {!reject_operation} for
      an operation that is still [Prepared], and so never dispatched. An
      operation marked [Sent] meanwhile is refused, so a concurrent
      dispatch is never classified as unsent.

      @raise Invalid_argument if [receipt] is empty, or if [id] is unknown
      or not [Prepared]. *)

  val observe_operation : t -> id:string -> receipt:string ->
    destination_uidvalidity:Imap.Uidvalidity.t option ->
    destination_uid:Imap.Uid.t option -> unit
  (** [observe_operation t ~id ~receipt ~destination_uidvalidity
      ~destination_uid] moves the sent or ambiguous operation [id] to
      [Observed] with the verified [receipt] and the destination UID
      [destination_uid] of epoch [destination_uidvalidity] it names.

      @raise Invalid_argument if [receipt] is empty, if [destination_uid]
      is given without [destination_uidvalidity], or if [id] is unknown or
      neither [Sent] nor [Ambiguous]. *)

  val commit_operation : t -> id:string -> unit
  (** [commit_operation t ~id] moves the observed unpaired operation [id]
      to [Committed]. A paired operation commits with
      {!commit_operation_with_pair}, so that its common state advances in
      the same transaction. A [Sent] or [Ambiguous] operation is reconciled
      before it commits.

      @raise Invalid_argument if [id] is not an observed unpaired
      operation. *)

  val commit_operation_with_pair : t -> id:string ->
    expected_pair_revision:int64 option -> pair ->
    [ `Committed of pair | `Stale_revision ]
  (** [commit_operation_with_pair t ~id ~expected_pair_revision pair]
      writes [pair] as the last common state and moves the observed
      operation [id] to [Committed], in one transaction, and is the
      committed pair. [expected_pair_revision] is as in {!put_pair}.
      [None] creates the pair of an unpaired operation. For a paired
      operation it must equal both the revision {!prepare_operation} saved
      and the stored revision, else the result is [`Stale_revision] and the
      operation stays observed. A committed FLAGS operation also
      resolves the pair's open flag conflicts when no other FLAGS operation
      on the pair is active.

      [pair] must agree with the evidence of the operation. Its local ID,
      its remote UID and UIDVALIDITY, its scope, the given content and the
      desired flags must match, where the remote identity is the receipt
      and the scope the destination for an APPEND, COPY or MOVE, and the
      source otherwise. A remote creation must also match any expected
      destination UIDVALIDITY. A deletion needs the tombstone of its side,
      and other kinds need both sides live. A FLAGS operation or a
      deletion may change only the flags or that tombstone of the stored
      pair. A local append's pair carries the operation's INTERNALDATE,
      when it has one. The rules of {!put_pair} apply.

      @raise Invalid_argument if [id] is not observed, if [pair] is not the
      operation's pair, if [expected_pair_revision] is [None] for a paired
      operation or given for an unpaired one, or if [pair] contradicts the
      operation's evidence. Nothing is committed then. *)

  (** {2 Operator repairs}

      The three repairs below act on one active operation of a pair after
      an operator verified both endpoints under the writer lease. They
      perform no network or Maildir write. Each needs [pair] to be the
      stored pair at the revision {!prepare_operation} saved, else the
      result is [`Stale_revision]. An operation of another kind, state or
      occurrence identity, or another active operation on the pair, is
      [`Invalid_operation]. [evidence] is nonblank, at most 1,024 bytes
      and free of control characters. *)

  val settle_flag_operation : t -> id:string -> pair ->
    flags:Mail_flag.Imap_flag.t list -> evidence:string ->
    [ `Settled of pair | `Stale_revision | `Invalid_operation ]
  (** [settle_flag_operation t ~id pair ~flags ~evidence] replaces the
      common flags of [pair] with [flags], rejects the sent, ambiguous or
      observed FLAGS operation [id] with [evidence], and resolves the
      pair's open flag conflicts, in one transaction, and is the settled
      pair. It follows the operator's check that the remote and local flags
      both equal [flags]. A pair with a tombstone is [`Invalid_operation].

      @raise Invalid_argument if [evidence] is invalid or [flags] breaks
      the flag rules of {!put_pair}. *)

  val reject_unchanged_delete_operation : t -> id:string -> pair ->
    evidence:string ->
    [ `Rejected | `Stale_revision | `Invalid_operation ]
  (** [reject_unchanged_delete_operation t ~id pair ~evidence] rejects the
      sent or ambiguous remote DELETE [id] of [pair] with [evidence] and is
      [`Rejected]. It follows the operator's check that the remote UID
      still holds its saved bytes and common flags. The pair needs a
      [Local_absence] tombstone and no remote tombstone. The operation's
      digest and length must equal the pair's, and its flag preimage, when
      present, must equal the pair's common flags as a set, else the
      result is [`Invalid_operation]. A later deletion needs a new
      operation.

      @raise Invalid_argument if [evidence] is invalid. *)

  val attest_targeted_expunge : t -> id:string -> pair ->
    evidence:string ->
    [ `Attested | `Stale_revision | `Invalid_operation ]
  (** [attest_targeted_expunge t ~id pair ~evidence] records the operator's
      authorization of a targeted UID EXPUNGE for the sent or ambiguous
      remote DELETE [id] of [pair] and is [`Attested]. The operation is
      left [Ambiguous] before the command is sent, so recovery never
      replays it. Immediately before, the operator verifies the same remote
      UID, its original bytes, the expected [\Deleted] flag and a stable
      MODSEQ. The checks of {!reject_unchanged_delete_operation} apply.

      @raise Invalid_argument if [evidence] is invalid or would grow the
      operation's receipt beyond 4,096 bytes. *)

  (** {2 Reading operations} *)

  val find_operation : t -> id:string -> operation option
  (** [find_operation t ~id] is the operation [id] in any state, or
      [None]. *)

  val active_operations_page : t -> scope:Imap.Mirror.scope ->
    ?after:string -> limit:int -> unit -> operation list
  (** [active_operations_page t ~scope ~after ~limit ()] is at most [limit]
      active operations of [scope] with IDs above [after], in ascending ID
      order. [after] is omitted by default, which starts at the lowest ID.
      The next page starts after the last ID returned, and a page shorter
      than [limit] is the last. An operation that reaches a final state
      between calls drops out of later pages.

      @raise Invalid_argument if [limit] is outside 1 to 10,000. *)

  val active_operation_for_pair : t -> pair_id:string -> operation option
  (** [active_operation_for_pair t ~pair_id] is the active operation with
      the lowest ID on the pair [pair_id], or [None]. Pair IDs are unique
      across scopes, so a caller can hold a pair while any mutation on it
      is pending without reading the whole journal. *)
end

(** {1 Message blobs} *)

module Blob : sig
  (** Content-addressed message bodies and their snapshot references.

      A blob is a file in the blob directory named by the SHA-256 of its
      bytes. A reference ties a blob to a message of a snapshot. The
      operations that read or write files raise [Invalid_argument] on a
      store opened without a blob directory. *)

  type blob = private { sha256 : string; length : int64 }
  (** The type for blobs. [sha256] is the lowercase hexadecimal digest and
      [length] the size in bytes. *)

  exception Digest_mismatch
  (** Raised by {!put} when the bytes do not have the expected digest. *)

  val put : t -> source:_ Eio.Flow.source -> length:int64 ->
    ?expected_sha256:string -> unit -> blob
  (** [put t ~source ~length ~expected_sha256 ()] reads exactly [length]
      bytes from [source] into the blob directory and is their blob. The
      file and its directory are synced before [put] returns. Bytes after
      [length] stay unread. [expected_sha256] is omitted by default. A
      failure removes the partial file and adds no reference, and a crash
      can leave an orphan file.

      @raise Digest_mismatch if [expected_sha256] is given and differs from
      the digest of the bytes.

      @raise Invalid_argument if [length] is negative or [expected_sha256]
      is not 64 lowercase hexadecimal digits.

      @raise End_of_file if [source] ends before [length] bytes. *)

  val verify : t -> blob -> bool
  (** [verify t blob] holds when the file of [blob] is a regular file of
      [blob.length] bytes with the digest [blob.sha256]. A missing file is
      [false]. *)

  val open_in : t -> sw:Eio.Switch.t -> blob -> Eio.File.ro_ty Eio.Resource.t
  (** [open_in t ~sw blob] is the file of [blob] open for reading, owned by
      [sw]. It does not check the digest, which {!verify} does. *)

  val attach : ?verify:bool -> t -> scope:Imap.Mirror.scope ->
    uidvalidity:Imap.Uidvalidity.t -> uid:Imap.Uid.t ->
    blob -> unit
  (** [attach ~verify t ~scope ~uidvalidity ~uid blob] references [blob]
      from the message [uid] of the published epoch [uidvalidity] of
      [scope] in one transaction, replacing any previous reference.
      [verify] defaults to [true], and then {!verify} checks [blob] first.
      [~verify:false] suits only a blob that {!put} returned immediately
      before.

      @raise Invalid_argument if [verify] holds and [blob] is missing or
      corrupt, if the published cursor of [scope] is for another epoch or
      names another raw name, encoding or mailbox ID, or if [uid] is not in
      the published snapshot. *)

  val find : t -> scope:Imap.Mirror.scope ->
    uidvalidity:Imap.Uidvalidity.t -> uid:Imap.Uid.t ->
    blob option @@ portable
  (** [find t ~scope ~uidvalidity ~uid] is the blob the message [uid] of
      epoch [uidvalidity] of [scope] references, or [None]. *)

  val missing_page : t -> scope:Imap.Mirror.scope ->
    cursor:Imap.Mirror.cursor -> ?after_uid:Imap.Uid.t ->
    limit:int -> unit ->
    [ `Uids of Imap.Uid.t list | `Stale_revision ] @@ portable
  (** [missing_page t ~scope ~cursor ~after_uid ~limit ()] is at most
      [limit] UIDs of the published snapshot of [scope] above [after_uid],
      in ascending order, whose messages have no blob reference, with
      [cursor] checked as in {!snapshot_page}. [after_uid] is omitted by
      default, which starts at the lowest UID. A concurrent {!attach} can
      shrink later pages.

      @raise Invalid_argument if [cursor] is for another scope or [limit]
      is outside 1 to 10,000. *)

  val referenced_page : t -> scope:Imap.Mirror.scope ->
    cursor:Imap.Mirror.cursor -> ?after_uid:Imap.Uid.t ->
    limit:int -> unit ->
    [ `Refs of (Imap.Uid.t * blob) list | `Stale_revision ] @@ portable
  (** [referenced_page t ~scope ~cursor ~after_uid ~limit ()] is at most
      [limit] messages of the published snapshot of [scope] above
      [after_uid] with their blobs, in ascending UID order, for the
      messages that reference one, with [cursor] checked as in
      {!snapshot_page}. [after_uid] is omitted by default, which starts at
      the lowest UID.

      @raise Invalid_argument if [cursor] is for another scope or [limit]
      is outside 1 to 10,000. *)

  val detach_if_matches : t -> scope:Imap.Mirror.scope ->
    cursor:Imap.Mirror.cursor -> uid:Imap.Uid.t -> blob ->
    [ `Detached | `Unchanged | `Stale_revision ] @@ portable
  (** [detach_if_matches t ~scope ~cursor ~uid blob] removes the reference
      from the message [uid] to [blob], after the caller found the file
      missing or corrupt, and is [`Detached]. It does not remove the file.
      A reference to another blob, or none, is [`Unchanged]. [cursor] is
      checked as in {!snapshot_page}.

      @raise Invalid_argument if [cursor] is for another scope. *)

  val iter_orphan_candidates : t -> (string -> unit) -> unit
  (** [iter_orphan_candidates t f] applies [f] to the name of every orphan
      candidate in the blob directory, in unspecified order. A candidate is
      a regular file that is either a temporary file or a blob that no
      snapshot of a retained epoch and no active sync operation
      references. A quarantined epoch's blobs become
      candidates only after {!forget_epochs} drops it. At most 256
      directory names are held in memory. [f] runs without the database
      lock and must not create files or references. Every blob writer,
      including one in another process, stays quiescent until the
      iteration ends. An exception from [f], or cancellation, closes the
      directory and propagates. *)

  val reap_orphans_iter : t -> removed:(string -> unit) -> unit
  (** [reap_orphans_iter t ~removed] removes every orphan candidate with
      the memory bound and quiescence rule of {!iter_orphan_candidates},
      calling [removed name] after each removal. When any removal was
      attempted, the directory is synced on return, exception or
      cancellation, and a failed sync after an exception does not replace
      it. [removed] runs before that sync, so it does not prove
      durability. A crash during reaping leaves the remaining candidates
      for the next call. *)
end @@ nonportable
