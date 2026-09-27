(** Mailbox scans, body transfers and journaled APPEND for one IMAP mailbox.

    {!scan_once} publishes a complete UID inventory through a disk-backed
    stage in {!Imap_store}. With a published CONDSTORE anchor in the same
    epoch it fetches only the metadata changed since that anchor with UID
    FETCH CHANGEDSINCE, plus the UIDs above the published frontier, and
    still proves membership with a complete UID SEARCH. It never uses
    QRESYNC. FETCH and SEARCH are separate commands, so a scan is not a
    snapshot of one instant, and a later scan catches concurrent edits. *)

val scan_once :
  ?max_windows:int -> ?expected_uidvalidity:Imap.Uidvalidity.t ->
  ctx:Ctx.t -> stage_id:string -> unit ->
  (Imap_store.staged_receipt, Error.t) result
(** [scan_once ~ctx ~stage_id ()] selects [ctx.mailbox] read-only, stages
    its FETCH and SEARCH windows of 1,000 UIDs in SQLite under [stage_id],
    and publishes the complete membership in one cursor compare-and-swap
    transaction. The receipt carries the new cursor and row count. No
    OCaml snapshot of the mailbox is built.

    With a CONDSTORE anchor from a complete scan of the same epoch, the
    stage is seeded from the published rows, only rows changed since the
    anchor are fetched below the published frontier, and every UID above it
    is fetched in full. Every other case fetches every window in full. The
    anchor advances only with a complete publication. [max_windows]
    defaults to 100,000 and bounds the number of windows, and a larger
    range returns [Limit]. [expected_uidvalidity] rejects a changed epoch
    with [Uidvalidity_changed] before any row is staged.

    A protocol error discards the stage. A crash leaves an inert stage that
    {!Imap_store.abandoned_stages} lists. When OBJECTID+ is offered and no
    binding is saved, the scan binds the mailbox identity only when the
    selected UIDVALIDITY equals the published one, so a scan that publishes
    a new epoch leaves the scope unbound and the next scan binds it.
    [stage_id] must be unique. *)

type append_outcome =
  | Identified of Imap_eio.Client.append_receipt
  | Needs_reconciliation

val append_journaled :
  ctx:Ctx.t -> id:string -> message_id:string -> content_digest:string ->
  spool_ref:string -> ?flags:Mail_flag.Imap_flag.t list ->
  ?internal_date:Imap.Internal_date.t -> length:int64 ->
  _ Eio.Flow.source -> (append_outcome, Error.t) result
(** [append_journaled ~ctx ~id ~message_id ~content_digest ~spool_ref
    ~length source] appends the [length] bytes of [source] to [ctx.mailbox]
    after committing the intent [id] as [Prepared] and then [Sent]. A tagged
    OK without APPENDUID, a disconnect or a cancellation leaves the intent
    pending for reconciliation, and it is never replayed. The caller
    provides a durable spool reference and a verified content digest.
    [internal_date] and [flags], which default to none, are saved in the
    intent before the send. A saved OBJECTID+ binding requires OBJECTID+ to
    be enabled on [ctx.client] already, as a scan or a body transfer does,
    and a destination whose STATUS identity differs from it returns
    [Invalid_scope] before any intent is saved. *)

val append_blob_journaled :
  ctx:Ctx.t -> id:string -> message_id:string ->
  ?flags:Mail_flag.Imap_flag.t list -> ?internal_date:Imap.Internal_date.t ->
  Imap_store.Blob.blob -> (append_outcome, Error.t) result
(** [append_blob_journaled ~ctx ~id ~message_id blob] verifies [blob] and
    sends it through {!append_journaled}. A blob that fails verification is
    [Incomplete]. The blob stays available for recovery when the outcome is
    ambiguous. *)

type archived = {
  blob : Imap_store.Blob.blob;
  flags : Mail_flag.Imap_flag.t list;
      (** [flags] are the durable flags read after the body. *)
  internal_date : Imap.Internal_date.t;
      (** [internal_date] is the INTERNALDATE read after the body. *)
}

val archive_uid :
  ?max_bytes:int64 -> ctx:Ctx.t -> uid:Imap.Uid.t -> spool:_ Eio.Path.t ->
  unit -> (archived, Error.t) result
(** [archive_uid ~ctx ~uid ~spool ()] fetches the exact BODY.PEEK[] bytes of
    [uid] into [spool], then reads its flags and INTERNALDATE in the same
    selection. Only after both commands complete does it put a synced
    content-addressed blob and attach it to the published snapshot.
    [spool] must not exist. The call creates it and removes it on every
    exit, and an existing file at [spool] raises [Eio.Io] and is left in
    place. [max_bytes] defaults to 1 GiB. Before any body is fetched,
    OBJECTID+ is enabled and a saved binding of [ctx.scope] is checked
    against [ctx.mailbox] and pinned, and a mismatch returns
    [Invalid_scope]. A selected UIDVALIDITY that differs from the published
    one returns [Uidvalidity_changed], and a UID gone after its body
    returns [Client (Missing_uid _)]. A UID removed from the snapshot by a
    concurrent publication raises [Invalid_argument] from the attach and
    leaves an orphan blob for the collector. *)

type hydration_receipt = {
  cursor : Imap.Mirror.cursor;
  hydrated : int;
  bytes : int64;
  last_uid : Imap.Uid.t option;
      (** [last_uid] is the last UID the pass hydrated or skipped. Pass it
          as [after_uid] to continue. *)
  skipped : Imap.Uid.t list;
      (** [skipped] lists, in order, the UIDs larger than the per-body or
          total byte budget, which no pass with these budgets can hydrate. *)
  more : bool;
}

type cache_audit_receipt = {
  cursor : Imap.Mirror.cursor;
  checked : int;
  invalidated : int;
  bytes : int64;
  last_uid : Imap.Uid.t option;
  skipped : Imap.Uid.t list;
      (** [skipped] lists, in order, the UIDs whose blobs are larger than
          [max_total_bytes] and were passed over unchecked. *)
  more : bool;
}

val audit_cache_once :
  ?after_uid:Imap.Uid.t -> ?expected_revision:int64 ->
  ?max_messages:int ->
  ?max_total_bytes:int64 -> store:Imap_store.t ->
  scope:Imap.Mirror.scope -> unit ->
  (cache_audit_receipt, Error.t) result
(** [audit_cache_once ~store ~scope ()] is an offline bounded integrity pass
    over the blob references of the published snapshot after [after_uid].
    It rehashes up to [max_messages] references and [max_total_bytes]
    declared bytes, then returns [last_uid] for the next pass and [more] if
    references remain. Missing or corrupt files have only
    their matching cache references detached; message inventory and files
    are unchanged. The caller can then run [hydrate_once] to refill them.
    [max_messages] defaults to 100 and must be 1 to 10,000, and
    [max_total_bytes] defaults to 1 GiB. A blob larger than [max_total_bytes]
    is skipped and listed in [skipped]. A blob larger than the remaining
    budget stops the pass before it with [more=true].
    [expected_revision] pins a continued audit to its first page. A changed
    cursor revision or epoch yields [Store_stale_revision] before any check,
    and ends the pass with the committed counts and [more=true] after
    one. *)

val hydrate_once :
  ?after_uid:Imap.Uid.t ->
  ?max_messages:int -> ?max_body_bytes:int64 -> ?max_total_bytes:int64 ->
  ctx:Ctx.t -> unit -> (hydration_receipt, Error.t) result
(** [hydrate_once ~ctx ()] fetches and durably attaches the exact
    BODY.PEEK[] bytes of the published UIDs after [after_uid] that lack blob
    references. Queries and transfers are paged, and each body goes through
    a spool file in [ctx.spool_dir] named with [ctx.next_id], which must be
    a directory. [max_messages] defaults to 100 and must be 1 to 10,000, and
    both byte budgets default to 1 GiB. One RFC822.SIZE FETCH preflights
    each page of up to 100 candidates before any of their body bytes is
    fetched. A message larger than [max_body_bytes] or [max_total_bytes] is
    skipped, listed in [skipped], and counted against [max_messages]. A
    message larger than the remaining total budget stops the pass before
    it. [more] is [true] when missing UIDs remain after [last_uid] or any
    UID was skipped. Missing UIDs, a changed epoch, failed FETCH completion
    and SQLite errors stop the pass with an error, and earlier attached
    blobs remain durable. A concurrent publication yields
    [Store_stale_revision] before any attach, and ends the pass with the
    committed counts and [more=true] after one. No message flags are
    changed. *)

type uid_digest = {
  sha256 : string;
  length : int64;
  flags : Mail_flag.Imap_flag.t list;
      (** [flags] are the durable flags read after the body. *)
  internal_date : Imap.Internal_date.t;
      (** [internal_date] is the INTERNALDATE read after the body. *)
}


val fetch_uid_digest :
  ?max_bytes:int64 -> ctx:Ctx.t -> uidvalidity:Imap.Uidvalidity.t ->
  uid:Imap.Uid.t -> spool:_ Eio.Path.t -> unit ->
  (uid_digest, Error.t) result
(** [fetch_uid_digest ~ctx ~uidvalidity ~uid ~spool ()] is the length and
    SHA-256 digest of the exact BODY.PEEK[] bytes of [uid], fetched into
    [spool], with the flags and INTERNALDATE read after them. No blob file or
    snapshot reference is created, so it serves an APPENDUID that has not
    entered the published snapshot yet. A selected UIDVALIDITY other than
    [uidvalidity] returns [Uidvalidity_changed]. [max_bytes] defaults to 1 GiB.
    The spool and OBJECTID+ rules of {!archive_uid} apply. *)
