(** Mailbox scans, body transfers and journaled APPEND for one IMAP mailbox.

    {!scan_once} publishes a complete UID inventory through a disk-backed
    stage in {!Imap_store}. With a published CONDSTORE anchor in the same
    epoch it fetches only the metadata changed since that anchor with UID
    FETCH CHANGEDSINCE, plus the UIDs above the published frontier, and
    still proves membership with a complete UID SEARCH. It never uses
    QRESYNC. FETCH and SEARCH are separate commands, so a scan is not a
    snapshot of one instant, and a later scan catches concurrent edits. *)

type error =
  | Client of Imap_eio.Error.t
  | Mirror of Imap.Mirror.error
  | Invalid_scope of string
  | Incomplete of string
  | Limit of string
  | Stale_revision
  | Uidvalidity_changed
      (** [Uidvalidity_changed] is returned when the selected mailbox's
          UIDVALIDITY differs from the epoch a call must preserve. *)

val pp_error : Format.formatter -> error -> unit

val validate_scope :
  client:Imap_eio.Client.t -> scope:Imap.Mirror.scope -> mailbox:string ->
  (unit, error) result
(** [validate_scope ~client ~scope ~mailbox] is [Ok ()] when [mailbox]
    encodes to [scope.raw_name] under [client]'s mailbox name encoding, and
    [Invalid_scope] otherwise. *)

val guard_bound_mailbox :
  client:Imap_eio.Client.t -> store:Imap_store.t ->
  scope:Imap.Mirror.scope -> mailbox:string -> (unit, error) result
(** For standalone repair paths, re-enable and verify a saved OBJECTID+
    account/mailbox binding against the configured name, then pin the client
    so later selections and APPENDs retain that identity. A scope without a
    binding needs no action. A missing capability or changed name fails
    before any repair mutation. *)

val scan_once :
  ?max_windows:int -> ?expected_uidvalidity:Imap.Uidvalidity.t ->
  client:Imap_eio.Client.t -> store:Imap_store.t ->
  scope:Imap.Mirror.scope -> mailbox:string -> stage_id:string -> unit ->
  (Imap_store.staged_receipt, error) result
(** [scan_once ~client ~store ~scope ~mailbox ~stage_id ()] selects
    [mailbox] read-only, stages its FETCH and SEARCH windows of 1,000 UIDs
    in SQLite under [stage_id], and publishes the complete membership in
    one cursor compare-and-swap transaction. The receipt carries the new
    cursor and row count. No OCaml snapshot of the mailbox is built.

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
  client:Imap_eio.Client.t -> store:Imap_store.t ->
  scope:Imap.Mirror.scope -> mailbox:string -> id:string ->
  message_id:string -> content_digest:string -> spool_ref:string ->
  ?flags:Mail_flag.Imap_flag.t list -> ?internal_date:Imap.Internal_date.t ->
  length:int64 -> _ Eio.Flow.source ->
  (append_outcome, error) result
(** Commits [Prepared] and [Sent] before sending any APPEND byte. A tagged OK
    without APPENDUID, disconnect, or cancellation leaves a pending journal
    entry for reconciliation; it is never automatically replayed. The caller
    must provide a durable spool reference and verified content digest.
    An optional validated [internal_date] is saved in the intent before send,
    as are [flags], which default to none. A saved OBJECTID+ binding
    requires OBJECTID+ to be enabled on [client] already, as
    {!guard_bound_mailbox} does, and a destination whose STATUS
    identity differs from it returns [Invalid_scope] before any intent is
    saved. *)

val append_blob_journaled :
  client:Imap_eio.Client.t -> store:Imap_store.t ->
  scope:Imap.Mirror.scope -> mailbox:string -> id:string ->
  message_id:string -> ?flags:Mail_flag.Imap_flag.t list ->
  ?internal_date:Imap.Internal_date.t -> Imap_store.Blob.blob ->
  (append_outcome, error) result
(** Verify a content-addressed local blob before sending it through
    [append_journaled]. The blob remains available for recovery if the server
    outcome is ambiguous. *)

val archive_uid :
  ?max_bytes:int64 -> client:Imap_eio.Client.t -> store:Imap_store.t ->
  scope:Imap.Mirror.scope -> mailbox:string -> uid:Imap.Uid.t ->
  spool:_ Eio.Path.t -> unit ->
  (Imap_store.Blob.blob, error) result
(** Fetches exact BODY.PEEK[] bytes to an exclusive provisional spool. Only
    after a successful tagged completion does it put a synced content-addressed
    blob and attach it to the current SQLite snapshot. [spool] must not exist.
    The call creates it and removes it on every exit, and an existing file at
    [spool] raises [Eio.Io] and is left in place. Callers supply a unique path
    on a filesystem with free space. A saved OBJECTID+ binding is checked
    against the configured name and used for selection before any body is
    fetched. A selected UIDVALIDITY that differs from the published one
    returns [Uidvalidity_changed]. A UID removed from the snapshot by a
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
  (cache_audit_receipt, error) result
(** Offline bounded integrity pass over blob references in the published
    snapshot after [after_uid]. Rehashes up to [max_messages] references and
    [max_total_bytes] declared bytes, then returns [last_uid] for the next
    pass and [more] if references remain. Missing or corrupt files have only
    their matching cache references detached; message inventory and files
    are unchanged. The caller can then run [hydrate_once] to refill them.
    [max_messages] defaults to 100 and must be 1 to 10,000, and
    [max_total_bytes] defaults to 1 GiB. A blob larger than [max_total_bytes]
    is skipped and listed in [skipped]. A blob larger than the remaining
    budget stops the pass before it with [more=true].
    [expected_revision] pins a continued audit to its first page. A changed
    cursor revision or epoch yields [Stale_revision] before any check, and
    ends the pass with the committed counts and [more=true] after one. *)

val hydrate_once :
  ?after_uid:Imap.Uid.t ->
  ?max_messages:int -> ?max_body_bytes:int64 -> ?max_total_bytes:int64 ->
  client:Imap_eio.Client.t -> store:Imap_store.t ->
  scope:Imap.Mirror.scope -> mailbox:string -> spool_dir:_ Eio.Path.t ->
  next_spool_id:(unit -> string) -> unit ->
  (hydration_receipt, error) result
(** Fetch and durably attach exact BODY.PEEK[] bytes for published UIDs after
    [after_uid] that lack blob references. Queries and transfers are paged.
    [max_messages] defaults to 100 and must be 1 to 10,000, and both byte
    budgets default to 1 GiB. One RFC822.SIZE FETCH preflights each page of up
    to 100 candidates before any of their body bytes is fetched. A message
    larger than [max_body_bytes] or [max_total_bytes] is skipped, listed in
    [skipped], and counted against [max_messages]. A message larger than the
    remaining total budget stops the pass before it. [more] is [true] when
    missing UIDs remain after [last_uid] or any UID was skipped. Missing UIDs, a
    changed epoch, failed FETCH completion and SQLite errors stop the pass with
    an error, and earlier attached blobs remain durable. A concurrent
    publication yields [Stale_revision] before any attach, and ends the pass
    with the committed counts and [more=true] after one. The caller supplies
    unique filesystem-safe spool IDs. No message flags are changed. *)

type uid_digest = { sha256:string; length:int64 }

val fetch_uid_digest :
  ?max_bytes:int64 -> client:Imap_eio.Client.t -> store:Imap_store.t ->
  scope:Imap.Mirror.scope -> mailbox:string ->
  uidvalidity:Imap.Uidvalidity.t -> uid:Imap.Uid.t ->
  spool:_ Eio.Path.t -> unit -> (uid_digest, error) result
(** [fetch_uid_digest ~client ~store ~scope ~mailbox ~uidvalidity ~uid ~spool
    ()] is the length and SHA-256 digest of the exact BODY.PEEK[] bytes of
    [uid], fetched into a provisional spool. No blob file or snapshot
    reference is created. Use it for an APPENDUID that has not entered the
    published snapshot yet. A selected UIDVALIDITY other than [uidvalidity]
    returns [Uidvalidity_changed]. [max_bytes] defaults to 1 GiB. The spool,
    scope, OBJECTID+ and literal-completion rules of [archive_uid] apply. *)
