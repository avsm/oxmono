(** Mailbox scans, body transfers and journaled APPEND for one IMAP mailbox.

    {!scan_once} publishes a complete UID inventory of the context's mailbox
    through a disk-backed stage in {!Imap_store}. {!archive_uid},
    {!hydrate_once} and {!fetch_uid_digest} read the exact [BODY.PEEK[]]
    bytes of the messages in that inventory, and {!append_journaled} sends
    one journaled APPEND. Every call but the offline {!audit_cache_once}
    takes a {!Ctx.t}, and failures follow the contract of {!Error}. *)

(** {1 Scans} *)

val scan_once :
  ?max_windows:int -> ?expected_uidvalidity:Imap.Uidvalidity.t ->
  ctx:Ctx.t -> stage_id:string -> unit ->
  (Imap_store.staged_receipt, Error.t) result
(** [scan_once ~ctx ~stage_id ()] selects [ctx.mailbox] read-only, stages
    its metadata in SQLite under [stage_id] in FETCH and UID SEARCH windows
    of 1,000 UIDs, and publishes the complete membership in one cursor
    compare-and-swap transaction. The receipt carries the new cursor and row
    count. It builds no OCaml snapshot of the mailbox.

    With a CONDSTORE anchor from a complete scan of the same epoch, the
    stage is seeded from the published rows, UID FETCH CHANGEDSINCE reads
    only the rows changed since the anchor below the published frontier,
    and every UID above it is fetched in full. Every other case fetches
    every window in full. UID SEARCH proves membership in every case, and
    QRESYNC is never used. FETCH and SEARCH are separate commands, so a
    scan is not a snapshot of one instant, and a later scan catches a
    concurrent edit. The anchor advances only with a complete publication.

    [max_windows] defaults to 100,000 and bounds the number of windows. A
    value below 1 returns [Invalid_configuration], and a larger UID range
    [Limit]. When
    [expected_uidvalidity] is given, a selected UIDVALIDITY that differs
    from it returns [Uidvalidity_changed] before any row is staged.

    When the server offers OBJECTID+ and ENABLE, the scan enables it and
    checks a saved binding of [ctx.scope] against [ctx.mailbox] first. With
    no saved binding it binds the selected mailbox identity only when the
    selected UIDVALIDITY equals the published one, so a scan that publishes
    a new epoch leaves the scope unbound and the next scan binds it. A
    binding that cannot be verified or differs returns [Invalid_scope].

    An error or an exception discards the stage. A crash leaves an inert
    stage that
    {!Imap_store.abandoned_stages} lists. A publication that lost the
    compare-and-swap returns [Store_stale_revision].

    @raise Sqlite3.SqliteError if [stage_id] names an existing stage. *)

(** {1 APPEND} *)

type append_outcome =
  | Identified of Imap_eio.Client.append_receipt
      (** [Identified r] is an APPEND whose APPENDUID [r] confirmed the
          intent. *)
  | Needs_reconciliation
      (** [Needs_reconciliation] is a tagged OK without APPENDUID, which
          leaves the intent ambiguous. *)
(** The type for the outcomes of a journaled APPEND. *)

val append_journaled :
  ctx:Ctx.t -> id:string -> message_id:string -> content_digest:string ->
  spool_ref:string -> ?flags:Mail_flag.Imap_flag.t list ->
  ?internal_date:Imap.Internal_date.t -> length:int64 ->
  _ Eio.Flow.source -> (append_outcome, Error.t) result
(** [append_journaled ~ctx ~id ~message_id ~content_digest ~spool_ref
    ~length source] appends the [length] bytes of [source] to
    [ctx.mailbox]. It first commits the intent [id] as [Prepared] with
    [message_id], the verified [content_digest], the durable [spool_ref],
    [length], [flags], [internal_date] and the published UID frontier, and
    then marks it [Sent]. [flags] defaults to none and [internal_date] to
    the server's choice.

    A tagged OK without APPENDUID is [Needs_reconciliation]. A rejection
    marks the intent rejected and returns [Client]. Any other failure marks
    it ambiguous and returns [Client], and a cancellation leaves it sent.
    An intent that is not confirmed is never replayed.

    A saved OBJECTID+ binding of [ctx.scope] requires OBJECTID+ already
    enabled on [ctx.client], as {!scan_once} leaves it. A missing mode, or a
    destination whose STATUS identity differs from the binding, returns
    [Invalid_scope] before any intent is saved.

    @raise Invalid_argument if the intent metadata is malformed.
    @raise Sqlite3.SqliteError if [id] names an existing intent. *)

val append_blob_journaled :
  ctx:Ctx.t -> id:string -> message_id:string ->
  ?flags:Mail_flag.Imap_flag.t list -> ?internal_date:Imap.Internal_date.t ->
  Imap_store.Blob.blob -> (append_outcome, Error.t) result
(** [append_blob_journaled ~ctx ~id ~message_id blob] rehashes [blob] and
    sends it with {!append_journaled}, using its digest as the content
    digest and the spool reference. [flags] and [internal_date] are passed
    on. A blob that fails verification returns [Incomplete] before any
    intent is saved. The blob stays available for reconciliation when the
    outcome is ambiguous. *)

(** {1 Bodies} *)

type archived = {
  blob : Imap_store.Blob.blob;
      (** [blob] is the synced content-addressed body. *)
  flags : Mail_flag.Imap_flag.t list;
      (** [flags] are the durable flags read after the body. *)
  internal_date : Imap.Internal_date.t;
      (** [internal_date] is the INTERNALDATE read after the body. *)
}
(** The type for an archived message. *)

val archive_uid :
  ?max_bytes:int64 -> ctx:Ctx.t -> uid:Imap.Uid.t -> spool:_ Eio.Path.t ->
  unit -> (archived, Error.t) result
(** [archive_uid ~ctx ~uid ~spool ()] fetches the exact [BODY.PEEK[]]
    bytes of [uid] into the file [spool], reads its flags and INTERNALDATE
    in the same selection, and then puts a synced blob and attaches it to
    the published snapshot. [max_bytes] defaults to 1 GiB. [spool] must not
    exist. The call creates it and removes it on every exit.

    When [ctx.scope] has a saved OBJECTID+ binding, OBJECTID+ is enabled and
    the binding is checked against [ctx.mailbox] and pinned before any body
    is fetched, and a mismatch returns [Invalid_scope]. A scope without a
    published epoch returns [Incomplete], a selected UIDVALIDITY other than
    the published one [Uidvalidity_changed], and a missing UID
    [Client (Missing_uid _)].

    @raise Eio.Io if a file already exists at [spool]. It is left in place.
    @raise Invalid_argument if a concurrent publication removed [uid] from
    the snapshot. The blob is left for the orphan collector. *)

type hydration_receipt = {
  cursor : Imap.Mirror.cursor;
      (** [cursor] is the published cursor the pass read. *)
  hydrated : int;  (** [hydrated] counts the bodies attached. *)
  bytes : int64;  (** [bytes] counts the body bytes attached. *)
  last_uid : Imap.Uid.t option;
      (** [last_uid] is the last UID the pass hydrated or skipped. Pass it
          as [after_uid] to continue. *)
  skipped : Imap.Uid.t list;
      (** [skipped] lists, in order, the UIDs larger than the per-body or
          total byte budget, which no pass with these budgets can hydrate. *)
  more : bool;
      (** [more] is [true] when missing bodies remain after [last_uid] or
          [skipped] is not empty. *)
}
(** The type for the result of one hydration pass. *)

val hydrate_once :
  ?after_uid:Imap.Uid.t ->
  ?max_messages:int -> ?max_body_bytes:int64 -> ?max_total_bytes:int64 ->
  ctx:Ctx.t -> unit -> (hydration_receipt, Error.t) result
(** [hydrate_once ~ctx ()] fetches and durably attaches the exact
    [BODY.PEEK[]] bytes of the published UIDs after [after_uid] that have
    no blob. [after_uid] defaults to the start of the snapshot. Each body
    passes through a spool file in [ctx.spool_dir] named with
    [ctx.next_id]. No message flags change.

    [max_messages] defaults to 100 and bounds the UIDs considered.
    [max_body_bytes] and [max_total_bytes] default to 1 GiB and bound one
    body and the pass. One RFC822.SIZE FETCH preflights each page of up to
    100 UIDs before any of their bodies. A body larger than either budget
    is skipped, listed in [skipped] and counted against [max_messages]. A
    body larger than the remaining total budget ends the pass before it
    with [more] set.

    A [max_messages] outside 1 to 10,000, a budget below 1, a [ctx.spool_dir]
    that is not a directory or an unusable [ctx.next_id] returns
    [Invalid_configuration]. The
    OBJECTID+ rule of {!archive_uid} applies. A scope without a complete
    published inventory, or a published UID the server no longer reports,
    returns [Incomplete]. A selected UIDVALIDITY other than the published
    one returns [Uidvalidity_changed]. A publication that replaces the
    snapshot returns [Store_stale_revision] before the first attach, and
    ends the pass with the committed counts and [more] set after it. A
    failure stops the pass, and the bodies attached before it stay
    durable. *)

type cache_audit_receipt = {
  cursor : Imap.Mirror.cursor;
      (** [cursor] is the published cursor the pass read. *)
  checked : int;  (** [checked] counts the blobs rehashed. *)
  invalidated : int;
      (** [invalidated] counts the references detached as missing or
          corrupt. *)
  bytes : int64;  (** [bytes] counts the declared bytes rehashed. *)
  last_uid : Imap.Uid.t option;
      (** [last_uid] is the last UID the pass checked or skipped. Pass it
          as [after_uid] to continue. *)
  skipped : Imap.Uid.t list;
      (** [skipped] lists, in order, the UIDs whose blobs are larger than
          [max_total_bytes] and were passed over unchecked. *)
  more : bool;  (** [more] is [true] when references remain. *)
}
(** The type for the result of one cache audit pass. *)

val audit_cache_once :
  ?after_uid:Imap.Uid.t -> ?expected_revision:int64 ->
  ?max_messages:int ->
  ?max_total_bytes:int64 -> store:Imap_store.t ->
  scope:Imap.Mirror.scope -> unit ->
  (cache_audit_receipt, Error.t) result
(** [audit_cache_once ~store ~scope ()] rehashes the blobs referenced by
    the published snapshot of [scope] in [store] after [after_uid], without
    connecting to IMAP. [after_uid] defaults to the start of the snapshot. A
    missing or corrupt file has only its matching reference detached, and
    the inventory and the files are unchanged, so {!hydrate_once} can
    refill it.

    [max_messages] defaults to 100 and bounds the references considered.
    [max_total_bytes] defaults to 1 GiB and bounds the declared bytes
    rehashed. A blob larger than [max_total_bytes] is skipped and listed in
    [skipped]. A blob larger than the remaining budget ends the pass before
    it with [more] set. A [max_messages] outside 1 to 10,000 or a budget
    below 1 returns [Invalid_configuration], and a scope without a complete
    published inventory returns [Incomplete].

    [expected_revision] pins a continued audit to the revision of its first
    pass, and a different revision returns [Store_stale_revision] before any
    check. A publication during the pass returns [Store_stale_revision]
    before the first check, and ends the pass with the committed counts and
    [more] set after it. *)

type uid_digest = {
  sha256 : string;  (** [sha256] is the lowercase SHA-256 of the body. *)
  length : int64;  (** [length] is the body length in bytes. *)
  flags : Mail_flag.Imap_flag.t list;
      (** [flags] are the durable flags read after the body. *)
  internal_date : Imap.Internal_date.t;
      (** [internal_date] is the INTERNALDATE read after the body. *)
}
(** The type for the digest of a remote message. *)

val fetch_uid_digest :
  ?max_bytes:int64 -> ctx:Ctx.t -> uidvalidity:Imap.Uidvalidity.t ->
  uid:Imap.Uid.t -> spool:_ Eio.Path.t -> unit ->
  (uid_digest, Error.t) result
(** [fetch_uid_digest ~ctx ~uidvalidity ~uid ~spool ()] is the length and
    SHA-256 digest of the exact [BODY.PEEK[]] bytes of [uid], fetched into
    [spool], with the flags and INTERNALDATE read after them. It creates no
    blob and no snapshot reference, so it serves an APPENDUID that the
    published snapshot does not hold yet. [max_bytes] defaults to 1 GiB. A
    selected UIDVALIDITY other than [uidvalidity] returns
    [Uidvalidity_changed]. The [spool] and OBJECTID+ rules of {!archive_uid}
    apply. *)
