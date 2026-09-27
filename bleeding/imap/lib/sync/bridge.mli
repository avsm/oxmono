(** Sync cycles between one IMAP mailbox and one Maildir.

    A cycle publishes a complete remote inventory, stages a complete Maildir
    inventory, settles the operations earlier cycles left pending, and then
    copies unpaired messages in both directions, reconciles the flags of
    each pair and applies the deletion policy. It never pairs two messages
    because their bytes match. A paired side is absent only when a complete
    inventory of that side omits it, and the cycle records the absence as a
    durable tombstone before a deletion policy may act on it. Journaling,
    uncertain outcomes and holds follow the contract of {!Error}.

    A call that takes a {!Maildir.t} takes the Maildir writer lease itself
    for its whole run, so its caller must not hold the lease, and a busy
    lease returns [Writer_busy]. A call that takes a
    {!Maildir.writer} runs under the lease its caller holds. Every other
    writer of the Maildir must take the same lease. {!Plan} previews a cycle
    offline, and {!Repair} settles what a cycle holds. *)

type receipt = {
  cursor : Imap.Mirror.cursor;
      (** [cursor] is the cursor the cycle published and planned from. *)
  remote_to_local : int;
      (** [remote_to_local] counts the IMAP messages copied to Maildir. *)
  local_to_remote : int;
      (** [local_to_remote] counts the Maildir messages appended to
          IMAP. *)
  flags_updated : int;
      (** [flags_updated] counts the pairs whose flags or common baseline
          changed. *)
  deletions : int;
      (** [deletions] counts the targeted deletions committed. *)
  flags_held : int;
      (** [flags_held] counts the flag, date, content and tombstone holds. *)
  deletions_held : int;
      (** [deletions_held] counts the deletion holds. *)
  held_pair_ids : string list;
      (** [held_pair_ids] lists at most 100 held pairs, in the order they
          were first held. *)
  more : bool;
      (** [more] is [true] when the transfer budget ran out. *)
}
(** The type for the result of one cycle. A hold means the policy has not
    converged, even when [more] is [false]. *)

val copy_once :
  ?max_transfers:int -> ?min_absence_scans:int ->
  ?allow_bootstrap_duplicates:bool ->
  ?deletion_policy:Imap.Sync_policy.deletion_policy ->
  ctx:Ctx.t -> maildir:Maildir.t -> stage_id:string -> unit ->
  (receipt, Error.t) result
(** [copy_once ~ctx ~maildir ~stage_id ()] runs one cycle between
    [ctx.mailbox] and [maildir] under the Maildir writer lease. It publishes
    the remote inventory with {!Engine.scan_once} under [stage_id].
    [max_transfers] defaults to 100 and bounds the copies, flag updates and
    deletions together. A [max_transfers] below 1, a negative
    [min_absence_scans] or a [ctx.spool_dir] that is not a directory returns
    [Invalid_configuration] before the scan. When the journal holds pairs or
    operations, a selected UIDVALIDITY other than the published one returns
    [Uidvalidity_changed], and so does an untombstoned pair of another
    epoch.

    A remote-to-Maildir copy archives the exact bytes with their flags and
    INTERNALDATE, reserves a Maildir ID and journals it before the write. A
    Maildir-to-remote copy archives the exact bytes and journals them before
    APPEND. A pair and its operation commit together only after the written
    occurrence verifies its bytes and flags, or the APPENDUID target its
    bytes, flags and INTERNALDATE. A remote message expunged before its body
    is archived returns [Source_vanished], and a staged local occurrence
    that changed returns [Local_source_changed] before any APPEND intent. A
    remote message that Maildir cannot store, such as one with an
    unrepresentable date, is rejected in the journal and returns
    [Invalid_operation].

    Before any new transfer, the cycle reconciles every pending operation
    it has evidence for, and returns [Pending_operations] with up to 256 of
    those that remain. A remote-to-Maildir write is reconciled from its
    reserved local ID, the source UID in the new inventory and its journaled
    digest, length, flags and INTERNALDATE. An APPEND with a confirmed or
    attested APPENDUID is reconciled after the UID's membership, bytes,
    length, flags and INTERNALDATE verify. An APPEND whose {!Imap_store}
    intent was never sent is rejected and may be attempted afresh. An APPEND
    without attribution stays pending and is never replayed.

    With no pairs yet and messages on both endpoints, the call returns
    [Bootstrap_requires_pairing] unless [allow_bootstrap_duplicates] is
    [true]. It defaults to [false].

    Paired flags reconcile with {!Flags.reconcile_pair}, whose remote writes
    use conditional UID STORE and so require CONDSTORE. A changed
    [\\Deleted] is always held while the other flags merge. A pair is held
    rather than failing the cycle when its local date or content differs
    from the pair, a remote write lacks CONDSTORE, a MODSEQ or a permanent
    flag, an endpoint changed concurrently, or it is tombstoned while both
    endpoints are present.

    [deletion_policy] defaults to [Preserve]. [Propagate] and its
    directional forms permit a targeted deletion with
    {!Deletion.reconcile_pair}, which holds it when the server or the
    configuration cannot perform it. [min_absence_scans] defaults to 0 and
    is the number of further complete scan generations that must follow
    the first durable absence. A positive value also holds a tombstone that
    recorded no generation.

    A Maildir format or policy failure returns [Maildir].
    {!Maildir.Metadata_lock_busy}, the other Maildir concurrency exceptions,
    store exceptions and Eio cancellation propagate. *)

val recover_local :
  maildir:Maildir.t -> spool_dir:_ Eio.Path.t -> unit ->
  (unit, Error.t) result
(** [recover_local ~maildir ~spool_dir ()] removes the temporary files an
    interrupted process left in [maildir] and the inventory staging files it
    left in [spool_dir]. It takes the Maildir writer lease. Call it at
    startup before the first cycle. *)

type local_verification = {
  checked : int64;
      (** [checked] counts the paired occurrences rehashed. *)
  mismatched : int64;
      (** [mismatched] counts the occurrences whose bytes differ or changed
          while read. *)
  restored : int64;
      (** [restored] counts the content conflicts resolved by matching
          bytes. *)
  missing : int64;
      (** [missing] counts the paired occurrences absent from the
          inventory. *)
  unverified : int64;
      (** [unverified] counts the pairs without a saved digest and
          length. *)
}
(** The type for the result of a local content check. *)

val verify_local_content :
  store:Imap_store.t -> maildir:Maildir.t ->
  scope:Imap.Mirror.scope -> next_id:(unit -> string) ->
  spool_dir:_ Eio.Path.t -> on_issue:(string -> string -> unit) -> unit ->
  (local_verification, Error.t) result
(** [verify_local_content ~store ~maildir ~scope ~next_id ~spool_dir
    ~on_issue ()] rehashes the Maildir occurrence of every pair of [scope]
    in [store] without a local tombstone against its saved digest and
    length, without connecting to IMAP. It takes the Maildir writer lease and pages a
    complete Maildir inventory staged in [spool_dir] and the pairs. A
    mismatch opens or keeps a durable content conflict with an ID from
    [next_id], and matching bytes resolve an open one. A missing occurrence
    and a pair without content evidence are counted and change nothing.
    [on_issue pair_id reason] is called for each mismatch, absence and
    unverified pair. No pair revision changes. A [spool_dir] that is not a
    directory returns [Invalid_configuration]. *)
