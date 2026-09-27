(** IMAP↔Maildir occurrence transfer with paired flag and deletion sync.

    A cycle publishes a complete remote inventory and then makes durable,
    individually journaled copies in both directions. A copied message
    keeps its INTERNALDATE as the Maildir file mtime. The driver never
    infers identity from matching bytes or replays an uncertain server
    mutation. It records complete-inventory absence and applies three-way
    flag and opt-in deletion policies with journaled writes. {!Plan}
    previews a cycle offline, and {!Repair} settles what a cycle holds. *)

type receipt = {
  cursor : Imap.Mirror.cursor;
  remote_to_local : int;
  local_to_remote : int;
  flags_updated : int;
  deletions : int;
  flags_held : int;
  deletions_held : int;
  held_pair_ids : string list;
  more : bool;
}

val copy_once :
  ?max_transfers:int -> ?min_absence_scans:int ->
  ?allow_bootstrap_duplicates:bool ->
  ?deletion_policy:Imap.Sync_policy.deletion_policy ->
  ctx:Ctx.t -> maildir:Maildir.t -> stage_id:string -> unit ->
  (receipt, Error.t) result
(** [copy_once ~ctx ~maildir ~stage_id ()] publishes a complete remote
    inventory of [ctx.mailbox] with {!Engine.scan_once} under [stage_id],
    then copies unpaired occurrences, reconciles paired flags and applies
    the deletion policy, up to [max_transfers] (default 100) copies, flag
    updates and deletions together.

    A remote-to-Maildir copy archives the exact bytes with their flags and
    INTERNALDATE, reserves a Maildir ID and journals it before the write. A
    Maildir-to-remote copy archives the exact bytes and journals them before
    APPEND, and a staged local occurrence that changed returns
    [Local_source_changed] before any APPEND intent. A pair and its
    operation commit together only after the written occurrence verifies its
    bytes and flags, or the APPENDUID target its bytes, flags and
    INTERNALDATE. A remote
    message that Maildir cannot store, such as one with an unrepresentable
    date, is rejected in the journal and returns [Invalid_operation].

    A crash or an ambiguous result leaves the operation pending, and the
    next call reconciles it before any new transfer or returns
    [Pending_operations]. A completed remote-to-Maildir write is reconciled
    from its reserved local ID, the source UID in the new inventory and its
    journaled digest, length, flags and INTERNALDATE. An APPEND whose legacy
    intent has a confirmed APPENDUID is reconciled after the UID's
    membership, bytes, length and flags verify. An APPEND marked sent before
    its lower-layer intent was prepared is proven unsent, rejected and may be
    attempted afresh. An APPEND without attribution stays pending and is
    never replayed. With no prior pairs and both endpoints populated, the
    call returns [Bootstrap_requires_pairing] unless
    [allow_bootstrap_duplicates] is [true], which defaults to [false].

    Paired flags reconcile with a journaled three-way merge, and remote
    writes use conditional UID STORE, which requires CONDSTORE. A changed
    [\\Deleted] is held by default while the other flags merge. A pair is
    held rather than failing the cycle when its local date or content
    differs from the pair, a remote write lacks CONDSTORE, a MODSEQ or a
    permanent flag, an endpoint changed concurrently, or it is tombstoned
    while both endpoints are present. [flags_held] and [deletions_held]
    count the holds, [held_pair_ids] lists at most 100 of the pairs, and a
    hold means the policy has not converged even when [more] is [false].

    [deletion_policy] defaults to [Preserve]. [Propagate] and its
    directional forms permit a journaled targeted deletion only after a
    complete inventory proves the absence and the survivor's bytes and
    flags verify, as {!Deletion.reconcile_pair} does. [min_absence_scans]
    (default 0) further complete scan generations must follow the first
    durable absence, and a positive value also holds a legacy local
    tombstone without a generation. A negative value returns
    [Invalid_configuration].

    The whole cycle holds the Maildir writer lease, and failing to acquire
    it returns [Writer_busy]. Every direct Maildir writer must honour the
    same lease. A Maildir format or policy failure returns [Maildir].
    [Maildir.Metadata_lock_busy], other Maildir concurrency exceptions,
    store exceptions and Eio cancellation propagate. *)

val recover_local :
  maildir:Maildir.t -> spool_dir:_ Eio.Path.t -> unit -> (unit, Error.t) result
(** [recover_local ~maildir ~spool_dir ()] removes the temporary files an
    interrupted process left in [maildir] and the inventory staging files it
    left in [spool_dir]. It holds the Maildir writer lease, and failing to
    acquire it yields [Writer_busy]. Call it at startup before the first
    cycle. *)

type local_verification = {
  checked : int64;
  mismatched : int64;
  restored : int64;
  missing : int64;
  unverified : int64;
}

val verify_local_content :
  store:Imap_store.t -> maildir:Maildir.t ->
  scope:Imap.Mirror.scope -> next_id:(unit -> string) ->
  spool_dir:_ Eio.Path.t -> on_issue:(string -> string -> unit) -> unit ->
  (local_verification, Error.t) result
(** Hash established local pairs with saved content evidence under the
    Maildir writer lease. The complete local inventory and pair table are
    paged. Pairs with a local tombstone are skipped. Mismatches create or
    refresh durable [Content_conflict] rows, and verified restorations
    resolve them. Missing occurrences and legacy pairs without content
    evidence are reported separately. No IMAP connection, remote mutation, or
    pair revision change occurs. [on_issue] receives a pair ID and reason for
    each mismatch, absence, or unverified pair. The local inventory is
    staged in [spool_dir], which must be a directory. *)
