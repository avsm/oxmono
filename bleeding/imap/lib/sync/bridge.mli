(** Conservative IMAP↔Maildir occurrence transfer and paired flag sync.

    This driver makes durable, individually journaled copies in both
    directions. Remote imports keep the server's INTERNALDATE in a durable
    Maildir sidecar; dated local occurrences supply it to APPEND. Undated
    local files use their filesystem mtime as a UTC upload date. It never
    infers identity from matching bytes or replays an
    uncertain server mutation. It records complete-inventory absence and
    applies three-way flag and opt-in deletion policies with journaled writes. *)

type error =
  | Sync of Engine.error
  | Flag_sync of Flags.error
  | Delete_sync of Deletion.error
  | Writer_busy
  | Client of Imap_eio.Error.t
  | Pending_operations of string list
  | Bootstrap_requires_pairing
  | Uidvalidity_changed
  | Source_vanished of Imap.Uid.t
  | Local_source_changed of string
  | Stale_revision
  | Content_diverged of string
  | Flags_diverged of string
  | Date_diverged of string
  | Invalid_operation of string
  | Invalid_configuration of string
  | Maildir of Maildir.error
      (** [Maildir e] is a Maildir format or policy failure, including one
          from {!Flags} or {!Deletion}. An operation already sent stays
          pending. *)

val pp_error : Format.formatter -> error -> unit

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
  client:Imap_eio.Client.t -> store:Imap_store.t ->
  maildir:Maildir.t -> scope:Imap.Mirror.scope -> mailbox:string ->
  stage_id:string -> next_id:(unit -> string) -> spool_dir:_ Eio.Path.t ->
  unit -> (receipt, error) result
(** Publish a complete remote inventory, then copy unpaired occurrences up
    to [max_transfers] (default 100). A remote→local copy reserves a Maildir ID
    and journals it before the write. A local→remote copy archives exact bytes
    and journals before APPEND. A changed staged local occurrence yields
    [Local_source_changed] before preparing an APPEND intent. UIDPLUS and
    fetched remote bytes, flags and INTERNALDATE must verify before
    a pair and operation commit atomically. On a crash or ambiguous result,
    the operation stays pending and the next call refuses new transfers.
    A bridge APPEND marked sent before its lower-layer intent was prepared is
    proven unsent on restart, rejected, and may be attempted afresh. Once the
    lower intent is sent, missing attribution remains pending and is never
    replayed automatically.
    With no prior pairs, both endpoints populated requires explicit
    [allow_bootstrap_duplicates] to avoid accidental duplicate import.
    On restart a completed remote-to-Maildir write is reconciled from its
    reserved local ID, source UID in the new complete inventory, and persisted
    digest, length, flags and source INTERNALDATE; an absent or divergent write remains pending or
    errors. A remote APPEND whose legacy intent has a confirmed UIDPLUS receipt
    is reconciled after verifying current UID membership, exact body digest,
    byte length, and flags; an APPEND without an attributable receipt stays
    pending and is never replayed automatically.
    Existing pairs also reconcile flags with a journaled three-way merge.
    Remote writes require CONDSTORE and use conditional UID STORE. A changed
    [\\Deleted] is held by default while the other flags merge. A pair is
    held rather than failing the cycle when its local date differs from the
    paired date, its local content differs from the paired digest, a remote
    write lacks CONDSTORE, a MODSEQ or a permanent flag, an endpoint changed
    concurrently, or it is tombstoned while both endpoints are present. A
    remote message that Maildir cannot store, such as one with an
    unrepresentable date, is rejected in the journal and returns
    [Invalid_operation]. [deletion_policy=Propagate] permits
    journaled targeted deletion only after complete-inventory absence and
    survivor byte/flag verification; the default is [Preserve].
    [min_absence_scans] requires that many additional complete scan
    generations after the first durable absence before propagation. It
    defaults to zero; a positive value also holds legacy local tombstones
    without first-observed generation metadata.
    [max_transfers] covers copies, flag updates, and deletions. The entire
    cycle holds the cross-process Maildir writer lease, and failing to
    acquire it yields [Writer_busy]. All direct Maildir writers must honor
    the same lease. A Maildir format or policy failure returns [Maildir].
    [Maildir.Metadata_lock_busy] from a contended Dovecot lock, other Maildir
    concurrency exceptions, Store exceptions and Eio cancellation
    propagate. [flags_held] and [deletions_held] count flag and deletion
    holds, and [held_pair_ids] includes at most 100 IDs for diagnostics. A
    hold means the requested policy has not fully converged, even when
    [more=false]. *)

val recover_local :
  maildir:Maildir.t -> spool_dir:_ Eio.Path.t -> unit -> (unit, error) result
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
  (local_verification, error) result
(** Hash established local pairs with saved content evidence under the
    Maildir writer lease. The complete local inventory and pair table are
    paged. Pairs with a local tombstone are skipped. Mismatches create or
    refresh durable [Content_conflict] rows, and verified restorations
    resolve them. Missing occurrences and legacy pairs without content
    evidence are reported separately. No IMAP connection, remote mutation, or
    pair revision change occurs. [on_issue] receives a pair ID and reason for
    each mismatch, absence, or unverified pair. The local inventory is
    staged in [spool_dir], which must be a directory. *)

val mark_local_retention :
  store:Imap_store.t -> maildir:Maildir.t ->
  scope:Imap.Mirror.scope -> pair_id:string -> evidence:string ->
  spool_dir:_ Eio.Path.t -> unit -> (unit, error) result
(** Attest that a missing local paired occurrence was removed by local
    retention, not by a user deletion. Requires a complete Maildir inventory,
    the Maildir writer lease, no active operation for the pair, and an extant
    remote binding. The durable tombstone prevents later propagation of this
    local absence to the server. No IMAP mutation is sent. The local
    inventory is staged in [spool_dir], which must be a directory. *)

type deletion_preview = {
  pair_id : string;
  remote_uid : Imap.Uid.t;
  local_id : string;
  remote_present : bool option;
  local_present : bool;
  decision : [ `Pending of string | `Stale_epoch |
    `Plan of Imap.Sync_policy.deletion_plan ];
}

val preview_deletions :
  ?min_absence_scans:int ->
  store:Imap_store.t -> maildir:Maildir.t ->
  scope:Imap.Mirror.scope -> policy:Imap.Sync_policy.deletion_policy ->
  spool_dir:_ Eio.Path.t -> on_preview:(deletion_preview -> unit) ->
  unit -> (Imap.Mirror.cursor, error) result
(** Stream one-sided paired occurrences from the latest complete published
    remote inventory and a freshly staged local inventory. Holds the Maildir
    writer lease and pages pairs, so memory is bounded. This is a read-only
    candidate plan: it does not connect to IMAP, verify live survivor content
    or flags, journal operations, or authorize deletion. [copy_once] must
    revalidate everything immediately before any mutation. A pending journal
    operation is reported instead of a deletion decision. A pair from an
    earlier UIDVALIDITY is reported as [`Stale_epoch] only while its local
    occurrence is present. [min_absence_scans] defaults to 0, and a negative
    value returns [Invalid_configuration]. The local inventory is staged in
    [spool_dir], which must be a directory. *)

type sync_preview =
  | Preview_pending of string
  | Preview_bootstrap_hold
  | Preview_copy_remote of Imap.Uid.t
  | Preview_copy_local of string
  | Preview_flags of {
      pair_id : string;
      to_remote : Imap.Sync_policy.flag_delta;
      to_local : Imap.Sync_policy.flag_delta;
    }
  | Preview_pair_hold of string * string
  | Preview_deletion of deletion_preview

val preview_sync :
  ?allow_bootstrap_duplicates:bool ->
  ?min_absence_scans:int ->
  store:Imap_store.t -> maildir:Maildir.t ->
  scope:Imap.Mirror.scope -> policy:Imap.Sync_policy.deletion_policy ->
  spool_dir:_ Eio.Path.t -> on_preview:(sync_preview -> unit) ->
  unit -> (Imap.Mirror.cursor, error) result
(** Stream a candidate plan for copies, paired flag changes and one-sided
    deletion using the latest complete published remote snapshot and a fresh
    staged Maildir inventory. It holds the writer lease and pages both
    inventories and pairs, with memory bounded by the caller's output buffer.
    Pending journal work and unsafe populated bootstrap stop planning, as in
    [copy_once]. Saved content conflicts appear as pair holds and suppress
    FLAGS candidates for those pairs. The plan does not connect to IMAP or mutate durable state;
    it cannot validate current server capabilities, survivor bytes or flags,
    or concurrent changes. A later [copy_once] must refresh the inventory
    and revalidate every action. [min_absence_scans] defaults to 0, and a
    negative value returns [Invalid_configuration]. The local inventory is
    staged in [spool_dir], which must be a directory. *)

val repair_local_append :
  client:Imap_eio.Client.t -> store:Imap_store.t ->
  maildir:Maildir.t -> scope:Imap.Mirror.scope -> mailbox:string ->
  id:string -> evidence:string -> spool_dir:_ Eio.Path.t ->
  unit -> (unit, error) result
(** Explicitly finish a pending remote-to-Maildir append whose reserved
    occurrence is absent. Requires a complete published inventory containing
    the source UID in the saved UIDVALIDITY, and verifies live flags, exact
    bytes, length, and INTERNALDATE before writing. A saved OBJECTID+ binding
    must still identify the configured mailbox. The reserved occurrence is
    published under the Maildir writer lease, and its bytes and flags are
    verified before it is observed and paired. A crash
    after publication is recovered by ordinary [copy_once]; it must not be
    repaired again. [evidence] is a printable operator audit note. *)

val record_appenduid_evidence :
  store:Imap_store.t -> maildir:Maildir.t ->
  scope:Imap.Mirror.scope -> id:string ->
  uidvalidity:Imap.Uidvalidity.t -> uid:Imap.Uid.t ->
  evidence:string -> unit -> (unit, error) result
(** Record a trusted, externally recovered APPENDUID for one pending upload.
    This is an explicit operator attestation of attribution: equal message
    bytes alone cannot prove which client appended the UID. The call checks
    the operation's scope, destination epoch and saved legacy intent, then
    persists the receipt under the Maildir writer lease. It does not create
    a pair. The next [copy_once] must find the exact UID in a complete scan
    and verify its body digest, length and flags against the unchanged local
    occurrence before committing. A missing or divergent UID remains pending. *)

type append_candidates = {
  uidvalidity : Imap.Uidvalidity.t;
  inspected_uids : int;
      (** [inspected_uids] is the width of the UID range above the saved
          frontier, including UIDs that no longer exist. *)
  matching_uids : Imap.Uid.t list;
}

val inspect_append_candidates :
  ?max_uids:int -> ?max_body_bytes:int64 ->
  client:Imap_eio.Client.t -> store:Imap_store.t ->
  scope:Imap.Mirror.scope -> mailbox:string -> id:string ->
  spool_dir:_ Eio.Path.t -> unit -> (append_candidates, error) result
(** Read-only, bounded diagnostic for a pending APPEND lacking APPENDUID.
    Inspect UIDs above the saved pre-send frontier, requiring the same epoch,
    flags, exact byte length and SHA-256 digest. If the intent saved an
    INTERNALDATE, compare the represented instant across timezone offsets
    before reading a candidate body. Refuse a candidate range over
    [max_uids] (default 1000) instead of silently truncating it. Body reads
    have an aggregate [max_body_bytes] budget (default 1 GiB). [max_uids]
    must be 1 to 10,000, [max_body_bytes] positive and [spool_dir] a
    directory, or the call returns [Invalid_configuration]. Matching bytes
    do not attribute an APPEND to this client, and this call never confirms
    an intent, pairs an occurrence or authorizes replay. The mailbox name
    must encode to the scope's wire name, and a saved OBJECTID+ mailbox
    binding is verified before inspecting any UID. *)
