(** Operator-attested repairs of what a sync cycle holds.

    A repair acts on one journal operation or pair that a cycle holds
    because its outcome or cause is uncertain, after an operator has checked
    the endpoints. The operator's [evidence], 1 to 1024 printable bytes that
    are not all spaces, is saved in the journal, and invalid [evidence]
    returns [Invalid_configuration] before anything else. Every repair takes
    the Maildir writer lease as {!Bridge} describes. An online repair checks
    a saved OBJECTID+ binding of [ctx.scope] against [ctx.mailbox] before
    any mutation, and a mismatch returns [Invalid_scope]. No repair replays
    an uncertain remote mutation, and failures follow the contract of
    {!Error}. {!inspect_append_candidates} is the read-only evidence an
    operator gathers before {!record_appenduid}. *)

(** {1 Flags} *)

val settle_flags :
  ctx:Ctx.t -> maildir:Maildir.t -> id:string -> evidence:string -> unit ->
  (Flags.outcome, Error.t) result
(** [settle_flags ~ctx ~maildir ~id ~evidence ()] settles the sent,
    ambiguous or observed FLAGS operation [id] after an operator brought
    both endpoints of its pair to the same flags. It verifies the pair
    revision and UIDVALIDITY the operation saved, the paired local content
    and date, and equal flags and a stable nonzero MODSEQ across two remote
    reads, each after a local read. It sends no STORE and changes no
    Maildir flags. One transaction adopts the agreed flags as common,
    rejects the superseded operation with [evidence] and resolves its flag
    conflict, and the result is [Updated].

    An unknown, finished or other operation returns [No_pending_operation],
    a pair in another scope or at another revision [Stale_pair], a changed
    local body [Content_mismatch], a missing MODSEQ
    [Conditional_store_unavailable], and any disagreement [Diverged]. Every
    failure leaves the operation pending. *)

(** {1 Deletions} *)

val local_delete :
  ctx:Ctx.t -> maildir:Maildir.t -> id:string -> evidence:string -> unit ->
  (Deletion.outcome, Error.t) result
(** [local_delete ~ctx ~maildir ~id ~evidence ()] finishes the sent or
    ambiguous local deletion [id] whose exact Maildir occurrence is still
    present. It verifies the saved pair revision and identity, the remote
    absence tombstone, the UID's absence from the current complete inventory
    and from a live read-only selection, and the local bytes, length and
    flags. It then unlinks the occurrence and commits the operation, and the
    result is [Deleted]. An unlink that leaves the occurrence present
    returns [Pending_operations], and a later complete inventory settles
    it.

    An operation that is not a pending local deletion of the scope returns
    [Diverged], a changed or locally tombstoned pair [Stale_pair], a
    present UID or a missing remote tombstone [Stale_inventory], and an
    absent or changed occurrence [Identity_changed]. *)

val reject_remote_delete :
  ctx:Ctx.t -> maildir:Maildir.t -> id:string -> evidence:string -> unit ->
  (unit, Error.t) result
(** [reject_remote_delete ~ctx ~maildir ~id ~evidence ()] rejects the sent
    or ambiguous remote deletion [id] whose UID still has the paired bytes
    and common flags and a stable nonzero MODSEQ across two reads. It
    requires the saved pair revision and identity, a recorded local absence
    that is still true, the UID in the current complete inventory, and
    CONDSTORE. It stages the body in [ctx.spool_dir] and sends no STORE or
    EXPUNGE.

    A [ctx.spool_dir] that is not a directory or a server without CONDSTORE
    returns [Unsupported]. An operation that is not a pending remote
    deletion of the scope returns [Diverged], a changed pair or one without
    a recorded local absence [Stale_pair], a local occurrence that is
    present, a UID absent from the published inventory or a publication
    during the check [Stale_inventory], and a changed or expunged target
    [Identity_changed].
    Every failure leaves the operation pending. *)

val finish_remote_delete :
  ctx:Ctx.t -> maildir:Maildir.t -> id:string -> evidence:string -> unit ->
  (Deletion.outcome, Error.t) result
(** [finish_remote_delete ~ctx ~maildir ~id ~evidence ()] completes the
    sent or ambiguous remote deletion [id] whose UID still has the paired
    bytes, the common flags plus [\Deleted] and a stable nonzero MODSEQ. It
    checks what {!reject_remote_delete} checks and requires UIDPLUS. It then
    saves [evidence] and marks the operation ambiguous before it sends a
    UID EXPUNGE of that UID alone, and the result is [Deleted] once the UID
    is absent. A remote edit between the final FETCH and the EXPUNGE cannot
    be excluded.

    The failures of {!reject_remote_delete} apply, and a server without
    UIDPLUS returns [Unsupported]. A target that changed after the evidence
    was saved returns [Identity_changed], a failed EXPUNGE [Client], and a
    UID still present [Pending_operations]. In each of these the operation
    stays pending, and a later complete inventory settles it. *)

val mark_local_retention :
  store:Imap_store.t -> scope:Imap.Mirror.scope -> spool_dir:_ Eio.Path.t ->
  maildir:Maildir.t -> pair_id:string -> evidence:string -> unit ->
  (unit, Error.t) result
(** [mark_local_retention ~store ~scope ~spool_dir ~maildir ~pair_id
    ~evidence ()] attests that the missing local occurrence of [pair_id] in
    [scope] was removed by local retention, not by a user, and records a
    durable retention tombstone with [evidence]. A later cycle never
    propagates that absence to the server. It stages a complete inventory of
    [maildir] in [spool_dir] and makes no IMAP connection.

    A [spool_dir] that is not a directory returns [Invalid_configuration]. A
    pair that is unknown, in another scope, without a live remote binding,
    with an active operation, with its local occurrence present or with an
    explicit local deletion returns [Invalid_operation]. A pair that changed
    concurrently returns [Store_stale_revision]. *)

(** {1 Copies} *)

val local_append :
  ctx:Ctx.t -> maildir:Maildir.t -> id:string -> evidence:string -> unit ->
  (unit, Error.t) result
(** [local_append ~ctx ~maildir ~id ~evidence ()] finishes the sent or
    ambiguous remote-to-Maildir copy [id] whose reserved occurrence is
    absent. It requires the source UID in the complete published inventory
    of the saved UIDVALIDITY, checks the live flags and INTERNALDATE before
    and after archiving the exact bytes and length, publishes the reserved
    occurrence, verifies its bytes and flags, and commits the operation and
    the pair together. A crash after publication is reconciled by
    {!Bridge.copy_once}, so the repair must not run again.

    An operation that is not a pending local append of the scope, a
    reserved occurrence that exists, a source or occurrence already paired
    and a message Maildir cannot store return [Invalid_operation]. Another
    epoch returns [Uidvalidity_changed], a publication during the repair
    [Store_stale_revision], and a changed source [Flags_diverged],
    [Date_diverged], [Content_diverged] or [Source_vanished]. *)

type append_candidates = {
  uidvalidity : Imap.Uidvalidity.t;
      (** [uidvalidity] is the epoch inspected. *)
  inspected_uids : int;
      (** [inspected_uids] is the width of the UID range above the saved
          frontier, including UIDs that no longer exist. *)
  matching_uids : Imap.Uid.t list;
      (** [matching_uids] lists, in ascending order, the UIDs whose flags,
          length, digest and date match the APPEND. *)
}
(** The type for the result of an APPEND candidate inspection. *)

val inspect_append_candidates :
  ?max_uids:int -> ?max_body_bytes:int64 -> ctx:Ctx.t -> id:string ->
  unit -> (append_candidates, Error.t) result
(** [inspect_append_candidates ~ctx ~id ()] lists the UIDs above the saved
    pre-send frontier of the pending APPEND [id] that match its epoch,
    flags, exact length and SHA-256 digest. When the operation saved an
    INTERNALDATE, the instant is compared across timezone offsets before
    any body is read. Body reads go through spool files in
    [ctx.spool_dir]. It takes neither the lease nor evidence and changes
    nothing. Matching bytes do not attribute an APPEND to this client, and
    this call never confirms an intent, pairs an occurrence or authorizes a
    replay.

    [max_uids] defaults to 1000 and must be 1 to 10,000. A wider candidate
    range is refused, not truncated. [max_body_bytes] defaults to 1 GiB, is
    shared by every body read and must be positive. A budget out of range, a
    range or body total over budget, or a [ctx.spool_dir] that is not a
    directory returns [Invalid_configuration]. An operation that is not a
    pending APPEND of the scope, or a UIDNEXT below the saved frontier,
    returns [Invalid_operation], and another epoch [Uidvalidity_changed]. *)

val record_appenduid :
  store:Imap_store.t -> scope:Imap.Mirror.scope -> maildir:Maildir.t ->
  id:string -> uidvalidity:Imap.Uidvalidity.t -> uid:Imap.Uid.t ->
  evidence:string -> unit -> (unit, Error.t) result
(** [record_appenduid ~store ~scope ~maildir ~id ~uidvalidity ~uid
    ~evidence ()] records [uid] in [uidvalidity], an APPENDUID recovered
    from an independent record, as the receipt of the pending APPEND [id]
    of [scope] in [store]. It attests attribution, since equal bytes cannot
    prove which client appended a UID. It checks the operation's scope,
    destination epoch and {!Imap_store} intent, then saves the receipt
    without connecting to IMAP or creating a pair. Recording the same UID
    again succeeds. The next {!Bridge.copy_once} must find [uid] in a
    complete scan and verify its bytes, length and flags against the
    unchanged local occurrence before it commits, and a missing or different
    UID stays pending. An operation or intent that does not match returns
    [Invalid_operation]. *)
