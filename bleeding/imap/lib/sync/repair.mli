(** Operator-attested repairs of what a sync cycle holds.

    A repair acts on one journal operation or pair that a cycle holds
    because its outcome or cause is uncertain, after an operator has checked
    the endpoints and supplied [evidence], a printable note of 1 to 1024
    bytes that is saved in the journal. Every repair takes the Maildir
    writer lease itself, so the caller must not hold it, and a busy lease
    returns [Writer_busy]. Online repairs check a saved OBJECTID+ binding of
    [ctx.scope] against [ctx.mailbox] before any mutation. No repair replays
    an uncertain remote mutation, and invalid [evidence] returns
    [Invalid_configuration]. {!inspect_append_candidates} is the read-only
    evidence an operator gathers before {!record_appenduid}. *)

val settle_flags :
  ctx:Ctx.t -> maildir:Maildir.t -> id:string -> evidence:string -> unit ->
  (Flags.outcome, Error.t) result
(** [settle_flags ~ctx ~maildir ~id ~evidence ()] settles the sent,
    ambiguous or observed FLAGS operation [id] after both endpoints were
    brought to the same flags by hand. It verifies the operation and pair
    revision, UIDVALIDITY, the paired local content and date, and equal
    flags and a stable nonzero MODSEQ across two remote reads with a local
    read between them. It sends no STORE and changes no Maildir flags. One
    SQLite transition adopts the agreed flags as common, rejects the
    superseded operation with [evidence] and resolves its flag conflict.
    An unknown or finished operation returns [No_pending_operation], a pair
    in another scope or at another revision [Stale_pair], a changed local
    body [Content_mismatch], a missing MODSEQ
    [Conditional_store_unavailable], and any disagreement [Diverged], which
    leaves every state pending. *)

val local_delete :
  ctx:Ctx.t -> maildir:Maildir.t -> id:string -> evidence:string -> unit ->
  (Deletion.outcome, Error.t) result
(** [local_delete ~ctx ~maildir ~id ~evidence ()] finishes the sent or
    ambiguous local deletion [id] whose exact Maildir occurrence is still
    present. It verifies the saved pair revision and identity, the remote
    absence tombstone, the current complete inventory, a live read-only
    absence of the UID and the local bytes, length and flags, then unlinks
    the occurrence and commits the operation. An unlink whose result is
    uncertain returns [Pending_operations] and is recovered by a later
    complete inventory. An unknown operation returns [Diverged], a changed
    pair [Stale_pair], a present remote UID [Stale_inventory] and a changed
    occurrence [Identity_changed]. *)

val reject_remote_delete :
  ctx:Ctx.t -> maildir:Maildir.t -> id:string -> evidence:string -> unit ->
  (unit, Error.t) result
(** [reject_remote_delete ~ctx ~maildir ~id ~evidence ()] rejects the sent
    or ambiguous remote deletion [id] whose UID is still present with the
    paired bytes and flags and a stable MODSEQ across two reads. It
    requires the UID in the current complete published inventory, the
    local side absent, the saved pair revision and identity, and CONDSTORE,
    and it stages the body in [ctx.spool_dir], which must be a directory.
    It sends no STORE or EXPUNGE. A missing capability or spool directory
    returns [Unsupported], and a changed or expunged UID [Identity_changed],
    which leaves the operation pending. *)

val finish_remote_delete :
  ctx:Ctx.t -> maildir:Maildir.t -> id:string -> evidence:string -> unit ->
  (Deletion.outcome, Error.t) result
(** [finish_remote_delete ~ctx ~maildir ~id ~evidence ()] completes the
    sent or ambiguous remote deletion [id] whose UID is still present with
    the paired bytes, its original flags plus [\Deleted] and a stable
    MODSEQ. It checks what {!reject_remote_delete} checks and requires
    UIDPLUS, then saves [evidence] and the [Ambiguous] state before sending
    only a UID EXPUNGE of that UID. A lost result returns
    [Pending_operations] and is recovered by a later complete inventory. A
    remote edit between the final FETCH and the EXPUNGE cannot be
    excluded. *)

val local_append :
  ctx:Ctx.t -> maildir:Maildir.t -> id:string -> evidence:string -> unit ->
  (unit, Error.t) result
(** [local_append ~ctx ~maildir ~id ~evidence ()] finishes the sent or
    ambiguous remote-to-Maildir copy [id] whose reserved occurrence is
    absent. It requires the source UID in the complete published inventory
    of the saved UIDVALIDITY, and it checks the live flags and INTERNALDATE
    before and after archiving the exact bytes and length. The reserved
    occurrence is published, its bytes and flags are verified, and the
    operation and pair commit together. A crash after publication is
    recovered by {!Bridge.copy_once} and must not be repaired again. An
    operation that is not a pending local append returns
    [Invalid_operation], and a changed source [Flags_diverged],
    [Date_diverged], [Content_diverged] or [Source_vanished]. *)

val record_appenduid :
  store:Imap_store.t -> scope:Imap.Mirror.scope -> maildir:Maildir.t ->
  id:string -> uidvalidity:Imap.Uidvalidity.t -> uid:Imap.Uid.t ->
  evidence:string -> unit -> (unit, Error.t) result
(** [record_appenduid ~store ~scope ~maildir ~id ~uidvalidity ~uid
    ~evidence ()] records an externally recovered APPENDUID [uid] for the
    pending upload [id]. It is an attestation of attribution, since equal
    bytes cannot prove which client appended a UID. It checks the
    operation's scope, destination epoch and legacy intent, then saves the
    receipt under the Maildir writer lease without connecting to IMAP or
    creating a pair. The next {!Bridge.copy_once} must find [uid] in a
    complete scan and verify its bytes, length and flags against the
    unchanged local occurrence before committing, and a missing or
    different UID stays pending. A mismatched operation or intent returns
    [Invalid_operation]. *)

val mark_local_retention :
  store:Imap_store.t -> scope:Imap.Mirror.scope -> spool_dir:_ Eio.Path.t ->
  maildir:Maildir.t -> pair_id:string -> evidence:string -> unit ->
  (unit, Error.t) result
(** [mark_local_retention ~store ~scope ~spool_dir ~maildir ~pair_id
    ~evidence ()] attests that the missing local occurrence of [pair_id]
    was removed by local retention, not by a user. It requires a complete
    Maildir inventory staged in [spool_dir], which must be a directory, no
    active operation for the pair and an extant remote binding. The durable
    retention tombstone stops later propagation of this absence to the
    server. No IMAP connection is made. A pair that does not qualify
    returns [Invalid_operation]. *)

type append_candidates = {
  uidvalidity : Imap.Uidvalidity.t;
  inspected_uids : int;
      (** [inspected_uids] is the width of the UID range above the saved
          frontier, including UIDs that no longer exist. *)
  matching_uids : Imap.Uid.t list;
}

val inspect_append_candidates :
  ?max_uids:int -> ?max_body_bytes:int64 -> ctx:Ctx.t -> id:string ->
  unit -> (append_candidates, Error.t) result
(** [inspect_append_candidates ~ctx ~id ()] lists the UIDs above the saved
    pre-send frontier of the pending APPEND [id] that match its epoch,
    flags, exact length and SHA-256 digest. When the intent saved an
    INTERNALDATE, the represented instant is compared across timezone
    offsets before any body is read. A candidate range wider than
    [max_uids] (default 1000) is refused rather than truncated, and body
    reads share a [max_body_bytes] budget (default 1 GiB) through spool
    files in [ctx.spool_dir]. [max_uids] must be 1 to 10,000,
    [max_body_bytes] positive and [ctx.spool_dir] a directory, or the call
    returns [Invalid_configuration]. It is read-only. Matching bytes do not
    attribute an APPEND to this client, and this call never confirms an
    intent, pairs an occurrence or authorizes a replay. *)
