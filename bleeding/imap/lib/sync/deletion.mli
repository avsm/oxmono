(** Policy-gated deletion of one IMAP and Maildir pair.

    A missing side is actionable only when a complete published inventory
    proves its absence, as {!Bridge} describes, and the survivor must still
    have the pair's digest, length and common flags. No call sends a
    mailbox-wide EXPUNGE or retries an uncertain remote mutation.
    Journaling and uncertain outcomes follow the contract of {!Error}.
    [reconcile_pair] and [recover_operation] run under the writer lease
    their caller holds across the remote scan, the local inventory and the
    call. {!Repair} holds the operator repairs of pending deletions. *)

type outcome =
  | Unchanged  (** [Unchanged] is a pair that needs no deletion. *)
  | Held of Imap.Sync_policy.deletion_hold
      (** [Held why] is a deletion held for [why]. *)
  | Deleted of Imap_store.Journal.pair
      (** [Deleted p] is the committed pair [p] with the tombstone of the
          deleted side. *)
(** The type for the results of a deletion. *)

val expunge_preflight :
  before_flags:Mail_flag.Imap_flag.t list -> before_modseq:int64 ->
  (Mail_flag.Imap_flag.t list * int64 option) option -> bool
(** [expunge_preflight ~before_flags ~before_modseq after] is [true] when
    [after] holds exactly [before_flags] plus [\\Deleted] and a MODSEQ
    above [before_modseq], or equal to it when [before_flags] already held
    [\\Deleted]. It is checked after the conditional STORE, immediately
    before a targeted UID EXPUNGE. *)

val plan :
  policy:Imap.Sync_policy.deletion_policy -> min_absence_scans:int ->
  current_generation:int64 ->
  last_presence:([ `Remote | `Local ] -> int64 option) ->
  remote_present:bool -> local_present:bool -> Imap_store.Journal.pair ->
  Imap.Sync_policy.deletion_plan
(** [plan ~policy ~min_absence_scans ~current_generation ~last_presence
    ~remote_present ~local_present pair] is the deletion decision under
    [policy] for [pair] when complete inventories of generation
    [current_generation] show its sides as [remote_present] and
    [local_present]. It applies
    {!Imap.Sync_policy.plan_disappearance_with_grace}, where the absence of
    the missing side matures [min_absence_scans] complete scan generations
    after its first durable absence unless [last_presence side], the last
    generation that saw [side] present, supersedes it. A delete action is
    held as [Missing_content_evidence] when [pair] saved no digest or
    length, and as [Unverified_absence] when the missing side lacks its
    absence tombstone. It reads no state except through [last_presence].

    @raise Invalid_argument if [min_absence_scans] is negative. *)

val reconcile_pair :
  ?min_absence_scans:int -> ctx:Ctx.t -> writer:Maildir.writer ->
  cursor:Imap.Mirror.cursor -> local_inventory:Local_inventory.t ->
  pair:Imap_store.Journal.pair -> policy:Imap.Sync_policy.deletion_policy ->
  unit -> (outcome, Error.t) result
(** [reconcile_pair ~ctx ~writer ~cursor ~local_inventory ~pair ~policy ()]
    plans with {!plan} from the complete inventories [cursor] and
    [local_inventory], and deletes the surviving side of [pair] when the
    plan allows it. [min_absence_scans] defaults to 0, and a positive value
    also holds an absence whose tombstone recorded no generation. [Preserve]
    only reports a hold. [Propagate] removes an unchanged local survivor
    when the remote UID is absent, or an unchanged remote survivor when the
    local occurrence is absent, and [Propagate_remote] and [Propagate_local]
    enable only the corresponding direction. A local [Retention] tombstone
    always holds remote deletion. A saved content or identity conflict holds
    either direction as [Survivor_changed].

    A local deletion requires a live read-only check that the UID is
    absent, and a present UID returns [Stale_inventory]. The operation is
    journaled before {!Maildir.remove}, and a survivor whose bytes, flags or
    file changed is held as [Survivor_changed]. An unlink that leaves the
    occurrence present is marked ambiguous and returns
    [Pending_operations].

    A remote deletion requires UIDPLUS, CONDSTORE, a [ctx.spool_dir]
    directory, [\\Deleted] in PERMANENTFLAGS and a nonzero MODSEQ on the
    target, and otherwise returns [Unsupported]. A local occurrence that is
    present again, or a target that is already gone, returns
    [Stale_inventory]. It verifies the remote body through [ctx.spool_dir]
    with bounded memory, adds [\\Deleted] with a conditional UID STORE,
    fetches the target again and requires {!expunge_preflight}, sends UID
    EXPUNGE for exactly the paired UID, and verifies the UID absent before
    committing the tombstone and the operation together. A concurrent flag
    change, including MODIFIED on the STORE, rejects the operation and is
    held as [Survivor_changed]. A STORE rejected or refused before dispatch
    rejects the operation and returns [Client]. Any other STORE or EXPUNGE
    failure marks the operation ambiguous and returns [Client]. Any other
    uncertain result marks it ambiguous with its cause and returns
    [Pending_operations].

    A pending operation for the pair returns [Pending_operations]. A pair
    that changed returns [Stale_pair] or [Missing_pair], a pair without a
    remote and local identity [Identity_changed], and a [cursor] that is not
    the current complete inventory of the pair's scope and epoch
    [Stale_inventory].

    @raise Invalid_argument if [min_absence_scans] is negative. *)

val recover_operation :
  store:Imap_store.t -> writer:Maildir.writer ->
  cursor:Imap.Mirror.cursor ->
  local_inventory:Local_inventory.t ->
  operation:Imap_store.Journal.operation -> unit ->
  (outcome, Error.t) result
(** [recover_operation ~store ~writer ~cursor ~local_inventory ~operation ()]
    settles the pending deletion [operation] from the complete inventories
    [cursor] and [local_inventory] without connecting to IMAP. A [Prepared]
    operation is rejected, because no send began, and the result is
    [Unchanged]. A sent, ambiguous or observed deletion commits only when
    both sides are absent and the pair carries the absence tombstone of the
    side the operation did not delete. Otherwise it stays pending, the
    result is [Pending_operations], and it is never replayed. Call it before
    any new copy or flag change for the pair.

    An operation that changed, is finished or is not a deletion returns
    [Diverged]. A missing pair returns [Missing_pair], a pair in another
    scope or with another identity [Stale_pair], a pair without its content
    identity [Identity_changed], and a [cursor] that is not the current
    complete inventory of the pair's epoch [Stale_inventory]. *)
