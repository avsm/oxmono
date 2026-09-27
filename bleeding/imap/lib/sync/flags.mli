(** Durable three-way flag reconciliation for one paired IMAP/Maildir
    occurrence. This module does not discover pairs or publish mailbox
    inventories. [reconcile_pair] and [recover_operation] take the
    {!Maildir.writer} of the lease the caller holds. {!Repair.settle_flags}
    settles a pending FLAGS operation by hand. *)

type outcome = Unchanged | Updated of Imap_store.Journal.pair

type plan = No_change | Apply of Mail_flag.Imap_flag.t list

type decision = {
  plan : plan;
      (** [Apply merged] carries the new common flags. [merged] keeps the
          base state of a held [\\Deleted], and each endpoint keeps its own
          [\\Deleted] state while its other flags move to [merged]. *)
  deleted_held : bool;
      (** [deleted_held] is [true] when the endpoints disagree on
          [\\Deleted] and propagation was not requested. *)
}

val plan_flags :
  ?propagate_deleted:bool -> base:Mail_flag.Imap_flag.t list ->
  remote:Mail_flag.Imap_flag.t list -> local:Mail_flag.Imap_flag.t list ->
  condstore:bool -> remote_modseq:int64 option ->
  unit -> (decision, Error.t) result
(** [plan_flags ~base ~remote ~local ~condstore ~remote_modseq ()] is the
    pure decision for the observed flags. Every flag other than [\\Deleted]
    merges. [propagate_deleted] defaults to [false], which holds a
    [\\Deleted] change and reports it in [deleted_held]. A remote write
    requires [condstore] and a positive [remote_modseq], and otherwise
    returns [Conditional_store_unavailable]. Local-only changes need
    neither. *)

val validate_permanent_flags :
  available:string list option -> defined:string list option ->
  remote:Mail_flag.Imap_flag.t list ->
  merged:Mail_flag.Imap_flag.t list -> (unit, Error.t) result
(** [validate_permanent_flags ~available ~defined ~remote ~merged] checks
    that moving the remote flags from [remote] to [merged] is permitted by
    SELECT's PERMANENTFLAGS [available] and FLAGS [defined]. When
    [available] is [None] every flag is permanent, as RFC 9051 section 6.3.2
    requires. Otherwise every added or removed flag must be listed, except
    that [\\*] permits adding a keyword absent from [defined]. A keyword in
    [defined] but not in [available] is never settable. *)

type reconciled = {
  outcome : outcome;
  deleted_held : bool;
      (** [deleted_held] is [true] when a [\\Deleted] change was held while
          the other flags reconciled. *)
}

val reconcile_pair :
  ?propagate_deleted:bool -> ?inventory:Local_inventory.t -> ctx:Ctx.t ->
  writer:Maildir.writer -> pair:Imap_store.Journal.pair -> unit ->
  (reconciled, Error.t) result
(** [reconcile_pair ~ctx ~writer ~pair ()] fetches the current UID FLAGS and
    MODSEQ and the Maildir flags, merges them against [pair.common_flags] as
    {!plan_flags} does, and journals an intent before changing either side.
    [inventory], when given, is the live paged view the caller holds, and the
    local occurrence is read from it.

    Remote writes use UID STORE FLAGS with UNCHANGEDSINCE, and a remote
    change not permitted by SELECT's PERMANENTFLAGS returns
    [Permanent_flag_unavailable]. When both endpoints already hold the merge,
    the common baseline advances without an operation.

    The local occurrence must match the pair's saved body digest and length.
    A mismatch creates a durable [Content_conflict] and returns
    [Content_mismatch] without creating a FLAGS operation. An observation
    that is stale because the file changed returns [Modified]. For a
    local-only change, a concurrent change on either side before the local
    write rejects the operation and returns [Modified]. A STORE rejected or
    refused before dispatch rejects the operation and returns [Client].

    A successful call verifies both endpoints before atomically committing
    the new pair revision and operation. A mutation with uncertain outcome,
    including a failed read after STORE, remains pending with a durable
    [Flag_conflict] record and returns [Pending_operations]. Callers must
    call [recover_operation] or investigate it before issuing a new write
    for this pair. A pending FLAGS operation for the pair returns
    [Pending_operations], and one of another kind returns [Diverged]. Store
    exceptions, Maildir concurrency exceptions and Eio cancellation
    propagate. *)

val recover_operation :
  ?inventory:Local_inventory.t -> ctx:Ctx.t -> writer:Maildir.writer ->
  operation:Imap_store.Journal.operation -> unit ->
  (outcome, Error.t) result
(** [recover_operation ~ctx ~writer ~operation ()] verifies
    the saved pair revision, UIDVALIDITY, remote target and paired local body
    hash and length. [inventory], when given, is the live paged view the
    caller holds. If local flags are still the saved preimage, it finishes
    the local write and commits. If both sides already match, it commits.
    It never replays an uncertain remote STORE. Divergence, a missing
    preimage or missing content evidence holds the operation and records a
    stable-ID flag conflict. A changed local body is held before any remote
    read, and repeated recovery preserves the conflict ID. A verified pair
    commit resolves that conflict atomically with the operation and new
    common flags, and a held operation returns [Pending_operations]. A
    [Prepared] intent is rejected because dispatch had not begun. A pair in
    another scope returns [Stale_pair]. *)
