(** Three-way flag reconciliation of one IMAP and Maildir pair.

    The module merges the flags of an established pair against the pair's
    common flags. It does not discover pairs or publish inventories.
    Journaling and uncertain outcomes follow the contract of {!Error}, and
    [reconcile_pair] and [recover_operation] run under the writer lease
    their caller holds, as {!Bridge} describes. {!Repair.settle_flags}
    settles a pending FLAGS operation by hand. *)

type outcome =
  | Unchanged  (** [Unchanged] is a pair left as it was. *)
  | Updated of Imap_store.Journal.pair
      (** [Updated p] is the committed pair [p] with its new common
          flags. *)
(** The type for the results of a flag reconciliation. *)

type plan =
  | No_change  (** [No_change] is a pair whose endpoints need no write. *)
  | Apply of Mail_flag.Imap_flag.t list
      (** [Apply merged] carries the new common flags [merged]. [merged]
          keeps the base state of a held [\\Deleted], and each endpoint
          keeps its own [\\Deleted] state while its other flags move to
          [merged]. *)
(** The type for flag plans. *)

type decision = {
  plan : plan;  (** [plan] is the write the merge needs. *)
  deleted_held : bool;
      (** [deleted_held] is [true] when the endpoints disagree on
          [\\Deleted] and propagation was not requested. *)
}
(** The type for flag decisions. *)

val plan_flags :
  ?propagate_deleted:bool -> base:Mail_flag.Imap_flag.t list ->
  remote:Mail_flag.Imap_flag.t list -> local:Mail_flag.Imap_flag.t list ->
  condstore:bool -> remote_modseq:int64 option ->
  unit -> (decision, Error.t) result
(** [plan_flags ~base ~remote ~local ~condstore ~remote_modseq ()] is the
    decision for a pair whose common flags are [base] and whose endpoints
    hold [remote] and [local]. Every flag other than [\\Deleted] merges.
    [propagate_deleted] defaults to [false], which holds a [\\Deleted]
    change and reports it in [deleted_held]. A remote write requires
    [condstore] and a positive [remote_modseq], and otherwise the result is
    [Conditional_store_unavailable]. A local-only change needs neither. *)

val validate_permanent_flags :
  available:string list option -> defined:string list option ->
  remote:Mail_flag.Imap_flag.t list ->
  merged:Mail_flag.Imap_flag.t list -> (unit, Error.t) result
(** [validate_permanent_flags ~available ~defined ~remote ~merged] is
    [Ok ()] when SELECT's PERMANENTFLAGS [available] and FLAGS [defined]
    permit moving the remote flags from [remote] to [merged]. When
    [available] is [None] every flag is permanent, as RFC 9051 section
    6.3.2 requires. Otherwise every added or removed flag must be listed,
    except that [\\*] permits adding a keyword absent from [defined]. A
    keyword in [defined] but not in [available] is never settable. The
    first flag that is not permitted is [Permanent_flag_unavailable], and a
    malformed [available] is [Diverged]. *)

type reconciled = {
  outcome : outcome;  (** [outcome] is the result for the pair. *)
  deleted_held : bool;
      (** [deleted_held] is [true] when a [\\Deleted] change was held while
          the other flags reconciled. *)
}
(** The type for the results of {!reconcile_pair}. *)

val reconcile_pair :
  ?propagate_deleted:bool -> ?inventory:Local_inventory.t -> ctx:Ctx.t ->
  writer:Maildir.writer -> pair:Imap_store.Journal.pair -> unit ->
  (reconciled, Error.t) result
(** [reconcile_pair ~ctx ~writer ~pair ()] reads the UID FLAGS and MODSEQ of
    [pair] in a read-write selection of [ctx.mailbox] and its Maildir flags
    through [writer], merges them against [pair.common_flags] as
    {!plan_flags} does, and journals an operation before it changes either
    side. [propagate_deleted] defaults to [false]. [inventory] is the live
    staged view the caller holds, and the local occurrence is read from it.
    It defaults to reading the Maildir.

    When both endpoints already hold the merge, the common baseline advances
    without an operation. Remote writes use UID STORE FLAGS with
    UNCHANGEDSINCE, and a remote change that PERMANENTFLAGS does not permit
    returns [Permanent_flag_unavailable] before any operation. The local
    occurrence must match the pair's saved digest and length. A mismatch
    opens a durable content conflict and returns [Content_mismatch] before
    any operation, and an observation of a file that changed returns
    [Modified].

    A concurrent change seen before any write, or MODIFIED for the pair's
    UID, rejects the operation and returns [Modified]. A STORE rejected or
    refused before dispatch rejects the operation and returns [Client]. Any
    other STORE failure, and a failed read after the STORE, marks the
    operation ambiguous with its cause, records a durable flag conflict
    and returns [Pending_operations] naming it. A write that cannot be
    verified on both endpoints leaves the operation pending with a flag
    conflict and returns [Pending_operations]. Call {!recover_operation} or
    an operator repair before a new write for the pair.

    A verified write commits the new pair revision and the operation
    together. A pair that changed returns [Stale_pair] or [Missing_pair], a
    tombstoned or absent occurrence [Missing_occurrence], and another epoch
    [Uidvalidity_changed]. A pending FLAGS operation for the pair returns
    [Pending_operations], and one of another kind [Diverged]. Store
    exceptions, Maildir concurrency exceptions and Eio cancellation
    propagate. *)

val recover_operation :
  ?inventory:Local_inventory.t -> ctx:Ctx.t -> writer:Maildir.writer ->
  operation:Imap_store.Journal.operation -> unit ->
  (outcome, Error.t) result
(** [recover_operation ~ctx ~writer ~operation ()] settles the pending FLAGS
    [operation] without replaying an uncertain remote STORE. [inventory] is
    as for {!reconcile_pair} and defaults to reading the Maildir.

    A [Prepared] operation is rejected, because dispatch had not begun, and
    the result is [Unchanged]. For a sent, ambiguous or observed operation
    it verifies the saved pair revision, UIDVALIDITY, remote target and
    paired local digest and length. It finishes the local write when the
    local flags are still the saved preimage, and commits when both
    endpoints hold the target. A changed local body is held before any
    remote read. Divergence, a missing preimage or missing content evidence
    holds the operation with a flag conflict whose ID stays stable across
    attempts, and returns [Pending_operations]. A verified commit resolves
    that conflict together with the operation and the new common flags.

    An operation of another kind or a finished one returns [Diverged], a
    pair in another scope or at another revision [Stale_pair], and a missing
    pair [Missing_pair]. *)
