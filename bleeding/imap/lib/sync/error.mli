(** Errors of every [Imap_sync] call.

    Every [Imap_sync] call that changes IMAP or the Maildir journals the
    change durably before it sends it, and commits it only after the result
    verifies. An error never means that a sent mutation was undone. A
    mutation whose outcome is uncertain stays pending in the journal, and no
    call replays it. A later cycle settles it from a complete inventory, or
    an operator settles it with {!Repair}.

    An error stops the call. A hold is not an error. {!Bridge.copy_once}
    records a durable conflict for a pair it cannot settle safely, counts it
    in its receipt and continues with the next pair. Each constructor below
    says whether a retry, a later reconciliation or an operator action
    follows it. *)

type t =
  | Client of Imap_eio.Error.t
      (** [Client e] is an IMAP connection or command failure. A later
          call retries it. A mutation sent before the failure stays pending,
          and the next cycle reconciles it first. *)
  | Maildir of Maildir.error
      (** [Maildir e] is a Maildir format or policy failure. A retry fails
          the same way until an operator repairs the Maildir. *)
  | Store_stale_revision
      (** [Store_stale_revision] is a published cursor or journal row that
          changed between a read and its compare-and-swap. A retry reads
          the new state. *)
  | Mirror of Imap.Mirror.error
      (** [Mirror e] is a SELECT response that no scan can be planned from.
          No retry fixes it. *)
  | Invalid_scope of string
      (** [Invalid_scope why] is a mailbox name, name encoding or OBJECTID+
          binding that no longer names the scope. An operator corrects the
          configuration, since no retry fixes it. *)
  | Incomplete of string
      (** [Incomplete why] is a server response that omitted part of an
          inventory or a message, a stored body that failed verification, or
          a call that needs a complete published inventory before one
          exists. A later scan or retry can succeed. *)
  | Limit of string
      (** [Limit why] is a scan, transfer or audit budget that is invalid or
          too small for the mailbox, or a spool directory or identifier the
          call cannot use. A retry with the same arguments fails the same
          way. *)
  | Uidvalidity_changed
      (** [Uidvalidity_changed] is a selected UIDVALIDITY that differs from
          the epoch the call must preserve. No retry in the same epoch
          succeeds, and pairs of the earlier epoch need operator
          reconciliation before a cycle proceeds. *)
  | Writer_busy
      (** [Writer_busy] is a Maildir writer lease that another process or
          handle holds. A later call retries it. *)
  | Missing_pair
      (** [Missing_pair] is a pair, or the pair an operation names, that is
          absent from the journal. An operator inspects the operation. *)
  | Stale_pair
      (** [Stale_pair] is a pair whose revision, scope or identity differs
          from the one the operation was journaled against. A later cycle
          rereads the pair, and a repair needs a fresh inspection. *)
  | Missing_occurrence
      (** [Missing_occurrence] is a paired endpoint whose occurrence is
          absent. A later complete inventory records the absence for the
          deletion policy. *)
  | Stale_inventory
      (** [Stale_inventory] is a live observation that contradicts the
          published complete inventory, or an inventory that is no longer
          current. A later cycle publishes a fresh inventory first. *)
  | Identity_changed
      (** [Identity_changed] is a paired occurrence whose bytes, flags or
          MODSEQ changed since the operation was journaled, or a pair that
          lacks its content identity. Nothing is committed, and an operator
          inspects the operation. *)
  | Modified
      (** [Modified] is an endpoint that changed after it was read and
          before any write, so the operation was rejected unapplied. A later
          cycle retries from fresh observations. *)
  | Conditional_store_unavailable
      (** [Conditional_store_unavailable] is a remote flag write that lacks
          CONDSTORE or a message MODSEQ. A cycle holds the pair instead. *)
  | Permanent_flag_unavailable of Mail_flag.Imap_flag.t
      (** [Permanent_flag_unavailable f] is a remote change of [f] that
          SELECT's PERMANENTFLAGS does not permit. A cycle holds the pair
          instead. *)
  | Unsupported of string
      (** [Unsupported why] is a targeted deletion or repair that the server
          or the configuration cannot perform. A cycle holds the pair
          instead. *)
  | Pending_operations of string list
      (** [Pending_operations ids] names journal operations that remain
          pending and block new work. A later cycle reconciles those its
          evidence settles, and an operator repairs the rest. *)
  | No_pending_operation
      (** [No_pending_operation] is a repair whose operation is unknown,
          finished or of another kind. The operator checks the ID. *)
  | Bootstrap_requires_pairing
      (** [Bootstrap_requires_pairing] is a first cycle with unpaired
          messages on both endpoints and duplicate import not allowed. An
          operator inspects both endpoints before allowing it. *)
  | Source_vanished of Imap.Uid.t
      (** [Source_vanished uid] is a remote message expunged before its
          body was archived. A later scan drops it, so a retry
          succeeds. *)
  | Local_source_changed of string
      (** [Local_source_changed id] is a Maildir occurrence that changed
          before or during its upload. A later cycle rereads it. *)
  | Content_mismatch of string
      (** [Content_mismatch pair_id] is a paired local body that differs
          from the paired digest. A durable content conflict records it, and
          restoring the paired bytes clears it. *)
  | Content_diverged of string
      (** [Content_diverged id] is a copy whose bytes differ from the
          journaled digest or length. The operation stays pending for an
          operator. *)
  | Flags_diverged of string
      (** [Flags_diverged id] is a copy whose flags differ from the
          journaled flags. The operation stays pending for an operator. *)
  | Date_diverged of string
      (** [Date_diverged id] is a copy whose INTERNALDATE differs from the
          journaled date. The operation stays pending for an operator. *)
  | Diverged of string
      (** [Diverged why] is an endpoint or journal state that disagrees with
          the operation in a way no other constructor names. Nothing is
          committed, and an operator inspects the operation. *)
  | Invalid_operation of string
      (** [Invalid_operation why] is a journal operation or operator request
          that the call cannot act on, or a remote message that Maildir
          cannot store. No retry changes the outcome. *)
  | Invalid_configuration of string
      (** [Invalid_configuration why] is an invalid argument, such as a
          negative budget, a missing spool directory or malformed operator
          evidence. The caller corrects it. *)
(** The type for sync errors. *)

val pp : Format.formatter -> t -> unit
(** [pp ppf e] prints a one-line description of [e] on [ppf]. *)

val to_string : t -> string
(** [to_string e] is the text [pp] prints for [e]. *)
