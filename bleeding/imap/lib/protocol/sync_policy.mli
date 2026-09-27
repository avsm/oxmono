@@ portable

(** Flag and deletion reconciliation policy for a paired message.

    The policy is conservative and pure. The caller must first prove that
    both observations refer to the same message occurrence and that each
    endpoint's inventory is complete. A plan is a decision only. The caller
    journals, sends and verifies every change it implies. *)

(** {1 Flags} *)

type flag_delta = {
  add : Mail_flag.Imap_flag.t list;
  remove : Mail_flag.Imap_flag.t list;
}
(** The type for the flag changes that bring one endpoint to the merged
    state. *)

type flag_plan = {
  merged : Mail_flag.Imap_flag.t list;
      (** The new common flag set, in {!Mail_flag.Imap_flag.compare}
          order. *)
  to_remote : flag_delta;  (** The changes the remote endpoint needs. *)
  to_local : flag_delta;  (** The changes the local endpoint needs. *)
  deleted_held : bool;
      (** [true] when the endpoints disagree on [\Deleted] and its
          propagation was not requested. [merged] then keeps the base state
          of [\Deleted] and neither delta mentions it. *)
}
(** The type for flag reconciliation plans. *)

val reconcile_flags :
  ?propagate_deleted:bool ->
  base:Mail_flag.Imap_flag.t list ->
  remote:Mail_flag.Imap_flag.t list ->
  local:Mail_flag.Imap_flag.t list ->
  unit -> flag_plan
(** [reconcile_flags ~propagate_deleted ~base ~remote ~local ()] is the
    three-way merge of [remote] and [local] from their last common flag set
    [base]. A flag added on either side is kept, and a flag removed on
    either side is dropped. Flags compare case-insensitively as IMAP flags,
    and where spellings differ the remote spelling wins. The session-only
    [\Recent] is ignored.

    [\Deleted] is a message flag, not proof of expunge or user deletion.
    [propagate_deleted] defaults to [false], which holds a [\Deleted] the
    endpoints disagree on and reports it through [deleted_held]. Every
    other flag still merges.

    Each delta is relative to the endpoint as observed. Send the deltas as
    conditional additions and removals, and verify them before persisting
    [merged] as the new common state. *)

(** {1 Deletions} *)

type deletion_policy =
  | Preserve  (** Never delete. *)
  | Propagate  (** Propagate a disappearance in either direction. *)
  | Propagate_remote
      (** Propagate a remote disappearance to the local side only. *)
  | Propagate_local
      (** Propagate a local disappearance to the remote side only. *)
(** The type for deletion policies. *)

type deletion_hold =
  | Incomplete_inventory
      (** The inventory of the missing endpoint is incomplete. *)
  | Unpaired_identity  (** The occurrences are not proven to be a pair. *)
  | Survivor_changed  (** The surviving occurrence changed. *)
  | Preservation_policy  (** The policy is [Preserve]. *)
  | Direction_policy  (** The policy excludes this direction. *)
  | Retention_policy  (** The local absence is marked retained. *)
  | Unverified_absence
      (** The absence lacks its durable tombstone. *)
  | Grace_period  (** The absence is not yet mature. *)
  | Missing_content_evidence
      (** The pair saved no content digest or length, so its survivor
          cannot be verified. *)
(** The type for reasons a deletion is held. {!plan_disappearance_with_grace}
    never returns [Unverified_absence] or [Missing_content_evidence], which
    the caller's own checks report. *)

type deletion_plan =
  | No_deletion  (** Both endpoints agree on presence. *)
  | Hold_deletion of deletion_hold
  | Delete_remote  (** Delete the remote occurrence. *)
  | Delete_local  (** Delete the local occurrence. *)
(** The type for deletion plans. *)

val absence_mature : last_present_generation:int64 option ->
  current_generation:int64 -> first_generation:int64 option ->
  min_scans:int -> bool
(** [absence_mature ~last_present_generation ~current_generation
    ~first_generation ~min_scans] is [true] when at least [min_scans]
    complete scan generations separate [current_generation] from
    [first_generation], the first durable absence observation. A presence
    at [last_present_generation] at or after [first_generation] supersedes
    the absence, and the result stays [false] until a new absence is
    published, even when [min_scans] is 0. Otherwise a [min_scans] of 0 or
    less is always mature, and an absence of unknown age, with
    [first_generation] [None], is mature only then. *)

type observation = {
  present : bool;  (** The endpoint holds the occurrence. *)
  complete : bool;
      (** [present] comes from a complete inventory of the endpoint. *)
}
(** The type for one endpoint's view of an occurrence. *)

val plan_disappearance_with_grace :
  absence_mature:bool -> policy:deletion_policy -> paired:bool ->
  local_retained:bool -> survivor_unchanged:bool ->
  remote:observation -> local:observation -> deletion_plan
(** [plan_disappearance_with_grace ~absence_mature ~policy ~paired
    ~local_retained ~survivor_unchanged ~remote ~local] is the deletion
    plan for a pair whose endpoints were observed as [remote] and [local].
    It is [No_deletion] when both are present or both absent. Otherwise
    the first failing check holds the deletion, in this order. The missing
    endpoint's inventory must be complete, the occurrences [paired], the
    survivor unchanged by [survivor_unchanged], a local absence not
    [local_retained], and [policy] must allow the direction. An allowed
    deletion is held as [Grace_period] while [absence_mature] is [false].
    Before acting on a [Delete_remote] or [Delete_local] the caller must
    check that the missing endpoint has its durable absence tombstone, and
    then journal and verify the mutation. [\Deleted] alone is not
    absence. *)
