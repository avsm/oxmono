(** Pure, conservative reconciliation policy for a paired message occurrence.
    The caller must first prove that both observations refer to the same
    occurrence and that each endpoint's inventory is complete. *)

type flag_delta = {
  add : Mail_flag.Imap_flag.t list;
  remove : Mail_flag.Imap_flag.t list;
}

type flag_plan = {
  merged : Mail_flag.Imap_flag.t list;
  to_remote : flag_delta;
  to_local : flag_delta;
  deleted_held : bool;
  (** [deleted_held] is [true] when the endpoints disagree on [\Deleted] and
      its propagation was not requested. [merged] then keeps the base state
      of [\Deleted] and neither delta mentions it. *)
}

val reconcile_flags :
  ?propagate_deleted:bool ->
  base:Mail_flag.Imap_flag.t list ->
  remote:Mail_flag.Imap_flag.t list ->
  local:Mail_flag.Imap_flag.t list ->
  unit -> flag_plan
(** [reconcile_flags ~base ~remote ~local ()] is the three-way merge of
    [remote] and [local] from their last common flag set [base]. Adds and
    removals on either side are preserved. Flags compare case-insensitively
    as IMAP flags, and where spellings differ the remote spelling wins.
    Session-only [\Recent] is ignored.

    [\Deleted] is a message flag, not proof of expunge or user deletion.
    [propagate_deleted] defaults to [false], which holds a [\Deleted] change
    the endpoints disagree on and reports it through [deleted_held]. Every
    other flag still merges.

    Deltas are relative to each observed endpoint. Send them as conditional
    additions and removals, and verify them before persisting [merged] as the
    new common state. *)

type deletion_policy = Preserve | Propagate | Propagate_remote | Propagate_local
type deletion_hold =
  | Incomplete_inventory
  | Unpaired_identity
  | Survivor_changed
  | Preservation_policy
  | Direction_policy
  | Retention_policy
  | Unverified_absence
  | Grace_period
  | Missing_content_evidence
      (** [Missing_content_evidence] holds a legacy pair that saved no
          content digest or length, so its survivor cannot be verified. *)
type deletion_plan =
  | No_deletion
  | Hold_deletion of deletion_hold
  | Delete_remote
  | Delete_local

val absence_mature : last_present_generation:int64 option ->
  current_generation:int64 -> first_generation:int64 option ->
  min_scans:int -> bool
(** [absence_mature ~last_present_generation ~current_generation
    ~first_generation ~min_scans] is [true] when [min_scans] complete scan
    generations have passed since [first_generation], the first durable
    absence observation. A presence observation at or after
    [first_generation] supersedes the absence, and the result stays [false]
    until a new absence is published, even when [min_scans] is 0. When
    [first_generation] is [None], a legacy absence of unknown age, the result
    is [true] only when [min_scans] is 0 or less. *)

val plan_disappearance :
  policy:deletion_policy -> paired:bool ->
  remote_present:bool -> remote_complete:bool ->
  local_present:bool -> local_complete:bool ->
  local_retained:bool ->
  survivor_unchanged:bool -> deletion_plan
(** Absence is actionable only after a complete inventory of that endpoint,
    a proven occurrence pair and an unchanged surviving occurrence. The
    default [Preserve] policy never emits a delete action. [Propagate_remote]
    propagates remote disappearance to local, while [Propagate_local]
    propagates local disappearance to remote. A retained local absence is
    never propagated. A [Delete_*] result
    is only a plan: the driver must also check that the missing endpoint has
    the expected durable absence tombstone, then journal and verify the mutation before
    committing a tombstone. [\Deleted] alone is not absence. *)

val plan_disappearance_with_grace :
  absence_mature:bool -> policy:deletion_policy -> paired:bool ->
  remote_present:bool -> remote_complete:bool ->
  local_present:bool -> local_complete:bool ->
  local_retained:bool -> survivor_unchanged:bool -> deletion_plan
(** The same decision with an additional gate: when [absence_mature=false],
    an otherwise allowed deletion is held as [Grace_period]. *)
