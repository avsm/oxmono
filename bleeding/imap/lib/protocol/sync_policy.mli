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
}

type error = Deleted_flag_requires_policy

val reconcile_flags :
  ?propagate_deleted:bool ->
  base:Mail_flag.Imap_flag.t list ->
  remote:Mail_flag.Imap_flag.t list ->
  local:Mail_flag.Imap_flag.t list ->
  unit -> (flag_plan, error) result
(** Three-way merge from the last common flag set. Adds and removals on
    either side are preserved. Flags compare case-insensitively as IMAP flags;
    where spellings differ, the remote spelling wins. Session-only [\Recent]
    is ignored. A changed [\Deleted] is held for explicit policy by default;
    it is a message flag, not proof of expunge or user deletion. Deltas are
    relative to each observed endpoint and should be sent as conditional
    additions/removals, then verified before persisting [merged] as the new
    common state. *)

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
type deletion_plan =
  | No_deletion
  | Hold_deletion of deletion_hold
  | Delete_remote
  | Delete_local

val absence_mature : last_present_generation:int64 option -> current_generation:int64 ->
  first_generation:int64 option -> min_scans:int -> bool
(** Require [min_scans] complete scan generations after the first durable
    absence observation. A missing legacy first-generation holds when the
    grace period is enabled. A later complete presence observation supersedes
    the old absence even with zero grace until a new absence is published. *)

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
