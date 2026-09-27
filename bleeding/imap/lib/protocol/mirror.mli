(** Pure, storage-independent mailbox reconciliation.

    [complete] only accepts a full finite inventory with a completed command
    receipt. It prepares a replacement snapshot; [publish] then produces the
    cursor and typed delta to commit atomically with that snapshot. Until that
    transaction commits, the old cursor and published snapshot remain valid.
    This is a baseline full-inventory planner, not a QRESYNC implementation.
    Large mailboxes should stage rows outside this in-memory reference model. *)

type scope = {
  endpoint : string;
  account : string;
  mailbox_key : string;
  raw_name : string;
  encoding : Mailbox_name.mode;
  mailbox_id : string option;
}

type mode = Baseline | Condstore
type phase = New | Live
type restart_reason = Uidvalidity_changed | Modseq_regressed | Nomodseq
type error =
  | Invalid of string
  | Stale_revision
  | Wrong_action
  | Incomplete_coverage
  | Modseq_regression

type cursor = private {
  schema_version : int;
  scope : scope;
  phase : phase;
  uidvalidity : Proto.Uidvalidity.t option;
  generation : int64;
  revision : int64;
  anchor : Proto.Modseq.t option;
  frontier : int64;
  inventory_ref : string option;
  mode : mode;
}

val initial : scope -> cursor
(** A new mailbox cursor. Empty scope components are rejected. *)

val restore : schema_version:int -> scope:scope -> phase:phase ->
  uidvalidity:Proto.Uidvalidity.t option -> generation:int64 ->
  revision:int64 -> anchor:Proto.Modseq.t option -> frontier:int64 ->
  inventory_ref:string option -> mode:mode -> (cursor, error) result
(** Validate a persisted cursor before using it for reconciliation. The caller
    must also load and validate its snapshot in the same storage transaction. *)

type selected = {
  uidvalidity : Proto.Uidvalidity.t;
  uidnext : int64;
  highestmodseq : Proto.Modseq.t option;
  nomodseq : bool;
}

type action = private {
  id : string;
  scope : scope;
  expected_revision : int64;
  expected_generation : int64;
  uidvalidity : Proto.Uidvalidity.t;
  upper_uid : int64;
  previous_anchor : Proto.Modseq.t option;
  mode : mode;
  restart : restart_reason option;
}

val plan : cursor -> stage_id:string -> selected -> (action, error) result
(** Fixes the finite [UIDNEXT - 1] upper bound. Every action performs a full
    inventory, including when counts and UIDNEXT appear unchanged. *)

type row = {
  uid : Proto.Uid.t;
  flags : Mail_flag.Imap_flag.t list;
  modseq : Proto.Modseq.t option;
}

type snapshot
val snapshot : uidvalidity:Proto.Uidvalidity.t -> row list ->
  (snapshot, error) result
val rows : snapshot -> row list
val snapshot_uidvalidity : snapshot -> Proto.Uidvalidity.t

type completed = {
  action_id : string;
  uidvalidity : Proto.Uidvalidity.t;
  covered_upper : int64;
  inventory_complete : bool;
  commands_complete : bool;
  rows : row list;
  explicit_highestmodseq : Proto.Modseq.t option;
  nomodseq : bool;
}

type staged
val complete : cursor -> action -> completed -> (staged, error) result
(** Rejects interrupted or mismatched work. The returned value is provisional:
    callers may discard it after a crash without advancing a checkpoint. *)

type flag_change = { before : row; after : row }
type transition = {
  cursor : cursor;
  snapshot : snapshot;
  added : row list;
  changed : flag_change list;
  removed : Proto.Uid.t list;
  invalidated_epoch : bool;
  restart : restart_reason option;
  stage_id : string;
  more : bool;
}

val publish : cursor -> published:snapshot option -> staged ->
  (transition, error) result
(** Apply [transition] under a revision check in one store transaction: replace
    the snapshot, install the new cursor and revision, and expose deltas together.
    [removed] is empty on UIDVALIDITY changes; the old epoch is quarantined. *)
