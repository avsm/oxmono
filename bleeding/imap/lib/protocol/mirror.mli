(** Pure mailbox cursor planning.

    A cursor records the last complete inventory published for a mailbox
    scope. [plan] fixes the UID range and mode of the next scan from the
    cursor and the SELECT response. [Imap_store] stages the scanned rows
    and publishes the next cursor with them in one transaction. *)

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
type error = Invalid of string

type cursor = private {
  schema_version : int;
  scope : scope;
  phase : phase;
  uidvalidity : Uidvalidity.t option;
  generation : int64;
  revision : int64;
  anchor : Modseq.t option;
  frontier : int64;
  inventory_ref : string option;
  mode : mode;
}

val initial : scope -> cursor
(** [initial scope] is a new cursor for [scope].

    @raise Invalid_argument if [endpoint], [account], [mailbox_key] or
    [raw_name] is empty. *)

val restore : schema_version:int -> scope:scope -> phase:phase ->
  uidvalidity:Uidvalidity.t option -> generation:int64 ->
  revision:int64 -> anchor:Modseq.t option -> frontier:int64 ->
  inventory_ref:string option -> mode:mode -> (cursor, error) result
(** Validate a persisted cursor before using it for reconciliation. The caller
    must also load and validate its snapshot in the same storage transaction. *)

type selected = {
  uidvalidity : Uidvalidity.t;
  uidnext : int64;
  highestmodseq : Modseq.t option;
  nomodseq : bool;
}

type action = private {
  id : string;
  scope : scope;
  expected_revision : int64;
  expected_generation : int64;
  uidvalidity : Uidvalidity.t;
  upper_uid : int64;
  previous_anchor : Modseq.t option;
  mode : mode;
  restart : restart_reason option;
}

val plan : cursor -> stage_id:string -> selected -> (action, error) result
(** Fixes the finite [UIDNEXT - 1] upper bound. Every action performs a full
    inventory, including when counts and UIDNEXT appear unchanged. *)

type row = {
  uid : Uid.t;
  flags : Mail_flag.Imap_flag.t list;
  modseq : Modseq.t option;
}

type snapshot
val snapshot : uidvalidity:Uidvalidity.t -> row list ->
  (snapshot, error) result
val rows : snapshot -> row list
val snapshot_uidvalidity : snapshot -> Uidvalidity.t
