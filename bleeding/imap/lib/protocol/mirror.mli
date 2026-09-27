@@ portable

(** Mailbox cursors and scan planning.

    A cursor records the last complete inventory published for a mailbox
    scope. {!plan} fixes the UID range and mode of the next scan from the
    cursor and the SELECT response. [Imap_store] stages the scanned rows
    and publishes the next cursor with them in one transaction. Every scan
    is a full inventory of its range. *)

(** {1 Scopes and cursors} *)

type scope = {
  endpoint : string;  (** The server, as the caller names it. *)
  account : string;  (** The account on [endpoint]. *)
  mailbox_key : string;  (** The caller's stable key for the mailbox. *)
  raw_name : string;  (** The mailbox name in its wire bytes. *)
  encoding : Mailbox_name.mode;  (** The encoding of [raw_name]. *)
  mailbox_id : string option;  (** The RFC 8474 MAILBOXID, when known. *)
}
(** The type for mirrored mailboxes. [endpoint], [account], [mailbox_key]
    and [raw_name] are nonempty in every cursor. *)

type mode =
  | Baseline  (** The inventory carries no usable MODSEQ values. *)
  | Condstore  (** The inventory carries per-message MODSEQ values. *)
(** The type for inventory modes. *)

type phase =
  | New  (** No inventory has been published. *)
  | Live  (** At least one inventory has been published. *)
(** The type for cursor phases. *)

type restart_reason =
  | Uidvalidity_changed
  | Modseq_regressed
      (** The HIGHESTMODSEQ fell below the cursor's anchor. *)
  | Nomodseq
      (** The mailbox reports NOMODSEQ while the cursor has an anchor. *)
(** The type for reasons a scan discards the cursor's anchor. *)

type error = Invalid of string  (** A message naming the failed check. *)
(** The type for planning errors. *)

type cursor = private {
  schema_version : int;  (** The cursor format, always 1. *)
  scope : scope;
  phase : phase;
  uidvalidity : Uidvalidity.t option;
      (** The UIDVALIDITY of the published inventory, [None] when [phase]
          is [New]. *)
  generation : int64;  (** The number of inventories published. *)
  revision : int64;  (** The concurrency revision, equal to [generation]. *)
  anchor : Modseq.t option;
      (** The explicit HIGHESTMODSEQ of the published inventory, [None] in
          [Baseline] mode. *)
  frontier : int64;
      (** The highest UID the published inventory covers, from 0 to
          4294967295. *)
  inventory_ref : string option;
      (** The staging identifier of the published inventory, [None] when
          [phase] is [New]. *)
  mode : mode;
}
(** The type for cursors. Every cursor passes the checks of {!restore}. *)

val initial : scope -> cursor
(** [initial scope] is the [New] cursor for [scope], with no inventory and
    every counter at 0.

    @raise Invalid_argument if [endpoint], [account], [mailbox_key] or
    [raw_name] of [scope] is empty. *)

val restore : schema_version:int -> scope:scope -> phase:phase ->
  uidvalidity:Uidvalidity.t option -> generation:int64 ->
  revision:int64 -> anchor:Modseq.t option -> frontier:int64 ->
  inventory_ref:string option -> mode:mode -> (cursor, error) result
(** [restore ~schema_version ~scope ~phase ~uidvalidity ~generation
    ~revision ~anchor ~frontier ~inventory_ref ~mode] is the persisted
    cursor with those fields. The error covers a [schema_version] other
    than 1, an empty component of [scope], a negative counter, a
    [frontier] above 4294967295, a [generation] different from [revision],
    an [anchor] in [Baseline] mode, a [New] cursor with a [uidvalidity],
    an [anchor], an [inventory_ref] or a nonzero counter, a [Live] cursor
    without a [uidvalidity], an [inventory_ref] or a nonzero [generation],
    and an empty [inventory_ref]. The caller must load and validate the
    matching snapshot in the same storage transaction. *)

(** {1 Planning} *)

type selected = {
  uidvalidity : Uidvalidity.t;
  uidnext : int64;
  highestmodseq : Modseq.t option;
  nomodseq : bool;  (** The SELECT response carried NOMODSEQ. *)
}
(** The type for the mailbox state a SELECT reports. *)

type action = private {
  id : string;  (** The staging identifier. *)
  scope : scope;
  expected_revision : int64;
      (** The cursor revision the publication must still find. *)
  expected_generation : int64;
  uidvalidity : Uidvalidity.t;
  upper_uid : int64;
      (** The highest UID the scan covers, [UIDNEXT - 1], which is 0 for a
          mailbox that has never held a message. *)
  previous_anchor : Modseq.t option;
      (** The anchor the published one must not fall below, [None] after a
          restart or in [Baseline] mode. *)
  mode : mode;
  restart : restart_reason option;
}
(** The type for planned scans. *)

val plan : cursor -> stage_id:string -> selected -> (action, error) result
(** [plan cursor ~stage_id selected] is the scan of UIDs 1 to
    [selected.uidnext - 1] that follows [cursor], staged under [stage_id].
    The scan is in [Condstore] mode when [selected] has a HIGHESTMODSEQ
    and no NOMODSEQ. It restarts, dropping the anchor, when the
    UIDVALIDITY changed, when NOMODSEQ appears while [cursor] has an
    anchor, or when the HIGHESTMODSEQ fell below the anchor, checked in
    that order. The error covers an empty [stage_id], a [uidnext] outside
    1 to 4294967296 and a cursor whose counters cannot advance. *)

(** {1 Inventories} *)

type row = {
  uid : Uid.t;
  flags : Mail_flag.Imap_flag.t list;
  modseq : Modseq.t option;
}
(** The type for inventory rows, one message's flags and MODSEQ. *)

type snapshot
(** The type for inventories of one UIDVALIDITY, with one row per UID. *)

val snapshot : uidvalidity:Uidvalidity.t -> row list ->
  (snapshot, error) result
(** [snapshot ~uidvalidity rows] is the inventory of [rows] under
    [uidvalidity]. The error covers a UID that appears twice in [rows]. *)

val rows : snapshot -> row list
(** [rows s] is the rows of [s] in ascending UID order. *)

val snapshot_uidvalidity : snapshot -> Uidvalidity.t
(** [snapshot_uidvalidity s] is the UIDVALIDITY of [s]. *)
