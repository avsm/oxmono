(** Durable pairs, conflicts and operations for the bidirectional driver,
    documented in [Imap_store.Journal]. *)

type t = Database.t

type tombstone_reason = Inventory_absence | Expunge_receipt
  | Local_absence | Explicit_delete | Retention

type tombstone = {
  reason : tombstone_reason;
  evidence : string;
  generation : int64 option;
}

type pair = {
  id : string;
  scope : Imap.Mirror.scope;
  remote_uidvalidity : Imap.Uidvalidity.t option;
  remote_uid : Imap.Uid.t option;
  local_id : string option;
  content_sha256 : string option;
  content_length : int64 option;
  internal_date : Imap.Internal_date.t option;
  common_flags : Mail_flag.Imap_flag.t list;
  remote_tombstone : tombstone option;
  local_tombstone : tombstone option;
  revision : int64;
}

val put_pair : t -> expected_revision:int64 option -> pair ->
  [ `Committed of pair | `Stale_revision ]
(** [put_pair t ~expected_revision pair] creates [pair] with [None] or
    compare-and-swaps it at [Some revision], and raises [Invalid_argument] for a
    change to an immutable identity or a weaker tombstone. *)

val find_pair : t -> id:string -> pair option
val note_presence : t -> pair:pair -> side:[ `Remote | `Local ] ->
  generation:int64 -> [ `Recorded | `Stale_revision ]
(** [note_presence t ~pair ~side ~generation] records that the complete
    published scan at [generation] saw [side] of [pair] present, or is
    [`Stale_revision] once a later publication replaced [generation]. *)

val last_presence_generation : t -> pair_id:string ->
  side:[ `Remote | `Local ] -> int64 option
val reactivate_local : t -> pair:pair -> generation:int64 ->
  [ `Reactivated of pair | `Stale_revision ]
(** [reactivate_local t ~pair ~generation] clears a [Local_absence] tombstone
    after the caller has verified the same occurrence in a complete local
    inventory. *)

val find_remote : t -> scope:Imap.Mirror.scope ->
  uidvalidity:Imap.Uidvalidity.t -> uid:Imap.Uid.t -> pair option
val find_local : t -> scope:Imap.Mirror.scope -> local_id:string -> pair option
val pairs : t -> scope:Imap.Mirror.scope -> pair list
val pairs_page : t -> scope:Imap.Mirror.scope -> ?after:string ->
  limit:int -> unit -> pair list

type conflict_kind = Flag_conflict | Identity_conflict | Content_conflict
  | Delete_conflict | Policy_conflict | Deletion_hold

type conflict = {
  id : string; pair_id : string; kind : conflict_kind;
  evidence : string; pair_revision : int64; resolved : bool;
}

val record_conflict : t -> conflict -> unit
val ensure_open_conflict : t -> pair:pair -> kind:conflict_kind ->
  id:string -> evidence:string -> [ `Open of conflict | `Stale_revision ]
(** [ensure_open_conflict t ~pair ~kind ~id ~evidence] creates or updates the
    single open conflict of [kind] for [pair]. *)

val resolve_open_conflicts : t -> pair:pair -> kind:conflict_kind ->
  [ `Resolved of int | `Stale_revision ]
val has_open_conflict : t -> pair:pair -> kind:conflict_kind -> bool
val resolve_conflict : t -> id:string -> unit
val open_conflicts : t -> scope:Imap.Mirror.scope -> conflict list
val open_conflicts_page : t -> scope:Imap.Mirror.scope -> ?after:string ->
  limit:int -> unit -> conflict list

type operation_kind = Append | Local_append | Copy | Move | Flags
  | Delete | Local_delete

type operation_state = Prepared | Sent | Ambiguous | Observed
  | Committed | Rejected

type operation = {
  id : string;
  pair_id : string option;
  local_id : string option;
  scope : Imap.Mirror.scope;
  kind : operation_kind;
  state : operation_state;
  source_uidvalidity : Imap.Uidvalidity.t option;
  source_uid : Imap.Uid.t option;
  destination : Imap.Mirror.scope option;
  destination_uidvalidity : Imap.Uidvalidity.t option;
  blob_sha256 : string option;
  blob_length : int64 option;
  desired_flags : Mail_flag.Imap_flag.t list option;
  receipt : string option;
  receipt_uidvalidity : Imap.Uidvalidity.t option;
  receipt_uid : Imap.Uid.t option;
}

val prepare_operation : ?local_flags:Mail_flag.Imap_flag.t list ->
  ?local_source_mtime:float ->
  ?source_internal_date:Imap.Internal_date.t ->
  t -> operation -> unit
(** [prepare_operation t op] journals the [Prepared] operation [op] and its
    source preimages before dispatch, and raises [Invalid_argument] for evidence
    that contradicts the pair. *)

val operation_source_mtime : t -> id:string -> float option
val operation_source_date : t -> id:string -> Imap.Internal_date.t option
val local_flags_preimage : t -> id:string ->
  Mail_flag.Imap_flag.t list option
val operation_pair_revision : t -> id:string -> int64 option
val mark_sent : t -> id:string -> unit
val mark_ambiguous : ?reason:string -> t -> id:string -> unit
(** [mark_ambiguous t ~id] records an uncertain outcome, which a mutation proven
    unsent must never take. *)

val reject_operation : t -> id:string -> receipt:string -> unit
val reject_prepared_operation : t -> id:string -> receipt:string -> unit
val observe_operation : t -> id:string -> receipt:string ->
  destination_uidvalidity:Imap.Uidvalidity.t option ->
  destination_uid:Imap.Uid.t option -> unit
val commit_operation : t -> id:string -> unit
(** [commit_operation t ~id] commits an observed operation that has no pair. *)

val commit_operation_with_pair : t -> id:string ->
  expected_pair_revision:int64 option -> pair ->
  [ `Committed of pair | `Stale_revision ]
(** [commit_operation_with_pair t ~id ~expected_pair_revision pair] commits an
    observed operation and publishes [pair] in one transaction. *)

val settle_flag_operation : t -> id:string -> pair ->
  flags:Mail_flag.Imap_flag.t list -> evidence:string ->
  [ `Settled of pair | `Stale_revision | `Invalid_operation ]
(** [settle_flag_operation t ~id pair ~flags ~evidence] is the operator repair
    that replaces the common flags of [pair] and rejects its uncertain FLAGS
    operation. *)

val reject_unchanged_delete_operation : t -> id:string -> pair ->
  evidence:string ->
  [ `Rejected | `Stale_revision | `Invalid_operation ]
(** [reject_unchanged_delete_operation t ~id pair ~evidence] rejects an
    uncertain remote DELETE whose target the caller has verified unchanged. *)

val attest_targeted_expunge : t -> id:string -> pair ->
  evidence:string ->
  [ `Attested | `Stale_revision | `Invalid_operation ]
(** [attest_targeted_expunge t ~id pair ~evidence] records operator
    authorization for a targeted UID EXPUNGE of an uncertain DELETE. *)

val find_operation : t -> id:string -> operation option
val active_operations : t -> scope:Imap.Mirror.scope -> operation list
val active_operations_page : t -> scope:Imap.Mirror.scope ->
  ?after:string -> limit:int -> unit -> operation list
val active_operation_for_pair : t -> pair_id:string -> operation option
