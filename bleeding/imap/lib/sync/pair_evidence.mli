(** Checks that tie an endpoint observation or a journal operation to its
    pair, shared by the sync drivers and the operator repairs.

    This module is private to [imap.sync]. *)

val check_evidence : string -> (unit, Error.t) result
(** [check_evidence s] is [Ok ()] when [s] is 1 to 1024 printable bytes that
    are not all spaces, and [Invalid_configuration] otherwise. *)

val require_spool_dir : _ Eio.Path.t -> (unit, Error.t) result
(** [require_spool_dir p] is [Ok ()] when [p] is a directory, and
    [Invalid_configuration] otherwise. *)

val with_lease :
  Maildir.t -> (Maildir.writer -> ('a, Error.t) result) ->
  ('a, Error.t) result
(** [with_lease maildir f] is [f writer] under the Maildir writer lease, or
    [Writer_busy] when another process holds it. A busy lease raised inside
    [f] propagates. *)

val maildir_result : ('a, Maildir.error) result -> ('a, Error.t) result
(** [maildir_result r] is [r] with a Maildir error as [Maildir]. *)

val find : Maildir.t -> id:string -> (Maildir.occurrence option, Error.t) result
(** [find maildir ~id] is {!Maildir.find} with a Maildir error as
    [Maildir]. *)

val present : Maildir.t -> id:string -> (bool, Error.t) result
(** [present maildir ~id] is [true] when [find] finds [id]. *)

val with_inventory :
  spool_dir:_ Eio.Path.t -> Maildir.t ->
  (Local_inventory.t -> ('a, Error.t) result) -> ('a, Error.t) result
(** [with_inventory ~spool_dir maildir f] is {!Local_inventory.with_pages}
    with a Maildir error as [Maildir]. *)

val storable :
  Maildir.writer -> flags:Mail_flag.Imap_flag.t list ->
  Imap.Internal_date.t -> (float, string) result
(** [storable writer ~flags date] is the mtime a Maildir append of [flags]
    and [date] would use, or the reason Maildir cannot store them. *)

val local_date : Maildir.occurrence -> (Imap.Internal_date.t, Error.t) result
(** [local_date o] is the INTERNALDATE of [o]'s mtime, or
    [Invalid_operation] when it cannot be represented. *)

val local_content :
  ?inventory:Local_inventory.t -> Maildir.t -> Imap_store.Journal.pair ->
  Maildir.occurrence -> [ `Matches | `Differs | `Changed ]
(** [local_content maildir pair o] is [`Matches] when [o] has the pair's
    saved length and SHA-256 digest, [`Differs] when it has other bytes or
    the pair saved no digest, and [`Changed] when [o] is no longer the
    current occurrence. [inventory] is checked as {!Local_inventory} does. *)

val local_date_matches : Imap_store.Journal.pair -> Maildir.occurrence -> bool
(** [local_date_matches pair o] is [true] when [pair] saved no INTERNALDATE
    or [o]'s mtime is the same instant. *)

val current_pair :
  Imap_store.t -> Imap_store.Journal.pair ->
  (Imap_store.Journal.pair, Error.t) result
(** [current_pair store pair] is [pair] when it is still the stored pair,
    [Missing_pair] when it is gone and [Stale_pair] when it changed. *)

val remote_absence_proven : Imap_store.Journal.pair -> bool
(** [remote_absence_proven pair] is [true] when a complete inventory
    recorded the remote side's absence with its generation. *)

val local_absence_recorded : Imap_store.Journal.pair -> bool
(** [local_absence_recorded pair] is [true] when a complete local
    inventory recorded the local side's absence. *)

val commit_tombstones :
  Imap_store.t -> Imap_store.Journal.pair -> id:string ->
  remote_tombstone:Imap_store.Journal.tombstone option ->
  local_tombstone:Imap_store.Journal.tombstone option ->
  (Imap_store.Journal.pair, Error.t) result
(** [commit_tombstones store pair ~id ~remote_tombstone ~local_tombstone]
    commits operation [id] with [pair] carrying the given tombstones, and is
    [Stale_pair] when [pair] changed. *)

val finish_unlink :
  Imap_store.t -> Imap_store.Journal.pair -> id:string -> receipt:string ->
  (Imap_store.Journal.pair, Error.t) result
(** [finish_unlink store pair ~id ~receipt] observes local deletion [id]
    with [receipt] and commits it with an explicit local tombstone. *)

val finish_expunge :
  Imap_store.t -> Imap_store.Journal.pair -> id:string -> receipt:string ->
  (Imap_store.Journal.pair, Error.t) result
(** [finish_expunge store pair ~id ~receipt] observes remote deletion [id]
    with [receipt] and commits it with an expunge-receipt remote
    tombstone. *)

val describe : Imap_eio.Error.t -> string
(** [describe e] is the text of [e], cut to 512 bytes for a journal
    receipt. *)

val describe_error : Error.t -> string
(** [describe_error e] is the text of [e], cut to 512 bytes for a journal
    receipt. *)

val unchanged :
  ?inventory:Local_inventory.t -> Maildir.t -> Maildir.occurrence -> bool
(** [unchanged maildir o] is [true] when [o] is still the current
    occurrence. *)

val live_target :
  Imap_store.Journal.pair ->
  (Imap.Uidvalidity.t * Imap.Uid.t * string, Error.t) result
(** [live_target pair] is the epoch, UID and local ID of an untombstoned
    [pair], and [Missing_occurrence] otherwise. *)

val occurrence :
  ?inventory:Local_inventory.t -> Maildir.t -> string ->
  (Maildir.occurrence, Error.t) result
(** [occurrence maildir id] is the occurrence [id], read from [inventory]
    when given, and [Missing_occurrence] when it is absent. *)

val journaled_at :
  Imap_store.t -> Imap_store.Journal.operation -> Imap_store.Journal.pair ->
  bool
(** [journaled_at store op pair] is [true] when [op] was journaled against
    the current revision of [pair]. *)

val content_identity :
  Imap_store.Journal.pair ->
  (Imap.Uidvalidity.t * Imap.Uid.t * string * string * int64, Error.t)
    result
(** [content_identity pair] is the epoch, UID, local ID, digest and length
    of [pair], or [Identity_changed] when one is missing. *)

val same_target :
  Imap_store.Journal.operation -> epoch:Imap.Uidvalidity.t ->
  uid:Imap.Uid.t -> local_id:string -> bool
(** [same_target op ~epoch ~uid ~local_id] is [true] when [op] names that
    remote and local occurrence. *)

val same_identity :
  Imap_store.Journal.operation -> Imap_store.Journal.pair ->
  epoch:Imap.Uidvalidity.t -> uid:Imap.Uid.t -> local_id:string ->
  digest:string -> length:int64 -> bool
(** [same_identity op pair ~epoch ~uid ~local_id ~digest ~length] is
    [same_target] with the journaled digest, length and flags equal to the
    content and common flags of [pair]. *)

val operation_pair :
  Imap_store.t -> Imap_store.Journal.operation ->
  (Imap_store.Journal.pair, Error.t) result
(** [operation_pair store op] is the pair [op] names, [Missing_pair] when
    it has none or it is gone, and [Stale_pair] when it is in another
    scope. *)

val enable_object_identity :
  ctx:Ctx.t -> (Imap_eio.Client.Objectid_plus.t option, Error.t) result
(** [enable_object_identity ~ctx] enables OBJECTID+ when the server offers
    it, checks a saved binding of [ctx.scope] against [ctx.mailbox] and pins
    it, and is the enabled witness or [None] when OBJECTID+ is not offered
    and nothing is bound. A saved binding that cannot be verified or no
    longer matches is [Invalid_scope]. *)

val guard_bound_mailbox : ctx:Ctx.t -> (unit, Error.t) result
(** [guard_bound_mailbox ~ctx] is [enable_object_identity] when [ctx.scope]
    has a saved binding and [Ok ()] otherwise. It runs before any repair
    mutation. *)

val verify_mutation_destination : ctx:Ctx.t -> (unit, Error.t) result
(** [verify_mutation_destination ~ctx] checks with STATUS that a saved
    binding of [ctx.scope] still names [ctx.mailbox] and that OBJECTID+ is
    enabled, and is [Invalid_scope] otherwise. An unbound scope needs no
    check. *)

val with_selected :
  Ctx.t -> mode:[ `Read_only | `Read_write ] ->
  (Imap_eio.Selected.t -> ('a, Error.t) result) -> ('a, Error.t) result
(** [with_selected ctx ~mode f] is [f] on a selection of [ctx.mailbox], with
    a selection failure as [Client]. *)

val remote_flags :
  Imap_eio.Selected.t -> uid:Imap.Uid.t -> modseq:bool ->
  ((Mail_flag.Imap_flag.t list * int64 option) option, Error.t) result
(** [remote_flags selected ~uid ~modseq] is the durable flags of [uid] and,
    when [modseq] is [true], its MODSEQ, or [None] when no row with FLAGS
    came back. *)

val epoch_flags :
  Imap_eio.Selected.t -> epoch:Imap.Uidvalidity.t -> uid:Imap.Uid.t ->
  modseq:bool ->
  ((Mail_flag.Imap_flag.t list * int64 option) option, Error.t) result
(** [epoch_flags selected ~epoch ~uid ~modseq] is [remote_flags] after
    checking that the selection is in [epoch], and [Stale_inventory]
    otherwise. *)

val remote_flags_now :
  Ctx.t -> epoch:Imap.Uidvalidity.t -> uid:Imap.Uid.t -> modseq:bool ->
  (Mail_flag.Imap_flag.t list * int64 option, Error.t) result
(** [remote_flags_now ctx ~epoch ~uid ~modseq] is [remote_flags] from a
    read-only selection. Another epoch is [Uidvalidity_changed] and a
    missing UID [Missing_occurrence]. *)

val remote_absent :
  Ctx.t -> epoch:Imap.Uidvalidity.t -> uid:Imap.Uid.t -> (bool, Error.t) result
(** [remote_absent ctx ~epoch ~uid] is [true] when a read-only selection in
    [epoch] has no row for [uid]. *)

val remote_flags_and_date :
  Ctx.t -> uid:Imap.Uid.t -> uidvalidity:Imap.Uidvalidity.t ->
  (Mail_flag.Imap_flag.t list * Imap.Internal_date.t, Error.t) result
(** [remote_flags_and_date ctx ~uid ~uidvalidity] is the durable flags and
    INTERNALDATE of [uid] from a read-only selection. Another epoch is
    [Uidvalidity_changed] and a missing UID [Source_vanished]. *)

val stable_remote_body :
  ?precheck:(Imap.Response.select_metadata -> (unit, Error.t) result) ->
  Ctx.t -> mode:[ `Read_only | `Read_write ] -> spool:_ Eio.Path.t ->
  epoch:Imap.Uidvalidity.t -> uid:Imap.Uid.t -> digest:string ->
  length:int64 -> expected:Mail_flag.Imap_flag.t list ->
  ([ `Absent | `Changed | `Unchanged of int64 option ], Error.t) result
(** [stable_remote_body ctx ~mode ~spool ~epoch ~uid ~digest ~length
    ~expected] reads the flags and MODSEQ of [uid], fetches its body into
    [spool] and reads them again in one selection, then hashes the body
    after the selection ends. It is [`Unchanged modseq] when both reads
    show [expected] and the same MODSEQ and the body has [digest] and
    [length], [`Absent] when the UID is gone and [`Changed] otherwise.
    [precheck] runs on the SELECT response first and defaults to
    accepting it. The rules of {!Spool.with_spool} apply to [spool]. *)

val published_presence :
  Imap_store.t -> cursor:Imap.Mirror.cursor -> Imap_store.Journal.pair ->
  Imap.Uidvalidity.t -> Imap.Uid.t -> (bool, Error.t) result
(** [published_presence store ~cursor pair epoch uid] is [true] when [uid]
    is in the complete inventory [cursor] published for [epoch], and
    [Stale_inventory] when [cursor] is not a complete inventory of [pair]'s
    scope in that epoch or is no longer current. *)

val snapshot_has_uid :
  Imap_store.t -> scope:Imap.Mirror.scope -> cursor:Imap.Mirror.cursor ->
  Imap.Uid.t -> (bool, Error.t) result
(** [snapshot_has_uid store ~scope ~cursor uid] is
    {!Imap_store.snapshot_contains_uid} with a stale cursor as
    [Store_stale_revision]. *)

val snapshot_row :
  Imap_store.t -> scope:Imap.Mirror.scope -> cursor:Imap.Mirror.cursor ->
  Imap.Uid.t -> (Imap.Mirror.row option, Error.t) result
(** [snapshot_row store ~scope ~cursor uid] is the published row of [uid],
    with a stale cursor as [Store_stale_revision]. *)

val new_pair :
  id:string -> scope:Imap.Mirror.scope -> uidvalidity:Imap.Uidvalidity.t ->
  uid:Imap.Uid.t -> local_id:string -> sha256:string -> length:int64 ->
  ?internal_date:Imap.Internal_date.t -> flags:Mail_flag.Imap_flag.t list ->
  unit -> Imap_store.Journal.pair
(** [new_pair ~id ~scope ~uidvalidity ~uid ~local_id ~sha256 ~length ~flags
    ()] is an untombstoned pair at revision 0 with durable [flags] as its
    common flags. *)

val commit_new_pair :
  Imap_store.t -> id:string -> Imap_store.Journal.pair -> (unit, Error.t) result
(** [commit_new_pair store ~id pair] commits operation [id] with the new
    [pair], and is [Store_stale_revision] when the pair already exists. *)
