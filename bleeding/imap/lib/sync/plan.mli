(** Offline previews of a sync cycle.

    A preview reads the latest complete published remote inventory and a
    fresh staged Maildir inventory. It does not connect to IMAP or change
    the journal, so it cannot check server capabilities, survivor bytes or
    flags, or concurrent changes. {!Bridge.copy_once} refreshes the
    inventory and revalidates every action before it changes anything. *)

type deletion_preview = {
  pair_id : string;
  remote_uid : Imap.Uid.t;
  local_id : string;
  remote_present : bool option;
      (** [remote_present] is [None] when the pair is from an earlier
          UIDVALIDITY than the published inventory. *)
  local_present : bool;
  decision : [ `Pending of string | `Stale_epoch |
    `Plan of Imap.Sync_policy.deletion_plan ];
      (** [decision] is [`Pending id] when journal operation [id] holds the
          pair. Otherwise a saved content or identity conflict plans
          [Hold_deletion Survivor_changed], a pair from an earlier
          UIDVALIDITY is [`Stale_epoch], and any other pair has the
          {!Deletion.plan} decision. *)
}
(** The type for the deletion decision of a pair with one side absent. *)

type sync_preview =
  | Preview_pending of string
      (** [Preview_pending id] is a pending journal operation, which stops
          the preview as it stops a cycle. *)
  | Preview_bootstrap_hold
      (** [Preview_bootstrap_hold] is a first cycle with unpaired messages
          on both endpoints, which stops the preview. *)
  | Preview_copy_remote of Imap.Uid.t
      (** [Preview_copy_remote uid] is an unpaired remote message a cycle
          would copy to Maildir. *)
  | Preview_copy_local of string
      (** [Preview_copy_local id] is an unpaired Maildir occurrence a cycle
          would append to IMAP. *)
  | Preview_flags of {
      pair_id : string;
      to_remote : Imap.Sync_policy.flag_delta;
      to_local : Imap.Sync_policy.flag_delta;
    }
      (** [Preview_flags {pair_id; to_remote; to_local}] is the flag change
          a cycle would write to each endpoint of [pair_id]. *)
  | Preview_pair_hold of string * string
      (** [Preview_pair_hold (pair_id, reason)] is a pair a cycle would
          hold, for [reason]. A pair can be reported more than once. *)
  | Preview_deletion of deletion_preview
      (** [Preview_deletion d] is a pair with one side present in the
          current epoch, or a pair of an earlier epoch whose local side is
          present. *)
(** The type for the events of a preview. *)

val preview_sync :
  ?allow_bootstrap_duplicates:bool -> ?propagate_deleted:bool ->
  ?min_absence_scans:int ->
  store:Imap_store.t -> maildir:Maildir.t -> scope:Imap.Mirror.scope ->
  policy:Imap.Sync_policy.deletion_policy -> spool_dir:_ Eio.Path.t ->
  on_preview:(sync_preview -> unit) -> unit ->
  (Imap.Mirror.cursor, Error.t) result
(** [preview_sync ~store ~maildir ~scope ~policy ~spool_dir ~on_preview ()]
    calls [on_preview] with each copy, flag change, hold and one-sided
    deletion a cycle of [scope] in [store] and [maildir] under [policy]
    would consider, and is the published cursor it planned from. It takes
    the Maildir writer lease, stages the Maildir inventory in [spool_dir]
    and pages both inventories and the pairs, so memory is bounded by what
    [on_preview] keeps.

    A pending journal operation or a first cycle with unpaired messages on
    both endpoints is reported and ends the preview, as in
    {!Bridge.copy_once}. [allow_bootstrap_duplicates] defaults to [false].
    [propagate_deleted] defaults to [false], and a changed [\\Deleted] then
    appears as a pair hold, as in {!Bridge.copy_once}. A saved content
    conflict appears as a pair hold and suppresses the flag change of its
    pair. [min_absence_scans] defaults to 0.

    A negative [min_absence_scans], a [spool_dir] that is not a directory or
    a scope without a complete published inventory returns
    [Invalid_configuration]. A publication during the preview returns
    [Store_stale_revision]. *)
