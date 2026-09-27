(** Offline candidate plans for a sync cycle.

    A plan reads the latest complete published remote inventory and a fresh
    staged Maildir inventory. It does not connect to IMAP or change durable
    state, so it cannot check current server capabilities, survivor bytes
    or flags, or concurrent changes. {!Bridge.copy_once} refreshes the
    inventory and revalidates every action before it mutates anything. *)

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
          pair, [`Stale_epoch] for a pair from an earlier UIDVALIDITY, and
          the {!Deletion.plan} decision otherwise. A saved content or
          identity conflict plans [Hold_deletion Survivor_changed]. *)
}

type sync_preview =
  | Preview_pending of string
      (** [Preview_pending id] is a pending journal operation, which stops
          the plan as it stops a cycle. *)
  | Preview_bootstrap_hold
      (** [Preview_bootstrap_hold] is an unsafe populated bootstrap, which
          stops the plan. *)
  | Preview_copy_remote of Imap.Uid.t
  | Preview_copy_local of string
  | Preview_flags of {
      pair_id : string;
      to_remote : Imap.Sync_policy.flag_delta;
      to_local : Imap.Sync_policy.flag_delta;
    }
  | Preview_pair_hold of string * string
      (** [Preview_pair_hold (pair_id, reason)] is a pair a cycle would
          hold. *)
  | Preview_deletion of deletion_preview
      (** [Preview_deletion d] is a pair with one side present in the
          current epoch, or a pair of an earlier epoch whose local side is
          present. *)

val preview_sync :
  ?allow_bootstrap_duplicates:bool -> ?min_absence_scans:int ->
  store:Imap_store.t -> maildir:Maildir.t -> scope:Imap.Mirror.scope ->
  policy:Imap.Sync_policy.deletion_policy -> spool_dir:_ Eio.Path.t ->
  on_preview:(sync_preview -> unit) -> unit ->
  (Imap.Mirror.cursor, Error.t) result
(** [preview_sync ~store ~maildir ~scope ~policy ~spool_dir ~on_preview ()]
    streams to [on_preview] the copies, paired flag changes, holds and
    one-sided deletions a cycle under [policy] would consider, and is the
    published cursor it planned from. It holds the Maildir writer lease and
    pages both inventories and the pairs, so memory is bounded by the
    caller's output. A pending journal operation or an unsafe populated
    bootstrap stops the plan, as in {!Bridge.copy_once}, and
    [allow_bootstrap_duplicates] defaults to [false]. Saved content
    conflicts appear as pair holds and suppress flag candidates for those
    pairs. [min_absence_scans] defaults to 0, and a negative value returns
    [Invalid_configuration]. The local inventory is staged in [spool_dir],
    which must be a directory. A busy lease returns [Writer_busy], a missing
    complete inventory [Invalid_configuration], and a publication during
    the plan [Store_stale_revision]. *)
