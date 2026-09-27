(** Durable three-way flag reconciliation for one paired IMAP/Maildir occurrence.
    This module does not discover pairs or publish mailbox inventories. The
    caller must serialize writers for the Maildir and SQLite store. *)

type error =
  | Client of Imap_eio.Error.t
  | Missing_pair
  | Stale_pair
  | Missing_occurrence
  | Uidvalidity_changed
  | Deleted_flag_held
  | Conditional_store_unavailable
  | Permanent_flag_unavailable of Mail_flag.Imap_flag.t
  | Modified
  | Pending_operation of string
  | Content_mismatch of string
  | Diverged of string

val pp_error : Format.formatter -> error -> unit

type outcome = Unchanged | Updated of Imap_store.Sync.pair

type plan = No_change | Apply of Mail_flag.Imap_flag.t list

val plan_flags :
  ?propagate_deleted:bool -> base:Mail_flag.Imap_flag.t list ->
  remote:Mail_flag.Imap_flag.t list -> local:Mail_flag.Imap_flag.t list ->
  condstore:bool -> remote_modseq:int64 option ->
  unit -> (plan, error) result
(** Pure decision for the observed flags. Remote mutation requires a usable
    MODSEQ and conditional STORE support; local-only changes do not. *)

val validate_permanent_flags :
  available:string list option ->
  remote:Mail_flag.Imap_flag.t list ->
  merged:Mail_flag.Imap_flag.t list -> (unit, error) result
(** Pure SELECT PERMANENTFLAGS preflight. The special [\\*] permits adding
    new keywords; removing a flag still requires its explicit listing. *)

val reconcile_pair :
  ?propagate_deleted:bool -> client:Imap_eio.Client.t ->
  store:Imap_store.t -> maildir:Imap_maildir.t -> mailbox:string ->
  pair:Imap_store.Sync.pair -> next_id:(unit -> string) ->
  unit -> (outcome, error) result
(** Fetch current UID FLAGS/MODSEQ and Maildir flags, merge against
    [pair.common_flags], and journal an intent before changing either side.
    Remote writes use UID STORE FLAGS with UNCHANGEDSINCE; if a remote write
    is needed without CONDSTORE/QRESYNC and a usable message MODSEQ, return
    [Conditional_store_unavailable]. The driver also refuses remote changes
    that SELECT's PERMANENTFLAGS does not permit. The default holds changes
    to [\\Deleted].
    The local occurrence must match the pair's saved body digest and length
    before dispatch. A mismatch creates a durable [Content_conflict] without
    creating a FLAGS operation. Its content is checked again around the local
    flag update, so a concurrent replacement leaves the journal pending
    instead of advancing the common baseline.
    A successful call verifies both endpoints before atomically committing
    the new pair revision and operation. A mutation with uncertain outcome
    remains pending with a durable [Flag_conflict] record describing verified
    divergence; callers must call [recover_operation] or investigate it
    before issuing a new write for this pair. Store/Maildir I/O exceptions and
    Eio cancellation propagate. The caller must hold the Maildir writer
    lease across the operation; [Bridge.copy_once] does this. *)

val recover_operation :
  client:Imap_eio.Client.t -> store:Imap_store.t ->
  maildir:Imap_maildir.t -> mailbox:string ->
  operation:Imap_store.Sync.operation ->
  (outcome, error) result
(** Verify the saved pair revision, UIDVALIDITY, remote target and paired
    local body hash/length. If local flags are still the saved preimage,
    finish the local write and commit; if both sides already match, just
    commit. Never replay an uncertain remote STORE. Divergence, missing
    preimage or missing content evidence holds the operation for repair and
    records a stable-ID flag conflict. A changed local body is held before
    any remote read, and repeated recovery preserves the conflict ID. A
    verified pair commit resolves that
    conflict atomically with the operation and new common flags.
    Prepared intents can be rejected because dispatch has not begun. The
    caller must hold the Maildir writer lease across recovery. *)

val settle_operation :
  client:Imap_eio.Client.t -> store:Imap_store.t ->
  maildir:Imap_maildir.t -> scope:Imap.Mirror.scope -> mailbox:string ->
  id:string -> evidence:string -> unit -> (outcome, error) result
(** Explicit operator repair of a sent, ambiguous or observed FLAGS intent
    after both endpoints have been manually brought to the same flags. Holds
    the Maildir writer lease, verifies the operation and pair revision,
    OBJECTID+ binding when present, UIDVALIDITY, paired local content and
    date, and stable remote MODSEQ across two reads. It sends no STORE and
    changes no Maildir flags. An atomic SQLite transition adopts the agreed
    flags as common, rejects the superseded intent with operator evidence,
    and resolves its flag conflict. Divergence leaves all state pending. *)
