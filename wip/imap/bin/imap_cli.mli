(** The [imap-sync] command line.

    Each command parses into a {!job} that holds only validated options. A
    job holds no password. A command that connects reads the password from
    the environment variable its [password_env] names when it runs, and an
    offline command never reads it. *)

type scope = private {
  endpoint : string;
      (** [endpoint] is the stable identifier of the IMAP server. *)
  account : string;
      (** [account] is the stable identifier of the account. *)
  mailbox : string;  (** [mailbox] is the UTF-8 mailbox name. *)
  mailbox_key : string;
      (** [mailbox_key] is the stable identifier of the mailbox across
          renames. It defaults to [mailbox]. *)
  db : string;
      (** [db] is the path of the SQLite database. *)
}
(** The type for the mailbox and the database of its journal. *)

type connection = private {
  host : string;
  port : int option;
      (** [port] is [None] for 993 with implicit TLS and 143 otherwise. *)
  tls : Imap_eio.Transport.tls;
  user : string;
  password_env : string;
      (** [password_env] names the environment variable that holds the
          password. *)
  auth : Imap_eio.Auth.mechanism;
}
(** The type for where and how an online command authenticates. With [tls]
    [`Plain] the credential permits every mechanism over clear text, which
    is meant only for a trusted fixture. *)

type budget = private {
  max_body_bytes : int64;
      (** [max_body_bytes] bounds one body. *)
  max_total_bytes : int64;
      (** [max_total_bytes] bounds the bytes of the pass. *)
}
(** The type for the byte budget of one hydration pass. *)

type sync = private {
  scope : scope;
  connection : connection;
  blob_dir : string;
  maildir : string;
  spool_dir : string;
  max_transfers : int;
      (** [max_transfers] bounds the transfers of one cycle and the
          messages of the hydration pass. *)
  max_cycles : int;
  min_absence_scans : int;
  deletion_policy : Imap.Sync_policy.deletion_policy;
  allow_bootstrap_duplicates : bool;
  propagate_deleted : bool;
      (** [propagate_deleted] merges a changed [\\Deleted] flag like any
          other flag instead of holding it. It is [false] by default. *)
  hydrate_bodies : budget option;
      (** [hydrate_bodies] is the budget of the hydration pass after a
          converged cycle, or [None] for no hydration. *)
}
(** The type for the options of [sync]. *)

type hydrate = private {
  scope : scope;
  connection : connection;
  blob_dir : string;
  spool_dir : string;
  max_transfers : int;
      (** [max_transfers] bounds the messages of one pass. *)
  budget : budget;
}
(** The type for the options of [hydrate]. *)

type audit_cache = private {
  scope : scope;
  encoding : Imap.Mailbox_name.mode option;
      (** [encoding] is [None] when the stored encoding is looked up. *)
  blob_dir : string;
  max_transfers : int;
      (** [max_transfers] bounds the references of one pass. *)
  max_total_bytes : int64;
  continuation : (Imap.Uid.t * int64) option;
      (** [continuation] is the UID to continue after and the revision it
          expects. *)
}
(** The type for the options of [audit-cache]. *)

type inspect = private {
  scope : scope;
  encoding : Imap.Mailbox_name.mode option;
  max_inspect : int;
      (** [max_inspect] bounds the operations and the conflicts
          printed. *)
  operation_id : string option;
      (** [operation_id] is the one operation to print, or [None] for the
          active operations and open conflicts. *)
}
(** The type for the options of [inspect]. *)

type append_candidates = private {
  scope : scope;
  connection : connection;
  spool_dir : string;
  operation_id : string;
  max_inspect : int;
      (** [max_inspect] is the widest UID range inspected. *)
  max_candidate_bytes : int64;
      (** [max_candidate_bytes] bounds the body bytes read. *)
}
(** The type for the options of [inspect-append-candidates]. *)

type appenduid = private {
  scope : scope;
  encoding : Imap.Mailbox_name.mode option;
  maildir : string;
  operation_id : string;
  uidvalidity : Imap.Uidvalidity.t;
  uid : Imap.Uid.t;
  evidence : string;
}
(** The type for the options of [repair-appenduid]. *)

type repair = private {
  scope : scope;
  connection : connection;
  maildir : string;
  spool_dir : string;
  operation_id : string;
  evidence : string;
}
(** The type for the options shared by the online operator repairs. *)

type retention = private {
  scope : scope;
  encoding : Imap.Mailbox_name.mode option;
  maildir : string;
  spool_dir : string;
  pair_id : string;
  evidence : string;
}
(** The type for the options of [mark-local-retention]. *)

type plan = private {
  scope : scope;
  encoding : Imap.Mailbox_name.mode option;
  maildir : string;
  spool_dir : string;
  max_inspect : int;
      (** [max_inspect] bounds the events printed. *)
  min_absence_scans : int;
  deletion_policy : Imap.Sync_policy.deletion_policy;
  allow_bootstrap_duplicates : bool;
      (** [allow_bootstrap_duplicates] is always [true] for
          [plan-deletions], so an unpaired bootstrap does not stop it. *)
  propagate_deleted : bool;
      (** [propagate_deleted] previews a changed [\\Deleted] flag as a
          flag change instead of a hold. It is [false] by default. *)
}
(** The type for the options of [plan-deletions] and [plan-sync]. *)

type verify_local = private {
  scope : scope;
  encoding : Imap.Mailbox_name.mode option;
  maildir : string;
  spool_dir : string;
  max_inspect : int;
      (** [max_inspect] bounds the issues printed. *)
}
(** The type for the options of [verify-local]. *)

type gc = private {
  db : string;
  blob_dir : string;
  maildir : string option;
      (** [maildir] is the Maildir whose writer lease is also held, or
          [None] for no lease. *)
}
(** The type for the options of [gc]. *)

type forget_epochs = private {
  scope : scope;
  encoding : Imap.Mailbox_name.mode option;
}
(** The type for the options of [forget-epochs]. *)

type job = private
  | Sync of sync
  | Hydrate of hydrate
  | Audit_cache of audit_cache
  | Inspect of inspect
  | Inspect_append_candidates of append_candidates
  | Repair_appenduid of appenduid
  | Repair_local_delete of repair
  | Repair_local_append of { repair : repair; blob_dir : string }
  | Settle_flags of repair
  | Reject_remote_delete of repair
  | Finish_remote_delete of repair
  | Mark_local_retention of retention
  | Plan_deletions of plan
  | Plan_sync of plan
  | Verify_local of verify_local
  | Gc of gc
  | Forget_epochs of forget_epochs
(** The type for parsed commands. Each constructor is the command of the
    same name in lowercase with hyphens, such as [Repair_local_delete] for
    [repair-local-delete]. An unset directory option defaults to the
    database path with [.blobs] or [.spool] appended. *)

val cmd : job Cmdliner.Cmd.t
(** [cmd] is the [imap-sync] command group, with one subcommand per {!job}
    constructor. A subcommand accepts only the options it uses, and each
    option falls back to the [IMAP_*] environment variable its help names.
    Its exit statuses are those {!run} returns. *)

val run :
  job -> env:(string -> string option) -> net:_ Eio.Net.t ->
  fs:_ Eio.Path.t -> random:_ Eio.Flow.source -> int
(** [run job ~env ~net ~fs ~random] performs [job] and is its exit status.
    It resolves paths under [fs], connects through [net], draws operation
    and stage IDs from [random], and looks up the password of a command
    that connects with [env]. Every failure is printed on standard error
    with the password replaced by [\[REDACTED\]]. Cancellation propagates.
    The statuses are these.

    - 0 means the command converged or a targeted operation is terminal.
    - 2 means bounded work remains.
    - 3 means pending journal work needs inspection.
    - 4 means a conflict, a held change or an unsafe state.
    - 5 means invalid configuration.
    - 6 means an IMAP connection or protocol failure.
    - 7 means a local filesystem, Maildir or SQLite failure.
    - 8 means the Maildir writer lease, the Maildir metadata lock or the
      database lock is busy.
    - 9 means the targeted operation or pair is not in the scope. *)

val eval :
  ?help:Format.formatter -> ?err:Format.formatter ->
  env:(string -> string option) -> argv:string array -> net:_ Eio.Net.t ->
  fs:_ Eio.Path.t -> random:_ Eio.Flow.source -> unit -> int
(** [eval ~env ~argv ~net ~fs ~random ()] parses [argv], which includes the
    program name, with {!cmd}, looking up environment defaults with [env],
    and is the status of {!run} on the job with [net], [fs] and [random]. A
    command line error is 5 and a help request 0. Help goes to [help] and
    parse errors to [err], which default to standard output and standard
    error. *)
