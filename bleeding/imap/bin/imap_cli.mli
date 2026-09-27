(** The [imap-sync] command line.

    Each command parses into a {!job} that holds only validated options. A
    job holds no password. A command that connects reads the password from
    the environment variable its [password_env] names when it runs. *)

type scope = private {
  endpoint : string;
  account : string;
  mailbox : string;
  mailbox_key : string;
      (** [mailbox_key] defaults to [mailbox]. *)
  db : string;
}
(** A [scope] names the mailbox and the SQLite database of its journal. *)

type connection = private {
  host : string;
  port : int option;
  tls : Imap_eio.Transport.tls;
  user : string;
  password_env : string;
  auth : Imap_eio.Auth.mechanism;
}
(** A [connection] is where and how an online command authenticates. With
    [tls] [`Plain] the credential permits every mechanism over clear
    text, which is meant only for a trusted fixture. *)

type budget = private { max_body_bytes : int64; max_total_bytes : int64 }
(** A [budget] bounds one hydration pass. *)

type sync = private {
  scope : scope;
  connection : connection;
  blob_dir : string;
  maildir : string;
  spool_dir : string;
  max_transfers : int;
  max_cycles : int;
  min_absence_scans : int;
  deletion_policy : Imap.Sync_policy.deletion_policy;
  allow_bootstrap_duplicates : bool;
  hydrate_bodies : budget option;
      (** [hydrate_bodies] is the budget of the hydration run after a
          converged cycle, or [None] for no hydration. *)
}

type hydrate = private {
  scope : scope;
  connection : connection;
  blob_dir : string;
  spool_dir : string;
  max_transfers : int;
  budget : budget;
}

type audit_cache = private {
  scope : scope;
  encoding : Imap.Mailbox_name.mode option;
      (** [encoding] is [None] when the stored encoding is looked up. *)
  blob_dir : string;
  max_transfers : int;
  max_total_bytes : int64;
  continuation : (Imap.Uid.t * int64) option;
      (** [continuation] is the UID to continue after and the revision it
          expects. *)
}

type inspect = private {
  scope : scope;
  encoding : Imap.Mailbox_name.mode option;
  max_inspect : int;
  operation_id : string option;
}

type append_candidates = private {
  scope : scope;
  connection : connection;
  spool_dir : string;
  operation_id : string;
  max_inspect : int;
  max_candidate_bytes : int64;
}

type appenduid = private {
  scope : scope;
  encoding : Imap.Mailbox_name.mode option;
  maildir : string;
  operation_id : string;
  uidvalidity : Imap.Uidvalidity.t;
  uid : Imap.Uid.t;
  evidence : string;
}

type repair = private {
  scope : scope;
  connection : connection;
  maildir : string;
  spool_dir : string;
  operation_id : string;
  evidence : string;
}
(** A [repair] is the configuration shared by the online operator
    repairs. *)

type retention = private {
  scope : scope;
  encoding : Imap.Mailbox_name.mode option;
  maildir : string;
  spool_dir : string;
  pair_id : string;
  evidence : string;
}

type plan = private {
  scope : scope;
  encoding : Imap.Mailbox_name.mode option;
  maildir : string;
  spool_dir : string;
  max_inspect : int;
  min_absence_scans : int;
  deletion_policy : Imap.Sync_policy.deletion_policy;
  allow_bootstrap_duplicates : bool;
      (** [allow_bootstrap_duplicates] is always [true] for
          [plan-deletions], so an unpaired bootstrap does not stop it. *)
}

type verify_local = private {
  scope : scope;
  encoding : Imap.Mailbox_name.mode option;
  maildir : string;
  spool_dir : string;
  max_inspect : int;
}

type gc = private { db : string; blob_dir : string; maildir : string option }

type forget_epochs = private {
  scope : scope;
  encoding : Imap.Mailbox_name.mode option;
}

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
(** A [job] is one parsed command. Unset directory options default to the
    database path with [.blobs] or [.spool] appended. *)

val cmd : job Cmdliner.Cmd.t
(** [cmd] is the [imap-sync] command group, with one subcommand per [job]
    constructor. A subcommand accepts only the options it uses, and each
    option falls back to the [IMAP_*] environment variable its help names.
    Its exit statuses are those {!run} returns. *)

val run :
  job -> env:(string -> string option) -> net:_ Eio.Net.t ->
  fs:_ Eio.Path.t -> random:_ Eio.Flow.source -> int
(** [run job ~env ~net ~fs ~random] performs [job] and is its exit status.
    [env] looks up the password of a command that connects. Every failure
    is printed on standard error with the password replaced by
    [\[REDACTED\]]. Cancellation propagates. The statuses are

    - 0 when the command converged or a targeted operation is terminal,
    - 2 when bounded work remains,
    - 3 when pending journal work needs inspection,
    - 4 on a conflict, a held change or an unsafe state,
    - 5 on invalid configuration,
    - 6 on an IMAP connection or protocol failure,
    - 7 on a local filesystem, Maildir or SQLite failure,
    - 8 when the Maildir writer lease, the Maildir metadata lock or the
      database lock is busy,
    - 9 when the targeted operation or pair is not in the scope. *)

val eval :
  ?help:Format.formatter -> ?err:Format.formatter ->
  env:(string -> string option) -> argv:string array -> net:_ Eio.Net.t ->
  fs:_ Eio.Path.t -> random:_ Eio.Flow.source -> unit -> int
(** [eval ~env ~argv ~net ~fs ~random ()] parses [argv] with {!cmd},
    looking up environment defaults with [env], and is the status of
    {!run} on the job. A command line error is 5 and a help request 0.
    [argv] includes the program name. Help goes to [help] and parse errors
    to [err], which default to standard output and standard error. *)
