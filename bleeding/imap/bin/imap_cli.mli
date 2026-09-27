(** Validated configuration and one-shot operational entry point. Credentials
    are read from an environment variable only for online commands. *)

type command = Sync | Hydrate | Audit_cache | Inspect | Inspect_append_candidates | Repair_appenduid
  | Repair_local_delete | Repair_local_append | Settle_flags
  | Mark_local_retention | Plan_deletions | Plan_sync | Verify_local
  | Reject_remote_delete
  | Finish_remote_delete
type config = private {
  command : command;
  host : string;
  port : int option;
  tls : Imap_eio.Transport.tls;
  username : string;
  password_env : string;
  mechanism : Imap_eio.Auth.mechanism;
  endpoint : string;
  account : string;
  mailbox : string;
  mailbox_key : string;
  encoding : Imap.Mailbox_name.mode;
  encoding_explicit : bool;
  db : string;
  blob_dir : string;
  maildir : string;
  spool_dir : string;
  max_transfers : int;
  min_absence_scans : int;
  max_cycles : int;
  max_inspect : int;
  max_candidate_bytes : int64;
  max_body_bytes : int64;
  max_total_bytes : int64;
  hydrate_bodies : bool;
  after_uid : Imap.Uid.t option;
  expected_revision : int64 option;
  propagate_deletions : bool;
  propagate_remote_deletions : bool;
  propagate_local_deletions : bool;
  allow_bootstrap_duplicates : bool;
  operation_id : string;
  pair_id : string;
  receipt_uidvalidity : Imap.Uidvalidity.t option;
  receipt_uid : Imap.Uid.t option;
  evidence : string;
}

val usage : string
val parse : getenv:(string -> string option) -> string array ->
  (config, string) result
(** This is the sole constructor of [config]; fields remain readable.
    [argv] includes the program name. Never includes the secret value in
    errors. [sync] requires host and username; online commands resolve a non-empty
    password from the configured environment variable when [run] is called;
    [inspect], [verify-local] and [repair-appenduid] require only local paths
    and scope;
    [hydrate], [inspect-append-candidates], [repair-local-delete], and
    [repair-local-append] and [settle-flags] also require a live IMAP
    connection. [hydrate] requires an existing published SQLite inventory,
    blob and spool directories, but no Maildir. *)

val run : config -> net:_ Eio.Net.t -> fs:_ Eio.Path.t ->
  random:_ Eio.Flow.source -> getenv:(string -> string option) -> int
(** Exit 0 when converged/inspected or a targeted operation is terminal;
    2 if bounded work remains; 3 for pending
    journal work; 4 for conflicts; 5 for configuration; 6 for IMAP/protocol
    failure; 7 for local storage failure; 8 when writer lease is busy;
    9 when a targeted operation is not found in the requested scope. *)
