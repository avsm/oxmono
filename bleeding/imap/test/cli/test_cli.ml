(* Test conveniences over [Maildir]: a format or policy error fails the
   test, and each mutation takes its own writer. *)
module Md = struct
  include Maildir
  let ok = function
    | Ok x -> x
    | Error e -> Alcotest.failf "unexpected Maildir error: %a" pp_error e
  let open_dir path = ok (open_dir path)
  let scan m = ok (scan m)
  let find m ~id = ok (find m ~id)
  let append m ?id ~source ~length ~flags ?mtime () =
    ok (with_writer m (fun w -> append w ?id ~source ~length ~flags ?mtime ()))
  let set_flags m o flags = ok (with_writer m (fun w -> set_flags w o flags))
  let remove m o = with_writer m (fun w -> remove w o)
end

let env = function
  | "IMAP_HOST" -> Some "mail.example"
  | "IMAP_USER" -> Some "alice"
  | "IMAP_ENDPOINT" -> Some "server-id"
  | "IMAP_ACCOUNT" -> Some "account-id"
  | "IMAP_MAILBOX" -> Some "INBOX"
  | "IMAP_DB" -> Some "/tmp/imap-cli.db"
  | "IMAP_MAILDIR" -> Some "/tmp/imap-cli-maildir"
  | "IMAP_PASSWORD_ENV" -> Some "SECRET_VARIABLE"
  | "SECRET_VARIABLE" -> Some "correct horse battery staple"
  | _ -> None

let null = Format.make_formatter (fun _ _ _ -> ()) ignore
let argv args = Array.of_list ("imap-sync" :: args)

let evaluate ?(env=env) args =
  Cmdliner.Cmd.eval_value' ~help:null ~err:null ~env ~argv:(argv args)
    Imap_cli.cmd

let parse args = match evaluate args with
  | `Ok job -> job
  | `Exit code -> Alcotest.failf "command line exited %d" code

let rejects what args = match evaluate args with
  | `Ok _ -> Alcotest.fail what
  | `Exit _ -> ()

let run ?(env=env) eio args =
  Imap_cli.eval ~help:null ~err:null ~env ~argv:(argv args)
    ~net:(Eio.Stdenv.net eio) ~fs:(Eio.Stdenv.fs eio)
    ~random:(Eio.Stdenv.secure_random eio) ()

let missing_file prefix suffix =
  let name=Filename.temp_file prefix suffix in
  Sys.remove name; name

let rec remove path =
  if Sys.is_directory path then (
    Sys.readdir path |> Array.iter (fun name ->
      remove (Filename.concat path name));
    Unix.rmdir path)
  else Sys.remove path

let with_root prefix f =
  let root=Filename.temp_file prefix "" in
  Sys.remove root; Unix.mkdir root 0o700;
  Fun.protect ~finally:(fun () -> remove root) (fun () -> f root)

let test_default_policy () =
  match parse ["sync";"--auth";"cram-md5"] with
  | Sync config ->
      Alcotest.(check bool) "deletion preserve" true
        (config.deletion_policy=Imap.Sync_policy.Preserve);
      Alcotest.(check bool) "no duplicate bootstrap" false
        config.allow_bootstrap_duplicates;
      Alcotest.(check bool) "CRAM-MD5" true
        (config.connection.auth=`Cram_md5);
      Alcotest.(check int) "one cycle" 1 config.max_cycles;
      Alcotest.(check string) "password env name" "SECRET_VARIABLE"
        config.connection.password_env
  | _ -> Alcotest.fail "sync parsed as another command"

let test_verify_local_config () =
  (match parse ["verify-local";"--max-inspect";"7"] with
   | Verify_local config ->
       Alcotest.(check int) "verification display bound" 7
         config.max_inspect
   | _ -> Alcotest.fail "offline local verification command");
  rejects "verification accepted operation ID"
    ["verify-local";"--operation-id";"op"];
  Eio_main.run @@ fun eio ->
  let missing=missing_file "imap-cli-verify-missing" ".db" in
  Alcotest.(check int) "missing DB rejected before scrub" 5
    (run eio ["verify-local";"--db";missing]);
  Alcotest.(check bool) "scrub did not create DB" false
    (Sys.file_exists missing)

let test_bounded_opts () =
  (match parse ["sync";"--max-transfers";"3";
      "--max-cycles";"2";"--deletion-policy";"propagate"] with
   | Sync config ->
       Alcotest.(check int) "transfer budget" 3 config.max_transfers;
       Alcotest.(check int) "cycle budget" 2 config.max_cycles;
       Alcotest.(check bool) "explicit deletion" true
         (config.deletion_policy=Imap.Sync_policy.Propagate)
   | _ -> Alcotest.fail "sync parsed as another command");
  rejects "accepted unbounded/zero cycles" ["sync";"--max-cycles";"0"]

let test_hydrate_config () =
  (match parse ["hydrate";"--max-transfers";"7";
      "--max-body-bytes";"8192";"--max-total-bytes";"16384"] with
   | Hydrate config ->
       Alcotest.(check int) "hydration count" 7 config.max_transfers;
       Alcotest.(check int64) "single body bound" 8192L
         config.budget.max_body_bytes;
       Alcotest.(check int64) "pass byte bound" 16384L
         config.budget.max_total_bytes
   | _ -> Alcotest.fail "hydration command");
  (match parse ["sync";"--hydrate-bodies";
      "--max-body-bytes";"8192";"--max-total-bytes";"16384"] with
   | Sync {hydrate_bodies=Some budget;_} ->
       Alcotest.(check int64) "sync hydration bound" 16384L
         budget.max_total_bytes
   | _ -> Alcotest.fail "sync did not schedule hydration");
  let rejects = rejects "accepted invalid hydration option" in
  rejects ["hydrate";"--max-body-bytes";"0"];
  rejects ["hydrate";"--max-total-bytes";"-1"];
  rejects ["sync";"--max-total-bytes";"1024"];
  rejects ["hydrate";"--hydrate-bodies"];
  rejects ["hydrate";"--deletion-policy";"propagate"];
  rejects ["hydrate";"--operation-id";"op"];
  rejects ["hydrate";"--maildir";"/tmp/imap-cli-maildir"];
  Eio_main.run @@ fun eio ->
  let missing=missing_file "imap-cli-hydrate-missing" ".db" in
  Alcotest.(check int) "missing inventory rejected before connection" 5
    (run eio ["hydrate";"--db";missing]);
  Alcotest.(check bool) "hydration did not create empty store" false
    (Sys.file_exists missing)

let test_cache_audit_config () =
  (match parse ["audit-cache";"--max-transfers";"9";
      "--max-total-bytes";"4096";"--after-uid";"17";
      "--expected-revision";"12"] with
   | Audit_cache config ->
       Alcotest.(check (option int64)) "audit continuation" (Some 17L)
         (Option.map (fun (uid,_) -> Imap.Uid.to_int64 uid)
           config.continuation);
       Alcotest.(check (option int64)) "pinned audit revision" (Some 12L)
         (Option.map snd config.continuation);
       Alcotest.(check int) "audit page bound" 9 config.max_transfers
   | _ -> Alcotest.fail "offline audit command");
  let rejects = rejects "accepted invalid audit option" in
  rejects ["audit-cache";"--after-uid";"0"];
  rejects ["audit-cache";"--after-uid";"4294967296"];
  rejects ["audit-cache";"--after-uid";"1"];
  rejects ["audit-cache";"--expected-revision";"1"];
  rejects ["audit-cache";"--max-body-bytes";"100"];
  rejects ["inspect";"--after-uid";"1"];
  Eio_main.run @@ fun eio ->
  let missing=missing_file "imap-cli-audit-missing" ".db" in
  Alcotest.(check int) "audit refuses missing store" 5
    (run eio ["audit-cache";"--db";missing]);
  Alcotest.(check bool) "audit did not create store" false
    (Sys.file_exists missing)

let test_deletion_direction_and_retention () =
  let policy args = match parse args with
    | Sync c -> c.deletion_policy
    | Plan_deletions c | Plan_sync c -> c.deletion_policy
    | _ -> Alcotest.fail "unexpected command" in
  Alcotest.(check bool) "remote direction" true
    (policy ["sync";"--deletion-policy";"propagate-remote"] =
     Imap.Sync_policy.Propagate_remote);
  Alcotest.(check bool) "local direction" true
    (policy ["sync";"--deletion-policy";"propagate-local"] =
     Imap.Sync_policy.Propagate_local);
  (match parse ["mark-local-retention";"--pair-id";"pair-1";
      "--evidence";"local cache expired"] with
   | Mark_local_retention retention ->
       Alcotest.(check string) "retention pair" "pair-1" retention.pair_id
   | _ -> Alcotest.fail "retention command");
  (match parse ["plan-deletions";"--deletion-policy";"propagate-local";
      "--max-inspect";"7"] with
   | Plan_deletions plan ->
       Alcotest.(check bool) "plan direction" true
         (plan.deletion_policy=Imap.Sync_policy.Propagate_local);
       Alcotest.(check int) "plan output cap" 7 plan.max_inspect
   | _ -> Alcotest.fail "plan command");
  (match parse ["plan-sync";"--deletion-policy";"propagate-remote";
      "--allow-bootstrap-duplicates";"--max-inspect";"8";
      "--min-absence-scans";"2"] with
   | Plan_sync full ->
       Alcotest.(check bool) "full plan bootstrap policy" true
         full.allow_bootstrap_duplicates;
       Alcotest.(check int) "complete-scan deletion grace" 2
         full.min_absence_scans
   | _ -> Alcotest.fail "full plan command");
  rejects "inspect accepted deletion grace"
    ["inspect";"--min-absence-scans";"1"];
  rejects "accepted ambiguous deletion policy"
    ["sync";"--deletion-policy";"propagate";
     "--deletion-policy";"propagate-local"];
  rejects "accepted unknown deletion policy"
    ["sync";"--deletion-policy";"remote"];
  rejects "accepted removed deletion flag" ["sync";"--propagate-deletions"]

let test_remote_delete_rejection_guards () =
  let command="reject-remote-delete" in
  rejects "accepted missing repair evidence" [command;"--operation-id";"op"];
  (match parse [command;"--operation-id";"op";
      "--evidence";"target unchanged after server failure"] with
   | Reject_remote_delete repair ->
       Alcotest.(check string) "operation ID" "op" repair.operation_id
   | _ -> Alcotest.fail "remote delete rejection command");
  rejects "accepted sync policy on repair"
    [command;"--operation-id";"op";"--evidence";"ok";
     "--deletion-policy";"propagate"];
  (match parse ["finish-remote-delete";"--operation-id";"op";
      "--evidence";"operator authorized UID EXPUNGE"] with
   | Finish_remote_delete _ -> ()
   | _ -> Alcotest.fail "finish command");
  rejects "accepted missing finish evidence"
    ["finish-remote-delete";"--operation-id";"op"]

let test_repair_requires_attestation () =
  rejects "accepted missing evidence"
    ["repair-appenduid";"--operation-id";"op";
     "--uidvalidity";"1";"--uid";"2"];
  match parse ["repair-appenduid";"--operation-id";"op";
      "--uidvalidity";"1";"--uid";"2";
      "--evidence";"operator read APPENDUID from audit"] with
  | Repair_appenduid repair ->
      Alcotest.(check int64) "UID" 2L (Imap.Uid.to_int64 repair.uid)
  | _ -> Alcotest.fail "repair command"

let test_repair_guards () =
  let base=["repair-appenduid";"--operation-id";"op";
    "--uidvalidity";"1";"--uid";"2"] in
  let reject evidence =
    rejects "accepted unsafe evidence" (base @ ["--evidence";evidence]) in
  reject (String.make 1025 'a');
  reject "line\nbreak";
  reject "   ";
  Eio_main.run @@ fun eio ->
  let missing=missing_file "imap-cli-missing" ".db" in
  Alcotest.(check int) "missing DB rejected" 5
    (run eio (base @ ["--evidence";"trusted audit";"--db";missing]));
  Alcotest.(check bool) "DB not created" false (Sys.file_exists missing);
  with_root "imap-cli-appenduid" @@ fun root ->
  let fs=Eio.Stdenv.fs eio in
  let database=Filename.concat root "sync.db" in
  Eio.Switch.run (fun sw ->
    ignore (Imap_store.open_path ~sw Eio.Path.(fs / database)));
  let maildir=Filename.concat root "Maildir" in
  let args=base @ ["--evidence";"trusted audit";"--db";database;
    "--maildir";maildir] in
  Alcotest.(check int) "missing Maildir rejected" 5 (run eio args);
  Alcotest.(check bool) "Maildir not created" false (Sys.file_exists maildir);
  ignore (Md.open_dir Eio.Path.(fs / maildir));
  Alcotest.(check int) "unknown operation not found" 9 (run eio args)

let test_online_repair_guards command ~what =
  let base=[command;"--operation-id";"op"] in
  let reject extras = rejects ("accepted unsafe " ^ what) (base @ extras) in
  reject [];
  reject ["--evidence";" "];
  reject ["--evidence";"line\nbreak"];
  reject ["--evidence";"audit";"--uid";"7"];
  reject ["--evidence";"audit";"--encoding";"utf8"];
  let job=parse (base @ ["--evidence";"operator verified UID absent"]) in
  Eio_main.run @@ fun eio ->
  let missing=missing_file ("imap-cli-" ^ command) ".db" in
  Alcotest.(check int) "missing DB rejected before connection" 5
    (run eio (base @ ["--evidence";"audit";"--db";missing]));
  Alcotest.(check bool) "repair did not create DB" false
    (Sys.file_exists missing);
  job

let test_local_delete_repair_guards () =
  match test_online_repair_guards "repair-local-delete"
      ~what:"local deletion repair" with
  | Repair_local_delete repair ->
      Alcotest.(check string) "evidence retained"
        "operator verified UID absent" repair.evidence
  | _ -> Alcotest.fail "local deletion repair command"

let test_local_append_repair_guards () =
  match test_online_repair_guards "repair-local-append"
      ~what:"local append repair" with
  | Repair_local_append _ -> ()
  | _ -> Alcotest.fail "local append repair command"

let test_flags_settlement_guards () =
  match test_online_repair_guards "settle-flags"
      ~what:"FLAGS settlement" with
  | Settle_flags _ -> ()
  | _ -> Alcotest.fail "FLAGS settlement command"

let test_append_candidate_guards () =
  let base=["inspect-append-candidates";"--operation-id";"op"] in
  let reject extras =
    rejects "accepted unsafe candidate inspection" (base @ extras) in
  reject ["--max-inspect";"10001"];
  reject ["--max-candidate-bytes";"0"];
  reject ["--uid";"7"];
  reject ["--evidence";"matching bytes"];
  reject ["--encoding";"utf8"];
  (match parse (base @ ["--max-inspect";"17";
      "--max-candidate-bytes";"4096"]) with
   | Inspect_append_candidates config ->
       Alcotest.(check int) "candidate budget" 17 config.max_inspect;
       Alcotest.(check int64) "body budget" 4096L config.max_candidate_bytes
   | _ -> Alcotest.fail "candidate command");
  Eio_main.run @@ fun eio ->
  let missing=missing_file "imap-cli-candidate-missing" ".db" in
  Alcotest.(check int) "missing DB rejected before connection" 5
    (run eio (base @ ["--db";missing]));
  Alcotest.(check bool) "inspection did not create DB" false
    (Sys.file_exists missing)

let test_readonly_inspect () =
  Eio_main.run @@ fun eio ->
  let filename=Filename.temp_file "imap-cli-inspect" ".sqlite" in
  let cleanup () = List.iter (fun name -> try Sys.remove name
    with Sys_error _ -> ()) [filename;filename^"-wal";filename^"-shm"] in
  Fun.protect ~finally:cleanup @@ fun () ->
  let fs=Eio.Stdenv.fs eio in
  Eio.Switch.run (fun sw ->
    ignore (Imap_store.open_path ~sw Eio.Path.(fs / filename)));
  Alcotest.(check int) "empty database" 0
    (run eio ["inspect";"--db";filename]);
  let no_password = function
    | "SECRET_VARIABLE" | "IMAP_PASSWORD" -> None
    | name -> env name in
  Alcotest.(check int) "offline command reads no password" 0
    (run ~env:no_password eio ["inspect";"--db";filename]);
  let missing=missing_file "imap-cli-inspect-missing" ".db" in
  Alcotest.(check int) "inspect requires an existing database" 5
    (run eio ["inspect";"--db";missing]);
  let maildir=Filename.temp_file "imap-cli-plan" "" in
  Sys.remove maildir;
  Fun.protect ~finally:(fun () -> remove maildir) @@ fun () ->
  ignore (Md.open_dir Eio.Path.(fs / maildir));
  let spool=filename ^ ".spool" in
  Fun.protect ~finally:(fun () ->
    if Sys.file_exists spool then remove spool) @@ fun () ->
  Alcotest.(check int) "plan requires complete published inventory" 5
    (run eio ["plan-deletions";"--db";filename;"--maildir";maildir;
      "--deletion-policy";"propagate"]);
  Alcotest.(check int) "full plan requires complete published inventory" 5
    (run eio ["plan-sync";"--db";filename;"--maildir";maildir]);
  Alcotest.(check bool) "plan spool defaults beside the database" true
    (Sys.is_directory spool)

let test_targeted_inspect () =
  (match parse ["inspect";"--operation-id";"active"] with
   | Inspect targeted ->
       Alcotest.(check (option string)) "target ID" (Some "active")
         targeted.operation_id
   | _ -> Alcotest.fail "inspect command");
  rejects "sync accepted operation ID" ["sync";"--operation-id";"active"];
  rejects "inspect accepted repair UID" ["inspect";"--uid";"1"];
  rejects "empty ID fell back to list mode" ["inspect";"--operation-id";""];
  Eio_main.run @@ fun eio ->
  let filename=Filename.temp_file "imap-cli-target" ".sqlite" in
  let cleanup () = List.iter (fun name -> try Sys.remove name
    with Sys_error _ -> ()) [filename;filename^"-wal";filename^"-shm"] in
  Fun.protect ~finally:cleanup @@ fun () ->
  let fs=Eio.Stdenv.fs eio in
  let scope : Imap.Mirror.scope = {
    endpoint="server-id";account="account-id";mailbox_key="INBOX";
    raw_name="INBOX";encoding=Imap.Mailbox_name.Rev1;mailbox_id=None} in
  let epoch=match Imap.Uidvalidity.of_int64 1L with
    | Ok x -> x | Error e -> Alcotest.fail e in
  let uid=match Imap.Uid.of_int64 1L with
    | Ok x -> x | Error e -> Alcotest.fail e in
  Eio.Switch.run (fun sw ->
    let store=Imap_store.open_path ~sw Eio.Path.(fs / filename) in
    let make id scope : Imap_store.Journal.operation = {
      id;pair_id=None;local_id=None;scope;kind=Flags;state=Prepared;
      source_uidvalidity=Some epoch;source_uid=Some uid;
      destination=None;destination_uidvalidity=None;
      blob_sha256=None;blob_length=None;desired_flags=Some [];
      receipt=None;receipt_uidvalidity=None;receipt_uid=None} in
    List.iter (fun id -> Imap_store.Journal.prepare_operation store
      (make id scope)) ["active";"committed";"rejected";"second"];
    Imap_store.Journal.prepare_operation store
      (make "foreign" {scope with account="other-account"});
    Imap_store.Journal.mark_sent store ~id:"committed";
    Imap_store.Journal.observe_operation store ~id:"committed"
      ~receipt:"verified" ~destination_uidvalidity:None
      ~destination_uid:None;
    Imap_store.Journal.commit_operation store ~id:"committed";
    Imap_store.Journal.reject_prepared_operation store ~id:"rejected"
      ~receipt:"unsent");
  let inspect id =
    run eio ["inspect";"--db";filename;"--operation-id";id;
      "--max-inspect";"1"] in
  Alcotest.(check int) "active" 3 (inspect "active");
  Alcotest.(check int) "committed" 0 (inspect "committed");
  Alcotest.(check int) "rejected" 0 (inspect "rejected");
  Alcotest.(check int) "another scope hidden" 9 (inspect "foreign");
  Alcotest.(check int) "unknown ID" 9 (inspect "missing");
  Alcotest.(check int) "two active operations listed" 3
    (run eio ["inspect";"--db";filename;"--max-inspect";"2"])

let orphan blob_dir =
  let name=Filename.concat blob_dir ".tmp-0123456789abcdef" in
  let output=open_out_bin name in
  output_string output "orphan";
  close_out output;
  name

let test_sync_recovers_before_connect () =
  Eio_main.run @@ fun eio ->
  with_root "imap-cli-recover" @@ fun root ->
  let maildir=Filename.concat root "maildir" in
  let blob_dir=Filename.concat root "blob" in
  let fs=Eio.Stdenv.fs eio in
  ignore (Md.open_dir Eio.Path.(fs / maildir));
  Unix.mkdir blob_dir 0o700;
  let orphan=orphan blob_dir in
  let abandoned=Filename.concat (Filename.concat maildir "tmp")
    ".tmp-0123456789abcdef0123456789abcdef" in
  let output=open_out_bin abandoned in
  output_string output "abandoned";
  close_out output;
  Alcotest.(check int) "refused connection is an IMAP failure" 6
    (run eio ["sync";"--host";"127.0.0.1";"--port";"1";
      "--tls";"plain";"--db";Filename.concat root "sync.db";
      "--maildir";maildir;"--blob-dir";blob_dir;
      "--spool-dir";Filename.concat root "spool"]);
  Alcotest.(check bool) "startup removed abandoned Maildir tmp" false
    (Sys.file_exists abandoned);
  Alcotest.(check bool) "startup removed orphan blob" false
    (Sys.file_exists orphan)

let test_missing_password () =
  Eio_main.run @@ fun eio ->
  with_root "imap-cli-password" @@ fun root ->
  let no_password = function "SECRET_VARIABLE" -> None | name -> env name in
  let database=Filename.concat root "sync.db" in
  Alcotest.(check int) "online command needs the password" 5
    (run ~env:no_password eio ["sync";"--tls";"plain";"--db";database;
      "--maildir";Filename.concat root "maildir"]);
  Alcotest.(check bool) "nothing created before the password" false
    (Sys.file_exists database)

let test_gc () =
  Eio_main.run @@ fun eio ->
  with_root "imap-cli-gc" @@ fun root ->
  let fs=Eio.Stdenv.fs eio in
  let database=Filename.concat root "sync.db" in
  let blob_dir=database ^ ".blobs" in
  Alcotest.(check int) "gc requires an existing database" 5
    (run eio ["gc";"--db";database]);
  Eio.Switch.run (fun sw ->
    ignore (Imap_store.open_path ~sw Eio.Path.(fs / database)));
  Alcotest.(check int) "gc requires an existing blob directory" 5
    (run eio ["gc";"--db";database]);
  Unix.mkdir blob_dir 0o700;
  let first=orphan blob_dir in
  let no_maildir = function "IMAP_MAILDIR" -> None | name -> env name in
  Alcotest.(check int) "gc without Maildir" 0
    (run ~env:no_maildir eio ["gc";"--db";database]);
  Alcotest.(check bool) "orphan removed" false (Sys.file_exists first);
  let maildir=Filename.concat root "Maildir" in
  let m=Md.open_dir Eio.Path.(fs / maildir) in
  let second=orphan blob_dir in
  Maildir.with_writer m (fun _ ->
    Alcotest.(check int) "busy lease" 8
      (run eio ["gc";"--db";database;"--maildir";maildir]));
  Alcotest.(check bool) "busy lease kept the orphan" true
    (Sys.file_exists second);
  Alcotest.(check int) "gc under the lease" 0
    (run eio ["gc";"--db";database;"--maildir";maildir]);
  Alcotest.(check bool) "orphan removed under the lease" false
    (Sys.file_exists second);
  rejects "gc accepted a scope option" ["gc";"--mailbox";"INBOX"]

let test_forget_epochs () =
  Eio_main.run @@ fun eio ->
  with_root "imap-cli-forget" @@ fun root ->
  let fs=Eio.Stdenv.fs eio in
  let database=Filename.concat root "sync.db" in
  Alcotest.(check int) "forget-epochs requires an existing database" 5
    (run eio ["forget-epochs";"--db";database]);
  Eio.Switch.run (fun sw ->
    ignore (Imap_store.open_path ~sw Eio.Path.(fs / database)));
  Alcotest.(check int) "no current epoch to keep" 4
    (run eio ["forget-epochs";"--db";database])

let test_mark_local_retention () =
  Eio_main.run @@ fun eio ->
  with_root "imap-cli-retention" @@ fun root ->
  let fs=Eio.Stdenv.fs eio in
  let database=Filename.concat root "sync.db"
  and local_path=Filename.concat root "Maildir" in
  ignore (Md.open_dir Eio.Path.(fs / local_path));
  let scope:Imap.Mirror.scope={endpoint="server-id";account="account-id";
    mailbox_key="INBOX";raw_name="INBOX";
    encoding=Imap.Mailbox_name.Rev1;mailbox_id=None} in
  let epoch=match Imap.Uidvalidity.of_int64 1L with
    | Ok x -> x | Error e -> Alcotest.fail e in
  let uid=match Imap.Uid.of_int64 1L with
    | Ok x -> x | Error e -> Alcotest.fail e in
  Eio.Switch.run @@ fun sw ->
  let store=Imap_store.open_path ~sw Eio.Path.(fs / database) in
  let module J=Imap_store.Journal in
  let pair:J.pair={id="pair-retained";scope;
    remote_uidvalidity=Some epoch;remote_uid=Some uid;
    local_id=Some "local-evicted";content_sha256=Some (String.make 64 'a');
    content_length=Some 1L;internal_date=None;common_flags=[];
    remote_tombstone=None;local_tombstone=None;revision=0L} in
  (match J.put_pair store ~expected_revision:None pair with
   | `Committed _ -> () | `Stale_revision -> Alcotest.fail "new pair stale");
  let retain id = run eio ["mark-local-retention";"--db";database;
    "--maildir";local_path;"--pair-id";id;
    "--evidence";"cache retention expired"] in
  Alcotest.(check int) "unknown pair not found" 9 (retain "pair-missing");
  Alcotest.(check int) "retention command succeeded" 0 (retain pair.id);
  let pair=Option.get (J.find_pair store ~id:pair.id) in
  Alcotest.(check bool) "durable retention tombstone" true
    (match pair.local_tombstone with
     | Some {reason=J.Retention;evidence="cache retention expired";_} -> true
     | _ -> false);
  Alcotest.(check bool) "retained absence blocks deletion" true
    (Imap.Sync_policy.plan_disappearance_with_grace ~absence_mature:true
      ~policy:Imap.Sync_policy.Propagate ~paired:true ~local_retained:true
      ~survivor_unchanged:true ~remote:{present=true;complete=true}
      ~local:{present=false;complete=true} =
      Imap.Sync_policy.Hold_deletion Imap.Sync_policy.Retention_policy)

let () = Alcotest.run "imap-cli" [
  "config", [
    Alcotest.test_case "default policy and CRAM-MD5" `Quick
      test_default_policy;
    Alcotest.test_case "offline local verification" `Quick
      test_verify_local_config;
    Alcotest.test_case "budgets" `Quick test_bounded_opts;
    Alcotest.test_case "hydration budgets" `Quick test_hydrate_config;
    Alcotest.test_case "cache audit continuation" `Quick
      test_cache_audit_config;
    Alcotest.test_case "deletion direction and retention" `Quick
      test_deletion_direction_and_retention;
    Alcotest.test_case "remote delete rejection guards" `Quick
      test_remote_delete_rejection_guards;
    Alcotest.test_case "repair attestation" `Quick
      test_repair_requires_attestation;
    Alcotest.test_case "repair guards" `Quick test_repair_guards;
    Alcotest.test_case "local delete repair guards" `Quick
      test_local_delete_repair_guards;
    Alcotest.test_case "local append repair guards" `Quick
      test_local_append_repair_guards;
    Alcotest.test_case "FLAGS settlement guards" `Quick
      test_flags_settlement_guards;
    Alcotest.test_case "APPEND candidate guards" `Quick
      test_append_candidate_guards;
    Alcotest.test_case "read-only inspection" `Quick
      test_readonly_inspect;
    Alcotest.test_case "targeted operation inspection" `Quick
      test_targeted_inspect;
    Alcotest.test_case "sync recovers before connecting" `Quick
      test_sync_recovers_before_connect;
    Alcotest.test_case "password only for online commands" `Quick
      test_missing_password;
    Alcotest.test_case "mark local retention" `Quick
      test_mark_local_retention;
  ];
  "reclamation", [
    Alcotest.test_case "gc" `Quick test_gc;
    Alcotest.test_case "forget-epochs" `Quick test_forget_epochs;
  ]]
