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

let parse args = match Imap_cli.parse ~getenv:env
  (Array.of_list ("imap-sync" :: args)) with
  | Ok config -> config
  | Error message -> Alcotest.fail message

let test_default_policy () =
  let config=parse ["sync";"--auth";"cram-md5"] in
  Alcotest.(check bool) "deletion preserve" false
    config.propagate_deletions;
  Alcotest.(check bool) "no duplicate bootstrap" false
    config.allow_bootstrap_duplicates;
  Alcotest.(check bool) "CRAM-MD5" true
    (config.mechanism=`Cram_md5);
  Alcotest.(check int) "one cycle" 1 config.max_cycles;
  Alcotest.(check string) "password env name" "SECRET_VARIABLE"
    config.password_env

let test_verify_local_config () =
  let config=parse ["verify-local";"--max-inspect";"7"] in
  Alcotest.(check bool) "offline local verification command" true
    (config.command=Imap_cli.Verify_local);
  Alcotest.(check int) "verification display bound" 7
    config.max_inspect;
  (match Imap_cli.parse ~getenv:env [|"imap-sync";"verify-local";
      "--operation-id";"op"|] with
   | Error _ -> ()
   | Ok _ -> Alcotest.fail "verification accepted operation ID");
  Eio_main.run @@ fun eio ->
  let missing=Filename.temp_file "imap-cli-verify-missing" ".db" in
  Sys.remove missing;
  let config=parse ["verify-local";"--db";missing] in
  let code=Imap_cli.run config ~net:(Eio.Stdenv.net eio)
    ~fs:(Eio.Stdenv.fs eio)
    ~random:(Eio.Stdenv.secure_random eio) ~getenv:env in
  Alcotest.(check int) "missing DB rejected before scrub" 5 code;
  Alcotest.(check bool) "scrub did not create DB" false
    (Sys.file_exists missing)

let test_bounded_opts () =
  let config=parse ["sync";"--max-transfers";"3";
    "--max-cycles";"2";"--propagate-deletions"] in
  Alcotest.(check int) "transfer budget" 3 config.max_transfers;
  Alcotest.(check int) "cycle budget" 2 config.max_cycles;
  Alcotest.(check bool) "explicit deletion" true
    config.propagate_deletions;
  (match Imap_cli.parse ~getenv:env [|"imap-sync";"sync";
      "--max-cycles";"0"|] with
  | Error _ -> () | Ok _ -> Alcotest.fail "accepted unbounded/zero cycles")

let test_hydrate_config () =
  let config=parse ["hydrate";"--max-transfers";"7";
    "--max-body-bytes";"8192";"--max-total-bytes";"16384"] in
  Alcotest.(check bool) "hydration command" true
    (config.command=Imap_cli.Hydrate);
  Alcotest.(check int) "hydration count" 7 config.max_transfers;
  Alcotest.(check int64) "single body bound" 8192L config.max_body_bytes;
  Alcotest.(check int64) "pass byte bound" 16384L config.max_total_bytes;
  let sync_config=parse ["sync";"--hydrate-bodies";
    "--max-body-bytes";"8192";"--max-total-bytes";"16384"] in
  Alcotest.(check bool) "sync schedules hydration" true
    sync_config.hydrate_bodies;
  let rejects args = match Imap_cli.parse ~getenv:env
      (Array.of_list ("imap-sync"::args)) with
    | Error _ -> () | Ok _ -> Alcotest.fail "accepted invalid hydration option" in
  rejects ["hydrate";"--max-body-bytes";"0"];
  rejects ["hydrate";"--max-total-bytes";"-1"];
  rejects ["sync";"--max-total-bytes";"1024"];
  rejects ["hydrate";"--hydrate-bodies"];
  rejects ["hydrate";"--propagate-deletions"];
  rejects ["hydrate";"--operation-id";"op"];
  Eio_main.run @@ fun eio ->
  let missing=Filename.temp_file "imap-cli-hydrate-missing" ".db" in
  Sys.remove missing;
  let config=parse ["hydrate";"--db";missing] in
  Alcotest.(check int) "missing inventory rejected before connection" 5
    (Imap_cli.run config ~net:(Eio.Stdenv.net eio)
      ~fs:(Eio.Stdenv.fs eio)
      ~random:(Eio.Stdenv.secure_random eio) ~getenv:env);
  Alcotest.(check bool) "hydration did not create empty store" false
    (Sys.file_exists missing)

let test_cache_audit_config () =
  let config=parse ["audit-cache";"--max-transfers";"9";
    "--max-total-bytes";"4096";"--after-uid";"17";
    "--expected-revision";"12"] in
  Alcotest.(check bool) "offline audit command" true
    (config.command=Imap_cli.Audit_cache);
  Alcotest.(check (option int64)) "audit continuation" (Some 17L)
    config.after_uid;
  Alcotest.(check (option int64)) "pinned audit revision" (Some 12L)
    config.expected_revision;
  Alcotest.(check int) "audit page bound" 9 config.max_transfers;
  let rejects args = match Imap_cli.parse ~getenv:env
      (Array.of_list ("imap-sync"::args)) with
    | Error _ -> () | Ok _ -> Alcotest.fail "accepted invalid audit option" in
  rejects ["audit-cache";"--after-uid";"0"];
  rejects ["audit-cache";"--after-uid";"4294967296"];
  rejects ["audit-cache";"--after-uid";"1"];
  rejects ["audit-cache";"--expected-revision";"1"];
  rejects ["audit-cache";"--max-body-bytes";"100"];
  rejects ["inspect";"--after-uid";"1"];
  Eio_main.run @@ fun eio ->
  let missing=Filename.temp_file "imap-cli-audit-missing" ".db" in
  Sys.remove missing;
  let config=parse ["audit-cache";"--db";missing] in
  Alcotest.(check int) "audit refuses missing store" 5
    (Imap_cli.run config ~net:(Eio.Stdenv.net eio)
      ~fs:(Eio.Stdenv.fs eio)
      ~random:(Eio.Stdenv.secure_random eio) ~getenv:env);
  Alcotest.(check bool) "audit did not create store" false
    (Sys.file_exists missing)

let test_deletion_direction_and_retention () =
  let remote=parse ["sync";"--propagate-remote-deletions"] in
  Alcotest.(check bool) "remote direction" true
    remote.propagate_remote_deletions;
  Alcotest.(check bool) "local direction disabled" false
    remote.propagate_local_deletions;
  let local=parse ["sync";"--propagate-local-deletions"] in
  Alcotest.(check bool) "local direction" true
    local.propagate_local_deletions;
  let retention=parse ["mark-local-retention";"--pair-id";"pair-1";
    "--evidence";"local cache expired"] in
  Alcotest.(check bool) "retention command" true
    (retention.command=Imap_cli.Mark_local_retention);
  Alcotest.(check string) "retention pair" "pair-1" retention.pair_id;
  let plan=parse ["plan-deletions";"--propagate-local-deletions";
    "--max-inspect";"7"] in
  Alcotest.(check bool) "plan command" true
    (plan.command=Imap_cli.Plan_deletions);
  Alcotest.(check bool) "plan direction" true
    plan.propagate_local_deletions;
  Alcotest.(check int) "plan output cap" 7 plan.max_inspect;
  let full=parse ["plan-sync";"--propagate-remote-deletions";
    "--allow-bootstrap-duplicates";"--max-inspect";"8";
    "--min-absence-scans";"2"] in
  Alcotest.(check bool) "full plan command" true
    (full.command=Imap_cli.Plan_sync);
  Alcotest.(check bool) "full plan bootstrap policy" true
    full.allow_bootstrap_duplicates;
  Alcotest.(check int) "complete-scan deletion grace" 2
    full.min_absence_scans;
  (match Imap_cli.parse ~getenv:env [|"imap-sync";"inspect";
      "--min-absence-scans";"1"|] with
   | Error _ -> () | Ok _ -> Alcotest.fail "inspect accepted deletion grace");
  (match Imap_cli.parse ~getenv:env [|"imap-sync";"sync";
      "--propagate-deletions";"--propagate-local-deletions"|] with
   | Error _ -> () | Ok _ -> Alcotest.fail "accepted ambiguous deletion policy")

let test_remote_delete_rejection_guards () =
  let command="reject-remote-delete" in
  (match Imap_cli.parse ~getenv:env [|"imap-sync";command;
      "--operation-id";"op"|] with
   | Error _ -> () | Ok _ -> Alcotest.fail "accepted missing repair evidence");
  let config=parse [command;"--operation-id";"op";
    "--evidence";"target unchanged after server failure"] in
  Alcotest.(check bool) "remote delete rejection command" true
    (config.command=Imap_cli.Reject_remote_delete);
  Alcotest.(check string) "operation ID" "op" config.operation_id;
  (match Imap_cli.parse ~getenv:env [|"imap-sync";command;
      "--operation-id";"op";"--evidence";"ok";
      "--propagate-deletions"|] with
   | Error _ -> () | Ok _ -> Alcotest.fail "accepted sync policy on repair");
  let finish=parse ["finish-remote-delete";"--operation-id";"op";
    "--evidence";"operator authorized UID EXPUNGE"] in
  Alcotest.(check bool) "finish command" true
    (finish.command=Imap_cli.Finish_remote_delete);
  (match Imap_cli.parse ~getenv:env [|"imap-sync";
      "finish-remote-delete";"--operation-id";"op"|] with
   | Error _ -> () | Ok _ -> Alcotest.fail "accepted missing finish evidence")

let test_repair_requires_attestation () =
  (match Imap_cli.parse ~getenv:env
    [|"imap-sync";"repair-appenduid";"--operation-id";"op";
      "--uidvalidity";"1";"--uid";"2"|] with
  | Error _ -> () | Ok _ -> Alcotest.fail "accepted missing evidence");
  let config=parse ["repair-appenduid";"--operation-id";"op";
    "--uidvalidity";"1";"--uid";"2";
    "--evidence";"operator read APPENDUID from audit"] in
  Alcotest.(check bool) "repair command" true
    (config.command=Imap_cli.Repair_appenduid);
  Alcotest.(check (option int64)) "UID" (Some 2L) config.receipt_uid

let test_repair_guards () =
  let base=["repair-appenduid";"--operation-id";"op";
    "--uidvalidity";"1";"--uid";"2"] in
  let reject evidence =
    match Imap_cli.parse ~getenv:env
      (Array.of_list ("imap-sync" :: base @ ["--evidence";evidence])) with
    | Error _ -> () | Ok _ -> Alcotest.fail "accepted unsafe evidence" in
  reject (String.make 1025 'a');
  reject "line\nbreak";
  reject "   ";
  Eio_main.run @@ fun eio ->
  let missing=Filename.temp_file "imap-cli-missing" ".db" in
  Sys.remove missing;
  let config=parse (base @ ["--evidence";"trusted audit";
    "--db";missing]) in
  let code=Imap_cli.run config ~net:(Eio.Stdenv.net eio)
    ~fs:(Eio.Stdenv.fs eio)
    ~random:(Eio.Stdenv.secure_random eio) ~getenv:env in
  Alcotest.(check int) "missing DB rejected" 5 code;
  Alcotest.(check bool) "DB not created" false (Sys.file_exists missing)

let test_local_delete_repair_guards () =
  let base=["repair-local-delete";"--operation-id";"op"] in
  let reject extras =
    match Imap_cli.parse ~getenv:env
      (Array.of_list ("imap-sync" :: base @ extras)) with
    | Error _ -> ()
    | Ok _ -> Alcotest.fail "accepted unsafe local deletion repair" in
  reject [];
  reject ["--evidence";" " ];
  reject ["--evidence";"line\nbreak"];
  reject ["--evidence";"audit";"--uid";"7"];
  reject ["--evidence";"audit";"--encoding";"utf8"];
  let config=parse (base @ ["--evidence";"operator verified UID absent"])
  in
  Alcotest.(check bool) "local deletion repair command" true
    (config.command=Imap_cli.Repair_local_delete);
  Alcotest.(check string) "evidence retained"
    "operator verified UID absent" config.evidence;
  Eio_main.run @@ fun eio ->
  let missing=Filename.temp_file "imap-cli-delete-missing" ".db" in
  Sys.remove missing;
  let config=parse (base @ ["--evidence";"audit";"--db";missing]) in
  let code=Imap_cli.run config ~net:(Eio.Stdenv.net eio)
    ~fs:(Eio.Stdenv.fs eio)
    ~random:(Eio.Stdenv.secure_random eio) ~getenv:env in
  Alcotest.(check int) "missing DB rejected before connection" 5 code;
  Alcotest.(check bool) "repair did not create DB" false
    (Sys.file_exists missing)

let test_local_append_repair_guards () =
  let base=["repair-local-append";"--operation-id";"op"] in
  let reject extras=match Imap_cli.parse ~getenv:env
    (Array.of_list ("imap-sync" :: base @ extras)) with
    | Error _ -> ()
    | Ok _ -> Alcotest.fail "accepted unsafe local append repair" in
  reject [];
  reject ["--evidence";" "];
  reject ["--evidence";"line\nbreak"];
  reject ["--evidence";"audit";"--uid";"7"];
  reject ["--evidence";"audit";"--encoding";"utf8"];
  let config=parse (base @ ["--evidence";"operator verified source"]) in
  Alcotest.(check bool) "local append repair command" true
    (config.command=Imap_cli.Repair_local_append);
  Eio_main.run @@ fun eio ->
  let missing=Filename.temp_file "imap-cli-append-missing" ".db" in
  Sys.remove missing;
  let config=parse (base @ ["--evidence";"audit";"--db";missing]) in
  let code=Imap_cli.run config ~net:(Eio.Stdenv.net eio)
    ~fs:(Eio.Stdenv.fs eio)
    ~random:(Eio.Stdenv.secure_random eio) ~getenv:env in
  Alcotest.(check int) "missing DB rejected before connection" 5 code;
  Alcotest.(check bool) "repair did not create DB" false
    (Sys.file_exists missing)

let test_flags_settlement_guards () =
  let base=["settle-flags";"--operation-id";"op"] in
  let reject extras=match Imap_cli.parse ~getenv:env
    (Array.of_list ("imap-sync" :: base @ extras)) with
    | Error _ -> ()
    | Ok _ -> Alcotest.fail "accepted unsafe FLAGS settlement" in
  reject [];
  reject ["--evidence";" "];
  reject ["--evidence";"line\nbreak"];
  reject ["--evidence";"audit";"--uid";"7"];
  reject ["--evidence";"audit";"--encoding";"utf8"];
  let config=parse (base @ ["--evidence";"operator aligned both sides"]) in
  Alcotest.(check bool) "FLAGS settlement command" true
    (config.command=Imap_cli.Settle_flags);
  Eio_main.run @@ fun eio ->
  let missing=Filename.temp_file "imap-cli-flags-missing" ".db" in
  Sys.remove missing;
  let config=parse (base @ ["--evidence";"audit";"--db";missing]) in
  let code=Imap_cli.run config ~net:(Eio.Stdenv.net eio)
    ~fs:(Eio.Stdenv.fs eio)
    ~random:(Eio.Stdenv.secure_random eio) ~getenv:env in
  Alcotest.(check int) "missing DB rejected before connection" 5 code;
  Alcotest.(check bool) "settlement did not create DB" false
    (Sys.file_exists missing)

let test_append_candidate_guards () =
  let base=["inspect-append-candidates";"--operation-id";"op"] in
  let reject extras=match Imap_cli.parse ~getenv:env
    (Array.of_list ("imap-sync" :: base @ extras)) with
    | Error _ -> ()
    | Ok _ -> Alcotest.fail "accepted unsafe candidate inspection" in
  reject ["--max-inspect";"10001"];
  reject ["--max-candidate-bytes";"0"];
  reject ["--uid";"7"];
  reject ["--evidence";"matching bytes"];
  reject ["--encoding";"utf8"];
  let config=parse (base @ ["--max-inspect";"17";
    "--max-candidate-bytes";"4096"]) in
  Alcotest.(check bool) "candidate command" true
    (config.command=Imap_cli.Inspect_append_candidates);
  Alcotest.(check int) "candidate budget" 17 config.max_inspect;
  Alcotest.(check int64) "body budget" 4096L
    config.max_candidate_bytes;
  Eio_main.run @@ fun eio ->
  let missing=Filename.temp_file "imap-cli-candidate-missing" ".db" in
  Sys.remove missing;
  let config=parse (base @ ["--db";missing]) in
  let code=Imap_cli.run config ~net:(Eio.Stdenv.net eio)
    ~fs:(Eio.Stdenv.fs eio)
    ~random:(Eio.Stdenv.secure_random eio) ~getenv:env in
  Alcotest.(check int) "missing DB rejected before connection" 5 code;
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
  let config=parse ["inspect";"--db";filename] in
  Alcotest.(check int) "empty database" 0
    (Imap_cli.run config ~net:(Eio.Stdenv.net eio) ~fs
      ~random:(Eio.Stdenv.secure_random eio) ~getenv:env);
  let maildir=Filename.temp_file "imap-cli-plan" "" in
  Sys.remove maildir;
  Fun.protect ~finally:(fun () ->
    let rec remove path=if Sys.is_directory path then (
      Sys.readdir path |> Array.iter (fun child ->
        remove (Filename.concat path child)); Unix.rmdir path)
      else Sys.remove path in
    remove maildir) @@ fun () ->
  ignore (Md.open_dir Eio.Path.(fs / maildir));
  let plan=parse ["plan-deletions";"--db";filename;
    "--maildir";maildir;"--propagate-deletions"] in
  Alcotest.(check int) "plan requires complete published inventory" 4
    (Imap_cli.run plan ~net:(Eio.Stdenv.net eio) ~fs
      ~random:(Eio.Stdenv.secure_random eio) ~getenv:env);
  let full=parse ["plan-sync";"--db";filename;
    "--maildir";maildir] in
  Alcotest.(check int) "full plan requires complete published inventory" 4
    (Imap_cli.run full ~net:(Eio.Stdenv.net eio) ~fs
      ~random:(Eio.Stdenv.secure_random eio) ~getenv:env)

let test_targeted_inspect () =
  let targeted=parse ["inspect";"--operation-id";"active"] in
  Alcotest.(check string) "target ID" "active" targeted.operation_id;
  (match Imap_cli.parse ~getenv:env
    [|"imap-sync";"sync";"--operation-id";"active"|] with
  | Error _ -> () | Ok _ -> Alcotest.fail "sync accepted operation ID");
  (match Imap_cli.parse ~getenv:env
    [|"imap-sync";"inspect";"--uid";"1"|] with
  | Error _ -> () | Ok _ -> Alcotest.fail "inspect accepted repair UID");
  (match Imap_cli.parse ~getenv:env
    [|"imap-sync";"inspect";"--operation-id";""|] with
  | Error _ -> () | Ok _ -> Alcotest.fail "empty ID fell back to list mode");
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
      (make id scope)) ["active";"committed";"rejected"];
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
    let config=parse ["inspect";"--db";filename;"--operation-id";id;
      "--max-inspect";"1"] in
    Imap_cli.run config ~net:(Eio.Stdenv.net eio) ~fs
      ~random:(Eio.Stdenv.secure_random eio) ~getenv:env in
  Alcotest.(check int) "active" 3 (inspect "active");
  Alcotest.(check int) "committed" 0 (inspect "committed");
  Alcotest.(check int) "rejected" 0 (inspect "rejected");
  Alcotest.(check int) "another scope hidden" 9 (inspect "foreign");
  Alcotest.(check int) "unknown ID" 9 (inspect "missing")

let test_sync_recovers_before_connect () =
  Eio_main.run @@ fun eio ->
  let root=Filename.temp_file "imap-cli-recover" "" in
  Sys.remove root;
  Unix.mkdir root 0o700;
  let rec remove path =
    if Sys.is_directory path then (
      Sys.readdir path |> Array.iter (fun name ->
        remove (Filename.concat path name));
      Unix.rmdir path)
    else Sys.remove path in
  Fun.protect ~finally:(fun () -> remove root) @@ fun () ->
  let maildir=Filename.concat root "maildir" in
  let fs=Eio.Stdenv.fs eio in
  ignore (Md.open_dir Eio.Path.(fs / maildir));
  let abandoned=Filename.concat (Filename.concat maildir "tmp")
    ".tmp-0123456789abcdef0123456789abcdef" in
  let output=open_out_bin abandoned in
  output_string output "abandoned";
  close_out output;
  let config=parse ["sync";"--host";"127.0.0.1";"--port";"1";
    "--tls";"plain";"--db";Filename.concat root "sync.db";
    "--maildir";maildir;"--blob-dir";Filename.concat root "blob";
    "--spool-dir";Filename.concat root "spool"] in
  ignore (Imap_cli.run config ~net:(Eio.Stdenv.net eio) ~fs
    ~random:(Eio.Stdenv.secure_random eio) ~getenv:env : int);
  Alcotest.(check bool) "startup removed abandoned Maildir tmp" false
    (Sys.file_exists abandoned)

let test_mark_local_retention () =
  Eio_main.run @@ fun eio ->
  let root=Filename.temp_file "imap-cli-retention" "" in
  Sys.remove root; Unix.mkdir root 0o700;
  let rec remove path =
    if Sys.is_directory path then (
      Sys.readdir path |> Array.iter (fun name ->
        remove (Filename.concat path name)); Unix.rmdir path)
    else Sys.remove path in
  Fun.protect ~finally:(fun () -> remove root) @@ fun () ->
  let fs=Eio.Stdenv.fs eio in
  let database=Filename.concat root "sync.db"
  and local_path=Filename.concat root "Maildir" in
  let maildir=Md.open_dir Eio.Path.(fs / local_path) in
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
  let config=parse ["mark-local-retention";"--db";database;
    "--maildir";local_path;"--pair-id";pair.id;
    "--evidence";"cache retention expired"] in
  Alcotest.(check int) "retention command succeeded" 0
    (Imap_cli.run config ~net:(Eio.Stdenv.net eio) ~fs
      ~random:(Eio.Stdenv.secure_random eio) ~getenv:env);
  let pair=Option.get (J.find_pair store ~id:pair.id) in
  Alcotest.(check bool) "durable retention tombstone" true
    (match pair.local_tombstone with
     | Some {reason=J.Retention;evidence="cache retention expired";_} -> true
     | _ -> false);
  Alcotest.(check bool) "retained absence blocks deletion" true
    (Imap.Sync_policy.plan_disappearance ~policy:Imap.Sync_policy.Propagate
      ~paired:true ~remote_present:true ~remote_complete:true
      ~local_present:false ~local_complete:true ~local_retained:true
      ~survivor_unchanged:true =
      Imap.Sync_policy.Hold_deletion Imap.Sync_policy.Retention_policy);
  ignore maildir

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
    Alcotest.test_case "mark local retention" `Quick
      test_mark_local_retention;
  ]]
