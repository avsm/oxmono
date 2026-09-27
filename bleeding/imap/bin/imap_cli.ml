type command = Sync | Hydrate | Audit_cache | Inspect | Inspect_append_candidates | Repair_appenduid
  | Repair_local_delete | Repair_local_append | Settle_flags
  | Mark_local_retention | Plan_deletions | Plan_sync | Verify_local
  | Reject_remote_delete
  | Finish_remote_delete
type config = {
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

let usage = {|Usage: imap-sync sync|hydrate|audit-cache|inspect|inspect-append-candidates|repair-appenduid|repair-local-delete|repair-local-append|settle-flags|mark-local-retention|plan-deletions|plan-sync|verify-local|reject-remote-delete|finish-remote-delete [options]

Shared: --endpoint ID --account ID --mailbox NAME --mailbox-key ID --db PATH
        --encoding rev1|utf8 (inspect only; sync uses negotiated encoding)
Sync:   --host HOST --port N --tls implicit|starttls|plain --user USER
        --password-env NAME --auth auto|cram-md5|plain|login
        --blob-dir PATH --maildir PATH --spool-dir PATH
        --max-transfers N --max-cycles N --min-absence-scans N
        --hydrate-bodies [--max-body-bytes N --max-total-bytes N]
        --propagate-deletions | --propagate-remote-deletions |
        --propagate-local-deletions --allow-bootstrap-duplicates
Hydrate: same connection and SQLite options, plus --blob-dir PATH
         --spool-dir PATH --max-transfers N --max-body-bytes N
         --max-total-bytes N (one pass; exit 2 if more bodies remain)
Audit cache: --blob-dir PATH --max-transfers N --max-total-bytes N
         [--after-uid N --expected-revision N]
         (offline; prints the next UID continuation and pinned revision)
Inspect: --max-inspect N [--operation-id ID]
Inspect APPEND candidates: --host HOST --port N --tls implicit|starttls|plain
         --user USER --password-env NAME --auth auto|cram-md5|plain|login
         --operation-id ID --spool-dir PATH --max-inspect N
         --max-candidate-bytes N (aggregate body reads, default 1 GiB)
Repair APPENDUID: --operation-id ID --uidvalidity N --uid N
         --evidence TEXT --maildir PATH --encoding rev1|utf8
Repair local deletion: --host HOST --port N --tls implicit|starttls|plain
         --user USER --password-env NAME --auth auto|cram-md5|plain|login
         --maildir PATH --operation-id ID --evidence TEXT
Repair local append: --host HOST --port N --tls implicit|starttls|plain
         --user USER --password-env NAME --auth auto|cram-md5|plain|login
         --blob-dir PATH --maildir PATH --spool-dir PATH
         --operation-id ID --evidence TEXT
Settle FLAGS: --host HOST --port N --tls implicit|starttls|plain
         --user USER --password-env NAME --auth auto|cram-md5|plain|login
         --maildir PATH --operation-id ID --evidence TEXT
Mark local retention: --maildir PATH --pair-id ID --evidence TEXT
         --encoding rev1|utf8 (offline mailbox scope)
Plan deletions: --maildir PATH --max-inspect N --min-absence-scans N
         [--propagate-deletions | --propagate-remote-deletions |
          --propagate-local-deletions]
Plan sync: --maildir PATH --max-inspect N --min-absence-scans N
         [--propagate-deletions | directional deletion flags]
         [--allow-bootstrap-duplicates]
Verify local content: --maildir PATH --max-inspect N
         --encoding rev1|utf8 (offline mailbox scope)
Reject remote delete: --host HOST --port N --tls implicit|starttls|plain
         --user USER --password-env NAME --auth auto|cram-md5|plain|login
         --maildir PATH --spool-dir PATH --operation-id ID --evidence TEXT
Finish remote delete: same connection, Maildir, spool and operation options;
         requires explicit operator evidence before targeted UID EXPUNGE

Defaults may be supplied as IMAP_HOST, IMAP_PORT, IMAP_TLS, IMAP_USER,
IMAP_PASSWORD_ENV, IMAP_AUTH, IMAP_ENDPOINT, IMAP_ACCOUNT, IMAP_MAILBOX,
IMAP_MAILBOX_KEY, IMAP_DB, IMAP_BLOB_DIR, IMAP_MAILDIR, IMAP_SPOOL_DIR.
The password is read from IMAP_PASSWORD by default; it cannot be an argument.
Exit codes: 0 complete, 2 more work, 3 pending, 4 conflicts, 5 config,
6 IMAP failure, 7 local storage failure, 8 Maildir writer busy,
9 requested operation not found in this scope.
|}

let ( let* ) r f = match r with Ok x -> f x | Error _ as e -> e

let redact secret message =
  if secret="" then message else
  let n=String.length secret and len=String.length message in
  let out=Buffer.create len in
  let rec loop i =
    if i>=len then Buffer.contents out
    else if i+n<=len && String.sub message i n=secret then (
      Buffer.add_string out "[REDACTED]"; loop (i+n))
    else (Buffer.add_char out message.[i]; loop (i+1)) in
  loop 0

let validate_evidence evidence =
  if String.trim evidence="" || String.length evidence>1024 ||
     not (String.for_all (fun c -> let n=Char.code c in
       n>=32 && n<>127) evidence) then
    Error "--evidence must be 1..1024 printable bytes"
  else Ok evidence

let parse ~getenv argv =
  let value env = ref (Option.value ~default:"" (getenv env)) in
  let host=value "IMAP_HOST" and user=value "IMAP_USER" in
  let port=value "IMAP_PORT" and tls=value "IMAP_TLS" in
  let auth=value "IMAP_AUTH" in
  let password_env=ref (Option.value ~default:"IMAP_PASSWORD"
    (getenv "IMAP_PASSWORD_ENV")) in
  let endpoint=value "IMAP_ENDPOINT"
  and account=value "IMAP_ACCOUNT"
  and mailbox=value "IMAP_MAILBOX"
  and mailbox_key=value "IMAP_MAILBOX_KEY"
  and db=value "IMAP_DB"
  and blob_dir=value "IMAP_BLOB_DIR"
  and maildir=value "IMAP_MAILDIR"
  and spool_dir=value "IMAP_SPOOL_DIR" in
  let encoding=ref "" and max_transfers=ref "100"
  and max_cycles=ref "1" and max_inspect=ref "100"
  and min_absence_scans=ref "0"
  and max_candidate_bytes=ref "1073741824"
  and max_body_bytes=ref "1073741824"
  and max_total_bytes=ref "1073741824" in
  let propagate_deletions=ref false
  and propagate_remote_deletions=ref false
  and propagate_local_deletions=ref false
  and hydrate_bodies=ref false
  and allow_bootstrap_duplicates=ref false in
  let fields=["--host",host;"--port",port;"--tls",tls;"--user",user;
    "--password-env",password_env;"--auth",auth;
    "--endpoint",endpoint;"--account",account;"--mailbox",mailbox;
    "--mailbox-key",mailbox_key;"--encoding",encoding;"--db",db;
    "--blob-dir",blob_dir;"--maildir",maildir;"--spool-dir",spool_dir;
    "--max-transfers",max_transfers;"--max-cycles",max_cycles;
    "--min-absence-scans",min_absence_scans;
    "--max-inspect",max_inspect;
    "--max-candidate-bytes",max_candidate_bytes;
    "--max-body-bytes",max_body_bytes;
    "--max-total-bytes",max_total_bytes] in
  let operation_id_arg=ref "" and operation_id_seen=ref false
  and pair_id_arg=ref ""
  and after_uid_arg=ref ""
  and expected_revision_arg=ref ""
  and candidate_bytes_seen=ref false
  and body_bytes_seen=ref false and total_bytes_seen=ref false
  and min_absence_seen=ref false
  and uidvalidity_arg=ref ""
  and uid_arg=ref "" and evidence_arg=ref "" in
  let fields=fields @ ["--operation-id",operation_id_arg;
    "--pair-id",pair_id_arg;
    "--after-uid",after_uid_arg;
    "--expected-revision",expected_revision_arg;
    "--uidvalidity",uidvalidity_arg;"--uid",uid_arg;
    "--evidence",evidence_arg] in
  let n=Array.length argv in
  let* command = if n<2 then Error "expected command"
    else match argv.(1) with
    | "sync" -> Ok Sync | "hydrate" -> Ok Hydrate
    | "audit-cache" -> Ok Audit_cache
    | "inspect" -> Ok Inspect
    | "inspect-append-candidates" -> Ok Inspect_append_candidates
    | "repair-appenduid" -> Ok Repair_appenduid
    | "repair-local-delete" -> Ok Repair_local_delete
    | "repair-local-append" -> Ok Repair_local_append
    | "settle-flags" -> Ok Settle_flags
    | "mark-local-retention" -> Ok Mark_local_retention
    | "plan-deletions" -> Ok Plan_deletions
    | "plan-sync" -> Ok Plan_sync
    | "verify-local" -> Ok Verify_local
    | "reject-remote-delete" -> Ok Reject_remote_delete
    | "finish-remote-delete" -> Ok Finish_remote_delete
    | _ -> Error "unknown IMAP command" in
  let rec options i =
    if i>=n then Ok ()
    else if argv.(i)="--propagate-deletions" then
      (propagate_deletions:=true; options (i+1))
    else if argv.(i)="--propagate-remote-deletions" then
      (propagate_remote_deletions:=true; options (i+1))
    else if argv.(i)="--propagate-local-deletions" then
      (propagate_local_deletions:=true; options (i+1))
    else if argv.(i)="--allow-bootstrap-duplicates" then
      (allow_bootstrap_duplicates:=true; options (i+1))
    else if argv.(i)="--hydrate-bodies" then
      (hydrate_bodies:=true; options (i+1))
    else match List.assoc_opt argv.(i) fields with
    | None -> Error "unknown option"
    | Some dst when i+1>=n ||
        (String.length argv.(i+1)>=2 &&
         String.sub argv.(i+1) 0 2="--") ->
        Error ("missing value for " ^ argv.(i))
    | Some dst ->
        if argv.(i)="--operation-id" then operation_id_seen:=true;
        if argv.(i)="--max-candidate-bytes" then
          candidate_bytes_seen:=true;
        if argv.(i)="--max-body-bytes" then body_bytes_seen:=true;
        if argv.(i)="--max-total-bytes" then total_bytes_seen:=true;
        if argv.(i)="--min-absence-scans" then
          min_absence_seen:=true;
        dst:=argv.(i+1); options (i+2) in
  let* ()=options 2 in
  let required label v = if v="" then Error ("missing " ^ label) else Ok v in
  let* endpoint=required "--endpoint" !endpoint in
  let* account=required "--account" !account in
  let* mailbox=required "--mailbox" !mailbox in
  let* db=required "--db" !db in
  let mailbox_key=if !mailbox_key="" then mailbox else !mailbox_key in
  let positive bound label s =
    match int_of_string_opt s with
    | Some n when n>=1 && n<=bound -> Ok n
    | _ -> Error (label ^ " must be between 1 and " ^ string_of_int bound) in
  let identifier of_int64 label s =
    match Option.map of_int64 (Int64.of_string_opt s) with
    | Some (Ok value) -> Ok value
    | _ -> Error (label ^ " must be between 1 and 4294967295") in
  let* max_transfers=positive 10000 "--max-transfers" !max_transfers in
  let* max_cycles=positive 100000 "--max-cycles" !max_cycles in
  let* max_inspect=positive 10000 "--max-inspect" !max_inspect in
  let* min_absence_scans=match int_of_string_opt !min_absence_scans with
    | Some n when n>=0 && n<=100000 -> Ok n
    | _ -> Error "--min-absence-scans must be between 0 and 100000" in
  let* max_candidate_bytes=match Int64.of_string_opt !max_candidate_bytes with
    | Some n when n>=1L && n<=1_099_511_627_776L -> Ok n
    | _ -> Error "--max-candidate-bytes must be between 1 and 1099511627776" in
  let bounded_bytes label value =
    match Int64.of_string_opt value with
    | Some n when n>=1L && n<=1_099_511_627_776L -> Ok n
    | _ -> Error (label ^ " must be between 1 and 1099511627776") in
  let* max_body_bytes=bounded_bytes "--max-body-bytes" !max_body_bytes in
  let* max_total_bytes=bounded_bytes "--max-total-bytes" !max_total_bytes in
  let* after_uid=if !after_uid_arg="" then Ok None else
    let* uid=identifier Imap.Uid.of_int64 "--after-uid" !after_uid_arg in
    Ok (Some uid) in
  let* expected_revision=if !expected_revision_arg="" then Ok None else
    match Int64.of_string_opt !expected_revision_arg with
    | Some revision when revision>=0L -> Ok (Some revision)
    | _ -> Error "--expected-revision must be nonnegative" in
  let* port=match !port with
    | "" -> Ok None
    | s -> (match int_of_string_opt s with
      | Some n when n>=1 && n<=65535 -> Ok (Some n)
      | _ -> Error "--port must be between 1 and 65535") in
  let* tls=match String.lowercase_ascii !tls with
    | "" | "implicit" -> Ok `Implicit
    | "starttls" -> Ok `Required_starttls
    | "plain" -> Ok `Plain
    | _ -> Error "--tls must be implicit, starttls or plain" in
  let* mechanism=match String.lowercase_ascii !auth with
    | "" | "auto" -> Ok `Auto
    | "cram-md5" -> Ok `Cram_md5
    | "plain" -> Ok `Plain
    | "login" -> Ok `Login
    | _ -> Error "--auth must be auto, cram-md5, plain or login" in
  let encoding_explicit= !encoding<>"" in
  let* encoding=match String.lowercase_ascii !encoding with
    | "" | "rev1" -> Ok Imap.Mailbox_name.Rev1
    | "utf8" -> Ok Imap.Mailbox_name.Utf8
    | _ -> Error "--encoding must be rev1 or utf8" in
  let* host,username,password_env,blob_dir,maildir,spool_dir=
    match command with
    | Audit_cache | Inspect | Repair_appenduid | Mark_local_retention |
      Plan_deletions | Plan_sync | Verify_local ->
      let blob_dir=if command=Audit_cache && !blob_dir="" then
        db ^ ".blobs" else !blob_dir in
      Ok (!host,!user,!password_env,blob_dir,!maildir,!spool_dir)
    | Sync | Hydrate | Repair_local_delete | Repair_local_append | Settle_flags |
      Reject_remote_delete | Finish_remote_delete |
      Inspect_append_candidates ->
      let* host=required "--host" !host in
      let* username=required "--user" !user in
      let* password_env=required "--password-env" !password_env in
      let* maildir=if command=Inspect_append_candidates || command=Hydrate
        then Ok !maildir
        else required "--maildir" !maildir in
      let blob_dir=if !blob_dir="" then db ^ ".blobs" else !blob_dir in
      let spool_dir=if !spool_dir="" then db ^ ".spool" else !spool_dir in
      Ok (host,username,password_env,blob_dir,maildir,spool_dir) in
  let* operation_id,receipt_uidvalidity,receipt_uid,evidence=
    match command with
    | Sync | Hydrate | Audit_cache | Mark_local_retention | Plan_deletions | Plan_sync |
      Verify_local ->
        Ok ("",None,None,"")
    | Inspect -> Ok (!operation_id_arg,None,None,"")
    | Inspect_append_candidates ->
      let* operation_id=required "--operation-id" !operation_id_arg in
      Ok (operation_id,None,None,"")
    | Repair_appenduid ->
      let* operation_id=required "--operation-id" !operation_id_arg in
      let* evidence=required "--evidence" !evidence_arg in
      let* _=required "--maildir" maildir in
      let* evidence=validate_evidence evidence in
      let* epoch=identifier Imap.Uidvalidity.of_int64 "--uidvalidity"
        !uidvalidity_arg in
      let* uid=identifier Imap.Uid.of_int64 "--uid" !uid_arg in
      Ok (operation_id,Some epoch,Some uid,evidence)
    | Repair_local_delete | Repair_local_append | Settle_flags |
      Reject_remote_delete | Finish_remote_delete ->
      let* operation_id=required "--operation-id" !operation_id_arg in
      let* evidence=required "--evidence" !evidence_arg in
      let* evidence=validate_evidence evidence in
      Ok (operation_id,None,None,evidence) in
  let* pair_id=match command with
    | Mark_local_retention -> required "--pair-id" !pair_id_arg
    | _ when !pair_id_arg<>"" ->
        Error "--pair-id is supported only by mark-local-retention"
    | _ -> Ok "" in
  let* evidence=if command=Mark_local_retention then
    let* evidence=required "--evidence" !evidence_arg in
    validate_evidence evidence
    else Ok evidence in
  let* ()=if command=Mark_local_retention || command=Plan_deletions ||
      command=Plan_sync || command=Verify_local then
    let* _=required "--maildir" maildir in Ok () else Ok () in
  if (command=Sync || command=Hydrate || command=Repair_local_delete ||
      command=Repair_local_append || command=Settle_flags ||
      command=Reject_remote_delete || command=Finish_remote_delete ||
      command=Inspect_append_candidates) && encoding_explicit then
    Error "--encoding is determined by negotiated IMAP mode for online commands"
  else if command=Inspect && !operation_id_seen &&
    !operation_id_arg="" then
    Error "--operation-id must be non-empty"
  else if (command=Sync || command=Hydrate || command=Audit_cache) &&
      !operation_id_seen then
    Error "--operation-id is supported only by inspection or repair commands"
  else if (command=Mark_local_retention || command=Plan_deletions ||
      command=Plan_sync || command=Verify_local) &&
    !operation_id_seen then
    Error "--operation-id is not supported by this command"
  else if command<>Repair_appenduid &&
    (!uidvalidity_arg<>"" || !uid_arg<>"") then
    Error "APPENDUID-only option supplied to another command"
  else if (command=Sync || command=Hydrate || command=Audit_cache || command=Inspect ||
      command=Inspect_append_candidates || command=Plan_deletions ||
      command=Plan_sync || command=Verify_local) &&
      !evidence_arg<>"" then
    Error "repair-only option supplied to another command"
  else if command<>Sync && command<>Plan_deletions &&
      command<>Plan_sync &&
    (!propagate_deletions || !propagate_remote_deletions ||
     !propagate_local_deletions) then
    Error "deletion policy option supplied to an unrelated command"
  else if command<>Sync && command<>Plan_sync &&
    !allow_bootstrap_duplicates then
    Error "bootstrap policy option supplied to an unrelated command"
  else if !propagate_deletions &&
    (!propagate_remote_deletions || !propagate_local_deletions) then
    Error "--propagate-deletions cannot be combined with directional options"
  else if command<>Inspect_append_candidates && !candidate_bytes_seen then
    Error "candidate byte budget is only for inspect-append-candidates"
  else if command<>Hydrate && command<>Sync && !body_bytes_seen then
    Error "body byte budget is only for hydrate or sync"
  else if command<>Hydrate && command<>Sync && command<>Audit_cache &&
      !total_bytes_seen then
    Error "total byte budget is only for hydrate, audit-cache or sync"
  else if command=Sync && not !hydrate_bodies &&
      (!body_bytes_seen || !total_bytes_seen) then
    Error "body hydration byte budgets require --hydrate-bodies"
  else if command<>Sync && !hydrate_bodies then
    Error "--hydrate-bodies applies only to sync"
  else if command<>Audit_cache && !after_uid_arg<>"" then
    Error "--after-uid applies only to audit-cache"
  else if command<>Audit_cache && !expected_revision_arg<>"" then
    Error "--expected-revision applies only to audit-cache"
  else if command=Audit_cache &&
      ((after_uid=None) <> (expected_revision=None)) then
    Error "--after-uid and --expected-revision must be supplied together"
  else if command<>Sync && command<>Plan_deletions &&
      command<>Plan_sync && !min_absence_seen then
    Error "absence grace applies only to sync and deletion plans"
  else Ok {command;host;port;tls;username;password_env;mechanism;
    endpoint;account;mailbox;mailbox_key;encoding;encoding_explicit;
    db;blob_dir;maildir;
    spool_dir;max_transfers;min_absence_scans;max_cycles;max_inspect;
    max_candidate_bytes;max_body_bytes;max_total_bytes;
    hydrate_bodies= !hydrate_bodies;after_uid;expected_revision;
    propagate_deletions=
    !propagate_deletions;propagate_remote_deletions=
    !propagate_remote_deletions;propagate_local_deletions=
    !propagate_local_deletions;allow_bootstrap_duplicates=
    !allow_bootstrap_duplicates;operation_id;pair_id;receipt_uidvalidity;
    receipt_uid;evidence}

let id ~random prefix =
  let bytes=Cstruct.create 16 in
  Eio.Flow.read_exact random bytes;
  let hex=Buffer.create 32 in
  for i=0 to 15 do
    Buffer.add_string hex (Printf.sprintf "%02x" (Cstruct.get_uint8 bytes i))
  done;
  prefix ^ Buffer.contents hex

let scope config encoding =
  let raw_name=match Imap.Mailbox_name.encode ~mode:encoding config.mailbox with
    | Ok s -> s | Error message -> invalid_arg message in
  {Imap.Mirror.endpoint=config.endpoint;account=config.account;
   mailbox_key=config.mailbox_key;raw_name;encoding;mailbox_id=None}

let with_password config ~getenv f =
  match getenv config.password_env with
  | None | Some "" ->
      prerr_endline "missing non-empty password environment variable"; 5
  | Some password -> f password

(* The callback owns the connection only for this lexical scope. *)
let with_connected config ~sw ~net ~password f =
  let transport=Imap_eio.Transport.v ~net ~host:config.host
    ?port:config.port ~tls:config.tls () in
  let auth=Imap_eio.Auth.password ~username:config.username ~password
    ~mechanism:config.mechanism () in
  match Imap_eio.Client.connect ~sw ~auth transport with
  | Error _ -> prerr_endline "IMAP connection/authentication failed"; 6
  | Ok client ->
      Fun.protect ~finally:(fun () ->
        Eio.Cancel.protect (fun () -> Imap_eio.Client.close client)) @@ fun () ->
      f client (scope config (Imap_eio.Client.mailbox_mode client))

(* [with_context] builds the sync context for the connected client. The
   scope comes from the client's own encoding, so [Ctx.v] fails only on a
   mailbox name that cannot be encoded, which [scope] already refuses. *)
let with_context config ~sw ~net ~password ~store ~spool_dir ~next_id f =
  with_connected config ~sw ~net ~password @@ fun client scope ->
  match Imap_sync.Ctx.v ~client ~store ~scope ~mailbox:config.mailbox
      ~spool_dir ~next_id with
  | Ok ctx -> f ctx
  | Error error ->
      Format.eprintf "IMAP mailbox scope: %a@." Imap_sync.Error.pp error; 6

let local_scope config store =
  let initial=scope config config.encoding in
  if config.encoding_explicit then initial
  else
    try
      ignore (Imap_store.load_cursor store ~scope:initial);
      initial
    with Imap_store.Scope_mismatch ->
        let alternate=scope config Imap.Mailbox_name.Utf8 in
        ignore (Imap_store.load_cursor store ~scope:alternate);
        alternate

let string_of_kind = function
  | Imap_store.Journal.Append -> "append"
  | Local_append -> "local_append" | Copy -> "copy" | Move -> "move"
  | Flags -> "flags" | Delete -> "delete" | Local_delete -> "local_delete"
let string_of_state = function
  | Imap_store.Journal.Prepared -> "prepared"
  | Sent -> "sent" | Ambiguous -> "ambiguous" | Observed -> "observed"
  | Committed -> "committed" | Rejected -> "rejected"
let string_of_conflict = function
  | Imap_store.Journal.Flag_conflict -> "flags"
  | Identity_conflict -> "identity" | Content_conflict -> "content"
  | Delete_conflict -> "delete"
  | Policy_conflict -> "policy"
  | Deletion_hold -> "deletion_hold"

let operation_identity (op:Imap_store.Journal.operation) =
  let number f = function None -> "?" | Some value ->
    Int64.to_string (f value) in
  let epoch=number Imap.Uidvalidity.to_int64
    op.source_uidvalidity in
  let uid=number Imap.Uid.to_int64 op.source_uid in
  let receipt_epoch=number Imap.Uidvalidity.to_int64
    op.receipt_uidvalidity in
  let receipt_uid=number Imap.Uid.to_int64 op.receipt_uid in
  Printf.sprintf " source=%s/%s local=%S target=%s/%s sha256=%S length=%s"
    epoch uid (Option.value ~default:"" op.local_id)
    receipt_epoch receipt_uid
    (Option.value ~default:"" op.blob_sha256)
    (match op.blob_length with None -> "?" | Some n -> Int64.to_string n)

let operation_context store (op:Imap_store.Journal.operation) =
  let module J = Imap_store.Journal in
  let flags = function
    | None -> "?"
    | Some flags -> String.concat ","
        (List.map Mail_flag.Imap_flag.to_wire flags) in
  let tombstone = function
    | None -> "-"
    | Some (t:J.tombstone) ->
        let reason=match t.reason with
          | J.Inventory_absence -> "inventory_absence"
          | J.Expunge_receipt -> "expunge_receipt"
          | J.Local_absence -> "local_absence"
          | J.Explicit_delete -> "explicit_delete"
          | J.Retention -> "retention" in
        reason ^ ":" ^ t.evidence in
  let pair=Option.bind op.pair_id (fun id -> J.find_pair store ~id) in
  let saved_revision=match J.operation_pair_revision store ~id:op.id with
    | None -> "?" | Some n -> Int64.to_string n in
  let current_revision=match pair with
    | None -> "?" | Some pair -> Int64.to_string pair.revision in
  let remote_tombstone,local_tombstone=match pair with
    | None -> "?","?"
    | Some pair -> tombstone pair.remote_tombstone,
        tombstone pair.local_tombstone in
  Printf.sprintf
    " desired=%S local_preimage=%S pair_revision=%s/%s remote_tombstone=%S local_tombstone=%S receipt=%S"
    (flags op.desired_flags)
    (flags (if op.kind=J.Flags then J.local_flags_preimage store ~id:op.id
      else None))
    saved_revision current_revision remote_tombstone local_tombstone
    (Option.value ~default:"" op.receipt)

let print_sync receipt cycle =
  Printf.printf "cycle=%d revision=%Ld remote_to_local=%d local_to_remote=%d flags=%d deletions=%d flags_held=%d deletions_held=%d more=%b\n%!"
    cycle receipt.Imap_sync.Bridge.cursor.revision receipt.remote_to_local
    receipt.local_to_remote receipt.flags_updated receipt.deletions
    receipt.flags_held receipt.deletions_held receipt.more;
  if receipt.held_pair_ids<>[] then
    Printf.eprintf "held pair IDs (first %d): %s\n%!"
      (List.length receipt.held_pair_ids)
      (String.concat "," (List.map (Printf.sprintf "%S")
        receipt.held_pair_ids))

let deletion_policy config =
  if config.propagate_deletions ||
     config.propagate_remote_deletions &&
     config.propagate_local_deletions then Imap.Sync_policy.Propagate
  else if config.propagate_remote_deletions then
    Imap.Sync_policy.Propagate_remote
  else if config.propagate_local_deletions then
    Imap.Sync_policy.Propagate_local
  else Imap.Sync_policy.Preserve

let local_failure error =
  Format.eprintf "local filesystem or SQLite operation failed: %a@."
    Maildir.pp_error error;
  7

let with_maildir path f =
  match Maildir.open_dir path with
  | Ok maildir -> f maildir
  | Error error -> local_failure error

let hydrate config ~net ~fs ~random ~getenv =
  with_password config ~getenv @@ fun password ->
      let db=Eio.Path.(fs / config.db) in
      if not (Eio.Path.is_file db) then (
        prerr_endline "hydration requires an existing published SQLite store"; 5)
      else
        let blob_dir=Eio.Path.(fs / config.blob_dir) in
        let spool_dir=Eio.Path.(fs / config.spool_dir) in
        Eio.Path.mkdirs ~exists_ok:true ~perm:0o700 blob_dir;
        Eio.Path.mkdirs ~exists_ok:true ~perm:0o700 spool_dir;
        Eio.Switch.run @@ fun sw ->
        let store=Imap_store.open_path ~sw ~blob_dir db in
        with_context config ~sw ~net ~password ~store ~spool_dir
          ~next_id:(fun () -> id ~random "hydrate-") @@ fun ctx ->
            match Imap_sync.Engine.hydrate_once ~max_messages:config.max_transfers
              ~max_body_bytes:config.max_body_bytes
              ~max_total_bytes:config.max_total_bytes ~ctx () with
            | Ok receipt ->
                Printf.printf "hydrated=%d bytes=%Ld more=%b revision=%Ld\n%!"
                  receipt.hydrated receipt.bytes receipt.more
                  receipt.cursor.revision;
                if receipt.more then 2 else 0
            | Error error ->
                Printf.eprintf "IMAP hydration failed: %s\n%!"
                  (redact password (Imap_sync.Error.to_string error));
                6

let audit_cache config ~fs =
  let db=Eio.Path.(fs / config.db) in
  let blob_dir=Eio.Path.(fs / config.blob_dir) in
  if not (Eio.Path.is_file db) || not (Eio.Path.is_directory blob_dir) then (
    prerr_endline "cache audit requires an existing SQLite store and blob directory";
    5)
  else
    Eio.Switch.run @@ fun sw ->
    let store=Imap_store.open_path ~sw ~blob_dir db in
    let scope=local_scope config store in
    match Imap_sync.Engine.audit_cache_once ?after_uid:config.after_uid
      ?expected_revision:config.expected_revision
      ~max_messages:config.max_transfers
      ~max_total_bytes:config.max_total_bytes ~store ~scope () with
    | Ok receipt ->
        Printf.printf
          "cache_checked=%d invalidated=%d bytes=%Ld last_uid=%Ld more=%b revision=%Ld\n%!"
          receipt.checked receipt.invalidated receipt.bytes
          (match receipt.last_uid with None -> 0L
           | Some uid -> Imap.Uid.to_int64 uid)
          receipt.more receipt.cursor.revision;
        if receipt.more then 2 else 0
    | Error error ->
        Format.eprintf "cache audit failed: %a@." Imap_sync.Error.pp error;
        (match error with
         | Imap_sync.Error.Store_stale_revision
         | Imap_sync.Error.Incomplete _ -> 4
         | Imap_sync.Error.Limit _ -> 5
         | _ -> 7)

let sync config ~net ~fs ~random ~getenv =
  with_password config ~getenv @@ fun password ->
    let blob_dir=Eio.Path.(fs / config.blob_dir) in
    let spool_dir=Eio.Path.(fs / config.spool_dir) in
    Eio.Path.mkdirs ~exists_ok:true ~perm:0o700 blob_dir;
    Eio.Path.mkdirs ~exists_ok:true ~perm:0o700 spool_dir;
    Eio.Switch.run @@ fun sw ->
    let store=Imap_store.open_path ~sw ~blob_dir Eio.Path.(fs / config.db) in
    with_maildir Eio.Path.(fs / config.maildir) @@ fun maildir ->
    let recovered=match
        Imap_sync.Bridge.recover_local ~maildir ~spool_dir () with
      | Ok () -> true
      | Error _ -> false in
    if not recovered then (
      prerr_endline "Maildir writer lease is busy"; 8)
    else
    with_context config ~sw ~net ~password ~store ~spool_dir
      ~next_id:(fun () -> id ~random "op-") @@ fun ctx ->
      let scope=ctx.scope in
      let deletion_policy=deletion_policy config in
      let rec cycles cycle =
        let stage_id=id ~random "stage-" in
        match Imap_sync.Bridge.copy_once ~max_transfers:config.max_transfers
          ~min_absence_scans:config.min_absence_scans
          ~allow_bootstrap_duplicates:config.allow_bootstrap_duplicates
          ~deletion_policy ~ctx ~maildir ~stage_id () with
        | Ok receipt ->
          print_sync receipt cycle;
          if receipt.flags_held>0 || receipt.deletions_held>0 then 4
          else if not receipt.more then (
            match Imap_store.Journal.open_conflicts_page store ~scope
              ~limit:1 () with
            | [] when not config.hydrate_bodies -> 0
            | [] ->
                (match Imap_sync.Engine.hydrate_once
                    ~max_messages:config.max_transfers
                    ~max_body_bytes:config.max_body_bytes
                    ~max_total_bytes:config.max_total_bytes ~ctx () with
                 | Ok hydration ->
                     Printf.printf
                       "hydrated=%d bytes=%Ld more=%b revision=%Ld\n%!"
                       hydration.hydrated hydration.bytes hydration.more
                       hydration.cursor.revision;
                     if hydration.more then 2 else 0
                 | Error error ->
                     Printf.eprintf "IMAP hydration failed: %s\n%!"
                       (redact password (Imap_sync.Error.to_string error));
                     6)
            | _ -> prerr_endline "unresolved sync conflict; run inspect"; 4)
          else if cycle>=config.max_cycles then 2
          else cycles (cycle+1)
        | Error (Imap_sync.Error.Pending_operations ids) ->
          Printf.eprintf "pending journal operations=%d; run inspect\n%!"
            (List.length ids); 3
        | Error (Imap_sync.Error.Source_vanished uid) ->
          Printf.eprintf "remote UID %Ld vanished before archival; rescanning\n%!"
            (Imap.Uid.to_int64 uid);
          if cycle>=config.max_cycles then 2 else cycles (cycle+1)
        | Error (Imap_sync.Error.Local_source_changed id) ->
          Printf.eprintf "local occurrence %s changed before archival; rescanning\n%!" id;
          if cycle>=config.max_cycles then 2 else cycles (cycle+1)
        | Error Imap_sync.Error.Writer_busy ->
          prerr_endline "Maildir writer lease is busy"; 8
        | Error (Imap_sync.Error.Content_mismatch pair_id) ->
          Printf.eprintf "paired local content changed for %s; run inspect\n%!"
            pair_id; 4
        | Error (Imap_sync.Error.Maildir error) -> local_failure error
        | Error (Imap_sync.Error.Bootstrap_requires_pairing) ->
          prerr_endline "both endpoints contain unpaired messages; inspect before enabling --allow-bootstrap-duplicates"; 4
        | Error (Imap_sync.Error.Uidvalidity_changed |
            Imap_sync.Error.Content_diverged _ |
            Imap_sync.Error.Flags_diverged _ |
            Imap_sync.Error.Date_diverged _ |
            Imap_sync.Error.Store_stale_revision) ->
          prerr_endline "sync identity or content conflict; run inspect"; 4
        | Error (Imap_sync.Error.Invalid_configuration message) ->
          Printf.eprintf "configuration: %s\n%!" message; 5
        | Error (Imap_sync.Error.Invalid_operation _) ->
          prerr_endline "invalid journal operation; run inspect"; 3
        | Error _ ->
          prerr_endline
            "IMAP sync operation failed; run inspect for journal state";
          6 in
      cycles 1

let inspect config ~fs =
  Eio.Switch.run @@ fun sw ->
  let store=Imap_store.open_readonly ~sw Eio.Path.(fs / config.db) in
  let scope=local_scope config store in
  let cursor=Imap_store.load_cursor store ~scope in
  Printf.printf "cursor revision=%Ld generation=%Ld frontier=%Ld\n%!"
    cursor.revision cursor.generation cursor.frontier;
  (match Imap_store.object_identity store ~scope with
   | `Unbound -> ()
   | `Conflict -> print_endline "objectid binding names another mailbox"
   | `Bound (identity:Imap_store.object_identity) ->
       Printf.printf "objectid account=%S mailbox=%S\n%!"
         identity.account_id identity.mailbox_id);
  let print_operation (op:Imap_store.Journal.operation) =
    Printf.printf "operation id=%S kind=%s state=%s pair=%S%s%s\n%!"
      op.id (string_of_kind op.kind) (string_of_state op.state)
      (Option.value ~default:"" op.pair_id)
      (operation_identity op) (operation_context store op) in
  if config.operation_id<>"" then
    match Imap_store.Journal.find_operation store ~id:config.operation_id with
    | None ->
      Printf.eprintf "operation id=%S not found in requested scope\n%!"
        config.operation_id;
      9
    | Some op when op.scope<>scope ->
      Printf.eprintf "operation id=%S not found in requested scope\n%!"
        config.operation_id;
      9
    | Some op ->
      print_operation op;
      (match op.state with
       | Prepared | Sent | Ambiguous | Observed -> 3
       | Committed | Rejected -> 0)
  else (
  let remaining=ref config.max_inspect and count=ref 0 in
  let rec operations after =
    if !remaining>0 then (
      let limit=min 256 !remaining in
      let page=Imap_store.Journal.active_operations_page store ~scope ?after
        ~limit () in
      List.iter (fun (op:Imap_store.Journal.operation) ->
        print_operation op;
        incr count; decr remaining) page;
      if List.length page=limit then
        operations (Some (List.hd (List.rev page)).id)) in
  operations None;
  let conflict_count=ref 0 in
  let rec conflicts after =
    if !conflict_count<config.max_inspect then (
      let limit=min 256 (config.max_inspect - !conflict_count) in
      let page=Imap_store.Journal.open_conflicts_page store ~scope ?after
        ~limit () in
      List.iter (fun (conflict:Imap_store.Journal.conflict) ->
        Printf.printf "conflict id=%S kind=%s pair=%S revision=%Ld\n%!"
          conflict.id (string_of_conflict conflict.kind) conflict.pair_id
          conflict.pair_revision;
        incr conflict_count) page;
      if List.length page=limit then
        conflicts (Some (List.hd (List.rev page)).id)) in
  conflicts None;
  Printf.printf "active_operations_shown=%d open_conflicts_shown=%d capped=%b\n%!"
    !count !conflict_count
    (!remaining=0 || !conflict_count=config.max_inspect);
  if !conflict_count>0 then 4 else if !count>0 then 3 else 0)

let repair_appenduid config ~fs =
  let uidvalidity=Option.get config.receipt_uidvalidity
  and uid=Option.get config.receipt_uid in
  Eio.Switch.run @@ fun sw ->
  let db_path=Eio.Path.(fs / config.db) in
  if Eio.Path.kind ~follow:false db_path <> `Regular_file then (
    prerr_endline "existing regular SQLite database required for repair";
    5)
  else
  let store=Imap_store.open_path ~sw db_path in
  with_maildir Eio.Path.(fs / config.maildir) @@ fun maildir ->
  let scope=local_scope config store in
  match Imap_sync.Repair.record_appenduid ~store ~scope ~maildir
    ~id:config.operation_id ~uidvalidity ~uid ~evidence:config.evidence () with
  | Ok () ->
    prerr_endline "APPENDUID attestation recorded; run sync to verify body and flags";
    0
  | Error Imap_sync.Error.Writer_busy ->
    prerr_endline "Maildir writer lease is busy"; 8
  | Error (Imap_sync.Error.Invalid_operation _) ->
    prerr_endline "operation cannot accept APPENDUID evidence"; 3
  | Error (Imap_sync.Error.Maildir error) -> local_failure error
  | Error _ -> prerr_endline "APPENDUID evidence was not recorded"; 4

let spool_path config ~fs =
  let spool_dir=Eio.Path.(fs / config.spool_dir) in
  Eio.Path.mkdirs ~exists_ok:true ~perm:0o700 spool_dir;
  spool_dir

let mark_local_retention config ~fs =
  let db_path=Eio.Path.(fs / config.db) in
  let maildir_path=Eio.Path.(fs / config.maildir) in
  if Eio.Path.kind ~follow:false db_path<>`Regular_file ||
     not (Eio.Path.is_directory maildir_path) then (
    prerr_endline "existing SQLite database and Maildir required for retention";
    5)
  else Eio.Switch.run @@ fun sw ->
    let store=Imap_store.open_path ~sw db_path in
    with_maildir maildir_path @@ fun maildir ->
    let scope=local_scope config store in
    match Imap_sync.Repair.mark_local_retention ~store ~maildir ~scope
      ~pair_id:config.pair_id ~evidence:config.evidence
      ~spool_dir:(spool_path config ~fs) () with
    | Ok () ->
        Printf.printf "retention recorded for pair %S\n%!" config.pair_id; 0
    | Error Imap_sync.Error.Writer_busy ->
        prerr_endline "Maildir writer lease is busy"; 8
    | Error (Imap_sync.Error.Invalid_operation message) ->
        Printf.eprintf "retention rejected: %s\n%!" message; 4
    | Error (Imap_sync.Error.Maildir error) ->
        local_failure error
    | Error error ->
        Format.eprintf "retention failed: %a@." Imap_sync.Error.pp error; 4

let verify_local config ~fs ~random =
  let db_path=Eio.Path.(fs / config.db) in
  let maildir_path=Eio.Path.(fs / config.maildir) in
  if Eio.Path.kind ~follow:false db_path<>`Regular_file ||
     not (Eio.Path.is_directory maildir_path) then (
    prerr_endline "existing SQLite database and Maildir required for verification";
    5)
  else Eio.Switch.run @@ fun sw ->
    let store=Imap_store.open_path ~sw db_path in
    with_maildir maildir_path @@ fun maildir ->
    let scope=local_scope config store in
    let shown=ref [] and shown_count=ref 0 and issues=ref 0L in
    let on_issue pair_id reason=
      issues:=Int64.succ !issues;
      if !shown_count<config.max_inspect then (
        shown:=(pair_id,reason)::!shown;
        incr shown_count) in
    match Imap_sync.Bridge.verify_local_content ~store ~maildir ~scope
      ~next_id:(fun () -> id ~random "content-")
      ~spool_dir:(spool_path config ~fs) ~on_issue () with
    | Error Imap_sync.Error.Writer_busy ->
        prerr_endline "Maildir writer lease is busy"; 8
    | Error (Imap_sync.Error.Maildir error) ->
        local_failure error
    | Error error ->
        Format.eprintf "local verification failed: %a@."
          Imap_sync.Error.pp error; 4
    | Ok report ->
        List.iter (fun (pair_id,reason) ->
          Printf.printf "pair=%S issue=%S\n" pair_id reason)
          (List.rev !shown);
        Printf.printf "checked=%Ld mismatched=%Ld restored=%Ld missing=%Ld unverified=%Ld shown=%d capped=%b\n%!"
          report.checked report.mismatched report.restored report.missing
          report.unverified !shown_count
          (!issues>Int64.of_int config.max_inspect);
        if report.mismatched>0L || report.missing>0L ||
           report.unverified>0L then 4 else 0

let plan_deletions config ~fs =
  let db_path=Eio.Path.(fs / config.db) in
  let maildir_path=Eio.Path.(fs / config.maildir) in
  if Eio.Path.kind ~follow:false db_path<>`Regular_file ||
     not (Eio.Path.is_directory maildir_path) then (
    prerr_endline "existing SQLite database and Maildir required for plan";
    5)
  else Eio.Switch.run @@ fun sw ->
    let store=Imap_store.open_readonly ~sw db_path in
    with_maildir maildir_path @@ fun maildir ->
    let scope=local_scope config store in
    let count=ref 0 and shown=ref [] and shown_count=ref 0
    and candidate=ref 0
    and held=ref 0 and pending=ref 0 in
    let on_deletion (item:Imap_sync.Plan.deletion_preview) =
      incr count;
      if !shown_count<config.max_inspect then (
        shown:=item::!shown; incr shown_count);
      match item.decision with
      | `Plan (Imap.Sync_policy.Delete_local | Imap.Sync_policy.Delete_remote) ->
          incr candidate
      | `Pending _ -> incr pending
      | `Stale_epoch | `Plan (Imap.Sync_policy.Hold_deletion _ |
          Imap.Sync_policy.No_deletion) -> incr held in
    (* The deletion plan is the deletion case of the full plan. Bootstrap
       duplicates are allowed so that an unpaired populated bootstrap,
       which holds no deletion, does not stop the plan. *)
    let on_preview = function
      | Imap_sync.Plan.Preview_deletion item -> on_deletion item
      | Imap_sync.Plan.Preview_pending _ -> incr pending
      | _ -> () in
    match Imap_sync.Plan.preview_sync ~allow_bootstrap_duplicates:true
      ~min_absence_scans:config.min_absence_scans ~store ~maildir ~scope
      ~policy:(deletion_policy config) ~spool_dir:(spool_path config ~fs)
      ~on_preview () with
    | Error Imap_sync.Error.Writer_busy ->
        prerr_endline "Maildir writer lease is busy"; 8
    | Error (Imap_sync.Error.Maildir error) ->
        local_failure error
    | Error error ->
        Format.eprintf "deletion plan failed: %a@."
          Imap_sync.Error.pp error; 4
    | Ok cursor ->
        let presence = function None -> "unknown" | Some true -> "present"
          | Some false -> "absent" in
        let decision = function
          | `Pending id -> "pending:" ^ id
          | `Stale_epoch -> "hold:stale-uidvalidity"
          | `Plan Imap.Sync_policy.No_deletion -> "none"
          | `Plan Imap.Sync_policy.Delete_local -> "candidate:delete-local"
          | `Plan Imap.Sync_policy.Delete_remote -> "candidate:delete-remote"
          | `Plan (Imap.Sync_policy.Hold_deletion reason) ->
              "hold:" ^ (match reason with
                | Imap.Sync_policy.Incomplete_inventory -> "incomplete-inventory"
                | Imap.Sync_policy.Unpaired_identity -> "unpaired-identity"
                | Imap.Sync_policy.Survivor_changed -> "survivor-unverified"
                | Imap.Sync_policy.Preservation_policy -> "preserve-policy"
                | Imap.Sync_policy.Direction_policy -> "direction-policy"
                | Imap.Sync_policy.Retention_policy -> "local-retention"
                | Imap.Sync_policy.Unverified_absence -> "unverified-absence"
                | Imap.Sync_policy.Grace_period -> "grace-period"
                | Imap.Sync_policy.Missing_content_evidence ->
                    "no-content-evidence") in
        Printf.printf "published_revision=%Ld generation=%Ld; candidates require live revalidation\n"
          cursor.revision cursor.generation;
        List.iter (fun (item:Imap_sync.Plan.deletion_preview) ->
          Printf.printf "pair=%S remote_uid=%Ld remote=%s local_id=%S local=%s decision=%s\n"
            item.pair_id (Imap.Uid.to_int64 item.remote_uid)
            (presence item.remote_present) item.local_id
            (if item.local_present then "present" else "absent")
            (decision item.decision)) (List.rev !shown);
        Printf.printf "one_sided=%d candidate=%d held=%d pending=%d shown=%d capped=%b\n%!"
          !count !candidate !held !pending !shown_count
          (!count>config.max_inspect);
        0

let plan_sync config ~fs =
  let db_path=Eio.Path.(fs / config.db) in
  let maildir_path=Eio.Path.(fs / config.maildir) in
  if Eio.Path.kind ~follow:false db_path<>`Regular_file ||
     not (Eio.Path.is_directory maildir_path) then (
    prerr_endline "existing SQLite database and Maildir required for plan";
    5)
  else Eio.Switch.run @@ fun sw ->
    let store=Imap_store.open_readonly ~sw db_path in
    with_maildir maildir_path @@ fun maildir ->
    let scope=local_scope config store in
    let count=ref 0 and shown=ref [] and shown_count=ref 0 in
    let remote_copies=ref 0 and local_copies=ref 0
    and flags=ref 0 and deletes=ref 0
    and holds=ref 0 and pending=ref 0 in
    let on_preview (item:Imap_sync.Plan.sync_preview) =
      incr count;
      if !shown_count<config.max_inspect then (
        shown:=item::!shown; incr shown_count);
      match item with
      | Imap_sync.Plan.Preview_copy_remote _ -> incr remote_copies
      | Imap_sync.Plan.Preview_copy_local _ -> incr local_copies
      | Imap_sync.Plan.Preview_flags _ -> incr flags
      | Imap_sync.Plan.Preview_pending _ -> incr pending
      | Imap_sync.Plan.Preview_bootstrap_hold |
        Imap_sync.Plan.Preview_pair_hold _ -> incr holds
      | Imap_sync.Plan.Preview_deletion item ->
          (match item.decision with
           | `Plan (Imap.Sync_policy.Delete_local |
               Imap.Sync_policy.Delete_remote) -> incr deletes
           | `Pending _ -> incr pending
           | `Stale_epoch | `Plan (Imap.Sync_policy.Hold_deletion _ |
               Imap.Sync_policy.No_deletion) -> incr holds) in
    match Imap_sync.Plan.preview_sync
      ~allow_bootstrap_duplicates:config.allow_bootstrap_duplicates
      ~min_absence_scans:config.min_absence_scans
      ~store ~maildir ~scope ~policy:(deletion_policy config)
      ~spool_dir:(spool_path config ~fs) ~on_preview () with
    | Error Imap_sync.Error.Writer_busy ->
        prerr_endline "Maildir writer lease is busy"; 8
    | Error (Imap_sync.Error.Maildir error) ->
        local_failure error
    | Error error ->
        Format.eprintf "sync plan failed: %a@."
          Imap_sync.Error.pp error; 4
    | Ok cursor ->
        let wires xs=String.concat ","
          (List.map Mail_flag.Imap_flag.to_wire xs) in
        let delta (x:Imap.Sync_policy.flag_delta) =
          Printf.sprintf "+[%s]-[%s]" (wires x.add) (wires x.remove) in
        let line = function
          | Imap_sync.Plan.Preview_pending id ->
              Printf.sprintf "pending operation=%S" id
          | Imap_sync.Plan.Preview_bootstrap_hold ->
              "hold: both endpoints have unpaired messages; bootstrap opt-in required"
          | Imap_sync.Plan.Preview_copy_remote uid ->
              Printf.sprintf "candidate:copy-remote uid=%Ld"
                (Imap.Uid.to_int64 uid)
          | Imap_sync.Plan.Preview_copy_local id ->
              Printf.sprintf "candidate:copy-local id=%S" id
          | Imap_sync.Plan.Preview_flags flags ->
              Printf.sprintf "candidate:flags pair=%S remote=%s local=%s"
                flags.pair_id (delta flags.to_remote)
                (delta flags.to_local)
          | Imap_sync.Plan.Preview_pair_hold (id,reason) ->
              Printf.sprintf "hold pair=%S reason=%S" id reason
          | Imap_sync.Plan.Preview_deletion item ->
              let decision=match item.decision with
                | `Pending id -> "pending:" ^ id
                | `Stale_epoch -> "hold:stale-uidvalidity"
                | `Plan Imap.Sync_policy.Delete_local ->
                    "candidate:delete-local"
                | `Plan Imap.Sync_policy.Delete_remote ->
                    "candidate:delete-remote"
                | `Plan Imap.Sync_policy.No_deletion -> "none"
                | `Plan (Imap.Sync_policy.Hold_deletion _) -> "hold:policy" in
              Printf.sprintf "pair=%S remote_uid=%Ld local_id=%S %s"
                item.pair_id (Imap.Uid.to_int64 item.remote_uid)
                item.local_id decision in
        Printf.printf "published_revision=%Ld generation=%Ld; all candidates require live revalidation\n"
          cursor.revision cursor.generation;
        List.iter (fun item -> Printf.printf "%s\n" (line item))
          (List.rev !shown);
        Printf.printf "events=%d copy_remote=%d copy_local=%d flags=%d delete=%d held=%d pending=%d shown=%d capped=%b\n%!"
          !count !remote_copies !local_copies !flags !deletes !holds
          !pending !shown_count (!count>config.max_inspect);
        0

let repair_local_delete config ~net ~fs ~random ~getenv =
  with_password config ~getenv @@ fun password ->
      let db_path=Eio.Path.(fs / config.db) in
      let maildir_path=Eio.Path.(fs / config.maildir) in
      if Eio.Path.kind ~follow:false db_path<>`Regular_file ||
         not (Eio.Path.is_directory maildir_path) then (
        prerr_endline "existing SQLite database and Maildir required for repair";
        5)
      else Eio.Switch.run @@ fun sw ->
      let store=Imap_store.open_path ~sw db_path in
      with_maildir maildir_path @@ fun maildir ->
      with_context config ~sw ~net ~password ~store
        ~spool_dir:Eio.Path.(fs / config.spool_dir)
        ~next_id:(fun () -> id ~random "op-") @@ fun ctx ->
          (try
            match Imap_sync.Repair.local_delete ~ctx ~maildir
                ~id:config.operation_id ~evidence:config.evidence () with
            | Ok (Imap_sync.Deletion.Deleted _) ->
                prerr_endline "local deletion repaired and committed"; 0
            | Ok _ ->
                prerr_endline "local deletion was not repaired"; 4
            | Error Imap_sync.Error.Writer_busy ->
                prerr_endline "Maildir writer lease is busy"; 8
            | Error (Imap_sync.Error.Client _) ->
                prerr_endline "IMAP verification failed; deletion unchanged"; 6
            | Error (Imap_sync.Error.Maildir error) ->
                local_failure error
            | Error error ->
                Format.eprintf "local deletion unchanged: %a@."
                  Imap_sync.Error.pp error; 4
           with Maildir.Writer_lock_busy _ ->
             prerr_endline "Maildir writer lease is busy"; 8)

let remote_delete_repair config ~finish ~net ~fs ~random ~getenv =
  with_password config ~getenv @@ fun password ->
      let db_path=Eio.Path.(fs / config.db) in
      let maildir_path=Eio.Path.(fs / config.maildir) in
      let spool_dir=Eio.Path.(fs / config.spool_dir) in
      if Eio.Path.kind ~follow:false db_path<>`Regular_file ||
         not (Eio.Path.is_directory maildir_path) ||
         not (Eio.Path.is_directory spool_dir) then (
        prerr_endline "existing SQLite database, Maildir and spool directory required for repair";
        5)
      else Eio.Switch.run @@ fun sw ->
      let store=Imap_store.open_path ~sw db_path in
      with_maildir maildir_path @@ fun maildir ->
      with_context config ~sw ~net ~password ~store ~spool_dir
        ~next_id:(fun () -> id ~random "op-") @@ fun ctx ->
          (try
            let result=if finish then
              (match Imap_sync.Repair.finish_remote_delete
                ~ctx ~maildir ~id:config.operation_id
                ~evidence:config.evidence () with
               | Ok (Imap_sync.Deletion.Deleted _) -> Ok ()
               | Ok _ -> Error (Imap_sync.Error.Diverged
                   "targeted UID EXPUNGE did not commit")
               | Error error -> Error error)
              else Imap_sync.Repair.reject_remote_delete
                ~ctx ~maildir ~id:config.operation_id
                ~evidence:config.evidence () in
            match result with
            | Ok () ->
                prerr_endline (if finish then
                  "targeted remote deletion committed" else
                  "unchanged remote target verified; pending deletion rejected");
                0
            | Error (Imap_sync.Error.Pending_operations _) ->
                prerr_endline "targeted deletion remains pending; run inspect";
                3
            | Error Imap_sync.Error.Writer_busy ->
                prerr_endline "Maildir writer lease is busy"; 8
            | Error (Imap_sync.Error.Client _) ->
                prerr_endline "IMAP verification failed; deletion remains pending";
                6
            | Error (Imap_sync.Error.Maildir error) ->
                local_failure error
            | Error error ->
                Format.eprintf "remote deletion remains pending: %a@."
                  Imap_sync.Error.pp error; 4
           with Maildir.Writer_lock_busy _ ->
             prerr_endline "Maildir writer lease is busy"; 8)

let repair_local_append config ~net ~fs ~random ~getenv =
  with_password config ~getenv @@ fun password ->
      let db_path=Eio.Path.(fs / config.db) in
      let maildir_path=Eio.Path.(fs / config.maildir) in
      let blob_dir=Eio.Path.(fs / config.blob_dir) in
      let spool_dir=Eio.Path.(fs / config.spool_dir) in
      if Eio.Path.kind ~follow:false db_path<>`Regular_file ||
         not (Eio.Path.is_directory maildir_path) ||
         not (Eio.Path.is_directory blob_dir) ||
         not (Eio.Path.is_directory spool_dir) then (
        prerr_endline "existing SQLite database, Maildir, blob and spool directories required for repair";
        5)
      else Eio.Switch.run @@ fun sw ->
      let store=Imap_store.open_path ~sw ~blob_dir db_path in
      with_maildir maildir_path @@ fun maildir ->
      with_context config ~sw ~net ~password ~store ~spool_dir
        ~next_id:(fun () -> id ~random "op-") @@ fun ctx ->
          match Imap_sync.Repair.local_append ~ctx ~maildir
              ~id:config.operation_id ~evidence:config.evidence () with
          | Ok () ->
              prerr_endline "local append repaired and committed"; 0
          | Error Imap_sync.Error.Writer_busy ->
              prerr_endline "Maildir writer lease is busy"; 8
          | Error (Imap_sync.Error.Client _) ->
              prerr_endline "IMAP verification failed; local append unchanged"; 6
          | Error (Imap_sync.Error.Invalid_operation _) ->
              prerr_endline "pending local append not found in this scope"; 9
          | Error (Imap_sync.Error.Maildir error) ->
              local_failure error
          | Error error ->
              Format.eprintf "local append unchanged: %a@."
                Imap_sync.Error.pp error; 4

let settle_flags config ~net ~fs ~random ~getenv =
  with_password config ~getenv @@ fun password ->
      let db_path=Eio.Path.(fs / config.db) in
      let maildir_path=Eio.Path.(fs / config.maildir) in
      if Eio.Path.kind ~follow:false db_path<>`Regular_file ||
         not (Eio.Path.is_directory maildir_path) then (
        prerr_endline "existing SQLite database and Maildir required for FLAGS settlement";
        5)
      else Eio.Switch.run @@ fun sw ->
      let store=Imap_store.open_path ~sw db_path in
      with_maildir maildir_path @@ fun maildir ->
      with_context config ~sw ~net ~password ~store
        ~spool_dir:Eio.Path.(fs / config.spool_dir)
        ~next_id:(fun () -> id ~random "op-") @@ fun ctx ->
          (try match Imap_sync.Repair.settle_flags ~ctx ~maildir
              ~id:config.operation_id ~evidence:config.evidence () with
           | Ok (Imap_sync.Flags.Updated _) ->
               prerr_endline "matching endpoint flags adopted; old intent rejected";
               0
           | Ok Imap_sync.Flags.Unchanged ->
               prerr_endline "FLAGS settlement made no change"; 4
           | Error Imap_sync.Error.Writer_busy ->
               prerr_endline "Maildir writer lease is busy"; 8
           | Error (Imap_sync.Error.Client _) ->
               prerr_endline "IMAP verification failed; FLAGS intent unchanged";
               6
           | Error Imap_sync.Error.No_pending_operation ->
               prerr_endline "pending FLAGS operation not found in this scope";
               9
           | Error (Imap_sync.Error.Maildir error) ->
               local_failure error
           | Error error ->
               Format.eprintf "FLAGS intent unchanged: %a@."
                 Imap_sync.Error.pp error; 4
           with Maildir.Writer_lock_busy _ ->
             prerr_endline "Maildir writer lease is busy"; 8)

let inspect_append_candidates config ~net ~fs ~random ~getenv =
  with_password config ~getenv @@ fun password ->
      let db_path=Eio.Path.(fs / config.db) in
      let spool_dir=Eio.Path.(fs / config.spool_dir) in
      if Eio.Path.kind ~follow:false db_path<>`Regular_file ||
         not (Eio.Path.is_directory spool_dir) then (
        prerr_endline "existing SQLite database and spool directory required";
        5)
      else Eio.Switch.run @@ fun sw ->
      let store=Imap_store.open_readonly ~sw db_path in
      with_context config ~sw ~net ~password ~store ~spool_dir
        ~next_id:(fun () -> id ~random "op-") @@ fun ctx ->
          match Imap_sync.Repair.inspect_append_candidates ~ctx
              ~id:config.operation_id ~max_uids:config.max_inspect
              ~max_body_bytes:config.max_candidate_bytes () with
          | Ok report ->
              Printf.printf "inspected %d UIDs in UIDVALIDITY %Ld\n%!"
                report.inspected_uids
                (Imap.Uidvalidity.to_int64 report.uidvalidity);
              List.iter (fun uid -> Printf.printf "candidate UID %Ld\n%!"
                (Imap.Uid.to_int64 uid)) report.matching_uids;
              prerr_endline
                "matching bytes do not attribute APPEND; independent APPENDUID evidence is required";
              0
          | Error (Imap_sync.Error.Invalid_operation _) ->
              prerr_endline "pending APPEND operation not found in this scope";
              9
          | Error (Imap_sync.Error.Client error) ->
              Format.eprintf "IMAP candidate inspection failed: %a@."
                Imap_eio.Client.pp_error error; 6
          | Error error ->
              Format.eprintf "APPEND candidate inspection failed: %a@."
                Imap_sync.Error.pp error; 4

let run config ~net ~fs ~random ~getenv =
  try match config.command with
    | Sync -> sync config ~net ~fs ~random ~getenv
    | Hydrate -> hydrate config ~net ~fs ~random ~getenv
    | Audit_cache -> audit_cache config ~fs
    | Inspect -> inspect config ~fs
    | Inspect_append_candidates ->
        inspect_append_candidates config ~net ~fs ~random ~getenv
    | Repair_appenduid -> repair_appenduid config ~fs
    | Mark_local_retention -> mark_local_retention config ~fs
    | Plan_deletions -> plan_deletions config ~fs
    | Plan_sync -> plan_sync config ~fs
    | Verify_local -> verify_local config ~fs ~random
    | Repair_local_delete ->
        repair_local_delete config ~net ~fs ~random ~getenv
    | Reject_remote_delete -> remote_delete_repair config ~finish:false
        ~net ~fs ~random ~getenv
    | Finish_remote_delete -> remote_delete_repair config ~finish:true
        ~net ~fs ~random ~getenv
    | Repair_local_append ->
        repair_local_append config ~net ~fs ~random ~getenv
    | Settle_flags -> settle_flags config ~net ~fs ~random ~getenv
  with
  | Eio.Cancel.Cancelled _ as exn -> raise exn
  | Invalid_argument message ->
      Printf.eprintf "configuration: %s\n%!" message; 5
  | exn ->
      let secret=Option.value ~default:"" (getenv config.password_env) in
      Printf.eprintf "local filesystem or SQLite operation failed: %s\n%!"
        (redact secret (Printexc.to_string exn));
      7
