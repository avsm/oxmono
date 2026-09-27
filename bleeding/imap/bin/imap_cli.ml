open Cmdliner

type scope = {
  endpoint : string;
  account : string;
  mailbox : string;
  mailbox_key : string;
  db : string;
}

type connection = {
  host : string;
  port : int option;
  tls : Imap_eio.Transport.tls;
  user : string;
  password_env : string;
  auth : Imap_eio.Auth.mechanism;
}

type budget = { max_body_bytes : int64; max_total_bytes : int64 }

type sync = {
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
}

type hydrate = {
  scope : scope;
  connection : connection;
  blob_dir : string;
  spool_dir : string;
  max_transfers : int;
  budget : budget;
}

type audit_cache = {
  scope : scope;
  encoding : Imap.Mailbox_name.mode option;
  blob_dir : string;
  max_transfers : int;
  max_total_bytes : int64;
  continuation : (Imap.Uid.t * int64) option;
}

type inspect = {
  scope : scope;
  encoding : Imap.Mailbox_name.mode option;
  max_inspect : int;
  operation_id : string option;
}

type append_candidates = {
  scope : scope;
  connection : connection;
  spool_dir : string;
  operation_id : string;
  max_inspect : int;
  max_candidate_bytes : int64;
}

type appenduid = {
  scope : scope;
  encoding : Imap.Mailbox_name.mode option;
  maildir : string;
  operation_id : string;
  uidvalidity : Imap.Uidvalidity.t;
  uid : Imap.Uid.t;
  evidence : string;
}

type repair = {
  scope : scope;
  connection : connection;
  maildir : string;
  spool_dir : string;
  operation_id : string;
  evidence : string;
}

type retention = {
  scope : scope;
  encoding : Imap.Mailbox_name.mode option;
  maildir : string;
  spool_dir : string;
  pair_id : string;
  evidence : string;
}

type plan = {
  scope : scope;
  encoding : Imap.Mailbox_name.mode option;
  maildir : string;
  spool_dir : string;
  max_inspect : int;
  min_absence_scans : int;
  deletion_policy : Imap.Sync_policy.deletion_policy;
  allow_bootstrap_duplicates : bool;
}

type verify_local = {
  scope : scope;
  encoding : Imap.Mailbox_name.mode option;
  maildir : string;
  spool_dir : string;
  max_inspect : int;
}

type gc = { db : string; blob_dir : string; maildir : string option }

type forget_epochs = {
  scope : scope;
  encoding : Imap.Mailbox_name.mode option;
}

type job =
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

let converged = 0
let more_work = 2
let pending = 3
let conflict = 4
let configuration = 5
let imap_failure = 6
let local_failure = 7
let busy = 8
let not_found = 9

let exits = [
  Cmd.Exit.info converged
    ~doc:"on convergence, or when a targeted operation is terminal.";
  Cmd.Exit.info more_work
    ~doc:"when bounded work remains. Run the command again.";
  Cmd.Exit.info pending
    ~doc:"when pending journal work needs operator inspection.";
  Cmd.Exit.info conflict
    ~doc:"on a conflict, a held change or an unsafe state.";
  Cmd.Exit.info configuration
    ~doc:"on invalid configuration, including a command line error.";
  Cmd.Exit.info imap_failure ~doc:"on an IMAP connection or protocol failure.";
  Cmd.Exit.info local_failure
    ~doc:"on a local filesystem, Maildir or SQLite failure.";
  Cmd.Exit.info busy
    ~doc:"when the Maildir writer lease, the Maildir metadata lock or the \
          database lock is busy.";
  Cmd.Exit.info not_found
    ~doc:"when the targeted operation or pair is not in the mailbox scope.";
  Cmd.Exit.info Cmd.Exit.internal_error
    ~doc:"on an unexpected internal error while parsing the command line.";
]

let error_code : Imap_sync.Error.t -> int = function
  | Client _ | Mirror _ | Incomplete _ | Conditional_store_unavailable
  | Permanent_flag_unavailable _ | Unsupported _ -> imap_failure
  | Maildir _ -> local_failure
  | Invalid_configuration _ | Limit _ -> configuration
  | Writer_busy -> busy
  | Pending_operations _ -> pending
  | No_pending_operation -> not_found
  | Source_vanished _ | Local_source_changed _ -> more_work
  | Store_stale_revision | Invalid_scope _ | Uidvalidity_changed
  | Missing_pair | Stale_pair | Missing_occurrence | Stale_inventory
  | Identity_changed | Modified | Bootstrap_requires_pairing
  | Content_mismatch _ | Content_diverged _ | Flags_diverged _
  | Date_diverged _ | Diverged _ | Invalid_operation _ -> conflict

let hint : Imap_sync.Error.t -> string = function
  | Pending_operations _ | Content_mismatch _ | Store_stale_revision
  | Identity_changed | Stale_pair | Modified -> "; run inspect"
  | Bootstrap_requires_pairing ->
      "; inspect both endpoints before enabling --allow-bootstrap-duplicates"
  | _ -> ""

exception Failed of int * string
exception Sync_failed of string * Imap_sync.Error.t
exception Lock_busy of string

let fail code fmt = Format.kasprintf (fun m -> raise (Failed (code, m))) fmt

let check what = function
  | Ok value -> value
  | Error error -> raise (Sync_failed (what, error))

let classify = function
  | Failed (code, message) -> code, message
  | Sync_failed (what, error) ->
      error_code error,
      Format.asprintf "%s: %a%s" what Imap_sync.Error.pp error (hint error)
  | Maildir.Writer_lock_busy path ->
      busy, "Maildir writer lease is busy: " ^ path
  | Maildir.Metadata_lock_busy path ->
      busy, "Maildir metadata lock is busy: " ^ path
  | Lock_busy path -> busy, "database lock is busy: " ^ path
  | Imap_store.Scope_mismatch ->
      configuration,
      "stored mailbox scope differs from the requested scope; check \
       --mailbox, --mailbox-key and --encoding"
  | Invalid_argument message ->
      local_failure, "local operation failed: " ^ message
  | exn ->
      local_failure,
      "local filesystem or SQLite operation failed: " ^ Printexc.to_string exn

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

let conv docv parse pp = Arg.Conv.make ~docv ~parser:parse ~pp ()

let int_range ~min ~max docv =
  conv docv (fun s -> match int_of_string_opt s with
    | Some n when n>=min && n<=max -> Ok n
    | _ -> Error (Printf.sprintf "%S is not an integer from %d to %d"
        s min max))
    Format.pp_print_int

let max_bytes = 1_099_511_627_776L
let gib = 1_073_741_824L

let bytes =
  conv "BYTES" (fun s -> match Int64.of_string_opt s with
    | Some n when n>=1L && n<=max_bytes -> Ok n
    | _ -> Error (Printf.sprintf "%S is not a byte count from 1 to %Ld" s
        max_bytes))
    (fun ppf n -> Format.fprintf ppf "%Ld" n)

let identifier docv of_int64 to_int64 =
  conv docv (fun s -> match Option.map of_int64 (Int64.of_string_opt s) with
    | Some (Ok value) -> Ok value
    | _ -> Error (Printf.sprintf "%S is not an integer from 1 to 4294967295" s))
    (fun ppf v -> Format.fprintf ppf "%Ld" (to_int64 v))

let uid = identifier "UID" Imap.Uid.of_int64 Imap.Uid.to_int64
let uidvalidity = identifier "UIDVALIDITY" Imap.Uidvalidity.of_int64
    Imap.Uidvalidity.to_int64

let revision =
  conv "REVISION" (fun s -> match Int64.of_string_opt s with
    | Some n when n>=0L -> Ok n
    | _ -> Error (Printf.sprintf "%S is not a nonnegative integer" s))
    (fun ppf n -> Format.fprintf ppf "%Ld" n)

let text docv =
  conv docv (fun s -> if s="" then Error "the value must not be empty"
    else Ok s) Format.pp_print_string

let evidence =
  conv "TEXT" (fun s ->
    if String.trim s="" || String.length s>1024 ||
       not (String.for_all (fun c -> let n=Char.code c in n>=32 && n<>127) s)
    then Error "evidence must be 1 to 1024 printable bytes"
    else Ok s) Format.pp_print_string

let keyword docv choices =
  let names=String.concat ", " (List.map fst choices) in
  conv docv (fun s ->
    match List.assoc_opt (String.lowercase_ascii s) choices with
    | Some v -> Ok v
    | None -> Error (Printf.sprintf "%S is not one of %s" s names))
    (fun ppf v -> Format.pp_print_string ppf
      (fst (List.find (fun (_,x) -> x=v) choices)))

let tls_choices : (string * Imap_eio.Transport.tls) list =
  ["implicit",`Implicit; "starttls",`Required_starttls; "plain",`Plain]
let auth_choices : (string * Imap_eio.Auth.mechanism) list =
  ["auto",`Auto; "cram-md5",`Cram_md5; "plain",`Plain; "login",`Login]
let encoding_choices =
  ["rev1",Imap.Mailbox_name.Rev1; "utf8",Imap.Mailbox_name.Utf8]
let policy_choices = Imap.Sync_policy.[
  "preserve",Preserve; "propagate",Propagate;
  "propagate-remote",Propagate_remote; "propagate-local",Propagate_local]

let s_scope = "MAILBOX SCOPE OPTIONS"
let s_connection = "CONNECTION OPTIONS"
let s_paths = "PATH OPTIONS"

let env = Cmd.Env.info

let required ?docs ?env names c ~doc =
  Arg.(required & opt (some c) None & info names ?docs ?env ~doc)

let optional ?docs ?env ?absent names c ~doc =
  Arg.(value & opt (some c) None & info names ?docs ?env ?absent ~doc)

open Term.Syntax

let db_t =
  required ["db"] (text "PATH") ~docs:s_scope ~env:(env "IMAP_DB")
    ~doc:"SQLite database holding the published inventory and the journal. \
          Its parent directory must exist."

let scope_t =
  let+ endpoint = required ["endpoint"] (text "ID") ~docs:s_scope
      ~env:(env "IMAP_ENDPOINT")
      ~doc:"Stable identifier of the IMAP server."
  and+ account = required ["account"] (text "ID") ~docs:s_scope
      ~env:(env "IMAP_ACCOUNT")
      ~doc:"Stable identifier of the account on the server."
  and+ mailbox = required ["mailbox"] (text "NAME") ~docs:s_scope
      ~env:(env "IMAP_MAILBOX") ~doc:"UTF-8 name of the mailbox."
  and+ mailbox_key = optional ["mailbox-key"] (text "ID") ~docs:s_scope
      ~env:(env "IMAP_MAILBOX_KEY") ~absent:"the mailbox name"
      ~doc:"Stable identifier of the mailbox across renames."
  and+ db = db_t in
  { endpoint; account; mailbox;
    mailbox_key=Option.value mailbox_key ~default:mailbox; db }

let encoding_t =
  optional ["encoding"] (keyword "ENCODING" encoding_choices) ~docs:s_scope
    ~absent:"rev1, retried once in utf8 when the stored scope differs"
    ~doc:"Mailbox name encoding of the stored scope, $(b,rev1) or $(b,utf8). \
          An explicit value pins it, and a stored scope in the other \
          encoding exits 5."

let connection_t =
  let+ host = required ["host"] (text "HOST") ~docs:s_connection
      ~env:(env "IMAP_HOST") ~doc:"IMAP server host name."
  and+ port = optional ["port"] (int_range ~min:1 ~max:65535 "PORT")
      ~docs:s_connection ~env:(env "IMAP_PORT")
      ~absent:"993 for implicit TLS and 143 otherwise"
      ~doc:"IMAP server port."
  and+ tls = Arg.(value & opt (keyword "MODE" tls_choices) `Implicit &
      info ["tls"] ~docs:s_connection ~env:(env "IMAP_TLS")
        ~doc:"Transport security. $(b,implicit) connects over TLS, \
              $(b,starttls) requires STARTTLS, and $(b,plain) sends \
              everything in clear text and permits any authentication \
              mechanism over it. Use $(b,plain) only for a trusted local \
              fixture.")
  and+ user = required ["user"] (text "USER") ~docs:s_connection
      ~env:(env "IMAP_USER") ~doc:"IMAP user name."
  and+ password_env = Arg.(value & opt (text "NAME") "IMAP_PASSWORD" &
      info ["password-env"] ~docs:s_connection ~env:(env "IMAP_PASSWORD_ENV")
        ~doc:"Environment variable holding the password. The password is \
              read only when the command runs and is never an argument.")
  and+ auth = Arg.(value & opt (keyword "MECHANISM" auth_choices) `Auto &
      info ["auth"] ~docs:s_connection ~env:(env "IMAP_AUTH")
        ~doc:"Authentication mechanism, one of $(b,auto), $(b,cram-md5), \
              $(b,plain) or $(b,login). $(b,auto) negotiates an advertised \
              mechanism.") in
  { host; port; tls; user; password_env; auth }

let path_opt names ~env:var ~absent ~doc =
  optional names (text "PATH") ~docs:s_paths ~env:(env var) ~absent ~doc

let blob_dir_t =
  path_opt ["blob-dir"] ~env:"IMAP_BLOB_DIR" ~absent:"$(i,DB).blobs"
    ~doc:"Content-addressed body cache belonging to the database alone."

let spool_dir_t =
  path_opt ["spool-dir"] ~env:"IMAP_SPOOL_DIR" ~absent:"$(i,DB).spool"
    ~doc:"Directory for provisional transfer and inventory files."

let maildir_t =
  required ["maildir"] (text "PATH") ~docs:s_paths ~env:(env "IMAP_MAILDIR")
    ~doc:"Maildir root paired with the mailbox."

let default_dir (s:scope) suffix = Option.value ~default:(s.db ^ suffix)

let max_transfers_t =
  Arg.(value & opt (int_range ~min:1 ~max:10000 "N") 100 &
    info ["max-transfers"]
      ~doc:"Transfers per cycle, or messages per pass, from 1 to 10000.")

let max_inspect_t ~default ~doc =
  Arg.(value & opt (int_range ~min:1 ~max:10000 "N") default &
    info ["max-inspect"] ~doc)

let max_inspect_shown =
  max_inspect_t ~default:100
    ~doc:"Items printed, from 1 to 10000. Counts cover every item."

let min_absence_scans_t =
  Arg.(value & opt (int_range ~min:0 ~max:100000 "N") 0 &
    info ["min-absence-scans"]
      ~doc:"Additional complete remote scans that must confirm an absence \
            before its survivor is deleted, from 0 to 100000.")

let deletion_policy_t =
  Arg.(value & opt (keyword "POLICY" policy_choices)
    Imap.Sync_policy.Preserve & info ["deletion-policy"]
      ~doc:"What a verified one-sided disappearance does. $(b,preserve) \
            holds it, $(b,propagate) deletes the survivor on either side, \
            $(b,propagate-remote) deletes a local survivor after a remote \
            disappearance, and $(b,propagate-local) deletes a remote \
            survivor after a local disappearance.")

let bootstrap_t =
  Arg.(value & flag & info ["allow-bootstrap-duplicates"]
    ~doc:"Import both populated sides of a new database without pairing \
          them. Messages are never paired because their bytes match.")

let total_bytes_t =
  Arg.(value & opt bytes gib & info ["max-total-bytes"]
    ~doc:"Bytes read in one pass, from 1 to 1099511627776.")

let budget_t =
  let+ max_body_bytes = Arg.(value & opt bytes gib & info ["max-body-bytes"]
      ~doc:"Largest body hydrated, from 1 to 1099511627776.")
  and+ max_total_bytes = total_bytes_t in
  { max_body_bytes; max_total_bytes }

let sync_hydration_t =
  let absent="1073741824, with $(b,--hydrate-bodies)" in
  let t =
    let+ on = Arg.(value & flag & info ["hydrate-bodies"]
        ~doc:"After a converged cycle without held work or open conflicts, \
              run one bounded hydration of missing bodies.")
    and+ body = optional ["max-body-bytes"] bytes ~absent
        ~doc:"Largest body hydrated. Requires $(b,--hydrate-bodies)."
    and+ total = optional ["max-total-bytes"] bytes ~absent
        ~doc:"Bytes read by hydration. Requires $(b,--hydrate-bodies)." in
    match on,body,total with
    | false,None,None -> `Ok None
    | false,_,_ ->
        `Error (true, "--max-body-bytes and --max-total-bytes require \
                       --hydrate-bodies")
    | true,body,total ->
        `Ok (Some { max_body_bytes=Option.value body ~default:gib;
                    max_total_bytes=Option.value total ~default:gib }) in
  Term.ret t

let operation_id_t =
  required ["operation-id"] (text "ID")
    ~doc:"Journal operation to act on, as $(b,inspect) prints it."

let evidence_t =
  required ["evidence"] evidence
    ~doc:"Operator evidence saved in the journal, 1 to 1024 printable \
          bytes."

let password_envs =
  [Cmd.Env.info "IMAP_PASSWORD"
     ~doc:"The IMAP password, unless $(b,--password-env) names another \
           variable."]

let command ?(online=false) name ~doc ~man term =
  let envs=if online then password_envs else [] in
  Cmd.v (Cmd.info name ~doc ~man ~exits ~envs) term

let sync_cmd =
  let term =
    let+ scope = scope_t and+ connection = connection_t
    and+ blob_dir = blob_dir_t and+ maildir = maildir_t
    and+ spool_dir = spool_dir_t and+ max_transfers = max_transfers_t
    and+ max_cycles = Arg.(value & opt (int_range ~min:1 ~max:100000 "N") 1
        & info ["max-cycles"] ~doc:"Bridge cycles, from 1 to 100000.")
    and+ min_absence_scans = min_absence_scans_t
    and+ deletion_policy = deletion_policy_t
    and+ allow_bootstrap_duplicates = bootstrap_t
    and+ hydrate_bodies = sync_hydration_t in
    Sync { scope; connection; maildir; max_transfers; max_cycles;
      min_absence_scans; deletion_policy; allow_bootstrap_duplicates;
      hydrate_bodies;
      blob_dir=default_dir scope ".blobs" blob_dir;
      spool_dir=default_dir scope ".spool" spool_dir } in
  command ~online:true "sync" term
    ~doc:"Run bounded IMAP and Maildir bridge cycles."
    ~man:[`S Manpage.s_description;
      `P "Recovers interrupted Maildir and spool files, removes orphan \
          body blobs, then runs up to $(b,--max-cycles) cycles. Each cycle \
          publishes a complete remote inventory and copies, flags and \
          deletes under the deletion policy. The command holds the \
          database lock for its whole run."]

let hydrate_cmd =
  let term =
    let+ scope = scope_t and+ connection = connection_t
    and+ blob_dir = blob_dir_t and+ spool_dir = spool_dir_t
    and+ max_transfers = max_transfers_t and+ budget = budget_t in
    Hydrate { scope; connection; max_transfers; budget;
      blob_dir=default_dir scope ".blobs" blob_dir;
      spool_dir=default_dir scope ".spool" spool_dir } in
  command ~online:true "hydrate" term
    ~doc:"Fetch missing bodies of the published inventory."
    ~man:[`S Manpage.s_description;
      `P "Requires an existing database. Bodies larger than a budget are \
          skipped and counted, and do not make the exit status 2."]

let audit_cache_cmd =
  let continuation =
    let+ after = optional ["after-uid"] uid
        ~doc:"Continue after this UID. Requires $(b,--expected-revision)."
    and+ expected = optional ["expected-revision"] revision
        ~doc:"Published revision the continuation must still see. \
              Requires $(b,--after-uid)." in
    match after,expected with
    | None,None -> `Ok None
    | Some uid,Some revision -> `Ok (Some (uid,revision))
    | _ -> `Error (true, "--after-uid and --expected-revision must be \
                          supplied together") in
  let term =
    let+ scope = scope_t and+ encoding = encoding_t
    and+ blob_dir = blob_dir_t and+ max_transfers = max_transfers_t
    and+ max_total_bytes = total_bytes_t
    and+ continuation = Term.ret continuation in
    Audit_cache { scope; encoding; max_transfers; max_total_bytes;
      continuation; blob_dir=default_dir scope ".blobs" blob_dir } in
  command "audit-cache" term
    ~doc:"Rehash cached bodies and detach missing or corrupt ones."
    ~man:[`S Manpage.s_description;
      `P "Offline. Requires an existing database and blob directory."]

let inspect_cmd =
  let term =
    let+ scope = scope_t and+ encoding = encoding_t
    and+ max_inspect = max_inspect_shown
    and+ operation_id = optional ["operation-id"] (text "ID")
        ~doc:"Show this operation alone, including a terminal one." in
    Inspect { scope; encoding; max_inspect; operation_id } in
  command "inspect" term
    ~doc:"Print the cursor, active operations and open conflicts."
    ~man:[`S Manpage.s_description;
      `P "Read-only. With $(b,--operation-id), exits 3 for an active \
          operation, 0 for a terminal one and 9 when it is not in the \
          scope."]

let append_candidates_cmd =
  let term =
    let+ scope = scope_t and+ connection = connection_t
    and+ spool_dir = spool_dir_t and+ operation_id = operation_id_t
    and+ max_inspect = max_inspect_t ~default:100
        ~doc:"Widest UID range inspected, from 1 to 10000. A wider range \
              is refused."
    and+ max_candidate_bytes = Arg.(value & opt bytes gib &
        info ["max-candidate-bytes"]
          ~doc:"Aggregate body bytes read, from 1 to 1099511627776.") in
    Inspect_append_candidates { scope; connection; operation_id;
      max_inspect; max_candidate_bytes;
      spool_dir=default_dir scope ".spool" spool_dir } in
  command ~online:true "inspect-append-candidates" term
    ~doc:"List UIDs that could be a pending APPEND."
    ~man:[`S Manpage.s_description;
      `P "Read-only. Matching bytes never attribute an APPEND."]

let appenduid_cmd =
  let term =
    let+ scope = scope_t and+ encoding = encoding_t
    and+ maildir = maildir_t and+ operation_id = operation_id_t
    and+ uidvalidity = required ["uidvalidity"] uidvalidity
        ~doc:"UIDVALIDITY of the recovered APPENDUID."
    and+ uid = required ["uid"] uid ~doc:"UID of the recovered APPENDUID."
    and+ evidence = evidence_t in
    Repair_appenduid { scope; encoding; maildir; operation_id; uidvalidity;
      uid; evidence } in
  command "repair-appenduid" term
    ~doc:"Attest an APPENDUID recovered from a trusted record."
    ~man:[`S Manpage.s_description;
      `P "Offline. The next sync verifies the UID before pairing."]

let repair_t =
  let+ scope = scope_t and+ connection = connection_t
  and+ maildir = maildir_t and+ spool_dir = spool_dir_t
  and+ operation_id = operation_id_t and+ evidence = evidence_t in
  { scope; connection; maildir; operation_id; evidence;
    spool_dir=default_dir scope ".spool" spool_dir }

let repair_cmd name job ~doc =
  command ~online:true name Term.(const job $ repair_t) ~doc
    ~man:[`S Manpage.s_description;
      `P "Operator repair under the Maildir writer lease. It never runs \
          automatically."]

let local_append_cmd =
  let term =
    let+ repair = repair_t and+ blob_dir = blob_dir_t in
    Repair_local_append { repair;
      blob_dir=default_dir repair.scope ".blobs" blob_dir } in
  command ~online:true "repair-local-append" term
    ~doc:"Finish a remote-to-Maildir copy whose file is absent."
    ~man:[`S Manpage.s_description;
      `P "Operator repair under the Maildir writer lease. It never runs \
          automatically."]

let retention_cmd =
  let term =
    let+ scope = scope_t and+ encoding = encoding_t
    and+ maildir = maildir_t and+ spool_dir = spool_dir_t
    and+ pair_id = required ["pair-id"] (text "ID")
        ~doc:"Pair whose local occurrence was evicted."
    and+ evidence = evidence_t in
    Mark_local_retention { scope; encoding; maildir; pair_id; evidence;
      spool_dir=default_dir scope ".spool" spool_dir } in
  command "mark-local-retention" term
    ~doc:"Record that a local absence is retention, not deletion."
    ~man:[`S Manpage.s_description;
      `P "Offline. Later cycles never propagate this absence."]

let plan_t ~bootstrap =
  let+ scope = scope_t and+ encoding = encoding_t
  and+ maildir = maildir_t and+ spool_dir = spool_dir_t
  and+ max_inspect = max_inspect_shown
  and+ min_absence_scans = min_absence_scans_t
  and+ deletion_policy = deletion_policy_t
  and+ allow_bootstrap_duplicates = bootstrap in
  { scope; encoding; maildir; max_inspect; min_absence_scans;
    deletion_policy; allow_bootstrap_duplicates;
    spool_dir=default_dir scope ".spool" spool_dir }

let plan_deletions_cmd =
  let term =
    let+ plan = plan_t ~bootstrap:(Term.const true) in
    Plan_deletions plan in
  command "plan-deletions" term
    ~doc:"Preview one-sided pairs and their deletion decisions."
    ~man:[`S Manpage.s_description;
      `P "Offline, from the latest complete published inventory."]

let plan_sync_cmd =
  let term = let+ plan = plan_t ~bootstrap:bootstrap_t in Plan_sync plan in
  command "plan-sync" term
    ~doc:"Preview the copies, flag changes and deletions of a cycle."
    ~man:[`S Manpage.s_description;
      `P "Offline, from the latest complete published inventory."]

let verify_local_cmd =
  let term =
    let+ scope = scope_t and+ encoding = encoding_t
    and+ maildir = maildir_t and+ spool_dir = spool_dir_t
    and+ max_inspect = max_inspect_shown in
    Verify_local { scope; encoding; maildir; max_inspect;
      spool_dir=default_dir scope ".spool" spool_dir } in
  command "verify-local" term
    ~doc:"Rehash paired Maildir bodies against their saved digests."
    ~man:[`S Manpage.s_description; `P "Offline."]

let gc_cmd =
  let term =
    let+ db = db_t and+ blob_dir = blob_dir_t
    and+ maildir = path_opt ["maildir"] ~env:"IMAP_MAILDIR"
        ~absent:"no Maildir lease"
        ~doc:"Maildir whose writer lease is also held." in
    Gc { db; maildir;
      blob_dir=Option.value blob_dir ~default:(db ^ ".blobs") } in
  command "gc" term
    ~doc:"Remove body blobs that nothing references."
    ~man:[`S Manpage.s_description;
      `P "Holds the database lock, and the Maildir writer lease when \
          $(b,--maildir) is given. Every other writer of the blob \
          directory must be stopped, including one in another process \
          that does not take these locks."]

let forget_epochs_cmd =
  let term =
    let+ scope = scope_t and+ encoding = encoding_t in
    Forget_epochs { scope; encoding } in
  command "forget-epochs" term
    ~doc:"Drop the snapshots of every UIDVALIDITY but the current one."
    ~man:[`S Manpage.s_description;
      `P "Offline. Blobs referenced only by a dropped epoch become \
          orphans for $(b,gc). Exits 4 when the cursor changes \
          concurrently."]

let cmd =
  let man = [
    `S Manpage.s_description;
    `P "$(tool) keeps an IMAP mailbox and a Maildir in step through a \
        SQLite journal. Each invocation does bounded work and exits, so a \
        scheduler can invoke it again with its own backoff.";
    `P "Options take their defaults from the environment variables listed \
        with each command. The password is read from the variable that \
        $(b,--password-env) names, only by a command that connects.";
    `P "A nonzero exit never authorizes replaying a possibly sent APPEND \
        or deletion.";
  ] in
  Cmd.group (Cmd.info "imap-sync" ~doc:"Bounded IMAP and Maildir sync"
      ~man ~exits) [
    sync_cmd; hydrate_cmd; audit_cache_cmd; inspect_cmd;
    append_candidates_cmd; appenduid_cmd;
    repair_cmd "repair-local-delete" (fun r -> Repair_local_delete r)
      ~doc:"Finish a pending local deletion whose file still exists.";
    local_append_cmd;
    repair_cmd "settle-flags" (fun r -> Settle_flags r)
      ~doc:"Adopt flags an operator aligned on both endpoints.";
    repair_cmd "reject-remote-delete" (fun r -> Reject_remote_delete r)
      ~doc:"Reject a pending remote deletion whose target is unchanged.";
    repair_cmd "finish-remote-delete" (fun r -> Finish_remote_delete r)
      ~doc:"Expunge the one UID of a pending remote deletion.";
    retention_cmd; plan_deletions_cmd; plan_sync_cmd; verify_local_cmd;
    gc_cmd; forget_epochs_cmd;
  ]

let id ~random prefix =
  let bytes=Cstruct.create 16 in
  Eio.Flow.read_exact random bytes;
  let hex=Buffer.create 32 in
  for i=0 to 15 do
    Buffer.add_string hex (Printf.sprintf "%02x" (Cstruct.get_uint8 bytes i))
  done;
  prefix ^ Buffer.contents hex

let path ~fs name = Eio.Path.(fs / name)

let require_db ~fs name =
  let p=path ~fs name in
  if Eio.Path.kind ~follow:true p<>`Regular_file then
    fail configuration "existing SQLite database required: %s" name;
  p

let require_dir ~fs what name =
  let p=path ~fs name in
  if not (Eio.Path.is_directory p) then
    fail configuration "existing %s required: %s" what name;
  p

let make_dir ~fs name =
  let p=path ~fs name in
  Eio.Path.mkdirs ~exists_ok:true ~perm:0o700 p;
  p

let open_maildir p =
  match Maildir.open_dir p with
  | Ok maildir -> maildir
  | Error e -> raise (Sync_failed ("Maildir", Imap_sync.Error.Maildir e))

let with_db_lock ~fs db f =
  let name=db ^ ".lock" in
  Eio.Switch.run ~name:"imap-sync-lock" @@ fun sw ->
  let file=Eio.Path.open_out ~sw ~create:(`If_missing 0o600) (path ~fs name) in
  let fd=match Eio_unix.Resource.fd_opt file with
    | Some fd -> fd
    | None -> fail local_failure "database lock has no descriptor: %s" name in
  (* Eio has no record locks. *)
  (try Eio_unix.Fd.use_exn "lockf" fd (fun fd ->
     Unix.lockf fd Unix.F_TLOCK 0) with
   | Unix.Unix_error ((Unix.EACCES | Unix.EAGAIN), _, _) ->
       raise (Lock_busy name));
  f ()

let mirror_scope (s:scope) encoding =
  match Imap.Mailbox_name.encode ~mode:encoding s.mailbox with
  | Ok raw_name ->
      {Imap.Mirror.endpoint=s.endpoint; account=s.account;
       mailbox_key=s.mailbox_key; raw_name; encoding; mailbox_id=None}
  | Error message -> fail configuration "--mailbox: %s" message

let local_scope (s:scope) encoding store =
  let load mode =
    let scope=mirror_scope s mode in
    scope, Imap_store.load_cursor store ~scope in
  match encoding with
  | Some mode -> load mode
  | None ->
      try load Imap.Mailbox_name.Rev1
      with Imap_store.Scope_mismatch -> load Imap.Mailbox_name.Utf8

let password (c:connection) ~env ~secret =
  match env c.password_env with
  | None | Some "" ->
      fail configuration "missing non-empty password environment variable %s"
        c.password_env
  | Some password -> secret:=password; password

let with_context (c:connection) (s:scope) ~password ~sw ~net ~store
    ~spool_dir ~next_id f =
  let transport=Imap_eio.Transport.v ~net ~host:c.host ?port:c.port
      ~tls:c.tls () in
  let auth=try Imap_eio.Auth.password ~username:c.user ~password
      ~mechanism:c.auth ~allow_insecure_transport:(c.tls=`Plain) ()
    with Invalid_argument message ->
      fail configuration "invalid IMAP credentials: %s" message in
  let client=check "IMAP connection or authentication failed"
      (Result.map_error (fun e -> Imap_sync.Error.Client e)
        (Imap_eio.Client.connect ~sw ~auth transport)) in
  Fun.protect ~finally:(fun () ->
    Eio.Cancel.protect (fun () -> Imap_eio.Client.close client)) @@ fun () ->
  let scope=mirror_scope s (Imap_eio.Client.mailbox_mode client) in
  f (check "IMAP mailbox scope" (Imap_sync.Ctx.v ~client ~store ~scope
    ~mailbox:s.mailbox ~spool_dir ~next_id))

let find_operation store ~scope id =
  match Imap_store.Journal.find_operation store ~id with
  | Some op when op.scope=scope -> op
  | _ -> fail not_found "operation id=%S not found in requested scope" id

let last page = List.nth page (List.length page - 1)

(* [bounded_pages ~max ~page ~id print] prints the first [max] items and is
   [(shown, truncated)], where [truncated] holds when more than [max]
   items exist. *)
let bounded_pages ~max ~page ~id print =
  let rec go after seen =
    if seen>max then seen else
    let limit=min 256 (max+1-seen) in
    let items=page after ~limit in
    let seen=List.fold_left (fun seen item ->
      if seen<max then print item; seen+1) seen items in
    if List.length items<limit then seen
    else go (Some (id (last items))) seen in
  let seen=go None 0 in
  min seen max, seen>max

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
    (" desired=%S local_preimage=%S pair_revision=%s/%s" ^^
     " remote_tombstone=%S local_tombstone=%S receipt=%S")
    (flags op.desired_flags)
    (flags (if op.kind=J.Flags then J.local_flags_preimage store ~id:op.id
      else None))
    saved_revision current_revision remote_tombstone local_tombstone
    (Option.value ~default:"" op.receipt)

let print_sync (receipt:Imap_sync.Bridge.receipt) cycle =
  Printf.printf ("cycle=%d revision=%Ld remote_to_local=%d " ^^
    "local_to_remote=%d flags=%d deletions=%d flags_held=%d " ^^
    "deletions_held=%d more=%b\n%!")
    cycle receipt.cursor.revision receipt.remote_to_local
    receipt.local_to_remote receipt.flags_updated receipt.deletions
    receipt.flags_held receipt.deletions_held receipt.more;
  if receipt.held_pair_ids<>[] then
    Printf.eprintf "held pair IDs (first %d): %s\n%!"
      (List.length receipt.held_pair_ids)
      (String.concat "," (List.map (Printf.sprintf "%S")
        receipt.held_pair_ids))

(* A pass that only skipped oversized bodies continues after them with
   unchanged budgets, so they cannot use up every invocation. [more]
   ignores UIDs that no pass with these budgets can hydrate. *)
let hydrate_bodies ~(ctx:Imap_sync.Ctx.t) ~max_transfers (budget:budget) =
  let rec pass after_uid skipped =
    let receipt=check "IMAP hydration failed"
        (Imap_sync.Engine.hydrate_once ?after_uid ~max_messages:max_transfers
          ~max_body_bytes:budget.max_body_bytes
          ~max_total_bytes:budget.max_total_bytes ~ctx ()) in
    let skipped=skipped @ receipt.skipped in
    match receipt.last_uid with
    | Some uid when receipt.more && receipt.hydrated=0 &&
        receipt.skipped<>[] -> pass (Some uid) skipped
    | last_uid ->
        let more=match last_uid with
          | Some uid when receipt.more && receipt.skipped<>[] ->
              (match Imap_store.Blob.missing_page ctx.store ~scope:ctx.scope
                  ~cursor:receipt.cursor ~after_uid:uid ~limit:1 () with
               | `Uids [] -> false
               | `Uids _ | `Stale_revision -> true)
          | _ -> receipt.more in
        receipt, skipped, more in
  let receipt,skipped,more=pass None [] in
  Printf.printf "hydrated=%d bytes=%Ld skipped=%d more=%b revision=%Ld\n%!"
    receipt.hydrated receipt.bytes (List.length skipped) more
    receipt.cursor.revision;
  if skipped<>[] then
    Printf.eprintf "UIDs over the byte budgets (first %d): %s\n%!"
      (min 100 (List.length skipped))
      (String.concat "," (List.filteri (fun i _ -> i<100)
        (List.map (fun u -> Int64.to_string (Imap.Uid.to_int64 u)) skipped)));
  if more then more_work else converged

let hydrate (c:hydrate) ~env ~secret ~net ~fs ~random =
  let password=password c.connection ~env ~secret in
  let db=require_db ~fs c.scope.db in
  let blob_dir=make_dir ~fs c.blob_dir and spool_dir=make_dir ~fs c.spool_dir in
  with_db_lock ~fs c.scope.db @@ fun () ->
  Eio.Switch.run @@ fun sw ->
  let store=Imap_store.open_path ~sw ~blob_dir db in
  with_context c.connection c.scope ~password ~sw ~net ~store ~spool_dir
    ~next_id:(fun () -> id ~random "hydrate-") @@ fun ctx ->
  hydrate_bodies ~ctx ~max_transfers:c.max_transfers c.budget

let audit_cache (c:audit_cache) ~fs =
  let db=require_db ~fs c.scope.db in
  let blob_dir=require_dir ~fs "blob directory" c.blob_dir in
  Eio.Switch.run @@ fun sw ->
  let store=Imap_store.open_path ~sw ~blob_dir db in
  let scope,_=local_scope c.scope c.encoding store in
  let receipt=check "cache audit failed"
      (Imap_sync.Engine.audit_cache_once
        ?after_uid:(Option.map fst c.continuation)
        ?expected_revision:(Option.map snd c.continuation)
        ~max_messages:c.max_transfers ~max_total_bytes:c.max_total_bytes
        ~store ~scope ()) in
  Printf.printf
    ("cache_checked=%d invalidated=%d bytes=%Ld last_uid=%Ld more=%b " ^^
     "revision=%Ld\n%!")
    receipt.checked receipt.invalidated receipt.bytes
    (match receipt.last_uid with None -> 0L
     | Some uid -> Imap.Uid.to_int64 uid)
    receipt.more receipt.cursor.revision;
  if receipt.more then more_work else converged

let reap store =
  let removed=ref 0 in
  Imap_store.Blob.reap_orphans_iter store ~removed:(fun _ -> incr removed);
  !removed

let sync (c:sync) ~env ~secret ~net ~fs ~random =
  let password=password c.connection ~env ~secret in
  let blob_dir=make_dir ~fs c.blob_dir and spool_dir=make_dir ~fs c.spool_dir in
  with_db_lock ~fs c.scope.db @@ fun () ->
  Eio.Switch.run @@ fun sw ->
  let store=Imap_store.open_path ~sw ~blob_dir (path ~fs c.scope.db) in
  let maildir=open_maildir (path ~fs c.maildir) in
  check "startup recovery"
    (Imap_sync.Bridge.recover_local ~maildir ~spool_dir ());
  let removed=Maildir.with_writer maildir (fun _ -> reap store) in
  if removed>0 then Printf.printf "orphans_removed=%d\n%!" removed;
  with_context c.connection c.scope ~password ~sw ~net ~store ~spool_dir
    ~next_id:(fun () -> id ~random "op-") @@ fun ctx ->
  let finish (receipt:Imap_sync.Bridge.receipt) =
    if receipt.flags_held>0 || receipt.deletions_held>0 then conflict
    else match Imap_store.Journal.open_conflicts_page store ~scope:ctx.scope
        ~limit:1 () with
    | _ :: _ -> prerr_endline "unresolved sync conflict; run inspect"; conflict
    | [] -> match c.hydrate_bodies with
      | None -> converged
      | Some budget ->
          hydrate_bodies ~ctx ~max_transfers:c.max_transfers budget in
  let rec cycles cycle =
    let again () =
      if cycle>=c.max_cycles then more_work else cycles (cycle+1) in
    match Imap_sync.Bridge.copy_once ~max_transfers:c.max_transfers
      ~min_absence_scans:c.min_absence_scans
      ~allow_bootstrap_duplicates:c.allow_bootstrap_duplicates
      ~deletion_policy:c.deletion_policy ~ctx ~maildir
      ~stage_id:(id ~random "stage-") () with
    | Ok receipt ->
        print_sync receipt cycle;
        if receipt.more then again () else finish receipt
    | Error (Imap_sync.Error.Source_vanished _
            | Imap_sync.Error.Local_source_changed _ as error) ->
        Format.eprintf "%a; rescanning@." Imap_sync.Error.pp error;
        again ()
    | Error error -> raise (Sync_failed ("sync", error)) in
  cycles 1

let inspect (c:inspect) ~fs =
  let db=require_db ~fs c.scope.db in
  Eio.Switch.run @@ fun sw ->
  let store=Imap_store.open_readonly ~sw db in
  let scope,cursor=local_scope c.scope c.encoding store in
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
  match c.operation_id with
  | Some id ->
      let op=find_operation store ~scope id in
      print_operation op;
      (match op.state with
       | Prepared | Sent | Ambiguous | Observed -> pending
       | Committed | Rejected -> converged)
  | None ->
      let operations,operations_capped=bounded_pages ~max:c.max_inspect
          ~page:(fun after ~limit ->
            Imap_store.Journal.active_operations_page store ~scope ?after
              ~limit ())
          ~id:(fun (op:Imap_store.Journal.operation) -> op.id)
          print_operation in
      let conflicts,conflicts_capped=bounded_pages ~max:c.max_inspect
          ~page:(fun after ~limit ->
            Imap_store.Journal.open_conflicts_page store ~scope ?after
              ~limit ())
          ~id:(fun (conflict:Imap_store.Journal.conflict) -> conflict.id)
          (fun (conflict:Imap_store.Journal.conflict) ->
            Printf.printf "conflict id=%S kind=%s pair=%S revision=%Ld\n%!"
              conflict.id (string_of_conflict conflict.kind)
              conflict.pair_id conflict.pair_revision) in
      Printf.printf
        "active_operations_shown=%d open_conflicts_shown=%d capped=%b\n%!"
        operations conflicts (operations_capped || conflicts_capped);
      if conflicts>0 then conflict else if operations>0 then pending
      else converged

let repair_appenduid (c:appenduid) ~fs =
  let db=require_db ~fs c.scope.db in
  let maildir_path=require_dir ~fs "Maildir" c.maildir in
  Eio.Switch.run @@ fun sw ->
  let store=Imap_store.open_path ~sw db in
  let maildir=open_maildir maildir_path in
  let scope,_=local_scope c.scope c.encoding store in
  ignore (find_operation store ~scope c.operation_id);
  check "APPENDUID evidence was not recorded"
    (Imap_sync.Repair.record_appenduid ~store ~scope ~maildir
      ~id:c.operation_id ~uidvalidity:c.uidvalidity ~uid:c.uid
      ~evidence:c.evidence ());
  prerr_endline
    "APPENDUID attestation recorded; run sync to verify body and flags";
  converged

let mark_local_retention (c:retention) ~fs =
  let db=require_db ~fs c.scope.db in
  let maildir_path=require_dir ~fs "Maildir" c.maildir in
  let spool_dir=make_dir ~fs c.spool_dir in
  Eio.Switch.run @@ fun sw ->
  let store=Imap_store.open_path ~sw db in
  let maildir=open_maildir maildir_path in
  let scope,_=local_scope c.scope c.encoding store in
  (match Imap_store.Journal.find_pair store ~id:c.pair_id with
   | Some pair when pair.scope=scope -> ()
   | _ -> fail not_found "pair id=%S not found in requested scope" c.pair_id);
  check "retention rejected"
    (Imap_sync.Repair.mark_local_retention ~store ~maildir ~scope
      ~pair_id:c.pair_id ~evidence:c.evidence ~spool_dir ());
  Printf.printf "retention recorded for pair %S\n%!" c.pair_id;
  converged

let verify_local (c:verify_local) ~fs ~random =
  let db=require_db ~fs c.scope.db in
  let maildir_path=require_dir ~fs "Maildir" c.maildir in
  let spool_dir=make_dir ~fs c.spool_dir in
  Eio.Switch.run @@ fun sw ->
  let store=Imap_store.open_path ~sw db in
  let maildir=open_maildir maildir_path in
  let scope,_=local_scope c.scope c.encoding store in
  let shown=ref [] and issues=ref 0 in
  let on_issue pair_id reason=
    if !issues<c.max_inspect then shown:=(pair_id,reason)::!shown;
    incr issues in
  let report=check "local verification failed"
      (Imap_sync.Bridge.verify_local_content ~store ~maildir ~scope
        ~next_id:(fun () -> id ~random "content-") ~spool_dir ~on_issue ()) in
  List.iter (fun (pair_id,reason) ->
    Printf.printf "pair=%S issue=%S\n" pair_id reason) (List.rev !shown);
  Printf.printf ("checked=%Ld mismatched=%Ld restored=%Ld missing=%Ld " ^^
    "unverified=%Ld shown=%d capped=%b\n%!")
    report.checked report.mismatched report.restored report.missing
    report.unverified (List.length !shown) (!issues>c.max_inspect);
  if report.mismatched>0L || report.missing>0L || report.unverified>0L then
    conflict
  else converged

let decision_label = function
  | `Pending id -> "pending:" ^ id
  | `Stale_epoch -> "hold:stale-uidvalidity"
  | `Plan Imap.Sync_policy.No_deletion -> "none"
  | `Plan Imap.Sync_policy.Delete_local -> "candidate:delete-local"
  | `Plan Imap.Sync_policy.Delete_remote -> "candidate:delete-remote"
  | `Plan (Imap.Sync_policy.Hold_deletion reason) ->
      "hold:" ^ (match reason with
        | Imap.Sync_policy.Incomplete_inventory -> "incomplete-inventory"
        | Unpaired_identity -> "unpaired-identity"
        | Survivor_changed -> "survivor-unverified"
        | Preservation_policy -> "preserve-policy"
        | Direction_policy -> "direction-policy"
        | Retention_policy -> "local-retention"
        | Unverified_absence -> "unverified-absence"
        | Grace_period -> "grace-period"
        | Missing_content_evidence -> "no-content-evidence")

type tally = {
  mutable events : int;
  mutable copy_remote : int;
  mutable copy_local : int;
  mutable flags : int;
  mutable delete : int;
  mutable held : int;
  mutable pending : int;
  mutable shown : Imap_sync.Plan.sync_preview list;
}

let count_deletion t (item:Imap_sync.Plan.deletion_preview) =
  match item.decision with
  | `Plan (Imap.Sync_policy.Delete_local | Delete_remote) ->
      t.delete<-t.delete+1
  | `Pending _ -> t.pending<-t.pending+1
  | `Stale_epoch | `Plan (Hold_deletion _ | No_deletion) ->
      t.held<-t.held+1

let count_preview t (item:Imap_sync.Plan.sync_preview) =
  match item with
  | Preview_copy_remote _ -> t.copy_remote<-t.copy_remote+1
  | Preview_copy_local _ -> t.copy_local<-t.copy_local+1
  | Preview_flags _ -> t.flags<-t.flags+1
  | Preview_pending _ -> t.pending<-t.pending+1
  | Preview_bootstrap_hold | Preview_pair_hold _ -> t.held<-t.held+1
  | Preview_deletion item -> count_deletion t item

(* [preview c ~fs ~keep ~count] runs the plan, tallying each event with
   [count] and keeping the first [c.max_inspect] events for which [keep]
   holds. *)
let preview (c:plan) ~fs ~keep ~count =
  let db=require_db ~fs c.scope.db in
  let maildir_path=require_dir ~fs "Maildir" c.maildir in
  let spool_dir=make_dir ~fs c.spool_dir in
  Eio.Switch.run @@ fun sw ->
  let store=Imap_store.open_readonly ~sw db in
  let maildir=open_maildir maildir_path in
  let scope,_=local_scope c.scope c.encoding store in
  let t={events=0; copy_remote=0; copy_local=0; flags=0; delete=0; held=0;
         pending=0; shown=[]} in
  let on_preview item =
    if keep item then (
      if t.events<c.max_inspect then t.shown<-item::t.shown;
      t.events<-t.events+1);
    count t item in
  let cursor=check "plan failed"
      (Imap_sync.Plan.preview_sync
        ~allow_bootstrap_duplicates:c.allow_bootstrap_duplicates
        ~min_absence_scans:c.min_absence_scans ~store ~maildir ~scope
        ~policy:c.deletion_policy ~spool_dir ~on_preview ()) in
  cursor,{t with shown=List.rev t.shown}

let presence = function
  | None -> "unknown" | Some true -> "present" | Some false -> "absent"

let plan_deletions (c:plan) ~fs =
  let keep = function Imap_sync.Plan.Preview_deletion _ -> true | _ -> false in
  let count t : Imap_sync.Plan.sync_preview -> unit = function
    | Preview_deletion item -> count_deletion t item
    | Preview_pending _ -> t.pending<-t.pending+1
    | _ -> () in
  let cursor,t=preview c ~fs ~keep ~count in
  Printf.printf ("published_revision=%Ld generation=%Ld; " ^^
    "candidates require live revalidation\n")
    cursor.revision cursor.generation;
  List.iter (function
    | Imap_sync.Plan.Preview_deletion item ->
        Printf.printf ("pair=%S remote_uid=%Ld remote=%s local_id=%S " ^^
          "local=%s decision=%s\n")
          item.pair_id (Imap.Uid.to_int64 item.remote_uid)
          (presence item.remote_present) item.local_id
          (if item.local_present then "present" else "absent")
          (decision_label item.decision)
    | _ -> ()) t.shown;
  Printf.printf ("one_sided=%d candidate=%d held=%d pending=%d shown=%d " ^^
    "capped=%b\n%!")
    t.events t.delete t.held t.pending (List.length t.shown)
    (t.events>c.max_inspect);
  converged

let plan_sync (c:plan) ~fs =
  let cursor,t=preview c ~fs ~keep:(fun _ -> true) ~count:count_preview in
  let wires xs=String.concat "," (List.map Mail_flag.Imap_flag.to_wire xs) in
  let delta (x:Imap.Sync_policy.flag_delta) =
    Printf.sprintf "+[%s]-[%s]" (wires x.add) (wires x.remove) in
  let line : Imap_sync.Plan.sync_preview -> string = function
    | Preview_pending id -> Printf.sprintf "pending operation=%S" id
    | Preview_bootstrap_hold ->
        "hold: both endpoints have unpaired messages; bootstrap opt-in required"
    | Preview_copy_remote uid ->
        Printf.sprintf "candidate:copy-remote uid=%Ld" (Imap.Uid.to_int64 uid)
    | Preview_copy_local id -> Printf.sprintf "candidate:copy-local id=%S" id
    | Preview_flags flags ->
        Printf.sprintf "candidate:flags pair=%S remote=%s local=%s"
          flags.pair_id (delta flags.to_remote) (delta flags.to_local)
    | Preview_pair_hold (id,reason) ->
        Printf.sprintf "hold pair=%S reason=%S" id reason
    | Preview_deletion item ->
        Printf.sprintf "pair=%S remote_uid=%Ld local_id=%S %s"
          item.pair_id (Imap.Uid.to_int64 item.remote_uid)
          item.local_id (decision_label item.decision) in
  Printf.printf ("published_revision=%Ld generation=%Ld; " ^^
    "all candidates require live revalidation\n")
    cursor.revision cursor.generation;
  List.iter (fun item -> print_endline (line item)) t.shown;
  Printf.printf ("events=%d copy_remote=%d copy_local=%d flags=%d " ^^
    "delete=%d held=%d pending=%d shown=%d capped=%b\n%!")
    t.events t.copy_remote t.copy_local t.flags t.delete t.held t.pending
    (List.length t.shown) (t.events>c.max_inspect);
  converged

let online_repair (r:repair) ~env ~secret ~net ~fs ~random ?blob_dir
    ~spool_required f =
  let password=password r.connection ~env ~secret in
  let db=require_db ~fs r.scope.db in
  let maildir_path=require_dir ~fs "Maildir" r.maildir in
  let blob_dir=Option.map (require_dir ~fs "blob directory") blob_dir in
  let spool_dir=if spool_required then
      require_dir ~fs "spool directory" r.spool_dir
    else path ~fs r.spool_dir in
  Eio.Switch.run @@ fun sw ->
  let store=Imap_store.open_path ~sw ?blob_dir db in
  let maildir=open_maildir maildir_path in
  with_context r.connection r.scope ~password ~sw ~net ~store ~spool_dir
    ~next_id:(fun () -> id ~random "op-") @@ fun ctx ->
  ignore (find_operation store ~scope:ctx.scope r.operation_id);
  f ~ctx ~maildir ~id:r.operation_id ~evidence:r.evidence

let repair_local_delete r ~env ~secret ~net ~fs ~random =
  online_repair r ~env ~secret ~net ~fs ~random ~spool_required:false
  @@ fun ~ctx ~maildir ~id ~evidence ->
  match check "local deletion unchanged"
      (Imap_sync.Repair.local_delete ~ctx ~maildir ~id ~evidence ()) with
  | Imap_sync.Deletion.Deleted _ ->
      prerr_endline "local deletion repaired and committed"; converged
  | _ -> prerr_endline "local deletion was not repaired"; conflict

let remote_delete_repair r ~finish ~env ~secret ~net ~fs ~random =
  online_repair r ~env ~secret ~net ~fs ~random ~spool_required:true
  @@ fun ~ctx ~maildir ~id ~evidence ->
  let what="remote deletion remains pending" in
  if finish then
    match check what (Imap_sync.Repair.finish_remote_delete ~ctx ~maildir
        ~id ~evidence ()) with
    | Imap_sync.Deletion.Deleted _ ->
        prerr_endline "targeted remote deletion committed"; converged
    | _ ->
        prerr_endline "targeted UID EXPUNGE did not commit; run inspect";
        conflict
  else (
    check what (Imap_sync.Repair.reject_remote_delete ~ctx ~maildir ~id
      ~evidence ());
    prerr_endline
      "unchanged remote target verified; pending deletion rejected";
    converged)

let repair_local_append r ~blob_dir ~env ~secret ~net ~fs ~random =
  online_repair r ~env ~secret ~net ~fs ~random ~blob_dir ~spool_required:true
  @@ fun ~ctx ~maildir ~id ~evidence ->
  check "local append unchanged"
    (Imap_sync.Repair.local_append ~ctx ~maildir ~id ~evidence ());
  prerr_endline "local append repaired and committed";
  converged

let settle_flags r ~env ~secret ~net ~fs ~random =
  online_repair r ~env ~secret ~net ~fs ~random ~spool_required:false
  @@ fun ~ctx ~maildir ~id ~evidence ->
  match check "FLAGS intent unchanged"
      (Imap_sync.Repair.settle_flags ~ctx ~maildir ~id ~evidence ()) with
  | Imap_sync.Flags.Updated _ ->
      prerr_endline "matching endpoint flags adopted; old intent rejected";
      converged
  | Imap_sync.Flags.Unchanged ->
      prerr_endline "FLAGS settlement made no change"; conflict

let inspect_append_candidates (c:append_candidates) ~env ~secret ~net ~fs
    ~random =
  let password=password c.connection ~env ~secret in
  let db=require_db ~fs c.scope.db in
  let spool_dir=require_dir ~fs "spool directory" c.spool_dir in
  Eio.Switch.run @@ fun sw ->
  let store=Imap_store.open_readonly ~sw db in
  with_context c.connection c.scope ~password ~sw ~net ~store ~spool_dir
    ~next_id:(fun () -> id ~random "op-") @@ fun ctx ->
  ignore (find_operation store ~scope:ctx.scope c.operation_id);
  let report=check "APPEND candidate inspection failed"
      (Imap_sync.Repair.inspect_append_candidates ~ctx ~id:c.operation_id
        ~max_uids:c.max_inspect ~max_body_bytes:c.max_candidate_bytes ()) in
  Printf.printf "inspected %d UIDs in UIDVALIDITY %Ld\n%!"
    report.inspected_uids (Imap.Uidvalidity.to_int64 report.uidvalidity);
  List.iter (fun uid -> Printf.printf "candidate UID %Ld\n%!"
    (Imap.Uid.to_int64 uid)) report.matching_uids;
  prerr_endline
    ("matching bytes do not attribute APPEND; independent APPENDUID " ^
     "evidence is required");
  converged

let gc (c:gc) ~fs =
  let db=require_db ~fs c.db in
  let blob_dir=require_dir ~fs "blob directory" c.blob_dir in
  let maildir=Option.map (require_dir ~fs "Maildir") c.maildir in
  with_db_lock ~fs c.db @@ fun () ->
  Eio.Switch.run @@ fun sw ->
  let store=Imap_store.open_path ~sw ~blob_dir db in
  let removed=match maildir with
    | None -> reap store
    | Some p -> Maildir.with_writer (open_maildir p) (fun _ -> reap store) in
  Printf.printf "orphans_removed=%d\n%!" removed;
  converged

let forget_epochs (c:forget_epochs) ~fs =
  let db=require_db ~fs c.scope.db in
  Eio.Switch.run @@ fun sw ->
  let store=Imap_store.open_path ~sw db in
  let scope,cursor=local_scope c.scope c.encoding store in
  if cursor.uidvalidity=None then
    fail conflict "no published UIDVALIDITY epoch to keep; run sync first";
  match Imap_store.forget_epochs store ~scope ~cursor with
  | `Dropped n -> Printf.printf "epochs_dropped=%d\n%!" n; converged
  | `Stale_revision ->
      fail conflict "published cursor changed concurrently; run again"

let run job ~env ~net ~fs ~random =
  let secret=ref "" in
  try match job with
    | Sync c -> sync c ~env ~secret ~net ~fs ~random
    | Hydrate c -> hydrate c ~env ~secret ~net ~fs ~random
    | Audit_cache c -> audit_cache c ~fs
    | Inspect c -> inspect c ~fs
    | Inspect_append_candidates c ->
        inspect_append_candidates c ~env ~secret ~net ~fs ~random
    | Repair_appenduid c -> repair_appenduid c ~fs
    | Repair_local_delete r ->
        repair_local_delete r ~env ~secret ~net ~fs ~random
    | Repair_local_append { repair; blob_dir } ->
        repair_local_append repair ~blob_dir ~env ~secret ~net ~fs ~random
    | Settle_flags r -> settle_flags r ~env ~secret ~net ~fs ~random
    | Reject_remote_delete r ->
        remote_delete_repair r ~finish:false ~env ~secret ~net ~fs ~random
    | Finish_remote_delete r ->
        remote_delete_repair r ~finish:true ~env ~secret ~net ~fs ~random
    | Mark_local_retention c -> mark_local_retention c ~fs
    | Plan_deletions c -> plan_deletions c ~fs
    | Plan_sync c -> plan_sync c ~fs
    | Verify_local c -> verify_local c ~fs ~random
    | Gc c -> gc c ~fs
    | Forget_epochs c -> forget_epochs c ~fs
  with
  | Eio.Cancel.Cancelled _ as exn -> raise exn
  | exn ->
      let code,message=classify exn in
      prerr_endline (redact !secret message);
      code

let eval ?help ?err ~env ~argv ~net ~fs ~random () =
  match Cmd.eval_value' ?help ?err ~env ~argv ~term_err:configuration cmd with
  | `Ok job -> run job ~env ~net ~fs ~random
  | `Exit code when code=Cmd.Exit.cli_error -> configuration
  | `Exit code -> code
