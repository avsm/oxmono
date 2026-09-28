(* Compile-time probes of the kind and mode claims in the sync interfaces.
   Each abbreviation in [Kinds] compiles only when its kind holds. Each
   probe is a closure bound at portable mode, as in [let (f @ portable) =
   fun () -> ...], that captures module-level values or takes its inputs
   as arguments and calls the library, so it compiles only when the
   captured types cross portability and contention and the functions
   called are portable. *)

module Sync = Imap_sync
module Flag = Mail_flag.Imap_flag

module Kinds = struct
  type error : immutable_data = Sync.Error.t
  type deletion_outcome : immutable_data = Sync.Deletion.outcome
  type flags_outcome : immutable_data = Sync.Flags.outcome
  type flags_plan : immutable_data = Sync.Flags.plan
  type flags_decision : immutable_data = Sync.Flags.decision
  type flags_reconciled : immutable_data = Sync.Flags.reconciled
  type bridge_receipt : immutable_data = Sync.Bridge.receipt
  type bridge_local_verification : immutable_data =
    Sync.Bridge.local_verification
  type plan_deletion_preview : immutable_data = Sync.Plan.deletion_preview
  type plan_sync_preview : immutable_data = Sync.Plan.sync_preview
  type engine_append_outcome : immutable_data = Sync.Engine.append_outcome
  type engine_archived : immutable_data = Sync.Engine.archived
  type engine_hydration_receipt : immutable_data =
    Sync.Engine.hydration_receipt
  type engine_cache_audit_receipt : immutable_data =
    Sync.Engine.cache_audit_receipt
  type engine_uid_digest : immutable_data = Sync.Engine.uid_digest
  type watch_retry_error : immutable_data = Sync.Watch.retry_error
  type watch_error : immutable_data = Sync.Watch.error
  type repair_append_candidates : immutable_data =
    Sync.Repair.append_candidates
end

let get = function Ok x -> x | Error e -> failwith e
let flag s = get (Flag.of_wire s)
let seen = flag "\\Seen"
let deleted = flag "\\Deleted"
let error = Sync.Error.Invalid_scope "probe"

let scope : Imap.Mirror.scope = {
  endpoint = "imap.example"; account = "alice"; mailbox_key = "inbox";
  raw_name = "INBOX"; encoding = Imap.Mailbox_name.Rev1; mailbox_id = None }
let pair : Imap_store.Journal.pair = {
  id = "p1"; scope; remote_uidvalidity = None; remote_uid = None;
  local_id = Some "im-1"; content_sha256 = Some "00"; content_length = Some 1L;
  internal_date = None; common_flags = [ seen ]; remote_tombstone = None;
  local_tombstone = None; revision = 1L }

let (report @ portable) = fun () ->
  Sync.Error.to_string error, Format.asprintf "%a" Sync.Error.pp error

let (planning @ portable) = fun () ->
  let decision = Sync.Flags.plan_flags ~base:[ seen ] ~remote:[ seen ]
      ~local:[ seen; deleted ] ~condstore:true ~remote_modseq:(Some 2L) () in
  let permitted = Sync.Flags.validate_permanent_flags ~available:None
      ~defined:None ~remote:[ seen ] ~merged:[ seen; deleted ] in
  let preflight = Sync.Deletion.expunge_preflight ~before_flags:[ seen ]
      ~before_modseq:2L (Some ([ seen; deleted ], Some 3L)) in
  let deletion = Sync.Deletion.plan ~policy:Imap.Sync_policy.Preserve
      ~min_absence_scans:0 ~current_generation:1L
      ~last_presence:(fun _ -> None) ~remote_present:false
      ~local_present:true pair in
  (match decision with Ok d -> d.deleted_held | Error _ -> false),
  Result.is_ok permitted, preflight, deletion

let (context @ portable) = fun ~client ~store ~spool_dir ->
  Sync.Ctx.v ~client ~store ~scope ~mailbox:"INBOX" ~spool_dir
    ~next_id:(fun () -> "id")

let (dates @ portable) = fun () ->
  let date = get (Imap_sync_local.Local_date.of_mtime 1_000_000.0) in
  Imap.Internal_date.to_string date, Imap_sync_local.Local_date.to_mtime date

let (staged @ portable) = fun inventory ->
  Imap_sync_local.Local_inventory.count inventory

let test_report () =
  let s, p = report () in
  Alcotest.(check string) "pp" s p

let test_planning () =
  let deleted_held, permitted, preflight, deletion = planning () in
  Alcotest.(check bool) "deleted held" true deleted_held;
  Alcotest.(check bool) "permitted" true permitted;
  Alcotest.(check bool) "preflight" true preflight;
  Alcotest.(check bool) "deletion" true
    (match deletion with Imap.Sync_policy.No_deletion -> false | _ -> true)

let test_dates () =
  let date, mtime = dates () in
  Alcotest.(check string) "date" "12-Jan-1970 13:46:40 +0000" date;
  Alcotest.(check (result (float 0.) string)) "mtime" (Ok 1_000_000.0) mtime

let () =
  Alcotest.run "Imap_sync kinds and modes" [
    "portable", [
      Alcotest.test_case "error printers" `Quick test_report;
      Alcotest.test_case "planning" `Quick test_planning;
      Alcotest.test_case "local dates" `Quick test_dates ] ]
