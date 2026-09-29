(* Compile-time probes of the kind and mode claims in the protocol
   interfaces. Each probe is a closure bound at portable mode that captures
   module-level values and reads them through the library, which compiles
   only when their types cross portability and contention and the functions
   called are portable. The binding must carry the mode, as in
   [let (f @ portable) = fun () -> ...]. The form [fun () @ portable -> e]
   constrains only the mode of [e], so a probe written that way passes
   whatever the kinds. The locality probes return a local value at global
   mode, which compiles only when its type crosses locality. Running the
   probes checks the values read. *)

(* Every protocol type other than the mutable [Wire.t] and the
   comparator-carrying [Capability.Set.t] and [Mirror.snapshot] is
   immutable data. Those two, and the Base sets and maps over the exported
   comparators, cross portability and contention. Each abbreviation
   compiles only when its kind holds. *)
module Kinds = struct
  type capability_set : value mod contended portable = Imap.Capability.Set.t
  type mirror_snapshot : value mod contended portable = Imap.Mirror.snapshot
  type uid_comparator : value mod portable = Imap.Uid.comparator_witness
  type capability_comparator : value mod portable =
    Imap.Capability.comparator_witness
  type uid_map : value mod contended portable =
    (Imap.Uid.t, string, Imap.Uid.comparator_witness) Base.Map.t
  type capability_base_set : value mod contended portable =
    (Imap.Capability.t, Imap.Capability.comparator_witness) Base.Set.t
  type uid_t : immutable_data = Imap.Uid.t
  type uidvalidity_t : immutable_data = Imap.Uidvalidity.t
  type modseq_t : immutable_data = Imap.Modseq.t
  type seq_t : immutable_data = Imap.Seq.t
  type uid_set_t : immutable_data = Imap.Uid_set.t
  type mailbox_name_mode : immutable_data = Imap.Mailbox_name.mode
  type mailbox_name_t : immutable_data = Imap.Mailbox_name.t
  type internal_date_t : immutable_data = Imap.Internal_date.t
  type wire_event : immutable_data = Imap.Wire.event
  type wire_error : immutable_data = Imap.Wire.error
  type response_compound_object_id : immutable_data =
    Imap.Response.compound_object_id
  type response_code : immutable_data = Imap.Response.code
  type response_fetch : immutable_data = Imap.Response.fetch
  type response_envelope_address : immutable_data =
    Imap.Response.envelope_address
  type response_envelope : immutable_data = Imap.Response.envelope
  type response_binary : immutable_data = Imap.Response.binary
  type response_body_extension : immutable_data = Imap.Response.body_extension
  type response_bodystructure : immutable_data = Imap.Response.bodystructure
  type response_list_result : immutable_data = Imap.Response.list_result
  type response_namespace_entry : immutable_data = Imap.Response.namespace_entry
  type response_namespace : immutable_data = Imap.Response.namespace
  type response_esearch : immutable_data = Imap.Response.esearch
  type response_uidbatches : immutable_data = Imap.Response.uidbatches
  type response_acl : immutable_data = Imap.Response.acl
  type response_list_rights : immutable_data = Imap.Response.list_rights
  type response_my_rights : immutable_data = Imap.Response.my_rights
  type response_quota : immutable_data = Imap.Response.quota
  type response_quota_root : immutable_data = Imap.Response.quota_root
  type response_metadata_payload : immutable_data =
    Imap.Response.metadata_payload
  type response_metadata : immutable_data = Imap.Response.metadata
  type response_mailbox_status : immutable_data = Imap.Response.mailbox_status
  type response_thread : immutable_data = Imap.Response.thread
  type response_untagged : immutable_data = Imap.Response.untagged
  type response_t : immutable_data = Imap.Response.t
  type response_select_metadata : immutable_data = Imap.Response.select_metadata
  type command_error : immutable_data = Imap.Command.error
  type capability_thread_algorithm : immutable_data =
    Imap.Capability.thread_algorithm
  type capability_t : immutable_data = Imap.Capability.t
  type search_date : immutable_data = Imap.Search.date
  type search_t : immutable_data = Imap.Search.t
  type search_error : immutable_data = Imap.Search.error
  type fetch_item_t : immutable_data = Imap.Fetch_item.t
  type status_item_t : immutable_data = Imap.Status_item.t
  type mailbox_list_selection : immutable_data = Imap.Mailbox_list.selection
  type mailbox_list_return : immutable_data = Imap.Mailbox_list.return
  type sort_key : immutable_data = Imap.Sort.key
  type sort_order : immutable_data = Imap.Sort.order
  type sort_return : immutable_data = Imap.Sort.return
  type thread_algorithm : immutable_data = Imap.Thread.algorithm
  type notify_filter : immutable_data = Imap.Notify.filter
  type notify_event : immutable_data = Imap.Notify.event
  type notify_group : immutable_data = Imap.Notify.group
  type metadata_depth : immutable_data = Imap.Metadata.depth
  type mirror_scope : immutable_data = Imap.Mirror.scope
  type mirror_mode : immutable_data = Imap.Mirror.mode
  type mirror_phase : immutable_data = Imap.Mirror.phase
  type mirror_restart_reason : immutable_data = Imap.Mirror.restart_reason
  type mirror_error : immutable_data = Imap.Mirror.error
  type mirror_cursor : immutable_data = Imap.Mirror.cursor
  type mirror_selected : immutable_data = Imap.Mirror.selected
  type mirror_action : immutable_data = Imap.Mirror.action
  type mirror_row : immutable_data = Imap.Mirror.row
  type sync_policy_flag_delta : immutable_data = Imap.Sync_policy.flag_delta
  type sync_policy_flag_plan : immutable_data = Imap.Sync_policy.flag_plan
  type sync_policy_deletion_policy : immutable_data =
    Imap.Sync_policy.deletion_policy
  type sync_policy_deletion_hold : immutable_data =
    Imap.Sync_policy.deletion_hold
  type sync_policy_deletion_plan : immutable_data =
    Imap.Sync_policy.deletion_plan
  type sync_policy_observation : immutable_data = Imap.Sync_policy.observation
end

let get = function Ok x -> x | Error e -> failwith e

let uid = get (Imap.Uid.of_int64 4_294_967_295L)
let uidvalidity = get (Imap.Uidvalidity.of_int64 7L)
let seq = get (Imap.Seq.of_int64 42L)
let modseq = get (Imap.Modseq.of_int64 9_223_372_036_854_775_807L)
let uid_set = get (Imap.Uid_set.of_wire "1:3,7,10:12")
let mailbox = Imap.Mailbox_name.of_wire ~mode:Imap.Mailbox_name.Rev1
    "Entw&APw-rfe"
let date = get (Imap.Internal_date.of_string "17-Jul-1996 02:44:25 -0700")
let capability = Imap.Capability.of_wire "THREAD=REFERENCES"
let flag = get (Mail_flag.Imap_flag.of_wire "$Important")

let criterion =
  Imap.Search.(And [ Uid uid_set; Modseq modseq; Keyword flag;
                     Since (date_of_internal_date date); Not Seen ])

let fetch_items = Imap.Fetch_item.[ Uid; Flags; Modseq; Binary_size [1; 2] ]
let status_items = Imap.Status_item.[ Messages; Uidnext ]
let selection : Imap.Mailbox_list.selection = Subscribed
let list_return = Imap.Mailbox_list.Children
let sort = Imap.Sort.(Date, Descending)
let sort_return = Imap.Sort.Partial (1L, 100L)
let thread = Imap.Thread.Other "X-ALGO"
let notify : Imap.Notify.group =
  Imap.Notify.(Selected, [ Message_new; Flag_change ])
let depth = Imap.Metadata.Infinity

let response =
  get (Imap.Response.parse
    "* 3 FETCH (UID 9 FLAGS (\\Seen) MODSEQ (12) \
     INTERNALDATE \"17-Jul-1996 02:44:25 -0700\")")
let capability_response = get (Imap.Response.parse
  "* CAPABILITY IMAP4rev1 CONDSTORE")
let command_error =
  match Imap.Command.create ~mailbox:"Caf\xe9" with
  | Error e -> e
  | Ok _ -> failwith "8-bit mailbox accepted"
let wire_event = Imap.Wire.Text "* OK\r\n"
let wire_error : Imap.Wire.error = { offset = 3L; message = "bad" }

let scope : Imap.Mirror.scope = {
  endpoint = "imap.example"; account = "alice"; mailbox_key = "inbox";
  raw_name = "INBOX"; encoding = Imap.Mailbox_name.Rev1; mailbox_id = None }
let cursor = Imap.Mirror.initial scope
let row : Imap.Mirror.row = { uid; flags = [ flag ]; modseq = Some modseq }
let selected : Imap.Mirror.selected = {
  uidvalidity; uidnext = 10L; highestmodseq = Some modseq; nomodseq = false }

let action = match Imap.Mirror.plan cursor ~stage_id:"captured" selected with
  | Ok action -> action
  | Error (Imap.Mirror.Invalid e) -> failwith e
let search_error =
  match Imap.Search.to_wire ~utf8:false (Imap.Search.Subject "caf\xc3\xa9")
  with
  | Error e -> e
  | Ok _ -> failwith "non-ASCII accepted without UTF-8"
let deletion : Imap.Sync_policy.deletion_plan =
  Hold_deletion Imap.Sync_policy.Grace_period
let capability_set = Imap.Capability.Set.of_list
    [ capability; Imap.Capability.Idle; Imap.Capability.of_wire "idle" ]
let snapshot = match Imap.Mirror.snapshot ~uidvalidity [ row ] with
  | Ok snapshot -> snapshot
  | Error (Imap.Mirror.Invalid e) -> failwith e
let uid_map = Base.Map.singleton (module Imap.Uid) uid "probe"

let global_uid (u : Imap.Uid.t @ local) : Imap.Uid.t = u
let global_uidvalidity (v : Imap.Uidvalidity.t @ local) : Imap.Uidvalidity.t
    = v
let global_seq (n : Imap.Seq.t @ local) : Imap.Seq.t = n

let (identifiers @ portable) = fun () ->
  String.concat " " [
    Imap.Uid.to_string uid; Imap.Uidvalidity.to_string uidvalidity;
    Imap.Seq.to_string seq; Imap.Modseq.to_string modseq;
    Imap.Uid_set.to_wire uid_set; Imap.Internal_date.to_string date;
    (match mailbox.utf8 with Ok s -> s | Error e -> e) ]

let (vocabulary @ portable) = fun () ->
  String.concat " " [
    Imap.Capability.to_wire capability;
    (match Imap.Search.to_wire ~utf8:false criterion with
     | Ok s -> s
     | Error e -> Imap.Search.error_to_string e);
    String.concat "," (List.map Imap.Fetch_item.to_wire fetch_items);
    String.concat "," (List.map Imap.Status_item.to_wire status_items);
    Imap.Mailbox_list.selection_to_wire selection;
    Imap.Mailbox_list.return_to_wire list_return;
    Imap.Sort.criterion_to_wire sort;
    Imap.Sort.return_to_wire sort_return;
    Imap.Thread.to_wire thread;
    String.concat "," (List.map Imap.Notify.event_to_wire (snd notify));
    Imap.Metadata.depth_to_wire depth ]

let (records @ portable) = fun () ->
  let fetch_uid = match response with
    | Imap.Response.Untagged (Imap.Response.Fetch f) -> f.uid
    | _ -> None in
  let capabilities = match capability_response with
    | Imap.Response.Untagged (Imap.Response.Capability l) -> List.length l
    | _ -> 0 in
  let event = match wire_event with Imap.Wire.Text s -> s | _ -> "" in
  let upper = match Imap.Mirror.plan cursor ~stage_id:"probe" selected with
    | Ok action -> action.upper_uid
    | Error (Imap.Mirror.Invalid e) -> failwith e in
  let rows = match Imap.Mirror.snapshot ~uidvalidity [ row ] with
    | Ok snapshot -> List.length (Imap.Mirror.rows snapshot)
    | Error (Imap.Mirror.Invalid e) -> failwith e in
  let plan = Imap.Sync_policy.reconcile_flags ~base:[] ~remote:[ flag ]
      ~local:[] () in
  let held = match deletion with
    | Imap.Sync_policy.Hold_deletion Grace_period -> true
    | _ -> false in
  fetch_uid, capabilities, Imap.Command.to_string command_error,
  String.trim event, wire_error.offset, upper, rows, List.length plan.merged,
  action.id, Imap.Search.error_to_string search_error, held

let (collections @ portable) = fun () ->
  let capabilities = Imap.Capability.Set.add Imap.Capability.Condstore
      capability_set in
  let by_capability = Base.Set.of_list (module Imap.Capability)
      (Imap.Capability.Set.to_list capabilities) in
  let uids = Base.Set.of_list (module Imap.Uid) [ uid; uid ] in
  List.map Imap.Capability.to_wire (Imap.Capability.Set.to_list capabilities),
  Base.Set.length by_capability,
  List.length (Imap.Mirror.rows snapshot),
  Imap.Uidvalidity.equal (Imap.Mirror.snapshot_uidvalidity snapshot)
    uidvalidity,
  Base.Set.length uids, Base.Map.find uid_map uid

let (framing @ portable) = fun () ->
  let wire = Imap.Wire.create () in
  match Imap.Wire.feed wire "* 1 EXISTS\r\n" with
  | Error e -> Error e.message
  | Ok events -> Imap.Response.parse_parts events

let test_immediates () =
  let u = global_uid uid and v = global_uidvalidity uidvalidity
  and n = global_seq seq in
  Alcotest.(check int64) "uid" 4_294_967_295L (Imap.Uid.to_int64 u);
  Alcotest.(check int64) "uidvalidity" 7L (Imap.Uidvalidity.to_int64 v);
  Alcotest.(check int64) "seq" 42L (Imap.Seq.to_int64 n)

let test_identifiers () =
  Alcotest.(check string) "identifiers"
    "4294967295 7 42 9223372036854775807 1:3,7,10:12 \
     17-Jul-1996 02:44:25 -0700 Entwürfe" (identifiers ())

let test_vocabulary () =
  Alcotest.(check string) "vocabulary"
    "THREAD=REFERENCES UID 1:3,7,10:12 MODSEQ 9223372036854775807 \
     KEYWORD $Important SINCE 17-Jul-1996 NOT SEEN \
     UID,FLAGS,MODSEQ,BINARY.SIZE[1.2] MESSAGES,UIDNEXT SUBSCRIBED \
     CHILDREN REVERSE DATE PARTIAL 1:100 X-ALGO MessageNew,FlagChange \
     infinity"
    (vocabulary ())

let test_records () =
  let fetch_uid, capabilities, error, event, offset, upper, rows, merged,
      action_id, search_error, held = records () in
  Alcotest.(check (option int64)) "fetch uid" (Some 9L) fetch_uid;
  Alcotest.(check int) "capabilities" 2 capabilities;
  Alcotest.(check string) "command error"
    "CREATE mailbox: control character or invalid UTF-8" error;
  Alcotest.(check string) "wire event" "* OK" event;
  Alcotest.(check int64) "wire error" 3L offset;
  Alcotest.(check int64) "planned upper UID" 9L upper;
  Alcotest.(check int) "snapshot rows" 1 rows;
  Alcotest.(check int) "flag plan" 1 merged;
  Alcotest.(check string) "action" "captured" action_id;
  Alcotest.(check bool) "search error" true (String.length search_error > 0);
  Alcotest.(check bool) "deletion plan" true held

let test_collections () =
  let tokens, by_capability, rows, same_validity, uids, found =
    collections () in
  Alcotest.(check (list string)) "capability set"
    [ "CONDSTORE"; "IDLE"; "THREAD=REFERENCES" ] tokens;
  Alcotest.(check int) "Base set of capabilities" 3 by_capability;
  Alcotest.(check int) "snapshot rows" 1 rows;
  Alcotest.(check bool) "snapshot uidvalidity" true same_validity;
  Alcotest.(check int) "Base set of UIDs" 1 uids;
  Alcotest.(check (option string)) "Base map of UIDs" (Some "probe") found

let test_framing () =
  match framing () with
  | Ok (Imap.Response.Untagged (Imap.Response.Exists 1L)) -> ()
  | Ok _ -> Alcotest.fail "wrong response"
  | Error e -> Alcotest.fail e

let () =
  Alcotest.run "IMAP kinds and modes" [
    "portable", [
      Alcotest.test_case "immediates cross locality" `Quick test_immediates;
      Alcotest.test_case "identifiers" `Quick test_identifiers;
      Alcotest.test_case "vocabulary" `Quick test_vocabulary;
      Alcotest.test_case "records" `Quick test_records;
      Alcotest.test_case "collections" `Quick test_collections;
      Alcotest.test_case "framing" `Quick test_framing ] ]
