module M = Imap.Mirror
module J = Imap_store.Journal

(* [all_pages page id] concatenates the pages of 1,000 that [page] reads
   after the ID of the last row of the previous page. *)
let all_pages page id =
  let rec go after acc =
    let rows=page after in
    let acc=List.rev_append rows acc in
    if List.length rows<1000 then List.rev acc
    else go (Some (id (List.nth rows (List.length rows-1)))) acc in
  go None []
let all_pairs store ~scope =
  all_pages (fun after ->
    Imap_store.Journal.pairs_page store ~scope ?after ~limit:1000 ())
    (fun (p:Imap_store.Journal.pair) -> p.id)
let all_open_conflicts store ~scope =
  all_pages (fun after ->
    Imap_store.Journal.open_conflicts_page store ~scope ?after ~limit:1000 ())
    (fun (c:Imap_store.Journal.conflict) -> c.id)
let all_active_operations store ~scope =
  all_pages (fun after ->
    Imap_store.Journal.active_operations_page store ~scope ?after
      ~limit:1000 ())
    (fun (o:Imap_store.Journal.operation) -> o.id)

let ok = function Ok x -> x | Error e -> Alcotest.fail e
let uid n = ok (Imap.Uid.of_int64 n)
let epoch n = ok (Imap.Uidvalidity.of_int64 n)
let scope : M.scope = {
  endpoint="scripted.example"; account="alice"; mailbox_key="INBOX";
  raw_name="INBOX"; encoding=Imap.Mailbox_name.Rev1; mailbox_id=None;
}

(* [context ~store ~spool_dir client] is the sync context of [client] for
   [scope] and INBOX, and fails the test when [Ctx.v] refuses it. The
   default [next_id] fails the test, so a call that needs no ID proves it. *)
let context ?(next_id=fun () -> Alcotest.fail "unexpected ID request")
    ~store ~spool_dir client =
  match Imap_sync.Ctx.v ~client ~store ~scope ~mailbox:"INBOX" ~spool_dir
      ~next_id with
  | Ok ctx -> ctx
  | Error e -> Alcotest.failf "context: %a" Imap_sync.Error.pp e

(* [connected ~store ~spool_dir result] is the context of a connected client
   as a [Watch.run] connect returns it. *)
let connected ~store ~spool_dir = function
  | Ok client -> Ok (context ~store ~spool_dir client)
  | Error e -> Error (Imap_sync.Error.Client e)

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

let mtime date = ok (Local_date.to_mtime date)
let local_date local = Result.to_option (Local_date.of_occurrence local)

let root () =
  let path=Filename.temp_file "imap-bridge-fault-" "" in
  Sys.remove path;
  Unix.mkdir path 0o700;
  List.iter (fun child -> Unix.mkdir (Filename.concat path child) 0o700)
    ["blob";"spool"];
  path

let rec remove_tree path =
  if Sys.is_directory path then (
    Sys.readdir path |> Array.iter (fun child ->
      remove_tree (Filename.concat path child));
    Unix.rmdir path)
  else Sys.remove path

let with_fixture f =
  let dir=root () in
  Fun.protect ~finally:(fun () -> remove_tree dir) (fun () ->
    Eio_main.run @@ fun env ->
    let fs=Eio.Stdenv.fs env in
    let database=Eio.Path.(fs / dir / "sync.db") in
    let blob_dir=Eio.Path.(fs / dir / "blob") in
    let spool_dir=Eio.Path.(fs / dir / "spool") in
    let maildir=Md.open_dir Eio.Path.(fs / dir / "maildir") in
    f ~database ~blob_dir ~spool_dir ~maildir)

let open_store ~sw ~database ~blob_dir =
  Imap_store.open_path ~sw ~blob_dir database

let message="From: fault@example.test\r\nSubject: recovery\r\n\r\nSame bytes\r\n"
let length=Int64.of_int (String.length message)

let contains_substring haystack needle =
  let n=String.length needle in
  let rec loop i=
    i+n<=String.length haystack &&
    (String.sub haystack i n=needle || loop (i+1)) in
  loop 0

let operation ~kind ~id ~local_id ~source_uid ~blob ~flags : J.operation = {
  id;pair_id=None;local_id=Some local_id;scope;kind;state=J.Prepared;
  source_uidvalidity=(Option.map (fun _ -> epoch 11L) source_uid);
  source_uid;destination=(if kind=J.Append then Some scope else None);
  destination_uidvalidity=(if kind=J.Append then Some (epoch 11L) else None);
  blob_sha256=Some blob.Imap_store.Blob.sha256;
  blob_length=Some blob.length;desired_flags=Some flags;
  internal_date=None;
  append=(if kind=J.Append then
    Some {message_id=id;spool_ref=blob.sha256;pre_send_frontier=0L}
  else None);
  receipt=None;receipt_uidvalidity=None;receipt_uid=None;
}

let scripted_scan ?(confirmed_body=false) ?(missing_body=false)
    ?(append_without_uidplus=false) ?(flags="") ?(missing_metadata=false)
    ?recovery_date ?confirmed_date
    ?(fetched_body=message)
    ?(uidvalidity=11L)
    ?(caps="IMAP4rev1 UNSELECT UIDPLUS")
    ~sw ~has_message () =
  let flow=Eio_mock.Flow.make "bridge-fault" in
  let lines=[
    `Return "* OK ready\r\n";
    `Return ("* CAPABILITY "^caps^"\r\nA00000001 OK done\r\n");
    `Return "A00000002 OK logged in\r\n";
    `Return ("* CAPABILITY "^caps^"\r\nA00000003 OK done\r\n");
    `Return (Printf.sprintf "* %d EXISTS\r\n* OK [UIDVALIDITY %Ld] valid\r\n* OK [UIDNEXT %d] next\r\nA00000004 OK [READ-ONLY] selected\r\n"
      (if has_message then 1 else 0) uidvalidity
      (if has_message then 2 else 1));
  ] in
  let lines=if has_message then lines @ [
    `Return ("* 1 FETCH (UID 1 FLAGS (" ^ flags ^
      "))\r\nA00000005 OK fetched\r\n");
    `Return "* SEARCH 1\r\nA00000006 OK searched\r\n";
    `Return "A00000007 OK unselected\r\n";
  ] else lines @ [`Return "A00000005 OK unselected\r\n"] in
  let lines=if missing_body then lines @ [
    `Return "* 1 EXISTS\r\n* OK [UIDVALIDITY 11] valid\r\n* OK [UIDNEXT 2] next\r\nA00000008 OK [READ-ONLY] selected\r\n";
    `Return "A00000009 OK fetched\r\n";
    `Return "A00000010 OK unselected\r\n";
  ] else if confirmed_body then lines @ [
    `Return "* 1 EXISTS\r\n* OK [UIDVALIDITY 11] valid\r\n* OK [UIDNEXT 2] next\r\nA00000008 OK [READ-ONLY] selected\r\n";
    `Return (Printf.sprintf "* 1 FETCH (UID 1 BODY[] {%d}\r\n"
      (String.length fetched_body));
    `Return (fetched_body ^ ")\r\nA00000009 OK fetched\r\n");
    `Return ((if missing_metadata then "" else
      "* 1 FETCH (UID 1 FLAGS (" ^ flags ^ ") INTERNALDATE " ^
      (match confirmed_date with
       | None -> "\"01-Jan-2020 00:00:00 +0000\""
       | Some date -> Imap.Internal_date.to_wire date) ^
      ")\r\n") ^ "A00000010 OK fetched\r\n");
    `Return "A00000011 OK unselected\r\n";
  ] else lines in
  let lines=if append_without_uidplus then lines @ [
    `Return "+ ready for literal\r\n";
    `Return "A00000006 OK appended\r\n";
  ] else lines in
  let lines=match recovery_date with
    | None -> lines
    | Some date -> lines @ [
        `Return "* 1 EXISTS\r\n* OK [UIDVALIDITY 11] valid\r\n* OK [UIDNEXT 2] next\r\nA00000008 OK [READ-ONLY] selected\r\n";
        `Return ("* 1 FETCH (UID 1 FLAGS () INTERNALDATE \"" ^
          Imap.Internal_date.to_string date ^
          "\")\r\nA00000009 OK fetched\r\n");
        `Return "A00000010 OK unselected\r\n"] in
  Eio_mock.Flow.on_read flow lines;
  let auth=Imap_eio.Auth.password ~username:"alice" ~password:"secret" ~allow_insecure_transport:true () in
  match Imap_eio.Client.of_flow ~sw ~auth flow with
  | Ok client -> client
  | Error e -> Alcotest.fail (Imap_eio.Client.error_to_string e)

let run_bridge ~client ~store ~maildir ~spool_dir =
  let next_id ()="unexpected-transfer" in
  Imap_sync.Bridge.copy_once ~ctx:(context ~store ~spool_dir ~next_id client)
    ~maildir ~stage_id:"fault-scan" ()

let scripted_condstore ?(nomodseq=false) ~sw ~modseq ~seen () =
  let wire=Buffer.create 512 in
  let pp ppf data=
    Buffer.add_string wire data;
    Format.pp_print_string ppf data in
  let flow=Eio_mock.Flow.make ~pp "staged-condstore" in
  let flags=if seen then "\\Seen" else "" in
  Eio_mock.Flow.on_read flow [
    `Return "* OK ready\r\n";
    `Return "* CAPABILITY IMAP4rev1 UNSELECT CONDSTORE\r\nA00000001 OK done\r\n";
    `Return "A00000002 OK logged in\r\n";
    `Return "* CAPABILITY IMAP4rev1 UNSELECT CONDSTORE\r\nA00000003 OK done\r\n";
    `Return (if nomodseq then
      "* 1 EXISTS\r\n* OK [UIDVALIDITY 11] valid\r\n* OK [UIDNEXT 2] next\r\n* OK [NOMODSEQ] baseline\r\nA00000004 OK [READ-ONLY] selected\r\n"
      else Printf.sprintf "* 1 EXISTS\r\n* OK [UIDVALIDITY 11] valid\r\n* OK [UIDNEXT 2] next\r\n* OK [HIGHESTMODSEQ %Ld] anchor\r\nA00000004 OK [READ-ONLY] selected\r\n" modseq);
    `Return (if nomodseq then Printf.sprintf
      "* 1 FETCH (UID 1 FLAGS (%s))\r\nA00000005 OK fetched\r\n"
      flags else Printf.sprintf
      "* 1 FETCH (UID 1 FLAGS (%s) MODSEQ (%Ld))\r\nA00000005 OK fetched\r\n"
      flags modseq);
    `Return "* SEARCH 1\r\nA00000006 OK searched\r\n";
    `Return "A00000007 OK unselected\r\n";
  ];
  let auth=Imap_eio.Auth.password ~username:"alice" ~password:"secret"
    ~allow_insecure_transport:true () in
  let client=match Imap_eio.Client.of_flow ~sw ~auth flow with
    | Ok client -> client
    | Error error -> Alcotest.fail
        (Imap_eio.Client.error_to_string error) in
  client,wire

let scripted_objectid_empty ?status_mailbox_id ?(select_objectid=true)
    ?(select_account_id=true) ?(uidvalidity=11L)
    ~sw ~mailbox_id () =
  let wire=Buffer.create 256 in
  let pp ppf data=
    Buffer.add_string wire data;
    Format.pp_print_string ppf data in
  let flow=Eio_mock.Flow.make ~pp "staged-objectid" in
  let caps="IMAP4rev1 UNSELECT ENABLE OBJECTID+" in
  let prelude=[
    `Return "* OK ready\r\n";
    `Return ("* CAPABILITY " ^ caps ^ "\r\nA00000001 OK done\r\n");
    `Return "A00000002 OK logged in\r\n";
    `Return ("* CAPABILITY " ^ caps ^ "\r\nA00000003 OK done\r\n");
    `Return "* ENABLED OBJECTID+\r\nA00000004 OK enabled\r\n";
  ] in
  let status=match status_mailbox_id with
    | None -> []
    | Some id -> [`Return ("* STATUS INBOX (OBJECTID (ACCOUNTID " ^
        "u_account MAILBOXID " ^ id ^ "))\r\nA00000005 OK status\r\n")] in
  let select_tag=if status=[] then 5 else 6 in
  Eio_mock.Flow.on_read flow (prelude @ status @ [
    `Return (Printf.sprintf "* 0 EXISTS\r\n* OK [UIDVALIDITY %Ld] valid\r\n"
      uidvalidity ^
      "* OK [UIDNEXT 1] next\r\n" ^
      (if select_objectid then
        "* OK [OBJECTID (" ^
        (if select_account_id then "ACCOUNTID u_account " else "") ^
        "MAILBOXID " ^
        mailbox_id ^ ")] identity\r\n" else "") ^
      Printf.sprintf "A%08d OK [READ-ONLY] selected\r\n" select_tag);
    `Return (Printf.sprintf "A%08d OK unselected\r\n" (select_tag+1));
  ]);
  let auth=Imap_eio.Auth.password ~username:"alice" ~password:"secret"
    ~allow_insecure_transport:true () in
  let client=match Imap_eio.Client.of_flow ~sw ~auth flow with
    | Ok client -> client
    | Error error -> Alcotest.fail
        (Imap_eio.Client.error_to_string error) in
  client,wire

let test_objectid_binding_guards_reconnect () =
  with_fixture @@ fun ~database ~blob_dir ~spool_dir ~maildir:_ ->
  Eio.Switch.run @@ fun sw ->
  let store=open_store ~sw ~database ~blob_dir in
  let first,_=scripted_objectid_empty ~sw ~mailbox_id:"F_box" () in
  (match Imap_sync.Engine.scan_once
    ~ctx:(context ~store ~spool_dir first) ~stage_id:"objectid-first" () with
   | Ok _ -> ()
   | Error error -> Alcotest.failf "first OBJECTID+ scan: %a"
       Imap_sync.Error.pp error);
  Alcotest.(check bool) "first scan bound identity" true
    (Imap_store.object_identity store ~scope=
      `Bound {Imap_store.account_id="u_account";mailbox_id="F_box"});
  let downgraded=scripted_scan ~sw ~has_message:false () in
  (match Imap_sync.Engine.scan_once
    ~ctx:(context ~store ~spool_dir downgraded)
    ~stage_id:"objectid-downgraded" () with
   | Error (Imap_sync.Error.Invalid_scope
       "saved OBJECTID+ identity cannot be verified") -> ()
   | Error error -> Alcotest.failf "wrong capability-loss error: %a"
       Imap_sync.Error.pp error
   | Ok _ -> Alcotest.fail "capability loss bypassed durable identity");
  let second,wire=scripted_objectid_empty ~sw ~mailbox_id:"F_box"
    ~status_mailbox_id:"F_box" () in
  (match Imap_sync.Engine.scan_once
    ~ctx:(context ~store ~spool_dir second) ~stage_id:"objectid-second" () with
   | Ok _ -> ()
   | Error error -> Alcotest.failf "second OBJECTID+ scan: %a"
       Imap_sync.Error.pp error);
  let output=Buffer.contents wire in
  let requested="EXAMINE INBOX (OBJECTID (MAILBOXID F_box ACCOUNTID u_account))" in
  Alcotest.(check bool) "reconnect selected by durable ID" true
    (let n=String.length requested in
     let rec contains i=i+n<=String.length output &&
       (String.sub output i n=requested || contains (i+1)) in
     contains 0);
  let before=(Imap_store.load_cursor store ~scope).revision in
  let replacement,_=scripted_objectid_empty ~sw
    ~status_mailbox_id:"F_replacement"
    ~mailbox_id:"F_replacement" () in
  (match Imap_sync.Engine.scan_once
    ~ctx:(context ~store ~spool_dir replacement)
    ~stage_id:"objectid-replaced" () with
   | Error (Imap_sync.Error.Invalid_scope
       "configured mailbox name no longer matches saved OBJECTID+") -> ()
   | Error error -> Alcotest.failf "wrong replacement error: %a"
       Imap_sync.Error.pp error
   | Ok _ -> Alcotest.fail "replaced mailbox advanced durable cursor");
  Alcotest.(check int64) "replacement did not publish" before
    (Imap_store.load_cursor store ~scope).revision;
  let append_client,_=scripted_objectid_empty ~sw
    ~status_mailbox_id:"F_replacement" ~mailbox_id:"F_replacement" () in
  (match Imap_eio.Client.Objectid_plus.enable append_client with
   | Ok _ -> ()
   | Error error -> Alcotest.fail
       (Imap_eio.Client.error_to_string error));
  J.prepare_operation store {id="wrong-destination";pair_id=None;
    local_id=None;scope;kind=J.Append;state=J.Prepared;
    source_uidvalidity=None;source_uid=None;destination=Some scope;
    destination_uidvalidity=None;blob_sha256=Some (String.make 64 'a');
    blob_length=Some 0L;desired_flags=Some [];internal_date=None;
    append=Some {message_id="<wrong@x>";spool_ref="dummy";
      pre_send_frontier=0L};
    receipt=None;receipt_uidvalidity=None;receipt_uid=None};
  (match Imap_sync.Engine.append_journaled
    ~ctx:(context ~store ~spool_dir append_client)
    ~id:"wrong-destination" (Eio.Flow.string_source "") with
   | Error (Imap_sync.Error.Invalid_scope
       "APPEND destination name no longer matches saved OBJECTID+") -> ()
   | Error error -> Alcotest.failf "wrong APPEND guard error: %a"
       Imap_sync.Error.pp error
   | Ok _ -> Alcotest.fail "APPEND to replaced mailbox was allowed");
  Alcotest.(check bool) "unsafe APPEND was not sent" true
    (match J.find_operation store ~id:"wrong-destination" with
     | Some {state=J.Prepared;_} -> true
     | _ -> false);
  let archive_client,_=scripted_objectid_empty ~sw
    ~status_mailbox_id:"F_replacement" ~mailbox_id:"F_replacement" () in
  (match Imap_sync.Engine.archive_uid
      ~ctx:(context ~store ~spool_dir archive_client) ~uid:(uid 1L)
      ~spool:Eio.Path.(spool_dir / "wrong-mailbox-archive") () with
   | Error (Imap_sync.Error.Invalid_scope
       "configured mailbox name no longer matches saved OBJECTID+") -> ()
   | Error error -> Alcotest.failf "wrong archive guard error: %a"
       Imap_sync.Error.pp error
   | Ok _ -> Alcotest.fail "archived UID from replacement mailbox")

let test_objectid_first_binding_requires_stable_epoch () =
  with_fixture @@ fun ~database ~blob_dir ~spool_dir ~maildir:_ ->
  Eio.Switch.run @@ fun sw ->
  let store=open_store ~sw ~database ~blob_dir in
  let baseline=scripted_scan ~sw ~has_message:false () in
  (match Imap_sync.Engine.scan_once
    ~ctx:(context ~store ~spool_dir baseline) ~stage_id:"pre-objectid" () with
   | Ok _ -> ()
   | Error error -> Alcotest.failf "baseline scan: %a"
       Imap_sync.Error.pp error);
  let before=(Imap_store.load_cursor store ~scope).revision in
  let scan stage_id=
    let client,_=scripted_objectid_empty ~sw ~mailbox_id:"F_unknown"
      ~uidvalidity:12L () in
    match Imap_sync.Engine.scan_once ~ctx:(context ~store ~spool_dir client)
      ~stage_id () with
    | Ok _ -> ()
    | Error error -> Alcotest.failf "%s: %a" stage_id
        Imap_sync.Error.pp error in
  scan "changed-epoch";
  Alcotest.(check bool) "changed epoch not bound" true
    (Imap_store.object_identity store ~scope=`Unbound);
  Alcotest.(check bool) "changed epoch published" true
    ((Imap_store.load_cursor store ~scope).revision>before &&
     (Imap_store.load_cursor store ~scope).uidvalidity=Some (epoch 12L));
  scan "stable-epoch";
  Alcotest.(check bool) "stable epoch binds" true
    (Imap_store.object_identity store ~scope=
      `Bound {Imap_store.account_id="u_account";mailbox_id="F_unknown"})

let test_objectid_missing_select_identity_cannot_publish () =
  with_fixture @@ fun ~database ~blob_dir ~spool_dir ~maildir:_ ->
  Eio.Switch.run @@ fun sw ->
  let store=open_store ~sw ~database ~blob_dir in
  List.iter (fun (name,options) ->
    let client,_=match options with
      | `Missing -> scripted_objectid_empty ~sw ~mailbox_id:"F_omitted"
          ~select_objectid:false ()
      | `Partial -> scripted_objectid_empty ~sw ~mailbox_id:"F_partial"
          ~select_account_id:false () in
    (match Imap_sync.Engine.scan_once ~ctx:(context ~store ~spool_dir client)
      ~stage_id:("objectid-" ^ name) () with
     | Error (Imap_sync.Error.Invalid_scope
         "OBJECTID+ SELECT omitted account/mailbox identity") -> ()
     | Error error -> Alcotest.failf "wrong %s identity error: %a"
         name Imap_sync.Error.pp error
     | Ok _ -> Alcotest.fail (name ^ " OBJECTID+ scan was published")))
    ["missing",`Missing; "partial",`Partial];
  Alcotest.(check bool) "missing identity was not bound" true
    (Imap_store.object_identity store ~scope=`Unbound);
  Alcotest.(check int64) "missing identity did not publish" 0L
    (Imap_store.load_cursor store ~scope).revision;
  Alcotest.(check (list string)) "missing identity made no stage" []
    (Imap_store.abandoned_stages store)

let test_flag_settlement_rejects_replaced_objectid () =
  with_fixture @@ fun ~database ~blob_dir ~spool_dir ~maildir ->
  Eio.Switch.run @@ fun sw ->
  let store=open_store ~sw ~database ~blob_dir in
  let module J=Imap_store.Journal in
  let pair:J.pair={
    id="objectid-flags-pair";scope;
    remote_uidvalidity=Some (epoch 11L);remote_uid=Some (uid 1L);
    local_id=Some "objectid-flags-local";
    content_sha256=Some (String.make 64 'a');content_length=Some 1L;
    internal_date=None;common_flags=[];
    remote_tombstone=None;local_tombstone=None;revision=0L} in
  let pair=match J.put_pair store ~expected_revision:None pair with
    | `Committed pair -> pair
    | `Stale_revision -> Alcotest.fail "new OBJECTID+ pair stale" in
  Alcotest.(check bool) "saved mailbox identity" true
    (Imap_store.observe_object_identity store ~scope
      {account_id="u_account";mailbox_id="F_original"}=`Bound);
  let operation:J.operation={
    id="objectid-flags-op";pair_id=Some pair.id;local_id=pair.local_id;
    scope;kind=Flags;state=Prepared;
    source_uidvalidity=pair.remote_uidvalidity;
    source_uid=pair.remote_uid;destination=None;
    destination_uidvalidity=None;blob_sha256=None;blob_length=None;
    desired_flags=Some [];
    internal_date=None;append=None;
    receipt=None;
    receipt_uidvalidity=None;receipt_uid=None} in
  J.prepare_operation store operation;
  J.mark_sent store ~id:operation.id;
  let client,_=scripted_objectid_empty ~sw
    ~status_mailbox_id:"F_replacement" ~mailbox_id:"F_replacement" () in
  (match Imap_sync.Repair.settle_flags
      ~ctx:(context ~store ~spool_dir client) ~maildir ~id:operation.id
      ~evidence:"operator audit" () with
   | Error (Imap_sync.Error.Invalid_scope
       "configured mailbox name no longer matches saved OBJECTID+") -> ()
   | Error error -> Alcotest.failf "wrong replacement settlement error: %a"
       Imap_sync.Error.pp error
   | Ok _ -> Alcotest.fail "FLAGS settlement accepted replaced mailbox");
  Alcotest.(check bool) "replacement left FLAGS operation pending" true
    ((Option.get (J.find_operation store ~id:operation.id)).state=J.Sent);
  Alcotest.(check int64) "replacement left pair unchanged" pair.revision
    (Option.get (J.find_pair store ~id:pair.id)).revision

let test_standalone_repairs_reject_replaced_objectid () =
  with_fixture @@ fun ~database ~blob_dir ~spool_dir ~maildir ->
  Eio.Switch.run @@ fun sw ->
  let store=open_store ~sw ~database ~blob_dir in
  let module J=Imap_store.Journal in
  let digest=String.make 64 'a' in
  Alcotest.(check bool) "saved repair mailbox identity" true
    (Imap_store.observe_object_identity store ~scope
      {account_id="u_account";mailbox_id="F_original"}=`Bound);
  let pair:J.pair={
    id="repair-objectid-pair";scope;
    remote_uidvalidity=Some (epoch 11L);remote_uid=Some (uid 1L);
    local_id=Some "repair-objectid-local";
    content_sha256=Some digest;content_length=Some 1L;
    internal_date=None;common_flags=[];
    remote_tombstone=None;local_tombstone=None;revision=0L} in
  let pair=match J.put_pair store ~expected_revision:None pair with
    | `Committed pair -> pair
    | `Stale_revision -> Alcotest.fail "new repair pair stale" in
  let delete:J.operation={
    id="repair-objectid-delete";pair_id=Some pair.id;
    local_id=pair.local_id;scope;kind=Local_delete;state=Prepared;
    source_uidvalidity=pair.remote_uidvalidity;
    source_uid=pair.remote_uid;destination=None;
    destination_uidvalidity=None;blob_sha256=Some digest;
    blob_length=Some 1L;desired_flags=Some [];
    internal_date=None;append=None;
    receipt=None;receipt_uidvalidity=None;receipt_uid=None} in
  J.prepare_operation store delete;
  J.mark_sent store ~id:delete.id;
  let client,_=scripted_objectid_empty ~sw
    ~status_mailbox_id:"F_replacement" ~mailbox_id:"F_replacement" () in
  (match Imap_sync.Repair.local_delete
      ~ctx:(context ~store ~spool_dir client) ~maildir ~id:delete.id
      ~evidence:"operator audit" () with
   | Error (Imap_sync.Error.Invalid_scope
       "configured mailbox name no longer matches saved OBJECTID+") -> ()
   | Error error -> Alcotest.failf "wrong DELETE repair guard: %a"
       Imap_sync.Error.pp error
   | Ok _ -> Alcotest.fail "local DELETE repaired against replacement");
  Alcotest.(check bool) "DELETE operation remains sent" true
    ((Option.get (J.find_operation store ~id:delete.id)).state=J.Sent);
  let append:J.operation={
    delete with id="repair-objectid-append";pair_id=None;
    local_id=Some (Md.reserve_id ());kind=Local_append;
    source_uid=Some (uid 2L);desired_flags=Some []} in
  J.prepare_operation store append;
  J.mark_sent store ~id:append.id;
  let client,_=scripted_objectid_empty ~sw
    ~status_mailbox_id:"F_replacement" ~mailbox_id:"F_replacement" () in
  (match Imap_sync.Repair.local_append
      ~ctx:(context ~store ~spool_dir client) ~maildir ~id:append.id
      ~evidence:"operator audit" () with
   | Error (Imap_sync.Error.Invalid_scope
       "configured mailbox name no longer matches saved OBJECTID+") -> ()
   | Error error -> Alcotest.failf "wrong local APPEND repair guard: %a"
       Imap_sync.Error.pp error
   | Ok () -> Alcotest.fail "local APPEND repaired against replacement");
  Alcotest.(check bool) "APPEND operation remains sent" true
    ((Option.get (J.find_operation store ~id:append.id)).state=J.Sent)

let test_candidate_inspection_rejects_replaced_objectid () =
  with_fixture @@ fun ~database ~blob_dir ~spool_dir ~maildir:_ ->
  Eio.Switch.run @@ fun sw ->
  let store=open_store ~sw ~database ~blob_dir in
  let blob=Imap_store.Blob.put store
    ~source:(Eio.Flow.string_source message) ~length () in
  let op=operation ~kind:J.Append ~id:"objectid-candidate"
    ~local_id:"candidate-local" ~source_uid:None ~blob ~flags:[] in
  J.prepare_operation store op;
  J.mark_sent store ~id:op.id;
  Alcotest.(check bool) "saved candidate mailbox identity" true
    (Imap_store.observe_object_identity store ~scope
      {account_id="u_account";mailbox_id="F_original"}=`Bound);
  let client,_=scripted_objectid_empty ~sw
    ~status_mailbox_id:"F_replacement" ~mailbox_id:"F_replacement" () in
  (match Imap_sync.Repair.inspect_append_candidates
      ~ctx:(context ~store ~spool_dir client) ~id:op.id () with
   | Error (Imap_sync.Error.Invalid_scope
       "configured mailbox name no longer matches saved OBJECTID+") -> ()

   | Error error -> Alcotest.failf "wrong candidate identity guard: %a"
       Imap_sync.Error.pp error
   | Ok _ -> Alcotest.fail "inspected APPEND candidates in replacement");
  Alcotest.(check bool) "candidate inspection left journal unchanged" true
    ((Option.get (J.find_operation store ~id:op.id)).state=J.Sent)

let scripted_client ?(caps="IMAP4rev1 UNSELECT UIDPLUS") ?(tail=[]) ~sw name
    lines =
  let wire=Buffer.create 512 in
  let pp ppf data=
    Buffer.add_string wire data;
    Format.pp_print_string ppf data in
  let flow=Eio_mock.Flow.make ~pp name in
  Eio_mock.Flow.on_read flow ([
    `Return "* OK ready\r\n";
    `Return ("* CAPABILITY " ^ caps ^ "\r\nA00000001 OK done\r\n");
    `Return "A00000002 OK logged in\r\n";
    `Return ("* CAPABILITY " ^ caps ^ "\r\nA00000003 OK done\r\n")]
    @ List.map (fun line -> `Return line) lines @ tail);
  let auth=Imap_eio.Auth.password ~username:"alice" ~password:"secret"
    ~allow_insecure_transport:true () in
  match Imap_eio.Client.of_flow ~sw ~auth flow with
  | Ok client -> client,wire
  | Error error -> Alcotest.fail (Imap_eio.Client.error_to_string error)

let examine ?(extra="") ~tag ~exists ~uidnext () =
  Printf.sprintf "* %d EXISTS\r\n* OK [UIDVALIDITY 11] valid\r\n\
    * OK [UIDNEXT %d] next\r\n%sA%08d OK [READ-ONLY] selected\r\n"
    exists uidnext extra tag

let publish_two ~sw ~store ~spool_dir =
  let client,_=scripted_client ~sw "two-messages" [
    examine ~tag:4 ~exists:2 ~uidnext:3 ();
    "* 1 FETCH (UID 1 FLAGS ())\r\n* 2 FETCH (UID 2 FLAGS ())\r\n\
     A00000005 OK fetched\r\n";
    "* SEARCH 1 2\r\nA00000006 OK searched\r\n";
    "A00000007 OK unselected\r\n"] in
  match Imap_sync.Engine.scan_once ~ctx:(context ~store ~spool_dir client)
      ~stage_id:"two-messages" () with
  | Ok _ -> ()
  | Error error -> Alcotest.failf "two-message scan: %a"
      Imap_sync.Error.pp error

let test_hydration_skips_oversized_message () =
  with_fixture @@ fun ~database ~blob_dir ~spool_dir ~maildir:_ ->
  Eio.Switch.run @@ fun sw ->
  let store=open_store ~sw ~database ~blob_dir in
  publish_two ~sw ~store ~spool_dir;
  let size=String.length message in
  let client,_=scripted_client ~sw "hydrate-oversized" [
    examine ~tag:4 ~exists:2 ~uidnext:3 ();
    Printf.sprintf "* 1 FETCH (UID 1 FLAGS () RFC822.SIZE 5000)\r\n\
      * 2 FETCH (UID 2 FLAGS () RFC822.SIZE %d)\r\n\
      A00000005 OK fetched\r\n" size;
    Printf.sprintf "* 2 FETCH (UID 2 BODY[] {%d}\r\n" size;
    message ^ ")\r\nA00000006 OK fetched\r\n";
    "A00000007 OK unselected\r\n"] in
  let spool_id=ref 0 in
  let next_id ()=incr spool_id; string_of_int !spool_id in
  (match Imap_sync.Engine.hydrate_once ~max_body_bytes:1000L
      ~ctx:(context ~store ~spool_dir ~next_id client) ()
   with
   | Ok receipt ->
       Alcotest.(check int) "later UID hydrated" 1 receipt.hydrated;
       Alcotest.(check (list int64)) "oversized UID reported" [1L]
         (List.map Imap.Uid.to_int64 receipt.skipped);
       Alcotest.(check (option int64)) "last UID considered" (Some 2L)
         (Option.map Imap.Uid.to_int64 receipt.last_uid);
       Alcotest.(check bool) "skipped UID leaves more" true receipt.more
   | Error error -> Alcotest.failf "oversized hydration: %a"
       Imap_sync.Error.pp error);
  let client,_=scripted_client ~sw "hydrate-after" [
    examine ~tag:4 ~exists:2 ~uidnext:3 ();
    "A00000005 OK unselected\r\n"] in
  match Imap_sync.Engine.hydrate_once ~after_uid:(uid 1L)
      ~max_body_bytes:1000L
      ~ctx:(context ~store ~spool_dir ~next_id:(fun () -> "unused") client)
      () with
  | Ok receipt ->
      Alcotest.(check int) "nothing missing after the skipped UID" 0
        receipt.hydrated;
      Alcotest.(check bool) "continuation complete" false receipt.more
  | Error error -> Alcotest.failf "continued hydration: %a"
      Imap_sync.Error.pp error

let test_hydration_skips_message_above_total_budget () =
  with_fixture @@ fun ~database ~blob_dir ~spool_dir ~maildir:_ ->
  Eio.Switch.run @@ fun sw ->
  let store=open_store ~sw ~database ~blob_dir in
  publish_two ~sw ~store ~spool_dir;
  let size=String.length message in
  let client,_=scripted_client ~sw "hydrate-total" [
    examine ~tag:4 ~exists:2 ~uidnext:3 ();
    Printf.sprintf "* 1 FETCH (UID 1 FLAGS () RFC822.SIZE %d)\r\n\
      * 2 FETCH (UID 2 FLAGS () RFC822.SIZE %d)\r\n\
      A00000005 OK fetched\r\n" (size*2) size;
    Printf.sprintf "* 2 FETCH (UID 2 BODY[] {%d}\r\n" size;
    message ^ ")\r\nA00000006 OK fetched\r\n";
    "A00000007 OK unselected\r\n"] in
  match Imap_sync.Engine.hydrate_once
      ~max_total_bytes:(Int64.of_int (size+1))
      ~ctx:(context ~store ~spool_dir ~next_id:(fun () -> "total") client)
      () with
  | Ok receipt ->
      Alcotest.(check int) "fitting UID hydrated" 1 receipt.hydrated;
      Alcotest.(check (list int64)) "unfittable UID skipped" [1L]
        (List.map Imap.Uid.to_int64 receipt.skipped)
  | Error error -> Alcotest.failf "total budget hydration: %a"
      Imap_sync.Error.pp error

let test_hydration_keeps_counts_after_concurrent_publish () =
  with_fixture @@ fun ~database ~blob_dir ~spool_dir ~maildir:_ ->
  Eio.Switch.run @@ fun sw ->
  let store=open_store ~sw ~database ~blob_dir in
  publish_two ~sw ~store ~spool_dir;
  let size=String.length message in
  let republish ()=
    let client,_=scripted_client ~sw "one-message" [
      examine ~tag:4 ~exists:1 ~uidnext:3 ();
      "* 1 FETCH (UID 1 FLAGS ())\r\nA00000005 OK fetched\r\n";
      "* SEARCH 1\r\nA00000006 OK searched\r\n";
      "A00000007 OK unselected\r\n"] in
    match Imap_sync.Engine.scan_once ~ctx:(context ~store ~spool_dir client)
        ~stage_id:"one-message" () with
    | Ok _ -> ""
    | Error error -> Alcotest.failf "concurrent scan: %a"
        Imap_sync.Error.pp error in
  let flow=Eio_mock.Flow.make "hydrate-concurrent" in
  let size_row n=Printf.sprintf
    "* %d FETCH (UID %d FLAGS () RFC822.SIZE %d)\r\n" n n size in
  let body n=
    `Return (Printf.sprintf "* %d FETCH (UID %d BODY[] {%d}\r\n" n n size) in
  Eio_mock.Flow.on_read flow [
    `Return "* OK ready\r\n";
    `Return "* CAPABILITY IMAP4rev1 UNSELECT\r\nA00000001 OK done\r\n";
    `Return "A00000002 OK logged in\r\n";
    `Return "* CAPABILITY IMAP4rev1 UNSELECT\r\nA00000003 OK done\r\n";
    `Return (examine ~tag:4 ~exists:2 ~uidnext:3 ());
    `Return (size_row 1 ^ size_row 2 ^ "A00000005 OK fetched\r\n");
    body 1;
    `Return (message ^ ")\r\nA00000006 OK fetched\r\n");
    body 2;
    `Run (fun () -> republish () ^ message ^
      ")\r\nA00000007 OK fetched\r\n");
    `Return "A00000008 OK unselected\r\n"];
  let auth=Imap_eio.Auth.password ~username:"alice" ~password:"secret"
    ~allow_insecure_transport:true () in
  let client=match Imap_eio.Client.of_flow ~sw ~auth flow with
    | Ok client -> client
    | Error error -> Alcotest.fail (Imap_eio.Client.error_to_string error) in
  let spool_id=ref 0 in
  let next_id ()=incr spool_id; string_of_int !spool_id in
  match Imap_sync.Engine.hydrate_once
      ~ctx:(context ~store ~spool_dir ~next_id client) ()
  with
  | Ok receipt ->
      Alcotest.(check int) "committed attach reported" 1 receipt.hydrated;
      Alcotest.(check bool) "concurrent publish leaves more" true
        receipt.more
  | Error error -> Alcotest.failf "concurrent hydration: %a"
      Imap_sync.Error.pp error

let test_audit_skips_blob_above_budget () =
  with_fixture @@ fun ~database ~blob_dir ~spool_dir ~maildir:_ ->
  Eio.Switch.run @@ fun sw ->
  let store=open_store ~sw ~database ~blob_dir in
  publish_two ~sw ~store ~spool_dir;
  let attach n body=
    let blob=Imap_store.Blob.put store ~source:(Eio.Flow.string_source body)
      ~length:(Int64.of_int (String.length body)) () in
    Imap_store.Blob.attach store ~scope ~uidvalidity:(epoch 11L) ~uid:(uid n)
      blob in
  attach 1L (message ^ message);
  attach 2L message;
  match Imap_sync.Engine.audit_cache_once
      ~max_total_bytes:(Int64.of_int (String.length message)) ~store ~scope
      () with
  | Ok receipt ->
      Alcotest.(check int) "fitting blob checked" 1 receipt.checked;
      Alcotest.(check (list int64)) "large blob skipped" [1L]
        (List.map Imap.Uid.to_int64 receipt.skipped);
      Alcotest.(check (option int64)) "last UID advanced past it" (Some 2L)
        (Option.map Imap.Uid.to_int64 receipt.last_uid);
      Alcotest.(check bool) "audit complete" false receipt.more
  | Error error -> Alcotest.failf "audit: %a" Imap_sync.Error.pp error

let test_digest_checks_receipt_epoch () =
  with_fixture @@ fun ~database ~blob_dir ~spool_dir ~maildir:_ ->
  Eio.Switch.run @@ fun sw ->
  let store=open_store ~sw ~database ~blob_dir in
  publish_two ~sw ~store ~spool_dir;
  let client,_=scripted_client ~sw "digest-epoch" [
    examine ~tag:4 ~exists:2 ~uidnext:3 ();
    "A00000005 OK unselected\r\n"] in
  match Imap_sync.Engine.fetch_uid_digest
      ~ctx:(context ~store ~spool_dir client) ~uidvalidity:(epoch 12L)
      ~uid:(uid 1L)
      ~spool:Eio.Path.(spool_dir / "digest-epoch") () with
  | Error Imap_sync.Error.Uidvalidity_changed -> ()
  | Error error -> Alcotest.failf "wrong digest epoch error: %a"
      Imap_sync.Error.pp error
  | Ok _ -> Alcotest.fail "digest ignored the receipt epoch"


(* [preview_deletions] is the deletion case of [Plan.preview_sync], as
   imap-sync plan-deletions reads it. *)
let preview_deletions ?min_absence_scans ~spool_dir ~store ~maildir ~scope
    ~policy ~on_preview () =
  Imap_sync.Plan.preview_sync ~allow_bootstrap_duplicates:true
    ?min_absence_scans ~store ~maildir ~scope ~policy ~spool_dir
    ~on_preview:(function
      | Imap_sync.Plan.Preview_deletion item -> on_preview item
      | _ -> ()) ()

(* [published_rows store] is every row of the published snapshot. *)
let published_rows store =
  let cursor=Imap_store.load_cursor store ~scope in
  match Imap_store.snapshot_page store ~scope ~cursor ~limit:10_000 () with
  | `Rows rows -> rows
  | `Stale_revision -> Alcotest.fail "published snapshot changed"

let test_staged_condstore_wire () =
  with_fixture @@ fun ~database ~blob_dir ~spool_dir ~maildir:_ ->
  Eio.Switch.run @@ fun sw ->
  let store=open_store ~sw ~database ~blob_dir in
  let initial,first_wire=scripted_condstore ~sw ~modseq:20L ~seen:false () in
  (match Imap_sync.Engine.scan_once ~ctx:(context ~store ~spool_dir initial)
    ~stage_id:"condstore-first" () with
   | Ok _ -> ()
   | Error error -> Alcotest.failf "first scan: %a" Imap_sync.Error.pp error);
  let second,second_wire=scripted_condstore ~sw ~modseq:21L ~seen:true () in
  (match Imap_sync.Engine.scan_once ~ctx:(context ~store ~spool_dir second)
    ~stage_id:"condstore-second" () with
   | Ok _ -> ()
   | Error error -> Alcotest.failf "second scan: %a" Imap_sync.Error.pp error);
  let contains haystack needle =
    let n=String.length needle in
    let rec loop i=i+n<=String.length haystack &&
      (String.sub haystack i n=needle || loop (i+1)) in
    loop 0 in
  Alcotest.(check bool) "initial scan requests full metadata" true
    (contains (Buffer.contents first_wire) "UID FETCH 1:1 (UID FLAGS MODSEQ)");
  Alcotest.(check bool) "follow-up sends CHANGEDSINCE" true
    (contains (Buffer.contents second_wire) "CHANGEDSINCE 20");
  let snapshot=published_rows store in
  Alcotest.(check (list string)) "delta flags published" ["\\Seen"]
    (List.map Mail_flag.Imap_flag.to_wire
      (List.hd snapshot).flags);
  let baseline,baseline_wire=scripted_condstore ~nomodseq:true
    ~sw ~modseq:22L ~seen:false () in
  (match Imap_sync.Engine.scan_once ~ctx:(context ~store ~spool_dir baseline)
    ~stage_id:"condstore-nomodseq" () with
   | Ok _ -> ()
   | Error error -> Alcotest.failf "NOMODSEQ scan: %a"
       Imap_sync.Error.pp error);
  Alcotest.(check bool) "NOMODSEQ uses full FETCH" true
    (contains (Buffer.contents baseline_wire) "UID FETCH 1:1 (UID FLAGS)");
  Alcotest.(check bool) "NOMODSEQ does not send CHANGEDSINCE" false
    (contains (Buffer.contents baseline_wire) "CHANGEDSINCE");
  Alcotest.(check bool) "NOMODSEQ clears anchor" true
    ((Imap_store.load_cursor store ~scope).anchor=None)

let test_staged_messagelimit_continuation () =
  with_fixture @@ fun ~database ~blob_dir ~spool_dir ~maildir:_ ->
  Eio.Switch.run @@ fun sw ->
  let store=open_store ~sw ~database ~blob_dir in
  let wire=Buffer.create 512 in
  let pp ppf data=
    Buffer.add_string wire data;
    Format.pp_print_string ppf data in
  let flow=Eio_mock.Flow.make ~pp "staged-messagelimit" in
  let caps="IMAP4rev1 UNSELECT MESSAGELIMIT=2" in
  Eio_mock.Flow.on_read flow [
    `Return "* OK ready\r\n";
    `Return ("* CAPABILITY "^caps^"\r\nA00000001 OK done\r\n");
    `Return "A00000002 OK logged in\r\n";
    `Return ("* CAPABILITY "^caps^"\r\nA00000003 OK done\r\n");
    `Return "* 3 EXISTS\r\n* OK [UIDVALIDITY 11] valid\r\n* OK [UIDNEXT 4] next\r\nA00000004 OK [READ-ONLY] selected\r\n";
    `Return "* 3 FETCH (UID 3 FLAGS ())\r\n* 2 FETCH (UID 2 FLAGS ())\r\nA00000005 OK [MESSAGELIMIT 2 2] partial\r\n";
    `Return "* 1 FETCH (UID 1 FLAGS ())\r\nA00000006 OK fetched\r\n";
    `Return "* SEARCH 3 2\r\nA00000007 OK [MESSAGELIMIT 2 2] partial\r\n";
    `Return "* SEARCH 1\r\nA00000008 OK searched\r\n";
    `Return "A00000009 OK unselected\r\n";
  ];
  let auth=Imap_eio.Auth.password ~username:"alice" ~password:"secret"
    ~allow_insecure_transport:true () in
  let client=match Imap_eio.Client.of_flow ~sw ~auth flow with
    | Ok client -> client
    | Error error -> Alcotest.fail
        (Imap_eio.Client.error_to_string error) in
  let published=match Imap_sync.Engine.scan_once
    ~ctx:(context ~store ~spool_dir client) ~stage_id:"partial-complete" () with
    | Ok published -> published
    | Error error -> Alcotest.failf "partial staged scan: %a"
        Imap_sync.Error.pp error in
  Alcotest.(check int64) "all partial rows published" 3L
    published.row_count;
  let snapshot=published_rows store in
  Alcotest.(check (list int64)) "all UIDs survived continuation"
    [1L;2L;3L]
    (List.map (fun (row:M.row) -> Imap.Uid.to_int64 row.uid)
      snapshot);
  let transcript=Buffer.contents wire in
  Alcotest.(check bool) "FETCH resumed below processed UID" true
    (contains_substring transcript "UID FETCH 1:1 (UID FLAGS)");
  Alcotest.(check bool) "SEARCH resumed below processed UID" true
    (contains_substring transcript "UIDBEFORE 2")

let test_staged_changedsince_messagelimit () =
  with_fixture @@ fun ~database ~blob_dir ~spool_dir ~maildir:_ ->
  Eio.Switch.run @@ fun sw ->
  let store=open_store ~sw ~database ~blob_dir in
  let make_client ~delta =
    let wire=Buffer.create 512 in
    let pp ppf data=
      Buffer.add_string wire data;
      Format.pp_print_string ppf data in
    let flow=Eio_mock.Flow.make ~pp "staged-changes-messagelimit" in
    let caps="IMAP4rev1 UNSELECT CONDSTORE MESSAGELIMIT=2" in
    let header=[
      `Return "* OK ready\r\n";
      `Return ("* CAPABILITY "^caps^"\r\nA00000001 OK done\r\n");
      `Return "A00000002 OK logged in\r\n";
      `Return ("* CAPABILITY "^caps^"\r\nA00000003 OK done\r\n");
      `Return ("* 3 EXISTS\r\n* OK [UIDVALIDITY 11] valid\r\n* OK [UIDNEXT 4] next\r\n* OK [HIGHESTMODSEQ "^
        (if delta then "21" else "20")^"] modseq\r\nA00000004 OK [READ-ONLY] selected\r\n")
    ] in
    let replies=if delta then [
      `Return "* 3 FETCH (UID 3 FLAGS (\\Seen) MODSEQ (21))\r\nA00000005 OK [MESSAGELIMIT 2 2] partial\r\n";
      `Return "* 1 FETCH (UID 1 FLAGS (\\Seen) MODSEQ (21))\r\nA00000006 OK changed\r\n";
      `Return "* SEARCH 1 2 3\r\nA00000007 OK searched\r\n";
      `Return "A00000008 OK unselected\r\n";
    ] else [
      `Return "* 1 FETCH (UID 1 FLAGS () MODSEQ (20))\r\n* 2 FETCH (UID 2 FLAGS () MODSEQ (20))\r\n* 3 FETCH (UID 3 FLAGS () MODSEQ (20))\r\nA00000005 OK fetched\r\n";
      `Return "* SEARCH 1 2 3\r\nA00000006 OK searched\r\n";
      `Return "A00000007 OK unselected\r\n";
    ] in
    Eio_mock.Flow.on_read flow (header@replies);
    let auth=Imap_eio.Auth.password ~username:"alice" ~password:"secret"
      ~allow_insecure_transport:true () in
    let client=match Imap_eio.Client.of_flow ~sw ~auth flow with
      | Ok client -> client
      | Error error -> Alcotest.fail
          (Imap_eio.Client.error_to_string error) in
    client,wire in
  let initial,_=make_client ~delta:false in
  (match Imap_sync.Engine.scan_once ~ctx:(context ~store ~spool_dir initial)
    ~stage_id:"changes-baseline" () with
   | Ok _ -> ()
   | Error error -> Alcotest.failf "baseline scan: %a"
       Imap_sync.Error.pp error);
  let changed,wire=make_client ~delta:true in
  (match Imap_sync.Engine.scan_once ~ctx:(context ~store ~spool_dir changed)
    ~stage_id:"changes-partial" () with
   | Ok receipt ->
       Alcotest.(check int64) "complete incremental inventory" 3L
         receipt.row_count
   | Error error -> Alcotest.failf "partial CHANGEDSINCE scan: %a"
       Imap_sync.Error.pp error);
  let snapshot=published_rows store in
  Alcotest.(check (list (list string))) "changed flags published"
    [["\\Seen"];[];["\\Seen"]]
    (List.map (fun (row:M.row) ->
      List.map Mail_flag.Imap_flag.to_wire row.flags)
      snapshot);
  Alcotest.(check bool) "CHANGEDSINCE resumed below processed UID" true
    (contains_substring (Buffer.contents wire)
      "UID FETCH 1:1 (UID FLAGS MODSEQ) (CHANGEDSINCE 20)")

let test_staged_timeout_discards_stage () =
  let dir=root () in
  Fun.protect ~finally:(fun () -> remove_tree dir) @@ fun () ->
  Eio_main.run @@ fun env ->
  Eio.Switch.run @@ fun sw ->
  let fs=Eio.Stdenv.fs env in
  let clock=Eio.Stdenv.clock env in
  let store=open_store ~sw
    ~database:Eio.Path.(fs / dir / "sync.db")
    ~blob_dir:Eio.Path.(fs / dir / "blob") in
  let spool_dir=Eio.Path.(fs / dir / "spool") in
  let flow=Eio_mock.Flow.make "staged-timeout" in
  Eio_mock.Flow.on_read flow [
    `Return "* OK ready\r\n";
    `Return "* CAPABILITY IMAP4rev1 UNSELECT\r\nA00000001 OK done\r\n";
    `Return "A00000002 OK logged in\r\n";
    `Return "* CAPABILITY IMAP4rev1 UNSELECT\r\nA00000003 OK done\r\n";
    `Return "* 1 EXISTS\r\n* OK [UIDVALIDITY 11] valid\r\n* OK [UIDNEXT 2] next\r\nA00000004 OK [READ-ONLY] selected\r\n";
    `Run (fun () ->
      Alcotest.(check (list string)) "stage exists before timeout"
        ["timeout-stage"] (Imap_store.abandoned_stages store);
      Eio.Time.sleep clock 1.;
      "");
  ];
  let auth=Imap_eio.Auth.password ~username:"alice" ~password:"secret"
    ~allow_insecure_transport:true () in
  let client=match Imap_eio.Client.of_flow ~sw ~auth flow with
    | Ok client -> client
    | Error error -> Alcotest.fail
        (Imap_eio.Client.error_to_string error) in
  (try
     ignore (Eio.Time.with_timeout_exn clock 0.02 (fun () ->
       Imap_sync.Engine.scan_once ~ctx:(context ~store ~spool_dir client)
         ~stage_id:"timeout-stage" ()));

     Alcotest.fail "staged FETCH did not time out"
   with Eio.Time.Timeout -> ());
  Alcotest.(check (list string)) "cancelled stage discarded" []
    (Imap_store.abandoned_stages store);
  Alcotest.(check int64) "published cursor unchanged" 0L
    (Imap_store.load_cursor store ~scope).revision

exception Watch_timeout_seen

let test_watch_deadlines () =
  let dir=root () in
  Fun.protect ~finally:(fun () -> remove_tree dir) @@ fun () ->
  Eio_main.run @@ fun env ->
  Eio.Switch.run @@ fun sw ->
  let fs=Eio.Stdenv.fs env in
  let clock=Eio.Stdenv.clock env in
  let store=open_store ~sw
    ~database:Eio.Path.(fs / dir / "sync.db")
    ~blob_dir:Eio.Path.(fs / dir / "blob") in
  let spool_dir=Eio.Path.(fs / dir / "spool") in
  let run ~connect ~stage_id ~expected =
    let seen=ref false in
    (try
       ignore (Imap_sync.Watch.run ~clock ~connect
         ~next_stage_id:(fun () -> stage_id)
         ~on_publish:(fun _ -> Alcotest.fail "timed-out watch published")
         ~on_retry:(fun issue ->
           seen:=true;
           if issue<>expected then Alcotest.fail "wrong watch timeout kind";
           raise Watch_timeout_seen)
         ~connect_timeout_seconds:0.02 ~scan_timeout_seconds:0.05 ());
       Alcotest.fail "watch returned without a timeout"
     with Watch_timeout_seen -> ());
    Alcotest.(check bool) "watch observed timeout" true !seen in
  run ~stage_id:"connect-timeout" ~expected:Imap_sync.Watch.Connect_timed_out
    ~connect:(fun ~sw:_ ->
      Eio.Time.sleep clock 1.;
      Error (Imap_sync.Error.Client Imap_eio.Error.Closed));
  let connect ~sw =
    let flow=Eio_mock.Flow.make "watch-scan-timeout" in
    Eio_mock.Flow.on_read flow [
      `Return "* OK ready\r\n";
      `Return "* CAPABILITY IMAP4rev1 UNSELECT\r\nA00000001 OK done\r\n";
      `Return "A00000002 OK logged in\r\n";
      `Return "* CAPABILITY IMAP4rev1 UNSELECT\r\nA00000003 OK done\r\n";
      `Return "* 1 EXISTS\r\n* OK [UIDVALIDITY 11] valid\r\n* OK [UIDNEXT 2] next\r\nA00000004 OK [READ-ONLY] selected\r\n";
      `Run (fun () ->
        Alcotest.(check (list string)) "watch stage before timeout"
          ["watch-scan-timeout"] (Imap_store.abandoned_stages store);
        Eio.Time.sleep clock 1.;
        "");
    ];
    let auth=Imap_eio.Auth.password ~username:"alice" ~password:"secret"
      ~allow_insecure_transport:true () in
    connected ~store ~spool_dir (Imap_eio.Client.of_flow ~sw ~auth flow) in
  run ~stage_id:"watch-scan-timeout" ~expected:Imap_sync.Watch.Scan_timed_out
    ~connect;
  Alcotest.(check (list string)) "watch discarded timed-out stage" []
    (Imap_store.abandoned_stages store);
  Alcotest.(check int64) "watch cursor stayed unpublished" 0L
    (Imap_store.load_cursor store ~scope).revision

exception Watch_stopped

let test_watch_keepalive_does_not_rescan () =
  let dir=root () in
  Fun.protect ~finally:(fun () -> remove_tree dir) @@ fun () ->
  Eio_main.run @@ fun env ->
  Eio.Switch.run @@ fun sw ->
  let fs=Eio.Stdenv.fs env in
  let clock=Eio.Stdenv.clock env in
  let store=open_store ~sw
    ~database:Eio.Path.(fs / dir / "sync.db")
    ~blob_dir:Eio.Path.(fs / dir / "blob") in
  let spool_dir=Eio.Path.(fs / dir / "spool") in
  let caps="IMAP4rev1 UNSELECT IDLE" in
  let prelude=[
    `Return "* OK ready\r\n";
    `Return ("* CAPABILITY " ^ caps ^ "\r\nA00000001 OK done\r\n");
    `Return "A00000002 OK logged in\r\n";
    `Return ("* CAPABILITY " ^ caps ^ "\r\nA00000003 OK done\r\n")] in
  let select tag uidnext=`Return (Printf.sprintf
    "* 0 EXISTS\r\n* OK [UIDVALIDITY 11] valid\r\n\
     * OK [UIDNEXT %d] next\r\nA%08d OK [READ-ONLY] selected\r\n"
    uidnext tag) in
  let empty_scan=prelude @ [select 4 1;
    `Return "A00000005 OK unselected\r\n"] in
  let idle_wire=Buffer.create 256 in
  let idle_session=prelude @ [
    select 4 1;
    `Return "+ idling\r\n";
    `Return "* OK Still here\r\n";
    `Return "A00000005 OK idle done\r\n";
    `Return "A00000006 OK unselected\r\n";
    select 7 1;
    `Return "+ idling\r\n";
    `Return "* 1 EXISTS\r\n";
    `Return "A00000008 OK idle done\r\n";
    `Return "A00000009 OK unselected\r\n";
    select 10 2;
    `Return "A00000011 OK unselected\r\n"] in
  let connections=ref [empty_scan,None; idle_session,Some idle_wire;
    empty_scan,None] in
  let connect ~sw =
    match !connections with
    | [] -> Alcotest.fail "watch opened an unexpected connection"
    | (lines,wire)::rest ->
        connections:=rest;
        let pp ppf data=
          Option.iter (fun wire -> Buffer.add_string wire data) wire;
          Format.pp_print_string ppf data in
        let flow=Eio_mock.Flow.make ~pp "watch-keepalive" in
        Eio_mock.Flow.on_read flow lines;
        let auth=Imap_eio.Auth.password ~username:"alice" ~password:"secret"
          ~allow_insecure_transport:true () in
        connected ~store ~spool_dir
          (Imap_eio.Client.of_flow ~sw ~auth flow) in
  let published=ref 0 in
  let stage=ref 0 in
  (try
     ignore (Imap_sync.Watch.run ~clock ~connect
       ~next_stage_id:(fun () -> incr stage;
         Printf.sprintf "keepalive-%d" !stage)
       ~on_publish:(fun _ ->
         incr published;
         if !published=2 then raise Watch_stopped)
       ~on_retry:(fun _ -> Alcotest.fail "keepalive watch retried") ());
     Alcotest.fail "watch returned"
   with Watch_stopped -> ());
  let idles=List.length (String.split_on_char '\n' (Buffer.contents idle_wire)
    |> List.filter (fun line -> String.starts_with ~prefix:"A" line &&
      String.ends_with ~suffix:" IDLE\r" line)) in
  Alcotest.(check int) "keepalive re-entered IDLE before the rescan" 2 idles;
  Alcotest.(check int) "every scripted connection used" 0
    (List.length !connections)

let test_watch_rejects_long_idle_renewal () =
  Eio_main.run @@ fun env ->
  match Imap_sync.Watch.run ~clock:(Eio.Stdenv.clock env)
      ~connect:(fun ~sw:_ -> Alcotest.fail "invalid watch connected")
      ~next_stage_id:(fun () -> "unused")
      ~on_publish:(fun _ -> ()) ~idle_renew_seconds:1741. () with

  | Error (Imap_sync.Watch.Invalid_configuration _) -> ()
  | _ -> Alcotest.fail "IDLE renewal above 29 minutes accepted"

let test_remote_source_vanishes_before_archive () =
  with_fixture @@ fun ~database ~blob_dir ~spool_dir ~maildir ->
  Eio.Switch.run @@ fun sw ->
  let store=open_store ~sw ~database ~blob_dir in
  let disappearing=scripted_scan ~sw ~has_message:true
    ~missing_body:true () in
  let next_id ()=Alcotest.fail "vanished source reserved journal" in
  (match Imap_sync.Bridge.copy_once
      ~ctx:(context ~store ~spool_dir ~next_id disappearing) ~maildir
      ~stage_id:"vanishing-first" () with
   | Error (Imap_sync.Error.Source_vanished vanished) when vanished=uid 1L -> ()
   | Error error -> Alcotest.failf "vanished source: %a"
       Imap_sync.Error.pp error
   | Ok _ -> Alcotest.fail "vanished source reported convergence");
  Alcotest.(check int) "vanished source made no journal" 0
    (List.length (all_active_operations store ~scope));
  Alcotest.(check int) "vanished source made no pair" 0
    (List.length (all_pairs store ~scope));
  Alcotest.(check int) "vanished source made no Maildir file" 0
    (List.length (Md.scan maildir));
  let absent=scripted_scan ~sw ~has_message:false () in
  let next_id ()=Alcotest.fail "absent source copied" in
  (match Imap_sync.Bridge.copy_once
      ~ctx:(context ~store ~spool_dir ~next_id absent) ~maildir
      ~stage_id:"vanishing-rescan" () with
   | Ok receipt ->
       Alcotest.(check int) "rescan has no remote copy" 0
         receipt.remote_to_local
   | Error error -> Alcotest.failf "vanished source rescan: %a"
       Imap_sync.Error.pp error)

let test_local_occurrence_changes_before_archive () =
  with_fixture @@ fun ~database:_ ~blob_dir:_ ~spool_dir:_ ~maildir ->
  let local=Md.append maildir
    ~source:(Eio.Flow.string_source message) ~length ~flags:[] () in
  let changed=Md.set_flags maildir local
    [Mail_flag.Imap_flag.system Mail_flag.Imap_flag.Seen] in
  (match Md.with_unchanged_occurrence maildir local
      (fun () -> Alcotest.fail "stale source was read") with
   | Error `Changed -> ()
   | Ok _ -> Alcotest.fail "stale source accepted");
  (match Md.with_unchanged_occurrence maildir changed
      (fun () -> Md.remove maildir changed) with
   | Error `Changed -> ()
   | Ok _ -> Alcotest.fail "source removed during archival accepted");
  Alcotest.(check int) "changed source left no occurrence" 0
    (List.length (Md.scan maildir))

let test_invalid_blob_rejects_unsent_append () =
  with_fixture @@ fun ~database ~blob_dir ~spool_dir ~maildir ->
  let local=Md.append maildir
    ~source:(Eio.Flow.string_source message) ~length ~flags:[] () in
  Eio.Switch.run @@ fun sw ->
  let store=open_store ~sw ~database ~blob_dir in
  let client=scripted_scan ~sw ~has_message:false () in
  let id="unsent-corrupt-append" in
  let corrupt_blob () =
    let digest=Digestif.SHA256.(to_hex (digest_string message)) in
    let path=Filename.concat (Eio.Path.native_exn blob_dir)
      ("sha256-" ^ digest) in
    let output=open_out_bin path in
    output_string output "corrupt";
    close_out output;
    id in
  (match Imap_sync.Bridge.copy_once
      ~ctx:(context ~store ~spool_dir ~next_id:corrupt_blob client) ~maildir
      ~stage_id:"corrupt-append-scan" () with
   | Error (Imap_sync.Error.Incomplete _) -> ()
   | Error error -> Alcotest.failf "corrupt source: %a"
       Imap_sync.Error.pp error
   | Ok _ -> Alcotest.fail "corrupt source was uploaded");
  Alcotest.(check bool) "unsent bridge operation rejected" true
    (match J.find_operation store ~id with
     | Some operation -> operation.state=J.Rejected
     | None -> false);
  Alcotest.(check int) "no active APPEND work" 0
    (List.length (all_active_operations store ~scope));
  Alcotest.(check int) "local source retained" 1
    (List.length (Md.scan maildir));
  Alcotest.(check string) "same local occurrence" local.id
    (List.hd (Md.scan maildir)).id

let test_missing_appenduid_keeps_reason () =
  with_fixture @@ fun ~database ~blob_dir ~spool_dir ~maildir ->
  ignore (Md.append maildir
    ~source:(Eio.Flow.string_source message) ~length ~flags:[] ());
  let id="missing-appenduid" in
  Eio.Switch.run (fun sw ->
    let store=open_store ~sw ~database ~blob_dir in
    let client=scripted_scan ~sw ~has_message:false
      ~caps:"IMAP4rev1 UNSELECT"
      ~append_without_uidplus:true () in
    (match Imap_sync.Bridge.copy_once
        ~ctx:(context ~store ~spool_dir ~next_id:(fun () -> id) client)
        ~maildir ~stage_id:"missing-appenduid-scan" () with
     | Error (Imap_sync.Error.Pending_operations [pending])
       when pending=id -> ()
     | Error error -> Alcotest.failf "missing APPENDUID: %a"
         Imap_sync.Error.pp error
     | Ok _ -> Alcotest.fail "unidentified APPEND reported convergence"));
  Eio.Switch.run (fun sw ->
    let store=open_store ~sw ~database ~blob_dir in
    let operation=Option.get (J.find_operation store ~id) in
    Alcotest.(check bool) "APPEND remains ambiguous" true
      (operation.state=J.Ambiguous);
    Alcotest.(check (option string)) "reason persisted across restart"
      (Some "APPEND completed without an attributable APPENDUID")
      operation.receipt;
    Alcotest.(check int) "no pair committed" 0
      (List.length (all_pairs store ~scope)));
  Alcotest.(check int) "local occurrence remains" 1
    (List.length (Md.scan maildir))

let test_appenduid_readback_rejects_changed_body () =
  with_fixture @@ fun ~database ~blob_dir ~spool_dir ~maildir ->
  let local=Md.append maildir
    ~source:(Eio.Flow.string_source message) ~length ~flags:[] () in
  Eio.Switch.run @@ fun sw ->
  let store=open_store ~sw ~database ~blob_dir in
  let wire=Buffer.create 512 in
  let pp ppf data=
    Buffer.add_string wire data;
    Format.pp_print_string ppf data in
  let flow=Eio_mock.Flow.make ~pp "appenduid-changed-body" in
  let changed=String.mapi
    (fun i c -> if i=0 then 'X' else c) message in
  Eio_mock.Flow.on_read flow [
    `Return "* OK ready\r\n";
    `Return "* CAPABILITY IMAP4rev1 UNSELECT UIDPLUS\r\nA00000001 OK done\r\n";
    `Return "A00000002 OK logged in\r\n";
    `Return "* CAPABILITY IMAP4rev1 UNSELECT UIDPLUS\r\nA00000003 OK done\r\n";
    `Return "* 0 EXISTS\r\n* OK [UIDVALIDITY 11] valid\r\n* OK [UIDNEXT 1] next\r\nA00000004 OK [READ-ONLY] selected\r\n";
    `Return "A00000005 OK unselected\r\n";
    `Return "+ ready for literal\r\n";
    `Return "A00000006 OK [APPENDUID 11 1] appended\r\n";
    `Return "* 1 EXISTS\r\n* OK [UIDVALIDITY 11] valid\r\n* OK [UIDNEXT 2] next\r\nA00000007 OK [READ-ONLY] selected\r\n";
    `Return (Printf.sprintf "* 1 FETCH (UID 1 BODY[] {%d}\r\n"
      (String.length changed));
    `Return (changed ^ ")\r\nA00000008 OK fetched\r\n");
    `Return "* 1 FETCH (UID 1 FLAGS () INTERNALDATE \"01-Jan-2020 00:00:00 +0000\")\r\nA00000009 OK fetched\r\n";
    `Return "A00000010 OK unselected\r\n";
  ];
  let auth=Imap_eio.Auth.password ~username:"alice" ~password:"secret"
    ~allow_insecure_transport:true () in
  let client=match Imap_eio.Client.of_flow ~sw ~auth flow with
    | Ok client -> client
    | Error error -> Alcotest.fail
        (Imap_eio.Client.error_to_string error) in
  let id="appenduid-body-mismatch" in
  (match Imap_sync.Bridge.copy_once
      ~ctx:(context ~store ~spool_dir ~next_id:(fun () -> id) client)
      ~maildir ~stage_id:"appenduid-body-scan" () with
   | Error (Imap_sync.Error.Content_diverged got) when got=id -> ()
   | Error error -> Alcotest.failf "APPENDUID readback: %a"
       Imap_sync.Error.pp error
   | Ok _ -> Alcotest.fail "changed remote APPEND bytes were paired");
  Alcotest.(check bool) "APPEND readback fetched remote bytes" true
    (contains_substring (Buffer.contents wire) "BODY.PEEK[]");
  Alcotest.(check bool) "identified mutation remains journaled" true
    (match J.find_operation store ~id with
     | Some {state=J.Observed;receipt_uid=Some _;_} -> true
     | _ -> false);
  Alcotest.(check int) "no corrupt pair committed" 0
    (List.length (all_pairs store ~scope));
  let changed_hash=Digestif.SHA256.(to_hex (digest_string changed)) in
  Alcotest.(check bool) "readback made no orphan changed-body blob" false
    (Sys.file_exists (Filename.concat (Eio.Path.native_exn blob_dir)
      ("sha256-" ^ changed_hash)));
  Alcotest.(check bool) "source occurrence preserved" true
    (Md.find maildir ~id:local.id<>None)

let test_append_date_survives_uncertain_reply () =
  with_fixture @@ fun ~database ~blob_dir ~spool_dir ~maildir:_ ->
  Eio.Switch.run @@ fun sw ->
  let store=open_store ~sw ~database ~blob_dir in
  let blob=Imap_store.Blob.put store
    ~source:(Eio.Flow.string_source message) ~length () in
  let flow=Eio_mock.Flow.make "dated-append" in
  Eio_mock.Flow.on_read flow [
    `Return "* OK ready\r\n";
    `Return "* CAPABILITY IMAP4rev1\r\nA00000001 OK done\r\n";
    `Return "A00000002 OK logged in\r\n";
    `Return "* CAPABILITY IMAP4rev1\r\nA00000003 OK done\r\n";
    `Return "+ continue\r\n";
    `Return "A00000004 OK appended\r\n";
  ];
  let auth=Imap_eio.Auth.password ~username:"alice" ~password:"secret"
    ~allow_insecure_transport:true () in
  let client=match Imap_eio.Client.of_flow ~sw ~auth flow with
    | Ok client -> client
    | Error error -> Alcotest.fail (Imap_eio.Client.error_to_string error) in
  let date=ok (Imap.Internal_date.of_string
    "26-Sep-2025 12:34:56 +0000") in
  J.prepare_operation store {(operation ~kind:J.Append ~id:"dated-append"
    ~local_id:"dated-local" ~source_uid:None ~blob ~flags:[]) with
    internal_date=Some date};
  (match Imap_sync.Engine.append_blob_journaled
    ~ctx:(context ~store ~spool_dir client) ~id:"dated-append" blob with
   | Ok Imap_sync.Engine.Needs_reconciliation -> ()
   | Ok _ -> Alcotest.fail "unattributed APPEND was identified"
   | Error error -> Alcotest.failf "dated APPEND: %a"
       Imap_sync.Error.pp error);
  let op=match J.find_operation store ~id:"dated-append" with
    | Some op -> op
    | None -> Alcotest.fail "dated APPEND operation missing" in
  Alcotest.(check bool) "APPEND remains ambiguous" true
    (op.state=J.Ambiguous);
  Alcotest.(check (option string)) "intended date retained"
    (Some "26-Sep-2025 12:34:56 +0000")
    (Option.map Imap.Internal_date.to_string op.internal_date)

(* Engine sends only an APPEND the caller prepared, for the blob it names,
   and settles the operation itself when the server refuses it. *)
let test_append_requires_prepared_operation () =
  with_fixture @@ fun ~database ~blob_dir ~spool_dir ~maildir:_ ->
  Eio.Switch.run @@ fun sw ->
  let store=open_store ~sw ~database ~blob_dir in
  let blob=Imap_store.Blob.put store
    ~source:(Eio.Flow.string_source message) ~length () in
  let other=Imap_store.Blob.put store
    ~source:(Eio.Flow.string_source "other") ~length:5L () in
  let flow=Eio_mock.Flow.make "refused-append" in
  Eio_mock.Flow.on_read flow [
    `Return "* OK ready\r\n";
    `Return "* CAPABILITY IMAP4rev1\r\nA00000001 OK done\r\n";
    `Return "A00000002 OK logged in\r\n";
    `Return "* CAPABILITY IMAP4rev1\r\nA00000003 OK done\r\n";
    `Return "A00000004 NO [OVERQUOTA] refused\r\n";
  ];
  let auth=Imap_eio.Auth.password ~username:"alice" ~password:"secret"
    ~allow_insecure_transport:true () in
  let client=match Imap_eio.Client.of_flow ~sw ~auth flow with
    | Ok client -> client
    | Error error -> Alcotest.fail (Imap_eio.Client.error_to_string error) in
  let ctx=context ~store ~spool_dir client in
  let state id=Option.map (fun (op:J.operation) -> op.state)
    (J.find_operation store ~id) in
  (match Imap_sync.Engine.append_blob_journaled ~ctx ~id:"unknown" blob with
   | Error Imap_sync.Error.No_pending_operation -> ()
   | _ -> Alcotest.fail "unprepared APPEND was sent");
  J.prepare_operation store (operation ~kind:J.Append ~id:"refused"
    ~local_id:"refused-local" ~source_uid:None ~blob ~flags:[]);
  (match Imap_sync.Engine.append_blob_journaled ~ctx ~id:"refused" other with
   | Error (Imap_sync.Error.Invalid_operation _) -> ()
   | _ -> Alcotest.fail "APPEND sent another blob");
  Alcotest.(check bool) "blob mismatch left operation prepared" true
    (state "refused"=Some J.Prepared);
  (match Imap_sync.Engine.append_blob_journaled ~ctx ~id:"refused" blob with
   | Error (Imap_sync.Error.Client (Imap_eio.Error.Rejected _)) -> ()
   | Error error -> Alcotest.failf "refused APPEND: %a"
       Imap_sync.Error.pp error
   | Ok _ -> Alcotest.fail "refused APPEND succeeded");
  Alcotest.(check bool) "refusal rejected the operation" true
    (match J.find_operation store ~id:"refused" with
     | Some {state=J.Rejected;receipt=Some "APPEND rejected";_} -> true
     | _ -> false);
  (match Imap_sync.Engine.append_blob_journaled ~ctx ~id:"refused" blob with
   | Error (Imap_sync.Error.Invalid_operation _) -> ()
   | _ -> Alcotest.fail "settled APPEND was sent again")

let test_prepared_append_restarts () =
  with_fixture @@ fun ~database ~blob_dir ~spool_dir ~maildir ->
  let local=Md.append maildir
    ~source:(Eio.Flow.string_source message) ~length ~flags:[] () in
  Eio.Switch.run (fun sw ->
    let store=open_store ~sw ~database ~blob_dir in
    let blob=Imap_store.Blob.put store
      ~source:(Eio.Flow.string_source message) ~length () in
    let op=operation ~kind:J.Append ~id:"prepared-before-send"
      ~local_id:local.id ~source_uid:None ~blob ~flags:[] in
    J.prepare_operation ~local_source_mtime:local.mtime store op);
  Eio.Switch.run (fun sw ->
    let store=open_store ~sw ~database ~blob_dir in
    let client=scripted_scan ~sw ~has_message:false
      ~caps:"IMAP4rev1 UNSELECT"
      ~append_without_uidplus:true () in
    let next_id ()="fresh-after-unsent" in
    (match Imap_sync.Bridge.copy_once
        ~ctx:(context ~store ~spool_dir ~next_id client) ~maildir
        ~stage_id:"prepared-append-rescan" () with
     | Error (Imap_sync.Error.Pending_operations ["fresh-after-unsent"]) -> ()
     | Error error -> Alcotest.failf "unsent APPEND restart: %a"
         Imap_sync.Error.pp error
     | Ok _ -> Alcotest.fail "new unidentified APPEND was not held");
    Alcotest.(check bool) "old unsent operation rejected" true
      (match J.find_operation store ~id:"prepared-before-send" with
       | Some op -> op.state=J.Rejected
       | None -> false);
    Alcotest.(check bool) "one fresh APPEND was journaled" true
      (match J.find_operation store ~id:"fresh-after-unsent" with
       | Some op -> op.state=J.Ambiguous
       | None -> false))

(* An APPEND marked sent may have reached the server, so a restart holds
   it instead of rejecting it as undispatched. *)
let test_sent_append_survives_restart () =
  with_fixture @@ fun ~database ~blob_dir ~spool_dir ~maildir ->
  let local=Md.append maildir
    ~source:(Eio.Flow.string_source message) ~length ~flags:[] () in
  Eio.Switch.run (fun sw ->
    let store=open_store ~sw ~database ~blob_dir in
    let blob=Imap_store.Blob.put store
      ~source:(Eio.Flow.string_source message) ~length () in
    let op=operation ~kind:J.Append ~id:"sent-append"
      ~local_id:local.id ~source_uid:None ~blob ~flags:[] in
    J.prepare_operation ~local_source_mtime:local.mtime store op;
    J.mark_sent store ~id:op.id);
  Eio.Switch.run (fun sw ->
    let store=open_store ~sw ~database ~blob_dir in
    let client=scripted_scan ~sw ~has_message:false () in
    (match run_bridge ~client ~store ~maildir ~spool_dir with
     | Error (Imap_sync.Error.Pending_operations ["sent-append"]) -> ()
     | Error e -> Alcotest.failf "wrong error: %a" Imap_sync.Error.pp e
     | Ok _ -> Alcotest.fail "sent APPEND was replayed");
    Alcotest.(check bool) "operation remains sent" true
      (match J.find_operation store ~id:"sent-append" with
       | Some {state=J.Sent;_} -> true | _ -> false))

let test_ambiguous_append_survives_restart () =
  with_fixture @@ fun ~database ~blob_dir ~spool_dir ~maildir ->
  let local=Md.append maildir
    ~source:(Eio.Flow.string_source message) ~length ~flags:[] () in
  Eio.Switch.run (fun sw ->
    let store=open_store ~sw ~database ~blob_dir in
    let blob=Imap_store.Blob.put store
      ~source:(Eio.Flow.string_source message) ~length () in
    let op=operation ~kind:J.Append ~id:"ambiguous-append"
      ~local_id:local.id ~source_uid:None ~blob ~flags:[] in
    J.prepare_operation store op;
    J.mark_sent store ~id:op.id;
    J.mark_ambiguous store ~id:op.id);
  Eio.Switch.run (fun sw ->
    let store=open_store ~sw ~database ~blob_dir in
    Alcotest.(check int) "pending survived restart" 1
      (List.length (all_active_operations store ~scope));
    let client=scripted_scan ~sw ~has_message:false () in
    (match run_bridge ~client ~store ~maildir ~spool_dir with
     | Error (Imap_sync.Error.Pending_operations ["ambiguous-append"]) -> ()
     | Error e -> Alcotest.failf "wrong error: %a" Imap_sync.Error.pp e
     | Ok _ -> Alcotest.fail "ambiguous APPEND was replayed");
    Alcotest.(check int) "no pair published" 0
      (List.length (all_pairs store ~scope));
    Alcotest.(check bool) "operation remains ambiguous" true
      (match J.find_operation store ~id:"ambiguous-append" with
       | Some {state=J.Ambiguous;_} -> true | _ -> false))

let test_local_write_recovery ?(missing_date=false) ?(wrong_date=false)
    ~divergent () =
  with_fixture @@ fun ~database ~blob_dir ~spool_dir ~maildir ->
  let local_id=Md.reserve_id () in
  let actual=if divergent then String.map
    (fun c -> if c='S' then 'X' else c) message else message in
  Alcotest.(check int) "same-length mutation" (String.length message)
    (String.length actual);
  let date=match Imap.Internal_date.of_string
    "26-Sep-2025 12:34:56 +0000" with
    | Ok date -> date | Error error -> Alcotest.fail error in
  let altered_date=match Imap.Internal_date.of_string
    "27-Sep-2025 12:34:56 +0000" with
    | Ok date -> date | Error error -> Alcotest.fail error in
  Eio.Switch.run (fun sw ->
    let store=open_store ~sw ~database ~blob_dir in
    let blob=Imap_store.Blob.put store
      ~source:(Eio.Flow.string_source message) ~length () in
    let op=operation ~kind:J.Local_append ~id:"reserved-local"
      ~local_id ~source_uid:(Some (uid 1L)) ~blob ~flags:[] in
    J.prepare_operation store {op with internal_date=Some date};
    J.mark_sent store ~id:op.id;
    ignore (Md.append maildir ~id:local_id
      ~source:(Eio.Flow.string_source actual) ~length ~flags:[]
      ?mtime:(if missing_date then None else
        Some (mtime (if wrong_date then altered_date else date))) ())); 
  Eio.Switch.run (fun sw ->
    let store=open_store ~sw ~database ~blob_dir in
    let client=scripted_scan ~sw ~has_message:true ~recovery_date:date () in
    (match run_bridge ~client ~store ~maildir ~spool_dir with
     | Error (Imap_sync.Error.Content_diverged "reserved-local")
         when divergent -> ()

     | Error (Imap_sync.Error.Date_diverged "reserved-local")
         when missing_date || wrong_date -> ()
     | Error e -> Alcotest.failf "wrong recovery error: %a"
         Imap_sync.Error.pp e
     | Ok receipt when not divergent && not missing_date &&
         not wrong_date ->
         Alcotest.(check int) "no duplicate local copy" 0
           receipt.remote_to_local
     | Ok _ -> Alcotest.fail "divergent bytes committed");
    Alcotest.(check int) "one physical local occurrence" 1
      (List.length (Md.scan maildir));
    Alcotest.(check bool) "pair outcome"
      (not divergent && not missing_date && not wrong_date)
      (Option.is_some (J.find_local store ~scope ~local_id));
    Alcotest.(check bool) "journal outcome"
      (not divergent && not missing_date && not wrong_date)
      (match J.find_operation store ~id:"reserved-local" with
       | Some {state=J.Committed;_} -> true | _ -> false))

let test_confirmed_uidplus_recovery ?(changed_mtime=false)
    ?(missing_preimage=false) ?(changed_remote_body=false)
    ?(missing_target=false) () =
  with_fixture @@ fun ~database ~blob_dir ~spool_dir ~maildir ->
  let flags=[] and wire_flags="" in
  let local=Md.append maildir
    ~source:(Eio.Flow.string_source message) ~length ~flags () in
  let local_path=Filename.concat
    (Filename.concat (Filename.dirname (Eio.Path.native_exn database))
      (if flags=[] then "maildir/new" else "maildir/cur")) local.filename in
  Unix.utimes local_path 1709164800. 1709164800.;
  let local=Option.get (Md.find maildir ~id:local.id) in
  Eio.Switch.run (fun sw ->
    let store=open_store ~sw ~database ~blob_dir in
    let blob=Imap_store.Blob.put store
      ~source:(Eio.Flow.string_source message) ~length () in
    let op=operation ~kind:J.Append ~id:"confirmed-append"
      ~local_id:local.id ~source_uid:None ~blob ~flags in
    if missing_preimage then J.prepare_operation store op
    else J.prepare_operation ~local_source_mtime:local.mtime store op;
    J.mark_sent store ~id:op.id;
    J.observe_operation store ~id:op.id ~receipt:"APPENDUID"
      ~destination_uidvalidity:(Some (epoch 11L))
      ~destination_uid:(Some (uid 1L)));
  if changed_mtime then Unix.utimes local_path 1709164801. 1709164801.;
  Eio.Switch.run (fun sw ->
    let store=open_store ~sw ~database ~blob_dir in
    let fetched_body=if changed_remote_body then
      String.sub message 0 (String.length message-1) ^ "!"
      else message in
    let client=scripted_scan ~sw ~has_message:true ~confirmed_body:true
      ?confirmed_date:(local_date local)
      ~fetched_body ~flags:wire_flags ~missing_metadata:missing_target () in
    (match run_bridge ~client ~store ~maildir ~spool_dir with
     | Error (Imap_sync.Error.Local_source_changed id)
       when changed_mtime && id=local.id -> ()
     | Error (Imap_sync.Error.Invalid_operation
         "APPENDUID target UID 1 is missing") when missing_target -> ()
     | Error (Imap_sync.Error.Pending_operations [id])
       when missing_preimage && id="confirmed-append" -> ()
     | Error (Imap_sync.Error.Content_diverged "confirmed-append")
       when changed_remote_body -> ()
     | Ok receipt when not changed_mtime && not missing_preimage &&
       not changed_remote_body && not missing_target ->
         Alcotest.(check int) "remote APPEND not replayed" 0
           receipt.local_to_remote;
         Alcotest.(check int) "remote body not copied locally again" 0
           receipt.remote_to_local
     | Error e -> Alcotest.failf "UIDPLUS recovery: %a"
         Imap_sync.Error.pp e
     | Ok _ -> Alcotest.fail "changed mtime was paired");
    Alcotest.(check bool) "mismatched remote body not cached" true
      (Imap_store.Blob.find store ~scope ~uidvalidity:(epoch 11L)
        ~uid:(uid 1L)=None);
    Alcotest.(check bool) "pair commit follows source evidence"
      (not changed_mtime && not missing_preimage &&
       not changed_remote_body && not missing_target)
      (Option.is_some (J.find_remote store ~scope
        ~uidvalidity:(epoch 11L) ~uid:(uid 1L)));
    Alcotest.(check bool) "operation commit follows source evidence"
      (not changed_mtime && not missing_preimage &&
       not changed_remote_body && not missing_target)
      (match J.find_operation store ~id:"confirmed-append" with
       | Some {state=J.Committed;_} -> true | _ -> false));
  if changed_mtime then (
    Unix.utimes local_path 1709164800. 1709164800.;
    Eio.Switch.run (fun sw ->
      let store=open_store ~sw ~database ~blob_dir in
      let client=scripted_scan ~sw ~has_message:true
        ~confirmed_body:true ?confirmed_date:(local_date local) () in
      (match run_bridge ~client ~store ~maildir ~spool_dir with
       | Ok receipt ->
           Alcotest.(check int) "restored source did not replay APPEND" 0
             receipt.local_to_remote
       | Error e -> Alcotest.failf "restored UIDPLUS recovery: %a"
           Imap_sync.Error.pp e);
      Alcotest.(check bool) "restored source paired" true
        (Option.is_some (J.find_remote store ~scope
          ~uidvalidity:(epoch 11L) ~uid:(uid 1L)))))

let test_writer_lease_blocks_bridge () =
  with_fixture @@ fun ~database ~blob_dir ~spool_dir ~maildir ->
  Eio.Switch.run (fun sw ->
    let store=open_store ~sw ~database ~blob_dir in
    let client=scripted_scan ~sw ~has_message:false () in
    Md.with_writer maildir (fun _ ->
      match run_bridge ~client ~store ~maildir ~spool_dir with
      | Error Imap_sync.Error.Writer_busy -> ()
      | Error error -> Alcotest.failf "wrong lease error: %a"
          Imap_sync.Error.pp error
      | Ok _ -> Alcotest.fail "bridge ignored competing writer lease"))

let test_maildir_error_is_typed () =
  with_fixture @@ fun ~database ~blob_dir ~spool_dir ~maildir ->
  Eio.Switch.run (fun sw ->
    let store=open_store ~sw ~database ~blob_dir in
    let dir=Filename.dirname (Eio.Path.native_exn database) in
    Out_channel.with_open_bin (Filename.concat dir "maildir/cur/a:b")
      (fun out -> output_string out "x");
    let client=scripted_scan ~sw ~has_message:false () in
    (match run_bridge ~client ~store ~maildir ~spool_dir with
     | Error (Imap_sync.Error.Maildir (Md.Malformed_filename "a:b")) -> ()
     | Error error -> Alcotest.failf "wrong malformed-name result: %a"
         Imap_sync.Error.pp error
     | Ok _ -> Alcotest.fail "bridge staged a malformed Maildir name");
    Alcotest.(check (list string)) "failed staging left no spool file" []
      (Sys.readdir (Filename.concat dir "spool") |> Array.to_list))

let test_recover_local () =
  with_fixture @@ fun ~database ~blob_dir:_ ~spool_dir ~maildir ->
  let dir=Filename.dirname (Eio.Path.native_exn database) in
  let temporary=".tmp-0123456789abcdef0123456789abcdef" in
  let staging="local-inventory-im-0123456789abcdef0123456789abcdef.sqlite3" in
  List.iter (fun path ->
    Out_channel.with_open_bin (Filename.concat dir path) (fun out ->
      output_string out "interrupted"))
    ["maildir/tmp/" ^ temporary;"spool/" ^ staging;"spool/imap-keep"];
  Md.with_writer maildir (fun _ ->
    match Imap_sync.Bridge.recover_local ~maildir ~spool_dir () with
    | Error Imap_sync.Error.Writer_busy -> ()
    | _ -> Alcotest.fail "recovery ran without the writer lease");
  (match Imap_sync.Bridge.recover_local ~maildir ~spool_dir () with
   | Ok () -> ()
   | Error error -> Alcotest.failf "recovery: %a"
       Imap_sync.Error.pp error);
  Alcotest.(check (list string)) "Maildir temporary removed" []
    (Sys.readdir (Filename.concat dir "maildir/tmp") |> Array.to_list);
  Alcotest.(check (list string)) "only the staging file removed"
    ["imap-keep"]
    (Sys.readdir (Filename.concat dir "spool") |> Array.to_list)

let test_metadata_lock_is_not_writer_lease () =
  let dir=root () in
  Fun.protect ~finally:(fun () -> remove_tree dir) @@ fun () ->
  Eio_main.run @@ fun env ->
  Eio.Switch.run @@ fun sw ->
  let fs=Eio.Stdenv.fs env in
  let store=open_store ~sw ~database:Eio.Path.(fs / dir / "sync.db")
    ~blob_dir:Eio.Path.(fs / dir / "blob") in
  let root=Eio.Path.(fs / dir / "maildir") in
  let maildir=Md.open_dir root in
  Eio.Path.save ~create:(`Exclusive 0o600)
    Eio.Path.(root / "dovecot-uidlist.lock") "1 other.example\n";
  let client=scripted_scan ~sw ~has_message:false () in
  match run_bridge ~client ~store ~maildir
      ~spool_dir:Eio.Path.(fs / dir / "spool") with
  | exception Md.Metadata_lock_busy _ -> ()
  | Error Imap_sync.Error.Writer_busy ->
      Alcotest.fail "metadata lock reported as a busy writer lease"
  | Error error -> Alcotest.failf "wrong metadata lock result: %a"
      Imap_sync.Error.pp error
  | Ok _ -> Alcotest.fail "bridge ignored the metadata lock"

let test_prepared_copies_rejected_without_send () =
  with_fixture @@ fun ~database ~blob_dir ~spool_dir ~maildir ->
  Eio.Switch.run (fun sw ->
    let store=open_store ~sw ~database ~blob_dir in
    let blob=Imap_store.Blob.put store
      ~source:(Eio.Flow.string_source message) ~length () in
    let local=operation ~kind:J.Local_append ~id:"prepared-local"
      ~local_id:(Md.reserve_id ()) ~source_uid:(Some (uid 1L))
      ~blob ~flags:[] in
    let remote=operation ~kind:J.Append ~id:"prepared-remote"
      ~local_id:(Md.reserve_id ()) ~source_uid:None
      ~blob ~flags:[] in
    J.prepare_operation store local;
    J.prepare_operation store remote);
  Eio.Switch.run (fun sw ->
    let store=open_store ~sw ~database ~blob_dir in
    let client=scripted_scan ~sw ~has_message:false () in
    (match run_bridge ~client ~store ~maildir ~spool_dir with
     | Ok receipt ->
         Alcotest.(check int) "no copy retried" 0
           (receipt.remote_to_local+receipt.local_to_remote)
     | Error e -> Alcotest.failf "prepared repair: %a"
         Imap_sync.Error.pp e);
    List.iter (fun id ->
      Alcotest.(check bool) (id ^ " rejected") true
        (match J.find_operation store ~id with
         | Some {state=J.Rejected;_} -> true | _ -> false))
      ["prepared-local";"prepared-remote"])

let test_operator_appenduid_evidence () =
  with_fixture @@ fun ~database ~blob_dir ~spool_dir ~maildir ->
  let local=Md.append maildir
    ~source:(Eio.Flow.string_source message) ~length ~flags:[] () in
  Eio.Switch.run (fun sw ->
    let store=open_store ~sw ~database ~blob_dir in
    let blob=Imap_store.Blob.put store
      ~source:(Eio.Flow.string_source message) ~length () in
    let op=operation ~kind:J.Append ~id:"operator-appenduid"
      ~local_id:local.id ~source_uid:None ~blob ~flags:[] in
    J.prepare_operation ~local_source_mtime:local.mtime store op;
    J.mark_sent store ~id:op.id;
    J.mark_ambiguous store ~id:op.id);
  Eio.Switch.run (fun sw ->
    let store=open_store ~sw ~database ~blob_dir in
    (match Imap_sync.Repair.record_appenduid ~store ~maildir ~scope
      ~id:"operator-appenduid" ~uidvalidity:(epoch 12L) ~uid:(uid 1L)
      ~evidence:"saved server receipt" () with
     | Error (Imap_sync.Error.Invalid_operation _) -> ()
     | _ -> Alcotest.fail "wrong epoch was accepted");
    Alcotest.(check bool) "wrong epoch left operation ambiguous" true
      (match J.find_operation store ~id:"operator-appenduid" with
       | Some {state=J.Ambiguous;_} -> true | _ -> false);
    (match Imap_sync.Repair.record_appenduid ~store ~maildir ~scope
      ~id:"operator-appenduid" ~uidvalidity:(epoch 11L) ~uid:(uid 1L)
      ~evidence:"saved server receipt" () with
     | Ok () -> ()
     | Error e -> Alcotest.failf "record APPENDUID: %a"
         Imap_sync.Error.pp e);
    let divergent=String.map (fun c -> if c='S' then 'X' else c)
      message in
    let wrong_client=scripted_scan ~sw ~has_message:true
      ~confirmed_body:true ?confirmed_date:(local_date local)
      ~fetched_body:divergent () in
    (match run_bridge ~client:wrong_client ~store ~maildir ~spool_dir with
     | Error (Imap_sync.Error.Content_diverged "operator-appenduid") -> ()
     | Error e -> Alcotest.failf "wrong divergent result: %a"
         Imap_sync.Error.pp e
     | Ok _ -> Alcotest.fail "unverified operator UID was committed");
    Alcotest.(check bool) "divergent remote body leaves operation observed"
      true (match J.find_operation store ~id:"operator-appenduid" with
        | Some {state=J.Observed;_} -> true | _ -> false);
    let client=scripted_scan ~sw ~has_message:true
      ~confirmed_body:true ?confirmed_date:(local_date local) () in
    (match run_bridge ~client ~store ~maildir ~spool_dir with
     | Ok receipt ->
         Alcotest.(check int) "no duplicate upload" 0
           receipt.local_to_remote
     | Error e -> Alcotest.failf "repair APPENDUID: %a"
         Imap_sync.Error.pp e);
    Alcotest.(check bool) "verified pair committed" true
      (Option.is_some (J.find_remote store ~scope
        ~uidvalidity:(epoch 11L) ~uid:(uid 1L)));
    Alcotest.(check bool) "operation committed" true
      (match J.find_operation store ~id:"operator-appenduid" with
       | Some {state=J.Committed;_} -> true | _ -> false))

let test_unresolved_delete_reports_pending () =
  with_fixture @@ fun ~database ~blob_dir ~spool_dir ~maildir ->
  Eio.Switch.run @@ fun sw ->
  let store=open_store ~sw ~database ~blob_dir in
  let blob=Imap_store.Blob.put store
    ~source:(Eio.Flow.string_source message) ~length () in
  let local_id=Md.reserve_id () in
  let pair : J.pair = {
    id="delete-pair";scope;remote_uidvalidity=Some (epoch 11L);
    remote_uid=Some (uid 1L);local_id=Some local_id;
    content_sha256=Some blob.sha256;content_length=Some length;
    internal_date=None;
    common_flags=[];remote_tombstone=None;
    local_tombstone=Some {J.reason=J.Local_absence;
      evidence="complete-local-inventory";generation=None};revision=0L} in
  (match J.put_pair store ~expected_revision:None pair with
   | `Committed _ -> ()
   | `Stale_revision -> Alcotest.fail "new pair was stale");
  let pair=Option.get (J.find_pair store ~id:pair.id) in
  let op : J.operation = {
    id="uncertain-delete";pair_id=Some pair.id;local_id=Some local_id;
    scope;kind=J.Delete;state=J.Prepared;
    source_uidvalidity=Some (epoch 11L);source_uid=Some (uid 1L);
    destination=None;destination_uidvalidity=None;
    blob_sha256=Some blob.sha256;blob_length=Some length;
    desired_flags=Some [];
    internal_date=None;append=None;
    receipt=None;receipt_uidvalidity=None;
    receipt_uid=None} in
  J.prepare_operation store op;
  J.mark_sent store ~id:op.id;
  J.mark_ambiguous store ~id:op.id;
  Alcotest.(check bool) "operator EXPUNGE attestation persisted" true
    (J.attest_targeted_expunge store ~id:op.id pair
      ~evidence:"operator verified exact deleted target"=`Attested);
  let client=scripted_scan ~sw ~has_message:true () in
  (match run_bridge ~client ~store ~maildir ~spool_dir with
   | Error (Imap_sync.Error.Pending_operations ["uncertain-delete"]) -> ()
   | Error error -> Alcotest.failf "wrong pending result: %a"
       Imap_sync.Error.pp error
   | Ok _ -> Alcotest.fail "uncertain delete was ignored");
  Alcotest.(check bool) "uncertain delete remains active" true
    (match J.find_operation store ~id:op.id with
     | Some {state=J.Ambiguous;receipt=Some receipt;_} ->
         String.equal receipt
           "operator authorized targeted UID EXPUNGE: operator verified exact deleted target"
     | _ -> false);
  let vanished=scripted_scan ~sw ~has_message:false () in
  let next_id ()="unexpected-transfer" in
  (match Imap_sync.Bridge.copy_once
    ~ctx:(context ~store ~spool_dir ~next_id vanished) ~maildir
    ~stage_id:"fault-scan-absent" () with
   | Ok _ -> ()
   | Error error -> Alcotest.failf "attested delete recovery: %a"
       Imap_sync.Error.pp error);
  Alcotest.(check bool) "attested delete recovered without replay" true
    (match J.find_operation store ~id:op.id with
     | Some {state=J.Committed;receipt=Some receipt;_} ->
         String.starts_with
           ~prefix:"operator authorized targeted UID EXPUNGE: operator verified exact deleted target; complete inventory proves deletion target absent"
           receipt
     | _ -> false)

let test_unsupported_targeted_delete_is_durable_hold () =
  with_fixture @@ fun ~database ~blob_dir ~spool_dir ~maildir ->
  Eio.Switch.run @@ fun sw ->
  let store=open_store ~sw ~database ~blob_dir in
  let blob=Imap_store.Blob.put store
    ~source:(Eio.Flow.string_source message) ~length () in
  let pair : J.pair = {
    id="unsupported-delete-pair";scope;
    remote_uidvalidity=Some (epoch 11L);remote_uid=Some (uid 1L);
    local_id=Some "vanished-local";content_sha256=Some blob.sha256;
    content_length=Some length;internal_date=None;
    common_flags=[];remote_tombstone=None;
    local_tombstone=Some {J.reason=J.Local_absence;
      evidence="complete-local-inventory";generation=None};revision=0L} in
  (match J.put_pair store ~expected_revision:None pair with
   | `Committed _ -> ()
   | `Stale_revision -> Alcotest.fail "new pair was stale");
  let copy stage_id=
    let client=scripted_scan ~sw ~has_message:true
      ~caps:"IMAP4rev1 UNSELECT" () in
    let next_id ()="unsupported-hold-" ^ stage_id in
    match Imap_sync.Bridge.copy_once ~deletion_policy:Imap.Sync_policy.Propagate
      ~ctx:(context ~store ~spool_dir ~next_id client) ~maildir ~stage_id
      () with
    | Ok receipt -> receipt
    | Error error -> Alcotest.failf "unsupported delete: %a"
        Imap_sync.Error.pp error in
  let first=copy "unsupported-first" in
  Alcotest.(check int) "missing UIDPLUS reports a hold" 1
    first.deletions_held;
  let conflicts=all_open_conflicts store ~scope in
  let conflict=match conflicts with
    | [conflict] when conflict.kind=J.Deletion_hold -> conflict
    | _ -> Alcotest.fail "unsupported delete has no durable hold" in
  let second=copy "unsupported-second" in
  Alcotest.(check int) "repeated unsupported delete still held" 1
    second.deletions_held;
  Alcotest.(check (list string)) "unsupported hold ID stable"
    [conflict.id]
    (List.map (fun (x:J.conflict) -> x.id)
      (all_open_conflicts store ~scope));
  Alcotest.(check int) "unsupported delete sent no operation" 0
    (List.length (all_active_operations store ~scope));
  let preview=ref [] in
  (match preview_deletions ~spool_dir ~store ~maildir ~scope
    ~policy:Imap.Sync_policy.Propagate_local
    ~on_preview:(fun item -> preview:=item::!preview) () with
   | Ok _ -> ()
   | Error error -> Alcotest.failf "candidate preview: %a"
       Imap_sync.Error.pp error);
  Alcotest.(check bool) "preview shows remote-delete candidate" true
    (match !preview with
     | [{decision=`Plan Imap.Sync_policy.Delete_remote;_}] -> true
     | _ -> false)

let test_deletion_grace_across_complete_scans () =
  with_fixture @@ fun ~database ~blob_dir ~spool_dir ~maildir ->
  Eio.Switch.run @@ fun sw ->
  let store=open_store ~sw ~database ~blob_dir in
  let blob=Imap_store.Blob.put store
    ~source:(Eio.Flow.string_source message) ~length () in
  let local_id=Md.reserve_id () in
  let pair:J.pair={
    id="grace-local-absence";scope;
    remote_uidvalidity=Some (epoch 11L);remote_uid=Some (uid 1L);
    local_id=Some local_id;content_sha256=Some blob.sha256;
    content_length=Some length;internal_date=None;common_flags=[];
    remote_tombstone=None;local_tombstone=None;revision=0L} in
  (match J.put_pair store ~expected_revision:None pair with
   | `Committed _ -> () | `Stale_revision -> Alcotest.fail "new pair stale");
  let copy suffix=
    let client=scripted_scan ~sw ~has_message:true () in
    let next_id ()="grace-op-"^suffix in
    match Imap_sync.Bridge.copy_once ~min_absence_scans:1
      ~deletion_policy:Imap.Sync_policy.Propagate
      ~ctx:(context ~store ~spool_dir ~next_id client) ~maildir
      ~stage_id:("grace-"^suffix) () with
    | Ok receipt -> receipt
    | Error error -> Alcotest.failf "grace scan: %a"
        Imap_sync.Error.pp error in
  let first=copy "first" in
  Alcotest.(check int) "first absence held" 1 first.deletions_held;
  Alcotest.(check int) "first absence not deleted" 0 first.deletions;
  let first_pair=Option.get (J.find_pair store ~id:pair.id) in
  let observed=Option.bind first_pair.local_tombstone
    (fun tombstone -> tombstone.generation) in
  Alcotest.(check (option int64)) "first observation recorded"
    (Some first.cursor.generation) observed;
  let preview=ref [] in
  (match preview_deletions ~spool_dir ~min_absence_scans:1
    ~store ~maildir
    ~scope ~policy:Imap.Sync_policy.Propagate
    ~on_preview:(fun item -> preview:=item::!preview) () with
   | Ok _ -> ()
   | Error error -> Alcotest.failf "grace preview: %a"
       Imap_sync.Error.pp error);
  Alcotest.(check bool) "preview holds during grace" true
    (match !preview with
     | [{decision=`Plan (Imap.Sync_policy.Hold_deletion
          Imap.Sync_policy.Grace_period);_}] -> true
     | _ -> false);
  let second=copy "second" in
  Alcotest.(check bool) "complete scan advanced generation" true
    (second.cursor.generation>first.cursor.generation);
  let second_pair=Option.get (J.find_pair store ~id:pair.id) in
  Alcotest.(check (option int64)) "first observation preserved"
    observed
    (Option.bind second_pair.local_tombstone
      (fun tombstone -> tombstone.generation));
  let preview=ref [] in
  (match preview_deletions ~spool_dir ~min_absence_scans:1
    ~store ~maildir
    ~scope ~policy:Imap.Sync_policy.Propagate
    ~on_preview:(fun item -> preview:=item::!preview) () with
   | Ok _ -> ()
   | Error error -> Alcotest.failf "mature preview: %a"
       Imap_sync.Error.pp error);
  Alcotest.(check bool) "later preview offers remote DELETE" true
    (match !preview with
     | [{decision=`Plan Imap.Sync_policy.Delete_remote;_}] -> true
     | _ -> false);
  let restored=Md.append maildir
    ~id:local_id ~source:(Eio.Flow.string_source message)
    ~length ~flags:[] () in
  let present=copy "restored" in
  Alcotest.(check int) "restored local file prevents deletion" 0
    present.deletions;
  Alcotest.(check (option int64)) "complete presence persisted"
    (Some present.cursor.generation)
    (J.last_presence_generation store ~pair_id:pair.id ~side:`Local);
  Eio.Switch.run @@ fun read_sw ->
  let reopened=Imap_store.open_readonly ~sw:read_sw database in
  Alcotest.(check (option int64)) "presence survives read-only reopen"
    (Some present.cursor.generation)
    (J.last_presence_generation reopened ~pair_id:pair.id ~side:`Local);
  Md.remove maildir restored;
  let preview=ref [] in
  (match preview_deletions ~spool_dir ~min_absence_scans:1
    ~store ~maildir
    ~scope ~policy:Imap.Sync_policy.Propagate
    ~on_preview:(fun item -> preview:=item::!preview) () with
   | Ok _ -> ()
   | Error error -> Alcotest.failf "reappeared preview: %a"
       Imap_sync.Error.pp error);
  Alcotest.(check bool) "old absence cannot mature after presence" true
    (match !preview with
     | [{decision=`Plan (Imap.Sync_policy.Hold_deletion
          Imap.Sync_policy.Grace_period);_}] -> true
     | _ -> false);
  let again=copy "absent-again" in
  Alcotest.(check int) "new absence held" 1 again.deletions_held;
  let updated=Option.get (J.find_pair store ~id:pair.id) in
  Alcotest.(check (option int64)) "absence clock restarted"
    (Some again.cursor.generation)
    (Option.bind updated.local_tombstone (fun x -> x.generation));
  let later=copy "absent-later" in
  Alcotest.(check bool) "new absence eventually matures" true
    (later.cursor.generation>again.cursor.generation)

let test_changed_reappearance_blocks_delete () =
  with_fixture @@ fun ~database ~blob_dir ~spool_dir ~maildir ->
  Eio.Switch.run @@ fun sw ->
  let store=open_store ~sw ~database ~blob_dir in
  let blob=Imap_store.Blob.put store
    ~source:(Eio.Flow.string_source message) ~length () in
  let local_id=Md.reserve_id () in
  let pair:J.pair={
    id="changed-reappearance";scope;
    remote_uidvalidity=Some (epoch 11L);remote_uid=Some (uid 1L);
    local_id=Some local_id;content_sha256=Some blob.sha256;
    content_length=Some length;internal_date=None;common_flags=[];
    remote_tombstone=None;local_tombstone=None;revision=0L} in
  (match J.put_pair store ~expected_revision:None pair with
   | `Committed _ -> () | `Stale_revision -> Alcotest.fail "new pair stale");
  let copy suffix=
    let client=scripted_scan ~sw ~has_message:true () in
    let next_id ()="changed-op-"^suffix in
    match Imap_sync.Bridge.copy_once ~min_absence_scans:1
      ~deletion_policy:Imap.Sync_policy.Propagate
      ~ctx:(context ~store ~spool_dir ~next_id client) ~maildir
      ~stage_id:("changed-"^suffix) () with
    | Ok receipt -> receipt
    | Error error -> Alcotest.failf "changed reappearance: %a"
        Imap_sync.Error.pp error in
  ignore (copy "first-absence");
  let changed=String.sub message 0 (String.length message-1) ^ "!" in
  let restored=Md.append maildir ~id:local_id
    ~source:(Eio.Flow.string_source changed) ~length ~flags:[] () in
  ignore (copy "changed-present");
  let content_conflicts ()=all_open_conflicts store ~scope
    |> List.filter (fun (x:J.conflict) -> x.kind=J.Content_conflict) in
  let conflict=match content_conflicts () with
    | [conflict] -> conflict
    | _ -> Alcotest.fail "changed reappearance has no content conflict" in
  Md.remove maildir restored;
  (match Imap_sync.Bridge.verify_local_content ~spool_dir ~store ~maildir
    ~scope
    ~next_id:(fun () -> "verify-changed-reappearance")
    ~on_issue:(fun _ _ -> ()) () with
   | Ok result -> Alcotest.(check int64)
       "offline scrub skips a tombstoned local absence" 0L result.missing
   | Error error -> Alcotest.failf "offline scrub: %a"
       Imap_sync.Error.pp error);
  Alcotest.(check (list string)) "offline scrub retains content conflict"
    [conflict.id] (List.map (fun (x:J.conflict) -> x.id)
      (content_conflicts ()));
  ignore (copy "new-absence");
  let mature=copy "mature-absence" in
  Alcotest.(check int) "content conflict blocks mature deletion" 0
    mature.deletions;
  Alcotest.(check (list string)) "conflict persists after disappearance"
    [conflict.id] (List.map (fun (x:J.conflict) -> x.id)
      (content_conflicts ()));
  let preview=ref [] in
  (match preview_deletions ~spool_dir ~min_absence_scans:1
    ~store ~maildir
    ~scope ~policy:Imap.Sync_policy.Propagate
    ~on_preview:(fun item -> preview:=item::!preview) () with
   | Ok _ -> ()
   | Error error -> Alcotest.failf "changed preview: %a"
       Imap_sync.Error.pp error);
  Alcotest.(check bool) "offline plan holds content conflict" true
    (match !preview with
     | [{decision=`Plan (Imap.Sync_policy.Hold_deletion
          Imap.Sync_policy.Survivor_changed);_}] -> true
     | _ -> false);
  let original=Md.append maildir ~id:local_id
    ~source:(Eio.Flow.string_source message) ~length ~flags:[] () in
  ignore (copy "original-restored");
  Alcotest.(check int) "exact bytes clear conflict" 0
    (List.length (content_conflicts ()));
  Md.remove maildir original

let test_wrong_date_reappearance_blocks_delete () =
  with_fixture @@ fun ~database ~blob_dir ~spool_dir ~maildir ->
  Eio.Switch.run @@ fun sw ->
  let store=open_store ~sw ~database ~blob_dir in
  let blob=Imap_store.Blob.put store
    ~source:(Eio.Flow.string_source message) ~length () in
  let local_id=Md.reserve_id () in
  let date=ok (Imap.Internal_date.of_string
    "26-Sep-2025 12:34:56 +0000") in
  let wrong_date=ok (Imap.Internal_date.of_string
    "27-Sep-2025 12:34:56 +0000") in
  let pair:J.pair={
    id="wrong-date-reappearance";scope;
    remote_uidvalidity=Some (epoch 11L);remote_uid=Some (uid 1L);
    local_id=Some local_id;content_sha256=Some blob.sha256;
    content_length=Some length;internal_date=Some date;common_flags=[];
    remote_tombstone=None;local_tombstone=None;revision=0L} in
  (match J.put_pair store ~expected_revision:None pair with
   | `Committed _ -> () | `Stale_revision -> Alcotest.fail "new pair stale");
  let run suffix=
    let client=scripted_scan ~sw ~has_message:true () in
    let next_id ()="date-op-"^suffix in
    Imap_sync.Bridge.copy_once ~min_absence_scans:1
      ~deletion_policy:Imap.Sync_policy.Propagate
      ~ctx:(context ~store ~spool_dir ~next_id client) ~maildir
      ~stage_id:("date-"^suffix) () in
  let copy suffix=match run suffix with
    | Ok receipt -> receipt
    | Error error -> Alcotest.failf "date reappearance: %a"
        Imap_sync.Error.pp error in
  ignore (copy "first-absence");
  let wrong=Md.append maildir ~id:local_id
    ~source:(Eio.Flow.string_source message) ~length ~flags:[]
    ~mtime:(mtime wrong_date) () in
  (match run "wrong-date" with
   | Ok receipt ->
       Alcotest.(check bool) "wrong date is a held pair" true
         (receipt.flags_held=1 && receipt.held_pair_ids=[pair.id])
   | Error error -> Alcotest.failf "wrong date: %a"
       Imap_sync.Error.pp error);
  let identity_conflicts ()=all_open_conflicts store ~scope
    |> List.filter (fun (x:J.conflict) -> x.kind=J.Identity_conflict) in
  Alcotest.(check int) "date conflict is durable" 1
    (List.length (identity_conflicts ()));
  Md.remove maildir wrong;
  let absent=copy "absent-again" in
  Alcotest.(check int) "wrong date blocks deletion after absence" 0
    absent.deletions;
  Alcotest.(check int) "date conflict remains open" 1
    (List.length (identity_conflicts ()));
  let preview=ref [] in
  (match preview_deletions ~spool_dir ~min_absence_scans:1
    ~store ~maildir
    ~scope ~policy:Imap.Sync_policy.Propagate
    ~on_preview:(fun item -> preview:=item::!preview) () with
   | Ok _ -> ()
   | Error error -> Alcotest.failf "date preview: %a"
       Imap_sync.Error.pp error);
  Alcotest.(check bool) "offline plan holds wrong-date identity" true
    (match !preview with
     | [{decision=`Plan (Imap.Sync_policy.Hold_deletion
          Imap.Sync_policy.Survivor_changed);_}] -> true
     | _ -> false);
  let restored=Md.append maildir ~id:local_id
    ~source:(Eio.Flow.string_source message) ~length ~flags:[]
    ~mtime:(mtime date) () in
  ignore (copy "correct-date");
  Alcotest.(check int) "exact date clears identity conflict" 0
    (List.length (identity_conflicts ()));
  Alcotest.(check bool) "exact date reactivates pair" true
    ((Option.get (J.find_pair store ~id:pair.id)).local_tombstone=None);
  Md.remove maildir restored

let test_retention_holds_remote_delete () =
  with_fixture @@ fun ~database ~blob_dir ~spool_dir ~maildir ->
  Eio.Switch.run @@ fun sw ->
  let store=open_store ~sw ~database ~blob_dir in
  let pair:J.pair={
    id="retained-pair";scope;
    remote_uidvalidity=Some (epoch 11L);remote_uid=Some (uid 1L);
    local_id=Some "evicted-local";
    content_sha256=Some (String.make 64 'a');content_length=Some 1L;
    internal_date=None;common_flags=[];remote_tombstone=None;
    local_tombstone=None;revision=0L} in
  (match J.put_pair store ~expected_revision:None pair with
   | `Committed _ -> () | `Stale_revision -> Alcotest.fail "new pair stale");
  (match Imap_sync.Repair.mark_local_retention ~spool_dir ~store ~maildir ~scope
    ~pair_id:pair.id ~evidence:"local size limit" () with
   | Ok () -> ()
   | Error error -> Alcotest.failf "mark retention: %a"
       Imap_sync.Error.pp error);
  let client=scripted_scan ~sw ~has_message:true () in
  let next_id ()="retention-operation" in
  let receipt=match Imap_sync.Bridge.copy_once
    ~deletion_policy:Imap.Sync_policy.Propagate
    ~ctx:(context ~store ~spool_dir ~next_id client) ~maildir
    ~stage_id:"retention-scan" () with
    | Ok receipt -> receipt
    | Error error -> Alcotest.failf "retention bridge: %a"
        Imap_sync.Error.pp error in
  Alcotest.(check int) "no deletion" 0 receipt.deletions;
  Alcotest.(check int) "retention hold" 1 receipt.deletions_held;
  Alcotest.(check bool) "retention conflict explains hold" true
    (match all_open_conflicts store ~scope with
     | [{kind=J.Deletion_hold;evidence;_}] ->
         String.equal evidence
           "local copy was retained or evicted; remote deletion is held"
     | _ -> false);
  Alcotest.(check bool) "retention persisted after scan" true
    (match (Option.get (J.find_pair store ~id:pair.id)).local_tombstone with
     | Some {reason=J.Retention;_} -> true | _ -> false);
  let planned=ref [] in
  let before=(Option.get (J.find_pair store ~id:pair.id)).revision in
  (match preview_deletions ~spool_dir ~store ~maildir ~scope
    ~policy:Imap.Sync_policy.Propagate
    ~on_preview:(fun item -> planned:=item::!planned) () with
   | Ok _ -> ()
   | Error error -> Alcotest.failf "retention preview: %a"
       Imap_sync.Error.pp error);
  Alcotest.(check bool) "preview preserves retention hold" true
    (match !planned with
     | [{decision=`Plan (Imap.Sync_policy.Hold_deletion
         Imap.Sync_policy.Retention_policy);_}] -> true
     | _ -> false);
  Alcotest.(check int64) "preview made no pair mutation" before
    (Option.get (J.find_pair store ~id:pair.id)).revision

let test_readonly_sync_plan () =
  with_fixture @@ fun ~database ~blob_dir ~spool_dir ~maildir ->
  Eio.Switch.run @@ fun sw ->
  let store=open_store ~sw ~database ~blob_dir in
  let seen=Mail_flag.Imap_flag.system Mail_flag.Imap_flag.Seen in
  let local=Md.append maildir
    ~source:(Eio.Flow.string_source message) ~length ~flags:[seen] () in
  let client=scripted_scan ~sw ~has_message:true () in
  (match Imap_sync.Engine.scan_once ~ctx:(context ~store ~spool_dir client)
    ~stage_id:"plan-source" () with
   | Ok _ -> ()
   | Error error -> Alcotest.failf "plan source scan: %a"
       Imap_sync.Error.pp error);
  let preview ?(allow_bootstrap_duplicates=false) () =
    let events=ref [] in
    let cursor=match Imap_sync.Plan.preview_sync ~spool_dir
      ~allow_bootstrap_duplicates ~store ~maildir ~scope
      ~policy:Imap.Sync_policy.Preserve
      ~on_preview:(fun event -> events:=event::!events) () with
      | Ok cursor -> cursor
      | Error error -> Alcotest.failf "sync preview: %a"
          Imap_sync.Error.pp error in
    cursor,List.rev !events in
  let cursor,blocked=preview () in
  Alcotest.(check bool) "populated bootstrap held" true
    (blocked=[Imap_sync.Plan.Preview_bootstrap_hold]);
  let _,allowed=preview ~allow_bootstrap_duplicates:true () in
  Alcotest.(check bool) "opt-in previews two separate copies" true
    (match allowed with
     | [Imap_sync.Plan.Preview_copy_remote remote_uid;
        Imap_sync.Plan.Preview_copy_local id] ->
         remote_uid=uid 1L && id=local.id
     | _ -> false);
  let pair:J.pair={id="plan-flags-pair";scope;
    remote_uidvalidity=Some (epoch 11L);remote_uid=Some (uid 1L);
    local_id=Some local.id;
    content_sha256=Some (Md.sha256 maildir local);
    content_length=Some length;internal_date=None;common_flags=[];
    remote_tombstone=None;local_tombstone=None;revision=0L} in
  let pair=match J.put_pair store ~expected_revision:None pair with
    | `Committed pair -> pair
    | `Stale_revision -> Alcotest.fail "new preview pair stale" in
  let after,planned=preview () in
  Alcotest.(check bool) "three-way flag plan" true
    (match planned with
     | [Imap_sync.Plan.Preview_flags flags] ->
         flags.pair_id=pair.id && flags.to_remote.add=[seen] &&
         flags.to_remote.remove=[] && flags.to_local.add=[] &&
         flags.to_local.remove=[]
     | _ -> false);
  Alcotest.(check int64) "preview retained published revision"
    cursor.revision after.revision;
  Alcotest.(check int64) "preview retained pair revision" pair.revision
    (Option.get (J.find_pair store ~id:pair.id)).revision;
  (match J.ensure_open_conflict store ~pair ~kind:J.Content_conflict
     ~id:"plan-content-hold" ~evidence:"saved body mismatch" with
   | `Open _ -> () | `Stale_revision -> Alcotest.fail "content hold stale");
  let _,held=preview () in
  Alcotest.(check bool) "saved content hold suppresses FLAGS candidate" true
    (match held with
     | [Imap_sync.Plan.Preview_pair_hold (id,reason)] ->
         id=pair.id && String.length reason>0
     | _ -> false);
  Alcotest.(check int64) "held preview kept pair revision" pair.revision
    (Option.get (J.find_pair store ~id:pair.id)).revision

let test_unknown_operation_is_not_pending () =
  with_fixture @@ fun ~database ~blob_dir ~spool_dir ~maildir ->
  Eio.Switch.run @@ fun sw ->
  let store=open_store ~sw ~database ~blob_dir in
  let client,_=scripted_client ~caps:"IMAP4rev1 UNSELECT UIDPLUS CONDSTORE"
    ~sw "unknown-operation" [] in
  let ctx=context ~store ~spool_dir client in
  let id="unknown-op" and evidence="operator audit" in
  let expect what = function
    | Error Imap_sync.Error.No_pending_operation -> ()
    | Error error -> Alcotest.failf "%s: wrong unknown ID error: %a" what
        Imap_sync.Error.pp error
    | Ok _ -> Alcotest.failf "%s accepted an unknown ID" what in
  expect "local_delete"
    (Imap_sync.Repair.local_delete ~ctx ~maildir ~id ~evidence ());
  expect "reject_remote_delete"
    (Imap_sync.Repair.reject_remote_delete ~ctx ~maildir ~id ~evidence ());
  expect "finish_remote_delete"
    (Imap_sync.Repair.finish_remote_delete ~ctx ~maildir ~id ~evidence ());
  expect "local_append"
    (Imap_sync.Repair.local_append ~ctx ~maildir ~id ~evidence ());
  expect "record_appenduid"
    (Imap_sync.Repair.record_appenduid ~store ~scope ~maildir ~id
      ~uidvalidity:(epoch 11L) ~uid:(uid 1L) ~evidence ());
  expect "inspect_append_candidates"
    (Imap_sync.Repair.inspect_append_candidates ~ctx ~id ())

let test_remote_repairs_missing_spool_is_configuration () =
  with_fixture @@ fun ~database ~blob_dir ~spool_dir ~maildir ->
  Eio.Switch.run @@ fun sw ->
  let store=open_store ~sw ~database ~blob_dir in
  let client,_=scripted_client ~caps:"IMAP4rev1 UNSELECT UIDPLUS CONDSTORE"
    ~sw "repair-missing-spool" [] in
  let ctx=context ~store ~spool_dir:Eio.Path.(spool_dir / "missing") client in
  let id="unknown-op" and evidence="operator audit" in
  let expect what = function
    | Error (Imap_sync.Error.Invalid_configuration _) -> ()
    | Error error -> Alcotest.failf "%s: wrong missing spool error: %a" what
        Imap_sync.Error.pp error
    | Ok _ -> Alcotest.failf "%s ran without a spool directory" what in
  expect "reject_remote_delete"
    (Imap_sync.Repair.reject_remote_delete ~ctx ~maildir ~id ~evidence ());
  expect "finish_remote_delete"
    (Imap_sync.Repair.finish_remote_delete ~ctx ~maildir ~id ~evidence ())

let test_engine_budgets_are_configuration () =
  with_fixture @@ fun ~database ~blob_dir ~spool_dir ~maildir:_ ->
  Eio.Switch.run @@ fun sw ->
  let store=open_store ~sw ~database ~blob_dir in
  let client,_=scripted_client ~sw "engine-budgets" [] in
  let ctx=context ~store ~spool_dir client in
  let expect what = function
    | Error (Imap_sync.Error.Invalid_configuration _) -> ()
    | Error error -> Alcotest.failf "%s: wrong budget error: %a" what
        Imap_sync.Error.pp error
    | Ok _ -> Alcotest.failf "%s accepted an invalid budget" what in
  expect "scan window budget"
    (Imap_sync.Engine.scan_once ~max_windows:0 ~ctx ~stage_id:"budget" ());
  expect "hydration count"
    (Imap_sync.Engine.hydrate_once ~max_messages:0 ~ctx ());
  expect "hydration body budget"
    (Imap_sync.Engine.hydrate_once ~max_body_bytes:0L ~ctx ());
  expect "hydration spool directory"
    (Imap_sync.Engine.hydrate_once
      ~ctx:(context ~store ~spool_dir:Eio.Path.(spool_dir / "missing")
        client) ());
  expect "audit count"
    (Imap_sync.Engine.audit_cache_once ~max_messages:0 ~store ~scope ());
  expect "audit byte budget"
    (Imap_sync.Engine.audit_cache_once ~max_total_bytes:0L ~store ~scope ())

let test_incompatible_absence_tombstone_holds () =
  with_fixture @@ fun ~database ~blob_dir ~spool_dir ~maildir ->
  Eio.Switch.run @@ fun sw ->
  let store=open_store ~sw ~database ~blob_dir in
  let pair:J.pair={
    id="incompatible-absence";scope;
    remote_uidvalidity=Some (epoch 11L);remote_uid=Some (uid 1L);
    local_id=Some "missing-after-explicit-delete";
    content_sha256=Some (String.make 64 'a');content_length=Some 1L;
    internal_date=None;common_flags=[];remote_tombstone=None;
    local_tombstone=Some {J.reason=J.Explicit_delete;
      evidence="old-local-delete";generation=None};revision=0L} in
  (match J.put_pair store ~expected_revision:None pair with
   | `Committed _ -> () | `Stale_revision -> Alcotest.fail "new pair stale");
  let client=scripted_scan ~sw ~has_message:true () in
  let next_id ()="incompatible-operation" in
  let receipt=match Imap_sync.Bridge.copy_once
    ~deletion_policy:Imap.Sync_policy.Propagate
    ~ctx:(context ~store ~spool_dir ~next_id client) ~maildir
    ~stage_id:"incompatible-scan" () with
    | Ok receipt -> receipt
    | Error error -> Alcotest.failf "incompatible tombstone bridge: %a"
        Imap_sync.Error.pp error in
  Alcotest.(check int) "incompatible evidence held" 1
    receipt.deletions_held;
  Alcotest.(check int) "no deletion operation" 0
    (List.length (all_active_operations store ~scope));
  let planned=ref [] in
  (match preview_deletions ~spool_dir ~store ~maildir ~scope
    ~policy:Imap.Sync_policy.Propagate
    ~on_preview:(fun item -> planned:=item::!planned) () with
   | Ok _ -> ()
   | Error error -> Alcotest.failf "incompatible preview: %a"
       Imap_sync.Error.pp error);
  Alcotest.(check bool) "preview reports unverified absence" true
    (match !planned with
     | [{decision=`Plan (Imap.Sync_policy.Hold_deletion
         Imap.Sync_policy.Unverified_absence);_}] -> true
     | _ -> false)

let test_prepared_flags_with_stale_pair () =
  with_fixture @@ fun ~database ~blob_dir ~spool_dir ~maildir ->
  Eio.Switch.run @@ fun sw ->
  let store=open_store ~sw ~database ~blob_dir in
  let local=Md.append maildir
    ~source:(Eio.Flow.string_source message) ~length ~flags:[] () in
  let blob=Imap_store.Blob.put store
    ~source:(Eio.Flow.string_source message) ~length () in
  let pair : J.pair = {
    id="stale-flags-pair";scope;remote_uidvalidity=Some (epoch 11L);
    remote_uid=Some (uid 1L);local_id=Some local.id;
    content_sha256=Some blob.sha256;content_length=Some length;
    internal_date=None;
    common_flags=[];remote_tombstone=None;local_tombstone=None;
    revision=0L} in
  let pair=match J.put_pair store ~expected_revision:None pair with
    | `Committed pair -> pair
    | `Stale_revision -> Alcotest.fail "new pair was stale" in
  let op : J.operation = {
    id="stale-prepared-flags";pair_id=Some pair.id;
    local_id=Some local.id;scope;kind=J.Flags;state=J.Prepared;
    source_uidvalidity=Some (epoch 11L);source_uid=Some (uid 1L);
    destination=None;destination_uidvalidity=None;
    blob_sha256=None;blob_length=None;desired_flags=Some [];
    internal_date=None;append=None;
    receipt=None;receipt_uidvalidity=None;receipt_uid=None} in
  J.prepare_operation ~local_flags:[] store op;
  (match J.put_pair store ~expected_revision:(Some pair.revision) pair with
   | `Committed _ -> ()
   | `Stale_revision -> Alcotest.fail "pair advance was stale");
  let client=scripted_scan ~sw ~has_message:true () in
  (match run_bridge ~client ~store ~maildir ~spool_dir with
   | Ok _ -> ()
   | Error error -> Alcotest.failf "stale prepared FLAGS: %a"
       Imap_sync.Error.pp error);
  Alcotest.(check bool) "unsent FLAGS rejected despite pair revision" true
    (match J.find_operation store ~id:op.id with
     | Some {state=J.Rejected;_} -> true | _ -> false)

let test_flag_write_rejects_replaced_local_body () =
  with_fixture @@ fun ~database ~blob_dir ~spool_dir ~maildir ->
  Eio.Switch.run @@ fun sw ->
  let store=open_store ~sw ~database ~blob_dir in
  let changed=String.mapi (fun i c -> if i=0 then 'X' else c) message in
  let local=Md.append maildir
    ~source:(Eio.Flow.string_source changed) ~length
    ~flags:[Mail_flag.Imap_flag.system Mail_flag.Imap_flag.Seen] () in
  let blob=Imap_store.Blob.put store
    ~source:(Eio.Flow.string_source message) ~length () in
  let pair : J.pair = {
    id="replaced-flags-body";scope;
    remote_uidvalidity=Some (epoch 11L);remote_uid=Some (uid 1L);
    local_id=Some local.id;content_sha256=Some blob.sha256;
    content_length=Some length;internal_date=None;common_flags=[];
    remote_tombstone=None;local_tombstone=None;revision=0L} in
  let pair=match J.put_pair store ~expected_revision:None pair with
    | `Committed pair -> pair
    | `Stale_revision -> Alcotest.fail "new pair was stale" in
  let client=scripted_scan ~sw ~has_message:true () in
  let reject ()=match Md.with_writer maildir (fun writer ->
      let next_id ()="replaced-body-conflict" in
      Imap_sync.Flags.reconcile_pair
        ~ctx:(context ~store ~spool_dir ~next_id client) ~writer ~pair ())
    with
    | Error (Imap_sync.Error.Content_mismatch id) when id=pair.id -> ()
    | Error error -> Alcotest.failf "replaced body: %a"
        Imap_sync.Error.pp error
    | Ok _ -> Alcotest.fail "FLAGS update accepted replaced body" in
  reject ();
  let first=all_open_conflicts store ~scope in
  reject ();
  let second=all_open_conflicts store ~scope in
  Alcotest.(check bool) "stable pre-dispatch content conflict" true
    (match first,second with
     | [{id=a;kind=J.Content_conflict;_}],
       [{id=b;kind=J.Content_conflict;_}] -> a=b
     | _ -> false);
  Alcotest.(check int) "body mismatch created no FLAGS operation" 0
    (List.length (all_active_operations store ~scope));
  Alcotest.(check bool) "pair baseline unchanged" true
    (match J.find_pair store ~id:pair.id with
     | Some current -> current.revision=pair.revision &&
         current.common_flags=[]
     | None -> false);
  Md.remove maildir local;
  ignore (Md.append maildir ~id:local.id
    ~source:(Eio.Flow.string_source message) ~length ~flags:[] ());
  let repair_client=scripted_scan ~sw ~has_message:true () in
  (match run_bridge ~client:repair_client ~store ~maildir ~spool_dir with
   | Ok _ -> ()
   | Error error -> Alcotest.failf "restored content scan: %a"
       Imap_sync.Error.pp error);
  Alcotest.(check int) "restored body clears content hold" 0
    (List.length (all_open_conflicts store ~scope))

let test_sent_flags_recovery_holds_replaced_local_body () =
  with_fixture @@ fun ~database ~blob_dir ~spool_dir ~maildir ->
  Eio.Switch.run @@ fun sw ->
  let store=open_store ~sw ~database ~blob_dir in
  let changed=String.mapi (fun i c -> if i=0 then 'X' else c) message in
  let local=Md.append maildir
    ~source:(Eio.Flow.string_source changed) ~length ~flags:[] () in
  let blob=Imap_store.Blob.put store
    ~source:(Eio.Flow.string_source message) ~length () in
  let pair : J.pair = {
    id="sent-flags-replaced-body";scope;
    remote_uidvalidity=Some (epoch 11L);remote_uid=Some (uid 1L);
    local_id=Some local.id;content_sha256=Some blob.sha256;
    content_length=Some length;internal_date=None;common_flags=[];
    remote_tombstone=None;local_tombstone=None;revision=0L} in
  let pair=match J.put_pair store ~expected_revision:None pair with
    | `Committed pair -> pair
    | `Stale_revision -> Alcotest.fail "new pair was stale" in
  let seen=Mail_flag.Imap_flag.system Mail_flag.Imap_flag.Seen in
  let op : J.operation = {
    id="sent-flags-body-op";pair_id=Some pair.id;
    local_id=Some local.id;scope;kind=J.Flags;state=J.Prepared;
    source_uidvalidity=pair.remote_uidvalidity;source_uid=pair.remote_uid;
    destination=None;destination_uidvalidity=None;
    blob_sha256=None;blob_length=None;desired_flags=Some [seen];
    internal_date=None;append=None;
    receipt=None;receipt_uidvalidity=None;receipt_uid=None} in
  J.prepare_operation ~local_flags:[] store op;
  J.mark_sent store ~id:op.id;
  let op=Option.get (J.find_operation store ~id:op.id) in
  let client=scripted_scan ~sw ~has_message:true () in
  let recover ()=match Md.with_writer maildir (fun writer ->
      Imap_sync.Flags.recover_operation
        ~ctx:(context ~store ~spool_dir client) ~writer ~operation:op ()) with
    | Error (Imap_sync.Error.Pending_operations [id]) when id=op.id -> ()
    | Error error -> Alcotest.failf "sent FLAGS body mismatch: %a"
        Imap_sync.Error.pp error
    | Ok _ -> Alcotest.fail "sent FLAGS committed with replaced body" in
  recover ();
  let first=all_open_conflicts store ~scope in
  recover ();
  let second=all_open_conflicts store ~scope in
  Alcotest.(check bool) "stable durable FLAGS conflict" true
    (match first,second with
     | [{id=first_id;kind=J.Flag_conflict;_}],
       [{id=second_id;kind=J.Flag_conflict;_}] -> first_id=second_id
     | _ -> false);
  Alcotest.(check bool) "sent operation remains pending" true
    (match J.find_operation store ~id:op.id with
     | Some {state=J.Sent;_} -> true | _ -> false)

let seen=Mail_flag.Imap_flag.system Mail_flag.Imap_flag.Seen
let flagged=Mail_flag.Imap_flag.system Mail_flag.Imap_flag.Flagged
let deleted=Mail_flag.Imap_flag.system Mail_flag.Imap_flag.Deleted

let flag_pair ~store ~maildir ?(local_flags=[]) ?(body=message) id =
  let local=Md.append maildir
    ~source:(Eio.Flow.string_source body) ~length ~flags:local_flags () in
  let blob=Imap_store.Blob.put store
    ~source:(Eio.Flow.string_source message) ~length () in
  let pair : J.pair = {
    id;scope;remote_uidvalidity=Some (epoch 11L);remote_uid=Some (uid 1L);
    local_id=Some local.id;content_sha256=Some blob.sha256;
    content_length=Some length;internal_date=None;common_flags=[];
    remote_tombstone=None;local_tombstone=None;revision=0L} in
  match J.put_pair store ~expected_revision:None pair with
  | `Committed pair -> pair,local
  | `Stale_revision -> Alcotest.fail "new flag pair was stale"

let condstore_select=
  "* 1 EXISTS\r\n* OK [UIDVALIDITY 11] valid\r\n* OK [UIDNEXT 2] next\r\n\
   * OK [HIGHESTMODSEQ 20] modseq\r\n* FLAGS (\\Seen \\Flagged \\Deleted)\r\n\
   * OK [PERMANENTFLAGS (\\Seen \\Flagged \\Deleted \\*)] permanent\r\n\
   A00000004 OK [READ-WRITE] selected\r\n"

let flag_fetch ~tag flags=Printf.sprintf
  "* 1 FETCH (UID 1 FLAGS (%s) MODSEQ (20))\r\nA%08d OK fetched\r\n"
  flags tag

let reconcile ~client ~store ~maildir ~spool_dir pair=
  let ctx=context ~store ~spool_dir ~next_id:(fun () -> "flag-op") client in
  Md.with_writer maildir (fun writer ->
    Imap_sync.Flags.reconcile_pair ~ctx ~writer ~pair ())

let operation_state store id=
  (Option.get (J.find_operation store ~id)).state

let test_rejected_store_rejects_operation () =
  with_fixture @@ fun ~database ~blob_dir ~spool_dir ~maildir ->
  Eio.Switch.run @@ fun sw ->
  let store=open_store ~sw ~database ~blob_dir in
  let pair,_=flag_pair ~store ~maildir ~local_flags:[seen] "rejected-store" in
  let client,_=scripted_client ~caps:"IMAP4rev1 UNSELECT CONDSTORE" ~sw
    "rejected-store" [
    condstore_select; flag_fetch ~tag:5 "";
    "A00000006 NO [CANNOT] refused\r\n";
    "A00000007 OK unselected\r\n"] in
  (match reconcile ~client ~store ~maildir ~spool_dir pair with
   | Error (Imap_sync.Error.Client (Imap_eio.Error.Rejected _)) -> ()
   | Error error -> Alcotest.failf "wrong rejected STORE error: %a"
       Imap_sync.Error.pp error
   | Ok _ -> Alcotest.fail "rejected STORE committed");
  Alcotest.(check bool) "rejected STORE rejects the operation" true
    (match J.find_operation store ~id:"flag-op" with
     | Some {state=J.Rejected;receipt=Some receipt;_} ->
         String.starts_with ~prefix:"UID STORE not applied" receipt
     | _ -> false);
  Alcotest.(check int) "rejected STORE opens no conflict" 0
    (List.length (all_open_conflicts store ~scope))

let test_uncertain_store_leaves_conflict () =
  with_fixture @@ fun ~database ~blob_dir ~spool_dir ~maildir ->
  Eio.Switch.run @@ fun sw ->
  let store=open_store ~sw ~database ~blob_dir in
  let pair,_=flag_pair ~store ~maildir ~local_flags:[seen] "uncertain-store" in
  let client,_=scripted_client ~caps:"IMAP4rev1 UNSELECT CONDSTORE" ~sw
    ~tail:[`Raise End_of_file] "uncertain-store" [
    condstore_select; flag_fetch ~tag:5 ""] in
  (match reconcile ~client ~store ~maildir ~spool_dir pair with
   | Error (Imap_sync.Error.Pending_operations ["flag-op"]) -> ()
   | Error error -> Alcotest.failf "wrong uncertain STORE error: %a"
       Imap_sync.Error.pp error
   | Ok _ -> Alcotest.fail "uncertain STORE committed");
  Alcotest.(check bool) "uncertain STORE is ambiguous with a reason" true
    (match J.find_operation store ~id:"flag-op" with
     | Some {state=J.Ambiguous;receipt=Some receipt;_} ->
         String.starts_with ~prefix:"UID STORE outcome unknown" receipt
     | _ -> false);
  Alcotest.(check bool) "uncertain STORE leaves a flag conflict" true
    (match all_open_conflicts store ~scope with
     | [{kind=J.Flag_conflict;_}] -> true
     | _ -> false)

let test_failed_verification_read_is_pending () =
  with_fixture @@ fun ~database ~blob_dir ~spool_dir ~maildir ->
  Eio.Switch.run @@ fun sw ->
  let store=open_store ~sw ~database ~blob_dir in
  let pair,_=flag_pair ~store ~maildir ~local_flags:[seen] "verify-read" in
  let client,_=scripted_client ~caps:"IMAP4rev1 UNSELECT CONDSTORE" ~sw
    ~tail:[`Raise End_of_file] "verify-read" [
    condstore_select; flag_fetch ~tag:5 "";
    "* 1 FETCH (UID 1 FLAGS (\\Seen) MODSEQ (21))\r\n\
     A00000006 OK stored\r\n"] in
  (match reconcile ~client ~store ~maildir ~spool_dir pair with
   | Error (Imap_sync.Error.Pending_operations ["flag-op"]) -> ()
   | Error error -> Alcotest.failf "wrong verification read error: %a"
       Imap_sync.Error.pp error
   | Ok _ -> Alcotest.fail "unverified STORE committed");
  Alcotest.(check bool) "failed verification read is ambiguous" true
    (match J.find_operation store ~id:"flag-op" with
     | Some {state=J.Ambiguous;receipt=Some receipt;_} ->
         String.starts_with
           ~prefix:"FLAGS write returned but its verification read failed"
           receipt
     | _ -> false);
  Alcotest.(check bool) "failed verification read leaves a conflict" true
    (match all_open_conflicts store ~scope with
     | [{kind=J.Flag_conflict;_}] -> true
     | _ -> false)

let test_local_only_race_rejects_prepared () =
  with_fixture @@ fun ~database ~blob_dir ~spool_dir ~maildir ->
  Eio.Switch.run @@ fun sw ->
  let store=open_store ~sw ~database ~blob_dir in
  let pair,local=flag_pair ~store ~maildir "local-only-race" in
  let client,_=scripted_client ~caps:"IMAP4rev1 UNSELECT CONDSTORE" ~sw
    "local-only-race" [
    condstore_select; flag_fetch ~tag:5 "\\Seen";
    "* 1 FETCH (UID 1 FLAGS (\\Seen \\Flagged))\r\n\
     A00000006 OK fetched\r\n";
    "A00000007 OK unselected\r\n"] in
  (match reconcile ~client ~store ~maildir ~spool_dir pair with
   | Error Imap_sync.Error.Modified -> ()
   | Error error -> Alcotest.failf "wrong local-only race error: %a"
       Imap_sync.Error.pp error
   | Ok _ -> Alcotest.fail "local-only write ignored a remote change");
  Alcotest.(check bool) "race rejected before dispatch" true
    (operation_state store "flag-op"=J.Rejected);
  Alcotest.(check int) "race opens no conflict" 0
    (List.length (all_open_conflicts store ~scope));
  Alcotest.(check bool) "local flags untouched" true
    ((Option.get (Md.find maildir ~id:local.id)).flags=[])

let test_held_deleted_merges_other_flags () =
  with_fixture @@ fun ~database ~blob_dir ~spool_dir ~maildir ->
  Eio.Switch.run @@ fun sw ->
  let store=open_store ~sw ~database ~blob_dir in
  let pair,local=flag_pair ~store ~maildir "held-deleted" in
  let client,_=scripted_client ~caps:"IMAP4rev1 UNSELECT CONDSTORE" ~sw
    "held-deleted" [
    condstore_select; flag_fetch ~tag:5 "\\Deleted \\Seen";
    "* 1 FETCH (UID 1 FLAGS (\\Deleted \\Seen))\r\n\
     A00000006 OK fetched\r\n";
    "A00000007 OK unselected\r\n"] in
  (match reconcile ~client ~store ~maildir ~spool_dir pair with
   | Ok {outcome=Imap_sync.Flags.Updated updated;deleted_held=true} ->
       Alcotest.(check bool) "common flags gain Seen only" true
         (Mail_flag.Imap_flag.equal_durable updated.common_flags [seen])
   | Ok _ -> Alcotest.fail "held \\Deleted blocked the Seen merge"
   | Error error -> Alcotest.failf "held \\Deleted merge: %a"
       Imap_sync.Error.pp error);
  Alcotest.(check bool) "local gains Seen without Deleted" true
    (Mail_flag.Imap_flag.equal_durable
      (Option.get (Md.find maildir ~id:local.id)).flags [seen])

let test_settle_reports_content_mismatch () =
  with_fixture @@ fun ~database ~blob_dir ~spool_dir ~maildir ->
  Eio.Switch.run @@ fun sw ->
  let store=open_store ~sw ~database ~blob_dir in
  let changed=String.mapi (fun i c -> if i=0 then 'X' else c) message in
  let pair,local=flag_pair ~store ~maildir ~body:changed "settle-content" in
  let op : J.operation = {
    id="settle-content-op";pair_id=Some pair.id;local_id=Some local.id;
    scope;kind=J.Flags;state=J.Prepared;
    source_uidvalidity=pair.remote_uidvalidity;source_uid=pair.remote_uid;
    destination=None;destination_uidvalidity=None;blob_sha256=None;
    blob_length=None;desired_flags=Some [];
    internal_date=None;append=None;
    receipt=None;
    receipt_uidvalidity=None;receipt_uid=None} in
  J.prepare_operation ~local_flags:[] store op;
  J.mark_sent store ~id:op.id;
  let client,_=scripted_client ~sw "settle-content" [] in
  let settle id=Imap_sync.Repair.settle_flags
    ~ctx:(context ~store ~spool_dir client) ~maildir ~id
    ~evidence:"operator audit" () in
  (match settle op.id with
   | Error (Imap_sync.Error.Content_mismatch id) when id=pair.id -> ()
   | Error error -> Alcotest.failf "wrong settle content error: %a"
       Imap_sync.Error.pp error
   | Ok _ -> Alcotest.fail "settled a changed local body");
  match settle "unknown-op" with
  | Error Imap_sync.Error.No_pending_operation -> ()
  | _ -> Alcotest.fail "unknown settle operation not typed"

let delete_select tag=Printf.sprintf
  "* 2 EXISTS\r\n* OK [UIDVALIDITY 11] valid\r\n* OK [UIDNEXT 3] next\r\n\
   * OK [HIGHESTMODSEQ 20] modseq\r\n* FLAGS (\\Seen \\Deleted)\r\n\
   * OK [PERMANENTFLAGS (\\Seen \\Deleted \\*)] permanent\r\n\
   A%08d OK [READ-WRITE] selected\r\n" tag

let remote_delete_pair ~store ?(evidence=true) id =
  let blob=Imap_store.Blob.put store
    ~source:(Eio.Flow.string_source message) ~length () in
  let pair : J.pair = {
    id;scope;remote_uidvalidity=Some (epoch 11L);remote_uid=Some (uid 1L);
    local_id=Some (id ^ "-local");
    content_sha256=(if evidence then Some blob.sha256 else None);
    content_length=(if evidence then Some length else None);
    internal_date=None;common_flags=[];remote_tombstone=None;
    local_tombstone=Some {J.reason=J.Local_absence;
      evidence="complete-local-inventory";generation=None};revision=0L} in
  match J.put_pair store ~expected_revision:None pair with
  | `Committed pair -> pair
  | `Stale_revision -> Alcotest.fail "new deletion pair was stale"

let delete_pair ~client ~store ~maildir ~spool_dir pair =
  Md.with_writer maildir (fun writer ->
    Md.ok @@ Local_inventory.with_pages ~spool_dir maildir
      (fun local_inventory ->
        Imap_sync.Deletion.reconcile_pair
          ~ctx:(context ~store ~spool_dir ~next_id:(fun () -> "delete-op")
            client)
          ~writer ~cursor:(Imap_store.load_cursor store ~scope)
          ~local_inventory ~pair ~policy:Imap.Sync_policy.Propagate ()))

let held_survivor = function
  | Ok (Imap_sync.Deletion.Held Imap.Sync_policy.Survivor_changed) -> true
  | _ -> false

let test_modified_delete_is_held () =
  with_fixture @@ fun ~database ~blob_dir ~spool_dir ~maildir ->
  Eio.Switch.run @@ fun sw ->
  let store=open_store ~sw ~database ~blob_dir in
  publish_two ~sw ~store ~spool_dir;
  let pair=remote_delete_pair ~store "modified-delete" in
  let size=String.length message in
  let meta tag=Printf.sprintf
    "* 1 FETCH (UID 1 FLAGS () MODSEQ (20))\r\nA%08d OK fetched\r\n" tag in
  let client,_=scripted_client
    ~caps:"IMAP4rev1 UNSELECT UIDPLUS CONDSTORE" ~sw "modified-delete" [
    delete_select 4; meta 5;
    Printf.sprintf "* 1 FETCH (UID 1 BODY[] {%d}\r\n" size;
    message ^ ")\r\nA00000006 OK fetched\r\n";
    meta 7; "A00000008 OK unselected\r\n";
    delete_select 9; meta 10;
    "A00000011 OK [MODIFIED 1] conditional STORE failed\r\n";
    "A00000012 OK unselected\r\n"] in
  let result=delete_pair ~client ~store ~maildir ~spool_dir pair in
  Alcotest.(check bool) "MODIFIED is a survivor hold" true
    (held_survivor result);
  Alcotest.(check bool) "MODIFIED rejects the operation" true
    (operation_state store "delete-op"=J.Rejected)

let test_uncertain_delete_dispatch_is_pending ~expunge () =
  with_fixture @@ fun ~database ~blob_dir ~spool_dir ~maildir ->
  Eio.Switch.run @@ fun sw ->
  let store=open_store ~sw ~database ~blob_dir in
  publish_two ~sw ~store ~spool_dir;
  let pair=remote_delete_pair ~store "uncertain-dispatch" in
  let size=String.length message in
  let meta tag=Printf.sprintf
    "* 1 FETCH (UID 1 FLAGS () MODSEQ (20))\r\nA%08d OK fetched\r\n" tag in
  let stored=if not expunge then [] else [
    "* 1 FETCH (UID 1 FLAGS (\\Deleted) MODSEQ (21))\r\n\
     A00000011 OK stored\r\n";
    "* 1 FETCH (UID 1 FLAGS (\\Deleted) MODSEQ (21))\r\n\
     A00000012 OK fetched\r\n"] in
  let client,_=scripted_client
    ~caps:"IMAP4rev1 UNSELECT UIDPLUS CONDSTORE" ~sw
    ~tail:[`Raise End_of_file] "uncertain-dispatch" ([
    delete_select 4; meta 5;
    Printf.sprintf "* 1 FETCH (UID 1 BODY[] {%d}\r\n" size;
    message ^ ")\r\nA00000006 OK fetched\r\n";
    meta 7; "A00000008 OK unselected\r\n";
    delete_select 9; meta 10] @ stored) in
  (match delete_pair ~client ~store ~maildir ~spool_dir pair with
   | Error (Imap_sync.Error.Pending_operations ["delete-op"]) -> ()
   | Error error -> Alcotest.failf "wrong uncertain dispatch error: %a"
       Imap_sync.Error.pp error
   | Ok _ -> Alcotest.fail "uncertain dispatch was not pending");
  let prefix=if expunge then "UID EXPUNGE outcome unknown"
    else "conditional STORE outcome unknown" in
  Alcotest.(check bool) "uncertain dispatch is ambiguous" true
    (match J.find_operation store ~id:"delete-op" with
     | Some {state=J.Ambiguous;receipt=Some receipt;_} ->
         String.starts_with ~prefix receipt
     | _ -> false)

let test_foreign_scope_pair_is_stale () =
  with_fixture @@ fun ~database ~blob_dir ~spool_dir ~maildir ->
  Eio.Switch.run @@ fun sw ->
  let store=open_store ~sw ~database ~blob_dir in
  let other={scope with mailbox_key="Archive";raw_name="Archive"} in
  let pair : J.pair = {
    id="foreign-pair";scope=other;remote_uidvalidity=Some (epoch 11L);
    remote_uid=Some (uid 1L);local_id=Some "foreign-local";
    content_sha256=None;content_length=None;internal_date=None;
    common_flags=[];remote_tombstone=None;local_tombstone=None;
    revision=0L} in
  let pair=match J.put_pair store ~expected_revision:None pair with
    | `Committed pair -> pair
    | `Stale_revision -> Alcotest.fail "new foreign pair was stale" in
  let client,_=scripted_client ~sw "foreign-scope" [] in
  match delete_pair ~client ~store ~maildir ~spool_dir pair with
  | Error Imap_sync.Error.Stale_pair -> ()
  | Error error -> Alcotest.failf "wrong foreign pair error: %a"
      Imap_sync.Error.pp error
  | Ok _ -> Alcotest.fail "reconciled a pair of another scope"

let test_remote_delete_missing_spool_is_configuration () =
  with_fixture @@ fun ~database ~blob_dir ~spool_dir ~maildir ->
  Eio.Switch.run @@ fun sw ->
  let store=open_store ~sw ~database ~blob_dir in
  publish_two ~sw ~store ~spool_dir;
  let pair=remote_delete_pair ~store "missing-spool" in
  let client,_=scripted_client
    ~caps:"IMAP4rev1 UNSELECT UIDPLUS CONDSTORE" ~sw "missing-spool" [] in
  let missing=Eio.Path.(spool_dir / "missing") in
  let result=Md.with_writer maildir (fun writer ->
    Md.ok @@ Local_inventory.with_pages ~spool_dir maildir
      (fun local_inventory ->
        Imap_sync.Deletion.reconcile_pair
          ~ctx:(context ~store ~spool_dir:missing client) ~writer
          ~cursor:(Imap_store.load_cursor store ~scope) ~local_inventory
          ~pair ~policy:Imap.Sync_policy.Propagate ())) in
  match result with
  | Error (Imap_sync.Error.Invalid_configuration _) -> ()
  | Error error -> Alcotest.failf "wrong missing spool error: %a"
      Imap_sync.Error.pp error
  | Ok _ -> Alcotest.fail "remote deletion ran without a spool directory"

let test_longer_remote_body_is_held () =
  with_fixture @@ fun ~database ~blob_dir ~spool_dir ~maildir ->
  Eio.Switch.run @@ fun sw ->
  let store=open_store ~sw ~database ~blob_dir in
  publish_two ~sw ~store ~spool_dir;
  let pair=remote_delete_pair ~store "longer-delete" in
  let longer=message ^ "extra" in
  let client,_=scripted_client
    ~caps:"IMAP4rev1 UNSELECT UIDPLUS CONDSTORE" ~sw "longer-delete" [
    delete_select 4;
    "* 1 FETCH (UID 1 FLAGS () MODSEQ (20))\r\nA00000005 OK fetched\r\n";
    Printf.sprintf "* 1 FETCH (UID 1 BODY[] {%d}\r\n"
      (String.length longer);
    longer ^ ")\r\nA00000006 OK fetched\r\n";
    "A00000007 OK unselected\r\n"] in
  let result=delete_pair ~client ~store ~maildir ~spool_dir pair in
  Alcotest.(check bool) "longer body is a survivor hold" true
    (held_survivor result);
  Alcotest.(check bool) "longer body prepared no operation" true
    (J.find_operation store ~id:"delete-op"=None)

let test_expunged_during_body_fetch_is_stale () =
  with_fixture @@ fun ~database ~blob_dir ~spool_dir ~maildir ->
  Eio.Switch.run @@ fun sw ->
  let store=open_store ~sw ~database ~blob_dir in
  publish_two ~sw ~store ~spool_dir;
  let pair=remote_delete_pair ~store "expunged-delete" in
  let client,_=scripted_client
    ~caps:"IMAP4rev1 UNSELECT UIDPLUS CONDSTORE" ~sw "expunged-delete" [
    delete_select 4;
    "* 1 FETCH (UID 1 FLAGS () MODSEQ (20))\r\nA00000005 OK fetched\r\n";
    "A00000006 OK fetched\r\n";
    "A00000007 OK unselected\r\n"] in
  match delete_pair ~client ~store ~maildir ~spool_dir pair with
  | Error Imap_sync.Error.Stale_inventory -> ()
  | Error error -> Alcotest.failf "wrong expunge race error: %a"
      Imap_sync.Error.pp error
  | Ok _ -> Alcotest.fail "expunge race was not stale"

let test_legacy_pair_holds_without_evidence () =
  with_fixture @@ fun ~database ~blob_dir ~spool_dir ~maildir ->
  Eio.Switch.run @@ fun sw ->
  let store=open_store ~sw ~database ~blob_dir in
  publish_two ~sw ~store ~spool_dir;
  let pair=remote_delete_pair ~store ~evidence:false "legacy-delete" in
  let client,_=scripted_client ~sw "legacy-delete" [] in
  match delete_pair ~client ~store ~maildir ~spool_dir pair with
  | Ok (Imap_sync.Deletion.Held
      Imap.Sync_policy.Missing_content_evidence) -> ()
  | Ok _ -> Alcotest.fail "legacy pair not held for content evidence"
  | Error error -> Alcotest.failf "legacy pair: %a"
      Imap_sync.Error.pp error

let test_changed_local_survivor_is_held () =
  with_fixture @@ fun ~database ~blob_dir ~spool_dir ~maildir ->
  Eio.Switch.run @@ fun sw ->
  let store=open_store ~sw ~database ~blob_dir in
  let empty,_=scripted_client ~sw "empty-scan" [
    examine ~tag:4 ~exists:0 ~uidnext:2 ();
    "A00000005 OK fetched\r\n";
    "* SEARCH\r\nA00000006 OK searched\r\n";
    "A00000007 OK unselected\r\n"] in
  (match Imap_sync.Engine.scan_once ~ctx:(context ~store ~spool_dir empty)
      ~stage_id:"empty-scan" () with
   | Ok _ -> ()
   | Error error -> Alcotest.failf "empty scan: %a"
       Imap_sync.Error.pp error);
  let cursor=Imap_store.load_cursor store ~scope in
  let pair,local=flag_pair ~store ~maildir "local-survivor" in
  let pair=match J.put_pair store ~expected_revision:(Some pair.revision)
      {pair with remote_tombstone=Some {J.reason=J.Inventory_absence;
        evidence=Option.get cursor.inventory_ref;
        generation=Some cursor.generation}} with
    | `Committed pair -> pair
    | `Stale_revision -> Alcotest.fail "remote absence was stale" in
  let client,_=scripted_client ~sw "local-survivor" [
    examine ~tag:4 ~exists:0 ~uidnext:2 ();
    "A00000005 OK fetched\r\n";
    "A00000006 OK unselected\r\n"] in
  let result=Md.with_writer maildir (fun writer ->
    Md.ok @@ Local_inventory.with_pages ~spool_dir maildir
      (fun local_inventory ->
        ignore (Md.ok (Maildir.set_flags writer local [seen]));
        Imap_sync.Deletion.reconcile_pair
          ~ctx:(context ~store ~spool_dir ~next_id:(fun () -> "delete-op")
            client)
          ~writer ~cursor ~local_inventory ~pair
          ~policy:Imap.Sync_policy.Propagate ())) in
  Alcotest.(check bool) "changed local survivor is held" true
    (held_survivor result);
  Alcotest.(check bool) "changed survivor was not unlinked" true
    (Md.find maildir ~id:local.id<>None)

let test_content_mismatch_is_bridge_hold () =
  with_fixture @@ fun ~database ~blob_dir ~spool_dir ~maildir ->
  Eio.Switch.run @@ fun sw ->
  let store=open_store ~sw ~database ~blob_dir in
  let changed=String.mapi (fun i c -> if i=0 then 'X' else c) message in
  let pair,_=flag_pair ~store ~maildir ~local_flags:[seen] ~body:changed
    "bridge-content-hold" in
  let client=scripted_scan ~sw ~has_message:true () in
  match run_bridge ~client ~store ~maildir ~spool_dir with
  | Ok receipt ->
      Alcotest.(check int) "content mismatch counted once" 1
        receipt.flags_held;
      Alcotest.(check (list string)) "content mismatch names the pair"
        [pair.id] receipt.held_pair_ids
  | Error error -> Alcotest.failf "content mismatch aborted the cycle: %a"
      Imap_sync.Error.pp error

let test_unconditional_store_is_bridge_hold () =
  with_fixture @@ fun ~database ~blob_dir ~spool_dir ~maildir ->
  Eio.Switch.run @@ fun sw ->
  let store=open_store ~sw ~database ~blob_dir in
  let pair,_=flag_pair ~store ~maildir ~local_flags:[seen]
    "bridge-condstore-hold" in
  let client,_=scripted_client ~sw "bridge-condstore-hold" [
    examine ~tag:4 ~exists:1 ~uidnext:2 ();
    "* 1 FETCH (UID 1 FLAGS ())\r\nA00000005 OK fetched\r\n";
    "* SEARCH 1\r\nA00000006 OK searched\r\n";
    "A00000007 OK unselected\r\n";
    "* 1 EXISTS\r\n* OK [UIDVALIDITY 11] valid\r\n\
     * OK [UIDNEXT 2] next\r\nA00000008 OK [READ-WRITE] selected\r\n";
    "* 1 FETCH (UID 1 FLAGS ())\r\nA00000009 OK fetched\r\n";
    "A00000010 OK unselected\r\n"] in
  match run_bridge ~client ~store ~maildir ~spool_dir with
  | Ok receipt ->
      Alcotest.(check (list string)) "missing CONDSTORE holds the pair"
        [pair.id] receipt.held_pair_ids;
      Alcotest.(check bool) "hold is durable" true
        (J.has_open_conflict store ~pair ~kind:J.Policy_conflict)
  | Error error -> Alcotest.failf "missing CONDSTORE aborted the cycle: %a"
      Imap_sync.Error.pp error

let test_preview_deleted_flag_propagation () =
  with_fixture @@ fun ~database ~blob_dir ~spool_dir ~maildir ->
  Eio.Switch.run @@ fun sw ->
  let store=open_store ~sw ~database ~blob_dir in
  let pair,_=flag_pair ~store ~maildir "preview-deleted-flag" in
  let client=scripted_scan ~sw ~has_message:true ~flags:"\\Deleted" () in
  (match Imap_sync.Engine.scan_once ~ctx:(context ~store ~spool_dir client)
    ~stage_id:"preview-deleted-flag" () with
   | Ok _ -> ()
   | Error error -> Alcotest.failf "preview source scan: %a"
       Imap_sync.Error.pp error);
  let preview ?propagate_deleted () =
    let events=ref [] in
    match Imap_sync.Plan.preview_sync ?propagate_deleted ~spool_dir ~store
      ~maildir ~scope ~policy:Imap.Sync_policy.Preserve
      ~on_preview:(fun event -> events:=event::!events) () with
    | Ok _ -> List.rev !events
    | Error error -> Alcotest.failf "deleted flag preview: %a"
        Imap_sync.Error.pp error in
  Alcotest.(check bool) "default previews a Deleted hold" true
    (match preview () with
     | [Imap_sync.Plan.Preview_pair_hold (id,_)] -> id=pair.id
     | _ -> false);
  Alcotest.(check bool) "propagation previews a local Deleted change" true
    (match preview ~propagate_deleted:true () with
     | [Imap_sync.Plan.Preview_flags flags] ->
         flags.pair_id=pair.id && flags.to_local.add=[deleted] &&
         flags.to_local.remove=[] && flags.to_remote.add=[] &&
         flags.to_remote.remove=[]
     | _ -> false)

let test_deleted_flag_propagation ~propagate () =
  with_fixture @@ fun ~database ~blob_dir ~spool_dir ~maildir ->
  Eio.Switch.run @@ fun sw ->
  let store=open_store ~sw ~database ~blob_dir in
  let pair,local=flag_pair ~store ~maildir "bridge-deleted-flag" in
  let remote tag=Printf.sprintf
    "* 1 FETCH (UID 1 FLAGS (\\Deleted) MODSEQ (20))\r\n\
     A%08d OK fetched\r\n" tag in
  let select tag=Printf.sprintf
    "* 1 EXISTS\r\n* OK [UIDVALIDITY 11] valid\r\n\
     * OK [UIDNEXT 2] next\r\n* OK [HIGHESTMODSEQ 20] modseq\r\n\
     * FLAGS (\\Seen \\Deleted)\r\n\
     * OK [PERMANENTFLAGS (\\Seen \\Deleted \\*)] permanent\r\n\
     A%08d OK [READ-WRITE] selected\r\n" tag in
  let flag_phase=if propagate then [
      select 8; remote 9;
      "* 1 FETCH (UID 1 FLAGS (\\Deleted))\r\nA00000010 OK fetched\r\n";
      "A00000011 OK unselected\r\n"]
    else [select 8; remote 9; "A00000010 OK unselected\r\n"] in
  let client,_=scripted_client ~caps:"IMAP4rev1 UNSELECT CONDSTORE" ~sw
    "bridge-deleted-flag" ([
    examine ~tag:4 ~exists:1 ~uidnext:2
      ~extra:"* OK [HIGHESTMODSEQ 20] modseq\r\n" ();
    remote 5;
    "* SEARCH 1\r\nA00000006 OK searched\r\n";
    "A00000007 OK unselected\r\n"] @ flag_phase) in
  let ids=ref 0 in
  let next_id ()=incr ids; Printf.sprintf "deleted-flag-%d" !ids in
  let receipt=match Imap_sync.Bridge.copy_once ~propagate_deleted:propagate
      ~ctx:(context ~store ~spool_dir ~next_id client) ~maildir
      ~stage_id:"deleted-flag" () with
    | Ok receipt -> receipt
    | Error error -> Alcotest.failf "deleted flag cycle: %a"
        Imap_sync.Error.pp error in
  let local_flags=(Option.get (Md.find maildir ~id:local.id)).flags in
  let common=(Option.get (J.find_pair store ~id:pair.id)).common_flags in
  if propagate then (
    Alcotest.(check (pair int int)) "Deleted merged, not held" (1,0)
      (receipt.flags_updated,receipt.flags_held);
    Alcotest.(check bool) "local and common flags gain Deleted" true
      (Mail_flag.Imap_flag.equal_durable local_flags [deleted] &&
       Mail_flag.Imap_flag.equal_durable common [deleted]);
    Alcotest.(check bool) "no policy hold" false
      (J.has_open_conflict store ~pair ~kind:J.Policy_conflict))
  else (
    Alcotest.(check (pair int int)) "Deleted held by default" (0,1)
      (receipt.flags_updated,receipt.flags_held);
    Alcotest.(check bool) "local and common flags unchanged" true
      (local_flags=[] && common=[]);
    Alcotest.(check bool) "durable policy hold" true
      (J.has_open_conflict store ~pair ~kind:J.Policy_conflict))

let test_invalid_configuration_before_lease () =
  with_fixture @@ fun ~database ~blob_dir ~spool_dir ~maildir ->
  Eio.Switch.run @@ fun sw ->
  let store=open_store ~sw ~database ~blob_dir in
  let client,_=scripted_client ~sw "configuration-before-lease" [] in
  let ctx=context ~store ~spool_dir client in
  Md.with_writer maildir (fun _ ->
    match Imap_sync.Bridge.copy_once ~max_transfers:0 ~ctx ~maildir
        ~stage_id:"configuration-before-lease" () with
    | Error (Imap_sync.Error.Invalid_configuration _) -> ()
    | Error error -> Alcotest.failf "wrong busy-lease configuration: %a"
        Imap_sync.Error.pp error
    | Ok _ -> Alcotest.fail "cycle ran with a zero transfer budget")

let test_tombstoned_pair_present_is_hold () =
  with_fixture @@ fun ~database ~blob_dir ~spool_dir ~maildir ->
  Eio.Switch.run @@ fun sw ->
  let store=open_store ~sw ~database ~blob_dir in
  let pair,_=flag_pair ~store ~maildir "bridge-tombstone-hold" in
  (match J.put_pair store ~expected_revision:(Some pair.revision)
      {pair with local_tombstone=Some {J.reason=J.Retention;
        evidence="retained";generation=None}} with
   | `Committed _ -> ()
   | `Stale_revision -> Alcotest.fail "retention tombstone stale");
  let client=scripted_scan ~sw ~has_message:true () in
  match run_bridge ~client ~store ~maildir ~spool_dir with
  | Ok receipt ->
      Alcotest.(check (list string)) "tombstoned present pair held"
        [pair.id] receipt.held_pair_ids
  | Error error -> Alcotest.failf "tombstoned pair: %a"
      Imap_sync.Error.pp error

let test_unstorable_remote_copy_is_rejected () =
  with_fixture @@ fun ~database ~blob_dir ~spool_dir ~maildir ->
  Eio.Switch.run @@ fun sw ->
  let store=open_store ~sw ~database ~blob_dir in
  let size=String.length message in
  let client,_=scripted_client ~sw "unstorable-copy" [
    examine ~tag:4 ~exists:1 ~uidnext:2 ();
    "* 1 FETCH (UID 1 FLAGS (\\Recent))\r\nA00000005 OK fetched\r\n";
    "* SEARCH 1\r\nA00000006 OK searched\r\n";
    "A00000007 OK unselected\r\n";
    examine ~tag:8 ~exists:1 ~uidnext:2 ();
    Printf.sprintf "* 1 FETCH (UID 1 BODY[] {%d}\r\n" size;
    message ^ ")\r\nA00000009 OK fetched\r\n";
    "* 1 FETCH (UID 1 FLAGS () INTERNALDATE \"31-Dec-2016 23:59:60 +0000\")\r\n\
     A00000010 OK fetched\r\n";
    "A00000011 OK unselected\r\n"] in
  let next_id ()="unstorable" in
  (match Imap_sync.Bridge.copy_once
      ~ctx:(context ~store ~spool_dir ~next_id client) ~maildir
      ~stage_id:"unstorable" () with
   | Error (Imap_sync.Error.Invalid_operation _) -> ()
   | Error error -> Alcotest.failf "wrong unstorable error: %a"
       Imap_sync.Error.pp error
   | Ok _ -> Alcotest.fail "unstorable date was written");
  Alcotest.(check bool) "unstorable copy rejected in the journal" true
    (match J.find_operation store ~id:"unstorable" with
     | Some {state=J.Rejected;receipt=Some receipt;_} ->
         String.starts_with ~prefix:"Maildir cannot store" receipt
     | _ -> false)

let test_verify_local_content_without_flag_change () =
  with_fixture @@ fun ~database ~blob_dir ~spool_dir ~maildir ->
  Eio.Switch.run @@ fun sw ->
  let store=open_store ~sw ~database ~blob_dir in
  let altered=String.mapi (fun i c -> if i=0 then 'X' else c) message in
  let local=Md.append maildir
    ~source:(Eio.Flow.string_source altered) ~length ~flags:[] () in
  let blob=Imap_store.Blob.put store
    ~source:(Eio.Flow.string_source message) ~length () in
  let pair:J.pair={id="silent-local-body-change";scope;
    remote_uidvalidity=Some (epoch 11L);remote_uid=Some (uid 1L);
    local_id=Some local.id;content_sha256=Some blob.sha256;
    content_length=Some length;internal_date=None;common_flags=[];
    remote_tombstone=None;local_tombstone=None;revision=0L} in
  let pair=match J.put_pair store ~expected_revision:None pair with
    | `Committed pair -> pair
    | `Stale_revision -> Alcotest.fail "new pair was stale" in
  let issues=ref [] in
  let verify ()=match Imap_sync.Bridge.verify_local_content ~spool_dir
      ~store ~maildir
      ~scope ~next_id:(fun () -> "silent-content-conflict")
      ~on_issue:(fun id reason -> issues:=(id,reason)::!issues) () with
    | Ok report -> report
    | Error error -> Alcotest.failf "local verification: %a"
        Imap_sync.Error.pp error in
  let first=verify () in
  Alcotest.(check int64) "same-length mismatch detected" 1L
    first.mismatched;
  Alcotest.(check bool) "issue identifies paired occurrence" true
    (List.exists (fun (id,_) -> id=pair.id) !issues);
  let conflict=match all_open_conflicts store ~scope with
    | [{id;kind=J.Content_conflict;_}] -> id
    | _ -> Alcotest.fail "silent change lacked content conflict" in
  let second=verify () in
  Alcotest.(check int64) "repeat detects mismatch" 1L
    second.mismatched;
  Alcotest.(check string) "silent change conflict ID stable" conflict
    (List.hd (all_open_conflicts store ~scope)).id;
  Md.remove maildir local;
  ignore (Md.append maildir ~id:local.id
    ~source:(Eio.Flow.string_source message) ~length ~flags:[] ());
  let restored=verify () in
  Alcotest.(check int64) "restored body verified" 1L
    restored.checked;
  Alcotest.(check int64) "restored conflict resolved" 1L
    restored.restored;
  Alcotest.(check int) "no remaining content conflict" 0
    (List.length (all_open_conflicts store ~scope));
  Alcotest.(check int64) "verification does not revise pair" pair.revision
    (Option.get (J.find_pair store ~id:pair.id)).revision

let crash_child dir phase =
  Eio_main.run @@ fun env ->
  Eio.Switch.run @@ fun sw ->
  let fs=Eio.Stdenv.fs env in
  let database=Eio.Path.(fs / dir / "sync.db") in
  let blob_dir=Eio.Path.(fs / dir / "blob") in
  let maildir=Md.open_dir Eio.Path.(fs / dir / "maildir") in
  let store=open_store ~sw ~database ~blob_dir in
  let blob=Imap_store.Blob.put store
    ~source:(Eio.Flow.string_source message) ~length () in
  let local_id=Md.reserve_id () in
  let id="killed-" ^ phase in
  let op=operation ~kind:J.Local_append ~id
    ~local_id ~source_uid:(Some (uid 1L)) ~blob ~flags:[] in
  J.prepare_operation store op;
  if phase="prepared" then Unix._exit 77;
  J.mark_sent store ~id:op.id;
  if phase="sent" then Unix._exit 77;
  ignore (Md.append maildir ~id:local_id
    ~source:(Eio.Flow.string_source message) ~length ~flags:[] ());
  if phase="maildir" then Unix._exit 77;
  J.observe_operation store ~id:op.id
    ~receipt:("maildir:" ^ local_id)
    ~destination_uidvalidity:None ~destination_uid:None;
  Unix._exit 77

let local_delete_crash_child dir =
  Eio_main.run @@ fun env ->
  Eio.Switch.run @@ fun sw ->
  let fs=Eio.Stdenv.fs env in
  let database=Eio.Path.(fs / dir / "sync.db") in
  let blob_dir=Eio.Path.(fs / dir / "blob") in
  let maildir=Md.open_dir Eio.Path.(fs / dir / "maildir") in
  let store=open_store ~sw ~database ~blob_dir in
  let spool_dir=Eio.Path.(fs / dir / "spool") in
  let client=scripted_scan ~sw ~has_message:false () in
  let published=match Imap_sync.Engine.scan_once
    ~ctx:(context ~store ~spool_dir client)
    ~stage_id:"delete-crash-initial" () with
    | Ok receipt -> receipt
    | Error error -> Alcotest.failf "initial scan: %a"
        Imap_sync.Error.pp error in
  let cursor=published.cursor in
  let local=Md.append maildir
    ~source:(Eio.Flow.string_source message) ~length ~flags:[] () in
  let blob=Imap_store.Blob.put store
    ~source:(Eio.Flow.string_source message) ~length () in
  let pair : J.pair = {
    id="delete-crash-pair";scope;remote_uidvalidity=Some (epoch 11L);
    remote_uid=Some (uid 1L);local_id=Some local.id;
    content_sha256=Some blob.sha256;content_length=Some length;
    internal_date=None;
    common_flags=[];
    remote_tombstone=Some {J.reason=J.Inventory_absence;
      evidence=Option.get cursor.inventory_ref;
      generation=Some cursor.generation};
    local_tombstone=None;revision=0L} in
  (match J.put_pair store ~expected_revision:None pair with
   | `Committed _ -> ()
   | `Stale_revision -> Alcotest.fail "new pair was stale");
  let operation : J.operation = {
    id="delete-crash-op";pair_id=Some pair.id;local_id=Some local.id;
    scope;kind=J.Local_delete;state=J.Prepared;
    source_uidvalidity=Some (epoch 11L);source_uid=Some (uid 1L);
    destination=None;destination_uidvalidity=None;
    blob_sha256=Some blob.sha256;blob_length=Some length;
    desired_flags=Some [];
    internal_date=None;append=None;
    receipt=None;receipt_uidvalidity=None;
    receipt_uid=None} in
  J.prepare_operation store operation;
  J.mark_sent store ~id:operation.id;
  Md.remove maildir local;
  Unix._exit 77

let test_local_delete_process_crash () =
  let dir=root () in
  Fun.protect ~finally:(fun () -> remove_tree dir) @@ fun () ->
  let executable=if Filename.is_relative Sys.executable_name then
    Filename.concat (Sys.getcwd ()) Sys.executable_name
    else Sys.executable_name in
  let pid=Unix.create_process executable
    [|executable;"--local-delete-crash-child";dir|]
    Unix.stdin Unix.stdout Unix.stderr in
  let _,status=Unix.waitpid [] pid in
  Alcotest.(check bool) "child exited after Maildir unlink" true
    (status=Unix.WEXITED 77);
  Eio_main.run @@ fun env ->
  Eio.Switch.run @@ fun sw ->
  let fs=Eio.Stdenv.fs env in
  let database=Eio.Path.(fs / dir / "sync.db") in
  let blob_dir=Eio.Path.(fs / dir / "blob") in
  let spool_dir=Eio.Path.(fs / dir / "spool") in
  let maildir=Md.open_dir Eio.Path.(fs / dir / "maildir") in
  let store=open_store ~sw ~database ~blob_dir in
  Alcotest.(check int) "Maildir occurrence removed before crash" 0
    (List.length (Md.scan maildir));
  Alcotest.(check bool) "delete journal remained Sent" true
    (match J.find_operation store ~id:"delete-crash-op" with
     | Some {state=J.Sent;_} -> true | _ -> false);
  let client=scripted_scan ~sw ~has_message:false () in
  (match run_bridge ~client ~store ~maildir ~spool_dir with
   | Ok receipt ->
       Alcotest.(check int) "no local copy replayed" 0
         receipt.remote_to_local
   | Error error -> Alcotest.failf "local delete recovery: %a"
       Imap_sync.Error.pp error);
  Alcotest.(check bool) "delete journal committed after restart" true
    (match J.find_operation store ~id:"delete-crash-op" with
     | Some {state=J.Committed;_} -> true | _ -> false);
  Alcotest.(check bool) "local tombstone committed" true
    (match J.find_pair store ~id:"delete-crash-pair" with
     | Some {local_tombstone=Some {reason=J.Explicit_delete;_};_} -> true
     | _ -> false)

let test_real_process_crash phase () =
  let dir=root () in
  Fun.protect ~finally:(fun () -> remove_tree dir) @@ fun () ->
  let executable=if Filename.is_relative Sys.executable_name then
    Filename.concat (Sys.getcwd ()) Sys.executable_name
    else Sys.executable_name in
  let pid=Unix.create_process executable
    [|executable;"--crash-child";dir;phase|]
    Unix.stdin Unix.stdout Unix.stderr in
  let _,status=Unix.waitpid [] pid in
  Alcotest.(check bool) "child terminated without cleanup" true
    (status=Unix.WEXITED 77);
  Eio_main.run @@ fun env ->
  Eio.Switch.run @@ fun sw ->
  let fs=Eio.Stdenv.fs env in
  let database=Eio.Path.(fs / dir / "sync.db") in
  let blob_dir=Eio.Path.(fs / dir / "blob") in
  let spool_dir=Eio.Path.(fs / dir / "spool") in
  let maildir=Md.open_dir Eio.Path.(fs / dir / "maildir") in
  let store=open_store ~sw ~database ~blob_dir in
  let id="killed-" ^ phase in
  let expected_state=match phase with
    | "prepared" -> J.Prepared | "observed" -> J.Observed
    | "sent" | "maildir" -> J.Sent | _ -> assert false in
  Alcotest.(check bool) "journal state survived process exit" true
    (match J.find_operation store ~id with
     | Some {state;_} -> state=expected_state | None -> false);
  let expected_occurrences=if phase="prepared" || phase="sent" then 0
    else 1 in
  Alcotest.(check int) "physical occurrence after process exit"
    expected_occurrences
    (List.length (Md.scan maildir));
  let recovery_date=match Md.scan maildir with
    | [local] -> local_date local | _ -> None in
  let client=scripted_scan ~sw ?recovery_date ~has_message:(phase<>"prepared") () in
  (match run_bridge ~client ~store ~maildir ~spool_dir with
   | Error (Imap_sync.Error.Pending_operations [pending_id])
       when phase="sent" && pending_id=id -> ()
   | Ok receipt when phase<>"sent" ->
       Alcotest.(check int) "no second local write after restart" 0
         receipt.remote_to_local
   | Error error -> Alcotest.failf "process-crash recovery: %a"
       Imap_sync.Error.pp error
   | Ok _ -> Alcotest.fail "sent without write was incorrectly retried");
  let expected_terminal=match phase with
    | "prepared" -> J.Rejected
    | "sent" -> J.Sent
    | "maildir" | "observed" -> J.Committed
    | _ -> assert false in
  Alcotest.(check bool) "journal resolved conservatively" true
    (match J.find_operation store ~id with
     | Some {state;_} -> state=expected_terminal | None -> false);
  Alcotest.(check int) "recovered pair count"
    (if phase="maildir" || phase="observed" then 1 else 0)
    (List.length (all_pairs store ~scope))

let test_epoch_reset_preserves_published_snapshot () =
  with_fixture @@ fun ~database ~blob_dir ~spool_dir ~maildir ->
  Eio.Switch.run @@ fun sw ->
  let store=open_store ~sw ~database ~blob_dir in
  let initial=scripted_scan ~sw ~has_message:true () in
  let published=match Imap_sync.Engine.scan_once
      ~ctx:(context ~store ~spool_dir initial) ~stage_id:"old-epoch" () with
    | Ok receipt -> receipt
    | Error error -> Alcotest.failf "initial scan: %a"
        Imap_sync.Error.pp error in
  let pair : J.pair = {
    id="old-epoch-pair";scope;remote_uidvalidity=Some (epoch 11L);
    remote_uid=Some (uid 1L);local_id=Some "old-local-id";
    content_sha256=None;content_length=None;internal_date=None;
    common_flags=[];
    remote_tombstone=None;local_tombstone=None;revision=0L} in
  (match J.put_pair store ~expected_revision:None pair with
   | `Committed _ -> ()
   | `Stale_revision -> Alcotest.fail "new pair was stale");
  let reset=scripted_scan ~sw ~uidvalidity:12L ~has_message:false () in
  let next_id ()="unexpected-transfer" in
  (match Imap_sync.Bridge.copy_once
      ~ctx:(context ~store ~spool_dir ~next_id reset) ~maildir
      ~stage_id:"new-epoch" () with
   | Error Imap_sync.Error.Uidvalidity_changed -> ()
   | Error error -> Alcotest.failf "wrong reset error: %a"
       Imap_sync.Error.pp error
   | Ok _ -> Alcotest.fail "reset was accepted");
  let current=Imap_store.load_cursor store ~scope in
  Alcotest.(check int64) "old generation remains published"
    published.cursor.generation current.generation;
  Alcotest.(check bool) "old UID remains in published view" true
    (match Imap_store.snapshot_contains_uid store ~scope ~cursor:current
       ~uid:(uid 1L) with
     | `Present true -> true | _ -> false)

let test_epoch_reset_preserves_pending_journal_view () =
  with_fixture @@ fun ~database ~blob_dir ~spool_dir ~maildir ->
  Eio.Switch.run @@ fun sw ->
  let store=open_store ~sw ~database ~blob_dir in
  let initial=scripted_scan ~sw ~has_message:true () in
  let published=match Imap_sync.Engine.scan_once
      ~ctx:(context ~store ~spool_dir initial)
      ~stage_id:"pending-old-epoch" () with

    | Ok receipt -> receipt
    | Error error -> Alcotest.failf "initial scan: %a"
        Imap_sync.Error.pp error in
  let blob=Imap_store.Blob.put store
    ~source:(Eio.Flow.string_source message) ~length () in
  let op=operation ~kind:J.Append ~id:"epoch-pending-append"
    ~local_id:"pending-local-id" ~source_uid:None ~blob ~flags:[] in
  J.prepare_operation store op;
  J.mark_sent store ~id:op.id;
  let reset=scripted_scan ~sw ~uidvalidity:12L ~has_message:false () in
  let next_id ()="unexpected-transfer" in
  (match Imap_sync.Bridge.copy_once
      ~ctx:(context ~store ~spool_dir ~next_id reset) ~maildir
      ~stage_id:"pending-new-epoch" () with

   | Error Imap_sync.Error.Uidvalidity_changed -> ()
   | Error error -> Alcotest.failf "wrong reset error: %a"
       Imap_sync.Error.pp error
   | Ok _ -> Alcotest.fail "reset was accepted");
  let current=Imap_store.load_cursor store ~scope in
  Alcotest.(check int64) "old generation remains published"
    published.cursor.generation current.generation;
  Alcotest.(check bool) "old UID remains in published view" true
    (match Imap_store.snapshot_contains_uid store ~scope ~cursor:current
       ~uid:(uid 1L) with
     | `Present true -> true | _ -> false);
  Alcotest.(check bool) "pending operation remains actionable" true
    (match J.find_operation store ~id:op.id with
     | Some {state=J.Sent;_} -> true | _ -> false)

let () = if Array.length Sys.argv=4 && Sys.argv.(1)="--crash-child" then
  crash_child Sys.argv.(2) Sys.argv.(3)
else if Array.length Sys.argv=3 &&
  Sys.argv.(1)="--local-delete-crash-child" then
  local_delete_crash_child Sys.argv.(2)
else Alcotest.run "imap-bridge-faults" [
  "restart", [
    Alcotest.test_case "OBJECTID+ binding guards reconnect" `Quick
      test_objectid_binding_guards_reconnect;
    Alcotest.test_case "OBJECTID+ first binding needs stable epoch" `Quick
      test_objectid_first_binding_requires_stable_epoch;
    Alcotest.test_case "OBJECTID+ missing SELECT identity blocks publication"
      `Quick test_objectid_missing_select_identity_cannot_publish;
    Alcotest.test_case "FLAGS settlement rejects replaced OBJECTID+" `Quick
      test_flag_settlement_rejects_replaced_objectid;
    Alcotest.test_case "standalone repairs reject replaced OBJECTID+" `Quick
      test_standalone_repairs_reject_replaced_objectid;
    Alcotest.test_case "candidate inspection rejects replaced OBJECTID+"
      `Quick test_candidate_inspection_rejects_replaced_objectid;
    Alcotest.test_case "staged CONDSTORE sends CHANGEDSINCE" `Quick
      test_staged_condstore_wire;
    Alcotest.test_case "staged MESSAGELIMIT continues FETCH and SEARCH" `Quick
      test_staged_messagelimit_continuation;
    Alcotest.test_case "staged MESSAGELIMIT continues CHANGEDSINCE" `Quick
      test_staged_changedsince_messagelimit;
    Alcotest.test_case "staged timeout discards provisional rows" `Quick
      test_staged_timeout_discards_stage;
    Alcotest.test_case "watch connection and scan deadlines" `Quick
      test_watch_deadlines;
    Alcotest.test_case "watch keepalive does not rescan" `Quick
      test_watch_keepalive_does_not_rescan;
    Alcotest.test_case "watch IDLE renewal limit" `Quick
      test_watch_rejects_long_idle_renewal;
    Alcotest.test_case "metadata lock is not the writer lease" `Quick
      test_metadata_lock_is_not_writer_lease;
    Alcotest.test_case "Maildir errors are typed" `Quick
      test_maildir_error_is_typed;
    Alcotest.test_case "startup recovery of Maildir and spool" `Quick
      test_recover_local;
    Alcotest.test_case "hydration skips oversized message" `Quick
      test_hydration_skips_oversized_message;
    Alcotest.test_case "hydration skips message above total budget" `Quick
      test_hydration_skips_message_above_total_budget;
    Alcotest.test_case "hydration keeps counts after concurrent publish"
      `Quick test_hydration_keeps_counts_after_concurrent_publish;
    Alcotest.test_case "audit skips blob above budget" `Quick
      test_audit_skips_blob_above_budget;
    Alcotest.test_case "digest checks receipt epoch" `Quick
      test_digest_checks_receipt_epoch;
    Alcotest.test_case "rejected STORE rejects the operation" `Quick
      test_rejected_store_rejects_operation;
    Alcotest.test_case "uncertain STORE leaves a conflict" `Quick
      test_uncertain_store_leaves_conflict;
    Alcotest.test_case "failed verification read is pending" `Quick
      test_failed_verification_read_is_pending;
    Alcotest.test_case "local-only race rejects before dispatch" `Quick
      test_local_only_race_rejects_prepared;
    Alcotest.test_case "held Deleted merges other flags" `Quick
      test_held_deleted_merges_other_flags;
    Alcotest.test_case "settle reports content mismatch" `Quick
      test_settle_reports_content_mismatch;
    Alcotest.test_case "MODIFIED delete is held" `Quick
      test_modified_delete_is_held;
    Alcotest.test_case "uncertain conditional STORE is pending" `Quick
      (test_uncertain_delete_dispatch_is_pending ~expunge:false);
    Alcotest.test_case "uncertain UID EXPUNGE is pending" `Quick
      (test_uncertain_delete_dispatch_is_pending ~expunge:true);
    Alcotest.test_case "pair of another scope is stale" `Quick
      test_foreign_scope_pair_is_stale;
    Alcotest.test_case "remote delete without spool is configuration" `Quick
      test_remote_delete_missing_spool_is_configuration;
    Alcotest.test_case "longer remote body is held" `Quick
      test_longer_remote_body_is_held;
    Alcotest.test_case "expunge during body fetch is stale" `Quick
      test_expunged_during_body_fetch_is_stale;
    Alcotest.test_case "legacy pair holds without content evidence" `Quick
      test_legacy_pair_holds_without_evidence;
    Alcotest.test_case "changed local survivor is held" `Quick
      test_changed_local_survivor_is_held;
    Alcotest.test_case "content mismatch is a bridge hold" `Quick
      test_content_mismatch_is_bridge_hold;
    Alcotest.test_case "missing CONDSTORE is a bridge hold" `Quick
      test_unconditional_store_is_bridge_hold;
    Alcotest.test_case "changed Deleted is held by default" `Quick
      (test_deleted_flag_propagation ~propagate:false);
    Alcotest.test_case "propagate_deleted merges Deleted" `Quick
      (test_deleted_flag_propagation ~propagate:true);
    Alcotest.test_case "invalid configuration before the lease" `Quick
      test_invalid_configuration_before_lease;
    Alcotest.test_case "tombstoned present pair is a hold" `Quick
      test_tombstoned_pair_present_is_hold;
    Alcotest.test_case "unstorable remote copy is rejected" `Quick
      test_unstorable_remote_copy_is_rejected;
    Alcotest.test_case "remote source vanishes before archival" `Quick
      test_remote_source_vanishes_before_archive;
    Alcotest.test_case "local source changes before archival" `Quick
      test_local_occurrence_changes_before_archive;
    Alcotest.test_case "corrupt blob rejects unsent APPEND" `Quick
      test_invalid_blob_rejects_unsent_append;
    Alcotest.test_case "missing APPENDUID keeps reason" `Quick
      test_missing_appenduid_keeps_reason;
    Alcotest.test_case "APPENDUID readback rejects changed body" `Quick
      test_appenduid_readback_rejects_changed_body;
    Alcotest.test_case "uncertain APPEND retains date" `Quick
      test_append_date_survives_uncertain_reply;
    Alcotest.test_case "APPEND requires a prepared operation" `Quick
      test_append_requires_prepared_operation;
    Alcotest.test_case "prepared APPEND rejected on restart" `Quick
      test_prepared_append_restarts;
    Alcotest.test_case "sent APPEND held on restart" `Quick
      test_sent_append_survives_restart;
    Alcotest.test_case "UIDVALIDITY reset preserves old snapshot" `Quick
      test_epoch_reset_preserves_published_snapshot;
    Alcotest.test_case "UIDVALIDITY reset preserves pending journal view" `Quick
      test_epoch_reset_preserves_pending_journal_view;
    Alcotest.test_case "ambiguous APPEND held" `Quick
      test_ambiguous_append_survives_restart;
    Alcotest.test_case "reserved local write recovered once" `Quick
      (test_local_write_recovery ~divergent:false);
    Alcotest.test_case "divergent local bytes block commit" `Quick
      (test_local_write_recovery ~divergent:true);
    Alcotest.test_case "missing local date blocks commit" `Quick
      (test_local_write_recovery ~divergent:false ~missing_date:true);
    Alcotest.test_case "altered local date blocks commit" `Quick
      (test_local_write_recovery ~divergent:false ~wrong_date:true);
    Alcotest.test_case "confirmed UIDPLUS recovered" `Quick
      test_confirmed_uidplus_recovery;
    Alcotest.test_case "changed mtime holds UIDPLUS recovery" `Quick
      (test_confirmed_uidplus_recovery ~changed_mtime:true);
    Alcotest.test_case "missing APPENDUID target is named" `Quick
      (test_confirmed_uidplus_recovery ~missing_target:true);
    Alcotest.test_case "changed UIDPLUS body leaves cache untouched" `Quick
      (test_confirmed_uidplus_recovery ~changed_remote_body:true);
    Alcotest.test_case "APPEND without saved mtime stays pending" `Quick
      (test_confirmed_uidplus_recovery ~missing_preimage:true);
    Alcotest.test_case "writer lease blocks bridge" `Quick
      test_writer_lease_blocks_bridge;
    Alcotest.test_case "prepared copies rejected before dispatch" `Quick
      test_prepared_copies_rejected_without_send;
    Alcotest.test_case "operator APPENDUID evidence verified" `Quick
      test_operator_appenduid_evidence;
    Alcotest.test_case "uncertain DELETE is pending" `Quick
      test_unresolved_delete_reports_pending;
    Alcotest.test_case "unsupported targeted DELETE is held" `Quick
      test_unsupported_targeted_delete_is_durable_hold;
    Alcotest.test_case "deletion grace waits for another complete scan" `Quick
      test_deletion_grace_across_complete_scans;
    Alcotest.test_case "changed reappearance blocks deletion" `Quick
      test_changed_reappearance_blocks_delete;
    Alcotest.test_case "wrong-date reappearance blocks deletion" `Quick
      test_wrong_date_reappearance_blocks_delete;
    Alcotest.test_case "retention holds remote DELETE" `Quick
      test_retention_holds_remote_delete;
    Alcotest.test_case "read-only sync plan" `Quick
      test_readonly_sync_plan;
    Alcotest.test_case "preview of Deleted propagation" `Quick
      test_preview_deleted_flag_propagation;
    Alcotest.test_case "unknown repair operation is not pending" `Quick
      test_unknown_operation_is_not_pending;
    Alcotest.test_case "remote repairs without spool are configuration"
      `Quick test_remote_repairs_missing_spool_is_configuration;
    Alcotest.test_case "engine budgets are configuration" `Quick
      test_engine_budgets_are_configuration;
    Alcotest.test_case "incompatible absence tombstone holds" `Quick
      test_incompatible_absence_tombstone_holds;
    Alcotest.test_case "stale pair cannot wedge Prepared FLAGS" `Quick
      test_prepared_flags_with_stale_pair;
    Alcotest.test_case "FLAGS rejects replaced local body" `Quick
      test_flag_write_rejects_replaced_local_body;
    Alcotest.test_case "sent FLAGS holds replaced local body" `Quick
      test_sent_flags_recovery_holds_replaced_local_body;
    Alcotest.test_case "verify silent local body change" `Quick
      test_verify_local_content_without_flag_change;
    Alcotest.test_case "process exit after PREPARED" `Quick
      (test_real_process_crash "prepared");
    Alcotest.test_case "process exit after SENT" `Quick
      (test_real_process_crash "sent");
    Alcotest.test_case "process exit after fsynced Maildir write" `Quick
      (test_real_process_crash "maildir");
    Alcotest.test_case "process exit after OBSERVED" `Quick
      (test_real_process_crash "observed");
    Alcotest.test_case "process exit after Maildir delete" `Quick
      test_local_delete_process_crash;
  ];
]
