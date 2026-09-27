(* Directed checks for typed store outcomes: scope mismatches, stale
   cursors and the errors each public entry point reports. *)
module M = Imap.Mirror
module Store = Imap_store

let orphan_candidates db =
  let names=ref [] in
  Imap_store.Blob.iter_orphan_candidates db (fun name ->
    names := name :: !names);
  List.sort String.compare !names

let ok = function Ok x -> x | Error _ -> failwith "unexpected error"
let check condition message = if not condition then failwith message
let uid n = ok (Imap.Uid.of_int64 n)
let epoch n = ok (Imap.Uidvalidity.of_int64 n)
let modseq n = ok (Imap.Modseq.of_int64 n)
let scope : M.scope = {
  endpoint="imap.example"; account="alice"; mailbox_key="inbox";
  raw_name="INBOX"; encoding=Imap.Mailbox_name.Rev1; mailbox_id=None }
let renamed = { scope with raw_name="Renamed" }
let row n : M.row = { uid=uid n; flags=[]; modseq=Some (modseq 17L) }

(* [publish db ~stage ~epoch_value rows] stages [rows] from the stored
   cursor as one FETCH and one SEARCH window and publishes them. *)
let publish db ~stage ~epoch_value rows =
  let cursor=Store.load_cursor db ~scope in
  let selected : M.selected = {
    uidvalidity=epoch epoch_value; uidnext=5L;
    highestmodseq=Some (modseq 17L); nomodseq=false } in
  let action=ok (M.plan cursor ~stage_id:stage selected) in
  Store.begin_stage db ~cursor ~action;
  let last=action.upper_uid in
  if last>0L then (
    Store.stage_rows db ~stage_id:stage ~first:1L ~last rows;
    Store.stage_membership db ~stage_id:stage ~first:1L ~last
      (List.map (fun (r:M.row) -> r.uid) rows));
  check (match Store.publish_stage db ~cursor ~action
      ~explicit_highestmodseq:(Some (modseq 17L)) ~nomodseq:false with
    | `Committed _ -> true | `Stale_revision -> false) ("publish " ^ stage)

(* [as_scope scope cursor] is [cursor] with the same counters under
   another scope, as a caller holding an outdated scope would have. *)
let as_scope other (c:M.cursor) =
  ok (M.restore ~schema_version:c.schema_version ~scope:other ~phase:c.phase
    ~uidvalidity:c.uidvalidity ~generation:c.generation ~revision:c.revision
    ~anchor:c.anchor ~frontier:c.frontier ~inventory_ref:c.inventory_ref
    ~mode:c.mode)

let contains ~needle haystack =
  let n=String.length needle in
  let rec from k = k+n<=String.length haystack &&
    (String.sub haystack k n=needle || from (k+1)) in
  from 0

let with_store env f =
  let path=Filename.temp_file "imap-store-outcomes-" ".db" in
  let dir=Filename.temp_file "imap-store-outcomes-blobs-" "" in
  Sys.remove dir;
  Unix.mkdir dir 0o700;
  Fun.protect ~finally:(fun () ->
    if Sys.file_exists dir then (
      Array.iter (fun name -> Sys.remove (Filename.concat dir name))
        (Sys.readdir dir);
      Unix.rmdir dir);
    List.iter (fun p -> try Sys.remove p with Sys_error _ -> ())
      [path;path^"-wal";path^"-shm"])
    (fun () -> Eio.Switch.run (fun sw ->
      let fs=Eio.Stdenv.fs env in
      f ~path ~dir (Store.open_path ~sw ~blob_dir:Eio.Path.(fs / dir)
        Eio.Path.(fs / path))))

let mutate path sql =
  let db=Sqlite3.db_open path in
  Fun.protect ~finally:(fun () -> ignore (Sqlite3.db_close db))
    (fun () -> Sqlite3.Rc.check (Sqlite3.exec db sql))

let decode_reasons env = with_store env (fun ~path ~dir:_ db ->
  publish db ~stage:"first" ~epoch_value:5L [row 1L];
  let expect_reason needle =
    match Store.load_cursor db ~scope with
    | exception Failure message when contains ~needle message -> ()
    | exception Failure message -> failwith ("reason lost: " ^ message)
    | _ -> failwith "corrupt cursor accepted" in
  mutate path "UPDATE mailboxes SET generation=generation+1";
  expect_reason "generation/revision mismatch";
  mutate path "UPDATE mailboxes SET generation=revision, encoding='bogus'";
  expect_reason "\"bogus\"")

let scope_mismatch env = with_store env (fun ~path:_ ~dir:_ db ->
  publish db ~stage:"first" ~epoch_value:5L [row 1L; row 2L];
  (match Store.load_cursor db ~scope:renamed with
   | exception Store.Scope_mismatch -> ()
   | _ -> failwith "load_cursor accepted a renamed scope");
  let cursor=as_scope renamed (Store.load_cursor db ~scope) in
  check (Store.snapshot_page db ~scope:renamed ~cursor ~limit:10 ()
    =`Stale_revision) "snapshot_page on renamed scope";
  check (Store.snapshot_contains_uid db ~scope:renamed ~cursor ~uid:(uid 1L)
    =`Stale_revision) "snapshot_contains_uid on renamed scope";
  check (Store.Blob.missing_page db ~scope:renamed ~cursor ~limit:10 ()
    =`Stale_revision) "missing_page on renamed scope";
  check (Store.Blob.referenced_page db ~scope:renamed ~cursor ~limit:10 ()
    =`Stale_revision) "referenced_page on renamed scope";
  let content="body" in
  let blob=Store.Blob.put db ~source:(Eio.Flow.string_source content)
    ~length:4L () in
  check (Store.Blob.detach_if_matches db ~scope:renamed ~cursor
    ~uid:(uid 1L) blob=`Stale_revision) "detach_if_matches on renamed scope")

let put db content =
  Store.Blob.put db ~source:(Eio.Flow.string_source content)
    ~length:(Int64.of_int (String.length content)) ()

let count path sql =
  let db=Sqlite3.db_open ~mode:`READONLY path in
  Fun.protect ~finally:(fun () -> ignore (Sqlite3.db_close db)) (fun () ->
    let n=ref (-1) in
    Sqlite3.Rc.check (Sqlite3.exec_not_null_no_headers db
      ~cb:(fun row -> n := int_of_string row.(0)) sql);
    !n)

let forget_epochs env = with_store env (fun ~path ~dir:_ db ->
  publish db ~stage:"old" ~epoch_value:5L [row 1L];
  let blob=put db "old epoch body" in
  Store.Blob.attach db ~scope ~uidvalidity:(epoch 5L) ~uid:(uid 1L) blob;
  let old_cursor=Store.load_cursor db ~scope in
  publish db ~stage:"reset" ~epoch_value:6L [row 1L];
  let name="sha256-" ^ blob.sha256 in
  check (not (List.mem name (orphan_candidates db)))
    "retained epoch reference not a GC root";
  check (Store.forget_epochs db ~scope ~cursor:old_cursor=`Stale_revision)
    "stale cursor dropped epochs";
  check (count path "SELECT count(*) FROM blob_refs"=1)
    "stale forget changed references";
  let cursor=Store.load_cursor db ~scope in
  check (Store.forget_epochs db ~scope ~cursor=`Dropped 1)
    "old epoch not dropped";
  check (count path "SELECT count(*) FROM snapshots WHERE uidvalidity=5"=0)
    "old snapshot rows kept";
  check (count path "SELECT count(*) FROM snapshots WHERE uidvalidity=6"=1)
    "current snapshot rows dropped";
  check (List.mem name (orphan_candidates db))
    "dropped epoch still roots its blob";
  check (Store.forget_epochs db ~scope ~cursor=`Dropped 0)
    "second forget found epochs")

let attach_without_verify env = with_store env (fun ~path:_ ~dir db ->
  publish db ~stage:"attach" ~epoch_value:5L [row 1L; row 2L];
  let blob=put db "attached body" in
  Sys.remove (Filename.concat dir ("sha256-" ^ blob.sha256));
  (match Store.Blob.attach db ~scope ~uidvalidity:(epoch 5L) ~uid:(uid 1L)
     blob with
   | exception Invalid_argument _ -> ()
   | () -> failwith "missing blob attached with verification");
  Store.Blob.attach ~verify:false db ~scope ~uidvalidity:(epoch 5L)
    ~uid:(uid 2L) blob;
  check (Store.Blob.find db ~scope ~uidvalidity:(epoch 5L) ~uid:(uid 2L)
    =Some blob) "unverified attach lost";
  (match Store.Blob.put db ~source:(Eio.Flow.string_source "body")
     ~length:4L ~expected_sha256:(String.make 64 '0') () with
   | exception Store.Blob.Digest_mismatch -> ()
   | _ -> failwith "wrong digest accepted");
  check (Array.for_all (fun name ->
    not (String.starts_with ~prefix:".tmp-" name)) (Sys.readdir dir))
    "digest mismatch left a temporary file")

(* Removing the blob directory inside the callback makes the directory
   sync in the finaliser fail. The callback's exception must survive. *)
let finaliser_keeps_exception env = with_store env (fun ~path:_ ~dir db ->
  ignore (put db "orphan one");
  ignore (put db "orphan two");
  (match Store.Blob.reap_orphans_iter db ~removed:(fun _ ->
     Array.iter (fun name -> Sys.remove (Filename.concat dir name))
       (Sys.readdir dir);
     Unix.rmdir dir;
     raise Exit) with
   | exception Exit -> ()
   | exception other ->
       failwith ("finaliser replaced exception: " ^
         Printexc.to_string other)
   | () -> failwith "callback exception lost");
  match orphan_candidates db with
  | exception Eio.Io _ -> ()
  | exception other ->
      failwith ("directory failure not Eio.Io: " ^ Printexc.to_string other)
  | _ -> failwith "missing blob directory listed")

let selected ?(highestmodseq=Some (modseq 40L)) epoch_value : M.selected = {
  uidvalidity=epoch epoch_value; uidnext=5L; highestmodseq; nomodseq=false }

(* A cursor at the current revision but an older epoch must not seed rows
   from the quarantined epoch. *)
let seed_checks_epoch env = with_store env (fun ~path:_ ~dir:_ db ->
  publish db ~stage:"five" ~epoch_value:5L [row 1L];
  let old_epoch=Store.load_cursor db ~scope in
  publish db ~stage:"six" ~epoch_value:6L [row 1L];
  let current=Store.load_cursor db ~scope in
  let cursor=ok (M.restore ~schema_version:current.schema_version ~scope
    ~phase:current.phase ~uidvalidity:old_epoch.uidvalidity
    ~generation:current.generation ~revision:current.revision
    ~anchor:current.anchor ~frontier:current.frontier
    ~inventory_ref:current.inventory_ref ~mode:current.mode) in
  let action=ok (M.plan cursor ~stage_id:"old-epoch-seed" (selected 5L)) in
  Store.begin_stage db ~cursor ~action;
  check (Store.seed_stage_from_published db ~cursor ~action=`Stale_revision)
    "seeded from a quarantined epoch")

let identity_conflict env = with_store env (fun ~path:_ ~dir:_ db ->
  let identity:Store.object_identity={account_id="u_a";mailbox_id="F_b"} in
  check (Store.observe_object_identity db ~scope identity=`Bound) "bind";
  check (Store.object_identity db ~scope=`Bound identity) "bound identity";
  check (Store.object_identity db ~scope:renamed=`Conflict)
    "renamed binding not a conflict")

let seeded_modseq_message env = with_store env (fun ~path:_ ~dir:_ db ->
  publish db ~stage:"plain" ~epoch_value:5L
    [{uid=uid 1L; flags=[]; modseq=None}];
  let cursor=Store.load_cursor db ~scope in
  let action=ok (M.plan cursor ~stage_id:"seeded-plain" (selected 5L)) in
  Store.begin_stage db ~cursor ~action;
  check (Store.seed_stage_from_published db ~cursor ~action=`Seeded) "seed";
  match Store.stage_rows ~preserve_newer:true db ~stage_id:action.id
      ~first:1L ~last:4L [{uid=uid 1L; flags=[]; modseq=None}] with
  | exception Invalid_argument message when contains ~needle:"seeded" message
    -> ()
  | exception Invalid_argument message ->
      failwith ("seeded row not named: " ^ message)
  | () -> failwith "rows without MODSEQ compared")

let anchor_requires_explicit env = with_store env (fun ~path:_ ~dir:_ db ->
  let cursor=Store.load_cursor db ~scope in
  let action=ok (M.plan cursor ~stage_id:"anchor" (selected 5L)) in
  check (action.mode=M.Condstore) "fixture is not CONDSTORE";
  Store.begin_stage db ~cursor ~action;
  let staged n m : M.row = {uid=uid n; flags=[]; modseq=Some (modseq m)} in
  Store.stage_rows db ~stage_id:action.id ~first:1L ~last:4L
    [staged 1L 20L; staged 2L 30L];
  Store.stage_membership db ~stage_id:action.id ~first:1L ~last:4L
    [uid 1L;uid 2L];
  match Store.publish_stage db ~cursor ~action ~explicit_highestmodseq:None
      ~nomodseq:false with
  | `Committed receipt ->
      check (receipt.row_count=2L) "row count";
      check (receipt.cursor.anchor=None)
        "anchor set without an explicit HIGHESTMODSEQ"
  | `Stale_revision -> failwith "fresh stage stale")

let text path sql =
  let db=Sqlite3.db_open ~mode:`READONLY path in
  Fun.protect ~finally:(fun () -> ignore (Sqlite3.db_close db)) (fun () ->
    let v=ref None in
    Sqlite3.Rc.check (Sqlite3.exec_no_headers db
      ~cb:(fun row -> v := Some row.(0)) sql);
    Option.join !v)

(* Snapshot and stage flags are one text column of wire spellings in the
   order FETCH gave them, carried through seeding and publication. *)
let flag_text env = with_store env (fun ~path ~dir:_ db ->
  let flag x = ok (Mail_flag.Imap_flag.of_wire x) in
  let flags=[flag "\\Seen"; flag "$Label"; flag "\\Flagged"] in
  publish db ~stage:"flags" ~epoch_value:5L
    [{(row 1L) with flags}; row 2L];
  check (text path "SELECT flags FROM snapshots WHERE uid=1"
    =Some "\\Seen $Label \\Flagged") "flag text";
  check (text path "SELECT flags FROM snapshots WHERE uid=2"=Some "")
    "empty flag text";
  let cursor=Store.load_cursor db ~scope in
  let action=ok (M.plan cursor ~stage_id:"reseed" (selected 5L)) in
  Store.begin_stage db ~cursor ~action;
  check (Store.seed_stage_from_published db ~cursor ~action=`Seeded) "seed";
  check (text path "SELECT flags FROM scan_rows WHERE uid=1"
    =Some "\\Seen $Label \\Flagged") "seeded flag text";
  Store.discard_stage db ~stage_id:action.id;
  (match Store.snapshot_page db ~scope ~cursor ~limit:10 () with
   | `Rows [first;second] ->
       check (List.map Mail_flag.Imap_flag.to_wire first.flags
         =["\\Seen";"$Label";"\\Flagged"]) "flag order";
       check (second.flags=[]) "empty flags"
   | _ -> failwith "snapshot page");
  mutate path "UPDATE snapshots SET flags='\\Seen  x' WHERE uid=1";
  match Store.snapshot_page db ~scope ~cursor ~limit:10 () with
  | exception Failure _ -> ()
  | _ -> failwith "malformed flag text accepted")

let () =
  Eio_main.run (fun env ->
    seed_checks_epoch env;
    identity_conflict env;
    seeded_modseq_message env;
    anchor_requires_explicit env;
    scope_mismatch env;
    decode_reasons env;
    forget_epochs env;
    attach_without_verify env;
    finaliser_keeps_exception env;
    flag_text env)
