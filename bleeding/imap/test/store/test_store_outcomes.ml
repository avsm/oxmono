(* Directed checks for typed store outcomes: scope mismatches, stale
   cursors and the errors each public entry point reports. *)
module M = Imap.Mirror
module P = Imap.Proto
module Store = Imap_store

let ok = function Ok x -> x | Error _ -> failwith "unexpected error"
let check condition message = if not condition then failwith message
let uid n = ok (P.Uid.of_int64 n)
let epoch n = ok (P.Uidvalidity.of_int64 n)
let modseq n = ok (P.Modseq.of_int64 n)
let scope : M.scope = {
  endpoint="imap.example"; account="alice"; mailbox_key="inbox";
  raw_name="INBOX"; encoding=Imap.Mailbox_name.Rev1; mailbox_id=None }
let renamed = { scope with raw_name="Renamed" }
let row n : M.row = { uid=uid n; flags=[]; modseq=Some (modseq 17L) }

let transition cursor published ~stage ~epoch_value rows =
  let selected : M.selected = {
    uidvalidity=epoch epoch_value; uidnext=5L;
    highestmodseq=Some (modseq 17L); nomodseq=false } in
  let action=ok (M.plan cursor ~stage_id:stage selected) in
  let completed : M.completed = {
    action_id=action.id; uidvalidity=action.uidvalidity;
    covered_upper=action.upper_uid; inventory_complete=true;
    commands_complete=true; rows; explicit_highestmodseq=Some (modseq 17L);
    nomodseq=false } in
  let staged=ok (M.complete cursor action completed) in
  ok (M.publish cursor ~published staged)

let publish db ~stage ~epoch_value rows =
  let current=Store.load db ~scope in
  let next=transition current.cursor current.snapshot ~stage ~epoch_value
    rows in
  check (Store.publish db next=`Committed) ("publish " ^ stage)

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
    Array.iter (fun name -> Sys.remove (Filename.concat dir name))
      (Sys.readdir dir);
    Unix.rmdir dir;
    List.iter (fun p -> try Sys.remove p with Sys_error _ -> ())
      [path;path^"-wal";path^"-shm"])
    (fun () -> Eio.Switch.run (fun sw ->
      let fs=Eio.Stdenv.fs env in
      f ~path (Store.open_path ~sw ~blob_dir:Eio.Path.(fs / dir)
        Eio.Path.(fs / path))))

let mutate path sql =
  let db=Sqlite3.db_open path in
  Fun.protect ~finally:(fun () -> ignore (Sqlite3.db_close db))
    (fun () -> Sqlite3.Rc.check (Sqlite3.exec db sql))

let decode_reasons env = with_store env (fun ~path db ->
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

let scope_mismatch env = with_store env (fun ~path:_ db ->
  publish db ~stage:"first" ~epoch_value:5L [row 1L; row 2L];
  (match Store.load_cursor db ~scope:renamed with
   | exception Store.Scope_mismatch -> ()
   | _ -> failwith "load_cursor accepted a renamed scope");
  (match Store.load db ~scope:renamed with
   | exception Store.Scope_mismatch -> ()
   | _ -> failwith "load accepted a renamed scope");
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

let () =
  Eio_main.run (fun env ->
    scope_mismatch env;
    decode_reasons env)
