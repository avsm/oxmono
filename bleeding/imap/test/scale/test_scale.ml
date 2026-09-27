module M = Imap_maildir
module S = Imap_store
module P = Imap.Proto

let fail fmt = Printf.ksprintf (fun message -> Alcotest.fail message) fmt
let ok = function Ok x -> x | Error _ -> fail "invalid protocol value"
let uid n = ok (P.Uid.of_int64 n)
let epoch = ok (P.Uidvalidity.of_int64 1L)
let scope : Imap.Mirror.scope = {
  endpoint="scale.local"; account="test"; mailbox_key="inbox";
  raw_name="INBOX"; encoding=Imap.Mailbox_name.Rev1;
  mailbox_id=None;
}

let count_from_env name default =
  match Sys.getenv_opt name with
  | None -> default
  | Some s ->
    let n = try int_of_string s with _ -> fail "%s must be an integer" name in
    if n < 1 || n > 1_000_001 then fail "%s must be 1..1,000,001" name;
    n

let with_root f =
  let root = Filename.temp_file "imap-scale-" "" in
  Sys.remove root;
  Unix.mkdir root 0o700;
  let rec remove path =
    match (Unix.lstat path).Unix.st_kind with
    | Unix.S_DIR ->
      let dh = Unix.opendir path in
      Fun.protect ~finally:(fun () -> Unix.closedir dh) (fun () ->
        let rec walk () =
          match Unix.readdir dh with
          | name ->
            if name <> "." && name <> ".." then
              remove (Filename.concat path name);
            walk ()
          | exception End_of_file -> () in
        walk ());
      Unix.rmdir path
    | _ -> Unix.unlink path in
  Fun.protect ~finally:(fun () -> remove root) (fun () -> f root)

let id i = Printf.sprintf "im-%032x" i
let pair_id i = Printf.sprintf "pair-%08d" i
let body = "From: scale@example.test\r\nSubject: duplicate\r\n\r\nBody\r\n"

let external_maildir_message root i =
  let name = id i ^ (if i mod 7 = 0 then ":2,S" else "") in
  let directory = if i mod 7 = 0 then "cur" else "new" in
  let path = Filename.concat (Filename.concat root directory) name in
  let fd = Unix.openfile path [Unix.O_WRONLY; Unix.O_CREAT; Unix.O_EXCL] 0o600 in
  Fun.protect ~finally:(fun () -> Unix.close fd) (fun () ->
    let rec write offset =
      if offset < String.length body then
        let n = Unix.write_substring fd body offset
          (String.length body - offset) in
        if n = 0 then fail "short write building Maildir fixture";
        write (offset + n) in
    write 0)

let vm_hwm_kib () =
  try
    let ic = open_in "/proc/self/status" in
    Fun.protect ~finally:(fun () -> close_in ic) (fun () ->
      let rec find () =
        match input_line ic with
        | line when String.starts_with ~prefix:"VmHWM:" line ->
          (try Some (Scanf.sscanf line "VmHWM: %d kB" Fun.id)
           with _ -> None)
        | _ -> find ()
        | exception End_of_file -> None in
      find ())
  with Sys_error _ -> None

let page_maildir m count =
  M.with_inventory_pages m (fun view ->
    Alcotest.(check int64) "complete disk inventory"
      (Int64.of_int count) (M.inventory_count view);
    List.iter (fun i ->
      match M.inventory_find view ~id:(id i) with
      | Some occurrence ->
        if occurrence.id <> id i then fail "wrong indexed identity %d" i;
        if occurrence.length <> Int64.of_int (String.length body) then
          fail "wrong indexed body length %d" i
      | None -> fail "missing indexed occurrence %d" i)
      [0; count / 2; count - 1];
    let seen = ref 0 and after = ref None in
    let rec walk () =
      let page = M.inventory_page view ?after:!after ~limit:257 () in
      List.iter (fun (entry:M.occurrence) ->
        if entry.id <> id !seen then
          fail "Maildir page order mismatch at %d: %s" !seen entry.id;
        incr seen) page.occurrences;
      match page.next_after with
      | None -> ()
      | Some boundary ->
        if page.occurrences = [] then fail "empty page has continuation";
        if boundary <> id (!seen - 1) then
          fail "Maildir continuation mismatch after %d" !seen;
        after := Some boundary;
        walk () in
    walk ();
    Alcotest.(check int) "every external occurrence paged once" count !seen)

let prepare_journal db count =
  for i = 0 to count - 1 do
    let local_id = id i and pair_id = pair_id i in
    let pair : S.Sync.pair = {
      id=pair_id; scope; remote_uidvalidity=Some epoch;
      remote_uid=Some (uid (Int64.of_int (i + 1)));
      local_id=Some local_id; content_sha256=None; content_length=None;
      internal_date=None;
      common_flags=[]; remote_tombstone=None; local_tombstone=None;
      revision=0L;
    } in
    (match S.Sync.put_pair db ~expected_revision:None pair with
     | `Committed _ -> ()
     | `Stale_revision -> fail "fresh pair %d was stale" i);
    let operation : S.Sync.operation = {
      id=Printf.sprintf "operation-%08d" i; pair_id=Some pair_id;
      local_id=Some local_id; scope; kind=Flags; state=Prepared;
      source_uidvalidity=Some epoch;
      source_uid=Some (uid (Int64.of_int (i + 1)));
      destination=None; destination_uidvalidity=None;
      blob_sha256=None; blob_length=None; desired_flags=Some [];
      receipt=None; receipt_uidvalidity=None; receipt_uid=None;
    } in
    S.Sync.prepare_operation db operation;
    if i mod 13 = 0 then
      S.Sync.reject_operation db ~id:operation.id ~receipt:"fixture terminal"
  done

let page_journal db count =
  let seen = ref 0 and after = ref None in
  let rec pairs () =
    let page = S.Sync.pairs_page db ~scope ?after:!after ~limit:113 () in
    List.iter (fun (pair:S.Sync.pair) ->
      if pair.id <> pair_id !seen then
        fail "pair page order mismatch at %d: %s" !seen pair.id;
      incr seen) page;
    match List.rev page with
    | [] -> ()
    | last::_ -> after := Some last.id; pairs () in
  pairs ();
  Alcotest.(check int) "all pairs paged" count !seen;
  let seen = ref 0 and after = ref None in
  let expected = ref 0 in
  let rec operations () =
    let page = S.Sync.active_operations_page db ~scope ?after:!after
      ~limit:97 () in
    List.iter (fun (op:S.Sync.operation) ->
      while !expected < count && !expected mod 13 = 0 do
        incr expected
      done;
      if !expected >= count then fail "extra active operation";
      if op.id <> Printf.sprintf "operation-%08d" !expected then
        fail "active operation page mismatch at %d: %s" !expected op.id;
      incr expected;
      incr seen) page;
    match List.rev page with
    | [] -> ()
    | last::_ -> after := Some last.id; operations () in
  operations ();
  let rejected = (count + 12) / 13 in
  Alcotest.(check int) "all and only active operations paged"
    (count - rejected) !seen;
  let active = S.Sync.active_operation_for_pair db
    ~pair_id:(pair_id (count - 1)) in
  Alcotest.(check bool) "indexed pair operation"
    (count mod 13 <> 1) (Option.is_some active)

let test_scale env = with_root (fun root ->
  let count = count_from_env "IMAP_SCALE_COUNT" 100_001 in
  let journal_count = count_from_env "IMAP_SCALE_JOURNAL_COUNT"
    (min count 1_201) in
  let started = Unix.gettimeofday () in
  let initial_hwm = vm_hwm_kib () in
  let report phase =
    let hwm = match vm_hwm_kib () with
      | None -> "unavailable"
      | Some kib -> Printf.sprintf "%d KiB" kib in
    Printf.printf "%s: elapsed %.1fs, process VmHWM %s\n%!"
      phase (Unix.gettimeofday () -. started) hwm in
  let fs = Eio.Stdenv.fs env in
  let maildir = M.open_dir Eio.Path.(fs / root) in
  for i = 0 to count - 1 do external_maildir_message root i done;
  report "Maildir fixture created";
  let before = vm_hwm_kib () in
  page_maildir maildir count;
  report "Maildir inventory paged";
  (match before, vm_hwm_kib () with
   | Some before, Some after ->
     let delta = after - before in
     Printf.printf "Maildir inventory VmHWM delta: %d KiB for %d occurrences\n%!"
       delta count;
     if count >= 100_000 && delta > 256 * 1024 then
       fail "Maildir paging raised process high-water RSS by %d KiB" delta
   | _ -> Printf.printf "VmHWM unavailable; skipping Linux RSS bound\n%!");
  M.with_inventory_pages maildir (fun inventory ->
    for i=0 to 99 do
      ignore (M.append ~inventory maildir ~id:(id (count+i))
        ~source:(Eio.Flow.string_source body)
        ~length:(Int64.of_int (String.length body)) ~flags:[] ())
    done);
  report "100 indexed Maildir imports completed";
  if not (Sys.file_exists
      (Filename.concat (Filename.concat root "new") (id (count+99)))) then
    fail "indexed Maildir import was not published";
  let before_recovery=vm_hwm_kib () in
  ignore (M.recover maildir);
  report "Maildir startup recovery completed";
  (match before_recovery,vm_hwm_kib () with
   | Some before,Some after when count>=100_000 &&
       after-before>256*1024 ->
       fail "Maildir recovery raised process high-water RSS by %d KiB"
         (after-before)
   | _ -> ());
  Eio.Switch.run (fun sw ->
    let db = S.open_path ~sw Eio.Path.(fs / root / "journal.sqlite3") in
    prepare_journal db journal_count;
    report "SQLite journal prepared";
    page_journal db journal_count);
  Eio.Switch.run (fun sw ->
    let db = S.open_readonly ~sw Eio.Path.(fs / root / "journal.sqlite3") in
    page_journal db journal_count);
  report "SQLite journal reopened and paged";
  (match initial_hwm, vm_hwm_kib () with
   | Some before, Some after ->
     Printf.printf "Whole-test VmHWM delta: %d KiB\n%!" (after - before)
   | _ -> ()))

let () = Eio_main.run @@ fun env ->
  Alcotest.run "IMAP resource scale" ["paged disk state", [
    Alcotest.test_case "Maildir inventory and bridge journal" `Slow
      (fun () -> test_scale env)]]
