open Database
open Record_codec
module M = Imap.Mirror

type t = Database.t

type blob = { sha256:string; length:int64 }
exception Digest_mismatch
module Hash = Digestif.SHA256

let directory t = match blob_dir t with
  | Some (Dir dir) -> Dir dir
  | None -> invalid_arg "Imap_store.Blob: open store with blob_dir"

let filename hash = "sha256-" ^ hash

let decode_blob sha256 length =
  let sha256=text sha256 and length=int length in
  if not (is_sha256_hex sha256) || length<0L then
    fail "invalid stored blob reference";
  {sha256;length}

(* [finally_keep body finally] runs [finally] after [body]. A failure in
   [finally] propagates only when [body] succeeded, so a body exception or
   cancellation is never replaced by a finaliser error. *)
let finally_keep body finally =
  match body () with
  | x -> finally (); x
  | exception ex ->
    let backtrace=Printexc.get_raw_backtrace () in
    (try finally () with _ -> ());
    Printexc.raise_with_backtrace ex backtrace

let unix_io what path f =
  try f () with Unix.Unix_error (e,fn,arg) ->
    raise (Eio.Exn.add_context (Eio_unix.Err.v e fn arg) "%s %s" what path)

(* Eio has no directory fsync, so the directory is opened with Unix. *)
let sync_directory path =
  unix_io "syncing blob directory" path (fun () ->
  Eio_unix.run_in_systhread ~label:"imap-blob-dir-fsync" (fun () ->
    let fd=Unix.openfile path [Unix.O_RDONLY;Unix.O_CLOEXEC] 0 in
    Fun.protect ~finally:(fun () -> Unix.close fd) (fun () ->
      if (Unix.fstat fd).Unix.st_kind <> Unix.S_DIR then
        invalid_arg "Imap_store.Blob: non-directory blob path";
      Unix.fsync fd)))

let nonce = Atomic.make 0
(* The process ID and clock separate writers sharing the directory. *)
let temp_name () =
  Printf.sprintf ".tmp-%d-%Lx-%x" (Unix.getpid ())
    (Int64.of_float (Unix.gettimeofday () *. 1_000_000.))
    (Atomic.fetch_and_add nonce 1)

let open_temp ~sw dir =
  let rec attempt retry =
    let temp=Eio.Path.(dir / temp_name ()) in
    match Eio.Path.open_out ~sw ~create:(`Exclusive 0o600) temp with
    | output -> temp,output
    | exception Eio.Io (Eio.Fs.E (Eio.Fs.Already_exists _),_) when retry ->
        attempt false in
  attempt true

let put t ~source ~length ?expected_sha256 () =
  if length < 0L then invalid_arg "Imap_store.Blob.put: negative length";
  Option.iter (fun hash ->
    if not (is_sha256_hex hash) then
      invalid_arg "Imap_store.Blob.put: expected SHA-256 must be lowercase hex")
    expected_sha256;
  let Dir dir = directory t in
  let native = Eio.Path.native_exn dir in
  Eio.Switch.run (fun sw ->
    let temp,output=open_temp ~sw dir in
    let renamed=ref false in
    finally_keep (fun () ->
      let buffer = Cstruct.create 65536 in
      let rec copy remaining hash =
        if remaining=0L then hash else
        let n=Int64.to_int (Int64.min remaining 65536L) in
        let chunk=Cstruct.sub buffer 0 n in
        Eio.Flow.read_exact source chunk;
        Eio.Flow.write output [chunk];
        copy (Int64.sub remaining (Int64.of_int n))
          (Hash.feed_bigstring hash ~off:chunk.Cstruct.off ~len:n
             chunk.Cstruct.buffer) in
      let hash=Hash.to_hex (Hash.get (copy length Hash.empty)) in
      (match expected_sha256 with
       | Some expected when expected <> hash -> raise Digest_mismatch
       | _ -> ());
      Eio.File.sync output;
      Eio.Path.rename temp Eio.Path.(dir / filename hash);
      renamed := true;
      sync_directory native;
      {sha256=hash;length})
      (fun () -> if not !renamed then
        Eio.Cancel.protect (fun () -> Eio.Path.unlink ~missing_ok:true temp)))

let open_in t ~sw blob =
  let Dir dir = directory t in
  Eio.Path.open_in ~sw Eio.Path.(dir / filename blob.sha256)

let verify t blob =
  let Dir dir = directory t in
  let path=Eio.Path.(dir / filename blob.sha256) in
  if not (Eio.Path.is_file path) then false else
  try Eio.Path.with_open_in path (fun input ->
    if (Eio.File.stat input).kind <> `Regular_file then false else
    let buffer=Cstruct.create 65536 in
    let rec digest count hash =
      match Eio.Flow.single_read input buffer with
      | n ->
        let count=Int64.add count (Int64.of_int n) in
        if count > blob.length then false else
        digest count
          (Hash.feed_bigstring hash ~off:buffer.Cstruct.off ~len:n
             buffer.Cstruct.buffer)
      | exception End_of_file ->
        count=blob.length && Hash.to_hex (Hash.get hash)=blob.sha256 in
    digest 0L Hash.empty)
  with Eio.Io (Eio.Fs.E (Eio.Fs.Not_found _), _) -> false
let verify_blob = verify

let find t ~scope ~uidvalidity ~uid =
  locked t (fun t ->
    match rows t "SELECT sha256,length FROM blob_refs WHERE endpoint=? \
      AND account=? AND mailbox_key=? AND uidvalidity=? AND uid=?"
      (scope_key scope @ [i (Imap.Uidvalidity.to_int64 uidvalidity);
                         i (Imap.Uid.to_int64 uid)]) with
    | [] -> None
    | r :: _ -> Some (decode_blob r.(0) r.(1)))

let missing_page t ~scope ~(cursor:M.cursor) ?after_uid ~limit () =
  check_page_args "Imap_store.Blob.missing_page" scope cursor limit;
  transaction ~begin_sql:"BEGIN" t (fun t ->
    if stale t cursor then `Stale_revision
    else match cursor.uidvalidity with
      | None -> `Uids []
      | Some epoch ->
          let found=rows t "SELECT m.uid FROM snapshots AS m \
            WHERE m.endpoint=? AND m.account=? AND m.mailbox_key=? \
            AND m.uidvalidity=? AND m.uid>? \
            AND NOT EXISTS (SELECT 1 FROM blob_refs AS b \
              WHERE b.endpoint=m.endpoint AND b.account=m.account \
              AND b.mailbox_key=m.mailbox_key \
              AND b.uidvalidity=m.uidvalidity AND b.uid=m.uid) \
            ORDER BY m.uid LIMIT ?"
            (scope_key scope @ [i (Imap.Uidvalidity.to_int64 epoch);
              i (match after_uid with None -> 0L
                 | Some uid -> Imap.Uid.to_int64 uid);
              i (Int64.of_int limit)]) in
          `Uids (List.map (fun r -> uid (int r.(0))) found))

let referenced_page t ~scope ~(cursor:M.cursor) ?after_uid ~limit () =
  check_page_args "Imap_store.Blob.referenced_page" scope cursor limit;
  transaction ~begin_sql:"BEGIN" t (fun t ->
    if stale t cursor then `Stale_revision
    else match cursor.uidvalidity with
    | None -> `Refs []
    | Some epoch ->
        let found=rows t "SELECT m.uid,b.sha256,b.length FROM snapshots AS m \
          JOIN blob_refs AS b ON b.endpoint=m.endpoint \
            AND b.account=m.account AND b.mailbox_key=m.mailbox_key \
            AND b.uidvalidity=m.uidvalidity AND b.uid=m.uid \
          WHERE m.endpoint=? AND m.account=? AND m.mailbox_key=? \
            AND m.uidvalidity=? AND m.uid>? ORDER BY m.uid LIMIT ?"
          (scope_key scope @ [i (Imap.Uidvalidity.to_int64 epoch);
            i (match after_uid with None -> 0L
               | Some value -> Imap.Uid.to_int64 value);
            i (Int64.of_int limit)]) in
        `Refs (List.map (fun r -> uid (int r.(0)),decode_blob r.(1) r.(2))
          found))

let detach_if_matches t ~(scope:M.scope) ~(cursor:M.cursor) ~uid:target
    blob =
  if cursor.scope<>scope then
    invalid_arg "Imap_store.Blob.detach_if_matches: scope/cursor mismatch";
  transaction t (fun t ->
    if stale t cursor then `Stale_revision
    else match cursor.uidvalidity with
    | None -> `Unchanged
    | Some epoch ->
        let key=scope_key scope @ [i (Imap.Uidvalidity.to_int64 epoch);
          i (Imap.Uid.to_int64 target)] in
        run t "DELETE FROM blob_refs WHERE endpoint=? AND account=? \
          AND mailbox_key=? AND uidvalidity=? AND uid=? AND sha256=? \
          AND length=?" (key @ [s blob.sha256;i blob.length]);
        if changes t=0 then `Unchanged else `Detached)

let attach ?(verify=true) t ~(scope:M.scope) ~uidvalidity ~uid blob =
  if verify && not (verify_blob t blob) then
    invalid_arg "Imap_store.Blob.attach: blob missing or corrupt";
  transaction t (fun t ->
    let current=rows t "SELECT raw_name,encoding,mailbox_id,uidvalidity \
      FROM mailboxes WHERE endpoint=? AND account=? AND mailbox_key=?"
      (scope_key scope) in
    (match current with
     | [r] when text r.(0)=scope.raw_name &&
                dec_enc (text r.(1))=scope.encoding &&
                nullable_text r.(2)=scope.mailbox_id &&
                nullable_int r.(3)=Some (Imap.Uidvalidity.to_int64 uidvalidity) -> ()
     | _ -> invalid_arg "Imap_store.Blob.attach: scope or epoch mismatch");
    let key=scope_key scope @ [i (Imap.Uidvalidity.to_int64 uidvalidity);
                               i (Imap.Uid.to_int64 uid)] in
    (match rows t "SELECT 1 FROM snapshots WHERE endpoint=? AND account=? \
      AND mailbox_key=? AND uidvalidity=? AND uid=?" key with
     | [_] -> ()
     | _ -> invalid_arg "Imap_store.Blob.attach: UID absent from snapshot");
    run t "INSERT INTO blob_refs VALUES (?,?,?,?,?,?,?) ON CONFLICT \
      (endpoint,account,mailbox_key,uidvalidity,uid) DO UPDATE SET \
      sha256=excluded.sha256,length=excluded.length"
      (key @ [s blob.sha256;i blob.length]))

(* Eio lists a directory only as a whole, and the listing must stay within
   256 names, so the directory is read with Unix. *)
let iter_directory native f =
  let handle=Eio.Cancel.protect (fun () ->
    unix_io "opening blob directory" native (fun () ->
      Eio_unix.run_in_systhread ~label:"imap-blob-opendir"
        (fun () -> Unix.opendir native))) in
  finally_keep (fun () ->
    let rec loop () =
      let names,finished=unix_io "reading blob directory" native (fun () ->
        Eio_unix.run_in_systhread ~label:"imap-blob-readdir" (fun () ->
          let rec take n acc =
            if n=0 then acc,false else
            match Unix.readdir handle with
            | "." | ".." -> take n acc
            | name -> take (n-1) (name::acc)
            | exception End_of_file -> acc,true in
          take 256 [])) in
      List.iter f names;
      if not finished then loop () in
    loop ())
    (fun () -> Eio.Cancel.protect (fun () ->
      unix_io "closing blob directory" native (fun () ->
        Eio_unix.run_in_systhread ~label:"imap-blob-closedir"
          (fun () -> Unix.closedir handle))))

(* Retained epochs keep their references, so a quarantined epoch's blobs
   stay live until Imap_store.forget_epochs drops it. *)
let reachability = "SELECT EXISTS (SELECT 1 FROM blob_refs WHERE sha256=?) \
  OR EXISTS (SELECT 1 FROM sync_operations WHERE blob_sha256=? \
    AND state NOT IN ('committed','rejected'))"

let iter_orphan_candidates t f =
  let Dir dir=directory t in
  with_stmt_across_locks t reachability (fun stmt ->
    let referenced hash =
      locked t (fun t ->
        match rows_prepared t stmt [s hash;s hash] with
        | r :: _ -> int r.(0)<>0L
        | [] -> fail "invalid blob reachability result") in
    iter_directory (Eio.Path.native_exn dir) (fun name ->
      let candidate =
        if String.starts_with ~prefix:".tmp-" name then true
        else if String.starts_with ~prefix:"sha256-" name then
          let hash=String.sub name 7 (String.length name-7) in
          is_sha256_hex hash && not (referenced hash)
        else false in
      if candidate && Eio.Path.is_file Eio.Path.(dir / name) then f name))

let reap_orphans_iter t ~removed =
  let Dir dir=directory t in
  let native=Eio.Path.native_exn dir in
  let dirty=ref false in
  finally_keep
    (fun () -> iter_orphan_candidates t (fun name ->
      dirty := true;
      Eio.Path.unlink ~missing_ok:true Eio.Path.(dir / name);
      removed name))
    (fun () ->
      if !dirty then Eio.Cancel.protect (fun () -> sync_directory native))
