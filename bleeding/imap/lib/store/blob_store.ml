open Database
open Record_codec
module M = Imap.Mirror
module P = Imap.Proto

type t = Database.t

type blob = { sha256:string; length:int64 }
exception Digest_mismatch
module Hash = Digestif.SHA256

let directory t = match t.blob_dir with
  | Some (Dir dir) -> Dir dir
  | None -> invalid_arg "Imap_store.Blob: open store with blob_dir"

let valid_hash x =
  String.length x = 64 && String.for_all (function
    | '0'..'9' | 'a'..'f' -> true | _ -> false) x

let filename hash =
  if not (valid_hash hash) then fail "invalid stored blob hash";
  "sha256-" ^ hash

let sync_directory path =
  Eio_unix.run_in_systhread ~label:"imap-blob-dir-fsync" (fun () ->
    let fd=Unix.openfile path [Unix.O_RDONLY;Unix.O_CLOEXEC] 0 in
    Fun.protect ~finally:(fun () -> Unix.close fd) (fun () ->
      if (Unix.fstat fd).Unix.st_kind <> Unix.S_DIR then
        invalid_arg "Imap_store.Blob: non-directory blob path";
      Unix.fsync fd))

let nonce = Atomic.make 0
let temp_name () =
  Printf.sprintf ".tmp-%d-%Lx-%x" (Unix.getpid ())
    (Int64.of_float (Unix.gettimeofday () *. 1_000_000.))
    (Atomic.fetch_and_add nonce 1)

let put t ~source ~length ?expected_sha256 () =
  if length < 0L then invalid_arg "Imap_store.Blob.put: negative length";
  Option.iter (fun hash ->
    if not (valid_hash hash) then
      invalid_arg "Imap_store.Blob.put: expected SHA-256 must be lowercase hex")
    expected_sha256;
  let Dir dir = directory t in
  let native = Eio.Path.native_exn dir in
  let temp = Eio.Path.(dir / temp_name ()) in
  let created = ref false in
  let renamed = ref false in
  Fun.protect ~finally:(fun () ->
    if !created && not !renamed then (try Eio.Cancel.protect (fun () ->
      Eio.Path.unlink ~missing_ok:true temp) with _ -> ()))
    (fun () ->
      let hash = Eio.Path.with_open_out ~create:(`Exclusive 0o600) temp
        (fun output ->
          created := true;
          let buffer = Cstruct.create 65536 in
          let rec copy remaining hash =
            if remaining=0L then hash else
            let n=Int64.to_int (Int64.min remaining 65536L) in
            let chunk=Cstruct.sub buffer 0 n in
            Eio.Flow.read_exact source chunk;
            Eio.Flow.write output [chunk];
            copy (Int64.sub remaining (Int64.of_int n))
              (Hash.feed_string hash (Cstruct.to_string chunk)) in
          let result=copy length Hash.empty in
          Eio.File.sync output;
          Hash.to_hex (Hash.get result)) in
      (match expected_sha256 with
       | Some expected when expected <> hash ->
         raise Digest_mismatch
       | _ -> ());
      let final = Eio.Path.(dir / filename hash) in
      Eio.Path.rename temp final;
      renamed := true;
      sync_directory native;
      {sha256=hash;length})

let open_in t ~sw blob =
  let Dir dir = directory t in
  Eio.Path.open_in ~sw Eio.Path.(dir / filename blob.sha256)

let verify t blob =
  let Dir dir = directory t in
  if not (valid_hash blob.sha256) || blob.length < 0L then false else
  let path=Eio.Path.(dir / filename blob.sha256) in
  if not (Eio.Path.is_file path) then false else
  try Eio.Path.with_open_in path (fun input ->
    if (Eio.File.stat input).kind <> `Regular_file then false else
    let buffer=Cstruct.create 65536 in
    let rec digest count hash =
      match Eio.Flow.single_read input buffer with
      | n ->
        let count=Int64.add count (Int64.of_int n) in
        if count < 0L || count > blob.length then false else
        digest count (Hash.feed_string hash
          (Cstruct.to_string (Cstruct.sub buffer 0 n)))
      | exception End_of_file ->
        count=blob.length && Hash.to_hex (Hash.get hash)=blob.sha256 in
    digest 0L Hash.empty)
  with Eio.Io (Eio.Fs.E (Eio.Fs.Not_found _), _) -> false

let find t ~scope ~uidvalidity ~uid =
  Eio.Mutex.use_ro t.mutex (fun () ->
    match rows t "SELECT sha256,length FROM blob_refs WHERE endpoint=? \
      AND account=? AND mailbox_key=? AND uidvalidity=? AND uid=?"
      (scope_key scope @ [i (P.Uidvalidity.to_int64 uidvalidity);
                         i (P.Uid.to_int64 uid)]) with
    | [] -> None
    | [r] ->
      let sha256=text r.(0) and length=int r.(1) in
      if not (valid_hash sha256) || length<0L then
        fail "invalid stored blob reference";
      Some {sha256;length}
    | _ -> fail "duplicate blob reference")

let current_cursor_unlocked t scope =
  match rows t "SELECT endpoint,account,mailbox_key,raw_name,encoding,mailbox_id,phase,uidvalidity,generation,revision,anchor,frontier,inventory_ref,mode FROM mailboxes WHERE endpoint=? AND account=? AND mailbox_key=?" (scope_key scope) with
  | [] -> M.initial scope
  | [r] -> decode_cursor scope r
  | _ -> fail "duplicate mailbox cursor"

let checked_cursor t scope (cursor:M.cursor) =
  let current=current_cursor_unlocked t scope in
  current.revision=cursor.M.revision &&
  current.uidvalidity=cursor.uidvalidity

let checked_page_args who scope (cursor:M.cursor) limit =
  if limit<1 || limit>10_000 then
    invalid_arg (who ^ ": limit must be 1..10000");
  if cursor.M.scope<>scope then
    invalid_arg (who ^ ": scope/cursor mismatch")

let missing_page t ~(scope:M.scope) ~(cursor:M.cursor) ?after_uid ~limit () =
  checked_page_args "Imap_store.Blob.missing_page" scope cursor limit;
  transaction ~begin_sql:"BEGIN" t (fun () ->
    if not (checked_cursor t scope cursor) then `Stale_revision
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
            (scope_key scope @ [i (P.Uidvalidity.to_int64 epoch);
              i (match after_uid with None -> 0L
                 | Some uid -> P.Uid.to_int64 uid);
              i (Int64.of_int limit)]) in
          `Uids (List.map (fun r -> uid (int r.(0))) found))

let referenced_page t ~(scope:M.scope) ~(cursor:M.cursor) ?after_uid
    ~limit () =
  checked_page_args "Imap_store.Blob.referenced_page" scope cursor limit;
  transaction ~begin_sql:"BEGIN" t (fun () ->
    if not (checked_cursor t scope cursor) then `Stale_revision
    else match cursor.uidvalidity with
    | None -> `Refs []
    | Some epoch ->
        let found=rows t "SELECT m.uid,b.sha256,b.length FROM snapshots AS m \
          JOIN blob_refs AS b ON b.endpoint=m.endpoint \
            AND b.account=m.account AND b.mailbox_key=m.mailbox_key \
            AND b.uidvalidity=m.uidvalidity AND b.uid=m.uid \
          WHERE m.endpoint=? AND m.account=? AND m.mailbox_key=? \
            AND m.uidvalidity=? AND m.uid>? ORDER BY m.uid LIMIT ?"
          (scope_key scope @ [i (P.Uidvalidity.to_int64 epoch);
            i (match after_uid with None -> 0L
               | Some value -> P.Uid.to_int64 value);
            i (Int64.of_int limit)]) in
        `Refs (List.map (fun r ->
          let sha256=text r.(1) and length=int r.(2) in
          if not (valid_hash sha256) || length<0L then
            fail "invalid stored blob reference";
          uid (int r.(0)),{sha256;length}) found))

let detach_if_matches t ~(scope:M.scope) ~(cursor:M.cursor) ~uid:target
    blob =
  if cursor.scope<>scope then
    invalid_arg "Imap_store.Blob.detach_if_matches: scope/cursor mismatch";
  transaction t (fun () ->
    if not (checked_cursor t scope cursor) then `Stale_revision
    else match cursor.uidvalidity with
    | None -> `Unchanged
    | Some epoch ->
        let key=scope_key scope @ [i (P.Uidvalidity.to_int64 epoch);
          i (P.Uid.to_int64 target)] in
        (match rows t "SELECT sha256,length FROM blob_refs WHERE \
          endpoint=? AND account=? AND mailbox_key=? AND uidvalidity=? \
          AND uid=?" key with
         | [r] when text r.(0)=blob.sha256 && int r.(1)=blob.length ->
             run t "DELETE FROM blob_refs WHERE endpoint=? AND account=? \
               AND mailbox_key=? AND uidvalidity=? AND uid=?" key;
             `Detached
         | _ -> `Unchanged))

let attach t ~scope ~uidvalidity ~uid blob =
  if not (verify t blob) then
    invalid_arg "Imap_store.Blob.attach: blob missing or corrupt";
  transaction t (fun () ->
    let current=rows t "SELECT raw_name,encoding,mailbox_id,uidvalidity \
      FROM mailboxes WHERE endpoint=? AND account=? AND mailbox_key=?"
      (scope_key scope) in
    (match current with
     | [r] when text r.(0)=scope.raw_name &&
                dec_enc (text r.(1))=scope.encoding &&
                nullable_text r.(2)=scope.mailbox_id &&
                nullable_int r.(3)=Some (P.Uidvalidity.to_int64 uidvalidity) -> ()
     | _ -> invalid_arg "Imap_store.Blob.attach: scope or epoch mismatch");
    let key=scope_key scope @ [i (P.Uidvalidity.to_int64 uidvalidity);
                               i (P.Uid.to_int64 uid)] in
    (match rows t "SELECT 1 FROM snapshots WHERE endpoint=? AND account=? \
      AND mailbox_key=? AND uidvalidity=? AND uid=?" key with
     | [_] -> ()
     | _ -> invalid_arg "Imap_store.Blob.attach: UID absent from snapshot");
    run t "INSERT INTO blob_refs VALUES (?,?,?,?,?,?,?) ON CONFLICT \
      (endpoint,account,mailbox_key,uidvalidity,uid) DO UPDATE SET \
      sha256=excluded.sha256,length=excluded.length"
      (key @ [s blob.sha256;i blob.length]))

let iter_directory native f =
  let owned = ref None in
  Fun.protect ~finally:(fun () -> Eio.Cancel.protect (fun () ->
    match !owned with
    | None -> ()
    | Some handle ->
        owned := None;
        Eio_unix.run_in_systhread ~label:"imap-blob-closedir"
          (fun () -> Unix.closedir handle))) (fun () ->
    Eio.Cancel.protect (fun () ->
      let handle=Eio_unix.run_in_systhread ~label:"imap-blob-opendir"
        (fun () -> Unix.opendir native) in
      owned := Some handle);
    let handle=Option.get !owned in
    let rec loop () =
      let names,finished=Eio_unix.run_in_systhread
        ~label:"imap-blob-readdir" (fun () ->
          let rec take n acc =
            if n=0 then acc,false else
            match Unix.readdir handle with
            | "." | ".." -> take n acc
            | name -> take (n-1) (name::acc)
            | exception End_of_file -> acc,true in
          take 256 []) in
      List.iter f names;
      if not finished then loop () in
    loop ())

let referenced t hash =
  Eio.Mutex.use_ro t.mutex (fun () ->
    match rows t "SELECT EXISTS (SELECT 1 FROM blob_refs WHERE sha256=?) OR \
      EXISTS (SELECT 1 FROM sync_operations WHERE blob_sha256=? \
        AND state NOT IN ('committed','rejected')) OR \
      EXISTS (SELECT 1 FROM intents WHERE digest=? \
        AND state NOT IN ('confirmed','rejected'))" [s hash;s hash;s hash] with
    | [r] -> int r.(0)<>0L
    | _ -> fail "invalid blob reachability result")

let iter_orphan_candidates t f =
  let Dir dir=directory t in
  iter_directory (Eio.Path.native_exn dir) (fun name ->
    let candidate =
      if String.starts_with ~prefix:".tmp-" name then true
      else if String.starts_with ~prefix:"sha256-" name then
        let hash=String.sub name 7 (String.length name-7) in
        valid_hash hash && not (referenced t hash)
      else false in
    if candidate && Eio.Path.is_file Eio.Path.(dir / name) then f name)

let orphan_candidates t =
  let names=ref [] in
  iter_orphan_candidates t (fun name -> names := name :: !names);
  List.sort String.compare !names

let reap_orphans_iter t ~removed =
  let Dir dir=directory t in
  let native=Eio.Path.native_exn dir in
  let dirty=ref false in
  Fun.protect ~finally:(fun () ->
    if !dirty then Eio.Cancel.protect (fun () -> sync_directory native))
    (fun () -> iter_orphan_candidates t (fun name ->
      dirty := true;
      Eio.Path.unlink ~missing_ok:true Eio.Path.(dir / name);
      removed name))

let reap_orphans t =
  let names=ref [] in
  reap_orphans_iter t ~removed:(fun name -> names := name :: !names);
  List.sort String.compare !names
