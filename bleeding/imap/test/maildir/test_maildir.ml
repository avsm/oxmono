module M = Imap_maildir
let flag s = match Mail_flag.Imap_flag.of_wire s with
  | Ok flag -> flag | Error e -> Alcotest.fail e
let wires flags = List.map Mail_flag.Imap_flag.to_wire flags
let with_root f =
  let root=Filename.temp_file "imap-maildir-" "" in
  Sys.remove root;
  Unix.mkdir root 0o700;
  let rec remove path =
    if Sys.is_directory path then (
      Sys.readdir path |> Array.iter (fun name -> remove (Filename.concat path name));
      Unix.rmdir path)
    else Sys.remove path in
  Fun.protect ~finally:(fun () -> remove root) (fun () -> f root)
let read_all source length =
  let buffer=Cstruct.create length in
  Eio.Flow.read_exact source buffer;
  Cstruct.to_string buffer

let test_flag_order env = with_root (fun root ->
  let m=M.open_dir Eio.Path.(Eio.Stdenv.fs env / root) in
  let flags=[flag "tag";flag "\\Seen";flag "\\Flagged";flag "\\Seen"] in
  let a=M.append m ~source:(Eio.Flow.string_source "body") ~length:4L ~flags () in
  Eio.Switch.run (fun sw ->
    Alcotest.(check string) "fresh append is readable" "body"
      (read_all (M.open_message m ~sw a) 4));
  let b=M.set_flags m a (List.rev flags) in
  Eio.Switch.run (fun sw ->
    Alcotest.(check string) "fresh flag update is readable" "body"
      (read_all (M.open_message m ~sw b) 4)))

let test_standard_format env = with_root (fun root ->
  let path=Eio.Path.(Eio.Stdenv.fs env / root) in
  let m=M.open_dir path in
  List.iter (fun name -> Alcotest.(check bool) "no custom sidecar directory" false
    (Sys.file_exists (Filename.concat root name))) [".imap-flags";".imap-dates"];
  Eio.Path.save ~create:(`Exclusive 0o600) Eio.Path.(path / "dovecot-keywords")
    "0 Custom\n25 LastSlot\n";
  let original="external,S=1,W=1:2,PSa,custom=value" in
  Eio.Path.save ~create:(`Exclusive 0o600) Eio.Path.(path / "cur" / original) "x";
  let occurrence=List.hd (M.scan m) in
  Alcotest.(check (list string)) "mapped external flags" ["\\Seen";"Custom"]
    (wires occurrence.flags);
  let changed=M.set_flags m occurrence [flag "\\Flagged"] in
  Alcotest.(check string) "basename, Passed and extension preserved"
    "external,S=1,W=1:2,FP,custom=value" changed.filename;
  Alcotest.(check bool) "mtime preserved by flags" true
    (changed.mtime=occurrence.mtime);
  let view=M.with_inventory_pages m Fun.id in
  (try ignore (M.inventory_count view); Alcotest.fail "expired inventory accepted"
   with Invalid_argument _ -> ());
  let lock=Eio.Path.(path / "dovecot-uidlist.lock") in
  Eio.Path.save ~create:(`Exclusive 0o600) lock
    (Printf.sprintf "%d %s\n" (Unix.getpid ()) (Unix.gethostname ()));
  (try ignore (M.set_flags m changed []); Alcotest.fail "live metadata lock ignored"
   with M.Writer_lock_busy _ -> ());
  Eio.Path.unlink lock;
  Alcotest.(check string) "content survives lock contention" "x"
    (Eio.Path.load Eio.Path.(path / "cur" / changed.filename)))

let test_replaced_occurrence env = with_root (fun root ->
  let path=Eio.Path.(Eio.Stdenv.fs env / root) in
  let m=M.open_dir path in
  let original=M.append m ~source:(Eio.Flow.string_source "x") ~length:1L
    ~flags:[] () in
  let target=Eio.Path.(path / "new" / original.filename) in
  Eio.Path.rename target Eio.Path.(path / "tmp" / "held-original");
  Eio.Path.save ~create:(`Exclusive 0o600) target "x";
  Unix.utimes (Eio.Path.native_exn target) original.mtime original.mtime;
  Alcotest.(check bool) "same-name replacement is stale" true
    (M.with_unchanged_occurrence m original (fun () -> ())=Error `Changed))

let test_keyword_limits env = with_root (fun root ->
  let path=Eio.Path.(Eio.Stdenv.fs env / root) in
  let m=M.open_dir path in
  let keywords=List.init 26 (fun i -> flag (Printf.sprintf "Key%d" i)) in
  let a=M.append m ~source:(Eio.Flow.string_source "x") ~length:1L
    ~flags:keywords () in
  let before=Eio.Path.load Eio.Path.(path / "dovecot-keywords") in
  List.iter (fun flags ->
    (try ignore (M.append m ~source:(Eio.Flow.string_source "x") ~length:1L
       ~flags ()); Alcotest.fail "unsupported flags published"
     with Failure _ -> ())) [[flag "TwentySeventh"];[flag "\\FutureFlag"]];
  Alcotest.(check int) "overflow leaves inventory intact" 1 (List.length (M.scan m));
  Alcotest.(check string) "overflow leaves mapping intact" before
    (Eio.Path.load Eio.Path.(path / "dovecot-keywords"));
  let changed=M.set_flags m a [flag "key0"] in
  Alcotest.(check int) "case alias reuses slot" 1 (List.length changed.flags);
  let leap=Result.get_ok (Imap.Internal_date.of_string "31-Dec-2016 23:59:60 +0000") in
  (try ignore (M.append m ~source:(Eio.Flow.string_source "x") ~length:1L
    ~flags:[] ~internal_date:leap ()); Alcotest.fail "leap second published"
   with Failure _ -> ());
  Alcotest.(check int) "invalid date leaves inventory intact" 1 (List.length (M.scan m)))

let test_invalid_mapping env = with_root (fun root ->
  let path=Eio.Path.(Eio.Stdenv.fs env / root) in
  let m=M.open_dir path in
  List.iter (fun text ->
    Eio.Path.save ~create:(`Or_truncate 0o600) Eio.Path.(path / "dovecot-keywords") text;
    (try ignore (M.scan m); Alcotest.fail "invalid keyword map accepted"
     with Failure _ -> ()))
    ["0 A\n0 B\n";"0 A\n1 a\n";"26 A\n";"00 A\n";"0\n";"0 A\nx\n"];
  Eio.Path.unlink Eio.Path.(path / "dovecot-keywords");
  Eio.Path.save ~create:(`Exclusive 0o600) Eio.Path.(path / "cur" / "external:2,a") "x";
  (try ignore (M.scan m); Alcotest.fail "unmapped keyword accepted"
   with Failure message ->
     Alcotest.(check string) "unmapped letter names file and letter"
       "Imap_maildir: external:2,a references unmapped keyword letter a"
       message);
  Eio.Path.unlink Eio.Path.(path / "cur" / "external:2,a");
  Eio.Path.save ~create:(`Exclusive 0o600)
    Eio.Path.(path / "cur" / "external:2,S!") "x";
  (try ignore (M.scan m); Alcotest.fail "unknown letter accepted"
   with Failure message ->
     Alcotest.(check string) "unknown letter names file and letter"
       "Imap_maildir: external:2,S! has invalid Maildir flag letter '!'"
       message);
  Eio.Path.unlink Eio.Path.(path / "cur" / "external:2,S!");
  Eio.Path.mkdir ~perm:0o700 Eio.Path.(path / ".imap-flags");
  Eio.Path.save ~create:(`Exclusive 0o600) Eio.Path.(path / ".imap-flags" / "old") "data";
  (try ignore (M.open_dir path); Alcotest.fail "legacy metadata silently ignored"
   with Failure _ -> ()))

let test_keyword_file_tolerance env = with_root (fun root ->
  let path=Eio.Path.(Eio.Stdenv.fs env / root) in
  let m=M.open_dir path in
  let keywords=Eio.Path.(path / "dovecot-keywords") in
  Eio.Path.save ~create:(`Exclusive 0o600)
    Eio.Path.(path / "cur" / "ext:2,TSDFRab") "x";
  List.iter (fun text ->
    Eio.Path.save ~create:(`Or_truncate 0o600) keywords text;
    match M.scan m with
    | [o] ->
      Alcotest.(check (list string)) ("flags sorted for " ^ String.escaped text)
        ["\\Seen";"\\Answered";"\\Flagged";"\\Deleted";"\\Draft";"A";"B"]
        (wires o.flags)
    | _ -> Alcotest.fail "one occurrence expected")
    ["0 A\n1 B";"\n0 A\n\n1 B\n\n";"1 B\n0 A\n"];
  let o=List.hd (M.scan m) in
  let changed=M.set_flags m o [flag "B";flag "\\Seen";flag "C";flag "A"] in
  Alcotest.(check string) "letters sorted with system letters first"
    "ext:2,Sabc" changed.filename;
  Alcotest.(check string) "added keyword appended"
    "0 A\n1 B\n2 C\n" (Eio.Path.load keywords))

let test_restart env = with_root (fun root ->
  let fs=Eio.Stdenv.fs env in
  let path=Eio.Path.(fs / root) in
  let m=M.open_dir path in
  let raw="Subject: same\r\n\r\nExact bytes\r\n" in
  let date=match Imap.Internal_date.of_string
      "26-Sep-2025 12:34:56 +0230" with
    | Ok date -> date | Error error -> Alcotest.fail error in
  let flags=[flag "\\Seen";flag "A-Custom"] in
  let reserved=M.reserve_id () in
  let a=M.append m ~id:reserved ~source:(Eio.Flow.string_source raw)
    ~length:(Int64.of_int (String.length raw)) ~flags
    ~internal_date:date () in
  let b=M.append m ~source:(Eio.Flow.string_source raw)
    ~length:(Int64.of_int (String.length raw)) ~flags:[] () in
  Alcotest.(check bool) "distinct occurrences" true (a.id<>b.id);
  Alcotest.(check bool) "new vs cur" true
    (a.location=Cur && b.location=New);
  let m=M.open_dir path in
  Alcotest.(check bool) "reserved ID survives restart" true
    (Option.is_some (M.find m ~id:reserved));
  (try ignore (M.append m ~id:reserved ~source:(Eio.Flow.string_source raw)
    ~length:(Int64.of_int (String.length raw)) ~flags ());
    Alcotest.fail "duplicate reserved ID accepted"
   with Failure _ -> ());
  let inventory=M.scan m in
  Alcotest.(check int) "same-content entries" 2 (List.length inventory);
  let a=List.find (fun x -> x.M.id=a.id) inventory in
  Alcotest.(check (list string)) "flags survive restart"
    ["\\Seen";"A-Custom"] (wires a.flags);
  Alcotest.(check (option string)) "date survives restart"
    (Some "26-Sep-2025 10:04:56 +0000")
    (Option.map Imap.Internal_date.to_string a.internal_date);
  Alcotest.(check string) "date comes from file mtime"
    "26-Sep-2025 10:04:56 +0000"
    (M.upload_internal_date a |> function
      | Ok date -> Imap.Internal_date.to_string date
      | Error error -> Alcotest.fail error);
  let expected=Digestif.SHA256.(to_hex (digest_string raw)) in
  Alcotest.(check string) "content digest" expected (M.sha256 m a);
  Eio.Switch.run (fun sw ->
    let input=M.open_message m ~sw a in
    Alcotest.(check string) "exact content" raw
      (read_all input (String.length raw)));
  let updated=M.set_flags m a [flag "\\Flagged";flag "NewTag"] in
  Alcotest.(check string) "stable id after rename" a.id updated.id;
  ignore (M.set_flags m updated [flag "\\Flagged";flag "MoreTag"]);
  let m=M.open_dir path in
  let after=M.scan m |> List.find (fun x -> x.M.id=a.id) in
  Alcotest.(check (list string)) "keyword-only change"
    ["\\Flagged";"MoreTag"] (wires after.flags);
  let old_path=Filename.concat (Filename.concat root "cur") after.filename in
  let external_name=after.id ^ ":2,S" in
  Unix.rename old_path (Filename.concat (Filename.concat root "cur") external_name);
  let changed=M.scan m |> List.find (fun x -> x.M.id=a.id) in
  Alcotest.(check (list string)) "external flag rename removes omitted keyword"
    ["\\Seen"] (wires changed.flags);
  Alcotest.(check (option string)) "flag rename keeps date"
    (Some "26-Sep-2025 10:04:56 +0000")
    (Option.map Imap.Internal_date.to_string changed.internal_date);
  let updated=M.set_flags m changed [flag "\\Flagged";flag "FinalTag"] in
  ignore (M.recover m);
  Alcotest.(check int) "two occurrences after recovery" 2
    (List.length (M.scan m));
  M.remove m updated;
  Alcotest.(check bool) "removed date sidecar" false
    (Sys.file_exists (Filename.concat
      (Filename.concat root ".imap-dates") reserved));
  Alcotest.(check int) "targeted removal" 1 (List.length (M.scan m)))

let test_failure_recovery env = with_root (fun root ->
  let fs=Eio.Stdenv.fs env in
  let path=Eio.Path.(fs / root) in
  let m=M.open_dir path in
  (try ignore (M.append m ~source:(Eio.Flow.string_source "short")
    ~length:10L ~flags:[flag "Lost"] ());
    Alcotest.fail "short source accepted"
   with End_of_file -> ());
  Alcotest.(check int) "no partial visible message" 0
    (List.length (M.scan m));
  let temp=".tmp-0123456789abcdef0123456789abcdef" in
  let oc=open_out_bin (Filename.concat (Filename.concat root "tmp") temp) in
  output_string oc "interrupted"; close_out oc;
  let stage=".inventory-0123456789abcdef0123456789abcdef.sqlite3" in
  let oc=open_out_bin (Filename.concat (Filename.concat root "tmp") stage) in
  output_string oc "interrupted index"; close_out oc;
  let recovery=M.recover (M.open_dir path) in
  Alcotest.(check (list string)) "orphan tmp and index" [stage;temp]
    (List.sort String.compare recovery.removed_temporary);
  let source=Eio.Flow.string_source "abcdef" in
  let o=M.append m ~source ~length:3L ~flags:[] () in
  Eio.Switch.run (fun sw ->
    let input=M.open_message m ~sw o in
    Alcotest.(check string) "exact bounded stream" "abc" (read_all input 3));
  Alcotest.(check int64) "exact length" 3L o.length)

let test_paged_inventory env = with_root (fun root ->
  let path=Eio.Path.(Eio.Stdenv.fs env / root) in
  let m=M.open_dir path in
  let raw="Subject: duplicate\r\n\r\nEqual bytes\r\n" in
  let expected=List.init 43 (fun i ->
    let flags=if i mod 2=0 then [flag "\\Seen";flag "Custom"] else [] in
    M.append m ~source:(Eio.Flow.string_source raw)
      ~length:(Int64.of_int (String.length raw)) ~flags () ) in
  let expected_ids=List.map (fun (o:M.occurrence) -> o.id) expected
    |> List.sort String.compare in
  M.with_inventory_pages m (fun view ->
    Alcotest.(check int64) "complete count" 43L
      (M.inventory_count view);
    (try ignore (M.inventory_page view ~limit:0 ());
      Alcotest.fail "zero page accepted"
     with Invalid_argument _ -> ());
    let rec pages after seen max_page =
      let p=M.inventory_page view ?after ~limit:7 () in
      let n=List.length p.occurrences in
      let ids=List.map (fun (o:M.occurrence) -> o.id) p.occurrences in
      let seen=List.rev_append ids seen in
      let max_page=max max_page n in
      match p.next_after with
      | None -> List.rev seen,max_page
      | Some id ->
        Alcotest.(check bool) "nonempty continuation" true (n>0);
        pages (Some id) seen max_page in
    let ids,max_page=pages None [] 0 in
    Alcotest.(check int) "page bound" 7 max_page;
    Alcotest.(check (list string)) "every occurrence once"
      expected_ids ids;
    let first=M.inventory_page view ~limit:1 () in
    let o=List.hd first.occurrences in
    Alcotest.(check string) "staged stable first ID"
      (List.hd expected_ids) o.id;
    Alcotest.(check bool) "staged lookup" true
      (Option.is_some (M.inventory_find view ~id:o.id));
    Alcotest.(check bool) "missing staged lookup" true
      (Option.is_none (M.inventory_find view ~id:"missing"));
    let flagged=List.find (fun (o:M.occurrence) ->
      List.exists ((=) "Custom") (wires o.flags)) expected in
    let staged=Option.get (M.inventory_find view ~id:flagged.id) in
    let digest=Digestif.SHA256.(to_hex (digest_string raw)) in
    Alcotest.(check string) "indexed occurrence hash" digest
      (M.sha256 ~inventory:view m staged);
    (match M.with_unchanged_occurrence ~inventory:view m staged
        (fun () -> M.set_flags m staged [flag "\\Seen";flag "Later"]) with
     | Error `Changed -> ()
     | Ok _ -> Alcotest.fail "indexed check accepted a flag rename");
    let _=M.append m ~source:(Eio.Flow.string_source raw)
      ~length:(Int64.of_int (String.length raw)) ~flags:[] () in
    Alcotest.(check int64) "snapshot excludes later append" 43L
      (M.inventory_count view);
    (try ignore (M.append ~inventory:view m ~id:staged.id
      ~source:(Eio.Flow.string_source raw)
      ~length:(Int64.of_int (String.length raw)) ~flags:[] ());
      Alcotest.fail "staged duplicate ID accepted"
     with Failure _ -> ());
    let fresh=M.reserve_id () in
    ignore (M.append ~inventory:view m ~id:fresh
      ~source:(Eio.Flow.string_source raw)
      ~length:(Int64.of_int (String.length raw)) ~flags:[] ());
    (try ignore (M.append ~inventory:view m ~id:fresh
      ~source:(Eio.Flow.string_source raw)
      ~length:(Int64.of_int (String.length raw)) ~flags:[flag "\\Seen"] ());
      Alcotest.fail "new duplicate ID accepted"
     with Failure _ -> ());
    Alcotest.(check int64) "snapshot remains fixed after indexed appends"
      43L (M.inventory_count view);
    let other=M.open_dir path in
    (try ignore (M.append ~inventory:view other ~id:(M.reserve_id ())
      ~source:(Eio.Flow.string_source raw)
      ~length:(Int64.of_int (String.length raw)) ~flags:[] ());
      Alcotest.fail "inventory accepted another Maildir handle"
     with Invalid_argument _ -> ()));
  Alcotest.(check (list string)) "temporary index removed" []
    (Sys.readdir (Filename.concat root "tmp") |> Array.to_list))

let test_paged_duplicate_id env = with_root (fun root ->
  let path=Eio.Path.(Eio.Stdenv.fs env / root) in
  let m=M.open_dir path in
  let a=M.append m ~source:(Eio.Flow.string_source "x") ~length:1L
    ~flags:[] () in
  let source=Filename.concat (Filename.concat root "new") a.filename in
  let duplicate=Filename.concat (Filename.concat root "cur")
    (a.id ^ ":2,S") in
  let input=open_in_bin source and output=open_out_bin duplicate in
  Fun.protect ~finally:(fun () -> close_in input; close_out output)
    (fun () -> output_char output (input_char input));
  (try M.with_inventory_pages m (fun _ ->
     Alcotest.fail "duplicate ID accepted")
   with Failure message ->
     Alcotest.(check string) "duplicate identity diagnosed"
       ("Imap_maildir: duplicate occurrence identity " ^ a.id) message);
  Alcotest.(check (list string)) "failed index removed" []
    (Sys.readdir (Filename.concat root "tmp") |> Array.to_list))

let test_external_mtime_date env = with_root (fun root ->
  let path=Eio.Path.(Eio.Stdenv.fs env / root) in
  let m=M.open_dir path in
  let original=M.append m ~source:(Eio.Flow.string_source "x")
    ~length:1L ~flags:[] () in
  let filename=Filename.concat (Filename.concat root "new")
    original.filename in
  Unix.utimes filename 1709164800. 1709164800.;
  let local=match M.scan m with
    | [local] -> local | _ -> Alcotest.fail "external file missing" in
  let date=match M.upload_internal_date local with
    | Ok date -> date | Error error -> Alcotest.fail error in
  Alcotest.(check string) "external mtime becomes UTC INTERNALDATE"
    "29-Feb-2024 00:00:00 +0000"
    (Imap.Internal_date.to_string date);
  M.with_inventory_pages m (fun inventory ->
    let staged=Option.get (M.inventory_find inventory ~id:local.id) in
    Alcotest.(check bool) "paged inventory retains mtime" true
      (local.mtime=staged.mtime);
    Unix.utimes filename 1709164801. 1709164801.;
    match M.with_unchanged_occurrence ~inventory m staged
      (fun () -> ()) with
    | Error `Changed -> ()
    | Ok () -> Alcotest.fail "changed mtime accepted as same source"))

let save path text = Eio.Path.save ~create:(`Exclusive 0o600) path text
let append_x ?inventory ?id ?internal_date m flags =
  M.append ?inventory m ?id ~source:(Eio.Flow.string_source "x") ~length:1L
    ~flags ?internal_date ()
let rejects label f =
  match f () with
  | exception Failure _ -> ()
  | _ -> Alcotest.fail (label ^ " accepted")

let test_supplied_id_variants env = with_root (fun root ->
  let path=Eio.Path.(Eio.Stdenv.fs env / root) in
  let m=M.open_dir path in
  let own=M.reserve_id () in
  ignore (append_x m ~id:own [flag "\\Seen"]);
  rejects "flagless republication of a flagged ID" (fun () ->
    append_x m ~id:own []);
  let external_id=M.reserve_id () in
  save Eio.Path.(path / "cur" / (external_id ^ ":2,FS")) "x";
  rejects "external flag variant" (fun () ->
    append_x m ~id:external_id [flag "\\Draft"]);
  let staged_later=M.reserve_id () in
  M.with_inventory_pages m (fun inventory ->
    save Eio.Path.(path / "cur" / (staged_later ^ ":2,S")) "x";
    rejects "variant published after staging" (fun () ->
      append_x ~inventory m ~id:staged_later []));
  Alcotest.(check bool) "nothing published in new" false
    (List.exists (fun name -> name=staged_later || name=external_id)
      (Eio.Path.read_dir Eio.Path.(path / "new"))))

let test_ignored_entries env = with_root (fun root ->
  let path=Eio.Path.(Eio.Stdenv.fs env / root) in
  let m=M.open_dir path in
  let real=append_x m [flag "\\Seen"] in
  save Eio.Path.(path / "new" / ".DS_Store") "x";
  save Eio.Path.(path / "cur" / ".hidden:2,S") "x";
  let dir_id=M.reserve_id () and link_id=M.reserve_id () in
  Eio.Path.mkdir ~perm:0o700 Eio.Path.(path / "cur" / (dir_id ^ ":2,S"));
  Unix.symlink (Filename.concat (Filename.concat root "cur") real.filename)
    (Filename.concat (Filename.concat root "new") link_id);
  Unix.mkfifo (Filename.concat (Filename.concat root "new") "fifo") 0o600;
  Alcotest.(check (list string)) "only the regular message" [real.id]
    (List.map (fun (o:M.occurrence) -> o.id) (M.scan m));
  M.with_inventory_pages m (fun view ->
    Alcotest.(check int64) "staging skips the same entries" 1L
      (M.inventory_count view));
  List.iter (fun id ->
    Alcotest.(check bool) ("no occurrence for " ^ id) true
      (M.find m ~id=None)) [dir_id;link_id;".DS_Store";".hidden"];
  Alcotest.(check bool) "real message found" true
    (Option.is_some (M.find m ~id:real.id));
  Unix.unlink (Filename.concat (Filename.concat root "new") link_id))

let test_find_by_name env = with_root (fun root ->
  let path=Eio.Path.(Eio.Stdenv.fs env / root) in
  let m=M.open_dir path in
  let fresh=append_x m [] and flagged=append_x m [flag "\\Flagged"] in
  save Eio.Path.(path / "cur" / "broken:2,S!") "x";
  (try ignore (M.scan m); Alcotest.fail "scan accepted an unknown letter"
   with Failure _ -> ());
  List.iter (fun (o:M.occurrence) ->
    match M.find m ~id:o.id with
    | Some found -> Alcotest.(check string) "found by name" o.filename
        found.filename
    | None -> Alcotest.fail ("missing " ^ o.id)) [fresh;flagged];
  Eio.Path.unlink Eio.Path.(path / "cur" / "broken:2,S!");
  save Eio.Path.(path / "cur" / (fresh.id ^ ":2,S")) "x";
  (try ignore (M.find m ~id:fresh.id); Alcotest.fail "duplicate found once"
   with Failure message ->
     Alcotest.(check string) "duplicate names the ID"
       ("Imap_maildir: duplicate occurrence identity " ^ fresh.id) message))

let test_epoch_date env = with_root (fun root ->
  let m=M.open_dir Eio.Path.(Eio.Stdenv.fs env / root) in
  let epoch=Result.get_ok
    (Imap.Internal_date.of_string "01-Jan-1970 00:00:00 +0000") in
  let o=append_x m [] ~internal_date:epoch in
  Alcotest.(check (float 0.)) "epoch mtime" 0. o.mtime;
  Alcotest.(check (option string)) "epoch date"
    (Some " 1-Jan-1970 00:00:00 +0000")
    (Option.map Imap.Internal_date.to_string o.internal_date))

let test_no_overwrite env = with_root (fun root ->
  let path=Eio.Path.(Eio.Stdenv.fs env / root) in
  let m=M.open_dir path in
  let o=append_x m [] in
  let occupied=Eio.Path.(path / "cur" / (o.id ^ ":2,S")) in
  save occupied "other";
  (match M.set_flags m o [flag "\\Seen"] with
   | exception Eio.Io (Eio.Fs.E (Eio.Fs.Already_exists _), _) -> ()
   | _ -> Alcotest.fail "flag change replaced an existing file");
  Alcotest.(check string) "existing file kept" "other" (Eio.Path.load occupied);
  Alcotest.(check string) "source kept" "x"
    (Eio.Path.load Eio.Path.(path / "new" / o.filename)))

let test_stale_occurrence env = with_root (fun root ->
  let path=Eio.Path.(Eio.Stdenv.fs env / root) in
  let m=M.open_dir path in
  let o=append_x m [] in
  Eio.Path.rename Eio.Path.(path / "new" / o.filename)
    Eio.Path.(path / "cur" / (o.id ^ ":2,S"));
  List.iter (fun (label,f) ->
    match f () with
    | exception M.Stale_occurrence -> ()
    | _ -> Alcotest.fail (label ^ " accepted a stale occurrence"))
    ["set_flags",(fun () -> ignore (M.set_flags m o []));
     "remove",(fun () -> M.remove m o);
     "sha256",(fun () -> ignore (M.sha256 m o));
     "open_message",(fun () ->
       Eio.Switch.run (fun sw -> ignore (M.open_message m ~sw o)))])

let test_keyword_file_change env = with_root (fun root ->
  let path=Eio.Path.(Eio.Stdenv.fs env / root) in
  let m=M.open_dir path in
  let o=append_x m [flag "First"] in
  let keywords=Eio.Path.(path / "dovecot-keywords") in
  let replacement=Eio.Path.(path / "tmp" / "keywords") in
  save replacement "0 Second\n";
  Eio.Path.rename replacement keywords;
  Alcotest.(check (list string)) "external mapping change observed"
    ["Second"] (wires (List.hd (M.scan m)).flags);
  Alcotest.(check bool) "old observation is stale" true
    (M.with_unchanged_occurrence m o (fun () -> ())=Error `Changed))

module Chmod_then_fail = struct
  type t = string
  let read_methods = []
  let single_read directory _ = Unix.chmod directory 0o500; raise Exit
end

let test_cleanup_keeps_exception env = with_root (fun root ->
  let m=M.open_dir Eio.Path.(Eio.Stdenv.fs env / root) in
  let tmp=Filename.concat root "tmp" in
  if Unix.geteuid ()<>0 then (
    let source=Eio.Resource.T (tmp,
      Eio.Flow.Pi.source (module Chmod_then_fail)) in
    (match M.append m ~source ~length:1L ~flags:[] () with
     | exception Exit -> ()
     | exception exn -> Unix.chmod tmp 0o700; raise exn
     | _ -> Alcotest.fail "failing source published");
    Unix.chmod tmp 0o700;
    Alcotest.(check int) "unremovable temporary left for recovery" 1
      (List.length (M.recover m).removed_temporary)))

let probe_writer root =
  let executable=Sys.executable_name in
  let pid=Unix.create_process executable
    [|executable; "--writer-probe"; root|]
    Unix.stdin Unix.stdout Unix.stderr in
  match snd (Unix.waitpid [] pid) with
  | Unix.WEXITED code -> code
  | Unix.WSIGNALED signal ->
    Alcotest.failf "writer probe killed by signal %d" signal
  | Unix.WSTOPPED signal ->
    Alcotest.failf "writer probe stopped by signal %d" signal

let test_writer_lock env = with_root (fun root ->
  let path=Eio.Path.(Eio.Stdenv.fs env / root) in
  let m=M.open_dir path in
  let second=M.open_dir path in
  M.with_writer_lock m (fun () ->
    (try M.with_writer_lock second (fun () ->
       Alcotest.fail "same-process writer admitted")
     with M.Writer_lock_busy _ -> ());
    Alcotest.(check int) "other process cannot enter" 42
      (probe_writer root));
  Alcotest.(check int) "other process enters after release" 0
    (probe_writer root);
  (try M.with_writer_lock m (fun () -> raise Exit)
   with Exit -> ());
  Alcotest.(check int) "exception releases lease" 0
    (probe_writer root);
  let entered, entered_u=Eio.Promise.create () in
  let never, _never_u=Eio.Promise.create () in
  (try Eio.Switch.run (fun sw ->
     Eio.Fiber.fork ~sw (fun () ->
       M.with_writer_lock m (fun () ->
         Eio.Promise.resolve entered_u ();
         Eio.Promise.await never));
     Eio.Promise.await entered;
     Alcotest.(check int) "cancelled writer still excludes child" 42
       (probe_writer root);
     Eio.Switch.fail sw Exit)
   with Exit -> ());
  Alcotest.(check int) "cancellation releases lease" 0
    (probe_writer root);
  let m=M.open_dir path in
  M.with_writer_lock m (fun () ->
    Alcotest.(check bool) "permanent lock file" true
      (Sys.file_exists (Filename.concat root ".imap-writer.lock")));
  Alcotest.(check int) "reopened handle enters after release" 0
    (probe_writer root))

let test_metadata_lock_recovery env = with_root (fun root ->
  let m=M.open_dir Eio.Path.(Eio.Stdenv.fs env / root) in
  let lock=Filename.concat root "dovecot-uidlist.lock" in
  let save content=Out_channel.with_open_bin lock (fun out ->
    output_string out content) in
  let busy label =
    let before=Unix.lstat lock in
    (try ignore (M.scan m); Alcotest.fail (label ^ " was reclaimed")
     with M.Writer_lock_busy _ -> ());
    let after=Unix.lstat lock in
    Alcotest.(check int) (label ^ " inode retained") before.Unix.st_ino after.Unix.st_ino in
  let pid=Unix.create_process "/bin/true" [|"/bin/true"|]
    Unix.stdin Unix.stdout Unix.stderr in
  ignore (Unix.waitpid [] pid);
  (try Unix.kill pid 0; Alcotest.fail "dead fixture PID reused"
   with Unix.Unix_error (Unix.ESRCH,_,_) -> ());
  save (Printf.sprintf "%d %s\n" pid (Unix.gethostname ()));
  busy "dead owner";
  Unix.unlink lock;
  ignore (M.scan m);
  Alcotest.(check bool) "offline removal allows new acquisition" false
    (Sys.file_exists lock);
  List.iter (fun (label,content) ->
    save content; busy label; Unix.unlink lock)
    ["live owner",Printf.sprintf "%d %s\n" (Unix.getpid ()) (Unix.gethostname ());
     "foreign host",Printf.sprintf "%d foreign.invalid\n" pid;
     "malformed","not-a-pid host\n";
     "oversized",String.make 2048 'x';
     "empty",""];
  Unix.mkfifo lock 0o600;
  busy "FIFO"; Unix.unlink lock;
  Unix.mkdir lock 0o700;
  busy "directory"; Unix.rmdir lock;
  let target=Filename.concat root "other-lock" in
  Out_channel.with_open_bin target (fun out ->
    Printf.fprintf out "%d %s\n" pid (Unix.gethostname ()));
  Unix.symlink target lock;
  busy "symlink"; Unix.unlink lock;
  Alcotest.(check bool) "symlink target retained" true (Sys.file_exists target);
  Unix.unlink target;
  ignore (M.scan m))

let () =
if Array.length Sys.argv=3 && Sys.argv.(1)="--writer-probe" then (
  let code=Eio_main.run (fun env ->
    let path=Eio.Path.(Eio.Stdenv.fs env / Sys.argv.(2)) in
    let m=M.open_dir path in
    try M.with_writer_lock m (fun () -> 0)
    with M.Writer_lock_busy _ -> 42) in
  exit code);
Eio_main.run (fun env ->
  Alcotest.run "imap-maildir" [
    "durability", [
      Alcotest.test_case "metadata lock recovery and foreign owners" `Quick
        (fun () -> test_metadata_lock_recovery env);
      Alcotest.test_case "same-name replacement" `Quick (fun () -> test_replaced_occurrence env);
      Alcotest.test_case "standard Dovecot metadata" `Quick (fun () -> test_standard_format env);
      Alcotest.test_case "keyword and date limits" `Quick (fun () -> test_keyword_limits env);
      Alcotest.test_case "invalid maps and legacy metadata" `Quick (fun () -> test_invalid_mapping env);
      Alcotest.test_case "keyword file tolerance and letter order" `Quick
        (fun () -> test_keyword_file_tolerance env);
      Alcotest.test_case "flag order and duplicate identity" `Quick (fun () -> test_flag_order env);
      Alcotest.test_case "restart and flags" `Quick (fun () -> test_restart env);
      Alcotest.test_case "failure recovery" `Quick
        (fun () -> test_failure_recovery env);
      Alcotest.test_case "paged inventory" `Quick
        (fun () -> test_paged_inventory env);
      Alcotest.test_case "paged duplicate identity" `Quick
        (fun () -> test_paged_duplicate_id env);
      Alcotest.test_case "external mtime date and change" `Quick
        (fun () -> test_external_mtime_date env);
      Alcotest.test_case "exclusive process writer lease" `Quick
        (fun () -> test_writer_lock env)];
    "review fixes", [
      Alcotest.test_case "supplied ID flag variants" `Quick
        (fun () -> test_supplied_id_variants env);
      Alcotest.test_case "dot, directory and symlink entries" `Quick
        (fun () -> test_ignored_entries env);
      Alcotest.test_case "find by name" `Quick
        (fun () -> test_find_by_name env);
      Alcotest.test_case "epoch INTERNALDATE" `Quick
        (fun () -> test_epoch_date env);
      Alcotest.test_case "publication never replaces" `Quick
        (fun () -> test_no_overwrite env);
      Alcotest.test_case "stale occurrence exception" `Quick
        (fun () -> test_stale_occurrence env);
      Alcotest.test_case "external keyword file change" `Quick
        (fun () -> test_keyword_file_change env);
      Alcotest.test_case "cleanup keeps the original exception" `Quick
        (fun () -> test_cleanup_keeps_exception env)]])
