module M = Maildir
let ok = function
  | Ok x -> x
  | Error e -> Alcotest.failf "unexpected Maildir error: %a" M.pp_error e
let error label = function
  | Ok _ -> Alcotest.fail (label ^ " accepted")
  | Error e -> e
let check_error label expected result =
  let actual=error label result in
  if actual<>expected then
    Alcotest.failf "%s: expected %a, got %a" label M.pp_error expected
      M.pp_error actual
let message e = Format.asprintf "%a" M.pp_error e
let open_dir path = ok (M.open_dir path)
let scan m = ok (M.scan m)
let find m ~id = ok (M.find m ~id)
let append m ?id ~source ~length ~flags ?mtime () =
  M.with_writer m (fun w -> M.append w ?id ~source ~length ~flags ?mtime ())
let set_flags m o flags = M.with_writer m (fun w -> M.set_flags w o flags)
let remove m o = M.with_writer m (fun w -> M.remove w o)
let recover m = M.with_writer m M.recover
let put m ?id ~source ~length ~flags ?mtime () =
  ok (append m ?id ~source ~length ~flags ?mtime ())
let relabel m o flags = ok (set_flags m o flags)
let flag s = match Mail_flag.Imap_flag.of_wire s with
  | Ok flag -> flag | Error e -> Alcotest.fail e
let wires flags = List.map Mail_flag.Imap_flag.to_wire flags
let with_root f =
  let root=Filename.temp_file "maildir-" "" in
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
  let m=open_dir Eio.Path.(Eio.Stdenv.fs env / root) in
  let flags=[flag "tag";flag "\\Seen";flag "\\Flagged";flag "\\Seen"] in
  let a=put m ~source:(Eio.Flow.string_source "body") ~length:4L ~flags () in
  Eio.Switch.run (fun sw ->
    Alcotest.(check string) "fresh append is readable" "body"
      (read_all (M.open_message m ~sw a) 4));
  let b=relabel m a (List.rev flags) in
  Eio.Switch.run (fun sw ->
    Alcotest.(check string) "fresh flag update is readable" "body"
      (read_all (M.open_message m ~sw b) 4)))

let test_standard_format env = with_root (fun root ->
  let path=Eio.Path.(Eio.Stdenv.fs env / root) in
  let m=open_dir path in
  List.iter (fun name -> Alcotest.(check bool) "no custom sidecar directory" false
    (Sys.file_exists (Filename.concat root name))) [".imap-flags";".imap-dates"];
  Eio.Path.save ~create:(`Exclusive 0o600) Eio.Path.(path / "dovecot-keywords")
    "0 Custom\n25 LastSlot\n";
  let original="external,S=1,W=1:2,PSa,custom=value" in
  Eio.Path.save ~create:(`Exclusive 0o600) Eio.Path.(path / "cur" / original) "x";
  let occurrence=List.hd (scan m) in
  Alcotest.(check (list string)) "mapped external flags" ["\\Seen";"Custom"]
    (wires occurrence.flags);
  let changed=relabel m occurrence [flag "\\Flagged"] in
  Alcotest.(check string) "basename, Passed and extension preserved"
    "external,S=1,W=1:2,FP,custom=value" changed.filename;
  Alcotest.(check bool) "mtime preserved by flags" true
    (changed.mtime=occurrence.mtime);
  let lock=Eio.Path.(path / "dovecot-uidlist.lock") in
  Eio.Path.save ~create:(`Exclusive 0o600) lock
    (Printf.sprintf "%d %s\n" (Unix.getpid ()) (Unix.gethostname ()));
  (try ignore (set_flags m changed []);
     Alcotest.fail "live metadata lock ignored"
   with M.Metadata_lock_busy _ -> ());
  Eio.Path.unlink lock;
  Alcotest.(check string) "content survives lock contention" "x"
    (Eio.Path.load Eio.Path.(path / "cur" / changed.filename)))

let test_replaced_occurrence env = with_root (fun root ->
  let path=Eio.Path.(Eio.Stdenv.fs env / root) in
  let m=open_dir path in
  let original=put m ~source:(Eio.Flow.string_source "x") ~length:1L
    ~flags:[] () in
  let target=Eio.Path.(path / "new" / original.filename) in
  Eio.Path.rename target Eio.Path.(path / "tmp" / "held-original");
  Eio.Path.save ~create:(`Exclusive 0o600) target "x";
  Unix.utimes (Eio.Path.native_exn target) original.mtime original.mtime;
  Alcotest.(check bool) "same-name replacement is stale" true
    (M.with_unchanged_occurrence m original (fun () -> ())=Error `Changed))

let test_keyword_limits env = with_root (fun root ->
  let path=Eio.Path.(Eio.Stdenv.fs env / root) in
  let m=open_dir path in
  let keywords=List.init 26 (fun i -> flag (Printf.sprintf "Key%d" i)) in
  let a=put m ~source:(Eio.Flow.string_source "x") ~length:1L
    ~flags:keywords () in
  let before=Eio.Path.load Eio.Path.(path / "dovecot-keywords") in
  List.iter (fun (flags,expected) ->
    check_error "unsupported flags" expected
      (append m ~source:(Eio.Flow.string_source "x") ~length:1L ~flags ()))
    [[flag "TwentySeventh"],M.Too_many_keywords (flag "TwentySeventh");
     [flag "\\FutureFlag"],M.Unsupported_flag (flag "\\FutureFlag")];
  Alcotest.(check int) "overflow leaves inventory intact" 1
    (List.length (scan m));
  Alcotest.(check string) "overflow leaves mapping intact" before
    (Eio.Path.load Eio.Path.(path / "dovecot-keywords"));
  let changed=relabel m a [flag "key0"] in
  Alcotest.(check int) "case alias reuses slot" 1 (List.length changed.flags);
  List.iter (fun mtime ->
    match append m ~source:(Eio.Flow.string_source "x") ~length:1L
      ~flags:[] ~mtime () with
    | Error (M.Unrepresentable_date _) -> ()
    | _ -> Alcotest.fail "unrepresentable time published")
    [Float.nan;Float.infinity];
  Alcotest.(check int) "invalid date leaves inventory intact" 1
    (List.length (scan m)))

let test_invalid_mapping env = with_root (fun root ->
  let path=Eio.Path.(Eio.Stdenv.fs env / root) in
  let m=open_dir path in
  List.iter (fun text ->
    Eio.Path.save ~create:(`Or_truncate 0o600) Eio.Path.(path / "dovecot-keywords") text;
    match M.scan m with
    | Error (M.Keyword_map _) -> ()
    | _ -> Alcotest.fail "invalid keyword map accepted")
    ["0 A\n0 B\n";"0 A\n1 a\n";"26 A\n";"00 A\n";"0\n";"0 A\nx\n"];
  Eio.Path.unlink Eio.Path.(path / "dovecot-keywords");
  Eio.Path.save ~create:(`Exclusive 0o600) Eio.Path.(path / "cur" / "external:2,a") "x";
  let e=error "unmapped keyword" (M.scan m) in
  Alcotest.(check bool) "unmapped letter is typed" true
    (e=M.Unknown_letter {file="external:2,a";letter='a'});
  Alcotest.(check string) "unmapped letter names file and letter"
    "external:2,a references unmapped keyword letter a" (message e);
  Eio.Path.unlink Eio.Path.(path / "cur" / "external:2,a");
  Eio.Path.save ~create:(`Exclusive 0o600)
    Eio.Path.(path / "cur" / "external:2,S!") "x";
  let e=error "unknown letter" (M.scan m) in
  Alcotest.(check bool) "unknown letter is typed" true
    (e=M.Unknown_letter {file="external:2,S!";letter='!'});
  Alcotest.(check string) "unknown letter names file and letter"
    "external:2,S! has invalid Maildir flag letter '!'" (message e);
  Eio.Path.unlink Eio.Path.(path / "cur" / "external:2,S!");
  Eio.Path.mkdir ~perm:0o700 Eio.Path.(path / ".imap-flags");
  Eio.Path.save ~create:(`Exclusive 0o600) Eio.Path.(path / ".imap-flags" / "old") "data";
  check_error "legacy metadata" (M.Legacy_metadata ".imap-flags")
    (M.open_dir path))

let test_keyword_file_tolerance env = with_root (fun root ->
  let path=Eio.Path.(Eio.Stdenv.fs env / root) in
  let m=open_dir path in
  let keywords=Eio.Path.(path / "dovecot-keywords") in
  Eio.Path.save ~create:(`Exclusive 0o600)
    Eio.Path.(path / "cur" / "ext:2,TSDFRab") "x";
  List.iter (fun text ->
    Eio.Path.save ~create:(`Or_truncate 0o600) keywords text;
    match scan m with
    | [o] ->
      Alcotest.(check (list string)) ("flags sorted for " ^ String.escaped text)
        ["\\Seen";"\\Answered";"\\Flagged";"\\Deleted";"\\Draft";"A";"B"]
        (wires o.flags)
    | _ -> Alcotest.fail "one occurrence expected")
    ["0 A\n1 B";"\n0 A\n\n1 B\n\n";"1 B\n0 A\n"];
  let o=List.hd (scan m) in
  let changed=relabel m o [flag "B";flag "\\Seen";flag "C";flag "A"] in
  Alcotest.(check string) "letters sorted with system letters first"
    "ext:2,Sabc" changed.filename;
  Alcotest.(check string) "added keyword appended"
    "0 A\n1 B\n2 C\n" (Eio.Path.load keywords))

let test_restart env = with_root (fun root ->
  let fs=Eio.Stdenv.fs env in
  let path=Eio.Path.(fs / root) in
  let m=open_dir path in
  let raw="Subject: same\r\n\r\nExact bytes\r\n" in
  (* 26-Sep-2025 10:04:56 +0000 *)
  let date=1758881096. in
  let flags=[flag "\\Seen";flag "A-Custom"] in
  let reserved=M.reserve_id () in
  let a=put m ~id:reserved ~source:(Eio.Flow.string_source raw)
    ~length:(Int64.of_int (String.length raw)) ~flags
    ~mtime:date () in
  let b=put m ~source:(Eio.Flow.string_source raw)
    ~length:(Int64.of_int (String.length raw)) ~flags:[] () in
  Alcotest.(check bool) "distinct occurrences" true (a.id<>b.id);
  Alcotest.(check bool) "new vs cur" true
    (a.location=Cur && b.location=New);
  let m=open_dir path in
  Alcotest.(check bool) "reserved ID survives restart" true
    (Option.is_some (find m ~id:reserved));
  check_error "duplicate reserved ID" (M.Duplicate_identity reserved)
    (append m ~id:reserved ~source:(Eio.Flow.string_source raw)
      ~length:(Int64.of_int (String.length raw)) ~flags ());
  let inventory=scan m in
  Alcotest.(check int) "same-content entries" 2 (List.length inventory);
  let a=List.find (fun x -> x.M.id=a.id) inventory in
  Alcotest.(check (list string)) "flags survive restart"
    ["\\Seen";"A-Custom"] (wires a.flags);
  Alcotest.(check (float 0.)) "date survives restart" date a.mtime;
  let expected=Digestif.SHA256.(to_hex (digest_string raw)) in
  Alcotest.(check string) "content digest" expected (M.sha256 m a);
  Eio.Switch.run (fun sw ->
    let input=M.open_message m ~sw a in
    Alcotest.(check string) "exact content" raw
      (read_all input (String.length raw)));
  let updated=relabel m a [flag "\\Flagged";flag "NewTag"] in
  Alcotest.(check string) "stable id after rename" a.id updated.id;
  ignore (relabel m updated [flag "\\Flagged";flag "MoreTag"]);
  let m=open_dir path in
  let after=scan m |> List.find (fun x -> x.M.id=a.id) in
  Alcotest.(check (list string)) "keyword-only change"
    ["\\Flagged";"MoreTag"] (wires after.flags);
  let old_path=Filename.concat (Filename.concat root "cur") after.filename in
  let external_name=after.id ^ ":2,S" in
  Unix.rename old_path (Filename.concat (Filename.concat root "cur") external_name);
  let changed=scan m |> List.find (fun x -> x.M.id=a.id) in
  Alcotest.(check (list string)) "external flag rename removes omitted keyword"
    ["\\Seen"] (wires changed.flags);
  Alcotest.(check (float 0.)) "flag rename keeps date" date changed.mtime;
  let updated=relabel m changed [flag "\\Flagged";flag "FinalTag"] in
  ignore (recover m);
  Alcotest.(check int) "two occurrences after recovery" 2
    (List.length (scan m));
  remove m updated;
  Alcotest.(check bool) "removed date sidecar" false
    (Sys.file_exists (Filename.concat
      (Filename.concat root ".imap-dates") reserved));
  Alcotest.(check int) "targeted removal" 1 (List.length (scan m)))

let test_failure_recovery env = with_root (fun root ->
  let fs=Eio.Stdenv.fs env in
  let path=Eio.Path.(fs / root) in
  let m=open_dir path in
  (try ignore (append m ~source:(Eio.Flow.string_source "short")
    ~length:10L ~flags:[flag "Lost"] ());
    Alcotest.fail "short source accepted"
   with End_of_file -> ());
  Alcotest.(check int) "no partial visible message" 0
    (List.length (scan m));
  let temp=".tmp-0123456789abcdef0123456789abcdef" in
  let oc=open_out_bin (Filename.concat (Filename.concat root "tmp") temp) in
  output_string oc "interrupted"; close_out oc;
  let recovery=recover (open_dir path) in
  Alcotest.(check (list string)) "orphan tmp" [temp]
    (List.sort String.compare recovery.removed_temporary);
  let source=Eio.Flow.string_source "abcdef" in
  let o=put m ~source ~length:3L ~flags:[] () in
  Eio.Switch.run (fun sw ->
    let input=M.open_message m ~sw o in
    Alcotest.(check string) "exact bounded stream" "abc" (read_all input 3));
  Alcotest.(check int64) "exact length" 3L o.length)

let save path text = Eio.Path.save ~create:(`Exclusive 0o600) path text
let try_append_x ?id ?mtime m flags =
  append m ?id ~source:(Eio.Flow.string_source "x") ~length:1L ~flags ?mtime ()
let append_x ?id ?mtime m flags = ok (try_append_x ?id ?mtime m flags)

let test_supplied_id_variants env = with_root (fun root ->
  let path=Eio.Path.(Eio.Stdenv.fs env / root) in
  let m=open_dir path in
  let own=M.reserve_id () in
  ignore (append_x m ~id:own [flag "\\Seen"]);
  check_error "flagless republication of a flagged ID"
    (M.Duplicate_identity own) (try_append_x m ~id:own []);
  let external_id=M.reserve_id () in
  save Eio.Path.(path / "cur" / (external_id ^ ":2,FS")) "x";
  check_error "external flag variant" (M.Duplicate_identity external_id)
    (try_append_x m ~id:external_id [flag "\\Draft"]);
  Alcotest.(check bool) "nothing published in new" false
    (List.exists (fun name -> name=external_id)
      (Eio.Path.read_dir Eio.Path.(path / "new"))))

let test_ignored_entries env = with_root (fun root ->
  let path=Eio.Path.(Eio.Stdenv.fs env / root) in
  let m=open_dir path in
  let real=append_x m [flag "\\Seen"] in
  save Eio.Path.(path / "new" / ".DS_Store") "x";
  save Eio.Path.(path / "cur" / ".hidden:2,S") "x";
  let dir_id=M.reserve_id () and link_id=M.reserve_id () in
  Eio.Path.mkdir ~perm:0o700 Eio.Path.(path / "cur" / (dir_id ^ ":2,S"));
  Unix.symlink (Filename.concat (Filename.concat root "cur") real.filename)
    (Filename.concat (Filename.concat root "new") link_id);
  Unix.mkfifo (Filename.concat (Filename.concat root "new") "fifo") 0o600;
  Alcotest.(check (list string)) "only the regular message" [real.id]
    (List.map (fun (o:M.occurrence) -> o.id) (scan m));
  Alcotest.(check int) "fold skips the same entries" 1
    (ok (M.fold m ~init:0 ~f:(fun n _ -> n+1)));
  List.iter (fun id ->
    Alcotest.(check bool) ("no occurrence for " ^ id) true
      (find m ~id=None)) [dir_id;link_id;".DS_Store";".hidden"];
  Alcotest.(check bool) "real message found" true
    (Option.is_some (find m ~id:real.id));
  Unix.unlink (Filename.concat (Filename.concat root "new") link_id))

let test_find_by_name env = with_root (fun root ->
  let path=Eio.Path.(Eio.Stdenv.fs env / root) in
  let m=open_dir path in
  let fresh=append_x m [] and flagged=append_x m [flag "\\Flagged"] in
  save Eio.Path.(path / "cur" / "broken:2,S!") "x";
  check_error "scan with an unknown letter"
    (M.Unknown_letter {file="broken:2,S!";letter='!'}) (M.scan m);
  List.iter (fun (o:M.occurrence) ->
    match find m ~id:o.id with
    | Some found -> Alcotest.(check string) "found by name" o.filename
        found.filename
    | None -> Alcotest.fail ("missing " ^ o.id)) [fresh;flagged];
  Eio.Path.unlink Eio.Path.(path / "cur" / "broken:2,S!");
  save Eio.Path.(path / "cur" / (fresh.id ^ ":2,S")) "x";
  let e=error "duplicate found once" (M.find m ~id:fresh.id) in
  Alcotest.(check bool) "duplicate is typed" true
    (e=M.Duplicate_identity fresh.id);
  Alcotest.(check string) "duplicate names the ID"
    ("duplicate occurrence identity " ^ fresh.id) (message e))

let test_epoch_date env = with_root (fun root ->
  let m=open_dir Eio.Path.(Eio.Stdenv.fs env / root) in
  let o=append_x m [] ~mtime:0. in
  Alcotest.(check (float 0.)) "epoch mtime" 0. o.mtime;
  Alcotest.(check (float 0.)) "epoch mtime after scan" 0.
    (List.hd (scan m)).mtime)

let test_duplicate_refused env = with_root (fun root ->
  let path=Eio.Path.(Eio.Stdenv.fs env / root) in
  let m=open_dir path in
  let o=append_x m [] in
  check_error "republication of a published ID" (M.Duplicate_identity o.id)
    (try_append_x m ~id:o.id []);
  let occupied=Eio.Path.(path / "cur" / (o.id ^ ":2,S")) in
  save occupied "other";
  check_error "flag change onto a duplicate identity"
    (M.Target_exists (o.id ^ ":2,S")) (set_flags m o [flag "\\Seen"]);
  Alcotest.(check string) "existing file kept" "other" (Eio.Path.load occupied);
  Alcotest.(check string) "source kept" "x"
    (Eio.Path.load Eio.Path.(path / "new" / o.filename)))

let test_stale_occurrence env = with_root (fun root ->
  let path=Eio.Path.(Eio.Stdenv.fs env / root) in
  let m=open_dir path in
  let o=append_x m [] in
  Eio.Path.rename Eio.Path.(path / "new" / o.filename)
    Eio.Path.(path / "cur" / (o.id ^ ":2,S"));
  List.iter (fun (label,f) ->
    match f () with
    | exception M.Stale_occurrence -> ()
    | _ -> Alcotest.fail (label ^ " accepted a stale occurrence"))
    ["set_flags",(fun () -> ignore (set_flags m o []));
     "remove",(fun () -> remove m o);
     "sha256",(fun () -> ignore (M.sha256 m o));
     "open_message",(fun () ->
       Eio.Switch.run (fun sw -> ignore (M.open_message m ~sw o)))])

let test_keyword_file_change env = with_root (fun root ->
  let path=Eio.Path.(Eio.Stdenv.fs env / root) in
  let m=open_dir path in
  let o=append_x m [flag "First"] in
  let keywords=Eio.Path.(path / "dovecot-keywords") in
  let replacement=Eio.Path.(path / "tmp" / "keywords") in
  save replacement "0 Second\n";
  Eio.Path.rename replacement keywords;
  Alcotest.(check (list string)) "external mapping change observed"
    ["Second"] (wires (List.hd (scan m)).flags);
  Alcotest.(check bool) "old observation is stale" true
    (M.with_unchanged_occurrence m o (fun () -> ())=Error `Changed))

module Chmod_then_fail = struct
  type t = string
  let read_methods = []
  let single_read directory _ = Unix.chmod directory 0o500; raise Exit
end

let test_cleanup_keeps_exception env = with_root (fun root ->
  let m=open_dir Eio.Path.(Eio.Stdenv.fs env / root) in
  let tmp=Filename.concat root "tmp" in
  if Unix.geteuid ()<>0 then (
    let source=Eio.Resource.T (tmp,
      Eio.Flow.Pi.source (module Chmod_then_fail)) in
    (match append m ~source ~length:1L ~flags:[] () with
     | exception Exit -> ()
     | exception exn -> Unix.chmod tmp 0o700; raise exn
     | _ -> Alcotest.fail "failing source published");
    Unix.chmod tmp 0o700;
    Alcotest.(check int) "unremovable temporary left for recovery" 1
      (List.length (recover m).removed_temporary)))

let test_expired_writer env = with_root (fun root ->
  let m=open_dir Eio.Path.(Eio.Stdenv.fs env / root) in
  let o=put m ~source:(Eio.Flow.string_source "x") ~length:1L ~flags:[] () in
  let escaped=M.with_writer m Fun.id in
  let expired label f =
    match f () with
    | exception M.Writer_expired -> ()
    | _ -> Alcotest.fail (label ^ " accepted an expired writer") in
  expired "append" (fun () ->
    ignore (M.append escaped ~source:(Eio.Flow.string_source "y")
      ~length:1L ~flags:[] ()));
  expired "check_append" (fun () ->
    ignore (M.check_append escaped ~flags:[] ()));
  expired "set_flags" (fun () ->
    ignore (M.set_flags escaped o [flag "\\Seen"]));
  expired "remove" (fun () -> M.remove escaped o);
  expired "recover" (fun () -> ignore (M.recover escaped));
  expired "of_writer" (fun () -> ignore (M.of_writer escaped));
  Alcotest.(check (list string)) "expired writer changed nothing"
    [o.filename] (List.map (fun (x:M.occurrence) -> x.filename) (scan m));
  let leaked=ref None in
  (try M.with_writer m (fun w -> leaked:=Some w; raise Exit)
   with Exit -> ());
  expired "writer after an exception" (fun () ->
    ignore (M.of_writer (Option.get !leaked)));
  M.with_writer m (fun w ->
    Alcotest.(check bool) "live writer names its Maildir" true
      (M.of_writer w==m)))

let test_typed_errors env = with_root (fun root ->
  let fs=Eio.Stdenv.fs env in
  let plain=Filename.concat root "plain" in
  Out_channel.with_open_bin plain (fun out -> output_string out "x");
  check_error "regular file as root" (M.Not_a_directory plain)
    (M.open_dir Eio.Path.(fs / plain));
  let box=Filename.concat root "box" in
  let m=open_dir Eio.Path.(fs / box) in
  let malformed=Filename.concat (Filename.concat box "cur") "a:b" in
  Out_channel.with_open_bin malformed (fun out -> output_string out "x");
  check_error "malformed name in scan" (M.Malformed_filename "a:b")
    (M.scan m);
  check_error "malformed name in fold" (M.Malformed_filename "a:b")
    (M.fold m ~init:() ~f:(fun () _ -> ()));
  Unix.unlink malformed;
  M.with_writer m (fun w ->
    let recent=flag "\\Recent" in
    check_error "Recent" (M.Unsupported_flag recent)
      (M.check_append w ~flags:[recent] ());
    (match M.check_append w ~flags:[] ~mtime:Float.nan () with
     | Error (M.Unrepresentable_date _) -> ()
     | _ -> Alcotest.fail "check_append accepted a NaN time");
    let keywords=List.init 26 (fun i -> flag (Printf.sprintf "K%d" i)) in
    ignore (ok (M.append w ~source:(Eio.Flow.string_source "x") ~length:1L
      ~flags:keywords ()));
    check_error "keyword slots" (M.Too_many_keywords (flag "Extra"))
      (M.check_append w ~flags:[flag "Extra"] ());
    Alcotest.(check bool) "check_append accepts a mapped keyword" true
      (M.check_append w ~flags:[flag "K3"] ()=Ok ()));
  let keywords=Filename.concat box "dovecot-keywords" in
  Unix.unlink keywords;
  Unix.mkdir keywords 0o700;
  (match M.scan m with
   | Error (M.Keyword_map _) -> ()
   | _ -> Alcotest.fail "directory accepted as dovecot-keywords");
  Unix.rmdir keywords;
  (match M.Keywords.parse "0 A\n0 B\n" with
   | Error (M.Keyword_map _) -> ()
   | _ -> Alcotest.fail "Keywords.parse accepted a duplicate index");
  Alcotest.(check bool) "Keywords.flags names the letter" true
    (M.Keywords.flags M.Keywords.empty ~file:"f" "a"=
      Error (M.Unknown_letter {file="f";letter='a'}));
  Alcotest.(check string) "printed target error" "target x already exists"
    (message (M.Target_exists "x"));
  let lock=Filename.concat box ".imap-writer.lock" in
  M.with_writer m ignore;
  Unix.link lock (Filename.concat root "second-link");
  (match M.with_writer m (fun _ -> ()) with
   | exception Eio.Io (M.Unusable_file _,_) -> ()
   | () -> Alcotest.fail "hard-linked writer lease accepted"))

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
  let m=open_dir path in
  let second=open_dir path in
  M.with_writer m (fun _ ->
    (try M.with_writer second (fun _ ->
       Alcotest.fail "same-process writer admitted")
     with M.Writer_lock_busy _ -> ());
    Alcotest.(check int) "other process cannot enter" 42
      (probe_writer root));
  Alcotest.(check int) "other process enters after release" 0
    (probe_writer root);
  (try M.with_writer m (fun _ -> raise Exit)
   with Exit -> ());
  Alcotest.(check int) "exception releases lease" 0
    (probe_writer root);
  let entered, entered_u=Eio.Promise.create () in
  let never, _never_u=Eio.Promise.create () in
  (try Eio.Switch.run (fun sw ->
     Eio.Fiber.fork ~sw (fun () ->
       M.with_writer m (fun _ ->
         Eio.Promise.resolve entered_u ();
         Eio.Promise.await never));
     Eio.Promise.await entered;
     Alcotest.(check int) "cancelled writer still excludes child" 42
       (probe_writer root);
     Eio.Switch.fail sw Exit)
   with Exit -> ());
  Alcotest.(check int) "cancellation releases lease" 0
    (probe_writer root);
  let m=open_dir path in
  M.with_writer m (fun _ ->
    Alcotest.(check bool) "permanent lock file" true
      (Sys.file_exists (Filename.concat root ".imap-writer.lock")));
  Alcotest.(check int) "reopened handle enters after release" 0
    (probe_writer root))

let test_metadata_lock_recovery env = with_root (fun root ->
  let m=open_dir Eio.Path.(Eio.Stdenv.fs env / root) in
  let lock=Filename.concat root "dovecot-uidlist.lock" in
  let save content=Out_channel.with_open_bin lock (fun out ->
    output_string out content) in
  let busy label =
    let before=Unix.lstat lock in
    (try ignore (scan m); Alcotest.fail (label ^ " was reclaimed")
     with M.Metadata_lock_busy _ -> ());
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
  ignore (scan m);
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
  ignore (scan m))

let () =
if Array.length Sys.argv=3 && Sys.argv.(1)="--writer-probe" then (
  let code=Eio_main.run (fun env ->
    let path=Eio.Path.(Eio.Stdenv.fs env / Sys.argv.(2)) in
    let m=open_dir path in
    try M.with_writer m (fun _ -> 0)
    with M.Writer_lock_busy _ -> 42) in
  exit code);
Eio_main.run (fun env ->
  Alcotest.run "maildir" [
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
      Alcotest.test_case "duplicate identity refused under the lock" `Quick
        (fun () -> test_duplicate_refused env);
      Alcotest.test_case "stale occurrence exception" `Quick
        (fun () -> test_stale_occurrence env);
      Alcotest.test_case "external keyword file change" `Quick
        (fun () -> test_keyword_file_change env);
      Alcotest.test_case "cleanup keeps the original exception" `Quick
        (fun () -> test_cleanup_keeps_exception env)];
    "capability and errors", [
      Alcotest.test_case "escaped writer expires" `Quick
        (fun () -> test_expired_writer env);
      Alcotest.test_case "typed errors" `Quick
        (fun () -> test_typed_errors env)]])
