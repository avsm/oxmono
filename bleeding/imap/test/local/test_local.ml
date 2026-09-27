module M = Maildir
module L = Local_inventory

let flag s = match Mail_flag.Imap_flag.of_wire s with
  | Ok flag -> flag | Error e -> Alcotest.fail e
let wires flags = List.map Mail_flag.Imap_flag.to_wire flags
let with_root f =
  let root=Filename.temp_file "imap-local-" "" in
  Sys.remove root;
  Unix.mkdir root 0o700;
  let rec remove path =
    if Sys.is_directory path then (
      Sys.readdir path |> Array.iter (fun name ->
        remove (Filename.concat path name));
      Unix.rmdir path)
    else Sys.remove path in
  Fun.protect ~finally:(fun () -> remove root) (fun () ->
    Unix.mkdir (Filename.concat root "spool") 0o700;
    f root)
let spool env root = Eio.Path.(Eio.Stdenv.fs env / root / "spool")
let maildir env root = M.open_dir Eio.Path.(Eio.Stdenv.fs env / root / "mail")
let spool_names root =
  Sys.readdir (Filename.concat root "spool") |> Array.to_list
let date s = match Imap.Internal_date.of_string s with
  | Ok date -> date | Error e -> Alcotest.fail e
let date_string = function
  | Ok date -> Imap.Internal_date.to_string date
  | Error e -> Alcotest.fail e
let append_x ?inventory ?id m flags =
  L.append ?inventory m ?id ~source:(Eio.Flow.string_source "x") ~length:1L
    ~flags ()
let rejects label f =
  match f () with
  | exception Failure _ -> ()
  | _ -> Alcotest.fail (label ^ " accepted")

let test_paged_inventory env = with_root (fun root ->
  let spool_dir=spool env root in
  let m=maildir env root in
  let raw="Subject: duplicate\r\n\r\nEqual bytes\r\n" in
  let expected=List.init 43 (fun i ->
    let flags=if i mod 2=0 then [flag "\\Seen";flag "Custom"] else [] in
    M.append m ~source:(Eio.Flow.string_source raw)
      ~length:(Int64.of_int (String.length raw)) ~flags () ) in
  let expected_ids=List.map (fun (o:M.occurrence) -> o.id) expected
    |> List.sort String.compare in
  L.with_pages ~spool_dir m (fun view ->
    Alcotest.(check int) "one staging file in the spool" 1
      (List.length (spool_names root));
    Alcotest.(check (list string)) "nothing staged in the Maildir" []
      (Sys.readdir (Filename.concat root "mail/tmp") |> Array.to_list);
    Alcotest.(check int64) "complete count" 43L (L.count view);
    (try ignore (L.page view ~limit:0 ());
      Alcotest.fail "zero page accepted"
     with Invalid_argument _ -> ());
    let rec pages after seen max_page =
      let p=L.page view ?after ~limit:7 () in
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
    let first=L.page view ~limit:1 () in
    let o=List.hd first.occurrences in
    Alcotest.(check string) "staged stable first ID"
      (List.hd expected_ids) o.id;
    Alcotest.(check bool) "staged lookup" true
      (Option.is_some (L.find view ~id:o.id));
    Alcotest.(check bool) "missing staged lookup" true
      (Option.is_none (L.find view ~id:"missing"));
    let flagged=List.find (fun (o:M.occurrence) ->
      List.exists ((=) "Custom") (wires o.flags)) expected in
    let staged=Option.get (L.find view ~id:flagged.id) in
    Alcotest.(check (list string)) "staged flags" (wires flagged.flags)
      (wires staged.flags);
    let digest=Digestif.SHA256.(to_hex (digest_string raw)) in
    Alcotest.(check string) "indexed occurrence hash" digest
      (L.sha256 ~inventory:view m staged);
    (match L.with_unchanged_occurrence ~inventory:view m staged
        (fun () -> M.set_flags m staged [flag "\\Seen";flag "Later"]) with
     | Error `Changed -> ()
     | Ok _ -> Alcotest.fail "indexed check accepted a flag rename");
    let _=M.append m ~source:(Eio.Flow.string_source raw)
      ~length:(Int64.of_int (String.length raw)) ~flags:[] () in
    Alcotest.(check int64) "snapshot excludes later append" 43L
      (L.count view);
    (try ignore (L.append ~inventory:view m ~id:staged.id
      ~source:(Eio.Flow.string_source raw)
      ~length:(Int64.of_int (String.length raw)) ~flags:[] ());
      Alcotest.fail "staged duplicate ID accepted"
     with Failure _ -> ());
    let fresh=M.reserve_id () in
    ignore (L.append ~inventory:view m ~id:fresh
      ~source:(Eio.Flow.string_source raw)
      ~length:(Int64.of_int (String.length raw)) ~flags:[] ());
    (try ignore (L.append ~inventory:view m ~id:fresh
      ~source:(Eio.Flow.string_source raw)
      ~length:(Int64.of_int (String.length raw)) ~flags:[flag "\\Seen"] ());
      Alcotest.fail "new duplicate ID accepted"
     with Failure _ -> ());
    Alcotest.(check int64) "snapshot remains fixed after indexed appends"
      43L (L.count view);
    let other=maildir env root in
    (try ignore (L.append ~inventory:view other ~id:(M.reserve_id ())
      ~source:(Eio.Flow.string_source raw)
      ~length:(Int64.of_int (String.length raw)) ~flags:[] ());
      Alcotest.fail "inventory accepted another Maildir handle"
     with Invalid_argument _ -> ()));
  Alcotest.(check (list string)) "staging file removed" [] (spool_names root);
  let view=L.with_pages ~spool_dir m Fun.id in
  (try ignore (L.count view); Alcotest.fail "expired inventory accepted"
   with Invalid_argument _ -> ()))

let test_paged_duplicate_id env = with_root (fun root ->
  let m=maildir env root in
  let a=M.append m ~source:(Eio.Flow.string_source "x") ~length:1L
    ~flags:[] () in
  let mail=Filename.concat root "mail" in
  let source=Filename.concat (Filename.concat mail "new") a.filename in
  let duplicate=Filename.concat (Filename.concat mail "cur")
    (a.id ^ ":2,S") in
  let input=open_in_bin source and output=open_out_bin duplicate in
  Fun.protect ~finally:(fun () -> close_in input; close_out output)
    (fun () -> output_char output (input_char input));
  (try L.with_pages ~spool_dir:(spool env root) m (fun _ ->
     Alcotest.fail "duplicate ID accepted")
   with Failure message ->
     Alcotest.(check string) "duplicate identity diagnosed"
       ("Imap_maildir: duplicate occurrence identity " ^ a.id) message);
  Alcotest.(check (list string)) "failed staging file removed" []
    (spool_names root))

let test_external_mtime_date env = with_root (fun root ->
  let m=maildir env root in
  let original=M.append m ~source:(Eio.Flow.string_source "x")
    ~length:1L ~flags:[] () in
  let filename=Filename.concat (Filename.concat root "mail/new")
    original.filename in
  Unix.utimes filename 1709164800. 1709164800.;
  let local=match M.scan m with
    | [local] -> local | _ -> Alcotest.fail "external file missing" in
  Alcotest.(check string) "external mtime becomes UTC INTERNALDATE"
    "29-Feb-2024 00:00:00 +0000"
    (date_string (Local_date.of_occurrence local));
  L.with_pages ~spool_dir:(spool env root) m (fun inventory ->
    let staged=Option.get (L.find inventory ~id:local.id) in
    Alcotest.(check bool) "paged inventory retains mtime" true
      (local.mtime=staged.mtime);
    Unix.utimes filename 1709164801. 1709164801.;
    match L.with_unchanged_occurrence ~inventory m staged
      (fun () -> ()) with
    | Error `Changed -> ()
    | Ok () -> Alcotest.fail "changed mtime accepted as same source"))

let test_staged_variants env = with_root (fun root ->
  let m=maildir env root in
  let staged_later=M.reserve_id () in
  L.with_pages ~spool_dir:(spool env root) m (fun inventory ->
    Eio.Path.save ~create:(`Exclusive 0o600)
      Eio.Path.(Eio.Stdenv.fs env / root / "mail" / "cur" /
        (staged_later ^ ":2,S")) "x";
    rejects "variant published after staging" (fun () ->
      append_x ~inventory m ~id:staged_later []));
  Alcotest.(check bool) "nothing published in new" false
    (List.mem staged_later
      (Sys.readdir (Filename.concat root "mail/new") |> Array.to_list)))

let test_ignored_entries env = with_root (fun root ->
  let m=maildir env root in
  let real=append_x m [flag "\\Seen"] in
  let mail=Filename.concat root "mail" in
  let save dir name =
    Out_channel.with_open_bin (Filename.concat (Filename.concat mail dir) name)
      (fun out -> output_string out "x") in
  save "new" ".DS_Store";
  save "cur" ".hidden:2,S";
  Unix.mkdir (Filename.concat (Filename.concat mail "cur")
    (M.reserve_id () ^ ":2,S")) 0o700;
  let link=Filename.concat (Filename.concat mail "new") (M.reserve_id ()) in
  Unix.symlink (Filename.concat (Filename.concat mail "cur") real.filename)
    link;
  Unix.mkfifo (Filename.concat (Filename.concat mail "new") "fifo") 0o600;
  L.with_pages ~spool_dir:(spool env root) m (fun view ->
    Alcotest.(check int64) "staging skips the same entries as scan" 1L
      (L.count view));
  Unix.unlink link)

let test_recover env = with_root (fun root ->
  let spool_dir=spool env root in
  let stage="local-inventory-im-0123456789abcdef0123456789abcdef.sqlite3" in
  let keep=["local-inventory-im-short.sqlite3";"imap-im-0123";
    "local-inventory-im-0123456789abcdef0123456789abcdeg.sqlite3"] in
  List.iter (fun name ->
    Eio.Path.save ~create:(`Exclusive 0o600) Eio.Path.(spool_dir / name)
      "interrupted") (stage::keep);
  Alcotest.(check (list string)) "orphan staging file" [stage]
    (L.recover spool_dir);
  Alcotest.(check (list string)) "other spool files kept"
    (List.sort String.compare keep)
    (List.sort String.compare (spool_names root)))

let test_dates () =
  let check label expected mtime =
    Alcotest.(check string) label expected
      (date_string (Local_date.of_mtime mtime)) in
  check "epoch" " 1-Jan-1970 00:00:00 +0000" 0.;
  check "whole seconds round down" "26-Sep-2025 10:04:56 +0000"
    1758881096.75;
  List.iter (fun mtime ->
    match Local_date.of_mtime mtime with
    | Error _ -> ()
    | Ok _ -> Alcotest.failf "mtime %g accepted" mtime)
    [Float.nan;Float.infinity;1e13;-1e13];
  let seconds s = match Local_date.to_mtime (date s) with
    | Ok mtime -> mtime | Error e -> Alcotest.fail e in
  Alcotest.(check (float 0.)) "zone applied" 1758881096.
    (seconds "26-Sep-2025 12:34:56 +0230");
  Alcotest.(check (float 0.)) "epoch date" 0.
    (seconds "01-Jan-1970 00:00:00 +0000");
  (match Local_date.to_mtime (date "31-Dec-2016 23:59:60 +0000") with
   | Error _ -> ()
   | Ok _ -> Alcotest.fail "leap second has a modification time")

let () =
  Eio_main.run (fun env ->
    Alcotest.run "imap-sync-local" [
      "inventory", [
        Alcotest.test_case "paged inventory" `Quick
          (fun () -> test_paged_inventory env);
        Alcotest.test_case "paged duplicate identity" `Quick
          (fun () -> test_paged_duplicate_id env);
        Alcotest.test_case "external mtime date and change" `Quick
          (fun () -> test_external_mtime_date env);
        Alcotest.test_case "variant published after staging" `Quick
          (fun () -> test_staged_variants env);
        Alcotest.test_case "dot, directory and symlink entries" `Quick
          (fun () -> test_ignored_entries env);
        Alcotest.test_case "spool recovery" `Quick
          (fun () -> test_recover env)];
      "dates", [
        Alcotest.test_case "mtime and INTERNALDATE" `Quick test_dates]])
