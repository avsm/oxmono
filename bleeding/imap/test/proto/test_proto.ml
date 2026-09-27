let fail s = Alcotest.fail s
let expect_ok = function Ok x -> x | Error e -> fail e
let command_ok = function
  | Ok x -> x | Error e -> fail (Imap.Command.to_string e)
let wire_ok = function
  | Ok x -> x
  | Error (e:Imap.Wire.error) -> fail e.message

let test_uid_set () =
  let u n = expect_ok (Imap.Uid.of_int64 n) in
  let set=Imap.Uid_set.of_intervals [u 7L,u 9L;u 1L,u 3L;u 4L,u 5L] in
  Alcotest.(check string) "normalized" "1:5,7:9"
    (Imap.Uid_set.to_wire set);
  Alcotest.(check bool) "hole" false (Imap.Uid_set.mem (u 6L) set)

let test_uid_set_syntax () =
  let module S = Imap.Uid_set in
  let rejected ?allow_star s =
    Alcotest.(check bool) ("reject " ^ s) true
      (Result.is_error (S.of_wire ?allow_star s)) in
  List.iter rejected ["01";"1:007";"+1";"0x10";"1_0";"0b11";"0u5";"0o7";
                      "0";"*";"1:*";"4294967296";""];
  rejected ~allow_star:true "*:0";
  (match S.of_wire "1:9,x" with
   | Error message ->
       Alcotest.(check bool) "error names token" true
         (String.ends_with ~suffix:" x" message)
   | Ok _ -> fail "accepted a non-numeric endpoint");
  let star=expect_ok (S.of_wire ~allow_star:true "5:*") in
  Alcotest.(check string) "star reads as the top UID" "5:4294967295"
    (S.to_wire star);
  Alcotest.(check bool) "empty" true (S.is_empty S.empty);
  Alcotest.(check bool) "nonempty" false (S.is_empty star);
  Alcotest.(check bool) "equal after normalisation" true
    (S.equal (expect_ok (S.of_wire "3,1:2")) (expect_ok (S.of_wire "1:3")));
  Alcotest.(check bool) "ordered" true
    (S.compare (expect_ok (S.of_wire "1")) (expect_ok (S.of_wire "2")) < 0);
  Alcotest.(check string) "pp" "1:3"
    (Format.asprintf "%a" S.pp (expect_ok (S.of_wire "1,2,3")))

let test_uid_set_algebra () =
  let module S = Imap.Uid_set in
  let u n = expect_ok (Imap.Uid.of_int64 n) in
  let set s = expect_ok (S.of_wire ~allow_star:true s) in
  let wire s = if S.is_empty s then "(empty)" else S.to_wire s in
  let check name expected s = Alcotest.(check string) name expected (wire s) in
  check "of_list" "1:3,7" (S.of_list [u 7L; u 2L; u 1L; u 3L; u 2L]);
  check "add merges" "1:4" (S.add (u 4L) (set "1:3"));
  check "union" "1:9" (S.union (set "1:4") (set "5:9"));
  check "inter" "3:4,8" (S.inter (set "1:4,8:10") (set "3:8"));
  check "inter disjoint" "(empty)" (S.inter (set "1:2") (set "5:6"));
  check "diff" "1:2,5:7,10" (S.diff (set "1:10") (set "3:4,8:9"));
  check "diff at the top" "4294967294"
    (S.diff (set "4294967294:*") (set "*"));
  check "diff all" "(empty)" (S.diff (set "2:3") (set "1:5"));
  Alcotest.(check (list int64)) "to_list" [1L; 2L; 5L]
    (List.map Imap.Uid.to_int64 (S.to_list (set "5,1:2")));
  Alcotest.(check int64) "fold" 8L
    (S.fold (fun uid acc -> Int64.add acc (Imap.Uid.to_int64 uid))
       (set "1:2,5") 0L);
  let seen = ref [] in
  S.iter (fun uid -> seen := Imap.Uid.to_int64 uid :: !seen) (set "4:5");
  Alcotest.(check (list int64)) "iter ascending" [5L; 4L] !seen;
  Alcotest.check_raises "empty has no wire form"
    (Invalid_argument "Uid_set.to_wire: empty set")
    (fun () -> ignore (S.to_wire S.empty));
  let succ n = Option.map Imap.Uid.to_int64 (Imap.Uid.succ (u n)) in
  let pred n = Option.map Imap.Uid.to_int64 (Imap.Uid.pred (u n)) in
  Alcotest.(check (option int64)) "succ" (Some 2L) (succ 1L);
  Alcotest.(check (option int64)) "succ at the top" None (succ 4294967295L);
  Alcotest.(check (option int64)) "pred" (Some 1L) (pred 2L);
  Alcotest.(check (option int64)) "pred at the bottom" None (pred 1L)

let test_fragmented_literal () =
  let d=Imap.Wire.create () in
  let a=wire_ok (Imap.Wire.feed d "* 3 FETCH (BODY[] {5}\r") in
  Alcotest.(check int) "partial" 0 (List.length a);
  let b=wire_ok (Imap.Wire.feed d "\nA\r\n") in
  let c=wire_ok (Imap.Wire.feed d "\000B UID 44 FLAGS (\\Seen Seen) MODSEQ (9))\r\n") in
  let events=b@c in
  let chunks=List.filter_map (function Imap.Wire.Literal_chunk s -> Some s | _ -> None) events in
  Alcotest.(check string) "exact bytes" "A\r\n\000B" (String.concat "" chunks);
  let response=expect_ok (Imap.Response.parse_parts events) in
  (match response with
  | Imap.Response.Untagged (Fetch f) ->
      Alcotest.(check (option int64)) "uid after literal" (Some 44L) f.uid;
      Alcotest.(check (option (list string))) "flags" (Some ["\\Seen";"Seen"]) f.flags;
      Alcotest.(check (option int64)) "modseq" (Some 9L) f.modseq;
      Alcotest.(check (list (pair string int64))) "literal metadata"
        ["BODY[]",5L] f.literals
  | _ -> fail "expected FETCH");
  ignore (wire_ok (Imap.Wire.finish d))

let test_literal_status_text () =
  let d=Imap.Wire.create () in
  let events=wire_ok (Imap.Wire.feed d "* OK message ends {123}\r\n") in
  Alcotest.(check int) "no literal event" 2 (List.length events);
  (match expect_ok (Imap.Response.parse_parts events) with
  | Imap.Response.Untagged (Ok (_,text)) ->
      Alcotest.(check string) "text" "message ends {123}" text
  | _ -> fail "expected OK")

let test_binary_literal () =
  let d=Imap.Wire.create () in
  let first=wire_ok (Imap.Wire.feed d "* 1 FETCH (UID 8 BINARY[] ~{3}\r\n") in
  let second=wire_ok (Imap.Wire.feed d "\000A\255)\r\n") in
  let events=first@second in
  let chunks=List.filter_map (function Imap.Wire.Literal_chunk s -> Some s
    | _ -> None) events in
  Alcotest.(check string) "binary bytes" "\000A\255" (String.concat "" chunks);
  (match expect_ok (Imap.Response.parse_parts events) with
   | Imap.Response.Untagged (Fetch x) ->
       Alcotest.(check (list (pair string int64))) "binary literal"
         ["BINARY[]",3L] x.literals
   | _ -> fail "missing BINARY FETCH")

let test_list_literal () =
  let d=Imap.Wire.create () in
  let events=wire_ok (Imap.Wire.feed d "* LIST (\\HasNoChildren) \"/\" {5}\r\nINBOX\r\n") in
  (match expect_ok (Imap.Response.parse_parts events) with
  | Imap.Response.Untagged (List x) ->
      Alcotest.(check string) "literal mailbox" "INBOX" x.mailbox;
      Alcotest.(check (option string)) "delimiter" (Some "/") x.delimiter
  | _ -> fail "expected LIST")

let test_wire_errors () =
  (match Imap.Wire.feed (Imap.Wire.create ())
     "* 1 FETCH (BODY[] {99999999999999999999}\r\n" with
   | Error e ->
       Alcotest.(check string) "int64 overflow" "literal exceeds limit"
         e.message
   | Ok _ -> fail "framed an overflowing literal length");
  let d=Imap.Wire.create () in
  let events=wire_ok (Imap.Wire.feed d "* BYE going\r\nbad\n") in
  Alcotest.(check bool) "events before the error survive" true
    (events=[Imap.Wire.Text "* BYE going\r\n";Imap.Wire.End_of_response]);
  (match Imap.Wire.feed d "" with
   | Error e -> Alcotest.(check string) "deferred error" "LF without CR"
                  e.message
   | Ok _ -> fail "lost the deferred error");
  (match Imap.Wire.finish d with
   | Error _ -> () | Ok () -> fail "error is not sticky");
  List.iter (fun (line,payload) ->
    let d=Imap.Wire.create () in
    let events=wire_ok (Imap.Wire.feed d (line ^ payload ^ ")\r\n")) in
    Alcotest.(check bool) (line ^ " frames a literal") true
      (List.mem (Imap.Wire.Literal_start 2L) events))
    ["* ESEARCH (TAG {2}\r\n","A1";
     "* LANGUAGE ({2}\r\n","EN"]

let test_command_validation () =
  let module C = Imap.Command in
  let rejected label = function
    | Error _ -> () | Ok s -> fail (label ^ " accepted: " ^ s) in
  List.iter (fun criterion ->
    rejected "uid_search" (C.uid_search ~criterion);
    rejected "uid_search_save" (C.uid_search_save ~criterion);
    rejected "uid_search_saved" (C.uid_search_saved ~criterion);
    rejected "uid_search_partial"
      (C.uid_search_partial ~range:(1L,10L) ~criterion);
    rejected "uid_sort" (C.uid_sort ~keys:[Imap.Sort.Date,Ascending]
      ~charset:"UTF-8" ~criterion);
    rejected "uid_sort_extended" (C.uid_sort_extended ~returns:[Imap.Sort.Count]
      ~keys:[Imap.Sort.Date,Ascending] ~charset:"UTF-8" ~criterion);
    rejected "uid_thread" (C.uid_thread ~algorithm:Imap.Thread.References
      ~charset:"UTF-8" ~criterion))
    ["SUBJECT {5}";"SUBJECT {5+}";"TEXT ~{12}";"BODY {0} "];
  Alcotest.(check string) "a quoted brace is not a marker"
    "UID SEARCH SUBJECT \"{5}\""
    (command_ok (C.uid_search ~criterion:"SUBJECT \"{5}\""));
  List.iter (fun set ->
    rejected ("uid_fetch " ^ set) (C.uid_fetch ~set ~items:["UID"]);
    rejected ("uid_store " ^ set) (C.uid_store ~set ~operation:`Add
      ~silent:true ~flags:["\\Seen"]);
    rejected ("uid_copy " ^ set) (C.uid_copy ~set ~mailbox:"Archive");
    rejected ("uid_expunge " ^ set) (C.uid_expunge ~set))
    ["+1";"0x10";"1_0";"0b11";"0u5";"0o7";"01";"1:007"];
  Alcotest.(check string) "star still accepted" "UID FETCH 1:* (UID)"
    (command_ok (C.uid_fetch ~set:"1:*" ~items:["UID"]));
  rejected "8-bit login" (C.login ~username:"caf\xe9" ~password:"p");
  Alcotest.(check string) "UTF-8 login" "LOGIN \"caf\xc3\xa9\" \"p\""
    (command_ok (C.login ~username:"caf\xc3\xa9" ~password:"p"));
  rejected "8-bit mailbox" (C.create ~mailbox:"Caf\xe9");
  (match C.create ~mailbox:"Caf\xe9" with
   | Error e ->
       Alcotest.(check string) "astring error names its argument"
         "CREATE mailbox: control character or invalid UTF-8"
         (C.to_string e)
   | Ok _ -> fail "accepted an 8-bit mailbox");
  (match C.rename ~old_name:"Old" ~new_name:"New\r\n" with
   | Error {argument=Some "new_name";_} -> ()
   | Error e -> fail ("wrong RENAME argument: " ^ C.to_string e)
   | Ok _ -> fail "accepted a CR LF mailbox");
  Alcotest.(check string) "CONDSTORE alongside QRESYNC"
    "SELECT INBOX (CONDSTORE QRESYNC (7 42))"
    (command_ok (C.select ~condstore:true ~qresync:(7L,42L) "INBOX"));
  List.iter (fun entry ->
    rejected ("metadata entry " ^ entry)
      (C.getmetadata ~mailbox:"INBOX" ~entries:[entry] ());
    rejected ("metadata entry " ^ entry)
      (C.setmetadata ~mailbox:"INBOX" ~values:[entry,None]))
    ["/a//b";"/a/";"/caf\xc3\xa9";"/";"a"];
  rejected "MAXSIZE above 32 bits" (C.getmetadata ~mailbox:"INBOX"
    ~entries:["/shared/comment"] ~maxsize:4_294_967_296L ());
  rejected "case-insensitive duplicate entry" (C.setmetadata ~mailbox:"INBOX"
    ~values:["/shared/Comment",None;"/shared/comment",Some "x"]);
  rejected "empty added rights" (C.setacl ~mailbox:"INBOX" ~identifier:"bob"
    ~operation:`Add ~rights:"");
  rejected "empty removed rights" (C.setacl ~mailbox:"INBOX"
    ~identifier:"bob" ~operation:`Remove ~rights:"");
  (match C.uid_store ~set:"1" ~operation:`Add ~silent:true ~flags:["\\*"]
   with
   | Error {command; argument; reason} ->
       Alcotest.(check string) "flag error names its command" "UID STORE"
         command;
       Alcotest.(check (option string)) "flag error names its argument"
         (Some "flags") argument;
       Alcotest.(check bool) "flag error keeps its cause" true
         (String.length reason > String.length "invalid flag: ")
   | Ok _ -> fail "accepted an invalid STORE flag");
  Alcotest.(check string) "LIST-STATUS matches LIST-EXTENDED"
    (command_ok (C.list_extended ~reference:"" ~patterns:["*"]
      ~status:[Imap.Status_item.Messages] ()))
    (command_ok (C.list_status ~reference:"" ~pattern:"*"
      ~items:[Imap.Status_item.Messages]))

let test_response_review () =
  let module R = Imap.Response in
  let parse s=expect_ok (R.parse s) in
  let rejected s = match R.parse s with
    | Error _ -> () | Ok _ -> fail ("accepted: " ^ String.escaped s) in
  (match parse "* 1 FETCH (UID 3 PREVIEW \"a    b\")\r\n" with
   | R.Untagged (Fetch f) ->
       Alcotest.(check (option (option string))) "quoted spaces kept"
         (Some (Some "a    b")) f.preview;
       Alcotest.(check string) "raw is the untouched suffix"
         "FETCH (UID 3 PREVIEW \"a    b\")" f.raw
   | _ -> fail "missing FETCH");
  let started=Sys.time () in
  let braces=String.concat " " (List.init 100_000 (fun _ -> "{")) in
  ignore (R.parse ("* LIST () \"/\" x (" ^ braces ^ ")\r\n"));
  let events=Imap.Wire.Text "* 1 FETCH (UID 1" ::
    List.concat (List.init 20_000 (fun i ->
      [Imap.Wire.Text (Printf.sprintf " X%d {0}\r\n" i);
       Imap.Wire.Literal_start 0L; Imap.Wire.Literal_end])) @
    [Imap.Wire.Text ")\r\n"; Imap.Wire.End_of_response] in
  (match expect_ok (R.parse_parts events) with
   | R.Untagged (Fetch f) ->
       Alcotest.(check int) "streamed literals" 20_000 (List.length f.literals)
   | _ -> fail "missing many-literal FETCH");
  Alcotest.(check bool) "linear tokenizing and literal handling" true
    (Sys.time () -. started < 5.0);
  let metadata=wire_ok (Imap.Wire.feed (Imap.Wire.create ())
    "* METADATA INBOX (/a {3}\r\nabc /b {3}\r\ndef)\r\n") in
  ignore (expect_ok (R.parse_parts ~max_control_literal:6 metadata));
  (match R.parse_parts ~max_control_literal:5 metadata with
   | Error e -> Alcotest.(check string) "aggregate bound"
                  "retained literals exceed aggregate limit" e
   | Ok _ -> fail "retained literals exceeded the aggregate bound");
  (match R.parse_parts ~max_control_literal:4
     [Imap.Wire.Text "* METADATA INBOX (/a {10}\r\n";
      Imap.Wire.Literal_start 10L; Imap.Wire.Literal_chunk "0123456789";
      Imap.Wire.Literal_end; Imap.Wire.Text " /b x\r\n";
      Imap.Wire.Literal_start 3L; Imap.Wire.Literal_chunk "abc";
      Imap.Wire.Literal_end; Imap.Wire.Text ")\r\n";
      Imap.Wire.End_of_response] with
   | Error e -> Alcotest.(check string) "first failure wins"
                  "control literal exceeds limit" e
   | Ok _ -> fail "oversized control literal accepted");
  (match expect_ok (R.parse_parts (wire_ok (Imap.Wire.feed
     (Imap.Wire.create ()) "* ESEARCH (TAG {2}\r\nA1) UID COUNT 3\r\n"))) with
   | R.Untagged (Esearch x) ->
       Alcotest.(check (option string)) "literal ESEARCH tag" (Some "A1") x.tag
   | _ -> fail "missing literal-tag ESEARCH");
  (match parse "* SEARCH 2 5 (MODSEQ 917)\r\n" with
   | R.Untagged (Search [2L;5L]) -> ()
   | _ -> fail "SEARCH MODSEQ suffix");
  (match parse "* SORT 5 2 (modseq 917)\r\n" with
   | R.Untagged (Sort [5L;2L]) -> ()
   | _ -> fail "SORT MODSEQ suffix");
  List.iter rejected ["* SEARCH (MODSEQ 917)\r\n";"* SEARCH 2 (MODSEQ 0)\r\n";
                      "* SEARCH 2 (MODSEQ x)\r\n";"* SORT 2 (MODSEQ 9) 3\r\n";
                      "* SORT (MODSEQ 9)\r\n"];
  (match parse "* VANISHED (earlier) 8:9\r\n" with
   | R.Untagged (Vanished {earlier=true;uids="8:9"}) -> ()
   | _ -> fail "lowercase EARLIER");
  List.iter rejected [
    "* 99999999999999999999 FETCH (UID 1)\r\n";
    "* 99999999999999999999 EXISTS\r\n";
    "* 99999999999999999999 EXPUNGE\r\n";
    "* 1 FETCH garbage\r\n";
    "* 1 FETCH (UID 1) trailing\r\n";
    "* 1 FETCH (UID 1\r\n";
    "* 1 FETCH (FLAGS () FLAGS (\\Seen))\r\n";
    "* 1 FETCH (RFC822.SIZE 1 RFC822.SIZE 2)\r\n";
    "* 1 FETCH (INTERNALDATE \" 1-Jan-2024 00:00:00 +0000\" " ^
    "INTERNALDATE \" 1-Jan-2024 00:00:00 +0000\")\r\n";
    "* 1 FETCH (MODSEQ (1) MODSEQ (2))\r\n";
    "* 1 FETCH (EMAILID (M1) EMAILID (M2))\r\n";
    "* 1 FETCH (THREADID NIL THREADID (T1))\r\n";
    "* STATUS INBOX (MESSAGES 1 MESSAGES 2)\r\n";
    "* STATUS INBOX (MAILBOXID (F1) MAILBOXID (F2))\r\n"];
  (match parse "* ESEARCH (TAG \"A1\") UID FUTURE (1 (2)) COUNT 3\r\n" with
   | R.Untagged (Esearch {count=Some 3L;_}) -> ()
   | _ -> fail "ESEARCH skipped a parenthesised extension badly");
  (match R.select_metadata [parse "A1 NO [NONEXISTENT] no such mailbox\r\n"]
   with
   | Error e -> Alcotest.(check string) "SELECT rejection kept"
                  "SELECT rejected with NO [NONEXISTENT]: no such mailbox" e
   | Ok _ -> fail "rejected SELECT produced metadata")

let test_bad_values () =
  (match Imap.Response.parse "* 1 FETCH (UID 0 FLAGS (\\Seen))\r\n" with
  | Error _ -> () | Ok _ -> fail "accepted UID zero");
  (match Imap.Response.parse "* 1 FETCH (UID 2 FLAGS (\\*))\r\n" with
  | Error _ -> () | Ok _ -> fail "accepted invalid flag");
  (match Imap.Command.login ~username:"x\r\nNOOP" ~password:"p" with
  | Error _ -> () | Ok _ -> fail "accepted command injection");
  (match Imap.Wire.feed (Imap.Wire.create ()) "* 1 FETCH (BODY[] {2+}\r\nx)" with
  | Error _ -> () | Ok _ -> fail "accepted non-synchronizing server literal");
  (match Imap.Response.parse "* OK [UIDVALIDITY 0] bad\r\n" with
  | Error _ -> () | Ok _ -> fail "accepted invalid UIDVALIDITY")

let test_sync_metadata () =
  (match expect_ok (Imap.Response.parse "* PREAUTH ready\r\n") with
  | Imap.Response.Untagged (Preauth (_, "ready")) -> ()
  | _ -> fail "missing PREAUTH");
  (match expect_ok (Imap.Response.parse
    "* ESEARCH (TAG \"A1\") UID ALL 1:3 COUNT 3 MIN 1 MAX 3 MODSEQ 42\r\n") with
  | Imap.Response.Untagged (Esearch x) ->
      Alcotest.(check bool) "UID result" true x.uid;
      Alcotest.(check (option string)) "tag" (Some "A1") x.tag;
      Alcotest.(check (option string)) "all" (Some "1:3") x.all;
      Alcotest.(check (option int64)) "modseq" (Some 42L) x.modseq
  | _ -> fail "missing ESEARCH");
  (match expect_ok (Imap.Response.parse "A2 OK [APPENDUID 7 9] done\r\n") with
  | Imap.Response.Tagged {code=Some (Appenduid (7L,9L));_} -> ()
  | _ -> fail "missing APPENDUID");
  (match expect_ok (Imap.Response.parse
    "* STATUS INBOX (MESSAGES 4 UIDNEXT 9 UIDVALIDITY 7 HIGHESTMODSEQ 22)\r\n") with
  | Imap.Response.Untagged (Status x) ->
      Alcotest.(check (option int64)) "messages" (Some 4L) x.messages;
      Alcotest.(check (option int64)) "uidnext" (Some 9L) x.uidnext;
      Alcotest.(check (option int64)) "highestmodseq" (Some 22L) x.highestmodseq
  | _ -> fail "missing STATUS")

let test_select_and_qresync () =
  let parse s=expect_ok (Imap.Response.parse s) in
  let replies=List.map parse [
    "* 3 EXISTS\r\n";
    "* 0 RECENT\r\n";
    "* FLAGS (\\Seen Custom)\r\n";
    "* OK [PERMANENTFLAGS (\\Seen Custom \\*)] flags\r\n";
    "* OK [UIDVALIDITY 77] validity\r\n";
    "* OK [UIDNEXT 91] next\r\n";
    "* OK [MAILBOXID (F_alice)] mailbox id\r\n";
    "* OK [HIGHESTMODSEQ 1234] modseq\r\n";
    "A1 OK [READ-WRITE] selected\r\n"] in
  let info=expect_ok (Imap.Response.select_metadata replies) in
  Alcotest.(check int64) "exists" 3L info.exists;
  Alcotest.(check int64) "validity" 77L info.uidvalidity;
  Alcotest.(check (option int64)) "modseq" (Some 1234L) info.highestmodseq;
  Alcotest.(check (option string)) "mailbox id" (Some "F_alice")
    info.mailbox_id;
  Alcotest.(check (option bool)) "read-write" (Some false) info.readonly;
  Alcotest.(check bool) "ordinary mailbox has sticky UIDs" false
    info.uidnotsticky;
  let nonsticky=expect_ok (Imap.Response.select_metadata
    (parse "* NO [UIDNOTSTICKY] Non-persistent UIDs\r\n" :: replies)) in
  Alcotest.(check bool) "UIDNOTSTICKY detected" true nonsticky.uidnotsticky;
  Alcotest.(check (option (list string))) "flags"
    (Some ["\\Seen";"Custom"]) info.flags;
  Alcotest.(check (option (list string))) "permanent flags"
    (Some ["\\Seen";"Custom";"\\*"]) info.permanentflags;
  Alcotest.(check string) "QRESYNC syntax"
    "SELECT INBOX (QRESYNC (77 1234 1:3 (1:3 10:12)))"
    (command_ok (Imap.Command.select ~qresync:(77L,1234L)
      ~known_uids:"1:3" ~sequence_match:("1:3","10:12") "INBOX"));
  Alcotest.(check string) "CONDSTORE syntax" "EXAMINE INBOX (CONDSTORE)"
    (command_ok (Imap.Command.select ~readonly:true ~condstore:true "INBOX"));
  (match Imap.Command.select ~qresync:(77L,1234L)
           ~known_uids:"1:3" ~sequence_match:("1:3","10:11") "INBOX" with
   | Error _ -> () | Ok _ -> fail "accepted unequal sequence match")

let test_mutation_extensions () =
  let parse s=expect_ok (Imap.Response.parse s) in
  Alcotest.(check string) "changed fetch"
    "UID FETCH 1:9 (UID FLAGS MODSEQ) (CHANGEDSINCE 42 VANISHED)"
    (command_ok (Imap.Command.uid_fetch_mod ~changedsince:42L ~vanished:true
      ~set:"1:9" ~items:["UID";"FLAGS";"MODSEQ"] ()));
  Alcotest.(check string) "conditional store"
    "UID STORE 8 (UNCHANGEDSINCE 0) +FLAGS.SILENT (\\Seen)"
    (command_ok (Imap.Command.uid_store_mod ~unchangedsince:0L ~set:"8"
      ~operation:`Add ~silent:true ~flags:["\\Seen"] ()));
  Alcotest.(check string) "UID COPY" "UID COPY 8:9 Archive"
    (command_ok (Imap.Command.uid_copy ~set:"8:9" ~mailbox:"Archive"));
  Alcotest.(check string) "UID MOVE" "UID MOVE 8:9 Archive"
    (command_ok (Imap.Command.uid_move ~set:"8:9" ~mailbox:"Archive"));
  Alcotest.(check string) "UID EXPUNGE" "UID EXPUNGE 8:9"
    (command_ok (Imap.Command.uid_expunge ~set:"8:9"));
  Alcotest.(check string) "UID EXPUNGE wildcard, RFC 4315" "UID EXPUNGE 8:*"
    (command_ok (Imap.Command.uid_expunge ~set:"8:*"));
  (match Imap.Command.uid_expunge ~set:"$" with
   | Error _ -> () | Ok _ -> fail "UID EXPUNGE accepted a saved result");
  (match parse "A2 OK [MODIFIED 8:9] partial\r\n" with
   | Imap.Response.Tagged {code=Some (Modified "8:9");_} -> ()
   | _ -> fail "missing MODIFIED");
  (match parse "* VANISHED (EARLIER) 8:9\r\n" with
   | Imap.Response.Untagged (Vanished {earlier=true;uids="8:9"}) -> ()
   | _ -> fail "missing VANISHED");
  (match parse "A3 OK [COPYUID 7 8:9 90:91] done\r\n" with
   | Imap.Response.Tagged {code=Some (Copyuid (7L,"8:9","90:91"));_} -> ()
   | _ -> fail "missing COPYUID");
  (match parse "A4 OK [APPENDUID 7 90:91] done\r\n" with
   | Imap.Response.Tagged {code=Some (Appenduid_set (7L,"90:91"));_} -> ()
   | _ -> fail "missing MULTIAPPEND receipt");
  List.iter (fun s -> match Imap.Response.parse s with
    | Error _ -> () | Ok _ -> fail ("accepted invalid response: " ^ s)) [
    "A3 OK [COPYUID 7 8:9 90] done\r\n";
    "A4 OK [APPENDUID 7 90:90] bad\r\n";
    "A2 OK [MODIFIED 0] bad\r\n";
    "* VANISHED 0\r\n";
    "* ESEARCH UID ALL 0 COUNT 1\r\n";
    "* 0 EXPUNGE\r\n";
    "* 4294967296 EXISTS\r\n"];
  (match Imap.Command.uid_fetch_mod ~vanished:true ~set:"8"
    ~items:["FLAGS"] () with Error _ -> () | Ok _ -> fail "VANISHED without CHANGEDSINCE")

let test_discovery_and_objectid () =
  let parse s=expect_ok (Imap.Response.parse s) in
  Alcotest.(check string) "IDLE command" "IDLE" Imap.Command.idle;
  Alcotest.(check string) "IDLE terminator" "DONE" Imap.Command.done_idle;
  Alcotest.(check string) "JMAP command" "GETJMAPACCESS"
    Imap.Command.get_jmap_access;
  Alcotest.(check string) "STATUS command"
    "STATUS INBOX (MESSAGES UIDNEXT MAILBOXID SIZE)"
    (command_ok (Imap.Command.status ~mailbox:"INBOX"
      ~items:[Imap.Status_item.Messages;Imap.Status_item.Uidnext;
              Imap.Status_item.Mailboxid;Imap.Status_item.Size]));
  Alcotest.(check string) "LIST-STATUS command"
    "LIST \"\" \"*\" RETURN (STATUS (MESSAGES UNSEEN))"
    (command_ok (Imap.Command.list_status ~reference:"" ~pattern:"*"
      ~items:[Imap.Status_item.Messages;Imap.Status_item.Unseen]));
  (match parse "* STATUS INBOX (MESSAGES 3 MAILBOXID (F_abc-09) SIZE 900)\r\n" with
   | Imap.Response.Untagged (Status x) ->
       Alcotest.(check (option string)) "status mailbox id"
         (Some "F_abc-09") x.mailbox_id;
       Alcotest.(check (option int64)) "status size" (Some 900L) x.size
   | _ -> fail "missing STATUS");
  (match parse "* OK [MAILBOXID (F_abc-09)] selected\r\n" with
   | Imap.Response.Untagged (Ok (Some (Mailboxid "F_abc-09"),_)) -> ()
   | _ -> fail "missing MAILBOXID response code");
  (match parse "* 1 FETCH (UID 7 EMAILID (M_abc-09) THREADID NIL)\r\n" with
   | Imap.Response.Untagged (Fetch x) ->
       Alcotest.(check (option string)) "EMAILID" (Some "M_abc-09") x.email_id;
       Alcotest.(check (option (option string))) "THREADID NIL"
         (Some None) x.thread_id
   | _ -> fail "missing OBJECTID FETCH");
  (match parse "* JMAPACCESS \"https://mail.example/.well-known/jmap\"\r\n" with
   | Imap.Response.Untagged (Jmapaccess url) ->
       Alcotest.(check string) "session URL"
         "https://mail.example/.well-known/jmap" url
   | _ -> fail "missing JMAPACCESS");
  List.iter (fun s -> match Imap.Response.parse s with
    | Error _ -> () | Ok _ -> fail ("accepted invalid OBJECTID/JMAP: " ^ s)) [
    "* OK [MAILBOXID (bad.id)] invalid\r\n";
    "* STATUS INBOX (MAILBOXID (bad.id))\r\n";
    "* 1 FETCH (EMAILID (bad.id))\r\n";
    "* JMAPACCESS https://mail.example/\r\n";
    "* JMAPACCESS \"https://mail.example/\\\"\r\n"]

let test_extended_discovery () =
  let parse s=expect_ok (Imap.Response.parse s) in
  Alcotest.(check string) "namespace command" "NAMESPACE"
    Imap.Command.namespace;
  Alcotest.(check string) "extended LIST"
    "LIST (SUBSCRIBED RECURSIVEMATCH) \"\" (INBOX \"Sent/*\") RETURN (CHILDREN SPECIAL-USE STATUS (MESSAGES UIDNEXT))"
    (command_ok (Imap.Command.list_extended ~reference:""
      ~patterns:["INBOX";"Sent/*"]
      ~selection:[Imap.Mailbox_list.Subscribed;Recursive_match]
      ~returns:[Imap.Mailbox_list.Children;Imap.Mailbox_list.Special_use]
      ~status:[Imap.Status_item.Messages;Imap.Status_item.Uidnext] ())); 
  (match Imap.Command.list_extended ~reference:"" ~patterns:["*"]
    ~selection:[Imap.Mailbox_list.Recursive_match] () with
   | Error _ -> () | Ok _ -> fail "accepted RECUSIVEMATCH without base");
  (match parse "* NAMESPACE ((\"\" \"/\")(\"#mh/\" \"/\" \"X-PARAM\" (\"FLAG1\" \"FLAG2\"))) NIL ((\"#shared.\" \".\"))\r\n" with
   | Imap.Response.Untagged (Namespace x) ->
       (match x.personal with
        | Some [one;two] ->
            Alcotest.(check string) "first namespace" "" one.prefix;
            Alcotest.(check string) "second namespace" "#mh/" two.prefix;
            Alcotest.(check (list (pair string (list string))))
              "namespace extension" ["X-PARAM",["FLAG1";"FLAG2"]]
              two.extensions
        | _ -> fail "missing personal namespaces");
       Alcotest.(check bool) "NIL other users" true (x.other_users=None)
   | _ -> fail "missing NAMESPACE");
  (match parse "* LIST (\\NoInferiors \\NonExistent \\Sent) \"/\" \"Old&-Sent\" (OLDNAME (\"Sent\"))\r\n" with
   | Imap.Response.Untagged (List x) ->
       Alcotest.(check bool) "nonexistent is not selectable" false x.selectable;
       Alcotest.(check bool) "NoInferiors implies no children" true
         (x.children=`Has_no_children);
       Alcotest.(check (option string)) "OLDNAME" (Some "Sent") x.old_name;
       Alcotest.(check (list string)) "special use" ["\\Sent"] x.special_use;
       Alcotest.(check string) "exact wire mailbox" "Old&-Sent" x.mailbox
   | _ -> fail "missing extended LIST");
  (match parse "* LIST (\\HasChildren) \"/\" \"Foo\" (CHILDINFO (\"SUBSCRIBED\"))\r\n" with
   | Imap.Response.Untagged (List x) ->
       Alcotest.(check (option (list string))) "CHILDINFO"
         (Some ["SUBSCRIBED"]) x.childinfo
   | _ -> fail "missing LIST CHILDINFO");
  List.iter (fun raw -> match Imap.Response.parse raw with
    | Error _ -> () | Ok _ -> fail ("accepted malformed discovery: " ^ raw)) [
    "* LIST (\\HasChildren) \"/\"\r\n";
    "* LIST (\\HasChildren \\HasNoChildren) \"/\" Foo\r\n";
    "* LIST () \"/\" \"unclosed\r\n";
    "* NAMESPACE ((\"\" \"/\")) NIL\r\n";
    "* NAMESPACE () NIL NIL\r\n";
    "* NAMESPACE ((\"\" NIL \"X\" \"not-a-list\")) NIL NIL\r\n"]

let test_uidbatches_and_partial () =
  let parse s=expect_ok (Imap.Response.parse s) in
  Alcotest.(check string) "batch command"
    "UIDBATCHES 2000 1:50"
    (command_ok (Imap.Command.uid_batches ~size:2000L ~range:(1L,50L) ()));
  (match Imap.Command.uid_batches ~size:499L () with
   | Error _ -> () | Ok _ -> fail "accepted undersized batch");
  (match Imap.Command.uid_batches ~size:2000L ~range:(1L,51L) () with
   | Error _ -> () | Ok _ -> fail "accepted oversized range span");
  (match parse "* UIDBATCHES (TAG \"A1\") 99:50,49:1\r\n" with
   | Imap.Response.Untagged (Uidbatches x) ->
       Alcotest.(check string) "batch tag" "A1" x.tag;
       Alcotest.(check (list (pair int64 int64))) "descending ranges"
         [99L,50L;49L,1L] x.ranges
   | _ -> fail "missing UIDBATCHES");
  (match parse "* UIDBATCHES (TAG \"A2\")\r\n" with
   | Imap.Response.Untagged (Uidbatches {ranges=[];_}) -> ()
   | _ -> fail "missing empty UIDBATCHES");
  (match Imap.Response.parse
     "* UIDBATCHES (TAG \"A1\") 99:50,55:1\r\n" with
   | Error _ -> () | Ok _ -> fail "accepted overlapping UID batches");
  Alcotest.(check string) "PARTIAL UID SEARCH"
    "UID SEARCH RETURN (PARTIAL -1:-10) UNDELETED"
    (command_ok (Imap.Command.uid_search_partial ~range:(-1L,-10L)
      ~criterion:"UNDELETED"));
  Alcotest.(check string) "PARTIAL UID FETCH"
    "UID FETCH 1:* (UID FLAGS) (PARTIAL 1:20 CHANGEDSINCE 5)"
    (command_ok (Imap.Command.uid_fetch_mod ~partial:(1L,20L)
      ~changedsince:5L ~set:"1:*" ~items:["UID";"FLAGS"] ()));
  (match parse "* ESEARCH (TAG \"A3\") UID PARTIAL (-1:-10 90:99) COUNT 100\r\n" with
   | Imap.Response.Untagged (Esearch x) ->
       Alcotest.(check (option (pair string (option string)))) "partial result"
         (Some ("-1:-10",Some "90:99")) x.partial
   | _ -> fail "missing PARTIAL ESEARCH");
  (match parse "* ESEARCH (TAG \"A4\") UID PARTIAL (11:20 NIL)\r\n" with
   | Imap.Response.Untagged (Esearch {partial=Some ("11:20",None);_}) -> ()
   | _ -> fail "missing empty PARTIAL ESEARCH");
  (match Imap.Command.uid_search_partial ~range:(-1L,10L)
    ~criterion:"ALL" with Error _ -> () | Ok _ -> fail "accepted mixed PARTIAL range");
  (match parse "A5 OK [MESSAGELIMIT 1000 23007] partial\r\n" with
   | Imap.Response.Tagged {code=Some (Messagelimit (1000L,Some 23007L));_} -> ()
   | _ -> fail "missing MESSAGELIMIT boundary");
  (match parse "A6 NO [MESSAGELIMIT 1000] too large\r\n" with
   | Imap.Response.Tagged {code=Some (Messagelimit (1000L,None));_} -> ()
   | _ -> fail "missing MESSAGELIMIT rejection");
  (match Imap.Response.parse "A7 OK [MESSAGELIMIT 0] bad\r\n" with
   | Error _ -> () | Ok _ -> fail "accepted invalid MESSAGELIMIT")

let test_uidonly () =
  let parse s=expect_ok (Imap.Response.parse s) in
  (match parse "* 25996 UIDFETCH (FLAGS (\\Seen))\r\n" with
   | Imap.Response.Untagged (Uidfetch row) ->
       Alcotest.(check int64) "leading UID" 25996L row.seq;
       Alcotest.(check (option int64)) "effective UID" (Some 25996L) row.uid;
       Alcotest.(check (option (list string))) "UIDONLY flags"
         (Some ["\\Seen"]) row.flags
   | _ -> fail "UIDFETCH not parsed");
  (match parse "* 25997 UIDFETCH (FLAGS () UID 25997)\r\n" with
   | Imap.Response.Untagged (Uidfetch {uid=Some 25997L;_}) -> ()
   | _ -> fail "UIDFETCH explicit UID not retained");
  (match Imap.Response.parse "* 25997 UIDFETCH (UID 10 FLAGS ())\r\n" with
   | Error _ -> () | Ok _ -> fail "accepted contradictory UIDFETCH UID");
  (match parse "* VANISHED 405,407\r\n" with
   | Imap.Response.Untagged (Vanished {earlier=false;uids="405,407"}) -> ()
   | _ -> fail "UIDONLY VANISHED missing");
  (match parse "A9 BAD [UIDREQUIRED] use UID commands\r\n" with
   | Imap.Response.Tagged {code=Some Uidrequired;_} -> ()
   | _ -> fail "UIDONLY UIDREQUIRED code missing")

let test_acl_quota () =
  let parse s=expect_ok (Imap.Response.parse s) in
  Alcotest.(check string) "GETACL" "GETACL INBOX"
    (command_ok (Imap.Command.getacl ~mailbox:"INBOX"));
  Alcotest.(check string) "SETACL add" "SETACL INBOX alice +lr"
    (command_ok (Imap.Command.setacl ~mailbox:"INBOX" ~identifier:"alice"
      ~operation:`Add ~rights:"lr"));
  Alcotest.(check string) "DELETEACL" "DELETEACL INBOX alice"
    (command_ok (Imap.Command.deleteacl ~mailbox:"INBOX" ~identifier:"alice"));
  (match Imap.Command.setacl ~mailbox:"INBOX" ~identifier:"alice"
    ~operation:`Replace ~rights:"R" with
   | Error _ -> () | Ok _ -> fail "accepted uppercase ACL right");
  (match parse "* ACL INBOX alice lrsw anyone lr\r\n" with
   | Imap.Response.Untagged (Acl x) ->
       Alcotest.(check (list (pair string string))) "ACL pairs"
         ["alice","lrsw";"anyone","lr"] x.entries
   | _ -> fail "missing ACL");
  (match parse "* LISTRIGHTS INBOX alice \"\" l r swicdkxte\r\n" with
   | Imap.Response.Untagged (List_rights x) ->
       Alcotest.(check string) "required rights" "" x.required;
       Alcotest.(check (list string)) "optional tied groups"
         ["l";"r";"swicdkxte"] x.optional
   | _ -> fail "missing LISTRIGHTS");
  (match parse "* MYRIGHTS INBOX lrsw\r\n" with
   | Imap.Response.Untagged (My_rights {rights="lrsw";_}) -> ()
   | _ -> fail "missing MYRIGHTS");
  Alcotest.(check string) "GETQUOTA"
    "GETQUOTA #user/alice"
    (command_ok (Imap.Command.getquota ~root:"#user/alice"));
  Alcotest.(check string) "SETQUOTA full replacement"
    "SETQUOTA #user/alice (STORAGE 1024 MESSAGE 500)"
    (command_ok (Imap.Command.setquota ~root:"#user/alice"
      ~limits:["storage",1024L;"message",500L]));
  (match parse "* QUOTA \"#user/alice\" (STORAGE 10 512 X-CUSTOM 2 9)\r\n" with
   | Imap.Response.Untagged (Quota x) ->
       Alcotest.(check (list (triple string int64 int64))) "quota resources"
         ["STORAGE",10L,512L;"X-CUSTOM",2L,9L] x.resources
   | _ -> fail "missing QUOTA");
  (match parse "* QUOTAROOT INBOX \"#user/alice\" \"\"\r\n" with
   | Imap.Response.Untagged (Quota_root x) ->
       Alcotest.(check (list string)) "quota roots" ["#user/alice";""] x.roots
   | _ -> fail "missing QUOTAROOT");
  (match parse "* STATUS INBOX (DELETED 2 DELETED-STORAGE 4096)\r\n" with
   | Imap.Response.Untagged (Status x) ->
       Alcotest.(check (option int64)) "deleted messages" (Some 2L) x.deleted;
       Alcotest.(check (option int64)) "deleted storage"
         (Some 4096L) x.deleted_storage
   | _ -> fail "missing QUOTA STATUS attributes");
  (match parse "A9 NO [OVERQUOTA] append failed\r\n" with
   | Imap.Response.Tagged {code=Some Overquota;_} -> ()
   | _ -> fail "missing OVERQUOTA");
  List.iter (fun s -> match Imap.Response.parse s with
    | Error _ -> () | Ok _ -> fail ("accepted invalid ACL/QUOTA: " ^ s)) [
    "* ACL INBOX alice\r\n";
    "* MYRIGHTS INBOX LR\r\n";
    "* QUOTA \"\" (STORAGE 1)\r\n"]

let test_metadata_notify () =
  let parse s=expect_ok (Imap.Response.parse s) in
  Alcotest.(check string) "GETMETADATA options"
    "GETMETADATA (MAXSIZE 1024 DEPTH 1) INBOX (/shared/comment /private/comment)"
    (command_ok (Imap.Command.getmetadata ~mailbox:"INBOX"
      ~entries:["/shared/comment";"/private/comment"]
      ~maxsize:1024L ~depth:Imap.Metadata.One ()));
  Alcotest.(check string) "SETMETADATA quoted/NIL"
    "SETMETADATA INBOX (/shared/comment \"Hello\" /private/comment NIL)"
    (command_ok (Imap.Command.setmetadata ~mailbox:"INBOX"
      ~values:["/shared/comment",Some "Hello";"/private/comment",None]));
  (match Imap.Command.setmetadata ~mailbox:"INBOX"
    ~values:["/shared/comment",Some "line\nnext"] with
   | Error _ -> () | Ok _ -> fail "accepted unframed METADATA literal");
  (match parse "* METADATA INBOX (/shared/comment \"Hi\" /private/comment NIL)\r\n" with
   | Imap.Response.Untagged (Metadata {payload=Metadata_values pairs;_}) ->
       Alcotest.(check (list (pair string (option string)))) "metadata values"
         ["/shared/comment",Some "Hi";"/private/comment",None] pairs
   | _ -> fail "missing METADATA values");
  (match parse "* METADATA INBOX /shared/comment /private/comment\r\n" with
   | Imap.Response.Untagged (Metadata {payload=Metadata_changed names;_}) ->
       Alcotest.(check (list string)) "changed names"
         ["/shared/comment";"/private/comment"] names
   | _ -> fail "missing METADATA names");
  let d=Imap.Wire.create () in
  let parts=wire_ok (Imap.Wire.feed d
    "* METADATA INBOX (/shared/comment {5}\r\nHello)\r\n") in
  (match expect_ok (Imap.Response.parse_parts parts) with
   | Imap.Response.Untagged (Metadata {payload=Metadata_values [_,Some "Hello"];_}) -> ()
   | _ -> fail "missing literal METADATA value");
  (match Imap.Response.parse_parts ~max_control_literal:4 parts with
   | Error _ -> () | Ok _ -> fail "accepted over-limit METADATA literal");
  (match parse "A1 OK [METADATA LONGENTRIES 2199] partial\r\n" with
   | Imap.Response.Tagged {code=Some (Metadata_longentries 2199L);_} -> ()
   | _ -> fail "missing METADATA LONGENTRIES");
  (match Imap.Response.parse
     "* METADATA INBOX (/shared/comment unquoted)\r\n" with
   | Error _ -> () | Ok _ -> fail "accepted unquoted METADATA value");
  Alcotest.(check string) "NOTIFY NONE" "NOTIFY NONE" Imap.Command.notify_none;
  Alcotest.(check string) "NOTIFY SET"
    "NOTIFY SET STATUS (selected (MessageNew MessageExpunge FlagChange)) (subtree Lists (MessageNew MessageExpunge))"
    (command_ok (Imap.Command.notify_set ~status:true
      ~groups:[Imap.Notify.Selected,
               [Imap.Notify.Message_new;Imap.Notify.Message_expunge;
                Imap.Notify.Flag_change];
               Imap.Notify.Subtree ["Lists"],
               [Imap.Notify.Message_new;Imap.Notify.Message_expunge]] ()));
  (match Imap.Command.notify_set ~groups:[Imap.Notify.Selected,
    [Imap.Notify.Flag_change]] () with
   | Error _ -> () | Ok _ -> fail "accepted unpaired NOTIFY events");
  (match parse "* OK [NOTIFICATIONOVERFLOW] dropped\r\n" with
   | Imap.Response.Untagged (Ok (Some Notificationoverflow,_)) -> ()
   | _ -> fail "missing NOTIFICATIONOVERFLOW");
  (match parse "A2 NO [BADEVENT (MessageNew FlagChange)] unsupported\r\n" with
   | Imap.Response.Tagged {code=Some (Badevent ["MessageNew";"FlagChange"]);_} -> ()
   | _ -> fail "missing BADEVENT")

let test_modified_utf7 () =
  let name="旅行/📧 & Inbox" in
  let wire=expect_ok (Imap.Mailbox_name.encode_rev1 name) in
  Alcotest.(check string) "round trip" name
    (expect_ok (Imap.Mailbox_name.decode_rev1 wire));
  Alcotest.(check string) "ampersand" "A&-B"
    (expect_ok (Imap.Mailbox_name.encode_rev1 "A&B"));
  Alcotest.(check string) "ASCII" "A&B"
    (expect_ok (Imap.Mailbox_name.decode_rev1 "A&-B"));
  List.iter (fun wire ->
    match Imap.Mailbox_name.decode_rev1 wire with
    | Error _ -> () | Ok _ -> fail ("accepted malformed mailbox " ^ wire))
    ["&";"&A-";"&2AA-";"&AEE-";"\255"];
  List.iter (fun name ->
    List.iter (fun (mode,label) ->
      (match Imap.Mailbox_name.encode ~mode name with
       | Error _ -> ()
       | Ok _ -> fail (label ^ " encoded a control or invalid name"));
      match Imap.Mailbox_name.decode ~mode name with
      | Error _ -> ()
      | Ok _ -> fail (label ^ " decoded a control or invalid name"))
      [Imap.Mailbox_name.Rev1,"Rev1";Imap.Mailbox_name.Utf8,"UTF-8"])
    ["a\000b";"a\r\nb";"a\027b";"a\127b";"\255"];
  Alcotest.(check string) "UTF-8 mode keeps valid names" "旅行"
    (expect_ok (Imap.Mailbox_name.encode ~mode:Imap.Mailbox_name.Utf8 "旅行"))

let test_preview () =
  let command=Imap.Command.uid_fetch_preview ~set:"2,7" ~lazy_:true
    |> command_ok in
  Alcotest.(check string) "LAZY syntax"
    "UID FETCH 2,7 (UID PREVIEW (LAZY))" command;
  let get s=match expect_ok (Imap.Response.parse s) with
    | Imap.Response.Untagged (Fetch f) -> f.preview
    | _ -> fail "expected FETCH PREVIEW" in
  Alcotest.(check (option (option string))) "absent" None
    (get "* 1 FETCH (UID 2 FLAGS ())\r\n");
  Alcotest.(check (option (option string))) "NIL" (Some None)
    (get "* 1 FETCH (UID 2 PREVIEW NIL)\r\n");
  Alcotest.(check (option (option string))) "empty" (Some (Some ""))
    (get "* 1 FETCH (UID 2 PREVIEW \"\")\r\n");
  Alcotest.(check (option (option string))) "UTF-8" (Some (Some "📧 hi"))
    (get "* 1 FETCH (PREVIEW \"📧 hi\" UID 2)\r\n");
  let max_utf8=String.concat "" (List.init 256 (fun _ -> "📧")) in
  Alcotest.(check (option (option string))) "256 Unicode characters"
    (Some (Some max_utf8))
    (get ("* 1 FETCH (UID 2 PREVIEW \"" ^ max_utf8 ^ "\")\r\n"));
  let d=Imap.Wire.create () in
  let parts=wire_ok (Imap.Wire.feed d
    "* 1 FETCH (UID 2 PREVIEW {7}\r\n📧 hi)\r\n") in
  (match expect_ok (Imap.Response.parse_parts parts) with
   | Imap.Response.Untagged (Fetch {preview=Some (Some "📧 hi");_}) -> ()
   | _ -> fail "lost literal PREVIEW");
  List.iter (fun value ->
    match Imap.Response.parse ("* 1 FETCH (UID 2 PREVIEW \"" ^ value ^
      "\")\r\n") with
    | Error _ -> () | Ok _ -> fail "accepted invalid PREVIEW")
    ["\255";"\192\128";"\237\160\128"; String.make 257 'a'];
  (match Imap.Response.parse
    "* 1 FETCH (UID 2 PREVIEW \"a\" PREVIEW \"b\")\r\n" with
   | Error _ -> () | Ok _ -> fail "accepted duplicate PREVIEW");
  let d=Imap.Wire.create () in
  let parts=wire_ok (Imap.Wire.feed d
    ("* 1 FETCH (UID 2 PREVIEW {1025}\r\n" ^
      String.make 1025 'a' ^ ")\r\n")) in
  (match Imap.Response.parse_parts parts with
   | Error _ -> () | Ok _ -> fail "accepted over-limit literal PREVIEW")

let test_internal_date () =
  let date=expect_ok (Imap.Internal_date.of_string
    "29-Feb-2024 23:59:60 -0000") in
  Alcotest.(check string) "negative zero zone retained"
    "29-Feb-2024 23:59:60 -0000"
    (Imap.Internal_date.to_string date);
  let one=expect_ok (Imap.Internal_date.of_string
    " 1-jan-2024 01:02:03 +0530") in
  Alcotest.(check string) "month normalized"
    " 1-Jan-2024 01:02:03 +0530"
    (Imap.Internal_date.to_string one);
  let instant raw=expect_ok (Imap.Internal_date.of_string raw) in
  Alcotest.(check bool) "same instant across year and zone" true
    (Imap.Internal_date.equal_instant
      (instant "31-Dec-2023 23:30:00 -0100")
      (instant " 1-Jan-2024 00:30:00 +0000"));
  Alcotest.(check bool) "different instant" false
    (Imap.Internal_date.equal_instant
      (instant " 1-Jan-2024 00:30:00 +0000")
      (instant " 1-Jan-2024 00:30:00 +0100"));
  Alcotest.(check bool) "leap second differs from next minute" false
    (Imap.Internal_date.equal_instant
      (instant "31-Dec-2023 23:59:60 +0000")
      (instant " 1-Jan-2024 00:00:00 +0000"));
  let unix_date seconds=expect_ok
    (Imap.Internal_date.of_unix_seconds seconds)
    |> Imap.Internal_date.to_string in
  Alcotest.(check string) "Unix epoch to UTC"
    " 1-Jan-1970 00:00:00 +0000" (unix_date 0L);
  Alcotest.(check string) "negative Unix second"
    "31-Dec-1969 23:59:59 +0000" (unix_date (-1L));
  Alcotest.(check string) "leap-day Unix second"
    "29-Feb-2024 00:00:00 +0000" (unix_date 1709164800L);
  (match Imap.Internal_date.of_unix_seconds Int64.max_int with
   | Error _ -> () | Ok _ -> fail "accepted out-of-range Unix timestamp");
  List.iter (fun raw ->
    match Imap.Internal_date.of_string raw with
    | Error _ -> ()
    | Ok _ -> fail ("accepted a leap second away from 23:59 UTC: " ^ raw))
    ["31-Dec-2016 12:00:60 +0000";"31-Dec-2016 23:58:60 +0000";
     "31-Dec-2016 23:59:60 +0100"];
  List.iter (fun raw -> ignore (instant raw))
    [" 1-Jan-2017 00:59:60 +0100";"31-Dec-2016 18:29:60 -0530"];
  List.iter (fun raw ->
    match Imap.Internal_date.to_unix_seconds (instant raw) with
    | Error _ -> ()
    | Ok seconds ->
        (match Imap.Internal_date.of_unix_seconds seconds with
         | Error _ -> fail ("to_unix_seconds escaped the range: " ^ raw)
         | Ok _ -> fail ("expected an out-of-range error: " ^ raw)))
    [" 1-Jan-0001 00:00:00 +0100";"31-Dec-9999 23:59:59 -0100"];
  Alcotest.(check (result int64 string)) "earliest instant in range"
    (Ok (-62135596800L))
    (Imap.Internal_date.to_unix_seconds (instant " 1-Jan-0001 00:00:00 +0000"));
  let command=command_ok (Imap.Command.append_prefix ~mailbox:"INBOX"
    ~flags:["\\Seen"] ~internal_date:one ~size:3L ()) in
  Alcotest.(check string) "APPEND preserves internal date"
    "APPEND INBOX (\\Seen) \" 1-Jan-2024 01:02:03 +0530\" {3}\r\n"
    command;
  (match expect_ok (Imap.Response.parse
    "* 7 FETCH (UID 19 INTERNALDATE \"29-Feb-2024 23:59:60 -0000\" FLAGS ())\r\n") with
   | Imap.Response.Untagged (Fetch fetch) ->
       Alcotest.(check (option string)) "typed FETCH internal date"
         (Some "29-Feb-2024 23:59:60 -0000")
         (Option.map Imap.Internal_date.to_string fetch.internal_date)
   | _ -> fail "missing FETCH INTERNALDATE");
  List.iter (fun value ->
    (match Imap.Internal_date.of_string value with
     | Error _ -> () | Ok _ -> fail "accepted invalid calendar date");
    match Imap.Response.parse ("* 1 FETCH (UID 2 INTERNALDATE \"" ^
      value ^ "\")\r\n") with
    | Error _ -> () | Ok _ -> fail "accepted invalid FETCH INTERNALDATE")
    ["29-Feb-2023 12:00:00 +0000";
     "31-Apr-2024 12:00:00 +0000";
     "01-Jan-2024 24:00:00 +0000";
     "01-Jan-2024 12:60:00 +0000";
     "01-Jan-2024 12:00:00 +2460";
     "01-Foo-2024 12:00:00 +0000";
     "01-Jan-2024 12:00:00 +0000\r\n"]

let test_mailbox_management () =
  Alcotest.(check string) "LSUB syntax" "LSUB \"\" \"Box*\""
    (command_ok (Imap.Command.lsub ~reference:"" ~pattern:"Box*"));
  Alcotest.(check string) "RENAME syntax" "RENAME Old New"
    (command_ok (Imap.Command.rename ~old_name:"Old" ~new_name:"New"));
  Alcotest.(check string) "SUBSCRIBE syntax" "SUBSCRIBE Box"
    (command_ok (Imap.Command.subscribe ~mailbox:"Box"));
  Alcotest.(check string) "UNSUBSCRIBE syntax" "UNSUBSCRIBE Box"
    (command_ok (Imap.Command.unsubscribe ~mailbox:"Box"));
  (match expect_ok (Imap.Response.parse
    "* LSUB (\\HasNoChildren) \"/\" \"Box\"\r\n") with
   | Imap.Response.Untagged (List {subscribed=true;mailbox="Box";_}) -> ()
   | _ -> fail "LSUB row not typed")

let test_objectid_plus_draft () =
  let parse s=expect_ok (Imap.Response.parse s) in
  let replies=List.map parse [
    "* 1 EXISTS\r\n";
    "* OK [UIDVALIDITY 7] valid\r\n";
    "* OK [UIDNEXT 8] next\r\n";
    "* OK [OBJECTID (ACCOUNTID u_account MAILBOXID F_box FUTURE X_1)] id\r\n";
    "A1 OK [READ-ONLY] selected\r\n"] in
  let info=expect_ok (Imap.Response.select_metadata replies) in
  (match info.objectid with
   | Some ids ->
       Alcotest.(check (option string)) "account ID" (Some "u_account")
         ids.account_id;
       Alcotest.(check (option string)) "mailbox ID" (Some "F_box")
         ids.mailbox_id;
       Alcotest.(check (list (pair string string))) "unknown ID retained"
         ["FUTURE","X_1"] ids.unknown
   | None -> fail "OBJECTID+ SELECT code missing");
  let row=match parse
    "* 1 FETCH (UID 7 OBJECTID (EMAILID M_7 THREADID T_7))\r\n" with
    | Imap.Response.Untagged (Fetch row) -> row
    | _ -> fail "OBJECTID+ FETCH row missing" in
  (match expect_ok (Imap.Response.fetch_objectid row) with
   | Some ids ->
       Alcotest.(check (option string)) "compound EMAILID" (Some "M_7")
         ids.email_id;
       Alcotest.(check (option string)) "compound THREADID" (Some "T_7")
         ids.thread_id
   | None -> fail "OBJECTID+ FETCH attribute missing");
  let nested=match parse
    "* 1 FETCH (UID 7 FUTURE (OBJECTID (EMAILID M_decoy)) OBJECTID (EMAILID M_7))\r\n" with
    | Imap.Response.Untagged (Fetch row) -> row
    | _ -> fail "nested FETCH row missing" in
  (match expect_ok (Imap.Response.fetch_objectid nested) with
   | Some {email_id=Some "M_7";_} -> ()
   | _ -> fail "nested OBJECTID was mistaken for a FETCH field");
  let empty=match parse "* 1 FETCH (UID 7 OBJECTID ())\r\n" with
    | Imap.Response.Untagged (Fetch row) -> row
    | _ -> fail "empty OBJECTID+ FETCH missing" in
  (match expect_ok (Imap.Response.fetch_objectid empty) with
   | Some {email_id=None;thread_id=None;_} -> ()
   | _ -> fail "empty OBJECTID+ compound rejected");
  (match Imap.Response.parse
    "* OK [OBJECTID (ACCOUNTID u_a ACCOUNTID u_b)] duplicate\r\n" with
   | Error _ -> () | Ok _ -> fail "duplicate compound key accepted");
  Alcotest.(check string) "draft STATUS command"
    "STATUS INBOX (OBJECTID)"
    (command_ok (Imap.Command.status ~mailbox:"INBOX"
      ~items:[Imap.Status_item.Objectid]));
  (match parse
    "* STATUS INBOX (OBJECTID (ACCOUNTID u_account MAILBOXID F_box FUTURE X_1))\r\n" with
   | Imap.Response.Untagged (Status {objectid=Some ids;_}) ->
       Alcotest.(check (option string)) "STATUS account"
         (Some "u_account") ids.account_id;
       Alcotest.(check (option string)) "STATUS mailbox"
         (Some "F_box") ids.mailbox_id;
       Alcotest.(check (list (pair string string))) "STATUS unknown"
         ["FUTURE","X_1"] ids.unknown
   | _ -> fail "OBJECTID+ STATUS missing");
  (match Imap.Response.parse
    "* STATUS INBOX (OBJECTID (ACCOUNTID u_a ACCOUNTID u_b))\r\n" with
   | Error _ -> () | Ok _ -> fail "duplicate STATUS compound key accepted");
  Alcotest.(check string) "OBJECTID+ identity SELECT"
    "EXAMINE INBOX (CONDSTORE OBJECTID (MAILBOXID F_box ACCOUNTID u_account))"
    (command_ok (Imap.Command.select ~readonly:true ~condstore:true
      ~objectid:("u_account","F_box") "INBOX"));
  Alcotest.(check string) "OBJECTID+ with QRESYNC"
    "SELECT INBOX (QRESYNC (7 42) OBJECTID (MAILBOXID F_box ACCOUNTID u_account))"
    (command_ok (Imap.Command.select ~qresync:(7L,42L)
      ~objectid:("u_account","F_box") "INBOX"));
  (match Imap.Command.select ~objectid:("invalid id","F_box") "INBOX" with
   | Error _ -> () | Ok _ -> fail "invalid OBJECTID+ ID accepted")

let test_envelope () =
  let row raw = match expect_ok (Imap.Response.parse raw) with
    | Imap.Response.Untagged (Fetch value) -> value
    | _ -> fail "missing FETCH row" in
  let basic=row
    "* 1 FETCH (UID 7 ENVELOPE (\"Mon, 1 Jan 2024 00:00:00 +0000\" \"Hello\" ((\"Alice\" NIL \"alice\" \"example.com\")) ((\"Alice\" NIL \"alice\" \"example.com\")) ((\"Alice\" NIL \"alice\" \"example.com\")) ((NIL NIL \"Team\" NIL) (NIL NIL \"bob\" \"example.com\") (NIL NIL NIL NIL)) NIL NIL NIL \"<id@example.com>\") FLAGS ())\r\n" in
  let env=match expect_ok (Imap.Response.fetch_envelope basic) with
    | Some value -> value | None -> fail "ENVELOPE absent" in
  Alcotest.(check (option string)) "subject" (Some "Hello") env.subject;
  Alcotest.(check (option string)) "message ID"
    (Some "<id@example.com>") env.message_id;
  (match env.to_ with
   | Some [{mailbox=Some "Team";host=None;_};
           {mailbox=Some "bob";host=Some "example.com";_};
           {mailbox=None;host=None;_}] -> ()
   | _ -> fail "ENVELOPE group markers lost");
  Alcotest.(check bool) "absent ENVELOPE" true
    (expect_ok (Imap.Response.fetch_envelope (row
      "* 1 FETCH (UID 7 FLAGS ())\r\n")) = None);
  let nested=row
    "* 1 FETCH (FUTURE (ENVELOPE (NIL)) ENVELOPE (NIL \"\" NIL NIL NIL NIL NIL NIL NIL NIL))\r\n" in
  (match expect_ok (Imap.Response.fetch_envelope nested) with
   | Some {subject=Some "";_} -> ()
   | _ -> fail "nested ENVELOPE mistaken for top-level field");
  List.iter (fun source ->
    match Imap.Response.fetch_envelope (row source) with
    | Error _ -> () | Ok _ -> fail "malformed ENVELOPE accepted") [
      "* 1 FETCH (ENVELOPE NIL)\r\n";
      "* 1 FETCH (ENVELOPE (NIL NIL NIL NIL NIL NIL NIL NIL NIL))\r\n";
      "* 1 FETCH (ENVELOPE (NIL NIL () NIL NIL NIL NIL NIL NIL NIL))\r\n";
      "* 1 FETCH (ENVELOPE (NIL NIL ((NIL NIL \"a\")) NIL NIL NIL NIL NIL NIL NIL))\r\n";
      "* 1 FETCH (ENVELOPE (NIL NIL NIL NIL NIL NIL NIL NIL NIL NIL) ENVELOPE (NIL NIL NIL NIL NIL NIL NIL NIL NIL NIL))\r\n"];
  let d=Imap.Wire.create () in
  let parts=wire_ok (Imap.Wire.feed d
    "* 1 FETCH (ENVELOPE (NIL {5}\r\nHello NIL NIL NIL NIL NIL NIL NIL NIL))\r\n") in
  let retained=match expect_ok (Imap.Response.parse_parts parts) with
    | Imap.Response.Untagged (Fetch value) -> value
    | _ -> fail "literal ENVELOPE FETCH absent" in
  (match expect_ok (Imap.Response.fetch_envelope retained) with
   | Some {subject=Some "Hello";_} -> ()
   | _ -> fail "literal ENVELOPE subject lost");
  (match Imap.Response.parse_parts ~max_control_literal:4 parts with
   | Error _ -> () | Ok _ -> fail "unbounded ENVELOPE literal accepted");
  let d=Imap.Wire.create () in
  let parts=wire_ok (Imap.Wire.feed d
    "* 1 FETCH (ENVELOPE (NIL NIL NIL NIL NIL NIL NIL NIL NIL NIL) BODY[] {4}\r\nTest)\r\n") in
  let with_body=match expect_ok (Imap.Response.parse_parts parts) with
    | Imap.Response.Untagged (Fetch value) -> value
    | _ -> fail "ENVELOPE and body FETCH absent" in
  Alcotest.(check (list (pair string int64))) "body still streamed"
    ["BODY[]",4L] with_body.literals;
  (match expect_ok (Imap.Response.fetch_envelope with_body) with
   | Some _ -> () | None -> fail "ENVELOPE before body absent");
  (* The unterminated quote swallows the closing parentheses, so the FETCH
     row itself is rejected before ENVELOPE decoding. *)
  (match Imap.Response.parse
    ("* 1 FETCH (ENVELOPE (NIL \"unterminated NIL NIL NIL NIL NIL NIL NIL " ^
     "NIL))\r\n") with
   | Error _ -> ()
   | Ok (Imap.Response.Untagged (Fetch value)) ->
       (match Imap.Response.fetch_envelope value with
        | Error _ -> () | Ok _ -> fail "unterminated ENVELOPE quote accepted")
   | Ok _ -> fail "unterminated ENVELOPE quote accepted")

let test_bodystructure () =
  let row source=match expect_ok (Imap.Response.parse source) with
    | Imap.Response.Untagged (Fetch row) -> row
    | _ -> fail "missing BODYSTRUCTURE FETCH" in
  let structure="((\"TEXT\" \"PLAIN\" (\"CHARSET\" \"UTF-8\") NIL NIL \"7BIT\" 12 2 NIL (\"INLINE\" NIL) (\"en\") NIL) (\"MESSAGE\" \"RFC822\" NIL NIL NIL \"7BIT\" 100 (NIL \"Forward\" NIL NIL NIL NIL NIL NIL NIL NIL) (\"TEXT\" \"PLAIN\" NIL NIL NIL \"7BIT\" 40 3) 5) \"MIXED\" (\"BOUNDARY\" \"abc\") NIL NIL NIL (\"future\" (NIL 7)))" in
  let fetched=row ("* 1 FETCH (UID 7 BODYSTRUCTURE " ^ structure ^ ")\r\n") in
  (match expect_ok (Imap.Response.fetch_bodystructure fetched) with
   | Some (Imap.Response.Multipart {parts=[
       Imap.Response.Single_part {media_type="TEXT";lines=Some 2L;
         parameters=Some ["CHARSET","UTF-8"];_};
       Imap.Response.Single_part {enclosed=Some
         ({subject=Some "Forward";_},
          Imap.Response.Single_part {media_type="TEXT";_},5L);_}];
       extensions=[Imap.Response.Ext_list _;Imap.Response.Ext_nil;
         Imap.Response.Ext_nil;Imap.Response.Ext_nil;
         Imap.Response.Ext_list _];_}) -> ()
   | _ -> fail "nested multipart/message BODYSTRUCTURE lost fields");
  List.iter (fun source ->
    match Imap.Response.fetch_bodystructure (row source) with
    | Error _ -> () | Ok _ -> fail "malformed BODYSTRUCTURE accepted") [
      "* 1 FETCH (BODYSTRUCTURE NIL)\r\n";
      "* 1 FETCH (BODYSTRUCTURE (\"TEXT\" \"PLAIN\" NIL NIL NIL \"7BIT\" 4))\r\n";
      "* 1 FETCH (BODYSTRUCTURE (\"TEXT\" \"PLAIN\" NIL NIL NIL \"7BIT\" 4 1) BODYSTRUCTURE (\"TEXT\" \"PLAIN\" NIL NIL NIL \"7BIT\" 4 1))\r\n";
      "* 1 FETCH (BODYSTRUCTURE (\"TEXT\" \"PLAIN\" NIL NIL NIL \"7BIT\" 4 1 NIL (\"INLINE\" ())))\r\n";
      "* 1 FETCH (BODYSTRUCTURE ((\"TEXT\" \"PLAIN\" NIL NIL NIL \"7BIT\" 4 1) \"MIXED\" (\"BAD\")))\r\n";
      "* 1 FETCH (BODYSTRUCTURE (\"TEXT\" \"PLAIN\" NIL NIL NIL \"7BIT\" 4294967296 1))\r\n";
      "* 1 FETCH (BODYSTRUCTURE (\"TEXT\" \"PLAIN\" NIL NIL NIL \"7BIT\" 4 1 NIL NIL bad-atom))\r\n"];
  let rec nested depth body=if depth=0 then body else
    nested (depth-1) ("(" ^ body ^ " \"MIXED\")") in
  let too_deep=nested 18
    "(\"TEXT\" \"PLAIN\" NIL NIL NIL \"7BIT\" 1 1)" in
  (match Imap.Response.fetch_bodystructure
    (row ("* 1 FETCH (BODYSTRUCTURE " ^ too_deep ^ ")\r\n")) with
   | Error _ -> () | Ok _ -> fail "deep BODYSTRUCTURE accepted");
  let d=Imap.Wire.create () in
  let parts=wire_ok (Imap.Wire.feed d
    "* 1 FETCH (BODYSTRUCTURE (\"TEXT\" \"PLAIN\" (\"CHARSET\" {5}\r\nUTF-8) NIL NIL \"7BIT\" 12 2))\r\n") in
  let literal_row=match expect_ok (Imap.Response.parse_parts parts) with
    | Imap.Response.Untagged (Fetch row) -> row
    | _ -> fail "literal BODYSTRUCTURE FETCH absent" in
  (match expect_ok (Imap.Response.fetch_bodystructure literal_row) with
   | Some (Imap.Response.Single_part
       {parameters=Some ["CHARSET","UTF-8"];_}) -> ()
   | _ -> fail "literal BODYSTRUCTURE parameter lost");
  (match Imap.Response.parse_parts ~max_control_literal:4 parts with
   | Error _ -> () | Ok _ -> fail "unbounded BODYSTRUCTURE literal accepted")

let test_sort_thread_commands () =
  let open Imap.Command in
  Alcotest.(check string) "per-key reverse" "UID SORT (SUBJECT REVERSE DATE) UTF-8 ALL"
    (command_ok (uid_sort ~keys:[Subject,Ascending;Date,Descending]
      ~charset:"UTF-8" ~criterion:"ALL"));
  Alcotest.(check string) "references" "UID THREAD REFERENCES US-ASCII UID 1:50"
    (command_ok (uid_thread ~algorithm:References ~charset:"US-ASCII"
      ~criterion:"UID 1:50"));
  Alcotest.(check string) "ordered subject" "UID THREAD ORDEREDSUBJECT UTF-8 ALL"
    (command_ok (uid_thread ~algorithm:Orderedsubject ~charset:"UTF-8"
      ~criterion:"ALL"));
  List.iter (function Error _ -> () | Ok _ -> fail "invalid SORT/THREAD command accepted")
    [uid_sort ~keys:[] ~charset:"UTF-8" ~criterion:"ALL";
     uid_sort ~keys:[Arrival,Ascending] ~charset:"" ~criterion:"ALL";
     uid_thread ~algorithm:References ~charset:"UTF-8" ~criterion:"   ";
     uid_thread ~algorithm:References ~charset:"UTF-8\r\nNOOP" ~criterion:"ALL";
     uid_thread ~algorithm:References ~charset:"UTF-8" ~criterion:"ALL\r\nNOOP"]

let test_sort_thread_responses () =
  let leaf n : Imap.Response.thread = {number=Some n;children=[]} in
  let node n children : Imap.Response.thread = {number=Some n;children} in
  (match expect_ok (Imap.Response.parse "* SORT 9 2 7\r\n") with
   | Imap.Response.Untagged (Sort [9L;2L;7L]) -> ()
   | _ -> fail "SORT order changed");
  (match expect_ok (Imap.Response.parse "* THREAD (2)(3 6 (4 23)(44 7 96))") with
   | Imap.Response.Untagged (Thread roots) ->
       if roots<>[leaf 2L;node 3L [node 6L
         [node 4L [leaf 23L];node 44L [node 7L [leaf 96L]]]]] then
         fail "THREAD chains or branch order changed"
   | _ -> fail "THREAD result missing");
  (match expect_ok (Imap.Response.parse "* THREAD ((3)(5))") with
   | Imap.Response.Untagged (Thread [{number=None;children}])
     when children=[leaf 3L;leaf 5L] -> ()
   | _ -> fail "THREAD dummy parent lost");
  List.iter (fun raw -> match expect_ok (Imap.Response.parse raw) with
    | Imap.Response.Untagged (Sort [] | Thread []) -> ()
    | _ -> fail "empty ordered result rejected") ["* SORT";"* THREAD";"* THREAD "];
  let wire=Imap.Wire.create () in
  let parts=List.concat_map (fun chunk -> wire_ok (Imap.Wire.feed wire chunk))
    ["* TH";"READ ((3)";"(5))\r";"\n"] in
  (match expect_ok (Imap.Response.parse_parts parts) with
   | Imap.Response.Untagged (Thread [{number=None;children}])
     when children=[leaf 3L;leaf 5L] -> ()
   | _ -> fail "fragmented THREAD changed")

let test_sort_thread_invalid () =
  List.iter (fun raw -> match Imap.Response.parse raw with
    | Error _ -> () | Ok _ -> fail ("invalid ordered result accepted: " ^ raw))
    ["* SORT 0";"* SORT 01";"* SORT 4294967296";"* SORT 2 2";
     "* SORT 1  2";"* SORT 1 ";"* SORT 1 x";"* SORT ";
     "* THREAD ()";"* THREAD (0)";"* THREAD (01)";
     "* THREAD (4294967296)";"* THREAD (1)(1)";"* THREAD ((1)(1))";
     "* THREAD (1 (2))";"* THREAD ((1))";"* THREAD (1(2)(3))";
     "* THREAD (1  2)";"* THREAD (1) (2)";"* THREAD (1)junk";
     "* THREAD (1";"* THREAD (1))";"* THREAD  "];
  let chain n="* THREAD (" ^ String.concat " "
    (List.init n (fun i -> string_of_int (i+1))) ^ ")" in
  ignore (expect_ok (Imap.Response.parse (chain 100)));
  (* A chain is one wire level, so its length is bounded by the node limit
     rather than the nesting limit. *)
  let rec chain_length n = function
    | [] -> n
    | [{Imap.Response.children;_}] -> chain_length (n+1) children
    | _ -> fail "THREAD chain branched" in
  (match expect_ok (Imap.Response.parse (chain 5000)) with
   | Imap.Response.Untagged (Thread [root]) ->
       Alcotest.(check int) "long THREAD chain" 5000 (chain_length 0 [root])
   | _ -> fail "long THREAD chain rejected");
  (match expect_ok (Imap.Response.parse "* THREAD (1 2 (3)(4 5))") with
   | Imap.Response.Untagged (Thread [{number=Some 1L;children=[
       {number=Some 2L;children=[{number=Some 3L;children=[]};
         {number=Some 4L;children=[{number=Some 5L;children=[]}]}]}]}]) -> ()
   | _ -> fail "chain before a branch misparsed");
  let rec dummy depth =
    if depth=1 then "(1)"
    else "(" ^ dummy (depth-1) ^ "(" ^ string_of_int depth ^ "))" in
  ignore (expect_ok (Imap.Response.parse ("* THREAD " ^ dummy 100)));
  (match Imap.Response.parse ("* THREAD " ^ dummy 101) with
   | Error _ -> () | Ok _ -> fail "THREAD dummy nesting limit ignored");
  let flat n="* THREAD " ^ String.concat ""
    (List.init n (fun i -> "(" ^ string_of_int (i+1) ^ ")")) in
  (match expect_ok (Imap.Response.parse (flat 100_000)) with
   | Imap.Response.Untagged (Thread roots) when List.length roots=100_000 -> ()
   | _ -> fail "bounded wide THREAD lost nodes");
  (match Imap.Response.parse (flat 100_001) with
   | Error _ -> () | Ok _ -> fail "THREAD count limit ignored");
  let sort="* SORT " ^ String.concat " "
    (List.init 100_001 (fun i -> string_of_int (i+1))) in
  (match Imap.Response.parse sort with
   | Error _ -> () | Ok _ -> fail "SORT count limit ignored")

let test_esort_commands () =
  let open Imap.Command in
  let command returns=uid_sort_extended ~returns ~keys:[Date,Descending]
    ~charset:"UTF-8" ~criterion:"UNDELETED" in
  Alcotest.(check string) "default ALL"
    "UID SORT RETURN () (REVERSE DATE) UTF-8 UNDELETED"
    (command_ok (command []));
  Alcotest.(check string) "summary and positive range"
    "UID SORT RETURN (MIN MAX COUNT PARTIAL 500:400) (REVERSE DATE) UTF-8 UNDELETED"
    (command_ok (command [Min;Max;Count;Partial (500L,400L)]));
  Alcotest.(check string) "all and count"
    "UID SORT RETURN (ALL COUNT) (REVERSE DATE) UTF-8 UNDELETED"
    (command_ok (command [All;Count]));
  List.iter (fun returns -> match command returns with
    | Error _ -> () | Ok _ -> fail "invalid ESORT return options accepted")
    [[Min;Min];[Max;Max];[Count;Count];[All;All];[All;Partial (1L,2L)];
     [Partial (1L,2L);Partial (3L,4L)];[Partial (0L,1L)];
     [Partial (-1L,-2L)];[Partial (1L,4_294_967_296L)]]

let test_esearch_fields () =
  let parse s=Imap.Response.parse ("* ESEARCH (TAG \"S1\") UID " ^ s) in
  (match expect_ok (parse "MIN 9 MAX 2 COUNT 4 ALL 9,4:3,2 MODSEQ 0") with
   | Imap.Response.Untagged (Esearch
       {min=Some 9L;max=Some 2L;count=Some 4L;all=Some "9,4:3,2";
        modseq=Some 0L;_}) -> ()
   | _ -> fail "ESORT order or boundary values changed");
  (match expect_ok (parse "COUNT 0 PARTIAL (500:400 NIL)") with
   | Imap.Response.Untagged (Esearch
       {count=Some 0L;partial=Some ("500:400",None);_}) -> ()
   | _ -> fail "empty ESORT range lost");
  (match expect_ok (parse
    "MIN 4294967295 MAX 1 COUNT 4294967295 MODSEQ 9223372036854775807") with
   | Imap.Response.Untagged (Esearch _) -> ()
   | _ -> fail "valid ESEARCH scalar boundaries rejected");
  List.iter (fun fields -> match parse fields with
    | Error _ -> () | Ok _ -> fail ("invalid ESEARCH fields accepted: " ^ fields))
    ["MIN 0";"MAX 0";"MIN 4294967296";"MAX 4294967296";
     "COUNT 4294967296";"COUNT -1";"MODSEQ -1";
     "MODSEQ 9223372036854775808";"MIN 1 min 2";"MAX 1 MAX 2";
     "COUNT 0 COUNT 1";"MODSEQ 0 MODSEQ 1";"ALL 1 ALL 2";
     "PARTIAL (1:2 NIL) PARTIAL (3:4 5)";"PARTIAL 1:2";
     "PARTIAL (1:2 NIL) PARTIAL 1:2";"PARTIAL (0:1 NIL)"]

let test_searchres_commands () =
  let open Imap.Command in
  let check label expected command =
    Alcotest.(check string) label expected (command_ok command) in
  check "SAVE COUNT" "UID SEARCH RETURN (SAVE COUNT) UNSEEN"
    (uid_search_save ~criterion:"UNSEEN");
  check "refine saved" "UID SEARCH RETURN (ALL COUNT) UID $ (SMALLER 4096)"
    (uid_search_saved ~criterion:"SMALLER 4096");
  check "grouped quoted refinement"
    "UID SEARCH RETURN (ALL COUNT) UID $ (OR (SEEN) SUBJECT \"a ) ( b\")"
    (uid_search_saved ~criterion:"OR (SEEN) SUBJECT \"a ) ( b\"");
  List.iter (fun criterion -> match uid_search_saved ~criterion with
    | Error _ -> () | Ok _ -> fail "unsafe saved search criterion accepted")
    ["";"   ";"ALL\r\nNOOP";"ALL) RETURN (SAVE) (";
     "(ALL";"ALL)";"SUBJECT \"unterminated";"SUBJECT \"bad\\q\"";
     String.make 101 '(' ^ "ALL" ^ String.make 101 ')'];
  check "FETCH saved" "UID FETCH $ (UID FLAGS)"
    (uid_fetch_saved ~items:["UID";"FLAGS"] ());
  check "FETCH saved modifiers"
    "UID FETCH $ (UID FLAGS MODSEQ) (PARTIAL 1:50 CHANGEDSINCE 7 VANISHED)"
    (uid_fetch_saved ~items:["UID";"FLAGS";"MODSEQ"] ~partial:(1L,50L)
      ~changedsince:7L ~vanished:true ());
  check "STORE saved" "UID STORE $ (UNCHANGEDSINCE 9) +FLAGS.SILENT (\\Seen)"
    (uid_store_saved ~unchangedsince:9L ~operation:`Add ~silent:true
      ~flags:["\\Seen"] ());
  check "COPY saved" "UID COPY $ \"Saved Mail\""
    (uid_copy_saved ~mailbox:"Saved Mail");
  check "MOVE saved" "UID MOVE $ Archive"
    (uid_move_saved ~mailbox:"Archive");
  Alcotest.(check string) "EXPUNGE saved" "UID EXPUNGE $" uid_expunge_saved;
  List.iter (function Error _ -> () | Ok _ -> fail "invalid saved command accepted")
    [uid_search_save ~criterion:"   ";uid_search_save ~criterion:"ALL\r\nNOOP";
     uid_fetch_saved ~items:["UNKNOWN"] ();
     uid_fetch_saved ~items:["UID"] ~vanished:true ();
     uid_fetch_saved ~items:["UID"] ~changedsince:(-1L) ();
     uid_fetch_saved ~items:["UID"] ~partial:(0L,1L) ();
     uid_store_saved ~unchangedsince:(-1L) ~operation:`Replace ~silent:false ~flags:[] ();
     uid_store_saved ~operation:`Add ~silent:false ~flags:["bad flag"] ();
     uid_copy_saved ~mailbox:"Archive\r\nNOOP";
     uid_move_saved ~mailbox:"Archive\r\nNOOP"];
  List.iter (function Error _ -> () | Ok _ -> fail "ordinary UID API accepted saved set")
    [uid_fetch ~set:"$" ~items:["UID"];
     uid_fetch_mod ~set:"$" ~items:["UID"] ~partial:(1L,10L) ();
     uid_fetch_preview ~set:"$" ~lazy_:true;
     uid_store ~set:"$" ~operation:`Add ~silent:true ~flags:[];
     uid_copy ~set:"$" ~mailbox:"Archive";
     uid_move ~set:"$" ~mailbox:"Archive";
     uid_expunge ~set:"$"]

let test_binary_sections () =
  let open Imap.Command in
  Alcotest.(check string) "numeric decoded section"
    "UID FETCH 7 (UID BINARY.PEEK[1.2]<4294967296.9>)"
    (command_ok (uid_fetch_binary ~set:"7" ~section:[1;2]
      ~partial:(4_294_967_296L,9L) ()));
  Alcotest.(check string) "empty section"
    "UID FETCH 7 (UID BINARY.PEEK[])"
    (command_ok (uid_fetch_binary ~set:"7" ~section:[] ()));
  Alcotest.(check string) "decoded size"
    "UID FETCH 7:9 (UID BINARY.SIZE[2])"
    (command_ok (uid_fetch_binary_size ~set:"7:9" ~section:[2]));
  ignore (command_ok (uid_fetch_binary ~set:"7" ~section:[1]
    ~partial:(Int64.max_int,Int64.max_int) ()));
  List.iter (function Error _ -> () | Ok _ -> fail "invalid BINARY command accepted")
    [uid_fetch_binary ~set:"7\r\nNOOP" ~section:[1] ();
     uid_fetch_binary ~set:"7" ~section:[0] ();
     uid_fetch_binary ~set:"7" ~section:[-1] ();
     uid_fetch_binary ~set:"7" ~section:[4_294_967_296] ();
     uid_fetch_binary ~set:"7" ~section:(List.init 101 (fun _ -> 1)) ();
     uid_fetch_binary ~set:"7" ~section:[1] ~partial:(-1L,2L) ();
     uid_fetch_binary ~set:"7" ~section:[1] ~partial:(0L,0L) ();
     uid_fetch_binary_size ~set:"$" ~section:[1]]

let test_binary_attributes () =
  List.iter (fun raw -> match Imap.Response.parse raw with
    | Error _ -> () | Ok _ -> fail "duplicate FETCH UID accepted")
    ["* 1 FETCH (UID 7 UID 7 BINARY[1] NIL)";
     "* 1 FETCH (UID 8 UID 7 BINARY[1] NIL)";
     "* 7 UIDFETCH (UID 7 UID 7 BINARY[1] NIL)"];
  let row fields = match expect_ok (Imap.Response.parse
    ("* 1 FETCH (UID 7 " ^ fields ^ ")")) with
    | Imap.Response.Untagged (Fetch row) -> row
    | _ -> fail "FETCH absent" in
  let get fields = Imap.Response.fetch_binary (row fields) ~section:[1] ~offset:None in
  (match expect_ok (get "FLAGS ()") with None -> () | _ -> fail "absent BINARY changed");
  (match expect_ok (get "BINARY[1] NIL") with Some Nil -> () | _ -> fail "BINARY NIL lost");
  (match expect_ok (get "BINARY[1] \"\"") with Some (Inline "") -> () | _ -> fail "empty inline lost");
  (match expect_ok (get "BINARY[1] \"a\\\"b\"") with
   | Some (Inline "a\"b") -> () | _ -> fail "quoted BINARY decoding changed");
  let offset_row=row "BINARY[1]<4294967296> \"abc\"" in
  (match expect_ok (Imap.Response.fetch_binary offset_row ~section:[1]
    ~offset:(Some 4_294_967_296L)) with
   | Some (Inline "abc") -> () | _ -> fail "BINARY origin lost");
  (match Imap.Response.fetch_binary offset_row ~section:[1] ~offset:None with
   | Error _ -> () | Ok _ -> fail "unrequested partial matched whole section");
  List.iter (fun fields -> match get fields with
    | Error _ -> () | Ok _ -> fail ("invalid BINARY accepted: " ^ fields))
    ["BINARY[2] \"wrong\" BINARY[1] \"right\"";"BINARY[2] NIL";
     "BINARY[1] 42";"BINARY[1] \"a\" BINARY[1] \"a\"";
     "BINARY[1] NIL BINARY[1] NIL";"BINARY[0] NIL";
     "BINARY[1.MIME] NIL";"BINARY[01] NIL";"BINARY[1]<0.3> NIL";
     "BINARY[1]<-1> NIL";"BINARY[1]<9223372036854775808> NIL";
     "BINARY.PEEK[1] NIL";"BINARY[1] \"bad\000value\""];
  List.iter (fun size ->
    let result=expect_ok (Imap.Response.fetch_binary_size
      (row ("BINARY.SIZE[1] " ^ Int64.to_string size)) ~section:[1]) in
    if result<>Some size then fail "BINARY.SIZE lost value")
    [0L;4_294_967_296L;Int64.max_int];
  List.iter (fun fields -> match Imap.Response.fetch_binary_size (row fields) ~section:[1] with
    | Error _ -> () | Ok _ -> fail "invalid BINARY.SIZE accepted")
    ["BINARY.SIZE[1] NIL";"BINARY.SIZE[1] -1";
     "BINARY.SIZE[1] 9223372036854775808";"BINARY.SIZE[1] \"42\"";
     "BINARY.SIZE[1] 1 BINARY.SIZE[1] 1";"BINARY.SIZE[1]<0> 1"];
  List.iter (fun marker ->
    let wire=Imap.Wire.create () in
    let events=List.concat_map (fun chunk -> wire_ok (Imap.Wire.feed wire chunk))
      ["* 1 FETCH (UID 7 BINARY[1] " ^ marker ^ "{3}\r\n";
       "a\000";"b)\r\n"] in
    let bytes=List.filter_map (function Imap.Wire.Literal_chunk s -> Some s | _ -> None)
      events |> String.concat "" in
    Alcotest.(check string) "binary octets" "a\000b" bytes;
    match expect_ok (Imap.Response.parse_parts events) with
    | Imap.Response.Untagged (Fetch row) ->
        (match expect_ok (Imap.Response.fetch_binary row ~section:[1] ~offset:None) with
         | Some (Literal 3L) -> () | _ -> fail "BINARY literal metadata lost")
    | _ -> fail "BINARY literal FETCH absent") ["";"~"]

let test_rejection_codes () =
  let open Imap.Response in
  let cases=[Unavailable,"UNAVAILABLE";Authenticationfailed,"AUTHENTICATIONFAILED";
    Authorizationfailed,"AUTHORIZATIONFAILED";Expired,"EXPIRED";
    Privacyrequired,"PRIVACYREQUIRED";Contactadmin,"CONTACTADMIN";
    Noperm,"NOPERM";Inuse,"INUSE";Expungeissued,"EXPUNGEISSUED";
    Corruption,"CORRUPTION";Serverbug,"SERVERBUG";Clientbug,"CLIENTBUG";
    Cannot,"CANNOT";Limit,"LIMIT";Overquota,"OVERQUOTA";
    Alreadyexists,"ALREADYEXISTS";Nonexistent,"NONEXISTENT";
    Unknown_cte,"UNKNOWN-CTE";Trycreate,"TRYCREATE";
    Compressionactive,"COMPRESSIONACTIVE"] in
  List.iter (fun (expected,name) ->
    Alcotest.(check (option string)) "constant name" (Some name)
      (response_code_name expected);
    List.iter (fun name ->
      (match expect_ok (parse ("A1 NO [" ^ name ^ "] server detail")) with
       | Tagged {code=Some actual;text="server detail";_} when actual=expected -> ()
       | _ -> fail ("typed rejection missing: " ^ name));
      (match expect_ok (parse ("* NO [" ^ name ^ "] server detail")) with
       | Untagged (No (Some actual,"server detail")) when actual=expected -> ()
       | _ -> fail ("typed untagged rejection missing: " ^ name)))
      [name;String.lowercase_ascii name];
    match parse ("A1 NO [" ^ name ^ " secret-argument] server detail") with
    | Error _ -> () | Ok _ -> fail "known no-argument code accepted payload") cases;
  (match expect_ok (parse "A1 NO [X-VENDOR arbitrary payload] detail") with
   | Tagged {code=Some (Other_code "X-VENDOR arbitrary payload" as code);_} ->
       Alcotest.(check (option string)) "unknown code has no safe name" None
         (response_code_name code)
   | _ -> fail "unknown response code was not retained");
  List.iter (fun (code,name) ->
    Alcotest.(check (option string)) "parameterized name has no payload" (Some name)
      (response_code_name code))
    [Mailboxid "server-secret","MAILBOXID";
     Modified "1:99","MODIFIED";Badevent ["server-secret"],"BADEVENT";
     Metadata_maxsize 12L,"METADATA";Messagelimit (100L,Some 8L),"MESSAGELIMIT";
     Appenduid_set (1L,"2:4"),"APPENDUID"];
  List.iter (fun raw -> match parse raw with
    | Error _ -> () | Ok _ -> fail "malformed known response code accepted")
    ["A1 NO [UIDNEXT 2 extra] detail";"A1 NO [AUTHENTICATIONFAILED detail"]

let test_binary_append_prefix () =
  let date=expect_ok (Imap.Internal_date.of_string " 1-Jan-2024 01:02:03 +0530") in
  Alcotest.(check string) "literal8 with flags and date"
    "APPEND \"Binary Mail\" (\\Seen custom) \" 1-Jan-2024 01:02:03 +0530\" ~{3}\r\n"
    (command_ok (Imap.Command.append_binary_prefix ~mailbox:"Binary Mail"
      ~flags:["\\Seen";"custom"] ~internal_date:date ~size:3L ()));
  Alcotest.(check string) "zero literal8" "APPEND INBOX ~{0}\r\n"
    (command_ok
      (Imap.Command.append_binary_prefix ~mailbox:"INBOX" ~size:0L ()));
  Alcotest.(check string) "ordinary marker unchanged" "APPEND INBOX {0}\r\n"
    (command_ok (Imap.Command.append_prefix ~mailbox:"INBOX" ~size:0L ()));
  List.iter (function Error _ -> () | Ok _ -> fail "invalid binary APPEND accepted")
    [Imap.Command.append_binary_prefix ~mailbox:"INBOX" ~size:(-1L) ();
     Imap.Command.append_binary_prefix ~mailbox:"INBOX\r\nNOOP" ~size:0L ();
     Imap.Command.append_binary_prefix ~mailbox:"INBOX" ~flags:["bad flag"] ~size:0L ();
     Imap.Command.append_binary_prefix ~mailbox:"INBOX" ~flags:["x\r\nNOOP"] ~size:0L ()]

let test_capability () =
  let module C = Imap.Capability in
  let cap = Alcotest.testable C.pp ( = ) in
  List.iter (fun (wire,expected,canonical) ->
    Alcotest.check cap ("parse " ^ wire) expected (C.of_wire wire);
    Alcotest.(check string) ("print " ^ wire) canonical
      (C.to_wire (C.of_wire wire)))
    C.[ "IMAP4rev2",Imap4rev2,"IMAP4REV2";
        "imap4rev1",Imap4rev1,"IMAP4REV1";
        "auth=plain",Auth "PLAIN","AUTH=PLAIN";
        "AUTH=SCRAM-SHA-256",Auth "SCRAM-SHA-256","AUTH=SCRAM-SHA-256";
        "LoginDisabled",Login_disabled,"LOGINDISABLED";
        "literal-",Literal_minus,"LITERAL-";
        "LITERAL+",Literal_plus,"LITERAL+";
        "Sort=Display",Sort_display,"SORT=DISPLAY";
        "CONTEXT=SEARCH",Context `Search,"CONTEXT=SEARCH";
        "context=sort",Context `Sort,"CONTEXT=SORT";
        "THREAD=orderedsubject",Thread Orderedsubject,
          "THREAD=ORDEREDSUBJECT";
        "THREAD=REFERENCES",Thread References,"THREAD=REFERENCES";
        "thread=refs",Thread (Imap.Thread.Other "REFS"),"THREAD=REFS";
        "objectid+",Objectid_plus,"OBJECTID+";
        "OBJECTID",Objectid,"OBJECTID";
        "MESSAGELIMIT=1000",Messagelimit 1000L,"MESSAGELIMIT=1000";
        "savelimit=4294967295",Savelimit 4294967295L,
          "SAVELIMIT=4294967295";
        "UTF8=Accept",Utf8 `Accept,"UTF8=ACCEPT";
        "UTF8=ONLY",Utf8 `Only,"UTF8=ONLY";
        "compress=deflate",Compress `Deflate,"COMPRESS=DEFLATE";
        "QUOTA=RES-STORAGE",Quota_res "STORAGE","QUOTA=RES-STORAGE";
        "quotaset",Quotaset,"QUOTASET";
        "Metadata-Server",Metadata_server,"METADATA-SERVER";
        "STATUS=SIZE",Status_size,"STATUS=SIZE";
        "X-Vendor",Other "X-Vendor","X-Vendor";
        "MESSAGELIMIT=x",Other "MESSAGELIMIT=x","MESSAGELIMIT=x";
        "MESSAGELIMIT=0",Other "MESSAGELIMIT=0","MESSAGELIMIT=0";
        "SAVELIMIT=01",Other "SAVELIMIT=01","SAVELIMIT=01";
        "MESSAGELIMIT=4294967296",Other "MESSAGELIMIT=4294967296",
          "MESSAGELIMIT=4294967296";
        "AUTH=",Other "AUTH=","AUTH=";
        "QUOTA=RES-",Other "QUOTA=RES-","QUOTA=RES-";
        "UTF8=MAYBE",Other "UTF8=MAYBE","UTF8=MAYBE" ];
  Alcotest.(check bool) "malformed limit" true
    (C.malformed_limit (C.of_wire "savelimit=0"));
  Alcotest.(check bool) "valid limit" false
    (C.malformed_limit (C.of_wire "SAVELIMIT=5"));
  Alcotest.(check bool) "unknown token" false
    (C.malformed_limit (C.of_wire "X-LIMIT=0"));
  Alcotest.(check bool) "Other equals its known spelling" true
    (C.equal (C.Other "idle") C.Idle);
  Alcotest.(check bool) "unknown compares case-insensitively" true
    (C.equal (C.Other "x-a") (C.Other "X-A"));
  Alcotest.(check bool) "parameters distinguish" false
    (C.equal (C.Messagelimit 1L) (C.Messagelimit 2L));
  let set = C.Set.of_list
    C.[Other "idle"; Idle; Messagelimit 9L; Messagelimit 3L; Auth "plain";
       Auth "XOAUTH2"; Thread References; Other "x-a"; Other "X-A";
       Quota_res "storage"] in
  Alcotest.(check int) "deduplicated" 8 (List.length (C.Set.to_list set));
  Alcotest.(check bool) "Other stored as known" true
    (List.mem C.Idle (C.Set.to_list set));
  Alcotest.(check bool) "mem ignores case" true
    (C.Set.mem (C.Other "IDLE") set);
  Alcotest.(check bool) "not member" false (C.Set.mem C.Move set);
  Alcotest.(check (option int64)) "smallest MESSAGELIMIT" (Some 3L)
    (C.messagelimit set);
  Alcotest.(check (option int64)) "no SAVELIMIT" None (C.savelimit set);
  Alcotest.(check (list string)) "mechanisms" ["PLAIN";"XOAUTH2"]
    (C.auth_mechanisms set);
  Alcotest.(check bool) "algorithms" true
    (C.thread_algorithms set = [Imap.Thread.References]);
  Alcotest.(check (list string)) "quota resources" ["STORAGE"]
    (C.quota_resources set);
  Alcotest.(check bool) "union" true
    (C.Set.mem C.Move (C.Set.union set (C.Set.of_list [C.Move])));
  Alcotest.(check bool) "empty" true (C.Set.is_empty C.Set.empty);
  List.iter (fun c ->
    Alcotest.(check bool) ("rev2 folds " ^ C.to_wire c) true
      (C.implied_by_rev2 c))
    C.[Enable; Idle; Namespace; Uidplus; Move; Searchres; Esearch;
       List_extended; List_status; Unselect; Sasl_ir; Literal_minus;
       Status_size; Other "move"];
  List.iter (fun c ->
    Alcotest.(check bool) ("rev2 does not fold " ^ C.to_wire c) false
      (C.implied_by_rev2 c))
    C.[Binary; Literal_plus; Condstore; Qresync; Special_use; Children;
       Imap4rev2; Imap4rev1; Multiappend; Esort; Utf8 `Accept; Objectid]

let test_capability_responses () =
  let module C = Imap.Capability in
  let open Imap.Response in
  (match expect_ok (parse "* CAPABILITY IMAP4rev1 idle IDLE AUTH=plain X-A")
   with
   | Untagged (Capability caps) ->
       Alcotest.(check (list string)) "typed and deduplicated"
         ["AUTH=PLAIN";"IDLE";"IMAP4REV1";"X-A"] (List.map C.to_wire caps)
   | _ -> fail "CAPABILITY response not typed");
  (match expect_ok (parse "* ENABLED QRESYNC condstore QRESYNC") with
   | Untagged (Enabled caps) ->
       Alcotest.(check bool) "ENABLED typed" true
         (caps = C.[Condstore; Qresync])
   | _ -> fail "ENABLED response not typed");
  (match expect_ok (parse "* ENABLED") with
   | Untagged (Enabled []) -> ()
   | _ -> fail "empty ENABLED rejected");
  (match expect_ok
     (parse "* OK [CAPABILITY IMAP4rev2 MOVE move SASL-IR] ready") with
   | Untagged (Ok (Some (Capability caps as code),"ready")) ->
       Alcotest.(check bool) "code typed" true
         (caps = C.[Imap4rev2; Move; Sasl_ir]);
       Alcotest.(check (option string)) "code name" (Some "CAPABILITY")
         (response_code_name code)
   | _ -> fail "CAPABILITY code not typed");
  (match expect_ok (parse "A1 OK [capability IMAP4rev1] done") with
   | Tagged {code=Some (Capability [C.Imap4rev1]);_} -> ()
   | _ -> fail "lowercase CAPABILITY code not typed");
  Alcotest.(check string) "ENABLE encoding" "ENABLE QRESYNC UTF8=ACCEPT X-a"
    (command_ok (Imap.Command.enable C.[Qresync; Utf8 `Accept; Other "X-a"]));
  List.iter (fun caps ->
    Alcotest.(check bool) "ENABLE refused" true
      (Result.is_error (Imap.Command.enable caps)))
    C.[[]; [Other "X Y"]; [Other "X\r\nA1 LOGOUT"]; [Other "(X"]]

let () =
  Alcotest.run "IMAP protocol"
    ["wire", [Alcotest.test_case "fragmented literal" `Quick test_fragmented_literal;
              Alcotest.test_case "status text" `Quick test_literal_status_text;
              Alcotest.test_case "BINARY literal" `Quick test_binary_literal;
              Alcotest.test_case "LIST literal" `Quick test_list_literal;
              Alcotest.test_case "framing errors" `Quick test_wire_errors;
              Alcotest.test_case "invalid values" `Quick test_bad_values;
              Alcotest.test_case "command validation" `Quick
                test_command_validation;
              Alcotest.test_case "response review" `Quick
                test_response_review;
              Alcotest.test_case "sync metadata" `Quick test_sync_metadata];
     "extensions", [Alcotest.test_case "SELECT/QRESYNC" `Quick test_select_and_qresync;
                    Alcotest.test_case "mutation receipts" `Quick test_mutation_extensions;
                    Alcotest.test_case "discovery and OBJECTID" `Quick test_discovery_and_objectid;
                    Alcotest.test_case "extended discovery" `Quick test_extended_discovery;
                    Alcotest.test_case "UIDBATCHES and PARTIAL" `Quick test_uidbatches_and_partial;
                    Alcotest.test_case "UIDONLY" `Quick test_uidonly;
                    Alcotest.test_case "ACL and QUOTA" `Quick test_acl_quota;
                    Alcotest.test_case "METADATA and NOTIFY" `Quick test_metadata_notify];
     "rejection codes", [Alcotest.test_case "typed names and sanitization" `Quick test_rejection_codes];
     "binary", [Alcotest.test_case "APPEND literal8" `Quick test_binary_append_prefix;
       Alcotest.test_case "section constructors" `Quick test_binary_sections;
       Alcotest.test_case "typed attributes" `Quick test_binary_attributes];
     "searchres", [Alcotest.test_case "saved-result commands" `Quick test_searchres_commands];
     "esort", [Alcotest.test_case "return options" `Quick test_esort_commands;
       Alcotest.test_case "strict ESEARCH fields" `Quick test_esearch_fields];
     "sort/thread", [Alcotest.test_case "commands" `Quick test_sort_thread_commands;
       Alcotest.test_case "responses" `Quick test_sort_thread_responses;
       Alcotest.test_case "bounds and malformed grammar" `Quick test_sort_thread_invalid];
     "mailboxes", [Alcotest.test_case "legacy management" `Quick
       test_mailbox_management];
     "drafts", [Alcotest.test_case "OBJECTID+ -06" `Quick
       test_objectid_plus_draft];
     "preview", [Alcotest.test_case "RFC 8970" `Quick test_preview];
     "envelope", [Alcotest.test_case "RFC 3501/9051" `Quick test_envelope];
     "bodystructure", [Alcotest.test_case "RFC 3501/9051" `Quick
       test_bodystructure];
     "capability", [Alcotest.test_case "tokens and sets" `Quick
                      test_capability;
                    Alcotest.test_case "responses" `Quick
                      test_capability_responses];
     "scalars", [Alcotest.test_case "UID set" `Quick test_uid_set;
                 Alcotest.test_case "UID set syntax" `Quick
                   test_uid_set_syntax;
                 Alcotest.test_case "UID set algebra" `Quick
                   test_uid_set_algebra;
                 Alcotest.test_case "INTERNALDATE" `Quick test_internal_date;
                 Alcotest.test_case "modified UTF-7" `Quick test_modified_utf7]]
