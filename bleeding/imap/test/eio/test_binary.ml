module C=Imap_eio.Client
module S=Imap_eio.Selected
module E=Imap_eio.Error
let ok = function Ok x -> x | Error e -> failwith (C.error_to_string e)
let expect label kind = function
  | Error e when kind e -> ()
  | Error e -> failwith (label ^ ": " ^ C.error_to_string e)
  | Ok _ -> failwith (label ^ ": unexpectedly succeeded")
let protocol=function E.Protocol _ -> true | _ -> false
let limit=function E.Limit _ -> true | _ -> false
let state=function E.State _ -> true | _ -> false
let rejected=function E.Rejected _ -> true | _ -> false
let missing=function E.Missing_uid 7L -> true | _ -> false
let tag n=Printf.sprintf "A%08d" n
let done_ n=tag n ^ " OK done\r\n"

let scripted ?(caps="IMAP4rev1 BINARY UNSELECT") ?revision ?(uidonly=false)
    ?(dispatched=true) ~reply f =
  Eio_mock.Backend.run @@ fun () ->
  Eio.Switch.run @@ fun sw ->
  let flow=Eio_mock.Flow.make "binary" in
  let caps=caps ^ if uidonly then " ENABLE UIDONLY" else "" in
  let next=ref 4 in
  let revision_replies=match revision with
    | None -> []
    | Some enabled ->
        let n= !next in incr next;
        [`Return (if enabled then "* ENABLED IMAP4rev2\r\n" ^ done_ n
          else tag n ^ " NO unsupported\r\n")] in
  let uidonly_replies=if uidonly then (
    let n= !next in incr next;
    [`Return ("* ENABLED UIDONLY\r\n" ^ done_ n)]) else [] in
  let selected= !next in
  Eio_mock.Flow.on_read flow ([
    `Return "* OK ready\r\n";
    `Return ("* CAPABILITY " ^ caps ^ "\r\n" ^ done_ 1);
    `Return (done_ 2);
    `Return ("* CAPABILITY " ^ caps ^ "\r\n" ^ done_ 3)] @
    revision_replies @ uidonly_replies @
    [`Return ("* 2 EXISTS\r\n* OK [UIDVALIDITY 1] valid\r\n" ^
      "* OK [UIDNEXT 10] next\r\n" ^ done_ selected)] @
    (if dispatched then reply (selected+1) else []) @
    [`Return (done_ (selected + if dispatched then 2 else 1))]);
  let auth=Imap_eio.Auth.password ~username:"u" ~password:"p"
    ~allow_insecure_transport:true () in
  let client=ok (C.of_flow ~sw ~auth flow) in
  if uidonly then ignore (ok (C.enable_uidonly client));
  Fun.protect ~finally:(fun () -> C.close client) (fun () ->
    C.with_mailbox client ~mode:`Read_only "INBOX" f)

let fetch ?max_bytes ?partial selected sink =
  S.fetch_binary_to selected ?max_bytes ?partial ~uid:7L ~section:[2] sink
let fetch_reply fields n = [`Return ("* 1 FETCH (UID 7 " ^ fields ^ ")\r\n" ^ done_ n)]
let check_value label expected bytes fields =
  let sink=Buffer.create 16 in
  let result=ok (scripted ~reply:(fetch_reply fields)
    (fun selected -> fetch selected (Eio.Flow.buffer_sink sink))) in
  if result<>expected || Buffer.contents sink<>bytes then failwith label

let test_representations () =
  check_value "literal8 NUL" (Some 3L) "a\000b" "BINARY[2] ~{3}\r\na\000b";
  check_value "ordinary literal" (Some 3L) "abc" "BINARY[2] {3}\r\nabc";
  check_value "quoted escapes" (Some 4L) "a\"\\b" "BINARY[2] \"a\\\"\\\\b\"";
  check_value "empty quoted" (Some 0L) "" "BINARY[2] \"\"";
  check_value "empty literal8" (Some 0L) "" "BINARY[2] ~{0}\r\n";
  check_value "empty ordinary literal" (Some 0L) "" "BINARY[2] {0}\r\n";
  check_value "NIL" None "" "BINARY[2] NIL"

let test_partial () =
  List.iter (fun (fields,expected,bytes) ->
    let sink=Buffer.create 8 in
    let result=ok (scripted ~reply:(fetch_reply fields) (fun selected ->
      fetch ~partial:(2L,5L) selected (Eio.Flow.buffer_sink sink))) in
    if result<>expected || Buffer.contents sink<>bytes then failwith "decoded partial")
    ["BINARY[2]<2> ~{5}\r\nhello",Some 5L,"hello";
     "BINARY[2]<2> \"hi\"",Some 2L,"hi";
     "BINARY[2]<2> \"\"",Some 0L,"";
     "BINARY[2]<2> NIL",None,""];
  List.iter (fun fields ->
    expect "partial origin mismatch" protocol
      (scripted ~reply:(fetch_reply fields) (fun selected ->
        fetch ~partial:(2L,5L) selected (Eio.Flow.buffer_sink (Buffer.create 8)))))
    ["BINARY[2]<3> \"x\"";"BINARY[2] \"x\""];
  expect "partial count bounds decoded bytes" limit
    (scripted ~reply:(fetch_reply "BINARY[2]<2> \"123456\"") (fun selected ->
      fetch ~partial:(2L,5L) selected (Eio.Flow.buffer_sink (Buffer.create 8))))

let test_metadata_mismatch () =
  List.iter (fun response ->
    expect "binary identity mismatch" protocol
      (scripted ~reply:(fun n -> [`Return (response ^ done_ n)]) (fun selected ->
        fetch selected (Eio.Flow.buffer_sink (Buffer.create 8)))))
    ["* 1 FETCH (UID 8 BINARY[2] ~{3}\r\nabc)\r\n";

     "* 1 FETCH (UID 7 BINARY[3] ~{3}\r\nabc)\r\n";
     "* 1 FETCH (UID 7 BINARY[2]<0> \"abc\")\r\n";
     "* 1 FETCH (UID 7 FLAGS ())\r\n";
     "* 1 FETCH (UID 7 BINARY[2] ~{0}\r\n BINARY[3] ~{0}\r\n)\r\n";
     "* 1 FETCH (UID 7 BINARY[2] NIL BODY[] {0}\r\n)\r\n";
     "* 1 FETCH (UID 7 BINARY[2] \"abc\" BINARY[3] NIL)\r\n";
     "* 1 FETCH (UID 7 UID 8 BINARY[2] \"abc\")\r\n"];
  let sink=Buffer.create 8 in
  let result=ok (scripted ~reply:(fun n -> [`Return (
    "* LIST () \"/\" {5}\r\nINBOX\r\n* 1 FETCH (UID 7 BINARY[2] \"abc\")\r\n" ^
    done_ n)]) (fun selected -> fetch selected (Eio.Flow.buffer_sink sink))) in
  if result<>Some 3L || Buffer.contents sink<>"abc" then
    failwith "unsolicited LIST literal was taken for the body";
  expect "payload missing UID" protocol
    (scripted
      ~reply:(fun n -> [`Return ("* 1 FETCH (BINARY[2] \"abc\")\r\n" ^ done_ n)])
      (fun selected ->
        let result=fetch selected (Eio.Flow.buffer_sink (Buffer.create 8)) in
        expect "payload without UID closes" (function E.State _ -> true | _ -> false)
          (S.info selected);
        result));
  expect "missing UID" missing
    (scripted ~reply:(fun n -> [`Return (done_ n)]) (fun selected ->
      fetch selected (Eio.Flow.buffer_sink (Buffer.create 8))))

let test_unsolicited_metadata () =
  let sink=Buffer.create 8 in
  let result=ok (scripted ~reply:(fun n -> [`Return (
    "* 2 FETCH (FLAGS (\\Seen))\r\n" ^
    "* 1 FETCH (UID 7 BINARY[2] ~{3}\r\nabc)\r\n" ^
    "* 2 FETCH (UID 8 FLAGS ())\r\n" ^ done_ n)])
    (fun selected -> fetch selected (Eio.Flow.buffer_sink sink))) in
  if result<>Some 3L || Buffer.contents sink<>"abc" then
    failwith "unsolicited FLAGS disrupted decoded fetch";
  let sink=Buffer.create 8 in
  ok (scripted ~reply:(fun n -> [`Return (
    "* 2 FETCH (FLAGS (\\Seen))\r\n" ^
    "* 1 FETCH (UID 7 BODY[] {3}\r\nraw)\r\n" ^
    "* 2 FETCH (UID 8 FLAGS ())\r\n" ^ done_ n)])
    (fun selected -> S.fetch_to selected ~uid:7L (Eio.Flow.buffer_sink sink)));
  if Buffer.contents sink<>"raw" then failwith "unsolicited FLAGS disrupted raw fetch"

let test_budgets_and_failure () =
  expect "declared literal limit before bytes" limit
    (scripted ~reply:(fun _ -> [`Return "* 1 FETCH (UID 7 BINARY[2] ~{100}\r\n"])
      (fun selected -> fetch ~max_bytes:2L selected (Eio.Flow.buffer_sink (Buffer.create 8))));
  let sink=Buffer.create 8 in
  expect "inline byte limit" limit
    (scripted ~reply:(fetch_reply "BINARY[2] \"abc\"") (fun selected ->
      fetch ~max_bytes:2L selected (Eio.Flow.buffer_sink sink)));
  if Buffer.length sink<>0 then failwith "oversized inline data written";
  let sink=Buffer.create 8 in
  expect "UNKNOWN-CTE after provisional bytes"
    (function E.Rejected {code=Some Imap.Response.Unknown_cte;_} -> true | _ -> false)
    (scripted ~reply:(fun n -> [`Return ("* 1 FETCH (UID 7 BINARY[2] ~{3}\r\nabc)\r\n" ^
      tag n ^ " NO [UNKNOWN-CTE] cannot decode\r\n")]) (fun selected ->
      fetch selected (Eio.Flow.buffer_sink sink)));
  if Buffer.contents sink<>"abc" then failwith "provisional bytes lost";
  expect "truncated literal" (function E.Transport _ | E.Protocol _ -> true | _ -> false)
    (scripted ~reply:(fun _ -> [`Return "* 1 FETCH (UID 7 BINARY[2] ~{3}\r\na";`Raise End_of_file])
      (fun selected -> fetch selected (Eio.Flow.buffer_sink (Buffer.create 8))))

let test_capabilities () =
  let f selected=fetch selected (Eio.Flow.buffer_sink (Buffer.create 8)) in
  expect "BINARY required" state
    (scripted ~caps:"IMAP4rev1 UNSELECT" ~dispatched:false ~reply:(fetch_reply "BINARY[2] NIL") f);
  ignore (ok (scripted ~caps:"IMAP4rev2 UNSELECT" ~reply:(fetch_reply "BINARY[2] NIL") f));
  ignore (ok (scripted ~caps:"IMAP4rev1 IMAP4rev2 ENABLE UNSELECT" ~revision:true
    ~reply:(fetch_reply "BINARY[2] NIL") f));
  expect "unnegotiated rev2 insufficient" state
    (scripted ~caps:"IMAP4rev1 IMAP4rev2 ENABLE UNSELECT" ~revision:false
      ~dispatched:false ~reply:(fetch_reply "BINARY[2] NIL") f);
  let result=ok (scripted ~uidonly:true ~reply:(fun n ->
    [`Return ("* 7 UIDFETCH (BINARY[2] ~{3}\r\na\000b)\r\n" ^ done_ n)]) f) in
  if result<>Some 3L then failwith "UIDONLY BINARY lost"

let test_sizes () =
  let sizes selected=S.uid_fetch_binary_sizes selected ~uids:[7L;3L] ~section:[2] () in
  let found=ok (scripted ~reply:(fun n -> [`Return (
    "* 2 FETCH (UID 7 BINARY.SIZE[2] 99)\r\n* 1 FETCH (UID 3 BINARY.SIZE[2] 0)\r\n" ^ done_ n)]) sizes) in
  if List.map (fun (row:S.binary_size_row) -> row.uid,row.size) found<>[3L,0L;7L,99L] then
    failwith "decoded sizes lost";
  if ok (scripted ~reply:(fun n -> [`Return (done_ n)]) sizes)<>[] then
    failwith "missing size rows invented";
  List.iter (fun response ->
    expect "invalid BINARY.SIZE" protocol
      (scripted ~reply:(fun n -> [`Return (response ^ done_ n)]) sizes))
    ["* 1 FETCH (UID 7 BINARY.SIZE[2] NIL)\r\n";
     "* 1 FETCH (BINARY.SIZE[2] 9)\r\n";
     "* 1 FETCH (UID 9 BINARY.SIZE[2] 9)\r\n";
     "* 1 FETCH (UID 7 BINARY.SIZE[2] 9)\r\n* 1 FETCH (UID 7 BINARY.SIZE[2] 9)\r\n"]

let test_cancelled_stream () =
  let entered,mark_entered=Eio.Promise.create () in
  let sink=Buffer.create 8 in
  ok (scripted ~reply:(fun _ -> [
    `Return "* 1 FETCH (UID 7 BINARY[2] ~{5}\r\nab";
    `Run (fun () -> Eio.Promise.resolve mark_entered (); Eio.Fiber.await_cancel ())])
    (fun selected ->
      Eio.Fiber.first
        (fun () -> ignore (fetch selected (Eio.Flow.buffer_sink sink)))
        (fun () -> Eio.Promise.await entered);
      expect "cancelled body closes selected connection" state (S.info selected);
      Ok ()));
  if Buffer.contents sink<>"ab" then failwith "cancelled stream provisional prefix lost"

let () =
  test_representations (); test_partial (); test_metadata_mismatch ();
  test_unsolicited_metadata (); test_budgets_and_failure (); test_capabilities (); test_sizes (); test_cancelled_stream ()
