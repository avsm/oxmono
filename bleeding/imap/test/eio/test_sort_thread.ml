module C = Imap.Command
module R = Imap.Response
module S = Imap_eio.Selected
module E = Imap_eio.Error

let ok = function
  | Ok value -> value
  | Error error -> failwith (Imap_eio.Client.error_to_string error)

let scripted ?(uidonly=false) ?(dispatched=true) ~capabilities ~reply f =
  Eio_mock.Backend.run @@ fun () ->
  Eio.Switch.run @@ fun sw ->
  let flow=Eio_mock.Flow.make "sort-thread" in
  let caps="IMAP4rev1 UNSELECT " ^ capabilities ^
    (if uidonly then " ENABLE UIDONLY" else "") in
  let selected_tag=if uidonly then 5 else 4 in
  let tag n=Printf.sprintf "A%08d" n in
  Eio_mock.Flow.on_read flow (
    [`Return "* OK ready\r\n";
     `Return ("* CAPABILITY " ^ caps ^ "\r\nA00000001 OK capabilities\r\n");
     `Return "A00000002 OK logged in\r\n";
     `Return ("* CAPABILITY " ^ caps ^ "\r\nA00000003 OK capabilities\r\n")]
    @ (if uidonly then
       [`Return "* ENABLED UIDONLY\r\nA00000004 OK enabled\r\n"] else [])
    @ [`Return ("* 3 EXISTS\r\n* OK [UIDVALIDITY 1] valid\r\n" ^
       "* OK [UIDNEXT 10] next\r\n" ^ tag selected_tag ^ " OK selected\r\n")]
    @ (if dispatched then
       [`Return (reply (tag (selected_tag+1)))] else [])
    @ [`Return (tag (selected_tag + if dispatched then 2 else 1) ^
       " OK unselected\r\n")]);
  let auth=Imap_eio.Auth.password ~username:"user" ~password:"pw"
    ~allow_insecure_transport:true () in
  let client=ok (Imap_eio.Client.of_flow ~sw ~auth flow) in
  if uidonly then ignore (ok (Imap_eio.Client.enable_uidonly client));
  let outcome=Imap_eio.Client.with_mailbox client ~mode:`Read_only "INBOX" f in
  Imap_eio.Client.close client;
  outcome

let sort ?(criterion="ALL") selected =
  S.uid_sort selected ~keys:[C.Date,C.Descending;C.Subject,C.Ascending]
    ~charset:"UTF-8" ~criterion
let thread ?(algorithm=C.References) ?(criterion="ALL") selected =
  S.uid_thread selected ~algorithm ~charset:"UTF-8" ~criterion
let complete response tag = response ^ tag ^ " OK completed\r\n"
let expect_error label kind = function
  | Error error when kind error -> ()
  | Error error -> failwith (label ^ ": " ^ Imap_eio.Client.error_to_string error)
  | Ok _ -> failwith (label ^ ": unexpectedly succeeded")
let state = function E.State _ -> true | _ -> false
let unsupported c = function
  | E.Unsupported x -> Imap.Capability.equal x c | _ -> false
let protocol = function E.Protocol _ -> true | _ -> false
let limit = function E.Limit _ -> true | _ -> false

let test_capability_gates () =
  expect_error "missing SORT" (unsupported Imap.Capability.Sort)
    (scripted ~dispatched:false ~capabilities:"" ~reply:(complete "") sort);
  expect_error "wrong THREAD algorithm"
    (unsupported Imap.Capability.(Thread References))
    (scripted ~dispatched:false ~capabilities:"THREAD=ORDEREDSUBJECT"
      ~reply:(complete "") thread);
  expect_error "SORT key list empty" state
    (scripted ~dispatched:false ~capabilities:"SORT" ~reply:(complete "")
      (fun selected -> S.uid_sort selected ~keys:[] ~charset:"UTF-8"
        ~criterion:"ALL"));
  let uids=ok (scripted ~capabilities:"SORT=DISPLAY"
    ~reply:(complete "* SORT 9 1 4\r\n") sort) in
  if uids<>[9L;1L;4L] then failwith "SORT order changed"

let test_empty_and_missing () =
  if ok (scripted ~capabilities:"SORT" ~reply:(complete "* SORT\r\n") sort)<>[]
  then failwith "empty SORT result";
  if ok (scripted ~capabilities:"THREAD=REFERENCES"
    ~reply:(complete "* THREAD\r\n") thread)<>[]
  then failwith "empty THREAD result";
  expect_error "missing SORT response" protocol
    (scripted ~capabilities:"SORT" ~reply:(complete "") sort);
  expect_error "missing THREAD response" protocol
    (scripted ~capabilities:"THREAD=REFERENCES" ~reply:(complete "") thread)

let test_tree () =
  let leaf uid:R.thread={uid=Some uid;children=[]} in
  let expected:R.thread list=[
    {uid=None;children=[{uid=Some 9L;children=[leaf 1L]};leaf 4L]};leaf 2L] in
  let found=ok (scripted ~capabilities:"THREAD=REFERENCES"
    ~reply:(complete "* THREAD ((9 1)(4))(2)\r\n") thread) in
  if found<>expected then failwith "THREAD dummy/chain/order changed";
  ignore (ok (scripted ~capabilities:"THREAD=ORDEREDSUBJECT"
    ~reply:(complete "* THREAD (9)(1 4)\r\n")
    (thread ~algorithm:C.Orderedsubject)))

let test_invalid_results () =
  List.iter (fun response ->
    expect_error "invalid SORT" protocol
      (scripted ~capabilities:"SORT" ~reply:(complete response) sort))
    ["* SORT 0\r\n";"* SORT 1 1\r\n";"* SORT 4294967296\r\n";
     "* SORT 1\r\n* SORT 2\r\n"];
  List.iter (fun response ->
    expect_error "invalid THREAD" protocol
      (scripted ~capabilities:"THREAD=REFERENCES"
        ~reply:(complete response) thread))
    ["* THREAD (0)\r\n";"* THREAD (1)(1)\r\n";"* THREAD ()\r\n";
     "* THREAD (1\r\n";"* THREAD (1)\r\n* THREAD (2)\r\n"]

let test_partial_results () =
  List.iter (fun reply ->
    expect_error "partial SORT" limit
      (scripted ~capabilities:"SORT MESSAGELIMIT=1" ~reply sort))
    [(fun tag -> "* SORT 9\r\n" ^ tag ^ " OK [MESSAGELIMIT 1 9] partial\r\n");
     (fun tag -> "* SORT 9\r\n* NO [MESSAGELIMIT 1 9] partial\r\n" ^
       tag ^ " OK completed\r\n")];
  expect_error "partial THREAD" limit
    (scripted ~capabilities:"THREAD=REFERENCES MESSAGELIMIT=1"
      ~reply:(fun tag -> "* THREAD (9)\r\n" ^ tag ^
        " OK [MESSAGELIMIT 1 9] partial\r\n") thread)

let test_uidonly () =
  expect_error "sequence SORT in UIDONLY" state
    (scripted ~uidonly:true ~dispatched:false ~capabilities:"SORT"
      ~reply:(complete "") (sort ~criterion:"1:3"));
  expect_error "sequence THREAD in UIDONLY" state
    (scripted ~uidonly:true ~dispatched:false ~capabilities:"THREAD=REFERENCES"
      ~reply:(complete "") (thread ~criterion:"1:3"));
  if ok (scripted ~uidonly:true ~capabilities:"SORT"
    ~reply:(complete "* SORT 9 1\r\n") (sort ~criterion:"UID 1:9"))<>[9L;1L]
  then failwith "UIDONLY SORT changed UIDs";
  ignore (ok (scripted ~uidonly:true ~capabilities:"THREAD=REFERENCES"
    ~reply:(complete "* THREAD (9 1)\r\n") thread))

let extended ?(returns=[]) selected =
  S.uid_sort_extended selected ~returns ~keys:[C.Date,C.Descending]
    ~charset:"UTF-8" ~criterion:"ALL"
let esort fields tag =
  Printf.sprintf "* ESEARCH (TAG \"%s\") UID %s\r\n%s OK sorted\r\n" tag fields tag

let test_esort () =
  let run ?(capabilities="ESORT") returns fields =
    scripted ~capabilities ~reply:(esort fields) (extended ~returns) in
  let result=ok (run [C.All;C.Min;C.Max] "MIN 90 MAX 7 COUNT 6 ALL 90,12:10,6:7") in
  if result.count<>6L || result.first<>Some 90L || result.last<>Some 7L ||
     result.uids<>Some [90L;10L;11L;12L;6L;7L] || result.range<>None then
    failwith "ESORT order/range expansion/MIN/MAX changed";
  let result=ok (run [] "COUNT 0") in
  if result.uids<>Some [] then failwith "empty default ALL was not explicit";
  let result=ok (run [C.Min;C.Max] "COUNT 0") in
  if result.first<>None || result.last<>None || result.uids<>None then
    failwith "empty ESORT endpoints/list incorrect";
  let result=ok (run [C.Count] "COUNT 4000000000") in
  if result.count<>4000000000L || result.uids<>None then
    failwith "COUNT unnecessarily limited by UID expansion budget";
  let result=ok (run [C.Min;C.Max] "COUNT 2 MIN 90 MAX 7") in
  if result.first<>Some 90L || result.last<>Some 7L then
    failwith "MIN/MAX treated as numeric extrema";
  List.iter (fun (returns,fields) ->
    expect_error ("inconsistent ESORT " ^ fields) protocol (run returns fields))
    [[C.All],"ALL 1"; [C.All],"COUNT 2";
     [C.All],"COUNT 2 ALL 1"; [C.All],"COUNT 2 ALL 1,1";
     [C.All],"COUNT 2 ALL 1,0"; [C.All],"COUNT 2 COUNT 2 ALL 1,2";
     [C.Min],"COUNT 1"; [C.Max],"COUNT 1";
     [C.Min;C.Max],"COUNT 1 MIN 2 MAX 3";
     [C.Min;C.Max],"COUNT 2 MIN 2 MAX 2";
     [C.Min],"COUNT 0 MIN 2";
     [C.Min;C.All],"COUNT 2 MIN 2 ALL 3,2";
     [C.Max;C.All],"COUNT 2 MAX 3 ALL 3,2";
     [C.Count],"COUNT 2 PARTIAL (1:2 1,2)"];
  expect_error "ESORT expansion bounded" limit
    (run [C.All] "COUNT 100001 ALL 1:100001");
  expect_error "ESORT required beyond SORT"
    (unsupported Imap.Capability.Esort)
    (scripted ~dispatched:false ~capabilities:"SORT" ~reply:(esort "COUNT 0") extended);
  List.iter (fun reply ->
    expect_error "ESORT correlation" protocol
      (scripted ~capabilities:"ESORT" ~reply extended))
    [(fun tag -> "* ESEARCH UID COUNT 0\r\n" ^ tag ^ " OK sorted\r\n");
     (fun tag -> "* ESEARCH (TAG \"other\") UID COUNT 0\r\n" ^ tag ^ " OK sorted\r\n");
     (fun tag -> "* ESEARCH (TAG \"" ^ tag ^ "\") COUNT 0\r\n" ^ tag ^ " OK sorted\r\n");
     (fun tag -> "* ESEARCH (TAG \"" ^ tag ^ "\") UID COUNT 0\r\n" ^ esort "COUNT 0" tag)];
  expect_error "ESORT MESSAGELIMIT is not positional PARTIAL" limit
    (scripted ~capabilities:"ESORT MESSAGELIMIT=1"
      ~reply:(fun tag -> "* ESEARCH (TAG \"" ^ tag ^ "\") UID COUNT 1 ALL 9\r\n" ^
        tag ^ " OK [MESSAGELIMIT 1 9] partial\r\n") extended)

let test_esort_partial () =
  let run returns fields=scripted ~capabilities:"ESORT CONTEXT=SORT"
    ~reply:(esort fields) (extended ~returns) in
  let result=ok (run [C.Partial (2L,4L)] "COUNT 8 PARTIAL (2:4 9,4:3)") in
  if result.uids<>Some [9L;3L;4L] || result.range<>Some (2L,4L) || result.count<>8L then
    failwith "ESORT positional page changed";
  let result=ok (run [C.Partial (4L,2L)] "COUNT 3 PARTIAL (4:2 9,3)") in
  if result.uids<>Some [9L;3L] || result.range<>Some (4L,2L) then
    failwith "ESORT reversed clipped page changed";
  let result=ok (run [C.Partial (9L,10L)] "COUNT 8 PARTIAL (9:10 NIL)") in
  if result.uids<>Some [] then failwith "ESORT out-of-range page not empty";
  let result=ok (run [C.Partial (1L,2L);C.Min;C.Max]
    "COUNT 2 MIN 9 MAX 3 PARTIAL (1:2 9,3)") in
  if result.first<>Some 9L || result.last<>Some 3L then failwith "page endpoints lost";
  List.iter (fun fields ->
    expect_error ("invalid ESORT page " ^ fields) protocol
      (run [C.Partial (2L,4L)] fields))
    ["COUNT 8"; "COUNT 8 PARTIAL (1:3 9,3,4)";
     "COUNT 8 PARTIAL (2:4 NIL)";"COUNT 8 PARTIAL (2:4 9,3)";
     "COUNT 8 PARTIAL (2:4 9,3,4,5)";"COUNT 8 PARTIAL (2:4 9,3,3)";
     "COUNT 8 ALL 1:8 PARTIAL (2:4 2:4)"];
  expect_error "PARTIAL alone does not authorize ESORT page"
    (unsupported (Imap.Capability.Context `Sort))
    (scripted ~dispatched:false ~capabilities:"ESORT PARTIAL"
      ~reply:(esort "COUNT 0") (extended ~returns:[C.Partial (1L,2L)]));
  expect_error "negative ESORT positions refused" state
    (scripted ~dispatched:false ~capabilities:"ESORT CONTEXT=SORT"
      ~reply:(esort "COUNT 0") (extended ~returns:[C.Partial (-2L,-1L)]))

let () =
  test_capability_gates ();
  test_empty_and_missing ();
  test_tree ();
  test_invalid_results ();
  test_partial_results ();
  test_uidonly ();
  test_esort ();
  test_esort_partial ()
