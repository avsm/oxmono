module C = Imap_eio.Client
module A = Imap_eio.Auth
module E = Imap_eio.Error
let ok = function Ok x -> x | Error e -> failwith (C.error_to_string e)
let u n = match Imap.Uid.of_int64 n with Ok v -> v | Error e -> failwith e
let raw_list = List.map Imap.Uid.to_int64
let contains text part =
  let rec loop i = i+String.length part<=String.length text &&
    (String.sub text i (String.length part)=part || loop (i+1)) in
  loop 0
let secret="synthetic-provider-secret"
let test_auth_redaction () =
  List.iter (fun mechanism ->
    let auth=A.password ~username:"user" ~password:secret ~mechanism
      ~allow_insecure_transport:true () in
    List.iter (fun reply ->
      Eio_mock.Backend.run @@ fun () ->
      Eio.Switch.run @@ fun sw ->
      let flow=Eio_mock.Flow.make "auth-redaction-review" in
      Eio_mock.Flow.on_read flow [
        `Return "* OK ready\r\n";
        `Return "* CAPABILITY IMAP4rev1 AUTH=CRAM-MD5 AUTH=PLAIN SASL-IR\r\nA00000001 OK done\r\n";
        `Return reply];
      match C.of_flow ~sw ~auth flow with
      | Ok _ -> failwith "invalid authentication succeeded"
      | Error e when contains (C.error_to_string e) secret ->
          failwith "authentication diagnostic exposed server-controlled secret"
      | Error _ -> ())
      ["* BYE " ^ secret ^ "\r\n";
       secret ^ " OK unexpected tag\r\n";
       "* " ^ secret ^ " invalid response\r\n"])
    [`Login;`Cram_md5;`Plain];
  List.iter (fun mechanism ->
    Eio_mock.Backend.run @@ fun () ->
    Eio.Switch.run @@ fun sw ->
    let flow=Eio_mock.Flow.make "provider-redaction-review" in
    Eio_mock.Flow.on_read flow [
      `Return "* OK ready\r\n";
      `Return "* CAPABILITY IMAP4rev1 AUTH=CRAM-MD5 AUTH=PLAIN SASL-IR\r\nA00000001 OK done\r\n";
      `Return "+ Y2hhbGxlbmdl\r\n"];
    let auth=A.refreshing ~username:"user" ~mechanism
      ~allow_insecure_transport:true (fun () -> failwith secret) in
    match C.of_flow ~sw ~auth flow with
    | Error e when not (contains (C.error_to_string e) secret) -> ()
    | _ -> failwith "provider exception was not redacted") [`Login;`Cram_md5;`Plain]

let with_client caps replies f =
  Eio_mock.Backend.run @@ fun () ->
  Eio.Switch.run @@ fun sw ->
  let flow=Eio_mock.Flow.make "client-review" in
  let caps="* CAPABILITY IMAP4rev1 UNSELECT " ^ caps ^ "\r\n" in
  Eio_mock.Flow.on_read flow ([`Return "* PREAUTH ready\r\n";
    `Return (caps ^ "A00000001 OK done\r\n")] @ replies);
  let client=ok (C.of_flow ~sw flow) in f client

let test_selection_cleanup () =
  with_client "" [`Return "A00000002 OK selected\r\n"] (fun client ->
    (match C.with_mailbox client ~mode:`Read_only "INBOX"
      (fun _ -> failwith "invalid selection exposed a lease") with
     | Error (E.Protocol _) -> () | _ -> failwith "invalid selection succeeded");
    if C.is_open client then failwith "invalid selection remained open");
  with_client "" [
    `Return "* 0 EXISTS\r\n* OK [UIDVALIDITY 1] valid\r\n* OK [UIDNEXT 1] next\r\nA00000002 OK selected\r\n";
    `Return "A00000003 NO cannot unselect\r\n"] (fun client ->
    (match C.with_mailbox client ~mode:`Read_only "INBOX" (fun _ -> Ok 7) with
     | Ok 7 -> ()
     | _ -> failwith "failed UNSELECT replaced the callback outcome");
    if C.is_open client then failwith "failed lease release remained open")

let test_metadata_scope () =
  with_client "METADATA-SERVER" [
    `Return "* METADATA \"\" (/shared/comment \"ok\")\r\nA00000002 OK done\r\n"]
    (fun client ->
      let metadata=ok (C.Metadata.require client) in
      ignore (ok (C.Metadata.get_metadata metadata ~mailbox:""
        ~entries:["/shared/comment"] ()));
      match C.Metadata.get_metadata metadata ~mailbox:"INBOX"
          ~entries:["/shared/comment"] () with
      | Error (E.Unsupported Imap.Capability.Metadata) -> ()
      | _ -> failwith "mailbox metadata bypassed capability gate")

let with_selected ?(caps="") reply f =
  with_client caps [
    `Return "* 9 EXISTS\r\n* OK [UIDVALIDITY 1] valid\r\n* OK [UIDNEXT 10] next\r\nA00000002 OK selected\r\n";
    `Return (reply ^ (if String.starts_with ~prefix:"A00000003 " reply then "" else "A00000003 OK done\r\n"));
    `Return "A00000004 OK unselected\r\n"] (fun client ->
      f (C.with_mailbox client ~mode:`Read_write "INBOX"))

let test_search_evidence () =
  List.iter (fun reply -> with_selected reply (fun with_mailbox ->
    match with_mailbox (fun selected -> Imap_eio.Selected.uid_search selected
      ~criteria:Imap.Search.All) with
    | Error (E.Protocol _) -> ()
    | _ -> failwith "missing, repeated or uncorrelated SEARCH accepted"))
    ["";"* SEARCH 1\r\n* SEARCH 2\r\n";
     "* ESEARCH (TAG \"other\") UID ALL 1\r\n";
     "* ESEARCH UID ALL 1:4294967295\r\n* ESEARCH UID ALL 1:4294967295\r\n"];
  List.iter (fun reply -> with_selected reply (fun with_mailbox ->
    if ok (with_mailbox (fun selected -> Imap_eio.Selected.uid_search selected
      ~criteria:Imap.Search.All))<>[]
    then failwith "explicit empty SEARCH changed"))
    ["* SEARCH\r\n";"* ESEARCH (TAG \"A00000003\") UID\r\n"];
  with_selected "* ESEARCH UID\r\n" (fun with_mailbox ->
    match with_mailbox (fun selected -> Imap_eio.Selected.uid_search selected
      ~criteria:Imap.Search.All) with
    | Error (E.Protocol _) -> ()
    | _ -> failwith "uncorrelated ESEARCH accepted");
  with_selected "* SEARCH 9 3 9\r\n" (fun with_mailbox ->
    if raw_list (ok (with_mailbox (fun selected ->
      Imap_eio.Selected.uid_search_range selected ~first:(u 3L)
        ~last:(u 9L))))<>[3L;9L]
    then failwith "ordinary SEARCH range was not normalized");
  with_selected "* SEARCH 2\r\n" (fun with_mailbox ->
    match with_mailbox (fun selected ->
      Imap_eio.Selected.uid_search_range selected ~first:(u 3L)
        ~last:(u 9L)) with
    | Error (E.Protocol _) -> () | _ -> failwith "out-of-range SEARCH accepted");
  with_selected ~caps:"MESSAGELIMIT=2" "" (fun with_mailbox ->
    match with_mailbox (fun selected ->
      Result.bind (Imap_eio.Selected.Messagelimit.require selected)
        (fun limit -> Imap_eio.Selected.Messagelimit.uid_search_page limit
          ~criteria:Imap.Search.All)) with
    | Error (E.Protocol _) -> () | _ -> failwith "missing page treated as complete")

let test_copy_correspondence () =
  List.iter (fun (source,destination,expected) ->
    with_selected ("A00000003 OK [COPYUID 7 " ^ source ^ " " ^ destination ^ "] copied\r\n")
      (fun with_mailbox ->
        let set=Result.get_ok (Imap.Uid_set.of_wire source) in
        let receipt=ok (with_mailbox (fun selected ->
          Imap_eio.Selected.uid_copy selected ~set ~mailbox:"Archive")) |> Option.get in
        let actual=List.map (fun (range:Imap_eio.Selected.copy_mapping) ->
          Imap.Uid.to_int64 range.source_first,
          Imap.Uid.to_int64 range.destination_first,range.length) receipt.mapping in
        if actual<>expected then failwith "COPYUID correspondence lost"))
    ["9,3","20:21",[9L,20L,1L;3L,21L,1L];
     "5:3,9","20:22,30",[3L,20L,3L;9L,30L,1L];
     "1:1000000000","1000000001:2000000000",[1L,1000000001L,1000000000L]]

let test_fetch_order () =
  let envelope subject=Printf.sprintf
    "(NIL %s NIL NIL NIL NIL NIL NIL NIL NIL)" subject in
  let row ?(subject="NIL") uid=
    Printf.sprintf "* 1 FETCH (UID %d ENVELOPE %s)\r\n" uid
      (envelope subject) in
  let envelopes uids selected=Imap_eio.Selected.fetch selected ~uids
    ~items:[Imap.Fetch_item.Envelope] in
  with_selected (row 1 ^ row 2) (fun with_mailbox ->
    let rows=ok (with_mailbox (envelopes [u 2L;u 1L])) in
    if List.map (fun (row:Imap_eio.Selected.row) ->
        Imap.Uid.to_int64 row.uid) rows<>[2L;1L]
    then failwith "structured FETCH lost request order");
  with_selected (row 1 ^ row ~subject:"\"changed\"" 1) (fun with_mailbox ->
    match with_mailbox (envelopes [u 1L]) with
    | Error (E.Protocol _) -> ()
    | _ -> failwith "conflicting ENVELOPE accepted")

let () =
  test_auth_redaction (); test_selection_cleanup (); test_metadata_scope ();
  test_search_evidence (); test_copy_correspondence (); test_fetch_order ()
