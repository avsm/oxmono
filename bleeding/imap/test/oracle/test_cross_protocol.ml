(* RFC 8621 Email/import/get/set and RFC 9051 APPEND/FETCH/STORE operate
   on the same isolated mailbox. Each direction checks exact message bytes. *)
module H=Oracle_harness
module J=Jmap.Proto
module Chain=Jmap.Chain
module C=Imap_eio.Client
module S=Imap_eio.Selected
let imap = function Ok x -> x | Error e -> Alcotest.fail (C.error_to_string e)
let jmap = function Ok x -> x | Error e -> Alcotest.fail (Jmap_eio.Client.error_to_string e)
let some = function Some x -> x | None -> Alcotest.fail "missing required result"
let value = function Ok x -> x | Error e -> Alcotest.fail e
let env key fallback=Option.value ~default:fallback (Sys.getenv_opt key)
let test t =
  let mailbox=H.unique "Oxmono-Cross-Protocol" in
  let key=J.Id.creation "mailbox" in
  let created=H.call t (Chain.mailbox_set ~account_id:t.H.account_id
    ~create:[key,value (J.Mailbox.create ~name:mailbox ())] ()) in
  let mailbox_id=some (some (J.Method.created created key)).id in
  Fun.protect ~finally:(fun () ->
    ignore (H.call t (Chain.mailbox_set ~account_id:t.H.account_id
      ~destroy:(Chain.ids [mailbox_id]) ~on_destroy_remove_emails:true ()))) (fun () ->
    let transport=Imap_eio.Transport.v ~net:(Eio.Stdenv.net t.H.env)
      ~host:(env "IMAP_ORACLE_HOST" "127.0.0.1")
      ~port:(int_of_string (env "IMAP_ORACLE_PORT" "18143")) ~tls:`Plain () in
    let auth=Imap_eio.Auth.password ~username:(H.user ()) ~password:(H.password ())
      ~allow_insecure_transport:true () in
    let client=imap (C.connect ~sw:t.H.sw ~auth transport) in
    Fun.protect ~finally:(fun () -> C.close client) (fun () ->
      let selected f=imap (C.with_mailbox client ~mode:`Read_write mailbox f) in
      let find subject=selected (fun s ->
        match imap (S.uid_search s ("HEADER Subject \"" ^ subject ^ "\"")) with
        | [uid] -> Ok uid | _ -> Alcotest.fail "expected exactly one IMAP occurrence") in
      let body uid expected=selected (fun s ->
        let out=Buffer.create 128 in
        imap (S.fetch_to s ~uid (Eio.Flow.buffer_sink out));
        Alcotest.(check string) "exact IMAP body" expected (Buffer.contents out);
        Ok ()) in
      let has uid criteria=selected (fun s ->
        let found=imap (S.uid_search s (Printf.sprintf "UID %Ld %s" uid criteria)) in
        Alcotest.(check (list int64)) criteria [uid] found;
        Ok ()) in
      let keywords id expected=
        let email=H.email t ~properties:[`Id;`Keywords] id in
        let names=List.map J.Keyword.to_string (J.Email.keyword_list email)
          |> List.sort String.compare in
        Alcotest.(check (list string)) "exact JMAP keywords"
          (List.sort String.compare expected) names in
      let subject,raw=H.message ~body:"JMAP import to IMAP\r\nexact octets\r\n" () in
      let uploaded=jmap (Jmap_eio.Client.upload t.H.client ~account_id:t.H.account_id
        ~content_type:"message/rfc822" ~data:raw) in
      let key=J.Id.creation "message" in
      let imported=H.call t (Chain.email_import ~account_id:t.H.account_id
        ~emails:[key,J.Email.Import.email ~blob_id:uploaded.J.Blob.blob_id
          ~mailbox_ids:[mailbox_id] ~keywords:[`Custom "cross-label";`Custom "seen"] ()] ()) in
      let id=some (some (J.Email.Import.created imported key)).id in
      let uid=find subject in
      body uid raw;
      has uid "UNSEEN KEYWORD cross-label KEYWORD seen";
      ignore (H.call t (Chain.email_set ~account_id:t.H.account_id
        ~update:[id,J.Patch.v [J.Email.Patch.set_keyword `Seen;
                              J.Email.Patch.set_keyword (`Custom "from-jmap")]] ()));
      has uid "SEEN KEYWORD from-jmap KEYWORD cross-label";
      selected (fun s ->
        let set=Imap.Proto.Uid_set.singleton (value (Imap.Proto.Uid.of_int64 uid)) in
        ignore (imap (S.uid_store_flags s ~set ~operation:`Add
          ~flags:[value (Mail_flag.Imap_flag.of_wire "\\Flagged");
                  value (Mail_flag.Imap_flag.of_wire "from-imap")] ()));
        Ok ());
      let expected=["$seen";"$flagged";"cross-label";"seen";"from-jmap";"from-imap"] in
      keywords id expected;
      (* RFC 8621 4.1.1 requires messages marked Deleted to be invisible
         through JMAP, even while their IMAP occurrence remains present. *)
      selected (fun s ->
        let set=Imap.Proto.Uid_set.singleton (value (Imap.Proto.Uid.of_int64 uid)) in
        ignore (imap (S.uid_store_flags s ~set ~operation:`Add
          ~flags:[Mail_flag.Imap_flag.system Deleted] ()));
        Ok ());
      has uid "DELETED";
      let hidden=H.call t (Chain.email_get ~account_id:t.H.account_id
        ~ids:(Chain.ids [id]) ~properties:[`Id;`Keywords] ()) in
      Alcotest.(check int) "Deleted hidden from JMAP" 0 (List.length hidden.list);
      Alcotest.(check (list string)) "Deleted reported notFound"
        [J.Id.to_string id] (List.map J.Id.to_string hidden.not_found);
      Alcotest.(check int) "Deleted absent from JMAP query" 0
        (List.length (H.query_by_subject t subject));
      selected (fun s ->
        let set=Imap.Proto.Uid_set.singleton (value (Imap.Proto.Uid.of_int64 uid)) in
        ignore (imap (S.uid_store_flags s ~set ~operation:`Remove
          ~flags:[Mail_flag.Imap_flag.system Deleted] ()));
        Ok ());
      has uid "UNDELETED";
      keywords id expected;
      let subject,raw=H.message ~body:"IMAP append to JMAP\r\nexact octets\r\n" () in
      let receipt=some (imap (C.append_flow_receipt client ~mailbox
        ~flags:["\\Answered";"append-label"] ~length:(Int64.of_int (String.length raw))
        (Eio.Flow.string_source raw))) in
      let id=H.wait_for_email t ~subject () in
      let email=H.email t ~properties:[`Id;`Blob_id;`Mailbox_ids;`Keywords] id in
      let downloaded=jmap (Jmap_eio.Client.download t.H.client
        ~account_id:t.H.account_id ~blob_id:(some email.blob_id) ()) in
      Alcotest.(check string) "exact JMAP downloaded body" raw downloaded;
      Alcotest.(check bool) "same isolated mailbox" true
        (List.mem (mailbox_id,true) (some email.mailbox_ids));
      keywords id ["$answered";"append-label"];
      body (Imap.Proto.Uid.to_int64 receipt.uid) raw))
let unrepresentable () =
  let long=String.make 256 'x' in
  let flag=value (Mail_flag.Imap_flag.of_wire long) in
  Alcotest.(check string) "IMAP keyword retained" long
    (Mail_flag.Imap_flag.to_wire flag);
  Alcotest.(check bool) "JMAP length restriction reported" true
    (Result.is_error (J.Keyword.validate (`Custom long)));
  List.iter (fun wire ->
    let flag=value (Mail_flag.Imap_flag.of_wire wire) in
    Alcotest.(check bool) ("no shared semantic alias for " ^ wire) true
      (Mail_flag.Imap_flag.semantic flag=None))
    ["\\Recent";"\\X-Extension";"$seen";"Seen"];
  Alcotest.(check bool) "Deleted excluded from JMAP" true
    (J.Keyword.of_mail_flag `Deleted=None);
  Alcotest.(check bool) "bare seen stays a custom JMAP keyword" true
    (J.Keyword.of_string "seen"=`Custom "seen")
let () =
  (* Unlike the generic harness, required IMAP CI may never silently skip. *)
  if env "IMAP_ORACLE_REQUIRED" "0"="1" &&
     (not (H.configured ()) || Sys.getenv_opt "IMAP_ORACLE_HOST"=None) then
    failwith "cross-protocol oracle requires both JMAP and IMAP fixture endpoints";
  H.run "imap-jmap-cross-protocol" ["mapping",[
    Alcotest.test_case "explicitly unrepresentable flags" `Quick unrepresentable];
    "roundtrip",[
    H.test_case "exact bodies and bidirectional keywords" test]]
