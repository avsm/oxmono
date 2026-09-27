module J = Imap_store.Journal
let scope : Imap.Mirror.scope = {
  endpoint="imap.example"; account="alice"; mailbox_key="inbox";
  raw_name="INBOX"; encoding=Imap.Mailbox_name.Rev1; mailbox_id=None }
let digest = String.make 64 'a'
let epoch n = Result.get_ok (Imap.Uidvalidity.of_int64 n)
let append ?(digest=digest) ?date id : J.operation = {
  id; pair_id=None; local_id=Some ("local-" ^ id); scope; kind=Append;
  state=Prepared; source_uidvalidity=None; source_uid=None;
  destination=Some scope; destination_uidvalidity=Some (epoch 9L);
  blob_sha256=Some digest; blob_length=Some 42L; desired_flags=Some [];
  internal_date=Option.map (fun d ->
    Result.get_ok (Imap.Internal_date.of_string d)) date;
  append=Some {message_id="<message@example>"; spool_ref="spool-message";
    pre_send_frontier=0L};
  receipt=None; receipt_uidvalidity=None; receipt_uid=None }
let with_append f id = let x=append id in
  {x with append=Option.map f x.append}
let run env =
  let path=Filename.temp_file "imap-append-validation-" ".db" in
  Fun.protect ~finally:(fun () -> List.iter (fun path ->
    try Sys.remove path with Sys_error _ -> ()) [path;path^"-wal";path^"-shm"])
    (fun () ->
      let location=Eio.Path.(Eio.Stdenv.fs env / path) in
      Eio.Switch.run (fun sw ->
        let db=Imap_store.open_path ~sw location in
        let reject (candidate:J.operation) =
          (match J.prepare_operation db candidate with
           | exception Invalid_argument _ -> ()
           | () -> failwith ("invalid APPEND accepted: " ^ candidate.id));
          if J.find_operation db ~id:candidate.id <> None then
            failwith "rejected APPEND was persisted";
          (* The failed preparation must leave both transaction and ID usable. *)
          J.prepare_operation db (append candidate.id)
        in
        List.iteri (fun n digest ->
          reject (append ~digest ("digest-" ^ string_of_int n)))
          [""; "sha256:abc"; String.make 63 'a'; String.make 65 'a';
           String.make 64 'A'; String.make 64 'g'; String.make 63 'a' ^ "\n"];
        reject (with_append (fun a -> {a with message_id=""}) "no-message");
        reject (with_append (fun a -> {a with spool_ref=""}) "no-spool");
        List.iteri (fun n frontier ->
          reject (with_append (fun a -> {a with pre_send_frontier=frontier})
            ("frontier-" ^ string_of_int n)))
          [-1L; 4_294_967_296L];
        reject {(append "no-metadata") with append=None};
        reject {(append "no-destination") with destination=None};
        reject {(append "receipt-uid") with
          receipt_uid=Some (Result.get_ok (Imap.Uid.of_int64 3L))};
        let flags : J.operation = {(append "flags-with-metadata") with
          kind=Flags; local_id=None; destination=None;
          destination_uidvalidity=None; blob_sha256=None; blob_length=None;
          source_uidvalidity=Some (epoch 9L);
          source_uid=Some (Result.get_ok (Imap.Uid.of_int64 1L))} in
        reject flags;
        reject {flags with id="flags-with-date"; append=None;
          internal_date=Some (Result.get_ok
            (Imap.Internal_date.of_string "26-Sep-2026 12:00:00 +0000"))};
        List.iter (fun date ->
          if Result.is_ok (Imap.Internal_date.of_string date) then
            failwith ("invalid date parsed: " ^ date))
          [""; "printable garbage"; "31-Feb-2026 12:00:00 +0000";
           "26-Sep-2026 25:00:00 +0000"; "26-Sep-2026 12:00:00 +9999";
           "26-Sep-2026 12:00:00 +0000\r\n"];
        List.iteri (fun n date ->
          let candidate=with_append (fun a -> {a with pre_send_frontier=
            if n=0 then 4_294_967_295L else Int64.of_int n})
            ("valid-" ^ string_of_int n) in
          let candidate={candidate with internal_date=
            Some (Result.get_ok (Imap.Internal_date.of_string date))} in
          J.prepare_operation db candidate;
          if J.find_operation db ~id:candidate.id <> Some candidate then
            failwith "valid APPEND failed round trip")
          ["26-Sep-2026 12:00:00 +0000"; "26-Sep-2026 12:00:00 -0000";
           "26-Sep-2026 12:00:00 +0230"];
        (* A receipt from another epoch never replaces the pre-send epoch. *)
        J.prepare_operation db (append "keeps-epoch");
        J.mark_sent db ~id:"keeps-epoch";
        J.observe_operation db ~id:"keeps-epoch" ~receipt:"APPENDUID"
          ~destination_uidvalidity:(Some (epoch 10L))
          ~destination_uid:(Some (Result.get_ok (Imap.Uid.of_int64 4L)));
        (match J.find_operation db ~id:"keeps-epoch" with
         | Some {state=Observed;destination_uidvalidity=Some kept;
                 receipt_uidvalidity=Some receipt;_}
           when kept=epoch 9L && receipt=epoch 10L -> ()
         | _ -> failwith "receipt replaced the pre-send UIDVALIDITY")))
let () = Eio_main.run run
