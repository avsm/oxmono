module Store = Imap_store
let scope : Imap.Mirror.scope = {
  endpoint="imap.example"; account="alice"; mailbox_key="inbox";
  raw_name="INBOX"; encoding=Imap.Mailbox_name.Rev1; mailbox_id=None }
let digest = String.make 64 'a'
let intent ?(digest=digest) ?date id : Store.intent = {
  id; scope; state=Prepared; uidvalidity=None; uid=None;
  kind=Append {message_id="<message@example>"; content_digest=digest;
    spool_ref="spool-message"; pre_send_uid_frontier=Some 0L;
    expected_length=Some 42L; expected_flags=Some [];
    expected_internal_date=date} }
let run env =
  let path=Filename.temp_file "imap-intent-validation-" ".db" in
  Fun.protect ~finally:(fun () -> List.iter (fun path ->
    try Sys.remove path with Sys_error _ -> ()) [path;path^"-wal";path^"-shm"])
    (fun () ->
      let location=Eio.Path.(Eio.Stdenv.fs env / path) in
      Eio.Switch.run (fun sw ->
        let db=Store.open_path ~sw location in
        let reject (candidate:Store.intent) =
          (match Store.prepare_intent db candidate with
           | exception Invalid_argument _ -> ()
           | () -> failwith "invalid metadata accepted");
          if Store.find_intent db ~id:candidate.id <> None then
            failwith "rejected intent was persisted";
          (* The failed preparation must leave both transaction and ID usable. *)
          Store.prepare_intent db (intent candidate.id)
        in
        List.iteri (fun n digest ->
          reject (intent ~digest ("digest-" ^ string_of_int n)))
          [""; "sha256:abc"; String.make 63 'a'; String.make 65 'a';
           String.make 64 'A'; String.make 64 'g'; String.make 63 'a' ^ "\n"];
        List.iteri (fun n date ->
          reject (intent ~date ("date-" ^ string_of_int n)))
          [""; "printable garbage"; "31-Feb-2026 12:00:00 +0000";
           "26-Sep-2026 25:00:00 +0000"; "26-Sep-2026 12:00:00 +9999";
           "26-Sep-2026 12:00:00 +0000\r\n"];
        List.iteri (fun n date ->
          let candidate=intent ~date ("valid-" ^ string_of_int n) in
          Store.prepare_intent db candidate;
          if Store.find_intent db ~id:candidate.id <> Some candidate then
            failwith "valid metadata failed round trip")
          ["26-Sep-2026 12:00:00 +0000"; "26-Sep-2026 12:00:00 -0000";
           "26-Sep-2026 12:00:00 +0230"];
        (match Store.prepare_intent db {(intent "uid-only") with
           uid=Some (Result.get_ok (Imap.Proto.Uid.of_int64 3L))} with
         | exception Invalid_argument _ -> ()
         | () -> failwith "UID without UIDVALIDITY accepted");
        if Store.find_intent db ~id:"uid-only" <> None then
          failwith "UID without UIDVALIDITY was persisted";
        let epoch=Result.get_ok (Imap.Proto.Uidvalidity.of_int64 9L) in
        Store.prepare_intent db {(intent "keeps-epoch") with
          uidvalidity=Some epoch};
        Store.set_intent_state db ~id:"keeps-epoch" Sent;
        Store.confirm_intent db ~id:"keeps-epoch" ~uidvalidity:None ~uid:None;
        (match Store.find_intent db ~id:"keeps-epoch" with
         | Some {state=Confirmed;uidvalidity=Some kept;_} when kept=epoch -> ()
         | _ -> failwith "confirmation without receipt dropped UIDVALIDITY");
        Store.prepare_intent db (intent "legacy");
        Store.prepare_intent db (intent "legacy-null"));
      (* Simulate a pre-validation journal without rewriting historical evidence. *)
      let raw=Sqlite3.db_open path in
      Fun.protect ~finally:(fun () -> ignore (Sqlite3.db_close raw)) (fun () ->
        Sqlite3.Rc.check (Sqlite3.exec raw
          "UPDATE intents SET digest='sha256:abc',\
           expected_internal_date='legacy-invalid-date' WHERE id='legacy'; \
           UPDATE intents SET message_id=NULL,digest=NULL,spool_ref=NULL \
           WHERE id='legacy-null'"));
      Eio.Switch.run (fun sw ->
        let db=Store.open_path ~sw location in
        (match Store.find_intent db ~id:"legacy" with
         | Some {kind=Append {content_digest="sha256:abc";
                   expected_internal_date=Some "legacy-invalid-date";_};_} -> ()
         | _ -> failwith "legacy evidence changed or became unreadable");
        (match Store.find_intent db ~id:"legacy-null" with
         | Some {kind=Append {message_id="";content_digest="";spool_ref="";_};
                 _} -> ()
         | _ -> failwith "legacy NULL recovery fields unreadable");
        if not (List.exists (fun (x:Store.intent) -> x.id="legacy-null")
          (Store.pending_intents db ~scope)) then
          failwith "legacy NULL pending intent hidden";
        if not (List.exists (fun (x:Store.intent) -> x.id="legacy")
          (Store.pending_intents db ~scope)) then
          failwith "legacy pending intent hidden";
        Store.set_intent_state db ~id:"legacy" Rejected))
let () = Eio_main.run run
