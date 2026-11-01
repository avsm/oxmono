(* Copyright (c) 2026 Anil Madhavapeddy. SPDX-License-Identifier: ISC *)
open! Core
module C = Dooit.Common
module M = Termanil_model

let temp f =
  let p = Stdlib.Filename.temp_file "termanil-workflow-" "" in
  Stdlib.Sys.remove p;
  C.mkdir p;
  Exn.protect
    ~f:(fun () -> f p)
    ~finally:(fun () -> Sortal_carddav.Common.remove_tree p)

let run root r =
  (* Each call reconstructs the worker configuration and native JMAP client. *)
  match Termanil_backend.execute (Termanil_backend.Config.demo ~root ()) r with
  | Ok r -> r
  | Error s -> failwith s

let workspace root =
  match run root M.Workspace with
  | Workspace_loaded (ns, ds, warnings) ->
      assert (List.is_empty warnings);
      (ns, ds)
  | _ -> assert false

let page root ?(query = "") ?(mailbox = "inbox") () =
  match
    run root (M.Messages { mailbox = Some mailbox; query; position = 0 })
  with
  | Messages_loaded p -> p
  | _ -> assert false

let source = (List.hd_exn Termanil_demo.messages).source

let save root source body expected =
  match run root (M.Save_reply { source; body; expected }) with
  | Reply_saved d -> d
  | _ -> assert false

let%expect_test
    "persistent demo searches collapsed history and keeps flags and linked \
     tasks" =
  temp (fun root ->
      let ns, ds = workspace root in
      printf "seed: %d tasks, %d drafts\n" (List.length ns) (List.length ds);
      let p = page root () in
      printf "inbox: %d conversations\n" (List.length p.messages);
      let m = List.hd_exn p.messages in
      let t = { M.initial with tasks = ns } in
      printf "linked task: %d\n" (List.length (M.linked_tasks t m));
      let old = List.hd_exn (page root ~query:"lavender" ()).messages in
      printf "history search: %s, linked task: %d\n" old.source.id
        (List.length (M.linked_tasks t old));
      (match run root (M.Conversation old.source) with
      | Conversation_loaded (_, ms, _) ->
          printf "conversation: %d emails\n" (List.length ms)
      | _ -> assert false);
      ignore (run root (M.Set_seen (source, true)));
      ignore (run root (M.Set_flagged (source, true)));
      let m = List.hd_exn (page root ()).messages in
      printf "reconnected: seen=%b starred=%b\n" m.seen m.flagged);
  [%expect
    {|
    seed: 2 tasks, 1 drafts
    inbox: 6 conversations
    linked task: 1
    history search: garden-previous-1, linked task: 1
    conversation: 3 emails
    reconnected: seen=true starred=true
    |}]

let%expect_test "agent edits unqueue a draft and stale saves preserve the file"
    =
  temp (fun root ->
      ignore (workspace root);
      let d = save root source "First reply\n" None in
      let d =
        match run root (M.Queue_reply (d, true)) with
        | Reply_saved d -> d
        | _ -> assert false
      in
      printf "queued: %s\n" d.state;
      C.atomic_write d.path (C.read d.path ^ "Agent adds a detail.\n");
      let _, ds = workspace root in
      let changed =
        List.find_exn ds ~f:(fun d -> M.same_email d.M.source source)
      in
      printf "after agent edit: %s\n" changed.state;
      (match
         Termanil_backend.execute
           (Termanil_backend.Config.demo ~root ())
           (M.Save_reply
              { source; body = "Overwrite"; expected = Some d.revision })
       with
      | Error _ -> print_endline "stale editor save refused"
      | _ -> assert false);
      printf "agent edit retained: %b\n"
        (String.is_suffix (C.read d.path) ~suffix:"Agent adds a detail.\n");
      match
        Termanil_backend.execute
          (Termanil_backend.Config.demo ~root ())
          (M.Send_replies [ d ])
      with
      | Error _ -> print_endline "stale send review refused"
      | _ -> assert false);
  [%expect
    {|
    queued: ready
    after agent edit: draft
    stale editor save refused
    agent edit retained: true
    stale send review refused
    |}]

let%expect_test
    "batch sending uses JMAP reply headers and survives reconnect without \
     duplicate sends" =
  temp (fun root ->
      let _, ds = workspace root in
      let d = save root source "Thanks Ada, I will review the plan.\n" None in
      let reviewed = d :: ds in
      (match run root (M.Send_replies reviewed) with
      | Replies_sent (sent, lines) ->
          print_s [%sexp (List.map sent ~f:(fun d -> d.M.state) : string list)];
          List.iter lines ~f:print_endline
      | _ -> assert false);
      printf "sent mailbox: %d conversations\n"
        (List.length (page root ~mailbox:"sent" ()).messages);
      let state = C.load_json (Filename.concat root "server.json") in
      let sent =
        List.filter (C.items "emails" state) ~f:(fun e ->
            List.mem
              (C.assoc (C.get "mailboxIds" e))
              ("sent", `Bool true)
              ~equal:Poly.equal)
      in
      printf "thread headers: %b\n"
        (List.for_all sent ~f:(fun e ->
             (not (List.is_empty (C.items "inReplyTo" e)))
             && not (List.is_empty (C.items "references" e))));
      (match
         Termanil_backend.execute
           (Termanil_backend.Config.demo ~root ())
           (M.Send_replies reviewed)
       with
      | Error _ -> print_endline "duplicate batch refused"
      | _ -> assert false);
      let state = C.load_json (Filename.concat root "server.json") in
      printf "accepted submissions: %d\n"
        (List.length (C.items "submissions" state)));
  [%expect
    {|
    (sent sent)
    Sent: Re: Review the garden plan
    Sent: Re: Lunch on Friday
    sent mailbox: 2 conversations
    thread headers: true
    duplicate batch refused
    accepted submissions: 2
    |}]

let%expect_test
    "uncertain sends stop the batch and cannot be retried by editing a file" =
  temp (fun root ->
      let d =
        Termanil_drafts.save root ~source ~thread_id:"thread-email-1"
          ~subject:"Re: Garden" ~recipients:[ "ada@example.test" ]
          ~body:"Reply\n" ~expected:None
      in
      let attempts = ref 0 in
      let prepare _ ~before_submit =
        incr attempts;
        before_submit "remote-email";
        failwith "connection lost"
      in
      let ds, _ = Termanil_drafts.send root [ d ] ~prepare in
      printf "outcome: %s\n" (List.hd_exn ds).state;
      C.atomic_write d.path (C.read d.path ^ "Edited after disconnect\n");
      let fresh = List.hd_exn (fst (Termanil_drafts.list root)) in
      printf "edited outcome: %s\n" fresh.state;
      (try ignore (Termanil_drafts.send root [ fresh ] ~prepare)
       with C.Error _ -> print_endline "retry refused");
      printf "submission attempts: %d\n" !attempts);
  [%expect
    {|
    outcome: uncertain
    edited outcome: uncertain
    retry refused
    submission attempts: 1
    |}]

let%expect_test
    "a lost submission response is reconciled by JMAP without resending" =
  temp (fun root ->
      ignore (workspace root);
      let d = save root source "Confirmed.\n" None in
      Eio_main.run (fun _ ->
          Eio.Switch.run (fun sw ->
              let client = Termanil_fake_jmap.connect ~sw ~root in
              let mail =
                Termanil_mail.create ~client ~service:source.service
                  ~account:source.account
              in
              let ds, _ =
                Termanil_drafts.send (Filename.dirname d.path) [ d ]
                  ~prepare:(fun d ->
                    let submit =
                      Termanil_mail.prepare_reply mail ~identity:(Some "demo") d
                    in
                    fun ~before_submit ->
                      ignore (submit ~before_submit);
                      failwith "response lost")
              in
              printf "before verify: %s\n" (List.hd_exn ds).state));
      (match run root M.Verify_replies with
      | Replies_sent (ds, _) ->
          let d =
            List.find_exn ds ~f:(fun d -> M.same_email d.M.source source)
          in
          printf "after verify: %s\n" d.state
      | _ -> assert false);
      let state = C.load_json (Filename.concat root "server.json") in
      printf "accepted submissions: %d\n"
        (List.length (C.items "submissions" state)));
  [%expect
    {|
    before verify: uncertain
    after verify: sent
    accepted submissions: 1
    |}]

let%expect_test
    "empty agent scaffolds preserve unknown frontmatter and cannot be queued" =
  temp (fun root ->
      let d =
        Termanil_drafts.save root ~source ~thread_id:"thread"
          ~subject:"Re: Garden" ~recipients:[ "ada@example.test" ] ~body:""
          ~expected:None
      in
      (try ignore (Termanil_drafts.queue root d true)
       with C.Error _ -> print_endline "empty queue refused");
      let raw = C.read d.path in
      let raw =
        String.substr_replace_first raw ~pattern:"{\n"
          ~with_:"# Agent annotation\n{\n  \"future_field\": [1, true],\n"
      in
      C.atomic_write d.path raw;
      let d = List.hd_exn (fst (Termanil_drafts.list root)) in
      ignore
        (Termanil_drafts.save root ~source ~thread_id:d.thread_id
           ~subject:d.subject ~recipients:d.recipients ~body:"Agent reply.\n"
           ~expected:(Some d.revision));
      printf "header preserved: %b\n"
        (String.equal (C.read d.path) (raw ^ "Agent reply.\n")));
  [%expect {|
    empty queue refused
    header preserved: true
    |}]

let%expect_test "rich JMAP reading, filtered search and archive persist" =
  temp (fun root ->
      let p =
        page root
          ~query:"from:samira has:attachment after:2026-09-01 before:2026-10-01"
          ()
      in
      let m = List.hd_exn p.messages in
      printf "attachment search: %s\n" m.source.id;
      (match run root (M.Read m.source) with
      | Message_loaded (m, body) ->
          List.iter m.metadata ~f:(fun (k, v) ->
              if String.equal k "Attachment" then printf "%s: %s\n" k v);
          printf "HTML readable: %b, link retained: %b, styles omitted: %b\n"
            (String.is_substring body ~substring:"16:20")
            (String.is_substring body
               ~substring:"https://trains.example.test/booking")
            (not (String.is_substring body ~substring:"color: red"))
      | _ -> assert false);
      printf "unread starred: %s\n"
        (String.concat ~sep:","
           (List.map (page root ~query:"is:unread is:starred" ()).messages
              ~f:(fun m -> m.M.source.id)));
      let path = Filename.concat root "server.json" in
      let state = C.load_json path in
      let emails =
        List.map (C.items "emails" state) ~f:(fun e ->
            if not (String.equal (C.field "id" e) m.source.id) then e
            else
              C.set "mailboxIds"
                (C.obj [ ("inbox", `Bool true); ("travel", `Bool true) ])
                e)
      in
      C.save_json path (C.set "emails" (C.arr emails) state);
      ignore (run root (M.Archive m.source));
      printf "inbox search after archive: %d\n"
        (List.length (page root ~query:"from:samira" ()).messages);
      printf "archive search: %d\n"
        (List.length
           (page root ~mailbox:"archive" ~query:"has:attachment" ()).messages);
      let e =
        List.find_exn
          (C.items "emails" (C.load_json path))
          ~f:(fun e -> String.equal (C.field "id" e) m.source.id)
      in
      printf "other mailbox and seen retained: %b\n"
        (Option.is_some (C.find "travel" (C.get "mailboxIds" e))
        && Option.is_some (C.find "$seen" (C.get "keywords" e))));
  [%expect
    {|
    attachment search: email-4
    Attachment: train-ticket.pdf | application/pdf | 24576 bytes
    HTML readable: true, link retained: true, styles omitted: true
    unread starred: email-3
    inbox search after archive: 0
    archive search: 1
    other mailbox and seen retained: true
    |}]

let%expect_test
    "identity signatures are editable draft content and not appended on send" =
  temp (fun root ->
      let signature =
        match run root (M.Conversation source) with
        | Conversation_loaded (_, _, Ok s) -> s
        | _ -> assert false
      in
      printf "identity signature: %S\n" signature;
      let config =
        {
          (Termanil_backend.Config.demo ~root ()) with
          signature = Some "-- \nPersonal override";
        }
      in
      (match Termanil_backend.execute config (M.Conversation source) with
      | Ok (Conversation_loaded (_, _, Ok s)) -> printf "override: %S\n" s
      | _ -> assert false);
      let config = { config with signature = Some "" } in
      (match Termanil_backend.execute config (M.Conversation source) with
      | Ok (Conversation_loaded (_, _, Ok s)) -> printf "disabled: %S\n" s
      | _ -> assert false);
      let reviewed = "Thanks Ada.\n\n" ^ signature ^ " (edited)\n" in
      let d = save root source reviewed None in
      ignore (run root (M.Send_replies [ d ]));
      let e =
        List.find_exn
          (C.items "emails" (C.load_json (Filename.concat root "server.json")))
          ~f:(fun e -> String.is_prefix (C.field "id" e) ~prefix:"reply-")
      in
      let values = C.assoc (C.get "bodyValues" e) in
      printf "submitted exactly reviewed body: %b\n"
        (List.exists values ~f:(fun (_, v) ->
             String.equal (C.field "value" v) reviewed)));
  [%expect
    {|
    identity signature: "-- \nYou\nCommunity garden & workshop"
    override: "-- \nPersonal override"
    disabled: ""
    submitted exactly reviewed body: true
    |}]
