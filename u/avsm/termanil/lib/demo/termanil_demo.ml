(* Copyright (c) 2026 Anil Madhavapeddy. SPDX-License-Identifier: ISC *)
open Termanil_model

let source id =
  {
    service = "https://mail.example.test/jmap/session";
    account = "personal";
    id;
  }

let samples =
  [
    ( "email-1",
      "Ada",
      "ada",
      "Review the garden plan",
      "Could you review the planting notes?",
      {|Hi,

Could you review the planting notes before Thursday?

The plan has three parts:

1. North bed
   Keep the existing rosemary and add thyme along the path.
   Leave enough room for the water butt.

2. South bed
   Plant beans against the trellis and lettuce in the shade.
   The seedlings will be ready next week.

3. Weekend jobs
   Measure the greenhouse bench.
   Order two bags of compost.
   Ask the neighbours about spare pots.

I have added the measurements to our shared notes.
Please tell me if the layout needs another pass.

Thanks,
Ada
|},
      false,
      false );
    ( "email-2",
      "Grace",
      "grace",
      "Lunch on Friday",
      "Shall we meet at noon?",
      "Hello!\n\n\
       Shall we meet at noon by the library?\n\
       The café has a quiet table outside.\n\n\
       Grace\n",
      true,
      true );
    ( "email-3",
      "Lin",
      "lin",
      "Notes from the design review",
      "The accessibility pass is ready for review.",
      "Hi team,\n\n\
       The new keyboard flow is ready to try:\n\n\
       - Open a message with Enter.\n\
       - Use Up and Down to read neighbouring messages.\n\
       - Press e to write a reply.\n\n\
       Please check narrow terminals and Unicode text: café, 日本語.\n\n\
       Lin\n",
      false,
      true );
    ( "email-4",
      "Samira",
      "samira",
      "Train tickets and arrival times",
      "I arrive at 16:20 on Saturday.",
      "Hello,\n\n\
       I arrive at 16:20 on Saturday.\n\
       Could someone collect the keys before then?\n\n\
       The return train is at 18:05 on Sunday.\n\n\
       Samira\n",
      true,
      false );
    ( "email-5",
      "Theo",
      "theo",
      "Reading group: next chapter",
      "Bring one question about chapter four.",
      "Hi everyone,\n\n\
       Our next session is Wednesday at 19:00.\n\
       Bring one question about chapter four and a passage to discuss.\n\n\
       We will choose next month's book afterwards.\n\n\
       Theo\n",
      false,
      false );
    ( "email-6",
      "Morgan",
      "morgan",
      "Re: Community workshop checklist",
      "The room booking is confirmed.",
      "The room booking is confirmed.\n\n\
       I can bring extension leads and name badges.\n\
       Could you check the projector and print the schedule?\n\n\
       > We need the room ready by 09:30.\n\
       > Tea and coffee arrive at 10:00.\n\n\
       Morgan\n",
      true,
      false );
  ]

let messages =
  List.mapi
    (fun i (id, name, handle, subject, preview, _, seen, flagged) ->
      {
        source = source id;
        thread_id = "thread-" ^ id;
        subject;
        sender = name ^ " <" ^ handle ^ "@example.test>";
        addresses = [ handle ^ "@example.test" ];
        received = Printf.sprintf "2026-09-%02dT09:00:00Z" (12 - i);
        preview;
        seen;
        flagged;
        metadata = [ ("To", "You <you@example.test>") ];
      })
    samples

let body id =
  match List.find_opt (fun (key, _, _, _, _, _, _, _) -> key = id) samples with
  | Some (_, _, _, _, _, body, _, _) -> body
  | None -> "Earlier conversation message."

let contacts =
  List.map
    (fun (handle, name, extra) ->
      let email = handle ^ "@example.test" in
      {
        key = "vcard:demo:" ^ handle;
        name;
        emails = [ email ];
        sources = [ "Demo / " ^ handle ^ ".vcf" ];
        metadata =
          [
            ("Name", name);
            ("Email", email);
            ("Sortal ID", handle);
            ("UID", "demo-" ^ handle);
          ]
          @ extra;
      })
    [
      ( "ada",
        "Ada",
        [
          ("Organisation", "Community garden");
          ("Atom feed", "https://ada.example.test/feed.xml");
          ("GitHub", "https://github.com/example-ada");
          ( "Note",
            "Coordinates the planting plan.\nPrefers email for arrangements." );
        ] );
      ( "grace",
        "Grace",
        [
          ("Organisation", "Library friends");
          ("Phone (WORK)", "+44 1632 960001");
        ] );
      ("lin", "Lin", [ ("Title", "Designer"); ("Languages", "English, 日本語") ]);
      ("samira", "Samira", [ ("Tags", "travel, friends") ]);
      ("theo", "Theo", [ ("RSS feed", "https://theo.example.test/rss.xml") ]);
      ( "morgan",
        "Morgan",
        [
          ("Organisation", "Community workshop");
          ("Website", "https://workshop.example.test/");
        ] );
      ( "river",
        "River",
        [ ("Pronouns", "they/them"); ("Availability", "Wednesday afternoons") ]
      );
      ("jo", "Jo", [ ("Note", "Ask about the seed exchange.") ]);
    ]

let create () =
  let mail = ref messages and tasks = ref [] and drafts = ref [] in
  let clock = ref 0 in
  let updated () =
    incr clock;
    Printf.sprintf "2026-09-13T10:00:%02dZ" !clock
  in
  fun req ->
    let find source =
      List.find_opt (fun (m : message) -> same_email source m.source) !mail
    in
    match req with
    | Verify_replies ->
        Ok (Replies_sent (!drafts, [ "No uncertain submissions" ]))
    | Workspace -> Ok (Workspace_loaded (!tasks, !drafts, []))
    | Conversation source -> (
        match find source with
        | None -> Error "Message not found"
        | Some m ->
            Ok (Conversation_loaded (source, [ (m, body source.id) ], Ok "")))
    | Save_reply { source; body; expected } -> (
        match find source with
        | None -> Error "Message not found"
        | Some m ->
            let old =
              List.find_opt
                (fun (d : draft) -> same_email source d.source)
                !drafts
            in
            if Option.map (fun (d : draft) -> d.revision) old <> expected then
              Error "Reply changed on disk"
            else
              let d =
                {
                  source;
                  thread_id = m.thread_id;
                  subject = "Re: " ^ m.subject;
                  recipients = m.addresses;
                  body;
                  revision = Digest.to_hex (Digest.string body);
                  state = "draft";
                  path = "/demo/replies/" ^ source.id ^ ".md";
                }
              in
              drafts :=
                d
                :: List.filter
                     (fun (x : draft) -> not (same_email x.source source))
                     !drafts;
              Ok (Reply_saved d))
    | Queue_reply (d, ready) ->
        let d = { d with state = (if ready then "ready" else "draft") } in
        drafts :=
          d
          :: List.filter
               (fun (x : draft) -> not (same_email x.source d.source))
               !drafts;
        Ok (Reply_saved d)
    | Send_replies ds ->
        let ds = List.map (fun (d : draft) -> { d with state = "sent" }) ds in
        List.iter
          (fun (d : draft) ->
            drafts :=
              d
              :: List.filter
                   (fun (x : draft) -> not (same_email x.source d.source))
                   !drafts)
          ds;
        Ok
          (Replies_sent
             (ds, List.map (fun (d : draft) -> "Sent: " ^ d.subject) ds))
    | Mailboxes ->
        Ok
          (Mailboxes_loaded
             [
               {
                 id = "inbox";
                 name = "Inbox";
                 unread =
                   List.length
                     (List.filter (fun (m : message) -> not m.seen) !mail);
                 inbox = true;
               };
               { id = "archive"; name = "Archive"; unread = 0; inbox = false };
             ])
    | Messages { mailbox; query; position } ->
        let rows =
          if mailbox = Some "archive" then []
          else
            List.filter
              (fun (m : message) -> matches ~query (m.subject ^ " " ^ m.sender))
              !mail
        in
        Ok
          (Messages_loaded
             {
               mailbox;
               query;
               position;
               next = None;
               messages = (if position = 0 then rows else []);
             })
    | Read source -> (
        match find source with
        | None -> Error "Message not found"
        | Some m ->
            let _, _, _, _, _, body, _, _ =
              List.find
                (fun (id, _, _, _, _, _, _, _) -> id = source.id)
                samples
            in
            Ok (Message_loaded (m, body)))
    | Archive source ->
        mail :=
          List.filter
            (fun (m : message) -> not (same_email source m.source))
            !mail;
        Ok (Archived source)
    | Set_seen (source, value) | Set_flagged (source, value) -> (
        match find source with
        | None -> Error "Message not found"
        | Some _ ->
            let kind = match req with Set_seen _ -> `Seen | _ -> `Flagged in
            mail :=
              List.map
                (fun (m : message) ->
                  if not (same_email source m.source) then m
                  else
                    match kind with
                    | `Seen -> { m with seen = value }
                    | `Flagged -> { m with flagged = value })
                !mail;
            Ok (Message_changed (source, kind, value)))
    | Contacts -> Ok (Contacts_loaded (contacts, []))
    | Tasks -> Ok (Tasks_loaded (!tasks, []))
    | Capture m ->
        let found =
          List.filter
            (fun n -> List.exists (same_email m.source) n.sources)
            !tasks
        in
        let found =
          if found <> [] then found
          else
            let n =
              {
                id = "task-" ^ m.source.id;
                revision = "revision-1";
                title = m.subject;
                updated = updated ();
                status = "open";
                tags = [ "inbox" ];
                body = "";
                sources = [ m.source ];
                threads = [ { m.source with id = m.thread_id } ];
              }
            in
            tasks := n :: !tasks;
            [ n ]
        in
        Ok (Captured found)
    | Complete task -> (
        match List.find_opt (fun (n : task) -> n.id = task.id) !tasks with
        | Some n when n.revision = task.revision ->
            let n =
              {
                n with
                status = "done";
                revision = "revision-2";
                updated = updated ();
              }
            in
            tasks := n :: List.filter (fun (x : task) -> x.id <> n.id) !tasks;
            Ok (Completed n)
        | _ -> Error "Stale task revision")
    | Sync_tasks { dry_run } ->
        Ok
          (Synced
             [
               (if dry_run then "Dry run: no writes"
                else "Demo: no network writes");
               Printf.sprintf "%d tasks unchanged" (List.length !tasks);
             ])
