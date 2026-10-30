module M = Siftbot_model
module S = Siftbot.Store

let check name condition = if not condition then failwith name
let template = List.hd Siftbot.Demo.messages

let mail id sender list =
  {
    template with
    M.source = { template.source with id };
    sender;
    headers = [ ("List-ID", list) ];
    revision = id;
  }

let assignment (m : M.message) dimension value : M.assignment =
  {
    target = m.source;
    conversation = false;
    dimension;
    value;
    origin = Model;
    version = "test";
    source_revision = m.revision;
    explanation = "A question";
    evidence = [ m.source ];
    score = None;
  }

let () =
  Eio_main.run (fun _ ->
      Eio.Switch.run (fun sw ->
          let db = S.memory ~sw () in
          let first = mail "one" "noreply@zulip.com" "Chat <chat.example>" in
          let second = mail "two" "NOREPLY@ZULIP.COM" "chat.example" in
          let other = mail "other" "person@example.org" "chat.example" in
          let account =
            { first with source = { first.source with account = "other" } }
          in
          let put job ms =
            S.commit_batch db ~job ~expected:None ~cursor:"1" ms []
          in
          put "initial" [ first; second; other; account ];
          S.propose db []
            (List.map
               (fun m -> assignment m M.Attention "needs-reply")
               [ first; second; other; account ]);
          S.cache db ~source:first.source ~revision:first.revision
            ~kind:"classification/active" ~value:"keep";
          let edit e =
            S.edit db ~revision:(S.revision db) e
              ~explanation:"No email reply is possible"
          in
          let labels m dimension =
            (S.snapshot db ~query:"").assignments
            |> List.filter (fun (a : M.assignment) ->
                   a.target = m.M.source && a.dimension = dimension)
            |> List.map (fun (a : M.assignment) -> a.value)
          in
          edit
            (M.Assign_rule (M.Mailing_list, assignment first Attention "waiting"));
          check "list applies across senders"
            (labels other Attention = [ "waiting" ]);
          edit (M.Assign_rule (M.Sender, assignment first Attention "fyi"));
          check "sender wins over list" (labels first Attention = [ "fyi" ]);
          check "sender matching ignores case"
            (labels second Attention = [ "fyi" ]);
          check "account scope" (labels account Attention = [ "needs-reply" ]);
          check "rule keeps classification cache"
            (S.cached db ~source:first.source ~kind:"classification/active"
            = Some "keep");
          edit (M.Assign_rule (Sender, assignment first Attention "deadline"));
          check "replacement applies immediately"
            (labels second Attention = [ "deadline" ]);
          edit M.Undo;
          check "undo replacement restores earlier rule"
            (labels second Attention = [ "fyi" ]);
          let future = mail "future" first.sender "chat.example" in
          put "future" [ future ];
          check "new mail needs no model run"
            (labels future Attention = [ "fyi" ]);
          S.propose db [] [ assignment first Attention "needs-reply" ];
          check "model cannot override saved rule"
            (labels first Attention = [ "fyi" ]);
          edit (M.Assign (assignment first Attention "deadline"));
          check "message override wins" (labels first Attention = [ "deadline" ]);
          edit M.Undo;
          check "undo message returns to sender rule"
            (labels first Attention = [ "fyi" ]);
          edit M.Undo;
          check "undo sender returns to list"
            (labels first Attention = [ "waiting" ]);
          edit M.Undo;
          check "undo rules restores model prediction"
            (labels first Attention = [ "needs-reply" ]);
          check "unrelated suggestions survive"
            (labels other Attention = [ "needs-reply" ]);
          let stale =
            {
              (assignment first Attention "fyi") with
              source_revision = "stale";
            }
          in
          check "stale source rejected"
            (try
               edit (M.Assign_rule (Sender, stale));
               false
             with S.Conflict -> true);
          let invalid = { other with headers = [] } in
          put "missing-list" [ invalid ];
          check "missing List-ID rejected"
            (try
               edit
                 (M.Assign_rule
                    (Mailing_list, assignment invalid Attention "fyi"));
               false
             with Invalid_argument _ -> true);
          edit (M.Assign_rule (Sender, assignment first Delivery "automated"));
          check "delivery correction works"
            (labels second Delivery = [ "automated" ]);
          check "attention is independent"
            (labels first Attention = [ "needs-reply" ]);
          let view = Siftbot.Review.snapshot db ~query:"" () in
          check "review retains List-ID for scope preview"
            (List.exists
               (fun (m : M.message) ->
                 m.source = first.source && m.headers <> [])
               view.messages);
          check "bounded review uses rules"
            (List.exists
               (fun (a : M.assignment) ->
                 a.target = first.source && a.dimension = Delivery
                 && a.value = "automated")
               view.assignments)));
  print_endline
    "Saved label rules: precedence, existing/future mail, account scope, cache \
     preservation and undo passed"

let () =
  let path = Filename.temp_file "sift-rules" ".sqlite3" in
  Fun.protect
    ~finally:(fun () ->
      List.iter
        (fun suffix -> try Sys.remove (path ^ suffix) with Sys_error _ -> ())
        [ ""; "-wal"; "-shm" ])
    (fun () ->
      Eio_main.run (fun env ->
          Eio.Switch.run (fun sw ->
              ignore (S.open_db ~sw Eio.Path.(Eio.Stdenv.fs env / path)));
          let legacy = Sqlite3.db_open path in
          Fun.protect
            ~finally:(fun () -> ignore (Sqlite3.db_close legacy))
            (fun () ->
              Sqlite3.Rc.check
                (Sqlite3.exec legacy
                   "DROP TABLE label_rules; PRAGMA user_version=6;"));
          let first = mail "persistent" "noreply@zulip.com" "chat.example" in
          Eio.Switch.run (fun sw ->
              let db = S.open_db ~sw Eio.Path.(Eio.Stdenv.fs env / path) in
              S.commit_batch db ~job:"initial" ~expected:None ~cursor:"1"
                [ first ] [];
              S.edit db ~revision:(S.revision db)
                (M.Assign_rule (Sender, assignment first Attention "fyi"))
                ~explanation:"Notification");
          Eio.Switch.run (fun sw ->
              let db = S.open_db ~sw Eio.Path.(Eio.Stdenv.fs env / path) in
              check "saved rules survive reopening"
                (List.exists
                   (fun (a : M.assignment) -> a.value = "fyi")
                   (S.labels db first.source)))))
