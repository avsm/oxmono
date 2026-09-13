(* Copyright (c) 2026 Anil Madhavapeddy. SPDX-License-Identifier: ISC *)
open! Core
module M = Termanil_model

let fst_update t a = fst (M.update t a)

let ready =
  {
    M.initial with
    page = { M.initial.page with messages = Termanil_demo.messages };
  }

let pending_id t = fst (Option.value_exn t.M.pending)

let%expect_test "late and foreign completions cannot replace current data" =
  let t, request = M.update ready M.Open in
  let id = pending_id t in
  let m = List.hd_exn Termanil_demo.messages in
  let late =
    fst_update t
      (Finished
         (id + 1, Ok (Conversation_loaded (m.source, [ (m, "late") ], Ok ""))))
  in
  printf "late ignored: %b\n" (Poly.equal t late);
  let other = { m with source = { m.source with account = "other" } } in
  let wrong =
    fst_update t
      (Finished
         ( id,
           Ok (Conversation_loaded (other.source, [ (other, "wrong") ], Ok ""))
         ))
  in
  printf "%s\n" wrong.status;
  print_s
    [%sexp
      (Option.map request ~f:(fun (_, r) -> M.request_name r) : string option)];
  [%expect
    {|
    late ignored: true
    Backend response identity mismatch
    ("Loading conversation")
    |}]

let%expect_test
    "navigation during a write preserves the target and serializes requests" =
  let t, request = M.update ready Capture_selected in
  let t = fst_update t (Move 1) in
  let _, second = M.update t Toggle_seen in
  (match request with
  | Some (_, Capture m) -> printf "target: %s\n" m.source.id
  | _ -> assert false);
  printf "second request: %b\n" (Option.is_some second);
  [%expect {|
    target: email-1
    second request: false
    |}]

let%expect_test "empty lists, unicode search and backspace" =
  let t = fst_update M.initial (Move (-10)) |> fun t -> fst_update t Open in
  printf "empty selection: %d\n" t.selected;
  let t = fst_update t Search |> fun t -> fst_update t (Type "a界") in
  let t = fst_update t Erase in
  print_s [%sexp (t.input : string option)];
  printf "unicode: %b\n" (M.matches ~query:"STRASSE café" "Straße CAFÉ");
  printf "controls: %s\n" (M.safe_text "a\027[31m\000b\226\128\174x");
  [%expect
    {|
    empty selection: 0
    (a)
    unicode: true
    controls: a?[31m?b?x
    |}]

let%expect_test "pagination uses actual server positions" =
  let t = { ready with page = { ready.page with next = Some 2 } } in
  let t, _ = M.update t Next_page in
  let page = { t.page with position = 2; next = Some 4; messages = [] } in
  let t = fst_update t (Finished (pending_id t, Ok (Messages_loaded page))) in
  let t =
    fst_update t (Finished (pending_id t, Ok (Workspace_loaded ([], [], []))))
  in
  let _, previous = M.update t Previous_page in
  print_s [%sexp (t.previous : int list)];
  (match previous with
  | Some (_, Messages { position; _ }) -> printf "back: %d\n" position
  | _ -> assert false);
  [%expect {|
    (0)
    back: 0
    |}]

let%expect_test "sync confirmation is explicit and cancellable" =
  let t = { M.initial with tab = Todos } in
  let t, first = M.update t Confirm_sync in
  printf "before confirmation: %b\n" (Option.is_some first);
  let cancelled = fst_update t Cancel in
  let _, after_cancel = M.update cancelled Submit in
  printf "after cancel: %b\n" (Option.is_some after_cancel);
  let _, confirmed = M.update t Submit in
  (match confirmed with
  | Some (_, Sync_tasks { dry_run }) -> printf "dry: %b\n" dry_run
  | _ -> assert false);
  [%expect
    {|
    before confirmation: false
    after cancel: false
    dry: false
    |}]

let%expect_test "tab switches during loading queue the selected view" =
  let t, _ = M.update M.initial Start in
  let t = fst_update t (Switch People) in
  let t, next =
    M.update t (Finished (pending_id t, Ok (Mailboxes_loaded [])))
  in
  printf "people: %b\n" (Poly.equal t.tab People);
  print_s
    [%sexp
      (Option.map next ~f:(fun (_, r) -> M.request_name r) : string option)];
  [%expect {|
    people: true
    ("Loading contacts")
    |}]

let%expect_test "failed search keeps the old query paired with the old results"
    =
  let t = fst_update ready Search |> fun t -> fst_update t (Type "new-query") in
  let t, _ = M.update t Submit in
  let t = fst_update t (Finished (pending_id t, Error "offline")) in
  printf "query: %S\nmessages: %d\n" t.page.query (List.length t.page.messages);
  [%expect {|
    query: ""
    messages: 6
    |}]

let%expect_test "refresh retains selection by identity when tasks reorder" =
  let note id : M.task =
    {
      id;
      revision = "r1";
      title = id;
      updated = "2026-09-13T10:00:00Z";
      status = "open";
      tags = [];
      body = "";
      sources = [];
      threads = [];
    }
  in
  let a = note "a" and b = note "b" in
  let t = { M.initial with tab = Todos; tasks = [ a; b ]; selected = 1 } in
  let t, _ = M.update t Refresh in
  let t =
    fst_update t (Finished (pending_id t, Ok (Tasks_loaded ([ b; a ], []))))
  in
  printf "selected: %s\n" (Option.value_exn (M.selected_task t)).id;
  [%expect {| selected: b |}]

let%expect_test
    "opened mail arrows navigate while body scrolling keeps identity" =
  let demo = Termanil_demo.create () in
  let apply t action =
    let t, request = M.update t action in
    match request with
    | None -> t
    | Some (id, request) -> fst_update t (Finished (id, demo request))
  in
  let id t = (Option.value_exn (M.selected_message t)).source.id in
  let t = apply ready Open in
  printf "opened: %s\n" (id t);
  let t = apply t (Move 1) in
  printf "down: %s\n" (id t);
  let t = apply t (Scroll 10) in
  printf "scroll: %s, offset %d\n" (id t) t.scroll;
  let t = apply t (Move (-1)) in
  printf "up: %s, offset %d\n" (id t) t.scroll;
  let t = apply t Focus_reply in
  let t = apply t (Move 1) in
  printf "reply cursor cannot select: %s\n" (id t);
  let source = (Option.value_exn (M.selected_message t)).source in
  let t = apply t (Draft_changed (source, "Remember me")) in
  let t =
    apply t Cancel |> fun t ->
    apply t (Move 1) |> fun t -> apply t (Move (-1))
  in
  printf "draft retained: %s\n"
    (List.Assoc.find_exn t.drafts source ~equal:M.same_email);
  [%expect
    {|
    opened: email-1
    down: email-2
    scroll: email-2, offset 10
    up: email-1, offset 0
    reply cursor cannot select: email-1
    draft retained: Remember me
    |}]

let%expect_test
    "switching tabs during chained mail and workspace loads refreshes the \
     chosen view" =
  let t =
    {
      ready with
      mailboxes = [ { id = "inbox"; name = "Inbox"; unread = 3; inbox = true } ];
    }
  in
  let t, _ = M.update t Refresh in
  let t = fst_update t (Switch People) in
  let t = fst_update t (Finished (pending_id t, Ok (Messages_loaded t.page))) in
  let _, next =
    M.update t (Finished (pending_id t, Ok (Workspace_loaded ([], [], []))))
  in
  print_s
    [%sexp
      (Option.map next ~f:(fun (_, r) -> M.request_name r) : string option)];
  [%expect {| ("Loading contacts") |}]

let%expect_test "mail to task to source round trip and explicit send review" =
  let demo = Termanil_demo.create () in
  let apply t a =
    let rec finish (t, request) =
      match request with
      | None -> t
      | Some (id, r) -> finish (M.update t (Finished (id, demo r)))
    in
    finish (M.update t a)
  in
  let t = apply ready Open |> fun t -> apply t Capture_selected in
  let t = apply t Task_selected in
  printf "task view: %b\n" (Poly.equal t.tab Todos && Poly.equal t.focus Detail);
  let t = apply t Source_selected in
  printf "source: %s\n" (Option.value_exn t.opened).id;
  let t =
    apply t
      (Draft_changed (Option.value_exn t.opened, "Confirming Thursday.\n"))
  in
  let t = apply t Save_draft |> fun t -> apply t Queue_draft in
  let t = apply t (Switch Outbox) in
  let t, send = M.update t Review_send in
  printf "review without send: %b\n"
    (Option.is_some t.send_review && Option.is_none send);
  let cancelled = apply t Cancel in
  printf "cancelled: %b\n" (Option.is_none cancelled.send_review);
  let _, send = M.update t Submit in
  (match send with
  | Some (_, Send_replies ds) ->
      printf "confirmed replies: %d\n" (List.length ds)
  | _ -> assert false);
  [%expect
    {|
    task view: true
    source: email-1
    review without send: true
    cancelled: true
    confirmed replies: 1
    |}]

let%expect_test
    "workspace refresh reloads pristine editors and retains concurrent typing" =
  let source = (List.hd_exn Termanil_demo.messages).source in
  let d : M.draft =
    {
      source;
      thread_id = "thread";
      subject = "Re: Garden";
      recipients = [ "ada@example.test" ];
      body = "Saved";
      revision = "r1";
      state = "draft";
      path = "/replies/draft.md";
    }
  in
  let reload t ds =
    let t, _ = M.update { t with tab = Outbox } Refresh in
    fst_update t (Finished (pending_id t, Ok (Workspace_loaded ([], ds, []))))
  in
  let t = reload ready [ d ] in
  let changed = { d with body = "Agent revised"; revision = "r2" } in
  let clean = reload t [ changed ] in
  printf "clean buffer reloads: %b\n"
    (String.equal
       (List.Assoc.find_exn clean.drafts source ~equal:M.same_email)
       changed.body);
  printf "editor instance changes: %b\n"
    (M.editor_version clean source > M.editor_version t source);
  let dirty = fst_update t (Draft_changed (source, "Still typing")) in
  let dirty = reload dirty [ changed ] in
  printf "dirty buffer retained: %s\n"
    (List.Assoc.find_exn dirty.drafts source ~equal:M.same_email);
  print_s
    [%sexp
      (List.Assoc.find_exn dirty.draft_bases source ~equal:M.same_email
        : string option)];
  [%expect
    {|
    clean buffer reloads: true
    editor instance changes: true
    dirty buffer retained: Still typing
    (r1)
    |}]

let%expect_test "queueing an outbox row preserves its selection and draft order"
    =
  let a : M.draft =
    {
      source = (List.hd_exn Termanil_demo.messages).source;
      thread_id = "thread";
      subject = "A";
      recipients = [];
      body = "reply";
      revision = "r1";
      state = "draft";
      path = "a.md";
    }
  in
  let b =
    {
      a with
      source = { a.source with id = "second" };
      subject = "B";
      path = "b.md";
    }
  in
  let t =
    { M.initial with tab = Outbox; saved_drafts = [ a; b ]; selected = 1 }
  in
  let t, _ = M.update t Queue_draft in
  let t =
    fst_update t
      (Finished (pending_id t, Ok (Reply_saved { b with state = "ready" })))
  in
  printf "selected after queue: %s\n"
    (Option.value_exn (M.selected_draft t)).subject;
  let t, _ = M.update t Refresh in
  let t =
    fst_update t
      (Finished (pending_id t, Ok (Workspace_loaded ([], [ b; a ], []))))
  in
  printf "selected after reload: %s\n"
    (Option.value_exn (M.selected_draft t)).subject;
  [%expect {|
    selected after queue: B
    selected after reload: B
    |}]

let%expect_test
    "signature templates seed once, preserve edits, and do not prompt on \
     untouched mail" =
  let m = List.hd_exn Termanil_demo.messages in
  let open_with_signature t signature =
    let t = fst_update t Open in
    fst_update t
      (Finished
         ( pending_id t,
           Ok (Conversation_loaded (m.source, [ (m, "Body") ], Ok signature)) ))
  in
  let t = open_with_signature ready "-- \nAnil" in
  let text = List.Assoc.find_exn t.drafts m.source ~equal:M.same_email in
  printf "template: %S, dirty: %b\n" text (M.draft_dirty t m.source text);
  let t = fst_update t (Draft_changed (m.source, "Written reply\n" ^ text)) in
  let t = open_with_signature (fst_update t Back) "CHANGED SERVER SIGNATURE" in
  printf "reopened: %S\n"
    (List.Assoc.find_exn t.drafts m.source ~equal:M.same_email);
  printf "dirty: %b\n"
    (M.draft_dirty t m.source
       (List.Assoc.find_exn t.drafts m.source ~equal:M.same_email));
  [%expect
    {|
    template: "\n\n-- \nAnil\n", dirty: false
    reopened: "Written reply\n\n\n-- \nAnil\n"
    dirty: true
    |}]

let%expect_test "completing an older task keeps its selection after reordering"
    =
  let demo = Termanil_demo.create () in
  let capture m =
    match demo (M.Capture m) with Ok (Captured [ n ]) -> n | _ -> assert false
  in
  let a = capture (List.nth_exn Termanil_demo.messages 0) in
  let b = capture (List.nth_exn Termanil_demo.messages 1) in
  let t =
    {
      M.initial with
      tab = Todos;
      focus = Detail;
      tasks = [ a; b ];
      selected = 1;
    }
  in
  let t = fst_update t Complete_selected in
  let t =
    fst_update t
      (Finished
         ( pending_id t,
           Ok
             (Completed
                { a with status = "done"; updated = "2026-09-14T10:00:00Z" }) ))
  in
  printf "selected: %s at row %d\n" (Option.value_exn (M.selected_task t)).title
    t.selected;
  [%expect {| selected: Review the garden plan at row 0 |}]
