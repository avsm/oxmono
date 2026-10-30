open! Core
open Bonsai_term
open Bonsai_test
module M = Siftbot_model
module Effect = Bonsai_term.Effect

let area : M.area =
  {
    id = "teaching";
    name = "Teaching";
    definition = "Teaching";
    exclusions = "";
    parent = None;
    status = "proposed";
    revision = 0;
  }

let message : M.message =
  {
    source =
      { service = "https://example.test/session"; account = "a"; id = "m1" };
    thread = "t1";
    date = "2026-01-01";
    sender = "Tutor";
    recipients = [];
    subject = "Timetable";
    original = "Please review the timetable.\027[31m";
    text = "Please review the timetable.";
    headers = [];
    message_ids = [];
    reply_ids = [];
    truncated = false;
    outside_interval = false;
    revision = "1";
  }

let snapshot =
  { M.empty with revision = 1; areas = [ area ]; messages = [ message ] }

let settle h =
  for _ = 1 to 8 do
    Handle.recompute_view h
  done

let event h e =
  Bonsai_term_test.send_event h e;
  settle h

let key h c = event h (Key_press { key = ASCII c; mods = [] })

let handle calls =
  let execute r =
    calls := r :: !calls;
    Ok snapshot
  in
  let h =
    Bonsai_term_test.create_handle
      ~initial_dimensions:{ width = 90; height = 14 }
      (Siftbot_ui.app ?profile_name:None ?configure:None
         ~execute:(Bonsai_term.Effect.of_sync_fun execute) ~exit:(fun () ->
           Bonsai_term.Effect.Ignore))
  in
  settle h;
  calls := [];
  h

let%expect_test "preview requires explanation and explicit commit" =
  let calls = ref [] in
  let h = handle calls in
  key h 'a';
  event h (Key_press { key = Enter; mods = [] });
  print_s [%sexp (List.length !calls : int)];
  key h 'y';
  key h 'e';
  key h 's';
  event h (Key_press { key = Enter; mods = [] });
  print_s [%sexp (!calls : M.request list)];
  [%expect {|
    0
    ((Edit 1 (Approve teaching) yes ""))
  |}]

let%expect_test "paste cannot approve, undo or dispatch a turn" =
  let calls = ref [] in
  let h = handle calls in
  Bonsai_term_test.send_event h (Paste `Start);
  String.iter "auc" ~f:(fun c ->
      Bonsai_term_test.send_event h (Key_press { key = ASCII c; mods = [] }));
  Bonsai_term_test.send_event h (Key_press { key = Enter; mods = [] });
  Bonsai_term_test.send_event h (Paste `End);
  settle h;
  print_s [%sexp (!calls : M.request list)];
  [%expect {| () |}]

let%expect_test "rapid preview and text follow separate frames" =
  let calls = ref [] in
  let h = handle calls in
  String.iter "ayes" ~f:(fun c ->
      Bonsai_term_test.send_event h (Key_press { key = ASCII c; mods = [] }));
  Bonsai_term_test.send_event h (Key_press { key = Enter; mods = [] });
  settle h;
  print_s [%sexp (!calls : M.request list)];
  [%expect {| ((Edit 1 (Approve teaching) yes "")) |}]

let%expect_test "navigation is independent of selection evidence" =
  let calls = ref [] in
  let h = handle calls in
  event h (Key_press { key = Tab; mods = [] });
  key h '3';
  event h (Key_press { key = Escape; mods = [] });
  print_s [%sexp (!calls : M.request list)];
  [%expect {| () |}]

let%expect_test "narrow terminal stays usable" =
  let calls = ref [] in
  let h = handle calls in
  Bonsai_term_test.set_dimensions h { width = 18; height = 5 };
  settle h;
  key h 'r';
  print_s [%sexp (!calls : M.request list)];
  [%expect {| ((Inspect "")) |}]

let%expect_test "profile setup masks tokens and only saves explicitly" =
  let module S = Siftbot_setup in
  let calls = ref [] in
  let selected = ref "" in
  let execute r =
    calls := r :: !calls;
    match r with
    | S.List -> S.Profiles []
    | Save (p, _, _) -> Saved { p with has_token = true }
  in
  let h =
    Bonsai_term_test.create_handle
      ~initial_dimensions:{ width = 100; height = 18 }
      (Siftbot_ui.Settings.app ~default_name:"personal"
         ~execute:(Bonsai_term.Effect.of_sync_fun execute)
         ~select:(Bonsai_term.Effect.of_sync_fun (fun name -> selected := name))
         ~exit:(fun () -> Bonsai_term.Effect.Ignore))
  in
  settle h;
  let tab () = event h (Key_press { key = Tab; mods = [] }) in
  tab ();
  tab ();
  tab ();
  event h (Paste `Start);
  String.iter "secret-token" ~f:(key h);
  event h (Key_press { key = Enter; mods = [] });
  event h (Paste `End);
  print_s [%sexp (List.length !calls : int)];
  Handle.show h;
  [%expect
    {|
    1
    ┌────────────────────────────────────────────────────────────────────────────────────────────────────┐
    │Siftbot | connection profiles                                                                       │
    │  Siftbot profile: personal                                                                         │
    │  Shared JMAP profile: personal                                                                     │
    │  HTTPS session URL:                                                                                │
    │> Bearer access token: ********                                                                     │
    │  Account ID (optional):                                                                            │
    │  Chat model (optional):                                                                            │
    │  [ Save profile ]                                                                                  │
    │Use a read-only mail token. Blank retains an unchanged shared login.                                │
    │                                                                                                    │
    │                                                                                                    │
    │                                                                                                    │
    │                                                                                                    │
    │                                                                                                    │
    │                                                                                                    │
    │                                                                                                    │
    │Settings only. No mail is fetched.                                                                  │
    │Tab/Shift-Tab field | arrows edit | Ctrl-U clear | Esc cancel | Ctrl-C quit                         │
    └────────────────────────────────────────────────────────────────────────────────────────────────────┘
  |}];
  tab ();
  tab ();
  tab ();
  event h (Key_press { key = Enter; mods = [] });
  print_s [%sexp (List.length !calls : int)];
  print_s
    [%sexp
      (match List.hd_exn !calls with
       | S.Save (_, token, fresh) -> String.equal token "secret-token" && fresh
       | _ -> false
        : bool)];
  event h (Key_press { key = Enter; mods = [] });
  print_endline !selected;
  [%expect {|
    2
    true
    personal
  |}]

let%expect_test "editing a profile clears a cancelled token and preserves names"
    =
  let module S = Siftbot_setup in
  let p : S.profile =
    {
      name = "work";
      jmap_profile = "shared-work";
      url = "https://example.test/session";
      account = "account";
      chat_model = "model";
      has_token = true;
    }
  in
  let calls = ref [] in
  let execute r =
    calls := r :: !calls;
    match r with S.List -> S.Profiles [ p ] | Save (p, _, _) -> S.Saved p
  in
  let h =
    Bonsai_term_test.create_handle
      (Siftbot_ui.Settings.app ~default_name:"work"
         ~execute:(Bonsai_term.Effect.of_sync_fun execute)
         ~select:(fun _ -> Bonsai_term.Effect.Ignore)
         ~exit:(fun () -> Bonsai_term.Effect.Ignore))
  in
  settle h;
  key h 'e';
  event h (Key_press { key = Tab; mods = [] });
  event h (Key_press { key = Tab; mods = [] });
  String.iter "discard-token" ~f:(key h);
  event h (Key_press { key = Escape; mods = [] });
  key h 'e';
  for _ = 1 to 5 do
    event h (Key_press { key = Tab; mods = [] })
  done;
  event h (Key_press { key = Enter; mods = [] });
  print_s
    [%sexp
      (match List.hd_exn !calls with
       | S.Save (saved, token, fresh) ->
           Poly.equal saved p && String.is_empty token && not fresh
       | _ -> false
        : bool)];
  [%expect {| true |}]

let%expect_test "profile picker selects the requested profile" =
  let module S = Siftbot_setup in
  let profile name : S.profile =
    {
      name;
      jmap_profile = name;
      url = "https://example.test/session";
      account = "";
      chat_model = "";
      has_token = true;
    }
  in
  let selected = ref "" in
  let h =
    Bonsai_term_test.create_handle
      (Siftbot_ui.Settings.app ~default_name:"work"
         ~execute:
           (Bonsai_term.Effect.of_sync_fun (fun _ ->
                S.Profiles [ profile "personal"; profile "work" ]))
         ~select:(Bonsai_term.Effect.of_sync_fun (fun s -> selected := s))
         ~exit:(fun () -> Bonsai_term.Effect.Ignore))
  in
  settle h;
  event h (Key_press { key = Enter; mods = [] });
  print_endline !selected;
  key h 'k';
  event h (Key_press { key = Enter; mods = [] });
  print_endline !selected;
  [%expect {|
    work
    personal
  |}]

let%expect_test "Ctrl-C exits setup and review while helpers are pending" =
  let waiting = Async.Ivar.create () in
  let exits = ref 0 in
  let exit () = Effect.of_sync_fun (fun () -> Int.incr exits) () in
  let h =
    Bonsai_term_test.create_handle
      (Siftbot_ui.Settings.app ~default_name:"new"
         ~execute:(Effect.of_deferred_fun (fun _ -> Async.Ivar.read waiting))
         ~select:(fun _ -> Effect.Ignore)
         ~exit)
  in
  settle h;
  event h (Key_press { key = ASCII 'C'; mods = [ Ctrl ] });
  event h (Key_press { key = Escape; mods = [] });
  let waiting = Async.Ivar.create () in
  let h =
    Bonsai_term_test.create_handle
      (Siftbot_ui.app ?profile_name:None ?configure:None
         ~execute:(Effect.of_deferred_fun (fun _ -> Async.Ivar.read waiting))
         ~exit)
  in
  settle h;
  event h (Key_press { key = ASCII 'C'; mods = [ Ctrl ] });
  print_s [%sexp (!exits : int)];
  [%expect {| 3 |}]

let%expect_test "Ctrl-C bypasses the paste queue" =
  let exits = ref 0 in
  let h =
    Bonsai_term_test.create_handle
      (Siftbot_ui.Settings.app ~default_name:"new"
         ~execute:(Effect.of_sync_fun (fun _ -> Siftbot_setup.Profiles []))
         ~select:(fun _ -> Effect.Ignore)
         ~exit:(fun () -> Effect.of_sync_fun (fun () -> Int.incr exits) ()))
  in
  settle h;
  Bonsai_term_test.send_event h (Paste `Start);
  for _ = 1 to 1000 do
    Bonsai_term_test.send_event h (Key_press { key = ASCII 'x'; mods = [] })
  done;
  Bonsai_term_test.send_event h (Key_press { key = ASCII 'C'; mods = [ Ctrl ] });
  print_s [%sexp (!exits : int)];
  [%expect {| 1 |}]

let%expect_test "setup edits UTF-8 at the cursor and navigates backwards" =
  let saved = ref None in
  let execute = function
    | Siftbot_setup.List -> Siftbot_setup.Profiles []
    | Save (p, _, _) ->
        saved := Some p;
        Siftbot_setup.Saved p
  in
  let h =
    Bonsai_term_test.create_handle
      (Siftbot_ui.Settings.app ~default_name:"new"
         ~execute:(Effect.of_sync_fun execute)
         ~select:(fun _ -> Effect.Ignore)
         ~exit:(fun () -> Effect.Ignore))
  in
  settle h;
  let press key = event h (Key_press { key; mods = [] }) in
  event h (Key_press { key = ASCII 'U'; mods = [ Ctrl ] });
  String.iter "fresh" ~f:(key h);
  press Tab;
  press Tab;
  String.iter "https://example.test/session" ~f:(key h);
  press Home;
  press (Uchar (Stdlib.Uchar.of_int 0x00e9));
  press Backspace;
  press End;
  press (Arrow `Left);
  press Delete;
  key h 'n';
  press (Arrow `Up);
  press (Arrow `Down);
  event h (Key_press { key = Tab; mods = [ Shift ] });
  for _ = 1 to 5 do
    press Tab
  done;
  press Enter;
  let saved = Option.value_exn !saved in
  print_endline saved.name;
  print_endline saved.url;
  [%expect {|
    fresh
    https://example.test/session
  |}]

let%expect_test "small setup viewport follows the save button" =
  let h =
    Bonsai_term_test.create_handle
      ~initial_dimensions:{ width = 36; height = 5 }
      (Siftbot_ui.Settings.app ~default_name:"new"
         ~execute:(Effect.of_sync_fun (fun _ -> Siftbot_setup.Profiles []))
         ~select:(fun _ -> Effect.Ignore)
         ~exit:(fun () -> Effect.Ignore))
  in
  settle h;
  for _ = 1 to 6 do
    event h (Key_press { key = Tab; mods = [] })
  done;
  Handle.show h;
  [%expect
    {|
    ┌────────────────────────────────────┐
    │Siftbot | connection profiles       │
    │  Chat model:                       │
    │> [ Save profile ]                  │
    │Settings only. No mail is fetched.  │
    │^C quit | Tab next | Esc back       │
    └────────────────────────────────────┘
  |}]

let%expect_test "a long token paste is batched without crossing focus \
                 boundaries" =
  let saved = ref "" in
  let execute = function
    | Siftbot_setup.List -> Siftbot_setup.Profiles []
    | Save (p, token, _) ->
        saved := token;
        Siftbot_setup.Saved p
  in
  let h =
    Bonsai_term_test.create_handle
      (Siftbot_ui.Settings.app ~default_name:"new"
         ~execute:(Effect.of_sync_fun execute)
         ~select:(fun _ -> Effect.Ignore)
         ~exit:(fun () -> Effect.Ignore))
  in
  settle h;
  for _ = 1 to 3 do
    event h (Key_press { key = Tab; mods = [] })
  done;
  Bonsai_term_test.send_event h (Paste `Start);
  for _ = 1 to 1000 do
    Bonsai_term_test.send_event h (Key_press { key = ASCII 'x'; mods = [] })
  done;
  Bonsai_term_test.send_event h (Paste `End);
  for _ = 1 to 3 do
    Bonsai_term_test.send_event h (Key_press { key = Tab; mods = [] })
  done;
  Bonsai_term_test.send_event h (Key_press { key = Enter; mods = [] });
  settle h;
  print_s [%sexp (String.equal !saved (String.make 1000 'x') : bool)];
  [%expect {| true |}]

let%expect_test "message navigation requests the selected body" =
  let second =
    {
      message with
      source = { message.source with id = "m2" };
      original = "Second body";
    }
  in
  let calls = ref [] in
  let execute request =
    calls := request :: !calls;
    let source =
      match request with
      | M.Inspect_message (_, source) -> source
      | _ -> message.source
    in
    Ok
      {
        snapshot with
        messages =
          List.map [ message; second ] ~f:(fun m ->
              if Poly.equal m.M.source source then m
              else { m with original = "" });
      }
  in
  let h =
    Bonsai_term_test.create_handle
      (Siftbot_ui.app ?profile_name:None ?configure:None
         ~execute:(Effect.of_sync_fun execute) ~exit:(fun () -> Effect.Ignore))
  in
  settle h;
  key h 'j';
  key h 'k';
  List.iter (List.rev !calls) ~f:(function
    | M.Inspect _ -> print_endline "list"
    | Inspect_message (_, source) -> print_endline source.id
    | _ -> failwith "unexpected request");
  [%expect {|
    list
    m2
    m1
  |}]

let%expect_test "pending review animates without repeating backend requests" =
  let waiting = Async.Ivar.create () in
  let requests = ref 0 in
  let h =
    Bonsai_term_test.create_handle
      ~initial_dimensions:{ width = 90; height = 24 }
      (Siftbot_ui.app ?profile_name:None ?configure:None
         ~execute:
           (Effect.of_deferred_fun (fun _ ->
                Int.incr requests;
                Async.Ivar.read waiting))
         ~exit:(fun () -> Effect.Ignore))
  in
  settle h;
  let before = Bonsai_term_test.last_view h |> View.Private.notty_image in
  Handle.advance_clock_by h (Time_ns.Span.of_ms 300.);
  settle h;
  let after = Bonsai_term_test.last_view h |> View.Private.notty_image in
  print_s [%sexp (not (Notty.I.equal before after) : bool), (!requests : int)];
  let h = handle (ref []) in
  let ready = Bonsai_term_test.last_view h |> View.Private.notty_image in
  Handle.advance_clock_by h (Time_ns.Span.of_ms 300.);
  settle h;
  let later = Bonsai_term_test.last_view h |> View.Private.notty_image in
  print_s [%sexp (Notty.I.equal ready later : bool), (!requests : int)];
  List.iter
    [ { Dimensions.width = 1; height = 1 }; { width = 30; height = 8 } ]
    ~f:(fun dimensions ->
      Bonsai_term_test.set_dimensions h dimensions;
      settle h;
      assert (View.width (Bonsai_term_test.last_view h) <= dimensions.width);
      assert (View.height (Bonsai_term_test.last_view h) <= dimensions.height));
  [%expect {|
    (true 1)
    (true 1)
  |}]

let%expect_test "message-first layout, explanations and optional topics" =
  let calls = ref [] in
  let messages =
    List.init 8 ~f:(fun i ->
        {
          message with
          source = { message.source with id = Int.to_string i };
          subject = "Review item " ^ Int.to_string i;
        })
  in
  let first = List.hd_exn messages in
  let prediction : M.assignment =
    {
      target = first.source;
      conversation = false;
      dimension = Attention;
      value = "needs-reply";
      origin = Model;
      version = "test";
      source_revision = first.revision;
      explanation = "The sender asks a question.";
      evidence = [ first.source ];
      score = None;
    }
  in
  let h =
    Bonsai_term_test.create_handle
      ~initial_dimensions:{ width = 120; height = 30 }
      (Siftbot_ui.app ?profile_name:None ?configure:None
         ~execute:
           (Effect.of_sync_fun (fun request ->
                calls := request :: !calls;
                Ok { snapshot with messages; assignments = [ prediction ] }))
         ~exit:(fun () -> Effect.Ignore))
  in
  let rendered () =
    let view = Bonsai_term_test.last_view h in
    let buffer = Buffer.create 4096 in
    Notty.Render.to_buffer buffer Notty.Cap.dumb (0, 0)
      (View.width view, View.height view)
      (View.Private.notty_image view);
    Buffer.contents buffer
  in
  let contains needle = String.is_substring (rendered ()) ~substring:needle in
  settle h;
  assert (contains "Review item 7");
  assert (contains "From: Tutor");
  assert (contains "Action: May need a reply");
  assert (not (contains "((service"));
  assert (not (contains "The sender asks"));
  key h 'e';
  assert (contains "Why: The sender asks a question.");
  key h 'g';
  assert (contains "Topics (optional)");
  assert (contains "Teaching");
  key h 'j';
  assert (List.length !calls = 1);
  event h (Key_press { key = Escape; mods = [] });
  assert (contains "From: Tutor");
  key h 'j';
  assert (
    List.exists !calls ~f:(function
      | M.Inspect_message (_, source) -> String.equal source.id "1"
      | _ -> false));
  print_endline
    "Layout, explanations, optional topics and message navigation passed";
  [%expect
    {| Layout, explanations, optional topics and message navigation passed |}]

let%expect_test "thread ordering drives navigation and keeps accounts separate"
    =
  let make id thread date account =
    { message with source = { message.source with id; account }; thread; date }
  in
  let messages =
    [
      make "a-new" "thread-a" "2026-01-30" "one";
      make "b" "thread-b" "2026-01-29" "one";
      make "a-old" "thread-a" "2026-01-28" "one";
      make "other-account" "thread-a" "2026-01-27" "two";
    ]
  in
  let calls = ref [] in
  let h =
    Bonsai_term_test.create_handle
      (Siftbot_ui.app ?profile_name:None ?configure:None
         ~execute:
           (Effect.of_sync_fun (fun request ->
                calls := request :: !calls;
                Ok { snapshot with messages }))
         ~exit:(fun () -> Effect.Ignore))
  in
  settle h;
  key h 't';
  assert (List.length !calls = 1);
  key h 'j';
  key h 'j';
  key h 'j';
  key h 't';
  key h 'k';
  List.iter (List.rev !calls) ~f:(function
    | M.Inspect _ -> print_endline "load"
    | Inspect_message (_, source) -> print_endline source.id
    | _ -> failwith "unexpected mutation");
  [%expect {|
    load
    a-old
    b
    other-account
    a-old
  |}]

let%expect_test "label correction previews sender scope before committing" =
  let calls = ref [] in
  let message = { message with sender = "noreply@zulip.com" } in
  let h =
    Bonsai_term_test.create_handle
      (Siftbot_ui.app ?profile_name:None ?configure:None
         ~execute:
           (Effect.of_sync_fun (fun request ->
                calls := request :: !calls;
                Ok { snapshot with messages = [ message ] }))
         ~exit:(fun () -> Effect.Ignore))
  in
  settle h;
  calls := [];
  key h 'l';
  key h '1';
  key h 's';
  assert (List.is_empty !calls);
  event h (Key_press { key = Enter; mods = [] });
  assert (List.is_empty !calls);
  String.iter "Notification only" ~f:(key h);
  event h (Key_press { key = Arrow `Down; mods = [] });
  assert (List.is_empty !calls);
  event h (Key_press { key = Enter; mods = [] });
  List.iter !calls ~f:(function
    | M.Edit (_, Assign_rule (Sender, a), explanation, _) ->
        print_s
          [%sexp
            (a.dimension : M.dimension),
              (a.value : string),
              (explanation : string)]
    | _ -> failwith "unexpected correction request");
  [%expect {| (Attention fyi "Notification only") |}]
