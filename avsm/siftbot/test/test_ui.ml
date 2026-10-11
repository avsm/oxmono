open! Core
open Bonsai_term
open Bonsai_test
module M = Siftbot_model

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
    │PROFILE SETUP | Tab/Enter next | Ctrl-U clear | Esc cancel                                          │
    │Shared JMAP login changes also affect other apps using that profile.                                │
    │  Siftbot profile: personal                                                                         │
    │  Shared JMAP profile: personal                                                                     │
    │  HTTPS session URL:                                                                                │
    │> Bearer access token: ********                                                                     │
    │  Account ID (blank = primary):                                                                     │
    │  Chat model (optional):                                                                            │
    │  [ Save profile ]                                                                                  │
    │Enter on Save writes settings. No mail is fetched.                                                  │
    │A changed URL requires a new token. Tokens are never displayed.                                     │
    │                                                                                                    │
    │                                                                                                    │
    │                                                                                                    │
    │                                                                                                    │
    │                                                                                                    │
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
