(* Copyright (c) 2026 Anil Madhavapeddy. SPDX-License-Identifier: ISC *)
open! Core
open Bonsai_term
open Bonsai_test
module M = Termanil_model

let handle ?(width = 72) ?(height = 12) ?(execute = Termanil_demo.create ())
    ?(exit = fun () -> Bonsai_term.Effect.Ignore) () =
  Bonsai_term_test.create_handle ~initial_dimensions:{ width; height }
    (Termanil_ui.app ~initial:M.initial ~autoload:true
       ~execute:(Bonsai_term.Effect.of_sync_fun execute)
       ~exit)

let settle h =
  for _ = 1 to 6 do
    Handle.recompute_view h
  done

let key h key =
  settle h;
  Bonsai_term_test.send_event h (Key_press { key; mods = [] });
  settle h

let press h c = key h (ASCII c)

let%expect_test "bracketed paste outside a search cannot run commands" =
  let calls = ref [] in
  let demo = Termanil_demo.create () in
  let h =
    handle
      ~execute:(fun r ->
        calls := M.request_name r :: !calls;
        demo r)
      ()
  in
  settle h;
  calls := [];
  Bonsai_term_test.send_event h (Paste `Start);
  settle h;
  String.iter "t3dS" ~f:(press h);
  key h Enter;
  Bonsai_term_test.send_event h (Paste `End);
  settle h;
  print_s [%sexp (!calls : string list)];
  [%expect {| () |}]

let%expect_test "mail list, read view, search and resize" =
  let h = handle () in
  settle h;
  Handle.show h;
  [%expect
    {|
    ┌────────────────────────────────────────────────────────────────────────┐
    │ termanil  1 Mail  2 Contacts  3 Dooit  4 Outbox 0                      │
    │Mail | 6 results | Inbox | Tab/Shift-Tab views                          │
    │------------------------------------------------------------------------│
    │> N  Review the garden plan | Ada <ada@example.test>                    │
    │   * Lunch on Friday | Grace <grace@example.test>                       │
    │  N* Notes from the design review | Lin <lin@example.test>              │
    │     Train tickets and arrival times | Samira <samira@example.test>     │
    │  N  Reading group: next chapter | Theo <theo@example.test>             │
    │     Re: Community workshop checklist | Morgan <morgan@example.test>    │
    │                                                                        │
    │j/k move  Enter open  / search  r refresh  ? help  q quit               │
    │0 tasks | 0 saved replies                                               │
    └────────────────────────────────────────────────────────────────────────┘
    |}];
  key h Enter;
  Handle.show h;
  [%expect
    {|
    ┌────────────────────────────────────────────────────────────────────────┐
    │ termanil  1 Mail  2 Contacts  3 Dooit  4 Outbox 0                      │
    │Mail | 6 results | Inbox | Tab/Shift-Tab views                          │
    │------------------------------------------------------------------------│
    │ Task: none | t create                                                  │
    │ Review the garden plan                                                 │
    │ From: Ada <ada@example.test>                                           │
    │ To: You <you@example.test>                                             │
    │  Reply (e to edit) | unsaved                                           │
    │                                                                        │
    │                                                                        │
    │e reply | t/g task | a archive | h history | Q queue | S send | Esc     │
    │e reply | t task | g linked task | h history | a archive | u read | f st│
    └────────────────────────────────────────────────────────────────────────┘
    |}];
  key h Escape;
  press h '/';
  String.iter "lunch" ~f:(press h);
  key h Enter;
  Handle.show h;
  [%expect
    {|
    ┌────────────────────────────────────────────────────────────────────────┐
    │ termanil  1 Mail  2 Contacts  3 Dooit  4 Outbox 0                      │
    │Mail | 1 results | /lunch | Inbox | Tab/Shift-Tab views                 │
    │------------------------------------------------------------------------│
    │>  * Lunch on Friday | Grace <grace@example.test>                       │
    │                                                                        │
    │                                                                        │
    │                                                                        │
    │                                                                        │
    │                                                                        │
    │                                                                        │
    │j/k move  Enter open  / search  r refresh  ? help  q quit               │
    │0 tasks | 0 saved replies                                               │
    └────────────────────────────────────────────────────────────────────────┘
    |}];
  Bonsai_term_test.set_dimensions h { width = 24; height = 5 };
  Handle.show h;
  [%expect
    {|
    ┌────────────────────────┐
    │termanil                │
    │Resize to at least 30 x │
    │q quit | ? help         │
    │                        │
    └────────────────────────┘
    |}]

let%expect_test "capture twice, complete, reopen the source" =
  let backend = Termanil_demo.create () in
  let calls = ref [] in
  let execute r =
    calls := M.request_name r :: !calls;
    backend r
  in
  let h = handle ~execute () in
  settle h;
  press h 't';
  press h 't';
  press h '3';
  Handle.show h;
  [%expect
    {|
    ┌────────────────────────────────────────────────────────────────────────┐
    │ termanil  1 Mail  2 Contacts  3 Dooit  4 Outbox 0                      │
    │Dooit | 1 results | newest updated first | Tab/Shift-Tab views          │
    │------------------------------------------------------------------------│
    │ Review the garden plan                                                 │
    │ Status: open                                                           │
    │ Updated: 2026-09-13T10:00:01Z                                          │
    │ o OPEN LINKED EMAIL | d complete task                                  │
    │ Tags: inbox                                                            │
    │ Email: Review the garden plan                                          │
    │                                                                        │
    │Up/Down tasks | PgUp/Dn scroll | o email | d done | s sync | Esc        │
    │o jumps back to email | d completes task                                │
    └────────────────────────────────────────────────────────────────────────┘
    |}];
  press h 'd';
  press h 'o';
  Handle.show h;
  [%expect
    {|
    ┌────────────────────────────────────────────────────────────────────────┐
    │ termanil  1 Mail  2 Contacts  3 Dooit  4 Outbox 0                      │
    │Mail | 6 results | Inbox | Tab/Shift-Tab views                          │
    │------------------------------------------------------------------------│
    │ Task [done]: Review the garden plan | g open                           │
    │ Review the garden plan                                                 │
    │ From: Ada <ada@example.test>                                           │
    │ To: You <you@example.test>                                             │
    │  Reply (e to edit) | unsaved                                           │
    │                                                                        │
    │                                                                        │
    │e reply | t/g task | a archive | h history | Q queue | S send | Esc     │
    │e reply | t task | g linked task | h history | a archive | u read | f st│
    └────────────────────────────────────────────────────────────────────────┘
    |}];
  print_s [%sexp (List.rev !calls : string list)];
  [%expect
    {|
    ("Loading mailboxes"
     "Loading messages"
     "Loading linked tasks and saved replies"
     "Capturing task"
     "Completing task"
     "Loading conversation")
    |}]

let%expect_test "sender contacts and original metadata" =
  let h = handle () in
  settle h;
  press h 'p';
  key h Enter;
  Handle.show h;
  [%expect
    {|
    ┌────────────────────────────────────────────────────────────────────────┐
    │ termanil  1 Mail  2 Contacts  3 Dooit  4 Outbox 0                      │
    │Contacts | 1 results | /ada@example.test | Tab/Shift-Tab views          │
    │------------------------------------------------------------------------│
    │ Name:                                                                  │
    │   Ada                                                                  │
    │ Email:                                                                 │
    │   ada@example.test                                                     │
    │ Sortal ID:                                                             │
    │   ada                                                                  │
    │ UID:                                                                   │
    │Up/Down items | PgUp/Dn scroll | Esc list | ? help | q quit             │
    │8 contacts                                                              │
    └────────────────────────────────────────────────────────────────────────┘
    |}]

let%expect_test "sync requires confirmation and search text cannot mutate" =
  let calls = ref [] in
  let backend = Termanil_demo.create () in
  let execute r =
    calls := M.request_name r :: !calls;
    backend r
  in
  let h = handle ~execute () in
  settle h;
  press h '3';
  press h '/';
  String.iter "Sduts" ~f:(press h);
  key h Escape;
  press h 'S';
  Handle.show h;
  [%expect
    {|
    ┌────────────────────────────────────────────────────────────────────────┐
    │ termanil  1 Mail  2 Contacts  3 Dooit  4 Outbox 0                      │
    │Dooit | 0 results | newest updated first | Tab/Shift-Tab views          │
    │------------------------------------------------------------------------│
    │  No results. r refresh | / search                                      │
    │                                                                        │
    │                                                                        │
    │                                                                        │
    │                                                                        │
    │                                                                        │
    │                                                                        │
    │Enter: sync Dooit to WebDAV   Esc: cancel                               │
    │Sync Dooit with configured WebDAV? Enter confirms; Esc cancels          │
    └────────────────────────────────────────────────────────────────────────┘
    |}];
  key h Escape;
  press h 's';
  Handle.show h;
  [%expect
    {|
    ┌────────────────────────────────────────────────────────────────────────┐
    │ termanil  1 Mail  2 Contacts  3 Dooit  4 Outbox 0                      │
    │Operation report                                                        │
    │------------------------------------------------------------------------│
    │ Dry run: no writes                                                     │
    │ 0 tasks unchanged                                                      │
    │                                                                        │
    │                                                                        │
    │                                                                        │
    │                                                                        │
    │                                                                        │
    │REPORT | PgUp/Dn scroll  Esc back                                       │
    │Sync result | Esc back | r refresh tasks                                │
    └────────────────────────────────────────────────────────────────────────┘
    |}];
  key h Escape;
  press h 'S';
  key h Enter;
  print_s [%sexp (List.rev !calls : string list)];
  [%expect
    {|
    ("Loading mailboxes"
     "Loading messages"
     "Loading linked tasks and saved replies"
     "Loading tasks"
     "Previewing task sync"
     "Syncing tasks")
    |}]

let%expect_test "failure stays visible and refresh retries" =
  let h = handle ~execute:(fun _ -> Error "offline") () in
  settle h;
  Handle.show h;
  [%expect
    {|
    ┌────────────────────────────────────────────────────────────────────────┐
    │ termanil  1 Mail  2 Contacts  3 Dooit  4 Outbox 0                      │
    │Mail | 0 results | All mail | Tab/Shift-Tab views                       │
    │------------------------------------------------------------------------│
    │  No results. r refresh | / search                                      │
    │                                                                        │
    │                                                                        │
    │                                                                        │
    │                                                                        │
    │                                                                        │
    │                                                                        │
    │j/k move  Enter open  / search  r refresh  ? help  q quit               │
    │Error: offline                                                          │
    └────────────────────────────────────────────────────────────────────────┘
    |}];
  press h '2';
  Handle.show h;
  [%expect
    {|
    ┌────────────────────────────────────────────────────────────────────────┐
    │ termanil  1 Mail  2 Contacts  3 Dooit  4 Outbox 0                      │
    │Contacts | 0 results | Tab/Shift-Tab views                              │
    │------------------------------------------------------------------------│
    │  No results. r refresh | / search                                      │
    │                                                                        │
    │                                                                        │
    │                                                                        │
    │                                                                        │
    │                                                                        │
    │                                                                        │
    │j/k move  Enter open  / search  r refresh  ? help  q quit               │
    │Error: offline                                                          │
    └────────────────────────────────────────────────────────────────────────┘
    |}]

let%expect_test "split view dimensions and unicode wrapping" =
  let h = handle ~width:104 ~height:9 () in
  settle h;
  Handle.show h;
  [%expect
    {|
    ┌────────────────────────────────────────────────────────────────────────────────────────────────────────┐
    │ termanil  1 Mail  2 Contacts  3 Dooit  4 Outbox 0                                                      │
    │Mail | 6 results | Inbox | Tab/Shift-Tab views                                                          │
    │--------------------------------------------------------------------------------------------------------│
    │> N  Review the garden plan | Ada <ada@example.test>| Task: none | t create                             │
    │   * Lunch on Friday | Grace <grace@example.test>   | Review the garden plan                            │
    │  N* Notes from the design review | Lin <lin@example| From: Ada <ada@example.test>                      │
    │     Train tickets and arrival times | Samira <samir| To: You <you@example.test>                        │
    │j/k move  Enter open  / search  r refresh  ? help  q quit                                               │
    │0 tasks | 0 saved replies                                                                               │
    └────────────────────────────────────────────────────────────────────────────────────────────────────────┘
    |}];
  print_s [%sexp (Termanil_ui.wrap ~width:4 "A界BC\ncafé\t!" : string list)];
  [%expect {| ("A\231\149\140B" C "cafe\204\129" "    " !) |}]

let%expect_test
    "Enter then arrows reads neighbouring messages and Esc restores list" =
  let reads = ref [] in
  let demo = Termanil_demo.create () in
  let h =
    handle
      ~execute:(fun req ->
        (match req with M.Read s -> reads := s.id :: !reads | _ -> ());
        demo req)
      ()
  in
  key h Enter;
  key h (Arrow `Down);
  key h (Arrow `Up);
  key h (Page `Down);
  key h Escape;
  key h (Arrow `Down);
  key h Enter;
  print_s [%sexp (List.rev !reads : string list)];
  [%expect {| () |}]

let%expect_test
    "reply editor has multiline cursor movement and per-message drafts" =
  let h = handle ~height:18 () in
  key h Enter;
  press h 'e';
  String.iter "one" ~f:(press h);
  key h Enter;
  String.iter "TWO" ~f:(press h);
  key h (Arrow `Up);
  press h '!';
  key h (Arrow `Down);
  press h '?';
  key h Escape;
  key h (Arrow `Down);
  press h 'e';
  String.iter "other" ~f:(press h);
  key h Escape;
  key h (Arrow `Up);
  press h 'e';
  Handle.show h;
  [%expect
    {|
    (cursor (((position ((x 0) (y 12))) (kind Bar_blinking))))
    (cursor (((position ((x 1) (y 12))) (kind Bar_blinking))))
    (cursor (((position ((x 2) (y 12))) (kind Bar_blinking))))
    (cursor (((position ((x 3) (y 12))) (kind Bar_blinking))))
    (cursor (((position ((x 0) (y 13))) (kind Bar_blinking))))
    (cursor (((position ((x 1) (y 13))) (kind Bar_blinking))))
    (cursor (((position ((x 2) (y 13))) (kind Bar_blinking))))
    (cursor (((position ((x 3) (y 13))) (kind Bar_blinking))))
    (cursor (((position ((x 3) (y 12))) (kind Bar_blinking))))
    (cursor (((position ((x 4) (y 12))) (kind Bar_blinking))))
    (cursor (((position ((x 3) (y 13))) (kind Bar_blinking))))
    (cursor (((position ((x 4) (y 13))) (kind Bar_blinking))))
    (cursor ())
    (cursor (((position ((x 0) (y 12))) (kind Bar_blinking))))
    (cursor (((position ((x 1) (y 12))) (kind Bar_blinking))))
    (cursor (((position ((x 2) (y 12))) (kind Bar_blinking))))
    (cursor (((position ((x 3) (y 12))) (kind Bar_blinking))))
    (cursor (((position ((x 4) (y 12))) (kind Bar_blinking))))
    (cursor (((position ((x 5) (y 12))) (kind Bar_blinking))))
    (cursor ())
    (cursor (((position ((x 4) (y 13))) (kind Bar_blinking))))
    ┌────────────────────────────────────────────────────────────────────────┐
    │ termanil  1 Mail  2 Contacts  3 Dooit  4 Outbox 0                      │
    │Mail | 6 results | Inbox | Tab/Shift-Tab views                          │
    │------------------------------------------------------------------------│
    │ Task: none | t create                                                  │
    │ Review the garden plan                                                 │
    │ From: Ada <ada@example.test>                                           │
    │ To: You <you@example.test>                                             │
    │                                                                        │
    │ Hi,                                                                    │
    │                                                                        │
    │ Could you review the planting notes before Thursday?                   │
    │> Reply | unsaved                                                       │
    │one!                                                                    │
    │TWO?                                                                    │
    │                                                                        │
    │                                                                        │
    │REPLY | Ctrl-S save  Enter newline  Esc reader | Tab next view          │
    │e reply | t task | g linked task | h history | a archive | u read | f st│
    └────────────────────────────────────────────────────────────────────────┘
    |}];
  Bonsai_term_test.set_dimensions h { width = 104; height = 16 };
  settle h;
  Handle.show h;
  [%expect
    {|
    (cursor (((position ((x 57) (y 12))) (kind Bar_blinking))))
    ┌────────────────────────────────────────────────────────────────────────────────────────────────────────┐
    │ termanil  1 Mail  2 Contacts  3 Dooit  4 Outbox 0                                                      │
    │Mail | 6 results | Inbox | Tab/Shift-Tab views                                                          │
    │--------------------------------------------------------------------------------------------------------│
    │> N  Review the garden plan | Ada <ada@example.test>| Task: none | t create                             │
    │   * Lunch on Friday | Grace <grace@example.test>   | Review the garden plan                            │
    │  N* Notes from the design review | Lin <lin@example| From: Ada <ada@example.test>                      │
    │     Train tickets and arrival times | Samira <samir| To: You <you@example.test>                        │
    │  N  Reading group: next chapter | Theo <theo@exampl|                                                   │
    │     Re: Community workshop checklist | Morgan <morg| Hi,                                               │
    │                                                    |                                                   │
    │                                                    |> Reply | unsaved                                  │
    │                                                    |one!                                               │
    │                                                    |TWO?                                               │
    │                                                    |                                                   │
    │REPLY | Ctrl-S save  Enter newline  Esc reader | Tab next view                                          │
    │e reply | t task | g linked task | h history | a archive | u read | f star                              │
    └────────────────────────────────────────────────────────────────────────────────────────────────────────┘
    |}]

let%expect_test
    "reply paste inserts command keys without requests or navigation" =
  let calls = ref [] in
  let demo = Termanil_demo.create () in
  let h =
    handle ~height:18
      ~execute:(fun req ->
        calls := M.request_name req :: !calls;
        demo req)
      ()
  in
  key h Enter;
  press h 'e';
  calls := [];
  Bonsai_term_test.send_event h (Paste `Start);
  settle h;
  String.iter "t3dS" ~f:(press h);
  key h Enter;
  press h 'q';
  Bonsai_term_test.send_event h (Paste `End);
  settle h;
  print_s [%sexp (!calls : string list)];
  Handle.show h;
  [%expect
    {|
    (cursor (((position ((x 0) (y 12))) (kind Bar_blinking))))
    (cursor (((position ((x 1) (y 13))) (kind Bar_blinking))))
    ()
    ┌────────────────────────────────────────────────────────────────────────┐
    │ termanil  1 Mail  2 Contacts  3 Dooit  4 Outbox 0                      │
    │Mail | 6 results | Inbox | Tab/Shift-Tab views                          │
    │------------------------------------------------------------------------│
    │ Task: none | t create                                                  │
    │ Review the garden plan                                                 │
    │ From: Ada <ada@example.test>                                           │
    │ To: You <you@example.test>                                             │
    │                                                                        │
    │ Hi,                                                                    │
    │                                                                        │
    │ Could you review the planting notes before Thursday?                   │
    │> Reply | unsaved                                                       │
    │t3dS                                                                    │
    │q                                                                       │
    │                                                                        │
    │                                                                        │
    │REPLY | Ctrl-S save  Enter newline  Esc reader | Tab next view          │
    │e reply | t task | g linked task | h history | a archive | u read | f st│
    └────────────────────────────────────────────────────────────────────────┘
    |}]

let%expect_test "quitting with a reply requires deliberate discard" =
  let exits = ref 0 in
  let h =
    handle
      ~exit:(fun () -> Bonsai_term.Effect.of_sync_fun (fun () -> incr exits) ())
      ()
  in
  key h Enter;
  press h 'e';
  press h 'q';
  Bonsai_term_test.send_event h (Key_press { key = ASCII 'c'; mods = [ Ctrl ] });
  settle h;
  printf "before discard: %d\n" !exits;
  key h Escape;
  press h 'q';
  printf "after cancel: %d\n" !exits;
  press h 'q';
  printf "after confirmation: %d\n" !exits;
  [%expect
    {|
    (cursor (((position ((x 0) (y 8))) (kind Bar_blinking))))
    (cursor (((position ((x 1) (y 8))) (kind Bar_blinking))))
    (cursor ())
    before discard: 0
    after cancel: 0
    after confirmation: 1
    |}]

let%expect_test "scrolling reverses immediately after reaching the end" =
  let h = handle ~height:18 () in
  let same a b =
    Notty.I.equal (View.Private.notty_image a) (View.Private.notty_image b)
  in
  key h Enter;
  for _ = 1 to 20 do
    key h (Page `Down)
  done;
  let at_end = Bonsai_term_test.last_view h in
  key h (Page `Up);
  printf "mail scroll reversed: %b\n"
    (not (same at_end (Bonsai_term_test.last_view h)));
  press h '2';
  key h Enter;
  for _ = 1 to 100 do
    key h (Page `Down)
  done;
  let at_end = Bonsai_term_test.last_view h in
  key h (Page `Up);
  printf "metadata scroll reversed: %b\n"
    (not (same at_end (Bonsai_term_test.last_view h)));
  [%expect
    {|
    mail scroll reversed: true
    metadata scroll reversed: true
    |}]

let%expect_test "a burst crosses focus boundaries before dispatching text" =
  let calls = ref [] in
  let demo = Termanil_demo.create () in
  let h =
    handle ~height:18
      ~execute:(fun req ->
        calls := M.request_name req :: !calls;
        demo req)
      ()
  in
  settle h;
  calls := [];
  let events : Event.Key.t list =
    [
      Enter;
      ASCII 'e';
      ASCII 'H';
      ASCII 'i';
      Enter;
      ASCII 'q';
      ASCII '3';
      ASCII 't';
      Escape;
    ]
  in
  List.iter events ~f:(fun key ->
      Bonsai_term_test.send_event h (Key_press { key; mods = [] }));
  for _ = 1 to 20 do
    Handle.recompute_view h
  done;
  print_s [%sexp (List.rev !calls : string list)];
  Handle.show h;
  [%expect
    {|
    (cursor (((position ((x 0) (y 12))) (kind Bar_blinking))))
    (cursor (((position ((x 3) (y 13))) (kind Bar_blinking))))
    (cursor ())
    ("Loading conversation")
    ┌────────────────────────────────────────────────────────────────────────┐
    │ termanil  1 Mail  2 Contacts  3 Dooit  4 Outbox 0                      │
    │Mail | 6 results | Inbox | Tab/Shift-Tab views                          │
    │------------------------------------------------------------------------│
    │ Task: none | t create                                                  │
    │ Review the garden plan                                                 │
    │ From: Ada <ada@example.test>                                           │
    │ To: You <you@example.test>                                             │
    │                                                                        │
    │ Hi,                                                                    │
    │                                                                        │
    │ Could you review the planting notes before Thursday?                   │
    │                                                                        │
    │ The plan has three parts:                                              │
    │  Reply (e to edit) | unsaved                                           │
    │Hi                                                                      │
    │q3t                                                                     │
    │e reply | t/g task | a archive | h history | Q queue | S send | Esc     │
    │e reply | t task | g linked task | h history | a archive | u read | f st│
    └────────────────────────────────────────────────────────────────────────┘
    |}]

let%expect_test
    "typing and saving in one burst, queue review, and saved-draft quit" =
  let demo = Termanil_demo.create () in
  let saves = ref [] and sends = ref 0 and exits = ref 0 in
  let h =
    handle ~width:100 ~height:24
      ~exit:(fun () -> Bonsai_term.Effect.of_sync_fun (fun () -> incr exits) ())
      ~execute:(fun r ->
        (match r with
        | M.Save_reply { body; _ } -> saves := body :: !saves
        | Send_replies _ -> incr sends
        | _ -> ());
        demo r)
      ()
  in
  key h Enter;
  press h 'e';
  List.iter
    [
      Event.Key_press { key = ASCII 'Y'; mods = [] };
      Key_press { key = ASCII 'e'; mods = [] };
      Key_press { key = ASCII 's'; mods = [] };
      Key_press { key = ASCII 'S'; mods = [ Ctrl ] };
    ]
    ~f:(Bonsai_term_test.send_event h);
  for _ = 1 to 20 do
    Handle.recompute_view h
  done;
  print_s [%sexp (!saves : string list)];
  key h Escape;
  press h 'Q';
  press h '4';
  press h 'S';
  printf "sends before confirmation: %d\n" !sends;
  key h Escape;
  printf "sends after cancel: %d\n" !sends;
  press h 'S';
  key h Enter;
  printf "sends after confirmation: %d\n" !sends;
  key h Escape;
  press h 'q';
  printf "saved reply quit: %d\n" !exits;
  [%expect
    {|
    (cursor (((position ((x 51) (y 16))) (kind Bar_blinking))))
    (cursor (((position ((x 54) (y 16))) (kind Bar_blinking))))
    (Yes)
    (cursor ())
    sends before confirmation: 0
    sends after cancel: 0
    sends after confirmation: 1
    saved reply quit: 1
    |}]

let%expect_test
    "mail badges, bidirectional task navigation and collapsed history" =
  let demo = Termanil_demo.create () in
  let source = (List.hd_exn Termanil_demo.messages).source in
  let execute = function
    | M.Conversation s when M.same_email s source ->
        let m = List.hd_exn Termanil_demo.messages in
        let older =
          {
            m with
            source = { source with id = "older" };
            received = "2026-09-01";
            sender = "River";
            seen = true;
          }
        in
        Ok
          (M.Conversation_loaded
             ( source,
               [
                 (older, "Older planting discussion.");
                 (m, "Could you review the planting notes?");
               ],
               Ok "" ))
    | r -> demo r
  in
  let h = handle ~width:100 ~height:20 ~execute () in
  key h Enter;
  press h 't';
  Handle.show h;
  [%expect
    {|
    ┌────────────────────────────────────────────────────────────────────────────────────────────────────┐
    │ termanil  1 Mail  2 Contacts  3 Dooit  4 Outbox 0                                                  │
    │Mail | 6 results | Inbox | Tab/Shift-Tab views                                                      │
    │----------------------------------------------------------------------------------------------------│
    │> N  [task] Review the garden plan | Ada <ada@exam| Task [open]: Review the garden plan | g open    │
    │   * Lunch on Friday | Grace <grace@example.test> | Conversation: 2 messages | h expand history     │
    │  N* Notes from the design review | Lin <lin@examp| > 2026-09-01 River                              │
    │     Train tickets and arrival times | Samira <sam| Review the garden plan                          │
    │  N  Reading group: next chapter | Theo <theo@exam| From: Ada <ada@example.test>                    │
    │     Re: Community workshop checklist | Morgan <mo| To: You <you@example.test>                      │
    │                                                  |                                                 │
    │                                                  | Could you review the planting notes?            │
    │                                                  |                                                 │
    │                                                  |                                                 │
    │                                                  |                                                 │
    │                                                  |                                                 │
    │                                                  |  Reply (e to edit) | unsaved                    │
    │                                                  |                                                 │
    │                                                  |                                                 │
    │e reply | t/g task | a archive | h history | Q queue | S send | Esc                                 │
    │Task: Review the garden plan [open]                                                                 │
    └────────────────────────────────────────────────────────────────────────────────────────────────────┘
    |}];
  press h 'g';
  Handle.show h;
  [%expect
    {|
    ┌────────────────────────────────────────────────────────────────────────────────────────────────────┐
    │ termanil  1 Mail  2 Contacts  3 Dooit  4 Outbox 0                                                  │
    │Dooit | 1 results | newest updated first | Tab/Shift-Tab views                                      │
    │----------------------------------------------------------------------------------------------------│
    │> [open] Review the garden plan #inbox            | Review the garden plan                          │
    │                                                  | Status: open                                    │
    │                                                  | Updated: 2026-09-13T10:00:01Z                   │
    │                                                  | o OPEN LINKED EMAIL | d complete task           │
    │                                                  | Tags: inbox                                     │
    │                                                  | Email: Review the garden plan                   │
    │                                                  |                                                 │
    │                                                  |                                                 │
    │                                                  |                                                 │
    │                                                  |                                                 │
    │                                                  |                                                 │
    │                                                  |                                                 │
    │                                                  |                                                 │
    │                                                  |                                                 │
    │                                                  |                                                 │
    │Up/Down tasks | PgUp/Dn scroll | o email | d done | s sync | Esc                                    │
    │o jumps back to email | d completes task                                                            │
    └────────────────────────────────────────────────────────────────────────────────────────────────────┘
    |}];
  press h 'o';
  press h 'h';
  Handle.show h;
  [%expect
    {|
    ┌────────────────────────────────────────────────────────────────────────────────────────────────────┐
    │ termanil  1 Mail  2 Contacts  3 Dooit  4 Outbox 0                                                  │
    │Mail | 6 results | Inbox | Tab/Shift-Tab views                                                      │
    │----------------------------------------------------------------------------------------------------│
    │> N  [task] Review the garden plan | Ada <ada@exam| Task [open]: Review the garden plan | g open    │
    │   * Lunch on Friday | Grace <grace@example.test> | Conversation: 2 messages | h collapse history   │
    │  N* Notes from the design review | Lin <lin@examp| v 2026-09-01 River                              │
    │     Train tickets and arrival times | Samira <sam| Older planting discussion.                      │
    │  N  Reading group: next chapter | Theo <theo@exam|                                                 │
    │     Re: Community workshop checklist | Morgan <mo| Review the garden plan                          │
    │                                                  | From: Ada <ada@example.test>                    │
    │                                                  | To: You <you@example.test>                      │
    │                                                  |                                                 │
    │                                                  | Could you review the planting notes?            │
    │                                                  |                                                 │
    │                                                  |                                                 │
    │                                                  |  Reply (e to edit) | unsaved                    │
    │                                                  |                                                 │
    │                                                  |                                                 │
    │e reply | t/g task | a archive | h history | Q queue | S send | Esc                                 │
    │e reply | t task | g linked task | h history | a archive | u read | f star                          │
    └────────────────────────────────────────────────────────────────────────────────────────────────────┘
    |}]

let%expect_test "send reports cannot activate an invisible reply editor" =
  let source = (List.hd_exn Termanil_demo.messages).source in
  let t =
    {
      M.initial with
      tab = Mail;
      focus = Detail;
      opened = Some source;
      page = { M.initial.page with messages = Termanil_demo.messages };
      report = Some [ "Accepted" ];
    }
  in
  let tab = Termanil_ui.key_action t (Key_press { key = Tab; mods = [] }) in
  printf "tab changes view: %b\n" (Poly.equal tab (Some (M.Switch People)));
  let t, request = M.update t (Move 1) in
  printf "arrows scroll report: %b\n" (t.scroll = 1 && Option.is_none request);
  [%expect {|
    tab changes view: true
    arrows scroll report: true
    |}]
