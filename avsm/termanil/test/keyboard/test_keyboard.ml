(* Copyright (c) 2026 Anil Madhavapeddy. SPDX-License-Identifier: ISC *)
open! Core
open Bonsai_term
open Event.Key
open Bonsai_test
module M = Termanil_model

let settle h =
  for _ = 1 to 64 do
    Handle.recompute_view h
  done

let burst h keys =
  List.iter keys ~f:(fun key ->
      Bonsai_term_test.send_event h (Key_press { key; mods = [] }));
  settle h

let linked_tasks () =
  let demo = Termanil_demo.create () in
  List.take Termanil_demo.messages 3
  |> List.rev
  |> List.iter ~f:(fun m -> ignore (demo (M.Capture m)));
  demo

let handle ~width execute =
  Bonsai_term_test.create_handle ~initial_dimensions:{ width; height = 18 }
    (Termanil_ui.app ~initial:M.initial ~autoload:true
       ~execute:(Bonsai_term.Effect.of_sync_fun execute) ~exit:(fun () ->
         Bonsai_term.Effect.Ignore))

let%expect_test "linked task arrows select tasks before opening their source" =
  List.iter [ 72; 110 ] ~f:(fun width ->
      printf "%d columns\n" width;
      let demo = linked_tasks () in
      let h =
        handle ~width (fun r ->
            (match r with
            | M.Conversation source -> printf "read %s\n" source.id
            | _ -> ());
            demo r)
      in
      settle h;
      burst h [ Enter; ASCII 'g'; Arrow `Down; ASCII 'o' ];
      burst h [ Arrow `Up; Arrow `Down ];
      burst h [ ASCII 'g'; Arrow `Up; ASCII 'o' ];
      burst h [ ASCII 'g'; ASCII 'j'; ASCII 'o' ];
      burst h [ ASCII 'g'; ASCII 'k'; ASCII 'o' ]);
  [%expect
    {|
    72 columns
    read email-1
    read email-2
    read email-1
    read email-2
    read email-1
    read email-2
    read email-1
    110 columns
    read email-1
    read email-2
    read email-1
    read email-2
    read email-1
    read email-2
    read email-1
    |}]

let%expect_test "opened contact arrows navigate before selecting another view" =
  let demo = Termanil_demo.create () in
  let h = handle ~width:72 demo in
  settle h;
  burst h [ ASCII '2'; Enter; Arrow `Down ];
  Handle.show h;
  [%expect
    {|
    ┌────────────────────────────────────────────────────────────────────────┐
    │ termanil  1 Mail  2 Contacts  3 Dooit  4 Outbox 0                      │
    │Contacts | 8 results | Tab/Shift-Tab views                              │
    │------------------------------------------------------------------------│
    │ Name:                                                                  │
    │   Grace                                                                │
    │ Email:                                                                 │
    │   grace@example.test                                                   │
    │ Sortal ID:                                                             │
    │   grace                                                                │
    │ UID:                                                                   │
    │   demo-grace                                                           │
    │ Organisation:                                                          │
    │   Library friends                                                      │
    │ Phone (WORK):                                                          │
    │   +44 1632 960001                                                      │
    │ Source:                                                                │
    │Up/Down items | PgUp/Dn scroll | Esc list | ? help | q quit             │
    │8 contacts                                                              │
    └────────────────────────────────────────────────────────────────────────┘
    |}]

let same_view a b =
  Notty.I.equal (View.Private.notty_image a) (View.Private.notty_image b)

let%expect_test
    "task scrolling retains identity and navigation resets the viewport" =
  let demo = linked_tasks () in
  let decorate (n : M.task) =
    if String.equal n.id "task-email-1" then
      {
        n with
        body =
          String.concat ~sep:"\n"
            (List.init 80 ~f:(fun i -> Printf.sprintf "Garden job %02d" i));
      }
    else n
  in
  let execute r =
    (match r with M.Conversation s -> printf "read %s\n" s.id | _ -> ());
    match demo r with
    | Ok (Workspace_loaded (ns, ds, warnings)) ->
        Ok (M.Workspace_loaded (List.map ns ~f:decorate, ds, warnings))
    | other -> other
  in
  let h = handle ~width:110 execute in
  settle h;
  burst h [ Enter; ASCII 'g' ];
  let top = Bonsai_term_test.last_view h in
  burst h [ Page `Down ];
  printf "page scrolls: %b\n"
    (not (same_view top (Bonsai_term_test.last_view h)));
  burst h [ Page `Up ];
  printf "page returns to top: %b\n"
    (same_view top (Bonsai_term_test.last_view h));
  burst h (List.init 20 ~f:(fun _ -> Event.Key.Page `Down));
  let at_end = Bonsai_term_test.last_view h in
  burst h [ Page `Up ];
  printf "reverses at end: %b\n"
    (not (same_view at_end (Bonsai_term_test.last_view h)));
  burst h [ ASCII 'o' ];
  burst h [ ASCII 'g'; Page `Down; Arrow `Down; Arrow `Up ];
  printf "navigation resets scroll: %b\n"
    (same_view top (Bonsai_term_test.last_view h));
  burst h [ Arrow `Down; ASCII 'o' ];
  [%expect
    {|
    read email-1
    page scrolls: true
    page returns to top: true
    reverses at end: true
    read email-1
    navigation resets scroll: true
    read email-2
    |}]

let%expect_test
    "returning to an older linked email navigates from its conversation row" =
  let demo = linked_tasks () in
  let latest = List.hd_exn Termanil_demo.messages in
  let older =
    { latest with source = { latest.source with id = "older-email" } }
  in
  let patch (n : M.task) =
    if String.equal n.id "task-email-1" then
      { n with sources = [ older.source ] }
    else n
  in
  let execute r =
    (match r with M.Conversation s -> printf "read %s\n" s.id | _ -> ());
    match r with
    | M.Conversation s when M.same_email s older.source ->
        Ok
          (M.Conversation_loaded
             ( s,
               [ (older, "Earlier message"); (latest, "Latest message") ],
               Ok "" ))
    | _ -> (
        match demo r with
        | Ok (Workspace_loaded (ns, ds, warnings)) ->
            Ok (M.Workspace_loaded (List.map ns ~f:patch, ds, warnings))
        | other -> other)
  in
  let h = handle ~width:72 execute in
  settle h;
  burst h [ Enter; ASCII 'g'; ASCII 'o'; Arrow `Down; Arrow `Up ];
  [%expect
    {|
    read email-1
    read older-email
    read email-2
    read email-1
    |}]

let%expect_test
    "task links and reply editor keys remain distinct in a single input burst" =
  let demo = linked_tasks () in
  let h =
    handle ~width:72 (fun r ->
        (match r with
        | M.Conversation source -> printf "read %s\n" source.id
        | Save_reply { source; body; _ } ->
            printf "save %s: %S\n" source.id body
        | _ -> ());
        demo r)
  in
  settle h;
  let keys : Event.Key.t list =
    [
      Enter;
      ASCII 'g';
      Arrow `Down;
      ASCII 'o';
      ASCII 'e';
      ASCII 'g';
      ASCII 't';
      Arrow `Left;
      ASCII 'o';
    ]
  in
  List.iter keys ~f:(fun key ->
      Bonsai_term_test.send_event h (Key_press { key; mods = [] }));
  Bonsai_term_test.send_event h (Key_press { key = ASCII 'S'; mods = [ Ctrl ] });
  List.iter [ Escape; ASCII 'g'; Arrow `Up; ASCII 'o' ] ~f:(fun key ->
      Bonsai_term_test.send_event h (Key_press { key; mods = [] }));
  settle h;
  [%expect
    {|
    read email-1
    read email-2
    (cursor (((position ((x 0) (y 12))) (kind Bar_blinking))))
    (cursor (((position ((x 2) (y 12))) (kind Bar_blinking))))
    save email-2: "got"
    (cursor ())
    read email-1
    |}]

let%expect_test
    "task navigation survives narrow-wide resize and a filtered task list" =
  let demo = linked_tasks () in
  let h =
    handle ~width:72 (fun r ->
        (match r with M.Conversation s -> printf "read %s\n" s.id | _ -> ());
        demo r)
  in
  settle h;
  burst h [ Enter; ASCII 'g' ];
  Bonsai_term_test.set_dimensions h { width = 110; height = 18 };
  burst h [ Arrow `Down ];
  Bonsai_term_test.set_dimensions h { width = 72; height = 18 };
  burst h [ Arrow `Up; ASCII 'o' ];
  burst h
    [
      ASCII 'g';
      ASCII '/';
      ASCII 'L';
      ASCII 'u';
      ASCII 'n';
      ASCII 'c';
      ASCII 'h';
      Enter;
      Enter;
      Arrow `Down;
      Arrow `Up;
      ASCII 'o';
    ];
  [%expect {|
    read email-1
    read email-1
    read email-2
    |}]

let%expect_test "Tab and Shift-Tab preserve reply text and cursor across views"
    =
  let demo = linked_tasks () in
  let h =
    handle ~width:72 (fun r ->
        (match r with
        | M.Save_reply { source; body; _ } ->
            printf "save %s: %S\n" source.id body
        | _ -> ());
        demo r)
  in
  settle h;
  burst h
    [ Enter; ASCII 'e'; ASCII 'a'; ASCII 'b'; Arrow `Left; Tab; Arrow `Down ];
  Bonsai_term_test.send_event h (Key_press { key = Tab; mods = [ Shift ] });
  settle h;
  burst h [ ASCII 'X' ];
  Bonsai_term_test.send_event h (Key_press { key = ASCII 's'; mods = [ Ctrl ] });
  settle h;
  burst h [ Escape; Tab; Tab; Enter; Arrow `Down; ASCII 'o' ];
  [%expect
    {|
    (cursor (((position ((x 0) (y 12))) (kind Bar_blinking))))
    (cursor (((position ((x 1) (y 12))) (kind Bar_blinking))))
    (cursor ())
    (cursor (((position ((x 1) (y 12))) (kind Bar_blinking))))
    (cursor (((position ((x 2) (y 12))) (kind Bar_blinking))))
    save email-1: "aXb"
    (cursor ())
    |}]

let%expect_test "reply keystrokes wait for asynchronous signature preparation" =
  let module E = Bonsai_term.Effect in
  let demo = Termanil_demo.create () in
  let response = E.For_testing.Svar.create () in
  let saves = ref [] in
  let execute = function
    | M.Conversation _ -> E.For_testing.of_svar_fun (fun () -> response) ()
    | r ->
        E.of_thunk (fun () ->
            (match r with
            | M.Save_reply { body; _ } -> saves := body :: !saves
            | _ -> ());
            demo r)
  in
  let h =
    Bonsai_term_test.create_handle
      ~initial_dimensions:{ width = 72; height = 18 }
      (Termanil_ui.app ~initial:M.initial ~autoload:true ~execute
         ~exit:(fun () -> E.Ignore))
  in
  settle h;
  burst h [ Enter; ASCII 'e'; ASCII 'H'; ASCII 'i' ];
  Bonsai_term_test.send_event h (Key_press { key = ASCII 's'; mods = [ Ctrl ] });
  settle h;
  printf "saves before response: %d\n" (List.length !saves);
  let m = List.hd_exn Termanil_demo.messages in
  E.For_testing.Svar.fill_if_empty response
    (Ok (M.Conversation_loaded (m.source, [ (m, "Body") ], Ok "-- \nAnil")));
  settle h;
  print_s [%sexp (!saves : string list)];
  [%expect
    {|
    saves before response: 0
    (cursor (((position ((x 0) (y 12))) (kind Bar_blinking))))
    (cursor (((position ((x 2) (y 12))) (kind Bar_blinking))))
    ("Hi\n\n-- \nAnil\n")
    |}]

let%expect_test "tab cycling is global across reader, search, help and review" =
  List.iter
    [
      M.initial;
      { M.initial with focus = Detail };
      { M.initial with focus = Reply };
      { M.initial with input = Some "query" };
      { M.initial with help = true };
      { M.initial with send_review = Some [] };
    ]
    ~f:(fun t ->
      let action mods =
        Termanil_ui.key_action t (Key_press { key = Tab; mods })
      in
      printf "%b %b\n"
        (Poly.equal (action []) (Some (M.Switch People)))
        (Poly.equal (action [ Shift ]) (Some (M.Switch Outbox))));
  let t = { M.initial with pasting = true } in
  printf "pasted tab ignored as navigation: %b\n"
    (Option.is_none
       (Termanil_ui.key_action t (Key_press { key = Tab; mods = [] })));
  [%expect
    {|
    true true
    true true
    true true
    true true
    true true
    true true
    pasted tab ignored as navigation: true
    |}]
