open! Core
open Bonsai_test
open Bonsai_term

let%expect_test "eprint_s effect can be created and used in handler" =
  let value = 42 in
  let handle =
    Bonsai_term_test.create_handle (fun ~dimensions:_ _graph ->
      let view = Bonsai.return (View.text "hello") in
      let handler =
        Bonsai.return (fun (event : Event.t) ->
          let%bind.Effect () =
            Effect.eprint_s [%message "test message" (value : int) (event : Event.t)]
          in
          Effect.Ignore)
      in
      ~view, ~handler)
  in
  Bonsai_term_test.set_dimensions handle { width = 10; height = 1 };
  Handle.show handle;
  [%expect
    {|
    ┌──────────┐
    │hello     │
    └──────────┘
    |}];
  (* Send an event to trigger the handler which uses eprint_s *)
  Bonsai_term_test.send_event handle (Key_press { key = ASCII 'a'; mods = [] });
  Handle.show handle;
  [%expect
    {|
    ("test message" (value 42) (event (Key_press (key (ASCII a)))))
    ┌──────────┐
    │hello     │
    └──────────┘
    |}]
;;
