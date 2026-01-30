open! Core
open Bonsai_test
open Bonsai_term
open Bonsai.Let_syntax

(* Tiny sanity check test for [Bonsai_term.stitch]. *)

let component ~dimensions:_ (local_ graph) =
  let count, inject =
    Bonsai.state_machine
      ~default_model:0
      ~apply_action:(fun _ctx count (_event : Event.t) -> count + 1)
      graph
  in
  let view =
    let%arr count in
    View.text (Int.to_string count)
  in
  Bonsai_term.unstitch (Bonsai_term.stitch (~view, ~handler:inject))
;;

let%expect_test "stitch/unstitch round-trip" =
  let handle = Bonsai_term_test.create_handle component in
  Bonsai_term_test.set_dimensions handle { width = 10; height = 1 };
  Handle.show handle;
  [%expect
    {|
    ┌──────────┐
    │0         │
    └──────────┘
    |}];
  Bonsai_term_test.send_event handle (Key_press { key = ASCII 'a'; mods = [] });
  Handle.show handle;
  [%expect
    {|
    ┌──────────┐
    │1         │
    └──────────┘
    |}]
;;
