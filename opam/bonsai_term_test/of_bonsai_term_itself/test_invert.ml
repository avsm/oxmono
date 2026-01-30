open! Core
open Bonsai_test
open Bonsai_term

let app ~dimensions:_ (local_ _graph) =
  Bonsai.return
    (View.hcat
       [ View.text "Normal text and "
       ; View.text ~attrs:[ Attr.invert ] "inverted text"
       ; View.text " back to normal"
       ])
;;

let%expect_test "Not_ansi" =
  let handle =
    Bonsai_term_test.create_handle_without_handler
      ~capability:Not_ansi
      ~initial_dimensions:{ width = 50; height = 1 }
      app
  in
  Handle.show handle;
  [%expect
    {|
    ┌──────────────────────────────────────────────────┐
    │Normal text and inverted text back to normal      │
    └──────────────────────────────────────────────────┘
    |}]
;;

let%expect_test "Ansi" =
  let handle =
    Bonsai_term_test.create_handle_without_handler
      ~capability:Ansi
      ~initial_dimensions:{ width = 50; height = 1 }
      app
  in
  Handle.show handle;
  [%expect
    {| (off)Normal text and (off +invert)inverted text(off) back to normal(off)(EraseLine:ToEnd)(off) |}]
;;
