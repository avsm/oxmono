open! Core
open Bonsai_test
open Bonsai_term

let run_test ?(dimensions = { Dimensions.width = 50; height = 10 }) view =
  let handle =
    Bonsai_term_test.create_handle_without_handler (fun ~dimensions:_ _ ->
      Bonsai.return view)
  in
  Bonsai_term_test.set_dimensions handle dimensions;
  Handle.show handle
;;

let%expect_test "box" =
  run_test (View.rectangle ~fill:'x' ~width:5 ~height:5 ());
  [%expect
    {|
    ┌──────────────────────────────────────────────────┐
    │xxxxx                                             │
    │xxxxx                                             │
    │xxxxx                                             │
    │xxxxx                                             │
    │xxxxx                                             │
    │                                                  │
    │                                                  │
    │                                                  │
    │                                                  │
    └──────────────────────────────────────────────────┘
    |}]
;;

let%expect_test "transparent rectangle on top of filled box" =
  run_test
    (View.zcat
       [ View.transparent_rectangle ~width:3 ~height:3
       ; View.rectangle ~fill:'x' ~width:5 ~height:5 ()
       ]);
  [%expect
    {|
    ┌──────────────────────────────────────────────────┐
    │xxxxx                                             │
    │xxxxx                                             │
    │xxxxx                                             │
    │xxxxx                                             │
    │xxxxx                                             │
    │                                                  │
    │                                                  │
    │                                                  │
    │                                                  │
    └──────────────────────────────────────────────────┘
    |}]
;;

let%expect_test "empty box on top of filled box" =
  run_test
    (View.zcat
       [ View.rectangle ~width:3 ~height:3 ()
       ; View.rectangle ~fill:'x' ~width:5 ~height:5 ()
       ]);
  [%expect
    {|
    ┌──────────────────────────────────────────────────┐
    │   xx                                             │
    │   xx                                             │
    │   xx                                             │
    │xxxxx                                             │
    │xxxxx                                             │
    │                                                  │
    │                                                  │
    │                                                  │
    │                                                  │
    └──────────────────────────────────────────────────┘
    |}]
;;

let%expect_test "space-filled box on top of x-filled box" =
  run_test
    (View.zcat
       [ View.rectangle ~fill:' ' ~width:3 ~height:3 ()
       ; View.rectangle ~fill:'x' ~width:5 ~height:5 ()
       ]);
  [%expect
    {|
    ┌──────────────────────────────────────────────────┐
    │   xx                                             │
    │   xx                                             │
    │   xx                                             │
    │xxxxx                                             │
    │xxxxx                                             │
    │                                                  │
    │                                                  │
    │                                                  │
    │                                                  │
    └──────────────────────────────────────────────────┘
    |}]
;;

let%expect_test "bold (unfilled) box on top of x-filled box" =
  run_test
    (View.zcat
       [ View.rectangle ~attrs:[ Attr.bold ] ~width:3 ~height:3 ()
       ; View.rectangle ~fill:'x' ~width:5 ~height:5 ()
       ]);
  [%expect
    {|
    ┌──────────────────────────────────────────────────┐
    │   xx                                             │
    │   xx                                             │
    │   xx                                             │
    │xxxxx                                             │
    │xxxxx                                             │
    │                                                  │
    │                                                  │
    │                                                  │
    │                                                  │
    └──────────────────────────────────────────────────┘
    |}]
;;
