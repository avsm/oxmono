open! Core
open Bonsai_test
open Bonsai_term

let%expect_test "rgb overflow" =
  let handle =
    Bonsai_term_test.create_handle_without_handler (fun ~dimensions:_ _graph ->
      Bonsai.return
        (View.text ~attrs:[ Attr.bg (Attr.Color.rgb ~r:300 ~g:300 ~b:300) ] "hello"))
  in
  Bonsai_term_test.set_dimensions handle { width = 10; height = 1 };
  Handle.show handle;
  [%expect
    {|
    ┌──────────┐
    │hello     │
    └──────────┘
    |}]
;;
