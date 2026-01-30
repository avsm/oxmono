open! Core
open Bonsai_test
open Bonsai_term

let app ~dimensions:_ (local_ _graph) =
  Bonsai.return
    (View.text
       ~attrs:[ Attr.bg (Attr.Color.rgb ~r:255 ~g:255 ~b:255); Attr.bold ]
       "hello")
;;

let%expect_test "Not_ansi" =
  let handle =
    Bonsai_term_test.create_handle_without_handler
      ~capability:Not_ansi
      ~initial_dimensions:{ width = 10; height = 1 }
      app
  in
  Handle.show handle;
  [%expect
    {|
    ┌──────────┐
    │hello     │
    └──────────┘
    |}]
;;

let%expect_test "Ansi" =
  let handle =
    Bonsai_term_test.create_handle_without_handler
      ~capability:Ansi
      ~initial_dimensions:{ width = 10; height = 1 }
      app
  in
  Handle.show handle;
  [%expect {| (off bg:rgb256-255-255-255 +bold)hello(off)(EraseLine:ToEnd)(off) |}]
;;
