open! Core
open Bonsai_test
open Bonsai_term
open Bonsai.Let_syntax

let%expect_test "Really basic sanity test for dimensions" =
  let handle =
    Bonsai_term_test.create_handle_without_handler (fun ~dimensions (local_ _graph) ->
      let%arr dimensions in
      View.sexp_for_debugging
        [%message
          (dimensions : Dimensions.t)
            (dimensions : Dimensions.t)
            (dimensions : Dimensions.t)
            (dimensions : Dimensions.t)
            (dimensions : Dimensions.t)])
  in
  Handle.show handle;
  [%expect
    {|
    ┌────────────────────────────────────────────────────────────────────────────────┐
    │((dimensions ((height 40) (width 80))) (dimensions ((height 40) (width 80)))    │
    │ (dimensions ((height 40) (width 80))) (dimensions ((height 40) (width 80)))    │
    │ (dimensions ((height 40) (width 80))))                                         │
    │                                                                                │
    │                                                                                │
    │                                                                                │
    │                                                                                │
    │                                                                                │
    │                                                                                │
    │                                                                                │
    │                                                                                │
    │                                                                                │
    │                                                                                │
    │                                                                                │
    │                                                                                │
    │                                                                                │
    │                                                                                │
    │                                                                                │
    │                                                                                │
    │                                                                                │
    │                                                                                │
    │                                                                                │
    │                                                                                │
    │                                                                                │
    │                                                                                │
    │                                                                                │
    │                                                                                │
    │                                                                                │
    │                                                                                │
    │                                                                                │
    │                                                                                │
    │                                                                                │
    │                                                                                │
    │                                                                                │
    │                                                                                │
    │                                                                                │
    │                                                                                │
    │                                                                                │
    │                                                                                │
    └────────────────────────────────────────────────────────────────────────────────┘
    |}];
  Bonsai_term_test.set_dimensions handle { width = 78; height = 10 };
  Handle.show handle;
  [%expect
    {|
    ┌──────────────────────────────────────────────────────────────────────────────┐
    │((dimensions ((height 10) (width 78))) (dimensions ((height 10) (width 78)))  │
    │ (dimensions ((height 10) (width 78))) (dimensions ((height 10) (width 78)))  │
    │ (dimensions ((height 10) (width 78))))                                       │
    │                                                                              │
    │                                                                              │
    │                                                                              │
    │                                                                              │
    │                                                                              │
    │                                                                              │
    └──────────────────────────────────────────────────────────────────────────────┘
    |}];
  (* make the ui small enough that it cuts off the content *)
  Bonsai_term_test.set_dimensions handle { width = 45; height = 10 };
  Handle.show handle;
  [%expect
    {|
    ┌─────────────────────────────────────────────┐
    │((dimensions ((height 10) (width 45))) (dimen│
    │ (dimensions ((height 10) (width 45))) (dimen│
    │ (dimensions ((height 10) (width 45))))      │
    │                                             │
    │                                             │
    │                                             │
    │                                             │
    │                                             │
    │                                             │
    └─────────────────────────────────────────────┘
    |}]
;;

let example_app ~dimensions (local_ _graph) =
  let%arr { Dimensions.width; height } = dimensions in
  let number_attr = Attr.many [ Attr.bold ] in
  let number n = View.text ~attrs:[ number_attr ] [%string "%{n#Int}"] in
  let image =
    View.hcat
      [ View.text "("; number width; View.text " x "; number height; View.text ")" ]
  in
  View.center image ~within:{ width; height }
;;

let%expect_test "Testing the example app with attrs" =
  let handle = Bonsai_term_test.create_handle_without_handler example_app in
  Handle.show handle;
  [%expect
    {|
    ┌────────────────────────────────────────────────────────────────────────────────┐
    │                                                                                │
    │                                                                                │
    │                                                                                │
    │                                                                                │
    │                                                                                │
    │                                                                                │
    │                                                                                │
    │                                                                                │
    │                                                                                │
    │                                                                                │
    │                                                                                │
    │                                                                                │
    │                                                                                │
    │                                                                                │
    │                                                                                │
    │                                                                                │
    │                                                                                │
    │                                                                                │
    │                                                                                │
    │                                   (80 x 40)                                    │
    │                                                                                │
    │                                                                                │
    │                                                                                │
    │                                                                                │
    │                                                                                │
    │                                                                                │
    │                                                                                │
    │                                                                                │
    │                                                                                │
    │                                                                                │
    │                                                                                │
    │                                                                                │
    │                                                                                │
    │                                                                                │
    │                                                                                │
    │                                                                                │
    │                                                                                │
    │                                                                                │
    │                                                                                │
    └────────────────────────────────────────────────────────────────────────────────┘
    |}];
  Bonsai_term_test.set_dimensions handle { width = 78; height = 10 };
  Handle.show handle;
  [%expect
    {|
    ┌──────────────────────────────────────────────────────────────────────────────┐
    │                                                                              │
    │                                                                              │
    │                                                                              │
    │                                                                              │
    │                                  (78 x 10)                                   │
    │                                                                              │
    │                                                                              │
    │                                                                              │
    │                                                                              │
    └──────────────────────────────────────────────────────────────────────────────┘
    |}]
;;
