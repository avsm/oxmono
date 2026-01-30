open! Base
open! Expect_test_helpers_base
open Basement

module%test [@name "single-domain behavior tests"] _ = struct
  module Operation = struct
    type t =
      | Get
      | Set_root of int
      | With_temporarily of int * t list

    let perform ts ~init =
      let dynamic = Dynamic.make init in
      let rec loop = function
        | Get -> [%message "get" ~_:(Dynamic.get dynamic : int)]
        | Set_root to_ ->
          Dynamic.set_root dynamic to_;
          [%message "set_root" ~_:(to_ : int)]
        | With_temporarily (to_, ts) ->
          let body =
            Dynamic.with_temporarily dynamic to_ ~f:(fun () -> List.map ts ~f:loop)
          in
          [%message "with_temporarily" ~_:(to_ : int) ~_:(body : Sexp.t list)]
      in
      List.map ts ~f:loop |> List.iter ~f:print_s
    ;;
  end

  let%expect_test "initial value" =
    Operation.perform ~init:0 [ Get ];
    [%expect {| (get 0) |}]
  ;;

  let%expect_test "[set_root] and [get]" =
    Operation.perform ~init:0 [ Set_root 1; Get; Set_root 2; Get ];
    [%expect
      {|
      (set_root 1)
      (get 1)
      (set_root 2)
      (get 2)
      |}]
  ;;

  let%expect_test "[with_temporarily]" =
    Operation.perform
      ~init:0
      [ Get
      ; With_temporarily (1, [ Get ])
      ; Get
      ; With_temporarily (2, [ Get; Set_root 3; Get ])
      ; Get
      ; With_temporarily
          ( 4
          , [ Get
            ; With_temporarily (5, [ Get ])
            ; Get
            ; With_temporarily (6, [ Get ])
            ; Get
            ] )
      ; Get
      ; With_temporarily (7, [ Get; With_temporarily (8, [ Get; Set_root 9; Get ]); Get ])
      ; Get
      ];
    [%expect
      {|
      (get 0)
      (with_temporarily 1 ((get 1)))
      (get 0)
      (with_temporarily 2 (
        (get      2)
        (set_root 3)
        (get      2)))
      (get 3)
      (with_temporarily 4 (
        (get 4)
        (with_temporarily 5 ((get 5)))
        (get 4)
        (with_temporarily 6 ((get 6)))
        (get 4)))
      (get 3)
      (with_temporarily 7 (
        (get 7)
        (with_temporarily 8 (
          (get      8)
          (set_root 9)
          (get      8)))
        (get 7)))
      (get 9)
      |}]
  ;;
end
