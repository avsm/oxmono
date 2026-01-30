open! Core
open! Import

let sexp_of_computation c =
  c
  |> Bonsai.Private.Skeleton.Computation.of_computation
  |> Bonsai.Private.Skeleton.Computation.sanitize_for_testing
  |> Bonsai.Private.Skeleton.Computation.minimal_sexp_of_t
;;

let print_computation c =
  print_s (sexp_of_computation (Bonsai.Private.top_level_handle c))
;;

module%test [@name "Cont"] _ = struct
  open! Bonsai_test
  open Bonsai.Let_syntax

  let%expect_test "dynamic scope modify without revert works" =
    let id =
      Bonsai.Dynamic_scope.create
        ~sexp_of:[%sexp_of: string]
        ~name:"my-id"
        ~fallback:"default"
        ()
    in
    let component graph =
      Bonsai.Dynamic_scope.modify
        id
        ~change:(fun original ->
          let%arr original in
          original ^ " (changed)")
        ~f:(fun { revert = _ } graph -> Bonsai.Dynamic_scope.lookup id graph)
        graph
    in
    let handle = Handle.create (Result_spec.sexp (module String)) component in
    Handle.show handle;
    [%expect {| "default (changed)" |}];
    print_computation component;
    [%expect
      {|
      (Sub
        (from (Fetch (id (Test 0))))
        (via (Test 1))
        (into (
          Sub
          (from (Return (value (Mapn (inputs ((Named (uid (Test 1)))))))))
          (via (Test 2))
          (into (
            Store
            (id (Test 0))
            (value (Named (uid (Test 2))))
            (inner (Fetch (id (Test 0)))))))))
      |}]
  ;;

  let%expect_test "dynamic scope modify with revert works" =
    let id =
      Bonsai.Dynamic_scope.create
        ~sexp_of:[%sexp_of: string]
        ~name:"my-id"
        ~fallback:"default"
        ()
    in
    let component graph =
      Bonsai.Dynamic_scope.modify
        id
        ~change:(fun original ->
          let%arr original in
          original ^ " (changed)")
        ~f:(fun { revert } graph ->
          let original = Bonsai.Dynamic_scope.lookup id graph in
          let reverted =
            revert (fun graph -> Bonsai.Dynamic_scope.lookup id graph) graph
          in
          let%arr original and reverted in
          [%string "Original: %{original}; Reverted: %{reverted}"])
        graph
    in
    let handle = Handle.create (Result_spec.sexp (module String)) component in
    Handle.show handle;
    [%expect {| "Original: default (changed); Reverted: default" |}];
    print_computation component;
    [%expect
      {|
      (Sub
        (from (Fetch (id (Test 0))))
        (via (Test 1))
        (into (
          Sub
          (from (Return (value (Mapn (inputs ((Named (uid (Test 1)))))))))
          (via (Test 2))
          (into (
            Store
            (id (Test 0))
            (value (Named (uid (Test 2))))
            (inner (
              Sub
              (from (Fetch (id (Test 0))))
              (via (Test 3))
              (into (
                Sub
                (from (
                  Store
                  (id (Test 0))
                  (value (Named (uid (Test 1))))
                  (inner (Fetch (id (Test 0))))))
                (via (Test 4))
                (into (
                  Return (
                    value (
                      Mapn (
                        inputs (
                          (Named (uid (Test 3)))
                          (Named (uid (Test 4))))))))))))))))))
      |}]
  ;;
end

module%test [@name "Proc"] _ = struct
  open! Bonsai_test
  module Bonsai = Bonsai_proc
  open Bonsai.Let_syntax

  let%expect_test "dynamic scope modify without revert works" =
    let id =
      Bonsai.Dynamic_scope.create
        ~sexp_of:[%sexp_of: string]
        ~name:"my-id"
        ~fallback:"default"
        ()
    in
    let component =
      Bonsai.Dynamic_scope.modify
        id
        ~change:(fun original ->
          let%map original in
          original ^ " (changed)")
        ~f:(fun { revert = _ } -> Bonsai.Dynamic_scope.lookup id)
    in
    let handle = Handle.create (Result_spec.sexp (module String)) component in
    Handle.show handle;
    [%expect {| "default (changed)" |}];
    print_computation component;
    [%expect
      {|
      (Sub
        (from (Fetch (id (Test 0))))
        (via (Test 1))
        (into (
          Store
          (id (Test 0))
          (value (Mapn (inputs ((Named (uid (Test 1)))))))
          (inner (Fetch (id (Test 0)))))))
      |}]
  ;;

  let%expect_test "dynamic scope modify with revert works" =
    let id =
      Bonsai.Dynamic_scope.create
        ~sexp_of:[%sexp_of: string]
        ~name:"my-id"
        ~fallback:"default"
        ()
    in
    let component graph =
      Bonsai.Dynamic_scope.modify
        id
        ~change:(fun original ->
          let%map original in
          original ^ " (changed)")
        ~f:(fun { revert } ->
          let%sub original = Bonsai.Dynamic_scope.lookup id in
          let%sub reverted = revert (Bonsai.Dynamic_scope.lookup id) in
          let%arr original and reverted in
          [%string "Original: %{original}; Reverted: %{reverted}"])
        graph
    in
    let handle = Handle.create (Result_spec.sexp (module String)) component in
    Handle.show handle;
    [%expect {| "Original: default (changed); Reverted: default" |}];
    print_computation component;
    [%expect
      {|
      (Sub
        (from (Fetch (id (Test 0))))
        (via (Test 1))
        (into (
          Store
          (id (Test 0))
          (value (Mapn (inputs ((Named (uid (Test 1)))))))
          (inner (
            Sub
            (from (Fetch (id (Test 0))))
            (via (Test 2))
            (into (
              Sub
              (from (
                Store
                (id (Test 0))
                (value (Named (uid (Test 1))))
                (inner (Fetch (id (Test 0))))))
              (via (Test 3))
              (into (
                Return (
                  value (
                    Mapn (
                      inputs (
                        (Named (uid (Test 2)))
                        (Named (uid (Test 3))))))))))))))))
      |}]
  ;;
end
