open! Core
open Bonsai
open Bonsai.Let_syntax
open Bonsai_test

let%expect_test "even / odd mutual recursion (match%sub [%lazy])" =
  let var = Bonsai.Expert.Var.create 42 in
  let component (local_ graph) =
    let rec is_even n (local_ graph) =
      match%sub [%lazy] n with
      | 0 -> Bonsai.return true
      | n ->
        is_odd
          (let%arr n in
           n - 1)
          graph
    and is_odd n (local_ graph) =
      match%sub [%lazy] n with
      | 0 -> Bonsai.return false
      | n ->
        is_even
          (let%arr n in
           n - 1)
          graph
    in
    is_even (Bonsai.Expert.Var.value var) graph
  in
  let handle = Handle.create (Result_spec.sexp (module Bool)) component in
  Handle.show handle;
  [%expect {| true |}];
  Bonsai.Expert.Var.set var 7;
  Handle.show handle;
  [%expect {| false |}]
;;

let%expect_test "even / odd mutual recursion (Bonsai.fix)" =
  let open Bonsai.Let_syntax in
  let even odd n (local_ graph) =
    Bonsai.fix n graph ~f:(fun ~recurse:_ n (local_ graph) ->
      match%sub n with
      | 0 -> Bonsai.return true
      | n ->
        odd
          (let%arr n in
           n - 1)
          graph)
  in
  let odd n (local_ graph) =
    Bonsai.fix n graph ~f:(fun ~recurse n (local_ graph) ->
      match%sub n with
      | 0 -> Bonsai.return false
      | n ->
        even
          recurse
          (let%arr n in
           n - 1)
          graph)
  in
  let is_even n (local_ graph) = even odd n graph in
  let var = Bonsai.Expert.Var.create 42 in
  let handle =
    Handle.create
      (Result_spec.sexp (module Bool))
      (fun graph -> is_even (Bonsai.Expert.Var.value var) graph)
  in
  Handle.show handle;
  [%expect {| true |}];
  Bonsai.Expert.Var.set var 7;
  Handle.show handle;
  [%expect {| false |}]
;;
