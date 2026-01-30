open! Core
open Bonsai
open Bonsai_test
open Bonsai.Let_syntax

module%test [@name "match%%{sub,arr} tuple evaluation order"] _ = struct
  let%expect_test "tuples are evaluated right to left because ocaml is silly" =
    let component (local_ _graph) =
      let a = Bonsai.return "a" >>| print_endline in
      let b = Bonsai.return "b" >>| print_endline in
      let c = Bonsai.return "c" >>| print_endline in
      (match%sub a, b, c with
       | _a, _b, _c -> Bonsai.return ()
       | _, _, _ -> Bonsai.return ())
      [@ocaml.warning "-11"]
      (* if we don't have multiple cases, match%sub will optimize itself into a series of
         let%subs, and unused pattern variables won't be computed *)
    in
    let handle = Handle.create (Result_spec.sexp (module Unit)) component in
    Handle.show handle;
    [%expect
      {|
      c
      b
      a
      ()
      |}]
  ;;

  let%expect_test "evaluation order depends only on match%%sub tuple" =
    let component (local_ _graph) =
      let a = Bonsai.return "a" >>| print_endline in
      let b = Bonsai.return "b" >>| print_endline in
      let c = Bonsai.return "c" >>| print_endline in
      (match%sub c, b, a with
       | _c, _b, _a -> Bonsai.return ()
       | _, _, _ -> Bonsai.return ())
      [@ocaml.warning "-11"]
    in
    let handle = Handle.create (Result_spec.sexp (module Unit)) component in
    Handle.show handle;
    [%expect
      {|
      a
      b
      c
      ()
      |}]
  ;;

  let%expect_test "identical order with match%%arr" =
    let component (local_ _graph) =
      let a = Bonsai.return "a" >>| print_endline in
      let b = Bonsai.return "b" >>| print_endline in
      let c = Bonsai.return "c" >>| print_endline in
      match%arr a, b, c with
      | _a, _b, _c -> ()
    in
    let handle = Handle.create (Result_spec.sexp (module Unit)) component in
    Handle.show handle;
    [%expect
      {|
      c
      b
      a
      ()
      |}]
  ;;
end

module%test [@name "let%%arr and match%%arr cutoffs"] _ = struct
  module Variant_one_case = struct
    type 'a t = Foo of 'a
  end

  module Variant_two_cases = struct
    type 'a t =
      | Alpha of 'a
      | Beta of int * 'a
  end

  module Record = struct
    type ('a, 'b) t =
      { a : 'a
      ; b : 'b
      }
  end

  module%test [@name "let%%arr cuts off in practice"] _ = struct
    let%expect_test "variant" =
      let var = Bonsai.Expert.Var.create (Variant_one_case.Foo 1) in
      let component (local_ _graph) =
        let%arr (Foo _) = Bonsai.Expert.Var.value var in
        print_endline "Recomputed";
        0
      in
      let handle = Handle.create (Result_spec.sexp (module Int)) component in
      Handle.show handle;
      [%expect
        {|
        Recomputed
        0
        |}];
      Bonsai.Expert.Var.set var (Foo 2);
      Handle.show handle;
      [%expect {| 0 |}]
    ;;

    let%expect_test "record" =
      let var = Bonsai.Expert.Var.create { Record.a = 3; b = true } in
      let component (local_ _graph) =
        let%arr { a; b = _ } = Bonsai.Expert.Var.value var in
        print_endline "Recomputed";
        a
      in
      let handle = Handle.create (Result_spec.sexp (module Int)) component in
      Handle.show handle;
      [%expect
        {|
        Recomputed
        3
        |}];
      Bonsai.Expert.Var.set var { a = 3; b = false };
      Handle.show handle;
      [%expect {| 3 |}];
      Bonsai.Expert.Var.set var { a = 4; b = false };
      Handle.show handle;
      [%expect
        {|
        Recomputed
        4
        |}]
    ;;
  end

  module%test [@name "match%%arr cuts off in practice"] _ = struct
    let%expect_test "single-case variant" =
      let var = Bonsai.Expert.Var.create (Variant_one_case.Foo (1, 2)) in
      let component (local_ _graph) =
        match%arr Bonsai.Expert.Var.value var with
        | Foo (a, _) ->
          print_endline "Recomputed";
          a
      in
      let handle = Handle.create (Result_spec.sexp (module Int)) component in
      Handle.show handle;
      [%expect
        {|
        Recomputed
        1
        |}];
      Bonsai.Expert.Var.set var (Foo (1, 3));
      Handle.show handle;
      [%expect {| 1 |}]
    ;;

    let%expect_test "multi-case variant" =
      let var = Bonsai.Expert.Var.create (Variant_two_cases.Alpha 1) in
      let component (local_ _graph) =
        match%arr Bonsai.Expert.Var.value var with
        | Beta (1, _) ->
          print_endline "Recomputed branch 0";
          0
        | Beta (_, b) ->
          print_endline "Recomputed branch 1";
          b
        | Alpha a ->
          print_endline "Recomputed branch 2";
          a
      in
      let handle = Handle.create (Result_spec.sexp (module Int)) component in
      Handle.show handle;
      [%expect
        {|
        Recomputed branch 2
        1
        |}];
      Bonsai.Expert.Var.set var (Beta (1, 3));
      Handle.show handle;
      [%expect
        {|
        Recomputed branch 0
        0
        |}];
      Bonsai.Expert.Var.set var (Beta (1, 4));
      Handle.show handle;
      [%expect {| 0 |}];
      Bonsai.Expert.Var.set var (Beta (2, 3));
      Handle.show handle;
      [%expect
        {|
        Recomputed branch 1
        3
        |}];
      Bonsai.Expert.Var.set var (Beta (3, 3));
      Handle.show handle;
      [%expect {| 3 |}]
    ;;

    let%expect_test "record" =
      let var = Bonsai.Expert.Var.create { Record.a = 1; b = true } in
      let component (local_ _graph) =
        match%arr Bonsai.Expert.Var.value var with
        | { a = 1; b = _ } ->
          print_endline "Recomputed branch 0";
          0
        | { b = true; _ } ->
          print_endline "Recomputed branch 1";
          1
        | { a; b = false } ->
          print_endline "Recomputed branch 2";
          a
      in
      let handle = Handle.create (Result_spec.sexp (module Int)) component in
      Handle.show handle;
      [%expect
        {|
        Recomputed branch 0
        0
        |}];
      Bonsai.Expert.Var.set var { a = 1; b = false };
      Handle.show handle;
      [%expect {| 0 |}];
      Bonsai.Expert.Var.set var { a = 2; b = true };
      Handle.show handle;
      [%expect
        {|
        Recomputed branch 1
        1
        |}];
      Bonsai.Expert.Var.set var { a = 3; b = true };
      Handle.show handle;
      [%expect {| 1 |}];
      Bonsai.Expert.Var.set var { a = 2; b = false };
      Handle.show handle;
      [%expect
        {|
        Recomputed branch 2
        2
        |}];
      Bonsai.Expert.Var.set var { a = 3; b = false };
      Handle.show handle;
      [%expect
        {|
        Recomputed branch 2
        3
        |}]
    ;;

    let%expect_test "tuple" =
      let var1 = Bonsai.Expert.Var.create (Variant_one_case.Foo (1, 2)) in
      let var2 = Bonsai.Expert.Var.create (Variant_two_cases.Beta (3, 4)) in
      let component (local_ _graph) =
        match%arr Bonsai.Expert.Var.value var1, Bonsai.Expert.Var.value var2 with
        | Foo (a, _), Alpha _ ->
          print_endline "Recomputed branch 0";
          a
        | Foo (a, _), Beta (b, _) ->
          print_endline "Recomputed branch 1";
          a + b
      in
      let handle = Handle.create (Result_spec.sexp (module Int)) component in
      Handle.show handle;
      [%expect
        {|
        Recomputed branch 1
        4
        |}];
      Bonsai.Expert.Var.set var1 (Foo (1, 3));
      Handle.show handle;
      [%expect {| 4 |}];
      Bonsai.Expert.Var.set var2 (Beta (3, 5));
      Handle.show handle;
      [%expect {| 4 |}];
      Bonsai.Expert.Var.set var2 (Beta (4, 4));
      Handle.show handle;
      [%expect
        {|
        Recomputed branch 1
        5
        |}];
      Bonsai.Expert.Var.set var2 (Alpha 5);
      Handle.show handle;
      [%expect
        {|
        Recomputed branch 0
        1
        |}]
    ;;
  end
end

module%test [@name "match%%sub [%%lazy]"] _ = struct
  let%expect_test "basic match without lazy" =
    let var = Bonsai.Expert.Var.create false in
    let component (local_ _graph) =
      match%sub Bonsai.Expert.Var.value var with
      | false ->
        print_endline "false";
        Bonsai.return 1
      | true ->
        print_endline "true";
        Bonsai.return 2
    in
    let handle = Handle.create (Result_spec.sexp (module Int)) component in
    Handle.show handle;
    [%expect
      {|
      false
      true
      1
      |}];
    Bonsai.Expert.Var.set var true;
    Handle.show handle;
    [%expect {| 2 |}];
    Bonsai.Expert.Var.set var false;
    Handle.show handle;
    [%expect {| 1 |}]
  ;;

  let%expect_test "basic match with lazy" =
    let var = Bonsai.Expert.Var.create false in
    let component (local_ graph) =
      match%sub [%lazy] Bonsai.Expert.Var.value var with
      | false ->
        print_endline "false";
        Bonsai.return 1
      | true ->
        print_endline "true";
        Bonsai.return 2
    in
    let handle = Handle.create (Result_spec.sexp (module Int)) component in
    Handle.show handle;
    [%expect
      {|
      false
      1
      |}];
    Bonsai.Expert.Var.set var true;
    Handle.show handle;
    [%expect
      {|
      true
      2
      |}];
    Bonsai.Expert.Var.set var false;
    Handle.show handle;
    [%expect {| 1 |}]
  ;;

  let%expect_test "nested lazy match" =
    let var = Bonsai.Expert.Var.create 0 in
    let component (local_ graph) =
      match%sub [%lazy] Bonsai.Expert.Var.value var with
      | 0 ->
        print_endline "branch 0";
        Bonsai.return 0
      | _ ->
        print_endline "branch 1";
        (match%sub [%lazy] Bonsai.Expert.Var.value var with
         | 1 ->
           print_endline "branch 1a";
           Bonsai.return 1
         | _ ->
           print_endline "branch 1b";
           Bonsai.return 2)
    in
    let handle = Handle.create (Result_spec.sexp (module Int)) component in
    Handle.show handle;
    [%expect
      {|
      branch 0
      0
      |}];
    Bonsai.Expert.Var.set var 2;
    Handle.show handle;
    [%expect
      {|
      branch 1
      branch 1b
      2
      |}];
    Bonsai.Expert.Var.set var 1;
    Handle.show handle;
    [%expect
      {|
      branch 1a
      1
      |}]
  ;;

  let%expect_test "lazy match on tuple" =
    let var = Bonsai.Expert.Var.create false in
    let component (local_ graph) =
      match%sub [%lazy] Bonsai.Expert.Var.value var, Bonsai.return 0 with
      | false, _ ->
        print_endline "false";
        Bonsai.return 1
      | true, _ ->
        print_endline "true";
        Bonsai.return 2
    in
    let handle = Handle.create (Result_spec.sexp (module Int)) component in
    Handle.show handle;
    [%expect
      {|
      false
      1
      |}];
    Bonsai.Expert.Var.set var true;
    Handle.show handle;
    [%expect
      {|
      true
      2
      |}];
    Bonsai.Expert.Var.set var false;
    Handle.show handle;
    [%expect {| 1 |}]
  ;;

  module Example = struct
    type t =
      | A of t
      | B
    [@@deriving sexp]
  end

  let%expect_test "lazy has meaningfully different capabilities, even though you should \
                   use [fix] instead"
    =
    let var = Bonsai.Expert.Var.create false in
    let component (local_ graph) =
      let rec helper acc graph =
        (* If this [%lazy] is removed, this causes a stack overflow. *)
        match%sub [%lazy] Bonsai.Expert.Var.value var with
        | false -> Bonsai.return acc
        | true -> helper (Example.A acc) graph
      in
      helper Example.B graph
    in
    let handle = Handle.create (Result_spec.sexp (module Example)) component in
    Handle.show handle;
    [%expect {| B |}]
  ;;
end

let%expect_test "match%sub with . cases goes to correct case" =
  let component (local_ _graph) =
    let a : (int, Nothing.t) Result.t Bonsai.t = Bonsai.return (Ok 3) in
    match%sub a with
    | Ok 0 -> Bonsai.return 0
    | Error _ -> .
    | Ok 1 -> Bonsai.return 1
    | Ok _ -> Bonsai.return 2
  in
  let handle = Handle.create (Result_spec.sexp (module Int)) component in
  Handle.show handle;
  [%expect {| 2 |}]
;;
