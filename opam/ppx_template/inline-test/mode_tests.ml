open! Ppx_template_test_common

(* It seems somewhat pointless to do something like duplicate all of the kind tests here,
   just with modes instead, since the ppx implementation itself doesn't really distinguish
   between them beyond being different types and thus appearing in different places in the
   AST. But the attributes and extension points are the same.

   So, instead, we implement a mode-polymorphic version of [Comparable.With_compare],
   exercising some of the capabilities. *)

module%template Comparable : sig
  [@@@mode.default m = (global, local)]

  type ('a, 'b) compare_fn := 'a @ m -> 'a @ m -> 'b
  type ('a, 'b) fn := 'a @ m -> 'b @ m
  type 'a select_fn := 'a @ m -> 'a @ m -> 'a @ m

  val lexicographic
    :  (('a, int) compare_fn[@mode m]) list
    -> (('a, int) compare_fn[@mode m])

  val lift
    :  (('a, 'result) compare_fn[@mode m])
    -> f:(('b, 'a) fn[@mode m])
    -> (('b, 'result) compare_fn[@mode m])

  val reverse : (('a, 'result) compare_fn[@mode m]) -> (('a, 'result) compare_fn[@mode m])

  type 'a reversed = 'a

  val compare_reversed
    :  (('a, int) compare_fn[@mode m])
    -> (('a reversed, int) compare_fn[@mode m])

  val equal : (('a, int) compare_fn[@mode m]) -> (('a, bool) compare_fn[@mode m])
  val max : (('a, int) compare_fn[@mode m]) -> ('a select_fn[@mode m])
  val min : (('a, int) compare_fn[@mode m]) -> ('a select_fn[@mode m])
end = struct
  [@@@mode.default m = (global, local)]

  let rec lexicographic cmps x y =
    match cmps with
    | cmp :: cmps ->
      let res = cmp x y in
      if res = 0 then (lexicographic [@mode m]) cmps x y else res
    | [] -> 0
  ;;

  let lift cmp ~f x y = cmp (f x) (f y) [@nontail]
  let reverse cmp x y = cmp y x

  type 'a reversed = 'a

  let compare_reversed cmp x y = cmp y x
  let equal cmp x y = cmp x y = 0
  let geq cmp x y = cmp x y >= 0
  let leq cmp x y = cmp x y <= 0

  let max cmp x y =
    let is_geq = (geq [@mode m]) cmp x y in
    Bool.select is_geq x y [@exclave_if_local m]
  ;;

  let min cmp x y =
    let is_leq = (leq [@mode m]) cmp x y in
    Bool.select is_leq x y [@exclave_if_local m]
  ;;
end

(* [Char], but not mode-crossing. *)
module%template T : sig
  type t

  val compare : t @ m -> t @ m -> int [@@mode m = (global, local)]
  val of_char : char -> t
  val sexp_of_t : t @ local -> Sexp.t
end = struct
  type t = Char.t

  let%template[@mode m = (global, local)] compare = (compare_char [@mode m])
  let of_char t = t
  let sexp_of_t = (sexp_of_char :> t @ local -> _)
end

let x = T.of_char 'x'
let y = T.of_char 'y'

[%%template
[@@@mode.default m = (global, local)]

let%expect_test "lexicographic" =
  let print_result x y =
    (Comparable.lexicographic [@mode m])
      [ (fun _ _ -> 0); (fun _ _ -> 0); (T.compare [@mode m]) ]
      x
      y
    |> [%sexp_of: int]
    |> print_s
  in
  print_result x x;
  [%expect {| 0 |}];
  print_result x y;
  [%expect {| -1 |}];
  print_result y x;
  [%expect {| 1 |}];
  print_result y y;
  [%expect {| 0 |}]
;;

let%expect_test "lift" =
  let print_result x y =
    (Comparable.lift [@mode m]) (T.compare [@mode m]) ~f:snd ((), x) ((), y)
    |> [%sexp_of: int]
    |> print_s
  in
  print_result x x;
  [%expect {| 0 |}];
  print_result x y;
  [%expect {| -1 |}];
  print_result y x;
  [%expect {| 1 |}];
  print_result y y;
  [%expect {| 0 |}]
;;

let%expect_test "reverse" =
  let print_result x y =
    (Comparable.reverse [@mode m]) (T.compare [@mode m]) x y |> [%sexp_of: int] |> print_s
  in
  print_result x x;
  [%expect {| 0 |}];
  print_result x y;
  [%expect {| 1 |}];
  print_result y x;
  [%expect {| -1 |}];
  print_result y y;
  [%expect {| 0 |}]
;;

let%expect_test "compare_reversed" =
  let print_result x y =
    (Comparable.compare_reversed [@mode m]) (T.compare [@mode m]) x y
    |> [%sexp_of: int]
    |> print_s
  in
  print_result x x;
  [%expect {| 0 |}];
  print_result x y;
  [%expect {| 1 |}];
  print_result y x;
  [%expect {| -1 |}];
  print_result y y;
  [%expect {| 0 |}]
;;

let%expect_test "equal" =
  let print_result x y =
    (Comparable.equal [@mode m]) (T.compare [@mode m]) x y |> [%sexp_of: bool] |> print_s
  in
  print_result x x;
  [%expect {| true |}];
  print_result x y;
  [%expect {| false |}];
  print_result y x;
  [%expect {| false |}];
  print_result y y;
  [%expect {| true |}]
;;

let%expect_test "max" =
  let print_result x y =
    (Comparable.max [@mode m]) (T.compare [@mode m]) x y |> [%sexp_of: T.t] |> print_s
  in
  print_result x x;
  [%expect {| x |}];
  print_result x y;
  [%expect {| y |}];
  print_result y x;
  [%expect {| y |}];
  print_result y y;
  [%expect {| y |}]
;;

let%expect_test "min" =
  let print_result x y =
    (Comparable.min [@mode m]) (T.compare [@mode m]) x y |> [%sexp_of: T.t] |> print_s
  in
  print_result x x;
  [%expect {| x |}];
  print_result x y;
  [%expect {| x |}];
  print_result y x;
  [%expect {| x |}];
  print_result y y;
  [%expect {| y |}]
;;]
