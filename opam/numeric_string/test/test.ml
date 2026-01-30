open Core

module type Comparison_to_test = sig
  type t [@@deriving compare, sexp_of]
end

let assert_is_sorted
  (type t)
  (module T : Comparison_to_test with type t = t)
  (xs : t list)
  =
  let ixs = List.mapi xs ~f:(fun i x -> i, x) in
  List.iter (List.cartesian_product ixs ixs) ~f:(fun ((x_pos, x), (y_pos, y)) ->
    let expected_comparison = Int.compare x_pos y_pos in
    let actual_comparison = T.compare x y in
    if Int.( <> ) expected_comparison actual_comparison
    then
      raise_s
        [%message
          "Elements not in expected order"
            (x : T.t)
            (y : T.t)
            (expected_comparison : int)
            (actual_comparison : int)])
;;

let%expect_test "verify assert_is_sorted works" =
  assert_is_sorted (module Int) [ 1; 2; 3 ];
  let module Int_but_always_equal = struct
    type t = int [@@deriving sexp_of]

    let compare _ _ = 0
  end
  in
  Expect_test_helpers_core.show_raise (fun () ->
    assert_is_sorted (module Int_but_always_equal) [ 1; 2; 3 ]);
  [%expect
    {|
    (raised (
      "Elements not in expected order"
      (x                   1)
      (y                   2)
      (expected_comparison -1)
      (actual_comparison   0)))
    |}];
  let module Int_but_wrong_in_one_case = struct
    type t = int [@@deriving sexp_of]

    let compare x y = if x = 3 && y = 1 then Int.compare y x else Int.compare x y
  end
  in
  Expect_test_helpers_core.show_raise (fun () ->
    assert_is_sorted (module Int_but_wrong_in_one_case) [ 0; 1; 2; 3; 4 ]);
  [%expect
    {|
    (raised (
      "Elements not in expected order"
      (x                   3)
      (y                   1)
      (expected_comparison 1)
      (actual_comparison   -1)))
    |}]
;;

module Reference_implementation = struct
  type t = string [@@deriving sexp_of]

  (* Components are alternating between non-numeric and numeric, always starting with
     non-numeric. If a string starts with a digit, its first component is the empty
     string. *)
  let validate_components t l =
    assert (not (List.is_empty l));
    assert (
      List.for_alli l ~f:(fun i s ->
        ((not (String.is_empty s)) || i = 0)
        &&
        let check = if i % 2 = 0 then Fn.non Char.is_digit else Char.is_digit in
        String.for_all s ~f:check));
    assert (String.equal t (String.concat l))
  ;;

  (* This is what I wish the code could look like, but it allocates like, quadratic space
     in the length of the input. We could make that a lot better by adding [take_while] to
     the [Substring] interface and using that, but we couldn't really get it to zero. *)
  let to_component_list =
    let rec take_next t ~numeric =
      let next = String.take_while t ~f:(fun c -> Bool.equal (Char.is_digit c) numeric) in
      let remainder = String.chop_prefix_exn ~prefix:next t in
      next
      ::
      (if String.is_empty remainder
       then []
       else take_next remainder ~numeric:(not numeric))
    in
    fun t ->
      let res = take_next t ~numeric:false in
      validate_components t res;
      res
  ;;

  let compare_numeric t1 t2 =
    match Int.compare (Int.of_string t1) (Int.of_string t2) with
    | 0 -> Int.compare (String.length t1) (String.length t2)
    | other -> other
  ;;

  let compare_chunk ~numeric = if numeric then compare_numeric else String.compare

  let compare t1 t2 =
    let with_index t = to_component_list t |> List.mapi ~f:(fun i s -> i, s) in
    List.compare
      (fun (i1, s1) (i2, s2) ->
        assert (i1 = i2);
        compare_chunk ~numeric:(i1 % 2 = 1) s1 s2)
      (with_index t1)
      (with_index t2)
  ;;
end

let%expect_test _ =
  Quickcheck.test [%quickcheck.generator: string * string] ~f:(fun (t1, t2) ->
    [%test_eq: int]
      (Numeric_string.compare t1 t2)
      (Reference_implementation.compare t1 t2));
  [%expect ""]
;;

let assert_is_sorted xs =
  assert_is_sorted (module Numeric_string) xs;
  assert_is_sorted (module Reference_implementation) xs
;;

module%test [@name "Numeric_string compare"] _ = struct
  let%expect_test _ = assert_is_sorted (List.init 100 ~f:Int.to_string)
  let numeric = [ "0"; "1"; "01"; "2"; "09"; "10"; "11"; "100"; "110" ]
  let alpha = [ "a"; "aa"; "aaa"; "ab" ]

  let%expect_test _ =
    assert_is_sorted numeric;
    assert_is_sorted alpha;
    assert_is_sorted (numeric @ alpha)
  ;;

  let all_concats =
    List.fold ~init:[ "" ] ~f:(fun acc strings ->
      List.cartesian_product acc strings |> List.map ~f:(fun (s1, s2) -> s1 ^ s2))
  ;;

  let%expect_test _ =
    print_s [%sexp (all_concats [ [ "a"; "b" ]; [ "1"; "2" ] ] : string list)];
    [%expect {| (a1 a2 b1 b2) |}]
  ;;

  let test pieces = assert_is_sorted ("" :: all_concats pieces)

  let%expect_test _ =
    test [ alpha; numeric ];
    test [ numeric; alpha ];
    test [ alpha; numeric; alpha ];
    test [ numeric; alpha; numeric ];
    test [ alpha; numeric; alpha; numeric ];
    test [ numeric; alpha; numeric; alpha ]
  ;;

  let%expect_test "Numeric_string.Set" =
    let test m =
      let examples =
        Set.of_list
          m
          (all_concats [ [ ""; "a"; "aa"; "b" ]; [ "0"; "2"; "002"; "10" ]; [ "z" ] ])
        |> Set.to_list
      in
      print_s [%sexp (examples : string list)]
    in
    test (module String);
    [%expect
      "(002z 0z 10z 2z a002z a0z a10z a2z aa002z aa0z aa10z aa2z b002z b0z b10z b2z)"];
    test (module Numeric_string);
    [%expect
      "(0z 2z 002z 10z a0z a2z a002z a10z aa0z aa2z aa002z aa10z b0z b2z b002z b10z)"]
  ;;

  let%expect_test "no allocation" =
    List.iter
      [ "a0b1", "a0b1c"
      ; "abz", "acz"
      ; "abc1z", "ab01z"
      ; "a0yz", "a01z"
      ; "a0z", "a1z"
      ; "a200z", "a1000z"
      ]
      ~f:(fun (x, y) ->
        Expect_test_helpers_core.require_no_allocation (fun () ->
          ignore (Numeric_string.compare x x : int);
          ignore (Numeric_string.compare x y : int);
          ignore (Numeric_string.compare y x : int)));
    [%expect ""]
  ;;
end
