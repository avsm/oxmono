open! Core
open! Import
module Bonsai = Bonsai_proc
open Bonsai_test
open Bonsai.For_open
open Bonsai.Let_syntax
module Path = Bonsai.Private.Path

let%expect_test "path" =
  let component =
    let%sub () = opaque_const () in
    let%sub path = Bonsai.Private.path in
    return (Value.map path ~f:Path.to_unique_identifier_string)
  in
  let handle = Handle.create (Result_spec.string (module String)) component in
  Handle.show handle;
  (* The first of these "Subst_from" is actually a component that is added by the testing
     helpers. *)
  [%expect {| bonsai_path |}]
;;

let%expect_test "path constant folding" =
  let component =
    let%sub () = Bonsai.const () in
    let%sub path = Bonsai.Private.path in
    return (Value.map path ~f:Path.to_unique_identifier_string)
  in
  let handle = Handle.create (Result_spec.string (module String)) component in
  Handle.show handle;
  (* The first of these "Subst_from" is actually a component that is added by the testing
     helpers. *)
  [%expect {| bonsai_path |}]
;;

let assert_path_unique_id_is_alpha path =
  let unique_id = Path.to_unique_identifier_string path in
  assert (
    String.for_all unique_id ~f:(function
      | 'a' .. 'z' | '_' -> true
      | _ -> false))
;;

let%test_unit "all the values are alpha" =
  let string_id = Type_equal.Id.create ~name:"string" [%sexp_of: string] in
  let keyed = Path.Elem.keyed ~compare:String.compare string_id |> unstage in
  Quickcheck.test
    String.quickcheck_generator
    ~sexp_of:[%sexp_of: string]
    ~f:(fun string ->
      let path = Path.append Path.empty (Path.Elem.Assoc (keyed string)) in
      assert_path_unique_id_is_alpha path)
;;

let%test_unit "larger groupings of paths behave" =
  let string_id = Type_equal.Id.create ~name:"string" [%sexp_of: string] in
  let keyed = Path.Elem.keyed ~compare:String.compare string_id |> unstage in
  let module P = struct
    (* Make a dumb version of this module so that we can derive quickcheck for it. *)
    type t =
      | From
      | Into
      | Assoc of string
      | Switch of int
    [@@deriving quickcheck, sexp]

    let to_path_element = function
      | From -> Path.Elem.Subst_from
      | Into -> Path.Elem.Subst_into
      | Assoc s -> Path.Elem.Assoc (keyed s)
      | Switch i -> Path.Elem.Switch i
    ;;
  end
  in
  Quickcheck.test
    [%quickcheck.generator: P.t list]
    ~sexp_of:[%sexp_of: P.t list]
    ~f:(fun path ->
      let path =
        path |> List.map ~f:P.to_path_element |> List.fold ~init:Path.empty ~f:Path.append
      in
      assert_path_unique_id_is_alpha path)
;;

type simple_path =
  [ `Subst_into_invert_lifecycles
  | `Subst_into
  | `Subst_from
  | `Assoc of Int.t
  | `Switch of Int.t
  ]
    list
[@@deriving sexp, quickcheck]

let iterations = ref 0
let compare_true = ref 0
let compare_false = ref 0
let compare_true_empty_list = ref 0
let compare_false_empty_list = ref 0

let%expect_test "Bisimulating run length encoding path id comparison and slow but \
                 simpler comparison"
  =
  let%quick_test prop ((a, b) : simple_path * simple_path) =
    incr iterations;
    let int_id = Type_equal.Id.create ~name:"int" [%sexp_of: int] in
    let path_a, path_b =
      Tuple2.map (a, b) ~f:(fun elements ->
        List.fold elements ~init:Bonsai.Private.Path.empty ~f:(fun path element ->
          let element =
            match element with
            | `Subst_into_invert_lifecycles ->
              Bonsai.Private.Path.Elem.Subst_into_invert_lifecycles
            | `Subst_from -> Subst_from
            | `Subst_into -> Subst_into
            | `Assoc i -> Assoc (T { key = i; id = int_id; compare = [%compare: int] })
            | `Switch i -> Switch i
          in
          Bonsai.Private.Path.append path element))
    in
    let correct_result =
      Bonsai.Private.Path.For_testing.slow_but_correct_compare_for_bisimulation
        path_a
        path_b
    in
    let fast_result = Bonsai.Private.Path.compare path_a path_b in
    if correct_result = 0
    then (
      incr compare_true;
      match a, b with
      | [], [] -> incr compare_true_empty_list
      | _ -> ())
    else (
      incr compare_false;
      match a, b with
      | [], _ | _, [] -> incr compare_false_empty_list
      | _ -> ());
    assert (correct_result = fast_result)
      [@@remember_failures]
  in
  ()
;;

let%expect_test ("distribution of quick_test samples" [@tags "no-js"]) =
  print_s
    [%message
      ""
        ~total:(!iterations : int)
        ~the_same:(!compare_true - !compare_true_empty_list : int)
        ~not_the_same:(!compare_false - !compare_false_empty_list : int)
        ~includes_the_empty_list:
          (!compare_true_empty_list + !compare_false_empty_list : int)];
  [%expect
    {|
    ((total                   10000)
     (the_same                57)
     (not_the_same            5990)
     (includes_the_empty_list 3953))
    |}]
;;

let%expect_test "Bisimulating run length encoding path id comparison and slow but same \
                 list"
  =
  let%quick_test prop (path : simple_path) =
    let int_id = Type_equal.Id.create ~name:"int" [%sexp_of: int] in
    let path_a, path_b =
      Tuple2.map (path, path) ~f:(fun path ->
        (* Constructing the same path twice is silly, but it's so that the phys_equal
           doesn't accidentally prevent the functions we want to compare from running... *)
        List.fold path ~init:Bonsai.Private.Path.empty ~f:(fun path element ->
          let element =
            match element with
            | `Subst_into_invert_lifecycles ->
              Bonsai.Private.Path.Elem.Subst_into_invert_lifecycles
            | `Subst_from -> Subst_from
            | `Subst_into -> Subst_into
            | `Assoc i -> Assoc (T { key = i; id = int_id; compare = [%compare: int] })
            | `Switch i -> Switch i
          in
          Bonsai.Private.Path.append path element))
    in
    let correct_result =
      Bonsai.Private.Path.For_testing.slow_but_correct_compare_for_bisimulation
        path_a
        path_b
    in
    let fast_result = Bonsai.Private.Path.compare path_a path_b in
    assert (correct_result = fast_result)
      [@@remember_failures]
  in
  ()
;;

module%test [@name "paths compare as expected"] _ = struct
  let%expect_test "regular lifecycles" =
    let open Bonsai.Private.Path in
    let from = append empty Subst_from in
    let into = append empty Subst_into in
    print_s [%message (compare from into : int)];
    [%expect {| ("compare from into" -1) |}];
    ()
  ;;

  let%expect_test "inverted lifecycles" =
    let open Bonsai.Private.Path in
    let from = append empty Subst_from in
    let into_inverted = append empty Subst_into_invert_lifecycles in
    print_s [%message (compare from into_inverted : int)];
    [%expect {| ("compare from into_inverted" 1) |}];
    ()
  ;;

  let%expect_test "regression: x_x_x vs x_w" =
    let open Bonsai.Private.Path in
    let a = append (append (append empty Subst_from) Subst_from) Subst_from in
    let b = append (append empty Subst_from) Subst_into_invert_lifecycles in
    print_s [%message (compare a b : int)];
    (* We expect [a] > [b], so [1]. *)
    [%expect {| ("compare a b" 1) |}];
    ()
  ;;
end
