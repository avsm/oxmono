open! Core
open! Import
open! Bonsai.For_open
open Bonsai.Let_syntax
module Private = Bonsai.Private

let sexp_of_computation c =
  c
  |> Private.Skeleton.Computation.of_computation
  |> Private.Skeleton.Computation.sanitize_for_testing
  |> Private.Skeleton.Computation.minimal_sexp_of_t
;;

let create c =
  (* Creating a test handle will apply optimizations _and_ perform an initial
     stabilization, so in order to split these regions out, we can apply the constant
     folding separately, and then remove its output from the beginning of the handle
     creation output. *)
  let during_optimization_output, optimized_shape, unoptimized_shape =
    let lowered = Private.top_level_handle c in
    let optimized = Private.Constant_fold.constant_fold lowered in
    ( Expect_test_helpers_core.expect_test_output ()
    , sexp_of_computation optimized
    , sexp_of_computation lowered )
  in
  let handle_creation_output, handle =
    ( Expect_test_helpers_core.expect_test_output ()
    , Bonsai_test.Handle.create Bonsai_test.Result_spec.invisible c )
  in
  let first_stabilization_output =
    String.chop_prefix_if_exists handle_creation_output ~prefix:during_optimization_output
  in
  let indent s =
    String.split_lines s |> List.iter ~f:(fun line -> print_endline ("  " ^ line))
  in
  print_endline "Input shape:";
  indent (Sexp.to_string_hum unoptimized_shape);
  print_endline "Optimized shape:";
  indent (Sexp.to_string_hum optimized_shape);
  print_endline "Optimization:";
  indent during_optimization_output;
  print_endline "First Stabilization:";
  indent first_stabilization_output;
  handle
;;

(* In this file, side-effects are used inside of let%arr blocks to show if a computation
   is happening or not. *)

let%expect_test "simple dependency on constant value" =
  let c _graph =
    let%arr () = Bonsai.return () in
    print_endline "here"
  in
  let _ : _ = create c in
  [%expect
    {|
    Input shape:
      (Return (value (Mapn (inputs (Constant)))))
    Optimized shape:
      (Return (value Constant))
    Optimization:
    First Stabilization:
      here
    |}]
;;

let%expect_test "chained dependency on constant value" =
  let c _graph =
    let f () = print_endline "here" in
    Bonsai.return () >>| f >>| f >>| f >>| f
  in
  let _ : _ = create c in
  [%expect
    {|
    Input shape:
      (Sub (from (Return (value (Mapn (inputs (Constant)))))) (via (Test 0))
       (into
        (Sub (from (Return (value (Mapn (inputs ((Named (uid (Test 0)))))))))
         (via (Test 1))
         (into
          (Sub (from (Return (value (Mapn (inputs ((Named (uid (Test 1)))))))))
           (via (Test 2))
           (into (Return (value (Mapn (inputs ((Named (uid (Test 2))))))))))))))
    Optimized shape:
      (Return (value Constant))
    Optimization:
    First Stabilization:
      here
      here
      here
      here
    |}]
;;

let%expect_test "side-effect inside of unused branch" =
  let c _graph =
    match%sub Bonsai.return true with
    | true -> Bonsai.return ()
    | false -> Bonsai.return () >>| fun () -> print_endline "unused"
  in
  let _ : _ = create c in
  [%expect
    {|
    Input shape:
      (Sub (from (Return (value (Mapn (inputs (Constant)))))) (via (Test 0))
       (into
        (Switch (match_ (Named (uid (Test 0))))
         (arms
          ((Return (value Constant)) (Return (value (Mapn (inputs (Constant))))))))))
    Optimized shape:
      (Return (value Constant))
    Optimization:
    First Stabilization:
    |}]
;;

let%expect_test "side-effect inside of used branch" =
  let c _graph =
    match%sub Bonsai.return true with
    | true -> Bonsai.return () >>| fun () -> print_endline "used"
    | false -> Bonsai.return ()
  in
  let _ : _ = create c in
  [%expect
    {|
    Input shape:
      (Sub (from (Return (value (Mapn (inputs (Constant)))))) (via (Test 0))
       (into
        (Switch (match_ (Named (uid (Test 0))))
         (arms
          ((Return (value (Mapn (inputs (Constant))))) (Return (value Constant)))))))
    Optimized shape:
      (Return (value Constant))
    Optimization:
    First Stabilization:
      used
    |}]
;;

(* in order to determine if the match%sub can be folded, the value must be forced. *)
let%expect_test "match%sub on a constant" =
  let c _graph =
    let f () =
      print_endline "here";
      true
    in
    match%sub Bonsai.return () >>| f with
    | false -> Bonsai.return ()
    | true -> Bonsai.return ()
  in
  let _ : _ = create c in
  [%expect
    {|
    Input shape:
      (Sub (from (Return (value (Mapn (inputs (Constant)))))) (via (Test 0))
       (into
        (Sub (from (Return (value (Mapn (inputs ((Named (uid (Test 0)))))))))
         (via (Test 1))
         (into
          (Switch (match_ (Named (uid (Test 1))))
           (arms ((Return (value Constant)) (Return (value Constant)))))))))
    Optimized shape:
      (Return (value Constant))
    Optimization:
      here
    First Stabilization:
    |}]
;;

(* in order to determine if the assoc can be folded, the value must be forced. *)
let%expect_test "assoc on a constant" =
  let c graph =
    let f () =
      print_endline "here";
      Int.Map.empty
    in
    Bonsai.assoc
      (module Int)
      (Bonsai.return () >>| f)
      graph
      ~f:(fun _ _ _ -> Bonsai.return ())
  in
  let _ : _ = create c in
  [%expect
    {|
    Input shape:
      (Sub (from (Return (value (Mapn (inputs (Constant)))))) (via (Test 0))
       (into
        (Assoc (map (Named (uid (Test 0)))) (key_id (Test 1)) (cmp_id (Test 2))
         (data_id (Test 3)) (by (Return (value Constant))))))
    Optimized shape:
      (Return (value Constant))
    Optimization:
      here
    First Stabilization:
    |}]
;;

let%expect_test "regular assoc with a constant inside " =
  let var = Bonsai.Expert.Var.create String.Map.empty in
  let f () = print_endline "here" in
  let c graph =
    Bonsai.assoc
      (module String)
      (Bonsai.Expert.Var.value var)
      graph
      ~f:(fun key data graph ->
        let state, _set_state = Bonsai.state () graph in
        let%arr key
        and () = data
        and () = state
        and () = Bonsai.return () >>| f in
        print_endline key)
  in
  (* When the input map is empty, don't force the lazies *)
  let handle = create c in
  [%expect
    {|
    Input shape:
      (Assoc (map Incr) (key_id (Test 0)) (cmp_id (Test 1)) (data_id (Test 2))
       (by
        (Sub (from Leaf0) (via (Test 3))
         (into
          (Sub (from (Return (value (Mapn (inputs ((Named (uid (Test 3)))))))))
           (via (Test 4))
           (into
            (Sub (from (Return (value (Mapn (inputs ((Named (uid (Test 3)))))))))
             (via (Test 5))
             (into
              (Sub (from (Return (value (Mapn (inputs (Constant))))))
               (via (Test 6))
               (into
                (Return
                 (value
                  (Mapn
                   (inputs
                    ((Named (uid (Test 0))) (Named (uid (Test 2)))
                     (Named (uid (Test 4))) (Named (uid (Test 6))))))))))))))))))
    Optimized shape:
      (Assoc (map Incr) (key_id (Test 0)) (cmp_id (Test 1)) (data_id (Test 2))
       (by
        (Sub (from Leaf0) (via (Test 3))
         (into
          (Sub (from (Return (value (Mapn (inputs ((Named (uid (Test 3)))))))))
           (via (Test 4))
           (into
            (Sub (from (Return (value (Mapn (inputs ((Named (uid (Test 3)))))))))
             (via (Test 5))
             (into
              (Return
               (value
                (Mapn
                 (inputs
                  ((Named (uid (Test 0))) (Named (uid (Test 2)))
                   (Named (uid (Test 4))))))))))))))))
    Optimization:
    First Stabilization:
    |}];
  Bonsai.Expert.Var.set var (String.Map.of_alist_exn [ "a", (); "b", () ]);
  Bonsai_test.Handle.show handle;
  (* Now that there's values flowing through the code, the lazy is forced *)
  [%expect
    {|
    here
    b
    a
    |}];
  Bonsai.Expert.Var.set var (String.Map.of_alist_exn [ "a", (); "b", (); "c", () ]);
  Bonsai_test.Handle.show handle;
  (* Because the lazy is shared between branches, adding a new branch doesn't re-trigger
     the lazy *)
  [%expect {| c |}]
;;

let%expect_test "simple_assoc with a constant inside " =
  let var = Bonsai.Expert.Var.create String.Map.empty in
  let f () = print_endline "here" in
  let c graph =
    Bonsai.assoc (module String) (Bonsai.Expert.Var.value var) graph ~f:(fun key data _ ->
      let%arr key
      and () = data
      and () = Bonsai.return () >>| f in
      print_endline key)
  in
  let handle = create c in
  (* When the input map is empty, don't force the lazies *)
  [%expect
    {|
    Input shape:
      (Assoc (map Incr) (key_id (Test 0)) (cmp_id (Test 1)) (data_id (Test 2))
       (by
        (Sub (from (Return (value (Mapn (inputs (Constant)))))) (via (Test 3))
         (into
          (Return
           (value
            (Mapn
             (inputs
              ((Named (uid (Test 0))) (Named (uid (Test 2)))
               (Named (uid (Test 3))))))))))))
    Optimized shape:
      (Assoc_simpl (map Incr))
    Optimization:
    First Stabilization:
    |}];
  Bonsai.Expert.Var.set var (String.Map.of_alist_exn [ "a", (); "b", () ]);
  Bonsai_test.Handle.show handle;
  (* Now that there's values flowing through the code, the lazy is forced *)
  [%expect
    {|
    here
    a
    b
    |}];
  Bonsai.Expert.Var.set var (String.Map.of_alist_exn [ "a", (); "b", (); "c", () ]);
  Bonsai_test.Handle.show handle;
  (* Because the lazy is shared between branches, adding a new branch doesn't re-trigger
     the lazy *)
  [%expect {| c |}]
;;

let%expect_test "side-effect inside of toggled branch" =
  let var = Bonsai.Expert.Var.create true in
  let c _graph =
    match%sub Bonsai.Expert.Var.value var with
    | true -> Bonsai.return () >>| fun () -> print_endline "true!"
    | false -> Bonsai.return () >>| fun () -> print_endline "false!"
  in
  let handle = create c in
  [%expect
    {|
    Input shape:
      (Sub (from (Return (value (Mapn (inputs (Incr)))))) (via (Test 0))
       (into
        (Switch (match_ (Named (uid (Test 0))))
         (arms
          ((Return (value (Mapn (inputs (Constant)))))
           (Return (value (Mapn (inputs (Constant))))))))))
    Optimized shape:
      (Sub (from (Return (value (Mapn (inputs (Incr)))))) (via (Test 0))
       (into
        (Switch (match_ (Named (uid (Test 0))))
         (arms ((Return (value Constant)) (Return (value Constant)))))))
    Optimization:
    First Stabilization:
      true!
    |}];
  Bonsai.Expert.Var.set var false;
  Bonsai_test.Handle.show handle;
  [%expect {| false! |}]
;;
