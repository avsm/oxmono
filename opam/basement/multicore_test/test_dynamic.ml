open! Base
open! Expect_test_helpers_base
open Basement
open Await

let%expect_test "set_root from two threads" =
  let%with.tilde.stack conc = Concurrent_in_thread.with_blocking Terminator.never in
  let dynamic = Dynamic.make 0 in
  let barrier = Barrier.create 2 in
  let result1 : Sexp.t Stdlib.Atomic.t = Stdlib.Atomic.make (Sexp.Atom "start") in
  let result2 : Sexp.t Stdlib.Atomic.t = Stdlib.Atomic.make (Sexp.Atom "start") in
  Concurrent.with_scope conc () ~f:(fun s ->
    Concurrent.spawn s ~f:(fun _ _ conc ->
      Barrier.await (Concurrent.await conc) barrier;
      let before_d1_set = Dynamic.get dynamic (* 0 *) in
      Dynamic.set_root dynamic 1;
      let after_d1_set = Dynamic.get dynamic (* 1 *) in
      Barrier.await (Concurrent.await conc) barrier;
      Barrier.await (Concurrent.await conc) barrier;
      let after_d2_set = Dynamic.get dynamic (* 1 *) in
      Stdlib.Atomic.set
        result1
        [%message (before_d1_set : int) (after_d1_set : int) (after_d2_set : int)]);
    Concurrent.spawn s ~f:(fun _ _ conc ->
      let before_d1_set = Dynamic.get dynamic (* 0 *) in
      Barrier.await (Concurrent.await conc) barrier;
      Barrier.await (Concurrent.await conc) barrier;
      let after_d1_set = Dynamic.get dynamic (* 0 *) in
      Dynamic.set_root dynamic 2;
      Barrier.await (Concurrent.await conc) barrier;
      let after_d2_set = Dynamic.get dynamic (* 2 *) in
      Stdlib.Atomic.set
        result2
        [%message (before_d1_set : int) (after_d1_set : int) (after_d2_set : int)]));
  let results_from_d1 = Stdlib.Atomic.get result1 in
  let results_from_d2 = Stdlib.Atomic.get result2 in
  let value_in_initial_thread = Dynamic.get dynamic in
  print_s
    [%message
      (results_from_d1 : Sexp.t)
        (results_from_d2 : Sexp.t)
        (value_in_initial_thread : int)];
  [%expect
    {|
    ((results_from_d1 (
       (before_d1_set 0)
       (after_d1_set  1)
       (after_d2_set  2)))
     (results_from_d2 (
       (before_d1_set 0)
       (after_d1_set  1)
       (after_d2_set  2)))
     (value_in_initial_thread 2))
    |}]
;;

let%expect_test "with_temporarily from two threads" =
  let%with.tilde.stack conc = Concurrent_in_thread.with_blocking Terminator.never in
  let dynamic = Dynamic.make 0 in
  let barrier = Barrier.create 2 in
  let result1 = Stdlib.Atomic.make (0, 0) in
  let result2 = Stdlib.Atomic.make (0, 0) in
  Concurrent.with_scope conc () ~f:(fun s ->
    let spawn_thread_with_value_with_temporarily_to
      (result : (int * int) Stdlib.Atomic.t)
      v
      =
      Concurrent.spawn s ~f:(fun _ _ conc ->
        Barrier.await (Concurrent.await conc) barrier;
        let value_in_with_temporarily =
          Dynamic.with_temporarily dynamic v ~f:(fun () -> Dynamic.get dynamic)
        in
        let value_after_with_temporarily = Dynamic.get dynamic in
        Stdlib.Atomic.set result (value_in_with_temporarily, value_after_with_temporarily))
    in
    spawn_thread_with_value_with_temporarily_to result1 1;
    spawn_thread_with_value_with_temporarily_to result2 2 [@nontail]);
  let value_in_with_temporarily_from_d1, value_after_with_temporarily_from_d1 =
    Stdlib.Atomic.get result1
  in
  let value_in_with_temporarily_from_d2, value_after_with_temporarily_from_d2 =
    Stdlib.Atomic.get result2
  in
  let value_in_initial_thread = Dynamic.get dynamic in
  print_s
    [%message
      (value_in_with_temporarily_from_d1 : int)
        (value_after_with_temporarily_from_d1 : int)
        (value_in_with_temporarily_from_d2 : int)
        (value_after_with_temporarily_from_d2 : int)
        (value_in_initial_thread : int)];
  [%expect
    {|
    ((value_in_with_temporarily_from_d1    1)
     (value_after_with_temporarily_from_d1 0)
     (value_in_with_temporarily_from_d2    2)
     (value_after_with_temporarily_from_d2 0)
     (value_in_initial_thread              0))
    |}]
;;
