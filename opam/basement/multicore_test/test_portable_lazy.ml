open! Base
open Expect_test_helpers_base
open Basement
open Await_sync
module Spin_barrier = Portable_test_helpers.Barrier

let%expect_test "if multiple threads force at the same time, the lazy still always \
                 returns the same value"
  =
  let%with.tilde.stack conc = Concurrent_in_thread.with_blocking Await.Terminator.never in
  let t = Portable_lazy.from_fun (fun () -> Portable_atomic.make 0) in
  let nthreads = 32 in
  Concurrent.with_scope conc () ~f:(fun s ->
    let barrier = Barrier.create nthreads in
    for _ = 1 to nthreads do
      Concurrent.spawn s ~f:(fun _ _ conc ->
        Barrier.await (Concurrent.await conc) barrier;
        Portable_atomic.incr (Portable_lazy.force t))
    done);
  require_equal (module Int) nthreads (Portable_atomic.get (Portable_lazy.force t))
;;

let%expect_test "if one thread forces while another is forcing, that thread blocks" =
  let%with.tilde.stack conc = Concurrent_in_thread.with_blocking Await.Terminator.never in
  let barrier = Spin_barrier.create 2 in
  let t =
    Portable_lazy.from_fun (fun () ->
      Spin_barrier.await barrier;
      (* Allow a context switch if we're running without parallelism enabled *)
      Thread.yield ();
      Portable_atomic.make 0)
  in
  Concurrent.with_scope conc () ~f:(fun s ->
    Concurrent.spawn s ~f:(fun _ _ _ -> Portable_atomic.incr (Portable_lazy.force t));
    Concurrent.spawn s ~f:(fun _ _ _ ->
      Spin_barrier.await barrier;
      Portable_atomic.incr (Portable_lazy.force t)));
  require_equal (module Int) 2 (Portable_atomic.get (Portable_lazy.force t))
;;

let%expect_test "if multiple threads force at the same time, the thunk is only called \
                 once"
  =
  let%with.tilde.stack conc = Concurrent_in_thread.with_blocking Await.Terminator.never in
  let times_force_called = Portable_atomic.make 0 in
  let t = Portable_lazy.from_fun (fun () -> Portable_atomic.incr times_force_called) in
  let nthreads = 32 in
  let barrier = Barrier.create nthreads in
  Concurrent.with_scope conc () ~f:(fun s ->
    for _ = 1 to nthreads do
      Concurrent.spawn s ~f:(fun _ _ conc ->
        Barrier.await (Concurrent.await conc) barrier;
        Portable_lazy.force t)
    done);
  require_equal (module Int) 1 (Portable_atomic.get times_force_called)
;;
