open! Base
open! Await
open Basement
open Capsule.Blocking_sync [@@alert "-deprecated"]
module Atomic = Stdlib.Atomic
module Capsule = Capsule.Expert

external ref : 'a -> 'a ref @@ portable = "%makemutable"
external ( ! ) : 'a ref -> 'a @@ portable = "%field0"
external ( := ) : 'a ref -> 'a -> unit @@ portable = "%setfield0"

let cpu_relax = Stdlib_shim.Domain.cpu_relax

let%expect_test "signal" =
  let%with.tilde.stack conc = Concurrent_in_thread.with_blocking Terminator.never in
  let (P k) = Capsule.create () in
  let mutex = Mutex.create k in
  let cond = Condition.create () in
  let go = Capsule.Data.create (fun () -> ref true) in
  let wait = Atomic.make true in
  Concurrent.with_scope conc () ~f:(fun s ->
    Concurrent.spawn s ~f:(fun _ _ _ ->
      Mutex.with_key mutex ~f:(fun key ->
        Atomic.set wait false;
        let rec loop key =
          let #(res, key) =
            Capsule.Key.access key ~f:(fun access -> !(Capsule.Data.unwrap ~access go))
          in
          match res with
          | true ->
            let key = Condition.wait cond ~mutex key in
            loop key
          | false -> key
        in
        #((), loop key)));
    while Atomic.get wait do
      Await.yield (Concurrent.Spawn.await s)
    done;
    Mutex.with_lock mutex ~f:(fun password ->
      Capsule.Data.iter go ~password ~f:(fun go -> go := false);
      Condition.signal cond))
;;

let%expect_test "broadcast" =
  let%with.tilde.stack conc = Concurrent_in_thread.with_blocking Terminator.never in
  let (P k) = Capsule.create () in
  let mutex = Mutex.create k in
  let cond = Condition.create () in
  let go = Capsule.Data.create (fun () -> ref true) in
  let ready = Atomic.make 0 in
  Concurrent.with_scope conc () ~f:(fun s ->
    for _ = 1 to 4 do
      Concurrent.spawn s ~f:(fun _ _ _ ->
        Mutex.with_key mutex ~f:(fun key ->
          Atomic.incr ready;
          let rec loop key =
            let #(res, key) =
              Capsule.Key.access key ~f:(fun access -> !(Capsule.Data.unwrap ~access go))
            in
            match res with
            | true ->
              let key = Condition.wait cond ~mutex key in
              loop key
            | false -> key
          in
          #((), loop key)))
    done;
    while Atomic.get ready < 4 do
      cpu_relax ()
    done;
    Mutex.with_lock mutex ~f:(fun password ->
      Capsule.Data.iter go ~password ~f:(fun go -> go := false);
      Condition.broadcast cond))
;;

let%expect_test "wait on a poisoned mutex" =
  let (P k) = Capsule.create () in
  let mutex = Mutex.create k in
  let cond = Condition.create () in
  let key = Mutex.destroy mutex in
  try
    let (_ : _ Capsule.Key.t) = Condition.wait cond ~mutex key in
    failwith "Expected Mutex.Poisoned exception, but no exception was raised"
  with
  | Mutex.Poisoned -> [%expect {| |}]
;;

let%expect_test "mutex poisoned during condition wait" =
  let (P k) = Capsule.create () in
  let mutex = Mutex.create k in
  let cond = Condition.create () in
  let poisoned = Atomic.make false in
  let ready = Atomic.make false in
  let going = Atomic.make true in
  (match
     Multicore.spawn
       (fun () ->
         (try
            Mutex.with_key mutex ~f:(fun key ->
              Atomic.set ready true;
              (* Raises [poisoned] after waiting. *)
              let key = Condition.wait cond ~mutex key in
              Capsule.Key.access key ~f:(fun _ : unit -> failwith "can't reach this"))
          with
          | Mutex.Poisoned -> Atomic.set poisoned true);
         Atomic.set going false)
       ()
   with
   | Spawned -> ()
   | Failed ((), exn, bt) -> Exn.raise_with_original_backtrace exn bt);
  while not (Atomic.get ready) do
    cpu_relax ()
  done;
  (* Poison the mutex while the other domain is waiting *)
  let _key = Mutex.destroy mutex in
  Condition.signal cond;
  while Atomic.get going do
    cpu_relax ()
  done;
  [%test_result: bool] (Atomic.get poisoned) ~expect:true;
  [%expect {| |}]
;;
