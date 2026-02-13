(* domain_safe.ml -- Domain-safe chdb usage patterns

   Demonstrates two layers of concurrency safety:

   1. Mutex-protected Connection (runtime safety):
      Connection.with_handle acquires a mutex so multiple threads
      can safely share a connection.  The C stubs release the
      domain lock during queries so other domains make progress.

   2. Capsule Safe API (compile-time + runtime safety):
      Safe.create wraps the connection with a Capsule_blocking_sync
      mutex.  Safe.with_lock provides a ['k Password.t @ local]
      that must be threaded through every operation.  The password
      cannot escape the lock scope (void layout, local mode) and
      its ['k] brand ties it to one specific connection. *)

open Chdb

(* --- Part 1: Mutex-protected connection with threads --- *)

let mutex_example () =
  Printf.printf "=== Mutex-Protected Connection ===\n";
  Connection.with_connection ~f:(fun conn ->
    (* Spawn a thread that queries concurrently.
       Connection.with_handle acquires the mutex internally,
       so multiple threads can safely share a connection.
       The C stubs release the domain lock during queries
       so other domains/threads can make progress. *)
    let thread_result = ref "" in
    let t =
      Thread.create (fun () ->
        thread_result :=
          Query.execute_string conn ~format:TSV
            "SELECT 'from_thread' AS source, 42 AS value") ()
    in
    let main_result =
      Query.execute_string conn ~format:TSV
        "SELECT 'from_main' AS source, 99 AS value"
    in
    Thread.join t;
    (* Both results are valid -- order depends on scheduling *)
    let results =
      List.sort String.compare [main_result; !thread_result]
    in
    List.iter print_string results
  ) ()

(* --- Part 2: Capsule Safe API --- *)

let safe_example () =
  Printf.printf "\n=== Branded Safe API ===\n";
  let (Safe.Pack conn) = Safe.create () in
  (* Safe.with_lock acquires the capsule mutex and provides a
     branded password.  The password has void layout and is local,
     so it cannot escape with_lock.  All query results are consumed
     inside the lock scope. *)
  Safe.with_lock conn ~f:(fun password ->
    let result =
      Safe.query_string conn ~password ~format:TSV
        "SELECT number, number * number AS sq FROM numbers(5)"
    in
    print_string result);
  (* DDL / side-effecting queries *)
  Safe.with_lock conn ~f:(fun password ->
    Safe.execute_ignore conn ~password
      "SELECT 1");
  Printf.printf "execute_ignore: ok\n";
  (* Clean up *)
  Safe.with_lock conn ~f:(fun password ->
    Safe.close conn ~password);
  Printf.printf "close: ok\n"

let () =
  mutex_example ();
  safe_example ()
