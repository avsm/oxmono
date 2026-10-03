(* Unit tests for [D10.Lock].

   POSIX [fcntl] record locks are owned per-process, not per-fd. Two
   acquires inside the same OCaml process never contend on each other,
   so meaningful contention tests have to fork a child holder. We do
   the fork *before* [Eio_main.run] in the parent so the child stays
   Eio-free and signals back via a sentinel file. *)

module L = D10.Lock

let acquired_path = "child-acquired.sentinel"

let with_child_holder ~mode ~lockfile f =
  let signal_file = Filename.concat (Filename.dirname lockfile) acquired_path in
  (try Unix.unlink signal_file with Unix.Unix_error _ -> ());
  (* Flush stdout/stderr in case anything has been buffered before fork. *)
  Stdlib.flush stdout;
  Stdlib.flush stderr;
  match Unix.fork () with
  | 0 ->
      (* Child: take the lock directly via Unix.lockf, signal, hold
         until the parent unlinks the sentinel, then exit. *)
      let fd = Unix.openfile lockfile [ Unix.O_RDWR; Unix.O_CREAT ] 0o644 in
      let cmd =
        match mode with `Excl -> Unix.F_LOCK | `Shared -> Unix.F_RLOCK
      in
      Unix.lockf fd cmd 0;
      Out_channel.with_open_text signal_file (fun oc ->
          Printf.fprintf oc "%d\n" (Unix.getpid ()));
      let rec wait_for_release () =
        if not (Sys.file_exists signal_file) then ()
        else begin
          Unix.sleepf 0.05;
          wait_for_release ()
        end
      in
      wait_for_release ();
      Unix.close fd;
      exit 0
  | child_pid ->
      let rec wait_for_signal () =
        if Sys.file_exists signal_file then ()
        else begin
          Unix.sleepf 0.02;
          wait_for_signal ()
        end
      in
      wait_for_signal ();
      let release_child () =
        (try Unix.unlink signal_file with Unix.Unix_error _ -> ());
        let rec reap () =
          match Unix.waitpid [] child_pid with
          | _ -> ()
          | exception Unix.Unix_error (Unix.EINTR, _, _) -> reap ()
        in
        reap ()
      in
      Fun.protect ~finally:release_child (fun () -> f ())

(* -- Tests that don't need a child ------------------------------------ *)

let test_with_lock_runs_body_and_creates_file () =
  Helpers.with_eio_temp_dir ~prefix:"lock_basic" @@ fun ~fs ~clock ~dir _env ->
  let path = Filename.concat dir "x.lock" in
  let ran = ref false in
  L.with_lock ~clock ~fs ~path ~mode:Exclusive (fun _ -> ran := true);
  Alcotest.(check bool) "body executed" true !ran;
  Alcotest.(check bool) "lockfile present" true (Sys.file_exists path)

let test_pid_written_to_lockfile () =
  Helpers.with_eio_temp_dir ~prefix:"lock_pid" @@ fun ~fs ~clock ~dir _env ->
  let path = Filename.concat dir "p.lock" in
  L.with_lock ~clock ~fs ~path ~mode:Exclusive (fun _ ->
      let content = In_channel.with_open_text path In_channel.input_all in
      match int_of_string_opt (String.trim content) with
      | Some p -> Alcotest.(check int) "pid is our own" (Unix.getpid ()) p
      | None -> Alcotest.failf "lockfile contents not an int: %S" content)

let test_release_is_idempotent () =
  Helpers.with_eio_temp_dir ~prefix:"lock_release"
  @@ fun ~fs ~clock ~dir _env ->
  let path = Filename.concat dir "r.lock" in
  L.with_lock ~clock ~fs ~path ~mode:Exclusive (fun t ->
      L.release t;
      L.release t;
      L.release t);
  (* Body returned normally — second/third release didn't raise. *)
  Alcotest.(check bool) "still alive" true true

(* -- Cross-process tests with a forked holder ------------------------- *)

let test_child_excl_blocks_parent_no_wait () =
  Helpers.with_temp_dir ~prefix:"lock_xp_excl" @@ fun dir ->
  let lockfile = Filename.concat dir "x.lock" in
  with_child_holder ~mode:`Excl ~lockfile @@ fun () ->
  Eio_main.run @@ fun env ->
  let fs = Eio.Stdenv.fs env in
  let clock = Eio.Stdenv.clock env in
  let outcome =
    try
      L.with_lock ~clock ~fs ~path:lockfile ~mode:Exclusive ~strategy:No_wait
        (fun _ -> ());
      `Acquired
    with L.Lock_unavailable _ -> `Blocked
  in
  Alcotest.(check string)
    "parent blocked by child" "blocked"
    (match outcome with `Acquired -> "acquired" | `Blocked -> "blocked")

let test_child_shared_blocks_parent_exclusive () =
  Helpers.with_temp_dir ~prefix:"lock_xp_sx" @@ fun dir ->
  let lockfile = Filename.concat dir "sx.lock" in
  with_child_holder ~mode:`Shared ~lockfile @@ fun () ->
  Eio_main.run @@ fun env ->
  let fs = Eio.Stdenv.fs env in
  let clock = Eio.Stdenv.clock env in
  let outcome =
    try
      L.with_lock ~clock ~fs ~path:lockfile ~mode:Exclusive ~strategy:No_wait
        (fun _ -> ());
      `Acquired
    with L.Lock_unavailable _ -> `Blocked
  in
  Alcotest.(check string)
    "exclusive blocked by child's shared" "blocked"
    (match outcome with `Acquired -> "acquired" | `Blocked -> "blocked")

let test_child_shared_allows_parent_shared () =
  Helpers.with_temp_dir ~prefix:"lock_xp_ss" @@ fun dir ->
  let lockfile = Filename.concat dir "ss.lock" in
  with_child_holder ~mode:`Shared ~lockfile @@ fun () ->
  Eio_main.run @@ fun env ->
  let fs = Eio.Stdenv.fs env in
  let clock = Eio.Stdenv.clock env in
  let acquired = ref false in
  L.with_lock ~clock ~fs ~path:lockfile ~mode:Shared ~strategy:No_wait (fun _ ->
      acquired := true);
  Alcotest.(check bool)
    "parent shared coexists with child shared" true !acquired

let test_timeout_fires_under_contention () =
  Helpers.with_temp_dir ~prefix:"lock_xp_timeout" @@ fun dir ->
  let lockfile = Filename.concat dir "t.lock" in
  with_child_holder ~mode:`Excl ~lockfile @@ fun () ->
  Eio_main.run @@ fun env ->
  let fs = Eio.Stdenv.fs env in
  let clock = Eio.Stdenv.clock env in
  let start = Unix.gettimeofday () in
  let raised = ref false in
  (try
     L.with_lock ~clock ~fs ~path:lockfile ~mode:Exclusive
       ~strategy:(L.Block_timeout 0.5) (fun _ -> ())
   with L.Lock_unavailable _ -> raised := true);
  let elapsed = Unix.gettimeofday () -. start in
  Alcotest.(check bool) "Lock_unavailable raised" true !raised;
  Alcotest.(check bool) "waited at least the timeout" true (elapsed >= 0.5);
  Alcotest.(check bool) "didn't wait excessively (< 3s)" true (elapsed < 3.0)

let test_on_wait_fires_under_contention () =
  Helpers.with_temp_dir ~prefix:"lock_xp_onwait" @@ fun dir ->
  let lockfile = Filename.concat dir "w.lock" in
  with_child_holder ~mode:`Excl ~lockfile @@ fun () ->
  Eio_main.run @@ fun env ->
  let fs = Eio.Stdenv.fs env in
  let clock = Eio.Stdenv.clock env in
  let calls = ref 0 in
  let on_wait ~waited_s:_ ~path:_ ~pid:_ = incr calls in
  (try
     L.with_lock ~clock ~fs ~path:lockfile ~mode:Exclusive
       ~strategy:(L.Block_timeout 0.5) ~log_interval_s:0.1 ~on_wait (fun _ ->
         ())
   with L.Lock_unavailable _ -> ());
  Alcotest.(check bool) "on_wait fired ≥ 1 time" true (!calls >= 1)

(* -- Switch-scoped [acquire] / [release] --------------------------------

   POSIX fcntl record locks are owned per-process, so same-process
   probes can't prove "the lock was released" — fork-based contention
   for that lives in the [child_holder] tests above. Here we just
   exercise the [acquire ~sw] entry point smoke-tests-style: the body
   runs, the lock is reported as held, [release] is idempotent. *)

let test_acquire_smoke () =
  Helpers.with_temp_dir ~prefix:"lock_acquire_sw" @@ fun dir ->
  let path = Filename.concat dir "a.lock" in
  Eio_main.run @@ fun env ->
  let fs = Eio.Stdenv.fs env in
  let clock = Eio.Stdenv.clock env in
  Eio.Switch.run @@ fun sw ->
  let t = L.acquire ~sw ~clock ~fs ~path ~mode:Exclusive () in
  Alcotest.(check string) "path accessor" path (L.path t);
  Alcotest.(check bool) "lockfile present" true (Sys.file_exists path);
  L.release t;
  L.release t;
  (* Idempotent. *)
  Alcotest.(check bool) "still alive" true true

let test_acquire_cross_process () =
  Helpers.with_temp_dir ~prefix:"lock_acquire_xp" @@ fun dir ->
  let lockfile = Filename.concat dir "x.lock" in
  with_child_holder ~mode:`Excl ~lockfile @@ fun () ->
  Eio_main.run @@ fun env ->
  let fs = Eio.Stdenv.fs env in
  let clock = Eio.Stdenv.clock env in
  Eio.Switch.run @@ fun sw ->
  let outcome =
    try
      let _t =
        L.acquire ~sw ~clock ~fs ~path:lockfile ~mode:Exclusive
          ~strategy:No_wait ()
      in
      `Acquired
    with L.Lock_unavailable _ -> `Blocked
  in
  Alcotest.(check string)
    "switch-scoped acquire blocked by child holder" "blocked"
    (match outcome with `Acquired -> "acquired" | `Blocked -> "blocked")

let suite =
  ( "lock",
    [
      Alcotest.test_case "with_lock runs body and creates lockfile" `Quick
        test_with_lock_runs_body_and_creates_file;
      Alcotest.test_case "acquire ~sw smoke-tests path/release" `Quick
        test_acquire_smoke;
      Alcotest.test_case "acquire ~sw blocked by foreign holder" `Slow
        test_acquire_cross_process;
      Alcotest.test_case "pid written to lockfile" `Quick
        test_pid_written_to_lockfile;
      Alcotest.test_case "release is idempotent" `Quick
        test_release_is_idempotent;
      Alcotest.test_case "child exclusive blocks parent no_wait" `Slow
        test_child_excl_blocks_parent_no_wait;
      Alcotest.test_case "child shared blocks parent exclusive" `Slow
        test_child_shared_blocks_parent_exclusive;
      Alcotest.test_case "two shared coexist across processes" `Slow
        test_child_shared_allows_parent_shared;
      Alcotest.test_case "Block_timeout fires under real contention" `Slow
        test_timeout_fires_under_contention;
      Alcotest.test_case "on_wait fires while blocked" `Slow
        test_on_wait_fires_under_contention;
    ] )
