exception Stop

let with_path env f =
  let filename = Filename.temp_file "imap-spool-test-" "" in
  Unix.unlink filename;
  let path = Eio.Path.(Eio.Stdenv.fs env / filename) in
  Fun.protect ~finally:(fun () -> Eio.Path.unlink ~missing_ok:true path)
    (fun () -> f path)

let absent path = Alcotest.(check bool) "spool removed" false
  (Eio.Path.is_file path)

let collision env = with_path env (fun path ->
  Eio.Path.save ~create:(`Exclusive 0o600) path "sentinel";
  (try Spool.with_spool path (fun _ -> ());
       Alcotest.fail "existing file accepted"
   with Eio.Io (Eio.Fs.E (Eio.Fs.Already_exists _), _) -> ());
  Alcotest.(check string) "pre-existing file preserved" "sentinel"
    (Eio.Path.load path))

let success env = with_path env (fun path ->
  let answer = Spool.with_spool path (fun output ->
    Eio.Flow.copy_string "body" output;
    Alcotest.(check string) "callback can consume bytes" "body"
      (Eio.Path.load path);
    42) in
  Alcotest.(check int) "callback result" 42 answer;
  absent path)

let failure env = with_path env (fun path ->
  (try Spool.with_spool path (fun output ->
     Eio.Flow.copy_string "partial" output;
     raise Stop)
   with Stop -> ());
  absent path)

let cancellation env = with_path env (fun path ->
  (try Eio.Switch.run (fun sw ->
     Spool.with_spool path (fun output ->
       Eio.Flow.copy_string "partial" output;
       Eio.Switch.fail sw Stop;
       Eio.Fiber.yield ()))
   with Stop -> ());
  absent path)

let hash_file env = with_path env (fun path ->
  Eio.Path.save ~create:(`Exclusive 0o600) path "abc";
  let length,digest = Spool.hash_file path in
  Alcotest.(check int64) "actual length" 3L length;
  Alcotest.(check string) "known SHA-256"
    "ba7816bf8f01cfea414140de5dae2223b00361a396177a9cb410ff61f20015ad"
    digest)

let () = Eio_main.run (fun env ->
  Alcotest.run "imap-io" ["spool", [
    Alcotest.test_case "streamed hash" `Quick (fun () -> hash_file env);
    Alcotest.test_case "collision preserves sentinel" `Quick (fun () -> collision env);
    Alcotest.test_case "success cleanup" `Quick (fun () -> success env);
    Alcotest.test_case "exception cleanup" `Quick (fun () -> failure env);
    Alcotest.test_case "cancellation cleanup" `Quick (fun () -> cancellation env) ]])
