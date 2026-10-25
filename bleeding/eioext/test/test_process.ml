let check message condition = if not condition then failwith message
let shell code = [ "/bin/sh"; "-c"; code ]

let main env =
  let process_mgr = Eio.Stdenv.process_mgr env in
  let clock = Eio.Stdenv.mono_clock env in
  let within f =
    Eio.Time.Timeout.run_exn (Eio.Time.Timeout.seconds clock 3.) f
  in
  let capture ?input ?max_stdout ?max_stderr code =
    Eioext.Process.capture ~process_mgr ?input ?max_stdout ?max_stderr
      (shell code)
  in
  let result = within (fun () -> capture "printf output; printf error >&2") in
  check "capture both streams"
    (result.status = `Exited 0
    && result.stdout = "output" && result.stderr = "error");
  let input = String.make (1024 * 1024) 'x' in
  let result = within (fun () -> capture ~input "cat; printf error >&2") in
  check "stdin and both output streams make progress"
    (result.stdout = input && result.stderr = "error");
  check "exact output bound accepted"
    ((within (fun () -> capture ~max_stdout:1 "printf x")).stdout = "x");
  check "zero output bound accepts empty stream"
    ((within (fun () -> capture ~max_stdout:0 "true")).stdout = "");
  let marker = Filename.temp_file "eioext-invalid-limit-" "" in
  Unix.unlink marker;
  Fun.protect
    ~finally:(fun () -> if Sys.file_exists marker then Unix.unlink marker)
    (fun () ->
      let command = "printf started > " ^ Filename.quote marker in
      List.iter
        (fun (stdout, stderr) ->
          (match capture ~max_stdout:stdout ~max_stderr:stderr command with
          | _ -> failwith "invalid bounds accepted"
          | exception Invalid_argument _ -> ());
          check "invalid bounds do not spawn a child"
            (not (Sys.file_exists marker)))
        [ (-1, 10); (10, -1); (max_int, 10); (10, max_int) ]);
  let result =
    within (fun () -> capture "sleep 60 & printf '%s\\n' \"$!\"; exit 0")
  in
  check "leader exit cleans up descendant pipes" (result.status = `Exited 0);
  let result =
    within (fun () -> capture ~input:(String.make 1048576 'x') "exit 7")
  in
  check "closed stdin preserves child status" (result.status = `Exited 7);
  let limit f =
    match within f with
    | _ -> failwith "expected bounded output refusal"
    | exception Eioext.Process.Output_limit 1000 -> ()
  in
  limit (fun () -> capture ~max_stdout:1000 "yes x");
  limit (fun () -> capture ~max_stderr:1000 "yes x >&2");
  (match
     within (fun () ->
         Eio.Time.Timeout.run_exn (Eio.Time.Timeout.seconds clock 0.05)
           (fun () -> capture "sleep 60 & wait"))
   with
  | _ -> failwith "expected cancellation"
  | exception Eio.Time.Timeout -> ());
  (match
     within (fun () ->
         Eioext.Process.with_stdout ~process_mgr (shell "sleep 60 & wait")
           (fun _ -> failwith "original consumer error"))
   with
  | _ -> failwith "expected consumer error"
  | exception Failure message when message = "original consumer error" -> ());
  print_endline "Eioext process bounds, groups and cancellation passed"

let () = Eio_main.run main
