type result = {
  status : Eio.Process.exit_status;
  stdout : string;
  stderr : string;
}

exception Output_limit of int

let find_executable env = function
  | [] -> invalid_arg "empty process arguments"
  | name :: _ when String.contains name '/' -> name
  | name :: _ ->
      let paths =
        Eio.Process.Env.get_opt "PATH" env
        |> Option.value ~default:"/usr/bin:/bin"
        |> String.split_on_char ':'
      in
      let denied = ref false in
      let rec find = function
        | [] ->
            raise
              (Eio.Process.err
                 (if !denied then Eio.Process.Permission_denied name
                  else Eio.Process.Executable_not_found name))
        | directory :: rest -> (
            let path = Filename.concat directory name in
            match
              Unix.access path [ Unix.X_OK ];
              Unix.stat path
            with
            | stat when stat.Unix.st_kind <> Unix.S_DIR -> path
            | _ -> find rest
            | exception Unix.Unix_error (Unix.EACCES, _, _) ->
                denied := true;
                find rest
            | exception Unix.Unix_error ((Unix.ENOENT | Unix.ENOTDIR), _, _) ->
                find rest)
      in
      find paths

let executable ~process_mgr ?env name =
  let env = Option.value env ~default:(Eio.Process.environment process_mgr) in
  Eio_unix.run_in_systhread (fun () -> find_executable env [ name ])

let check_limit limit =
  if limit < 0 || limit = max_int then invalid_arg "invalid output limit"

let bounded limit source =
  try
    let value =
      Eio.Buf_read.take_all (Eio.Buf_read.of_flow source ~max_size:(limit + 1))
    in
    if String.length value > limit then raise (Output_limit limit);
    value
  with Eio.Buf_read.Buffer_limit_exceeded -> raise (Output_limit limit)

let execute ~process_mgr ?env ?input ~stdout ~stderr args =
  let env = Option.value env ~default:(Eio.Process.environment process_mgr) in
  let executable =
    Eio_unix.run_in_systhread (fun () -> find_executable env args)
  in
  Eio.Fiber.check ();
  Eio.Switch.run (fun sw ->
      (* Recover missed SIGCHLD wakeups without reaping another library's
         children or changing the application's signal handler. *)
      Eio.Fiber.fork_daemon ~sw (fun () ->
          let rec poll () =
            Eio_unix.sleep 0.25;
            Eio.Condition.broadcast Eio_unix.Process.sigchld;
            poll ()
          in
          poll ());
      let fd = Eio_unix.Resource.fd in
      let output target inherited =
        match target with
        | None -> (inherited, None)
        | Some consume ->
            let reader, writer = Eio_unix.pipe sw in
            (fd writer, Some (reader, writer, consume))
      in
      let out, output = output stdout Eio_unix.Fd.stdout in
      let err, errors =
        match stderr with
        | None -> (Eio_unix.Fd.stderr, None)
        | Some consume ->
            let reader, writer = Eio_unix.pipe sw in
            (fd writer, Some (reader, writer, consume))
      in
      let stdin, input =
        match input with
        | None -> (Eio_unix.Fd.stdin, None)
        | Some data ->
            let reader, writer = Eio_unix.pipe sw in
            (fd reader, Some (reader, writer, data))
      in
      let child =
        Eio.Cancel.protect (fun () ->
            Eio_unix.Process.spawn_unix ~sw process_mgr ~env ~executable ~pgid:0
              ~fds:
                [
                  (0, stdin, `Blocking); (1, out, `Blocking); (2, err, `Blocking);
                ]
              args)
      in
      let killed = ref false in
      let terminate () =
        if not !killed then (
          killed := true;
          try Unix.kill (-Eio.Process.pid child) Sys.sigkill
          with Unix.Unix_error _ -> ())
      in
      Fun.protect ~finally:terminate (fun () ->
          Option.iter (fun (_, writer, _) -> Eio.Flow.close writer) output;
          Option.iter (fun (_, writer, _) -> Eio.Flow.close writer) errors;
          Option.iter
            (fun (reader, writer, data) ->
              Eio.Flow.close reader;
              Eio.Fiber.fork ~sw (fun () ->
                  Fun.protect
                    ~finally:(fun () -> Eio.Flow.close writer)
                    (fun () ->
                      try Eio.Flow.copy_string data writer
                      with
                      | Eio.Io (Eio.Net.E (Eio.Net.Connection_reset _), _) ->
                        ())))
            input;
          Option.iter
            (fun (reader, _, consume) ->
              Eio.Fiber.fork ~sw (fun () ->
                  consume (reader :> Eio.Flow.source_ty Eio.Resource.t)))
            errors;
          let status, resolved = Eio.Promise.create () in
          Eio.Fiber.fork ~sw (fun () ->
              let value = Eio.Process.await child in
              (* A wrapper may exit while a descendant retains its pipes. Kill
                 that group before waiting for EOF in the stream consumers. *)
              terminate ();
              Eio.Promise.resolve resolved value);
          let value =
            Option.map
              (fun (reader, _, consume) ->
                consume (reader :> Eio.Flow.source_ty Eio.Resource.t))
              output
          in
          (Eio.Promise.await status, value)))

let with_stdout ~process_mgr ?env ?(input = "") ?(max_stderr = 65536) args read
    =
  check_limit max_stderr;
  let stderr = ref "" in
  let status, result =
    execute ~process_mgr ?env ~input ~stdout:(Some read)
      ~stderr:(Some (fun source -> stderr := bounded max_stderr source))
      args
  in
  (status, Option.get result, !stderr)

let capture ~process_mgr ?env ?input ?(max_stdout = 16 * 1024 * 1024)
    ?max_stderr args =
  check_limit max_stdout;
  let status, stdout, stderr =
    with_stdout ~process_mgr ?env ?input ?max_stderr args (bounded max_stdout)
  in
  { status; stdout; stderr }

let run ~process_mgr ?env ?stdout ?stderr args =
  let copy sink source = Eio.Flow.copy source sink in
  let status, _ =
    execute ~process_mgr ?env ~stdout:(Option.map copy stdout)
      ~stderr:(Option.map copy stderr) args
  in
  match status with
  | `Exited 0 -> ()
  | status -> raise (Eio.Process.err (Eio.Process.Child_error status))
