module I = Siftbot.Import
module D = Console.Display

let interruptible ~clock run =
  let interrupted = Atomic.make false in
  let previous =
    Sys.Safe.signal Sys.sigint
      (Sys.Signal_handle (fun _ -> Atomic.set interrupted true))
  in
  let rec watch () =
    Eio.Time.sleep clock 0.05;
    if Atomic.get interrupted then raise Sys.Break;
    watch ()
  in
  Fun.protect
    ~finally:(fun () -> Sys.Safe.set_signal Sys.sigint previous)
    (fun () -> Eio.Fiber.first run watch)

let label = function
  | I.Listing -> "Importing emails"
  | Hydrating -> "Loading thread context"
  | Catching_up | Idle -> "Checking new mail"
  | Reconciling -> "Checking existing emails"

let with_display ~clock ~enabled render =
  let is_tty = enabled && Unix.isatty Unix.stderr in
  let dimensions () =
    try
      let size = Eio_unix.Pty.get_window_size Eio_unix.Fd.stderr in
      if size.cols > 0 && size.rows > 0 then (min 100 size.cols, size.rows)
      else Console_eio.dimensions ()
    with Unix.Unix_error _ -> Console_eio.dimensions ()
  in
  let ctx =
    D.ctx ~ppf:Format.err_formatter
      ~now:(fun () -> Eio.Time.now clock)
      ~wait:(Eio.Time.sleep clock) ~dimensions ~is_tty ()
  in
  let bar =
    D.Line.(
      v
        [
          spinner;
          text " ";
          label;
          text " ";
          bar 18;
          text " ";
          count;
          text " ";
          percent;
          text " ";
          elapsed;
        ])
  in
  interruptible ~clock (fun () ->
      Console_eio.run_on ~ctx
        ~mode:(if is_tty then `Activity else `History)
        ~bar
        (fun display -> render ~transient:is_tty display))

let sync ~clock ~enabled work =
  with_display ~clock ~enabled (fun ~transient display ->
      let current = ref None in
      let on_progress (p : I.progress) =
        let task, threads =
          match !current with
          | Some (phase, task, threads, listed)
            when phase = p.phase && p.listed >= listed ->
              (task, threads)
          | previous ->
              Option.iter
                (fun (phase, task, _, _) ->
                  if phase = p.phase then
                    D.cancel ~message:"Mailbox changed; restarting listing" task
                  else D.succeed task)
                previous;
              let task = D.task ~transient display (label p.phase) in
              (task, p.pending_threads)
        in
        current := Some (p.phase, task, threads, p.listed);
        (match p.phase with
        | Listing ->
            D.set_label task
              (Printf.sprintf "Importing emails (%d listed)" p.listed);
            Option.iter
              (fun total -> D.set_count task ~cur:p.listed ~total)
              p.total
        | Hydrating ->
            D.set_count task
              ~cur:(max 0 (threads - p.pending_threads))
              ~total:threads
        | Catching_up | Reconciling | Idle -> ());
        D.tick display
      in
      let finish f = Option.iter (fun (_, task, _, _) -> f task) !current in
      match work ~on_progress with
      | I.Complete ->
          finish (D.succeed ~message:"Sync complete");
          I.Complete
      | Paused ->
          finish (D.cancel ~message:"Step limit reached; progress saved");
          I.Paused
      | exception exn ->
          let trace = Printexc.get_raw_backtrace () in
          (match exn with
          | Sys.Break | Eio.Cancel.Cancelled _ ->
              finish (D.cancel ~message:"Sync interrupted; progress saved")
          | _ -> finish (D.fail ~message:"Sync failed; progress saved"));
          Printexc.raise_with_backtrace exn trace)

let classify ~clock ~enabled work =
  with_display ~clock ~enabled (fun ~transient display ->
      let task =
        D.task ~transient ~log_lines:2 display "Preparing classification"
      in
      let remaining = ref 0 in
      D.tick display;
      let single_line text =
        Siftbot_model.sanitize text
        |> String.map (function '\n' -> ' ' | c -> c)
      in
      let on_classified ~subject assignments =
        let labels =
          List.map
            (fun (a : Siftbot_model.assignment) ->
              Siftbot_model.dimension a.dimension ^ "=" ^ single_line a.value)
            assignments
          |> String.concat "  "
        in
        D.log ~keep:(not transient) task (labels ^ "\n" ^ single_line subject);
        D.tick display
      in
      let on_progress (p : Siftbot.Classifier.progress) =
        remaining := p.remaining;
        D.set_label task "Classifying emails";
        D.set_count task ~cur:p.completed ~total:p.total;
        D.tick display
      in
      match work ~on_progress ~on_classified with
      | count ->
          D.succeed
            ~message:
              (if count = 0 && !remaining = 0 then "Classification up to date"
               else Printf.sprintf "Classified %d emails" count)
            task;
          (count, !remaining)
      | exception exn ->
          let trace = Printexc.get_raw_backtrace () in
          (match exn with
          | Sys.Break | Eio.Cancel.Cancelled _ ->
              D.cancel
                ~message:"Classification interrupted; completed results saved"
                task
          | _ ->
              D.fail ~message:"Classification failed; completed results saved"
                task);
          Printexc.raise_with_backtrace exn trace)
