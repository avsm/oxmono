(* The refresh waits on the display's own clock, the one its rows read, so a
   context whose clock a caller moves by hand paces the refresh as well. *)
let animate ~sw display =
  let wait = Console.Display.wait display in
  let running = ref true in
  Eio.Fiber.fork_daemon ~sw (fun () ->
      let rec loop () =
        wait 0.12;
        if !running then begin
          Console.Display.tick display;
          loop ()
        end
        else `Stop_daemon
      in
      loop ());
  fun () -> running := false

let active = Atomic.make false

let event_level : Logs.level -> Console.Display.Event.level = function
  | Logs.Warning -> `Warning
  | Logs.Error -> `Error
  | Logs.App | Logs.Info | Logs.Debug -> `Info

let reporter display =
  let report _src level ~over k msgf =
    msgf (fun ?header:_ ?tags:_ fmt ->
        Fmt.kstr
          (fun message ->
            if message <> "" then
              Console.Display.event ~level:(event_level level) display message;
            over ();
            k ())
          fmt)
  in
  { Logs.report }

let run ~sw ~ctx ?mode ?theme ?palette ?bar ?width ?height ?header f =
  if not (Atomic.compare_and_set active false true) then
    invalid_arg "Console_eio.run: another display is active";
  Fun.protect
    (fun () ->
      Console.Display.run ~ctx ?mode ?theme ?palette ?bar ?width ?height ?header
        (fun display ->
          let previous = Logs.reporter () in
          Logs.set_reporter (reporter display);
          Fun.protect
            (fun () ->
              let stop = animate ~sw display in
              Fun.protect (fun () -> f display) ~finally:stop)
            ~finally:(fun () -> Logs.set_reporter previous)))
    ~finally:(fun () -> Atomic.set active false)
