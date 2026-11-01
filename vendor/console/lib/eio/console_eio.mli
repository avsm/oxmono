(*---------------------------------------------------------------------------
  Copyright (c) 2026 Thomas Gazagnaire. All rights reserved.
  SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

(** Console with {{:https://github.com/ocaml-multicore/eio}Eio}.

    [Console_eio] shows the tasks of a program ({!Console.Task}) on the
    terminal. {!run}:

    - makes the root of the work and passes it to the program;
    - finds out whether the output is a terminal, and its size, and shows the
      tasks as {!Console.Progress.show} does;
    - redraws the progress rows ten times a second;
    - gives the current task the program's output and its {!Logs} messages;
    - gives the terminal back as it found it when the run ends, when the program
      exits and when SIGINT or SIGTERM ends it ({{!signals}signals and exits}).

    {1:output Where text goes}

    A task's output shows under it while it runs, and stays if the task fails.

    - Outside a task, output goes above the rows.
    - On a pipe, each line is printed at once, after its task's path
      ({!Console.Progress.design}).
    - [Task.log] gives a task lines from where it is not current, such as a
      fiber that reads a child process.

    Output is everything the program prints while a task is current:

    - its [Logs] messages ({{!logs}Logs messages});
    - its [Task.log] lines;
    - the lines of {!Fmt.stdout} and {!Fmt.stderr} that {!run} takes.

    Of what [run] takes, bytes belong to the task current when they are written.
    A task's end flushes {!Fmt.stdout} and {!Fmt.stderr}, so bytes that another
    fiber printed there without flushing them go to whichever task ends first.
    Each task collects its own lines, so a line is what one task wrote up to a
    line feed, and a task's unended last line is ended when the task ends. Bytes
    written outside any task are the output of no task, ended when the display
    closes.

    Text that [print_string], [Printf] or a child process writes on the terminal
    goes past the display and garbles the rows, so print it with {!with_output}.
*)

module Prompt = Prompt
(** Questions asked on the terminal: a line, a yes or no, or a password. *)

val run :
  clock:_ Eio.Time.Mono.t ->
  ?ppf:Format.formatter ->
  ?progress:Console.Progress.t ->
  ?layout:Console.Progress.Layout.t ->
  ?label:string ->
  ?view:(Console.Task.t -> Console.View.t) ->
  ?keep_ended:bool ->
  ?commit:(Console.Task.t -> Console.View.t option) ->
  ?plain:(Console.Task.Event.t -> Console.View.t option) ->
  ?delay:float ->
  (Console.Task.t -> 'a) ->
  'a
(** [run ~clock f] is [f root], for the root of the work, shown on [ppf] until
    [f] ends. The root is what {!Console.Task.run} makes of a task: it succeeds
    when [f] returns, fails when [f] raises and is cancelled when its fiber is,
    unless [f] ended it ({!Console.Task.succeed}), and its ending ends the tasks
    still running.

    - [clock] is a monotonic clock, such as [Eio.Stdenv.mono_clock env]. The
      elapsed times, the rates, the spinners and the motion of a view read it,
      and the redraws wait on it, so a change of the system's time moves none of
      them. A test passes an [Eio_mock.Clock.Mono] clock and moves it by hand.
    - [ppf] is where the tasks show (defaults to {!Fmt.stderr}, so that standard
      output stays the program's), in [ppf]'s theme ({!Console.Theme.get}).
    - [label] names the work on the summary line ({!Console.Task.root}).
    - [view] is applied to the root, and [view root] is the [view] of
      {!Console.Progress.show}; [keep_ended], [commit], [plain], [delay] and
      [layout] are [show]'s too.
    - [progress] says how progress shows ({!Console.Progress}, defaults to
      [`Auto]). With [`Tty] the rows take the size of the terminal behind the
      output, if there is one, else [$COLUMNS] by [$LINES], each when it is
      between [1] and [4096], else 80 by 24.

    [run] first calls {!Console_tty.ensure}, so that a standard output on which
    the program chose nothing gets the colours its terminal and environment
    allow.

    The rows are redrawn in place when [ppf] is {!Fmt.stdout} or {!Fmt.stderr},
    that output is a terminal whose [TERM] is set and not [dumb], and [progress]
    does not ask for lines. What the program then prints on {!Fmt.stdout} or
    {!Fmt.stderr} on that terminal is taken a whole line at a time ({!output}).
    A line longer than 64 KiB is taken in pieces as it is written, each cut
    between two characters and, unless an escape code holds the whole piece,
    outside an escape code. What it prints on another output goes out as it is
    written.

    - With [`Tty] on another output, what the program prints on [ppf] itself is
      taken in the same way.
    - With [`Quiet] or [`Json], what the program prints on {!Fmt.stdout} or
      {!Fmt.stderr}, when it writes where [ppf] does, as with [2>&1], is taken
      in the same way. With [`Quiet] the output of a task that succeeds is not
      printed, and with [`Json] each line is an [output] event, so that every
      line of [ppf] is an event.

    The display closes once the work ended, and an exception from [f] is raised
    again. A fiber cancelled inside a {!Console.Task.run} cancels its task. A
    [view] that raises is dropped for the default one, and [run] raises its
    exception: at once, cancelling [f], when a redraw meets it, and once the
    display closed otherwise, unless [f] raised first.

    The sections below say what [run] does with {{!logs}Logs messages},
    {{!signals}signals and exits}, {{!nested}nested runs} and a
    {{!deterministic}deterministic clock}.

    @raise Invalid_argument if a run starts in another domain while one runs. *)

(** {1:running Running a display}

    {2:logs Logs messages}

    While [f] runs, {!Logs} messages go to the display.

    - A message is the output of the task current in the calling fiber: the
      innermost {!Console.Task.run} or {!Console.Task.within} of that fiber or
      of the fibers it was forked from. With no such task, it goes above the
      rows.
    - A message from a source other than {!Logs.default} comes after the
      source's name in brackets, as in [[http] connected].
    - A message logged in another domain that Eio runs, such as one of an
      [Eio.Executor_pool], is passed to the domain that runs [f]. A worker does
      not inherit the task of the fiber that submitted its work, so the message
      goes above the rows unless the worker made a task current with
      {!Console.Task.within}. The tasks and the display belong to that domain
      alone.

    A message the display cannot take goes to the reporter installed before
    [run], so that a log call never fails. Such a message is one of:
    - a message logged after the display closed, through a reporter kept from
      [f];
    - a message still waiting for the display when it closes;
    - a message from a domain that Eio does not run.

    That reporter is put back when [f] ends, and when the program exits during
    [f], so what an [at_exit] function logs after the display closed goes to it.

    {2:signals Signals and exits}

    The display closes when the program exits during [f], and signals left at
    their default disposition are handled as follows.

    - SIGINT or SIGTERM during [f]: the display closes once the frame it is
      writing, if any, is written, then the signal ends the program as it would
      have. If that write does not end within a second, the signal ends the
      program with the frame as it is.
    - SIGTSTP (Ctrl-Z): the rows are taken off the terminal and the cursor is
      shown before the program stops, and the rows are drawn again once [fg]
      continues it. In a process group that no shell controls, where the stop
      does not take effect, the rows are drawn again at once and the program
      goes on.

    A signal the program handles or ignores is left to it.

    A closed pipe on [ppf] ends the display's output and nothing else, while one
    on standard output is met by the program's own writes, as without console
    ({!Console.Display}).

    {2:nested Nested runs}

    A [run] called while another runs in the same domain passes its own function
    the task current where it is called, or the root of the run: so a library
    that calls [run] adds its tasks under the task of its caller. This holds
    inside [f], in a fiber that [f] started and in a fiber beside it, so a
    library can call [run] without knowing whether its caller already did.

    - The [clock], [ppf], [layout], [label], [view], [keep_ended], [commit],
      [plain], [delay] and [progress] of the inner [run] are ignored, since the
      display keeps those of the first [run], and the inner [run] ends no task.
    - The display closes once the last of their functions ends, and the first
      [run] waits for the others.
    - A [run] that starts while that display closes opens its own once it is
      gone. So does one from a fiber that outlived the display it was started
      under, unless another display runs then, which it joins.

    {2:deterministic A deterministic clock}

    [CONSOLE_DETERMINISTIC_CLOCK], set in the environment to a value other than
    the empty string, makes the time the display reads depend on the program's
    sleeps alone, so that the progress a test captures from the program reads
    the same on every machine.

    On a clock that {!clock} made, the time is virtual, as on a mock clock, and
    never the real time. It starts at zero, and a sleep that starts at time [t]
    for [d] wakes at [t + d]: sleeps at once overlap. Sleeps wake one at a time,
    in the order of their wake-up times, those due at once in the order they
    started, and the clock reads the wake-up time of the fiber that woke last,
    while it and the fibers it makes runnable run. Each sleep also lasts at
    least [d] on the clock behind it, so the rows move as the program runs, and
    one whose real duration ended waits for those due before it. What the
    program does between two sleeps, and how long that takes, then does not
    change the times it shows or the [time] and [elapsed] of its
    {{!Console.Task.json}JSON events}, unless that work itself waits, on input
    or output for example: a sleep can then wake in the meantime and move the
    time from which the next sleep starts.

    The display redraws at the instants of the virtual time, every tenth of a
    second while the program sleeps, and never moves it: on a terminal, the
    frames it draws are the same on every run.

    [Eio.Time.with_timeout] on such a clock times out once its duration has
    passed on the clock behind it and the sleeps due before it have woken, and a
    timeout that fires moves the time as a sleep does. Under [CONSOLE_CAST], it
    times out as soon as the sleeps due before it have woken.

    On any other clock the time stays at zero:
    - every elapsed time reads [0.0s], and every [time] and [elapsed] of a JSON
      event reads [0];
    - no rate shows;
    - the spinners and the motion stand still;
    - the display redraws on that clock.

    [CONSOLE_CAST], set to a file name, records the run there instead of on its
    output, with the clock of [CONSOLE_DETERMINISTIC_CLOCK] whether that is set
    or not: what it writes on [ppf], and on both standard outputs when [ppf] is
    one, from then until the program exits, is an
    {{:https://docs.asciinema.org/manual/asciicast/v2/}asciicast v2} recording
    of a terminal of [$COLUMNS] by [$LINES] (80 by 24 without them), with the
    rows drawn by [`Auto] too, each write at its time on the clock, and each
    line feed after a carriage return, as a terminal's driver writes it. The
    program's sleeps then take no real time, so that a recording takes none,
    while [CONSOLE_DETERMINISTIC_CLOCK] alone keeps the real pace that a
    recorder of the terminal, such as [k7 rec], films. A file that cannot be
    opened ends the program with an error message and exit status [1], before it
    runs. {!Console_tty.setup} starts the recording too, so that what a program
    prints before a run, or without one, is recorded at time zero. *)

val with_output : (Format.formatter -> 'a) -> 'a
(** [with_output f] prints what console does not see, such as the text of
    [print_string] or of a child process, as {!Console.Display.with_output} does
    for the display of the run of the calling fiber: [f] is called with the
    display's formatter, the region off the screen. Outside a run, it is
    [f Fmt.stderr]. *)

val print : Console.View.t -> unit
(** [print v] prints [v] above the rows, in their scrollback, as
    {!Console.Display.print} does for the display of the run of the calling
    fiber: styled and as wide as the terminal, or whole on a pipe, where it
    comes in turn with the lines of the tasks. With [`Json] each of its lines,
    without escape codes, is an [output] event of no task. Outside a run, it is
    printed on {!Fmt.stderr}, at its margin. *)

val size : Format.formatter -> (unit -> int * int) option
(** [size ppf] is [Some size] when [ppf] is {!Fmt.stdout} or {!Fmt.stderr} and
    that output is a terminal whose [TERM] is set and not [dumb], as {!run}
    finds out, and [None] otherwise. [size ()] is the terminal's width and
    height, read again at each call, which is what {!Console.Display.v} takes:

    {[
    let display ~now ppf view =
      Console.Display.v ?size:(Console_eio.size ppf) ~now ppf view
    ]} *)

val clock : _ Eio.Time.Mono.t -> Eio.Time.Mono.ty Eio.Resource.t
(** [clock c] is [c], or, under {{!deterministic}[CONSOLE_DETERMINISTIC_CLOCK]}
    or [CONSOLE_CAST], a virtual clock that starts at zero and moves to the time
    each sleep on it wakes at ({!deterministic}). A sleep until [t] on it lasts
    at least the time from its clock's time to [t] on [c], so the elapsed times,
    the rates and the spinners of a display that {!run} draws on it follow the
    program's sleeps. *)
