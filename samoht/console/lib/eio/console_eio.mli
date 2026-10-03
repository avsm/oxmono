(** Eio integration for terminal-aware Console rendering. *)

module Prompt = Prompt
(** Interactive prompts over Eio flows: a yes/no, a line, or a typed word. *)

val run :
  clock:float Eio.Time.clock_ty Eio.Resource.t ->
  ?ppf:Format.formatter ->
  ?mode:Console.Display.mode ->
  ?theme:Console.Theme.t ->
  ?palette:Console.Color.t list ->
  ?bar:Console.Display.Line.t ->
  ?width:int ->
  ?height:int ->
  ?header:string ->
  (Console.Display.t -> 'a) ->
  'a
(** [run ~clock f] is the Eio driver for {!Console.Display.run} on the terminal.
    It constructs the terminal context, whose clock is [clock] for both its
    reading and its waits, scopes the refresh fiber and Logs reporter, and
    passes the ordinary {!Console.Display.t} to [f]. All task operations are in
    {!Console.Display}; this module adds no parallel display API.

    Calls compose: when [run] or {!run_on} is called from inside an existing
    scope of either in the same fiber tree, [f] receives the existing display.
    Only the outermost call owns terminal configuration and teardown. This lets
    a library expose a complete Console operation without forcing its caller to
    coordinate display ownership.

    [ppf] selects the output formatter and defaults to stdout. Unix terminal
    dimensions and tty detection are automatic: the width is the terminal's
    columns, and when stdout is no terminal it is [$COLUMNS] if that names more
    than 10 columns and 80 otherwise; the height is likewise the terminal's
    rows, [$LINES] above 2, or 24. {!Console.Display.with_output} hands that
    width to a formatter-based widget as its margin. Cursor visibility is
    restored on return, exception and SIGINT.

    [CONSOLE_FROZEN_CLOCK], set in the environment to any non-empty value, stops
    the display clock's reading at zero, so every elapsed time renders as
    [0.0s]; the refresh still waits on [clock]. A test that captures this output
    would otherwise record how fast the machine that ran it was, and fail on a
    slower one. It replaces the reading [run] builds for itself and only that
    one: a context handed to {!run_on} has already said what time the display
    reads. *)

val run_on :
  ctx:Console.Display.ctx ->
  ?mode:Console.Display.mode ->
  ?theme:Console.Theme.t ->
  ?palette:Console.Color.t list ->
  ?bar:Console.Display.Line.t ->
  ?width:int ->
  ?height:int ->
  ?header:string ->
  (Console.Display.t -> 'a) ->
  'a
(** [run_on ~ctx f] is {!run} on the context [ctx], so the formatter, the
    display clock and its waits, the geometry and the terminal answer are all
    the caller's, and the refresh waits on [ctx]'s clock. It configures and
    restores no terminal, because a caller that says which surface it draws on
    has taken that ownership, and the surface it named need not be a terminal at
    all. It is how a command grades its own live region -- a pinned geometry and
    a clock the caller moves make the frames something a diff can hold --
    without going around the entry point it ships.

    @raise Invalid_argument if [ctx] was built without [~wait]. *)

val setup : ?style_renderer:Fmt.style_renderer -> unit -> unit
(** [setup ~style_renderer ()] configures the process's terminal output once,
    before anything is drawn:
    - the style renderer of {!Fmt.stdout} and {!Fmt.stderr} is
      {!Console.style_renderer} of [style_renderer] (the value of
      [Fmt_cli.style_renderer]'s [--color] option), the environment and whether
      that output is a terminal, so [--color], [NO_COLOR] and [TERM] are
      honoured the same way by every command;
    - {!Console.Color.val-depth} is {!Console.Color.depth_of_env} of the
      environment, so a colour a terminal cannot show is drawn as its nearest
      one. *)

val is_tty : unit -> bool
(** [is_tty ()] is [true] iff stdout is connected to a terminal. It is the
    terminal question answered once for every CLI that renders through this
    library: a command choosing a table over a stream of plain records asks the
    console it already draws on, rather than asking Unix itself. *)

val dimensions : unit -> int * int
(** [dimensions ()] is the terminal size as [(columns, rows)]: the [TIOCGWINSZ]
    ioctl, else [$COLUMNS] and [$LINES], else 80 by 24. It always answers, and
    no source of an answer reports less than a single cell in either direction.
*)
