(*---------------------------------------------------------------------------
  Copyright (c) 2026 Thomas Gazagnaire. All rights reserved.
  SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

(** A view redrawn in place: the live region.

    A display draws a {!View.t} at the bottom of the terminal's normal output,
    its {e region}, and redraws it in place at each frame, in the room of the
    terminal and at the time of the frame ({!Room}), so that its spinners turn
    and the frames of a theme that moves move. Above the region is the
    {e scrollback}, where {!print} writes what is printed once. A display knows
    nothing of tasks: {!Progress.show} makes the display of a root's tasks, and
    [Console_eio.run] runs one.

    The matrix theme's frame moving in a display with no task:

    {[
    let matrix ~now ~size =
      let table =
        Console.Table.view
          [ Console.Table.column (Console.View.text "Name") ]
          [ [ Console.View.text "Alice" ] ]
      in
      Console.Theme.set Fmt.stderr Console.Theme.matrix;
      let d = Console.Display.v ~size ~now Fmt.stderr table in
      Console.Display.redraw d;
      Console.Display.close d
    ]}

    {1:guarantees Guarantees}

    On a terminal of [w] columns and [h] rows:
    - No line the display draws in its region is wider than [w], so none wraps.
    - The region is never taller than [h - 1] lines, so a redraw never scrolls
      the terminal: a view taller than that loses its first lines. A table or a
      panel that this would cut in its height loses all its lines, so that no
      fragment of its frame shows, in a {!View.hcat} too; the lines of a tree go
      one by one.
    - The region starts at the first column of a row of its own, below any line
      the program left unended before the display started.
    - Each change is written in one piece, as one synchronized update (mode
      2026), which a terminal that supports it shows whole. The region is never
      shown half erased or half drawn, and only its rows change between two of
      its frames. On a pipe no such escape is written.
    - A redraw that would change nothing writes nothing, and one where nothing
      the view reads changed computes nothing. A view that reads neither the
      time ({!Room.time}) nor the program's state is drawn again only when it is
      set, when something is printed above it or when the terminal's size
      changes, and one that reads the time also when the time moves. A view of
      the program's state ({!View.fit}), the tasks included, is drawn again at
      every redraw, unless the display is told of each change to it: the display
      of {!Progress.show} is told of each change to its tasks.
    - A line cut to fit is cut between two characters, never inside an escape
      code, and it ends with an ellipsis that turns its style off.
    - After a resize the old frame is erased, counting the rows that a terminal
      which reflows its lines gives it. On one that does not reflow, such as
      xterm or GNU screen, a narrowing below the width of the old lines also
      erases the lines above the region that a reflow would have filled.
    - The cursor is hidden while the display runs, and shown again when it
      closes. A display installs no signal handler, so a program interrupted
      before it closes its display leaves the cursor hidden: [Console_eio.run]
      closes its display on SIGINT and SIGTERM, and a program that runs a
      display itself closes it from its own handler.
    - Every line the display prints ends with ["\r\n"], so a tty with output
      processing off shows the same screen as one with it on.
    - The display is drawn in its formatter's theme ({!Theme.get}), read again
      each time it draws, in colour iff the formatter's style renderer is
      [`Ansi_tty], at its {!Color.val-depth}. A theme's frames move only in
      colour, and stand still in the scrollback.

    Without a terminal there is no region: {!print} writes at once, and the view
    is printed once, as its last frame, when the display closes. Each of these
    lines starts on a line of its own, after a line feed when what the program
    wrote on the same formatter did not end its line. A write error on the
    display's output ends the display's output and nothing else, as when a
    reader closes its pipe in [prog 2>&1 | head -1]. Without a terminal, console
    flushes {!Fmt.stdout} before each line it writes, so that [2>&1] keeps the
    order of the two outputs; a write error on standard output belongs to the
    program, whose own writes meet it as they would without console.

    A view that raises is dropped and nothing is drawn in its place. Its
    exception is raised again by the next call to {!redraw}, {!set_view} or
    {!close}, so that a frame drawn as a task changes or as a line is printed
    raises in none of the program's other calls. {!close} raises it once the
    display closed.

    A display belongs to the domain that runs it, so call its functions from
    that domain alone. *)

type t = Display_core.t
(** The type for displays. *)

val v :
  ?size:(unit -> int * int) ->
  ?delay:float ->
  now:(unit -> float) ->
  Format.formatter ->
  View.t ->
  t
(** [v ~size ~now ppf view] draws [view] on [ppf], timed by the clock [now], in
    seconds.

    - With [size], [ppf] is a terminal of that width and height, which the
      display reads again before each redraw: [view] is redrawn in place, in a
      room of at most [h - 1] lines of [w] cells. A width or a height is taken
      in \[[1];[4096]\]: a larger one is [4096], and a smaller one [1]. The
      region shows once [delay] seconds (defaults to [0.]) passed on [now], so
      that work done sooner draws no region: what {!print} prints goes out at
      once all the same, and {!close} prints the last frame. A negative [delay]
      is [0.]; with [nan] or [infinity] the region never shows, and only
      {!close} draws the view.
    - Without [size], [view] is printed once, still and whole, when the display
      closes. *)

val set_view : t -> View.t -> unit
(** [set_view d view] draws [view] from now on, at once. *)

val print : t -> View.t -> unit
(** [print d v] prints [v] once, above the region, in the scrollback: laid out
    as wide as the terminal, as tall as it is and still, and written at once
    with the region drawn again below it, in one update. Without a terminal it
    is written at once, whole, in turn with the display's other lines. *)

val with_output : t -> (Format.formatter -> 'a) -> 'a
(** [with_output d f] prints what console does not see, such as the text of
    [print_string] or of a child process. It takes the region off the screen,
    calls [f] with the display's formatter, as wide as the terminal, and then
    draws the region again below what [f] printed, also when [f] raises.

    Output that [f] does not end with a line feed is ended, and the region is
    drawn from the first column of the row where [f] left the cursor, on rows it
    erases first.

    The cursor stays hidden while [f] runs, as it is while the region shows: a
    child process that [f] runs on the terminal and that reads from it shows no
    cursor unless it shows one itself. *)

val redraw : t -> unit
(** [redraw d] draws the view again, at the time of the call: a view that reads
    the time ({!Room.time}) moves. [Console_eio.run] calls it ten times a
    second; a program that runs a display without it calls it a few times a
    second. It raises the exception of a view that raised since the last call
    ({!guarantees}). *)

val close : t -> unit
(** [close d] takes the region off the screen, shows the cursor and prints the
    view once more, its last frame, as the scrollback keeps it: still, as wide
    as the terminal, or whole without one, and as tall as it is. A closed
    display ignores what it is told: closing it again, {!redraw}, {!set_view}
    and {!print} do nothing, and [with_output d f] is [f ppf], with [ppf] the
    formatter of {!v}. *)

val pp : t Fmt.t
(** [pp] formats a display for diagnostics, in an unspecified format. *)
