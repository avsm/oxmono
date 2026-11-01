(*---------------------------------------------------------------------------
  Copyright (c) 2026 Thomas Gazagnaire. All rights reserved.
  SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

(** How tasks show: rows, lines, failures alone or JSON.

    The tasks of a {!Task.root} show as rows redrawn in place, as lines printed
    in turn, as only what fails, or as JSON events, the choices that the
    [--progress] option of [docker build] gives. The rows are ordinary views:
    {!row} is the row of a task, {!tree} the tree of rows of a task and those
    under it, drawn with {!Tree.view}, {!summary} the line that says how far the
    work is, and {!view} all of them. {!show} connects a root to a {!Display} in
    a mode, and [Console_eio.run] does it for a program.

    The {{!design}design} of the rows and the {{!guarantees}guarantees} they
    keep close this page. *)

type t =
  [ `Auto
    (** The rows are redrawn in place on a terminal, and lines are printed in
        turn otherwise. *)
  | `Tty
    (** The rows are redrawn in place on any output, such as a pipe that a
        terminal recorder reads. *)
  | `Plain
    (** Lines are printed in turn, as on a pipe. A task prints a line when it
        starts, its output at once and a line when it ends, each naming the task
        by its path ({!design}). The view comes last, once: by default the
        summary line. The lines are in colour if the output has colour. *)
  | `Quiet
    (** Only what fails is printed: a task that fails or is cancelled prints its
        line, which names its path, then all its output under it. A task that
        succeeds prints nothing, and there are no rows and no summary. The
        output of the root is printed as it comes, and the message the work
        ended with last. *)
  | `Json
    (** Each event ({!Task.Event}) is one JSON object on a line of its own, and
        nothing else is printed: what the program prints on the display's
        formatter is the output of no task, as events too. *) ]
(** The type for progress modes, how the tasks of a root show. Constructors may
    be added, so a match on a progress mode has a case for the others. *)

val all : (string * t) list
(** [all] is every progress mode under its name, [`Auto] first, which are the
    choices of a [--progress] option as [Cmdliner.Arg.enum all] reads them:
    [[("auto", `Auto); ("tty", `Tty); ("plain", `Plain); ("quiet", `Quiet);
     ("json", `Json)]]. *)

val of_string : string -> (t, [ `Msg of string ]) result
(** [of_string s] is the progress mode of {!all} named [s]. The error message
    lists the names. *)

val to_string : t -> string
(** [to_string p] is the name of [p] in {!all}. *)

(** {1:layout Row layout} *)

(** What a running task's row shows, left to right.

    A layout is pieces put together with {!Layout.(++)} and {!Layout.list}. The
    lines of the tasks that ended have pieces of their own, whatever the layout:
    those of {!Layout.default}, with the mark of how the task ended in place of
    the spinner, in the same columns, and the bar as far as the task went.

    {b Packed from the left.} {!Layout.default} packs a row from the left: the
    label, in a column as wide as the widest label; then the bar, which grows to
    fill the room, up to 40 cells; then the count, the rate and the elapsed
    time. The rest of the line stays empty. The bars of all rows start in one
    column and are as wide, one cell after the label column. A task under
    another is indented under it, behind the guides of its tree, and its label
    column ends where the others do, so its columns are those of every row. The
    spinners and the marks take as many cells as the widest of them, so the
    labels start in one column too.

    {b What gives way.} The label is what tells two rows apart, so it gives way
    last. When a row does not fit its room, its bar first shrinks, down to 10
    cells; then its pieces give way stage by stage, each piece at the stage
    {!Layout.give_way} sets, by default:
    + the rate and the time left;
    + the bar and the percentage;
    + the count, which leaves the column it shares with the other rows and, for
      bytes, gets shorter, such as [256M/2.0G];
    + the elapsed time.

    Only once all of them have is the label cut, with an ellipsis. A piece that
    gives way, or that shows nothing, takes with it the blank {!Layout.const}
    before it, or after it. Every row's label fits its column, so the rows of a
    region give way together, and once the count leaves its column, the label,
    the spinner and the mark leave theirs too.

    {b Columns.} The label, the bar, the count and the rate each take a column
    that the rows of a root share, and the elapsed time a column of six cells; a
    row without a bar, a count or a rate leaves its column blank. The columns
    are as wide as the labels and the kinds of progress seen so far need, and
    they only widen:
    - The label column is as wide as the widest label a row of the root has
      drawn, behind the guides of its depth: a running row whose layout holds no
      label ({!Layout.choose}) does not count. A task with a wider label widens
      it in the first frame after it starts, or after it is renamed.
    - The bar column opens with the first task that counts.
    - A count of items takes the cells its total needs, at least [999/999], so a
      display of items is as dense as its counts allow. A task of items with a
      larger total widens the count column when it starts counting.
    - The first task that counts bytes widens the count column at once to the
      widest count of bytes, [1023.99 KiB / 1023.99 TiB], and opens the rate
      column at the widest rate, [1023.99 MiB/s]. Later tasks of bytes, larger
      totals and first rates widen nothing.
    - No column narrows while the display is open.

    So a column widens at most once for each new kind of progress, for a count
    of items with more digits, or for a wider label: a display of bytes alone,
    or one whose tasks of bytes count from their start, never widens after its
    first frame of bytes but for a label, and one where a task of bytes comes
    among rows of items widens once. While no piece gives way and no column
    widens each piece sits in the same columns on every row and in every frame.

    A program that wants the count, the rate and the time against the right edge
    puts a {!Layout.spacer} before them, which takes the room the bar leaves:

    {[
    let right_aligned =
      let space = Console.Progress.Layout.const (Console.View.text " ") in
      Console.Progress.Layout.(
        list
          [
            spinner;
            space;
            label ();
            spacer;
            bar ();
            space;
            count;
            space;
            rate;
            space;
            elapsed;
          ])
    ]} *)
module Layout : sig
  type t = Progress_layout.t
  (** The type for row layouts: at most one {!label} and one {!spacer}. *)

  val spinner : t
  (** [spinner] is the spinner of the row's theme ({!Spinner.view}), in the
      row's colour: the colour of the {!Theme.palette} the row takes. *)

  val label : ?style:Style.t -> unit -> t
  (** [label ()] is the task's label, in [style] (defaults to {!Style.bold}). It
      takes the room the other pieces leave, once they have given way, and is
      cut to it with an ellipsis. *)

  val bar : ?width:int -> unit -> t
  (** [bar ()] is a progress bar ({!Bar.horizontal}), [width] cells wide, or
      without [width] growing to fill the room, up to 40 cells, before a
      {!spacer} takes the rest. No bar shows before the task counts something.

      @raise Invalid_argument if [width] is negative. *)

  val percent : t
  (** [percent] is the share of the work done, as a percentage. *)

  val count : t
  (** [count] is the count of items or bytes done and the total, such as [3/10],
      right-aligned in the column of the counts. *)

  val rate : t
  (** [rate] is the bytes per second of a task that counts bytes, right-aligned
      in the column of the rates. The rate weighs the past by the time since it
      was measured, not by how often the count is said, so a program that
      reports often and one that reports seldom show the same rate for the same
      work. *)

  val eta : t
  (** [eta] is the time the task has left at its rate, such as [ETA 1m05s], once
      it has a rate. *)

  val elapsed : t
  (** [elapsed] is the time since the task started, rounded, such as [6.4s],
      [1m05s] or [1h02m], right-aligned in a column of its own. *)

  val spacer : t
  (** [spacer] is the room the other pieces leave, at least one cell: the pieces
      after it sit against the right edge. *)

  val const : View.t -> t
  (** [const v] is [v] on every row, on one line. *)

  val custom : (Task.t -> View.t) -> t
  (** [custom f] is [f task] on the row of [task], on one line, drawn again at
      every frame. *)

  val choose : (Task.t -> t) -> t
  (** [choose f] is the layout [f task] on the row of [task], chosen again at
      every frame, so that tasks of different kinds show different pieces. A
      label of [f task] counts towards the one label of the layout it is put in,
      and so does a spacer: a row that would hold two raises [Invalid_argument]
      where it is drawn ({!Display}). *)

  val give_way : int -> t -> t
  (** [give_way n l] is [l] whose pieces give way at stage [n] when the row is
      short of room ({!Layout}).

      @raise Invalid_argument if [n] is less than [1]. *)

  val ( ++ ) : t -> t -> t
  (** [l0 ++ l1] is [l0] then [l1].

      @raise Invalid_argument if they hold two {!label}s or two {!spacer}s. *)

  val list : ?sep:t -> t list -> t
  (** [list ~sep ls] is [ls] in turn, [sep] between each two.

      @raise Invalid_argument if they hold two {!label}s or two {!spacer}s. *)

  val default : t
  (** [default] is a spinner, the label, a bar that grows, the count, the rate
      and the elapsed time, a space between each two, packed from the left
      ({!Layout}). *)
end

val pp_bytes : int64 Fmt.t
(** [pp_bytes] formats a count of bytes as {!Layout.count} and {!Layout.rate}
    show it: in bytes below 1 KiB, such as [512 B], and above in the largest
    binary unit that keeps it below 1024 once rounded to two decimals, such as
    [1.50 MiB]. A negative count is [0 B]. *)

(** {1:views Task views} *)

val row : ?layout:Layout.t -> Task.t -> View.t
(** [row task] is the row of [task] in its room ({!View.fit}): drawn by [layout]
    (defaults to {!Layout.default}) while it runs, its pieces giving way to fit,
    and its line once it ended: its mark, then its label, bar, count, rate and
    time in the columns of the rows ({!Layout}). *)

val tree : ?layout:Layout.t -> Task.t -> View.t
(** [tree task] is the tree of [task] in its room ({!View.fit}): the row of
    [task], its window of output under it, then the trees of the tasks under it,
    each under its parent with the guides of a {!Tree.view} two cells in, in the
    order they started. The tree of a root is those of its top tasks.

    - In a room of a height, a tree too tall folds as {!design} says, and a task
      that ended is its line.
    - Without a height, as when the display closes or a task moves to the
      scrollback, the tree is whole: every task, none folded, each failure or
      cancellation followed by its output, and a label never cut: one too wide
      goes on over the next lines, never in fewer than 12 cells, its count and
      time at the end of its last line, or giving way as {!Layout} says.
    - A transient task that succeeded leaves its place to the tasks under it,
      and a task forgotten, once in the scrollback, is {!View.empty}. *)

val summary : Task.t -> View.t
(** [summary root] is the summary line of the work of [root], if its root has a
    label ({!Task.root}), and {!View.empty} otherwise: the mark of how the work
    is, its label, the time since it started and the count of the tasks that
    ended out of those started. In a room, the time and the count give way
    before the label. The mark is the running mark until the root ends, then the
    worst of how the root and its tasks ended. *)

val view : ?layout:Layout.t -> Task.t -> View.t
(** [view root] is the tree of [root] above its summary, and the message the
    work ended with under it, if any, at most 100 cells wide: what a display
    shows by default. *)

(** {1:showing Showing tasks} *)

val show :
  ?progress:t ->
  ?layout:Layout.t ->
  ?view:View.t ->
  ?keep_ended:bool ->
  ?commit:(Task.t -> View.t option) ->
  ?plain:(Task.Event.t -> View.t option) ->
  ?delay:float ->
  ?size:(unit -> int * int) ->
  Format.formatter ->
  Task.t ->
  Display.t
(** [show ppf root] shows the tasks of [root] on [ppf] and is the display that
    does it. Closing it ends the work as cancelled if [root] still runs.

    - [size], if given, says that [ppf] is a terminal and gives its width and
      height, as {!Display.v} takes them.
    - [progress] (defaults to [`Auto]) is the mode: [`Auto] redraws the rows on
      a terminal, as [size] says, [`Tty] on 80 columns by 24 rows without
      [size], and [`Plain], [`Quiet] and [`Json] print lines, whatever [size]
      says. [`Plain] prints its last frame as wide as the terminal [size] gives,
      and whole without [size].
    - [view] (defaults to {!val-view}[ root]) is the region, redrawn in place,
      and the last frame printed when the display closes, with [`Plain] too. A
      view that raises is dropped for the default one, and the display raises
      its exception as {!Display} says.
    - [commit] (defaults to the {!tree} of the task) is what a top task that
      ends leaves in the scrollback, given the task as it ended, with the output
      it kept ({!Task.output}) even when it succeeded; [None] leaves nothing.
    - [plain], if given, prints the program's own lines in place of those of the
      tasks when they print as lines, with [`Plain] or with [`Auto] off a
      terminal: [plain e] for each event [e] of a task, as wide as [size] says
      or whole, and nothing for [None]. The output of no task and the view's
      last frame print as without it. A program learns so whether the rows show:
      [plain] is called only when they do not.
    - [delay] (defaults to [0.]) is the seconds before the rows first show, as
      {!Display.v} takes it: a work that ends sooner shows no rows, only its top
      tasks in the scrollback and the last frame.
    - [keep_ended] (defaults to [false]) keeps the tasks that ended in the
      region until the display closes, where they otherwise move to the
      scrollback with their top task ({!design}). With [`Plain] it keeps them
      too, for the view's last frame and {!Task.children}, so the default view
      then ends with their whole tree.
    - [layout] (defaults to {!Layout.default}) draws the rows of the default
      [view] and of the default [commit]; a [view] or a [commit] given draws its
      own.

    The display takes none of what the program prints on [ppf] but what
    {!Display.with_output} prints: with [`Quiet] it shows, and with [`Json] it
    breaks the lines of events. [Console_eio.run] takes the standard outputs.
    What {!Display.print} and {!Display.with_output} print is output of no task:
    above the rows, in turn with the lines, and with [`Json] an [output] event
    for each of its lines, without escape codes. *)

(** {1:design Design}

    On a terminal, the rows are the region of a {!Display}, and what leaves the
    region goes to its scrollback.

    - The summary is the last line of the default view, so the line that says
      how far the work is stays where the eye left it, below the rows.
    - A task that ends stays in the region, marked as it ended, under its
      parent, while its top task runs.
    - When a top task ends, it moves to the scrollback with every task under it,
      as its tree: each task once, under its parent, in the order they started,
      and a task that failed or was cancelled followed by its output. Work moves
      by top task so that a task never lands above its parent and the tasks of
      one top task stay together, however their endings interleave. The tree is
      laid out, then the task forgotten, then the tree printed with the region
      drawn again below it, so it never shows twice.
    - With [keep_ended] ({!show}), no task moves: the tasks that ended stay in
      the region until the display closes, as in a view of trees side by side,
      where a column should not empty while the others run.
    - The output of the root, and a [Logs] message or a printed line from a
      fiber whose task ended, go to the scrollback at once. A line given to a
      task that ended with {!Task.log} is dropped. A line wider than the
      terminal goes on over the next rows as the terminal wraps it, mid-word, as
      it would without the display, while a line after a mark, such as a
      warning, goes on under its text, broken between words.
    - Only running rows, their windows and the summary change: a task that ended
      keeps its line as it ended until it leaves the region, or folds to fit.
    - A change to the tasks is drawn at once if the last frame is a tenth of a
      second old, and with the next redraw otherwise, while what is printed to
      the scrollback is written at once, with the region as it is then. So a
      line printed just before a task starts may show for a tenth of a second
      above rows that lack the task.
    - Any change to the tasks may draw a frame, such as the end of a task that
      {!Task.run} ends before it returns. A view that reads the program's own
      state, such as a count of the steps done, shows it as it is then: change
      it inside the function given to {!Task.run}, before the task ends, so that
      no frame shows the task ended and the count not yet raised.
    - A tree too tall for its room first folds the tasks that ended under each
      task into one line, such as [12 done, 2 failed] after the mark of the
      worst ending among them, then the lines of its windows give way, the
      oldest of them first, whichever task's window holds it, then only its
      latest running tasks show, after a line such as [3 more running].
    - At close, the root ends cancelled if it runs, its tasks move as above,
      then the view's last frame is printed once, still, and stays: the summary
      alone in the default view, unless [keep_ended] kept the tasks.
    - The tasks that wait in the region under a task are bounded: past 1 MiB,
      the oldest are dropped, and a line counts them.

    Without a terminal there is no region. The tasks print the lines of
    [`Plain]: one line when a task starts and one when it ends, in the order
    they happen, and at close the view's last frame comes once: the summary
    alone in the default view, since every task has printed its lines (unless
    [keep_ended] kept them), then the message the work ended with.

    The lines of tasks that run at once interleave on a pipe, so no line is
    placed by its indentation: each line a task prints, as it starts, as it ends
    and before each line of its output, names the task by its {e path}, the
    labels from its top task down to it, such as [linux > compile]. On a UTF-8
    formatter the separator is a small black right-pointing triangle, U+25B8, a
    glyph that no mark takes and that a label seldom holds; on another it is
    [>]. The lines of tasks start at the first column, and a line of output two
    columns in. A task's line when it ends says [failed] or [cancelled] when it
    did not succeed, as no colour may. With [`Quiet], the line of a failure
    names its path too, and its output follows it, two columns in.

    A line of output that comes a second or more after the task's line before
    it, or after the task started, ends with that wait, muted, such as [+3.0s],
    so that a pause in the work shows where it happened.

    {1:guarantees Guarantees}

    Those of {!Display}, and on a terminal of [w] columns:
    - While no piece gives way ({!Layout}), the bars, counts, rates and timers
      of all rows sit in the same columns, packed from the left: the bars start
      one cell after the widest label, are as wide, at most 40 cells, and the
      rest of the line stays empty, within the first [min w 100] columns.
      Nothing in the region moves sideways unless the terminal width changes or
      a column widens, at most once for each new kind of progress or for a wider
      label ({!Layout}).
    - A label cut to fit is cut between two characters and ends with an ellipsis
      that turns its style off. *)
