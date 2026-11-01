(*---------------------------------------------------------------------------
  Copyright (c) 2026 Thomas Gazagnaire. All rights reserved.
  SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

(** The display that runs, and what goes on around it: the program's output and
    {!Logs} messages, the redraws, and its closing however the program ends. *)

type run = { display : Console.Display.t; root : Console.Task.t; json : bool }
(** The type for what {!run} runs: a display, the root of the work it shows, and
    whether it shows it as JSON events. *)

val find : unit -> Console.Task.t option
(** [find ()] is the root of the run of the calling fiber, if any and its
    display does not close yet. *)

val display : unit -> Console.Display.t option
(** [display ()] is the display of the run of the calling fiber, if any and it
    does not close yet. It is [None] outside Eio. *)

val given : Console.Task.t -> Console.Task.t
(** [given root] is the task a run inside another is given: the task of [root]'s
    work current where it is called, or [root]. *)

val scope : Task_driver.scope
(** [scope] is the scope in which the current task is that of the innermost
    {!Console.Task.run} or {!Console.Task.within} of the calling fiber or of the
    fibers it forked from. A fiber cancelled in a {!Console.Task.run} cancels
    its task. *)

val write_error : exn -> bool
(** [write_error e] is [true] iff [e] is a write error of an Eio flow, such as a
    closed pipe. *)

val join : (Console.Task.t -> 'a) -> 'a option
(** [join f] is [Some (f task)] for the run in the calling domain, with [task]
    {!given} by its root, as the calling fiber's, if one runs and its display
    does not close yet: the display then closes once [f] returned too. It is
    [None] otherwise. *)

val await_release : unit -> unit
(** [await_release ()] waits until the display that {!run} runs in the calling
    domain is gone, if it closes. *)

val claim : unit -> unit
(** [claim ()] makes the caller the one that opens a display, until {!release}.

    @raise Invalid_argument if a display already runs. *)

val release : unit -> unit
(** [release ()] ends a {!claim}. *)

val with_terminal : (unit -> 'a) -> 'a
(** [with_terminal f] is [f ()] with the region of the calling fiber's display,
    if any, off the screen, and what [f] and the fibers it forks write on the
    standard formatters going straight out. What other fibers write meanwhile is
    taken as usual. *)

val run :
  ?cursor:(bool -> unit) ->
  sleep:(float -> unit) ->
  intercept:Format.formatter list ->
  run ->
  (Console.Task.t -> 'a) ->
  'a
(** [run ~sleep ~intercept r f] is [f r.root], with [r] the calling fiber's and
    the program's run, its display redrawn ten times a second, waiting with
    [sleep], {!Logs} messages and what is written on the formatters of
    [intercept] given, a line at a time, to the task current where it is
    written, a task's unended line ended when the task ends, or to the root
    outside a task. The root succeeds when [f] returns, fails when it raises and
    is cancelled when its fiber is, unless [f] ended it, and the display closes
    when [f] returns, raises, is cancelled, the program exits or a signal ends
    it ({!Interrupt}). While SIGTSTP stops the program, the region is off the
    screen and the cursor shown with [cursor true] (default does nothing); once
    continued, [cursor false] hides it and the region is drawn again. A {!claim}
    must be held; it is released. *)
