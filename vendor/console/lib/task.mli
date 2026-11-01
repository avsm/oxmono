(*---------------------------------------------------------------------------
  Copyright (c) 2026 Thomas Gazagnaire. All rights reserved.
  SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

(** Tasks: the work a program reports, with no drawing.

    A task has a label, a count of items or bytes, lines of output and an end.
    Tasks form a tree under a {!root}, the work as a whole, and ending the root
    says how the work ended. {!Progress} shows the tasks of a root as rows,
    lines, failures alone or JSON, and [Console_eio.run] makes a root, shows it
    and passes it to the program:

    {[
    let build root =
      Console.Task.run root "compile" (fun step ->
          Console.Task.set_items step ~count:1 ~total:2;
          Console.Task.log step "ocamlopt parser.ml";
          Console.Task.set_items step ~count:2 ~total:2)
    ]}

    A task belongs to the domain that runs its root, so call its functions from
    that domain alone. A task that ended ignores what it is told, and a line
    given to it with {!log} is dropped; the root alone takes {!log} after it
    ended, as output of no task. *)

type t = Task_core.t
(** The type for tasks. *)

type level = [ `App | `Error | `Warning | `Info | `Debug ]
(** The type for the levels of a line of output: those of [Logs]. *)

type state = [ `Running | `Succeeded | `Failed | `Cancelled ]
(** The type for where a task is: running, or how it ended. A task that ended
    has the {!Event.type-status} of the same name in its ["end"] event, and the
    {!Theme.type-mark} of the same name on its line. *)

type progress = [ `Spin | `Items of int * int | `Bytes of int64 * int64 ]
(** The type for how much of a task is done: not said yet, so its row shows a
    spinner, or a count of items or of bytes done out of a total. *)

(** {1:tree Roots and tasks} *)

val root : ?label:string -> now:(unit -> float) -> unit -> t
(** [root ~now ()] is the work as a whole, timed by the clock [now], in seconds.
    [label], if given, names the work on the summary line ({!Progress.summary}).
    A root is a task: it is current where no other is, its output is the output
    of no task, and ending it ends the work. *)

val start : ?key:string -> ?log_lines:int -> ?transient:bool -> t -> string -> t
(** [start parent label] starts a task named [label] (a line feed in it is a
    space) under [parent]: a root, or a task.

    - [key], if given, names the task. While it runs, a later call with the same
      [key] under the same root returns the same task, and its [label],
      [parent], [log_lines] and [transient] are ignored.
    - [log_lines] is how many of the task's latest lines its row shows under it
      while it runs (defaults to [8]).
    - [transient] (defaults to [false]) makes a task that shows only while it
      runs, and leaves neither a line of its own nor any of its output when it
      succeeds. Its output is all that is printed while it is current, [Logs]
      messages included, so a line that should stay is printed outside it, or
      after it ended. One that fails or is cancelled shows as any other task
      does.

    A task started under a task that ended, the root included, takes its ending
    at once.

    @raise Invalid_argument if [key] is empty or [log_lines] is negative. *)

val run :
  ?key:string ->
  ?log_lines:int ->
  ?transient:bool ->
  t ->
  string ->
  (t -> 'a) ->
  'a
(** [run parent label f] is [f task], for a [task] started as {!start} starts it
    and current while [f] runs ({!within}). The task succeeds when [f] returns
    and fails when [f] raises, with the message
    [label ^ ": " ^ Printexc.to_string e] for the exception [e] ({!fail}), which
    is raised again; but if [f] ended the task itself, that ending counts. Under
    [Console_eio.run], a cancelled fiber cancels the task instead of failing it.

    When [key] names a task that runs, [run] is [within task (fun () -> f task)]
    for that task. It ends nothing, and the task ends as the call that started
    it says. *)

val within : t -> (unit -> 'a) -> 'a
(** [within t f] is [f ()] with [t] current while [f] runs and, under
    [Console_eio.run], in the fibers [f] starts. The current task decides only
    where output goes: what [f] prints, and its [Logs] messages, are [t]'s
    output. Where {!run} starts a task, [within] enters one that exists, so it
    ends nothing, even when [f] raises. *)

(** {1:changes Changes} *)

val set_label : t -> string -> unit
(** [set_label t label] renames [t]. The root keeps its label. *)

val set_items : t -> count:int -> total:int -> unit
(** [set_items t ~count ~total] says that [count] items of [total] are done. A
    count never goes back: a smaller [count] or [total] than before is ignored,
    and a [count] above [total] raises the total.

    The root counts nothing, as its summary line counts its tasks
    ({!Progress.summary}): a count of the whole work is that of a task started
    for it. So a library that a nested [Console_eio.run] gives the root counts
    as it would under any task.

    @raise Invalid_argument
      if [count] or [total] is negative, or [t] counts bytes. *)

val set_bytes : t -> count:int64 -> total:int64 -> unit
(** [set_bytes t ~count ~total] is like {!set_items} for a task that counts
    bytes, whose row also shows the bytes per second. The root counts nothing.

    @raise Invalid_argument
      if [count] or [total] is negative, or [t] counts items. *)

val log : ?level:level -> t -> string -> unit
(** [log t text] gives each line of [text] to [t] as its output, at [level] for
    a [Logs] message (by default, a line the program printed). It is the
    one-line form of {!within}, for code where [t] is not current.

    - A tab goes to the next stop of eight columns from the start of its line.
    - A running task keeps its latest lines, which its row shows, and up to 1
      MiB of all of them for its failure: past that, the oldest go first and a
      line counts them. A line, a label and a message are each kept up to 4 KiB,
      cut between two characters and ended with three dots.
    - The output of the root is the output of no task.
    - A line given to a task that ended with [log] is dropped. A [Logs] message
      or a printed line from a fiber whose task ended is not: it goes to the
      scrollback at once, as that task's ({!Progress.design}). *)

val succeed : ?message:string -> t -> unit
(** [succeed t] ends [t] as a success, with [message] in place of its label if
    given. A task ends once: the first of {!succeed}, {!fail} and {!cancel}
    counts, and the others do nothing. A task's ending is what the program says
    of it: a task that failed under [t] does not fail it.

    The tasks under [t] that still run end first, in the order they started:
    they succeed with it, and are cancelled when it fails or is cancelled. On
    the root, this says how the work ended. *)

val fail : ?message:string -> t -> unit
(** [fail t] is like {!succeed} but ends [t] as a failure. *)

val cancel : ?message:string -> t -> unit
(** [cancel t] is like {!succeed} but ends [t] as cancelled. *)

(** {1:read Accessors} *)

val label : t -> string
(** [label t] is the label of [t], as it shows: on one line, at most 4 KiB. *)

val parent : t -> t option
(** [parent t] is the task [t] was started under, [None] for a root. *)

val children : t -> t list
(** [children t] are the tasks under [t], in the order they started: those that
    run, and those that ended and are still shown ({!Progress}). A transient
    task that succeeded is not among them: the tasks under it take its place, as
    in {!Progress.tree}. *)

val path : t -> string list
(** [path t] is the labels from the top task down to [t], [[]] for a root. *)

val state : t -> state
(** [state t] is where [t] is. *)

val progress : t -> progress
(** [progress t] is how much of [t] is done. *)

val message : t -> string option
(** [message t] is the message [t] ended with, if any. *)

val elapsed : t -> float
(** [elapsed t] is the seconds [t] ran for, or has run for so far. *)

val rate : t -> float option
(** [rate t] is the items or bytes per second that [t] counts, as
    {!Progress.Layout.rate} shows it, once a count was said after [t] started:
    it weighs the past by [exp (-dt /. 2.)] for [dt] seconds ago, so it depends
    on the time, not on how often the count is said. It is [None] before, and
    while nothing was counted. *)

val output : t -> string list
(** [output t] is the lines of output [t] keeps, oldest first, each as {!log}
    keeps it, the latest within 1 MiB. A task that failed or was cancelled keeps
    them until {!Progress} printed it: as its top task moves to the scrollback,
    or as it ends without a terminal ({!Progress.design}). A task that succeeded
    keeps them only while it ends, for {!Progress.show}'s [commit] to draw. The
    root has none ({!log}). The last [log_lines] of them ({!start}) are those
    its row shows. *)

val dropped : t -> int
(** [dropped t] is the number of [t]'s oldest lines that {!output} no longer
    holds, to keep it within 1 MiB. *)

val pp : t Fmt.t
(** [pp] formats a task for diagnostics, in an unspecified format. *)

(** {1:json JSON events}

    Each change of a task is an event, which {!on_event} gives a program and
    which [`Json] ({!Progress}) writes as one JSON object on a line of its own,
    in the order the events happen: the line is the {!Event.to_string} of an
    {!Event.t}, and {!Event.jsont} reads it back. The summary line is not an
    event.

    Objects may gain members, new events may be added, and a member whose values
    are listed may gain values, but the members of {!Event} keep their meaning.
    A consumer skips a member, an event or a value it does not know, as
    {!Event.jsont} does. An event of another name may give a listed member
    another type: {!Event.jsont} then skips it too. *)

(** JSON events.

    An event is a JSON object with two members:
    - [time], the seconds since the root started, a number rounded to the
      millisecond, such as [1.5] or [0] ({!Event.time}). A time is held in
      \[[-10{^ 12}];[10{^ 12}]\] seconds: a larger magnitude is the bound, and a
      time that is not finite is [0];
    - [event], the name of the event ({!Event.type-kind}).

    An event of a task also has:
    - [id], an integer that names the task in its work: the tasks are numbered
      from [0] in the order they start, and no two tasks of a root have the same
      number;
    - [task], the labels from the top task down to it, an array of strings.

    An event of no task, such as output outside any task or the ["end"] of the
    work as a whole, has neither.

    The other members are those of each event, in the order of
    {{!Event.members}its accessors}, a member that an event does not have being
    absent. Strings are valid UTF-8 and hold no C1 control (U+0080 to U+009F),
    which a terminal may take for an escape code: each C1 control is U+FFFD, and
    so is each maximal subpart of an ill-formed sequence, as
    [String.get_utf_8_uchar] decodes it. The C0 controls and DEL are escaped.
    Numbers have at most 15 significant digits, so an integer, never negative,
    is a JSON number below 10{^ 15}, and a string of decimal digits from there.

    A label, in [task] or in ["label"], and the [message] of ["end"] are at most
    4 KiB: a longer one is cut between two characters and ends with [...]
    ({!log}). A line of output is whole, up to the pieces of 64 KiB in which
    [Console_eio.run] reads the program's output.

    The task of the example above gives these events, and the work then ends:
    {v
    {"time":0,"event":"start","id":0,"task":["compile"]}
    {"time":0,"event":"progress","id":0,"task":["compile"],"count":1,"total":2,"unit":"items"}
    {"time":0,"event":"output","id":0,"task":["compile"],"text":"ocamlopt parser.ml"}
    {"time":0,"event":"progress","id":0,"task":["compile"],"count":2,"total":2,"unit":"items"}
    {"time":0,"event":"end","id":0,"task":["compile"],"status":"succeeded","elapsed":0}
    {"time":0,"event":"end","status":"succeeded","elapsed":0}
    v}

    A program reads a line with {!Event.of_string}, or with any
    {{:https://erratique.ch/software/jsont}jsont} decoder of {!Event.jsont}. *)
module Event : sig
  (** {1:values Values}

      The types of the names of events and of the values of their members are
      open: constructors may be added, so a value read as [`Other s] today may
      read as a constructor of its own later, and a match on one has a case for
      the others. An event never holds [`Other s] with [s] a listed name: the
      constructors take it as that name's constructor, and {!jsont} reads [s] as
      it. *)

  type kind =
    [ `Start  (** ["start"]: the task started. *)
    | `Progress  (** ["progress"]: the count of the task changed. *)
    | `Label  (** ["label"]: the label of the task changed. *)
    | `Output  (** ["output"]: a line of output, of a task or of none. *)
    | `End
      (** ["end"]: the task ended, or the work as a whole, which has no task. *)
    | `Other of string  (** An event of another name. *) ]
  (** The type for the names of events, the [event] member. *)

  type count_unit = [ `Items | `Bytes | `Other of string ]
  (** The type for the units of a count, the [unit] member: ["items"], ["bytes"]
      or another string. *)

  type nonrec level = [ level | `Other of string ]
  (** The type for the levels of Logs messages, the [level] member: ["app"],
      ["error"], ["warning"], ["info"], ["debug"] or another string. *)

  type status = [ `Succeeded | `Failed | `Cancelled | `Other of string ]
  (** The type for how a task or the work ended, the [status] member of ["end"]:
      ["succeeded"], ["failed"], ["cancelled"] or another string. *)

  type task = int * string list
  (** The type for the task of an event: its [id] member, and its [task] member,
      the labels from the top task down to it. *)

  type t = Event.t
  (** The type for events. A transient task ({!start}) has its events as any
      other task has. *)

  (** {1:make Constructors}

      Each event has a constructor that takes its members, those it may lack as
      optional arguments, so that a member added later is an optional argument.
      The task of an event is its [id] and its [task], the labels from the top
      task down to it, the last positional argument but for {!Event.label}'s
      text. [time] and [elapsed] are taken as {!jsont} writes them, and each
      string as valid UTF-8 without a C1 control.

      The constructors raise [Invalid_argument] if given a negative [id],
      [parent], [count] or [total]. *)

  val start : ?parent:int -> time:float -> task -> t
  (** [start ~parent ~time task] is ["start"]: [task] started, under the task
      whose [id] is [parent], if any. *)

  val progress :
    time:float -> count:int64 -> total:int64 -> unit:count_unit -> task -> t
  (** [progress ~time ~count ~total ~unit task] is ["progress"]: the count of
      [task] changed to [count] of [total], in [unit]. Each call to {!set_items}
      or {!set_bytes} that changes them is one event, however often it comes. *)

  val label : time:float -> task -> string -> t
  (** [label ~time task text] is ["label"]: the label of [task] changed to
      [text]. *)

  val output : ?task:task -> ?level:level -> time:float -> string -> t
  (** [output ~task ~level ~time text] is ["output"]: [text] is a line of output
      of [task], or of no task if [task] is absent, and [level], only for a Logs
      message, its level. *)

  val succeeded :
    ?task:task -> ?message:string -> time:float -> elapsed:float -> unit -> t
  (** [succeeded ~task ~message ~time ~elapsed ()] is ["end"] with [status]
      ["succeeded"]: [task] ended after [elapsed] seconds, or the work as a
      whole without [task], and [message] is the message it ended with, if any.
  *)

  val failed :
    ?task:task -> ?message:string -> time:float -> elapsed:float -> unit -> t
  (** [failed] is {!succeeded} with [status] ["failed"]. *)

  val cancelled :
    ?task:task -> ?message:string -> time:float -> elapsed:float -> unit -> t
  (** [cancelled] is {!succeeded} with [status] ["cancelled"]. *)

  val other : ?task:task -> time:float -> string -> t
  (** [other ~task ~time name] is the event named [name], of [task] if given,
      with no other member.

      @raise Invalid_argument if [name] is the name of a listed {!type-kind}. *)

  (** {1:members Members}

      An event's members are in the order of these accessors. A member that an
      event does not have is [None], and is absent from its object. *)

  val time : t -> float
  (** [time e] is the seconds since the root started when [e] happened. *)

  val kind : t -> kind
  (** [kind e] is the name of [e]. *)

  val task : t -> task option
  (** [task e] is the task of [e]: [None] for output outside any task, for the
      ["end"] of the work and for an event of another name that has none. *)

  val parent : t -> int option
  (** [parent e] is the [id] of the task over that of ["start"], if any. *)

  val count : t -> int64 option
  (** [count e] is the count of ["progress"]. It is written as a JSON number
      below 10{^ 15} in magnitude, and as a string of decimal digits from there.
  *)

  val total : t -> int64 option
  (** [total e] is the total of ["progress"], written as {!count} is. *)

  val unit : t -> count_unit option
  (** [unit e] is the unit of the count of ["progress"]. *)

  val status : t -> status option
  (** [status e] is how the task or the work of ["end"] ended. *)

  val elapsed : t -> float option
  (** [elapsed e] is the seconds the task or the work of ["end"] ran for. *)

  val text : t -> string option
  (** [text e] is the label of ["label"] and the line of ["output"]. *)

  val message : t -> string option
  (** [message e] is the message of ["end"], if any. *)

  val level : t -> level option
  (** [level e] is the level of ["output"], for a Logs message. *)

  (** {1:json JSON} *)

  val jsont : t Jsont.t
  (** [jsont] maps an event to its JSON object. It reads and writes [time] and
      [elapsed] rounded to the millisecond in \[[-10{^ 12}];[10{^ 12}]\], a
      larger magnitude as the bound and [0] if not finite, so that each reads
      back as it was written. It encodes an integer of 10{^ 15} or more as a
      string, and each string as valid UTF-8 without a C1 control. It decodes an
      event of another name as [`Other] with its [time], and its [id] and [task]
      if they have the types of {{!json}an event of a task}: its other members,
      whatever their types, are skipped. It decodes a value of a
      {!type-count_unit}, a {!type-level} or a {!type-status} it does not list
      as [`Other], and skips the members it does not know. An event of a listed
      name that lacks a member of its kind, or has a listed member of another
      type, does not decode, and neither does a negative or fractional integer.
  *)

  val of_string : string -> (t, [ `Msg of string ]) result
  (** [of_string s] is the event of the JSON object [s], decoded with {!jsont}.
      This is [Error (`Msg m)] if [s] is not the JSON object of an event, with
      [m] plain text, without escape codes. *)

  val to_string : t -> string
  (** [to_string e] is the line [`Json] writes for [e], without the line feed:
      the object of {!jsont} on one line, its members in order, written by
      [Jsont_bytesrw.encode_string] with 15 significant digits for numbers. *)

  val pp : Format.formatter -> t -> unit
  (** [pp] formats an event as {!to_string} does. *)
end

val on_event : t -> (Event.t -> unit) -> unit
(** [on_event t f] calls [f] with each event of the work of [t], every task
    under its root and the root itself, in the order they happen, from now on.
    [f] runs where the change is made, and must not change a task. A program
    writes its own lines from them, as [`Json] writes the JSON objects. *)
