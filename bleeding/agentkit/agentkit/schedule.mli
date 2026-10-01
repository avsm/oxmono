(*---------------------------------------------------------------------------
   Copyright (c) 2026 Anil Madhavapeddy. All rights reserved.
   SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

(** The work a person asks an agent for.

    A schedule is a file of tasks, each with an id, a prompt and one of three
    triggers. [every D] repeats on a duration. [at HH:MM] fires daily on the
    local wall clock, narrowed by an optional set of weekdays. [once] fires at
    the next opportunity and then never again. There is no crontab expression,
    which is a parser and a set of edge cases in exchange for expressiveness
    nothing here has asked for.

    {[
      { "tasks": [
        { "id": "feeds", "every": "15m", "prompt": "Check the feeds in ..." },
        { "id": "digest", "at": "07:00", "days": ["mon","tue"],
          "prompt": "Summarise yesterday's journal into memory." },
        { "id": "probe", "once": true, "prompt": "Fetch ... and report." } ] }
    ]}

    Nothing in the schedule is state. Whether a task has fired, whether a [once]
    task is done and whether a [run_now] has been honoured are all read out of
    the journal, which is why {!poll} takes what it knows about a task rather
    than reading it back from a file. That is what keeps a firing idempotent
    under a reread and survivable across a restart. *)

(** {1 Days and times} *)

(** A day of the week, as [days] names them. *)
type day = Mon | Tue | Wed | Thu | Fri | Sat | Sun

val day_name : day -> string
(** [day_name d] is [d] as it is written, such as ["mon"]. *)

val day_of_string : string -> (day, string) result
(** [day_of_string s] is the day written [s]. The error says what the accepted
    forms are. *)

val duration_of_string : string -> (int, string) result
(** [duration_of_string s] is the seconds in [s], which is a positive number and
    one of [s], [m], [h] or [d], such as ["15m"], ["6h"] or ["1d"]. The error
    says that. *)

val duration_to_string : int -> string
(** [duration_to_string secs] is [secs] in the largest unit that divides it
    exactly, so [duration_of_string] of the result is [secs] again. *)

val time_of_string : string -> (int * int, string) result
(** [time_of_string s] is the hour and minute in [s], which is [HH:MM] on a
    24-hour clock. The error says that. *)

type date = { year : int; month : int; day : int }
(** A calendar date, with [month] and [day] counting from 1. *)

type local = { date : date; hour : int; minute : int; second : int }
(** A wall clock reading in some zone. *)

val weekday : date -> day
(** [weekday d] is the day of the week [d] falls on. *)

type zone = {
  local : float -> local;
      (** [local t] is the wall clock at the POSIX instant [t] *)
  instant : date -> hour:int -> minute:int -> float;
      (** [instant d ~hour ~minute] is the POSIX instant at which the wall clock
          reads that time on [d]. A time a daylight saving change skips or
          repeats has one instant here, which is what makes such a time fire
          exactly once that day. *)
}
(** How wall clock time relates to POSIX time. It is an argument rather than an
    assumption so that the firing arithmetic can be tested against a zone that
    changes, rather than against whatever zone the test machine is in. *)

val system : zone
(** [system] is the zone the machine is set to, which is what an [at] task
    means. *)

val utc : zone
(** [utc] is UTC, which is what the journal is written in. *)

(** {1 Tasks} *)

(** What makes a task fire. Exactly one of these is written per task. *)
type trigger =
  | Every of int  (** repeat on this many seconds *)
  | At of { hour : int; minute : int; days : day list }
      (** fire at this local wall clock time, on these weekdays, or on every day
          when the list is empty *)
  | Once  (** fire at the next opportunity and then never again *)

(** What to do about the fires a stopped daemon was not there for. *)
type on_missed =
  | Run_once  (** run one of them, which is the default *)
  | Skip  (** run none of them, and wait for the next due time *)

type task = {
  id : string;  (** what the journal calls it *)
  prompt : string;  (** what the agent is asked to do *)
  trigger : trigger;
  disabled : bool;  (** written [false] by omission *)
  run_now : int;
      (** the serial [numpty task run] last set, and 0 when it never has. It is
          a serial rather than a flag because the daemon does not write this
          file and so cannot clear one. *)
  on_missed : on_missed;
}

type t = { tasks : task list }
(** A whole schedule. *)

val jsont : t Jsont.t
(** [jsont] is the codec, which is the schema. *)

val of_string : string -> (t, string) result
(** [of_string s] is the schedule in [s], or says what is wrong with it. A task
    that names no trigger, that names two, that repeats an id, or that carries a
    duration or a time in the wrong form is refused, and the message says what
    the right form is. *)

val to_string : t -> string
(** [to_string t] is [t] as JSON, indented, since a person reads and diffs this
    file. *)

(** {1 Reading, which is what the daemon does}

    The daemon reads this file and never writes it, so nothing it concludes can
    change what it was told to do. The two directions are separate entry points
    to keep that visible. *)

val read : _ Eio.Path.t -> (t, string) result
(** [read path] is the schedule in the file [path], the empty schedule if there
    is no such file, and an error naming the problem if it does not parse. A
    caller that already holds a schedule keeps it on an error, since a person
    mid-edit should not lose a daemon. *)

(** {1 Writing, which is what the person's command does}

    [numpty task] runs as the person, holds no model, and needs no daemon. *)

val rewrite : _ Eio.Path.t -> (t -> t) -> (t, string) result
(** [rewrite path edit] reads [path], applies [edit], checks that the result is
    a schedule this module would read back, writes it to a sibling temporary
    file, and renames that over [path]. It is the new schedule, or an error.

    A reader therefore sees the old file or the new one and never a half written
    one. An error leaves [path] exactly as it was, which includes the case where
    [path] does not parse: a person's half-edited file is not something to
    overwrite with a guess at what they meant. *)

(** {1 When a task fires} *)

(** Why a task is firing, which reaches the journal's [wake] record. *)
type why =
  | First  (** it has never run, which is also how a [once] task fires *)
  | Due  (** its due time has come round *)
  | Missed  (** more than one due time passed while nothing was running *)
  | Run_now  (** a person asked for it with a serial above the last one *)

val why_name : why -> string
(** [why_name w] is [w] as the journal writes it, such as ["run_now"]. *)

type firing = {
  due : float;  (** the due time it is firing for, as a POSIX instant *)
  why : why;
  serial : int option;  (** the [run_now] serial, when that is why *)
}

type status = {
  fire : firing option;  (** fire it now, and why *)
  next : float option;
      (** when it next fires after [now], taking [fire] as having happened, and
          [None] for a task that never fires again *)
}

val poll :
  zone:zone ->
  task ->
  last:float option ->
  serial:int ->
  since:float option ->
  now:float ->
  status
(** [poll ~zone task ~last ~serial ~since ~now] is whether [task] fires at [now]
    and when it fires next. [last] is when the journal last has a [wake] for it,
    and [None] if it never has. [serial] is the [run_now] serial of that record,
    and 0 if none. [since] is when the caller began watching, and [None] before
    it has watched at all, which is what it is at startup.

    A [run_now] above [serial] fires whatever the trigger says, since a person
    asking for a task now means now.

    Missed fires do not queue. A daemon down over four due times of a fifteen
    minute task fires once at startup under {!Run_once} and not at all under
    {!Skip}. Only {!Skip} reads [since], and it is what tells a due time nobody
    was there for from one the caller was watching when it came round. A caller
    that passed [None] for ever would never fire a {!Skip} task at all. A [once]
    task fires only while [last] is [None], so the journal is the whole of the
    record of it having run.

    An [at] task's due time is one instant per local calendar day, so a wall
    clock time that a daylight saving change skips or repeats still fires
    exactly once that day. Due times more than eight days behind [now] are not
    looked for, which is enough to find the most recent one under any set of
    weekdays and to tell one from several. *)
