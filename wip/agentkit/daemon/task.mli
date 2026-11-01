(*---------------------------------------------------------------------------
   Copyright (c) 2026 Anil Madhavapeddy. All rights reserved.
   SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

(** Editing the schedule, which is what [numpty task] does.

    The schedule file is the only place work is asked for. This writes it and
    the daemon only reads it, so nothing numpty concludes can change what it was
    told to do. It needs no daemon: a change made while numpty is stopped is
    picked up at startup, and one made while it runs is picked up on its next
    tick.

    Every edit goes through {!Agentkit.Schedule.rewrite}, which validates the
    result and renames a complete file over the old one, so the daemon never
    stats a half-written file and a refusal leaves the file exactly as it was.
*)

val add :
  _ Eio.Path.t ->
  id:string ->
  trigger:Agentkit.Schedule.trigger ->
  on_missed:Agentkit.Schedule.on_missed ->
  prompt:string ->
  (Agentkit.Schedule.t, string) result
(** [add path ~id ~trigger ~on_missed ~prompt] adds the task, or replaces the
    one of that id, keeping its [run_now] serial. The serial is the daemon's
    only record of what has been asked for, and lowering it would fire the task
    again. *)

val rm : _ Eio.Path.t -> string -> (Agentkit.Schedule.t, string) result
(** [rm path id] removes the task [id], and is an error naming the ids there are
    if none has it. *)

val enable : _ Eio.Path.t -> string -> (Agentkit.Schedule.t, string) result
(** [enable path id] clears [disabled] on the task [id]. *)

val disable : _ Eio.Path.t -> string -> (Agentkit.Schedule.t, string) result
(** [disable path id] sets [disabled] on the task [id]. It stays in the file, so
    what was asked for is still readable. *)

val run_now : _ Eio.Path.t -> string -> (int, string) result
(** [run_now path id] raises the task's [run_now] serial by one and is the new
    serial.

    The daemon fires the task when the serial is above the last one it
    journalled for that id, so a serial rather than a flag: the daemon does not
    write this file and so could not clear a flag. Bumping it twice while numpty
    is stopped fires the task once at startup. *)

(** {1 Printing} *)

val list : Agentkit.Schedule.t -> string
(** [list s] is the tasks of [s] as a table, one line each, with the trigger,
    whether it is disabled, its [run_now] serial and the first line of its
    prompt. *)

val check :
  zone:Agentkit.Schedule.zone ->
  now:float ->
  last:(string -> float option) ->
  Agentkit.Schedule.t ->
  string
(** [check ~zone ~now ~last s] is {!list} with, for each task, when it fires
    next. [last] is when the journal last has a [wake] for that task, since the
    journal is the only state and a next fire time is computed rather than
    stored. A task that never fires again says so. *)
