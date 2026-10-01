(*---------------------------------------------------------------------------
   Copyright (c) 2026 Anil Madhavapeddy. All rights reserved.
   SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

(** The loop that waits for a task to be due.

    It stats the schedule file on every tick and rereads it when it changes,
    asks {!Agentkit.Schedule.poll} which tasks are due, and fires them one at a
    time. One job runs at a time, because a process holds one engine.

    The journal is the only state. When a task last fired, whether a [once] task
    is done and whether a [run_now] serial has been honoured are all read out of
    it at startup, so a firing is idempotent under a reread and survives a
    restart with nothing to transfer.

    Nothing here writes the schedule file. That is [numpty task]'s, which runs
    as the person and holds no model, so the orders stay a thing a person can
    read, diff and keep in git and nothing numpty concludes can change what it
    was told to do. *)

(** Why the loop stopped. *)
type stopped =
  | Signalled of string
      (** the signal that asked it to. The turn in flight finished, the handover
          was taken, and the run may exit zero. *)
  | Faulted of string
      (** numptyd is gone. The wake-up that met it handed over, and the run must
          exit nonzero, since there is no respawn and healing is a supervisor's
          restart. *)

val stopping : unit -> string option
(** [stopping ()] is the signal that has asked the run to stop, and [None] until
    one has. A wake-up reads it at its turn boundary and takes the handover
    rather than starting more work, which is what makes [SIGTERM] finish the
    turn in flight rather than cut it off. *)

val run :
  clock:_ Eio.Time.clock ->
  store:Store.t ->
  schedule:_ Eio.Path.t ->
  tick:float ->
  publish_tasks:(Status.due list -> unit) ->
  wake:(task:string -> prompt:string -> Wake.outcome) ->
  stopped
(** [run ~clock ~store ~schedule ~tick ~publish_tasks ~wake] polls every [tick]
    seconds and calls [wake] for each task that is due, in file order, one at a
    time.

    [publish_tasks] is given every task and when it next fires, on every tick,
    for the control socket to answer with. Next fire times are not stored, they
    are computed from the journal, so this is the only place they exist.

    It journals a [schedule_load] whenever the file is read, saying what was
    read and what changed, and a [wake] before each firing, saying which task,
    the due time it is firing for, why it fired now and the [run_now] serial
    where that is why. A schedule file that does not parse is reported at error
    level and journalled, and the schedule in force stays in force, since a
    person mid-edit should not lose a daemon.

    For its duration it takes over [SIGHUP], which rereads the file at once, and
    [SIGTERM] and [SIGINT], which stop it. A signal that arrives while a wake-up
    is running is read at the next turn boundary, so the turn in flight finishes
    and hands over rather than being cut off. The previous handlers are put back
    before it returns. *)
