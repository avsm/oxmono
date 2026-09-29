(*---------------------------------------------------------------------------
   Copyright (c) 2026 Anil Madhavapeddy. All rights reserved.
   SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

(** What a running numpty is doing now, which is the one thing the files cannot
    say.

    {!Ds4.Agent.send} blocks for minutes at a time and {!Ds4.Agent.stats} waits
    for the engine, so the control fiber must never ask the agent anything. The
    agent loop instead publishes one of these, an ordinary immutable record, at
    every turn boundary and at every state change it makes, and the control
    fiber reads the record.

    The exception is {!job.prefilled}, which is live. It comes from
    {!Ds4.Agent.prefill_progress}, which reads two atomics the engine's progress
    hook writes and does not wait for the engine. That is what lets a status
    answered during a four minute prefill say how far it has got.

    So a status is as fresh as the last turn boundary, apart from the prefill
    counter. A turn that runs for four minutes reports the tool call it is in
    and the context it had when the turn began, since reading anything more
    current would mean entering the engine the turn is inside. *)

type job = {
  task : string;  (** the task the wake-up is working on *)
  started : string;  (** when the wake-up began, in RFC 3339 UTC *)
  session : int;  (** which session of the wake-up, counting from 1 *)
  ctx_used : int;  (** tokens the session held at the last turn boundary *)
  ctx_size : int;  (** the window they must fit in *)
  turns : int;  (** model turns the session has taken *)
  tool_calls : int;  (** tools it has called *)
  tool : string option;  (** the tool call in flight, if one is *)
  prefilled : int;  (** tokens the turn now running has prefilled *)
  prefill_total : int;  (** how many it expects, and 0 when nothing is *)
}
(** The one job that is running. There is at most one, since a process holds one
    engine and the loop is sequential. *)

type due = {
  task : string;
  next : string option;
      (** when it next fires, in RFC 3339 UTC, and [None] for a task that never
          fires again *)
  waiting : bool;  (** whether it is due now and waiting for the engine *)
}
(** A task the schedule holds and when it fires next. Next fire times are not
    stored, they are computed from the journal, so this is the daemon's own
    arithmetic rather than a file being read back. *)

type t = {
  run : int;  (** the run number every journal record carries *)
  since : string;  (** when the run started, in RFC 3339 UTC *)
  model : string;  (** the model it loaded *)
  backend : string;  (** the inference backend it linked *)
  netd : string;  (** ["alive"], or the fault that killed the network child *)
  version : int;  (** the memory version in force *)
  job : job option;
  tasks : due list;
}
(** The whole of what a run knows about itself. The agent loop publishes it and
    the answers below are projections of it. *)

(** {1 The answers}

    Each is a plain record with a jsont codec, which maps onto a capnp struct
    without rearrangement. That is what makes capnp-rpc a second adapter over
    the same surface rather than a second surface. *)

type running = {
  r_run : int;
  r_since : string;
  r_model : string;
  r_backend : string;
  r_netd : string;
  r_version : int;
  r_job : string option;  (** the id of the running job, if one is running *)
}
(** The answer to [status]: the daemon itself. *)

type jobs = { j_job : job option; j_tasks : due list }
(** The answer to [jobs]: the work. The running job, and beside it every task
    with when it fires next. *)

type memory = { m_version : int; m_text : string }
(** The answer to [memory]. It is served from the store rather than from the
    daemon's memory, so that one client reaches everything without knowing which
    side of the line a thing falls on. *)

type lines = { lines : string list }
(** The answer to [log], and one line of a [follow]. *)

val running : t -> running
(** [running t] is the [status] answer for [t]. *)

val jobs : t -> jobs
(** [jobs t] is the [jobs] answer for [t]. *)

val job_jsont : job Jsont.t
val due_jsont : due Jsont.t
val running_jsont : running Jsont.t
val jobs_jsont : jobs Jsont.t
val memory_jsont : memory Jsont.t
val lines_jsont : lines Jsont.t
