(*---------------------------------------------------------------------------
   Copyright (c) 2026 Anil Madhavapeddy. All rights reserved.
   SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

(** One wake-up.

    A wake-up creates a fresh agent on the selected backend, sends it the brief,
    lets it work, asks it what should survive, and closes it. The engine is
    loaded once for the life of the run, so a new context costs a prefill and
    not a model load.

    Everything the agent does reaches the journal before it is allowed to
    proceed, from the event callback {!Agentkit.Driver.send} calls. A journal
    that cannot be written raises from there and the wake-up stops, since an
    account with a hole in it reads as complete.

    A turn boundary is the only place this loop can act, because
    {!Agentkit.Driver.send} runs a whole turn including its tool calls. Two
    things are decided there: whether the context has filled far enough to hand
    over and carry on in a fresh session, and whether numptyd has faulted. *)

type config = {
  ctx_size : int;  (** the context a session starts with *)
  max_ctx_size : int;  (** how far it may grow *)
  handover_at : float;
      (** the fraction of [max_ctx_size] past which the wake-up stops feeding
          the agent work and hands over, three quarters by default *)
  max_sessions : int;
      (** how many sessions one wake-up may take. A task that fills its context
          every time would otherwise carry on for ever, and a daemon has other
          tasks waiting. *)
  thinking : Dsml.thinking_mode;
  seed : int64;
  tool_result_limit : int;
      (** how much of a tool result reaches the model, which is also what a
          [tool_result] record calls truncated *)
}

val default_config : config
(** [default_config] is a 32768 token context growing to 262144, handing over at
    three quarters of that, at most five sessions to a wake-up, replying without
    reasoning, and the agent's own tool result limit. *)

(** How a wake-up ended. *)
type outcome =
  | Finished  (** the agent finished and handed over *)
  | Faulted of string
      (** numptyd is gone. The turn finished on the answers it gave the agent,
          the handover was taken, and the run must now stop nonzero, since there
          is no respawn and healing is a supervisor's restart. *)

val run :
  cancel_on_stop:bool ->
  clock:_ Eio.Time.clock ->
  store:Store.t ->
  create:(system:string -> Agentkit.Driver.session) ->
  config:config ->
  fault:(unit -> string option) ->
  stop:(unit -> bool) ->
  publish:(Status.job option -> unit) ->
  prefill:((unit -> int * int) -> unit) ->
  task:string ->
  prompt:string ->
  outcome
(** [run ~store ~create ~config ~fault ~stop ~task ~prompt] works one wake-up on
    [task] and is how it ended.

    [cancel_on_stop] is false for a backend whose cancellation closes the
    session, so the active turn completes before the handover uses that session.

    [create ~system] builds a session for each brief. It must include memory
    tools, since without them nothing the agent learns survives the wake-up.

    [fault] is read at each turn boundary, and is {!Numpty_net.Client.fault}
    where a numptyd is held. Its answer ends the wake-up after the handover
    rather than at once, so that the model is told what happened and can write
    it down.

    [stop] is read there too, and is {!Daemon.stopping} under the daemon. It
    ends the wake-up after the handover rather than carrying on into another
    session, which is what makes a [SIGTERM] finish the turn in flight rather
    than cut it off.

    [publish] is given a whole {!Status.job} at every state change, and [None]
    once the wake-up is over. It is how the control socket answers while a turn
    is blocked, so it must not do anything that waits. [prefill] is given a
    reader for the session's prefill progress each time a session opens, and one
    that reads zero when it closes. That reader is
    {!Ds4.Agent.prefill_progress}, which is the only call into the agent a
    control fiber may make.

    It journals a [brief] before each session, a [prompt] before each message,
    every event the agent reports, a [handover] naming the memory version the
    session finished at, and a [continued] linking a session to the one it
    succeeded. It raises whatever the journal raised, and the run then stops. *)
