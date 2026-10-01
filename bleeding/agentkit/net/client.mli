(*---------------------------------------------------------------------------
   Copyright (c) 2026 Anil Madhavapeddy. All rights reserved.
   SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

(** The numptyd end that numpty holds.

    A client spawns numptyd, reads its output from the first byte on a fiber of
    its own, and turns each network operation into one call and one answer in
    the protocol {!Proto} defines. It exists so that a process holding a model
    forks once, to make numptyd, and never again.

    Every failure of the far end is final. There is no respawn, because making
    another numptyd would fork the process that holds the model, which is the
    cost running the network apart is there to avoid. From the first fault
    onwards every call answers at once with what went wrong and the tail of
    numptyd's standard error, so that a caller, and the model it serves, are
    told rather than left waiting.

    That fault is terminal for the run as well as for the session. {!fault} is
    how the daemon sees it: a turn in flight finishes on the answers this gives
    it, the handover is taken, and the run stops nonzero, so a supervisor's
    restart is what gets the network back. *)

type t
(** A running numptyd and the session with it. *)

val start :
  sw:Eio.Switch.t ->
  proc:[> [ `Generic | `Unix ] Eio.Process.mgr_ty ] Eio.Resource.t ->
  clock:_ Eio.Time.clock ->
  trace:(string -> unit) ->
  argv:string list ->
  (t, string) result
(** [start ~sw ~proc ~clock ~trace ~argv] spawns [argv], which is
    [["numpty"; "netd"]] or a stand-in a test supplies, and returns once numptyd
    has greeted.

    Every line numptyd writes belonging to no call, during the start and after
    it, is passed to [trace]. An exception [trace] raises is discarded, since a
    caller that cannot record a line must not end the session.

    The error is a spawn that failed, a line numptyd wrote that is not a
    message, an exit before the greeting, or a greeting that did not arrive
    within thirty seconds. numptyd is killed in each of those cases and the tail
    of its standard error is part of the message.

    numptyd is killed when [sw] is released, so no session outlives its switch.
*)

val hello : t -> Proto.hello
(** [hello t] is the greeting numptyd sent, saying whether it found a curl and
    carrying the note a person or a journal is shown. *)

val trace : t -> string -> unit
(** [trace t line] passes [line] to the sink {!start} was given, as numptyd's
    own id-less traces are passed. It is there so that a caller which streams a
    call's traces has one channel to stream them to rather than two, and an
    exception it raises is discarded as {!start}'s is. *)

val call :
  t ->
  ?timeout:float ->
  Proto.op ->
  on_trace:(string -> unit) ->
  (string, string) result
(** [call t op ~on_trace] performs [op] and is the text of the answer, ordinary
    output and refusals alike. Every trace numptyd writes while the call runs is
    passed to [on_trace], and an exception it raises is discarded as [trace]'s
    is. Calls are sequential, so a second one waits for the first to be
    answered.

    An operation numptyd will not serve, such as a fetch on a machine with no
    curl, and a body over the bound, are each an [Ok] carrying the refusal in
    words. The protocol is not what failed there, and the words are for the
    model to read.

    The [Error] is the death of the session, which is permanent. [timeout]
    bounds the wait in seconds and defaults to the bound the operation carries:
    a minute for a fetch or a head, which curl bounds below that anyway, and
    five minutes for a run, which is somebody else's program. Exceeding it kills
    numptyd and answers with the operation and the bound.

    A call cannot be taken back once it is sent, so a fiber cancelled while
    waiting for one ends the session. Cancelling is the caller saying it will
    not wait, and numptyd would go on to answer a question nobody asked, which
    the next call would then be matched against. *)

val alive : t -> bool
(** [alive t] is whether calls are still worth making. It is false once the
    session has died or been stopped, and from then on every call is an [Error]
    giving the reason it first died of. *)

val fault : t -> string option
(** [fault t] is why numptyd is gone, where it went of its own accord or was
    killed for not answering, and [None] where it is running or was stopped by
    {!stop}.

    A stop is not a fault. The daemon reads this at the end of a turn to decide
    whether the run may go on, so a session it closed itself must not read as
    one that broke. *)

val stop : t -> unit
(** [stop t] sends the shutdown message, closes numptyd's standard input, which
    numptyd reads as a shutdown too, and waits a few seconds for it to exit. One
    that has not gone by then is killed. Calling it twice, or on a session that
    has already died, does nothing.

    A call another fiber has in flight is abandoned rather than waited for, and
    answers at once with the stop. Waiting for it would mean waiting on the
    numptyd being closed, which is what a caller reaching for [stop] has decided
    against. *)
