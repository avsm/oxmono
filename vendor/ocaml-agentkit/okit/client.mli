(*---------------------------------------------------------------------------
   Copyright (c) 2026 Anil Madhavapeddy. All rights reserved.
   SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

(** The okitd end that humpty holds.

    A client spawns okitd, reads its output from the first byte on a fiber of
    its own, and turns each okit operation into one call and one answer in the
    protocol {!Proto} defines. It exists so that a process holding a model forks
    once, to make okitd, and never again.

    Every failure of the far end is final. There is no respawn, because making
    another okitd would fork the process that holds the model, which is the cost
    running okit apart is there to avoid. From the first fault onwards every
    call answers at once with what went wrong and the tail of okitd's standard
    error, so that a caller, and the model it serves, are told rather than left
    waiting. *)

type t
(** A running okitd and the session with it. *)

val start :
  sw:Eio.Switch.t ->
  proc:[> [ `Generic | `Unix ] Eio.Process.mgr_ty ] Eio.Resource.t ->
  clock:_ Eio.Time.clock ->
  trace:(string -> unit) ->
  argv:string list ->
  (t, string) result
(** [start ~sw ~proc ~clock ~trace ~argv] spawns [argv], which is
    [["humpty"; "okitd"; "--dir"; ws]] or a stand-in a test supplies, and
    returns once okitd has greeted.

    okitd starts a dune session before it greets and writes a trace for each
    step that takes, so the greeting can be tens of lines and tens of seconds
    away. Every line belonging to no call, during the start and after it, is
    passed to [trace]. An exception [trace] raises is discarded, since an
    interface that cannot draw a line must not end the session.

    The error is a spawn that failed, a line okitd wrote that is not a message,
    an exit before the greeting, or a greeting that did not arrive within ninety
    seconds. okitd is killed in each of those cases and the tail of its standard
    error is part of the message.

    okitd is killed when [sw] is released, so no session outlives its switch. *)

val hello : t -> Proto.hello
(** [hello t] is the greeting okitd sent, saying which tool families answer and
    carrying the note the interface shows. The note holds a refused session's
    reason whole, over several lines, so a caller with room for one line folds
    it first. *)

val trace : t -> string -> unit
(** [trace t line] passes [line] to the sink {!start} was given, as okitd's own
    id-less traces are passed. It is there so that a caller which streams a
    call's traces has one channel to stream them to rather than two, and an
    exception it raises is discarded as {!start}'s is. *)

val call :
  t ->
  ?timeout:float ->
  Proto.op ->
  on_trace:(string -> unit) ->
  (string, string) result
(** [call t op ~on_trace] performs [op] and is the text of the answer, ordinary
    output and refusals alike. Every trace okitd writes while the call runs is
    passed to [on_trace], and an exception it raises is discarded as [trace]'s
    is. Calls are sequential, so a second one waits for the first to be
    answered.

    An operation okitd will not serve, such as a build in a workspace with no
    [dune-project], is an [Ok] carrying the refusal in words. The protocol is
    not what failed there, and the words are for the model to read.

    The [Error] is the death of the session, which is permanent. [timeout]
    bounds the wait in seconds and defaults to no bound, the caller being the
    end that knows how long the operation is worth. Exceeding it kills okitd and
    answers with the operation and the bound.

    A call cannot be taken back once it is sent, so a fiber cancelled while
    waiting for one ends the session. Cancelling is the caller saying it will
    not wait, and okitd would go on to answer a question nobody asked, which the
    next call would then be matched against. *)

val alive : t -> bool
(** [alive t] is whether calls are still worth making. It is false once the
    session has died or been stopped, and from then on every call is an [Error]
    giving the reason it first died of. *)

val stop : t -> unit
(** [stop t] sends the shutdown message, closes okitd's standard input, which
    okitd reads as a shutdown too, and waits a few seconds for it to exit. One
    that has not gone by then is killed. Calling it twice, or on a session that
    has already died, does nothing.

    A call another fiber has in flight is abandoned rather than waited for, and
    answers at once with the stop. Waiting for it would mean waiting on the
    okitd being closed, which is what a caller reaching for [stop] has decided
    against. *)
