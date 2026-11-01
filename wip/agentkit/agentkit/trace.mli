(*---------------------------------------------------------------------------
   Copyright (c) 2026 Anil Madhavapeddy. All rights reserved.
   SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

(** The fold from what an agent does to what an account of it says.

    {!Agent} reports a reply a token at a time and a tool call without the
    result that answers it. A {!type:Journal.kind} is a whole statement: one
    passage of text, or one call paired with its result and timed. This turns
    the first into the second, and knows nothing about where the kinds go. A
    command sends them to a store, to a stream, or to both.

    One trace covers a whole run, not a turn and not a session. The call ids it
    hands out count from 1 over its life, which is what a person reading the
    account follows across a session that handed over and continued. *)

type t
(** A fold in progress, holding the text of the turn now running and the calls
    it has not yet seen a result for. *)

val create :
  ?tool_result_limit:int ->
  ?now:(unit -> float) ->
  emit:(Journal.kind -> unit) ->
  unit ->
  t
(** [create ~emit ()] is a fold that passes each kind to [emit] as it becomes
    complete. An exception [emit] raises is let out, since a caller writing to a
    journal must stop the run rather than go on producing an account that reads
    as complete.

    [tool_result_limit] is how much of a result the model is given, and defaults
    to 4000 characters. A result longer than it is marked truncated, so the
    account says the model saw less than the record holds. A limit of zero or
    less means no limit, as it does in the adapter, and nothing is marked
    truncated. Pass the agent's own limit when it differs.

    [now] reads the POSIX clock and defaults to [Unix.gettimeofday]. A call's
    [seconds] is the span between two readings of it, one when the call appeared
    in the event stream and one when its result arrived. That is not the time
    the tool itself took. A turn is decoded before any of its calls is made, so
    the span for the first call of a turn that asked for several also covers the
    rest of that turn's token generation. *)

val event : t -> Agent.event -> unit
(** [event t ev] folds [ev] in, emitting whatever it completed.

    Reasoning and content are buffered and each is emitted as one kind at the
    boundary it is complete at, which is the tool call it precedes, the stats
    that end a turn, or the end of the exchange. Reasoning comes before content.
    A record per token would be an account nobody can read.

    A turn asks for all of its tool calls before any of them is made, so each
    result is paired with the oldest call still waiting. A result that answers
    no call is emitted with call 0 rather than dropped.

    The pairing holds only while every {!Agent.Tool_call} is answered by exactly
    one {!Agent.Tool_result}. An adapter should turn a handler exception into a
    result. A caller feeding events of its own making that leaves a call
    unanswered shifts every later pairing by one. *)
