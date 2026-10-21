(*---------------------------------------------------------------------------
   Copyright (c) 2026 Anil Madhavapeddy. All rights reserved.
   SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

(** A tool-using agent loop over the DeepSeek-V4 engine.

    An agent holds a conversation and a set of {!Tool.t}s the model may call.
    Each user turn runs to completion. The model generates a reply, and if that
    reply calls tools the agent runs them, adds their results to the
    conversation, and generates again. The turn ends when the model replies with
    text alone.

    {[
    let agent =
      Eio.Path.with_subtree Eio.Path.(fs / "workspace") @@ fun ws ->
      Agent.create engine
        ~tools:
          [ Toolbox.list ~dir:ws; Toolbox.read ~dir:ws; Toolbox.grep ~dir:ws ]
    in
    Agent.send agent "read README.md and summarise it" ~on_event:(function
      | Agent.Content c -> print_string c
      | _ -> ())
    ]} *)

(** {1 Events} *)

type stats = {
  ctx_used : int;  (** tokens the session holds *)
  ctx_size : int;  (** the context window they must fit in *)
  prompt_tokens : int;  (** length of the conversation at the last turn *)
  generated : int;  (** tokens generated during the exchange *)
  generate_seconds : float;  (** time spent generating them *)
  prefill_seconds : float;  (** time spent prefilling prompts *)
  tool_calls : int;  (** tools called during the exchange *)
  turns : int;  (** model turns the exchange took *)
  drafted : int;
      (** of [generated], the tokens a draft head proposed and the model
          accepted, which cost no step of their own *)
  total_generated : int;  (** tokens generated since the agent was created *)
  total_generate_seconds : float;  (** time spent generating them *)
}
(** What an exchange has cost so far. The [ctx_] fields describe the session as
    it stands. The [total_] fields cover the agent's whole life. The rest cover
    the current {!send}, accumulating over its turns. *)

type cut = {
  tokens : int;  (** the ceiling the reply stopped at *)
  tool_call : bool;
      (** whether it stopped part way through a tool call, which is then
          discarded and never made *)
}
(** A reply that generation stopped rather than the model. *)

type compaction = {
  before : int;  (** tokens the conversation held *)
  after : int;  (** tokens it holds now *)
  summary : string;  (** what the model wrote in place of the rest *)
}
(** A conversation replaced by a summary. See {!send}. *)

(** Agent progress, passed to {!send}'s [on_event] callback as it happens. *)
type event =
  | Reasoning of string  (** part of the model's reasoning text *)
  | Content of string  (** part of the model's reply *)
  | Tool_call of Dsml.tool_call  (** the model asked to call a tool *)
  | Tool_result of string * string  (** a tool name and what it returned *)
  | Stats of stats  (** what the exchange has cost after each model turn *)
  | Expanded of int  (** the context grew to this many tokens *)
  | Cut_off of cut
      (** generation reached its ceiling and stopped the reply, rather than the
          model ending it. A reply of text is left unfinished, and a tool call
          is discarded: {!send} then tells the model so and gives it another
          turn. *)
  | Squeezed of int
      (** the context could not grow, so the turn had only this many tokens to
          reply in. This says the room was short before the turn ran; {!Cut_off}
          says a reply actually hit the end of it. *)
  | Compacted of compaction
      (** the conversation no longer fitted, and was replaced by a summary the
          model wrote, the system prompt, and a verbatim tail of the most recent
          history *)
  | Done  (** the turn ended with a plain text reply *)

(** {1 Agent} *)

type t
(** An agent: an engine, a conversation, and the tools the model may call. *)

exception Context_exhausted of { needed : int; ctx : int }
(** Raised by {!send} when the conversation needs [needed] tokens, does not fit
    in a window of [ctx], and the context has already reached [max_ctx_size]. A
    conversation that fits but leaves too little room to reply in does not raise
    this. It grows the context, compacts the conversation once it cannot grow,
    and reports {!Squeezed} once neither helps. The conversation is restored to
    its state before the failed turn, so the agent remains usable. Retry with a
    shorter prompt or start again. *)

exception Tool_call_cut_off of { tokens : int; attempts : int }
(** Raised by {!send} when [attempts] turns in a row ended with generation
    reaching its [tokens] ceiling part way through a tool call, each of them
    making no other call. Nothing those turns asked for was done.

    A single such turn is not this. The model is told what happened and given
    another turn, since it cannot see the ceiling and would otherwise have the
    work reported as finished. This is what is left when telling it does not
    help. The conversation is restored as it is for {!Context_exhausted}. *)

exception Empty_reply of { attempts : int }
(** Raised by {!send} when the model ends [attempts] turns without producing
    content or a tool call. The model is prompted to answer after each earlier
    empty turn. The conversation is restored to its state before {!send}. *)

exception Malformed_tool_call of { message : string; attempts : int }
(** Raised after [attempts] consecutive malformed tool calls. Each refused call
    is shown to the caller and the model is told the correct form before this
    exception is raised. No partial call is invoked. *)

val create :
  ?system:string ->
  ?thinking:Dsml.thinking_mode ->
  ?ctx_size:int ->
  ?temperature:float ->
  ?top_p:float ->
  ?min_p:float ->
  ?seed:int64 ->
  ?max_tokens:int ->
  ?tool_result_limit:int ->
  ?max_ctx_size:int ->
  ?now:(unit -> float) ->
  ?tools:Tool.t list ->
  V4.engine ->
  t
(** [create engine] starts a conversation on [engine]. [system] is the system
    prompt, and [tools] are advertised to the model within it. [thinking]
    selects a direct reply, the default, or a reasoned one. [temperature],
    [top_p], [min_p], [seed] and [max_tokens] match {!V4.generate}.

    Sampling defaults follow the model family, as upstream's agent sets them.
    DeepSeek and Qwen use temperature 1, top-p 1 and min-p 0.05. GLM uses
    temperature 1, top-p 0.95 and min-p 0.

    [now] measures elapsed generation and prefill time. It must be monotonic.

    [ctx_size] is the context window and defaults to 32768 tokens. An agent
    accumulates tool output as well as conversation, so it needs a larger window
    than a single prompt does. Memory use grows with it.

    [tool_result_limit] is how many characters of a tool result may be added to
    the conversation, and defaults to 4000. A longer result has its middle
    removed, and says so along with what to do about it, since what goes that
    way cannot be asked for again. Results are also shortened when the
    conversation and a full reply would exceed [max_ctx_size]. [on_event] still
    receives the whole result.

    [max_ctx_size] is how far the context may grow, and defaults to 262144
    tokens. The context grows once a turn no longer leaves room for a
    [max_tokens] reply, rather than once the prompt no longer fits, so a reply
    is not cut short while the window could still have grown. The conversation
    moves to a larger session on the same engine, so the model is not reloaded,
    and an {!Expanded} event reports the new size. The move costs one prefill of
    the whole conversation, so the context doubles rather than creeping. At
    [max_ctx_size] a turn runs in the room that is left and reports {!Squeezed}.
    Set it to [ctx_size] to keep the context fixed. *)

val stats : t -> stats
(** [stats t] is what the most recent {!send} cost and how full the context is.
    {!send} also reports this as a {!Stats} event after every model turn, which
    is what to use to follow a long exchange as it runs. *)

val prefill_progress : t -> int * int
(** [prefill_progress t] is the number of tokens the turn now running has
    prefilled and the number it expects, and [(0, 0)] when nothing is
    prefilling. The two are equal once a prefill has finished.

    This is for an interface that wants to show progress while {!send} is
    blocked in another fiber, which is where the wait is worth reporting: a turn
    that follows an {!Expanded} event prefills the whole conversation again, and
    that is minutes of nothing else happening. It is safe to call from another
    fiber on the same domain as the one in {!send}, and unlike {!stats} it does
    not wait for the engine. The count advances a chunk at a time rather than a
    token at a time, and a prompt the engine prefills in one chunk is reported
    only once it is done. *)

val cancel : t -> unit
(** [cancel t] interrupts the prefill or generation now running on [t]. The
    interrupted {!send} raises {!V4.Session_interrupted}. It retains any tool
    results already committed because their effects cannot be undone. *)

val send : t -> on_event:(event -> unit) -> string -> unit
(** [send t ~on_event prompt] adds [prompt] to the conversation and runs the
    agent until the model replies with text alone, passing each {!event} to
    [on_event]. The conversation is kept, so a later call continues it.

    A turn whose tool call ran past the [max_tokens] ceiling reports {!Cut_off},
    and the call is discarded: the model was still writing it, so there is
    nothing to run. The conversation then carries a note saying so, and the
    model takes another turn, which is what lets it answer with a smaller call.
    What the model wrote of the discarded call is reported as {!Content} but
    kept out of the conversation. Repeating that on {!Tool_call_cut_off}'s terms
    raises it.

    A malformed call is reported as content, refused without invoking a tool,
    and followed by a tool-role correction. Three malformed calls running raise
    {!Malformed_tool_call}.

    A conversation that no longer fits a context at [max_ctx_size] is compacted,
    at most once a turn. The model writes a summary of it, greedily and without
    reasoning, in at most 4096 tokens or an eighth of the context, and the
    conversation is rebuilt from the system prompt, that summary, and a verbatim
    tail: as much of the recent conversation as fits a tenth of the context, up
    to the turn now in progress, which is always kept whole and never summarised
    away. {!Compacted} reports it. The same happens before a tool result that
    would otherwise lose its middle. A turn that compacted keeps the compaction
    if it then fails, so the conversation is restored to the compacted one
    rather than to the one before the prompt. A conversation holding an image is
    never compacted. *)

val close : t -> unit
(** [close t] releases the session's KV cache now rather than at the collector's
    convenience, which matters to a program that creates one agent after another
    over a long run, since a cache is sized to the context it serves. [t] cannot
    be used afterwards: {!send} and {!stats} raise [Invalid_argument] saying the
    agent is closed. Closing a closed agent does nothing. *)

(** {1 Context arithmetic}

    What {!send} decides about the window, apart from the engine so that it can
    be checked without one. *)

val has_room : squeeze:bool -> max_tokens:int -> needed:int -> ctx:int -> bool
(** [has_room ~squeeze ~max_tokens ~needed ~ctx] is whether a turn whose prompt
    is [needed] tokens may run in a window of [ctx] tokens. It must hold room
    for a [max_tokens] reply and for the token generation stops on, unless the
    turn is squeezed, in which case the prompt need only fit. A turn is squeezed
    when the context has reached [max_ctx_size] and cannot grow. *)

val grow_to :
  max_tokens:int -> max_ctx_size:int -> needed:int -> ctx:int -> int option
(** [grow_to ~max_tokens ~max_ctx_size ~needed ~ctx] is the window to move a
    session of [ctx] tokens to so that a prompt of [needed] tokens has room for
    a full reply, or [None] if [max_ctx_size] admits no such window. The result
    doubles [ctx] where that is enough, since the move costs a prefill of the
    whole conversation. *)
