(*---------------------------------------------------------------------------
   Copyright (c) 2026 Anil Madhavapeddy. All rights reserved.
   SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

(** One user turn: model requests and tool calls until there is an answer.

    The loop offers tools until the model answers or spends its call allowance.
    It then makes one tool-free request for an answer, retries once if that
    reply is empty, and otherwise returns a fallback. It never returns blank
    text. *)

type guard = Agent.tool_call -> (unit, string) result
(** Decides whether a call may run. An [Error] is the reason given to the model.
    A guard that raises denies the call. *)

val unguarded : string -> guard
(** [unguarded reason] allows every call. [reason] says why no check is needed,
    so that an unguarded tool set is visible where it is built. *)

type event =
  | Request of { round : int; budget : int }
      (** a model request, with the calls still allowed *)
  | Budget_exceeded of { calls : int; budget : int }
      (** the model asked for more calls than allowed, and none ran *)
  | Empty of { error : exn option }
      (** the answer request failed or was blank, so it is retried once *)
  | Recovered  (** the retry produced an answer *)
  | Fallback  (** no answer was produced *)
  | Cut_off  (** the answer reached the token ceiling and is marked *)

exception Budget_exceeded
(** Raised when the model asks for more calls than its remaining allowance. *)

val run :
  complete:Chat.complete ->
  tools:Agent.Tool.t list ->
  guard:guard ->
  dispatch:(Agent.tool_call -> (string, string) result) ->
  ?around:(Agent.tool_call -> (unit -> (string, string) result) -> string) ->
  ?check:(unit -> unit) ->
  ?on_event:(event -> unit) ->
  ?budget:int ->
  ?max_tokens:int ->
  ?max_result_bytes:int ->
  ?max_answer_bytes:int ->
  ?fallback:(tools_used:bool -> string) ->
  Chat.message list ->
  string
(** [run ~complete ~tools ~guard ~dispatch messages] answers the transcript
    [messages], whose first message should be the {!Chat.System} prompt.

    Each call passes [guard], then [dispatch]. [around call f] wraps every call,
    including refused ones, and renders its result. It is the place to audit.
    The default renders [Error e] as ["Error: " ^ e]. Results are clipped to
    [max_result_bytes], 32768 by default.

    [budget] is the number of calls allowed, 6 by default. Asking for more than
    remain runs [around] on each with an error and raises {!Budget_exceeded}.
    When the allowance is spent the answer request has no tools. Its
    instruction is added to the system prompt and repeated as a final user
    message, because a model part way through a tool plan ignores a distant
    system message and answers with nothing.

    [check] runs before every request and call and may raise to abort, for
    example when the requester loses access. Transport failures raise, except
    during the tool-free answer request, which is retried instead. An answer
    that reached the token ceiling ends with a marker. Answers are clipped to
    [max_answer_bytes], 12000 by default. [fallback ~tools_used] is the reply
    when no answer was produced. *)

val bind :
  guard:guard ->
  dispatch:(Agent.tool_call -> (string, string) result) ->
  ?around:(Agent.tool_call -> (unit -> (string, string) result) -> string) ->
  ?max_result_bytes:int ->
  Agent.Tool.t list ->
  Agent.Tool.t list
(** [bind ~guard ~dispatch tools] attaches the same guarded execution to each
    tool, for adapters such as DS4 that run tools inside their own loop. *)

val clip : bytes:int -> string -> string
(** [clip ~bytes s] is [s], or its longest UTF-8 prefix within [bytes] followed
    by a ["\n[truncated]"] line. *)
