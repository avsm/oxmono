(** A single structured model completion without tool execution. *)

type finish = Stop | Length | Tool_calls
type t = { message : Dsml.parsed_message; finish : finish }

val dialect : V4.engine -> Dsml.dialect
(** [dialect engine] is the prompt protocol used by [engine]. *)

val complete :
  V4.engine ->
  ctx_size:int ->
  ?max_tokens:int ->
  ?thinking:Dsml.thinking_mode ->
  ?reasoning_effort:Dsml.reasoning_effort ->
  Dsml.message list ->
  t
(** [complete engine ~ctx_size messages] generates one reply to [messages].
    Tools advertised in the system message are returned as calls and are never
    executed. [Length] responses contain no executable calls. The caller must
    neutralise reserved tokens in untrusted message content with
    {!Dsml.neutralise_specials}. Cancellation releases the session after its
    native operation finishes. An oversized prompt raises
    {!Agent.Context_exhausted}. Malformed tool syntax raises
    {!Dsml.Parse_error}. Maximum reasoning is reduced to high below a 384K
    context. *)
