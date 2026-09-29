(*---------------------------------------------------------------------------
   Copyright (c) 2026 Anil Madhavapeddy. All rights reserved.
   SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

(** The DS4 inference engine.

    Open a GGUF model, DeepSeek V4, DeepSeek V4.1 Flash, GLM 5.3 Flash or
    Qwen3.8 Flash Next, send it a prompt, and stream the generated tokens. Use
    {!generate} for a single prompt and reply, or {!Session} to keep a
    conversation across turns.

    The backend is fixed at link time by the implementation library that is
    linked in. This interface is the same for all of them. *)

type engine
(** A loaded model. *)

exception Session_interrupted
(** Raised when cooperative cancellation interrupts a session operation. *)

val backend : [ `Metal | `Cuda | `Cpu ]
(** The inference backend compiled into this build. *)

val log_src : Logs.src
(** The {!Logs} source for the engine's diagnostics, named [ds4.engine]. Its
    verbosity can be set independently of the rest of the program. *)

val forward_logs : unit -> unit
(** [forward_logs ()] sends the engine's diagnostics to {!Logs} instead of
    [stderr]. Errors and warnings keep their level. An untyped upstream
    diagnostic is a warning when its text reports a failure, such as the reason
    a model would not open, and an informational message otherwise, as are
    timings. Progress lines and internal traces are debug messages.

    Call this after configuring {!Logs}, and again after changing the level.
    Diagnostics emitted beforehand still go to [stderr]. *)

type think = [ `None | `High | `Max ]
(** Reasoning effort encoded into the chat prompt. *)

val create :
  ?domain_mgr:_ Eio.Domain_manager.t ->
  ?sw:Eio.Switch.t ->
  ?vision:_ Eio.Path.t ->
  ?mtp:bool ->
  cache:_ Eio.Path.t ->
  model:_ Eio.Path.t ->
  unit ->
  engine
(** [create ~cache ~model ()] opens the GGUF model at [model], writing the
    backend's GPU kernels under [cache] first. It raises [Failure] if the model
    cannot be opened.

    [vision] loads the vision sidecar matching the model, which exists for GLM
    5.3, DeepSeek V4 Vision Experimental, DeepSeek V4.1 Flash and Qwen3.8 Flash
    Next.

    [mtp] arms the multi-token prediction head that GLM 5.3 and Qwen3.8 Flash
    Next carry in their GGUF, as upstream's [--mtp] does, and defaults to false.
    {!Session.eval_speculative} then commits drafted tokens along with the
    sampled one. It has no effect on the CPU backend or on a DeepSeek model.
    {!draft_tokens} says whether it took.

    [domain_mgr] runs the engine on its own domain, leaving the calling domain
    free to run other fibers during inference. It requires [sw], which bounds
    the worker's lifetime, and raises [Invalid_argument] without it. Omit both
    to run inference on the calling domain.

    Calls to one engine are serialised, so it is never entered concurrently.

    Only one engine may be opened successfully per process, and a later call
    raises [Invalid_argument]. A failed attempt does not consume this allowance.
    The backend's kernels are located through the process environment, which a
    second engine would overwrite. Any number of sessions and agents may share
    the engine. *)

val has_draft_head : _ Eio.Path.t -> bool
(** [has_draft_head model] is whether the GGUF file [model] carries a
    multi-token prediction head, read from its metadata without loading it. It
    is false for a file that cannot be read or is not a GGUF, which the engine
    then reports when it opens it. *)

val draft_tokens : engine -> int
(** [draft_tokens engine] is how many tokens one speculative step may draft
    beyond the sampled one, and 0 when [engine] has no draft head armed. *)

val model_name : engine -> string
(** [model_name engine] is the model's name as recorded in its GGUF file. *)

val has_vision : engine -> bool
(** [has_vision engine] is true when a vision sidecar is loaded. *)

val vocab_size : engine -> int
(** [vocab_size engine] is the number of tokens in the model's vocabulary. *)

val token_eos : engine -> int
(** [token_eos engine] is the end-of-sequence token. Sampling returns it when a
    turn is complete. *)

val token_is_stop : engine -> int -> bool
(** [token_is_stop engine tok] is true when [tok] ends the turn. That is
    {!token_eos} alone for a DeepSeek model, [<|im_end|>] or [<|endoftext|>] for
    a Qwen model, and also any role marker such as the user or assistant tag for
    a GLM model, which ends its turns with them. A loop that compares against
    {!token_eos} alone runs a GLM model into the next simulated turn. *)

val token_text : engine -> int -> string
(** [token_text engine tok] is the text of token [tok]. Concatenating the text
    of a sampled sequence gives the generated reply. *)

val family : engine -> [ `Deepseek | `Deepseek41 | `Glm | `Qwen ]
(** [family engine] is the loaded model's family, which decides the markup a
    conversation is rendered in and the grammar its replies are read by.
    [`Deepseek] is DeepSeek V4 Flash or PRO, [`Deepseek41] is DeepSeek V4.1
    Flash, [`Glm] is GLM 5.3 and [`Qwen] is Qwen3.8 Flash Next. *)

val generate :
  engine ->
  ?system:string ->
  ?ctx_size:int ->
  ?max_tokens:int ->
  ?temperature:float ->
  ?top_p:float ->
  ?min_p:float ->
  ?think:think ->
  ?seed:int64 ->
  on_token:(string -> unit) ->
  string ->
  unit
(** [generate engine ~on_token prompt] sends [prompt] to the model and passes
    each piece of the reply to [on_token] as it is generated. It stops at a
    token {!token_is_stop} accepts or after [max_tokens] tokens, which defaults
    to 2048.

    [ctx_size] is the context window and defaults to 4096. The prompt and the
    reply must both fit within it. [temperature], [top_p] and [min_p] control
    sampling and default to 1.0, 1.0 and 0.05. *)

(** A conversation that keeps its KV cache across turns.

    Render the conversation, tokenise it with {!Session.tokenize}, feed it with
    {!Session.sync}, then alternate {!Session.sample} and {!Session.eval} to
    stream the reply. {!Session.sync} keeps whatever prefix the cache already
    holds, so extending a conversation by one turn only prefills the new tokens.
*)
module Session : sig
  type t
  (** A session on one engine, holding the KV cache and the sampling state.

      Drive a session from one fiber at a time. Calls are serialised
      individually but a sequence of them is not, so two fibers interleaving
      {!sample} and {!eval} on the same session would corrupt its cache.
      Separate sessions on one engine are safe. *)

  val create : engine -> ctx_size:int -> seed:int64 -> t
  (** [create engine ~ctx_size ~seed] opens a session with a context window of
      [ctx_size] tokens and its sampling seeded from [seed]. *)

  val tokenize : engine -> string -> int array
  (** [tokenize engine prompt] tokenises an already rendered conversation,
      leaving its markup untouched. Use it with the output of
      [Dsml.encode_messages]. *)

  val sync : t -> int array -> unit
  (** [sync t tokens] makes [tokens] the session's prompt, prefilling only those
      the KV cache does not already hold. *)

  val prefill_progress : t -> int * int
  (** [prefill_progress t] is the number of tokens the sync now running has
      prefilled and the number it expects, and [(0, 0)] when no sync is running
      or the session is closed. The two are equal once a sync has finished. The
      engine reports a chunk at a time, so the first figure advances in steps
      rather than token by token, and a prompt short enough to prefill in one
      chunk is reported only when it is done.

      Unlike every other call here this one does not take the engine's worker
      domain. It reads counters the engine writes as it prefills and touches no
      engine state, so it answers while a {!sync} on the same session is still
      running, which is the only time it says anything. Call it from another
      fiber on the same domain as the one blocked in {!sync}. *)

  val cancel : t -> unit
  (** [cancel t] asks the operation now running on [t] to stop. It is safe to
      call from another fiber and does not wait for the engine. *)

  val clear_cancel : t -> unit
  (** [clear_cancel t] lets later operations run after {!cancel}. *)

  val is_cancelled : t -> bool
  (** [is_cancelled t] is whether [t] has a pending cancellation request. *)

  val common_prefix : t -> int array -> int
  (** [common_prefix t tokens] is the number of leading [tokens] already held by
      the live KV cache. *)

  val directional_steering_ffn : t -> float
  (** [directional_steering_ffn t] is the live FFN steering scale of [t]. *)

  val set_directional_steering_ffn : t -> float -> unit
  (** [set_directional_steering_ffn t scale] changes the FFN steering scale for
      future evaluation without rebuilding the KV cache. Distributed sessions do
      not support live changes. *)

  val sample : t -> temperature:float -> top_p:float -> min_p:float -> int
  (** [sample t ~temperature ~top_p ~min_p] draws the next token. Pass it to
      {!eval} to commit it. *)

  val eval : t -> int -> unit
  (** [eval t tok] commits [tok] to the KV cache, so the next {!sample}
      continues from it. *)

  val eval_speculative :
    t ->
    int ->
    max:int ->
    temperature:float ->
    top_p:float ->
    min_p:float ->
    int array
  (** [eval_speculative t tok ~max ~temperature ~top_p ~min_p] commits [tok], as
      {!eval} does, and then as many tokens the draft head proposed as the model
      verifies, up to [max] in all. It returns the committed tokens, [tok]
      first. The sampling parameters decide acceptance and should be those [tok]
      was sampled with.

      On an engine whose {!draft_tokens} is 0 it is {!eval} and returns
      [[|tok|]]. Drafted tokens are committed without being checked by the
      caller, so the cache may hold a stop token or text past the point the
      caller wants to end at. {!rewind} to the length the caller keeps before
      the next {!sync}, since a cache that is not a prefix of the prompt is
      prefilled again from scratch. It raises [Invalid_argument] when [max] is
      not positive. *)

  val rewind : t -> int -> unit
  (** [rewind t pos] drops the cached tokens after the first [pos], keeping the
      rest, so a {!sync} to a prompt that extends those [pos] tokens prefills
      only what follows them. A [pos] at or past {!pos} does nothing. Where the
      engine cannot restore its state at [pos], the next {!sync} rebuilds it. *)

  val checkpoint_valid : t -> bool
  (** [checkpoint_valid t] is whether the retained tokens have usable cached
      state. If false after {!rewind}, call {!sync} before sampling or
      evaluating. For a transcript containing images, use {!Transcript.sync}. *)

  val close : t -> unit
  (** [close t] releases the session's KV cache now rather than at the next
      collection, which matters when replacing a session with a larger one,
      since both are otherwise live at once. Calling it twice is harmless, and
      the session cannot be used afterwards. *)

  val ctx : t -> int
  (** [ctx t] is the session's context window in tokens. *)

  val pos : t -> int
  (** [pos t] is the number of tokens the session's KV cache holds. *)
end

(** An exact rendered-chat token transcript. *)
module Transcript : sig
  type t

  val create : engine -> t
  (** [create engine] starts the model family's rendered-chat transcript. *)

  val of_tokens : engine -> int array -> t
  (** [of_tokens engine tokens] starts a transcript with [tokens]. *)

  val tokens : t -> int array
  (** [tokens t] is the exact token sequence held by [t]. Do not modify the
      returned array. *)

  val length : t -> int
  (** [length t] is the number of tokens in [t]. *)

  val truncate : t -> int -> unit
  (** [truncate t length] discards tokens after [length]. [length] must name a
      prefix of [t]. *)

  val append_message : t -> role:string -> string -> unit
  (** [append_message t ~role content] appends a native chat message. [role]
      must be ["system"], ["user"], ["assistant"] or ["tool"]. *)

  val append_multimodal_message :
    t -> role:string -> text_parts:string list -> images:string list -> unit
  (** [append_multimodal_message t ~role ~text_parts ~images] appends a user or
      tool message containing encoded PNG or JPEG images. It requires a loaded
      vision sidecar. [text_parts] contains the text before, between and after
      the images. *)

  val sync : Session.t -> t -> unit
  (** [sync session t] synchronises [session] with [t], including its images. *)

  val append_rendered : t -> string -> unit
  (** [append_rendered t text] appends trusted rendered-chat control [text].
      Never pass user or tool output to this function. *)

  val append_think_prefix : t -> think:think -> ctx_size:int -> unit
  (** [append_think_prefix t ~think ~ctx_size] appends the reasoning-effort
      instruction that opens a conversation in the model's native form, and
      nothing for a mode the model has no instruction for. Call it once, before
      the system prompt. *)

  val append_assistant_prefix : t -> think:think -> ctx_size:int -> unit
  (** [append_assistant_prefix t ~think ~ctx_size] opens an assistant turn,
      applying the model's context-dependent thinking policy. *)

  val append_token : t -> int -> unit
  (** [append_token t token] appends the exact token [token]. *)

  val append_tokens : t -> int array -> unit
  (** [append_tokens t tokens] appends [tokens] exactly, as {!append_token} does
      each of them. *)

  val has_images : t -> bool
  (** [has_images t] is whether [t] holds an image. A slice of such a transcript
      cannot be rebuilt from its tokens alone. *)

  val finish_assistant : t -> unit
  (** [finish_assistant t] closes an assistant turn in the model's native
      dialect. DeepSeek appends EOS. Qwen appends its end token and a newline.
      GLM needs no closing token. *)

  val copy : t -> t
  (** [copy t] is an independent transcript with the same tokens and images as
      [t]. Image embeddings are shared until both copies are unreachable. *)
end
