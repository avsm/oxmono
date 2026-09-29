(*---------------------------------------------------------------------------
   Copyright (c) 2026 Anil Madhavapeddy. All rights reserved.
   SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

(** A coding agent over a workspace, on a plain terminal.

    This is the whole of [ds4-agent agent], taken apart so that another command
    can reuse the parts it wants: the system prompt, the tools, the printer that
    shows an exchange, the loop that reads prompts, and the subcommand that puts
    them together.

    {[
    Eio.Switch.run @@ fun sw ->
    Eio.Path.with_subtree Eio.Path.(fs / ".") @@ fun ws ->
    let caps = Ds4.Toolbox.Caps.create ~sw ~fs ws in
    let agent =
      Ds4.Agent.create engine
        ~system:(Coder.system_prompt ~now:(Unix.time ()) ws)
        ~tools:(Coder.tools caps)
    in
    Coder.repl ~stdin ~stdout agent
    ]} *)

(** {1 Parts} *)

val default_system : string
(** [default_system] is the instruction an agent starts from. It states the
    capability discipline the file tools are built on and how to work with a
    model that runs at the speed of local inference. It is the default of
    [--system]. *)

val system_prompt : ?base:string -> now:float -> _ Eio.Path.t -> string
(** [system_prompt ~base ~now ws] is [base], which defaults to
    {!default_system}, followed by the local date and time [now] and by the
    workspace's [AGENTS.md] when [ws] has one. An [AGENTS.md] longer than 8000
    bytes is cut at a line boundary, with a note saying where the rest is, since
    it joins every prompt. *)

val tools : ?vision:bool -> Ds4.Toolbox.Caps.t -> Ds4.Tool.t list
(** [tools caps] are the capability file tools: [open_dir], [caps], [tree],
    [list], [read], [read_lines], [find], [grep], [stat], [write], [append] and
    [edit]. There is no shell.

    [vision], which defaults to false, adds [view_image], confined to the same
    capability as the rest. Pass it only when the engine was opened with a
    matching vision sidecar. *)

val printer : stdout:_ Eio.Flow.sink -> Ds4.Agent.event -> unit
(** [printer ~stdout] is an [on_event] callback for {!Ds4.Agent.send}. The reply
    goes to [stdout]. Each tool call, the first line of its result, and any
    change to the context go to standard error, so that a reply can be captured
    on its own. Statistics are logged at info level.

    Each call returns a fresh printer, which tracks whether the reply's last
    line is open. Use one per agent. *)

val repl :
  stdin:_ Eio.Flow.source ->
  stdout:_ Eio.Flow.sink ->
  ?interactive:bool ->
  Ds4.Agent.t ->
  unit
(** [repl ~stdin ~stdout agent] sends each line of [stdin] to [agent] as a
    prompt, in one conversation, until the end of input. Blank lines are
    skipped.

    [interactive], which defaults to whether standard input is a terminal, shows
    a prompt on standard error before each line. A prompt the context cannot
    hold, or one interrupted by {!Ds4.Agent.cancel}, is reported on standard
    error and the loop goes on, since the agent keeps the conversation as it was
    before that prompt. *)

(** {1 The subcommand} *)

val run :
  model:string option ->
  workspace:string ->
  system:string ->
  thinking:Dsml.thinking_mode ->
  seed:int ->
  ctx_size:int ->
  max_ctx_size:int ->
  max_tokens:int ->
  temperature:float option ->
  mtp:bool ->
  vision:string option ->
  string option ->
  (unit, string) result
(** [run ... prompt] resolves the model, confines the tools to [workspace],
    loads the engine and runs [prompt], or {!repl} over standard input when
    [prompt] is [None]. Ctrl-C interrupts the prompt in flight, and a second one
    while none is running leaves. A directory outside [workspace] is granted
    when the model asks for it, and the grant is reported on standard error,
    since nobody can be asked mid-exchange.

    [vision], a path relative to the current directory rather than to
    [workspace], loads the sidecar matching the model and adds [view_image] to
    the tools. *)

val cmd : (unit, string) result Cmdliner.Cmd.t
(** [cmd] is the [agent] subcommand over {!run}. *)
