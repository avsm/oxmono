(*---------------------------------------------------------------------------
   Copyright (c) 2026 Anil Madhavapeddy. All rights reserved.
   SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

(** The tools an agent working in a workspace is given, and what it is told
    about them.

    A command that puts a model to work on a workspace assembles the same list:
    the capability file tools of {!Ds4.Toolbox}, name resolution, and the dune
    and merlin tools when okitd can serve them. It is here rather than in each
    command so that what one command's transcripts exercise is what the others
    give a model. *)

val default_system : string
(** [default_system] is the system prompt an agent starts from, which states the
    capability discipline the file tools are built on. It is the default of a
    command's [--system], so a person may replace it. *)

val dune_system : string
(** [dune_system] is what the agent is told about the dune tools. {!assemble}
    adds it only when those tools are there, since a model that hears about a
    tool it has not got asks for it and then falls back to the shell. *)

val merlin_system : string
(** [merlin_system] is what the agent is told about the merlin tools, and is
    added after {!dune_system} when okitd found an ocamlmerlin. It also narrows
    what the base prompt said about reading files, which is why it is stated
    rather than left implied. *)

(** Why okit's tools are there or absent. *)
type status =
  | Serving of { hello : Proto.hello; refused : string option }
      (** okitd greeted, and [hello] is what it said. [refused] is why the dune
          tools are absent from a workspace that has a [dune-project], and is
          [None] when they are there or when there was no [dune-project] to
          serve. The two are told apart here rather than by each caller, since
          the answer is in the workspace and this call is the one holding it. *)
  | Unstarted of string  (** okitd would not start, for this reason. *)

type agent = { agent : Ds4.Agent.t; status : status; instructions : bool }
(** A workspace agent and what its frontend needs to describe its setup. *)

val system_prompt : base:string -> okit:string option -> _ Eio.Path.t -> string
(** [system_prompt ~base ~okit root] appends [okit], when present, and the
    workspace's [AGENTS.md] instructions to [base], in that order. *)

val assemble :
  vision:bool ->
  sw:Eio.Switch.t ->
  proc:[> [ `Generic | `Unix ] Eio.Process.mgr_ty ] Eio.Resource.t ->
  net:_ Eio.Net.t ->
  clock:_ Eio.Time.clock ->
  caps:Ds4.Toolbox.Caps.t ->
  trace:(string -> unit) ->
  argv:string list ->
  root:_ Eio.Path.t ->
  Ds4.Tool.t list * string option * status
(** [assemble ~sw ~proc ~net ~clock ~caps ~trace ~argv ~root] is the tools for
    the workspace [root], what they add to the system prompt, and why okit's
    part of them is there.

    [vision] adds [view_image].

    [argv] spawns okitd, and is the caller's own binary under its internal
    subcommand, such as [["humpty"; "okitd"; "--dir"; ws]]. Every command spawns
    itself, so the argv is the caller's to supply.

    okitd is spawned whether or not the workspace has a [dune-project], and its
    greeting decides which tools exist: the dune tools when it started a session
    and the merlin tools when it found an ocamlmerlin beside it. The
    model-facing set has no shell because one would bypass the file capability
    boundary. An okitd that will not start leaves the plain tools and says why
    in the {!status}: a workspace okit cannot serve is a reason to work without
    it rather than a reason to refuse to run.

    Nothing is logged. The {!status} is the whole of what happened to okit, and
    reporting it is the caller's, which knows whether a person is reading a
    stream, an interface or a journal.

    Call this before the engine is created. Spawning okitd forks the calling
    process, and forking one that has a model mapped into it costs minutes on
    macOS.

    [trace] takes the step okit is on, for a caller that shows what a call in
    flight is doing. Stopping the client is registered on [sw], so a switch
    released after a failure takes okitd and the dune server it started with it.
*)

val create_agent :
  vision:_ Eio.Path.t option ->
  sw:Eio.Switch.t ->
  proc:[> [ `Generic | `Unix ] Eio.Process.mgr_ty ] Eio.Resource.t ->
  net:_ Eio.Net.t ->
  clock:_ Eio.Time.clock ->
  domain_mgr:_ Eio.Domain_manager.t ->
  fs:_ Eio.Path.t ->
  cache:_ Eio.Path.t ->
  model:_ Eio.Path.t ->
  root:_ Eio.Path.t ->
  approve:(string -> bool) ->
  trace:(string -> unit) ->
  argv:string list ->
  system:string ->
  thinking:Dsml.thinking_mode ->
  ctx_size:int ->
  max_ctx_size:int ->
  seed:int64 ->
  agent
(** [create_agent] assembles Okit before opening the model, appends the Okit and
    workspace instructions to [system], and creates the agent. This ordering
    prevents okitd from being forked after the model is mapped. *)
