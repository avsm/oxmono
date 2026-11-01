(*---------------------------------------------------------------------------
   Copyright (c) 2026 Anil Madhavapeddy. All rights reserved.
   SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

(** What every command over the engine repeats.

    A command front-end is the same few decisions each time: which backend this
    build linked, how a failure becomes an exit status, how to name itself for a
    spawn, how a seed and a model are settled, and how logging is set up. The
    subcommands that manage and query a model are the same in every command too.
    They are here so that the commands agree on them rather than each keeping
    its own copy.

    Only the arguments whose wording is the same in every command are here. One
    whose documentation differs, such as a seed that defaults to a fixed value
    for a repeatable transcript, belongs to the command that has that meaning
    for it. *)

val backend_name : string
(** [backend_name] is the backend this build linked, being ["Metal"], ["CUDA"]
    or ["CPU"]. It is what a manual page and a [run_start] record name. *)

val guard : (unit -> 'a) -> ('a, string) result
(** [guard f] runs [f] and turns a failure into the message a cmdliner term
    reports and exits nonzero on. A [Failure] gives its message, and anything
    else gives its printed exception. *)

val run : (Eio_unix.Stdenv.base -> Xdge.t -> 'a) -> ('a, string) result
(** [run f] is [f env xdg] under {!guard} and an Eio main loop, where [xdg] is
    the [ds4] XDG layout that models and caches are kept under. *)

val self : unit -> string
(** [self ()] is the path to run to spawn this command again, which is how a
    command starts a child under an internal subcommand of its own.

    It is [Sys.executable_name], resolved against the PATH at startup, falling
    back to [Sys.argv.(0)] when that names nothing, since a binary unlinked or
    renamed since startup leaves it naming nothing. *)

val resolve_seed : int -> int64
(** [resolve_seed n] is the sampler seed for a [--seed n]. The sampler's state
    must not be zero, so 0 becomes a seed taken from the clock and the process
    id. The value is logged at info level, so a random run can be repeated by
    passing it back. *)

val resolve_model : dir:string -> string option -> string
(** [resolve_model ~dir override] is the path of the model to load, and fails
    with what {!Model.resolve} said when there is none. [dir] is
    [Model.dir xdg], and [override] is the [--model] argument.

    It also fails when the path names no regular file, or one that cannot be
    looked at. The engine reports a model it cannot open and then exits the
    process, so this is the last point at which the failure can be a message.

    It raises rather than returning a result, because every caller is inside
    {!guard} and a model that is not there ends the command. *)

(** {1 Arguments} *)

val logs : ?threaded:bool -> unit -> unit Cmdliner.Term.t
(** [logs ~threaded ()] is the [--verbosity] and [--color] arguments, whose term
    sets up the reporter and forwards the engine's own diagnostics to {!Logs}.
    Evaluating it is what installs them, so a command combines it with
    {!with_logs} rather than reading its value.

    [threaded] enables the mutex that makes the reporter safe to call from more
    than one domain, and defaults to false. Set it in a command that runs the
    engine on a domain of its own, since the engine logs from there. *)

val with_logs : ?threaded:bool -> 'a Cmdliner.Term.t -> 'a Cmdliner.Term.t
(** [with_logs t] is [t] with {!logs} evaluated first, so that a command's own
    arguments are read with logging already set up. *)

val seed : int Cmdliner.Term.t
(** [seed] is the [--seed] argument, defaulting to 0, which {!resolve_seed}
    reads as a fresh seed each run. *)

val mtp : bool Cmdliner.Term.t
(** [mtp] is whether to arm a model's draft head for speculative decoding, true
    unless [--no-mtp] is given. See {!Ds4.V4.create}. *)

val think : Dsml.thinking_mode Cmdliner.Term.t
(** [think] is the [--think] argument of an agent, choosing between a direct
    reply and a reasoned one. *)

(** {1 Subcommands} *)

val list_cmd : (unit, string) result Cmdliner.Cmd.t
(** [list_cmd] is the [list] subcommand, which prints every download target and
    marks those present, followed by any other GGUF files in the model
    directory. *)

val download_cmd : (unit, string) result Cmdliner.Cmd.t
(** [download_cmd] is the [download] subcommand, over {!Model.download}. *)

val chat_cmd : (unit, string) result Cmdliner.Cmd.t
(** [chat_cmd] is the [chat] subcommand, which sends one prompt, prints the
    reply as it is generated, and exits. *)
