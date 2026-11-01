(*---------------------------------------------------------------------------
   Copyright (c) 2026 Anil Madhavapeddy. All rights reserved.
   SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

(** Output options shared by AT Protocol command-line tools.

    {!term} reads [--color], [--theme] and [-v]/[-q], and sets up the standard
    outputs before the command runs. [-v] traces each HTTP exchange that
    {!Xrpc.Trace.wrap} logs, one line each on standard error, naming the XRPC
    method, its status and its time. [-v -v] adds the request and response
    fields, with secrets redacted.

    [--color=always] keeps colour, and {!rich} output, when standard output
    is a pipe, so that [cmd --color=always | less -R] shows what a terminal
    would. The width is then that of the terminal behind standard error. *)

val term : ?env:string -> unit -> unit Cmdliner.Term.t
(** [term ?env ()] reads the options and sets up the outputs when it is
    evaluated, before the terms to its right. [env] is a prefix for
    environment variables, such as ["TANGLED"]: [TANGLED_THEME] then picks the
    theme and [TANGLED_COLOR] the colour mode. *)

val rich : unit -> bool
(** [rich ()] is [true] iff standard output should get drawn views, such as
    tables and panels, rather than plain lines: when it is a terminal, or when
    [--color=always] was given. *)

val tracing : unit -> bool
(** [tracing ()] is [true] iff [-v] or more was given. A program then shows
    no transient progress, which would take the trace's lines with it. *)

val reporter : unit -> Logs.reporter
(** [reporter ()] prints {!Logs} messages on {!Fmt.stderr} in its theme.
    {!Xrpc.Trace} messages are laid out by their tags. {!term} installs it. *)
