(*---------------------------------------------------------------------------
   Copyright (c) 2026 Anil Madhavapeddy. All rights reserved.
   SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

(** A session with the dune server of one workspace.

    A session speaks one request at a time and waits for its answer, so a call
    made while another is in flight waits its turn. *)

type t
(** A session. It holds a connection to one server and, when it started that
    server, the process as well. *)

val start :
  ?trace:(string -> unit) ->
  ?client:string ->
  sw:Eio.Switch.t ->
  proc:[> [ `Generic | `Unix ] Eio.Process.mgr_ty ] Eio.Resource.t ->
  net:_ Eio.Net.t ->
  clock:_ Eio.Time.clock ->
  root:Eio.Fs.dir_ty Eio.Path.t ->
  unit ->
  (t, string) result
(** [start ~sw ~proc ~net ~clock ~root ()] serves the dune workspace at [root].
    When no server is running it spawns [dune build --passive-watch-mode] there
    and owns it until [sw] finishes. When a server already holds the workspace
    it connects to that one and leaves it running on {!stop}. The error says
    what failed: no dune on PATH, no socket within 30 seconds, or a server
    without the methods a session needs, which dune 3.24 is the first to have.

    [client] is the id the handshake gives the server, which the server prints
    when it names its peer. It defaults to ["okit"].

    [trace] is called with one short line at each step the session takes, here
    and on every later call, so that a caller can show what a request is waiting
    on. A step is named before it begins rather than once it has returned. It
    defaults to {!ignore}, and an exception it raises is swallowed. *)

val trace : t -> string -> unit
(** [trace t s] passes [s] to the callback {!start} was given, guarded as the
    session's own steps are. A tool built on a session names itself through
    this, so that what a caller shows is one sequence rather than two. *)

type build = {
  ok : bool;  (** Whether the build reached its targets. *)
  diagnostics : Dune_rpc.Private.Diagnostic.t list;
      (** Every error and warning the server holds, which is the whole workspace
          and not only the requested targets. *)
}
(** The outcome of a build and what the server has to say about the workspace
    once it is over. *)

val build : t -> targets:string list -> (build, string) result
(** [build t ~targets] flushes the file watcher, asks the server to build
    [targets] and returns the outcome with the diagnostics the server then
    holds.

    A target is a path relative to the workspace root, ["."] for everything,
    ["lib"] for a directory, ["src/foo.exe"] for one file, or an alias in dune's
    command-line spelling. ["@check"] is the alias in the root directory and
    every one below it, as [dune build @check] is, and ["@@check"] is the alias
    in that directory alone. A directory goes in the name, as in
    ["@lib/runtest"]. Paths and aliases mix in one call.

    A raw dep-spec such as ["(alias check)"] is refused with the form to write,
    as is an alias whose name is empty or holds whitespace, a parenthesis or a
    double quote. *)

val runtest : t -> (build, string) result
(** [runtest t] flushes the file watcher, runs the tests of the whole workspace
    and returns the outcome with the diagnostics the server then holds. A test
    whose output differs from what is recorded reports a diagnostic carrying a
    promotion, which {!promote} accepts. *)

val promote : t -> path:string -> (unit, string) result
(** [promote t ~path] accepts the built file for [path], which is the
    [in_source] path of a diagnostic's promotion and not the file dune wrote. A
    relative [path] is resolved against the workspace root. *)

val stop : t -> unit
(** [stop t] ends the session. A server this session started is asked to shut
    down, and one it merely attached to is left running for its owner. A call in
    flight on another fiber finishes first, and one made afterwards is refused
    with an error saying the session is stopped rather than raising. Calling it
    twice does nothing the second time.

    Call it. A session left to the switch is not shut down: the switch ends the
    connection before it runs the release, so the notification never reaches the
    server, and a server this session spawned is killed with the switch that
    owns its process. *)
