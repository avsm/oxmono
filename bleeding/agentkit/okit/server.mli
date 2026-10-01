(*---------------------------------------------------------------------------
   Copyright (c) 2026 Anil Madhavapeddy. All rights reserved.
   SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

(** okitd: okit's operations run in a process of their own.

    A server owns the dune session, the merlin and every process okit runs for
    one workspace, and answers calls over a pair of pipes in the protocol
    {!Proto} defines. It exists so that the humpty holding a model forks nothing
    once that model is loaded: a fork of an address space with tens of gigabytes
    mapped into it costs minutes on macOS, and it blocks the domain that asked.

    The server reads no file of the workspace. Source text a merlin query needs
    arrives in the call, read by the caller through the capability its own tools
    hold, so the capability discipline stays where it is. *)

val run :
  ?dir:string ->
  stdin:_ Eio.Flow.source ->
  stdout:_ Eio.Flow.sink ->
  proc:[> [ `Generic | `Unix ] Eio.Process.mgr_ty ] Eio.Resource.t ->
  net:_ Eio.Net.t ->
  clock:_ Eio.Time.clock ->
  fs:Eio.Fs.dir_ty Eio.Path.t ->
  unit ->
  unit
(** [run ~stdin ~stdout ~proc ~net ~clock ~fs ()] serves the workspace [dir]
    under [fs], which defaults to the current directory, until it is told to
    stop. It returns when the peer sends a shutdown or closes [stdin], having
    stopped the dune session and the server that session started.

    It starts by attempting a {!Session} session on the workspace, when that
    workspace has a [dune-project], and by looking for an [ocamlmerlin] to
    answer beside it. It then writes one {!Proto.Hello} saying which tool
    families are live and carrying {!Status.note} for the reason, whole rather
    than folded onto one line. Every step either of those takes is written as a
    trace before the greeting, so a peer must expect traces before the hello it
    is waiting for.

    The peer must read okitd's standard output from the moment it spawns it, on
    a fiber of its own. Nothing here waits for a reader: once the pipe is full
    okitd blocks on the write, and a block that lands inside the thirty seconds
    the dune session gives a server to open its socket turns a slow reader into
    a session that is reported as having found no socket.

    Each call is answered by one {!Proto.Result}, preceded by a trace for every
    step the operation takes, carrying the id of the call in flight. Calls are
    run one at a time, in the order they arrive. An operation that needs the
    dune session or the merlin that is not there is answered with a result
    saying so, since a workspace without either is a workspace to work in
    differently and not a fault of the protocol.

    A line that does not parse is a protocol fault, the peer being the same
    binary. The session is stopped and [Failure] is raised carrying the
    offending line, truncated, for the caller to report and exit nonzero with.

    It handles [SIGTERM] while it is serving, and restores the previous handler
    before it returns. A peer that gives up on a server kills it, and a server
    that died where it stood would leave the dune server it started holding the
    workspace's build lock. A dune exchange in flight finishes first, since the
    session holds its mutex against cancellation and a build takes as long as it
    takes, and whatever a call had reached besides is abandoned. The kill that
    follows a peer's term a few seconds later cannot be caught, so a server
    still inside an operation then does leave that dune behind.

    Nothing it runs inherits [stdin], which is the pipe the protocol arrives on.
    Each child is given an empty one, since a command that read a byte of the
    real one would take the peer's next call for its own input.

    [proc] runs dune, ocamlmerlin and the shell, so it carries the whole
    authority of this process, which is the authority the peer has and grants by
    spawning the server at all. *)
