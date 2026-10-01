(*---------------------------------------------------------------------------
   Copyright (c) 2026 Anil Madhavapeddy. All rights reserved.
   SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

(** numptyd: the process numpty reaches the network from.

    A server answers calls over a pair of pipes in the protocol {!Proto}
    defines, and holds every program numpty runs. It exists so that a numpty
    which has loaded a model forks nothing: a fork of an address space with tens
    of gigabytes mapped into it costs minutes on macOS, and it blocks the domain
    that asked.

    It writes no file of numpty's. A fetched body goes back over the pipe, and
    numpty saves it, if it saves it at all, through a capability its own tools
    hold. *)

val default_max_bytes : int
(** [default_max_bytes] is the bound on a fetched body when a call names none,
    being one mebibyte. *)

val max_bytes_ceiling : int
(** [max_bytes_ceiling] is the largest bound a call may ask for, being an eighth
    of {!Agentkit.Line.max_line}. A body is written into one line of JSON, and
    escaping grows it, so the ceiling leaves room for that growth. A call asking
    for more is served this instead, since the alternative is a line the peer
    reads as a protocol fault. *)

val run :
  stdin:_ Eio.Flow.source ->
  stdout:_ Eio.Flow.sink ->
  proc:[> [ `Generic | `Unix ] Eio.Process.mgr_ty ] Eio.Resource.t ->
  unit ->
  unit

(** [run ~stdin ~stdout ~proc ()] serves calls until it is told to stop. It
    returns when the peer sends a shutdown or closes [stdin].

    It starts by asking a curl on the PATH for its version, and writes one
    {!Proto.Hello} saying whether it found one. A numptyd without curl still
    serves: {!Proto.Run} needs none, and a fetch is answered with a refusal in
    words rather than a fault, since a machine without curl is one to work on
    differently.

    Each call is answered by one {!Proto.Result}, preceded by a trace naming
    what it is doing. Calls are run one at a time, in the order they arrive. The
    text of a result is {!Report}'s, refusals included.

    A line that does not parse is a protocol fault, the peer being the same
    binary. [Failure] is raised carrying the offending line, truncated, for the
    caller to report and exit nonzero with.

    Nothing it runs inherits [stdin], which is the pipe the protocol arrives on.
    Each child is given an empty one, since a curl that would prompt for a
    password would otherwise take the peer's next call for its own input.

    There is no bound here on how long a call takes. curl is given a
    [--max-time] of its own, and everything else is bounded by the peer, which
    kills numptyd for exceeding it. A numptyd killed part way through leaves the
    program it was running to finish on its own, which for a curl is what
    [--max-time] already limits.

    [proc] runs curl and whatever a {!Proto.Run} names, so it carries the whole
    authority of this process, which is the authority the peer grants by
    spawning the server at all. Egress is not restricted, and the journal the
    peer writes before each call is the account of what was reached. *)
