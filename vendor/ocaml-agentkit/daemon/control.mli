(*---------------------------------------------------------------------------
   Copyright (c) 2026 Anil Madhavapeddy. All rights reserved.
   SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

(** The socket a running numpty answers questions on.

    It answers only what the store cannot. The journal, the memory versions and
    the schedule are on disk, durable before the daemon proceeds past them, so a
    client reads those from the files and needs no daemon to do it. What is left
    is live state: which job is running, how far into its context it is, how far
    a prefill has got, which tool call is in flight, when each task fires next,
    and whether numptyd is still alive.

    Nothing here mutates. Work is asked for by writing the schedule file, which
    [numpty task] does without a daemon, so the socket reports and the file
    instructs.

    Authority is the file system's. The socket is created mode 0600 in a store
    directory the owner already controls, and there is no authentication,
    because anyone who can open it can already read the journal that says
    everything the socket would.

    {1 Shape}

    The surface is the OCaml signature {!S} over plain records, and the line
    protocol is one adapter over it. capnp-rpc becomes a second adapter against
    the same signature, and the daemon holds one implementation for both. Every
    response is a record with a jsont codec, apart from [follow], which is a
    stream here and would be a callback interface there. *)

(** What a client asks for. *)
type request =
  | Status  (** the daemon itself *)
  | Jobs  (** the running job, and when each task fires next *)
  | Memory of { at : int option }  (** memory, at a version or in force *)
  | Log of { since : float option; kinds : string list; limit : int }
      (** the tail of the journal *)
  | Follow of { kinds : string list }
      (** journal records as they are appended, filtered by kind *)

(** What it is answered with. *)
type answer =
  | Running of Status.running
  | Jobs_are of Status.jobs
  | Memory_is of Status.memory
  | Lines of Status.lines
  | Refused of string  (** the request could not be served, and why *)

(** The control surface. *)
module type S = sig
  val status : unit -> Status.running
  val jobs : unit -> Status.jobs
  val memory : at:int option -> Status.memory
  val log : since:float option -> kinds:string list -> limit:int -> Status.lines

  val follow : kinds:string list -> emit:(string -> unit) -> unit
  (** [follow ~kinds ~emit] passes each journal record appended from now on to
      [emit], as {!Show.record_line} writes it. It does not return. *)
end

val make :
  snapshot:(unit -> Status.t) ->
  clock:_ Eio.Time.clock ->
  root:_ Eio.Path.t ->
  (module S)
(** [make ~snapshot ~clock ~root] is the surface over the store at [root].

    [snapshot] is read for every question about the run, and must not enter the
    engine. It is what the agent loop published at the last turn boundary, with
    the live prefill counter filled in. Everything else is read from the files
    under [root]. *)

val answer : (module S) -> request -> answer
(** [answer impl request] serves [request], which must not be a {!Follow}. *)

(** {1 The line adapter}

    One compact JSON object per line, request and response, over
    {!Agentkit.Line}. A line that does not parse is a terminal fault for that
    connection, and an invalid UTF-8 sequence becomes [U+FFFD] rather than
    stopping the exchange over somebody's bytes. *)

val serve :
  sw:Eio.Switch.t ->
  net:_ Eio.Net.t ->
  root:_ Eio.Path.t ->
  (module S) ->
  [ `Serving of string | `Unbound of string ]
(** [serve ~sw ~net ~root impl] binds the socket at [control] under [root] and
    answers on it until [sw] finishes. It is [`Serving path], or [`Unbound why]
    where the path is too long for a unix address, which is about a hundred
    characters. An agent that cannot be queried is still an agent, so the caller
    reports that at warning level once and runs without a socket.

    A socket file left by a crashed run belongs to nobody, the store's lock
    being taken before this is called, and is unlinked and replaced. A live run
    holds the lock, so a second run is refused before it reaches the socket at
    all.

    A client that disconnects mid-response is not an event. Its fiber ends and
    the daemon does not notice, which is the point of answering from a snapshot.
*)

val ask :
  sw:Eio.Switch.t ->
  net:_ Eio.Net.t ->
  root:_ Eio.Path.t ->
  request ->
  (answer, string) result
(** [ask ~sw ~net ~root request] asks the numpty running on the store at [root]
    and is its answer. The error is that there was nothing to ask, or that what
    answered did not speak this protocol, and it says which. *)

val stream :
  sw:Eio.Switch.t ->
  net:_ Eio.Net.t ->
  root:_ Eio.Path.t ->
  kinds:string list ->
  (string -> unit) ->
  (unit, string) result
(** [stream ~sw ~net ~root ~kinds emit] passes each journal line the daemon
    appends to [emit] until the connection ends. *)
