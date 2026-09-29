(*---------------------------------------------------------------------------
   Copyright (c) 2026 Anil Madhavapeddy. All rights reserved.
   SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

(** Reading the store back, which is what [numpty log] and [numpty memory] do.

    Both open the store, decode with the codecs that wrote it, and print. They
    work while numpty runs, while it is stopped, and on a store copied off the
    machine, since everything they show was durable before the daemon proceeded
    past it. Neither takes the lock. *)

val record_line : Agentkit.Journal.record -> string
(** [record_line r] is one line for [r]: its sequence number, its time, its run,
    its kind and a summary of what it says. Every kind has a line, a kind this
    build does not know included, since a log that dropped a record would read
    as complete. *)

val log :
  ?since:float ->
  ?kinds:string list ->
  ?task:string ->
  ?run:int ->
  _ Eio.Path.t ->
  (string -> unit) ->
  unit
(** [log dir emit] passes one line per record of the journal in [dir] to [emit],
    oldest first.

    [since] drops records written before it. [kinds] keeps only those kinds.
    [run] keeps one run. [task] keeps the records of the wake-ups for one task,
    which are those from its [wake] record up to the next one, since a record
    does not name a task and the wake before it is what says which one it
    belongs to. *)

val show : Agentkit.Memory.t -> at:int option -> string
(** [show m ~at] is the whole of memory at the version [at], or at the version
    in force. Later versions do not change what an earlier one says. *)

val history : Agentkit.Memory.t -> string
(** [history m] is one line per version, oldest first, with the version it
    succeeded, when it was written, the journal record that caused it and why.
*)

val diff : Agentkit.Memory.t -> int -> int -> string
(** [diff m v w] is what changed between versions [v] and [w]: the entries
    added, the entries removed and the entries whose text, kind or tags moved.
*)
