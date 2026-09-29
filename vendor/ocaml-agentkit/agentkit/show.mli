(*---------------------------------------------------------------------------
   Copyright (c) 2026 Anil Madhavapeddy. All rights reserved.
   SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

(** Rendering journal records for a person.

    Every command that prints a journal prints it through here, so a record
    reads the same wherever it is met. A line carries the sequence number, the
    time, the run, the kind and a one-line summary. Long text is cut, and the
    whole of a record stays in the journal file, one JSON object per line. *)

val brief : ?limit:int -> string -> string
(** [brief s] is the first line of [s] cut to [limit] characters, which defaults
    to 120, with the byte count appended when anything was cut. *)

val summary : ?agent:string -> Journal.kind -> string
(** [summary k] is the one-line summary of [k]. A record does not say which
    program wrote it, so [agent] names it on the [run_start] line for a reader
    looking at more than one journal. *)

val record_line : ?agent:string -> Journal.record -> string
(** [record_line r] is one line for [r]: its sequence number, its time, its run,
    its kind and {!summary}. Every kind has a line, a kind this build does not
    know included, since a log that dropped a record would read as complete. *)

val check_kinds : string list -> unit
(** [check_kinds ks] raises [Failure] naming the kinds this build writes if any
    of [ks] is not among them, since a misspelled kind would otherwise match
    nothing and read as a journal with nothing in it. *)

val keep : ?since:float -> ?run:int -> Journal.record -> bool
(** [keep r] is whether [r] passes the filters: written at or after [since], and
    belonging to run [run]. *)

val log :
  ?agent:string ->
  ?since:float ->
  ?kinds:string list ->
  ?run:int ->
  _ Eio.Path.t ->
  (string -> unit) ->
  unit
(** [log dir emit] passes one {!record_line} per record of the journal in [dir]
    to [emit], oldest first, filtered as {!keep} and [kinds] say. *)
