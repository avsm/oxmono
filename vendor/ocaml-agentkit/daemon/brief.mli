(*---------------------------------------------------------------------------
   Copyright (c) 2026 Anil Madhavapeddy. All rights reserved.
   SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

(** What a wake-up starts from.

    A wake-up has no conversation to continue. It builds a fresh context out of
    four things and discards it at the end:

    + the memory at its current version, [fact] and [procedure] entries in full
    + the [open_item] entries, which are what was left unfinished
    + the scheduled task that fired, with its prompt
    + a digest of the journal since the last handover, being what was done and
      what it returned rather than the full text of it

    Scheduled tasks and open items are different things and the brief keeps them
    apart. A scheduled task is an instruction from a person, in a file numpty
    cannot write. An open item is numpty's own note that something is
    unfinished. Confusing the two would let the agent edit its own orders.

    This module is pure apart from {!since_handover}, so what goes into a
    context can be checked without an engine. *)

val system_prompt : string
(** [system_prompt] tells the model that its context does not survive the
    wake-up, that its memory does, and that anything it wants next time has to
    be written down. A model that is not told this writes nothing and loses a
    day's work. *)

val handover_prompt : string
(** [handover_prompt] is the last thing a wake-up sends. It asks what should
    survive, and the agent answers by writing memory through its tools. *)

type t = {
  system : string;  (** the system prompt, which is {!system_prompt} *)
  user : string;  (** the first and only user message *)
  version : int;  (** the memory version it was assembled from *)
  open_items : int;  (** how many [open_item] entries it carries *)
  bytes : int;  (** how large the two together are *)
}
(** An assembled brief. The three numbers are what a {!Agentkit.Journal.Brief}
    record carries, the text itself reaching the journal as the [prompt] record
    that follows. *)

val assemble :
  version:int ->
  entries:Agentkit.Memory.entry list ->
  task:string ->
  prompt:string ->
  session:int ->
  history:Agentkit.Journal.record list ->
  t
(** [assemble ~version ~entries ~task ~prompt ~session ~history] is the brief
    for a wake-up on the task [task], whose instruction is [prompt].

    [entries] is the whole of memory at [version], sorted into its sections
    here. [session] counts from 1 and is above it for a session that succeeded
    another after a handover, which the brief says so that the model knows it is
    carrying on rather than starting. [history] are the journal records since
    the last handover, which {!digest} compresses. *)

val digest : Agentkit.Journal.record list -> string
(** [digest records] is what [records] say happened, one line each, as titles
    and outcomes rather than as the text of them. A tool result runs to
    kilobytes and a reply to paragraphs, and a digest that carried either whole
    would cost the context the work needs. It is bounded, and says how many
    lines it left out. *)

val since_handover : _ Eio.Path.t -> Agentkit.Journal.record list
(** [since_handover dir] are the records of the journal in [dir] appended after
    the last {!Agentkit.Journal.Handover}, in order.

    Only the two newest segments are read. A wake-up ends in a handover, so a
    day and the day before it hold one, and reading a year of journal to find
    what the last hour did would cost more each month. *)
