(*---------------------------------------------------------------------------
   Copyright (c) 2026 Anil Madhavapeddy. All rights reserved.
   SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

(** A tool-free request for a bounded summary.

    Compaction asks a model to rewrite old context as a short summary. Two
    things defeat it in practice. A reasoning model asked for a byte count
    spends its whole budget counting characters, and a model often wraps the
    requested JSON in a code fence. This module asks for words, disables
    reasoning, accepts a fenced or prefixed object, and enforces the byte limit
    itself. *)

type failure =
  | Json  (** the reply held no [{"summary": ...}] object *)
  | Empty  (** the summary was blank *)
  | Oversized  (** the summary exceeded the byte limit *)
  | Length  (** the token ceiling cut the reply off *)
  | Tools  (** the model asked for a tool *)

exception Failed of failure

val failure_name : failure -> string
(** [failure_name f] describes [f] for a log line. *)

val run :
  complete:Chat.complete ->
  instructions:(words:int -> string) ->
  limit:int ->
  ?max_tokens:int ->
  ?reasoning:string option ->
  ?on_retry:(failure -> words:int -> unit) ->
  string ->
  string
(** [run ~complete ~instructions ~limit input] asks for a summary of [input]
    of at most [limit] UTF-8 bytes. [instructions ~words] is the system prompt,
    which must ask for a JSON object with one string field, [summary], and
    about [words] words. The first attempt asks for [limit / 10] words, at
    least 64. A {!Failed} attempt other than {!Tools} is retried once with half
    as many words, and [on_retry] is told why.

    [max_tokens] defaults to 4096. [reasoning] defaults to [Some "none"].
    Pass [None] to leave the backend default. Raises {!Failed} when no attempt
    succeeds, and passes on transport failures. *)

val json_object : string -> string
(** [json_object text] is the span of [text] from its first ['{'] to its last
    ['}'], or [text] when there is no such span. *)
