(*---------------------------------------------------------------------------
   Copyright (c) 2026 Anil Madhavapeddy. All rights reserved.
   SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

(** Pieces of humpty's command line that stand on their own. *)

(** The script and the transcript of [humpty expect].

    The subcommand reads a script of tool calls and prompts, runs each call
    against the tools [humpty agent] would assemble and each prompt through the
    agent loop, and prints what they returned. The parts that decide what a
    transcript says are here, apart from the subcommand, so that they can be
    tested without a workspace, a process or a model. *)
module Expect : sig
  type call = {
    line : int;  (** the line of the script the call was read from *)
    tool : string;  (** the tool's name *)
    arguments : string;  (** the arguments, as a JSON object *)
  }
  (** One call read from a script. [arguments] is passed to the tool as written,
      exactly as a model's call would be, so a malformed object is answered by
      the tool rather than refused here. *)

  type prompt = {
    line : int;  (** the line of the script the prompt was read from *)
    text : string;  (** the text to send to the model *)
  }
  (** One prompt read from a script, written as [?] followed by its text. *)

  (** One line of a script that does something: a tool call run directly, or a
      prompt sent to the model, which then runs the agent loop. *)
  type item = Call of call | Prompt of prompt

  val parse : string -> (item list, string) result
  (** [parse text] is the items [text] holds, in order. A line is blank, a [#]
      comment, a prompt, or a tool call: the tool's name, one space, and its
      arguments as a JSON object. The error names the line and the forms a line
      may take. *)

  val roots : string list -> string list
  (** [roots dirs] is the workspace paths {!scrub} rewrites, taken from [dirs]
      with the relative ones dropped, trailing slashes removed, duplicates
      merged and the longest first. A workspace is named two ways, as Eio names
      it and as the filesystem resolves it, and tools report both. *)

  val scrub : roots:string list -> string -> string
  (** [scrub ~roots text] is [text] with each of [roots] replaced by [$WS], and
      with a trailing duration such as [0.4s] dropped from each line, so that
      two runs of the same script give the same text. A line of output that
      genuinely ends in a count of seconds loses it too, which is the price of
      one rule. *)

  val one_line : string -> string
  (** [one_line text] is [text] with its line breaks turned into single spaces,
      for a message that has one line to be said in: the transcript's status
      line, the note the interface shows, and an error reported beside the
      command's own diagnostics. An error wrapped over several lines keeps every
      sentence it had.

      A dune server's last output, which okit appends after a line reading
      ["The server's last output was:"], is dropped rather than flattened. It is
      the program's own output rather than part of the sentence, and it names a
      pid. *)
end
