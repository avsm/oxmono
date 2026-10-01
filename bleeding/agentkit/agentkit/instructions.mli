(*---------------------------------------------------------------------------
   Copyright (c) 2026 Anil Madhavapeddy. All rights reserved.
   SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

(** What a workspace leaves for an agent.

    A project states its conventions once, in [AGENTS.md] at its root, rather
    than in every request. A command appends what this returns to the system
    prompt it gives the agent. *)

val load : _ Eio.Path.t -> string option
(** [load ws] is the text of [AGENTS.md] under [ws], or [None] when the file is
    absent or holds only whitespace.

    It is capped at 8000 characters, because the file joins every turn's prompt
    and a long one would quietly consume the context the conversation needs. A
    file that was cut is cut at a line boundary and says so in the text that is
    returned, naming itself as where the rest is, so the model is neither left
    reading a sentence that stops in the middle nor unaware that there is more
    to read. *)
