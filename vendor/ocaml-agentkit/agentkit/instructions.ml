(*---------------------------------------------------------------------------
   Copyright (c) 2026 Anil Madhavapeddy. All rights reserved.
   SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

(* Capped, because the file joins every turn's prompt and a long one would
   quietly consume the context the conversation needs. *)
let max_size = 8000

let load ws =
  match Eio.Path.load Eio.Path.(ws / "AGENTS.md") with
  | exception Eio.Exn.Io _ -> None
  | text when String.trim text = "" -> None
  | text when String.length text <= max_size -> Some text
  | text ->
      (* The rest of the file is on disk and the agent can read it, so what is
         dropped here is not lost. The cut falls at a line boundary, since half
         a sentence quoted as though it were whole is worse than saying less. *)
      let cut =
        match String.rindex_from_opt text max_size '\n' with
        | Some i when i > 0 -> i
        | _ -> max_size
      in
      Some
        (String.sub text 0 cut
       ^ "\n\n\
          [AGENTS.md is longer than what joins every prompt and stops here. \
          Read AGENTS.md itself, from this line on, for the rest.]")
