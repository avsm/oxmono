module Tool = struct
  type t = { name : string; description : string; parameters : Jsont.json }
  let v ~name ~description ~parameters =
    if name = "" then invalid_arg "Agentkit.Tool.v: empty name";
    { name; description; parameters }
  let name t = t.name
  let description t = t.description
  let parameters t = t.parameters
end

(*---------------------------------------------------------------------------
   Copyright (c) 2026 Anil Madhavapeddy. All rights reserved.
   SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

type stats = {
  ctx_used : int;
  ctx_size : int;
  prompt_tokens : int;
  generated : int;
  generate_seconds : float;
  prefill_seconds : float;
  tool_calls : int;
  turns : int;
  drafted : int;
  total_generated : int;
  total_generate_seconds : float;
}

type tool_call = { id : string; name : string; arguments : string }
type cut = { tokens : int; tool_call : bool }
type compaction = { before : int; after : int; summary : string }

type event =
  | Reasoning of string
  | Content of string
  | Tool_call of tool_call
  | Tool_result of string * string
  | Stats of stats
  | Expanded of int
  | Cut_off of cut
  | Squeezed of int
  | Compacted of compaction
  | Done

module type S = sig
  type t

  val send : t -> on_event:(event -> unit) -> string -> unit
  val stats : t -> stats
  val cancel : t -> unit
  val close : t -> unit
end
