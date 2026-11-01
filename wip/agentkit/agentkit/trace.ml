(*---------------------------------------------------------------------------
   Copyright (c) 2026 Anil Madhavapeddy. All rights reserved.
   SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

type t = {
  limit : int;
  now : unit -> float;
  emit : Journal.kind -> unit;
  reasoning : Buffer.t;
  content : Buffer.t;
  pending : (int * float) Queue.t;
  mutable calls : int;
}

let create ?(tool_result_limit = 4000) ?(now = Unix.gettimeofday) ~emit () =
  {
    limit = tool_result_limit;
    now;
    emit;
    reasoning = Buffer.create 512;
    content = Buffer.create 512;
    pending = Queue.create ();
    calls = 0;
  }

(* A reply arrives a token at a time, and one record per token would be an
   account nobody can read. The text is gathered and emitted as one kind at the
   boundary it is complete at, which is the tool call it precedes or the end of
   the turn, so it is still emitted before anything it explains happens. *)
let flush t =
  if Buffer.length t.reasoning > 0 then begin
    t.emit (Journal.Reasoning (Buffer.contents t.reasoning));
    Buffer.clear t.reasoning
  end;
  if Buffer.length t.content > 0 then begin
    t.emit (Journal.Content (Buffer.contents t.content));
    Buffer.clear t.content
  end

let event t = function
  | Agent.Reasoning r -> Buffer.add_string t.reasoning r
  | Agent.Content c -> Buffer.add_string t.content c
  | Agent.Tool_call tc ->
      flush t;
      t.calls <- t.calls + 1;
      (* A turn asks for its tool calls before any of them is made, so the
         pending ones queue here and each result is paired with the oldest. *)
      Queue.add (t.calls, t.now ()) t.pending;
      t.emit
        (Journal.Tool_call
           {
             Journal.call = t.calls;
             name = tc.Agent.name;
             arguments = tc.Agent.arguments;
           })
  | Agent.Tool_result (name, output) ->
      let call, began =
        match Queue.take_opt t.pending with
        | Some (call, began) -> (call, began)
        | None -> (0, t.now ())
      in
      t.emit
        (Journal.Tool_result
           {
             Journal.call;
             name;
             output;
             seconds = t.now () -. began;
             (* Adapters treat a limit of zero or less as no limit. *)
             truncated = t.limit > 0 && String.length output > t.limit;
           })
  | Agent.Stats s ->
      flush t;
      t.emit (Journal.Stats s)
  | Agent.Expanded n -> t.emit (Journal.Expanded n)
  | Agent.Cut_off c ->
      (* Flushed first, so that the text the turn did produce is on the record
         before what stopped it. *)
      flush t;
      t.emit
        (Journal.Cut_off
           { Journal.tokens = c.Agent.tokens; tool_call = c.Agent.tool_call })
  | Agent.Squeezed n -> t.emit (Journal.Squeezed n)
  | Agent.Compacted c ->
      (* Flushed first, since compaction can land mid-turn, before a tool
         result that would not otherwise fit, with text already buffered. *)
      flush t;
      t.emit (Journal.Compacted c)
  | Agent.Done -> flush t
