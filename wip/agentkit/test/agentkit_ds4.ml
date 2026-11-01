(*---------------------------------------------------------------------------
   Copyright (c) 2026 Anil Madhavapeddy. All rights reserved.
   SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

let fail message = failwith message

let () =
  let driver =
    Agentkit_ds4.driver
      ~models:(fun () ->
        [ { Agentkit.Driver.name = "q4"; description = "test" } ])
      ~create:(fun _ -> fail "model constructor ran while listing")
  in
  (match Agentkit.Driver.models (Agentkit.Driver.merge [ driver ]) with
  | [ { Agentkit.Driver.name = "ds4/q4"; _ } ] -> ()
  | _ -> fail "DS4 driver did not list its qualified model");
  (match Agentkit_ds4.model_path ~dir:"." "local/../outside.gguf" with
  | exception Failure _ -> ()
  | _ -> fail "local model names must not escape the model directory");
  let call = Dsml.tool_call ~name:"read" ~arguments:{|{"path":"a"}|} () in
  (match Agentkit_ds4.event (Ds4.Agent.Tool_call call) with
  | Agentkit.Agent.Tool_call converted
    when converted.name = "read" && converted.arguments = {|{"path":"a"}|} ->
      ()
  | _ -> fail "DS4 tool call conversion lost a field");
  let source : Ds4.Agent.stats =
    {
      ctx_used = 1;
      ctx_size = 2;
      prompt_tokens = 3;
      generated = 4;
      generate_seconds = 5.;
      prefill_seconds = 6.;
      tool_calls = 7;
      turns = 8;
      drafted = 9;
      total_generated = 10;
      total_generate_seconds = 11.;
    }
  in
  let converted = Agentkit_ds4.stats source in
  if
    converted.ctx_used <> 1 || converted.ctx_size <> 2
    || converted.prompt_tokens <> 3
    || converted.generated <> 4
    || converted.generate_seconds <> 5.
    || converted.prefill_seconds <> 6.
    || converted.tool_calls <> 7 || converted.turns <> 8
    || converted.drafted <> 9
    || converted.total_generated <> 10
    || converted.total_generate_seconds <> 11.
  then fail "DS4 statistics conversion lost a field";
  (match
     Agentkit_ds4.event
       (Ds4.Agent.Cut_off { Ds4.Agent.tokens = 12; tool_call = true })
   with
  | Agentkit.Agent.Cut_off { tokens = 12; tool_call = true } -> ()
  | _ -> fail "DS4 cut-off conversion lost a field");
  let compacted : Ds4.Agent.compaction =
    { before = 100; after = 20; summary = "kept" }
  in
  (match Agentkit_ds4.event (Ds4.Agent.Compacted compacted) with
  | Agentkit.Agent.Compacted { before = 100; after = 20; summary = "kept" } ->
      ()
  | _ -> fail "DS4 compaction conversion lost a field");
  (match Agentkit_ds4.event (Ds4.Agent.Tool_result ("read", "body")) with
  | Agentkit.Agent.Tool_result ("read", "body") -> ()
  | _ -> fail "DS4 tool result conversion lost a field");
  let module _ : Agentkit.Agent.S with type t = Ds4.Agent.t = Agentkit_ds4.Agent
  in
  print_endline "Agentkit DS4 adapter test passed."
