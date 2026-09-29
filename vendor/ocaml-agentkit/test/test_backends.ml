module Dummy = struct
  type t = unit
  let send () ~on_event:_ _ = ()
  let stats _ =
    { Agentkit.Agent.ctx_used = 0; ctx_size = 0; prompt_tokens = 0;
      generated = 0; generate_seconds = 0.; prefill_seconds = 0.; tool_calls = 0;
      turns = 0; drafted = 0; total_generated = 0; total_generate_seconds = 0. }
  let cancel _ = ()
  let close _ = ()
end

let dummy name =
  Agentkit.Driver.v ~name ~models:(fun () -> [])
    ~create:(fun _ -> Agentkit.Driver.session (module Dummy) ())

let () =
  let registry =
    Agentkit_backends.registry ~ds4:(dummy "ds4") ~apple:(dummy "apple")
      ~openrouter:(dummy "openrouter")
  in
  let names = Agentkit.Driver.models registry |> List.map (fun m -> m.Agentkit.Driver.name) in
  assert (List.mem "ds4/" names = false);
  match Agentkit.Driver.select registry "openrouter/test" with
  | Ok selection -> assert (Agentkit.Driver.driver_name selection = "openrouter")
  | Error message -> failwith message
