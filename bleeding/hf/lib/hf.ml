let run ~process_mgr ?env ?token ?(retry_failed = false) ?stdout ?stderr args =
  let env =
    Option.value env ~default:(Eio.Process.environment process_mgr)
    |> Eio.Process.Env.override
         (match token with
         | None -> []
         | Some token -> [ ("HF_TOKEN", Some token) ])
  in
  let command =
    try
      ignore (Eioext.Process.executable ~process_mgr ~env "hf");
      [ "hf" ]
    with Eio.Io (Eio.Process.E (Eio.Process.Executable_not_found _), _) ->
      [ "uvx"; "hf" ]
  in
  try Eioext.Process.run ~process_mgr ~env ?stdout ?stderr (command @ args)
  with
  | Eio.Io (Eio.Process.E (Eio.Process.Child_error (`Exited _)), _)
  when retry_failed && command = [ "hf" ]
  ->
    Eio.Fiber.check ();
    Eioext.Process.run ~process_mgr ~env ?stdout ?stderr ([ "uvx"; "hf" ] @ args)
