let () =
  (* Schema formatting forces the recursive JSON codec without any framework. *)
  assert (
    Format.asprintf "%a" Apple_fm.Schema.pp Apple_fm.Schema.string
    = {|{"type":"string"}|});
  assert (Ds4.V4.backend = `Metal);
  (match Apple_fm.Availability.get () with
  | `Unavailable _ -> ()
  | _ -> failwith "Foundation Models should be unavailable");
  Eio_main.run (fun env ->
      let cwd = Eio.Stdenv.cwd env in
      (* Repeated failures must not consume the single-engine allowance. *)
      for _ = 1 to 2 do
        match
          Ds4.V4.create ~cache:cwd ~model:Eio.Path.(cwd / "missing.gguf") ()
        with
        | _ -> failwith "Metal engine creation unexpectedly succeeded"
        | exception Failure message ->
            assert (
              String.starts_with ~prefix:"DS4 Metal backend requires macOS"
                message)
      done;
      Eio.Switch.run (fun sw ->
          match Agentkit_apple_fm.Agent.create ~sw [] with
          | _ -> failwith "Apple agent creation unexpectedly succeeded"
          | exception Eio.Io (Apple_fm.Error.E (`Unsupported_version message), _)
            ->
              assert (message = "Apple Foundation Models requires macOS")))
