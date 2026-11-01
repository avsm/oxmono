let () =
  Eio_main.run @@ fun env ->
  let code =
    Cmdliner.Cmd.eval'
      (Ecosystems_cli.main ~out:Format.std_formatter ~err:Format.err_formatter
         env)
  in
  (try Format.pp_print_flush Format.std_formatter () with Sys_error _ -> ());
  exit code
