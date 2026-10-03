let () =
  Eio_main.run @@ fun env ->
  exit
    (Cmdliner.Cmd.eval'
       (Ecosystems_cli.main ~out:Format.std_formatter ~err:Format.err_formatter
          env))
