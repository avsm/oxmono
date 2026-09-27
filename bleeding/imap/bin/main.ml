let () =
  exit @@ Eio_main.run @@ fun env ->
  Imap_cli.eval ~env:Sys.getenv_opt ~argv:Sys.argv
    ~net:(Eio.Stdenv.net env) ~fs:(Eio.Stdenv.fs env)
    ~random:(Eio.Stdenv.secure_random env) ()
