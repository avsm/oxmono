let () =
  if Array.exists (fun s -> s="--help" || s="-h") Sys.argv then
    print_string Imap_cli.usage
  else match Imap_cli.parse ~getenv:Sys.getenv_opt Sys.argv with
  | Error message ->
      Printf.eprintf "%s\n%s%!" message Imap_cli.usage;
      exit 5
  | Ok config ->
      let code=Eio_main.run @@ fun env ->
        Imap_cli.run config ~net:(Eio.Stdenv.net env)
          ~fs:(Eio.Stdenv.fs env)
          ~random:(Eio.Stdenv.secure_random env)
          ~getenv:Sys.getenv_opt in
      exit code
