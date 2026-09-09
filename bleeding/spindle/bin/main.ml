(* SPDX-License-Identifier: ISC *)
open Cmdliner

let serve hostname owner repo source plc state_dir port addr jetstream
    allow_http =
  let repo =
    match (repo, source) with
    | None, None -> None
    | Some repo, Some source -> Some (repo, source)
    | _ -> invalid_arg "--repo and --source must be supplied together"
  in
  Eio_main.run @@ fun env ->
  Spindle.run ~addr env
    {
      hostname;
      owner;
      repo;
      plc;
      state_dir;
      port;
      jobs = [ Spindle.Job.inspect ];
      jetstream;
      allow_http;
    }

let required name doc =
  Arg.(required & opt (some string) None & info [ name ] ~doc)

let option name default doc =
  Arg.(value & opt string default & info [ name ] ~doc)

let command =
  let term =
    Term.(
      const serve
      $ option "hostname" "spindle.tangled.test" "Spindle did:web hostname."
      $ required "owner" "Spindle owner DID."
      $ Arg.(
          value
          & opt (some string) None
          & info [ "repo" ] ~doc:"Optional static repository DID.")
      $ Arg.(
          value
          & opt (some string) None
          & info [ "source" ]
              ~doc:"Static Git clone URL or absolute repository path.")
      $ required "plc" "PLC HTTP(S) origin used for signature verification."
      $ option "state-dir" "spindle-state" "Persistent state directory."
      $ Arg.(value & opt int 9000 & info [ "port" ] ~doc:"HTTP listening port.")
      $ option "addr" "127.0.0.1" "HTTP listening IP address."
      $ Arg.(
          value
          & opt (some string) None
          & info [ "jetstream" ]
              ~doc:
                "Jetstream WSS URL for automatic Tangled discovery and events.")
      $ Arg.(
          value & flag
          & info [ "allow-http" ]
              ~doc:"Permit HTTP and WS on a development network."))
  in
  Cmd.v (Cmd.info "spindle" ~doc:"Run an OCaml Tangled inspection job") term

let () = exit (Cmd.eval command)
