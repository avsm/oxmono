(* SPDX-License-Identifier: ISC *)
open Cmdliner

let serve hostname owner repo source plc state_dir port addr =
  Eio_main.run @@ fun env ->
  Spindle.run ~addr env { hostname; owner; repo; source; plc; state_dir; port;
    job = Spindle.Job.inspect }

let required name doc =
  Arg.(required & opt (some string) None & info [name] ~doc)

let option name default doc =
  Arg.(value & opt string default & info [name] ~doc)

let command =
  let term = Term.(const serve
    $ option "hostname" "spindle.tangled.test" "Spindle did:web hostname."
    $ required "owner" "Authorized owner PLC DID."
    $ required "repo" "Repository DID mapped to the Git source."
    $ required "source" "Git clone URL or absolute local repository path."
    $ required "plc" "PLC HTTP origin used for signature verification."
    $ option "state-dir" "spindle-state" "Persistent state directory."
    $ Arg.(value & opt int 9000 & info ["port"] ~doc:"HTTP listening port.")
    $ option "addr" "127.0.0.1" "HTTP listening IP address.") in
  Cmd.v (Cmd.info "spindle" ~doc:"Run an OCaml Tangled inspection job") term

let () = exit (Cmd.eval command)
