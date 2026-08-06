(*---------------------------------------------------------------------------
   Copyright (c) 2025 Anil Madhavapeddy. All rights reserved.
   SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

open Cmdliner

let version = "0.1.0"

let () =
  let exit_code =
    try
      Eio_main.run @@ fun env ->
      let fs = env#fs in
      let doc = "PeerTube CLI - A command-line interface for PeerTube servers" in
      let man = [
        `S Manpage.s_description;
        `P "A command-line interface for interacting with PeerTube servers.";
        `P "Use $(b,peertube auth login) to authenticate with your server.";
        `S Manpage.s_commands;
        `S Manpage.s_bugs;
        `P "Report bugs at https://github.com/avsm/ocaml-peertube/issues";
      ] in
      let info = Cmd.info "peertube" ~version ~doc ~man in
      let cmds =
        [ Peertube_auth.Cmd.auth_cmd env fs
        ]
      in
      Cmd.eval' (Cmd.group info cmds)
    with
    | Eio.Cancel.Cancelled Stdlib.Exit ->
        (* Eio wraps Exit in Cancelled when a fiber is cancelled *)
        0
    | Peertube_auth.Error.Exit_code code ->
        (* Exit code from Error.wrap - already printed error message *)
        code
    | Openapi.Runtime.Api_error _ as exn ->
        (* Handle PeerTube API errors with nice formatting *)
        Peertube_auth.Error.handle_exn exn
    | Failure msg ->
        Fmt.epr "Error: %s@." msg;
        1
    | exn ->
        Fmt.epr "Unexpected error: %s@." (Printexc.to_string exn);
        125
  in
  exit exit_code
