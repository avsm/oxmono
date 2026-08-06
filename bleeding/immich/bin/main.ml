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
      let doc = "Immich CLI - A command-line interface for Immich photo server" in
      let man = [
        `S Manpage.s_description;
        `P "A command-line interface for interacting with Immich photo servers.";
        `P "Use $(b,immich auth login) to authenticate with your server.";
        `S Manpage.s_commands;
        `S Manpage.s_bugs;
        `P "Report bugs at https://github.com/avsm/ocaml-immich/issues";
      ] in
      let info = Cmd.info "immich" ~version ~doc ~man in
      let cmds =
        [ Immich_auth.Cmd.auth_cmd env fs
        ; Cmd_server.server_cmd env fs
        ; Cmd_albums.albums_cmd env fs
        ; Cmd_faces.faces_cmd env fs
        ]
      in
      Cmd.eval (Cmd.group info cmds)
    with
    | Eio.Cancel.Cancelled Stdlib.Exit ->
        (* Eio wraps Exit in Cancelled when a fiber is cancelled *)
        0
    | Immich_auth.Error.Exit_code code ->
        (* Exit code from Error.wrap - already printed error message *)
        code
    | Openapi.Runtime.Api_error _ as exn ->
        (* Handle Immich API errors with nice formatting *)
        Immich_auth.Error.handle_exn exn
    | Failure msg ->
        Fmt.epr "Error: %s@." msg;
        1
    | exn ->
        Fmt.epr "Unexpected error: %s@." (Printexc.to_string exn);
        125
  in
  exit exit_code
