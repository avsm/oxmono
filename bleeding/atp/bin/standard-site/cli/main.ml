(*---------------------------------------------------------------------------
   Copyright (c) 2025 Anil Madhavapeddy. All rights reserved.
   SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

open Cmdliner

let version = "0.1.0"
let app_name = "standard-site"

let main_cmd =
  let doc = "Standard Site - Blog and publication management on AT Protocol" in
  let man =
    [
      `S Manpage.s_description;
      `P
        "Standard Site is a decentralized blogging platform built on AT \
         Protocol. Use this CLI to manage publications and documents.";
      `S Manpage.s_commands;
      `P "Use $(mname) $(i,COMMAND) --help for help on a specific command.";
      `S Manpage.s_bugs;
      `P "Report bugs at https://github.com/user/standard-site/issues";
    ]
  in
  let info = Cmd.info "standard-site" ~version ~doc ~man in
  Cmd.group info
    [
      Xrpc_auth.Cmd.auth_cmd ~app_name (); Cmd_publication.cmd; Cmd_document.cmd;
    ]

let () =
  Fmt_tty.setup_std_outputs ();
  exit (Cmd.eval main_cmd)
