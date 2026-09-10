(*---------------------------------------------------------------------------
   Copyright (c) 2025 Anil Madhavapeddy. All rights reserved.
   SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

open Cmdliner

let version = "0.2.0"
let app_name = "tangled"

let main_cmd =
  let doc = "Tangled - Decentralized git collaboration on AT Protocol" in
  let man =
    [
      `S Manpage.s_description;
      `P
        "Tangled is a decentralized git collaboration platform built on AT \
         Protocol. Use this CLI to manage repositories hosted on Tangled knot \
         servers.";
      `S Manpage.s_commands;
      `P "Use $(mname) $(i,COMMAND) --help for help on a specific command.";
      `S Manpage.s_environment;
      `P (Xdge.Cmd.env_docs app_name);
    ]
  in
  let info = Cmd.info "tangled" ~version ~doc ~man in
  Cmd.group info
    [
      Cmd_auth.cmd;
      Cmd_repo.cmd;
      Cmd_knot.cmd;
      Cmd_profile.cmd;
      Cmd_star.cmd;
      Cmd_pipeline.cmd;
      Cmd_spindle.cmd;
      Cmd_record.cmd;
      Cmd_api.cmd;
      Cmd_issue.cmd;
      Cmd_issue.pull_cmd;
      Cmd_key.cmd;
    ]

let () =
  Fmt_tty.setup_std_outputs ();
  exit (Cmd.eval main_cmd)
