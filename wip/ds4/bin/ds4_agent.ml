(*---------------------------------------------------------------------------
   Copyright (c) 2026 Anil Madhavapeddy. All rights reserved.
   SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

(* ds4-agent: the engine and its agent loop on a plain terminal.

   The subcommands.
     list      show which models are available
     download  fetch a model onto this machine
     chat      send one prompt and print the reply
     agent     run that model in a tool-using loop over a workspace

   Each is a part of ds4.cli, which a richer front-end can reuse. This command
   links nothing beyond the ds4 package. *)

module Cli = Ds4_cli.Cli
module Coder = Ds4_cli.Coder

let version = "0.1"

open Cmdliner

let () =
  let doc = "Run a local model, alone or as an agent." in
  let man =
    [
      `S Manpage.s_description;
      `P
        "ds4-agent runs DeepSeek V4, DeepSeek V4.1 Flash, GLM 5.3 Flash or \
         Qwen3.8 Flash Next on this machine. $(b,list) shows the models, \
         $(b,download) fetches one, $(b,chat) sends it one prompt, and \
         $(b,agent) gives it tools.";
      `P
        (Printf.sprintf "This build runs the model on the %s backend."
           Cli.backend_name);
    ]
  in
  let info = Cmd.info "ds4-agent" ~version ~doc ~man in
  exit
    (Cmd.eval_result
       (Cmd.group info
          [ Cli.list_cmd; Cli.download_cmd; Cli.chat_cmd; Coder.cmd ]))
