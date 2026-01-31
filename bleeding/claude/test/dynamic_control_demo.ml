(*---------------------------------------------------------------------------
  Copyright (c) 2025 Anil Madhavapeddy <anil@recoil.org>. All rights reserved.
  SPDX-License-Identifier: ISC
 ---------------------------------------------------------------------------*)

open Claude
open Eio.Std

let () = Logs.set_reporter (Logs_fmt.reporter ())
let () = Logs.set_level (Some Logs.Info)

let run env =
  Switch.run @@ fun sw ->
  let process_mgr = Eio.Stdenv.process_mgr env in
  let clock = Eio.Stdenv.clock env in

  (* Create client with default options *)
  let options = Options.default in
  let client = Client.create ~options ~sw ~process_mgr ~clock () in

  traceln "=== Dynamic Control Demo ===\n";

  (* First query with default model *)
  traceln "1. Initial query with default model";
  Client.query client "What model are you?";

  (* Consume initial responses *)
  let responses = Client.receive_all client in
  List.iter
    (function
      | Response.Text text ->
          traceln "Assistant: %s" (Response.Text.content text)
      | _ -> ())
    responses;

  traceln "\n2. Getting server info...";
  (try
     let info = Client.get_server_info client in
     traceln "Server version: %s" (Claude.Server_info.version info);
     traceln "Capabilities: [%s]"
       (String.concat ", " (Claude.Server_info.capabilities info));
     traceln "Commands: [%s]"
       (String.concat ", " (Claude.Server_info.commands info));
     traceln "Output styles: [%s]"
       (String.concat ", " (Claude.Server_info.output_styles info))
   with
  | Failure msg -> traceln "Failed to get server info: %s" msg
  | exn -> traceln "Error getting server info: %s" (Printexc.to_string exn));

  traceln "\n3. Switching to a different model (if available)...";
  (try
     Client.set_model client (Proto.Model.of_string "claude-sonnet-4");
     traceln "Model switched successfully";

     (* Query with new model *)
     Client.query client "Confirm your model again please.";
     let responses = Client.receive_all client in
     List.iter
       (function
         | Response.Text text ->
             traceln "Assistant (new model): %s" (Response.Text.content text)
         | _ -> ())
       responses
   with
  | Failure msg -> traceln "Failed to switch model: %s" msg
  | exn -> traceln "Error switching model: %s" (Printexc.to_string exn));

  traceln "\n4. Changing permission mode...";
  (try
     Client.set_permission_mode client Permissions.Mode.Accept_edits;
     traceln "Permission mode changed to Accept_edits"
   with
  | Failure msg -> traceln "Failed to change permission mode: %s" msg
  | exn -> traceln "Error changing permission mode: %s" (Printexc.to_string exn));

  traceln "\n=== Demo Complete ===";
  ()

let () =
  Eio_main.run @@ fun env ->
  try run env with
  | Transport.CLI_not_found msg ->
      traceln "Error: %s" msg;
      traceln "Make sure the 'claude' CLI is installed and authenticated.";
      exit 1
  | exn ->
      traceln "Unexpected error: %s" (Printexc.to_string exn);
      Printexc.print_backtrace stderr;
      exit 1
