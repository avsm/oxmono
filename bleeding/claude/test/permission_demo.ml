(*---------------------------------------------------------------------------
  Copyright (c) 2025 Anil Madhavapeddy <anil@recoil.org>. All rights reserved.
  SPDX-License-Identifier: ISC
 ---------------------------------------------------------------------------*)

open Eio.Std

let src =
  Logs.Src.create "permission_demo" ~doc:"Permission callback demonstration"

module Log = (val Logs.src_log src : Logs.LOG)

(* Mutable state to track what permissions have been granted *)
module Granted = struct
  module StringSet = Set.Make (String)

  let tools = ref StringSet.empty

  let grant tool_name =
    tools := StringSet.add tool_name !tools;
    Log.app (fun m -> m "✅ Permission granted for: %s" tool_name)

  let deny tool_name =
    Log.app (fun m -> m "❌ Permission denied for: %s" tool_name)

  let is_granted tool_name = StringSet.mem tool_name !tools

  let list () =
    if StringSet.is_empty !tools then
      Log.app (fun m -> m "No permissions granted yet")
    else
      Log.app (fun m ->
          m "Currently granted permissions: %s"
            (StringSet.elements !tools |> String.concat ", "))
end

(* Interactive permission callback *)
let interactive_permission_callback ctx =
  let open Claude.Permissions in
  let tool_name = ctx.tool_name in
  let input = ctx.input in

  Log.info (fun m -> m "🔔 Permission callback invoked for tool: %s" tool_name);
  Log.app (fun m -> m "\n🔐 PERMISSION REQUEST 🔐");
  Log.app (fun m -> m "Tool: %s" tool_name);

  (* Log the full input for debugging *)
  let input_json = Claude.Tool_input.to_json input in
  Log.info (fun m ->
      m "Full input JSON: %s" (Test_json_utils.to_string input_json));

  (* Show input details *)
  (* Try to extract key information from the input *)
  (try
     match tool_name with
     | "Read" -> (
         match Test_json_utils.get_string input_json "file_path" with
         | Some file_path -> Log.app (fun m -> m "File: %s" file_path)
         | None -> ())
     | "Bash" -> (
         match Test_json_utils.get_string input_json "command" with
         | Some command -> Log.app (fun m -> m "Command: %s" command)
         | None -> ())
     | "Write" | "Edit" -> (
         match Test_json_utils.get_string input_json "file_path" with
         | Some file_path -> Log.app (fun m -> m "File: %s" file_path)
         | None -> ())
     | "Glob" -> (
         match Test_json_utils.get_string input_json "pattern" with
         | Some pattern -> (
             Log.app (fun m -> m "Pattern: %s" pattern);
             match Test_json_utils.get_string input_json "path" with
             | Some path -> Log.app (fun m -> m "Path: %s" path)
             | None -> Log.app (fun m -> m "Path: (current directory)"))
         | None -> ())
     | "Grep" -> (
         match Test_json_utils.get_string input_json "pattern" with
         | Some pattern -> (
             Log.app (fun m -> m "Pattern: %s" pattern);
             match Test_json_utils.get_string input_json "path" with
             | Some path -> Log.app (fun m -> m "Path: %s" path)
             | None -> Log.app (fun m -> m "Path: (current directory)"))
         | None -> ())
     | _ ->
         Log.app (fun m -> m "Input: %s" (Test_json_utils.to_string input_json))
   with exn ->
     Log.info (fun m ->
         m "Failed to parse input details: %s" (Printexc.to_string exn)));

  (* Check if already granted *)
  if Granted.is_granted tool_name then begin
    Log.app (fun m -> m "→ Auto-approved (previously granted)");
    Log.info (fun m -> m "Returning allow result for %s" tool_name);
    Decision.allow ()
  end
  else begin
    (* Ask user - read from /dev/tty since stdin is connected to Claude process *)
    Printf.printf "Allow? [y/N/always]: %!";
    let tty = open_in "/dev/tty" in
    let response = input_line tty |> String.lowercase_ascii in
    close_in tty;
    match response with
    | "y" | "yes" ->
        Log.app (fun m -> m "→ Allowed (this time only)");
        Log.info (fun m -> m "User approved %s for this request only" tool_name);
        Decision.allow ()
    | "a" | "always" ->
        Granted.grant tool_name;
        Log.info (fun m ->
            m "User granted permanent permission for %s" tool_name);
        Decision.allow ()
    | _ ->
        Granted.deny tool_name;
        Log.info (fun m -> m "User denied permission for %s" tool_name);
        Decision.deny
          ~message:(Printf.sprintf "User denied access to %s" tool_name)
          ~interrupt:false
  end

let process_response client =
  let responses = Claude.Client.receive_all client in
  List.iter
    (fun response ->
      match response with
      | Claude.Response.Text t ->
          let text = Claude.Response.Text.content t in
          Log.app (fun m -> m "\n📝 Claude says:\n%s" text)
      | Claude.Response.Tool_use t ->
          Log.info (fun m ->
              m "🔧 Tool use: %s (id: %s)"
                (Claude.Response.Tool_use.name t)
                (Claude.Response.Tool_use.id t))
      | Claude.Response.Complete c ->
          (if Claude.Response.Complete.result_text c = None then
             Log.err (fun m -> m "❌ Error occurred!")
           else
             match Claude.Response.Complete.total_cost_usd c with
             | Some cost -> Log.info (fun m -> m "💰 Cost: $%.6f" cost)
             | None -> ());
          Log.info (fun m ->
              m "⏱️ Duration: %dms" (Claude.Response.Complete.duration_ms c))
      | Claude.Response.Error e ->
          Log.err (fun m -> m "❌ Error: %s" (Claude.Response.Error.message e))
      | _ -> ())
    responses

let run_demo ~sw ~env =
  Log.app (fun m -> m "🚀 Starting Permission Demo");
  Log.app (fun m -> m "==================================");
  Log.app (fun m -> m "This demo starts with NO permissions.");
  Log.app (fun m -> m "Claude will request permissions as needed.\n");

  (* Create options with custom permission callback *)
  (* DON'T specify allowed_tools - let the permission callback handle everything.
     The Default permission mode with a callback should send requests for all tools. *)
  let options =
    Claude.Options.default
    |> Claude.Options.with_model (Claude.Model.of_string "sonnet")
    |> Claude.Options.with_permission_mode Claude.Permissions.Mode.Default
    |> Claude.Options.with_permission_callback interactive_permission_callback
  in

  let client =
    Claude.Client.create ~options ~sw ~process_mgr:env#process_mgr
      ~clock:env#clock ()
  in

  (* First prompt - Claude will need to request Read permission for ../lib *)
  Log.app (fun m -> m "\n📤 Sending first prompt (reading from ../lib)...");
  Claude.Client.query client
    "Please read and analyze the source files in the ../lib directory. Focus \
     on the main OCaml modules and their purpose. What is the overall \
     architecture of this Claude library?";
  process_response client;

  (* Show current permissions *)
  Log.app (fun m -> m "\n📋 Current permission status:");
  Granted.list ();

  (* Second prompt - will need Write permission *)
  Log.app (fun m -> m "\n📤 Sending second prompt (writing TEST.md)...");
  Claude.Client.query client
    "Now write a summary of what you learned about the Claude library \
     architecture to a file called TEST.md in the current directory. Include \
     the main modules, their purposes, and how they work together.";
  process_response client;

  (* Show final permissions *)
  Log.app (fun m -> m "\n📋 Final permission status:");
  Granted.list ();

  Log.app (fun m -> m "\n==================================");
  Log.app (fun m -> m "✨ Demo complete!")

let main ~env = Switch.run @@ fun sw -> run_demo ~sw ~env

(* Command-line interface *)
open Cmdliner

let main_term env =
  let setup_log style_renderer level =
    Fmt_tty.setup_std_outputs ?style_renderer ();
    Logs.set_level level;
    Logs.set_reporter (Logs_fmt.reporter ());
    (* Set default to App level if not specified *)
    if level = None then Logs.set_level (Some Logs.App);
    (* Enable info level for Client module if in info mode or above *)
    match level with
    | Some Logs.Info | Some Logs.Debug ->
        Logs.Src.set_level Claude.Client.src (Some Logs.Info)
    | _ -> ()
  in
  let run style level =
    setup_log style level;
    main ~env
  in
  Term.(const run $ Fmt_cli.style_renderer () $ Logs_cli.level ())

let cmd env =
  let doc = "Demonstrate Claude's dynamic permission system" in
  let man =
    [
      `S Manpage.s_description;
      `P
        "This program demonstrates how to use permission callbacks with Claude.";
      `P "It starts with no permissions and asks for them interactively.";
      `P "You can grant permissions for:";
      `P "- Individual requests (y/yes)";
      `P "- All future requests of that type (a/always)";
      `P "- Or deny the request (n/no or just press Enter)";
      `S Manpage.s_examples;
      `P "Run the demo:";
      `Pre "  $(mname)";
      `P "Run with verbose output to see message flow:";
      `Pre "  $(mname) -v";
      `S Manpage.s_bugs;
      `P "Report bugs at https://github.com/your-repo/issues";
    ]
  in
  let info = Cmd.info "permission_demo" ~version:"1.0" ~doc ~man in
  Cmd.v info (main_term env)

let () = Eio_main.run @@ fun env -> exit (Cmd.eval (cmd env))
