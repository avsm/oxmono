(*---------------------------------------------------------------------------
  Copyright (c) 2025 Anil Madhavapeddy <anil@recoil.org>. All rights reserved.
  SPDX-License-Identifier: ISC
 ---------------------------------------------------------------------------*)

open Eio.Std

let src = Logs.Src.create "hooks_example" ~doc:"Hooks example"

module Log = (val Logs.src_log src : Logs.LOG)

(* Example 1: Block dangerous bash commands *)
let block_dangerous_bash input =
  if input.Claude.Hooks.PreToolUse.tool_name = "Bash" then
    match
      Claude.Tool_input.get_string input.Claude.Hooks.PreToolUse.tool_input
        "command"
    with
    | Some command ->
        if String.length command >= 6 && String.sub command 0 6 = "rm -rf" then begin
          Log.app (fun m -> m "🚫 Blocked dangerous command: %s" command);
          Claude.Hooks.PreToolUse.deny
            ~reason:"Command contains dangerous 'rm -rf' pattern" ()
        end
        else Claude.Hooks.PreToolUse.continue ()
    | _ -> Claude.Hooks.PreToolUse.continue ()
  else Claude.Hooks.PreToolUse.continue ()

(* Example 2: Log all tool usage *)
let log_tool_usage input =
  Log.app (fun m ->
      m "📝 Tool %s called" input.Claude.Hooks.PreToolUse.tool_name);
  Claude.Hooks.PreToolUse.continue ()

let run_example ~sw ~env =
  Log.app (fun m -> m "🔧 Hooks System Example");
  Log.app (fun m -> m "====================\n");

  (* Configure hooks *)
  let hooks =
    Claude.Hooks.empty
    |> Claude.Hooks.on_pre_tool_use log_tool_usage
    |> Claude.Hooks.on_pre_tool_use ~pattern:"Bash" block_dangerous_bash
  in

  let options =
    Claude.Options.default
    |> Claude.Options.with_model (Claude.Model.of_string "sonnet")
    |> Claude.Options.with_hooks hooks
  in

  let client =
    Claude.Client.create ~options ~sw ~process_mgr:env#process_mgr
      ~clock:env#clock ()
  in

  (* Test 1: Safe command (should work) *)
  Log.app (fun m -> m "Test 1: Safe bash command");
  Claude.Client.query client "Run the bash command: echo 'Hello from hooks!'";

  let messages = Claude.Client.receive_all client in
  List.iter
    (fun resp ->
      match resp with
      | Claude.Response.Text text ->
          let content = Claude.Response.Text.content text in
          if String.length content > 0 then
            Log.app (fun m -> m "Claude: %s" content)
      | Claude.Response.Complete _ -> Log.app (fun m -> m "✅ Test 1 complete\n")
      | Claude.Response.Error err ->
          Log.err (fun m -> m "❌ Error: %s" (Claude.Response.Error.message err))
      | _ -> ())
    messages;

  (* Test 2: Dangerous command (should be blocked) *)
  Log.app (fun m -> m "Test 2: Dangerous bash command (should be blocked)");
  Claude.Client.query client "Run the bash command: rm -rf /tmp/test";

  let messages = Claude.Client.receive_all client in
  List.iter
    (fun resp ->
      match resp with
      | Claude.Response.Text text ->
          let content = Claude.Response.Text.content text in
          if String.length content > 0 then
            Log.app (fun m -> m "Claude: %s" content)
      | Claude.Response.Complete _ -> Log.app (fun m -> m "✅ Test 2 complete")
      | Claude.Response.Error err ->
          Log.err (fun m -> m "❌ Error: %s" (Claude.Response.Error.message err))
      | _ -> ())
    messages;

  Log.app (fun m -> m "\n====================");
  Log.app (fun m -> m "✨ Example complete!")

let main ~env = Switch.run @@ fun sw -> run_example ~sw ~env

(* Command-line interface *)
open Cmdliner

let main_term env =
  let setup_log style_renderer level =
    Fmt_tty.setup_std_outputs ?style_renderer ();
    Logs.set_level level;
    Logs.set_reporter (Logs_fmt.reporter ());
    if level = None then Logs.set_level (Some Logs.App);
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
  let doc = "Demonstrate Claude's hooks system" in
  let info = Cmd.info "hooks_example" ~version:"1.0" ~doc in
  Cmd.v info (main_term env)

let () = Eio_main.run @@ fun env -> exit (Cmd.eval (cmd env))
