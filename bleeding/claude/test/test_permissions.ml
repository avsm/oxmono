(*---------------------------------------------------------------------------
  Copyright (c) 2025 Anil Madhavapeddy <anil@recoil.org>. All rights reserved.
  SPDX-License-Identifier: ISC
 ---------------------------------------------------------------------------*)

open Eio.Std

let src = Logs.Src.create "test_permissions" ~doc:"Permission callback test"

module Log = (val Logs.src_log src : Logs.LOG)

(* Simple auto-allow permission callback *)
let auto_allow_callback ctx =
  Log.app (fun m ->
      m "✅ Auto-allowing tool: %s" ctx.Claude.Permissions.tool_name);
  Claude.Permissions.Decision.allow ()

let run_test ~sw ~env =
  Log.app (fun m -> m "🧪 Testing Permission Callbacks");
  Log.app (fun m -> m "================================");

  (* Create options with custom permission callback *)
  let options =
    Claude.Options.default
    |> Claude.Options.with_model (Claude.Model.of_string "sonnet")
    |> Claude.Options.with_permission_callback auto_allow_callback
  in

  Log.app (fun m -> m "Creating client with permission callback...");
  let client =
    Claude.Client.create ~options ~sw ~process_mgr:env#process_mgr
      ~clock:env#clock ()
  in

  (* Simple query that will trigger tool use *)
  Log.app (fun m -> m "\n📤 Sending test query...");
  Claude.Client.query client "What is 2 + 2? Just give me the number.";

  (* Process response *)
  let messages = Claude.Client.receive_all client in
  Log.app (fun m -> m "\n📨 Received %d messages" (List.length messages));

  List.iter
    (fun resp ->
      match resp with
      | Claude.Response.Text text ->
          Log.app (fun m -> m "Claude: %s" (Claude.Response.Text.content text))
      | Claude.Response.Tool_use t ->
          Log.app (fun m ->
              m "🔧 Tool use: %s" (Claude.Response.Tool_use.name t))
      | Claude.Response.Complete result ->
          Log.app (fun m -> m "✅ Success!");
          Log.app (fun m ->
              m "Duration: %dms" (Claude.Response.Complete.duration_ms result))
      | Claude.Response.Error err ->
          Log.err (fun m -> m "❌ Error: %s" (Claude.Response.Error.message err))
      | _ -> ())
    messages;

  Log.app (fun m -> m "\n================================");
  Log.app (fun m -> m "✨ Test complete!")

let main ~env = Switch.run @@ fun sw -> run_test ~sw ~env

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
  let doc = "Test permission callback functionality" in
  let info = Cmd.info "test_permissions" ~version:"1.0" ~doc in
  Cmd.v info (main_term env)

let () = Eio_main.run @@ fun env -> exit (Cmd.eval (cmd env))
