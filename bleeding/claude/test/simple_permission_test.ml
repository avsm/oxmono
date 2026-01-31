(*---------------------------------------------------------------------------
  Copyright (c) 2025 Anil Madhavapeddy <anil@recoil.org>. All rights reserved.
  SPDX-License-Identifier: ISC
 ---------------------------------------------------------------------------*)

open Eio.Std

let src = Logs.Src.create "simple_permission_test" ~doc:"Simple permission test"

module Log = (val Logs.src_log src : Logs.LOG)

(* Auto-allow callback that logs what it sees *)
let auto_allow_callback ctx =
  Log.app (fun m -> m "\n🔐 Permission callback invoked!");
  Log.app (fun m -> m "   Tool: %s" ctx.Claude.Permissions.tool_name);
  Log.app (fun m ->
      m "   Input: %s"
        (Test_json_utils.to_string
           (Claude.Tool_input.to_json ctx.Claude.Permissions.input)));
  Log.app (fun m -> m "   ✅ Auto-allowing");
  Claude.Permissions.Decision.allow ()

let run_test ~sw ~env =
  Log.app (fun m -> m "🧪 Testing Permission Callbacks (Auto-Allow Mode)");
  Log.app (fun m -> m "====================================================");

  (* Create options with permission callback *)
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

  (* Query that should trigger Write tool *)
  Log.app (fun m -> m "\n📤 Asking Claude to write a file...");
  Claude.Client.query client
    "Write a simple hello world message to /tmp/test_permission.txt";

  (* Process response *)
  let messages = Claude.Client.receive_all client in
  Log.app (fun m -> m "\n📨 Received %d messages" (List.length messages));

  let tool_count = ref 0 in
  let write_used = ref false in

  List.iter
    (fun resp ->
      match resp with
      | Claude.Response.Text text ->
          let content = Claude.Response.Text.content text in
          if String.length content > 0 then
            Log.app (fun m -> m "\n💬 Claude: %s" content)
      | Claude.Response.Tool_use t ->
          incr tool_count;
          let tool_name = Claude.Response.Tool_use.name t in
          if tool_name = "Write" then write_used := true;
          Log.app (fun m -> m "🔧 Tool use #%d: %s" !tool_count tool_name)
      | Claude.Response.Tool_result r ->
          let tool_use_id = Claude.Content_block.Tool_result.tool_use_id r in
          let is_error =
            Claude.Content_block.Tool_result.is_error r
            |> Option.value ~default:false
          in
          if is_error then begin
            Log.app (fun m -> m "\n⚠️  Tool result error for %s:" tool_use_id);
            match Claude.Content_block.Tool_result.content r with
            | Some json ->
                let s =
                  match Jsont_bytesrw.encode_string' Jsont.json json with
                  | Ok str -> str
                  | Error _ -> "<encoding error>"
                in
                Log.app (fun m -> m "   %s" s)
            | None -> ()
          end
      | Claude.Response.Complete result ->
          Log.app (fun m -> m "\n✅ Success!");
          (match Claude.Response.Complete.total_cost_usd result with
          | Some cost -> Log.app (fun m -> m "💰 Cost: $%.6f" cost)
          | None -> ());
          Log.app (fun m ->
              m "⏱️  Duration: %dms"
                (Claude.Response.Complete.duration_ms result))
      | Claude.Response.Error err ->
          Log.err (fun m ->
              m "\n❌ Error: %s" (Claude.Response.Error.message err))
      | _ -> ())
    messages;

  Log.app (fun m -> m "\n====================================================");
  Log.app (fun m -> m "📊 Test Results:");
  Log.app (fun m -> m "   Total tools used: %d" !tool_count);
  Log.app (fun m -> m "   Write tool used: %b" !write_used);

  if !write_used then
    Log.app (fun m ->
        m "   ✅ Permission callback successfully intercepted Write tool!")
  else Log.app (fun m -> m "   ⚠️  Write tool was not used (unexpected)");

  Log.app (fun m -> m "====================================================");
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
        Logs.Src.set_level Claude.Client.src (Some Logs.Info);
        Logs.Src.set_level Claude.Transport.src (Some Logs.Info)
    | _ -> ()
  in
  let run style level =
    setup_log style level;
    main ~env
  in
  Term.(const run $ Fmt_cli.style_renderer () $ Logs_cli.level ())

let cmd env =
  let doc = "Test permission callback with auto-allow" in
  let info = Cmd.info "simple_permission_test" ~version:"1.0" ~doc in
  Cmd.v info (main_term env)

let () = Eio_main.run @@ fun env -> exit (Cmd.eval (cmd env))
