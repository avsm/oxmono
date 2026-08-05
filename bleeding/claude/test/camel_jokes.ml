(*---------------------------------------------------------------------------
  Copyright (c) 2025 Anil Madhavapeddy <anil@recoil.org>. All rights reserved.
  SPDX-License-Identifier: ISC
 ---------------------------------------------------------------------------*)

open Eio.Std

let src = Logs.Src.create "camel_jokes" ~doc:"Camel joke competition"

module Log = (val Logs.src_log src : Logs.LOG)

let process_claude_response client name =
  Log.info (fun m -> m "=== %s's Response ===" name);
  let responses = Claude.Client.receive_all client in
  List.iter
    (fun resp ->
      match resp with
      | Claude.Response.Text t ->
          let text = Claude.Response.Text.content t in
          Log.app (fun m -> m "%s: %s" name text)
      | Claude.Response.Tool_use t ->
          Log.debug (fun m ->
              m "%s using tool: %s" name (Claude.Response.Tool_use.name t))
      | Claude.Response.Thinking t ->
          Log.debug (fun m ->
              m "%s thinking: %s" name (Claude.Response.Thinking.content t))
      | Claude.Response.Complete c ->
          (if Claude.Response.Complete.total_cost_usd c <> None then
             let cost =
               Option.get (Claude.Response.Complete.total_cost_usd c)
             in
             Log.info (fun m -> m "%s's joke cost: $%.6f" name cost));
          Log.debug (fun m ->
              m "%s session: %s, duration: %dms" name
                (Claude.Response.Complete.session_id c)
                (Claude.Response.Complete.duration_ms c))
      | Claude.Response.Error e ->
          Log.err (fun m ->
              m "Error from %s: %s" name (Claude.Response.Error.message e))
      | Claude.Response.Init _ ->
          (* Init messages are already logged by the library *)
          ()
      | Claude.Response.Tool_result _ ->
          (* Tool results are user messages, skip *)
          ())
    responses

let run_claude ~sw ~env name prompt =
  Log.info (fun m -> m "🐪 Starting %s..." name);
  let options =
    Claude.Options.default
    |> Claude.Options.with_model (Claude.Model.of_string "sonnet")
    |> Claude.Options.with_allowed_tools []
  in

  let client =
    Claude.Client.create ~options ~sw ~process_mgr:env#process_mgr
      ~clock:env#clock ()
  in

  Claude.Client.query client prompt;
  process_claude_response client name

let main ~env =
  Switch.run @@ fun sw ->
  Log.app (fun m -> m "🐪 Starting the Great Camel Joke Competition! 🐪");
  Log.app (fun m -> m "================================================\n");

  let prompts =
    [
      ( "Claude 1",
        "Tell me a short, funny joke about camels! Make it original and clever."
      );
      ( "Claude 2",
        "Give me your best camel joke - something witty and unexpected!" );
      ("Claude 3", "Share a hilarious camel joke that will make everyone laugh!");
    ]
  in

  (* Run all three Claudes concurrently *)
  Fiber.all
    (List.map
       (fun (name, prompt) -> fun () -> run_claude ~sw ~env name prompt)
       prompts);

  Log.app (fun m -> m "\n================================================");
  Log.app (fun m -> m "🎉 The Camel Joke Competition is complete! 🎉")

(* Command-line interface *)
open Cmdliner

let main_term env =
  let setup_log style_renderer level =
    Fmt_tty.setup_std_outputs ?style_renderer ();
    Logs.set_level level;
    Logs.set_reporter (Logs_fmt.reporter ());
    (* Set default to App level if not specified *)
    if level = None then Logs.set_level (Some Logs.App);
    (* Enable debug for Client module if in debug mode *)
    if level = Some Logs.Debug then
      Logs.Src.set_level Claude.Client.src (Some Logs.Debug)
  in
  let run style level =
    setup_log style level;
    main ~env
  in
  Term.(const run $ Fmt_cli.style_renderer () $ Logs_cli.level ())

let cmd env =
  let doc = "Run the Great Camel Joke Competition using Claude" in
  let man =
    [
      `S Manpage.s_description;
      `P
        "This program runs three concurrent Claude instances to generate camel \
         jokes.";
      `P "Use $(b,-v) or $(b,--verbosity=info) to see RPC message traffic.";
      `P
        "Use $(b,-vv) or $(b,--verbosity=debug) to see all internal operations.";
      `S Manpage.s_examples;
      `P "Run with normal output:";
      `Pre "  $(mname)";
      `P "Run with info-level logging (RPC traffic):";
      `Pre "  $(mname) -v";
      `Pre "  $(mname) --verbosity=info";
      `P "Run with debug logging (all operations):";
      `Pre "  $(mname) -vv";
      `Pre "  $(mname) --verbosity=debug";
      `P "Enable debug for specific modules:";
      `Pre "  LOGS='claude.transport=debug' $(mname)";
      `Pre "  LOGS='claude.message=info,camel_jokes=debug' $(mname)";
      `S Manpage.s_bugs;
      `P "Report bugs at https://github.com/your-repo/issues";
    ]
  in
  let info = Cmd.info "camel_jokes" ~version:"1.0" ~doc ~man in
  Cmd.v info (main_term env)

let () = Eio_main.run @@ fun env -> exit (Cmd.eval (cmd env))
