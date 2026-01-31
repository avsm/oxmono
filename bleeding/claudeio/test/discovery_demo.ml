(*---------------------------------------------------------------------------
  Copyright (c) 2025 Anil Madhavapeddy <anil@recoil.org>. All rights reserved.
  SPDX-License-Identifier: ISC
 ---------------------------------------------------------------------------*)

open Eio.Std

let src =
  Logs.Src.create "discovery_demo" ~doc:"Permission discovery demonstration"

module Log = (val Logs.src_log src : Logs.LOG)

let process_response client =
  let responses = Claude.Client.receive_all client in
  List.iter
    (fun resp ->
      match resp with
      | Claude.Response.Text text ->
          let content = Claude.Response.Text.content text in
          Log.app (fun m ->
              m "Claude: %s"
                (if String.length content > 100 then
                   String.sub content 0 100 ^ "..."
                 else content))
      | Claude.Response.Tool_use t ->
          Log.info (fun m -> m "Tool use: %s" (Claude.Response.Tool_use.name t))
      | Claude.Response.Error err ->
          Log.err (fun m -> m "Error: %s" (Claude.Response.Error.message err))
      | Claude.Response.Complete result -> (
          match Claude.Response.Complete.total_cost_usd result with
          | Some cost -> Log.info (fun m -> m "Cost: $%.6f" cost)
          | None -> ())
      | _ -> ())
    responses

let run_discovery ~sw ~env =
  Log.app (fun m -> m "🔍 Permission Discovery Demo");
  Log.app (fun m -> m "=============================");
  Log.app (fun m -> m "This will discover what permissions Claude needs.\n");

  (* Create client with discovery mode *)
  let options =
    Claude.Options.default
    |> Claude.Options.with_model (Claude.Proto.Model.of_string "sonnet")
  in
  let client =
    Claude.Client.create ~options ~sw ~process_mgr:env#process_mgr
      ~clock:env#clock ()
  in
  Claude.Client.enable_permission_discovery client;

  (* Send a prompt that will need permissions *)
  Log.app (fun m -> m "Asking Claude to read a secret file...");
  Claude.Client.query client
    "Please read the file test/secret_data.txt and tell me what the secret \
     code is.";
  process_response client;

  (* Check what permissions were requested *)
  let permissions = Claude.Client.discovered_permissions client in
  if permissions = [] then
    Log.app (fun m ->
        m
          "\n\
           📋 No permissions were requested (Claude may have used its \
           knowledge).")
  else begin
    Log.app (fun m -> m "\n📋 Permissions that were requested:");
    List.iter
      (fun rule ->
        Log.app (fun m ->
            m "  - Tool: %s%s"
              (Claude.Permissions.Rule.tool_name rule)
              (match Claude.Permissions.Rule.rule_content rule with
              | Some content -> Printf.sprintf " (rule: %s)" content
              | None -> "")))
      permissions
  end

let main ~env = Switch.run @@ fun sw -> run_discovery ~sw ~env

(* Command-line interface *)
open Cmdliner

let main_term env =
  let setup_log style_renderer level =
    Fmt_tty.setup_std_outputs ?style_renderer ();
    Logs.set_level level;
    Logs.set_reporter (Logs_fmt.reporter ());
    if level = None then Logs.set_level (Some Logs.App)
  in
  let run style level =
    setup_log style level;
    main ~env
  in
  Term.(const run $ Fmt_cli.style_renderer () $ Logs_cli.level ())

let cmd env =
  let doc = "Discover what permissions Claude needs" in
  let man =
    [
      `S Manpage.s_description;
      `P
        "This program runs Claude in discovery mode to see what permissions it \
         requests.";
    ]
  in
  let info = Cmd.info "discovery_demo" ~version:"1.0" ~doc ~man in
  Cmd.v info (main_term env)

let () = Eio_main.run @@ fun env -> exit (Cmd.eval (cmd env))
