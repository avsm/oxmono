(*---------------------------------------------------------------------------
  Copyright (c) 2025 Anil Madhavapeddy <anil@recoil.org>. All rights reserved.
  SPDX-License-Identifier: ISC
 ---------------------------------------------------------------------------*)

open Eio.Std

let src = Logs.Src.create "claude.transport" ~doc:"Claude transport layer"

module Log = (val Logs.src_log src : Logs.LOG)

exception CLI_not_found of string
exception Process_error of string
exception Connection_error of string

type process = P : _ Eio.Process.t -> process

type t = {
  process : process;
  stdin : Eio.Flow.sink_ty r;
  stdin_close : [ `Close | `Flow ] r;
  stdout : Eio.Buf_read.t;
}

let setting_source_to_string = function
  | Proto.Options.User -> "user"
  | Proto.Options.Project -> "project"
  | Proto.Options.Local -> "local"

let build_command ~claude_path ~options =
  let cmd = [ claude_path; "--output-format"; "stream-json"; "--verbose" ] in

  let cmd =
    match Options.system_prompt options with
    | Some prompt -> cmd @ [ "--system-prompt"; prompt ]
    | None -> cmd
  in

  let cmd =
    match Options.append_system_prompt options with
    | Some prompt -> cmd @ [ "--append-system-prompt"; prompt ]
    | None -> cmd
  in

  let cmd =
    match Options.allowed_tools options with
    | [] -> cmd
    | tools -> cmd @ [ "--allowedTools"; String.concat "," tools ]
  in

  let cmd =
    match Options.disallowed_tools options with
    | [] -> cmd
    | tools -> cmd @ [ "--disallowedTools"; String.concat "," tools ]
  in

  let cmd =
    match Options.model options with
    | Some model -> cmd @ [ "--model"; Model.to_string model ]
    | None -> cmd
  in

  let cmd =
    match Options.permission_mode options with
    | Some mode ->
        let mode_str = Permissions.Mode.to_string mode in
        cmd @ [ "--permission-mode"; mode_str ]
    | None -> cmd
  in

  let cmd =
    match Options.permission_prompt_tool_name options with
    | Some tool_name -> cmd @ [ "--permission-prompt-tool"; tool_name ]
    | None -> cmd
  in

  (* Advanced configuration options *)
  let cmd =
    match Options.max_budget_usd options with
    | Some budget -> cmd @ [ "--max-budget-usd"; Float.to_string budget ]
    | None -> cmd
  in

  let cmd =
    match Options.fallback_model options with
    | Some model -> cmd @ [ "--fallback-model"; Model.to_string model ]
    | None -> cmd
  in

  let cmd =
    match Options.setting_sources options with
    | Some sources ->
        let sources_str =
          String.concat "," (List.map setting_source_to_string sources)
        in
        cmd @ [ "--setting-sources"; sources_str ]
    | None -> cmd
  in

  (* Add JSON Schema if specified *)
  let cmd =
    match Options.output_format options with
    | Some format ->
        let schema = Proto.Structured_output.to_json_schema format in
        let schema_str =
          match Jsont_bytesrw.encode_string' Jsont.json schema with
          | Ok s -> s
          | Error err -> failwith (Jsont.Error.to_string err)
        in
        cmd @ [ "--json-schema"; schema_str ]
    | None -> cmd
  in

  (* Use streaming input mode *)
  cmd @ [ "--input-format"; "stream-json" ]

let create ~sw ~process_mgr ~options () =
  let claude_path = "claude" in
  let cmd = build_command ~claude_path ~options in

  (* Build environment - preserve essential vars for Claude config/auth access *)
  let home = Option.value (Sys.getenv_opt "HOME") ~default:"/tmp" in
  let path = Option.value (Sys.getenv_opt "PATH") ~default:"/usr/bin:/bin" in

  (* Preserve other potentially important environment variables *)
  let preserve_vars =
    [
      "USER";
      "LOGNAME";
      "SHELL";
      "TERM";
      "XDG_CONFIG_HOME";
      "XDG_DATA_HOME";
      "XDG_CACHE_HOME";
      "ANTHROPIC_API_KEY";
      "CLAUDE_API_KEY" (* In case API key is set via env *);
    ]
  in

  let preserved =
    List.filter_map
      (fun var ->
        Option.map
          (fun value -> Printf.sprintf "%s=%s" var value)
          (Sys.getenv_opt var))
      preserve_vars
  in

  let base_env =
    [
      Printf.sprintf "HOME=%s" home;
      Printf.sprintf "PATH=%s" path;
      "CLAUDE_CODE_ENTRYPOINT=sdk-ocaml";
    ]
    @ preserved
  in

  let custom_env =
    List.map (fun (k, v) -> Printf.sprintf "%s=%s" k v) (Options.env options)
  in
  let env = Array.of_list (base_env @ custom_env) in
  Log.debug (fun m -> m "Environment: HOME=%s, PATH=%s" home path);
  Log.info (fun m ->
      m "Full environment variables: %s"
        (String.concat ", " (Array.to_list env)));

  let stdin_r, stdin_w = Eio.Process.pipe ~sw process_mgr in
  let stdout_r, stdout_w = Eio.Process.pipe ~sw process_mgr in
  let stderr_r, stderr_w = Eio.Process.pipe ~sw process_mgr in
  (* Close stderr pipes - we don't need them *)
  Eio.Flow.close stderr_r;
  Eio.Flow.close stderr_w;

  let process =
    try
      Log.info (fun m ->
          m "Spawning claude with command: %s" (String.concat " " cmd));
      Log.info (fun m -> m "Command arguments breakdown:");
      List.iteri (fun i arg -> Log.info (fun m -> m "  [%d]: %s" i arg)) cmd;
      Eio.Process.spawn ~sw process_mgr ~env
        ~stdin:(stdin_r :> Eio.Flow.source_ty r)
        ~stdout:(stdout_w :> Eio.Flow.sink_ty r)
        ?cwd:(Options.cwd options) cmd
    with exn ->
      Log.err (fun m ->
          m "Failed to spawn claude CLI: %s" (Printexc.to_string exn));
      Log.err (fun m -> m "Make sure 'claude' is installed and authenticated");
      Log.err (fun m -> m "You may need to run 'claude login' first");
      raise
        (CLI_not_found
           (Printf.sprintf "Failed to spawn claude CLI: %s"
              (Printexc.to_string exn)))
  in

  let stdin = (stdin_w :> Eio.Flow.sink_ty r) in
  let stdin_close = (stdin_w :> [ `Close | `Flow ] r) in
  let max_size =
    match Options.max_buffer_size options with
    | Some size -> size
    | None -> 100_000_000 (* Default 100MB *)
  in
  let stdout =
    Eio.Buf_read.of_flow ~max_size (stdout_r :> Eio.Flow.source_ty r)
  in

  { process = P process; stdin; stdin_close; stdout }

let send t json =
  let data =
    match Jsont_bytesrw.encode_string' Jsont.json json with
    | Ok s -> s
    | Error err -> failwith (Jsont.Error.to_string err)
  in
  Log.debug (fun m -> m "Sending: %s" data);
  try Eio.Flow.write t.stdin [ Cstruct.of_string (data ^ "\n") ]
  with exn ->
    Log.err (fun m -> m "Failed to send message: %s" (Printexc.to_string exn));
    raise
      (Connection_error
         (Printf.sprintf "Failed to send message: %s" (Printexc.to_string exn)))

let receive_line t =
  try
    match Eio.Buf_read.line t.stdout with
    | line ->
        Log.debug (fun m -> m "Raw JSON: %s" line);
        Some line
    | exception End_of_file ->
        Log.debug (fun m -> m "Received EOF");
        None
  with exn ->
    Log.err (fun m ->
        m "Failed to receive message: %s" (Printexc.to_string exn));
    raise
      (Connection_error
         (Printf.sprintf "Failed to receive message: %s"
            (Printexc.to_string exn)))

let interrupt t =
  Log.info (fun m -> m "Sending interrupt signal");
  (* Create interrupt request using Proto types *)
  let request = Proto.Control.Request.interrupt () in
  let envelope = Proto.Control.create_request ~request_id:"" ~request () in
  let outgoing = Proto.Outgoing.Control_request envelope in
  let interrupt_msg = Proto.Outgoing.to_json outgoing in
  send t interrupt_msg

let close t =
  try
    Eio.Flow.close t.stdin_close;
    let (P process) = t.process in
    Eio.Process.await_exn process
  with _ -> ()
