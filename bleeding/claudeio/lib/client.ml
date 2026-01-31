(*---------------------------------------------------------------------------
  Copyright (c) 2025 Anil Madhavapeddy <anil@recoil.org>. All rights reserved.
  SPDX-License-Identifier: ISC
 ---------------------------------------------------------------------------*)

let src = Logs.Src.create "claude.client" ~doc:"Claude client"

module Log = (val Logs.src_log src : Logs.LOG)

(** Control response builders using Sdk_control codecs *)
module Control_response = struct
  let success ~request_id ~response =
    let resp = Sdk_control.Response.success ~request_id ?response () in
    let ctrl = Sdk_control.create_response ~response:resp () in
    Jsont.Json.encode Sdk_control.jsont ctrl
    |> Err.get_ok ~msg:"Control_response.success: "

  let error ~request_id ~code ~message ?data () =
    let error_detail =
      Sdk_control.Response.error_detail ~code ~message ?data ()
    in
    let resp = Sdk_control.Response.error ~request_id ~error:error_detail () in
    let ctrl = Sdk_control.create_response ~response:resp () in
    Jsont.Json.encode Sdk_control.jsont ctrl
    |> Err.get_ok ~msg:"Control_response.error: "
end

(* Helper functions for JSON manipulation using jsont *)
let json_to_string json =
  Jsont_bytesrw.encode_string' Jsont.json json
  |> Result.map_error Jsont.Error.to_string
  |> Err.get_ok ~msg:""

(** Wire-level codec for hook matcher configuration sent to CLI. *)
module Hook_matcher_wire = struct
  type t = { matcher : string option; hook_callback_ids : string list }

  let jsont : t Jsont.t =
    let make matcher hook_callback_ids = { matcher; hook_callback_ids } in
    Jsont.Object.map ~kind:"HookMatcherWire" make
    |> Jsont.Object.opt_mem "matcher" Jsont.string ~enc:(fun r -> r.matcher)
    |> Jsont.Object.mem "hookCallbackIds" (Jsont.list Jsont.string)
         ~enc:(fun r -> r.hook_callback_ids)
    |> Jsont.Object.finish

  let encode matchers =
    List.map
      (fun m ->
        Jsont.Json.encode jsont m
        |> Err.get_ok ~msg:"Hook_matcher_wire.encode: ")
      matchers
    |> Jsont.Json.list
end

type t = {
  transport : Transport.t;
  mutable permission_callback : Permissions.callback option;
  mutable permission_log : Permissions.Rule.t list ref option;
  hook_callbacks : (string, Jsont.json -> Proto.Hooks.result) Hashtbl.t;
  mutable session_id : string option;
  control_responses : (string, Jsont.json) Hashtbl.t;
  control_mutex : Eio.Mutex.t;
  control_condition : Eio.Condition.t;
  clock : float Eio.Time.clock_ty Eio.Resource.t;
  (* Track tool_use_ids we've already responded to, preventing duplicates *)
  responded_tool_ids : (string, unit) Hashtbl.t;
  (* In-process MCP servers for custom tools *)
  mcp_servers : (string, Mcp_server.t) Hashtbl.t;
}

let session_id t = t.session_id

let handle_control_request t (ctrl_req : Sdk_control.control_request) =
  let request_id = ctrl_req.request_id in
  Log.info (fun m -> m "Handling control request: %s" request_id);

  match ctrl_req.request with
  | Sdk_control.Request.Permission req ->
      let tool_name = req.tool_name in
      let input_json = req.input in
      Log.info (fun m ->
          m "Permission request for tool '%s' with input: %s" tool_name
            (json_to_string input_json));
      (* Convert permission_suggestions to suggested rules *)
      let suggestions = Option.value req.permission_suggestions ~default:[] in
      let suggested_rules =
        Permissions.extract_rules_from_proto_updates suggestions
      in

      (* Convert input to Tool_input.t *)
      let input = Tool_input.of_json input_json in

      (* Create context *)
      let context : Permissions.context =
        { tool_name; input; suggested_rules }
      in

      Log.info (fun m ->
          m "Invoking permission callback for tool: %s" tool_name);
      let callback =
        Option.value t.permission_callback ~default:Permissions.default_allow
      in
      let decision = callback context in
      Log.info (fun m ->
          m "Permission callback returned: %s"
            (if Permissions.Decision.is_allow decision then "ALLOW" else "DENY"));

      (* Convert permission decision to proto result *)
      let proto_result =
        Permissions.Decision.to_proto_result ~original_input:input decision
      in

      (* Encode to JSON *)
      let response_data =
        match Jsont.Json.encode Proto.Permissions.Result.jsont proto_result with
        | Ok json -> json
        | Error err ->
            Log.err (fun m -> m "Failed to encode permission result: %s" err);
            failwith "Permission result encoding failed"
      in
      let response =
        Control_response.success ~request_id ~response:(Some response_data)
      in
      Log.info (fun m ->
          m "Sending control response: %s" (json_to_string response));
      Transport.send t.transport response
  | Sdk_control.Request.Hook_callback req -> (
      let callback_id = req.callback_id in
      let input = req.input in
      let _tool_use_id = req.tool_use_id in
      Log.info (fun m ->
          m "Hook callback request for callback_id: %s" callback_id);

      try
        let callback = Hashtbl.find t.hook_callbacks callback_id in
        let result = callback input in

        let result_json =
          Jsont.Json.encode Proto.Hooks.result_jsont result
          |> Err.get_ok ~msg:"Failed to encode hook result: "
        in
        Log.debug (fun m ->
            m "Hook result JSON: %s" (json_to_string result_json));
        let response =
          Control_response.success ~request_id ~response:(Some result_json)
        in
        Log.info (fun m -> m "Hook callback succeeded, sending response");
        Transport.send t.transport response
      with
      | Not_found ->
          let error_msg =
            Printf.sprintf "Hook callback not found: %s" callback_id
          in
          Log.err (fun m -> m "%s" error_msg);
          Transport.send t.transport
            (Control_response.error ~request_id ~code:`Method_not_found
               ~message:error_msg ())
      | exn ->
          let error_msg =
            Printf.sprintf "Hook callback error: %s" (Printexc.to_string exn)
          in
          Log.err (fun m -> m "%s" error_msg);
          Transport.send t.transport
            (Control_response.error ~request_id ~code:`Internal_error
               ~message:error_msg ()))
  | Sdk_control.Request.Mcp_message req -> (
      let module
        (* Handle MCP request for in-process SDK servers *)
        J =
        Jsont.Json
      in
      let server_name = req.server_name in
      let message = req.message in
      Log.info (fun m -> m "MCP request for server '%s'" server_name);

      match Hashtbl.find_opt t.mcp_servers server_name with
      | None ->
          let error_msg =
            Printf.sprintf "MCP server '%s' not found" server_name
          in
          Log.err (fun m -> m "%s" error_msg);
          (* Return JSONRPC error in mcp_response format *)
          let mcp_error =
            J.object'
              [
                J.mem (J.name "jsonrpc") (J.string "2.0");
                J.mem (J.name "id") (J.null ());
                J.mem (J.name "error")
                  (J.object'
                     [
                       J.mem (J.name "code") (J.number (-32601.0));
                       J.mem (J.name "message") (J.string error_msg);
                     ]);
              ]
          in
          let response_data =
            J.object' [ J.mem (J.name "mcp_response") mcp_error ]
          in
          let response =
            Control_response.success ~request_id ~response:(Some response_data)
          in
          Transport.send t.transport response
      | Some server ->
          let mcp_response = Mcp_server.handle_json_message server message in
          Log.debug (fun m ->
              m "MCP response: %s" (json_to_string mcp_response));
          let response_data =
            J.object' [ J.mem (J.name "mcp_response") mcp_response ]
          in
          let response =
            Control_response.success ~request_id ~response:(Some response_data)
          in
          Transport.send t.transport response)
  | _ ->
      (* Other request types not handled here *)
      let error_msg = "Unsupported control request type" in
      Transport.send t.transport
        (Control_response.error ~request_id ~code:`Invalid_request
           ~message:error_msg ())

let handle_control_response t control_resp =
  let request_id =
    match control_resp.Sdk_control.response with
    | Sdk_control.Response.Success s -> s.request_id
    | Sdk_control.Response.Error e -> e.request_id
  in
  Log.debug (fun m ->
      m "Received control response for request_id: %s" request_id);

  (* Store the response as JSON and signal waiting threads *)
  let json =
    Jsont.Json.encode Sdk_control.control_response_jsont control_resp
    |> Err.get_ok ~msg:"Failed to encode control response: "
  in
  Eio.Mutex.use_rw ~protect:false t.control_mutex (fun () ->
      Hashtbl.replace t.control_responses request_id json;
      Eio.Condition.broadcast t.control_condition)

let handle_raw_messages t =
  let rec loop () =
    match Transport.receive_line t.transport with
    | None ->
        (* EOF *)
        Log.debug (fun m -> m "Handle messages: EOF received");
        Seq.Nil
    | Some line -> (
        (* Use unified Incoming codec for all message types *)
        match Jsont_bytesrw.decode_string' Incoming.jsont line with
        | Ok incoming -> Seq.Cons (incoming, loop)
        | Error err ->
            Log.err (fun m ->
                m "Failed to decode incoming message: %s\nLine: %s"
                  (Jsont.Error.to_string err)
                  line);
            loop ())
  in
  Log.debug (fun m -> m "Starting message handler");
  loop

let handle_messages t =
  let raw_seq = handle_raw_messages t in
  let rec loop raw_seq =
    match raw_seq () with
    | Seq.Nil -> Seq.Nil
    | Seq.Cons (incoming, rest) -> (
        match incoming with
        | Incoming.Message msg ->
            Log.info (fun m -> m "← %a" Message.pp msg);

            (* Extract session ID from system messages *)
            (match msg with
            | Message.System sys ->
                Message.System.session_id sys
                |> Option.iter (fun session_id ->
                    t.session_id <- Some session_id;
                    Log.debug (fun m -> m "Stored session ID: %s" session_id))
            | _ -> ());

            (* Convert message to response events *)
            let responses = Response.of_message msg in
            emit_responses responses rest
        | Incoming.Control_response resp ->
            handle_control_response t resp;
            loop rest
        | Incoming.Control_request ctrl_req ->
            Log.info (fun m ->
                m "Received control request (request_id: %s)"
                  ctrl_req.request_id);
            handle_control_request t ctrl_req;
            loop rest)
  and emit_responses responses rest =
    match responses with
    | [] -> loop rest
    | r :: rs -> Seq.Cons (r, fun () -> emit_responses rs rest)
  in
  loop raw_seq

let create ?(options = Options.default) ~sw ~process_mgr ~clock () =
  (* Automatically enable permission prompt tool when callback is configured
     (matching Python SDK behavior in client.py:104-121) *)
  let options =
    match Options.permission_callback options with
    | Some _ when Options.permission_prompt_tool_name options = None ->
        (* Set permission_prompt_tool_name to "stdio" to enable control protocol *)
        Options.with_permission_prompt_tool_name "stdio" options
    | _ -> options
  in
  let transport = Transport.create ~sw ~process_mgr ~options () in

  (* Setup hook callbacks *)
  let hook_callbacks = Hashtbl.create 16 in
  let next_callback_id = ref 0 in

  (* Setup MCP servers from options *)
  let mcp_servers_ht = Hashtbl.create 16 in
  List.iter
    (fun (name, server) ->
      Log.info (fun m -> m "Registering MCP server: %s" name);
      Hashtbl.add mcp_servers_ht name server)
    (Options.mcp_servers options);

  let t =
    {
      transport;
      permission_callback = Options.permission_callback options;
      permission_log = None;
      hook_callbacks;
      session_id = None;
      control_responses = Hashtbl.create 16;
      control_mutex = Eio.Mutex.create ();
      control_condition = Eio.Condition.create ();
      clock;
      responded_tool_ids = Hashtbl.create 16;
      mcp_servers = mcp_servers_ht;
    }
  in

  (* Register hooks and send initialize if hooks are configured *)
  Options.hooks options
  |> Option.iter (fun hooks_config ->
      Log.info (fun m -> m "Registering hooks...");

      (* Get callbacks in wire format from the new Hooks API *)
      let callbacks_by_event = Hooks.get_callbacks hooks_config in

      (* Build hooks configuration with callback IDs as (string * Jsont.json) list *)
      let hooks_list =
        List.map
          (fun (event, matchers) ->
            let event_name = Proto.Hooks.event_to_string event in
            let matcher_wires =
              List.map
                (fun (pattern, callback) ->
                  let callback_id =
                    Printf.sprintf "hook_%d" !next_callback_id
                  in
                  incr next_callback_id;
                  Hashtbl.add hook_callbacks callback_id callback;
                  Log.debug (fun m ->
                      m "Registered callback: %s for event: %s" callback_id
                        event_name);
                  Hook_matcher_wire.
                    { matcher = pattern; hook_callback_ids = [ callback_id ] })
                matchers
            in
            (event_name, Hook_matcher_wire.encode matcher_wires))
          callbacks_by_event
      in

      (* Create initialize request using Sdk_control codec *)
      let request = Sdk_control.Request.initialize ~hooks:hooks_list () in
      let ctrl_req =
        Sdk_control.create_request ~request_id:"init_hooks" ~request ()
      in
      let initialize_msg =
        Jsont.Json.encode Sdk_control.jsont ctrl_req
        |> Err.get_ok ~msg:"Failed to encode initialize request: "
      in
      Log.info (fun m -> m "Sending hooks initialize request");
      Transport.send t.transport initialize_msg);

  t

(* Helper to send a message with proper "type" wrapper via Proto.Outgoing *)
let send_message t msg =
  Log.info (fun m -> m "→ %a" Message.pp msg);
  let proto_msg = Message.to_proto msg in
  let outgoing = Proto.Outgoing.Message proto_msg in
  let json = Proto.Outgoing.to_json outgoing in
  Transport.send t.transport json

let query t prompt =
  let msg = Message.user_string prompt in
  send_message t msg

let respond_to_tool t ~tool_use_id ~content ?(is_error = false) () =
  (* Check for duplicate response - prevents API errors from multiple responses *)
  if Hashtbl.mem t.responded_tool_ids tool_use_id then begin
    Log.warn (fun m ->
        m "Skipping duplicate tool response for tool_use_id: %s" tool_use_id)
  end
  else begin
    Hashtbl.add t.responded_tool_ids tool_use_id ();
    let user_msg =
      Message.User.with_tool_result ~tool_use_id ~content ~is_error ()
    in
    let msg = Message.User user_msg in
    send_message t msg
  end

let respond_to_tools t responses =
  (* Filter out duplicates *)
  let new_responses =
    List.filter
      (fun (tool_use_id, _, _) ->
        if Hashtbl.mem t.responded_tool_ids tool_use_id then begin
          Log.warn (fun m ->
              m "Skipping duplicate tool response for tool_use_id: %s"
                tool_use_id);
          false
        end
        else begin
          Hashtbl.add t.responded_tool_ids tool_use_id ();
          true
        end)
      responses
  in
  if new_responses <> [] then begin
    let tool_results =
      List.map
        (fun (tool_use_id, content, is_error_opt) ->
          let is_error = Option.value is_error_opt ~default:false in
          Content_block.tool_result ~tool_use_id ~content ~is_error ())
        new_responses
    in
    let user_msg = Message.User.of_blocks tool_results in
    let msg = Message.User user_msg in
    send_message t msg
  end

let clear_tool_response_tracking t = Hashtbl.clear t.responded_tool_ids
let receive t = fun () -> handle_messages t

let run t ~handler =
  (* Stop after Complete response - don't wait for EOF since Claude CLI
     waits for stdin to close before exiting, causing a deadlock *)
  let rec loop seq =
    match seq () with
    | Seq.Nil -> ()
    | Seq.Cons ((Response.Complete _ as resp), _) ->
        Handler.dispatch handler resp
    | Seq.Cons (resp, rest) ->
        Handler.dispatch handler resp;
        loop rest
  in
  loop (receive t)

let receive_all t =
  let rec collect acc seq =
    match seq () with
    | Seq.Nil ->
        Log.debug (fun m ->
            m "End of response sequence (%d responses)" (List.length acc));
        List.rev acc
    | Seq.Cons ((Response.Complete _ as resp), _) ->
        Log.debug (fun m -> m "Received final Complete response");
        List.rev (resp :: acc)
    | Seq.Cons (resp, rest) -> collect (resp :: acc) rest
  in
  collect [] (receive t)

let interrupt t = Transport.interrupt t.transport

let enable_permission_discovery t =
  let log = ref [] in
  let callback = Permissions.discovery log in
  t.permission_callback <- Some callback;
  t.permission_log <- Some log

let discovered_permissions t =
  t.permission_log |> Option.map ( ! ) |> Option.value ~default:[]

(* Helper to send a control request and wait for response *)
let send_control_request t ~request_id request =
  (* Send the control request *)
  let control_msg = Sdk_control.create_request ~request_id ~request () in
  let json =
    Jsont.Json.encode Sdk_control.jsont control_msg
    |> Err.get_ok ~msg:"Failed to encode control request: "
  in
  Log.info (fun m -> m "Sending control request: %s" (json_to_string json));
  Transport.send t.transport json;

  (* Wait for the response with timeout *)
  let max_wait = 10.0 in
  (* 10 seconds timeout *)
  let start_time = Eio.Time.now t.clock in

  let rec wait_for_response () =
    Eio.Mutex.use_rw ~protect:false t.control_mutex (fun () ->
        match Hashtbl.find_opt t.control_responses request_id with
        | Some response_json ->
            (* Remove it from the table *)
            Hashtbl.remove t.control_responses request_id;
            response_json
        | None ->
            let elapsed = Eio.Time.now t.clock -. start_time in
            if elapsed > max_wait then
              raise
                (Failure
                   (Printf.sprintf "Timeout waiting for control response: %s"
                      request_id))
            else (
              (* Release mutex and wait for signal *)
              Eio.Condition.await_no_mutex t.control_condition;
              wait_for_response ()))
  in

  let response_json = wait_for_response () in
  Log.debug (fun m ->
      m "Received control response: %s" (json_to_string response_json));

  (* Parse the response - extract the "response" field using jsont codec *)
  let response_field_codec =
    Jsont.Object.map ~kind:"ResponseField" Fun.id
    |> Jsont.Object.mem "response" Jsont.json ~enc:Fun.id
    |> Jsont.Object.finish
  in
  let response_data =
    Jsont.Json.decode response_field_codec response_json
    |> Err.get_ok' ~msg:"Failed to extract response field: "
  in
  let response =
    Jsont.Json.decode Sdk_control.Response.jsont response_data
    |> Err.get_ok' ~msg:"Failed to decode response: "
  in
  match response with
  | Sdk_control.Response.Success s -> s.response
  | Sdk_control.Response.Error e ->
      raise
        (Failure
           (Printf.sprintf "Control request failed: [%d] %s" e.error.code
              e.error.message))

let set_permission_mode t mode =
  let request_id = Printf.sprintf "set_perm_mode_%f" (Eio.Time.now t.clock) in
  let proto_mode = Permissions.Mode.to_proto mode in
  let request = Sdk_control.Request.set_permission_mode ~mode:proto_mode () in
  let _response = send_control_request t ~request_id request in
  Log.info (fun m ->
      m "Permission mode set to: %s" (Permissions.Mode.to_string mode))

let set_model t model =
  let model_str = Model.to_string model in
  let request_id = Printf.sprintf "set_model_%f" (Eio.Time.now t.clock) in
  let request = Sdk_control.Request.set_model ~model:model_str () in
  let _response = send_control_request t ~request_id request in
  Log.info (fun m -> m "Model set to: %s" model_str)

let get_server_info t =
  let request_id = Printf.sprintf "get_server_info_%f" (Eio.Time.now t.clock) in
  let request = Sdk_control.Request.get_server_info () in
  let response_data =
    send_control_request t ~request_id request
    |> Option.to_result ~none:"No response data from get_server_info request"
    |> Err.get_ok ~msg:""
  in
  let server_info =
    Jsont.Json.decode Sdk_control.Server_info.jsont response_data
    |> Err.get_ok' ~msg:"Failed to decode server info: "
  in
  Log.info (fun m ->
      m "Retrieved server info: %a"
        (Jsont.pp_value Sdk_control.Server_info.jsont ())
        server_info);
  Server_info.of_sdk_control server_info

module Advanced = struct
  let send_message t msg = send_message t msg

  let send_user_message t user_msg =
    let msg = Message.User user_msg in
    send_message t msg

  let send_raw t control =
    let json =
      Jsont.Json.encode Sdk_control.jsont control
      |> Err.get_ok ~msg:"Failed to encode control message: "
    in
    Log.info (fun m -> m "→ Raw control: %s" (json_to_string json));
    Transport.send t.transport json

  let send_json t json =
    Log.info (fun m -> m "→ Raw JSON: %s" (json_to_string json));
    Transport.send t.transport json

  let receive_raw t = handle_raw_messages t
end
