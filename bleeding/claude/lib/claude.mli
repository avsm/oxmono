(*---------------------------------------------------------------------------
  Copyright (c) 2025 Anil Madhavapeddy <anil@recoil.org>. All rights reserved.
  SPDX-License-Identifier: ISC
 ---------------------------------------------------------------------------*)

(** OCaml Eio library for Claude Code CLI.

    This library provides an interface to the Claude Code command-line interface
    using OCaml's Eio concurrency library. It wraps Claude CLI invocations with
    JSON streaming for asynchronous communication.

    {1 Overview}

    The Claude library enables you to:
    - Send messages to Claude and receive streaming responses
    - Control tool permissions and execution
    - Configure system prompts and model parameters
    - Handle content blocks including text, tool use, and thinking blocks
    - Manage sessions with proper resource cleanup

    {1 Architecture}

    The library is structured into two layers:

    {2 High-Level API}
    - {!Client}: High-level client interface for interacting with Claude
    - {!Response}: High-level response events from Claude
    - {!Handler}: Object-oriented response handler with sensible defaults
    - {!Options}: Configuration options for Claude sessions
    - {!Permissions}: Fine-grained permission system for tool usage
    - {!Hooks}: Fully typed hook callbacks for event interception

    {2 Domain Types}
    - {!Content_block}: Content blocks (text, tool use, tool results, thinking)
    - {!Message}: Messages exchanged with Claude (user, assistant, system,
      result)
    - {!Tool_input}: Opaque tool input with typed accessors
    - {!Server_info}: Server capabilities and metadata

    {2 Wire Format (Advanced)}
    - {!Proto}: Direct access to wire-format types and JSON codecs

    {1 Quick Start}

    {[
      open Eio.Std

      let () =
        Eio_main.run @@ fun env ->
        Switch.run @@ fun sw ->
        let client =
          Claude.Client.create ~sw ~process_mgr:(Eio.Stdenv.process_mgr env) ()
        in

        Claude.Client.query client "What is 2+2?";

        let handler =
          object
            inherit Claude.Handler.default
            method! on_text t = print_endline (Claude.Response.Text.content t)
          end
        in

        Claude.Client.run client ~handler
    ]}

    {1 Response Handling}

    The library provides two ways to handle responses:

    {2 Object-Oriented Handler (Recommended)}

    Subclass {!Handler.default} and override only the methods you need:

    {[
      let my_handler =
        object
          inherit Claude.Handler.default
          method! on_text t = print_endline (Claude.Response.Text.content t)

          method! on_tool_use t =
            Printf.printf "Tool: %s\n" (Claude.Response.Tool_use.name t)

          method! on_complete c =
            Printf.printf "Done! Cost: $%.4f\n"
              (Option.value ~default:0.0
                 (Claude.Response.Complete.total_cost_usd c))
        end
      in

      Claude.Client.run client ~handler:my_handler
    ]}

    {2 Functional Sequence}

    For more control, use {!Client.receive} to get a lazy sequence:

    {[
      Claude.Client.receive client
      |> Seq.iter (function
        | Claude.Response.Text t ->
            print_endline (Claude.Response.Text.content t)
        | Claude.Response.Complete c -> Printf.printf "Done!\n"
        | _ -> ())
    ]}

    {1 Tool Permissions}

    Control which tools Claude can use:

    {[
      let options =
        Claude.Options.default
        |> Claude.Options.with_allowed_tools [ "Read"; "Write"; "Bash" ]
        |> Claude.Options.with_permission_mode
             Claude.Permissions.Mode.Accept_edits
    ]}

    {2 Custom Permission Callbacks}

    Implement custom logic for tool approval:

    {[
      let my_callback ctx =
        if ctx.Claude.Permissions.tool_name = "Bash" then
          Claude.Permissions.Decision.deny ~message:"Bash not allowed"
            ~interrupt:false
        else Claude.Permissions.Decision.allow ()

      let options =
        Claude.Options.default
        |> Claude.Options.with_permission_callback my_callback
    ]}

    {1 Typed Hooks}

    Intercept and control tool execution with fully typed callbacks:

    {[
      let hooks =
        Claude.Hooks.empty
        |> Claude.Hooks.on_pre_tool_use ~pattern:"Bash" (fun input ->
            if
              String.is_prefix ~prefix:"rm"
                (input.tool_input
                |> Claude.Tool_input.get_string "command"
                |> Option.value ~default:"")
            then Claude.Hooks.PreToolUse.deny ~reason:"Dangerous command" ()
            else Claude.Hooks.PreToolUse.continue ())

      let options = Claude.Options.default |> Claude.Options.with_hooks hooks
    ]}

    {1 Error Handling}

    The library uses a structured exception type {!Err.E} for all errors:

    {[
      try Claude.Client.query client "Hello"
      with Claude.Err.E err ->
        Printf.eprintf "Error: %s\n" (Claude.Err.to_string err)
    ]}

    Error types include:
    - {!Err.Cli_not_found}: Claude CLI not found
    - {!Err.Process_error}: Process execution failure
    - {!Err.Protocol_error}: JSON/protocol parsing error
    - {!Err.Timeout}: Operation timed out
    - {!Err.Permission_denied}: Tool permission denied
    - {!Err.Hook_error}: Hook callback error

    {1 Logging}

    The library uses the Logs library for structured logging. Each module has
    its own log source allowing fine-grained control:

    {[
      Logs.Src.set_level Claude.Client.src (Some Logs.Debug);
      Logs.Src.set_level Claude.Transport.src (Some Logs.Info)
    ]} *)

(** {1 Core Modules} *)

module Err = Err
(** Error handling with structured exception type. *)

module Client = Client
(** High-level client interface for Claude interactions. *)

module Options = Options
(** Configuration options for Claude sessions. *)

module Response = Response
(** High-level response events from Claude. *)

module Handler = Handler
(** Object-oriented response handler with sensible defaults. *)

(** {1 Domain Types} *)

module Tool_input = Tool_input
(** Opaque tool input with typed accessors. *)

module Content_block = Content_block
(** Content blocks for messages (text, tool use, tool results, thinking). *)

module Message = Message
(** Messages exchanged with Claude (user, assistant, system, result). *)

module Permissions = Permissions
(** Permission system for tool invocations. *)

module Hooks = Hooks
(** Fully typed hook callbacks for event interception. *)

module Server_info = Server_info
(** Server capabilities and metadata. *)

module Model = Model
(** Claude AI model identifiers. *)

module Structured_output = Structured_output
(** Structured output configuration using JSON Schema. *)

(** {1 Custom Tools (MCP)}

    These modules enable custom tool definitions that run in-process via MCP
    (Model Context Protocol). Unlike built-in tools which Claude CLI handles
    internally, custom tools are executed by your application.

    {2 Example}

    {[
      let greet =
        Claude.Tool.create ~name:"greet" ~description:"Greet a user"
          ~input_schema:
            (Claude.Tool.schema_object
               [ ("name", Claude.Tool.schema_string) ]
               ~required:[ "name" ])
          ~handler:(fun args ->
            match Claude.Tool_input.get_string args "name" with
            | Some name -> Ok (Claude.Tool.text_result ("Hello, " ^ name ^ "!"))
            | None -> Error "Missing name")

      let server = Claude.Mcp_server.create ~name:"my-tools" ~tools:[ greet ] ()

      let options =
        Claude.Options.default
        |> Claude.Options.with_mcp_server ~name:"tools" server
        |> Claude.Options.with_allowed_tools [ "mcp__tools__greet" ]
    ]} *)

module Tool = Tool
(** Custom tool definitions for MCP servers. *)

module Mcp_server = Mcp_server
(** In-process MCP servers for custom tools. *)

(** {1 Infrastructure} *)

module Transport = Transport
(** Low-level transport layer for CLI communication. *)

(** {1 Wire Format (Advanced)}

    The {!Proto} module provides direct access to wire-format types and JSON
    codecs. Use this for advanced scenarios like custom transports or debugging.

    Most users should use the high-level types above instead. *)

module Proto = Proto
(** Wire-format types and JSON codecs. *)
