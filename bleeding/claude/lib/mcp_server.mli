(*---------------------------------------------------------------------------
  Copyright (c) 2025 Anil Madhavapeddy <anil@recoil.org>. All rights reserved.
  SPDX-License-Identifier: ISC
 ---------------------------------------------------------------------------*)

(** In-process MCP server for custom tools.

    SDK MCP servers run directly in your OCaml application, eliminating
    subprocess overhead. They handle MCP protocol requests (tools/list,
    tools/call) and route them to your tool handlers.

    {2 Basic Usage}

    {[
      let greet =
        Tool.create ~name:"greet" ~description:"Greet a user"
          ~input_schema:
            (Tool.schema_object
               [ ("name", Tool.schema_string) ]
               ~required:[ "name" ])
          ~handler:(fun args ->
            match Tool_input.get_string args "name" with
            | Some name ->
                Ok (Tool.text_result (Printf.sprintf "Hello, %s!" name))
            | None -> Error "Missing name")

      let server = Mcp_server.create ~name:"my-tools" ~tools:[ greet ] ()

      let options =
        Options.default
        |> Options.with_mcp_server ~name:"tools" server
        |> Options.with_allowed_tools [ "mcp__tools__greet" ]
    ]}

    {2 Tool Naming}

    When you register an MCP server with name "foo" containing a tool "bar", the
    full tool name becomes [mcp__foo__bar]. This is how Claude CLI routes MCP
    tool calls.

    {2 Protocol}

    The server handles these MCP JSONRPC methods:
    - [initialize]: Returns server capabilities
    - [tools/list]: Returns available tools with schemas
    - [tools/call]: Executes a tool and returns result *)

type t
(** Abstract type for MCP servers. *)

val create : name:string -> ?version:string -> tools:Tool.t list -> unit -> t
(** [create ~name ?version ~tools ()] creates an in-process MCP server.

    @param name Server identifier. Used in tool naming: [mcp__<name>__<tool>].
    @param version Server version string (default "1.0.0").
    @param tools List of tools this server provides. *)

val name : t -> string
(** [name t] returns the server name. *)

val version : t -> string
(** [version t] returns the server version. *)

val tools : t -> Tool.t list
(** [tools t] returns the list of registered tools. *)

(** {1 MCP Protocol Handling} *)

val handle_request :
  t -> method_:string -> params:Jsont.json -> id:Jsont.json -> Jsont.json
(** [handle_request t ~method_ ~params ~id] handles an MCP JSONRPC request.

    Returns a JSONRPC response object with the given [id].

    Supported methods:
    - ["initialize"]: Returns server capabilities (tools only)
    - ["tools/list"]: Returns list of available tools
    - ["tools/call"]: Executes tool, params must have "name" and "arguments"

    Unknown methods return a JSONRPC error response. *)

val handle_json_message : t -> Jsont.json -> Jsont.json
(** [handle_json_message t msg] handles a complete JSONRPC message.

    Extracts method, params, and id from the message and delegates to
    {!handle_request}. *)
