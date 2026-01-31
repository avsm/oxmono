# Claude OCaml SDK Architecture (v2)

This document describes the rearchitected OCaml SDK, aligned with the Python Claude Agent SDK while maintaining idiomatic OCaml/Eio patterns.

## Core Design Principles

### 1. MCP for Custom Tools (Python SDK Pattern)

The Python SDK's key insight: **built-in tools are handled by Claude CLI, custom tools via MCP**.

```
Claude CLI         SDK (OCaml Process)
    |                     |
    |--[tool_use Read]--->|  (CLI handles internally)
    |                     |
    |<--[tool_result]----|
    |                     |
    |--[mcp_request]----->|  (SDK handles via in-process MCP)
    |<--[mcp_response]----|
```

This eliminates the current problem where the OCaml SDK intercepts ALL tool_use events.

### 2. Hooks Intercept, Don't Execute

Hooks provide interception points for **observation and control**, not execution:
- PreToolUse: Allow/Deny/Modify before execution (by CLI or MCP)
- PostToolUse: Observe/Modify after execution
- Other lifecycle hooks remain the same

### 3. Two-Level API

Like Python, provide both simple and advanced interfaces:
- `Claude.query`: One-shot queries, simple async iterator
- `Claude.Client`: Full bidirectional, multi-turn, custom tools

---

## Module Structure

```
lib/
├── claude.ml              # Main module, re-exports public API
├── claude.mli
│
├── client.ml              # Bidirectional client
├── client.mli
│
├── tool.ml                # Custom tool definition
├── tool.mli
│
├── mcp_server.ml          # In-process MCP server
├── mcp_server.mli
│
├── hook.ml                # Hook types and matchers
├── hook.mli
│
├── options.ml             # Configuration
├── options.mli
│
├── message.ml             # Message types
├── message.mli
│
├── response.ml            # Response events
├── response.mli
│
├── model.ml               # Model identifiers
├── model.mli
│
├── permission_mode.ml     # Permission modes
├── permission_mode.mli
│
├── server_info.ml         # Server metadata
├── server_info.mli
│
├── err.ml                 # Error types
├── err.mli
│
└── internal/
    ├── process.ml         # CLI process management
    ├── protocol.ml        # JSON wire protocol
    └── mcp_handler.ml     # MCP message routing
```

---

## Core Types

### Tool Definition

```ocaml
(* tool.mli *)

(** Custom tool for MCP servers.

    Tools are functions that Claude can invoke. They run in-process
    within your OCaml application via MCP protocol.

    {[
      let greet = Tool.create
        ~name:"greet"
        ~description:"Greet a user by name"
        ~input_schema:(`O ["name", `String "string"])
        ~handler:(fun args ->
          match Jsont.find_string "name" args with
          | Some name -> Ok (`String (Printf.sprintf "Hello, %s!" name))
          | None -> Error "Missing 'name' parameter")
    ]} *)

type t

val create :
  name:string ->
  description:string ->
  input_schema:Jsont.json ->
  handler:(Jsont.json -> (Jsont.json, string) result) ->
  t
(** [create ~name ~description ~input_schema ~handler] creates a custom tool.

    @param name Unique tool identifier. Claude uses this in function calls.
    @param description Human-readable description for Claude.
    @param input_schema JSON Schema for input validation.
    @param handler Function that executes the tool and returns result or error. *)

val name : t -> string
val description : t -> string
val input_schema : t -> Jsont.json
val call : t -> Jsont.json -> (Jsont.json, string) result

(** {2 Async Tools}

    For tools that need Eio concurrency: *)

type async_t

val create_async :
  name:string ->
  description:string ->
  input_schema:Jsont.json ->
  handler:(sw:Eio.Switch.t -> Jsont.json -> (Jsont.json, string) result) ->
  async_t
(** Create a tool that runs under an Eio switch for async operations. *)
```

### MCP Server

```ocaml
(* mcp_server.mli *)

(** In-process MCP server.

    SDK MCP servers run directly in your OCaml application, eliminating
    subprocess overhead. They handle tools/list and tools/call requests.

    {[
      let server = Mcp_server.create
        ~name:"my-tools"
        ~tools:[greet_tool; calculate_tool]
        ()

      let options = Options.default
        |> Options.with_mcp_server ~name:"tools" server
        |> Options.with_allowed_tools ["mcp__tools__greet"]
    ]} *)

type t

val create :
  name:string ->
  ?version:string ->
  tools:Tool.t list ->
  unit ->
  t
(** [create ~name ?version ~tools ()] creates an in-process MCP server.

    @param name Server identifier. Tools are accessed as [mcp__<name>__<tool>].
    @param version Server version (default "1.0.0").
    @param tools List of tools this server provides. *)

val name : t -> string
val version : t -> string
val tools : t -> Tool.t list

(** {2 MCP Protocol Handling} *)

val handle_request :
  t ->
  method_:string ->
  params:Jsont.json ->
  (Jsont.json, string) result
(** [handle_request t ~method_ ~params] handles MCP JSONRPC requests.

    Supports:
    - [initialize]: Returns server capabilities
    - [tools/list]: Returns available tools
    - [tools/call]: Executes a tool *)
```

### Hooks

```ocaml
(* hook.mli *)

(** Hook callbacks for event interception.

    Hooks intercept events in the Claude agent loop. They can observe,
    allow, deny, or modify tool execution.

    {b Key difference from tool execution}: Hooks don't execute built-in
    tools - Claude CLI handles those. Hooks only intercept for control.

    {[
      let block_rm = Hook.PreToolUse.handler (fun input ->
        if input.tool_name = "Bash" then
          match Tool_input.get_string input.tool_input "command" with
          | Some cmd when String.is_substring cmd ~substring:"rm -rf" ->
              Hook.PreToolUse.deny ~reason:"Dangerous command"
          | _ -> Hook.PreToolUse.allow ()
        else Hook.PreToolUse.allow ())

      let hooks = Hook.Config.empty
        |> Hook.Config.on_pre_tool_use ~pattern:"Bash" block_rm
    ]} *)

type context = {
  session_id : string option;
  transcript_path : string option;
}
(** Context provided to all hooks. *)

(** {1 PreToolUse Hook}

    Fires before any tool execution (built-in or MCP). *)
module PreToolUse : sig
  type input = {
    tool_name : string;
    tool_input : Tool_input.t;
    context : context;
  }

  type decision =
    | Allow
    | Deny of { reason : string }
    | Modify of { input : Tool_input.t }
    | Ask of { reason : string option }

  val allow : ?updated_input:Tool_input.t -> unit -> decision
  val deny : reason:string -> decision
  val ask : ?reason:string -> unit -> decision
  val modify : input:Tool_input.t -> decision

  type handler = input -> decision

  val handler : (input -> decision) -> handler
end

(** {1 PostToolUse Hook}

    Fires after tool execution completes. *)
module PostToolUse : sig
  type input = {
    tool_name : string;
    tool_input : Tool_input.t;
    tool_result : Jsont.json;
    context : context;
  }

  type decision =
    | Continue
    | Block of { reason : string option }
    | AddContext of { context : string }

  val continue : unit -> decision
  val block : ?reason:string -> unit -> decision
  val add_context : string -> decision

  type handler = input -> decision
end

(** {1 Other Hooks} *)
module UserPromptSubmit : sig
  type input = { prompt : string; context : context }
  type decision = Continue | Block of { reason : string option }
  type handler = input -> decision
end

module Stop : sig
  type input = { stop_hook_active : bool; context : context }
  type decision = Continue | Block of { reason : string option }
  type handler = input -> decision
end

module PreCompact : sig
  type input = { context : context }
  type handler = input -> unit  (* Notification only *)
end

(** {1 Hook Configuration} *)
module Config : sig
  type t

  val empty : t

  val on_pre_tool_use : ?pattern:string -> PreToolUse.handler -> t -> t
  val on_post_tool_use : ?pattern:string -> PostToolUse.handler -> t -> t
  val on_user_prompt_submit : UserPromptSubmit.handler -> t -> t
  val on_stop : Stop.handler -> t -> t
  val on_pre_compact : PreCompact.handler -> t -> t
end
```

### Options

```ocaml
(* options.mli *)

(** Configuration options for Claude sessions.

    {[
      let options = Options.default
        |> Options.with_model Model.opus
        |> Options.with_mcp_server ~name:"tools" my_server
        |> Options.with_allowed_tools ["mcp__tools__greet"; "Read"; "Write"]
        |> Options.with_hooks my_hooks
        |> Options.with_max_budget_usd 1.0
    ]} *)

type t

val default : t

(** {1 Builder Pattern} *)

val with_system_prompt : string -> t -> t
val with_append_system_prompt : string -> t -> t
val with_model : Model.t -> t -> t
val with_fallback_model : Model.t -> t -> t
val with_max_turns : int -> t -> t
val with_max_thinking_tokens : int -> t -> t
val with_max_budget_usd : float -> t -> t

val with_allowed_tools : string list -> t -> t
val with_disallowed_tools : string list -> t -> t
val with_permission_mode : Permission_mode.t -> t -> t

val with_cwd : [> Eio.Fs.dir_ty ] Eio.Path.t -> t -> t
val with_env : (string * string) list -> t -> t

val with_mcp_server : name:string -> Mcp_server.t -> t -> t
(** Add an in-process MCP server. Multiple servers can be added. *)

val with_hooks : Hook.Config.t -> t -> t

val with_no_settings : t -> t
val with_cli_path : string -> t -> t

(** {1 Accessors} *)

val system_prompt : t -> string option
val model : t -> Model.t option
val mcp_servers : t -> (string * Mcp_server.t) list
val hooks : t -> Hook.Config.t option
(* ... other accessors ... *)
```

### Permission Mode

```ocaml
(* permission_mode.mli *)

(** Permission modes for tool authorization. *)

type t =
  | Default           (** Prompt for all permissions *)
  | Accept_edits      (** Auto-accept file edits *)
  | Plan              (** Planning mode - restricted execution *)
  | Bypass            (** Skip all permission checks - DANGEROUS *)

val to_string : t -> string
val of_string : string -> t option
```

### Model

```ocaml
(* model.mli *)

(** Claude AI model identifiers. *)

type t =
  | Sonnet_4_5
  | Opus_4
  | Haiku_4
  | Custom of string

val sonnet : t
val opus : t
val haiku : t

val to_string : t -> string
val of_string : string -> t
```

### Messages and Responses

```ocaml
(* message.mli *)

(** Messages exchanged with Claude. *)

module Content_block : sig
  type t =
    | Text of { text : string }
    | Tool_use of { id : string; name : string; input : Jsont.json }
    | Tool_result of { tool_use_id : string; content : Jsont.json; is_error : bool }
    | Thinking of { text : string }
end

module User : sig
  type t
  val of_string : string -> t
  val of_blocks : Content_block.t list -> t
  val of_tool_results : (string * Jsont.json * bool) list -> t
end

module Assistant : sig
  type t
  val content : t -> Content_block.t list
  val text : t -> string  (* Concatenated text blocks *)
end

type t =
  | User of User.t
  | Assistant of Assistant.t
  | System of { session_id : string option }
  | Result of { text : string }


(* response.mli *)

(** Response events from Claude. *)

module Text : sig
  type t
  val content : t -> string
end

module Tool_use : sig
  type t
  val id : t -> string
  val name : t -> string
  val input : t -> Jsont.json
end

module Thinking : sig
  type t
  val content : t -> string
end

module Complete : sig
  type t
  val total_cost_usd : t -> float option
  val input_tokens : t -> int
  val output_tokens : t -> int
  val duration_ms : t -> int option
end

module Init : sig
  type t
  val session_id : t -> string option
end

module Error : sig
  type t
  val message : t -> string
  val code : t -> string option
end

type t =
  | Text of Text.t
  | Tool_use of Tool_use.t
  | Thinking of Thinking.t
  | Init of Init.t
  | Error of Error.t
  | Complete of Complete.t
```

---

## Client Interface

```ocaml
(* client.mli *)

(** Bidirectional client for Claude interactions.

    The client handles:
    - Message streaming via Eio
    - MCP routing for custom tools
    - Hook callbacks
    - Permission requests
    - Dynamic control (model/permission changes)

    {2 Basic Usage}

    {[
      Eio.Switch.run @@ fun sw ->
      let client = Client.create ~sw ~process_mgr ~clock () in

      Client.query client "What is 2+2?";

      Client.receive client |> Seq.iter (function
        | Response.Text t -> print_endline (Response.Text.content t)
        | Response.Complete c ->
            Printf.printf "Cost: $%.4f\n"
              (Option.value ~default:0.0 (Response.Complete.total_cost_usd c))
        | _ -> ())
    ]}

    {2 With Custom Tools}

    {[
      let greet = Tool.create
        ~name:"greet"
        ~description:"Greet someone"
        ~input_schema:(`O ["name", `String "string"])
        ~handler:(fun args -> Ok (`String "Hello!"))

      let server = Mcp_server.create ~name:"tools" ~tools:[greet] ()

      let options = Options.default
        |> Options.with_mcp_server ~name:"tools" server
        |> Options.with_allowed_tools ["mcp__tools__greet"]

      let client = Client.create ~sw ~process_mgr ~clock ~options ()
    ]} *)

type t

val create :
  sw:Eio.Switch.t ->
  process_mgr:_ Eio.Process.mgr ->
  clock:float Eio.Time.clock_ty Eio.Resource.t ->
  ?options:Options.t ->
  unit ->
  t
(** Create a new Claude client. *)

(** {1 Querying} *)

val query : t -> string -> unit
(** [query t prompt] sends a text prompt to Claude. *)

val send_message : t -> Message.User.t -> unit
(** [send_message t msg] sends a user message (can include tool results). *)

(** {1 Receiving Responses} *)

val receive : t -> Response.t Seq.t
(** [receive t] returns a lazy sequence of response events.

    Built-in tool executions happen internally (by Claude CLI).
    Custom tool calls are routed to MCP servers automatically.
    You only see the responses. *)

val receive_all : t -> Response.t list
(** [receive_all t] collects all responses into a list. *)

(** {1 Dynamic Control} *)

val set_model : t -> Model.t -> unit
val set_permission_mode : t -> Permission_mode.t -> unit
val get_server_info : t -> Server_info.t
val interrupt : t -> unit

val session_id : t -> string option
(** Get session ID if available. *)
```

---

## Simple Query API

```ocaml
(* claude.mli *)

(** OCaml SDK for Claude Code CLI.

    {1 Quick Start}

    {[
      open Eio.Std

      let () = Eio_main.run @@ fun env ->
        Switch.run @@ fun sw ->
        let process_mgr = Eio.Stdenv.process_mgr env in
        let clock = Eio.Stdenv.clock env in

        (* Simple one-shot query *)
        let response = Claude.query_text ~sw ~process_mgr ~clock
          ~prompt:"What is 2+2?" () in
        print_endline response
    ]}

    {1 With Custom Tools}

    {[
      let greet = Claude.Tool.create
        ~name:"greet"
        ~description:"Greet a user"
        ~input_schema:(`O ["name", `String "string"])
        ~handler:(fun args ->
          Ok (`String (Printf.sprintf "Hello, %s!"
            (Jsont.get_string_exn "name" args))))

      let server = Claude.Mcp_server.create
        ~name:"my-tools"
        ~tools:[greet]
        ()

      let options = Claude.Options.default
        |> Claude.Options.with_mcp_server ~name:"tools" server
        |> Claude.Options.with_allowed_tools ["mcp__tools__greet"]

      let client = Claude.Client.create ~sw ~process_mgr ~clock ~options ()
    ]} *)

(** {1 Simple Query Functions} *)

val query :
  sw:Eio.Switch.t ->
  process_mgr:_ Eio.Process.mgr ->
  clock:float Eio.Time.clock_ty Eio.Resource.t ->
  ?options:Options.t ->
  prompt:string ->
  unit ->
  Response.t Seq.t
(** [query ~sw ~process_mgr ~clock ?options ~prompt ()] performs a one-shot query.

    Returns a lazy sequence of response events. The client is created and
    cleaned up automatically. *)

val query_text :
  sw:Eio.Switch.t ->
  process_mgr:_ Eio.Process.mgr ->
  clock:float Eio.Time.clock_ty Eio.Resource.t ->
  ?options:Options.t ->
  prompt:string ->
  unit ->
  string
(** [query_text ...] is like [query] but returns concatenated text response. *)

(** {1 Core Modules} *)

module Client = Client
module Options = Options
module Tool = Tool
module Mcp_server = Mcp_server
module Hook = Hook
module Message = Message
module Response = Response
module Model = Model
module Permission_mode = Permission_mode
module Server_info = Server_info
module Err = Err
```

---

## Error Handling

```ocaml
(* err.mli *)

(** Structured error types. *)

type t =
  | Cli_not_found of string
  | Process_error of { exit_code : int; message : string }
  | Protocol_error of { message : string; raw : string option }
  | Timeout of { operation : string }
  | Permission_denied of { tool : string; reason : string }
  | Hook_error of { hook : string; error : string }
  | Mcp_error of { server : string; method_ : string; error : string }

exception E of t

val to_string : t -> string
val raise_cli_not_found : string -> 'a
val raise_process_error : exit_code:int -> message:string -> 'a
val raise_protocol_error : message:string -> ?raw:string -> unit -> 'a
val raise_timeout : operation:string -> 'a
```

---

## Internal Architecture

### Process Management

The internal process module spawns Claude CLI and manages bidirectional communication:

```ocaml
(* internal/process.ml *)

type t = {
  proc : Eio.Process.t;
  stdin : Eio.Flow.sink;
  stdout : Eio.Flow.source;
  stderr : Eio.Flow.source;
}

val spawn :
  sw:Eio.Switch.t ->
  process_mgr:_ Eio.Process.mgr ->
  ?cli_path:string ->
  ?cwd:Eio.Fs.dir_ty Eio.Path.t ->
  args:string list ->
  unit ->
  t

val send_line : t -> string -> unit
val read_line : t -> string option
val close : t -> unit
```

### Protocol Handling

The protocol module handles JSON wire format:

```ocaml
(* internal/protocol.ml *)

type incoming =
  | Message of Message.t
  | Control_request of {
      request_id : string;
      request : control_request;
    }
  | Control_response of {
      request_id : string;
      response : control_response;
    }

and control_request =
  | Permission_request of { tool_name : string; input : Jsont.json }
  | Hook_callback of { callback_id : string; input : Jsont.json }
  | Mcp_request of { server : string; message : Jsont.json }

and control_response =
  | Success of { response : Jsont.json option }
  | Error of { message : string }

type outgoing =
  | User_message of Message.User.t
  | Control_request of { request : Request.t }
  | Control_response of { request_id : string; response : Response.t }

val decode : string -> incoming
val encode : outgoing -> string
```

### MCP Handler

Routes MCP requests to appropriate in-process servers:

```ocaml
(* internal/mcp_handler.ml *)

type t

val create : servers:(string * Mcp_server.t) list -> t

val handle_request :
  t ->
  server:string ->
  message:Jsont.json ->
  Jsont.json
(** Handle MCP JSONRPC request and return response. *)
```

---

## Message Flow Diagrams

### Built-in Tool Execution (passthrough)

```
User          SDK Client        Claude CLI       Claude
  |                |                |              |
  |--query()------>|                |              |
  |                |--UserMsg------>|              |
  |                |                |--API call--->|
  |                |                |<--tool_use---|
  |                |                |  (Read file) |
  |                |                |              |
  |                |                | [CLI executes Read internally]
  |                |                |              |
  |                |                |--tool_result>|
  |                |                |<--text-------|
  |                |<--Response-----|              |
  |<--Response.Text|                |              |
```

### Custom MCP Tool Execution

```
User          SDK Client        Claude CLI       Claude
  |                |                |              |
  |--query()------>|                |              |
  |                |--UserMsg------>|              |
  |                |                |--API call--->|
  |                |                |<--tool_use---|
  |                |                | (mcp__x__y)  |
  |                |<--mcp_request--|              |
  |                |                |              |
  |                | [SDK routes to Mcp_server]    |
  |                |                |              |
  |                |--mcp_response->|              |
  |                |                |--tool_result>|
  |                |                |<--text-------|
  |                |<--Response-----|              |
  |<--Response.Text|                |              |
```

### Hook Interception

```
User          SDK Client        Claude CLI       Claude
  |                |                |              |
  |--query()------>|                |              |
  |                |--UserMsg------>|              |
  |                |                |--API call--->|
  |                |                |<--tool_use---|
  |                |                |  (Bash)      |
  |                |<--hook_callback|              |
  |                | [PreToolUse]   |              |
  |                |                |              |
  |                | [SDK runs hook, returns Deny] |
  |                |                |              |
  |                |--hook_response>| (denied)     |
  |                |                |--error msg-->|
  |                |                |<--text-------|
  |                |<--Response-----|              |
  |<--Response.Text|                |              |
```

---

## Migration from Current SDK

### Key Changes

1. **Remove explicit tool execution**
   - Current: SDK receives tool_use, executes tool, returns result
   - New: Built-in tools handled by CLI; only MCP tools executed by SDK

2. **Add MCP server support**
   - New: `Tool.t`, `Mcp_server.t` for custom tool definition

3. **Simplify hooks**
   - Current: Hooks can have complex tool execution logic
   - New: Hooks intercept only; execution is separate

4. **Clean up Handler module**
   - Current: Object-oriented handler class
   - New: Functional response handling via `Seq.t`

### Compatibility Notes

- `Options.with_hooks` remains similar
- `Client.query/receive` API stays the same
- New: `Options.with_mcp_server` for custom tools
- Removed: Direct tool execution callbacks

---

## Example: Complete Application

```ocaml
open Eio.Std

(* Define custom tools *)
let calculator_add = Claude.Tool.create
  ~name:"add"
  ~description:"Add two numbers"
  ~input_schema:(`O [
    "a", `O ["type", `String "number"];
    "b", `O ["type", `String "number"];
  ])
  ~handler:(fun args ->
    match Jsont.(find_float "a" args, find_float "b" args) with
    | Some a, Some b -> Ok (`String (Printf.sprintf "%.2f" (a +. b)))
    | _ -> Error "Missing a or b parameter")

let calculator_multiply = Claude.Tool.create
  ~name:"multiply"
  ~description:"Multiply two numbers"
  ~input_schema:(`O [
    "a", `O ["type", `String "number"];
    "b", `O ["type", `String "number"];
  ])
  ~handler:(fun args ->
    match Jsont.(find_float "a" args, find_float "b" args) with
    | Some a, Some b -> Ok (`String (Printf.sprintf "%.2f" (a *. b)))
    | _ -> Error "Missing a or b parameter")

(* Create MCP server *)
let calculator_server = Claude.Mcp_server.create
  ~name:"calculator"
  ~version:"1.0.0"
  ~tools:[calculator_add; calculator_multiply]
  ()

(* Define hook to block dangerous commands *)
let block_dangerous_bash input =
  if input.Claude.Hook.PreToolUse.tool_name = "Bash" then
    match Claude.Tool_input.get_string input.tool_input "command" with
    | Some cmd when String.is_substring cmd ~substring:"rm -rf" ->
        Claude.Hook.PreToolUse.deny ~reason:"Dangerous command blocked"
    | _ -> Claude.Hook.PreToolUse.allow ()
  else Claude.Hook.PreToolUse.allow ()

let hooks = Claude.Hook.Config.empty
  |> Claude.Hook.Config.on_pre_tool_use ~pattern:"Bash" block_dangerous_bash

(* Main application *)
let () = Eio_main.run @@ fun env ->
  Switch.run @@ fun sw ->
  let process_mgr = Eio.Stdenv.process_mgr env in
  let clock = Eio.Stdenv.clock env in

  let options = Claude.Options.default
    |> Claude.Options.with_model Claude.Model.opus
    |> Claude.Options.with_mcp_server ~name:"calc" calculator_server
    |> Claude.Options.with_allowed_tools [
         "mcp__calc__add";
         "mcp__calc__multiply";
         "Read";
         "Bash";
       ]
    |> Claude.Options.with_hooks hooks
    |> Claude.Options.with_max_budget_usd 0.50
  in

  let client = Claude.Client.create ~sw ~process_mgr ~clock ~options () in

  (* Multi-turn conversation *)
  Claude.Client.query client "What is 23 + 45?";
  Claude.Client.receive client |> Seq.iter (function
    | Claude.Response.Text t ->
        Printf.printf "Claude: %s\n" (Claude.Response.Text.content t)
    | Claude.Response.Tool_use tu ->
        Printf.printf "[Using tool: %s]\n" (Claude.Response.Tool_use.name tu)
    | Claude.Response.Complete c ->
        Printf.printf "[Cost: $%.4f]\n"
          (Option.value ~default:0.0 (Claude.Response.Complete.total_cost_usd c))
    | _ -> ());

  Claude.Client.query client "Now multiply that result by 2";
  Claude.Client.receive_all client |> ignore
```

---

## Implementation Priority

1. **Phase 1: Core Types**
   - `Tool.t`, `Mcp_server.t`
   - Updated `Options.t` with MCP support
   - `Permission_mode.t`, `Model.t`

2. **Phase 2: Internal MCP Routing**
   - `internal/mcp_handler.ml`
   - Protocol updates for MCP messages
   - Remove built-in tool execution from client

3. **Phase 3: Hook Simplification**
   - Update `Hook` module to intercept-only model
   - Remove tool execution from hook callbacks

4. **Phase 4: API Polish**
   - Simple `query` function
   - Documentation and examples
   - Error handling improvements

5. **Phase 5: Testing & Migration**
   - Comprehensive tests
   - Migration guide
   - Deprecation of old patterns
