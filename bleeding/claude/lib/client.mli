(*---------------------------------------------------------------------------
  Copyright (c) 2025 Anil Madhavapeddy <anil@recoil.org>. All rights reserved.
  SPDX-License-Identifier: ISC
 ---------------------------------------------------------------------------*)

(** Client interface for interacting with Claude.

    This module provides the high-level client API for sending messages to
    Claude and receiving responses. It handles the bidirectional streaming
    protocol, permission callbacks, and hooks.

    {2 Basic Usage}

    {[
      Eio.Switch.run @@ fun sw ->
      let client = Client.create ~sw ~process_mgr ~clock () in
      Client.query client "What is 2+2?";

      let messages = Client.receive_all client in
      List.iter
        (function
          | Message.Assistant msg ->
              Printf.printf "Claude: %s\n" (Message.Assistant.text msg)
          | _ -> ())
        messages
    ]}

    {2 Features}

    - {b Message Streaming}: Messages are streamed lazily via {!Seq.t}
    - {b Permission Control}: Custom permission callbacks for tool usage
    - {b Hooks}: Intercept and modify tool execution
    - {b Dynamic Control}: Change settings mid-conversation
    - {b Resource Management}: Automatic cleanup via Eio switches

    {2 Message Flow}

    1. Create a client with {!create} 2. Send messages with {!query} or
    {!Advanced.send_message} 3. Receive responses with {!receive} or
    {!receive_all} 4. Continue multi-turn conversations by sending more messages
    5. Client automatically cleans up when the switch exits

    {2 Advanced Features}

    - Permission discovery mode for understanding required permissions
    - Mid-conversation model switching and permission mode changes
    - Server capability introspection *)

val src : Logs.Src.t
(** The log source for client operations *)

type t
(** The type of Claude clients. *)

val session_id : t -> string option
(** [session_id t] returns the session ID if one has been received from Claude.
    The session ID is provided in system init messages and uniquely identifies
    the current conversation session. *)

val create :
  ?options:Options.t ->
  sw:Eio.Switch.t ->
  process_mgr:_ Eio.Process.mgr ->
  clock:float Eio.Time.clock_ty Eio.Resource.t ->
  unit ->
  t
(** [create ?options ~sw ~process_mgr ~clock ()] creates a new Claude client.

    @param options Configuration options (defaults to {!Options.default})
    @param sw Eio switch for resource management
    @param process_mgr Eio process manager for spawning the Claude CLI
    @param clock Eio clock for time operations *)

(** {1 Simple Query Interface} *)

val query : t -> string -> unit
(** [query t prompt] sends a text message to Claude.

    This is a convenience function for simple string messages. For more complex
    messages with tool results or multiple content blocks, use
    {!Advanced.send_message} instead. *)

val respond_to_tool :
  t ->
  tool_use_id:string ->
  content:Jsont.json ->
  ?is_error:bool ->
  unit ->
  unit
(** [respond_to_tool t ~tool_use_id ~content ?is_error ()] responds to a tool
    use request.

    {b Duplicate protection:} If the same [tool_use_id] has already been
    responded to, this call is silently skipped with a warning log. This
    prevents API errors from duplicate tool responses.

    @param tool_use_id The ID from the {!Response.Tool_use.t} event
    @param content
      The result content (can be a string or array of content blocks)
    @param is_error Whether this is an error response (default: false) *)

val respond_to_tools : t -> (string * Jsont.json * bool option) list -> unit
(** [respond_to_tools t responses] responds to multiple tool use requests at
    once.

    {b Duplicate protection:} Any [tool_use_id] that has already been responded
    to is filtered out with a warning log.

    Each tuple is [(tool_use_id, content, is_error option)] where content can be
    a string or array of content blocks.

    Example:
    {[
      Client.respond_to_tools client
        [
          ("tool_use_123", Jsont.string "Success", None);
          ("tool_use_456", Jsont.string "Error occurred", Some true);
        ]
    ]} *)

val clear_tool_response_tracking : t -> unit
(** [clear_tool_response_tracking t] clears the internal tracking of which
    tool_use_ids have been responded to.

    This is useful when starting a new conversation or turn where you want to
    allow responses to previously-seen tool IDs. Normally this is not needed as
    tool IDs are unique per conversation turn. *)

(** {1 Response Handling} *)

val run : t -> handler:#Handler.handler -> unit
(** [run t ~handler] processes all responses using the given handler.

    This is the recommended way to handle responses in an event-driven style.
    The handler's methods will be called for each response event as it arrives.

    Example:
    {[
      let my_handler =
        object
          inherit Claude.Handler.default
          method! on_text t = print_endline (Response.Text.content t)

          method! on_complete c =
            Printf.printf "Cost: $%.4f\n"
              (Option.value ~default:0.0 (Response.Complete.total_cost_usd c))
        end
      in
      Client.query client "Hello";
      Client.run client ~handler:my_handler
    ]} *)

val receive : t -> Response.t Seq.t
(** [receive t] returns a lazy sequence of responses from Claude.

    The sequence yields response events as they arrive from Claude, including:
    - {!constructor:Response.Text} - Text content from assistant
    - {!constructor:Response.Tool_use} - Tool invocation requests
    - {!constructor:Response.Thinking} - Internal reasoning
    - {!constructor:Response.Init} - Session initialization
    - {!constructor:Response.Error} - Error events
    - {!constructor:Response.Complete} - Final result with usage statistics

    Control messages (permission requests, hook callbacks) are handled
    internally and not yielded to the sequence.

    For simple cases, prefer {!run} with a handler instead. *)

val receive_all : t -> Response.t list
(** [receive_all t] collects all responses into a list.

    This is a convenience function that consumes the {!receive} sequence. Use
    this when you want to process all responses at once rather than streaming
    them.

    For most cases, prefer {!run} with a handler instead. *)

val interrupt : t -> unit
(** [interrupt t] sends an interrupt signal to stop Claude's execution. *)

(** {1 Dynamic Control}

    These methods allow you to change Claude's behavior mid-conversation without
    recreating the client. This is useful for:

    - Adjusting permission strictness based on user feedback
    - Switching to faster/cheaper models for simple tasks
    - Adapting to changing requirements during long conversations
    - Introspecting server capabilities

    {2 Example: Adaptive Permission Control}

    {[
      (* Start with strict permissions *)
      let client = Client.create ~sw ~process_mgr ~clock
        ~options:(Options.default
                  |> Options.with_permission_mode Permissions.Mode.Default) ()
      in

      Client.query client "Analyze this code";
      let _ = Client.receive_all client in

      (* User approves, switch to auto-accept edits *)
      Client.set_permission_mode client Permissions.Mode.Accept_edits;

      Client.query client "Now refactor it";
      let _ = Client.receive_all client in
    ]}

    {2 Example: Model Switching for Efficiency}

    {[
      (* Use powerful model for complex analysis *)
      let client = Client.create ~sw ~process_mgr ~clock
        ~options:(Options.default |> Options.with_model "claude-sonnet-4-5") ()
      in

      Client.query client "Design a new architecture for this system";
      let _ = Client.receive_all client in

      (* Switch to faster model for simple tasks *)
      Client.set_model client "claude-haiku-4";

      Client.query client "Now write a README";
      let _ = Client.receive_all client in
    ]}

    {2 Example: Server Introspection}

    {[
      let info = Client.get_server_info client in
      Printf.printf "Claude CLI version: %s\n"
        (Sdk_control.Server_info.version info);
      Printf.printf "Capabilities: %s\n"
        (String.concat ", " (Sdk_control.Server_info.capabilities info))
    ]} *)

val set_permission_mode : t -> Permissions.Mode.t -> unit
(** [set_permission_mode t mode] changes the permission mode mid-conversation.

    This allows switching between permission modes without recreating the
    client:
    - {!Permissions.Mode.Default} - Prompt for all permissions
    - {!Permissions.Mode.Accept_edits} - Auto-accept file edits
    - {!Permissions.Mode.Plan} - Planning mode with restricted execution
    - {!Permissions.Mode.Bypass_permissions} - Skip all permission checks

    @raise Failure if the server returns an error *)

val set_model : t -> Model.t -> unit
(** [set_model t model] switches to a different AI model mid-conversation.

    Common models:
    - [`Sonnet_4_5] - Most capable, balanced performance
    - [`Opus_4] - Maximum capability for complex tasks
    - [`Haiku_4] - Fast and cost-effective

    @raise Failure if the model is invalid or unavailable *)

val get_server_info : t -> Server_info.t
(** [get_server_info t] retrieves server capabilities and metadata.

    Returns information about:
    - Server version string
    - Available capabilities
    - Supported commands
    - Available output styles

    Useful for feature detection and debugging.

    @raise Failure if the server returns an error *)

(** {1 Permission Discovery} *)

val enable_permission_discovery : t -> unit
(** [enable_permission_discovery t] enables permission discovery mode.

    In discovery mode, all tool usage is logged but allowed. Use
    {!discovered_permissions} to retrieve the list of permissions that were
    requested during execution.

    This is useful for understanding what permissions your prompt requires. *)

val discovered_permissions : t -> Permissions.Rule.t list
(** [discovered_permissions t] returns permissions discovered during execution.

    Only useful after enabling {!enable_permission_discovery}. *)

(** {1 Advanced Interface}

    Low-level access to the protocol for advanced use cases. *)

module Advanced : sig
  val send_message : t -> Message.t -> unit
  (** [send_message t msg] sends a message to Claude.

      Supports all message types including user messages with tool results. *)

  val send_user_message : t -> Message.User.t -> unit
  (** [send_user_message t msg] sends a user message to Claude. *)

  val send_raw : t -> Sdk_control.t -> unit
  (** [send_raw t control] sends a raw SDK control message.

      This is for advanced use cases that need direct control protocol access.
  *)

  val send_json : t -> Jsont.json -> unit
  (** [send_json t json] sends raw JSON to Claude.

      This is the lowest-level send operation. Use with caution. *)

  val receive_raw : t -> Incoming.t Seq.t
  (** [receive_raw t] returns a lazy sequence of raw incoming messages.

      This includes all message types before Response conversion:
      - {!Proto.Incoming.t.constructor-Message} - Regular messages
      - {!Proto.Incoming.t.constructor-Control_response} - Control responses
        (normally handled internally)
      - {!Proto.Incoming.t.constructor-Control_request} - Control requests
        (normally handled internally)

      Most users should use {!receive} or {!run} instead. *)
end
