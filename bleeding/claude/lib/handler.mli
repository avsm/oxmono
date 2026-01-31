(*---------------------------------------------------------------------------
  Copyright (c) 2025 Anil Madhavapeddy <anil@recoil.org>. All rights reserved.
  SPDX-License-Identifier: ISC
 ---------------------------------------------------------------------------*)

(** Object-oriented response handler with sensible defaults.

    This module provides an object-oriented interface for handling response
    events from Claude. It offers both a concrete default implementation (where
    all methods do nothing) and an abstract base class (where all methods must
    be implemented).

    {1 Usage}

    The simplest approach is to inherit from {!default} and override only the
    methods you care about:

    {[
      let my_handler =
        object
          inherit Claude.Handler.default
          method! on_text t = print_endline (Response.Text.content t)

          method! on_complete c =
            Printf.printf "Done! Cost: $%.4f\n"
              (Option.value ~default:0.0 (Response.Complete.total_cost_usd c))
        end
    ]}

    For compile-time guarantees that all events are handled, inherit from
    {!abstract}:

    {[
      let complete_handler = object
        inherit Claude.Handler.abstract
        method on_text t = (* must implement *)
        method on_tool_use t = (* must implement *)
        method on_tool_result t = (* must implement *)
        method on_thinking t = (* must implement *)
        method on_init t = (* must implement *)
        method on_error t = (* must implement *)
        method on_complete t = (* must implement *)
      end
    ]} *)

(** {1 Handler Interface} *)

(** The handler interface for processing response events.

    Each method corresponds to a variant of {!Response.t}. Handlers can be
    passed to {!Client.run} to process responses in an event-driven style. *)
class type handler = object
  method on_text : Response.Text.t -> unit
  (** [on_text t] is called when text content is received from the assistant. *)

  method on_tool_use : Response.Tool_use.t -> unit
  (** [on_tool_use t] is called when the assistant requests a tool invocation.
      The caller is responsible for responding with {!Client.respond_to_tool}.
  *)

  method on_tool_result : Content_block.Tool_result.t -> unit
  (** [on_tool_result t] is called when a tool result is observed in the message
      stream. This is typically an echo of what was sent to Claude. *)

  method on_thinking : Response.Thinking.t -> unit
  (** [on_thinking t] is called when internal reasoning content is received. *)

  method on_init : Response.Init.t -> unit
  (** [on_init t] is called when the session is initialized. This provides
      session metadata like session_id and model. *)

  method on_error : Response.Error.t -> unit
  (** [on_error t] is called when an error occurs. Errors can come from the
      system (e.g., CLI errors) or from the assistant (e.g., rate limits). *)

  method on_complete : Response.Complete.t -> unit
  (** [on_complete t] is called when the conversation completes. This provides
      final metrics like duration, cost, and token usage. *)
end

(** {1 Concrete Implementations} *)

class default : handler
(** Default handler that does nothing for all events.

    This is the recommended base class for most use cases. Override only the
    methods you need:

    {[
      let handler =
        object
          inherit Claude.Handler.default

          method! on_text t =
            Printf.printf "Text: %s\n" (Response.Text.content t)
        end
    ]}

    Methods you don't override will simply be ignored, making this ideal for
    prototyping and for cases where you only care about specific events. *)

(** Abstract handler requiring all methods to be implemented.

    Use this when you want compile-time guarantees that all events are handled:

    {[
      let handler = object
        inherit Claude.Handler.abstract
        method on_text t = (* required *)
        method on_tool_use t = (* required *)
        method on_tool_result t = (* required *)
        method on_thinking t = (* required *)
        method on_init t = (* required *)
        method on_error t = (* required *)
        method on_complete t = (* required *)
      end
    ]}

    The compiler will enforce that you implement all methods, ensuring no events
    are silently ignored. *)
class virtual abstract : object
  method virtual on_text : Response.Text.t -> unit
  (** [on_text t] must be implemented by subclasses. *)

  method virtual on_tool_use : Response.Tool_use.t -> unit
  (** [on_tool_use t] must be implemented by subclasses. *)

  method virtual on_tool_result : Content_block.Tool_result.t -> unit
  (** [on_tool_result t] must be implemented by subclasses. *)

  method virtual on_thinking : Response.Thinking.t -> unit
  (** [on_thinking t] must be implemented by subclasses. *)

  method virtual on_init : Response.Init.t -> unit
  (** [on_init t] must be implemented by subclasses. *)

  method virtual on_error : Response.Error.t -> unit
  (** [on_error t] must be implemented by subclasses. *)

  method virtual on_complete : Response.Complete.t -> unit
  (** [on_complete t] must be implemented by subclasses. *)
end

(** {1 Dispatch Functions} *)

val dispatch : #handler -> Response.t -> unit
(** [dispatch handler response] dispatches a response event to the appropriate
    handler method based on the response type.

    Example:
    {[
      let handler =
        object
          inherit Claude.Handler.default
          method! on_text t = print_endline (Response.Text.content t)
        end
      in
      dispatch handler (Response.Text text_event)
    ]} *)

val dispatch_all : #handler -> Response.t list -> unit
(** [dispatch_all handler responses] dispatches all response events to the
    handler.

    This is equivalent to calling [List.iter (dispatch handler) responses] but
    may be more convenient:

    {[
      let responses = Client.receive_all client in
      dispatch_all handler responses
    ]} *)
