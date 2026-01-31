(*---------------------------------------------------------------------------
  Copyright (c) 2025 Anil Madhavapeddy <anil@recoil.org>. All rights reserved.
  SPDX-License-Identifier: ISC
 ---------------------------------------------------------------------------*)

(** Object-oriented response handler implementations. *)

(** {1 Handler Interface} *)

class type handler = object
  method on_text : Response.Text.t -> unit
  method on_tool_use : Response.Tool_use.t -> unit
  method on_tool_result : Content_block.Tool_result.t -> unit
  method on_thinking : Response.Thinking.t -> unit
  method on_init : Response.Init.t -> unit
  method on_error : Response.Error.t -> unit
  method on_complete : Response.Complete.t -> unit
end

(** {1 Concrete Implementations} *)

class default : handler =
  object
    method on_text (_ : Response.Text.t) = ()
    method on_tool_use (_ : Response.Tool_use.t) = ()
    method on_tool_result (_ : Content_block.Tool_result.t) = ()
    method on_thinking (_ : Response.Thinking.t) = ()
    method on_init (_ : Response.Init.t) = ()
    method on_error (_ : Response.Error.t) = ()
    method on_complete (_ : Response.Complete.t) = ()
  end

class virtual abstract =
  object
    method virtual on_text : Response.Text.t -> unit
    method virtual on_tool_use : Response.Tool_use.t -> unit
    method virtual on_tool_result : Content_block.Tool_result.t -> unit
    method virtual on_thinking : Response.Thinking.t -> unit
    method virtual on_init : Response.Init.t -> unit
    method virtual on_error : Response.Error.t -> unit
    method virtual on_complete : Response.Complete.t -> unit
  end

(** {1 Dispatch Functions} *)

let dispatch (handler : #handler) (response : Response.t) =
  match response with
  | Response.Text t -> handler#on_text t
  | Response.Tool_use t -> handler#on_tool_use t
  | Response.Tool_result t -> handler#on_tool_result t
  | Response.Thinking t -> handler#on_thinking t
  | Response.Init t -> handler#on_init t
  | Response.Error t -> handler#on_error t
  | Response.Complete t -> handler#on_complete t

let dispatch_all (handler : #handler) (responses : Response.t list) =
  List.iter (dispatch handler) responses
