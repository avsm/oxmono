(*---------------------------------------------------------------------------
   Copyright (c) 2026 Anil Madhavapeddy. All rights reserved.
   SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

(** One model request over a whole transcript.

    {!Agent.S} keeps a conversation inside the adapter. A bot that stores its
    own history, compacts it, and asks tool-free questions such as summaries
    needs the opposite: it supplies every message each time, chooses the tools
    and budget per request, and is told how the reply ended. An adapter provides
    a {!complete} function for that. *)

type message =
  | System of string
  | User of string
  | Assistant of { text : string; calls : Agent.tool_call list }
  | Tool_result of { id : string; content : string }
      (** the result of the call with that [id], following its assistant
          message *)

type finish =
  | Stop  (** the model ended its reply *)
  | Length  (** the token ceiling ended it, so text may be unfinished *)
  | Tool_calls  (** the model is waiting for tool results *)
  | Other of string

type request = {
  messages : message list;
  tools : Agent.Tool.t list;  (** none makes the request tool-free *)
  max_tokens : int option;  (** includes any reasoning tokens *)
  reasoning : string option;
      (** an effort such as ["none"] or ["high"], or the backend default *)
}

val request :
  ?tools:Agent.Tool.t list ->
  ?max_tokens:int ->
  ?reasoning:string ->
  message list ->
  request
(** [request messages] is a request with the backend's defaults. It raises
    [Invalid_argument] for an empty transcript or a nonpositive budget. *)

type response = {
  text : string option;  (** the visible reply, without reasoning *)
  calls : Agent.tool_call list;  (** in the order the model made them *)
  finish : finish option;  (** [None] when the backend did not say *)
}

val response :
  ?calls:Agent.tool_call list -> ?finish:finish -> string option -> response
(** [response text] is a reply with no calls and an unknown finish. *)

type complete = request -> response
(** A function that sends one request. It raises on transport failure. *)

val system_text : message list -> string option
(** [system_text messages] is the content of a leading {!System} message. *)

val text_of_response : response -> string option
(** [text_of_response r] is [r.text] when it holds non-blank text. *)
