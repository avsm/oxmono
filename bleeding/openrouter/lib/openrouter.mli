(** OpenRouter inference over a caller-supplied Fetch capability.

    Requests run in the calling Eio fiber. Cancellation interrupts the
    request and closes its response. This library creates no backend or
    background fibers. Retries, deadlines and connection policy belong to
    the caller. *)

type t

val of_fetch :
  ?base_url:string ->
  ?api_key:string ->
  ?app_url:string ->
  ?app_title:string ->
  ?max_response_bytes:int ->
  _ Fetch.t ->
  t
(** [of_fetch fetch] is a client at [https://openrouter.ai/api/v1].
    [base_url] includes the API prefix, such as [/v1] for a compatible
    server. Requests and credentials are restricted to that prefix. Bearer
    credentials require HTTPS. Response bodies default to a 16 MiB limit. *)

type error =
  | Http_error of { status : int; code : string option; message : string }
  | Stream_error of { code : string option; message : string }
  | Protocol_error of string
  | Image_too_large of { limit : int }

type Eio.Exn.err +=
  | E of error
        (** Errors are raised as [Eio.Io (E error, context)]. Fetch
            transport and decoding errors retain their Fetch identity.
            Cancellation propagates. *)

module Tool : sig
  type t

  val v :
    name:string ->
    ?description:string ->
    ?strict:bool ->
    parameters:Jsont.json ->
    unit ->
    t
  (** [v ~name ~parameters ()] defines a function tool. [parameters] is its
      JSON Schema. Tools are declared to the model and never executed here.
  *)

  type call = { id : string; name : string; arguments : string }

  val arguments : 'a Jsont.t -> call -> ('a, string) result
  (** [arguments codec call] decodes a completed tool call's arguments.
      Validate them before running a tool. *)
end

module Image : sig
  type t
  type format = Png | Jpeg | Webp | Gif
  type detail = Auto | Low | High | Original

  val of_url : ?detail:detail -> string -> t
  (** [of_url url] refers to an HTTP(S) image that the model provider
      fetches. No local request is made. Local Fetch restrictions do not
      constrain the provider's fetch. Credentials in URL userinfo are
      rejected. *)

  val of_string :
    ?max_bytes:int -> ?detail:detail -> format:format -> string -> t
  (** [of_string ~format bytes] embeds image bytes as a base64 data URL.
      [max_bytes] defaults to 20 MiB before encoding. Empty data is
      rejected. The caller supplies the actual format. Image contents are
      not decoded. *)

  val of_flow :
    ?max_bytes:int ->
    ?detail:detail ->
    format:format ->
    _ Eio.Flow.source ->
    t
  (** [of_flow ~format flow] reads and embeds a bounded image in the calling
      fiber. It uses the same limit as {!of_string} and leaves the flow
      open. Cancellation propagates. Oversized data raises
      [Eio.Io (E (Image_too_large _), context)]. *)
end

module Content : sig
  type t

  val text : string -> t
  val image : Image.t -> t
end

module Message : sig
  type t

  val system : string -> t
  val developer : string -> t
  val user : string -> t

  val user_parts : Content.t list -> t
  (** [user_parts parts] constructs a multimodal message, preserving part
      order. [parts] must be nonempty. Image support depends on the model.
  *)

  val assistant : ?tool_calls:Tool.call list -> string -> t

  val tool_result : tool_call_id:string -> string -> t
  (** Text messages and function-tool results for a conversation. *)
end

module Models : sig
  type model = {
    id : string;
    name : string option;
    context_length : int option;
  }

  val list : t -> model list
  (** [list client] retrieves model identifiers and available display
      metadata. Accepts OpenRouter and OpenAI-compatible model-list
      responses. *)
end

module Chat : sig
  type request
  type tool_choice = Auto | None_ | Required | Function of string

  val request :
    ?max_tokens:int ->
    ?temperature:float ->
    ?top_p:float ->
    ?seed:int ->
    ?stop:string list ->
    ?tools:Tool.t list ->
    ?tool_choice:tool_choice ->
    ?parallel_tool_calls:bool ->
    model:string ->
    messages:Message.t list ->
    unit ->
    request
  (** [request ~model ~messages ()] builds a request usable with both
      {!complete} and {!stream}. [max_tokens] is the completion-token
      budget. Invalid sampling options, empty models and empty messages are
      rejected before I/O. *)

  type usage = {
    prompt_tokens : int;
    completion_tokens : int;
    total_tokens : int;
    cost : float option;
  }

  type finish_reason =
    | Stop
    | Length
    | Tool_calls
    | Content_filter
    | Other of string

  type choice = {
    index : int;
    text : string option;
    reasoning : string option;
    refusal : string option;
    tool_calls : Tool.call list;
    finish_reason : finish_reason option;
  }

  type completion = {
    id : string;
    model : string;
    created : int;
    choices : choice list;
    usage : usage option;
  }

  val complete : t -> request -> completion
  (** [complete client request] performs one non-streaming completion. *)

  type event =
    | Started of { id : string; model : string; created : int }
    | Text of { choice : int; text : string }
    | Reasoning of { choice : int; text : string }
    | Refusal of { choice : int; text : string }
    | Tool_call of {
        choice : int;
        index : int;
        id : string option;
        name : string option;
        arguments : string option;
      }
    | Finished of { choice : int; reason : finish_reason }
    | Usage of usage

  val stream :
    ?max_event:int ->
    on_event:(event -> [ `Continue | `Stop ]) ->
    t ->
    request ->
    [ `Complete | `Stopped ]
  (** [stream ~on_event client request] delivers events in wire order while
      the response is open. The callback runs in the calling fiber, so its
      pace provides backpressure and it may perform Eio operations.

      [max_event] defaults to 1 MiB per SSE block. [`Complete] requires the
      server's [[DONE]] sentinel. [`Stop] returns [`Stopped] and closes the
      response immediately. Callback exceptions and cancellation also close
      it. EOF before the sentinel is a protocol error. Streams are not
      reconnected.

      Tool argument fragments are delivered unchanged. Assemble them by
      choice and tool index before parsing. A Finished event ends a choice,
      while a later Usage event can still arrive before the completion
      sentinel. *)
end
