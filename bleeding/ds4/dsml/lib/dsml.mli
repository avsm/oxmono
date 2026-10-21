(*---------------------------------------------------------------------------
   Copyright (c) 2026 Anil Madhavapeddy. All rights reserved.
   SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

(** dsml encodes model prompts and parses model replies, in the DeepSeek-V4
    markup or the V4.1, GLM and Qwen ones.

    {1 Synopsis}

    {[
    open Dsml

    let prompt =
      encode_messages Thinking
        [ system "You are helpful."; user "What is 2+2?" ]

    let reply = parse_message_from_completion_text Thinking completion
    ]}

    {1 Description}

    A conversation is a list of {!message} values, being system, user, assistant
    and tool results. {!encode_messages} renders them into the prompt a
    DeepSeek-V4 model expects. {!parse_message_from_completion_text} turns one
    reply back into its text, reasoning and tool calls. Use {!Chat} for a direct
    answer, or {!Thinking} to have the model reason first.

    Tool calls travel as DSML, the model's tool-call markup:

    {v
    <｜DSML｜tool_calls>
    <｜DSML｜invoke name="edit">
    <｜DSML｜parameter name="path" string="true">/tmp/x.c</｜DSML｜parameter>
    <｜DSML｜parameter name="line" string="false">42</｜DSML｜parameter>
    </｜DSML｜invoke>
    </｜DSML｜tool_calls>
    v}

    A parameter with [string="true"] holds raw text, and one with
    [string="false"] holds JSON. The markup resembles XML but is not XML, since
    parameter bodies are raw text. Tool results are given as {!tool} messages
    and fold into the preceding turn. Tool schemas and tool-call arguments are
    {!Json.t} values, and the tool-call grammar itself is the {!Codec}.

    A GLM model speaks another markup over the same conversation values. Role
    markers open every turn, a tool result is an observation turn wrapped in
    [<tool_response>], and a call is one block per invocation with untyped
    values:

    {v
    <tool_call>edit<arg_key>path</arg_key><arg_value>/tmp/x.c</arg_value><arg_key>line</arg_key><arg_value>42</arg_value></tool_call>
    v}

    Pass {!Glm} to {!encode_messages}, {!Stream.create} and
    {!neutralise_specials} to speak it.

    DeepSeek V4.1 speaks DSML with the tags renamed, [<｜DSML｜ calls>],
    [<｜DSML｜ invoke>] and [<｜DSML｜ parameter>], the space included, and marks a
    system turn with [<｜System｜>]. Pass {!Deepseek41} for it.

    A Qwen model speaks ChatML. Every turn opens with [<|im_start|>] and its
    role and closes with [<|im_end|>], a tool result is a [<tool_response>]
    inside a user turn, and a call is one block per invocation with untyped
    values, each on lines of its own:

    {v
    <tool_call>
    <function=edit>
    <parameter=path>
    /tmp/x.c
    </parameter>
    </function>
    </tool_call>
    v}

    Pass {!Qwen} for it. The {!Codec} and the whole-reply parser are DeepSeek
    V4's alone. *)

(** {1 Dialect} *)

(** The model family whose markup to speak. The conversation representation is
    shared, and the dialect chooses the rendering and the reply grammar. *)
type dialect =
  | Deepseek  (** DeepSeek V4 Flash and PRO *)
  | Deepseek41  (** DeepSeek V4.1 Flash *)
  | Glm  (** GLM 5.3 *)
  | Qwen  (** Qwen3.8 Flash Next *)

(** {1 Tokens} *)

(** The model's reserved marker strings. *)

val bos_token : string
val eos_token : string
val thinking_start_token : string
val thinking_end_token : string
val dsml_token : string
val user_token : string
val assistant_token : string
val latest_reminder_token : string

(** The GLM marker strings, structure to a GLM prompt exactly as the markers
    above are to a DeepSeek one. *)

val glm_bos_token : string
val glm_sop_token : string
val glm_eos_token : string
val glm_system_token : string
val glm_user_token : string
val glm_assistant_token : string
val glm_observation_token : string
val glm_tool_call_open : string
val glm_tool_call_close : string
val glm_tool_response_open : string
val glm_tool_response_close : string
val glm_arg_key_open : string
val glm_arg_key_close : string
val glm_arg_value_open : string
val glm_arg_value_close : string

val deepseek41_system_token : string
(** [deepseek41_system_token] opens a system turn in a DeepSeek V4.1 prompt. *)

(** The Qwen ChatML markers. A Qwen prompt spells [<tool_call>] and
    [<tool_response>] as {!glm_tool_call_open} and {!glm_tool_response_open} do.
*)

val qwen_im_start_token : string
val qwen_im_end_token : string
val qwen_endoftext_token : string

val neutralise_specials : ?dialect:dialect -> string -> string
(** [neutralise_specials s] is [s] with every marker the dialect gives a meaning
    to rewritten to an inert look-alike, [|DSML|] for {!dsml_token} and [[User]]
    for {!user_token} among them. Text the model generated must pass through
    this before it is recorded as a message, since a conversation is re-encoded
    on every turn and a marker left in it would be read as structure the model
    never meant: a forged turn boundary, an unbalanced [<think>], a tool-call
    block out of nowhere. Ordinary text is returned unchanged. [dialect]
    defaults to {!Deepseek}. The other dialects also rewrite the DeepSeek-style
    markers, which their tokenizers map onto reserved tokens of their own. *)

(** {1 JSON}

    JSON values in jsont's representation, so that jsont's own combinators apply
    directly. Tool schemas and tool-call arguments are {!Json.t} values. *)
module Json : sig
  type t = Jsont.json
  (** A generic JSON value. *)

  module Value : sig
    val to_string : t -> string
    (** [to_string j] is the compact JSON text of [j]. *)

    val of_string : string -> (t, string) result
    (** [of_string s] parses [s], or returns an error message. *)

    val of_string_exn : string -> t
    (** [of_string_exn s] parses [s]. It raises [Invalid_argument] if [s] is not
        valid JSON. *)
  end
end

(** {1 Types} *)

(** Reply directly, or reason in [<think>] first. *)
type thinking_mode = Chat | Thinking

(** [Max] asks for maximum reasoning; [High] is accepted but inert. *)
type reasoning_effort = High | Max

(** Quick-instruction tasks for DeepSeek's internal pipeline. *)
type task = Action | Query | Authority | Domain | Title | Read_url

type tool_call = { id : string option; name : string; arguments : string }
(** A tool call. [arguments] is a JSON object string. *)

type parsed_message = {
  content : string;
  reasoning_content : string;
  tool_calls : tool_call list;
}
(** A parsed assistant turn. *)

type message
(** A conversation message. *)

exception Parse_error of string
(** Malformed model output. *)

(** {1 Messages} *)

val system : ?tools:Json.t list -> ?response_format:Json.t -> string -> message
(** [system content] is a system turn; [tools] advertises callable tools. *)

val developer :
  ?tools:Json.t list -> ?response_format:Json.t -> string -> message
(** [developer content] is a developer turn. [content] must not be empty. *)

val user : ?task:task -> string -> message
(** [user content] is a user turn. *)

val assistant :
  ?content:string ->
  ?reasoning_content:string ->
  ?tool_calls:tool_call list ->
  ?wo_eos:bool ->
  ?task:task ->
  unit ->
  message
(** [assistant ()] is an assistant turn; [wo_eos] leaves it open for continued
    generation. *)

val tool : id:string -> string -> message
(** [tool ~id result] is a tool result for call [id]. *)

val latest_reminder : string -> message
(** [latest_reminder content] is a latest-reminder turn (date, locale). *)

val tool_call :
  ?id:string -> name:string -> arguments:string -> unit -> tool_call
(** [tool_call ~name ~arguments ()] builds a tool call. *)

(** {1 Encode} *)

val encode_messages :
  ?dialect:dialect ->
  ?context:message list ->
  ?drop_thinking:bool ->
  ?add_default_bos_token:bool ->
  ?reasoning_effort:reasoning_effort ->
  thinking_mode ->
  message list ->
  string
(** [encode_messages mode messages] renders the conversation to the prompt
    string. [context] prepends an already-encoded prefix and suppresses the
    leading token; [drop_thinking] (default true) drops reasoning from turns
    before the last user message; [reasoning_effort] [Max] maximises reasoning.

    [dialect] defaults to {!Deepseek}. Under {!Glm} a tool result renders as an
    observation turn rather than folding into a user turn, {!Thinking} writes a
    reasoning-effort system line ahead of the conversation ([High] unless
    [reasoning_effort] is [Max]), and no end token closes an assistant turn, the
    next role marker being the boundary.

    Under {!Deepseek41} a system turn opens with {!deepseek41_system_token}, and
    {!Thinking} writes a numeric effort line ahead of the conversation, 75
    unless [reasoning_effort] is [Max], which is 100.

    Under {!Qwen} {!Thinking} writes the xhigh effort instruction into the first
    system turn, whatever [reasoning_effort] is, consecutive tool results share
    one user turn, and [add_default_bos_token] has no effect, Qwen having no BOS
    token. *)

(** {1 Parse} *)

val parse_message_from_completion_text :
  thinking_mode -> string -> parsed_message
(** [parse_message_from_completion_text mode text] parses one raw reply,
    trailing EOS included. It raises {!Parse_error} on malformed output. *)

(** {1 Codec}

    Typed DSML tool calls. A ['a value] converts one parameter. {!Object} builds
    structured parameter values. {!Invoke} builds a named invocation and
    provides the JSON Schema advertised to the model.

    {[
    type edit = { path : string; line : int }

    let edit_codec =
      let open Dsml.Codec in
      Invoke.map "edit" (fun path line -> { path; line })
      |> Invoke.param ~enc:(fun e -> e.path) "path" string
      |> Invoke.param ~enc:(fun e -> e.line) "line" int
      |> Invoke.seal

    let block =
      Dsml.Codec.encode edit_codec [ { path = "/tmp/x.c"; line = 42 } ]

    let calls = Dsml.Codec.decode edit_codec block
    ]} *)
module Codec : sig
  type 'a value
  (** A codec for one parameter value. *)

  val string : string value
  (** [string] is a raw-text parameter ([string="true"]). *)

  val bool : bool value
  (** [bool] is a JSON boolean parameter. *)

  val int : int value
  (** [int] is a JSON integer parameter. *)

  val float : float value
  (** [float] is a finite JSON number parameter. *)

  val array : ?minimum:int -> ?maximum:int -> 'a value -> 'a list value
  (** [array item] is a JSON array of [item] values. [minimum] and [maximum]
      constrain its length during encoding and decoding. *)

  val json : Json.t value
  (** [json] is any JSON parameter (or a JSON string). *)

  val map_value : dec:('a -> 'b) -> enc:('b -> 'a) -> 'a value -> 'b value
  (** [map_value ~dec ~enc v] adapts the value codec [v] to another type. *)

  module Object : sig
    type ('o, 'dec) map
    (** An object codec under construction. *)

    val map : string -> 'dec -> ('o, 'dec) map
    (** [map name constructor] starts a named object. *)

    val param :
      enc:('o -> 'a) ->
      ?description:string ->
      ?default:'a ->
      string ->
      'a value ->
      ('o, 'a -> 'b) map ->
      ('o, 'b) map
    (** [param ~enc name value map] adds a member. [default] permits omission.
    *)

    val optional :
      enc:('o -> 'a option) ->
      ?description:string ->
      string ->
      'a value ->
      ('o, 'a option -> 'b) map ->
      ('o, 'b) map
    (** [optional ~enc name value map] adds a member that may be absent. *)

    val seal : ('o, 'o) map -> 'o value
    (** [seal map] finishes the object. *)
  end

  type 'a t
  (** A codec for one tool invocation, producing a value of type ['a]. *)

  module Invoke : sig
    type ('o, 'dec) map = ('o, 'dec) Object.map
    (** Tool arguments under construction. *)

    val map : string -> 'dec -> ('o, 'dec) map
    (** [map name constructor] starts the arguments for tool [name]. *)

    val param :
      enc:('o -> 'a) ->
      ?description:string ->
      ?default:'a ->
      string ->
      'a value ->
      ('o, 'a -> 'b) map ->
      ('o, 'b) map
    (** [param ~enc name value map] adds a parameter. [default] permits
        omission. *)

    val optional :
      enc:('o -> 'a option) ->
      ?description:string ->
      string ->
      'a value ->
      ('o, 'a option -> 'b) map ->
      ('o, 'b) map
    (** [optional ~enc name value map] adds a parameter that may be absent. *)

    val seal : ('o, 'o) map -> 'o t
    (** [seal map] finishes the arguments. *)
  end

  val dynamic : tool_call t
  (** [dynamic] decodes/encodes any tool, mapping parameters to JSON arguments.
  *)

  val map : dec:('a -> 'b) -> enc:('b -> 'a) -> 'a t -> 'b t
  (** [map ~dec ~enc c] adapts an invoke codec to another type. *)

  val name : 'a t -> string
  (** [name c] is the tool name [c] decodes and encodes ([""] for {!dynamic} and
      {!choice}). *)

  val schema : 'a t -> Json.t
  (** [schema c] is the JSON Schema for [c]'s parameters object, with one
      property per {!Invoke.param} and a [required] list. Pass it to {!Tool.v}
      to advertise the tool to the model. *)

  val decode_arguments : 'a t -> string -> ('a, string) result
  (** [decode_arguments c arguments] decodes one tool call's JSON-object
      [arguments] string (as carried by {!tool_call}) through [c], returning the
      typed value or an error message. *)

  val encode_arguments : 'a t -> 'a -> (string, string) result
  (** [encode_arguments c value] encodes [value] as a JSON object suitable for a
      tool call's [arguments] field. *)

  type 'b case
  (** A case in a {!choice}: one tool name handled by a typed codec. *)

  val case : inject:('a -> 'b) -> project:('b -> 'a option) -> 'a t -> 'b case
  (** [case ~inject ~project c] makes invoke codec [c] a case of a sum type
      ['b]: [inject] lifts a decoded value in, [project] selects it for
      encoding. *)

  val choice : ?default:'b t -> 'b case list -> 'b t
  (** [choice ?default cases] dispatches an invocation on its tool name to the
      matching case; [default] handles unmatched names (e.g. {!dynamic}). *)

  val decode : 'a t -> string -> ('a list, string) result
  (** [decode c s] decodes the tool-call block found in [s] with [c]. *)

  val encode : 'a t -> 'a list -> string
  (** [encode c calls] renders [calls] as a [<｜DSML｜tool_calls>] block. *)
end

(** {1 Tools} *)

(** Build the OpenAI tool objects passed to {!system} and {!developer}. *)
module Tool : sig
  val v :
    name:string -> ?description:string -> ?parameters:Json.t -> unit -> Json.t
  (** [v ~name ?description ?parameters ()] is the tool object
      [{"type":"function","function":{"name";"description";"parameters"}}].
      [parameters] is a JSON Schema. *)
end

val tool_prompt : ?dialect:dialect -> Json.t list -> string
(** [tool_prompt ~dialect tools] renders the trusted tool grammar and schemas
    for [tools]. It contains control syntax and must not be used for untrusted
    content. *)

(** {1 Streaming decode}

    Decode a completion as it is generated, token by token, emitting reasoning
    and content text as it arrives and tool calls once each block completes.
    This is the primitive an interactive agent feeds with model output. *)
module Stream : sig
  (** A piece of decoded output. *)
  type event =
    | Reasoning of string  (** a chunk of [<think>] reasoning *)
    | Content of string  (** a chunk of reply text *)
    | Tool_call of tool_call  (** a completed tool call *)
    | Tool_error of { message : string; generated : string }
        (** a malformed or incomplete tool call and its inert generated text *)
    | Done  (** end of turn (EOS reached) *)

  type t
  (** An incremental decoder. *)

  val create : ?dialect:dialect -> thinking_mode -> t
  (** [create mode] is a fresh decoder. [dialect] defaults to {!Deepseek}. A
      {!Glm} or {!Qwen} decoder reads one call per [<tool_call>] block and goes
      on decoding content after a block, since such a model may write another
      block, or its answer, after one. A Qwen block closes at the first
      [</tool_call>] outside its parameter values. *)

  val feed : t -> string -> event list
  (** [feed d chunk] decodes the next [chunk] of generated text, in order.
      Partial markers spanning chunk boundaries are held back until complete.

      A tool-call block that does not parse surfaces as {!Tool_error} rather
      than being silenced. Its generated text is passed through
      {!neutralise_specials}, so recording it cannot re-enter a conversation as
      this module's own markup. Decoding then continues as content.

      Only a well-formed block ends the turn's decoding. A turn whose first
      block is refused can therefore still yield a tool call from a later one.
  *)

  val in_tool_call : t -> bool
  (** [in_tool_call d] is whether [d] is part way through a tool-call block,
      having seen the markup that opens one and not the markup that closes it.

      Ask this before {!finish}, and only a caller that stops the stream on a
      budget of its own needs to. It tells a call the stream ended in the middle
      of from a reply that was text: the call is discarded, and what the model
      wrote of it is not an answer to pass off as one. *)

  val sampling_mode : t -> [ `Configured | `Greedy ]
  (** [sampling_mode d] is [`Greedy] while [d] is reading tool-protocol
      structure and [`Configured] for prose, reasoning and argument values. Ask
      before sampling the next token. *)

  val finish : t -> event list
  (** [finish d] flushes buffered text at end of stream and emits {!Done}. A
      tool-call block left open by the stream surfaces as {!Tool_error}, with
      its special tokens made inert. *)
end

(** {1 Byte streams} *)

val encode_messages_to_writer :
  Bytesrw.Bytes.Writer.t ->
  ?dialect:dialect ->
  ?context:message list ->
  ?drop_thinking:bool ->
  ?add_default_bos_token:bool ->
  ?reasoning_effort:reasoning_effort ->
  thinking_mode ->
  message list ->
  unit
(** [encode_messages_to_writer w mode messages] renders the conversation to byte
    stream [w]. *)

val parse_message_from_reader :
  thinking_mode -> Bytesrw.Bytes.Reader.t -> parsed_message
(** [parse_message_from_reader mode r] parses one reply from byte stream [r]. *)
