(** Persistent Foundation Models conversations. *)

type t = Apple_fm_base.Session.t
(** An Apple
    {{:https://developer.apple.com/documentation/foundationmodels/languagemodelsession}
      [LanguageModelSession]}. *)

val create :
  sw:Eio.Switch.t ->
  ?model:Model.t ->
  ?instructions:string ->
  ?transcript:Transcript.t ->
  Tool.t list ->
  t
(** [create ~sw tools] creates a session and advertises [tools]. [transcript]
    restores a previous session and is mutually exclusive with [instructions].
    The switch owns the session. Failures raise contextual {!Error.E} values. *)

val prewarm : ?prompt_prefix:string -> t -> unit
(** [prewarm session] asks Apple to prepare [session] without waiting. *)

val close : t -> unit
(** [close session] releases [session]. Repeated calls have no effect. *)

val is_responding : t -> bool
(** [is_responding session] reports whether a response owns [session]. *)

val cancel : t -> unit
(** [cancel session] cancels the active response and closes [session]. *)

val respond_stream :
  ?options:Generation.options ->
  ?context:Context.t ->
  output:_ Eio.Flow.sink ->
  t ->
  string ->
  string
(** [respond_stream ~output session prompt] streams text deltas to [output] and
    returns the complete response. Tool handlers run in this fiber. *)

val respond :
  ?options:Generation.options -> ?context:Context.t -> t -> string -> string
(** [respond session prompt] returns a complete text response. *)

val respond_prompt_stream :
  ?options:Generation.options ->
  ?context:Context.t ->
  output:_ Eio.Flow.sink ->
  t ->
  Prompt.t ->
  string
(** [respond_prompt_stream] streams a prompt that may contain images. *)

val respond_prompt :
  ?options:Generation.options -> ?context:Context.t -> t -> Prompt.t -> string
(** [respond_prompt session prompt] returns a complete multimodal response. *)

val respond_json :
  ?options:Generation.options ->
  ?context:Context.t ->
  ?include_schema_in_prompt:bool ->
  t ->
  string ->
  'a Codec.value ->
  'a
(** [respond_json session prompt codec] generates and decodes a typed value. *)

val respond_prompt_json :
  ?options:Generation.options ->
  ?context:Context.t ->
  ?include_schema_in_prompt:bool ->
  t ->
  Prompt.t ->
  'a Codec.value ->
  'a
(** [respond_prompt_json] generates a typed value from a multimodal prompt. *)

val transcript : t -> Transcript.t
(** [transcript session] returns a serializable snapshot after prior responses. *)

val replace_transcript : t -> Transcript.t -> unit
(** [replace_transcript session transcript] replaces history on macOS 27. *)

val usage : t -> Context.usage option
(** [usage session] returns cumulative token usage on macOS 27 or [None]. *)
