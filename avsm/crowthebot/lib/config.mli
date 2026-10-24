type backend = Openrouter | Ds4 | Apple_fm

type t = {
  admin : string;
  homeserver : string;
  base_url : string;
  allow_http_model : bool;
  backend : backend;
  model : string;
  model_path : string option;
  cache_dir : string option;
  system_prompt : string;
  plugins : string list;
  context_messages : int;
  context_bytes : int;
  max_tokens : int;
      (** Completion budget per model call, including reasoning tokens. *)
  tool_calls : int;
      (** Tool calls allowed per user turn, including native adapter calls. *)
  tool_result_bytes : int;
      (** Maximum bytes returned by each tool to the model. *)
  tool_request_bytes : int;
      (** Additional bytes allowed for current-turn model requests. *)
  parallel_tool_calls : int;
      (** Maximum concurrent calls returned by the model in one batch. *)
  model_timeout_seconds : int;
      (** Timeout in seconds for each model request. *)
  turn_timeout_seconds : int;
      (** Timeout in seconds for a whole user or scheduled turn. *)
  compaction_reasoning_effort : string option;
      (** Reasoning effort sent with compaction requests. [None] omits the
          field. *)
  log_level : string;
  log_file : string option;
  improvements_file : string option;
      (** Markdown file for self-improvement requests, relative to the profile
          directory unless absolute. [None] disables the tools. *)
  voice_messages : bool;  (** transcribe incoming voice messages *)
  voice_locale : string option;
      (** speech locale such as ["en-GB"]. [None] uses the system locale. *)
  speech_voice : string option;
      (** voice for spoken notes, as [apple-speech voices] names it. [None]
          disables them. *)
  image_messages : bool;  (** pass images to a model that accepts them *)
}
(** Non-secret, operator-edited profile settings. *)

val default_speech_voice : string
(** [default_speech_voice] is ["Grandpa (English (UK))"]. *)

val default : admin:string -> homeserver:string -> t
val jsont : t Jsont.t
val tomlt : t Tomlt.t
val of_toml_string : string -> (t, string) result

val validate : t -> unit
(** [validate t] checks identity, server URLs and resource bounds. *)

val upgrade : t -> t
(** [upgrade t] removes the retired fixed blogroll plugin and its old default
    prompt suffix when loading an existing profile. It replaces the old built-in
    assistant prompts with the current built-in personality, concise reply style
    and Matrix Markdown guidance, preserving custom prompts.
*)
