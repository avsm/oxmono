(** System-model selection and inspection. *)

type use_case = [ `General | `Content_tagging ]
(** Apple's
    {{:https://developer.apple.com/documentation/foundationmodels/systemlanguagemodel/usecase}
      system-model specialization}. *)

val pp_use_case : Format.formatter -> use_case -> unit
(** [pp_use_case formatter use_case] prints [use_case]. *)

type guardrails = [ `Default | `Permissive_content_transformations ]
(** The system model's content policy. Permissive transformations are for
    transforming user-provided content, not unrestricted generation. *)

val pp_guardrails : Format.formatter -> guardrails -> unit
(** [pp_guardrails formatter guardrails] prints [guardrails]. *)

type t = Apple_fm_base.model
(** A system language model configuration. *)

val create : ?use_case:use_case -> ?guardrails:guardrails -> unit -> t
(** [create ()] selects the general model with default guardrails. *)

val default : t
(** [default] is [create ()]. *)

val pp : Format.formatter -> t -> unit
(** [pp formatter model] prints [model]. *)

type capabilities = Apple_fm_base.capabilities = {
  vision : bool;
  guided_generation : bool;
  reasoning : bool;
  tool_calling : bool;
}
(** Capabilities reported by the selected system model. *)

val pp_capabilities : Format.formatter -> capabilities -> unit
(** [pp_capabilities formatter capabilities] prints [capabilities]. *)

type info = Apple_fm_base.model_info = {
  context_size : int;
  supported_languages : string list;
  variant : string option;
  capabilities : capabilities;
}
(** Runtime properties. [variant] is available on macOS 27 or later. *)

val info : ?model:t -> unit -> info
(** [info ()] reads the model's context size, languages, variant, and
    capabilities. *)

val supports_locale : ?model:t -> string -> bool
(** [supports_locale locale] checks a locale such as ["en-GB"]. *)

val pp_info : Format.formatter -> info -> unit
(** [pp_info formatter info] prints [info]. *)

val count_prompt_tokens : ?model:t -> Prompt.t -> int
(** [count_prompt_tokens prompt] counts [prompt]. This requires macOS 26.4. *)

val count_text_tokens : ?model:t -> string -> int
(** [count_text_tokens text] counts a text-only prompt. *)

val count_instructions_tokens : ?model:t -> string -> int
(** [count_instructions_tokens text] counts instruction tokens. *)

val count_schema_tokens : ?model:t -> Schema.t -> int
(** [count_schema_tokens schema] counts a generation schema. *)

val count_transcript_tokens : ?model:t -> Transcript.t -> int
(** [count_transcript_tokens transcript] counts a transcript. *)

val count_tool_tokens : ?model:t -> Tool.t list -> int
(** [count_tool_tokens tools] counts the tool definitions. *)

val compact_transcript :
  ?keep_last_turns:int -> summary:string -> Transcript.t -> Transcript.t
(** [compact_transcript ~summary transcript] keeps the original instructions and
    tools, [summary], and the last two complete turns. [keep_last_turns] changes
    that number. It requires Apple's public transcript-entry API and raises a
    contextual {!Error.E} on failure. *)
