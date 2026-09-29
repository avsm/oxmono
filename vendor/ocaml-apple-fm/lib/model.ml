type use_case = Apple_fm_base.use_case
type guardrails = Apple_fm_base.guardrails
type t = Apple_fm_base.model

type capabilities = Apple_fm_base.capabilities = {
  vision : bool;
  guided_generation : bool;
  reasoning : bool;
  tool_calling : bool;
}

type info = Apple_fm_base.model_info = {
  context_size : int;
  supported_languages : string list;
  variant : string option;
  capabilities : capabilities;
}

let create = Apple_fm_base.model
let default = Apple_fm_base.default_model
let pp = Apple_fm_base.pp_model
let pp_use_case = Apple_fm_base.pp_use_case
let pp_guardrails = Apple_fm_base.pp_guardrails
let info = Apple_fm_base.model_info
let supports_locale = Apple_fm_base.supports_locale
let pp_capabilities = Apple_fm_base.pp_capabilities
let pp_info = Apple_fm_base.pp_model_info
let count_prompt_tokens = Apple_fm_base.count_prompt_tokens
let count_text_tokens = Apple_fm_base.count_text_tokens
let count_instructions_tokens = Apple_fm_base.count_instructions_tokens
let count_schema_tokens = Apple_fm_base.count_schema_tokens
let count_transcript_tokens = Apple_fm_base.count_transcript_tokens
let count_tool_tokens = Apple_fm_base.count_tool_tokens
let compact_transcript = Apple_fm_base.compact_transcript
