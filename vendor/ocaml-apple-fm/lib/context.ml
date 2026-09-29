type reasoning_level = Apple_fm_base.reasoning_level
type t = Apple_fm_base.context

type usage = Apple_fm_base.usage = {
  input_tokens : int;
  cached_input_tokens : int;
  output_tokens : int;
  reasoning_tokens : int;
}

let create = Apple_fm_base.context
let pp = Apple_fm_base.pp_context
let pp_reasoning_level = Apple_fm_base.pp_reasoning_level
let pp_usage = Apple_fm_base.pp_usage
