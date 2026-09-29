type top_k = Apple_fm_base.top_k = { k : int; seed : int64 option }

type probability = Apple_fm_base.probability = {
  threshold : float;
  seed : int64 option;
}

type sampling = Apple_fm_base.sampling
type tool_calling = Apple_fm_base.tool_calling
type options = Apple_fm_base.options

let options = Apple_fm_base.options
let pp_sampling = Apple_fm_base.pp_sampling
let pp_tool_calling = Apple_fm_base.pp_tool_calling
let pp_options = Apple_fm_base.pp_options
