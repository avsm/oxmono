type context_size_exceeded = Apple_fm_base.context_size_exceeded = {
  context_size : int option;
  token_count : int option;
  message : string;
}

type rate_limited = Apple_fm_base.rate_limited = {
  reset_time : float option;
  message : string;
}

type unsupported_capability = Apple_fm_base.unsupported_capability = {
  capability : string option;
  message : string;
}

type unsupported_guide = Apple_fm_base.unsupported_guide = {
  schema : string option;
  message : string;
}

type unsupported_language = Apple_fm_base.unsupported_language = {
  language : string option;
  message : string;
}

type t = Apple_fm_base.error
type Eio.Exn.err += E = Apple_fm_base.E

let pp = Apple_fm_base.pp_error
