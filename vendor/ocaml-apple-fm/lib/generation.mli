(** Options for one generation request. *)

type top_k = Apple_fm_base.top_k = { k : int; seed : int64 option }
(** Parameters for [`Top_k] sampling. *)

type probability = Apple_fm_base.probability = {
  threshold : float;
  seed : int64 option;
}
(** Parameters for [`Probability] sampling. *)

type sampling =
  [ `Default | `Greedy | `Top_k of top_k | `Probability of probability ]
(** A sampling mode. *)

val pp_sampling : Format.formatter -> sampling -> unit
(** [pp_sampling formatter sampling] prints [sampling]. *)

type tool_calling = [ `Allowed | `Required | `Disallowed ]
(** Whether a response may or must call a tool. [`Required] and [`Disallowed]
    require macOS 27 or later. *)

val pp_tool_calling : Format.formatter -> tool_calling -> unit
(** [pp_tool_calling formatter policy] prints [policy]. *)

type options = Apple_fm_base.options
(** Apple's
    {{:https://developer.apple.com/documentation/foundationmodels/generationoptions}
      generation options}. *)

val options :
  ?sampling:sampling ->
  ?temperature:float ->
  ?maximum_response_tokens:int ->
  ?tool_calling:tool_calling ->
  unit ->
  options
(** [options ()] uses Apple's defaults. Numeric arguments are validated locally
    and invalid values raise [Invalid_argument]. *)

val pp_options : Format.formatter -> options -> unit
(** [pp_options formatter options] prints [options]. *)
