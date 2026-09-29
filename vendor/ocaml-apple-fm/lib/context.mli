(** Per-response context and session usage. *)

type reasoning_level = [ `Light | `Moderate | `Deep | `Custom of string ]
(** Reasoning effort on macOS 27 or later. *)

val pp_reasoning_level : Format.formatter -> reasoning_level -> unit
(** [pp_reasoning_level formatter level] prints [level]. *)

type t = Apple_fm_base.context
(** Apple's per-response [ContextOptions]. *)

val create :
  ?include_schema_in_prompt:bool ->
  ?reasoning_level:reasoning_level ->
  unit ->
  t
(** [create ()] leaves context construction to Apple's defaults. Non-default
    values require macOS 27 or later. *)

val pp : Format.formatter -> t -> unit
(** [pp formatter context] prints [context]. *)

type usage = Apple_fm_base.usage = {
  input_tokens : int;
  cached_input_tokens : int;
  output_tokens : int;
  reasoning_tokens : int;
}
(** Cumulative
    {{:https://developer.apple.com/documentation/foundationmodels/languagemodelsession/usage-swift.property}
      session token usage}
    on macOS 27 or later. *)

val pp_usage : Format.formatter -> usage -> unit
(** [pp_usage formatter usage] prints [usage]. *)
