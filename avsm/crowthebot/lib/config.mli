type backend = Openrouter | Ds4 | Apple_fm

type t = {
  admin : string;
  homeserver : string;
  base_url : string;
  backend : backend;
  model : string;
  model_path : string option;
  cache_dir : string option;
  system_prompt : string;
  plugins : string list;
  context_messages : int;
  context_bytes : int;
  max_tokens : int;
  log_level : string;
  log_file : string option;
}
(** Non-secret, operator-edited profile settings. *)

val default : admin:string -> homeserver:string -> t
val jsont : t Jsont.t
val tomlt : t Tomlt.t
val of_toml_string : string -> (t, string) result

val validate : t -> unit
(** [validate t] checks identity, server URLs and resource bounds. *)

val upgrade : t -> t
(** [upgrade t] removes the retired fixed blogroll plugin and its old default
    prompt suffix when loading an existing profile. It replaces the old built-in
    assistant prompt with Crow's robot personality, preserving custom prompts.
*)
