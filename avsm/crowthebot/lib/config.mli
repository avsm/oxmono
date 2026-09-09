type t = {
  admin : string;
  homeserver : string;
  base_url : string;
  model : string;
  system_prompt : string;
  plugins : string list;
  context_messages : int;
  context_bytes : int;
  max_tokens : int;
}
(** Non-secret, operator-edited profile settings. *)

val default : admin:string -> homeserver:string -> t
val jsont : t Jsont.t

val validate : t -> unit
(** [validate t] checks identity, server URLs and resource bounds. *)

val upgrade : t -> t
(** [upgrade t] removes the retired fixed blogroll plugin and its old default
    prompt suffix when loading an existing profile. It preserves other settings.
*)
