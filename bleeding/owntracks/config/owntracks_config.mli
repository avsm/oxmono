type device = { id : string; name : string }
(** Explicit OwnTracks TOML configuration. Unknown keys are errors. *)

type recorder = {
  url : string option;
  user : string option;
  password : string option;
}

type owntracks = {
  topic : string;
  default_device : string option;
  recorder : recorder;
  devices : device list;
}

type t = { owntracks : owntracks; mqtt : Mqttz_config.t }

val default : client_id:string -> t

val of_string : client_id:string -> string -> (t, string) result
(** [of_string ~client_id text] reads [owntracks] and [mqtt] tables. The
    supplied client ID is used when the [mqtt] table does not specify one. *)

val device_name : t -> string -> string
(** [device_name config id] resolves a configured alias, or returns [id]. *)

val default_toml : string
