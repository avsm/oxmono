type device = { id : string; name : string }
(** Explicit OwnTracks TOML configuration. Unknown keys are errors. *)

type recorder = {
  url : string option;
  user : string option;
  password : string option;
}

type overpass = { url : string; enabled : bool; allow_http : bool }

val default_overpass : overpass
(** [default_overpass] selects the public OpenStreetMap Overpass interpreter. *)

type owntracks = {
  topic : string;
  default_device : string option;
  recorder : recorder;
  overpass : overpass;
  devices : device list;
}

type t = { owntracks : owntracks; mqtt : Mqttz_config.t }

val default : client_id:string -> t

val of_string : client_id:string -> string -> (t, string) result
(** [of_string ~client_id text] reads [owntracks] and [mqtt] tables. The
    supplied client ID is used when the [mqtt] table does not specify one. *)

val device_name : t -> string -> string
(** [device_name config id] resolves a configured alias, or returns [id]. *)

val device_id : t -> string -> (string, string) result
(** [device_id config name] resolves a configured display name or validates a
    raw device ID. Ambiguous names are rejected. *)

val default_path : unit -> string
(** [default_path ()] resolves the CLI configuration path using XDG_CONFIG_HOME,
    falling back to HOME/.config/owntracks/owntracks.toml. *)

val default_toml : string
