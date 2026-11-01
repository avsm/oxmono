type t = { host : string; port : int; tls : bool; client : Mqttz_eio.config }
(** Explicit TOML configuration. No implicit file discovery or environment
    overrides are applied. Unknown keys and invalid values are errors. *)

val codec : ?client_id:string -> unit -> t Toml.Codec.t
(** [codec ()] decodes connection settings. [client_id] supplies an optional
    default for an enclosing application configuration. Encoding is unsupported.
*)

val of_string : string -> (t, string) result
(** [of_string text] reads a flat TOML table containing connection settings.
    [client_id] is required. [version] is ["5.0"] or ["3.1.1"]. [host], [port],
    [tls], [keep_alive], [username], [password], [max_packet_size],
    [message_capacity] and [operation_timeout] are optional. Plaintext defaults
    to port 1883. TLS defaults to port 8883. *)
