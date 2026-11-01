type t = { host : string; port : int; tls : bool; client : Mqttz_eio.config }

let make host port tls client_id version keep_alive username password
    max_packet_size message_capacity operation_timeout =
  let version =
    match version with
    | "5.0" -> `V5_0
    | "3.1.1" -> `V3_1_1
    | _ -> invalid_arg "version must be 3.1.1 or 5.0"
  in
  let credentials =
    match (username, password) with
    | None, None -> None
    | Some username, None -> Some (`Username username)
    | Some username, Some password ->
        Some (`Username_password (username, password))
    | None, Some password when version = `V5_0 -> Some (`Password password)
    | None, Some _ -> invalid_arg "MQTT 3.1.1 password requires username"
  in
  let port = Option.value port ~default:(if tls then 8883 else 1883) in
  if host = "" then invalid_arg "host must not be empty";
  if port < 1 || port > 65535 then invalid_arg "port must be 1..65535";
  let client =
    Mqttz_eio.
      {
        client_id;
        version;
        keep_alive;
        credentials;
        will = None;
        max_packet_size;
        message_capacity;
        operation_timeout;
      }
  in
  Mqttz_eio.validate_config client;
  { host; port; tls; client }

let codec ?client_id () =
  Toml.Codec.(
    Table.(
      obj make
      |> mem "host" string ~dec_absent:"127.0.0.1"
      |> opt_mem "port" int
      |> mem "tls" bool ~dec_absent:false
      |> mem "client_id" string ?dec_absent:client_id
      |> mem "version" string ~dec_absent:"5.0"
      |> mem "keep_alive" int ~dec_absent:60
      |> opt_mem "username" string |> opt_mem "password" string
      |> mem "max_packet_size" int ~dec_absent:Mqttz.Frame.default_max_size
      |> mem "message_capacity" int ~dec_absent:32
      |> mem "operation_timeout" float ~dec_absent:30.
      |> error_unknown |> finish))

let of_string text =
  try Result.map_error Toml.Error.to_string (Toml.of_string (codec ()) text)
  with Invalid_argument message -> Error message
