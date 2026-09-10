type device = { id : string; name : string }

type recorder = {
  url : string option;
  user : string option;
  password : string option;
}

type overpass = { url : string; enabled : bool; allow_http : bool }

let default_overpass =
  {
    url = "https://overpass-api.de/api/interpreter";
    enabled = true;
    allow_http = false;
  }

type owntracks = {
  topic : string;
  default_device : string option;
  recorder : recorder;
  overpass : overpass;
  devices : device list;
}

type t = { owntracks : owntracks; mqtt : Mqttz_config.t }

let empty_recorder = { url = None; user = None; password = None }

let empty_owntracks =
  {
    topic = Owntracks.Mqtt.default_topic;
    default_device = None;
    recorder = empty_recorder;
    overpass = default_overpass;
    devices = [];
  }

let default ~client_id =
  {
    owntracks = empty_owntracks;
    mqtt =
      {
        host = "127.0.0.1";
        port = 1883;
        tls = false;
        client = Mqttz_eio.default_config ~client_id;
      };
  }

let device_codec =
  Toml.Codec.(
    Table.(
      obj (fun id name -> { id; name })
      |> mem "id" string |> mem "name" string |> error_unknown |> finish))

let recorder_codec =
  Toml.Codec.(
    Table.(
      obj (fun url user password -> { url; user; password })
      |> opt_mem "url" string |> opt_mem "user" string
      |> opt_mem "password" string |> error_unknown |> finish))

let overpass_codec =
  Toml.Codec.(
    Table.(
      obj (fun url enabled allow_http -> { url; enabled; allow_http })
      |> mem "url" string ~dec_absent:default_overpass.url
      |> mem "enabled" bool ~dec_absent:true
      |> mem "allow_http" bool ~dec_absent:false
      |> error_unknown |> finish))

let owntracks_codec =
  Toml.Codec.(
    Table.(
      obj (fun topic default_device recorder overpass devices ->
          { topic; default_device; recorder; overpass; devices })
      |> mem "topic" string ~dec_absent:Owntracks.Mqtt.default_topic
      |> opt_mem "default_device" string
      |> mem "recorder" recorder_codec ~dec_absent:empty_recorder
      |> mem "overpass" overpass_codec ~dec_absent:default_overpass
      |> mem "devices" (list device_codec) ~dec_absent:[]
      |> error_unknown |> finish))

let of_string ~client_id text =
  let codec =
    Toml.Codec.(
      Table.(
        obj (fun owntracks mqtt -> { owntracks; mqtt })
        |> mem "owntracks" owntracks_codec ~dec_absent:empty_owntracks
        |> mem "mqtt"
             (Mqttz_config.codec ~client_id ())
             ~dec_absent:(default ~client_id).mqtt
        |> error_unknown |> finish))
  in
  try
    match Toml.of_string codec text with
    | Error e -> Error (Toml.Error.to_string e)
    | Ok config ->
        if not (Mqttz.Topic.Filter.validate config.owntracks.topic) then
          invalid_arg "Invalid OwnTracks topic filter";
        List.iter
          (fun d ->
            ignore (Owntracks.Mqtt.device_topic ~user:"user" ~device:d.id);
            if d.name = "" then invalid_arg "Empty device name")
          config.owntracks.devices;
        let ids = List.map (fun d -> d.id) config.owntracks.devices in
        if List.length ids <> List.length (List.sort_uniq String.compare ids)
        then invalid_arg "Duplicate device ID";
        Ok config
  with Invalid_argument message -> Error message

let device_name config id =
  List.find_map
    (fun d -> if d.id = id then Some d.name else None)
    config.owntracks.devices
  |> Option.value ~default:id

let device_id config name =
  match
    List.filter (fun d -> d.id = name || d.name = name) config.owntracks.devices
  with
  | [] -> (
      try
        ignore (Owntracks.Mqtt.device_topic ~user:"user" ~device:name);
        Ok name
      with Invalid_argument _ -> Error "Invalid device ID")
  | [ device ] -> Ok device.id
  | _ -> Error "Ambiguous device name. Use an unambiguous device ID"

let default_path () =
  let dir =
    match Sys.getenv_opt "XDG_CONFIG_HOME" with
    | Some dir when dir <> "" && not (Filename.is_relative dir) -> dir
    | _ -> Filename.concat (Sys.getenv "HOME") ".config"
  in
  Filename.concat dir "owntracks/owntracks.toml"

let default_toml =
  {|[owntracks]
topic = "owntracks/#"
# default_device = "phone"

# [[owntracks.devices]]
# id = "phone"
# name = "My Phone"

# [owntracks.recorder]
# url = "https://recorder.example.com"
# user = "api-user"
# password = "secret"

[owntracks.overpass]
url = "https://overpass-api.de/api/interpreter"
enabled = true
# allow_http = false

[mqtt]
host = "127.0.0.1"
version = "5.0"
# port = 8883
# tls = true
# username = "user"
# password = "secret"
keep_alive = 60
|}
