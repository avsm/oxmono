(* Copyright (c) 2025 Anil Madhavapeddy. SPDX-License-Identifier: ISC *)
open Cmdliner
module Config = Owntracks_config
module Recorder = Owntracks_recorder_client

let with_timeout clock seconds f =
  Eio.Time.Timeout.run_exn (Eio.Time.Timeout.seconds clock seconds) f

let get = function Ok value -> value | Error message -> failwith message

let choose override fallback =
  match override with Some _ -> override | None -> fallback

let value override fallback = Option.value override ~default:fallback

let default_path () =
  let dir =
    match Sys.getenv_opt "XDG_CONFIG_HOME" with
    | Some dir when dir <> "" && not (Filename.is_relative dir) -> dir
    | _ -> Filename.concat (Sys.getenv "HOME") ".config"
  in
  Filename.concat dir "owntracks/owntracks.toml"

let load path =
  let client_id =
    Printf.sprintf "owntracks-%d-%.0f" (Unix.getpid ())
      (Unix.gettimeofday () *. 1e6)
  in
  let filename =
    match path with Some path -> path | None -> default_path ()
  in
  if path = None && not (Sys.file_exists filename) then
    Config.default ~client_id
  else
    let text = In_channel.with_open_bin filename In_channel.input_all in
    get (Config.of_string ~client_id text)

type mqtt_options = {
  host : string option;
  port : int option;
  tls : bool option;
  username : string option;
  password : string option;
  client_id : string option;
  version : Mqttz.Protocol_version.t option;
  keep_alive : int option;
}

let mqtt_config (config : Config.t) options =
  let mqtt = config.mqtt in
  let client = mqtt.client in
  let username, password =
    match client.credentials with
    | None -> (None, None)
    | Some (`Username u) -> (Some u, None)
    | Some (`Password p) -> (None, Some p)
    | Some (`Username_password (u, p)) -> (Some u, Some p)
  in
  let credentials =
    match
      (choose options.username username, choose options.password password)
    with
    | None, None -> None
    | Some u, None -> Some (`Username u)
    | None, Some p -> Some (`Password p)
    | Some u, Some p -> Some (`Username_password (u, p))
  in
  let tls = value options.tls mqtt.tls in
  let port =
    value options.port
      (if tls = mqtt.tls then mqtt.port else if tls then 8883 else 1883)
  in
  let client =
    {
      client with
      credentials;
      client_id = value options.client_id client.client_id;
      version = value options.version client.version;
      keep_alive = value options.keep_alive client.keep_alive;
    }
  in
  Mqttz_eio.validate_config client;
  { Mqttz_config.host = value options.host mqtt.host; port; tls; client }

let with_mqtt env config options topic f =
  Eio.Switch.run @@ fun sw ->
  let mqtt = mqtt_config config options in
  let topic = value topic config.Config.owntracks.topic in
  if not (Mqttz.Topic.Filter.validate topic) then
    invalid_arg "Invalid topic filter";
  let connect =
    if mqtt.tls then Mqttz_tls.connect ?authenticator:None
    else Mqttz_eio.connect
  in
  let client =
    connect ~sw ~net:(Eio.Stdenv.net env)
      ~clock:(Eio.Stdenv.mono_clock env)
      ~config:mqtt.client ~host:mqtt.host ~port:mqtt.port ()
  in
  Fun.protect
    ~finally:(fun () -> Mqttz_eio.close client)
    (fun () ->
      Mqttz_eio.subscribe ~qos:`At_least_once client [ topic ];
      let result = f client in
      Mqttz_eio.disconnect client;
      result)

let run f =
  try
    Eio_main.run f;
    0
  with
  | Sys.Break -> 130
  | ex ->
      Format.eprintf "owntracks: %s@." (Printexc.to_string ex);
      1

let listen path options topic count =
  run @@ fun env ->
  if count < 0 then invalid_arg "count must be nonnegative";
  let config = load path in
  with_mqtt env config options topic @@ fun client ->
  let rec loop remaining =
    match Owntracks_eio.receive client with
    | Error error ->
        Format.eprintf "Ignoring message: %s@." error;
        loop remaining
    | Ok message ->
        Format.printf "%a@." Owntracks.Mqtt.pp message;
        if count = 0 || remaining > 1 then loop (remaining - 1)
  in
  loop count

type recorder_options = {
  url : string option;
  user : string option;
  password : string option;
}

let recorder env config options =
  let defaults = config.Config.owntracks.recorder in
  let url =
    match choose options.url defaults.url with
    | None -> invalid_arg "Set --recorder-url or [owntracks.recorder] url"
    | Some url -> url
  in
  let auth =
    match
      ( choose options.user defaults.user,
        choose options.password defaults.password )
    with
    | None, None -> None
    | Some u, Some p -> Some (u, p)
    | _ -> invalid_arg "Recorder authentication needs both user and password"
  in
  Recorder.v ?auth (Fetch_httpz.std ~cookies:`Off env) ~url

let recorder_result = function
  | Ok value -> value
  | Error error -> failwith (Format.asprintf "%a" Recorder.pp_error error)

let list_recorder path options user =
  run @@ fun env ->
  let client = recorder env (load path) options in
  let items =
    with_timeout (Eio.Stdenv.mono_clock env) 30. (fun () ->
        recorder_result
          (match user with
          | None -> Recorder.list_users client
          | Some user -> Recorder.list_devices client ~user))
  in
  List.iter print_endline items

let today () =
  let tm = Unix.gmtime (Unix.gettimeofday ()) in
  Printf.sprintf "%04d-%02d-%02d" (tm.tm_year + 1900) (tm.tm_mon + 1) tm.tm_mday

let check_date date =
  let fail () = invalid_arg "Dates must be valid YYYY-MM-DD values" in
  if String.length date <> 10 || date.[4] <> '-' || date.[7] <> '-' then fail ();
  String.iteri
    (fun i c -> if i <> 4 && i <> 7 && (c < '0' || c > '9') then fail ())
    date;
  let year = int_of_string (String.sub date 0 4)
  and month = int_of_string (String.sub date 5 2)
  and day = int_of_string (String.sub date 8 2) in
  let leap = year mod 4 = 0 && (year mod 100 <> 0 || year mod 400 = 0) in
  let days =
    match month with
    | 2 -> if leap then 29 else 28
    | 4 | 6 | 9 | 11 -> 30
    | 1 | 3 | 5 | 7 | 8 | 10 | 12 -> 31
    | _ -> fail ()
  in
  if year = 0 || day < 1 || day > days then fail ()

let track_feature config device locations =
  let device_name = Config.device_name config device in
  match locations with
  | [] -> invalid_arg "No locations received"
  | [ location ] -> Owntracks.Geojson.point_feature ~device_name location
  | _ -> Owntracks.Geojson.linestring_feature ~device_name locations

let print_geojson json = print_endline (get (Owntracks.Geojson.to_string json))

let geojson path options topic device duration track max_points from_date
    to_date user recorder_options =
  run @@ fun env ->
  if not (duration > 0. && Float.is_finite duration) then
    invalid_arg "duration must be finite and positive";
  if max_points < 1 then invalid_arg "max-points must be positive";
  let config = load path in
  let device = choose device config.owntracks.default_device in
  match from_date with
  | Some from_date ->
      let to_date = value to_date (today ()) in
      check_date from_date;
      check_date to_date;
      if from_date > to_date then invalid_arg "from must not follow to";
      let device = value device "phone" in
      let matches =
        List.filter
          (fun (d : Config.device) -> d.id = device || d.name = device)
          config.owntracks.devices
      in
      let device =
        match matches with
        | [] -> device
        | [ d ] -> d.id
        | _ -> invalid_arg "Ambiguous device name. Use the device ID"
      in
      let client = recorder env config recorder_options in
      let locations =
        with_timeout (Eio.Stdenv.mono_clock env) duration (fun () ->
            Recorder.locations client ~user:(value user "owntracks") ~device
              ~from_date ~to_date
            |> recorder_result)
      in
      if List.length locations > max_points then
        invalid_arg "max-points exceeded";
      print_geojson (track_feature config device locations)
  | None -> (
      if to_date <> None then invalid_arg "--to requires --from";
      with_mqtt env config options topic @@ fun client ->
      let groups = Hashtbl.create 8 and total = ref 0 and first = ref None in
      let rec collect () =
        (match Owntracks_eio.receive client with
        | Error error -> Format.eprintf "Ignoring message: %s@." error
        | Ok message -> (
            match Owntracks.Mqtt.message message with
            | Owntracks.Message.Location loc ->
                let id =
                  value
                    (Owntracks.Mqtt.device message)
                    (Owntracks.Mqtt.topic message)
                in
                let matches =
                  (match device with
                    | None -> true
                    | Some d -> d = id || d = Config.device_name config id)
                  &&
                  match user with
                  | None -> true
                  | Some u -> Owntracks.Mqtt.user message = Some u
                in
                if matches then begin
                  if !total >= max_points then invalid_arg "max-points exceeded";
                  incr total;
                  if track then begin
                    let key = Owntracks.Mqtt.topic message in
                    let previous =
                      value (Hashtbl.find_opt groups key) (id, [])
                    in
                    Hashtbl.replace groups key (id, loc :: snd previous)
                  end
                  else first := Some (track_feature config id [ loc ])
                end
            | _ -> ()));
        if track || !first = None then collect ()
      in
      (try with_timeout (Eio.Stdenv.mono_clock env) duration collect
       with Eio.Time.Timeout -> ());
      if track then begin
        let groups =
          Hashtbl.to_seq groups |> List.of_seq
          |> List.sort (fun (a, _) (b, _) -> String.compare a b)
        in
        let features =
          List.map (fun (_, (id, locs)) -> track_feature config id locs) groups
        in
        match features with
        | [] -> failwith "No locations received before the deadline"
        | [ feature ] -> print_geojson feature
        | features -> print_geojson (Owntracks.Geojson.collection features)
      end
      else
        match !first with
        | None -> failwith "No location received before the deadline"
        | Some feature -> print_geojson feature)

let devices path =
  run @@ fun _env ->
  let config = load path in
  List.iter
    (fun (d : Config.device) -> Printf.printf "%s\t%s\n" d.id d.name)
    config.owntracks.devices

let init path force =
  run @@ fun _env ->
  let path = match path with Some path -> path | None -> default_path () in
  let rec mkdir dir =
    if not (Sys.file_exists dir) then begin
      mkdir (Filename.dirname dir);
      Unix.mkdir dir 0o700
    end
  in
  mkdir (Filename.dirname path);
  let flags =
    [
      Open_wronly;
      Open_creat;
      Open_binary;
      (if force then Open_trunc else Open_excl);
    ]
  in
  let oc = open_out_gen flags 0o600 path in
  Fun.protect
    ~finally:(fun () -> close_out oc)
    (fun () -> output_string oc Config.default_toml);
  print_endline path

let optional names converter doc =
  Arg.(value & opt (some converter) None & info names ~doc)

let flag names doc = Arg.(value & flag & info names ~doc)
let path = optional [ "config" ] Arg.string "Read this TOML configuration file."

let topic =
  Arg.(
    value
    & opt (some string) None
    & info [ "t"; "topic" ]
        ~env:(Cmd.Env.info "OWNTRACKS_TOPIC")
        ~doc:"MQTT topic filter.")

let user =
  optional [ "user" ] Arg.string
    "OwnTracks user (separate from HTTP authentication)."

let device =
  optional [ "d"; "device" ] Arg.string "Device ID or configured display name."

let mqtt_options =
  let make host port tls username password client_id version keep_alive =
    { host; port; tls; username; password; client_id; version; keep_alive }
  in
  Term.(
    const make
    $ optional [ "h"; "host" ] Arg.string "MQTT broker host."
    $ optional [ "p"; "port" ] Arg.int "MQTT broker port."
    $ Arg.(
        value
        & vflag None
            [ (Some true, info [ "tls" ]); (Some false, info [ "no-tls" ]) ])
    $ optional [ "username" ] Arg.string "MQTT username."
    $ optional [ "password" ] Arg.string "MQTT password."
    $ optional [ "client-id" ] Arg.string "MQTT client identifier."
    $ optional [ "mqtt-version" ]
        (Arg.enum [ ("5.0", `V5_0); ("3.1.1", `V3_1_1) ])
        "MQTT version."
    $ optional [ "keep-alive" ] Arg.int "MQTT keepalive seconds.")

let recorder_options =
  Term.(
    const (fun url user password -> { url; user; password })
    $ optional [ "recorder-url" ] Arg.string "Recorder base URL."
    $ optional [ "recorder-user" ] Arg.string "Recorder HTTP Basic username."
    $ optional [ "recorder-password" ] Arg.string
        "Recorder HTTP Basic password.")

let command name doc term = Cmd.v (Cmd.info name ~doc) term

let commands =
  [
    command "listen" "Listen to OwnTracks messages."
      Term.(
        const listen $ path $ mqtt_options $ topic
        $ Arg.(
            value & opt int 0
            & info [ "count" ]
                ~doc:"Stop after N valid messages (0 means unlimited)."));
    command "geojson" "Export a location, live tracks, or Recorder history."
      Term.(
        const geojson $ path $ mqtt_options $ topic $ device
        $ Arg.(
            value & opt float 30.
            & info [ "duration" ] ~doc:"Collection or HTTP timeout in seconds.")
        $ flag [ "track" ] "Collect tracks, grouped by MQTT topic."
        $ Arg.(
            value & opt int 100000
            & info [ "max-points" ] ~doc:"Maximum collected locations.")
        $ optional [ "from" ] Arg.string "Historical start date (YYYY-MM-DD)."
        $ optional [ "to" ] Arg.string
            "Historical end date (defaults to today)."
        $ user $ recorder_options);
    command "recorder" "List Recorder users or a user's devices."
      Term.(const list_recorder $ path $ recorder_options $ user);
    command "devices" "List configured device names."
      Term.(const devices $ path);
    command "init" "Create a default configuration."
      Term.(
        const init $ path
        $ flag [ "f"; "force" ] "Overwrite an existing configuration.");
  ]

let () =
  Sys.catch_break true;
  exit
    (Cmd.eval'
       (Cmd.group
          (Cmd.info "owntracks" ~version:"0.1.0"
             ~doc:"OwnTracks location tracking with mqttz")
          commands))
