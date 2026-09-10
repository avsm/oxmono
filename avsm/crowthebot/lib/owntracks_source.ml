type settings = {
  config_file : string;
  user : string;
  device : string;
  allow_http : bool;
  lookback_days : int;
}

let jsont =
  Jsont.Object.map (fun config_file user device allow_http lookback_days ->
      { config_file; user; device; allow_http; lookback_days })
  |> Jsont.Object.mem "config_file" Jsont.string ~enc:(fun s -> s.config_file)
  |> Jsont.Object.mem "user" Jsont.string ~enc:(fun s -> s.user)
  |> Jsont.Object.mem "device" Jsont.string ~enc:(fun s -> s.device)
  |> Jsont.Object.mem "allow_http" Jsont.bool ~enc:(fun s -> s.allow_http)
  |> Jsont.Object.mem "lookback_days" Jsont.int ~enc:(fun s -> s.lookback_days)
  |> Jsont.Object.error_unknown |> Jsont.Object.finish

let validate s =
  if
    s.config_file = ""
    || Filename.is_relative s.config_file
    || String.length s.config_file > 4096
  then
    invalid_arg "OwnTracks configuration must reference an absolute file path.";
  List.iter Location_store.validate_label [ s.user; s.device ];
  (try ignore (Owntracks.Mqtt.device_topic ~user:s.user ~device:s.device)
   with Invalid_argument _ -> invalid_arg "Invalid OwnTracks user/device ID.");
  if s.lookback_days < 1 || s.lookback_days > 31 then
    invalid_arg "Lookback must be between 1 and 31 days."

let load_config path =
  (* Only configuration and startup code call this reader. Runtime callbacks
     close over the scoped HTTP client and never retain a file capability. *)
  let text =
    try
      Profile.private_file path;
      let ic = open_in_bin path in
      Fun.protect
        ~finally:(fun () -> close_in ic)
        (fun () ->
          let length = in_channel_length ic in
          if length > 1024 * 1024 then invalid_arg "configuration too large";
          really_input_string ic length)
    with _ ->
      invalid_arg
        "Cannot read OwnTracks config. Use an owned regular file with mode \
         0600, at most 1 MiB."
  in
  match Owntracks_config.of_string ~client_id:"crowthebot-config" text with
  | Ok config -> config
  | Error _ ->
      invalid_arg
        "Invalid OwnTracks TOML configuration. Check it with the OwnTracks CLI."

let recorder s (config : Owntracks_config.t) =
  let r = config.owntracks.recorder in
  let url =
    match r.url with
    | Some url -> Tool_config.endpoint ~allow_http:s.allow_http url
    | None ->
        invalid_arg "Set [owntracks.recorder] url in the referenced TOML file."
  in
  let auth =
    match (r.user, r.password) with
    | None, None -> None
    | Some user, Some password ->
        Location_store.validate_label user;
        if
          String.contains user ':' || password = ""
          || String.length password > 16384
          || String.exists
               (fun c -> Char.code c < 32 || Char.code c = 127)
               password
        then
          invalid_arg "Invalid Recorder credentials in OwnTracks configuration.";
        Some (user, password)
    | _ ->
        invalid_arg
          "Recorder authentication needs both user and password in OwnTracks \
           configuration."
  in
  (url, auth)

let configuration =
  let open Cmdliner in
  let term =
    Term.(
      const (fun config_file user device allow_http lookback_days () ->
          let config_file =
            match config_file with
            | Some path -> path
            | None -> Owntracks_config.default_path ()
          in
          let config_file =
            if Filename.is_relative config_file then
              Filename.concat (Sys.getcwd ()) config_file
            else config_file
          in
          let config = load_config config_file in
          let device =
            match Owntracks_config.device_id config device with
            | Ok id -> id
            | Error message -> invalid_arg message
          in
          let s = { config_file; user; device; allow_http; lookback_days } in
          validate s;
          ignore (recorder s config);
          Tool_config.encode jsont s)
      $ Arg.(
          value
          & opt (some string) None
          & info [ "owntracks-config" ] ~docv:"FILE"
              ~doc:
                "Existing OwnTracks TOML. Defaults to the OwnTracks CLI's XDG \
                 config file. Credentials remain in this file.")
      $ Arg.(
          required
          & opt (some string) None
          & info [ "user" ] ~docv:"USER"
              ~doc:
                "Only this tracked OwnTracks user is accessible. Separate from \
                 HTTP authentication.")
      $ Arg.(
          required
          & opt (some string) None
          & info [ "device" ] ~docv:"DEVICE"
              ~doc:
                "Only this device is accessible. Accepts a device ID or a \
                 configured OwnTracks display name.")
      $ Arg.(
          value & flag
          & info [ "allow-http" ]
              ~doc:
                "Allow HTTP to the trusted Recorder in the referenced config, \
                 including credentials.")
      $ Arg.(
          value & opt int 7
          & info [ "lookback-days" ]
              ~doc:"Bound location queries to 1 to 31 days. Default 7."))
  in
  Tool_config.v ~name:"owntracks"
    ~doc:"Link an existing OwnTracks config and allow one user/device." term

type t = {
  user : string;
  device : string;
  latest : unit -> Location_store.point option;
  history : from:float -> until:float -> Location_store.point list;
  map : Overpass.t;
}

let user t = t.user
let device t = t.device
let permits t ~user ~device = t.user = user && t.device = device
let latest t = t.latest ()
let history t ~from ~until = t.history ~from ~until
let resolve t request = Overpass.resolve t.map request

let initialize ~load ~fetch ~clock ~now json =
  let s =
    try Tool_config.decode jsont json
    with Invalid_argument _ ->
      invalid_arg
        "OwnTracks needs a config reference and an allowed user/device. \
         Replace the entry with config owntracks set NAME --user USER --device \
         DEVICE."
  in
  validate s;
  let config : Owntracks_config.t = load s.config_file in
  let map = Overpass.create ~config:config.owntracks.overpass ~fetch ~clock in
  let url, auth = recorder s config in
  let url = if String.ends_with ~suffix:"/" url then url else url ^ "/" in
  let user = s.user and device = s.device in
  let fetch =
    Fetch.restrict ~methods:[ `GET ]
      ~under:[ url ^ "api/0/locations" ]
      ~filter:(fun request ->
        let uri =
          Uri.of_string (Fetch.Middleware.Url.path_and_query request.url)
        in
        let query = Uri.query uri in
        let exactly key expected =
          List.filter (fun (k, _) -> k = key) query = [ (key, [ expected ]) ]
        in
        if
          Uri.path uri = Uri.path (Uri.of_string url) ^ "api/0/locations"
          && exactly "user" user && exactly "device" device
          && List.for_all
               (fun (key, values) ->
                 List.mem key [ "user"; "device"; "from"; "to" ]
                 && List.length values = 1)
               query
        then `Allow
        else `Reject "Only the configured OwnTracks user/device is allowed.")
      fetch
  in
  let bounded f =
    let result =
      try Eio.Time.Timeout.run_exn (Eio.Time.Timeout.seconds clock 20.) f with
      | Eio.Cancel.Cancelled _ as exn -> raise exn
      | _ -> invalid_arg "Recorder request failed or timed out."
    in
    match result with
    | Ok value -> value
    | Error (Owntracks_recorder_client.Http_status status) ->
        invalid_arg (Printf.sprintf "Recorder returned HTTP %d." status)
    | Error (Owntracks_recorder_client.Invalid_response _) ->
        invalid_arg "Recorder returned an invalid response."
  in
  let lookback_days = s.lookback_days in
  let date seconds =
    match Ptime.of_float_s seconds with
    | None -> invalid_arg "Location query date is out of range."
    | Some time ->
        let y, m, d = Ptime.to_date time in
        Printf.sprintf "%04d-%02d-%02d" y m d
  in
  let points ~current ~from ~until =
    let from_date = date from
    and to_date = date (min until current +. 86400.) in
    let fetch =
      Fetch.restrict
        ~filter:(fun request ->
          let uri =
            Uri.of_string (Fetch.Middleware.Url.path_and_query request.url)
          in
          if
            Uri.get_query_param uri "from" = Some from_date
            && Uri.get_query_param uri "to" = Some to_date
          then `Allow
          else
            `Reject "Recorder redirects cannot change the requested time range.")
        fetch
    in
    let client =
      Owntracks_recorder_client.v ~max_response:(1024 * 1024) ?auth fetch ~url
    in
    bounded (fun () ->
        Owntracks_recorder_client.locations client ~user ~device ~from_date
          ~to_date)
    |> List.filter_map (fun location ->
        let network_text value =
          Option.bind value (fun text ->
              if text = "" || String.length text > 256 then None else Some text)
        in
        let point =
          Location_store.
            {
              latitude = Owntracks.Location.lat location;
              longitude = Owntracks.Location.lon location;
              accuracy = Owntracks.Location.acc location;
              recorded_at = float_of_int (Owntracks.Location.tst location);
              reported_at =
                Option.map float_of_int (Owntracks.Location.created_at location);
              ssid = network_text (Owntracks.Location.ssid location);
              bssid = network_text (Owntracks.Location.bssid location);
              conn = network_text (Owntracks.Location.conn location);
            }
        in
        if
          Location_store.valid_point ~now:current point
          && point.recorded_at >= from && point.recorded_at <= until
        then Some point
        else None)
    |> List.sort_uniq (fun (a : Location_store.point) b ->
        compare
          ( a.recorded_at,
            a.reported_at,
            a.latitude,
            a.longitude,
            a.accuracy,
            a.ssid,
            a.bssid,
            a.conn )
          ( b.recorded_at,
            b.reported_at,
            b.latitude,
            b.longitude,
            b.accuracy,
            b.ssid,
            b.bssid,
            b.conn ))
  in
  {
    user;
    device;
    map;
    history =
      (fun ~from ~until ->
        let current = now () in
        if
          (not (Float.is_finite from && Float.is_finite until))
          || from < 0. || from > until || until > current
          || from < current -. (float_of_int lookback_days *. 86400.)
        then
          invalid_arg
            (Printf.sprintf
               "History needs from <= to <= now, within the last %d days."
               lookback_days);
        points ~current ~from ~until);
    latest =
      (fun () ->
        let current = now () in
        let from = current -. (float_of_int lookback_days *. 86400.) in
        points ~current ~from ~until:(current +. 300.)
        |> List.fold_left
             (fun latest point ->
               match latest with
               | Some old
                 when Location_store.report_time old
                      > Location_store.report_time point ->
                   latest
               | _ -> Some point)
             None);
  }
