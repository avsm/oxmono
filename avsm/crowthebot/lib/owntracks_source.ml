type settings = {
  url : string;
  username : string option;
  password : string option;
  allow_http : bool;
  lookback_days : int;
}

let jsont =
  Jsont.Object.map (fun url username password allow_http lookback_days ->
      { url; username; password; allow_http; lookback_days })
  |> Jsont.Object.mem "url" Jsont.string ~enc:(fun s -> s.url)
  |> Jsont.Object.mem "username" (Jsont.option Jsont.string) ~enc:(fun s ->
      s.username)
  |> Jsont.Object.mem "password" (Jsont.option Jsont.string) ~enc:(fun s ->
      s.password)
  |> Jsont.Object.mem "allow_http" Jsont.bool ~enc:(fun s -> s.allow_http)
  |> Jsont.Object.mem "lookback_days" Jsont.int ~enc:(fun s -> s.lookback_days)
  |> Jsont.Object.finish

let validate s =
  ignore (Tool_config.endpoint ~allow_http:s.allow_http s.url);
  if s.lookback_days < 1 || s.lookback_days > 31 then
    invalid_arg "Lookback must be between 1 and 31 days.";
  match (s.username, s.password) with
  | None, None -> ()
  | Some user, Some password ->
      Location_store.validate_label user;
      if
        String.contains user ':' || password = ""
        || String.length password > 16384
        || String.exists
             (fun c -> Char.code c < 32 || Char.code c = 127)
             password
      then invalid_arg "Invalid Recorder credentials."
  | _ -> invalid_arg "Recorder authentication needs both username and password."

let configuration =
  let open Cmdliner in
  let term =
    Term.(
      const
        (fun url username password_file anonymous allow_http lookback_days () ->
          if anonymous && (username <> None || password_file <> None) then
            invalid_arg
              "Choose --anonymous or username/password authentication.";
          if (not anonymous) && username = None then
            invalid_arg "Supply --username or --anonymous.";
          ignore (Tool_config.endpoint ~allow_http url);
          let password =
            if anonymous then None
            else
              Some
                (Tool_config.secret ~label:"OwnTracks password" password_file)
          in
          let s = { url; username; password; allow_http; lookback_days } in
          validate s;
          Tool_config.encode jsont s)
      $ Arg.(
          required
          & opt (some string) None
          & info [ "url" ] ~docv:"URL" ~doc:"Recorder base URL, before api/0/.")
      $ Arg.(
          value
          & opt (some string) None
          & info [ "username" ] ~doc:"HTTP Basic username.")
      $ Arg.(
          value
          & opt (some string) None
          & info [ "password-file" ]
              ~doc:"0600 password file. Otherwise prompt without echo.")
      $ Arg.(
          value & flag
          & info [ "anonymous" ] ~doc:"Recorder needs no authentication.")
      $ Arg.(
          value & flag
          & info [ "allow-http" ]
              ~doc:
                "Allow HTTP for this trusted endpoint, including credentials.")
      $ Arg.(
          value & opt int 7
          & info [ "lookback-days" ]
              ~doc:"Bound location queries to 1 to 31 days. Default 7."))
  in
  Tool_config.v ~name:"owntracks" ~doc:"Named OwnTracks Recorder connections."
    term

type t = {
  users : unit -> string list;
  devices : user:string -> string list;
  latest : user:string -> device:string -> Location_store.point option;
}

let users t = t.users ()
let devices t ~user = t.devices ~user
let latest t ~user ~device = t.latest ~user ~device

let initialize ~fetch ~clock ~now json =
  let s = Tool_config.decode jsont json in
  validate s;
  let url = if String.ends_with ~suffix:"/" s.url then s.url else s.url ^ "/" in
  let fetch =
    Fetch.restrict ~methods:[ `GET ]
      ~under:[ url ^ "api/0/list"; url ^ "api/0/locations" ]
      ~filter:(fun request ->
        let path =
          Uri.path
            (Uri.of_string (Fetch.Middleware.Url.path_and_query request.url))
        in
        let base = Uri.path (Uri.of_string url) in
        if List.mem path [ base ^ "api/0/list"; base ^ "api/0/locations" ] then
          `Allow
        else `Reject "Only Recorder list and location endpoints are allowed.")
      fetch
  in
  let auth =
    Option.bind s.username (fun user ->
        Option.map (fun pass -> (user, pass)) s.password)
  in
  let client =
    Owntracks_recorder_client.v ~max_response:(1024 * 1024) ?auth fetch ~url
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
  let labels values =
    List.filter
      (fun value ->
        try
          Location_store.validate_label value;
          true
        with Invalid_argument _ -> false)
      values
    |> List.sort_uniq String.compare
  in
  let lookback_days = s.lookback_days in
  {
    users =
      (fun () ->
        bounded (fun () -> Owntracks_recorder_client.list_users client)
        |> labels);
    devices =
      (fun ~user ->
        Location_store.validate_label user;
        bounded (fun () -> Owntracks_recorder_client.list_devices client ~user)
        |> labels);
    latest =
      (fun ~user ~device ->
        List.iter Location_store.validate_label [ user; device ];
        let current = now () in
        let from = current -. (float_of_int lookback_days *. 86400.) in
        let date seconds =
          match Ptime.of_float_s seconds with
          | None -> invalid_arg "Location query date is out of range."
          | Some time ->
              let y, m, d = Ptime.to_date time in
              Printf.sprintf "%04d-%02d-%02d" y m d
        in
        let values =
          bounded (fun () ->
              Owntracks_recorder_client.locations client ~user ~device
                ~from_date:(date from)
                ~to_date:(date (current +. 86400.)))
        in
        List.fold_left
          (fun best location ->
            let p =
              Location_store.
                {
                  latitude = Owntracks.Location.lat location;
                  longitude = Owntracks.Location.lon location;
                  accuracy = Owntracks.Location.acc location;
                  recorded_at = float_of_int (Owntracks.Location.tst location);
                }
            in
            if
              (not (Location_store.valid_point ~now:current p))
              || p.recorded_at < from
            then best
            else
              match best with
              | Some old when old.Location_store.recorded_at >= p.recorded_at ->
                  best
              | _ -> Some p)
          None values);
  }
