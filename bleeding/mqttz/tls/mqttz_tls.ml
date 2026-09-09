let connect ?(authenticator : X509.Authenticator.t option @ portable)
    ~sw ~net ~clock ~config ~host ~port () =
  Mqttz_eio.validate_config config;
  if host = "" || port < 1 || port > 65535 then
    invalid_arg "invalid MQTT TLS endpoint";
  let authenticator = match authenticator with
    | Some authenticator -> authenticator
    | None -> (match Ca_certs.system_authenticator () with
        | Ok authenticator -> authenticator
        | Error (`Msg message) -> failwith message)
  in
  let peer_name, ip = match Ipaddr.of_string host with
    | Ok ip -> None, Some ip
    | Error _ -> Some (Domain_name.host_exn (Domain_name.of_string_exn host)), None
  in
  let tls = match Tls.Config.client_no_cert ~authenticator ?peer_name ?ip () with
    | Ok tls -> tls
    | Error (`Msg message) -> invalid_arg message
  in
  Eio.Time.Timeout.run_exn
    (Eio.Time.Timeout.seconds clock config.operation_timeout) (fun () ->
      let rec attempt = function
        | [] -> invalid_arg "MQTT TLS host has no addresses"
        | address :: rest ->
            (try Eio.Net.connect ~sw net address with
             | Eio.Io _ when rest <> [] -> attempt rest)
      in
      let socket = attempt
        (Eio.Net.getaddrinfo_stream net host ~service:(string_of_int port)) in
      try
        let g = Mirage_crypto_rng_unix.fresh_generator () in
        let flow = Tls_eio.client_of_flow_with_rng ~g tls socket in
        Mqttz_eio.of_flow ~sw ~clock ~config flow
      with ex -> Eio.Resource.close socket; raise ex)
