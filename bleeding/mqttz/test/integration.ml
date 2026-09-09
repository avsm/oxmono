module C = Mqttz_eio
module S = Mqttz.Slice
module P = Mqttz.V5.Property

let port name = int_of_string (Sys.getenv name)
let require condition message = if not condition then failwith message
let equal expected actual = require (expected = actual) "payload differs"

let exercise env version =
  Eio.Switch.run (fun sw ->
      let clock = Eio.Stdenv.mono_clock env and net = Eio.Stdenv.net env in
      let suffix = Mqttz.Protocol_version.to_string version in
      let config id =
        {
          (C.default_config ~client_id:("mqttz-" ^ suffix ^ id)) with
          version;
          keep_alive = 2;
          operation_timeout = 5.;
        }
      in
      let connect config =
        C.connect ~sw ~net ~clock ~config ~host:"127.0.0.1"
          ~port:(port "MQTTZ_PORT") ()
      in
      let publisher = connect (config "publisher") in
      let subscriber = connect (config "subscriber") in
      let root = "mqttz/" ^ suffix in
      C.subscribe ~qos:`Exactly_once subscriber [ root ^ "/+" ];
      List.iter
        (fun qos ->
          let payload = "binary\000\255" ^ Mqttz.Qos.to_string qos in
          let properties =
            if version = `V3_1_1 then []
            else
              [
                P.User_property ("source", "mqttz");
                P.Content_type "application/octet-stream";
                P.Correlation_data "\000\255";
              ]
          in
          C.publish ~qos ~properties publisher ~topic:(root ^ "/binary")
            (S.of_string payload);
          let message = C.receive subscriber in
          equal payload (S.to_string message.payload);
          require (message.qos = qos) "wrong delivered QoS";
          if version = `V5_0 then
            require
              (List.mem (P.Correlation_data "\000\255") message.properties)
              "lost binary correlation data")
        [ `At_most_once; `At_least_once; `Exactly_once ];
      (* Retention of a received view across subsequent reads. *)
      C.publish ~qos:`At_least_once publisher ~topic:(root ^ "/first")
        (S.of_string "first");
      let first = C.receive subscriber in
      let big = Bytes.init (1024 * 1024) (fun i -> Char.chr (i land 255)) in
      C.publish ~qos:`Exactly_once publisher ~topic:(root ^ "/large")
        (S.make big);
      let message = C.receive subscriber in
      equal (Bytes.to_string big) (S.to_string message.payload);
      equal "first" (S.to_string first.payload);
      C.unsubscribe subscriber [ root ^ "/+" ];
      (* A retained publication sent before subscription must be delivered on subscribe. *)
      C.publish ~retain:true ~qos:`At_least_once publisher
        ~topic:(root ^ "/retained") (S.of_string "retained");
      C.subscribe ~qos:`At_least_once subscriber [ root ^ "/retained" ];
      let retained = C.receive subscriber in
      equal "retained" (S.to_string retained.payload);
      require retained.retain "retained delivery did not set RETAIN";
      C.unsubscribe subscriber [ root ^ "/retained" ];
      C.publish ~retain:true ~qos:`At_least_once publisher
        ~topic:(root ^ "/retained") S.empty;
      (* Keep-alive must keep an otherwise idle connection alive. *)
      Eio.Time.Mono.sleep clock 3.2;
      C.publish ~qos:`At_least_once publisher ~topic:(root ^ "/alive") S.empty;
      require (C.is_connected subscriber) "subscriber keep-alive failed";
      let will_topic = root ^ "/will" in
      C.subscribe ~qos:`At_least_once subscriber [ will_topic ];
      let will =
        Mqttz.Will.create ~topic:will_topic ~payload:"gone" ~qos:`At_least_once
          ~retain:false
      in
      let doomed = connect { (config "will") with will = Some will } in
      C.close doomed;
      equal "gone" (S.to_string (C.receive subscriber).payload);
      C.disconnect subscriber;
      C.disconnect publisher;
      Printf.printf
        "PASS MQTT %s: QoS 0/1/2, binary properties, 1MiB, retain, \
         unsubscribe, keep-alive, Will\n\
         %!"
        suffix)

let authentication env =
  Eio.Switch.run (fun sw ->
      let net = Eio.Stdenv.net env and clock = Eio.Stdenv.mono_clock env in
      let config =
        {
          (C.default_config ~client_id:"mqttz-auth") with
          operation_timeout = 3.;
        }
      in
      let connect config =
        C.connect ~sw ~net ~clock ~config ~host:"127.0.0.1"
          ~port:(port "MQTTZ_AUTH_PORT") ()
      in
      (match connect config with
      | client ->
          C.close client;
          failwith "anonymous connection accepted"
      | exception C.Rejected _ -> ());
      (match
         connect
           {
             config with
             credentials = Some (`Username_password ("mqttz", "wrong"));
           }
       with
      | client ->
          C.close client;
          failwith "wrong password accepted"
      | exception C.Rejected _ -> ());
      let client =
        connect
          {
            config with
            credentials = Some (`Username_password ("mqttz", "mqttz-test"));
          }
      in
      C.publish ~qos:`Exactly_once client ~topic:"authenticated" S.empty;
      C.disconnect client;
      print_endline "PASS password authentication and refusal")

let tls env =
  let pem =
    In_channel.with_open_bin (Sys.getenv "MQTTZ_CA_FILE") In_channel.input_all
  in
  let certificate =
    match X509.Certificate.decode_pem pem with
    | Ok certificate -> certificate
    | Error (`Msg e) -> failwith e
  in
  let authenticator =
    X509.Authenticator.chain_of_trust_no_crl
      ~time:(fun () -> Some (Ptime_clock.now ()))
      [ certificate ]
  in
  Eio.Switch.run (fun sw ->
      let net = Eio.Stdenv.net env and clock = Eio.Stdenv.mono_clock env in
      let config =
        {
          (C.default_config ~client_id:"mqttz-tls") with
          operation_timeout = 3.;
        }
      in
      let client =
        Mqttz_tls.connect ~authenticator ~sw ~net ~clock ~config
          ~host:"127.0.0.1" ~port:(port "MQTTZ_TLS_PORT") ()
      in
      C.subscribe ~qos:`Exactly_once client [ "mqttz/tls" ];
      C.publish ~qos:`Exactly_once client ~topic:"mqttz/tls"
        (S.of_string "secure");
      equal "secure" (S.to_string (C.receive client).payload);
      C.disconnect client;
      let dns_client =
        Mqttz_tls.connect ~authenticator ~sw ~net ~clock ~config
          ~host:"localhost" ~port:(port "MQTTZ_TLS_PORT") ()
      in
      C.disconnect dns_client;
      (match
         Mqttz_tls.connect ~sw ~net ~clock ~config ~host:"127.0.0.1"
           ~port:(port "MQTTZ_TLS_PORT") ()
       with
      | c ->
          C.close c;
          failwith "untrusted certificate accepted"
      | exception (Tls_eio.Tls_failure _ | Tls_eio.Tls_alert _) -> ());
      print_endline
        "PASS verified TLS DNS/IP names, QoS2 and rejection of untrusted \
         certificate")

let oracle env =
  Eio.Switch.run (fun sw ->
      let client =
        C.connect ~sw ~net:(Eio.Stdenv.net env)
          ~clock:(Eio.Stdenv.mono_clock env)
          ~host:"127.0.0.1" ~port:(port "MQTTZ_PORT")
          ~config:(C.default_config ~client_id:"mqttz-oracle")
          ()
      in
      C.subscribe ~qos:`Exactly_once client [ "mqttz/oracle/in" ];
      equal "from-mosquitto" (S.to_string (C.receive client).payload);
      C.publish ~qos:`Exactly_once ~retain:true client ~topic:"mqttz/oracle/out"
        (S.of_string "from-mqttz");
      C.disconnect client)

let () =
  Eio_main.run (fun env ->
      Eio.Time.Timeout.run_exn
        (Eio.Time.Timeout.seconds (Eio.Stdenv.mono_clock env) 60.)
        (fun () ->
          exercise env `V3_1_1;
          exercise env `V5_0;
          authentication env;
          tls env;
          oracle env))
