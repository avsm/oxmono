let get = function Ok value -> value | Error error -> failwith error

let () =
  Eio_main.run @@ fun env ->
  let port = int_of_string (Sys.getenv "MQTTZ_PORT") in
  List.iter
    (fun version ->
      Eio.Switch.run @@ fun sw ->
      let connect client_id =
        let config = { (Mqttz_eio.default_config ~client_id) with version } in
        Mqttz_eio.connect ~sw ~net:(Eio.Stdenv.net env)
          ~clock:(Eio.Stdenv.mono_clock env)
          ~config ~host:"127.0.0.1" ~port ()
      in
      let subscriber = connect "owntracks-test-sub" in
      let publisher = connect "owntracks-test-pub" in
      let topic = Owntracks.Mqtt.device_topic ~user:"alice" ~device:"phone" in
      Mqttz_eio.subscribe ~qos:`Exactly_once subscriber [ topic ];
      List.iteri
        (fun i qos ->
          let loc =
            Owntracks.Location.v ~tst:i ~lat:51.5 ~lon:(-0.1) ~tid:"ab" ()
          in
          Owntracks_eio.publish ~qos publisher ~topic
            (Owntracks.Message.Location loc);
          let message = get (Owntracks_eio.receive subscriber) in
          if Owntracks.Mqtt.user message <> Some "alice" then
            failwith "wrong user";
          match Owntracks.Mqtt.message message with
          | Owntracks.Message.Location received ->
              if
                Owntracks.Location.tst received <> i
                || Owntracks.Location.topic received <> Some topic
              then failwith "wrong decoded location"
          | _ -> failwith "wrong message type")
        [ `At_most_once; `At_least_once; `Exactly_once ];
      Mqttz_eio.publish publisher ~topic (Mqttz.Slice.of_string "bad JSON");
      (match Owntracks_eio.receive subscriber with
      | Error _ -> ()
      | Ok _ -> failwith "malformed JSON accepted");
      Owntracks_eio.publish publisher ~topic
        (Owntracks.Message.Lwt (Owntracks.Lwt.v ~tst:1));
      ignore (get (Owntracks_eio.receive subscriber));
      Mqttz_eio.disconnect publisher;
      Mqttz_eio.disconnect subscriber;
      Printf.printf
        "PASS OwnTracks MQTT %s QoS 0/1/2 and malformed-message recovery\n%!"
        (Mqttz.Protocol_version.to_string version))
    [ `V3_1_1; `V5_0 ]
