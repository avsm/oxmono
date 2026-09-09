module Client = Mqttz_eio
module P = Mqttz.V5.Packet
module S = Mqttz.Slice

let read flow reader =
  let first = Eio.Buf_read.any_char reader in
  let header = Buffer.create 5 in
  Buffer.add_char header first;
  let rec length shift acc =
    let byte = Eio.Buf_read.any_char reader in
    Buffer.add_char header byte;
    let n = Char.code byte in
    let acc = acc lor ((n land 127) lsl shift) in
    if n land 128 = 0 then acc else length (shift + 7) acc
  in
  let n = length 0 0 in
  let body = Eio.Buf_read.take n reader in
  ignore flow;
  match P.decode (S.of_string (Buffer.contents header ^ body)) with
  | Ok packet -> packet
  | Error message -> Alcotest.fail message

let write flow packet =
  List.iter
    (fun slice -> Eio.Flow.copy_string (S.to_string slice) flow)
    (P.encode packet)

let setup ?(properties = []) env server client =
  Eio.Switch.run (fun sw ->
      let a, b = Eio_unix.Net.socketpair_stream ~sw () in
      let clock = Eio.Stdenv.mono_clock env in
      Eio.Time.Timeout.run_exn (Eio.Time.Timeout.seconds clock 3.) (fun () ->
          Eio.Fiber.both
            (fun () ->
              let reader = Eio.Buf_read.of_flow b ~max_size:65536 in
              (match read b reader with
              | P.Connect _ -> ()
              | _ -> Alcotest.fail "expected CONNECT");
              write b
                (P.Connack
                   {
                     session_present = false;
                     reason_code = `Success;
                     properties;
                   });
              server b (fun () -> read b reader))
            (fun () ->
              let config =
                {
                  (Client.default_config ~client_id:"test") with
                  keep_alive = 0;
                  operation_timeout = 0.1;
                }
              in
              let t = Client.of_flow ~sw ~clock ~config a in
              client t clock)))

let published read =
  match read () with
  | P.Publish { packet_id = Some id; _ } -> id
  | _ -> Alcotest.fail "expected PUBLISH"

let disconnected read =
  match read () with
  | P.Disconnect _ -> ()
  | _ -> Alcotest.fail "expected flushed DISCONNECT"

let expect_exception f =
  match f () with
  | () -> Alcotest.fail "expected exception"
  | exception
      (Client.Closed | End_of_file | Client.Protocol_error _ | Eio.Time.Timeout)
    ->
      ()

let test_drop env () =
  setup env
    (fun flow read ->
      ignore (published read);
      Eio.Resource.close flow)
    (fun t _ ->
      expect_exception (fun () ->
          Client.publish ~qos:`At_least_once t ~topic:"a"
            (S.of_string "payload"));
      Alcotest.(check bool) "closed after EOF" false (Client.is_connected t))

let test_timeout env () =
  setup env
    (fun _ read ->
      ignore (published read);
      match read () with
      | _ -> Alcotest.fail "expected connection close"
      | exception End_of_file -> ())
    (fun t _ ->
      (match
         Client.publish ~qos:`At_least_once t ~topic:"a" (S.of_string "x")
       with
      | () -> Alcotest.fail "missing operation timeout"
      | exception Eio.Time.Timeout -> ());
      Alcotest.(check bool)
        "timeout closes ambiguous exchange" false (Client.is_connected t))

let test_rejection env () =
  setup env
    (fun flow read ->
      let id = published read in
      write flow
        (P.Puback
           { packet_id = id; reason_code = `Not_authorized; properties = [] });
      let id = published read in
      write flow
        (P.Puback { packet_id = id; reason_code = `Success; properties = [] });
      disconnected read)
    (fun t _ ->
      (match Client.publish ~qos:`At_least_once t ~topic:"a" S.empty with
      | () -> Alcotest.fail "ignored rejection"
      | exception Client.Rejected _ -> ());
      Client.publish ~qos:`At_least_once t ~topic:"a" S.empty;
      Client.disconnect t)

let test_qos2_duplicate env () =
  setup env
    (fun flow read ->
      let send dup =
        write flow
          (P.Publish
             {
               topic = "a";
               packet_id = Some 7;
               qos = `Exactly_once;
               dup;
               retain = false;
               properties = [];
               payload = S.of_string "once";
             })
      in
      let pubrec () =
        match read () with
        | P.Pubrec { packet_id = 7; _ } -> ()
        | _ -> Alcotest.fail "expected PUBREC"
      in
      send false;
      pubrec ();
      send true;
      pubrec ();
      write flow
        (P.Pubrel { packet_id = 7; reason_code = `Success; properties = [] });
      (match read () with
      | P.Pubcomp { packet_id = 7; reason_code = `Success; _ } -> ()
      | _ -> Alcotest.fail "expected PUBCOMP");
      write flow
        (P.Pubrel { packet_id = 7; reason_code = `Success; properties = [] });
      (match read () with
      | P.Pubcomp
          { packet_id = 7; reason_code = `Packet_identifier_not_found; _ } ->
          ()
      | _ -> Alcotest.fail "expected id-not-found PUBCOMP");
      disconnected read)
    (fun t clock ->
      let message = Client.receive t in
      Alcotest.(check string)
        "first delivery" "once"
        (S.to_string message.payload);
      (match
         Eio.Time.Timeout.run_exn (Eio.Time.Timeout.seconds clock 0.05)
           (fun () -> Client.receive t)
       with
      | _ -> Alcotest.fail "duplicate QoS2 delivery"
      | exception Eio.Time.Timeout -> ());
      Client.disconnect t)

let test_malformed env () =
  setup env
    (fun flow read ->
      Eio.Flow.copy_string "\209\000" flow;
      match read () with
      | _ -> Alcotest.fail "expected close on malformed packet"
      | exception End_of_file -> ())
    (fun t _ -> expect_exception (fun () -> ignore (Client.receive t)))

let test_suback env () =
  setup env
    (fun flow read ->
      (match read () with
      | P.Subscribe s ->
          write flow
            (P.Suback
               {
                 packet_id = s.packet_id;
                 properties = [];
                 reason_codes = [ `Not_authorized ];
               })
      | _ -> Alcotest.fail "expected SUBSCRIBE");
      disconnected read)
    (fun t _ ->
      (match Client.subscribe t [ "denied" ] with
      | () -> Alcotest.fail "ignored SUBACK rejection"
      | exception Client.Rejected _ -> ());
      Client.disconnect t)

let test_cancel env () =
  setup env
    (fun _ read ->
      ignore (published read);
      match read () with
      | _ -> Alcotest.fail "expected cancellation close"
      | exception End_of_file -> ())
    (fun t clock ->
      (match
         Eio.Time.Timeout.run_exn (Eio.Time.Timeout.seconds clock 0.01)
           (fun () -> Client.publish ~qos:`Exactly_once t ~topic:"a" S.empty)
       with
      | () -> Alcotest.fail "expected cancellation"
      | exception Eio.Time.Timeout -> ());
      Alcotest.(check bool)
        "cancel closes exchange" false (Client.is_connected t))

let test_negotiated_limits env () =
  let module Property = Mqttz.V5.Property in
  setup
    ~properties:
      [
        Property.Maximum_packet_size 32l;
        Property.Maximum_qos `At_most_once;
        Property.Retain_available false;
      ]
    env
    (fun _ read ->
      (match read () with
      | P.Publish { packet_id = None; _ } -> ()
      | _ -> Alcotest.fail "expected QoS 0 PUBLISH");
      disconnected read)
    (fun t _ ->
      let invalid f =
        match f () with
        | () -> Alcotest.fail "negotiated limit ignored"
        | exception Invalid_argument _ -> ()
      in
      invalid (fun () ->
          Client.publish t ~topic:"a" (S.make (Bytes.make 64 'x')));
      invalid (fun () ->
          Client.publish ~qos:`At_least_once t ~topic:"a" S.empty);
      invalid (fun () -> Client.publish ~retain:true t ~topic:"a" S.empty);
      Alcotest.(check bool)
        "validation preserves connection" true (Client.is_connected t);
      Client.publish t ~topic:"a" S.empty;
      Client.disconnect t)

let () =
  Eio_main.run (fun env ->
      Alcotest.run "mqttz client"
        [
          ( "faults",
            List.map
              (fun (name, f) -> Alcotest.test_case name `Quick (f env))
              [
                ("broker EOF", test_drop);
                ("operation timeout", test_timeout);
                ("negative PUBACK", test_rejection);
                ("duplicate QoS2", test_qos2_duplicate);
                ("malformed server packet", test_malformed);
                ("negative SUBACK", test_suback);
                ("cancelled publish", test_cancel);
                ("negotiated broker limits", test_negotiated_limits);
              ] );
        ])
