module S = Mqttz.Slice
module V3 = Mqttz.V3.Packet
module V5 = Mqttz.V5.Packet
module P = Mqttz.V5.Property

let check = Alcotest.(check bool)

let decode f bytes =
  match f (S.make bytes) with Ok p -> p | Error e -> Alcotest.fail e

let rejects f text =
  match f (S.of_string text) with
  | Error _ -> ()
  | Ok _ -> Alcotest.failf "accepted malformed packet %S" text

let rt3 p =
  let bytes = V3.to_bytes p in
  let result = decode V3.decode bytes in
  Alcotest.(check bytes) "v3 wire roundtrip" bytes (V3.to_bytes result)

let rt5 p =
  let bytes = V5.to_bytes p in
  let result = decode V5.decode bytes in
  Alcotest.(check bytes) "v5 wire roundtrip" bytes (V5.to_bytes result)

let test_packets () =
  let will =
    Mqttz.Will.create ~topic:"will" ~payload:"\000\255" ~qos:`Exactly_once
      ~retain:true
  in
  List.iter rt3
    [
      V3.Connect
        {
          client_id = "client";
          clean_session = true;
          keep_alive = 60;
          credentials = Some (`Username_password ("user", "\000\255"));
          will = Some will;
        };
      V3.Connack { session_present = false; return_code = `Accepted };
      V3.Puback 1;
      V3.Pubrec 2;
      V3.Pubrel 3;
      V3.Pubcomp 4;
      V3.Subscribe
        { packet_id = 5; topics = [ { filter = "a/+"; qos = `Exactly_once } ] };
      V3.Suback { packet_id = 5; return_codes = [ `Granted_qos_2; `Failure ] };
      V3.Unsubscribe { packet_id = 6; topics = [ "a/#" ] };
      V3.Unsuback 6;
      V3.Pingreq;
      V3.Pingresp;
      V3.Disconnect;
    ];
  List.iter rt5
    [
      V5.Connect
        {
          client_id = "client";
          clean_start = true;
          keep_alive = 60;
          credentials = Some (`Password "\000\255");
          properties =
            [
              P.Receive_maximum 16;
              P.Maximum_packet_size 65536l;
              P.Authentication_method "test";
              P.Authentication_data "\000\255";
            ];
          will =
            Some
              {
                will_topic = "will";
                will_payload = "\000\255";
                will_qos = `Exactly_once;
                will_retain = true;
                will_properties = [ P.Will_delay_interval 1l ];
              };
        };
      V5.Connack
        {
          session_present = false;
          reason_code = `Success;
          properties =
            [
              P.Assigned_client_identifier "assigned";
              P.Maximum_qos `At_least_once;
            ];
        };
      V5.Puback
        {
          packet_id = 1;
          reason_code = `No_matching_subscribers;
          properties = [];
        };
      V5.Pubrec { packet_id = 2; reason_code = `Success; properties = [] };
      V5.Pubrel { packet_id = 3; reason_code = `Success; properties = [] };
      V5.Pubcomp
        {
          packet_id = 4;
          reason_code = `Packet_identifier_not_found;
          properties = [];
        };
      V5.Subscribe
        {
          packet_id = 5;
          properties = [ P.Subscription_identifier 12345 ];
          topics =
            [
              {
                filter = "a/+";
                options = Mqttz.V5.Subscription_options.default `Exactly_once;
              };
            ];
        };
      V5.Suback
        {
          packet_id = 5;
          properties = [];
          reason_codes = [ `Granted_qos_2; `Not_authorized ];
        };
      V5.Unsubscribe { packet_id = 6; properties = []; topics = [ "a/#" ] };
      V5.Unsuback
        {
          packet_id = 6;
          properties = [];
          reason_codes = [ `No_subscription_existed ];
        };
      V5.Pingreq;
      V5.Pingresp;
      V5.Disconnect { reason_code = `Normal_disconnection; properties = [] };
      V5.Auth
        {
          reason_code = `Continue_authentication;
          properties =
            [ P.Authentication_method "test"; P.Authentication_data "\000\255" ];
        };
    ]

let test_publish () =
  List.iter
    (fun n ->
      let payload = S.make (Bytes.init n (fun i -> Char.chr (i land 255))) in
      List.iter
        (fun qos ->
          let packet_id = if qos = `At_most_once then None else Some 65535 in
          rt3
            (V3.Publish
               {
                 topic = "a/b";
                 qos;
                 packet_id;
                 payload;
                 dup = false;
                 retain = true;
               });
          rt5
            (V5.Publish
               {
                 topic = "a/b";
                 qos;
                 packet_id;
                 payload;
                 dup = false;
                 retain = true;
                 properties =
                   [
                     P.Correlation_data "\000\255";
                     P.User_property ("a", "b");
                     P.User_property ("a", "c");
                     P.Subscription_identifier 1;
                     P.Subscription_identifier 16384;
                   ];
               }))
        [ `At_most_once; `At_least_once; `Exactly_once ])
    [ 0; 1; 120; 127; 128; 16380; 16383; 16384; 65536 ]

let test_malformed () =
  let both text =
    rejects V3.decode text;
    rejects V5.decode text
  in
  List.iter both
    [
      "";
      "\192";
      "\000\000";
      "\193\000";
      "\192\001\000";
      "\192\000\208\000";
      "\192\128\000";
      "\192\128\128\128\128\000";
      "\054\000";
      "\056\003\000\001a";
      "\064\002\000\000";
      "\064\001\001";
      "\048\005\000\003a\000b";
      "\048\003\000\001+";
      "\048\004\000\002\192\128";
      "\048\005\000\003\237\160\128";
      "\048\006\000\004\244\144\128\128";
    ];
  List.iter (rejects V3.decode)
    [
      "\144\003\000\001\003";
      "\144\002\000\001";
      "\128\006\000\001\000\001a\128";
      "\032\002\002\000";
      "\032\002\001\005";
      "\240\000";
      "\064\003\000\001\000";
    ];
  List.iter (rejects V5.decode)
    [
      "\032\002\000\000";
      (* property length is mandatory *)
      "\032\005\000\000\002\037\002";
      (* boolean 2 *)
      "\032\005\000\000\002\036\002";
      (* maximum QoS 2 *)
      "\032\006\000\000\003\033\000\000";
      (* zero receive max *)
      "\048\009\000\001a\005\001\000\001\001x";
      (* repeated singleton *)
      "\048\007\000\001a\003\033\000\001";
      (* wrong property context *)
      "\048\004\000\000\001\035";
      (* truncated property *)
      "\048\003\000\000\000";
      (* no topic or alias *)
      "\144\003\000\001\000";
      (* empty suback *)
      "\176\003\000\001\000";
      (* empty unsuback *)
      "\240\001\135";
      (* illegal AUTH reason *)
      "\064\003\000\001\024";
      (* illegal PUBACK reason *)
      "\128\007\000\001\000\000\001a\048";
    ];
  ignore (decode V5.decode (Bytes.of_string "\240\000"));
  ignore (decode V5.decode (Bytes.of_string "\240\001\024"));
  ignore (decode V5.decode (Bytes.of_string "\064\003\000\001\016"));
  (* NUL is legal in character-data payloads, unlike MQTT strings. *)
  rt5
    (V5.Publish
       {
         topic = "a";
         qos = `At_most_once;
         packet_id = None;
         dup = false;
         retain = false;
         properties = [ P.Payload_format_indicator 1 ];
         payload = S.of_string "\000";
       })

let test_connect_flags () =
  let base =
    V3.to_bytes
      (V3.Connect
         {
           client_id = "id";
           clean_session = true;
           keep_alive = 0;
           credentials = None;
           will = None;
         })
  in
  List.iter
    (fun flags ->
      let b = Bytes.copy base in
      Bytes.set_uint8 b 9 flags;
      rejects V3.decode (Bytes.to_string b))
    [ 1; 8; 16; 24; 32; 64 ]

let test_topics () =
  List.iter
    (fun name -> check name false (Mqttz.Topic.Name.validate name))
    [ ""; "a/+"; "a/#"; "a\000b"; "\192\128" ];
  List.iter
    (fun filter -> check filter false (Mqttz.Topic.Filter.validate filter))
    [ ""; "a/#/b"; "a+"; "a\000b" ];
  List.iter
    (fun (filter, topic, expected) ->
      check
        (filter ^ " matches " ^ topic)
        expected
        (Mqttz.Topic.Filter.matches ~filter ~topic))
    [
      ("a/#", "a", true);
      ("a/+", "a/", true);
      ("a/+", "a", false);
      ("#", "$SYS/broker", false);
      ("+/broker", "$SYS/broker", false);
      ("$SYS/#", "$SYS/broker", true);
      ("#", "a", true);
      ("$share/group/a/+", "a/b", true);
      ("$share//a", "a", false);
      ("$share/+/a", "a", false);
      ("$share/group/#", "$SYS/broker", false);
      ("a/", "a/", true);
      ("a//b", "a/b", false);
    ]

let test_framing () =
  List.iter
    (fun header ->
      Alcotest.(check int)
        "partial header" 0
        (Mqttz.Frame.length (S.of_string header)))
    [ ""; "\048"; "\048\128"; "\048\128\128"; "\048\128\128\128" ];
  Alcotest.(check int)
    "body need not be present" 131
    (Mqttz.Frame.length (S.of_string "\048\128\001"));
  (match Mqttz.Frame.length ~max_size:130 (S.of_string "\048\128\001") with
  | _ -> Alcotest.fail "oversized header accepted"
  | exception Mqttz.Frame.Malformed _ -> ());
  match Mqttz.Frame.length (S.of_string "\048\255\255\255\127") with
  | _ -> Alcotest.fail "default packet limit ignored"
  | exception Mqttz.Frame.Malformed _ -> ()

let test_borrowing () =
  let bytes = Bytes.of_string "prefixPAYLOADsuffix" in
  let payload = S.make ~off:6 ~len:7 bytes in
  let packet =
    V5.Publish
      {
        topic = "a";
        qos = `At_most_once;
        packet_id = None;
        dup = false;
        retain = false;
        properties = [];
        payload;
      }
  in
  (match V5.encode packet with
  | [ _; view ] -> check "encode borrows" true (view.bytes == bytes)
  | _ -> Alcotest.fail "PUBLISH should have header and payload views");
  let wire = V5.to_bytes packet in
  (match decode V5.decode wire with
  | V5.Publish p ->
      check "decode borrows" true (p.payload.bytes == wire);
      Alcotest.(check string) "payload offset" "PAYLOAD" (S.to_string p.payload);
      let owned = S.copy p.payload in
      Bytes.fill wire 0 (Bytes.length wire) '\000';
      Alcotest.(check string)
        "explicit copy owns bytes" "PAYLOAD" (S.to_string owned)
  | _ -> Alcotest.fail "expected PUBLISH");
  Alcotest.(check int)
    "local view" 5
    (Allocation_guard.local_view_sum (S.of_string "\001\002\003"))

let test_allocations () =
  let wire size =
    V5.to_bytes
      (V5.Publish
         {
           topic = "a";
           qos = `At_most_once;
           packet_id = None;
           dup = false;
           retain = false;
           properties = [];
           payload = S.make (Bytes.make size 'x');
         })
  in
  let measure bytes =
    let slice = S.make bytes in
    Gc.full_major ();
    let before = Gc.allocated_bytes () in
    for _ = 1 to 100 do
      ignore (Sys.opaque_identity (V5.decode slice))
    done;
    Gc.allocated_bytes () -. before
  in
  let small = measure (wire 16) and large = measure (wire (1024 * 1024)) in
  check "decode allocation does not scale with payload" true
    (large <= small +. 4096.);
  Printf.printf "100 decodes: small=%.0f bytes, 1MiB=%.0f bytes\n%!" small large

let test_mutations () =
  let state = Random.State.make [| 20260909 |] in
  let seed =
    V5.to_bytes
      (V5.Publish
         {
           topic = "a/b";
           qos = `Exactly_once;
           packet_id = Some 10;
           dup = false;
           retain = false;
           properties = [ P.User_property ("key", "value") ];
           payload = S.of_string "hello";
         })
  in
  for _ = 1 to 10000 do
    let bytes = Bytes.copy seed in
    for _ = 1 to 3 do
      Bytes.set_uint8 bytes
        (Random.State.int state (Bytes.length bytes))
        (Random.State.int state 256)
    done;
    ignore (V3.decode (S.make bytes));
    ignore (V5.decode (S.make bytes))
  done

let test_field_validation () =
  let too_long = String.make 65536 'a' in
  let invalid f =
    match f () with
    | () -> Alcotest.fail "invalid field accepted by validate"
    | exception Invalid_argument _ -> ()
  in
  List.iter
    (fun credentials ->
      invalid (fun () ->
          V3.validate
            (V3.Connect
               {
                 client_id = "x";
                 clean_session = true;
                 keep_alive = 0;
                 credentials = Some credentials;
                 will = None;
               }));
      invalid (fun () ->
          V5.validate
            (V5.Connect
               {
                 client_id = "x";
                 clean_start = true;
                 keep_alive = 0;
                 credentials = Some credentials;
                 will = None;
                 properties = [];
               })))
    [
      `Username "\000"; `Username too_long; `Username_password ("user", too_long);
    ];
  List.iter
    (fun property ->
      invalid (fun () ->
          V5.validate
            (V5.Publish
               {
                 topic = "x";
                 dup = false;
                 retain = false;
                 qos = `At_most_once;
                 packet_id = None;
                 properties = [ property ];
                 payload = S.empty;
               })))
    [
      P.User_property ("\000", "value");
      P.Content_type too_long;
      P.Correlation_data too_long;
    ];
  invalid (fun () ->
      V3.validate
        (V3.Connect
           {
             client_id = "x";
             clean_session = true;
             keep_alive = 0;
             credentials = None;
             will =
               Some
                 (Mqttz.Will.create ~topic:"w" ~payload:too_long
                    ~qos:`At_most_once ~retain:false);
           }));
  let connect payload =
    V5.Connect
      {
        client_id = "x";
        clean_start = true;
        keep_alive = 0;
        credentials = None;
        properties = [];
        will =
          Some
            {
              will_topic = "w";
              will_payload = payload;
              will_qos = `At_most_once;
              will_retain = false;
              will_properties = [ P.Payload_format_indicator 1 ];
            };
      }
  in
  invalid (fun () -> V5.validate (connect too_long));
  invalid (fun () -> V5.validate (connect "\255"));
  rt5 (connect "\000valid UTF-8")

let () =
  Alcotest.run "mqttz"
    [
      ( "codec",
        List.map
          (fun (name, f) -> Alcotest.test_case name `Quick f)
          [
            ("all control packets", test_packets);
            ("publish sizes and QoS", test_publish);
            ("malformed wire packets", test_malformed);
            ("CONNECT flags", test_connect_flags);
            ("public field validation", test_field_validation);
            ("topic matching", test_topics);
            ("incremental framing and limits", test_framing);
            ("borrowed buffers and local views", test_borrowing);
            ("payload-independent allocation", test_allocations);
            ("10,000 mutations", test_mutations);
          ] );
    ]
