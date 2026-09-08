(** Round-trip tests for the codecs in [matrix-chat.proto]: decode, encode, and
    decode again, so that a codec that loses a member is caught. *)

open Matrix_proto

let tests_run = ref 0
let tests_passed = ref 0
let tests_failed = ref 0

let test name f =
  incr tests_run;
  Printf.printf "  %s... " name;
  flush stdout;
  try
    f ();
    incr tests_passed;
    Printf.printf "OK\n"
  with e ->
    incr tests_failed;
    Printf.printf "FAIL: %s\n" (Printexc.to_string e)

let check_true msg b = if not b then failwith msg

let check_eq pp msg a b =
  if a <> b then
    failwith (Printf.sprintf "%s: expected %s, got %s" msg (pp a) (pp b))

let group name tests =
  Printf.printf "\n%s:\n" name;
  List.iter (fun (n, f) -> test n f) tests

let read_fixture name =
  let path = "fixtures/" ^ name in
  In_channel.with_open_bin path In_channel.input_all

let roundtrip_test jsont json_str =
  match Jsont_bytesrw.decode_string jsont json_str with
  | Error e -> failwith ("decode: " ^ e)
  | Ok value -> (
      match Jsont_bytesrw.encode_string jsont value with
      | Error e -> failwith ("encode: " ^ e)
      | Ok encoded -> (
          match Jsont_bytesrw.decode_string jsont encoded with
          | Error e -> failwith ("re-decode: " ^ e)
          | Ok _ -> ()))

let decode_test jsont json_str =
  match Jsont_bytesrw.decode_string jsont json_str with
  | Ok value -> value
  | Error e -> failwith ("decode: " ^ e)

let expect_decode_error jsont json =
  match Jsont_bytesrw.decode_string jsont json with
  | Error _ -> ()
  | Ok _ -> failwith ("expected decode failure: " ^ json)

let expect_encode_error jsont value =
  match Jsont_bytesrw.encode_string jsont value with
  | Error _ -> ()
  | Ok encoded -> failwith ("expected encode failure, got " ^ encoded)

let json_codec_tests =
  let module Codec = Json.Codec in
  [
    ( "checked integers reject coercion and truncation",
      fun () ->
        List.iter
          (expect_decode_error Codec.int)
          [ {|"1"|}; "1.25"; "-1.25"; "1e999"; "null" ];
        check_eq string_of_int "integer" 1 (decode_test Codec.int "1") );
    ( "checked integers enforce Matrix safe range",
      fun () ->
        let max = 9_007_199_254_740_991L in
        let min = Int64.neg max in
        check_eq Int64.to_string "maximum" max
          (decode_test Codec.int64 "9007199254740991");
        check_eq Int64.to_string "minimum" min
          (decode_test Codec.int64 "-9007199254740991");
        List.iter
          (expect_decode_error Codec.int64)
          [ "9007199254740992"; "-9007199254740992" ];
        expect_encode_error Codec.int64 (Int64.add max 1L);
        expect_encode_error Codec.int64 (Int64.sub min 1L) );
    ( "checked bounded integers inspect the fraction first",
      fun () ->
        List.iter
          (expect_decode_error Codec.uint8)
          [ "-1"; "0.9"; "255.9"; "256" ];
        check_eq string_of_int "uint8 maximum" 255
          (decode_test Codec.uint8 "255") );
    ( "finite number is distinct from integer",
      fun () ->
        check_true "fraction accepted" (decode_test Codec.number "1.25" = 1.25);
        List.iter (expect_decode_error Codec.number) [ "null"; "1e999" ];
        expect_encode_error Codec.number Float.nan;
        expect_encode_error Codec.number Float.infinity );
    ( "checked strings reject caller-built invalid UTF-8",
      fun () -> expect_encode_error Codec.string "\xFF" );
    ( "generic JSON is checked recursively",
      fun () ->
        let meta = Jsont.Meta.none in
        let name value = (value, meta) in
        let duplicate =
          Jsont.Object
            ( [
                (name "x", Jsont.Null ((), meta));
                (name "x", Jsont.Bool (true, meta));
              ],
              meta )
        in
        let invalid_string = Jsont.String ("\xFF", meta) in
        let infinite = Jsont.Number (Float.infinity, meta) in
        List.iter
          (expect_encode_error Codec.json)
          [ duplicate; invalid_string; infinite ];
        List.iter
          (expect_decode_error Codec.json)
          [ {|{"x":1,"x":2}|}; "1e999" ] );
    ( "checked maps preserve the object invariants",
      fun () ->
        let assoc = Codec.string_map Codec.int in
        expect_decode_error assoc {|{"x":1,"x":2}|};
        expect_encode_error assoc [ ("x", 1); ("x", 2) ];
        expect_encode_error assoc [ ("\xFF", 1) ];
        expect_decode_error (Codec.as_string_map Codec.int) {|{"x":1,"x":2}|} );
    ( "wire-text validation catches typed duplicate members",
      fun () ->
        List.iter
          (fun source ->
            match Codec.validate_text source with
            | Error _ -> ()
            | Ok () -> failwith ("expected validation failure: " ^ source))
          [ {|{"x":1,"x":2}|}; {|{"x":1,"\u0078":2}|}; "\xFF" ];
        match Codec.validate_text {|{"outer":{"x":1},"x":2}|} with
        | Ok () -> ()
        | Error message -> failwith message );
  ]

let identifier_tests =
  [
    ( "user_id valid",
      fun () ->
        let ids =
          [ "@alice:example.org"; "@bob:matrix.org"; "@user123:localhost" ]
        in
        List.iter
          (fun id ->
            match Id.User_id.of_string id with
            | Ok uid ->
                check_eq Fun.id "roundtrip" id (Id.User_id.to_string uid)
            | Error _ -> failwith ("expected valid: " ^ id))
          ids );
    ( "user_id historical",
      fun () ->
        (* The specification requires that localparts outside its permitted
           set, such as uppercase letters, still be accepted. *)
        let historical =
          [
            "@Samoht:recoil.org";
            "@Alice_B:example.org";
            "@:example.org";
            "@δοκιμή:example.org";
          ]
        in
        List.iter
          (fun id ->
            match Id.User_id.of_string id with
            | Ok uid ->
                check_eq Fun.id "roundtrip" id (Id.User_id.to_string uid);
                check_eq string_of_bool "not conformant" false
                  (Id.User_id.is_spec_conformant uid)
            | Error _ -> failwith ("expected accepted: " ^ id))
          historical;
        match Id.User_id.of_string "@alice:example.org" with
        | Ok uid ->
            check_eq string_of_bool "conformant" true
              (Id.User_id.is_spec_conformant uid)
        | Error _ -> failwith "expected valid" );
    ( "user_id invalid",
      fun () ->
        let ids = [ "alice:example.org"; "@"; "@alice" ] in
        List.iter
          (fun id ->
            match Id.User_id.of_string id with
            | Ok _ -> failwith ("expected invalid: " ^ id)
            | Error _ -> ())
          ids );
    ( "room_id valid",
      fun () ->
        let ids =
          [
            "!SVkFJHzfwvuaIEawgC:localhost";
            "!opaqueid:matrix.org";
            "!:example.org";
            "!opaque:/";
            "!opaque:example.org:notaport";
          ]
        in
        List.iter
          (fun id ->
            match Id.Room_id.of_string id with
            | Ok rid ->
                check_eq Fun.id "roundtrip" id (Id.Room_id.to_string rid)
            | Error _ -> failwith ("expected valid: " ^ id))
          ids );
    ( "server_name grammar",
      fun () ->
        let valid =
          [
            "example.org";
            "EXAMPLE.org:8448";
            "127.0.0.1";
            "127.0.0.1:443";
            "[2001:db8::1]";
            "[2001:DB8::1]:8448";
          ]
        and invalid =
          [
            "";
            "example.org:";
            "example.org:65536";
            "example.org:+443";
            "example.org:abc";
            "256.1.1.1";
            "1.2.3";
            "2001:db8::1";
            "[2001:db8::1";
            "[2001:db8::1]x";
          ]
        in
        List.iter
          (fun name ->
            match Id.Server_name.of_string name with
            | Ok parsed ->
                check_eq Fun.id "server spelling" name
                  (Id.Server_name.to_string parsed)
            | Error _ -> failwith ("expected valid server name: " ^ name))
          valid;
        List.iter
          (fun name ->
            match Id.Server_name.of_string name with
            | Ok _ -> failwith ("expected invalid server name: " ^ name)
            | Error _ -> ())
          invalid );
    ( "identifier limits and domainless rooms",
      fun () ->
        let expect_ok kind parse render value =
          match parse value with
          | Ok parsed -> check_eq Fun.id kind value (render parsed)
          | Error _ -> failwith ("expected valid " ^ kind)
        and expect_error kind parse value =
          match parse value with
          | Ok _ -> failwith ("expected invalid " ^ kind)
          | Error _ -> ()
        in
        let room = String.make 253 'a' in
        expect_ok "domainless room" Id.Room_id.of_string Id.Room_id.to_string
          ("!" ^ room);
        (match Id.Room_id.of_string "!hash" with
        | Ok room ->
            check_true "domainless server" (Id.Room_id.server_name room = None)
        | Error _ -> failwith "expected domainless room");
        (match Id.Room_id.of_string "!opaque:example.org:notaport" with
        | Ok room ->
            check_true "invalid legacy suffix stays opaque"
              (Id.Room_id.server_name room = None);
            check_eq Fun.id "opaque suffix" "opaque:example.org:notaport"
              (Id.Room_id.opaque_id room)
        | Error _ -> failwith "expected opaque room with colon");
        expect_error "long user" Id.User_id.of_string
          ("@" ^ String.make 249 'a' ^ ":x.org");
        expect_error "long room alias" Id.Room_alias.of_string
          ("#" ^ String.make 250 'a' ^ ":x.org");
        expect_error "long event" Id.Event_id.of_string
          ("$" ^ String.make 255 'a') );
    ( "event_id v1",
      fun () ->
        let id = "$152037280074GZeOm:localhost" in
        match Id.Event_id.of_string id with
        | Ok eid -> check_eq Fun.id "roundtrip" id (Id.Event_id.to_string eid)
        | Error _ -> failwith "expected valid" );
    ( "event_id v4",
      fun () ->
        let id = "$Rqnc-F-dvnEYJTyHq_iKxU2bZ1CI92-kuZq3a5lr5Zg" in
        match Id.Event_id.of_string id with
        | Ok eid -> check_eq Fun.id "roundtrip" id (Id.Event_id.to_string eid)
        | Error _ -> failwith "expected valid" );
    ( "room_alias valid",
      fun () ->
        let ids = [ "#test:localhost"; "#general:matrix.org" ] in
        List.iter
          (fun id ->
            match Id.Room_alias.of_string id with
            | Ok alias ->
                check_eq Fun.id "roundtrip" id (Id.Room_alias.to_string alias)
            | Error _ -> failwith ("expected valid: " ^ id))
          ids );
    ( "transaction_id of_bytes",
      fun () ->
        let txn1 = Id.Transaction_id.of_bytes "\x01\x02\x03\x04" in
        let txn2 = Id.Transaction_id.of_bytes "\x05\x06\x07\x08" in
        check_eq Fun.id "hex encoding" "01020304"
          (Id.Transaction_id.to_string txn1);
        check_true "distinct bytes give distinct ids"
          (Id.Transaction_id.to_string txn1 <> Id.Transaction_id.to_string txn2)
    );
  ]

let event_type_tests =
  [
    ( "retention type spellings",
      fun () ->
        check_eq Fun.id "stable retention" "m.room.retention"
          (Event.Event_type.to_string Event.Event_type.Room_retention);
        check_eq Fun.id "MSC1763 retention" "org.matrix.msc1763.retention"
          (Event.Event_type.to_string Event.Event_type.Room_retention_unstable);
        check_true "stable retention parses distinctly"
          (Event.Event_type.of_string "m.room.retention"
          = Event.Event_type.Room_retention);
        check_true "MSC1763 retention parses distinctly"
          (Event.Event_type.of_string "org.matrix.msc1763.retention"
          = Event.Event_type.Room_retention_unstable) );
    ( "service member type mappings",
      fun () ->
        check_true "member hints type"
          (Event.Event_type.of_string "m.room.member_hints"
          = Event.Event_type.Room_member_hints);
        check_true "MSC4171 member hints alias"
          (Event.Event_type.of_string "m.member_hints"
          = Event.Event_type.Room_member_hints);
        check_true "legacy functional members type"
          (Event.Event_type.of_string "io.element.functional_members"
          = Event.Event_type.Io_element_functional_members) );
  ]

let event_content_tests =
  [
    ( "room_create_content",
      fun () ->
        roundtrip_test Event.Room_create_content.jsont
          {|{"creator": "@example:localhost", "room_version": "10"}|} );
    ( "room_name_content",
      fun () ->
        roundtrip_test Event.Room_name_content.jsont {|{"name": "Test Room"}|}
    );
    ( "room_topic_content",
      fun () ->
        roundtrip_test Event.Room_topic_content.jsont
          {|{"topic": "This is a test room"}|} );
    ( "room_avatar_content",
      fun () ->
        roundtrip_test Event.Room_avatar_content.jsont
          {|{"url": "mxc://localhost/abc123", "info": {"h": 480, "w": 640, "mimetype": "image/png", "size": 12345}}|}
    );
    ( "room_member_content join",
      fun () ->
        roundtrip_test Event.Room_member_content.jsont
          {|{"membership": "join", "displayname": "Example User"}|} );
    ( "room_member_content invite",
      fun () ->
        roundtrip_test Event.Room_member_content.jsont
          {|{"membership": "invite"}|} );
    ( "room_member_content leave",
      fun () ->
        roundtrip_test Event.Room_member_content.jsont
          {|{"membership": "leave", "reason": "Goodbye!"}|} );
    ( "room_member_content ban",
      fun () ->
        roundtrip_test Event.Room_member_content.jsont
          {|{"membership": "ban", "reason": "Violated rules"}|} );
    ( "room_join_rules public",
      fun () ->
        roundtrip_test Event.Room_join_rules_content.jsont
          {|{"join_rule": "public"}|} );
    ( "room_join_rules restricted",
      fun () ->
        roundtrip_test Event.Room_join_rules_content.jsont
          {|{"join_rule": "restricted", "allow": [{"type": "m.room_membership", "room_id": "!other:localhost"}]}|}
    );
    ( "room_history_visibility",
      fun () ->
        roundtrip_test Event.Room_history_visibility_content.jsont
          {|{"history_visibility": "shared"}|} );
    ( "room_power_levels",
      fun () ->
        roundtrip_test Event.Room_power_levels_content.jsont
          {|{"ban": 50, "events": {"m.room.name": 50}, "users": {"@example:localhost": 100}}|}
    );
    ( "room_retention",
      fun () ->
        let content =
          decode_test Event.Room_retention_content.jsont
            {|{"min_lifetime": 60000, "max_lifetime": 604800000}|}
        in
        check_eq Int64.to_string "minimum lifetime" 60000L
          (Event.Room_retention_content.min_lifetime content |> Option.get);
        check_eq Int64.to_string "maximum lifetime" 604800000L
          (Event.Room_retention_content.max_lifetime content |> Option.get);
        roundtrip_test Event.Room_retention_content.jsont
          {|{"min_lifetime": 60000, "max_lifetime": 604800000}|} );
    ( "room_retention empty",
      fun () ->
        let content = decode_test Event.Room_retention_content.jsont {|{}|} in
        check_true "minimum lifetime absent"
          (Event.Room_retention_content.min_lifetime content = None);
        check_true "maximum lifetime absent"
          (Event.Room_retention_content.max_lifetime content = None) );
    ( "room_member_hints",
      fun () ->
        let content =
          decode_test Event.Room_member_hints_content.jsont
            {|{"service_members": ["@bot:example.org"]}|}
        in
        check_true "typed service member"
          (List.map Id.User_id.to_string
             (Event.Room_member_hints_content.service_members content)
          = [ "@bot:example.org" ]);
        roundtrip_test Event.Room_member_hints_content.jsont
          {|{"service_members": ["@bot:example.org"]}|} );
    ( "room_member_hints absent is empty",
      fun () ->
        let content =
          decode_test Event.Room_member_hints_content.jsont {|{}|}
        in
        check_true "absent service members are empty"
          (Event.Room_member_hints_content.service_members content = []) );
    ( "legacy functional members",
      fun () ->
        let content =
          decode_test Event.Io_element_functional_members_content.jsont
            {|{"service_members": ["@bot:example.org"]}|}
        in
        check_true "typed legacy service member"
          (List.map Id.User_id.to_string
             (Event.Io_element_functional_members_content.service_members
                content)
          = [ "@bot:example.org" ]);
        roundtrip_test Event.Io_element_functional_members_content.jsont
          {|{"service_members": ["@bot:example.org"]}|} );
    ( "room_canonical_alias",
      fun () ->
        roundtrip_test Event.Room_canonical_alias_content.jsont
          {|{"alias": "#test:localhost", "alt_aliases": ["#other:localhost"]}|}
    );
    ( "room_encryption",
      fun () ->
        roundtrip_test Event.Room_encryption_content.jsont
          {|{"algorithm": "m.megolm.v1.aes-sha2"}|} );
    ( "room_encryption with rotation",
      fun () ->
        roundtrip_test Event.Room_encryption_content.jsont
          {|{"algorithm": "m.megolm.v1.aes-sha2", "rotation_period_ms": 604800000, "rotation_period_msgs": 100}|}
    );
    ( "room_pinned_events",
      fun () ->
        roundtrip_test Event.Room_pinned_events_content.jsont
          {|{"pinned": ["$event1:localhost", "$event2:localhost"]}|} );
    ( "room_server_acl",
      fun () ->
        roundtrip_test Event.Room_server_acl_content.jsont
          {|{"allow": ["*"], "allow_ip_literals": false, "deny": ["evil.server"]}|}
    );
    ( "room_tombstone",
      fun () ->
        roundtrip_test Event.Room_tombstone_content.jsont
          {|{"body": "Room has been upgraded", "replacement_room": "!newroom:localhost"}|}
    );
    ( "room_guest_access",
      fun () ->
        roundtrip_test Event.Room_guest_access_content.jsont
          {|{"guest_access": "can_join"}|} );
    ( "text_message simple",
      fun () ->
        roundtrip_test Event.Text_message_content.jsont
          {|{"body": "Hello, world!", "msgtype": "m.text"}|} );
    ( "text_message formatted",
      fun () ->
        roundtrip_test Event.Text_message_content.jsont
          {|{"body": "Hello", "msgtype": "m.text", "format": "org.matrix.custom.html", "formatted_body": "<b>Hello</b>"}|}
    );
    ( "emote_message",
      fun () ->
        roundtrip_test Event.Text_message_content.jsont
          {|{"body": "waves", "msgtype": "m.emote"}|} );
    ( "notice_message",
      fun () ->
        roundtrip_test Event.Text_message_content.jsont
          {|{"body": "Notice", "msgtype": "m.notice"}|} );
    ( "image_message",
      fun () ->
        roundtrip_test Event.Media_message_content.jsont
          {|{"body": "image.png", "msgtype": "m.image", "url": "mxc://localhost/abc123", "info": {"mimetype": "image/png", "size": 12345, "h": 480, "w": 640}}|}
    );
    ( "file_message",
      fun () ->
        roundtrip_test Event.Media_message_content.jsont
          {|{"body": "document.pdf", "msgtype": "m.file", "url": "mxc://localhost/file789", "info": {"mimetype": "application/pdf", "size": 54321}}|}
    );
    ( "audio_message",
      fun () ->
        roundtrip_test Event.Media_message_content.jsont
          {|{"body": "audio.mp3", "msgtype": "m.audio", "url": "mxc://localhost/audio456", "info": {"mimetype": "audio/mpeg", "size": 98765, "duration": 180000}}|}
    );
    ( "video_message",
      fun () ->
        roundtrip_test Event.Media_message_content.jsont
          {|{"body": "video.mp4", "msgtype": "m.video", "url": "mxc://localhost/video789", "info": {"mimetype": "video/mp4", "size": 1234567, "duration": 60000, "h": 720, "w": 1280}}|}
    );
    ( "location_message",
      fun () ->
        roundtrip_test Event.Location_message_content.jsont
          {|{"body": "My Location", "msgtype": "m.location", "geo_uri": "geo:51.5074,-0.1278"}|}
    );
    ( "sticker",
      fun () ->
        roundtrip_test Event.Sticker_content.jsont
          {|{"body": "sticker", "url": "mxc://localhost/sticker123", "info": {"mimetype": "image/png", "size": 1234, "h": 128, "w": 128}}|}
    );
  ]

let space_content_tests =
  [
    ( "space_child",
      fun () ->
        roundtrip_test Event.Space_child_content.jsont
          {|{"via": ["matrix.org"], "order": "a", "suggested": true}|} );
    ( "space_parent",
      fun () ->
        roundtrip_test Event.Space_parent_content.jsont
          {|{"via": ["matrix.org"], "canonical": true}|} );
  ]

let call_content_tests =
  [
    ( "call_invite",
      fun () ->
        roundtrip_test Event.Call_invite_content.jsont
          {|{"call_id": "12345", "version": 1, "lifetime": 60000, "offer": {"type": "offer", "sdp": "v=0..."}}|}
    );
    ( "call_answer",
      fun () ->
        roundtrip_test Event.Call_answer_content.jsont
          {|{"call_id": "12345", "version": 1, "answer": {"type": "answer", "sdp": "v=0..."}}|}
    );
    ( "call_hangup",
      fun () ->
        roundtrip_test Event.Call_hangup_content.jsont
          {|{"call_id": "12345", "version": 1, "reason": "user_hangup"}|} );
    ( "call_candidates",
      fun () ->
        roundtrip_test Event.Call_candidates_content.jsont
          {|{"call_id": "12345", "version": 1, "candidates": [{"candidate": "candidate:...", "sdpMid": "0", "sdpMLineIndex": 0}]}|}
    );
    ( "call_member",
      fun () ->
        roundtrip_test Event.Call_member_content.jsont
          {|{"memberships": [{"call_id": "call1", "scope": "m.room", "application": "m.call", "device_id": "DEVICE1", "expires": 3600000}]}|}
    );
  ]

let key_verification_tests =
  [
    ( "key_verification_ready",
      fun () ->
        roundtrip_test Event.Key_verification_ready_content.jsont
          {|{"from_device": "DEVICE1", "methods": ["m.sas.v1", "m.qr_code.show.v1"]}|}
    );
    ( "key_verification_start",
      fun () ->
        roundtrip_test Event.Key_verification_start_content.jsont
          {|{"from_device": "DEVICE1", "method": "m.sas.v1", "key_agreement_protocols": ["curve25519-hkdf-sha256"], "hashes": ["sha256"], "message_authentication_codes": ["hkdf-hmac-sha256"], "short_authentication_string": ["decimal", "emoji"]}|}
    );
    ( "key_verification_accept",
      fun () ->
        roundtrip_test Event.Key_verification_accept_content.jsont
          {|{"method": "m.sas.v1", "key_agreement_protocol": "curve25519-hkdf-sha256", "hash": "sha256", "message_authentication_code": "hkdf-hmac-sha256", "short_authentication_string": ["decimal", "emoji"], "commitment": "fQpGIW1Snz+pwLZu6sMy2nF92MYN89TDAhYhPxVvBZc"}|}
    );
    ( "key_verification_key",
      fun () ->
        roundtrip_test Event.Key_verification_key_content.jsont
          {|{"key": "fQpGIW1Snz+pwLZu6sMy2nF92MYN89TDAhYhPxVvBZc"}|} );
    ( "key_verification_mac",
      fun () ->
        roundtrip_test Event.Key_verification_mac_content.jsont
          {|{"mac": {"ed25519:DEVICEID": "fQpGIW1Snz+pwLZu6sMy2nF92MYN89TDAhYhPxVvBZc"}, "keys": "fQpGIW1Snz+pwLZu6sMy2nF92MYN89TDAhYhPxVvBZc"}|}
    );
    ( "key_verification_cancel",
      fun () ->
        roundtrip_test Event.Key_verification_cancel_content.jsont
          {|{"code": "m.user", "reason": "User cancelled"}|} );
    ( "key_verification_done",
      fun () -> roundtrip_test Event.Key_verification_done_content.jsont {|{}|}
    );
  ]

let policy_rule_tests =
  [
    ( "policy_rule_ban",
      fun () ->
        roundtrip_test Event.Policy_rule_content.jsont
          {|{"entity": "@spam:*", "reason": "Spamming", "recommendation": "m.ban"}|}
    );
    ( "policy_rule_unknown",
      fun () ->
        roundtrip_test Event.Policy_rule_content.jsont
          {|{"entity": "evil.server", "reason": "Known bad actor", "recommendation": "org.custom.action"}|}
    );
  ]

let account_data_tests =
  [
    ( "marked_unread",
      fun () ->
        roundtrip_test Event.Marked_unread_content.jsont {|{"unread": true}|} );
    ( "marked_unread_false",
      fun () ->
        roundtrip_test Event.Marked_unread_content.jsont {|{"unread": false}|}
    );
  ]

let encrypted_content_tests =
  [
    ( "megolm_encrypted",
      fun () ->
        roundtrip_test Event.Encrypted_content.jsont
          {|{"algorithm": "m.megolm.v1.aes-sha2", "sender_key": "abc123", "ciphertext": "encrypted...", "session_id": "session123", "device_id": "DEVICE"}|}
    );
    ( "olm_encrypted",
      fun () ->
        roundtrip_test Event.Encrypted_content.jsont
          {|{"algorithm": "m.olm.v1.curve25519-aes-sha2", "sender_key": "abc123", "ciphertext": {"DEVICE": {"type": 0, "body": "encrypted..."}}}|}
    );
  ]

let reaction_content_tests =
  [
    ( "reaction",
      fun () ->
        roundtrip_test Event.Reaction_content.jsont
          {|{"m.relates_to": {"rel_type": "m.annotation", "event_id": "$event:localhost", "key": "👍"}}|}
    );
  ]

let beacon_content_tests =
  [
    ( "beacon_info",
      fun () ->
        roundtrip_test Event.Beacon_info_content.jsont
          {|{"live": true, "timeout": 300000, "org.matrix.msc3488.ts": 1234567890000, "org.matrix.msc3488.asset": {"type": "m.self"}}|}
    );
    ( "beacon",
      fun () ->
        roundtrip_test Event.Beacon_content.jsont
          {|{"org.matrix.msc3488.location": {"uri": "geo:51.5074,-0.1278"}, "org.matrix.msc3488.ts": 1234567890000, "m.relates_to": {"rel_type": "m.reference", "event_id": "$beacon:localhost"}}|}
    );
  ]

let poll_content_tests =
  [
    ( "poll_start",
      fun () ->
        roundtrip_test Event.Poll_start_content.jsont
          {|{"org.matrix.msc3381.poll.start": {"question": "What is your favorite color?", "kind": "org.matrix.msc3381.poll.disclosed", "max_selections": 1, "answers": [{"id": "1", "org.matrix.msc1767.text": "Red"}, {"id": "2", "org.matrix.msc1767.text": "Blue"}]}, "org.matrix.msc1767.text": "What is your favorite color?"}|}
    );
    ( "poll_response",
      fun () ->
        roundtrip_test Event.Poll_response_content.jsont
          {|{"m.relates_to": {"rel_type": "m.reference", "event_id": "$poll:localhost"}, "org.matrix.msc3381.poll.response": ["1"]}|}
    );
    ( "poll_end",
      fun () ->
        roundtrip_test Event.Poll_end_content.jsont
          {|{"m.relates_to": {"rel_type": "m.reference", "event_id": "$poll:localhost"}, "org.matrix.msc1767.text": "Poll ended"}|}
    );
  ]

let raw_event_tests =
  [
    ( "raw room message",
      fun () ->
        roundtrip_test Event.Raw_event.jsont
          {|{"content": {"body": "Hello", "msgtype": "m.text"}, "event_id": "$event123:localhost", "origin_server_ts": 152037280000000, "sender": "@example:localhost", "type": "m.room.message"}|}
    );
    ( "raw state event",
      fun () ->
        roundtrip_test Event.Raw_event.jsont
          {|{"content": {"name": "Test Room"}, "event_id": "$event456:localhost", "origin_server_ts": 151393755000000, "sender": "@example:localhost", "state_key": "", "type": "m.room.name"}|}
    );
    ( "raw event with unsigned",
      fun () ->
        roundtrip_test Event.Raw_event.jsont
          {|{"content": {}, "event_id": "$event789:localhost", "origin_server_ts": 151393755000000, "sender": "@example:localhost", "type": "m.room.create", "unsigned": {"age": 12345}}|}
    );
    ( "raw pre-v11 redaction",
      fun () ->
        roundtrip_test Event.Raw_event.jsont
          {|{"content": {"reason": "spam"}, "event_id": "$redaction:localhost", "origin_server_ts": 151393755000001, "redacts": "$target:localhost", "sender": "@example:localhost", "type": "m.room.redaction"}|}
    );
    ( "stripped state event",
      fun () ->
        roundtrip_test Event.Stripped_event.jsont
          {|{"content": {"membership": "invite"}, "sender": "@inviter:localhost", "state_key": "@invitee:localhost", "type": "m.room.member"}|};
        match
          Jsont_bytesrw.decode_string Event.Stripped_event.jsont
            {|{"content": {}, "sender": "not-a-user-id", "state_key": "", "type": "m.room.name"}|}
        with
        | Error _ -> ()
        | Ok _ -> failwith "malformed stripped-event sender was accepted" );
    ( "raw event MSC4115 membership aliases",
      fun () ->
        let decode member =
          decode_test Event.Raw_event.jsont
            (Printf.sprintf
               {|{"content": {}, "event_id": "$event789:localhost", "origin_server_ts": 151393755000000, "sender": "@example:localhost", "type": "m.room.encrypted", "unsigned": {%s: "leave"}}|}
               member)
        in
        List.iter
          (fun member ->
            let event = decode member in
            check_true "membership retained"
              (Option.bind event.unsigned Event.Unsigned.membership
              = Some "leave"))
          [ {|"membership"|}; {|"io.element.msc4115.membership"|} ] );
  ]

let sync_tests =
  [
    ( "timeline",
      fun () ->
        roundtrip_test Sync.Timeline.jsont
          {|{"events": [], "limited": true, "prev_batch": "token123"}|} );
    ( "timeline with events",
      fun () ->
        roundtrip_test Sync.Timeline.jsont
          {|{"events": [{"content": {"body": "Hello", "msgtype": "m.text"}, "event_id": "$event:localhost", "origin_server_ts": 123456, "sender": "@user:localhost", "type": "m.room.message"}]}|}
    );
    ( "room_state",
      fun () -> roundtrip_test Sync.Room_state.jsont {|{"events": []}|} );
    ( "ephemeral",
      fun () ->
        roundtrip_test Sync.Ephemeral.jsont
          {|{"events": [{"type": "m.typing", "content": {"user_ids": []}}]}|} );
    ( "room_summary",
      fun () ->
        roundtrip_test Sync.Room_summary.jsont
          {|{"m.heroes": ["@alice:localhost"], "m.joined_member_count": 5, "m.invited_member_count": 1}|}
    );
    ( "unread_notifications",
      fun () ->
        roundtrip_test Sync.Unread_notification_counts.jsont
          {|{"highlight_count": 0, "notification_count": 2}|} );
    ( "joined_room",
      fun () ->
        roundtrip_test Sync.Joined_room.jsont
          {|{"timeline": {"events": []}, "state": {"events": []}}|} );
    ( "invited_room",
      fun () ->
        roundtrip_test Sync.Invited_room.jsont
          {|{"invite_state": {"events": []}}|} );
    ( "left_room",
      fun () ->
        roundtrip_test Sync.Left_room.jsont {|{"timeline": {"events": []}}|} );
    ( "rooms",
      fun () ->
        roundtrip_test Sync.Rooms.jsont
          {|{"join": {"!room:localhost": {"timeline": {"events": []}}}, "invite": {}, "leave": {}, "knock": {}}|}
    );
    ( "device_lists",
      fun () ->
        roundtrip_test Sync.Device_lists.jsont
          {|{"changed": ["@alice:localhost"], "left": []}|} );
    ("presence", fun () -> roundtrip_test Sync.Presence.jsont {|{"events": []}|});
    ( "response minimal",
      fun () -> roundtrip_test Sync.Response.jsont {|{"next_batch": "s12345"}|}
    );
    ( "response with rooms",
      fun () ->
        let json = read_fixture "sync_response.json" in
        roundtrip_test Sync.Response.jsont json );
  ]

let () =
  Printf.printf "Matrix Protocol Tests\n";
  Printf.printf "=====================\n";

  group "Checked JSON codecs" json_codec_tests;
  group "Identifiers" identifier_tests;
  group "Event Types" event_type_tests;
  group "Event Contents" event_content_tests;
  group "Space Contents" space_content_tests;
  group "Call Contents" call_content_tests;
  group "Key Verification Contents" key_verification_tests;
  group "Policy Rule Contents" policy_rule_tests;
  group "Account Data Contents" account_data_tests;
  group "Encrypted Contents" encrypted_content_tests;
  group "Reaction Contents" reaction_content_tests;
  group "Beacon Contents" beacon_content_tests;
  group "Poll Contents" poll_content_tests;
  group "Raw Events" raw_event_tests;
  group "Sync" sync_tests;

  Printf.printf "\n=====================\n";
  Printf.printf "Results: %d/%d passed" !tests_passed !tests_run;
  if !tests_failed > 0 then Printf.printf " (%d failed)" !tests_failed;
  Printf.printf "\n";

  if !tests_failed > 0 then exit 1
