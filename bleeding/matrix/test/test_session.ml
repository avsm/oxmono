(** Round-trip tests for the JSON codecs in {!Matrix_client.Session}.

    Every persisted record is encoded with {!Jsont_bytesrw.encode_string} and
    decoded again; the result must be structurally equal to the original and
    must re-encode to the same bytes. *)

module Session = Matrix_client.Session

let failures = ref 0

let find_substring ~needle haystack =
  let needle_length = String.length needle in
  let last = String.length haystack - needle_length in
  let rec loop offset =
    if offset > last then None
    else if String.sub haystack offset needle_length = needle then Some offset
    else loop (offset + 1)
  in
  loop 0

let quote_json_integer_member member json =
  let marker = Printf.sprintf "%S:" member in
  match find_substring ~needle:marker json with
  | None -> invalid_arg ("missing JSON integer member " ^ member)
  | Some marker_offset ->
      let value_offset = marker_offset + String.length marker in
      let rec end_of_integer offset =
        if offset >= String.length json then offset
        else
          match json.[offset] with
          | '-' | '0' .. '9' -> end_of_integer (offset + 1)
          | _ -> offset
      in
      let value_end = end_of_integer value_offset in
      if value_end = value_offset then
        invalid_arg ("non-integer JSON member " ^ member);
      String.sub json 0 value_offset
      ^ "\""
      ^ String.sub json value_offset (value_end - value_offset)
      ^ "\""
      ^ String.sub json value_end (String.length json - value_end)

let ptime_of_rfc3339 s =
  match Ptime.of_rfc3339 s with
  | Ok (t, _, _) -> t
  | Error _ -> invalid_arg ("bad timestamp: " ^ s)

let user_id s =
  match Matrix_proto.Id.User_id.of_string s with
  | Ok id -> id
  | Error (`Msg m) -> invalid_arg m

let device_id s =
  match Matrix_proto.Id.Device_id.of_string s with
  | Ok id -> id
  | Error (`Msg m) -> invalid_arg m

let room_id s =
  match Matrix_proto.Id.Room_id.of_string s with
  | Ok id -> id
  | Error (`Msg m) -> invalid_arg m

let check name jsont equal value =
  match Jsont_bytesrw.encode_string ~format:Jsont.Indent jsont value with
  | Error e ->
      incr failures;
      Printf.printf "FAIL %s: encode: %s\n" name e
  | Ok json -> (
      match Jsont_bytesrw.decode_string jsont json with
      | Error e ->
          incr failures;
          Printf.printf "FAIL %s: decode: %s\n%s\n" name e json
      | Ok value' ->
          let reencoded =
            Result.get_ok
              (Jsont_bytesrw.encode_string ~format:Jsont.Indent jsont value')
          in
          if not (equal value value') then (
            incr failures;
            Printf.printf "FAIL %s: value differs after round trip\n" name)
          else if not (String.equal json reencoded) then (
            incr failures;
            Printf.printf "FAIL %s: bytes differ after round trip\n" name)
          else Printf.printf "ok   %s\n" name)

let t0 = ptime_of_rfc3339 "2024-01-02T03:04:05Z"
let t1 = ptime_of_rfc3339 "2024-05-06T07:08:09Z"

let session_file : Session.Session_file.t =
  {
    server =
      {
        homeserver = Uriz.of_string_exn "https://matrix.example.org";
        user_id = user_id "@alice:example.org";
      };
    auth =
      {
        access_token = "syt_token";
        device_id = device_id "DEVICEID";
        refresh_token = Some "syr_token";
        access_token_expires_at = Some t1;
        method_ = Session.Auth.Matrix;
      };
    sync = { next_batch = Some "s72_1"; filter_id = None };
    metadata = { created_at = t0; last_used_at = t1; client_name = "omatrix" };
  }

let session_file_equal (a : Session.Session_file.t) (b : Session.Session_file.t)
    =
  Uriz.equal a.server.homeserver b.server.homeserver
  && Matrix_proto.Id.User_id.to_string a.server.user_id
     = Matrix_proto.Id.User_id.to_string b.server.user_id
  && a.auth.access_token = b.auth.access_token
  && Matrix_proto.Id.Device_id.to_string a.auth.device_id
     = Matrix_proto.Id.Device_id.to_string b.auth.device_id
  && a.auth.refresh_token = b.auth.refresh_token
  && a.auth.access_token_expires_at = b.auth.access_token_expires_at
  && a.auth.method_ = b.auth.method_
  && a.sync = b.sync && a.metadata = b.metadata

let auth_json_tests () =
  let has_member json name =
    match Jsont_bytesrw.decode_string Matrix_proto.Json.Codec.json json with
    | Ok (Jsont.Object (members, _)) ->
        Option.is_some (Jsont.Json.find_mem name members)
    | Ok _ | Error _ -> false
  in
  let legacy =
    {|{"access_token":"at","device_id":"DEVICEID","refresh_token":"rt"}|}
  in
  (match Jsont_bytesrw.decode_string Session.Auth.jsont legacy with
  | Ok
      {
        Session.Auth.method_ = Session.Auth.Matrix;
        access_token_expires_at = None;
        _;
      } ->
      Printf.printf "ok   legacy auth defaults to Matrix\n"
  | Ok _ ->
      incr failures;
      Printf.printf "FAIL legacy auth defaults to Matrix\n"
  | Error e ->
      incr failures;
      Printf.printf "FAIL legacy auth decode: %s\n" e);
  let encode auth =
    Result.get_ok (Jsont_bytesrw.encode_string Session.Auth.jsont auth)
  in
  let matrix_json = encode session_file.auth in
  if has_member matrix_json "access_token_expires_at" then
    Printf.printf "ok   auth retains access-token expiry\n"
  else begin
    incr failures;
    Printf.printf "FAIL auth retains access-token expiry\n"
  end;
  if not (has_member matrix_json "oauth_client_id") then
    Printf.printf "ok   Matrix auth omits oauth_client_id\n"
  else begin
    incr failures;
    Printf.printf "FAIL Matrix auth omits oauth_client_id\n"
  end;
  let oauth =
    {
      session_file.auth with
      method_ = Session.Auth.OAuth { client_id = "cid" };
    }
  in
  let oauth_json = encode oauth in
  match Jsont_bytesrw.decode_string Session.Auth.jsont oauth_json with
  | Ok { Session.Auth.method_ = Session.Auth.OAuth { client_id }; _ }
    when String.equal client_id "cid" ->
      Printf.printf "ok   OAuth auth retains client_id\n"
  | Ok _ ->
      incr failures;
      Printf.printf "FAIL OAuth auth retains client_id\n"
  | Error e ->
      incr failures;
      Printf.printf "FAIL OAuth auth decode: %s\n" e

let legacy_integer_tests () =
  let one_time_keys_json =
    {|{"config":{"target_count":"51","next_key_id":"7"},"keys":[]}|}
  in
  (match
     Jsont_bytesrw.decode_string Session.One_time_keys_file.jsont
       one_time_keys_json
   with
  | Ok { target_count = 51; next_key_id = 7; _ } ->
      Printf.printf "ok   persisted session accepts numeric strings\n"
  | Ok _ ->
      incr failures;
      Printf.printf "FAIL persisted session decoded numeric strings wrongly\n"
  | Error error ->
      incr failures;
      Printf.printf "FAIL persisted session numeric strings: %s\n" error);
  let megolm_json =
    {|{"sessions":[{"room_id":"!room:example.org","session_id":"sid","pickle":"{}","message_index":"3","created_at":"2024-01-02T03:04:05Z","message_count":"12","max_age_ms":9007199254740992}]}|}
  in
  match
    Jsont_bytesrw.decode_string Session.Megolm_outbound_file.jsont megolm_json
  with
  | Ok
      {
        sessions = [ { max_age_ms; message_index = 3; message_count = 12; _ } ];
      }
    when Int64.equal max_age_ms 9_007_199_254_740_992L ->
      Printf.printf "ok   persisted session accepts pre-Jsont-0.2 integers\n"
  | Ok _ ->
      incr failures;
      Printf.printf "FAIL persisted session decoded legacy integer wrongly\n"
  | Error error ->
      incr failures;
      Printf.printf "FAIL persisted session legacy integer: %s\n" error

let device_keys : Session.Device_keys.t =
  {
    ed25519_public = "ed_pub";
    ed25519_private = "ed_priv";
    curve25519_public = "c_pub";
    curve25519_private = "c_priv";
    uploaded_at = Some t0;
    algorithms = [ "m.olm.v1.curve25519-aes-sha2"; "m.megolm.v1.aes-sha2" ];
  }

let one_time_key : Session.One_time_key.t =
  { key_id = "AAAAAQ"; public = "pub"; private_ = "priv"; created_at = t0 }

let one_time_keys : Session.One_time_keys_file.t =
  {
    target_count = 50;
    last_upload_at = Some t1;
    next_key_id = 7;
    keys = [ one_time_key ];
    fallback = Some { one_time_key with key_id = "fallback" };
    previous_fallback = Some { one_time_key with key_id = "previous-fallback" };
    fallback_used = true;
  }

let olm_sessions : Session.Olm_sessions_file.t =
  {
    sessions =
      [
        {
          their_identity_key = "their_key";
          session_id = "sid";
          pickle = "{}";
          created_at = t0;
          last_used_at = t1;
        };
      ];
  }

let megolm_inbound : Session.Megolm_inbound_file.t =
  {
    sessions =
      [
        {
          room_id = room_id "!room:example.org";
          session_id = "sid";
          sender_key = "sender";
          signing_key = "signing";
          pickle = "{}";
          first_known_index = 0;
          created_at = t0;
        };
      ];
  }

let megolm_inbound_equal (a : Session.Megolm_inbound_file.t)
    (b : Session.Megolm_inbound_file.t) =
  List.length a.sessions = List.length b.sessions
  && List.for_all2
       (fun (x : Session.Megolm_inbound.t) (y : Session.Megolm_inbound.t) ->
         Matrix_proto.Id.Room_id.to_string x.room_id
         = Matrix_proto.Id.Room_id.to_string y.room_id
         && { x with room_id = y.room_id } = y)
       a.sessions b.sessions

let megolm_outbound : Session.Megolm_outbound_file.t =
  {
    sessions =
      [
        {
          room_id = room_id "!room:example.org";
          session_id = "sid";
          pickle = "{}";
          message_index = 3;
          created_at = t0;
          message_count = 12;
          max_age_ms = 604_800_000L;
          shared_with =
            [
              {
                user_id = user_id "@bob:example.org";
                device_id = device_id "BOBDEV";
                shared_at = t1;
              };
            ];
        };
      ];
  }

let megolm_outbound_equal (a : Session.Megolm_outbound_file.t)
    (b : Session.Megolm_outbound_file.t) =
  List.length a.sessions = List.length b.sessions
  && List.for_all2
       (fun (x : Session.Megolm_outbound.t) (y : Session.Megolm_outbound.t) ->
         Matrix_proto.Id.Room_id.to_string x.room_id
         = Matrix_proto.Id.Room_id.to_string y.room_id
         && x.session_id = y.session_id
         && x.pickle = y.pickle
         && x.message_index = y.message_index
         && x.created_at = y.created_at
         && x.message_count = y.message_count
         && Int64.equal x.max_age_ms y.max_age_ms
         && List.length x.shared_with = List.length y.shared_with
         && List.for_all2
              (fun (u : Session.Shared_with.t) (v : Session.Shared_with.t) ->
                Matrix_proto.Id.User_id.to_string u.user_id
                = Matrix_proto.Id.User_id.to_string v.user_id
                && Matrix_proto.Id.Device_id.to_string u.device_id
                   = Matrix_proto.Id.Device_id.to_string v.device_id
                && u.shared_at = v.shared_at)
              x.shared_with y.shared_with)
       a.sessions b.sessions

(* A pickle is an opaque string inside the records above, so there is no
   file codec to exercise. What must hold is that pickling succeeds — a codec
   written without [~enc] fails at encode time, not at compile time — and
   that unpickling restores the observable state. *)

module Olm = Matrix_client.Olm
module Pickle = Matrix_client.Session_pickle
module Id = Matrix_proto.Id

(* A [Random.t] over a fixed block of bytes. The pickles have to round-trip
   whatever the key material is, so the test wants repeatable bytes rather
   than real entropy. *)
let random =
  Matrix_client.Random.of_source
    (Eio.Flow.string_source (String.make 8192 '\007'))

(* [observe] projects the state a caller can actually see. *)
let pickle_check name ~pickle ~unpickle ~observe value =
  match pickle value with
  | Error (`Msg e) ->
      incr failures;
      Printf.printf "FAIL %s: pickle: %s\n" name e
  | Ok s -> (
      match unpickle s with
      | Error (`Msg e) ->
          incr failures;
          Printf.printf "FAIL %s: unpickle: %s\n%s\n" name e s
      | Ok value' -> (
          if not (String.equal (observe value) (observe value')) then (
            incr failures;
            Printf.printf "FAIL %s: state differs after round trip\n" name)
          else
            match pickle value' with
            | Error (`Msg e) ->
                incr failures;
                Printf.printf "FAIL %s: re-pickle: %s\n" name e
            | Ok s' ->
                if not (String.equal s s') then (
                  incr failures;
                  Printf.printf "FAIL %s: bytes differ after round trip\n" name)
                else Printf.printf "ok   %s\n" name))

let pickle_room_id = Id.Room_id.of_string_exn "!room:example.org"
let ck_ed = Matrix_client.Crypto_key.Ed25519.Public.to_base64
let ck_curve = Matrix_client.Crypto_key.Curve25519.Public.to_base64
let olm_err e = Format.asprintf "%a" Olm.pp_error e

(* Alice talks to Bob: enough of a handshake to give both an Olm session, one
   with an active sending ratchet and one with a receiving chain. *)
let alice = Olm.Account.create ~random ()
let bob = Olm.Account.create ~random ()

let () =
  Olm.Account.generate_one_time_keys ~random alice 2;
  Olm.Account.generate_fallback_key ~random alice;
  Olm.Account.generate_one_time_keys ~random bob 1

let key_id = Matrix_client.Crypto_key.Key_id.to_string

let account_observed (a : Olm.Account.t) =
  let ed, curve = Olm.Account.identity_keys a in
  String.concat "|"
    ([ ck_ed ed; ck_curve curve ]
    @ List.map
        (fun (id, k) -> key_id id ^ "=" ^ ck_curve k)
        (Olm.Account.one_time_keys a)
    @ [
        (match Olm.Account.fallback_key a with
        | None -> "-"
        | Some (id, k) -> key_id id ^ "=" ^ ck_curve k);
        string_of_int (Olm.Account.one_time_keys_count a);
        string_of_int (Olm.Account.max_one_time_keys a);
      ])

let session_observed (s : Olm.Session.t) =
  String.concat "|"
    [
      Olm.Session.session_id s;
      ck_curve (Olm.Session.their_identity_key s);
      string_of_bool (Olm.Session.has_received_message s);
    ]

let outbound_session =
  let their_identity_key = Olm.Account.curve25519_key bob in
  let their_one_time_key =
    match Olm.Account.one_time_keys bob with
    | (_, k) :: _ -> k
    | [] -> invalid_arg "bob has no one-time key"
  in
  match
    Olm.Session.create_outbound ~random alice ~their_identity_key
      ~their_one_time_key
  with
  | Ok s -> s
  | Error e -> invalid_arg ("create_outbound: " ^ olm_err e)

(* The receiving side, which carries a receiver chain rather than an active
   sending ratchet — the other branch of the [sending] codec. *)
let inbound_session =
  match Olm.Session.encrypt ~random outbound_session "hello" with
  | Error e -> invalid_arg ("encrypt: " ^ olm_err e)
  | Ok (m : Olm.Session.message) -> (
      match
        Olm.Session.create_inbound bob
          ~their_identity_key:(Olm.Account.curve25519_key alice)
          ~ciphertext:m.ciphertext
      with
      | Ok (s, _plaintext) -> s
      | Error e -> invalid_arg ("create_inbound: " ^ olm_err e))

let megolm_outbound_session =
  Olm.Megolm.Outbound.create ~random ~room_id:pickle_room_id ()

let () =
  ignore (Olm.Megolm.Outbound.encrypt megolm_outbound_session "one");
  ignore (Olm.Megolm.Outbound.encrypt megolm_outbound_session "two");
  Olm.Megolm.Outbound.mark_shared_with megolm_outbound_session
    ~user_id:(Id.User_id.of_string_exn "@bob:example.org")
    ~device_id:(Id.Device_id.of_string_exn "BOBDEV")

let megolm_outbound_observed (s : Olm.Megolm.Outbound.t) =
  String.concat "|"
    ([
       Id.Session_id.to_string (Olm.Megolm.Outbound.session_id s);
       Id.Room_id.to_string (Olm.Megolm.Outbound.room_id s);
       string_of_int (Olm.Megolm.Outbound.message_index s);
       Olm.Megolm.Outbound.session_key s;
       string_of_bool (Olm.Megolm.Outbound.needs_rotation s);
     ]
    @ List.map
        (fun (u, d) -> Id.User_id.to_string u ^ "/" ^ Id.Device_id.to_string d)
        (Olm.Megolm.Outbound.shared_with s))

let megolm_inbound_session =
  match
    Olm.Megolm.Inbound.of_session_key
      ~claimed_ed25519:(Olm.Account.ed25519_key alice)
      ~sender_key:(Olm.Account.curve25519_key alice)
      ~room_id:pickle_room_id
      ~session_key:(Olm.Megolm.Outbound.session_key megolm_outbound_session)
      ()
  with
  | Ok s -> s
  | Error e -> invalid_arg ("of_session_key: " ^ olm_err e)

let megolm_inbound_observed (s : Olm.Megolm.Inbound.t) =
  String.concat "|"
    [
      Id.Session_id.to_string (Olm.Megolm.Inbound.session_id s);
      Id.Room_id.to_string (Olm.Megolm.Inbound.room_id s);
      ck_curve (Olm.Megolm.Inbound.sender_key s);
      ck_ed (Olm.Megolm.Inbound.signing_key s);
      (match Olm.Megolm.Inbound.sender_claimed_ed25519_key s with
      | None -> "-"
      | Some k -> ck_ed k);
      string_of_bool (Olm.Megolm.Inbound.signing_key_verified s);
      string_of_int (Olm.Megolm.Inbound.first_known_index s);
      Olm.Megolm.Inbound.export_at_first_known_index s;
    ]

(* Keep the message signature valid while changing the MAC.  The MAC is part
   of the signed body, so use the outbound session's private signing key to
   authenticate the deliberately corrupt message. *)
let corrupt_megolm_mac ~signing_key ciphertext =
  let bytes = Result.get_ok (Matrix_proto.Base64.decode ciphertext) in
  let length = String.length bytes in
  if length < 72 then invalid_arg "Megolm message is too short";
  let body_length = length - 64 in
  let mac_offset = body_length - 8 in
  let mac = String.sub bytes mac_offset 8 in
  let bad_mac =
    String.init 8 (fun i ->
        if i = 0 then Char.chr (Char.code mac.[i] lxor 1) else mac.[i])
  in
  let body =
    String.sub bytes 0 mac_offset
    ^ bad_mac
    ^ String.sub bytes (mac_offset + 8) (body_length - mac_offset - 8)
  in
  let signature =
    Matrix_client.Crypto_key.Ed25519.Private.sign signing_key body
  in
  Matrix_proto.Base64.encode
    (body ^ Matrix_client.Crypto_key.Signature.to_bytes signature)

let megolm_bad_mac_does_not_advance () =
  let outbound =
    Olm.Megolm.Outbound.create ~random ~room_id:pickle_room_id ()
  in
  let inbound =
    match
      Olm.Megolm.Inbound.of_session_key
        ~sender_key:(Olm.Account.curve25519_key alice)
        ~room_id:pickle_room_id
        ~session_key:(Olm.Megolm.Outbound.session_key outbound)
        ()
    with
    | Ok inbound -> inbound
    | Error e -> invalid_arg ("bad MAC regression setup: " ^ olm_err e)
  in
  ignore (Olm.Megolm.Outbound.encrypt outbound "first");
  let future = Olm.Megolm.Outbound.encrypt outbound "future" in
  let signing_key = (Olm.Megolm.Outbound.to_pickle outbound).signing_key in
  let corrupt = corrupt_megolm_mac ~signing_key future.ciphertext in
  (match Olm.Megolm.Inbound.decrypt inbound ~ciphertext:corrupt with
  | Error Matrix_client.Olm_error.Bad_mac -> ()
  | Error e ->
      incr failures;
      Printf.printf "FAIL Megolm bad MAC regression: %s\n" (olm_err e)
  | Ok _ ->
      incr failures;
      Printf.printf
        "FAIL Megolm bad MAC regression: corrupt message decrypted\n");
  if Olm.Megolm.Inbound.latest_index inbound <> 0 then begin
    incr failures;
    Printf.printf "FAIL Megolm bad MAC advanced cached ratchet\n"
  end
  else Printf.printf "ok   Megolm bad MAC does not advance cached ratchet\n";
  match Olm.Megolm.Inbound.decrypt inbound ~ciphertext:future.ciphertext with
  | Ok { plaintext = "future"; message_index = 1 } ->
      Printf.printf "ok   Megolm bad MAC leaves future message decryptable\n"
  | Ok _ ->
      incr failures;
      Printf.printf "FAIL Megolm future message decrypted incorrectly\n"
  | Error e ->
      incr failures;
      Printf.printf "FAIL Megolm future message after bad MAC: %s\n" (olm_err e)

let pickle_tests () =
  pickle_check "Pickle account" ~pickle:Pickle.pickle_account
    ~unpickle:Pickle.unpickle_account ~observe:account_observed alice;
  pickle_check "Pickle Olm session (sending)" ~pickle:Pickle.pickle_session
    ~unpickle:Pickle.unpickle_session ~observe:session_observed outbound_session;
  pickle_check "Pickle Olm session (receiving)" ~pickle:Pickle.pickle_session
    ~unpickle:Pickle.unpickle_session ~observe:session_observed inbound_session;
  pickle_check "Pickle Megolm inbound" ~pickle:Pickle.pickle_megolm_inbound
    ~unpickle:Pickle.unpickle_megolm_inbound ~observe:megolm_inbound_observed
    megolm_inbound_session;
  pickle_check "Pickle Megolm outbound" ~pickle:Pickle.pickle_megolm_outbound
    ~unpickle:Pickle.unpickle_megolm_outbound ~observe:megolm_outbound_observed
    megolm_outbound_session;
  (match Pickle.pickle_account alice with
  | Error (`Msg error) ->
      incr failures;
      Printf.printf "FAIL Pickle account legacy integers: pickle: %s\n" error
  | Ok pickle -> (
      let pickle =
        pickle
        |> quote_json_integer_member "next_key_id"
        |> quote_json_integer_member "max_one_time_keys"
      in
      match Pickle.unpickle_account pickle with
      | Ok restored
        when String.equal (account_observed alice) (account_observed restored)
        ->
          Printf.printf "ok   Pickle account accepts numeric strings\n"
      | Ok _ ->
          incr failures;
          Printf.printf "FAIL Pickle account legacy integers changed state\n"
      | Error (`Msg error) ->
          incr failures;
          Printf.printf "FAIL Pickle account legacy integers: %s\n" error));
  (* An unpickled inbound session must still decrypt what the outbound one
     encrypts: the round trip preserves the ratchet, not just the metadata. *)
  (match
     Result.bind
       (Pickle.pickle_megolm_inbound megolm_inbound_session)
       Pickle.unpickle_megolm_inbound
   with
  | Error (`Msg e) ->
      incr failures;
      Printf.printf "FAIL Pickle Megolm inbound decrypts: %s\n" e
  | Ok restored -> (
      let ciphertext =
        (Olm.Megolm.Outbound.encrypt megolm_outbound_session "three").ciphertext
      in
      match Olm.Megolm.Inbound.decrypt restored ~ciphertext with
      | Ok { plaintext = "three"; _ } ->
          Printf.printf "ok   Pickle Megolm inbound decrypts\n"
      | Ok _ ->
          incr failures;
          Printf.printf "FAIL Pickle Megolm inbound decrypts: wrong plaintext\n"
      | Error e ->
          incr failures;
          Printf.printf "FAIL Pickle Megolm inbound decrypts: %s\n" (olm_err e)));
  (* A pickle that is not JSON at all is an [Error], never an exception. *)
  (match Pickle.unpickle_account "not json" with
  | Error _ -> Printf.printf "ok   Pickle rejects a malformed pickle\n"
  | Ok _ ->
      incr failures;
      Printf.printf "FAIL Pickle accepted a malformed pickle\n");
  megolm_bad_mac_does_not_advance ()

let () =
  auth_json_tests ();
  legacy_integer_tests ();
  check "Session_file" Session.Session_file.jsont session_file_equal
    session_file;
  check "Device_keys" Session.Device_keys.jsont ( = ) device_keys;
  check "One_time_keys_file" Session.One_time_keys_file.jsont ( = )
    one_time_keys;
  check "Olm_sessions_file" Session.Olm_sessions_file.jsont ( = ) olm_sessions;
  check "Megolm_inbound_file" Session.Megolm_inbound_file.jsont
    megolm_inbound_equal megolm_inbound;
  check "Megolm_outbound_file" Session.Megolm_outbound_file.jsont
    megolm_outbound_equal megolm_outbound;
  (* Decoding a malformed timestamp must be a jsont decode error, not an
     exception. *)
  (match
     Jsont_bytesrw.decode_string Session.Metadata.jsont
       {|{"created_at":"nope","last_used_at":"2024-01-02T03:04:05Z","client_name":"x"}|}
   with
  | Error _ -> Printf.printf "ok   Metadata rejects bad timestamp\n"
  | Ok _ ->
      incr failures;
      Printf.printf "FAIL Metadata accepted a bad timestamp\n");
  (match
     Jsont_bytesrw.decode_string Session.Server.jsont
       {|{"homeserver":"https://h","user_id":"not-a-user-id"}|}
   with
  | Error _ -> Printf.printf "ok   Server rejects bad user_id\n"
  | Ok _ ->
      incr failures;
      Printf.printf "FAIL Server accepted a bad user_id\n");
  pickle_tests ();
  (* Show one encoded document. *)
  print_endline "--- session.json ---";
  print_endline
    (Result.get_ok
       (Jsont_bytesrw.encode_string ~format:Jsont.Indent
          Session.Session_file.jsont session_file));
  if !failures > 0 then (
    Printf.printf "%d failure(s)\n" !failures;
    exit 1)
  else print_endline "all round trips ok"
