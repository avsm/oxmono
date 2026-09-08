(** Room-scoped identity warning projection. *)

module Id = Matrix_proto.Id
module Base = Matrix_client.Base_client
module Keys = Matrix_client.Keys
module Cs = Matrix_client.Cross_signing
module Ck = Matrix_client.Crypto_key

let alice = Id.User_id.of_string_exn "@alice:example.org"
let bob = Id.User_id.of_string_exn "@bob:example.org"
let carol = Id.User_id.of_string_exn "@carol:example.org"
let room = Id.Room_id.of_string_exn "!room:example.org"

let random seed =
  Matrix_client.Random.of_source
    (Eio.Flow.string_source (String.make 4096 seed.[0]))

let identity ~seed user_id =
  let private_ = Cs.create_private_identity ~user_id in
  Cs.generate_private_keys ~random:(random seed) private_;
  (private_, Option.get (Cs.build_upload private_))

let query user_id upload : Keys.query_keys_response =
  {
    failures = [];
    device_keys = [];
    master_keys = [ (user_id, upload.Cs.master_key) ];
    self_signing_keys = [ (user_id, upload.Cs.self_signing_key) ];
    user_signing_keys = [];
  }

let state_with_members members =
  let event user membership =
    Printf.sprintf
      {|{"type":"m.room.member","sender":"%s","state_key":"%s","event_id":"$%s","origin_server_ts":1,"content":{"membership":"%s"}}|}
      user user user membership
  in
  let events =
    String.concat ","
      (List.map (fun (user, membership) -> event user membership) members)
  in
  let json =
    Printf.sprintf
      {|{"next_batch":"s","rooms":{"join":{"!room:example.org":{"state":{"events":[%s]},"timeline":{"events":[]}}}}}|}
      events
  in
  match Jsont_bytesrw.decode_string Matrix_proto.Sync.Response.jsont json with
  | Ok response -> fst (Base.apply (Base.create ~user_id:alice ()) response)
  | Error error -> Alcotest.fail error

let warning_ids model =
  Matrix_ui.Observable.List.snapshot
    (Matrix_ui.Room_identity.members model room)
  |> Array.to_list
  |> List.map (fun member ->
      Id.User_id.to_string (Matrix_ui.Room_identity.user_id member))

let warning_kinds model =
  Matrix_ui.Observable.List.snapshot
    (Matrix_ui.Room_identity.members model room)
  |> Array.to_list
  |> List.map Matrix_ui.Room_identity.violation

let test_room_identity_projection () =
  Eio_main.run @@ fun _env ->
  Eio.Switch.run @@ fun sw ->
  let machine =
    Matrix_eio.Encryption.create ~random:(random "e") ~user_id:alice
      ~device_id:(Id.Device_id.of_string_exn "ALICE")
      ()
  in
  let _, bob_first = identity ~seed:"b" bob in
  let _, bob_rotated = identity ~seed:"c" bob in
  Matrix_eio.Encryption.receive_keys_query machine (query bob bob_first);
  Matrix_eio.Encryption.trust_user_identity machine bob;
  Matrix_eio.Encryption.receive_keys_query machine (query bob bob_rotated);
  let model =
    Matrix_ui.Room_identity.create ~own_user:alice ~encryption:machine ()
  in
  let list = Matrix_ui.Room_identity.members model room in
  let initial, subscription = Matrix_ui.Observable.List.subscribe ~sw list in
  Alcotest.(check int) "initially empty" 0 (Array.length initial);
  let state =
    state_with_members
      [ ("@alice:example.org", "join"); ("@bob:example.org", "join") ]
  in
  Matrix_ui.Room_identity.refresh model state;
  Alcotest.(check (list string))
    "violation is room-scoped" [ "@bob:example.org" ] (warning_ids model);
  Alcotest.(check bool)
    "verification warning is distinct" true
    (warning_kinds model = [ Matrix_ui.Room_identity.Verification_violation ]);
  (match Matrix_ui.Observable.List.next subscription with
  | Some [ Matrix_ui.Observable.List.Insert { index = 0; _ } ] -> ()
  | _ -> Alcotest.fail "expected an insertion diff");
  (* A leave removes the warning. *)
  (* The projection sees the current members from the supplied state. *)
  let leaving =
    state_with_members
      [ ("@alice:example.org", "join"); ("@bob:example.org", "leave") ]
  in
  Matrix_ui.Room_identity.refresh model leaving;
  Alcotest.(check (list string))
    "member leave removes warning" [] (warning_ids model);
  (match Matrix_ui.Observable.List.next subscription with
  | Some [ Matrix_ui.Observable.List.Truncate { length = 0 } ] -> ()
  | _ -> Alcotest.fail "expected a removal diff");
  (* A verified identity is not surfaced even while the member remains. *)
  Matrix_eio.Encryption.trust_user_identity machine bob;
  let joined =
    state_with_members
      [ ("@alice:example.org", "join"); ("@bob:example.org", "join") ]
  in
  Matrix_ui.Room_identity.refresh model joined;
  Alcotest.(check (list string))
    "acknowledged identity clears warning" [] (warning_ids model);
  Matrix_ui.Observable.List.unsubscribe subscription

let test_no_encryption_is_empty () =
  Eio_main.run @@ fun _env ->
  let model = Matrix_ui.Room_identity.create ~own_user:alice () in
  let list = Matrix_ui.Room_identity.members model room in
  Alcotest.(check int)
    "no-encryption model is empty" 0
    (Array.length (Matrix_ui.Observable.List.snapshot list))

let test_room_identity_pin_violation () =
  Eio_main.run @@ fun _env ->
  let machine =
    Matrix_eio.Encryption.create ~random:(random "p") ~user_id:alice
      ~device_id:(Id.Device_id.of_string_exn "ALICE")
      ()
  in
  let _, first = identity ~seed:"d" bob in
  let _, rotated = identity ~seed:"f" bob in
  Matrix_eio.Encryption.receive_keys_query machine (query bob first);
  Matrix_eio.Encryption.receive_keys_query machine (query bob rotated);
  let model =
    Matrix_ui.Room_identity.create ~own_user:alice ~encryption:machine ()
  in
  let state =
    state_with_members
      [ ("@alice:example.org", "join"); ("@bob:example.org", "join") ]
  in
  Matrix_ui.Room_identity.refresh model state;
  Alcotest.(check bool)
    "unverified rotation is a distinct pin warning" true
    (warning_kinds model = [ Matrix_ui.Room_identity.Pin_violation ]);
  Matrix_eio.Encryption.pin_user_identity machine bob;
  Matrix_ui.Room_identity.refresh model state;
  Alcotest.(check (list string))
    "pin acknowledgement clears the warning" [] (warning_ids model)

let () =
  Alcotest.run "room identity"
    [
      ( "projection",
        [
          Alcotest.test_case "room-scoped violation lifecycle" `Quick
            test_room_identity_projection;
          Alcotest.test_case "no encryption" `Quick test_no_encryption_is_empty;
          Alcotest.test_case "pin violation" `Quick
            test_room_identity_pin_violation;
        ] );
    ]
