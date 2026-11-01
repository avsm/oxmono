module E = Matrix_proto.Event
module Id = Matrix_proto.Id

let member_event =
  {|{"content": {"membership": "join", "displayname": "Alice"},
     "event_id": "$abc123:localhost",
     "origin_server_ts": 1700000000000,
     "sender": "@alice:localhost",
     "state_key": "@alice:localhost",
     "type": "m.room.member"}|}

let () =
  let content = E.Text_message_content.make ~body:"Hello, Matrix" () in
  let json =
    match Jsont_bytesrw.encode_string E.Text_message_content.jsont content with
    | Ok json -> json
    | Error e -> failwith e
  in
  Printf.printf "%s\n" json;
  (match Jsont_bytesrw.decode_string E.Text_message_content.jsont json with
  | Error e -> failwith e
  | Ok content ->
      Printf.printf "body: %s\n" (E.Text_message_content.body content));

  match Jsont_bytesrw.decode_string E.Raw_event.jsont member_event with
  | Error e -> failwith e
  | Ok event ->
      (Printf.printf "sender: %s\n" (Id.User_id.to_string event.sender);
       (match event.event_id with
       | None -> ()
       | Some event_id ->
           Printf.printf "event_id: %s\n" (Id.Event_id.to_string event_id));
       match Jsont.Json.decode E.Room_member_content.jsont event.content with
       | Error e -> failwith e
       | Ok member ->
           Printf.printf "membership: %s\n"
             (E.Membership.to_string (E.Room_member_content.membership member)));

      (match Id.User_id.of_string "not-a-user-id" with
      | Ok _ -> assert false
      | Error (`Msg msg) -> Printf.printf "rejected: %s\n" msg);
      (match Id.User_id.of_string "@Alice_B:example.org" with
      | Error _ -> assert false
      | Ok uid ->
          Printf.printf "historical, spec conformant: %b\n"
            (Id.User_id.is_spec_conformant uid));

      let alice = Id.User_id.of_string_exn "@alice:localhost" in
      let alice' = Id.User_id.of_string_exn "@alice:localhost" in
      let bob = Id.User_id.of_string_exn "@bob:localhost" in
      Printf.printf "alice equal alice: %b\n" (Id.User_id.equal alice alice');
      Printf.printf "alice before bob: %b\n" (Id.User_id.compare alice bob < 0)
