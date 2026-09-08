module Bot = Matrix_bot.Bot
module Room = Matrix_bot.Room
module Event = Matrix_bot.Event
module Main = Matrix_bot.Main
module Id = Matrix_proto.Id
module P = Matrix_ui.Presentation

let membership bot (m : Event.membership) =
  let room = m.envelope.room in
  let who = Id.User_id.to_string m.user in
  (if not (Id.User_id.equal m.user (Bot.user_id bot)) then
     match m.change with
     | P.Joined | P.Invitation_accepted | P.Knock_accepted ->
         ignore (Room.send_notice room (Printf.sprintf "Welcome, %s." who))
     | P.Left ->
         ignore (Room.send_notice room (Printf.sprintf "%s left the room." who))
     | P.Kicked | P.Kicked_and_banned ->
         ignore
           (Room.send_notice room
              (Printf.sprintf "%s was removed from the room." who))
     | _ -> ());
  match Room.members room with
  | [ only ] when Id.User_id.equal only (Bot.user_id bot) ->
      ignore (Room.leave room)
  | _ -> ()

let room_state _ (s : Event.room_state) =
  let room = s.envelope.room in
  match s.state with
  | P.Room_name (Some name) ->
      ignore
        (Room.send_notice room
           (Printf.sprintf "The room is now called %s." name))
  | P.Room_topic (Some topic) ->
      ignore
        (Room.send_notice room (Printf.sprintf "The topic is now %s." topic))
  | _ -> ()

let invite _ (i : Event.invitation) =
  Printf.printf "invited to %s by %s\n%!"
    (Id.Room_id.to_string i.room_id)
    (match i.inviter with
    | Some user -> Id.User_id.to_string user
    | None -> "an unknown user")

let spec =
  Bot.v ~name:"membership" ()
  |> Bot.on_membership membership
  |> Bot.on_room_state room_state
  |> Bot.on_invite invite

let () =
  Main.run ~name:"membership"
    ~doc:"Narrates who is in the room and what it is called"
    (Cmdliner.Term.const spec)
