open Zulip_bot

let where _ (command : Event.command) =
  let room = command.message.envelope.room in
  let description =
    if Room.is_direct room then
      Printf.sprintf "A direct message with %d participants."
        (List.length (Room.participants room))
    else
      Printf.sprintf "A channel, in topic %S."
        (Option.value ~default:"" (Room.topic room))
  in
  ignore
    (Event.reply command.message.envelope
       (description ^ " Conversation key: " ^ Room.key room))

let spec =
  Bot.v ()
  |> Bot.command ~name:"where" ~doc:"describe this conversation" where
  |> Bot.help

let () = Zulip_bot_cli.Main.run ~name:"rooms" spec ()
