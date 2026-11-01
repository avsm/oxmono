open Zulip_bot

let spec =
  Bot.v ()
  |> Bot.on_message (fun _ message ->
      ignore (Event.reply message.envelope ("You said: " ^ message.body)))

let () = Zulip_bot_cli.Main.run ~name:"echo" spec ()
