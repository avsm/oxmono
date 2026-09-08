open Zulip_bot

let ping _ (command : Event.command) =
  ignore (Event.reply command.message.envelope "pong")

let echo _ (command : Event.command) =
  let reply = if command.args = "" then "Usage: !echo TEXT" else command.args in
  ignore (Event.reply command.message.envelope reply)

let spec =
  Bot.v ()
  |> Bot.command ~name:"ping" ~doc:"check that the bot is alive" ping
  |> Bot.command ~name:"echo" ~args:"TEXT" ~doc:"repeat your text" echo
  |> Bot.help

let () = Zulip_bot_cli.Main.run ~name:"commands" spec ()
