open Zulip_bot

let spec =
  Bot.v ()
  |> Greeting.command ~name:"hello" ~greeting:"Hello"
  |> Greeting.command ~name:"bye" ~greeting:"Goodbye"
  |> Bot.help

let () = Zulip_bot_cli.Main.run ~name:"plugins" spec ()
