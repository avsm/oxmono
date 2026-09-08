module Bot = Matrix_bot.Bot
module Room = Matrix_bot.Room
module Event = Matrix_bot.Event
module Main = Matrix_bot.Main

let plugin : Bot.plugin =
  Bot.on_message (fun _ (message : Event.message) ->
      ignore
        (Room.send_notice message.envelope.room
           ("you said: " ^ message.content.body)))

let spec = plugin (Bot.v ~name:"bot" ())

let () =
  Main.run ~name:"bot" ~doc:"An echo bot built from a matrix-chat.bot plugin"
    (Cmdliner.Term.const spec)
