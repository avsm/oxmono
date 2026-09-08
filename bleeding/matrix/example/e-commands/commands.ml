module Bot = Matrix_bot.Bot
module Room = Matrix_bot.Room
module Event = Matrix_bot.Event
module Args = Matrix_bot.Args
module Context = Matrix_bot.Context
module Main = Matrix_bot.Main

let ping _ (c : Event.command) = ignore (Event.reply c.message.envelope "pong")

let random bot =
  Matrix_client.Client.random
    (Matrix_eio.Client.base (Context.client (Bot.context bot)))

let roll bot (c : Event.command) =
  let sides =
    match Args.find_word c 0 with None -> Some 6 | Some _ -> Args.find_int c 0
  in
  match sides with
  | Some sides when sides >= 1 ->
      let bytes = Matrix_client.Random.generate (random bot) 2 in
      let n = (Char.code bytes.[0] * 256) + Char.code bytes.[1] in
      ignore
        (Event.reply c.message.envelope (string_of_int ((n mod sides) + 1)))
  | Some _ | None -> ignore (Event.reply c.message.envelope "usage: !roll [N]")

let thumbs_up _ (c : Event.command) =
  ignore (Event.react c.message.envelope "\xf0\x9f\x91\x8d" (* 👍 *))

let topic _ (c : Event.command) =
  if String.equal c.args "" then
    ignore (Event.reply c.message.envelope "usage: !topic TEXT")
  else
    match Room.set_topic c.message.envelope.room c.args with
    | Ok () -> ignore (Event.react c.message.envelope "\xe2\x9c\x85" (* ✅ *))
    | Error error ->
        ignore
          (Event.reply c.message.envelope
             (Format.asprintf "cannot set the topic: %a" Matrix_client.Error.pp
                error))

let unknown _ (c : Event.command) =
  ignore
    (Event.reply c.message.envelope
       (Printf.sprintf "unknown command !%s, try !help" c.name))

let spec =
  Bot.v ~name:"commands" ()
  |> Bot.command ~name:"ping" ~doc:"answer pong, as a reply" ping
  |> Bot.command ~name:"roll" ~args:"[N]"
       ~doc:"roll an N-sided die, N defaults to 6" roll
  |> Bot.command ~name:"react" ~doc:"react to the command with a thumbs up"
       thumbs_up
  |> Bot.command ~name:"topic" ~args:"TEXT" ~doc:"set the room topic"
       ~admin:true topic
  |> Bot.help
  |> Bot.on_unknown_command unknown

let () =
  Main.run ~name:"commands"
    ~doc:"A bot with commands, arguments and generated help"
    (Cmdliner.Term.const spec)
