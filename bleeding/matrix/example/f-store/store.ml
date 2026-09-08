module Bot = Matrix_bot.Bot
module Room = Matrix_bot.Room
module Event = Matrix_bot.Event
module Args = Matrix_bot.Args
module Plugin_store = Matrix_bot.Plugin_store
module Main = Matrix_bot.Main

let remember bot (c : Event.command) =
  match String.index_opt c.args ' ' with
  | None -> ignore (Event.reply c.message.envelope "usage: !remember KEY TEXT")
  | Some i -> (
      let key = String.sub c.args 0 i in
      let text =
        String.trim (String.sub c.args (i + 1) (String.length c.args - i - 1))
      in
      if String.equal text "" then
        ignore (Event.reply c.message.envelope "usage: !remember KEY TEXT")
      else
        match
          Plugin_store.set (Bot.plugin_store bot)
            ~room:(Room.id c.message.envelope.room)
            ~plugin:"store" ~key Jsont.string text
        with
        | Ok () ->
            ignore (Event.react c.message.envelope "\xe2\x9c\x85" (* ✅ *))
        | Error error ->
            ignore
              (Event.reply c.message.envelope
                 ("cannot remember that: " ^ Plugin_store.error_to_string error))
      )

let recall bot (c : Event.command) =
  match Args.find_word c 0 with
  | None -> ignore (Event.reply c.message.envelope "usage: !recall KEY")
  | Some key -> (
      match
        Plugin_store.find (Bot.plugin_store bot)
          ~room:(Room.id c.message.envelope.room)
          ~plugin:"store" ~key Jsont.string
      with
      | Ok (Some text) -> ignore (Event.reply c.message.envelope text)
      | Ok None ->
          ignore
            (Event.reply c.message.envelope
               (Printf.sprintf "%s is not remembered" key))
      | Error error ->
          ignore
            (Event.reply c.message.envelope
               ("cannot recall that: " ^ Plugin_store.error_to_string error)))

let forget bot (c : Event.command) =
  match Args.find_word c 0 with
  | None -> ignore (Event.reply c.message.envelope "usage: !forget KEY")
  | Some key -> (
      match
        Plugin_store.remove (Bot.plugin_store bot)
          ~room:(Room.id c.message.envelope.room)
          ~plugin:"store" ~key ()
      with
      | Ok () -> ignore (Event.react c.message.envelope "\xe2\x9c\x85" (* ✅ *))
      | Error error ->
          ignore
            (Event.reply c.message.envelope
               ("cannot forget that: " ^ Plugin_store.error_to_string error)))

let facts bot (c : Event.command) =
  match
    Plugin_store.keys (Bot.plugin_store bot)
      ~room:(Room.id c.message.envelope.room)
      ~plugin:"store" ()
  with
  | [] -> ignore (Event.reply c.message.envelope "nothing is remembered here")
  | keys -> ignore (Event.reply c.message.envelope (String.concat ", " keys))

let count bot (c : Event.command) =
  match
    Plugin_store.update (Bot.plugin_store bot)
      ~room:(Room.id c.message.envelope.room)
      ~plugin:"store" ~key:"count" Jsont.int (function
      | None -> 1
      | Some n -> n + 1)
  with
  | Ok n -> ignore (Event.reply c.message.envelope (string_of_int n))
  | Error error ->
      ignore
        (Event.reply c.message.envelope
           ("cannot update the counter: " ^ Plugin_store.error_to_string error))

let spec =
  Bot.v ~name:"store" ()
  |> Bot.command ~name:"remember" ~args:"KEY TEXT"
       ~doc:"remember TEXT under KEY in this room" remember
  |> Bot.command ~name:"recall" ~args:"KEY"
       ~doc:"print what is remembered under KEY" recall
  |> Bot.command ~name:"forget" ~args:"KEY"
       ~doc:"forget what is remembered under KEY" forget
  |> Bot.command ~name:"facts" ~doc:"list the keys remembered in this room"
       facts
  |> Bot.command ~name:"count" ~doc:"increment and print a per-room counter"
       count
  |> Bot.help

let () =
  Main.run ~name:"store"
    ~doc:"A bot that remembers per-room facts across restarts"
    (Cmdliner.Term.const spec)
