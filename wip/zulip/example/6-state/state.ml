open Zulip_bot

let count bot (command : Event.command) =
  let room = Room.key command.message.envelope.room in
  let result =
    Plugin_store.update (Bot.plugin_store bot) ~room ~plugin:"tutorial.counter"
      ~key:"value" Jsont.int (fun old -> Option.value ~default:0 old + 1)
  in
  let reply =
    match result with
    | Ok value -> Printf.sprintf "Count: %d" value
    | Error error ->
        Format.eprintf "Counter update failed: %a@." Plugin_store.pp_error error;
        "I could not save the counter."
  in
  ignore (Event.reply command.message.envelope reply)

let spec =
  Bot.v ()
  |> Bot.command ~name:"count" ~doc:"increment this conversation's counter"
       count
  |> Bot.help

let () = Zulip_bot_cli.Main.run ~name:"state" spec ()
