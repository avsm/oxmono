open Zulip_bot

let command ~name ~greeting : Bot.plugin =
  Bot.command ~name ~args:"[NAME]" ~doc:("say " ^ greeting) (fun _ command ->
      let name = if command.args = "" then "there" else command.args in
      ignore
        (Event.reply command.message.envelope
           (Printf.sprintf "%s, %s!" greeting name)))
