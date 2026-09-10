type t = {
  message : Matrix_bot.Event.message;
  original : Matrix_proto.Id.Event_id.t option;
}

let register handle spec =
  let message bot message = handle bot { message; original = None } in
  spec
  |> Matrix_bot.Bot.command ~name:"crow" (fun bot command ->
      message bot command.message)
  |> Matrix_bot.Bot.on_unknown_command (fun bot command ->
      message bot command.message)
  |> Matrix_bot.Bot.on_message message
  |> Matrix_bot.Bot.on_edit (fun bot edit ->
      (* Without m.new_content the library retains the fallback presentation.
         That fallback is not a revised command. *)
      if edit.message.presentation.relation = None then begin
        let message = edit.message in
        let presentation =
          Matrix_ui.Presentation.of_event message.presentation.raw
        in
        let reply_to =
          match presentation.relation with
          | Some { kind = Matrix_ui.Presentation.Reply; target } -> Some target
          | _ -> None
        in
        handle bot
          {
            message = { message with presentation; reply_to };
            original = Some edit.original;
          }
      end)
