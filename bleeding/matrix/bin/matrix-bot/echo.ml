open Matrix_bot

(* An [m.notice] is what stops two echo bots in one room from talking for
   ever: the default spec drops notices, and this is one. *)
let plugin ?(reply_prefix = "you said: ") =
  Bot.on_message (fun _ (message : Event.message) ->
      ignore
        (Room.send_notice message.envelope.room
           (reply_prefix ^ message.content.body)))
