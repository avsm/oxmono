(** Matrix messages, commands and edits accepted by Crow's text adapter. *)

type t = {
  message : Matrix_bot.Event.message;
  original : Matrix_proto.Id.Event_id.t option;
}

val register :
  (Matrix_bot.Bot.t -> t -> unit) -> Matrix_bot.Bot.spec -> Matrix_bot.Bot.spec
(** [register handle spec] sends text events and replacements to [handle]. Edits
    carry their own event ID and authenticated sender, with [original]
    identifying the message to reply to. Malformed replacement fallbacks are
    ignored. The handler must check content type and authorize the sender. *)
