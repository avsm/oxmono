(** args — the command parser, and lookups over a command's words.

    A message becomes an {!Event.command} when it begins with the bot's prefix.
    {!parse} is what decides that, and the rest reads the words that follow. *)

val parse : prefix:string -> string -> (string * string) option
(** [parse ~prefix body] is the command name and its argument string when [body]
    begins with [prefix], and [None] otherwise. [body] is trimmed first, the
    name runs to the first whitespace, and the arguments are what follows,
    trimmed. A body that is [prefix] and nothing else is not a command. *)

val argv : string -> string list
(** [argv args] is [args] split on whitespace, without empty words. It is what
    {!Event.command.argv} holds, for a plugin that splits a subcommand's own
    arguments the same way. *)

val find_word : Event.command -> int -> string option
(** [find_word c n] is the [n]th word of the command's arguments, counting from
    zero, and [None] when there are fewer. *)

val find_int : Event.command -> int -> int option
(** [find_int c n] is {!find_word} read as a decimal integer, and [None] when
    the word is missing or is not one. *)

val find_user : Event.command -> int -> Matrix_proto.Id.User_id.t option
(** [find_user c n] is {!find_word} read as a user identifier, and [None] when
    the word is missing or is not one. *)
