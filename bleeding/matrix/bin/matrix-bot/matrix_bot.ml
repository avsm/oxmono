(** One executable over the plugins in [bin/matrix-bot]. *)

open Cmdliner
module Bot = Matrix_bot.Bot
module Main = Matrix_bot.Main
module Id = Matrix_proto.Id

let echo =
  Term.(
    const (fun reply_prefix -> Matrix_bots.Echo.plugin ~reply_prefix)
    $ Arg.(
        value & opt string "you said: "
        & info [ "prefix" ] ~docv:"TEXT"
            ~doc:"What $(b,--echo) puts before a body."))

let moderator =
  Term.(
    const (fun words strikes -> Matrix_bots.Moderator.plugin ~words ~strikes)
    $ Arg.(
        value
        & opt_all string [ "badger"; "spoiler" ]
        & info [ "w"; "words" ] ~docv:"WORD"
            ~doc:"A word $(b,--moderator) redacts. Repeatable.")
    $ Arg.(
        value & opt int 3
        & info [ "strikes" ] ~docv:"N"
            ~doc:"How many strikes $(b,--moderator) allows before a kick."))

let plugins =
  Term.(
    const (fun echo commands welcome moderator logger ->
        [ echo; commands; welcome; moderator; logger ])
    $ Main.plugin_flag ~name:"echo" ~doc:"Repeat every message back." echo
    $ Main.plugin_flag ~name:"commands"
        ~doc:
          "Answer $(b,!ping), $(b,!roll), $(b,!react), $(b,!topic) and \
           $(b,!help)."
        (Term.const Matrix_bots.Commands.plugin)
    $ Main.plugin_flag ~name:"welcome"
        ~doc:"Narrate arrivals, departures and room state."
        (Term.const Matrix_bots.Welcome.plugin)
    $ Main.plugin_flag ~name:"moderator"
        ~doc:"Redact forbidden words, warn, and kick on the last strike."
        moderator
    $ Main.plugin_flag ~name:"logger"
        ~doc:"Print the room list and every event."
        (Term.const (fun spec -> Matrix_bots.Logger.plugin spec)))

(* [--notify] is a mode rather than a plugin. It uses [Main.Run_once] so its
   result, unlike a self-stopping bot, can become the process's exit status. *)
let room_of_string text =
  match Id.Room_id.of_string text with
  | Ok room_id -> Ok (`Room_id room_id)
  | Error _ -> (
      match Id.Room_alias.of_string text with
      | Ok alias -> Ok (`Room_alias alias)
      | Error (`Msg message) ->
          Error (`Msg (text ^ " is not a room id or alias: " ^ message)))

let notify =
  Term.(
    const (fun room body ->
        match (room, body) with
        | None, _ -> Ok None
        | Some _, "" -> Error (`Msg "--notify needs --body")
        | Some room, body ->
            Result.map (fun room -> Some (room, body)) (room_of_string room))
    $ Arg.(
        value
        & opt (some string) None
        & info [ "notify" ] ~docv:"ROOM"
            ~doc:"Send one message to $(i,ROOM), print its event id and exit.")
    $ Arg.(
        value & opt string ""
        & info [ "body" ] ~docv:"TEXT" ~doc:"What $(b,--notify) sends."))

let notify_action room body ctx =
  match Matrix_bots.Notify.send ctx ~room ~body () with
  | Ok event_id ->
      print_endline (Id.Event_id.to_string event_id);
      Cmd.Exit.ok
  | Error message ->
      prerr_endline ("matrix-bot: " ^ message);
      Cmd.Exit.some_error

let mode =
  Term.(
    const (fun notify spec ->
        match notify with
        | Some (room, body) -> Main.Run_once (notify_action room body)
        | None -> Main.Run_bot spec)
    $ term_result notify
    $ Main.compose plugins (Bot.v ~name:"matrix-bot" ()))

let man =
  [
    `S Manpage.s_description;
    `P
      "Runs the example plugins from $(b,example). Each $(b,--plugin) flag \
       adds one to the same bot, so several can run in one room at once. With \
       none of them the bot joins what it is invited to and says nothing.";
    `P
      "$(b,--notify) is the exception: it sends one message to a room, prints \
       the event id and returns, which is the shape a cron job wants. The \
       plugin flags are ignored with it.";
    `S Manpage.s_examples;
    `Pre "  matrix-bot --username echobot --profile echobot --echo --commands";
    `Noblank;
    `Pre "  matrix-bot --profile cronbot --notify '#ops:localhost' --body done";
  ]

let () =
  Main.run_mode ~name:"matrix-bot"
    ~doc:"Matrix bots built from matrix-chat.bot plugins" ~man mode
