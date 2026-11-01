module Bot = Matrix_bot.Bot
module Room = Matrix_bot.Room
module Sent = Matrix_bot.Sent
module Main = Matrix_bot.Main
module Id = Matrix_proto.Id

let notify ~room_id ~body bot room =
  if Id.Room_id.equal (Room.id room) room_id then (
    (match Sent.await (Room.send_text room body) with
    | Sent.Sent event_id -> print_endline (Id.Event_id.to_string event_id)
    | Sent.Uploaded _ ->
        Printf.eprintf "notify: an unexpected media upload completed\n%!"
    | Sent.Failed (Some error) ->
        Printf.eprintf "notify: %s\n%!" (Matrix_client.Error.to_string error)
    | Sent.Failed None -> Printf.eprintf "notify: the send failed\n%!"
    | Sent.Cancelled -> Printf.eprintf "notify: the send was cancelled\n%!"
    | Sent.Timed_out -> Printf.eprintf "notify: the send timed out\n%!");
    Bot.stop bot)

let build room_id body =
  Bot.v ~name:"notify" () |> Bot.on_join (notify ~room_id ~body)

let room =
  Cmdliner.Arg.(
    required
    & opt (some Matrix_cli.room_id_conv) None
    & info [ "room" ] ~docv:"ROOM_ID" ~doc:"The room to send $(b,--body) to.")

let body =
  Cmdliner.Arg.(
    required
    & opt (some string) None
    & info [ "body" ] ~docv:"TEXT" ~doc:"The message to send.")

let () =
  Main.run ~name:"notify" ~doc:"Sends one message to a room and exits"
    Cmdliner.Term.(const build $ room $ body)
