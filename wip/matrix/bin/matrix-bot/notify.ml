open Matrix_bot
module Directory = Matrix_client.Directory

let clock bot = Context.clock (Bot.context bot)

let until bot ~deadline predicate =
  let rec loop () =
    predicate ()
    || Eio.Time.now (clock bot) < deadline
       &&
       (Eio.Time.sleep (clock bot) 0.1;
        loop ())
  in
  loop ()

(* Joining is idempotent on the server, but a cron run already in the room
   should not spend a round trip on it. An alias has to go to the server. *)
let resolve bot room =
  match room with
  | `Room_id room_id when Bot.find_room bot room_id <> None -> Ok room_id
  | room_id_or_alias ->
      Result.map_error
        (fun error ->
          Format.asprintf "cannot join %s: %a"
            (Directory.room_id_or_alias_to_string room_id_or_alias)
            Matrix_client.Error.pp error)
        (Matrix_client.Rooms.join
           (Matrix_eio.Client.base (Context.client (Bot.context bot)))
           ~room_id_or_alias ())

let attempt ~room ~body ~timeout bot =
  let deadline = Eio.Time.now (clock bot) +. timeout in
  match resolve bot room with
  | Error message -> Error message
  | Ok room_id -> (
      ignore (until bot ~deadline (fun () -> Bot.find_room bot room_id <> None));
      match Bot.find_room bot room_id with
      | None -> Error "the room did not appear in the room list"
      | Some the_room -> (
          match Room.sync_members the_room with
          | Error error ->
              Error
                ("cannot synchronize the room's members: "
                ^ Matrix_client.Error.to_string error)
          | Ok () -> (
              let remaining =
                Float.max 0. (deadline -. Eio.Time.now (clock bot))
              in
              if not (Room.await_ready_to_send ~timeout:remaining the_room) then
                Error "the room did not become safe to send to"
              else
                let remaining =
                  Float.max 0. (deadline -. Eio.Time.now (clock bot))
                in
                match
                  Sent.await ~timeout:remaining (Room.send_text the_room body)
                with
                | Sent.Sent event_id -> Ok event_id
                | Sent.Uploaded _ ->
                    Error "an unexpected media upload completed"
                | Sent.Timed_out ->
                    Error "the message was not delivered in time"
                | Sent.Failed None -> Error "the send failed"
                | Sent.Failed (Some error) ->
                    Error
                      ("the send failed: " ^ Matrix_client.Error.to_string error)
                | Sent.Cancelled -> Error "the send was cancelled")))

let spec ~room ~body ?(timeout = 120.) report =
  (* The room is joined by name rather than waited for, so an invite is not
     what starts this bot; a live sync loop is, and [Joined] only if the room
     list reaches the bot first. Either way it runs once. *)
  let busy = ref false in
  let once bot outcome =
    if not !busy then (
      busy := true;
      report (outcome bot);
      Bot.stop bot)
  in
  Bot.v ~name:"notify" ~auto_join:false ()
  |> Bot.on_sync (fun bot -> function
    | Matrix_ui.Runtime.Live _ -> once bot (attempt ~room ~body ~timeout)
    | Matrix_ui.Runtime.Failed message ->
        once bot (fun _ -> Error ("the sync loop failed: " ^ message))
    | Matrix_ui.Runtime.Offline | Matrix_ui.Runtime.Not_started
    | Matrix_ui.Runtime.Syncing | Matrix_ui.Runtime.Stopped ->
        ())
  |> Bot.on_join (fun bot _ -> once bot (attempt ~room ~body ~timeout))

let send ctx ~room ~body ?(timeout = 120.) () =
  let outcome = ref (Error "the bot stopped before it could send") in
  Bot.run ctx (spec ~room ~body ~timeout (fun result -> outcome := result));
  !outcome
