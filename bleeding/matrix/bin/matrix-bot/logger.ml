open Matrix_bot
module Ui = Matrix_ui

let describe (event : Event.t) =
  let detail =
    match event with
    | Event.Message m -> " " ^ m.content.body
    | Event.Edit e -> " " ^ e.message.content.body
    | Event.Command c -> " " ^ c.message.content.body
    | Event.Sticker { body; _ } -> " " ^ body
    | Event.Poll { text; _ } -> " " ^ text
    | Event.Reaction { key; _ } -> " " ^ key
    | _ -> ""
  in
  Format.asprintf "%a%s" Event.pp event detail

let print_rooms out bot =
  let rooms =
    Ui.Observable.List.snapshot
      (Ui.Room_list.rooms (Ui.Runtime.room_list (Bot.runtime bot)))
  in
  Format.fprintf out "%d room(s):@." (Array.length rooms);
  Array.iter
    (fun (room : Ui.Room_list.room) ->
      Format.fprintf out "  %s%s - %s@." room.name
        (if room.encrypted then " [e2ee]" else "")
        (match room.latest with
        | Some preview -> Ui.Matching.truncate_graphemes ~max:60 preview
        | None -> "(nothing yet)"))
    rooms

(* Paging back from the [Joined] handler is the whole of the back-fill: the
   pages land in the event cache, the room's collector sees them and hands
   them on, and the printer below prints them without knowing where they
   came from. *)
let page out room ~pages =
  Format.fprintf out "%s@."
    (match Room.backfill room ~pages () with
    | Ok `Reached_start -> "--- the beginning of " ^ Room.name room ^ " ---"
    | Ok `More ->
        Printf.sprintf "--- %d page(s) of %s fetched, more above ---" pages
          (Room.name room)
    | Ok `Nothing_to_do ->
        "--- nothing to page in for " ^ Room.name room ^ " ---"
    | Error error ->
        Printf.sprintf "--- could not page in %s: %s ---" (Room.name room)
          (Matrix_client.Error.to_string error))

let plugin ?(out = Format.std_formatter) ?(backfill = 20) spec =
  spec
  |> Bot.on_join (fun _ room ->
      if backfill > 0 then page out room ~pages:backfill)
  |> Bot.on (fun _ event -> Format.fprintf out "%s@." (describe event))
  |> Bot.on_sync (fun bot -> function
    | Matrix_ui.Runtime.Live _ -> print_rooms out bot
    | _ -> ())
