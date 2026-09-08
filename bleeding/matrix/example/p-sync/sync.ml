module S = Matrix_proto.Sync
module E = Matrix_proto.Event

let read_file path = In_channel.with_open_bin path In_channel.input_all

let print_state_event (event : E.Raw_event.t) =
  Printf.printf "  state %s %S\n"
    (E.Event_type.to_string event.type_)
    (Option.value event.state_key ~default:"")

let print_timeline_event (event : E.Raw_event.t) =
  let sender = Matrix_proto.Id.User_id.to_string event.sender in
  let type_ = E.Event_type.to_string event.type_ in
  match
    if E.Event_type.equal event.type_ E.Event_type.Room_message then
      Jsont.Json.decode E.Text_message_content.jsont event.content
      |> Result.to_option
    else None
  with
  | Some content ->
      Printf.printf "  %s %s: %s\n" sender type_
        (E.Text_message_content.body content)
  | None -> Printf.printf "  %s %s\n" sender type_

let print_room (room_id, (room : S.Joined_room.t)) =
  Printf.printf "%s\n" room_id;
  (match room.state with
  | None -> ()
  | Some state -> List.iter print_state_event state.events);
  match room.timeline with
  | None -> ()
  | Some timeline ->
      List.iter print_timeline_event timeline.events;
      Printf.printf "  limited: %b\n"
        (Option.value timeline.limited ~default:false)

let () =
  let path = if Array.length Sys.argv > 1 then Sys.argv.(1) else "sync.json" in
  match Jsont_bytesrw.decode_string S.Response.jsont (read_file path) with
  | Error e ->
      Printf.eprintf "%s\n" e;
      exit 1
  | Ok response -> (
      Printf.printf "next_batch %s\n" response.next_batch;
      match response.rooms with
      | None -> ()
      | Some rooms -> List.iter print_room rooms.join)
