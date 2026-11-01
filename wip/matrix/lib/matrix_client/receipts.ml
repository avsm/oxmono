open Result.Syntax

type receipt_type = Read | Read_private | Fully_read

let receipt_type_to_string = function
  | Read -> "m.read"
  | Read_private -> "m.read.private"
  | Fully_read -> "m.fully_read"

let send_route = Route.v "/rooms/{room_id}/receipt/{receipt_type}/{event_id}"
let read_markers_route = Route.v "/rooms/{room_id}/read_markers"

let send_receipt client ~room_id ~event_id ?(receipt_type = Read) ?thread_id ()
    =
  match (receipt_type, thread_id) with
  | Fully_read, Some _ ->
      Error
        (Error.Policy_denied "m.fully_read receipts cannot carry a thread_id")
  | _ -> (
      let path =
        Route.expand_exn send_route
          [
            ("room_id", Matrix_proto.Id.Room_id.to_string room_id);
            ("receipt_type", receipt_type_to_string receipt_type);
            ("event_id", Matrix_proto.Id.Event_id.to_string event_id);
          ]
      in
      let* body =
        match thread_id with
        | None -> Ok "{}"
        | Some thread_id -> (
            let json =
              Jsont.Json.object'
                [
                  Jsont.Json.mem
                    (Jsont.Json.name "thread_id")
                    (Jsont.Json.string
                       (Matrix_proto.Id.Event_id.to_string thread_id));
                ]
            in
            match
              Jsont_bytesrw.encode_string Matrix_proto.Json.Codec.json json
            with
            | Ok body -> Ok body
            | Error message -> Error (Error.Json_error message))
      in
      let* _ = Client.Http.post client ~path ~body () in
      match thread_id with
      | Some _ -> Ok ()
      | None -> Account_data.set_marked_unread client ~room_id ~unread:false)

type read_marker_request = {
  fully_read : string option;
  read : string option;
  read_private : string option;
}
[@@warning "-69"]

let read_marker_request_jsont =
  Jsont.Object.(
    map (fun fully_read read read_private -> { fully_read; read; read_private })
    |> opt_mem "m.fully_read" Matrix_proto.Json.Codec.string ~enc:(fun t ->
        t.fully_read)
    |> opt_mem "m.read" Matrix_proto.Json.Codec.string ~enc:(fun t -> t.read)
    |> opt_mem "m.read.private" Matrix_proto.Json.Codec.string ~enc:(fun t ->
        t.read_private)
    |> finish)

let set_read_marker client ~room_id ?fully_read ?read ?read_private () =
  let path =
    Route.expand_exn read_markers_route
      [ ("room_id", Matrix_proto.Id.Room_id.to_string room_id) ]
  in
  let request =
    {
      fully_read = Option.map Matrix_proto.Id.Event_id.to_string fully_read;
      read = Option.map Matrix_proto.Id.Event_id.to_string read;
      read_private = Option.map Matrix_proto.Id.Event_id.to_string read_private;
    }
  in
  if fully_read = None && read = None && read_private = None then
    Error (Error.Policy_denied "a read-markers request cannot be empty")
  else
    let* body = Client.Http.encode_body read_marker_request_jsont request in
    let* _ = Client.Http.post client ~path ~body () in
    Account_data.set_marked_unread client ~room_id ~unread:false
