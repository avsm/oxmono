open Result.Syntax

type notification = {
  actions : Matrix_proto.Push.Action.t list;
  event : Matrix_proto.Event.Raw_event.t;
  profile_tag : string option;
  read : bool;
  room_id : Matrix_proto.Id.Room_id.t;
  ts : Matrix_proto.Event.Timestamp.t;
}

let notification_jsont =
  Jsont.Object.(
    map ~kind:"notification" (fun actions event profile_tag read room_id ts ->
        { actions; event; profile_tag; read; room_id; ts })
    |> mem "actions"
         (Jsont.list Matrix_proto.Push.Action.jsont)
         ~dec_absent:(fun () -> [])
         ~enc:(fun t -> t.actions)
    |> mem "event" Matrix_proto.Event.Raw_event.jsont ~enc:(fun t -> t.event)
    |> mem "profile_tag"
         (Jsont.option Matrix_proto.Json.Codec.string)
         ~dec_absent:(fun () -> None)
         ~enc:(fun t -> t.profile_tag)
    |> mem "read" Jsont.bool
         ~dec_absent:(fun () -> false)
         ~enc:(fun t -> t.read)
    |> mem "room_id" Matrix_proto.Id.Room_id.jsont ~enc:(fun t -> t.room_id)
    |> mem "ts" Matrix_proto.Event.Timestamp.jsont ~enc:(fun t -> t.ts)
    |> finish)

type notifications = { chunk : notification list; next_token : string option }

let notifications_jsont =
  Jsont.Object.(
    map ~kind:"notifications" (fun chunk next_token -> { chunk; next_token })
    |> mem "notifications"
         (Jsont.list notification_jsont)
         ~dec_absent:(fun () -> [])
         ~enc:(fun t -> t.chunk)
    |> mem "next_token"
         (Jsont.option Matrix_proto.Json.Codec.string)
         ~dec_absent:(fun () -> None)
         ~enc:(fun t -> t.next_token)
    |> finish)

let get client ?from ?limit ?only () =
  let query =
    List.filter_map Fun.id
      [
        Option.map (fun f -> ("from", f)) from;
        Option.map (fun l -> ("limit", string_of_int l)) limit;
        Option.map (fun `Highlight -> ("only", "highlight")) only;
      ]
  in
  let query = if query = [] then None else Some query in
  let* body = Client.Http.get client ~path:"/notifications" ?query () in
  Client.Http.decode_response notifications_jsont body
