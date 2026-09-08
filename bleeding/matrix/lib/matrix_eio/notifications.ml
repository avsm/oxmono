type notification = Matrix_client.Notifications.notification = {
  actions : Matrix_proto.Push.Action.t list;
  event : Matrix_proto.Event.Raw_event.t;
  profile_tag : string option;
  read : bool;
  room_id : Matrix_proto.Id.Room_id.t;
  ts : Matrix_proto.Event.Timestamp.t;
}

type notifications = Matrix_client.Notifications.notifications = {
  chunk : notification list;
  next_token : string option;
}

let get client ?from ?limit ?only () =
  Error.unwrap ~context:"getting notifications"
    (Matrix_client.Notifications.get (Client.base client) ?from ?limit ?only ())
