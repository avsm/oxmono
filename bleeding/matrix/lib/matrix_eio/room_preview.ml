type t = Matrix_client.Room_preview.t

let get client ~store ~room_id_or_alias ?via () =
  Error.unwrap ~context:"getting room preview"
    (Matrix_client.Room_preview.get (Client.base client) ~store
       ~room_id_or_alias ?via ())
