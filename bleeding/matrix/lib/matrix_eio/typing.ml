let set_typing client ~room_id ~typing ?timeout () =
  Error.unwrap ~context:"setting typing notification"
    (Matrix_client.Typing.set_typing (Client.base client) ~room_id ~typing
       ?timeout ())
