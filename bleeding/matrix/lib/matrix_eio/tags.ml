let favourite = Matrix_client.Tags.favourite
let low_priority = Matrix_client.Tags.low_priority
let server_notice = Matrix_client.Tags.server_notice

let get client ~user_id ~room_id =
  Error.unwrap ~context:"getting room tags"
    (Matrix_client.Tags.get (Client.base client) ~user_id ~room_id)

let set client ~user_id ~room_id ~tag ?order () =
  Error.unwrap ~context:"setting room tag"
    (Matrix_client.Tags.set (Client.base client) ~user_id ~room_id ~tag ?order
       ())

let remove client ~user_id ~room_id ~tag =
  Error.unwrap ~context:"removing room tag"
    (Matrix_client.Tags.remove (Client.base client) ~user_id ~room_id ~tag)

let set_favourite client ~room_id ~favourite ?order () =
  Error.unwrap ~context:"setting favourite room tag"
    (Matrix_client.Tags.set_favourite (Client.base client) ~room_id ~favourite
       ?order ())

let set_low_priority client ~room_id ~low_priority ?order () =
  Error.unwrap ~context:"setting low-priority room tag"
    (Matrix_client.Tags.set_low_priority (Client.base client) ~room_id
       ~low_priority ?order ())
