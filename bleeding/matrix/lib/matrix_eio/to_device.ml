type messages = Matrix_client.To_device.messages

type recipient = Matrix_client.To_device.recipient =
  | All
  | Device of Matrix_proto.Id.Device_id.t

let send client ~event_type ~txn_id messages =
  Error.unwrap ~context:"sending to-device messages"
    (Matrix_client.To_device.send (Client.base client) ~event_type ~txn_id
       messages)

let send_with_new_txn client ~event_type messages =
  Error.unwrap ~context:"sending to-device messages with new transaction"
    (Matrix_client.To_device.send_with_new_txn (Client.base client) ~event_type
       messages)
