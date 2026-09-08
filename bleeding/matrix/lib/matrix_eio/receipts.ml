type receipt_type = Matrix_client.Receipts.receipt_type =
  | Read
  | Read_private
  | Fully_read

let send_receipt client ~room_id ~event_id ?receipt_type ?thread_id () =
  Error.unwrap ~context:"sending receipt"
    (Matrix_client.Receipts.send_receipt (Client.base client) ~room_id ~event_id
       ?receipt_type ?thread_id ())

let set_read_marker client ~room_id ?fully_read ?read ?read_private () =
  Error.unwrap ~context:"setting read marker"
    (Matrix_client.Receipts.set_read_marker (Client.base client) ~room_id
       ?fully_read ?read ?read_private ())
