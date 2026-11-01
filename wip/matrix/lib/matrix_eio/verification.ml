module V = Matrix_client.Verification
module Cancel_code = V.Cancel_code
module Method = V.Method
module Transaction = V.Transaction
module Message = V.Message
module Sas = V.Sas
module Qr = V.Qr
module Flow = V.Flow
module Cross_signing = Matrix_client.Cross_signing

let send_to_devices client ~their_user_id ~devices msg =
  match Message.to_json msg with
  | Error (`Msg m) -> raise (Error.err (Error.Json m))
  | Ok json ->
      To_device.send_with_new_txn client ~event_type:(Message.event_type msg)
        [ (their_user_id, List.map (fun d -> (d, json)) devices) ]
