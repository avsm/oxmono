let receive client =
  let message = Mqttz_eio.receive client in
  Owntracks.Mqtt.of_mqtt ~topic:message.topic ~payload:message.payload

let publish ?qos ?retain client ~topic message =
  match Owntracks.Message.encode message with
  | Error message -> invalid_arg message
  | Ok payload -> Mqttz_eio.publish ?qos ?retain client ~topic payload
