type device = Matrix_client.Devices.device = {
  device_id : Matrix_proto.Id.Device_id.t;
  display_name : string option;
  last_seen_ip : string option;
  last_seen_ts : Matrix_proto.Event.Timestamp.t option;
}

let get_devices client =
  Error.unwrap ~context:"getting devices"
    (Matrix_client.Devices.get_devices (Client.base client))

let get_device client ~device_id =
  Error.unwrap ~context:"getting device"
    (Matrix_client.Devices.get_device (Client.base client) ~device_id)

let update_device client ~device_id ~display_name =
  Error.unwrap ~context:"updating device"
    (Matrix_client.Devices.update_device (Client.base client) ~device_id
       ~display_name)

let delete_device client ~device_id =
  Error.unwrap ~context:"deleting device"
    (Matrix_client.Devices.delete_device (Client.base client) ~device_id)

let delete_devices client ~device_ids =
  Error.unwrap ~context:"deleting devices"
    (Matrix_client.Devices.delete_devices (Client.base client) ~device_ids)
